# Project Cache for fast LSP startup
#
# Caches per-file analysis state to disk, reducing startup time when
# reopening a project. Cache is invalidated when file mtimes change.
#
# Cache format (v6):
# - Magic: "CV2P" (4 bytes)
# - Version: UInt32
# - Project root hash: UInt64
# - File count: UInt32
# - Files: [CachedFileState...] with binary summaries (not JSON)
# - TypeIndex data size: UInt32
# - TypeIndex data: Bytes (binary serialized TypeIndex with per-file storage)

require "../semantic/types/type_index"

module CrystalV2
  module Compiler
    module LSP
      # Cached state for a single file
      # Note: Expression types are stored in TypeIndex, not per-file JSON
      struct CachedFileState
        getter path : String
        getter mtime : Int64             # Unix timestamp
        getter symbols : Array(String)   # Top-level symbol names
        getter requires : Array(String)  # Required file paths
        getter diagnostics_count : Int32 # Just count, not full diagnostics
        @summary_bytes : Bytes           # Binary-encoded symbol summaries
        @cached_summaries : Array(SymbolSummary)?  # Lazy-loaded summaries

        def initialize(
          @path : String,
          @mtime : Int64,
          @symbols : Array(String),
          @requires : Array(String),
          @diagnostics_count : Int32 = 0,
          @summary_bytes : Bytes = Bytes.empty,
        )
          @cached_summaries = nil
        end

        # Create from summaries array (converts to binary)
        def self.from_summaries(
          path : String,
          mtime : Int64,
          symbols : Array(String),
          requires : Array(String),
          summaries : Array(SymbolSummary),
          diagnostics_count : Int32 = 0,
        ) : CachedFileState
          io = IO::Memory.new
          SymbolSummary.to_bytes_array(io, summaries)
          new(path, mtime, symbols, requires, diagnostics_count, io.to_slice.dup)
        end

        # Get summaries (lazy parsing from binary)
        def summaries : Array(SymbolSummary)
          @cached_summaries ||= begin
            return [] of SymbolSummary if @summary_bytes.empty?
            io = IO::Memory.new(@summary_bytes)
            SymbolSummary.from_bytes_array(io)
          rescue
            [] of SymbolSummary
          end
        end

        # Legacy compatibility: return JSON string
        def summary_json : String
          summaries.to_json
        end

        # Binary serialization (v6 format)
        def to_bytes(io : IO)
          write_string(io, @path)
          io.write_bytes(@mtime, IO::ByteFormat::LittleEndian)
          write_string_array(io, @symbols)
          write_string_array(io, @requires)
          io.write_bytes(@diagnostics_count, IO::ByteFormat::LittleEndian)
          # Write binary summaries
          io.write_bytes(@summary_bytes.size.to_u32, IO::ByteFormat::LittleEndian)
          io.write(@summary_bytes)
        end

        def self.from_bytes(io : IO) : CachedFileState
          path = read_string(io)
          mtime = io.read_bytes(Int64, IO::ByteFormat::LittleEndian)
          symbols = read_string_array(io)
          requires = read_string_array(io)
          diagnostics_count = io.read_bytes(Int32, IO::ByteFormat::LittleEndian)
          # Read binary summaries
          summary_size = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian)
          summary_bytes = Bytes.new(summary_size)
          io.read_fully(summary_bytes) if summary_size > 0

          new(path, mtime, symbols, requires, diagnostics_count, summary_bytes)
        end

        private def write_string(io : IO, str : String)
          io.write_bytes(str.bytesize.to_u32, IO::ByteFormat::LittleEndian)
          io.write(str.to_slice)
        end

        private def write_string_array(io : IO, arr : Array(String))
          io.write_bytes(arr.size.to_u32, IO::ByteFormat::LittleEndian)
          arr.each { |s| write_string(io, s) }
        end

        private def self.read_string(io : IO) : String
          size = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian)
          slice = Bytes.new(size)
          io.read_fully(slice)
          String.new(slice)
        end

        private def self.read_string_array(io : IO) : Array(String)
          size = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian)
          Array.new(size.to_i) { read_string(io) }
        end

        # Check if cached state is still valid (file hasn't changed)
        def valid? : Bool
          return false unless File.exists?(@path)
          current_mtime = File.info(@path).modification_time.to_unix
          current_mtime == @mtime
        end
      end

      # Project-level cache
      class ProjectCache
        MAGIC   = "CV2P"
        VERSION = 6_u32  # v6: Binary summaries instead of JSON

        getter files : Array(CachedFileState)
        getter project_root : String
        getter root_hash : UInt64
        getter type_index : Semantic::TypeIndex?

        def initialize(
          @files : Array(CachedFileState),
          @project_root : String,
          @root_hash : UInt64,
          @type_index : Semantic::TypeIndex? = nil
        )
        end

        # Get cache file path for a project
        def self.cache_path(project_root : String) : String
          cache_dir = ENV["XDG_CACHE_HOME"]? || File.join(ENV["HOME"]? || "/tmp", ".cache")
          # Use hash of project root for unique filename
          hash = fnv_hash(project_root)
          File.join(cache_dir, "crystal_v2_lsp", "projects", "#{hash}.cache")
        end

        # Load cache for a project
        def self.load(project_root : String) : ProjectCache?
          path = cache_path(project_root)
          return nil unless File.exists?(path)

          current_hash = fnv_hash(project_root)

          File.open(path, "rb") do |io|
            # Read header
            magic = Bytes.new(4)
            io.read_fully(magic)
            return nil unless String.new(magic) == MAGIC

            version = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian)
            return nil unless version == VERSION

            # Read stored hash
            stored_hash = io.read_bytes(UInt64, IO::ByteFormat::LittleEndian)
            return nil unless stored_hash == current_hash

            # Read files
            count = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian)
            files = Array(CachedFileState).new(count.to_i)
            count.times do
              files << CachedFileState.from_bytes(io)
            end

            # Read TypeIndex (v4+)
            type_index : Semantic::TypeIndex? = nil
            begin
              type_index_size = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian)
              if type_index_size > 0
                type_index_data = Bytes.new(type_index_size)
                io.read_fully(type_index_data)
                type_index = Semantic::TypeIndex.read(IO::Memory.new(type_index_data))
              end
            rescue IO::EOFError
              # Old cache without TypeIndex - that's fine
            end

            new(files, project_root, stored_hash, type_index)
          end
        rescue ex
          nil
        end

        # Maximum cache size (100MB) - if larger, something is wrong
        MAX_CACHE_SIZE = 100_000_000

        # Save cache to disk
        def save
          path = ProjectCache.cache_path(@project_root)
          Dir.mkdir_p(File.dirname(path))

          # Write to memory first to check size
          mem_io = IO::Memory.new

          # Write header
          mem_io.write(MAGIC.to_slice)
          mem_io.write_bytes(VERSION, IO::ByteFormat::LittleEndian)

          # Write hash
          mem_io.write_bytes(@root_hash, IO::ByteFormat::LittleEndian)

          # Write files
          mem_io.write_bytes(@files.size.to_u32, IO::ByteFormat::LittleEndian)
          @files.each(&.to_bytes(mem_io))

          files_size = mem_io.size

          # Write TypeIndex
          if type_idx = @type_index
            # Serialize to memory first to get size
            type_index_io = IO::Memory.new
            type_idx.write(type_index_io)
            type_index_data = type_index_io.to_slice

            mem_io.write_bytes(type_index_data.size.to_u32, IO::ByteFormat::LittleEndian)
            mem_io.write(type_index_data)
          else
            # No TypeIndex
            mem_io.write_bytes(0_u32, IO::ByteFormat::LittleEndian)
          end

          total_size = mem_io.size
          type_index_size = total_size - files_size - 4 # subtract header for type_index_size field

          # Sanity check: reject giant caches
          if total_size > MAX_CACHE_SIZE
            STDERR.puts "[ProjectCache] WARNING: Cache too large (#{total_size} bytes, files=#{files_size}, typeindex=#{type_index_size}), not saving"
            return
          end

          # Write to file
          File.write(path, mem_io.to_slice)
        end

        # Get valid cached files (those that haven't changed)
        def valid_files : Array(CachedFileState)
          @files.select(&.valid?)
        end

        # Get invalid files (need re-parsing)
        def invalid_file_paths : Array(String)
          @files.reject(&.valid?).map(&.path)
        end

        # Create cache from UnifiedProjectState
        def self.from_project(project : UnifiedProjectState, project_root : String) : ProjectCache
          # Build TypeIndex from cached_expr_types (TypeIndex is now the only storage)
          type_index = Semantic::TypeIndex.new

          files = project.files.map do |path, state|
            mtime = state.mtime.try(&.to_unix) || 0_i64

            # Populate TypeIndex from cached_expr_types (per-file storage)
            if expr_types = project.cached_expr_types[path]?
              min_expr_id = Int32::MAX
              max_expr_id = Int32::MIN

              expr_types.each do |expr_id, type_str|
                ptype = Semantic::PrimitiveType.new(type_str)
                type_index.set_type(path, expr_id, ptype)

                min_expr_id = expr_id if expr_id < min_expr_id
                max_expr_id = expr_id if expr_id > max_expr_id
              end

              if min_expr_id <= max_expr_id
                type_index.register_file(path, min_expr_id, max_expr_id + 1)
              end
            end

            CachedFileState.from_summaries(
              path: path,
              mtime: mtime,
              symbols: state.symbols,
              requires: state.requires,
              summaries: state.symbol_summaries,
              diagnostics_count: state.diagnostics.size
            )
          end

          new(files, project_root, fnv_hash(project_root), type_index)
        end

        # FNV-1a hash for project root
        private def self.fnv_hash(str : String) : UInt64
          fnv_offset = 14695981039346656037_u64
          fnv_prime = 1099511628211_u64

          hash = fnv_offset
          str.each_byte do |byte|
            hash ^= byte.to_u64
            hash &*= fnv_prime
          end
          hash
        end
      end

      # Integration methods for UnifiedProjectState
      module ProjectCacheLoader
        # Load project from cache, returns files that need re-parsing
        # Expression types are loaded from TypeIndex (binary format)
        def self.load_from_cache(
          project : UnifiedProjectState,
          project_root : String,
        ) : {valid_count: Int32, invalid_paths: Array(String)}
          cache = ProjectCache.load(project_root)

          unless cache
            return {valid_count: 0, invalid_paths: [] of String}
          end

          valid_files = cache.valid_files
          invalid_paths = cache.invalid_file_paths
          type_index = cache.type_index

          # Load valid files into project (symbols only, no AST)
          valid_files.each do |cached|
            state = FileAnalysisState.new(
              path: cached.path,
              version: 0,
              mtime: Time.unix(cached.mtime),
              root_ids: [] of Frontend::ExprId,
              symbols: cached.symbols,
              diagnostics: [] of Diagnostic,
              requires: cached.requires,
              symbol_summaries: cached.summaries,  # Binary parsing, cached
              from_cache: true # No AST available, only summaries
            )

            project.files[cached.path] = state
            project.file_order << cached.path unless project.file_order.includes?(cached.path)
            project.restore_symbols_from_cache(state)

            cached.symbols.each do |name|
              project.symbol_files[name] = cached.path
            end

            cached.requires.each do |req|
              project.dependencies[cached.path] << req
              project.dependents[req] << cached.path
            end

            # Restore expression types from TypeIndex (binary format)
            if type_index
              restore_expr_types_from_typeindex(project, cached.path, type_index)
            end
          end

          {valid_count: valid_files.size, invalid_paths: invalid_paths}
        end

        # Restore expression types from TypeIndex into project.cached_expr_types
        private def self.restore_expr_types_from_typeindex(
          project : UnifiedProjectState,
          path : String,
          type_index : Semantic::TypeIndex
        )
          if file_index = type_index.file_index(path)
            expr_map = {} of Int32 => String
            file_index.@dense.each_with_index do |type_id, expr_idx|
              next unless type_id.valid?
              if type = type_index.arena[type_id]?
                expr_map[expr_idx] = type.to_s
              end
            end
            if sparse = file_index.@sparse
              sparse.each do |expr_idx, type_id|
                if type = type_index.arena[type_id]?
                  expr_map[expr_idx] = type.to_s
                end
              end
            end
            project.cached_expr_types[path] = expr_map unless expr_map.empty?
          end
        end

        # Save project state to cache
        def self.save_to_cache(project : UnifiedProjectState, project_root : String)
          cache = ProjectCache.from_project(project, project_root)
          cache.save
        end
      end
    end
  end
end
