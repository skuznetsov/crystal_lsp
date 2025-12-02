# Project Cache for fast LSP startup
#
# Caches per-file analysis state to disk, reducing startup time when
# reopening a project. Cache is invalidated when file mtimes change.
#
# Cache format:
# - Magic: "CV2P" (4 bytes)
# - Version: UInt32
# - Project root hash: UInt64
# - File count: UInt32
# - Files: [CachedFileState...]

module CrystalV2
  module Compiler
    module LSP
      # Cached state for a single file
      struct CachedFileState
        getter path : String
        getter mtime : Int64             # Unix timestamp
        getter symbols : Array(String)   # Top-level symbol names
        getter requires : Array(String)  # Required file paths
        getter diagnostics_count : Int32 # Just count, not full diagnostics
        getter summary_json : String     # JSON-encoded symbol summaries
        getter expr_types_json : String  # JSON-encoded expression types (ExprId index -> type string)

        def initialize(
          @path : String,
          @mtime : Int64,
          @symbols : Array(String),
          @requires : Array(String),
          @diagnostics_count : Int32 = 0,
          @summary_json : String = "[]",
          @expr_types_json : String = "{}",
        )
        end

        # Binary serialization
        def to_bytes(io : IO)
          write_string(io, @path)
          io.write_bytes(@mtime, IO::ByteFormat::LittleEndian)
          write_string_array(io, @symbols)
          write_string_array(io, @requires)
          io.write_bytes(@diagnostics_count, IO::ByteFormat::LittleEndian)
          write_string(io, @summary_json)
          write_string(io, @expr_types_json)
        end

        def self.from_bytes(io : IO) : CachedFileState
          path = read_string(io)
          mtime = io.read_bytes(Int64, IO::ByteFormat::LittleEndian)
          symbols = read_string_array(io)
          requires = read_string_array(io)
          diagnostics_count = io.read_bytes(Int32, IO::ByteFormat::LittleEndian)
          summary_json = read_string(io)
          expr_types_json = read_string(io)

          new(path, mtime, symbols, requires, diagnostics_count, summary_json, expr_types_json)
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
        VERSION = 3_u32

        getter files : Array(CachedFileState)
        getter project_root : String
        getter root_hash : UInt64

        def initialize(@files : Array(CachedFileState), @project_root : String, @root_hash : UInt64)
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

            new(files, project_root, stored_hash)
          end
        rescue ex
          nil
        end

        # Save cache to disk
        def save
          path = ProjectCache.cache_path(@project_root)
          Dir.mkdir_p(File.dirname(path))

          File.open(path, "wb") do |io|
            # Write header
            io.write(MAGIC.to_slice)
            io.write_bytes(VERSION, IO::ByteFormat::LittleEndian)

            # Write hash
            io.write_bytes(@root_hash, IO::ByteFormat::LittleEndian)

            # Write files
            io.write_bytes(@files.size.to_u32, IO::ByteFormat::LittleEndian)
            @files.each(&.to_bytes(io))
          end
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
          files = project.files.map do |path, state|
            mtime = state.mtime.try(&.to_unix) || 0_i64
            summary_json = SymbolSummary.to_json_array(state.symbol_summaries)
            expr_types_json = project.cached_expr_types[path]?.try(&.to_json) || "{}"
            CachedFileState.new(
              path: path,
              mtime: mtime,
              symbols: state.symbols,
              requires: state.requires,
              diagnostics_count: state.diagnostics.size,
              summary_json: summary_json,
              expr_types_json: expr_types_json
            )
          end

          new(files, project_root, fnv_hash(project_root))
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

          # Load valid files into project (symbols only, no AST)
          valid_files.each do |cached|
            # Create minimal FileAnalysisState
            state = FileAnalysisState.new(
              path: cached.path,
              version: 0,
              mtime: Time.unix(cached.mtime),
              root_ids: [] of Frontend::ExprId, # No AST in cache
              symbols: cached.symbols,
              diagnostics: [] of Diagnostic, # Will be regenerated if needed
              requires: cached.requires,
              symbol_summaries: SymbolSummary.from_json_array(cached.summary_json)
            )

            # Register in project
            project.files[cached.path] = state
            project.file_order << cached.path unless project.file_order.includes?(cached.path)
            project.restore_symbols_from_cache(state)

            # Register symbols (placeholder - actual symbol objects need parsing)
            cached.symbols.each do |name|
              project.symbol_files[name] = cached.path
            end

            # Update dependencies
            cached.requires.each do |req|
              project.dependencies[cached.path] << req
              project.dependents[req] << cached.path
            end

            # Restore cached expression types (ExprId index -> type string)
            begin
              expr_map = Hash(Int32, String).from_json(cached.expr_types_json)
              project.cached_expr_types[cached.path] = expr_map
            rescue
              # ignore malformed
            end
          end

          {valid_count: valid_files.size, invalid_paths: invalid_paths}
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
