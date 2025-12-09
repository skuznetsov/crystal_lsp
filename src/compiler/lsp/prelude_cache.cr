# Prelude Symbol Cache for fast LSP startup
#
# Caches parsed stdlib symbols to disk, reducing startup time from ~1s to ~50ms.
# Cache is invalidated when stdlib files change.

require "./project_cache"
require "./unified_project"

module CrystalV2
  module Compiler
    module LSP
      # Cached symbol information - lightweight version of Symbol for serialization
      struct CachedSymbolInfo
        getter name : String
        getter kind : SymbolKind
        getter file_path : String
        getter line : Int32
        getter column : Int32
        getter type_annotation : String?
        getter parent_scope : String?
        getter params : Array(String)?        # for methods
        getter return_type : String?          # for methods
        getter superclass : String?           # for classes
        getter type_params : Array(String)?   # for generic classes/methods
        getter? is_class_method : Bool        # for methods: def self.* vs def *

        enum SymbolKind : UInt8
          Class
          Module
          Method
          Constant
          Variable
          Macro
          InstanceVar
          ClassVar
          GlobalVar
        end

        def initialize(
          @name : String,
          @kind : SymbolKind,
          @file_path : String,
          @line : Int32,
          @column : Int32,
          @type_annotation : String? = nil,
          @parent_scope : String? = nil,
          @params : Array(String)? = nil,
          @return_type : String? = nil,
          @superclass : String? = nil,
          @type_params : Array(String)? = nil,
          @is_class_method : Bool = false
        )
        end

        # Binary serialization (custom format for speed)
        def to_bytes(io : IO)
          write_string(io, @name)
          io.write_byte(@kind.value)
          write_string(io, @file_path)
          io.write_bytes(@line, IO::ByteFormat::LittleEndian)
          io.write_bytes(@column, IO::ByteFormat::LittleEndian)
          write_optional_string(io, @type_annotation)
          write_optional_string(io, @parent_scope)
          write_optional_string_array(io, @params)
          write_optional_string(io, @return_type)
          write_optional_string(io, @superclass)
          write_optional_string_array(io, @type_params)
          io.write_byte(@is_class_method ? 1_u8 : 0_u8)
        end

        def self.from_bytes(io : IO) : CachedSymbolInfo
          name = read_string(io)
          kind = SymbolKind.new(io.read_byte.not_nil!)
          file_path = read_string(io)
          line = io.read_bytes(Int32, IO::ByteFormat::LittleEndian)
          column = io.read_bytes(Int32, IO::ByteFormat::LittleEndian)
          type_annotation = read_optional_string(io)
          parent_scope = read_optional_string(io)
          params = read_optional_string_array(io)
          return_type = read_optional_string(io)
          superclass = read_optional_string(io)
          type_params = read_optional_string_array(io)
          is_class_method = io.read_byte.not_nil! == 1_u8

          new(name, kind, file_path, line, column, type_annotation, parent_scope, params, return_type, superclass, type_params, is_class_method)
        end

        private def write_string(io : IO, str : String)
          io.write_bytes(str.bytesize.to_u32, IO::ByteFormat::LittleEndian)
          io.write(str.to_slice)
        end

        private def write_optional_string(io : IO, str : String?)
          if str
            io.write_byte(1_u8)
            write_string(io, str)
          else
            io.write_byte(0_u8)
          end
        end

        private def write_optional_string_array(io : IO, arr : Array(String)?)
          if arr
            io.write_byte(1_u8)
            io.write_bytes(arr.size.to_u32, IO::ByteFormat::LittleEndian)
            arr.each { |s| write_string(io, s) }
          else
            io.write_byte(0_u8)
          end
        end

        private def self.read_string(io : IO) : String
          size = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian)
          slice = Bytes.new(size)
          io.read_fully(slice)
          String.new(slice)
        end

        private def self.read_optional_string(io : IO) : String?
          flag = io.read_byte.not_nil!
          flag == 1_u8 ? read_string(io) : nil
        end

        private def self.read_optional_string_array(io : IO) : Array(String)?
          flag = io.read_byte.not_nil!
          return nil if flag == 0_u8
          size = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian)
          Array.new(size.to_i) { read_string(io) }
        end
      end

      # Cache file format (v5):
      # - Magic: "CV2C" (4 bytes)
      # - Version: UInt32
      # - Hash of stdlib mtimes (UInt64, 8 bytes)
      # - Symbol count: UInt32
      # - Symbols: [CachedSymbolInfo...]
      # - File count: UInt32
      # - Files: [CachedFileState...]
      # - TypeIndex data size: UInt32 (0 if none)
      # - TypeIndex data: Bytes (binary serialized TypeIndex)
      class PreludeCache
        MAGIC   = "CV2C"
        VERSION = 6_u32  # v6: Binary summaries instead of JSON in CachedFileState

        getter symbols : Array(CachedSymbolInfo)
        getter stdlib_hash : UInt64
        getter files : Array(CachedFileState)
        getter type_index : Semantic::TypeIndex?

        def initialize(
          @symbols : Array(CachedSymbolInfo),
          @stdlib_hash : UInt64,
          @files : Array(CachedFileState) = [] of CachedFileState,
          @type_index : Semantic::TypeIndex? = nil
        )
        end

        def self.cache_path : String
          cache_dir = ENV["XDG_CACHE_HOME"]? || File.join(ENV["HOME"]? || "/tmp", ".cache")
          File.join(cache_dir, "crystal_v2_lsp", "prelude.cache")
        end

        def self.load(stdlib_path : String) : PreludeCache?
          path = cache_path
          return nil unless File.exists?(path)

          current_hash = compute_stdlib_hash(stdlib_path)

          File.open(path, "rb") do |io|
            # Read header
            magic = Bytes.new(4)
            io.read_fully(magic)
            return nil unless String.new(magic) == MAGIC

            version = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian)
            return nil unless version == VERSION

            # Read stored hash
            stored_hash = io.read_bytes(UInt64, IO::ByteFormat::LittleEndian)

            # Validate hash
            return nil unless stored_hash == current_hash

            # Read symbols
            count = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian)
            symbols = Array(CachedSymbolInfo).new(count.to_i)
            count.times do
              symbols << CachedSymbolInfo.from_bytes(io)
            end

            file_count = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian)
            files = Array(CachedFileState).new(file_count.to_i)
            file_count.times do
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

            new(symbols, stored_hash, files, type_index)
          end
        rescue ex
          nil
        end

        def save
          path = PreludeCache.cache_path
          Dir.mkdir_p(File.dirname(path))

          File.open(path, "wb") do |io|
            # Write header
            io.write(MAGIC.to_slice)
            io.write_bytes(VERSION, IO::ByteFormat::LittleEndian)

            # Write hash
            io.write_bytes(@stdlib_hash, IO::ByteFormat::LittleEndian)

            # Write symbols
            io.write_bytes(@symbols.size.to_u32, IO::ByteFormat::LittleEndian)
            @symbols.each(&.to_bytes(io))

            io.write_bytes(@files.size.to_u32, IO::ByteFormat::LittleEndian)
            @files.each(&.to_bytes(io))

            # Write TypeIndex
            if type_idx = @type_index
              # Serialize to memory first to get size
              type_index_io = IO::Memory.new
              type_idx.write(type_index_io)
              type_index_data = type_index_io.to_slice

              io.write_bytes(type_index_data.size.to_u32, IO::ByteFormat::LittleEndian)
              io.write(type_index_data)
            else
              # No TypeIndex
              io.write_bytes(0_u32, IO::ByteFormat::LittleEndian)
            end
          end
        end

        # Compute simple hash of all stdlib file mtimes (no OpenSSL needed)
        # Uses FNV-1a algorithm for fast, reasonable collision resistance
        def self.compute_stdlib_hash(stdlib_path : String) : UInt64
          # FNV-1a constants for 64-bit
          fnv_offset = 14695981039346656037_u64
          fnv_prime = 1099511628211_u64

          hash = fnv_offset

          # Get mtime of prelude and key stdlib files
          key_files = [
            "prelude.cr",
            "object.cr",
            "class.cr",
            "string.cr",
            "int.cr",
            "array.cr",
            "hash.cr",
            "io.cr",
            "file.cr",
          ]

          key_files.each do |name|
            full_path = File.join(stdlib_path, name)
            if File.exists?(full_path)
              mtime = File.info(full_path).modification_time.to_unix
              # Hash the filename
              name.each_byte do |byte|
                hash ^= byte.to_u64
                hash &*= fnv_prime
              end
              # Hash the mtime
              8.times do |i|
                hash ^= ((mtime >> (i * 8)) & 0xFF).to_u64
                hash &*= fnv_prime
              end
            end
          end

          hash
        end
      end

      # Extracts CachedSymbolInfo from a live SymbolTable
      module SymbolExtractor
        def self.extract_symbols(
          table : Semantic::SymbolTable,
          program : Frontend::Program,
          file_path : String,
          parent_scope : String? = nil
        ) : Array(CachedSymbolInfo)
          result = [] of CachedSymbolInfo

          table.each_local_symbol do |name, symbol|
            info = extract_symbol(symbol, program, file_path, parent_scope)
            result << info if info

            # Recurse into nested scopes
            case symbol
            when Semantic::ClassSymbol
              # Extract instance methods from scope
              nested = extract_symbols(symbol.scope, program, symbol.file_path || file_path, name)
              result.concat(nested)
              # Extract class methods from class_scope (def self.*)
              class_nested = extract_symbols(symbol.class_scope, program, symbol.file_path || file_path, name)
              result.concat(class_nested)
            when Semantic::ModuleSymbol
              nested = extract_symbols(symbol.scope, program, symbol.file_path || file_path, name)
              result.concat(nested)
            end
          end

          result
        end

        private def self.extract_symbol(
          symbol : Semantic::Symbol,
          program : Frontend::Program,
          file_path : String,
          parent_scope : String?
        ) : CachedSymbolInfo?
          node_id = symbol.node_id
          return nil if node_id.index < 0 || node_id.index >= program.arena.size

          span = program.arena[node_id].span
          line = span.start_line.to_i32
          column = span.start_column.to_i32
          actual_path = symbol.file_path || file_path

          case symbol
          when Semantic::ClassSymbol
            CachedSymbolInfo.new(
              name: symbol.name,
              kind: CachedSymbolInfo::SymbolKind::Class,
              file_path: actual_path,
              line: line,
              column: column,
              parent_scope: parent_scope,
              superclass: symbol.superclass_name,
              type_params: symbol.type_parameters
            )
          when Semantic::ModuleSymbol
            CachedSymbolInfo.new(
              name: symbol.name,
              kind: CachedSymbolInfo::SymbolKind::Module,
              file_path: actual_path,
              line: line,
              column: column,
              parent_scope: parent_scope
            )
          when Semantic::MethodSymbol
            params = symbol.params.compact_map { |p| p.name.try { |n| String.new(n) } }
            CachedSymbolInfo.new(
              name: symbol.name,
              kind: CachedSymbolInfo::SymbolKind::Method,
              file_path: actual_path,
              line: line,
              column: column,
              parent_scope: parent_scope,
              params: params,
              return_type: symbol.return_annotation,
              type_params: symbol.type_parameters,
              is_class_method: symbol.is_class_method?
            )
          when Semantic::MacroSymbol
            CachedSymbolInfo.new(
              name: symbol.name,
              kind: CachedSymbolInfo::SymbolKind::Macro,
              file_path: actual_path,
              line: line,
              column: column,
              parent_scope: parent_scope,
              params: symbol.params
            )
          when Semantic::ConstantSymbol
            CachedSymbolInfo.new(
              name: symbol.name,
              kind: CachedSymbolInfo::SymbolKind::Constant,
              file_path: actual_path,
              line: line,
              column: column,
              parent_scope: parent_scope
            )
          when Semantic::VariableSymbol
            CachedSymbolInfo.new(
              name: symbol.name,
              kind: CachedSymbolInfo::SymbolKind::Variable,
              file_path: actual_path,
              line: line,
              column: column,
              parent_scope: parent_scope,
              type_annotation: symbol.declared_type
            )
          when Semantic::InstanceVarSymbol
            CachedSymbolInfo.new(
              name: symbol.name,
              kind: CachedSymbolInfo::SymbolKind::InstanceVar,
              file_path: actual_path,
              line: line,
              column: column,
              parent_scope: parent_scope,
              type_annotation: symbol.declared_type
            )
          when Semantic::ClassVarSymbol
            CachedSymbolInfo.new(
              name: symbol.name,
              kind: CachedSymbolInfo::SymbolKind::ClassVar,
              file_path: actual_path,
              line: line,
              column: column,
              parent_scope: parent_scope,
              type_annotation: symbol.declared_type
            )
          when Semantic::GlobalVarSymbol
            CachedSymbolInfo.new(
              name: symbol.name,
              kind: CachedSymbolInfo::SymbolKind::GlobalVar,
              file_path: actual_path,
              line: line,
              column: column,
              parent_scope: parent_scope,
              type_annotation: symbol.declared_type
            )
          else
            nil
          end
        end
      end

      # Reconstructs SymbolTable from cached symbols
      module SymbolReconstructor
        def self.rebuild_table(cache : PreludeCache) : Semantic::SymbolTable
          table = Semantic::SymbolTable.new
          scope_cache = {} of String => Semantic::SymbolTable
          class_scope_cache = {} of String => Semantic::SymbolTable # for def self.* methods

          # First pass: create all top-level symbols and scopes
          cache.symbols.each do |info|
            next if info.parent_scope # Skip nested for now

            symbol = create_symbol(info)
            next unless symbol

            begin
              table.define(info.name, symbol)
              if symbol.is_a?(Semantic::ClassSymbol)
                scope_cache[info.name] = symbol.scope
                class_scope_cache[info.name] = symbol.class_scope
              elsif symbol.is_a?(Semantic::ModuleSymbol)
                scope_cache[info.name] = symbol.scope
              end
            rescue Semantic::SymbolRedefinitionError
              table.redefine(info.name, symbol)
            end
          end

          # Second pass: add nested symbols
          cache.symbols.each do |info|
            next unless parent = info.parent_scope

            # For class methods (def self.*), use class_scope; otherwise use instance scope
            scope = if info.is_class_method? && info.kind.method?
                      class_scope_cache[parent]?
                    else
                      scope_cache[parent]?
                    end
            next unless scope

            symbol = create_symbol(info)
            next unless symbol

            begin
              scope.define(info.name, symbol)
              if symbol.is_a?(Semantic::ClassSymbol)
                scope_cache["#{parent}::#{info.name}"] = symbol.scope
                class_scope_cache["#{parent}::#{info.name}"] = symbol.class_scope
              elsif symbol.is_a?(Semantic::ModuleSymbol)
                scope_cache["#{parent}::#{info.name}"] = symbol.scope
              end
            rescue Semantic::SymbolRedefinitionError
              scope.redefine(info.name, symbol)
            end
          end

          table
        end

        private def self.create_symbol(info : CachedSymbolInfo) : Semantic::Symbol?
          # Use dummy node_id (0) since we don't have AST
          node_id = Frontend::ExprId.new(0)

          case info.kind
          when .class?
            scope = Semantic::SymbolTable.new
            class_scope = Semantic::SymbolTable.new
            symbol = Semantic::ClassSymbol.new(
              info.name,
              node_id,
              scope: scope,
              class_scope: class_scope,
              superclass_name: info.superclass,
              type_parameters: info.type_params
            )
            symbol.file_path = info.file_path
            symbol
          when .module?
            scope = Semantic::SymbolTable.new
            symbol = Semantic::ModuleSymbol.new(info.name, node_id, scope: scope)
            symbol.file_path = info.file_path
            symbol
          when .method?
            params = (info.params || [] of String).map do |name|
              Frontend::Parameter.new(name: name.to_slice)
            end
            scope = Semantic::SymbolTable.new
            symbol = Semantic::MethodSymbol.new(
              info.name,
              node_id,
              params: params,
              return_annotation: info.return_type,
              scope: scope,
              type_parameters: info.type_params,
              is_class_method: info.is_class_method?
            )
            symbol.file_path = info.file_path
            symbol
          when .macro?
            symbol = Semantic::MacroSymbol.new(
              info.name,
              node_id,
              body: node_id,
              params: info.params
            )
            symbol.file_path = info.file_path
            symbol
          when .constant?
            symbol = Semantic::ConstantSymbol.new(info.name, node_id, value: node_id)
            symbol.file_path = info.file_path
            symbol
          when .variable?
            symbol = Semantic::VariableSymbol.new(info.name, node_id, info.type_annotation)
            symbol.file_path = info.file_path
            symbol
          when .instance_var?
            symbol = Semantic::InstanceVarSymbol.new(info.name, node_id, info.type_annotation, info.file_path)
            symbol
          when .class_var?
            symbol = Semantic::ClassVarSymbol.new(info.name, node_id, info.type_annotation, info.file_path)
            symbol
          when .global_var?
            symbol = Semantic::GlobalVarSymbol.new(info.name, node_id, info.type_annotation, info.file_path)
            symbol
          else
            nil
          end
        end
      end
    end
  end
end
