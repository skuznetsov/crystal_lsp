require "./type"
require "./primitive_type"
require "./class_type"
require "./instance_type"
require "./union_type"
require "./array_type"
require "./range_type"
require "./hash_type"
require "./tuple_type"
require "./module_type"

module CrystalV2
  module Compiler
    module Semantic
      # TypeId - compact identifier for types in TypeArena
      # Similar to ExprId for AST nodes, enables efficient storage and lookup
      struct TypeId
        getter index : Int32

        def initialize(@index : Int32)
        end

        def invalid?
          @index < 0
        end

        def valid?
          @index >= 0
        end

        INVALID = TypeId.new(-1)

        def ==(other : TypeId)
          @index == other.index
        end

        def hash
          @index.hash
        end
      end

      # TypeArena - Arena allocator for Type objects with interning
      #
      # Key features:
      # - Deduplication via interning (same type → same TypeId)
      # - O(1) lookup by TypeId
      # - Compact storage (types stored once)
      # - Fast serialization (just dump the array)
      #
      # PostgreSQL analogy: Like a catalog table with unique constraint
      class TypeArena
        # All unique types stored in order
        @types : Array(Type)

        # Intern table: type signature → TypeId
        # Enables deduplication of structurally equal types
        @intern : Hash(UInt64, TypeId)

        # Pre-allocated primitive types (always at fixed indices)
        getter nil_id : TypeId      # index 0
        getter bool_id : TypeId     # index 1
        getter int32_id : TypeId    # index 2
        getter int64_id : TypeId    # index 3
        getter float64_id : TypeId  # index 4
        getter string_id : TypeId   # index 5
        getter char_id : TypeId     # index 6
        getter symbol_id : TypeId   # index 7

        def initialize
          @types = [] of Type
          @intern = {} of UInt64 => TypeId

          # Pre-allocate primitives at fixed indices for fast access
          @nil_id = intern_new(PrimitiveType.new("Nil"))
          @bool_id = intern_new(PrimitiveType.new("Bool"))
          @int32_id = intern_new(PrimitiveType.new("Int32"))
          @int64_id = intern_new(PrimitiveType.new("Int64"))
          @float64_id = intern_new(PrimitiveType.new("Float64"))
          @string_id = intern_new(PrimitiveType.new("String"))
          @char_id = intern_new(PrimitiveType.new("Char"))
          @symbol_id = intern_new(PrimitiveType.new("Symbol"))
        end

        # Intern a type: returns existing TypeId if type exists, or creates new
        def intern(type : Type) : TypeId
          key = type.hash
          if existing = @intern[key]?
            # Verify it's actually the same type (hash collision check)
            if @types[existing.index] == type
              return existing
            end
            # Hash collision - use linear probe or fall through to new allocation
          end
          intern_new(type)
        end

        # Add new type without checking intern table (internal use)
        private def intern_new(type : Type) : TypeId
          id = TypeId.new(@types.size)
          @types << type
          @intern[type.hash] = id
          id
        end

        # Lookup type by ID - O(1)
        def [](id : TypeId) : Type
          @types[id.index]
        end

        # Safe lookup
        def []?(id : TypeId) : Type?
          return nil if id.invalid?
          return nil if id.index >= @types.size
          @types[id.index]
        end

        # Total number of unique types
        def size : Int32
          @types.size
        end

        # Create union type, interned
        def union(type_ids : Array(TypeId)) : TypeId
          return @nil_id if type_ids.empty?
          return type_ids[0] if type_ids.size == 1

          # Normalize: sort by index, deduplicate
          normalized = type_ids.uniq.sort_by(&.index)
          return normalized[0] if normalized.size == 1

          # Flatten nested unions
          flattened = [] of TypeId
          normalized.each do |tid|
            t = self[tid]
            if t.is_a?(UnionType) && t.responds_to?(:type_ids)
              # Would need to store type_ids in UnionType for this optimization
              flattened << tid
            else
              flattened << tid
            end
          end

          # Build union type
          types = flattened.map { |tid| self[tid] }
          union_type = UnionType.new(types)
          intern(union_type)
        end

        # Create nilable type (T | Nil)
        def nilable(type_id : TypeId) : TypeId
          return type_id if type_id == @nil_id
          union([type_id, @nil_id])
        end

        # Iterate all types
        def each(&)
          @types.each_with_index do |type, idx|
            yield TypeId.new(idx), type
          end
        end
      end

      # ExprTypeIndex - Maps ExprId → TypeId with O(1) access
      #
      # Uses dense array when expression IDs are consecutive (common case),
      # falls back to hash for sparse mappings.
      #
      # PostgreSQL analogy: Like a B-tree index on (expr_id) → type_id
      class ExprTypeIndex
        # Dense storage for consecutive ExprIds
        # Index i stores TypeId for ExprId(i), or TypeId::INVALID if not set
        @dense : Array(TypeId)

        # Sparse fallback for non-consecutive IDs (e.g., after incremental updates)
        @sparse : Hash(Int32, TypeId)?

        # Statistics for optimization decisions
        @hits : Int64 = 0
        @misses : Int64 = 0

        def initialize(capacity : Int32 = 4096)
          @dense = Array(TypeId).new(capacity, TypeId::INVALID)
          @sparse = nil
        end

        # Set type for expression - O(1) amortized
        # Accepts Int32 index directly for decoupling from AST module
        def set(expr_index : Int32, type_id : TypeId)
          idx = expr_index
          if idx >= 0 && idx < @dense.size
            @dense[idx] = type_id
          elsif idx >= 0 && idx < @dense.size * 2
            # Grow dense array
            old_size = @dense.size
            (idx - old_size + 1).times { @dense << TypeId::INVALID }
            @dense[idx] = type_id
          else
            # Use sparse storage for outliers
            (@sparse ||= {} of Int32 => TypeId)[idx] = type_id
          end
        end

        # Get type for expression - O(1)
        # Accepts Int32 index directly for decoupling from AST module
        def get(expr_index : Int32) : TypeId?
          idx = expr_index
          if idx >= 0 && idx < @dense.size
            tid = @dense[idx]
            if tid.valid?
              @hits += 1
              return tid
            end
          end

          # Check sparse
          if sparse = @sparse
            if tid = sparse[idx]?
              @hits += 1
              return tid
            end
          end

          @misses += 1
          nil
        end

        # Bulk invalidate types for a range of ExprIds (e.g., when file changes)
        def invalidate_range(start_id : Int32, end_id : Int32)
          (start_id...end_id).each do |idx|
            if idx >= 0 && idx < @dense.size
              @dense[idx] = TypeId::INVALID
            end
          end
          if sparse = @sparse
            sparse.reject! { |k, _| k >= start_id && k < end_id }
          end
        end

        # Clear all mappings
        def clear
          @dense.fill(TypeId::INVALID)
          @sparse = nil
          @hits = 0
          @misses = 0
        end

        # Statistics
        def hit_rate : Float64
          total = @hits + @misses
          return 0.0 if total == 0
          @hits.to_f / total
        end

        def size : Int32
          count = @dense.count(&.valid?)
          if sparse = @sparse
            count += sparse.size
          end
          count
        end
      end

      # TypeIndex - Main entry point combining all indexes
      #
      # Designed for:
      # - Fast hover/completion (cached type lookups)
      # - Incremental updates (invalidate affected ranges)
      # - Persistent storage (serialize to disk)
      #
      # Types are partitioned by file to avoid ExprId collisions:
      # - Each file has its own ExprTypeIndex
      # - TypeArena is shared (types are interned globally)
      class TypeIndex
        getter arena : TypeArena

        # Legacy global ExprTypeIndex (for backwards compatibility)
        getter expr_types : ExprTypeIndex

        # Per-file type indexes (primary storage)
        # file_path → ExprTypeIndex for that file
        @file_expr_types : Hash(String, ExprTypeIndex)

        # Per-file tracking for incremental invalidation
        # file_path → (start_expr_id, end_expr_id)
        @file_ranges : Hash(String, {Int32, Int32})

        def initialize
          @arena = TypeArena.new
          @expr_types = ExprTypeIndex.new
          @file_expr_types = {} of String => ExprTypeIndex
          @file_ranges = {} of String => {Int32, Int32}
        end

        # Register file's expression ID range for incremental updates
        def register_file(path : String, start_id : Int32, end_id : Int32)
          @file_ranges[path] = {start_id, end_id}
        end

        # Invalidate all types for a file (called when file changes)
        def invalidate_file(path : String)
          # Clear per-file index
          @file_expr_types.delete(path)
          # Also clear legacy global index range
          if range = @file_ranges[path]?
            @expr_types.invalidate_range(range[0], range[1])
          end
        end

        # Set type for expression with file context (preferred)
        def set_type(path : String, expr_index : Int32, type : Type)
          type_id = @arena.intern(type)
          file_index = @file_expr_types[path] ||= ExprTypeIndex.new(1024)
          file_index.set(expr_index, type_id)
        end

        # Set type for expression (legacy global, may collide across files)
        def set_type(expr_index : Int32, type : Type)
          type_id = @arena.intern(type)
          @expr_types.set(expr_index, type_id)
        end

        # Get type for expression with file context (preferred)
        def get_type(path : String, expr_index : Int32) : Type?
          if file_index = @file_expr_types[path]?
            if type_id = file_index.get(expr_index)
              return @arena[type_id]
            end
          end
          nil
        end

        # Get type for expression (legacy global lookup)
        def get_type(expr_index : Int32) : Type?
          if type_id = @expr_types.get(expr_index)
            @arena[type_id]
          end
        end

        # Get per-file index (for validation/debugging)
        def file_index(path : String) : ExprTypeIndex?
          @file_expr_types[path]?
        end

        # Number of files with type info
        def file_count : Int32
          @file_expr_types.size
        end

        # Quick primitive access
        def nil_type : Type
          @arena[@arena.nil_id]
        end

        def int32_type : Type
          @arena[@arena.int32_id]
        end

        def string_type : Type
          @arena[@arena.string_id]
        end

        def bool_type : Type
          @arena[@arena.bool_id]
        end

        # Create union type
        def union_of(types : Array(Type)) : Type
          type_ids = types.map { |t| @arena.intern(t) }
          @arena[@arena.union(type_ids)]
        end

        # Create nilable type
        def nilable(type : Type) : Type
          type_id = @arena.intern(type)
          @arena[@arena.nilable(type_id)]
        end

        # Statistics
        def stats : String
          String.build do |io|
            io << "TypeIndex Stats:\n"
            io << "  Unique types: #{@arena.size}\n"
            io << "  Mapped expressions: #{@expr_types.size}\n"
            io << "  Cache hit rate: #{(@expr_types.hit_rate * 100).round(1)}%\n"
            io << "  Tracked files: #{@file_ranges.size}\n"
          end
        end

        # ══════════════════════════════════════════════════════════════════
        # BINARY SERIALIZATION (PostgreSQL-style page format)
        # ══════════════════════════════════════════════════════════════════

        MAGIC   = "CV2T" # Crystal V2 Types
        VERSION = 2_u32  # v2: per-file ExprTypeIndex storage

        # Serialize to IO
        def write(io : IO)
          io.write(MAGIC.to_slice)
          io.write_bytes(VERSION, IO::ByteFormat::LittleEndian)

          # Write type arena
          write_type_arena(io)

          # Write expression type mappings
          write_expr_types(io)

          # Write file ranges
          write_file_ranges(io)
        end

        # Deserialize from IO
        def self.read(io : IO) : TypeIndex?
          # Verify magic
          magic = Bytes.new(4)
          io.read_fully(magic)
          return nil unless String.new(magic) == MAGIC

          # Verify version
          version = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian)
          return nil unless version == VERSION

          index = TypeIndex.new

          # Read type arena (skip primitives - already initialized)
          read_type_arena(io, index)

          # Read expression type mappings
          read_expr_types(io, index)

          # Read file ranges
          read_file_ranges(io, index)

          index
        end

        private def write_type_arena(io : IO)
          # Skip first 8 primitives (they're fixed)
          custom_start = 8
          count = @arena.size - custom_start

          io.write_bytes(count.to_u32, IO::ByteFormat::LittleEndian)

          @arena.each do |type_id, type|
            next if type_id.index < custom_start
            write_type(io, type)
          end
        end

        private def self.read_type_arena(io : IO, index : TypeIndex)
          count = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian)
          count.times do
            type = read_type(io, index)
            index.arena.intern(type) if type
          end
        end

        # Type tag enum for serialization
        enum TypeTag : UInt8
          Primitive
          ClassType
          InstanceType
          UnionType
          ArrayType
          HashType
          TupleType
          RangeType
          ModuleType
        end

        private def write_type(io : IO, type : Type)
          case type
          when PrimitiveType
            io.write_bytes(TypeTag::Primitive.value, IO::ByteFormat::LittleEndian)
            write_string(io, type.name)
          when ClassType
            # ClassType references runtime symbols - serialize as primitive with name
            io.write_bytes(TypeTag::ClassType.value, IO::ByteFormat::LittleEndian)
            write_string(io, type.symbol.name)
          when InstanceType
            # InstanceType references runtime symbols - serialize as primitive with class name
            io.write_bytes(TypeTag::InstanceType.value, IO::ByteFormat::LittleEndian)
            write_string(io, type.class_symbol.name)
          when UnionType
            io.write_bytes(TypeTag::UnionType.value, IO::ByteFormat::LittleEndian)
            io.write_bytes(type.types.size.to_u32, IO::ByteFormat::LittleEndian)
            type.types.each { |t| write_type(io, t) }
          when ArrayType
            io.write_bytes(TypeTag::ArrayType.value, IO::ByteFormat::LittleEndian)
            write_type(io, type.element_type)
          when HashType
            io.write_bytes(TypeTag::HashType.value, IO::ByteFormat::LittleEndian)
            write_type(io, type.key_type)
            write_type(io, type.value_type)
          when TupleType
            io.write_bytes(TypeTag::TupleType.value, IO::ByteFormat::LittleEndian)
            io.write_bytes(type.element_types.size.to_u32, IO::ByteFormat::LittleEndian)
            type.element_types.each { |t| write_type(io, t) }
          when RangeType
            io.write_bytes(TypeTag::RangeType.value, IO::ByteFormat::LittleEndian)
            write_type(io, type.begin_type)
            write_type(io, type.end_type)
          when ModuleType
            # ModuleType references runtime symbols - serialize with name
            io.write_bytes(TypeTag::ModuleType.value, IO::ByteFormat::LittleEndian)
            write_string(io, type.symbol.name)
          else
            # Unknown type - write as primitive with to_s
            io.write_bytes(TypeTag::Primitive.value, IO::ByteFormat::LittleEndian)
            write_string(io, type.to_s)
          end
        end

        private def self.read_type(io : IO, index : TypeIndex) : Type?
          tag = TypeTag.new(io.read_bytes(UInt8, IO::ByteFormat::LittleEndian))

          case tag
          when .primitive?
            name = read_string(io)
            # Check if it's a known primitive
            case name
            when "Nil"     then index.arena[index.arena.nil_id]
            when "Bool"    then index.arena[index.arena.bool_id]
            when "Int32"   then index.arena[index.arena.int32_id]
            when "Int64"   then index.arena[index.arena.int64_id]
            when "Float64" then index.arena[index.arena.float64_id]
            when "String"  then index.arena[index.arena.string_id]
            when "Char"    then index.arena[index.arena.char_id]
            when "Symbol"  then index.arena[index.arena.symbol_id]
            else
              PrimitiveType.new(name)
            end
          when .class_type?
            # ClassType requires a ClassSymbol which we don't have when loading from disk
            # Fall back to PrimitiveType with name for now (type display purposes)
            name = read_string(io)
            PrimitiveType.new(name)
          when .instance_type?
            # InstanceType requires a ClassSymbol - fall back to PrimitiveType
            name = read_string(io)
            PrimitiveType.new(name)
          when .union_type?
            count = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian)
            types = Array(Type).new(count.to_i)
            count.times { types << (read_type(io, index) || index.nil_type) }
            UnionType.new(types)
          when .array_type?
            elem = read_type(io, index) || index.nil_type
            ArrayType.new(elem)
          when .hash_type?
            key = read_type(io, index) || index.nil_type
            value = read_type(io, index) || index.nil_type
            HashType.new(key, value)
          when .tuple_type?
            count = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian)
            types = Array(Type).new(count.to_i)
            count.times { types << (read_type(io, index) || index.nil_type) }
            TupleType.new(types)
          when .range_type?
            begin_t = read_type(io, index) || index.nil_type
            end_t = read_type(io, index) || index.nil_type
            RangeType.new(begin_t, end_t)
          when .module_type?
            # ModuleType requires a ModuleSymbol - fall back to PrimitiveType
            name = read_string(io)
            PrimitiveType.new(name)
          else
            nil
          end
        end

        private def write_expr_types(io : IO)
          # Write per-file expr types (primary storage)
          io.write_bytes(@file_expr_types.size.to_u32, IO::ByteFormat::LittleEndian)

          @file_expr_types.each do |path, file_index|
            write_string(io, path)

            # Count valid entries in this file's index
            count = file_index.size
            io.write_bytes(count.to_u32, IO::ByteFormat::LittleEndian)

            # Write dense entries
            file_index.@dense.each_with_index do |type_id, expr_idx|
              next unless type_id.valid?
              io.write_bytes(expr_idx.to_i32, IO::ByteFormat::LittleEndian)
              io.write_bytes(type_id.index.to_i32, IO::ByteFormat::LittleEndian)
            end

            # Write sparse entries
            if sparse = file_index.@sparse
              sparse.each do |expr_idx, type_id|
                io.write_bytes(expr_idx.to_i32, IO::ByteFormat::LittleEndian)
                io.write_bytes(type_id.index.to_i32, IO::ByteFormat::LittleEndian)
              end
            end
          end

          # Also write legacy global expr_types for backwards compat
          legacy_count = @expr_types.size
          io.write_bytes(legacy_count.to_u32, IO::ByteFormat::LittleEndian)

          @expr_types.@dense.each_with_index do |type_id, expr_idx|
            next unless type_id.valid?
            io.write_bytes(expr_idx.to_i32, IO::ByteFormat::LittleEndian)
            io.write_bytes(type_id.index.to_i32, IO::ByteFormat::LittleEndian)
          end

          if sparse = @expr_types.@sparse
            sparse.each do |expr_idx, type_id|
              io.write_bytes(expr_idx.to_i32, IO::ByteFormat::LittleEndian)
              io.write_bytes(type_id.index.to_i32, IO::ByteFormat::LittleEndian)
            end
          end
        end

        private def self.read_expr_types(io : IO, index : TypeIndex)
          # Read per-file expr types
          file_count = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian)
          file_count.times do
            path = read_string(io)
            entry_count = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian)

            file_index = ExprTypeIndex.new(1024)
            entry_count.times do
              expr_idx = io.read_bytes(Int32, IO::ByteFormat::LittleEndian)
              type_idx = io.read_bytes(Int32, IO::ByteFormat::LittleEndian)
              type_id = TypeId.new(type_idx)
              file_index.set(expr_idx, type_id)
            end
            index.@file_expr_types[path] = file_index
          end

          # Read legacy global expr_types
          legacy_count = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian)
          legacy_count.times do
            expr_idx = io.read_bytes(Int32, IO::ByteFormat::LittleEndian)
            type_idx = io.read_bytes(Int32, IO::ByteFormat::LittleEndian)
            type_id = TypeId.new(type_idx)
            index.expr_types.set(expr_idx, type_id)
          end
        end

        private def write_file_ranges(io : IO)
          io.write_bytes(@file_ranges.size.to_u32, IO::ByteFormat::LittleEndian)
          @file_ranges.each do |path, range|
            write_string(io, path)
            io.write_bytes(range[0].to_i32, IO::ByteFormat::LittleEndian)
            io.write_bytes(range[1].to_i32, IO::ByteFormat::LittleEndian)
          end
        end

        private def self.read_file_ranges(io : IO, index : TypeIndex)
          count = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian)
          count.times do
            path = read_string(io)
            start_id = io.read_bytes(Int32, IO::ByteFormat::LittleEndian)
            end_id = io.read_bytes(Int32, IO::ByteFormat::LittleEndian)
            index.register_file(path, start_id, end_id)
          end
        end

        # String helpers
        private def write_string(io : IO, str : String)
          io.write_bytes(str.bytesize.to_u32, IO::ByteFormat::LittleEndian)
          io.write(str.to_slice)
        end

        private def self.read_string(io : IO) : String
          len = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian)
          bytes = Bytes.new(len)
          io.read_fully(bytes)
          String.new(bytes)
        end
      end
    end
  end
end
