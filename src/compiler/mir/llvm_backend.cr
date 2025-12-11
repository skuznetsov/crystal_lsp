# Crystal v2 MIR → LLVM Backend
#
# This module generates LLVM IR from MIR, with:
# 1. Debug info injection from day 1
# 2. Hybrid memory management (Stack/Slab/ARC/GC)
# 3. Global type metadata for enhanced debugging (LLDB Python + DAP)
#
# Design approach:
# - Generate LLVM IR as text (.ll files)
# - Can be compiled with llc or fed to LLVM JIT
# - Type metadata embedded as global arrays for debugger access
#
# When LLVM Crystal bindings are available, this can be upgraded to
# use LLVM::Module directly for better performance.

require "./mir"

module Crystal::MIR
  # ═══════════════════════════════════════════════════════════════════════════
  # TYPE METADATA STRUCTURES (embedded in binary for debugger access)
  # ═══════════════════════════════════════════════════════════════════════════

  # Type info entry - embedded as __crystal_type_info global array
  struct TypeInfoEntry
    property type_id : UInt32
    property flags : UInt32          # is_struct, is_class, is_union, etc.
    property name_offset : UInt32    # Offset into string table
    property size : UInt32
    property alignment : UInt32
    property parent_type_id : UInt32 # 0xFFFFFFFF for no parent
    property fields_count : UInt32
    property fields_offset : UInt32  # Offset into fields table

    # Flags
    FLAG_STRUCT    = 0x0001_u32
    FLAG_CLASS     = 0x0002_u32
    FLAG_UNION     = 0x0004_u32
    FLAG_CLOSURE   = 0x0008_u32
    FLAG_ENUM      = 0x0010_u32
    FLAG_ABSTRACT  = 0x0020_u32
    FLAG_GENERIC   = 0x0040_u32
    FLAG_PRIMITIVE = 0x0080_u32

    def initialize(@type_id, @flags, @name_offset, @size, @alignment,
                   @parent_type_id, @fields_count, @fields_offset)
    end
  end

  # Field info - embedded as __crystal_field_info global array
  struct FieldInfoEntry
    property name_offset : UInt32
    property type_id : UInt32
    property offset : UInt32
    property flags : UInt32  # is_pointer, is_nilable, etc.

    FLAG_POINTER  = 0x0001_u32
    FLAG_NILABLE  = 0x0002_u32
    FLAG_CAPTURED = 0x0004_u32  # Captured in closure

    def initialize(@name_offset, @type_id, @offset, @flags)
    end
  end

  # Union variant info - embedded as __crystal_union_variant_info global array
  struct UnionVariantInfoEntry
    property union_type_id : UInt32      # Which union this variant belongs to
    property variant_type_id : UInt32    # Discriminator value for this variant
    property variant_type_ref : UInt32   # Type ID of the variant's actual type
    property name_offset : UInt32        # Full name offset in string table
    property size : UInt32               # Size of this variant in bytes
    property alignment : UInt32          # Alignment requirement

    def initialize(@union_type_id, @variant_type_id, @variant_type_ref, @name_offset, @size, @alignment)
    end
  end

  # Union descriptor - embedded as __crystal_union_info global array
  struct UnionInfoEntry
    property type_id : UInt32            # Type ID of this union
    property name_offset : UInt32        # Display name (e.g., "Int32 | String | Nil")
    property variant_count : UInt32      # Number of variants
    property variants_offset : UInt32    # Offset into variant info array
    property total_size : UInt32         # Total union size (header + payload)
    property alignment : UInt32          # Alignment requirement
    property payload_offset : UInt32     # Offset to payload from start

    def initialize(@type_id, @name_offset, @variant_count, @variants_offset, @total_size, @alignment, @payload_offset)
    end
  end

  # Closure capture info - embedded as __crystal_closure_info
  struct ClosureInfoEntry
    property closure_type_id : UInt32
    property captured_count : UInt32
    property captures_offset : UInt32  # Offset into captured vars table

    def initialize(@closure_type_id, @captured_count, @captures_offset)
    end
  end

  struct CapturedVarEntry
    property name_offset : UInt32      # Original variable name
    property type_id : UInt32
    property offset : UInt32           # Offset in closure_data

    def initialize(@name_offset, @type_id, @offset)
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # LLVM TYPE MAPPING (MIR types → LLVM IR type names)
  # ═══════════════════════════════════════════════════════════════════════════

  class LLVMTypeMapper
    @type_registry : TypeRegistry
    @type_ref_cache : Hash(TypeRef, String)

    def initialize(@type_registry : TypeRegistry)
      @type_ref_cache = {} of TypeRef => String
    end

    def llvm_type(type_ref : TypeRef) : String
      @type_ref_cache[type_ref] ||= compute_llvm_type(type_ref)
    end

    def llvm_type(type : Type) : String
      compute_llvm_type_for_type(type)
    end

    # For alloca - returns actual struct type (not ptr)
    def llvm_alloca_type(type_ref : TypeRef) : String
      if type = @type_registry.get(type_ref)
        if type.kind.struct?
          # Struct needs actual type for alloca, not ptr
          "%#{mangle_name(type.name)}"
        else
          compute_llvm_type_for_type(type)
        end
      else
        compute_llvm_type(type_ref)
      end
    end

    private def compute_llvm_type(type_ref : TypeRef) : String
      if type = @type_registry.get(type_ref)
        compute_llvm_type_for_type(type)
      else
        # Primitive type based on TypeRef ID
        case type_ref
        when TypeRef::VOID    then "void"
        when TypeRef::NIL     then "void"
        when TypeRef::BOOL    then "i1"
        when TypeRef::INT8    then "i8"
        when TypeRef::INT16   then "i16"
        when TypeRef::INT32   then "i32"
        when TypeRef::INT64   then "i64"
        when TypeRef::INT128  then "i128"
        when TypeRef::UINT8   then "i8"
        when TypeRef::UINT16  then "i16"
        when TypeRef::UINT32  then "i32"
        when TypeRef::UINT64  then "i64"
        when TypeRef::UINT128 then "i128"
        when TypeRef::FLOAT32 then "float"
        when TypeRef::FLOAT64 then "double"
        when TypeRef::CHAR    then "i32"
        when TypeRef::STRING  then "ptr"
        when TypeRef::SYMBOL  then "i32"
        when TypeRef::POINTER then "ptr"
        else                       "ptr"  # Unknown → opaque pointer
        end
      end
    end

    private def compute_llvm_type_for_type(type : Type) : String
      case type.kind
      when .void?                       then "void"
      when .bool?                       then "i1"
      when .int8?, .u_int8?             then "i8"
      when .int16?, .u_int16?           then "i16"
      when .int32?, .u_int32?           then "i32"
      when .int64?, .u_int64?           then "i64"
      when .int128?, .u_int128?         then "i128"
      when .float32?                    then "float"
      when .float64?                    then "double"
      when .char?                       then "i32"
      when .symbol?                     then "i32"
      when .pointer?                    then "ptr"
      when .reference?                  then "ptr"
      when .struct?                     then "ptr"  # Struct returns ptr (stack allocated)
      when .union?                      then "%#{mangle_name(type.name)}.union"
      when .proc?                       then "%__crystal_proc"  # { ptr, ptr }
      when .tuple?                      then compute_tuple_type(type)
      when .array?                      then compute_array_type(type)
      else                                   "ptr"
      end
    end

    private def compute_tuple_type(type : Type) : String
      if elements = type.element_types
        element_types = elements.map { |e| compute_llvm_type_for_type(e) }
        "{ #{element_types.join(", ")} }"
      else
        "{}"
      end
    end

    private def compute_array_type(type : Type) : String
      if elem = type.element_type
        "[0 x #{compute_llvm_type_for_type(elem)}]"  # Variable-size
      else
        "[0 x i8]"
      end
    end

    def mangle_name(name : String) : String
      name.gsub(/[^a-zA-Z0-9_]/, "_")
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # LLVM IR TEXT GENERATOR
  # ═══════════════════════════════════════════════════════════════════════════

  class LLVMIRGenerator
    @module : Module
    @type_mapper : LLVMTypeMapper
    @output : IO::Memory
    @indent : Int32
    @value_names : Hash(ValueId, String)
    @block_names : Hash(BlockId, String)
    @current_return_type : String = "void"
    @constant_values : Hash(ValueId, String)  # For inlining constants
    @value_types : Hash(ValueId, TypeRef)     # For tracking operand types
    @array_info : Hash(ValueId, {String, Int32})  # Array element_type and size
    @string_constants : Hash(String, String)  # String value -> global name
    @string_counter : Int32 = 0

    # Type metadata for debug DX
    @type_info_entries : Array(TypeInfoEntry)
    @field_info_entries : Array(FieldInfoEntry)
    @union_info_entries : Array(UnionInfoEntry)
    @union_variant_entries : Array(UnionVariantInfoEntry)
    @string_table : IO::Memory
    @string_offsets : Hash(String, UInt32)

    property emit_debug_info : Bool = true
    property emit_type_metadata : Bool = true
    property target_triple : String = {% if flag?(:darwin) %}
                                        {% if flag?(:aarch64) %}
                                          "arm64-apple-macosx"
                                        {% else %}
                                          "x86_64-apple-macosx"
                                        {% end %}
                                      {% else %}
                                        "x86_64-unknown-linux-gnu"
                                      {% end %}

    def initialize(@module : Module)
      @type_mapper = LLVMTypeMapper.new(@module.type_registry)
      @output = IO::Memory.new
      @indent = 0
      @value_names = {} of ValueId => String
      @block_names = {} of BlockId => String
      @constant_values = {} of ValueId => String
      @value_types = {} of ValueId => TypeRef
      @array_info = {} of ValueId => {String, Int32}
      @string_constants = {} of String => String

      # Type metadata
      @type_info_entries = [] of TypeInfoEntry
      @field_info_entries = [] of FieldInfoEntry
      @union_info_entries = [] of UnionInfoEntry
      @union_variant_entries = [] of UnionVariantInfoEntry
      @string_table = IO::Memory.new
      @string_offsets = {} of String => UInt32
    end

    def generate : String
      emit_header
      emit_type_definitions
      emit_runtime_declarations
      emit_union_debug_helpers
      emit_global_variables

      if @emit_type_metadata
        collect_type_metadata
        collect_union_metadata
      end

      @module.functions.each do |func|
        emit_function(func)
      end

      # Emit string constants at end (LLVM allows globals anywhere)
      emit_string_constants

      if @emit_type_metadata
        emit_type_metadata_globals
        emit_union_metadata_globals
      end

      @output.to_s
    end

    private def emit_string_constants
      return if @string_constants.empty?

      emit_raw "\n; String constants\n"
      @string_constants.each do |str, global_name|
        # Escape string for LLVM: replace special chars
        escaped = str.gsub("\\", "\\\\")
                    .gsub("\n", "\\0A")
                    .gsub("\r", "\\0D")
                    .gsub("\t", "\\09")
                    .gsub("\"", "\\22")
        len = str.bytesize + 1  # +1 for null terminator
        emit_raw "#{global_name} = private unnamed_addr constant [#{len} x i8] c\"#{escaped}\\00\", align 1\n"
      end
    end

    private def emit_global_variables
      return if @module.globals.empty?

      @module.globals.each do |global|
        llvm_type = @type_mapper.llvm_type(global.type)
        initial = global.initial_value || 0_i64
        emit_raw "@#{global.name} = global #{llvm_type} #{initial}\n"
      end
      emit_raw "\n"
    end

    private def emit(s : String)
      @output << ("  " * @indent) << s << "\n"
    end

    private def emit_raw(s : String)
      @output << s
    end

    private def emit_header
      source = @module.source_file || "unknown.cr"
      emit_raw "; ModuleID = '#{@module.name}'\n"
      emit_raw "source_filename = \"#{source}\"\n"
      emit_raw "target triple = \"#{@target_triple}\"\n"
      emit_raw "\n"
    end

    private def emit_type_definitions
      # Emit proc type
      emit_raw "%__crystal_proc = type { ptr, ptr }\n"

      # Emit struct types from registry
      @module.type_registry.types.each do |type|
        if type.kind.struct? && !type.kind.primitive?
          emit_struct_type(type)
        elsif type.kind.union?
          emit_union_type(type)
        end
      end

      emit_raw "\n"
    end

    private def emit_struct_type(type : Type)
      name = @type_mapper.mangle_name(type.name)
      fields = type.fields

      if fields && !fields.empty?
        field_types = [] of String

        # Add type_id as first field for class types (not value types)
        unless type.is_value_type?
          field_types << "i32"  # type_id
        end

        fields.each do |field|
          field_types << @type_mapper.llvm_type(field.type_ref)
        end

        emit_raw "%#{name} = type { #{field_types.join(", ")} }\n"
      else
        emit_raw "%#{name} = type {}\n"
      end
    end

    private def emit_union_type(type : Type)
      name = @type_mapper.mangle_name(type.name)
      # Union = { i32 discriminator, [max_size x i8] data }
      max_size = type.variants.try(&.map(&.size).max) || 8_u64
      emit_raw "%#{name}.union = type { i32, [#{max_size} x i8] }\n"
    end

    private def emit_runtime_declarations
      # Memory allocation
      emit_raw "declare ptr @__crystal_v2_malloc64(i64)\n"
      emit_raw "declare ptr @__crystal_malloc_atomic64(i64)\n"
      emit_raw "declare ptr @__crystal_realloc64(ptr, i64)\n"
      emit_raw "declare void @free(ptr)\n"
      emit_raw "\n"

      # ARC runtime
      emit_raw "declare void @__crystal_v2_rc_inc(ptr)\n"
      emit_raw "declare void @__crystal_v2_rc_dec(ptr, ptr)\n"
      emit_raw "\n"

      # Slab allocator
      emit_raw "declare ptr @__crystal_v2_slab_alloc(i32)\n"
      emit_raw "declare void @__crystal_v2_slab_free(ptr, i32)\n"
      emit_raw "\n"

      # IO functions
      emit_raw "declare void @__crystal_v2_puts(ptr)\n"
      emit_raw "declare void @__crystal_v2_print_int32(i32)\n"
      emit_raw "declare void @__crystal_v2_print_int32_ln(i32)\n"
      emit_raw "declare void @__crystal_v2_print_int64(i64)\n"
      emit_raw "declare void @__crystal_v2_print_int64_ln(i64)\n"
      emit_raw "\n"
    end

    # Union debug helper function declarations
    private def emit_union_debug_helpers
      emit_raw "; Union debug helper functions\n"
      # Debug print: prints union value with type name
      emit_raw "declare void @__crystal_v2_union_debug_print(ptr, ptr)\n"
      # Type check with error: verifies type_id, traps on mismatch
      emit_raw "declare void @__crystal_v2_union_type_check(i32, i32, ptr)\n"
      # Get type name from union descriptor
      emit_raw "declare ptr @__crystal_v2_union_type_name(i32, ptr)\n"
      emit_raw "\n"
    end

    private def emit_function(func : Function)
      reset_value_names(func)

      # Function signature
      param_types = func.params.map { |p| "#{@type_mapper.llvm_type(p.type)} %#{p.name}" }
      return_type = @type_mapper.llvm_type(func.return_type)
      @current_return_type = return_type  # Store for terminator emission

      mangled_name = @type_mapper.mangle_name(func.name)
      emit_raw "define #{return_type} @#{mangled_name}(#{param_types.join(", ")}) {\n"

      func.blocks.each do |block|
        emit_block(block, func)
      end

      emit_raw "}\n\n"
    end

    private def reset_value_names(func : Function)
      @value_names.clear
      @block_names.clear
      @constant_values.clear
      @value_types.clear
      @array_info.clear

      func.params.each do |param|
        @value_names[param.index] = param.name
        @value_types[param.index] = param.type
      end

      func.blocks.each do |block|
        @block_names[block.id] = "bb#{block.id}"
      end
    end

    private def emit_block(block : BasicBlock, func : Function)
      emit_raw "#{@block_names[block.id]}:\n"
      @indent = 1

      block.instructions.each do |inst|
        emit_instruction(inst, func)
      end

      emit_terminator(block.terminator)
      @indent = 0
    end

    private def emit_instruction(inst : Value, func : Function)
      name = "%r#{inst.id}"
      @value_names[inst.id] = "r#{inst.id}"
      @value_types[inst.id] = inst.type

      case inst
      when Constant
        emit_constant(inst, name)
      when Alloc
        # For Alloc, store the alloc_type (the actual type being allocated)
        # so GEP can use proper struct type
        @value_types[inst.id] = inst.alloc_type
        emit_alloc(inst, name)
      when Free
        emit_free(inst)
      when RCIncrement
        emit_rc_inc(inst)
      when RCDecrement
        emit_rc_dec(inst)
      when Load
        emit_load(inst, name)
      when Store
        emit_store(inst)
      when GetElementPtr
        emit_gep(inst, name)
      when BinaryOp
        emit_binary_op(inst, name)
      when UnaryOp
        emit_unary_op(inst, name)
      when Cast
        emit_cast(inst, name)
      when Phi
        emit_phi(inst, name)
      when Select
        emit_select(inst, name)
      when Call
        emit_call(inst, name, func)
      when IndirectCall
        emit_indirect_call(inst, name)
      when ExternCall
        emit_extern_call(inst, name)
      when GlobalLoad
        emit_global_load(inst, name)
      when GlobalStore
        emit_global_store(inst, name)
      when UnionWrap
        emit_union_wrap(inst, name)
      when UnionUnwrap
        emit_union_unwrap(inst, name)
      when UnionTypeIdGet
        emit_union_type_id_get(inst, name)
      when UnionIs
        emit_union_is(inst, name)
      when ArrayLiteral
        emit_array_literal(inst, name)
      when ArraySize
        emit_array_size(inst, name)
      when ArrayGet
        emit_array_get(inst, name)
      end
    end

    private def emit_constant(inst : Constant, name : String)
      type = @type_mapper.llvm_type(inst.type)

      # Handle string constants specially
      if v = inst.value.as?(String)
        global_name = get_or_create_string_global(v)
        # String constant is a pointer to the global
        @constant_values[inst.id] = global_name
        emit "#{name} = bitcast ptr #{global_name} to ptr"
        return
      end

      value = case v = inst.value
              when Int64   then v.to_s
              when UInt64  then v.to_s
              when Float64 then v.to_s
              when Bool    then v ? "1" : "0"
              when Nil     then "null"
              else              "0"
              end
      # Store constant for inlining at use sites
      @constant_values[inst.id] = value
      # Generate real instruction so phi nodes can reference it
      # Using add 0, X is a common LLVM idiom for materializing constants
      if type == "void" || value == "null"
        emit "; #{name} = #{type} #{value}"
      else
        emit "#{name} = add #{type} 0, #{value}"
      end
    end

    private def get_or_create_string_global(str : String) : String
      if existing = @string_constants[str]?
        return existing
      end

      global_name = "@.str.#{@string_counter}"
      @string_counter += 1
      @string_constants[str] = global_name
      global_name
    end

    private def emit_alloc(inst : Alloc, name : String)
      case inst.strategy
      when MemoryStrategy::Stack
        # Use llvm_alloca_type to get actual struct type (not ptr)
        type = @type_mapper.llvm_alloca_type(inst.alloc_type)
        emit "#{name} = alloca #{type}, align #{inst.align}"
      when MemoryStrategy::Slab
        size_class = compute_size_class(inst.size)
        emit "#{name} = call ptr @__crystal_v2_slab_alloc(i32 #{size_class})"
      when MemoryStrategy::ARC, MemoryStrategy::AtomicARC
        # ARC: allocate extra 8 bytes for RC, initialize to 1
        total_size = inst.size + 8
        emit "%raw#{inst.id} = call ptr @__crystal_v2_malloc64(i64 #{total_size})"
        emit "store i64 1, ptr %raw#{inst.id}, align 8"
        emit "#{name} = getelementptr i8, ptr %raw#{inst.id}, i64 8"
      when MemoryStrategy::GC
        emit "#{name} = call ptr @__crystal_v2_malloc64(i64 #{inst.size})"
      end
    end

    private def compute_size_class(size : UInt64) : Int32
      return 0 if size <= 16
      return 1 if size <= 32
      return 2 if size <= 64
      return 3 if size <= 128
      return 4 if size <= 256
      return 5 if size <= 512
      return 6 if size <= 1024
      7  # Large
    end

    private def emit_free(inst : Free)
      ptr = value_ref(inst.ptr)
      case inst.strategy
      when MemoryStrategy::Slab
        emit "call void @__crystal_slab_free(ptr #{ptr}, i32 0)"
      else
        emit "call void @free(ptr #{ptr})"
      end
    end

    private def emit_rc_inc(inst : RCIncrement)
      ptr = value_ref(inst.ptr)
      emit "call void @__crystal_v2_rc_inc(ptr #{ptr})"
    end

    private def emit_rc_dec(inst : RCDecrement)
      ptr = value_ref(inst.ptr)
      destructor = "null"  # TODO: function pointer for destructor
      emit "call void @__crystal_v2_rc_dec(ptr #{ptr}, ptr #{destructor})"
    end

    private def emit_load(inst : Load, name : String)
      type = @type_mapper.llvm_type(inst.type)
      ptr = value_ref(inst.ptr)
      emit "#{name} = load #{type}, ptr #{ptr}"
    end

    private def emit_store(inst : Store)
      ptr = value_ref(inst.ptr)
      val = value_ref(inst.value)
      # Look up the type of the value being stored
      val_type = @value_types[inst.value]? || TypeRef::POINTER
      val_type_str = @type_mapper.llvm_type(val_type)
      emit "store #{val_type_str} #{val}, ptr #{ptr}"
    end

    private def emit_gep(inst : GetElementPtr, name : String)
      base = value_ref(inst.base)

      # Check if base is a struct type (from registered types)
      base_value_type = @value_types[inst.base]?
      if base_value_type && (mir_type = @module.type_registry.get(base_value_type))
        if mir_type.kind.struct?
          # Struct GEP: use actual struct type and field index
          struct_type = "%#{@type_mapper.mangle_name(mir_type.name)}"
          # Convert byte offset to field index (assuming 4-byte fields for now)
          field_idx = inst.indices.first? || 0_u32
          field_index = field_idx // 4  # Simple heuristic for i32 fields
          emit "#{name} = getelementptr #{struct_type}, ptr #{base}, i32 0, i32 #{field_index}"
          return
        end
      end

      # Default: pointer arithmetic GEP
      type = @type_mapper.llvm_type(inst.type)
      indices = inst.indices.map { |i| "i32 #{i}" }.join(", ")
      emit "#{name} = getelementptr #{type}, ptr #{base}, #{indices}"
    end

    private def emit_binary_op(inst : BinaryOp, name : String)
      result_type = @type_mapper.llvm_type(inst.type)
      left = value_ref(inst.left)
      right = value_ref(inst.right)

      # For comparisons, use operand type; for others, use result type
      operand_type = @value_types[inst.left]? || TypeRef::INT32
      operand_type_str = @type_mapper.llvm_type(operand_type)

      is_signed = operand_type.id <= TypeRef::INT128.id

      op = case inst.op
           when .add? then "add"
           when .sub? then "sub"
           when .mul? then "mul"
           when .div? then is_signed ? "sdiv" : "udiv"
           when .rem? then is_signed ? "srem" : "urem"
           when .shl? then "shl"
           when .shr? then is_signed ? "ashr" : "lshr"
           when .and? then "and"
           when .or?  then "or"
           when .xor? then "xor"
           when .eq?  then "icmp eq"
           when .ne?  then "icmp ne"
           when .lt?  then is_signed ? "icmp slt" : "icmp ult"
           when .le?  then is_signed ? "icmp sle" : "icmp ule"
           when .gt?  then is_signed ? "icmp sgt" : "icmp ugt"
           when .ge?  then is_signed ? "icmp sge" : "icmp uge"
           else            "add"
           end

      if op.starts_with?("icmp")
        # Comparisons use operand type
        emit "#{name} = #{op} #{operand_type_str} #{left}, #{right}"
      else
        emit "#{name} = #{op} #{result_type} #{left}, #{right}"
      end
    end

    private def emit_unary_op(inst : UnaryOp, name : String)
      type = @type_mapper.llvm_type(inst.type)
      operand = value_ref(inst.operand)

      case inst.op
      when .neg?
        emit "#{name} = sub #{type} 0, #{operand}"
      when .not?
        emit "#{name} = xor i1 #{operand}, 1"
      when .bit_not?
        emit "#{name} = xor #{type} #{operand}, -1"
      end
    end

    private def emit_cast(inst : Cast, name : String)
      src_type = "ptr"  # Simplified; real impl needs source type
      dst_type = @type_mapper.llvm_type(inst.type)
      value = value_ref(inst.value)

      op = case inst.kind
           when .bitcast?   then "bitcast"
           when .trunc?     then "trunc"
           when .z_ext?     then "zext"
           when .s_ext?     then "sext"
           when .fp_to_si?  then "fptosi"
           when .fp_to_ui?  then "fptoui"
           when .si_to_fp?  then "sitofp"
           when .ui_to_fp?  then "uitofp"
           when .fp_trunc?  then "fptrunc"
           when .fp_ext?    then "fpext"
           when .ptr_to_int? then "ptrtoint"
           when .int_to_ptr? then "inttoptr"
           else                  "bitcast"
           end

      emit "#{name} = #{op} #{src_type} #{value} to #{dst_type}"
    end

    private def emit_phi(inst : Phi, name : String)
      type = @type_mapper.llvm_type(inst.type)
      incoming = inst.incoming.map { |(block, val)| "[#{value_ref(val)}, %#{@block_names[block]}]" }
      emit "#{name} = phi #{type} #{incoming.join(", ")}"
    end

    private def emit_select(inst : Select, name : String)
      type = @type_mapper.llvm_type(inst.type)
      cond = value_ref(inst.condition)
      then_val = value_ref(inst.then_value)
      else_val = value_ref(inst.else_value)
      emit "#{name} = select i1 #{cond}, #{type} #{then_val}, #{type} #{else_val}"
    end

    private def emit_call(inst : Call, name : String, func : Function)
      return_type = @type_mapper.llvm_type(inst.type)

      # Look up callee function for name and param types
      callee_func = @module.functions.find { |f| f.id == inst.callee }
      callee_name = if callee_func
                      @type_mapper.mangle_name(callee_func.name)
                    else
                      "func#{inst.callee}"
                    end

      # Format arguments with proper types
      args = if callee_func
               inst.args.map_with_index { |a, i|
                 param_type = callee_func.params[i]?.try(&.type) || TypeRef::POINTER
                 "#{@type_mapper.llvm_type(param_type)} #{value_ref(a)}"
               }.join(", ")
             else
               inst.args.map { |a| "ptr #{value_ref(a)}" }.join(", ")
             end

      if return_type == "void"
        emit "call void @#{callee_name}(#{args})"
      else
        emit "#{name} = call #{return_type} @#{callee_name}(#{args})"
      end
    end

    private def emit_indirect_call(inst : IndirectCall, name : String)
      return_type = @type_mapper.llvm_type(inst.type)
      callee = value_ref(inst.callee_ptr)
      args = inst.args.map { |a| "ptr #{value_ref(a)}" }.join(", ")
      if return_type == "void"
        emit "call void #{callee}(#{args})"
      else
        emit "#{name} = call #{return_type} #{callee}(#{args})"
      end
    end

    private def emit_extern_call(inst : ExternCall, name : String)
      return_type = @type_mapper.llvm_type(inst.type)

      # Format arguments based on function name
      args = if inst.extern_name.includes?("print_int64")
               inst.args.map { |a| "i64 #{value_ref(a)}" }.join(", ")
             elsif inst.extern_name.includes?("print_int32")
               inst.args.map { |a| "i32 #{value_ref(a)}" }.join(", ")
             else
               inst.args.map { |a| "ptr #{value_ref(a)}" }.join(", ")
             end

      if return_type == "void"
        emit "call void @#{inst.extern_name}(#{args})"
      else
        emit "#{name} = call #{return_type} @#{inst.extern_name}(#{args})"
      end
    end

    private def emit_global_load(inst : GlobalLoad, name : String)
      llvm_type = @type_mapper.llvm_type(inst.type)
      emit "#{name} = load #{llvm_type}, ptr @#{inst.global_name}"
    end

    private def emit_global_store(inst : GlobalStore, name : String)
      # Get value type from the stored value
      val = value_ref(inst.value)
      llvm_type = @type_mapper.llvm_type(inst.type)
      emit "store #{llvm_type} #{val}, ptr @#{inst.global_name}"
    end

    # ═══════════════════════════════════════════════════════════════════════
    # UNION OPERATIONS
    # ═══════════════════════════════════════════════════════════════════════

    private def emit_union_wrap(inst : UnionWrap, name : String)
      # Union layout: { i32 type_id, [N x i8] payload }
      # name already has % prefix, use base name for temps
      base_name = name.lstrip('%')

      # 1. Allocate union on stack
      union_type = @type_mapper.llvm_type(inst.union_type)
      emit "%#{base_name}.ptr = alloca #{union_type}, align 8"

      # 2. Store type_id discriminator
      emit "%#{base_name}.type_id_ptr = getelementptr #{union_type}, ptr %#{base_name}.ptr, i32 0, i32 0"
      emit "store i32 #{inst.variant_type_id}, ptr %#{base_name}.type_id_ptr"

      # 3. Store value in payload
      val = value_ref(inst.value)
      val_type = @value_types[inst.value]? || TypeRef::POINTER
      val_type_str = @type_mapper.llvm_type(val_type)
      emit "%#{base_name}.payload_ptr = getelementptr #{union_type}, ptr %#{base_name}.ptr, i32 0, i32 1"
      emit "store #{val_type_str} #{val}, ptr %#{base_name}.payload_ptr"

      # 4. Load the completed union value from stack
      emit "#{name} = load #{union_type}, ptr %#{base_name}.ptr"
    end

    private def emit_union_unwrap(inst : UnionUnwrap, name : String)
      # Get payload from union, assuming type_id matches
      # Union may be passed by value - need to store to stack first
      union_val = value_ref(inst.union_value)
      union_type_ref = @value_types[inst.union_value]? || TypeRef::POINTER
      union_type = @type_mapper.llvm_type(union_type_ref)
      result_type = @type_mapper.llvm_type(inst.type)
      base_name = name.lstrip('%')

      # Store union value to stack to get pointer for GEP
      emit "%#{base_name}.union_ptr = alloca #{union_type}, align 8"
      emit "store #{union_type} #{union_val}, ptr %#{base_name}.union_ptr"

      if inst.safe
        # Safe unwrap: check type_id first, return null/zero on mismatch
        emit "%#{base_name}.type_id_ptr = getelementptr #{union_type}, ptr %#{base_name}.union_ptr, i32 0, i32 0"
        emit "%#{base_name}.actual_type_id = load i32, ptr %#{base_name}.type_id_ptr"
        emit "%#{base_name}.type_match = icmp eq i32 %#{base_name}.actual_type_id, #{inst.variant_type_id}"
        emit "%#{base_name}.payload_ptr = getelementptr #{union_type}, ptr %#{base_name}.union_ptr, i32 0, i32 1"
        emit "%#{base_name}.payload = load #{result_type}, ptr %#{base_name}.payload_ptr"
        # Select null/zero if type doesn't match
        emit "#{name} = select i1 %#{base_name}.type_match, #{result_type} %#{base_name}.payload, #{result_type} zeroinitializer"
      else
        # Unsafe unwrap: just load payload (UB if type_id doesn't match)
        emit "%#{base_name}.payload_ptr = getelementptr #{union_type}, ptr %#{base_name}.union_ptr, i32 0, i32 1"
        emit "#{name} = load #{result_type}, ptr %#{base_name}.payload_ptr"
      end
    end

    private def emit_union_type_id_get(inst : UnionTypeIdGet, name : String)
      # Load type_id from union
      # Union may be passed by value - need to store to stack first
      union_val = value_ref(inst.union_value)
      union_type_ref = @value_types[inst.union_value]? || TypeRef::POINTER
      union_type = @type_mapper.llvm_type(union_type_ref)
      base_name = name.lstrip('%')

      # Store union value to stack to get pointer for GEP
      emit "%#{base_name}.union_ptr = alloca #{union_type}, align 8"
      emit "store #{union_type} #{union_val}, ptr %#{base_name}.union_ptr"

      emit "%#{base_name}.type_id_ptr = getelementptr #{union_type}, ptr %#{base_name}.union_ptr, i32 0, i32 0"
      emit "#{name} = load i32, ptr %#{base_name}.type_id_ptr"
    end

    private def emit_union_is(inst : UnionIs, name : String)
      # Check if union is specific variant
      # Union may be passed by value (from load) - need to store to stack first
      union_val = value_ref(inst.union_value)
      union_type_ref = @value_types[inst.union_value]? || TypeRef::POINTER
      union_type = @type_mapper.llvm_type(union_type_ref)
      base_name = name.lstrip('%')

      # Store union value to stack to get pointer for GEP
      emit "%#{base_name}.union_ptr = alloca #{union_type}, align 8"
      emit "store #{union_type} #{union_val}, ptr %#{base_name}.union_ptr"

      # Get type_id from stored union
      emit "%#{base_name}.type_id_ptr = getelementptr #{union_type}, ptr %#{base_name}.union_ptr, i32 0, i32 0"
      emit "%#{base_name}.actual_type_id = load i32, ptr %#{base_name}.type_id_ptr"
      emit "#{name} = icmp eq i32 %#{base_name}.actual_type_id, #{inst.variant_type_id}"
    end

    # ─────────────────────────────────────────────────────────────────────────
    # Array Operations
    # ─────────────────────────────────────────────────────────────────────────

    private def emit_array_literal(inst : ArrayLiteral, name : String)
      base_name = name.lstrip('%')
      element_type = @type_mapper.llvm_type(inst.element_type)
      size = inst.size

      # Array struct type: { i32 size, [N x T] data }
      array_type = "{ i32, [#{size} x #{element_type}] }"

      # Allocate array struct on stack
      emit "%#{base_name}.ptr = alloca #{array_type}, align 8"

      # Store size
      emit "%#{base_name}.size_ptr = getelementptr #{array_type}, ptr %#{base_name}.ptr, i32 0, i32 0"
      emit "store i32 #{size}, ptr %#{base_name}.size_ptr"

      # Store elements
      inst.elements.each_with_index do |elem_id, idx|
        elem_val = value_ref(elem_id)
        emit "%#{base_name}.elem#{idx}_ptr = getelementptr #{array_type}, ptr %#{base_name}.ptr, i32 0, i32 1, i32 #{idx}"
        emit "store #{element_type} #{elem_val}, ptr %#{base_name}.elem#{idx}_ptr"
      end

      # Return pointer to array struct
      emit "#{name} = bitcast ptr %#{base_name}.ptr to ptr"

      # Remember array info for later use
      @array_info[inst.id] = {element_type, size}
    end

    private def emit_array_size(inst : ArraySize, name : String)
      base_name = name.lstrip('%')
      array_ptr = value_ref(inst.array_value)

      # Get size from array struct (first field)
      emit "%#{base_name}.size_ptr = getelementptr { i32, [0 x i32] }, ptr #{array_ptr}, i32 0, i32 0"
      emit "#{name} = load i32, ptr %#{base_name}.size_ptr"
    end

    private def emit_array_get(inst : ArrayGet, name : String)
      base_name = name.lstrip('%')
      array_ptr = value_ref(inst.array_value)
      index = value_ref(inst.index_value)
      element_type = @type_mapper.llvm_type(inst.element_type)

      # Get element from data array (second field)
      emit "%#{base_name}.elem_ptr = getelementptr { i32, [0 x #{element_type}] }, ptr #{array_ptr}, i32 0, i32 1, i32 #{index}"
      emit "#{name} = load #{element_type}, ptr %#{base_name}.elem_ptr"
    end

    private def emit_terminator(term : Terminator)
      case term
      when Return
        if @current_return_type == "void"
          emit "ret void"
        elsif val = term.value
          emit "ret #{@current_return_type} #{value_ref(val)}"
        else
          emit "ret void"
        end
      when Jump
        emit "br label %#{@block_names[term.target]}"
      when Branch
        cond = value_ref(term.condition)
        then_block = @block_names[term.then_block]
        else_block = @block_names[term.else_block]
        emit "br i1 #{cond}, label %#{then_block}, label %#{else_block}"
      when Switch
        val = value_ref(term.value)
        default = @block_names[term.default_block]
        emit "switch i64 #{val}, label %#{default} ["
        @indent += 1
        term.cases.each do |(case_val, block)|
          emit "i64 #{case_val}, label %#{@block_names[block]}"
        end
        @indent -= 1
        emit "]"
      when Unreachable
        emit "unreachable"
      end
    end

    private def value_ref(id : ValueId) : String
      # Check if it's a constant (inline the value)
      if const_val = @constant_values[id]?
        return const_val
      end
      # Otherwise reference by name
      if name = @value_names[id]?
        "%#{name}"
      else
        "%r#{id}"
      end
    end

    # ═══════════════════════════════════════════════════════════════════════
    # TYPE METADATA GENERATION (for Debug DX)
    # ═══════════════════════════════════════════════════════════════════════

    private def collect_type_metadata
      @module.type_registry.types.each do |type|
        name_offset = add_string(type.name)

        flags = 0_u32
        flags |= TypeInfoEntry::FLAG_STRUCT if type.is_value_type?
        flags |= TypeInfoEntry::FLAG_CLASS if type.kind.reference?
        flags |= TypeInfoEntry::FLAG_UNION if type.kind.union?
        flags |= TypeInfoEntry::FLAG_CLOSURE if type.is_closure
        flags |= TypeInfoEntry::FLAG_PRIMITIVE if type.kind.primitive?

        parent_type_id = type.parent_type_id || 0xFFFFFFFF_u32
        fields_offset = @field_info_entries.size.to_u32
        fields_count = 0_u32

        if fields = type.fields
          fields_count = fields.size.to_u32
          fields.each do |field|
            field_name_offset = add_string(field.name)
            field_flags = 0_u32
            field_flags |= FieldInfoEntry::FLAG_NILABLE if field.nilable?
            field_flags |= FieldInfoEntry::FLAG_CAPTURED if field.captured?

            @field_info_entries << FieldInfoEntry.new(
              field_name_offset,
              field.type_ref.id,
              field.offset,
              field_flags
            )
          end
        end

        @type_info_entries << TypeInfoEntry.new(
          type.id,
          flags,
          name_offset,
          type.size.to_u32,
          type.alignment,
          parent_type_id,
          fields_count,
          fields_offset
        )
      end
    end

    private def add_string(s : String) : UInt32
      if offset = @string_offsets[s]?
        return offset
      end
      offset = @string_table.pos.to_u32
      @string_table << s
      @string_table.write_byte(0_u8)
      @string_offsets[s] = offset
      offset
    end

    private def emit_type_metadata_globals
      emit_raw "; Type metadata for debug DX (LLDB Python + DAP)\n"

      # __crystal_type_count
      emit_raw "@__crystal_type_count = constant i32 #{@type_info_entries.size}\n"

      # __crystal_type_info array
      if @type_info_entries.empty?
        emit_raw "@__crystal_type_info = constant [0 x %__crystal_type_info_entry] []\n"
      else
        emit_raw "%__crystal_type_info_entry = type { i32, i32, i32, i32, i32, i32, i32, i32 }\n"
        emit_raw "@__crystal_type_info = constant [#{@type_info_entries.size} x %__crystal_type_info_entry] [\n"
        @type_info_entries.each_with_index do |entry, idx|
          comma = idx < @type_info_entries.size - 1 ? "," : ""
          emit_raw "  %__crystal_type_info_entry { "
          emit_raw "i32 #{entry.type_id}, "
          emit_raw "i32 #{entry.flags}, "
          emit_raw "i32 #{entry.name_offset}, "
          emit_raw "i32 #{entry.size}, "
          emit_raw "i32 #{entry.alignment}, "
          emit_raw "i32 #{entry.parent_type_id}, "
          emit_raw "i32 #{entry.fields_count}, "
          emit_raw "i32 #{entry.fields_offset}"
          emit_raw " }#{comma}\n"
        end
        emit_raw "]\n"
      end

      # __crystal_type_strings
      string_bytes = @string_table.to_slice
      emit_raw "@__crystal_type_strings = constant [#{string_bytes.size} x i8] ["
      string_bytes.each_with_index do |b, idx|
        comma = idx < string_bytes.size - 1 ? ", " : ""
        emit_raw "i8 #{b}#{comma}"
      end
      emit_raw "]\n"

      # __crystal_field_info array
      unless @field_info_entries.empty?
        emit_raw "%__crystal_field_info_entry = type { i32, i32, i32, i32 }\n"
        emit_raw "@__crystal_field_info = constant [#{@field_info_entries.size} x %__crystal_field_info_entry] [\n"
        @field_info_entries.each_with_index do |entry, idx|
          comma = idx < @field_info_entries.size - 1 ? "," : ""
          emit_raw "  %__crystal_field_info_entry { "
          emit_raw "i32 #{entry.name_offset}, "
          emit_raw "i32 #{entry.type_id}, "
          emit_raw "i32 #{entry.offset}, "
          emit_raw "i32 #{entry.flags}"
          emit_raw " }#{comma}\n"
        end
        emit_raw "]\n"
      end

      emit_raw "\n"
    end

    # ═══════════════════════════════════════════════════════════════════════
    # UNION METADATA GENERATION (for enhanced debug DX)
    # ═══════════════════════════════════════════════════════════════════════

    private def collect_union_metadata
      @module.union_descriptors.each do |type_ref, descriptor|
        name_offset = add_string(descriptor.name)
        variants_offset = @union_variant_entries.size.to_u32

        # Add variant entries
        descriptor.variants.each do |variant|
          variant_name_offset = add_string(variant.full_name)
          @union_variant_entries << UnionVariantInfoEntry.new(
            type_ref.id,
            variant.type_id.to_u32,
            variant.type_ref.id,
            variant_name_offset,
            variant.size.to_u32,
            variant.alignment.to_u32
          )
        end

        @union_info_entries << UnionInfoEntry.new(
          type_ref.id,
          name_offset,
          descriptor.variants.size.to_u32,
          variants_offset,
          descriptor.total_size.to_u32,
          descriptor.alignment.to_u32,
          descriptor.payload_offset.to_u32
        )
      end
    end

    private def emit_union_metadata_globals
      return if @union_info_entries.empty?

      emit_raw "; Union metadata for enhanced debug DX\n"

      # __crystal_union_count
      emit_raw "@__crystal_union_count = constant i32 #{@union_info_entries.size}\n"

      # __crystal_union_info array
      emit_raw "%__crystal_union_info_entry = type { i32, i32, i32, i32, i32, i32, i32 }\n"
      emit_raw "@__crystal_union_info = constant [#{@union_info_entries.size} x %__crystal_union_info_entry] [\n"
      @union_info_entries.each_with_index do |entry, idx|
        comma = idx < @union_info_entries.size - 1 ? "," : ""
        emit_raw "  %__crystal_union_info_entry { "
        emit_raw "i32 #{entry.type_id}, "
        emit_raw "i32 #{entry.name_offset}, "
        emit_raw "i32 #{entry.variant_count}, "
        emit_raw "i32 #{entry.variants_offset}, "
        emit_raw "i32 #{entry.total_size}, "
        emit_raw "i32 #{entry.alignment}, "
        emit_raw "i32 #{entry.payload_offset}"
        emit_raw " }#{comma}\n"
      end
      emit_raw "]\n"

      # __crystal_union_variant_info array
      unless @union_variant_entries.empty?
        emit_raw "%__crystal_union_variant_entry = type { i32, i32, i32, i32, i32, i32 }\n"
        emit_raw "@__crystal_union_variant_info = constant [#{@union_variant_entries.size} x %__crystal_union_variant_entry] [\n"
        @union_variant_entries.each_with_index do |entry, idx|
          comma = idx < @union_variant_entries.size - 1 ? "," : ""
          emit_raw "  %__crystal_union_variant_entry { "
          emit_raw "i32 #{entry.union_type_id}, "
          emit_raw "i32 #{entry.variant_type_id}, "
          emit_raw "i32 #{entry.variant_type_ref}, "
          emit_raw "i32 #{entry.name_offset}, "
          emit_raw "i32 #{entry.size}, "
          emit_raw "i32 #{entry.alignment}"
          emit_raw " }#{comma}\n"
        end
        emit_raw "]\n"
      end

      emit_raw "\n"
    end
  end
end
