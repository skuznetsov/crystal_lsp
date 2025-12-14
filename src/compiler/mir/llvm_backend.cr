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
      when .struct?                     then "ptr"  # Structs passed by pointer in our ABI
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
    @current_func_name : String = ""
    @tsan_needs_func_entry : Bool = false
    @constant_values : Hash(ValueId, String)  # For inlining constants
    @value_types : Hash(ValueId, TypeRef)     # For tracking operand types
    @alloc_element_types : Hash(ValueId, TypeRef)  # For GEP element type lookup
    @array_info : Hash(ValueId, {String, Int32})  # Array element_type and size
    @string_constants : Hash(String, String)  # String value -> global name
    @string_counter : Int32 = 0
    @cond_counter : Int32 = 0  # For unique branch condition variable names

    # Type metadata for debug DX
    @type_info_entries : Array(TypeInfoEntry)
    @field_info_entries : Array(FieldInfoEntry)
    @union_info_entries : Array(UnionInfoEntry)
    @union_variant_entries : Array(UnionVariantInfoEntry)
    @string_table : IO::Memory
    @string_offsets : Hash(String, UInt32)

    property emit_debug_info : Bool = true
    property emit_type_metadata : Bool = true
    property emit_tsan : Bool = false  # Thread Sanitizer instrumentation
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
      @alloc_element_types = {} of ValueId => TypeRef
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
        mangled_name = @type_mapper.mangle_name(global.name)
        emit_raw "@#{mangled_name} = global #{llvm_type} #{initial}\n"
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

      # Track emitted types to avoid duplicates
      emitted_types = Set(String).new

      # Emit struct types from registry
      @module.type_registry.types.each do |type|
        type_name = @type_mapper.mangle_name(type.name)
        next if emitted_types.includes?(type_name)

        if (type.kind.struct? || type.kind.reference?) && !type.kind.primitive?
          emit_struct_type(type)
          emitted_types << type_name
        elsif type.kind.union?
          emit_union_type(type)
          emitted_types << "#{type_name}.union"
        end
      end

      emit_raw "\n"
    end

    private def emit_struct_type(type : Type)
      name = @type_mapper.mangle_name(type.name)
      fields = type.fields

      if fields && !fields.empty?
        field_types = [] of String

        # Add vtable ptr as first field for class types (not value types)
        if type.kind.reference?
          field_types << "ptr"  # vtable pointer for classes
        end

        fields.each do |field|
          field_types << @type_mapper.llvm_type(field.type_ref)
        end

        emit_raw "%#{name} = type { #{field_types.join(", ")} }\n"
      else
        # For types without fields, still emit proper layout for classes
        if type.kind.reference?
          emit_raw "%#{name} = type { ptr }\n"  # just vtable
        else
          emit_raw "%#{name} = type {}\n"
        end
      end
    end

    private def emit_union_type(type : Type)
      name = @type_mapper.mangle_name(type.name)
      # Union = { i32 discriminator, [max_size x i8] data }
      max_size = type.variants.try(&.map(&.size).max) || 8_u64
      emit_raw "%#{name}.union = type { i32, [#{max_size} x i8] }\n"
    end

    private def emit_runtime_declarations
      # External C library functions
      emit_raw "declare ptr @malloc(i64)\n"
      emit_raw "declare ptr @calloc(i64, i64)\n"
      emit_raw "declare ptr @realloc(ptr, i64)\n"
      emit_raw "declare void @free(ptr)\n"
      emit_raw "declare i32 @printf(ptr, ...)\n"
      emit_raw "declare i32 @puts(ptr)\n"
      emit_raw "declare void @exit(i32)\n"
      emit_raw "\n"

      # Format strings for printing
      emit_raw "@.int_fmt = private constant [4 x i8] c\"%d\\0A\\00\"\n"
      emit_raw "@.int_fmt_no_nl = private constant [3 x i8] c\"%d\\00\"\n"
      emit_raw "@.long_fmt = private constant [5 x i8] c\"%ld\\0A\\00\"\n"
      emit_raw "@.long_fmt_no_nl = private constant [4 x i8] c\"%ld\\00\"\n"
      emit_raw "@.float_fmt_no_nl = private constant [3 x i8] c\"%g\\00\"\n"
      emit_raw "@.str.true = private constant [5 x i8] c\"true\\00\"\n"
      emit_raw "@.str.false = private constant [6 x i8] c\"false\\00\"\n"
      emit_raw "\n"

      # Memory allocation - just wrap malloc
      emit_raw "define ptr @__crystal_v2_malloc64(i64 %size) {\n"
      emit_raw "  %ptr = call ptr @malloc(i64 %size)\n"
      emit_raw "  ret ptr %ptr\n"
      emit_raw "}\n\n"

      emit_raw "define ptr @__crystal_malloc_atomic64(i64 %size) {\n"
      emit_raw "  %ptr = call ptr @malloc(i64 %size)\n"
      emit_raw "  ret ptr %ptr\n"
      emit_raw "}\n\n"

      emit_raw "define ptr @__crystal_realloc64(ptr %ptr, i64 %size) {\n"
      emit_raw "  %new_ptr = call ptr @realloc(ptr %ptr, i64 %size)\n"
      emit_raw "  ret ptr %new_ptr\n"
      emit_raw "}\n\n"

      # ARC runtime - stubs (no-op for bootstrap)
      emit_raw "define void @__crystal_v2_rc_inc(ptr %ptr) {\n"
      emit_raw "  ret void\n"
      emit_raw "}\n\n"

      emit_raw "define void @__crystal_v2_rc_dec(ptr %ptr, ptr %destructor) {\n"
      emit_raw "  ret void\n"
      emit_raw "}\n\n"

      emit_raw "define void @__crystal_v2_rc_inc_atomic(ptr %ptr) {\n"
      emit_raw "  ret void\n"
      emit_raw "}\n\n"

      emit_raw "define void @__crystal_v2_rc_dec_atomic(ptr %ptr, ptr %destructor) {\n"
      emit_raw "  ret void\n"
      emit_raw "}\n\n"

      # Slab allocator - just use malloc for bootstrap
      emit_raw "define ptr @__crystal_v2_slab_alloc(i32 %size_class) {\n"
      emit_raw "  %size = sext i32 %size_class to i64\n"
      emit_raw "  %shift = shl i64 1, %size\n"
      emit_raw "  %ptr = call ptr @malloc(i64 %shift)\n"
      emit_raw "  ret ptr %ptr\n"
      emit_raw "}\n\n"

      emit_raw "define void @__crystal_v2_slab_free(ptr %ptr, i32 %size_class) {\n"
      emit_raw "  call void @free(ptr %ptr)\n"
      emit_raw "  ret void\n"
      emit_raw "}\n\n"

      # IO functions - use printf
      emit_raw "define void @__crystal_v2_puts(ptr %str) {\n"
      emit_raw "  call i32 @puts(ptr %str)\n"
      emit_raw "  ret void\n"
      emit_raw "}\n\n"

      emit_raw "define void @__crystal_v2_print_int32(i32 %val) {\n"
      emit_raw "  call i32 (ptr, ...) @printf(ptr @.int_fmt_no_nl, i32 %val)\n"
      emit_raw "  ret void\n"
      emit_raw "}\n\n"

      emit_raw "define void @__crystal_v2_print_int32_ln(i32 %val) {\n"
      emit_raw "  call i32 (ptr, ...) @printf(ptr @.int_fmt, i32 %val)\n"
      emit_raw "  ret void\n"
      emit_raw "}\n\n"

      emit_raw "define void @__crystal_v2_print_int64(i64 %val) {\n"
      emit_raw "  call i32 (ptr, ...) @printf(ptr @.long_fmt_no_nl, i64 %val)\n"
      emit_raw "  ret void\n"
      emit_raw "}\n\n"

      emit_raw "define void @__crystal_v2_print_int64_ln(i64 %val) {\n"
      emit_raw "  call i32 (ptr, ...) @printf(ptr @.long_fmt, i64 %val)\n"
      emit_raw "  ret void\n"
      emit_raw "}\n\n"

      # String functions - implemented using C library
      emit_raw "declare i64 @strlen(ptr)\n"
      emit_raw "declare ptr @strcpy(ptr, ptr)\n"
      emit_raw "declare ptr @strcat(ptr, ptr)\n"
      emit_raw "declare i32 @sprintf(ptr, ptr, ...)\n"
      emit_raw "\n"

      # int_to_string: allocate buffer and sprintf
      emit_raw "define ptr @__crystal_v2_int_to_string(i32 %val) {\n"
      emit_raw "  %buf = call ptr @malloc(i64 16)\n"
      emit_raw "  call i32 (ptr, ptr, ...) @sprintf(ptr %buf, ptr @.int_fmt_no_nl, i32 %val)\n"
      emit_raw "  ret ptr %buf\n"
      emit_raw "}\n\n"

      emit_raw "define ptr @__crystal_v2_int64_to_string(i64 %val) {\n"
      emit_raw "  %buf = call ptr @malloc(i64 24)\n"
      emit_raw "  call i32 (ptr, ptr, ...) @sprintf(ptr %buf, ptr @.long_fmt_no_nl, i64 %val)\n"
      emit_raw "  ret ptr %buf\n"
      emit_raw "}\n\n"

      # int64 to i32 (truncate)
      emit_raw "define i32 @__crystal_v2_int64_to_i32(i64 %val) {\n"
      emit_raw "  %trunc = trunc i64 %val to i32\n"
      emit_raw "  ret i32 %trunc\n"
      emit_raw "}\n\n"

      # int32 absolute value
      emit_raw "define i32 @__crystal_v2_int_abs(i32 %val) {\n"
      emit_raw "entry:\n"
      emit_raw "  %neg = icmp slt i32 %val, 0\n"
      emit_raw "  br i1 %neg, label %do_neg, label %done\n"
      emit_raw "do_neg:\n"
      emit_raw "  %negated = sub i32 0, %val\n"
      emit_raw "  br label %done\n"
      emit_raw "done:\n"
      emit_raw "  %result = phi i32 [ %negated, %do_neg ], [ %val, %entry ]\n"
      emit_raw "  ret i32 %result\n"
      emit_raw "}\n\n"

      # int32 to int64 (sign extend)
      emit_raw "define i64 @__crystal_v2_int_to_i64(i32 %val) {\n"
      emit_raw "  %ext = sext i32 %val to i64\n"
      emit_raw "  ret i64 %ext\n"
      emit_raw "}\n\n"

      # int32 to float64
      emit_raw "define double @__crystal_v2_int_to_f64(i32 %val) {\n"
      emit_raw "  %conv = sitofp i32 %val to double\n"
      emit_raw "  ret double %conv\n"
      emit_raw "}\n\n"

      # float64 to string
      emit_raw "define ptr @__crystal_v2_f64_to_string(double %val) {\n"
      emit_raw "  %buf = call ptr @malloc(i64 32)\n"
      emit_raw "  call i32 (ptr, ptr, ...) @sprintf(ptr %buf, ptr @.float_fmt_no_nl, double %val)\n"
      emit_raw "  ret ptr %buf\n"
      emit_raw "}\n\n"

      # float64 to int32
      emit_raw "define i32 @__crystal_v2_f64_to_i32(double %val) {\n"
      emit_raw "  %conv = fptosi double %val to i32\n"
      emit_raw "  ret i32 %conv\n"
      emit_raw "}\n\n"

      # float64 to int64
      emit_raw "define i64 @__crystal_v2_f64_to_i64(double %val) {\n"
      emit_raw "  %conv = fptosi double %val to i64\n"
      emit_raw "  ret i64 %conv\n"
      emit_raw "}\n\n"

      # bool to string
      emit_raw "define ptr @__crystal_v2_bool_to_string(i1 %val) {\n"
      emit_raw "entry:\n"
      emit_raw "  br i1 %val, label %is_true, label %is_false\n"
      emit_raw "is_true:\n"
      emit_raw "  ret ptr @.str.true\n"
      emit_raw "is_false:\n"
      emit_raw "  ret ptr @.str.false\n"
      emit_raw "}\n\n"

      # string_concat: allocate new buffer and concatenate
      emit_raw "define ptr @__crystal_v2_string_concat(ptr %a, ptr %b) {\n"
      emit_raw "entry:\n"
      emit_raw "  %a_null = icmp eq ptr %a, null\n"
      emit_raw "  br i1 %a_null, label %a_is_null, label %a_not_null\n"
      emit_raw "a_is_null:\n"
      emit_raw "  %b_null_1 = icmp eq ptr %b, null\n"
      emit_raw "  br i1 %b_null_1, label %both_null, label %ret_b\n"
      emit_raw "both_null:\n"
      emit_raw "  ret ptr null\n"
      emit_raw "ret_b:\n"
      emit_raw "  ret ptr %b\n"
      emit_raw "a_not_null:\n"
      emit_raw "  %b_null_2 = icmp eq ptr %b, null\n"
      emit_raw "  br i1 %b_null_2, label %ret_a, label %do_concat\n"
      emit_raw "ret_a:\n"
      emit_raw "  ret ptr %a\n"
      emit_raw "do_concat:\n"
      emit_raw "  %len_a = call i64 @strlen(ptr %a)\n"
      emit_raw "  %len_b = call i64 @strlen(ptr %b)\n"
      emit_raw "  %total = add i64 %len_a, %len_b\n"
      emit_raw "  %total_plus_1 = add i64 %total, 1\n"
      emit_raw "  %buf = call ptr @malloc(i64 %total_plus_1)\n"
      emit_raw "  call ptr @strcpy(ptr %buf, ptr %a)\n"
      emit_raw "  call ptr @strcat(ptr %buf, ptr %b)\n"
      emit_raw "  ret ptr %buf\n"
      emit_raw "}\n\n"

      # Initialize buffer with empty string (null terminator)
      emit_raw "define void @__crystal_v2_init_buffer(ptr %buf) {\n"
      emit_raw "  store i8 0, ptr %buf\n"
      emit_raw "  ret void\n"
      emit_raw "}\n\n"

      # IO_shovel: append string to buffer (for String.build)
      # Takes buffer ptr and string to append, returns buffer ptr
      emit_raw "define ptr @IO_shovel(ptr %buf, ptr %str) {\n"
      emit_raw "entry:\n"
      emit_raw "  %str_null = icmp eq ptr %str, null\n"
      emit_raw "  br i1 %str_null, label %done, label %do_append\n"
      emit_raw "do_append:\n"
      emit_raw "  call ptr @strcat(ptr %buf, ptr %str)\n"
      emit_raw "  br label %done\n"
      emit_raw "done:\n"
      emit_raw "  ret ptr %buf\n"
      emit_raw "}\n\n"

      # IO_shovel for integers - convert to string and append
      emit_raw "define ptr @IO_shovel_int(ptr %buf, i32 %val) {\n"
      emit_raw "  %str = call ptr @__crystal_v2_int_to_string(i32 %val)\n"
      emit_raw "  call ptr @strcat(ptr %buf, ptr %str)\n"
      emit_raw "  ret ptr %buf\n"
      emit_raw "}\n\n"

      # String repeat: "str" * n -> repeat string n times
      emit_raw "define ptr @__crystal_v2_string_repeat(ptr %str, i32 %count) {\n"
      emit_raw "entry:\n"
      emit_raw "  %cmp = icmp sle i32 %count, 0\n"
      emit_raw "  br i1 %cmp, label %empty, label %repeat\n"
      emit_raw "empty:\n"
      emit_raw "  %empty_str = call ptr @malloc(i64 1)\n"
      emit_raw "  store i8 0, ptr %empty_str\n"
      emit_raw "  ret ptr %empty_str\n"
      emit_raw "repeat:\n"
      emit_raw "  %str_len = call i64 @strlen(ptr %str)\n"
      emit_raw "  %count64 = zext i32 %count to i64\n"
      emit_raw "  %total_len = mul i64 %str_len, %count64\n"
      emit_raw "  %buf_size = add i64 %total_len, 1\n"
      emit_raw "  %buf = call ptr @malloc(i64 %buf_size)\n"
      emit_raw "  store i8 0, ptr %buf\n"
      emit_raw "  br label %loop\n"
      emit_raw "loop:\n"
      emit_raw "  %i = phi i32 [0, %repeat], [%next_i, %loop]\n"
      emit_raw "  call ptr @strcat(ptr %buf, ptr %str)\n"
      emit_raw "  %next_i = add i32 %i, 1\n"
      emit_raw "  %done = icmp sge i32 %next_i, %count\n"
      emit_raw "  br i1 %done, label %exit, label %loop\n"
      emit_raw "exit:\n"
      emit_raw "  ret ptr %buf\n"
      emit_raw "}\n\n"

      # Thread Sanitizer (TSan) instrumentation
      if @emit_tsan
        emit_raw "; Thread Sanitizer runtime functions\n"
        emit_raw "declare void @__tsan_read1(ptr)\n"
        emit_raw "declare void @__tsan_read2(ptr)\n"
        emit_raw "declare void @__tsan_read4(ptr)\n"
        emit_raw "declare void @__tsan_read8(ptr)\n"
        emit_raw "declare void @__tsan_read16(ptr)\n"
        emit_raw "declare void @__tsan_write1(ptr)\n"
        emit_raw "declare void @__tsan_write2(ptr)\n"
        emit_raw "declare void @__tsan_write4(ptr)\n"
        emit_raw "declare void @__tsan_write8(ptr)\n"
        emit_raw "declare void @__tsan_write16(ptr)\n"
        emit_raw "declare void @__tsan_func_entry(ptr)\n"
        emit_raw "declare void @__tsan_func_exit()\n"
        emit_raw "declare void @__tsan_acquire(ptr)\n"
        emit_raw "declare void @__tsan_release(ptr)\n"
        emit_raw "\n"
      end

      # Synchronization primitives runtime - stubs for bootstrap
      emit_raw "; Synchronization runtime functions\n"
      emit_raw "define void @__crystal_v2_mutex_lock(ptr %mutex) {\n"
      emit_raw "  ret void\n"
      emit_raw "}\n\n"

      emit_raw "define void @__crystal_v2_mutex_unlock(ptr %mutex) {\n"
      emit_raw "  ret void\n"
      emit_raw "}\n\n"

      emit_raw "define i1 @__crystal_v2_mutex_trylock(ptr %mutex) {\n"
      emit_raw "  ret i1 true\n"
      emit_raw "}\n\n"

      emit_raw "define void @__crystal_v2_channel_send(ptr %chan, ptr %val) {\n"
      emit_raw "  ret void\n"
      emit_raw "}\n\n"

      emit_raw "define ptr @__crystal_v2_channel_receive(ptr %chan) {\n"
      emit_raw "  ret ptr null\n"
      emit_raw "}\n\n"

      emit_raw "define void @__crystal_v2_channel_close(ptr %chan) {\n"
      emit_raw "  ret void\n"
      emit_raw "}\n\n"
    end

    # Union debug helper function definitions - stubs for bootstrap
    private def emit_union_debug_helpers
      emit_raw "; Union debug helper functions\n"
      # Debug print: prints union value with type name
      emit_raw "define void @__crystal_v2_union_debug_print(ptr %union_ptr, ptr %descriptor) {\n"
      emit_raw "  ret void\n"
      emit_raw "}\n\n"

      # Type check with error: verifies type_id, traps on mismatch
      emit_raw "define void @__crystal_v2_union_type_check(i32 %expected, i32 %actual, ptr %msg) {\n"
      emit_raw "  ret void\n"
      emit_raw "}\n\n"

      # Get type name from union descriptor
      emit_raw "define ptr @__crystal_v2_union_type_name(i32 %type_id, ptr %descriptor) {\n"
      emit_raw "  ret ptr null\n"
      emit_raw "}\n\n"

      # Exception handling runtime functions using setjmp/longjmp
      emit_raw "; Exception handling runtime functions\n"
      emit_raw "declare void @abort()\n"
      emit_raw "\n"

      # jmp_buf is platform-specific; macOS ARM64 is 192 bytes = 24 i64s
      emit_raw "; jmp_buf type and setjmp/longjmp declarations\n"
      emit_raw "%jmp_buf = type [24 x i64]\n"
      emit_raw "declare i32 @setjmp(ptr) nounwind returns_twice\n"
      emit_raw "declare void @longjmp(ptr, i32) noreturn nounwind\n"
      emit_raw "\n"

      # Global exception state
      emit_raw "; Global exception handling state\n"
      emit_raw "@__crystal_exc_jmpbuf = global %jmp_buf zeroinitializer\n"
      emit_raw "@__crystal_exc_ptr = global ptr null\n"
      emit_raw "@__crystal_exc_handler_active = global i1 false\n"
      emit_raw "\n"

      # Note: __crystal_v2_try_begin is NOT a wrapper for setjmp because setjmp
      # cannot work correctly when called from a wrapper (stack frame issues).
      # Instead, callers should inline the setjmp call directly.
      # This is just a placeholder that sets up the handler flag.
      emit_raw "define void @__crystal_v2_try_begin_setup() {\n"
      emit_raw "  store i1 true, ptr @__crystal_exc_handler_active\n"
      emit_raw "  ret void\n"
      emit_raw "}\n\n"

      # End exception handler scope
      emit_raw "define void @__crystal_v2_try_end() {\n"
      emit_raw "  store i1 false, ptr @__crystal_exc_handler_active\n"
      emit_raw "  store ptr null, ptr @__crystal_exc_ptr\n"
      emit_raw "  ret void\n"
      emit_raw "}\n\n"

      emit_raw "define void @__crystal_v2_raise(ptr %exc) {\n"
      emit_raw "  store ptr %exc, ptr @__crystal_exc_ptr\n"
      emit_raw "  %active = load i1, ptr @__crystal_exc_handler_active\n"
      emit_raw "  br i1 %active, label %do_longjmp, label %no_handler\n"
      emit_raw "do_longjmp:\n"
      emit_raw "  call void @longjmp(ptr @__crystal_exc_jmpbuf, i32 1)\n"
      emit_raw "  unreachable\n"
      emit_raw "no_handler:\n"
      emit_raw "  call void @abort()\n"
      emit_raw "  unreachable\n"
      emit_raw "}\n\n"

      emit_raw "define void @__crystal_v2_raise_msg(ptr %msg) {\n"
      emit_raw "  ; For now, treat message as exception pointer\n"
      emit_raw "  store ptr %msg, ptr @__crystal_exc_ptr\n"
      emit_raw "  %active = load i1, ptr @__crystal_exc_handler_active\n"
      emit_raw "  br i1 %active, label %do_longjmp, label %no_handler\n"
      emit_raw "do_longjmp:\n"
      emit_raw "  call void @longjmp(ptr @__crystal_exc_jmpbuf, i32 1)\n"
      emit_raw "  unreachable\n"
      emit_raw "no_handler:\n"
      emit_raw "  call void @abort()\n"
      emit_raw "  unreachable\n"
      emit_raw "}\n\n"

      emit_raw "define void @__crystal_v2_reraise() {\n"
      emit_raw "  %exc = load ptr, ptr @__crystal_exc_ptr\n"
      emit_raw "  %active = load i1, ptr @__crystal_exc_handler_active\n"
      emit_raw "  br i1 %active, label %do_longjmp, label %no_handler\n"
      emit_raw "do_longjmp:\n"
      emit_raw "  call void @longjmp(ptr @__crystal_exc_jmpbuf, i32 1)\n"
      emit_raw "  unreachable\n"
      emit_raw "no_handler:\n"
      emit_raw "  call void @abort()\n"
      emit_raw "  unreachable\n"
      emit_raw "}\n\n"

      emit_raw "define ptr @__crystal_v2_get_exception() {\n"
      emit_raw "  %exc = load ptr, ptr @__crystal_exc_ptr\n"
      emit_raw "  ret ptr %exc\n"
      emit_raw "}\n\n"
    end

    private def emit_function(func : Function)
      reset_value_names(func)

      # Pre-pass: collect constant values for phi node resolution
      # This ensures forward-referenced constants are available
      prepass_collect_constants(func)

      # Function signature
      # Note: void is not valid for parameters, substitute with ptr
      param_types = func.params.map do |p|
        llvm_type = @type_mapper.llvm_type(p.type)
        llvm_type = "ptr" if llvm_type == "void"
        "#{llvm_type} %#{p.name}"
      end
      return_type = @type_mapper.llvm_type(func.return_type)
      @current_return_type = return_type  # Store for terminator emission
      @current_func_name = @type_mapper.mangle_name(func.name)

      mangled_name = @current_func_name
      emit_raw "define #{return_type} @#{mangled_name}(#{param_types.join(", ")}) {\n"

      # TSan: emit function entry in first block
      @tsan_needs_func_entry = @emit_tsan

      func.blocks.each do |block|
        emit_block(block, func)
      end

      emit_raw "}\n\n"
    end

    # Pre-pass to collect constant values AND all value types before emitting IR
    # This resolves forward reference issues with phi nodes
    private def prepass_collect_constants(func : Function)
      # First pass: collect constants and initial types
      func.blocks.each do |block|
        block.instructions.each do |inst|
          # Collect ALL value types upfront (critical for phi forward references)
          # Special handling for instructions that emit different types than their MIR type:
          effective_type = inst.type

          if inst.is_a?(Phi) && @type_mapper.llvm_type(inst.type) == "void"
            # Void phi nodes become ptr in LLVM
            effective_type = TypeRef::POINTER
          elsif inst.is_a?(ArrayGet)
            # ArrayGet emits load of element_type, not inst.type
            effective_type = inst.element_type
          elsif inst.is_a?(ArraySize)
            # ArraySize always emits i32 load
            effective_type = TypeRef::INT32
          end

          @value_types[inst.id] = effective_type

          if const_inst = inst.as?(Constant)
            type = @type_mapper.llvm_type(const_inst.type)
            value = case v = const_inst.value
                    when Int64   then v.to_s
                    when UInt64  then v.to_s
                    when Float64 then v.to_s
                    when Bool    then v ? "1" : "0"
                    when Nil     then "null"
                    when String  then nil  # String constants handled specially
                    else              "0"
                    end
            next unless value  # Skip string constants
            # For pointer types, use "null" instead of "0"
            value = "null" if type == "ptr" && value == "0"
            @constant_values[const_inst.id] = value
          end
        end
      end

      # Second pass: identify union phis that will be converted to ptr
      # This needs to iterate until no more changes (fixpoint)
      changed = true
      while changed
        changed = false
        func.blocks.each do |block|
          block.instructions.each do |inst|
            next unless inst.is_a?(Phi)
            phi_llvm_type = @type_mapper.llvm_type(@value_types[inst.id])
            next unless phi_llvm_type.includes?(".union")

            # Check if any incoming value is ptr type (including from other phis that were converted)
            has_ptr_incoming = inst.incoming.any? do |(block_id, val)|
              val_type = @value_types[val]?
              const_val = @constant_values[val]?
              val_llvm_type = val_type ? @type_mapper.llvm_type(val_type) : nil
              (val_llvm_type == "ptr") || (const_val == "null")
            end

            if has_ptr_incoming
              # Mark this phi as ptr type
              @value_types[inst.id] = TypeRef::POINTER
              changed = true
            end
          end
        end
      end
    end

    private def reset_value_names(func : Function)
      @value_names.clear
      @block_names.clear
      @constant_values.clear
      @value_types.clear
      @array_info.clear
      @cond_counter = 0  # Reset for each function

      func.params.each do |param|
        @value_names[param.index] = param.name
        # Use POINTER for void params (void is not valid for values)
        param_type = param.type == TypeRef::VOID ? TypeRef::POINTER : param.type
        @value_types[param.index] = param_type
      end

      func.blocks.each do |block|
        @block_names[block.id] = "bb#{block.id}"
      end
    end

    private def emit_block(block : BasicBlock, func : Function)
      emit_raw "#{@block_names[block.id]}:\n"
      @indent = 1

      # TSan: emit function entry at start of first block
      if @tsan_needs_func_entry
        emit "; TSan function entry"
        emit "%__tsan_func_ptr = bitcast ptr @#{@current_func_name} to ptr"
        emit "call void @__tsan_func_entry(ptr %__tsan_func_ptr)"
        @tsan_needs_func_entry = false
      end

      block.instructions.each do |inst|
        emit_instruction(inst, func)
      end

      emit_terminator(block.terminator)
      @indent = 0
    end

    private def emit_instruction(inst : Value, func : Function)
      name = "%r#{inst.id}"
      @value_names[inst.id] = "r#{inst.id}"
      # Preserve prepass type for phi nodes that were converted to ptr
      # For other instructions, set the MIR type (will be overwritten by emit_* if needed)
      unless inst.is_a?(Phi) && @value_types[inst.id]? == TypeRef::POINTER
        @value_types[inst.id] = inst.type
      end

      case inst
      when Constant
        emit_constant(inst, name)
      when Alloc
        # Alloc always returns a pointer, regardless of alloc_type
        # Store POINTER type for the result (not the element type)
        @value_types[inst.id] = TypeRef::POINTER
        # But also store the alloc_type for GEP element type lookups
        @alloc_element_types[inst.id] = inst.alloc_type
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
      when GetElementPtrDynamic
        emit_gep_dynamic(inst, name)
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
      when ArraySet
        emit_array_set(inst, name)
      when StringInterpolation
        emit_string_interpolation(inst, name)
      # Synchronization primitives
      when AtomicLoad
        emit_atomic_load(inst, name)
      when AtomicStore
        emit_atomic_store(inst, name)
      when AtomicCAS
        emit_atomic_cas(inst, name)
      when AtomicRMW
        emit_atomic_rmw(inst, name)
      when Fence
        emit_fence(inst, name)
      when MutexLock
        emit_mutex_lock(inst, name)
      when MutexUnlock
        emit_mutex_unlock(inst, name)
      when MutexTryLock
        emit_mutex_trylock(inst, name)
      when ChannelSend
        emit_channel_send(inst, name)
      when ChannelReceive
        emit_channel_receive(inst, name)
      when ChannelClose
        emit_channel_close(inst, name)
      when TryBegin
        emit_try_begin(inst, name)
      when TryEnd
        emit_try_end(inst, name)
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
        @value_types[inst.id] = TypeRef::POINTER
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

      # For pointer types, use "null" instead of "0"
      if type == "ptr" && value == "0"
        value = "null"
      end

      # Store constant for inlining at use sites
      @constant_values[inst.id] = value
      # Generate real instruction so phi nodes can reference it
      # Using add 0, X is a common LLVM idiom for materializing constants
      if type == "void" || value == "null" || type == "ptr"
        # void/null/ptr constants are treated as ptr type in LLVM
        # Must emit real instruction (not comment) so phi nodes can reference it
        emit "#{name} = inttoptr i64 0 to ptr"
        @value_types[inst.id] = TypeRef::POINTER
      elsif type.includes?(".union")
        # Union types can't use add instruction - create zeroinit union
        base_name = name.lstrip('%')
        emit "%#{base_name}.ptr = alloca #{type}, align 8"
        # Zero-initialize by setting type_id to 1 (nil variant)
        emit "%#{base_name}.type_id_ptr = getelementptr #{type}, ptr %#{base_name}.ptr, i32 0, i32 0"
        emit "store i32 1, ptr %#{base_name}.type_id_ptr"
        emit "#{name} = load #{type}, ptr %#{base_name}.ptr"
        @value_types[inst.id] = inst.type
      else
        emit "#{name} = add #{type} 0, #{value}"
        # Track actual constant type
        @value_types[inst.id] = inst.type
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
        # Guard against void type - use i8 for minimal allocation
        type = "i8" if type == "void"
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
      # Alloc always produces a pointer
      @value_types[inst.id] = TypeRef::POINTER
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
      if inst.atomic
        # Inline atomic increment for thread-safe RC
        # RC is stored at ptr - 8 (object layout: [RC:i64][data...])
        emit "%rc_ptr.#{inst.id} = getelementptr i8, ptr #{ptr}, i64 -8"
        # atomicrmw add with seq_cst ordering for full thread safety
        emit "%old_rc.#{inst.id} = atomicrmw add ptr %rc_ptr.#{inst.id}, i64 1 seq_cst"

        # TSan: release semantics - this thread "releases" the object
        # Other threads that later acquire this object will see our writes
        if @emit_tsan
          emit "call void @__tsan_release(ptr #{ptr})"
        end
      else
        # Non-atomic: simple load/add/store (faster for single-threaded)
        emit "call void @__crystal_v2_rc_inc(ptr #{ptr})"
      end
    end

    private def emit_rc_dec(inst : RCDecrement)
      ptr = value_ref(inst.ptr)
      if inst.atomic
        # TSan: acquire semantics before decrement
        # This thread "acquires" all writes from threads that released this object
        if @emit_tsan
          emit "call void @__tsan_acquire(ptr #{ptr})"
        end

        # Inline atomic decrement with conditional deallocation
        # RC is stored at ptr - 8
        emit "%rc_ptr.#{inst.id} = getelementptr i8, ptr #{ptr}, i64 -8"
        # atomicrmw sub with acq_rel ordering (release on dec, acquire on read for dealloc check)
        emit "%old_rc.#{inst.id} = atomicrmw sub ptr %rc_ptr.#{inst.id}, i64 1 acq_rel"
        # Check if old RC was 1 (meaning new RC is 0 → deallocate)
        emit "%should_free.#{inst.id} = icmp eq i64 %old_rc.#{inst.id}, 1"
        emit "br i1 %should_free.#{inst.id}, label %do_free.#{inst.id}, label %skip_free.#{inst.id}"
        emit_raw "do_free.#{inst.id}:\n"
        @indent = 1
        # Free the raw allocation (ptr - 8 is start of allocation)
        emit "call void @free(ptr %rc_ptr.#{inst.id})"
        emit "br label %skip_free.#{inst.id}"
        emit_raw "skip_free.#{inst.id}:\n"
      else
        # Non-atomic: call runtime function
        destructor = "null"  # TODO: function pointer for destructor
        emit "call void @__crystal_v2_rc_dec(ptr #{ptr}, ptr #{destructor})"
      end
    end

    private def emit_load(inst : Load, name : String)
      type = @type_mapper.llvm_type(inst.type)
      ptr = value_ref(inst.ptr)

      # Can't load void - use ptr instead
      if type == "void"
        type = "ptr"
        @value_types[inst.id] = TypeRef::POINTER
      else
        # Track actual loaded type for downstream use
        @value_types[inst.id] = inst.type
      end

      # TSan instrumentation: report read before load
      if @emit_tsan
        tsan_size = tsan_access_size(inst.type)
        emit "call void @__tsan_read#{tsan_size}(ptr #{ptr})"
      end

      emit "#{name} = load #{type}, ptr #{ptr}"
    end

    private def emit_store(inst : Store)
      ptr = value_ref(inst.ptr)
      val = value_ref(inst.value)
      # Look up the type of the value being stored
      val_type = @value_types[inst.value]? || TypeRef::POINTER
      val_type_str = @type_mapper.llvm_type(val_type)

      # Fix for VOID types - if value looks like a pointer reference, use ptr type
      if val_type_str == "void" && val.starts_with?("%")
        val_type_str = "ptr"
      end

      # TSan instrumentation: report write before store
      if @emit_tsan
        tsan_size = tsan_access_size(val_type)
        emit "call void @__tsan_write#{tsan_size}(ptr #{ptr})"
      end

      emit "store #{val_type_str} #{val}, ptr #{ptr}"
    end

    # Get TSan access size (1, 2, 4, 8, or 16 bytes)
    private def tsan_access_size(type : TypeRef) : Int32
      case type
      when TypeRef::BOOL, TypeRef::INT8, TypeRef::UINT8
        1
      when TypeRef::INT16, TypeRef::UINT16
        2
      when TypeRef::INT32, TypeRef::UINT32, TypeRef::CHAR, TypeRef::SYMBOL
        4
      when TypeRef::INT64, TypeRef::UINT64, TypeRef::POINTER, TypeRef::STRING
        8
      when TypeRef::INT128, TypeRef::UINT128
        16
      when TypeRef::FLOAT32
        4
      when TypeRef::FLOAT64
        8
      else
        8  # Default to pointer size for unknown types
      end
    end

    private def emit_gep(inst : GetElementPtr, name : String)
      base = value_ref(inst.base)

      # Check if base is a struct or reference type (from registered types)
      base_value_type = @value_types[inst.base]?
      if base_value_type && (mir_type = @module.type_registry.get(base_value_type))
        if mir_type.kind.struct? || mir_type.kind.reference?
          # Struct/Class GEP: use actual struct type and field index
          struct_type = "%#{@type_mapper.mangle_name(mir_type.name)}"
          # Convert byte offset to field index
          # We need to lookup the field layout of the struct to get correct index
          field_byte_offset = inst.indices.first? || 0_u32
          field_index = compute_field_index(mir_type, field_byte_offset)
          emit "#{name} = getelementptr #{struct_type}, ptr #{base}, i32 0, i32 #{field_index}"
          @value_types[inst.id] = TypeRef::POINTER  # GEP always returns pointer
          return
        end
      end

      # Default: byte-level pointer arithmetic GEP
      byte_offset = inst.indices.first? || 0_u32
      emit "#{name} = getelementptr i8, ptr #{base}, i32 #{byte_offset}"
      @value_types[inst.id] = TypeRef::POINTER  # GEP always returns pointer
    end

    # Compute field index from byte offset by examining the type's field layout
    private def compute_field_index(mir_type : Type, byte_offset : UInt32) : Int32
      # Class layout: vtable ptr (8 bytes) + fields
      # Struct layout: just fields
      base_offset = mir_type.kind.reference? ? 8_u32 : 0_u32

      if byte_offset < base_offset
        return 0  # Accessing vtable or pre-field area
      end

      fields = mir_type.fields
      return 0 unless fields

      adjusted_offset = byte_offset - base_offset
      current_offset = 0_u32
      field_idx = 0

      fields.each_with_index do |field, idx|
        if current_offset >= adjusted_offset
          return mir_type.kind.reference? ? idx + 1 : idx  # +1 for vtable in classes
        end
        # Compute field size (8 for pointers, 4 for i32, etc.)
        field_size = compute_type_size(field.type_ref)
        current_offset += field_size
        field_idx = idx
      end

      # If we're at or past the offset, return the last matching field index
      # +1 for classes to account for vtable at index 0
      mir_type.kind.reference? ? field_idx + 1 : field_idx
    end

    private def compute_type_size(type_ref : TypeRef) : UInt32
      case type_ref
      when TypeRef::BOOL then 1_u32
      when TypeRef::INT8, TypeRef::UINT8 then 1_u32
      when TypeRef::INT16, TypeRef::UINT16 then 2_u32
      when TypeRef::INT32, TypeRef::UINT32, TypeRef::FLOAT32 then 4_u32
      when TypeRef::INT64, TypeRef::UINT64, TypeRef::FLOAT64 then 8_u32
      when TypeRef::INT128, TypeRef::UINT128 then 16_u32
      else
        # Pointers, references, etc. - 8 bytes on 64-bit
        8_u32
      end
    end

    private def emit_gep_dynamic(inst : GetElementPtrDynamic, name : String)
      base = value_ref(inst.base)
      index = value_ref(inst.index)
      element_type = @type_mapper.llvm_type(inst.element_type)

      # Check if index needs to be converted to i64 for GEP
      index_type = @value_types[inst.index]? || TypeRef::INT32
      index_type_str = @type_mapper.llvm_type(index_type)
      if index_type_str == "i64"
        emit "#{name} = getelementptr #{element_type}, ptr #{base}, i64 #{index}"
      else
        # Convert i32 to i64 with sign extension
        ext_name = "#{name}.idx64"
        emit "#{ext_name} = sext #{index_type_str} #{index} to i64"
        emit "#{name} = getelementptr #{element_type}, ptr #{base}, i64 #{ext_name}"
      end
      @value_types[inst.id] = TypeRef::POINTER  # GEP always returns pointer
    end

    private def emit_binary_op(inst : BinaryOp, name : String)
      result_type = @type_mapper.llvm_type(inst.type)
      left = value_ref(inst.left)
      right = value_ref(inst.right)

      # For comparisons, use operand type; for others, use result type
      operand_type = @value_types[inst.left]? || TypeRef::INT32
      operand_type_str = @type_mapper.llvm_type(operand_type)
      right_type = @value_types[inst.right]? || TypeRef::INT32
      right_type_str = @type_mapper.llvm_type(right_type)

      # Determine operation type
      is_arithmetic = inst.op.add? || inst.op.sub? || inst.op.mul? ||
                      inst.op.div? || inst.op.rem? || inst.op.shl? ||
                      inst.op.shr? || inst.op.and? || inst.op.or? || inst.op.xor?
      is_comparison = inst.op.eq? || inst.op.ne? || inst.op.lt? || inst.op.le? ||
                      inst.op.gt? || inst.op.ge?

      # Handle union types in comparisons - check type_id for nil comparison
      if is_comparison && operand_type_str.includes?(".union")
        base_name = name.lstrip('%')
        # Union comparison: compare type_id (0=non-nil, 1=nil)
        # For == 0 or != 0, compare type_id to 0 (non-nil check)
        emit "%#{base_name}.union_ptr = alloca #{operand_type_str}, align 8"
        emit "store #{operand_type_str} #{left}, ptr %#{base_name}.union_ptr"
        emit "%#{base_name}.type_id_ptr = getelementptr #{operand_type_str}, ptr %#{base_name}.union_ptr, i32 0, i32 0"
        emit "%#{base_name}.type_id = load i32, ptr %#{base_name}.type_id_ptr"
        # type_id 0 = non-nil (truthy), type_id 1 = nil
        if inst.op.eq?
          emit "#{name} = icmp eq i32 %#{base_name}.type_id, 1"  # Check if nil
        else
          emit "#{name} = icmp ne i32 %#{base_name}.type_id, 1"  # Check if not nil
        end
        @value_types[inst.id] = TypeRef::BOOL  # Comparisons return i1
        return
      end

      # Guard: arithmetic/shift ops can't use ptr or void type
      # Use operand type instead of defaulting to i64
      if (result_type == "ptr" || result_type == "void") && is_arithmetic
        # Try to use the actual operand type if it's a concrete int type
        if operand_type_str != "ptr" && operand_type_str != "void"
          result_type = operand_type_str
        elsif right_type_str != "ptr" && right_type_str != "void"
          result_type = right_type_str
        else
          result_type = "i64"  # Fallback only when both operands are ptr/void
        end
      end

      # Convert ptr operands to appropriate int type for arithmetic/comparison ops
      needs_int_operands = is_arithmetic || is_comparison
      if needs_int_operands
        # Helper to check if type is valid for int conversion
        valid_int = ->(t : String) { t != "ptr" && t != "void" && !t.includes?(".union") }

        # Use result type for arithmetic, non-ptr operand type for comparison
        int_type = if is_arithmetic
                     valid_int.call(result_type) ? result_type : "i64"
                   else
                     # For comparisons, use the type of the non-ptr/non-void operand, or i64 if both are ptr/void
                     if valid_int.call(operand_type_str)
                       operand_type_str
                     elsif valid_int.call(right_type_str)
                       right_type_str
                     else
                       "i64"
                     end
                   end
        if operand_type_str == "ptr"
          emit "%binop#{inst.id}.left = ptrtoint ptr #{left} to #{int_type}"
          left = "%binop#{inst.id}.left"
          operand_type_str = int_type
        end
        if right_type_str == "ptr"
          emit "%binop#{inst.id}.right = ptrtoint ptr #{right} to #{int_type}"
          right = "%binop#{inst.id}.right"
        end
      end

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
        # Comparisons need both operands to be same type
        # If sizes differ, extend smaller to larger type
        cmp_type = operand_type_str
        if operand_type_str != right_type_str && operand_type_str.starts_with?("i") && right_type_str.starts_with?("i")
          left_size = operand_type_str[1..].to_i? || 32
          right_size = right_type_str[1..].to_i? || 32
          if left_size < right_size
            # Extend left to right's size
            cmp_type = right_type_str
            emit "%cmp.#{inst.id}.left = #{is_signed ? "sext" : "zext"} #{operand_type_str} #{left} to #{cmp_type}"
            left = "%cmp.#{inst.id}.left"
          elsif right_size < left_size
            # Extend right to left's size
            emit "%cmp.#{inst.id}.right = #{is_signed ? "sext" : "zext"} #{right_type_str} #{right} to #{cmp_type}"
            right = "%cmp.#{inst.id}.right"
          end
        end
        emit "#{name} = #{op} #{cmp_type} #{left}, #{right}"
        @value_types[inst.id] = TypeRef::BOOL
      else
        emit "#{name} = #{op} #{result_type} #{left}, #{right}"
        # Track actual emitted type for downstream use
        actual_type = case result_type
                      when "i1" then TypeRef::BOOL
                      when "i8" then TypeRef::INT8
                      when "i16" then TypeRef::INT16
                      when "i32" then TypeRef::INT32
                      when "i64" then TypeRef::INT64
                      when "ptr" then TypeRef::POINTER
                      else inst.type  # Use MIR type as fallback
                      end
        @value_types[inst.id] = actual_type
      end
    end

    private def emit_unary_op(inst : UnaryOp, name : String)
      type = @type_mapper.llvm_type(inst.type)
      operand = value_ref(inst.operand)
      operand_type = @value_types[inst.operand]? || TypeRef::BOOL
      operand_llvm_type = @type_mapper.llvm_type(operand_type)

      case inst.op
      when .neg?
        emit "#{name} = sub #{type} 0, #{operand}"
        @value_types[inst.id] = operand_type  # neg preserves operand type
      when .not?
        # NOT needs boolean operand - convert if needed
        if operand_llvm_type != "i1"
          base_name = name.lstrip('%')
          if operand_llvm_type == "ptr"
            emit "%#{base_name}.bool = icmp ne ptr #{operand}, null"
          elsif operand_llvm_type.includes?(".union")
            # For unions, check if type_id != 1 (1 = nil)
            emit "%#{base_name}.union_ptr = alloca #{operand_llvm_type}, align 8"
            emit "store #{operand_llvm_type} #{operand}, ptr %#{base_name}.union_ptr"
            emit "%#{base_name}.type_id_ptr = getelementptr #{operand_llvm_type}, ptr %#{base_name}.union_ptr, i32 0, i32 0"
            emit "%#{base_name}.type_id = load i32, ptr %#{base_name}.type_id_ptr"
            emit "%#{base_name}.bool = icmp ne i32 %#{base_name}.type_id, 1"
          else
            emit "%#{base_name}.bool = icmp ne #{operand_llvm_type} #{operand}, 0"
          end
          emit "#{name} = xor i1 %#{base_name}.bool, 1"
        else
          emit "#{name} = xor i1 #{operand}, 1"
        end
        @value_types[inst.id] = TypeRef::BOOL  # NOT always returns i1
      when .bit_not?
        emit "#{name} = xor #{type} #{operand}, -1"
        @value_types[inst.id] = operand_type  # bit_not preserves operand type
      end
    end

    private def emit_cast(inst : Cast, name : String)
      # Get source type from value_types registry
      src_type_ref = @value_types[inst.value]? || TypeRef::POINTER
      src_type = @type_mapper.llvm_type(src_type_ref)
      dst_type = @type_mapper.llvm_type(inst.type)
      value = value_ref(inst.value)

      # Special case: casting to union type (e.g., nil → Option(T))
      # Can't bitcast ptr/value to union struct - must construct it
      if dst_type.includes?(".union")
        base_name = name.lstrip('%')
        emit "%#{base_name}.ptr = alloca #{dst_type}, align 8"
        emit "%#{base_name}.type_id_ptr = getelementptr #{dst_type}, ptr %#{base_name}.ptr, i32 0, i32 0"

        # Check if source is null/nil - create nil union
        is_nil_cast = value == "null" || src_type == "void" || src_type_ref == TypeRef::VOID
        if is_nil_cast
          emit "store i32 1, ptr %#{base_name}.type_id_ptr"  # 1 = nil
        else
          # Non-nil value - store type_id=0 and payload
          emit "store i32 0, ptr %#{base_name}.type_id_ptr"  # 0 = non-nil
          emit "%#{base_name}.payload_ptr = getelementptr #{dst_type}, ptr %#{base_name}.ptr, i32 0, i32 1"
          emit "store #{src_type} #{value}, ptr %#{base_name}.payload_ptr"
        end
        emit "#{name} = load #{dst_type}, ptr %#{base_name}.ptr"
        @value_types[inst.id] = inst.type
        return
      end

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
      # Track actual destination type for downstream use
      @value_types[inst.id] = inst.type
    end

    private def emit_phi(inst : Phi, name : String)
      phi_type = @type_mapper.llvm_type(inst.type)

      # Check if prepass already marked this phi as ptr (union→ptr conversion)
      prepass_type = @value_types[inst.id]?
      if prepass_type == TypeRef::POINTER && phi_type.includes?(".union")
        # Prepass determined this union phi should be ptr - emit as ptr phi
        incoming = inst.incoming.map do |(block, val)|
          val_type = @value_types[val]?
          val_type_str = val_type ? @type_mapper.llvm_type(val_type) : nil
          if val_type_str && val_type_str.includes?(".union")
            # Union value can't be used in ptr phi - use null
            "[null, %#{@block_names[block]}]"
          elsif val_type_str && val_type_str.starts_with?("i") && !val_type_str.includes?(".union")
            # Int value can't be used in ptr phi - use null
            "[null, %#{@block_names[block]}]"
          else
            ref = value_ref(val)
            "[#{ref}, %#{@block_names[block]}]"
          end
        end
        emit "#{name} = phi ptr #{incoming.join(", ")}"
        # @value_types already set by prepass
        return
      end

      # Void type phi nodes are invalid in LLVM - emit as ptr with null values
      if phi_type == "void"
        # Emit as ptr phi with null values so the register exists for downstream use
        incoming = inst.incoming.map do |(block, val)|
          "[null, %#{@block_names[block]}]"
        end
        emit "#{name} = phi ptr #{incoming.join(", ")}"
        @value_types[inst.id] = TypeRef::POINTER
        return
      end

      is_int_type = phi_type.starts_with?("i") && phi_type != "i1" && !phi_type.includes?(".union")
      is_bool_type = phi_type == "i1"
      is_ptr_type = phi_type == "ptr"
      is_union_type = phi_type.includes?(".union")

      # Check if i1 phi has union incoming - use 0 for union values
      if is_bool_type
        has_union_incoming = inst.incoming.any? do |(block, val)|
          val_type = @value_types[val]?
          val_type && @type_mapper.llvm_type(val_type).includes?(".union")
        end
        if has_union_incoming
          incoming = inst.incoming.map do |(block, val)|
            val_type = @value_types[val]?
            val_type_str = val_type ? @type_mapper.llvm_type(val_type) : nil
            if val_type_str && val_type_str.includes?(".union")
              # Union value - use 0 (false) as default
              "[0, %#{@block_names[block]}]"
            else
              ref = value_ref(val)
              "[#{ref}, %#{@block_names[block]}]"
            end
          end
          emit "#{name} = phi i1 #{incoming.join(", ")}"
          @value_types[inst.id] = TypeRef::BOOL
          return
        end
      end

      # Check if ptr phi has union incoming - use null for union values
      # Can't extract in current block for phi, so use null (lossy but compiles)
      if is_ptr_type
        has_union_incoming = inst.incoming.any? do |(block, val)|
          val_type = @value_types[val]?
          val_type && @type_mapper.llvm_type(val_type).includes?(".union")
        end
        if has_union_incoming
          incoming = inst.incoming.map do |(block, val)|
            val_type = @value_types[val]?
            val_type_str = val_type ? @type_mapper.llvm_type(val_type) : nil
            if val_type_str && val_type_str.includes?(".union")
              # Union value can't be directly used in ptr phi - use null
              "[null, %#{@block_names[block]}]"
            elsif val_type_str && val_type_str.starts_with?("i") && !val_type_str.includes?(".union")
              # Int value in ptr phi - use null
              "[null, %#{@block_names[block]}]"
            else
              ref = value_ref(val)
              "[#{ref}, %#{@block_names[block]}]"
            end
          end
          emit "#{name} = phi ptr #{incoming.join(", ")}"
          @value_types[inst.id] = TypeRef::POINTER
          return
        end
      end

      # Check if any incoming value is ptr OR unknown type when phi expects union
      # Unknown types might be forward-referenced ptrs, so emit as ptr phi to be safe
      # This is a MIR type mismatch - handle by emitting as ptr phi with null for union values
      if is_union_type
        has_ptr_or_unknown_incoming = inst.incoming.any? do |(block, val)|
          val_type = @value_types[val]?
          const_val = @constant_values[val]?
          val_llvm_type = val_type ? @type_mapper.llvm_type(val_type) : nil
          # Has ptr type, OR has no known type (forward reference that might be ptr)
          # But exclude constants which have known values
          # Also check if the incoming value is from a block that might have ptr phis
          (val_llvm_type == "ptr") ||
          (val_type.nil? && const_val.nil?) ||
          # If const_val is "null", it's being used as ptr
          (const_val == "null")
        end
        if has_ptr_or_unknown_incoming
          # Emit as ptr phi - for union/int values use null (lossy but compiles)
          # This handles MIR bugs where array literal (ptr) and union flow to same phi
          incoming = inst.incoming.map do |(block, val)|
            val_type = @value_types[val]?
            val_type_str = val_type ? @type_mapper.llvm_type(val_type) : nil
            if val_type_str && val_type_str.includes?(".union")
              # Union value can't be used in ptr phi - use null
              # This is lossy but allows compilation to proceed
              "[null, %#{@block_names[block]}]"
            elsif val_type_str && val_type_str.starts_with?("i") && !val_type_str.includes?(".union")
              # Int value can't be used in ptr phi - use null
              "[null, %#{@block_names[block]}]"
            else
              ref = value_ref(val)
              # For unknown types, assume they're ptr-compatible
              "[#{ref}, %#{@block_names[block]}]"
            end
          end
          emit "#{name} = phi ptr #{incoming.join(", ")}"
          @value_types[inst.id] = TypeRef::POINTER
          return
        end
      end

      incoming = inst.incoming.map do |(block, val)|
        # Check if this is a null constant - use literal "0" for int types
        # Constants are pre-collected so this works for forward references too
        const_val = @constant_values[val]?

        # Check if value was never emitted (e.g., unreachable code after raise)
        # Also check for void type - void calls are registered but don't produce %rN
        # This must come FIRST before we try to use the value
        val_emitted = @value_names.has_key?(val)
        val_is_const = @constant_values.has_key?(val)
        val_type_for_void = @value_types[val]?
        val_llvm_type_for_void = val_type_for_void ? @type_mapper.llvm_type(val_type_for_void) : nil
        is_void_value = val_llvm_type_for_void == "void"

        if (!val_emitted || is_void_value) && !val_is_const
          # DEBUG
          STDERR.puts "[PHI-UNDEF] phi=#{inst.id}, name=#{name}, block=#{@block_names[block]}, val=#{val}, phi_type=#{phi_type}"
          # Use safe default based on phi type
          if is_union_type
            "[zeroinitializer, %#{@block_names[block]}]"
          elsif is_ptr_type
            "[null, %#{@block_names[block]}]"
          elsif is_int_type || is_bool_type
            "[0, %#{@block_names[block]}]"
          else
            "[null, %#{@block_names[block]}]"
          end
        elsif const_val == "null" && is_int_type
          "[0, %#{@block_names[block]}]"
        elsif const_val == "0" && is_ptr_type
          "[null, %#{@block_names[block]}]"
        else
          # Check for type mismatch
          val_type = @value_types[val]?
          val_type_str = val_type ? @type_mapper.llvm_type(val_type) : nil

          if is_int_type && val_type_str && val_type_str.includes?(".union")
            # Union flowing into int phi - use 0 (nil case)
            "[0, %#{@block_names[block]}]"
          elsif is_int_type && (val_type_str == "ptr" || val_type_str == "void")
            # Ptr/void flowing into int phi - use 0 (type mismatch from MIR)
            "[0, %#{@block_names[block]}]"
          elsif is_int_type && val_type_str.nil?
            # Unknown type flowing into int phi - check if value_ref looks like ptr
            ref = value_ref(val)
            if ref == "null" || ref.starts_with?("%r")
              # Forward-referenced value - can't determine type, use 0 as safe default
              # This handles MIR bugs where phi references values from wrong control paths
              "[0, %#{@block_names[block]}]"
            else
              "[#{ref}, %#{@block_names[block]}]"
            end
          elsif is_ptr_type && val_type_str && val_type_str.starts_with?("i") && !val_type_str.includes?(".union")
            # Int flowing into ptr phi - use null (type mismatch from MIR)
            "[null, %#{@block_names[block]}]"
          else
            ref = value_ref(val)
            # Also check if value_ref returned "null"
            if ref == "null" && is_int_type
              ref = "0"
            elsif (ref == "0" || ref.starts_with?("%r")) && is_ptr_type && val_type_str && val_type_str.starts_with?("i")
              # Int value flowing into ptr phi
              ref = "null"
            end
            "[#{ref}, %#{@block_names[block]}]"
          end
        end
      end
      emit "#{name} = phi #{phi_type} #{incoming.join(", ")}"
      @value_types[inst.id] = inst.type
    end

    private def emit_select(inst : Select, name : String)
      type = @type_mapper.llvm_type(inst.type)
      cond = value_ref(inst.condition)
      then_val = value_ref(inst.then_value)
      else_val = value_ref(inst.else_value)
      # For non-pointer types, convert "null" to "0"
      if type != "ptr" && !type.includes?(".union")
        then_val = "0" if then_val == "null"
        else_val = "0" if else_val == "null"
      end
      emit "#{name} = select i1 #{cond}, #{type} #{then_val}, #{type} #{else_val}"
      @value_types[inst.id] = inst.type
    end

    private def emit_call(inst : Call, name : String, func : Function)
      # Look up callee function for name and param types
      callee_func = @module.functions.find { |f| f.id == inst.callee }
      callee_name = if callee_func
                      @type_mapper.mangle_name(callee_func.name)
                    else
                      "func#{inst.callee}"
                    end

      # Use callee's return type instead of inst.type for correct ABI
      # But if callee returns void and inst.type is not void, use inst.type
      # (this handles cases where method resolution found wrong overload)
      inst_return_type = @type_mapper.llvm_type(inst.type)
      return_type = if callee_func
                      callee_ret = @type_mapper.llvm_type(callee_func.return_type)
                      # Prefer inst.type when callee returns void but MIR expects a value
                      if callee_ret == "void" && inst_return_type != "void"
                        inst_return_type
                      else
                        callee_ret
                      end
                    else
                      # Unresolved function - if void, default to ptr to be safe
                      # because downstream code may use the result
                      if inst_return_type == "void"
                        "ptr"
                      else
                        inst_return_type
                      end
                    end
      # Final fallback: constructors and common methods should return appropriate types
      if return_type == "void"
        # Methods that typically return self/object/value (ptr)
        ptr_returning_methods = ["new", "create", "build", "initialize", "allocate", "clone", "dup",
                                  "monotonic", "now", "utc", "local", "at", "from", "parse",
                                  "to_s", "to_a", "to_h", "to_json", "to_yaml",
                                  "fetch", "get", "first", "last", "pop", "shift", "each",
                                  "next", "prev", "succ", "pred", "value", "key",
                                  "not_nil_",  # Bang method that returns unwrapped value
                                  "position", "capture_position", "start", "finish", "begin", "end",
                                  "span", "build_span", "source", "string", "buffer", "slice", "token",
                                  "node_literal"]  # Returns Slice(UInt8) | Nil union
        # Methods that return i64
        i64_returning_methods = ["to_i64", "to_u64"]
        # Methods that return i32/int (includes chr - converts int to Char/i32)
        i32_returning_methods = ["size", "length", "count", "index", "rindex", "ord", "chr",
                                  "hash", "kind", "id", "line", "column", "offset",
                                  "to_i32", "to_u32", "to_i", "to_u"]
        # Methods that return i16
        i16_returning_methods = ["to_i16", "to_u16"]
        # Methods that return i8/byte
        i8_returning_methods = ["byte", "current_byte", "peek_byte", "char", "current_char",
                                 "to_i8", "to_u8"]

        # Helper to check name matches (handles type suffixes like _Void_Void)
        matches_name = ->(m : String, name : String) {
          name == m ||
          name.ends_with?("_#{m}") ||
          name.includes?("##{m}") ||
          name.ends_with?("__#{m}") ||
          name.includes?("_#{m}_")  # Handle method_name_TypeSuffix patterns
        }

        returns_ptr = ptr_returning_methods.any? { |m| matches_name.call(m, callee_name) }
        returns_i64 = i64_returning_methods.any? { |m| matches_name.call(m, callee_name) }
        returns_i32 = i32_returning_methods.any? { |m| matches_name.call(m, callee_name) }
        returns_i16 = i16_returning_methods.any? { |m| matches_name.call(m, callee_name) }
        returns_i8 = i8_returning_methods.any? { |m| matches_name.call(m, callee_name) }
        # Known predicate methods (methods ending in ?) that return Bool
        # Note: Can't just check ends_with?("_") because bang methods (!) also mangle to _
        # Also exclude "not_" prefixed methods as those are bang methods returning values
        bool_returning_methods = ["nil_", "empty_", "valid_", "invalid_", "blank_", "present_",
                                   "includes_", "any_", "all_", "none_", "one_", "starts_with_",
                                   "ends_with_", "ascii_letter_", "ascii_digit_", "whitespace_",
                                   "is_a_", "responds_to_", "same_", "equal_", "definition_start_",
                                   "is_identifier_part_", "operator_token_", "keyword_token_"]
        # Bang methods that return values, not booleans (e.g., not_nil! returns the unwrapped value)
        is_bang_method = callee_name.includes?("not_nil_") || callee_name.includes?("_not_nil") ||
                         callee_name.ends_with?("_") && !bool_returning_methods.any? { |m| callee_name.ends_with?(m) }
        returns_bool = !is_bang_method && bool_returning_methods.any? { |m| callee_name.ends_with?(m) || callee_name.includes?("_#{m}") }

        # Check ptr first (bang methods like not_nil! return ptr, not bool)
        if returns_ptr
          return_type = "ptr"
          @value_types[inst.id] = TypeRef::POINTER
        elsif returns_bool
          return_type = "i1"
          @value_types[inst.id] = TypeRef::BOOL
        elsif returns_i8
          return_type = "i8"
          @value_types[inst.id] = TypeRef::INT8
        elsif returns_i16
          return_type = "i16"
          @value_types[inst.id] = TypeRef::INT16
        elsif returns_i32
          return_type = "i32"
          @value_types[inst.id] = TypeRef::INT32
        elsif returns_i64
          return_type = "i64"
          @value_types[inst.id] = TypeRef::INT64
        end
      end

      # Format arguments with proper types, handling type coercion where needed
      # Use callee function params only if param count matches arg count
      use_callee_params = callee_func && callee_func.params.size == inst.args.size
      # Debug void args
      has_void_arg = inst.args.any? { |a| @type_mapper.llvm_type(@value_types[a]? || TypeRef::POINTER) == "void" }
      STDERR.puts "[CALL-DEBUG] #{callee_name}, use_callee_params=#{use_callee_params}, has_void_arg=#{has_void_arg}" if has_void_arg
      args = if use_callee_params && callee_func
               inst.args.map_with_index { |a, i|
                 param_type = callee_func.params[i].type
                 expected_llvm_type = @type_mapper.llvm_type(param_type)
                 actual_type = @value_types[a]? || TypeRef::POINTER
                 actual_llvm_type = @type_mapper.llvm_type(actual_type)
                 # Guard against void - not valid for function arguments
                 # Remember if original was void so we can use null instead of value_ref
                 original_was_void = actual_llvm_type == "void"
                 expected_llvm_type = "ptr" if expected_llvm_type == "void"
                 actual_llvm_type = "ptr" if actual_llvm_type == "void"

                 # If original type was void, use null instead of value reference
                 if original_was_void
                   "ptr null"
                 # If types match, use directly
                 elsif expected_llvm_type == actual_llvm_type
                   "#{expected_llvm_type} #{value_ref(a)}"
                 elsif expected_llvm_type == "ptr" && actual_llvm_type.starts_with?("%") && actual_llvm_type.includes?(".union")
                   # Coerce union to ptr: extract payload and interpret as ptr
                   # Union layout: { type_id : i32, payload : [8 x i8] }
                   c = @cond_counter
                   @cond_counter += 1
                   temp_alloca = "%alloca.#{c}"
                   temp_ptr = "%ptr.#{c}"
                   temp_load = "%load.#{c}"
                   emit "#{temp_alloca} = alloca #{actual_llvm_type}, align 8"
                   emit "store #{actual_llvm_type} #{value_ref(a)}, ptr #{temp_alloca}"
                   emit "#{temp_ptr} = getelementptr #{actual_llvm_type}, ptr #{temp_alloca}, i32 0, i32 1"
                   emit "#{temp_load} = load ptr, ptr #{temp_ptr}"
                   "ptr #{temp_load}"
                 elsif expected_llvm_type == "ptr" && actual_llvm_type == "i1"
                   # Bool to ptr - likely string context, convert bool to string
                   val = value_ref(a)
                   c = @cond_counter
                   @cond_counter += 1
                   emit "%bool_to_str.#{c} = call ptr @__crystal_v2_bool_to_string(i1 #{val})"
                   "ptr %bool_to_str.#{c}"
                elsif expected_llvm_type == "ptr" && (actual_llvm_type.starts_with?("i") || actual_llvm_type == "ptr")
                   # Int to ptr conversion needed (inttoptr)
                   val = value_ref(a)
                   if val == "0"
                     "ptr null"
                   else
                     if actual_llvm_type == "ptr"
                       "ptr #{val}"  # Already a ptr
                     else
                       c = @cond_counter
                       @cond_counter += 1
                       temp_ptr = "%inttoptr.#{c}"
                       emit "#{temp_ptr} = inttoptr #{actual_llvm_type} #{val} to ptr"
                       "ptr #{temp_ptr}"
                     end
                   end
                 elsif expected_llvm_type == "i1" && actual_llvm_type.starts_with?("i") && actual_llvm_type != "i1"
                   # Larger int to i1 (bool) - truncate
                   val = value_ref(a)
                   c = @cond_counter
                   @cond_counter += 1
                   emit "%trunc_to_i1.#{c} = trunc #{actual_llvm_type} #{val} to i1"
                   "i1 %trunc_to_i1.#{c}"
                 elsif expected_llvm_type.starts_with?("i") && actual_llvm_type.includes?(".union")
                   # Coerce union to int: extract payload as int
                   # Union layout: { type_id : i32, payload : [N x i8] }
                   c = @cond_counter
                   @cond_counter += 1
                   val = value_ref(a)
                   emit "%union_to_int.#{c}.ptr = alloca #{actual_llvm_type}, align 8"
                   emit "store #{actual_llvm_type} #{val}, ptr %union_to_int.#{c}.ptr"
                   emit "%union_to_int.#{c}.payload_ptr = getelementptr #{actual_llvm_type}, ptr %union_to_int.#{c}.ptr, i32 0, i32 1"
                   emit "%union_to_int.#{c}.val = load #{expected_llvm_type}, ptr %union_to_int.#{c}.payload_ptr"
                   "#{expected_llvm_type} %union_to_int.#{c}.val"
                 elsif expected_llvm_type.starts_with?("i") && actual_llvm_type == "ptr"
                   # Ptr to int conversion needed (ptrtoint)
                   val = value_ref(a)
                   c = @cond_counter
                   @cond_counter += 1
                   temp_int = "%ptrtoint.#{c}"
                   emit "#{temp_int} = ptrtoint ptr #{val} to #{expected_llvm_type}"
                   "#{expected_llvm_type} #{temp_int}"
                 elsif expected_llvm_type.includes?(".union") && actual_llvm_type == "ptr"
                   # Ptr to union conversion - wrap ptr in union with type_id=0 (non-nil)
                   val = value_ref(a)
                   c = @cond_counter
                   @cond_counter += 1
                   # Check for null - use nil union (type_id=1)
                   if val == "null"
                     emit "%ptr_to_union.#{c}.ptr = alloca #{expected_llvm_type}, align 8"
                     emit "%ptr_to_union.#{c}.type_id_ptr = getelementptr #{expected_llvm_type}, ptr %ptr_to_union.#{c}.ptr, i32 0, i32 0"
                     emit "store i32 1, ptr %ptr_to_union.#{c}.type_id_ptr"
                     emit "%ptr_to_union.#{c}.val = load #{expected_llvm_type}, ptr %ptr_to_union.#{c}.ptr"
                     "#{expected_llvm_type} %ptr_to_union.#{c}.val"
                   else
                     emit "%ptr_to_union.#{c}.ptr = alloca #{expected_llvm_type}, align 8"
                     emit "%ptr_to_union.#{c}.type_id_ptr = getelementptr #{expected_llvm_type}, ptr %ptr_to_union.#{c}.ptr, i32 0, i32 0"
                     emit "store i32 0, ptr %ptr_to_union.#{c}.type_id_ptr"
                     emit "%ptr_to_union.#{c}.payload_ptr = getelementptr #{expected_llvm_type}, ptr %ptr_to_union.#{c}.ptr, i32 0, i32 1"
                     emit "store ptr #{val}, ptr %ptr_to_union.#{c}.payload_ptr"
                     emit "%ptr_to_union.#{c}.val = load #{expected_llvm_type}, ptr %ptr_to_union.#{c}.ptr"
                     "#{expected_llvm_type} %ptr_to_union.#{c}.val"
                   end
                 else
                   # Fallback: use expected type
                   # If actual type was void (converted to ptr), use null
                   original_actual_type = @value_types[a]? || TypeRef::POINTER
                   original_actual_llvm = @type_mapper.llvm_type(original_actual_type)
                   if original_actual_llvm == "void"
                     next "ptr null"
                   end
                   val = value_ref(a)
                   # For pointer types, convert 0 to null
                   if expected_llvm_type == "ptr" && val == "0"
                     "ptr null"
                   else
                     "#{expected_llvm_type} #{val}"
                   end
                 end
               }.join(", ")
             else
               # Fallback: use actual argument types from value registry
               inst.args.map { |a|
                 arg_type = @value_types[a]? || TypeRef::POINTER
                 arg_llvm_type = @type_mapper.llvm_type(arg_type)
                 # Guard against void argument type - use ptr null
                 if arg_llvm_type == "void"
                   "ptr null"
                 else
                   "#{arg_llvm_type} #{value_ref(a)}"
                 end
               }.join(", ")
             end

      if return_type == "void"
        emit "call void @#{callee_name}(#{args})"
      else
        emit "#{name} = call #{return_type} @#{callee_name}(#{args})"
        # Update value_types to match actual return type for downstream use
        # This is critical when MIR type was void but actual call returns a value
        if return_type.includes?(".union")
          # For union types, if we have a callee_func, use its return_type
          # Otherwise mark as POINTER (can be cast later)
          if callee_func
            @value_types[inst.id] = callee_func.return_type
          end
        else
          actual_type_ref = case return_type
                            when "i1" then TypeRef::BOOL
                            when "i8" then TypeRef::INT8
                            when "i16" then TypeRef::INT16
                            when "i32" then TypeRef::INT32
                            when "i64" then TypeRef::INT64
                            when "ptr" then TypeRef::POINTER
                            else TypeRef::POINTER  # Default fallback
                            end
          @value_types[inst.id] = actual_type_ref
        end
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
        # Track actual return type for downstream use
        actual_type = case return_type
                      when "i1" then TypeRef::BOOL
                      when "i8" then TypeRef::INT8
                      when "i16" then TypeRef::INT16
                      when "i32" then TypeRef::INT32
                      when "i64" then TypeRef::INT64
                      when "ptr" then TypeRef::POINTER
                      else inst.type  # Fallback to MIR type
                      end
        @value_types[inst.id] = actual_type
      end
    end

    private def emit_extern_call(inst : ExternCall, name : String)
      return_type = @type_mapper.llvm_type(inst.type)

      # Format arguments using actual types from value_types registry
      args = inst.args.map do |a|
        arg_type_ref = @value_types[a]? || TypeRef::POINTER
        arg_type = @type_mapper.llvm_type(arg_type_ref)
        # Guard against void argument type - use ptr null instead
        if arg_type == "void"
          "ptr null"
        else
          "#{arg_type} #{value_ref(a)}"
        end
      end.join(", ")

      # Mangle the extern name to be a valid LLVM identifier
      mangled_extern_name = @type_mapper.mangle_name(inst.extern_name)

      # Fallback for constructors and methods that return values
      if return_type == "void"
        ptr_returning_methods = ["new", "create", "build", "initialize", "allocate", "clone", "dup",
                                  "monotonic", "now", "utc", "local", "at", "from", "parse",
                                  "to_s", "to_a", "to_h", "to_json", "to_yaml",
                                  "fetch", "get", "first", "last", "pop", "shift", "each",
                                  "next", "prev", "succ", "pred", "value", "key",
                                  "not_nil_",  # Bang method that returns unwrapped value
                                  "position", "capture_position", "start", "finish", "begin", "end",
                                  "span", "build_span", "source", "string", "buffer", "slice", "token",
                                  "node_literal",  # Returns Slice(UInt8) | Nil union
                                  "[]", "_"]  # [] becomes _ after mangling

        # Methods that return i64
        i64_returning_methods = ["to_i64", "to_u64"]
        # Methods that return i32/int (includes chr - converts int to Char/i32)
        i32_returning_methods = ["size", "length", "count", "index", "rindex", "ord", "chr",
                                  "hash", "kind", "id", "line", "column", "offset",
                                  "to_i32", "to_u32", "to_i", "to_u"]
        # Methods that return i16
        i16_returning_methods = ["to_i16", "to_u16"]
        # Methods that return i8/byte
        i8_returning_methods = ["byte", "current_byte", "peek_byte", "char", "current_char",
                                 "to_i8", "to_u8"]

        # Check for ptr-returning methods (also check with _Method_ pattern for type suffixes)
        matches_extern = ->(m : String, name : String) {
          name == m || name.ends_with?("_#{m}") || name.includes?("_#{m}_")
        }
        returns_ptr = ptr_returning_methods.any? { |m| matches_extern.call(m, mangled_extern_name) }
        # Check for patterns like ClassName___MethodName (accessor methods)
        returns_ptr = true if mangled_extern_name.includes?("____") || mangled_extern_name.includes?("_____")

        # Check for i64-returning methods
        returns_i64 = i64_returning_methods.any? { |m| matches_extern.call(m, mangled_extern_name) }

        # Check for i32-returning methods
        returns_i32 = i32_returning_methods.any? { |m| matches_extern.call(m, mangled_extern_name) }

        # Check for i16-returning methods
        returns_i16 = i16_returning_methods.any? { |m| matches_extern.call(m, mangled_extern_name) }

        # Check for i8-returning methods
        returns_i8 = i8_returning_methods.any? { |m| matches_extern.call(m, mangled_extern_name) }

        # Known predicate methods (methods ending in ?) that return Bool
        # Note: Can't just check ends_with?("_") because bang methods (!) also mangle to _
        # Also exclude "not_" prefixed methods as those are bang methods returning values
        bool_returning_methods = ["nil_", "empty_", "valid_", "invalid_", "blank_", "present_",
                                   "includes_", "any_", "all_", "none_", "one_", "starts_with_",
                                   "ends_with_", "ascii_letter_", "ascii_digit_", "whitespace_",
                                   "is_a_", "responds_to_", "same_", "equal_", "definition_start_",
                                   "is_identifier_part_", "operator_token_", "keyword_token_"]
        # Bang methods that return values, not booleans (e.g., not_nil! returns the unwrapped value)
        is_bang_method = mangled_extern_name.includes?("not_nil_") || mangled_extern_name.includes?("_not_nil") ||
                         mangled_extern_name.ends_with?("_") && !bool_returning_methods.any? { |m| mangled_extern_name.ends_with?(m) }
        returns_bool = !is_bang_method && bool_returning_methods.any? { |m| mangled_extern_name.ends_with?(m) || mangled_extern_name.includes?("_#{m}") }

        # Check ptr first (bang methods like not_nil! return ptr, not bool)
        if returns_ptr
          return_type = "ptr"
          @value_types[inst.id] = TypeRef::POINTER
        elsif returns_bool
          return_type = "i1"
          @value_types[inst.id] = TypeRef::BOOL
        elsif returns_i8
          return_type = "i8"
          @value_types[inst.id] = TypeRef::INT8
        elsif returns_i16
          return_type = "i16"
          @value_types[inst.id] = TypeRef::INT16
        elsif returns_i32
          return_type = "i32"
          @value_types[inst.id] = TypeRef::INT32
        elsif returns_i64
          return_type = "i64"
          @value_types[inst.id] = TypeRef::INT64
        end
      end

      if return_type == "void"
        emit "call void @#{mangled_extern_name}(#{args})"
      else
        emit "#{name} = call #{return_type} @#{mangled_extern_name}(#{args})"
        # Track type for downstream use (if not already set by pattern matching above)
        unless @value_types.has_key?(inst.id)
          actual_type = case return_type
                        when "i1" then TypeRef::BOOL
                        when "i8" then TypeRef::INT8
                        when "i16" then TypeRef::INT16
                        when "i32" then TypeRef::INT32
                        when "i64" then TypeRef::INT64
                        when "ptr" then TypeRef::POINTER
                        else inst.type  # Fallback to MIR type
                        end
          @value_types[inst.id] = actual_type
        end
      end
    end

    private def emit_global_load(inst : GlobalLoad, name : String)
      llvm_type = @type_mapper.llvm_type(inst.type)
      mangled_global = @type_mapper.mangle_name(inst.global_name)
      # Can't load void - use ptr instead
      if llvm_type == "void"
        llvm_type = "ptr"
        @value_types[inst.id] = TypeRef::POINTER
      else
        @value_types[inst.id] = inst.type
      end
      emit "#{name} = load #{llvm_type}, ptr @#{mangled_global}"
    end

    private def emit_global_store(inst : GlobalStore, name : String)
      # Get value type from the stored value
      val = value_ref(inst.value)
      llvm_type = @type_mapper.llvm_type(inst.type)
      # Can't store void - use ptr instead (typically for nil assignment)
      if llvm_type == "void"
        llvm_type = "ptr"
        val = "null" if val == "null" || val.starts_with?("%r") # likely void value
      end
      mangled_global = @type_mapper.mangle_name(inst.global_name)
      emit "store #{llvm_type} #{val}, ptr @#{mangled_global}"
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

      # 3. Store value in payload (skip for void/nil types - they have no payload)
      val_type = @value_types[inst.value]? || TypeRef::POINTER
      val_type_str = @type_mapper.llvm_type(val_type)
      unless val_type_str == "void"
        val = value_ref(inst.value)
        emit "%#{base_name}.payload_ptr = getelementptr #{union_type}, ptr %#{base_name}.ptr, i32 0, i32 1"
        emit "store #{val_type_str} #{val}, ptr %#{base_name}.payload_ptr"
      end

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
      # Void is not valid for array elements - use ptr instead
      element_type = "ptr" if element_type == "void"
      size = inst.size

      # Array struct type: { i32 size, [N x T] data }
      array_type = "{ i32, [#{size} x #{element_type}] }"

      # Allocate array struct on stack
      emit "%#{base_name}.ptr = alloca #{array_type}, align 8"

      # Store size
      emit "%#{base_name}.size_ptr = getelementptr #{array_type}, ptr %#{base_name}.ptr, i32 0, i32 0"
      emit "store i32 #{size}, ptr %#{base_name}.size_ptr"

      # Store elements
      original_element_type = @type_mapper.llvm_type(inst.element_type)
      inst.elements.each_with_index do |elem_id, idx|
        emit "%#{base_name}.elem#{idx}_ptr = getelementptr #{array_type}, ptr %#{base_name}.ptr, i32 0, i32 1, i32 #{idx}"
        # If original element was void, store null; otherwise store actual value
        if original_element_type == "void"
          emit "store ptr null, ptr %#{base_name}.elem#{idx}_ptr"
        else
          elem_val = value_ref(elem_id)
          emit "store #{element_type} #{elem_val}, ptr %#{base_name}.elem#{idx}_ptr"
        end
      end

      # Return pointer to array struct
      emit "#{name} = bitcast ptr %#{base_name}.ptr to ptr"

      # Remember array info for later use
      @array_info[inst.id] = {element_type, size}
    end

    private def emit_array_size(inst : ArraySize, name : String)
      base_name = name.lstrip('%')
      array_ptr = value_ref(inst.array_value)

      # Check if array value is a union type - need to extract ptr from payload
      array_value_type = @value_types[inst.array_value]?
      if array_value_type
        array_llvm_type = @type_mapper.llvm_type(array_value_type)
        if array_llvm_type.includes?(".union")
          # Extract pointer from union payload
          emit "%#{base_name}.union_ptr = alloca #{array_llvm_type}, align 8"
          emit "store #{array_llvm_type} #{array_ptr}, ptr %#{base_name}.union_ptr"
          emit "%#{base_name}.payload_ptr = getelementptr #{array_llvm_type}, ptr %#{base_name}.union_ptr, i32 0, i32 1"
          emit "%#{base_name}.arr_ptr = load ptr, ptr %#{base_name}.payload_ptr"
          array_ptr = "%#{base_name}.arr_ptr"
        end
      end

      # Get size from array struct (first field)
      emit "%#{base_name}.size_ptr = getelementptr { i32, [0 x i32] }, ptr #{array_ptr}, i32 0, i32 0"
      emit "#{name} = load i32, ptr %#{base_name}.size_ptr"

      # Update @value_types - size is always i32
      @value_types[inst.id] = TypeRef::INT32
    end

    private def emit_array_get(inst : ArrayGet, name : String)
      base_name = name.lstrip('%')
      array_ptr = value_ref(inst.array_value)
      index = value_ref(inst.index_value)
      element_type = @type_mapper.llvm_type(inst.element_type)

      # Check if array value is a union type - need to extract ptr from payload
      array_value_type = @value_types[inst.array_value]?
      if array_value_type
        array_llvm_type = @type_mapper.llvm_type(array_value_type)
        if array_llvm_type.includes?(".union")
          # Extract pointer from union payload
          emit "%#{base_name}.union_ptr = alloca #{array_llvm_type}, align 8"
          emit "store #{array_llvm_type} #{array_ptr}, ptr %#{base_name}.union_ptr"
          emit "%#{base_name}.payload_ptr = getelementptr #{array_llvm_type}, ptr %#{base_name}.union_ptr, i32 0, i32 1"
          emit "%#{base_name}.arr_ptr = load ptr, ptr %#{base_name}.payload_ptr"
          array_ptr = "%#{base_name}.arr_ptr"
        end
      end

      # Check if index is ptr type and convert to i32
      index_type = @value_types[inst.index_value]?
      if index_type && @type_mapper.llvm_type(index_type) == "ptr"
        emit "%#{base_name}.idx_int = ptrtoint ptr #{index} to i32"
        index = "%#{base_name}.idx_int"
      end

      # Get element from data array (second field)
      emit "%#{base_name}.elem_ptr = getelementptr { i32, [0 x #{element_type}] }, ptr #{array_ptr}, i32 0, i32 1, i32 #{index}"
      emit "#{name} = load #{element_type}, ptr %#{base_name}.elem_ptr"

      # Update @value_types with actual emitted LLVM type (may differ from MIR type)
      # This is critical for phi nodes that reference this value
      @value_types[inst.id] = inst.element_type
    end

    private def emit_array_set(inst : ArraySet, name : String)
      base_name = name.lstrip('%')
      array_ptr = value_ref(inst.array_value)
      index = value_ref(inst.index_value)
      value = value_ref(inst.value_id)
      element_type = @type_mapper.llvm_type(inst.element_type)

      # Check if array value is a union type - need to extract ptr from payload
      array_value_type = @value_types[inst.array_value]?
      if array_value_type
        array_llvm_type = @type_mapper.llvm_type(array_value_type)
        if array_llvm_type.includes?(".union")
          # Extract pointer from union payload
          emit "%#{base_name}.union_ptr = alloca #{array_llvm_type}, align 8"
          emit "store #{array_llvm_type} #{array_ptr}, ptr %#{base_name}.union_ptr"
          emit "%#{base_name}.payload_ptr = getelementptr #{array_llvm_type}, ptr %#{base_name}.union_ptr, i32 0, i32 1"
          emit "%#{base_name}.arr_ptr = load ptr, ptr %#{base_name}.payload_ptr"
          array_ptr = "%#{base_name}.arr_ptr"
        end
      end

      # Check if index is ptr type and convert to i32
      index_type = @value_types[inst.index_value]?
      if index_type && @type_mapper.llvm_type(index_type) == "ptr"
        emit "%#{base_name}.idx_int = ptrtoint ptr #{index} to i32"
        index = "%#{base_name}.idx_int"
      end

      # Check if value type matches element type
      value_type = @value_types[inst.value_id]?
      actual_value_type = value_type ? @type_mapper.llvm_type(value_type) : element_type
      if actual_value_type == "ptr" && element_type == "i32"
        # Convert ptr to i32
        emit "%#{base_name}.val_int = ptrtoint ptr #{value} to i32"
        value = "%#{base_name}.val_int"
      elsif actual_value_type == "ptr" && element_type != "ptr"
        # Convert ptr to target type
        emit "%#{base_name}.val_cast = ptrtoint ptr #{value} to #{element_type}"
        value = "%#{base_name}.val_cast"
      end

      # Get element pointer from data array (second field) and store
      emit "%#{base_name}.elem_ptr = getelementptr { i32, [0 x #{element_type}] }, ptr #{array_ptr}, i32 0, i32 1, i32 #{index}"
      emit "store #{element_type} #{value}, ptr %#{base_name}.elem_ptr"
    end

    private def emit_string_interpolation(inst : StringInterpolation, name : String)
      base_name = name.lstrip('%')

      # Convert each part to string ptr, handling type conversion
      string_parts = [] of String
      inst.parts.each_with_index do |part_id, idx|
        part_ref = value_ref(part_id)
        part_type = @value_types[part_id]?

        # Check if part is already a string (ptr type from string literal)
        if part_type == TypeRef::STRING || part_type == TypeRef::POINTER || part_type.nil?
          string_parts << part_ref
        elsif part_type == TypeRef::INT32 || part_type == TypeRef::UINT32 || part_type == TypeRef::CHAR
          # Convert int32 to string
          emit "%#{base_name}.conv#{idx} = call ptr @__crystal_v2_int_to_string(i32 #{part_ref})"
          string_parts << "%#{base_name}.conv#{idx}"
        elsif part_type == TypeRef::INT64 || part_type == TypeRef::UINT64
          # Convert int64 to string
          emit "%#{base_name}.conv#{idx} = call ptr @__crystal_v2_int64_to_string(i64 #{part_ref})"
          string_parts << "%#{base_name}.conv#{idx}"
        elsif part_type == TypeRef::INT8 || part_type == TypeRef::UINT8
          # Extend i8 to i32 then convert to string
          emit "%#{base_name}.ext#{idx} = zext i8 #{part_ref} to i32"
          emit "%#{base_name}.conv#{idx} = call ptr @__crystal_v2_int_to_string(i32 %#{base_name}.ext#{idx})"
          string_parts << "%#{base_name}.conv#{idx}"
        elsif part_type == TypeRef::INT16 || part_type == TypeRef::UINT16
          # Extend i16 to i32 then convert to string
          emit "%#{base_name}.ext#{idx} = zext i16 #{part_ref} to i32"
          emit "%#{base_name}.conv#{idx} = call ptr @__crystal_v2_int_to_string(i32 %#{base_name}.ext#{idx})"
          string_parts << "%#{base_name}.conv#{idx}"
        elsif part_type == TypeRef::BOOL
          # Convert bool to string
          emit "%#{base_name}.conv#{idx} = call ptr @__crystal_v2_bool_to_string(i1 #{part_ref})"
          string_parts << "%#{base_name}.conv#{idx}"
        else
          # Check actual LLVM type - might be i32 (enum/symbol) that needs conversion
          part_llvm_type = part_type ? @type_mapper.llvm_type(part_type) : "ptr"
          if part_llvm_type == "i32"
            emit "%#{base_name}.conv#{idx} = call ptr @__crystal_v2_int_to_string(i32 #{part_ref})"
            string_parts << "%#{base_name}.conv#{idx}"
          elsif part_llvm_type == "i64"
            emit "%#{base_name}.conv#{idx} = call ptr @__crystal_v2_int64_to_string(i64 #{part_ref})"
            string_parts << "%#{base_name}.conv#{idx}"
          elsif part_llvm_type == "i8"
            emit "%#{base_name}.ext#{idx} = zext i8 #{part_ref} to i32"
            emit "%#{base_name}.conv#{idx} = call ptr @__crystal_v2_int_to_string(i32 %#{base_name}.ext#{idx})"
            string_parts << "%#{base_name}.conv#{idx}"
          elsif part_llvm_type == "i16"
            emit "%#{base_name}.ext#{idx} = zext i16 #{part_ref} to i32"
            emit "%#{base_name}.conv#{idx} = call ptr @__crystal_v2_int_to_string(i32 %#{base_name}.ext#{idx})"
            string_parts << "%#{base_name}.conv#{idx}"
          elsif part_llvm_type == "i1"
            emit "%#{base_name}.conv#{idx} = call ptr @__crystal_v2_bool_to_string(i1 #{part_ref})"
            string_parts << "%#{base_name}.conv#{idx}"
          else
            # Fallback - treat as ptr
            string_parts << part_ref
          end
        end
      end

      if string_parts.size == 1
        # Single part - just use it directly
        emit "#{name} = bitcast ptr #{string_parts[0]} to ptr"
      elsif string_parts.size == 2
        # Two parts - call __crystal_v2_string_concat
        emit "#{name} = call ptr @__crystal_v2_string_concat(ptr #{string_parts[0]}, ptr #{string_parts[1]})"
      else
        # Multiple parts - chain concatenation
        emit "%#{base_name}.tmp0 = bitcast ptr #{string_parts[0]} to ptr"
        (1...string_parts.size).each do |i|
          prev = i == 1 ? "%#{base_name}.tmp0" : "%#{base_name}.tmp#{i-1}"
          if i == string_parts.size - 1
            emit "#{name} = call ptr @__crystal_v2_string_concat(ptr #{prev}, ptr #{string_parts[i]})"
          else
            emit "%#{base_name}.tmp#{i} = call ptr @__crystal_v2_string_concat(ptr #{prev}, ptr #{string_parts[i]})"
          end
        end
      end
    end

    # ═══════════════════════════════════════════════════════════════════════════
    # SYNCHRONIZATION PRIMITIVES
    # ═══════════════════════════════════════════════════════════════════════════

    private def llvm_ordering(ordering : MemoryOrdering) : String
      case ordering
      when .relaxed? then "monotonic"
      when .acquire? then "acquire"
      when .release? then "release"
      when .acq_rel? then "acq_rel"
      else                "seq_cst"
      end
    end

    private def emit_atomic_load(inst : AtomicLoad, name : String)
      ptr = value_ref(inst.ptr)
      type = @type_mapper.llvm_type(inst.type)
      ordering = llvm_ordering(inst.ordering)

      if @emit_tsan
        tsan_size = tsan_access_size(inst.type)
        emit "call void @__tsan_read#{tsan_size}(ptr #{ptr})"
      end

      emit "#{name} = load atomic #{type}, ptr #{ptr} #{ordering}, align 8"

      if @emit_tsan && inst.ordering.acquire? || inst.ordering.acq_rel? || inst.ordering.seq_cst?
        emit "call void @__tsan_acquire(ptr #{ptr})"
      end
    end

    private def emit_atomic_store(inst : AtomicStore, name : String)
      ptr = value_ref(inst.ptr)
      val = value_ref(inst.value)
      type = @value_types[inst.value]? || "i64"
      ordering = llvm_ordering(inst.ordering)

      if @emit_tsan && inst.ordering.release? || inst.ordering.acq_rel? || inst.ordering.seq_cst?
        emit "call void @__tsan_release(ptr #{ptr})"
      end

      if @emit_tsan
        tsan_size = case type
                    when "i8" then 1
                    when "i16" then 2
                    when "i32" then 4
                    else 8
                    end
        emit "call void @__tsan_write#{tsan_size}(ptr #{ptr})"
      end

      emit "store atomic #{type} #{val}, ptr #{ptr} #{ordering}, align 8"
    end

    private def emit_atomic_cas(inst : AtomicCAS, name : String)
      base_name = name.lstrip('%')
      ptr = value_ref(inst.ptr)
      expected = value_ref(inst.expected)
      desired = value_ref(inst.desired)
      type = @type_mapper.llvm_type(inst.type)
      success_ord = llvm_ordering(inst.success_ordering)
      failure_ord = llvm_ordering(inst.failure_ordering)

      if @emit_tsan
        emit "call void @__tsan_release(ptr #{ptr})"
      end

      # LLVM cmpxchg returns { T, i1 } - the old value and success flag
      emit "%#{base_name}.result = cmpxchg ptr #{ptr}, #{type} #{expected}, #{type} #{desired} #{success_ord} #{failure_ord}"
      # Extract old value
      emit "#{name} = extractvalue { #{type}, i1 } %#{base_name}.result, 0"

      if @emit_tsan
        emit "call void @__tsan_acquire(ptr #{ptr})"
      end
    end

    private def emit_atomic_rmw(inst : AtomicRMW, name : String)
      ptr = value_ref(inst.ptr)
      val = value_ref(inst.value)
      type = @type_mapper.llvm_type(inst.type)
      ordering = llvm_ordering(inst.ordering)

      op = case inst.op
           when .xchg? then "xchg"
           when .add?  then "add"
           when .sub?  then "sub"
           when .and?  then "and"
           when .or?   then "or"
           when .xor?  then "xor"
           when .max?  then "max"
           when .min?  then "min"
           when .u_max? then "umax"
           when .u_min? then "umin"
           else              "xchg"
           end

      if @emit_tsan && (inst.ordering.release? || inst.ordering.acq_rel? || inst.ordering.seq_cst?)
        emit "call void @__tsan_release(ptr #{ptr})"
      end

      emit "#{name} = atomicrmw #{op} ptr #{ptr}, #{type} #{val} #{ordering}"

      if @emit_tsan && (inst.ordering.acquire? || inst.ordering.acq_rel? || inst.ordering.seq_cst?)
        emit "call void @__tsan_acquire(ptr #{ptr})"
      end
    end

    private def emit_fence(inst : Fence, name : String)
      ordering = llvm_ordering(inst.ordering)
      emit "fence #{ordering}"
    end

    private def emit_mutex_lock(inst : MutexLock, name : String)
      ptr = value_ref(inst.mutex_ptr)

      emit "call void @__crystal_v2_mutex_lock(ptr #{ptr})"

      if @emit_tsan
        emit "call void @__tsan_acquire(ptr #{ptr})"
      end
    end

    private def emit_mutex_unlock(inst : MutexUnlock, name : String)
      ptr = value_ref(inst.mutex_ptr)

      if @emit_tsan
        emit "call void @__tsan_release(ptr #{ptr})"
      end

      emit "call void @__crystal_v2_mutex_unlock(ptr #{ptr})"
    end

    private def emit_mutex_trylock(inst : MutexTryLock, name : String)
      ptr = value_ref(inst.mutex_ptr)

      emit "#{name} = call i1 @__crystal_v2_mutex_trylock(ptr #{ptr})"

      if @emit_tsan
        # TSan acquire only on successful lock
        emit "br i1 #{name}, label %#{name.lstrip('%')}.tsan_acquire, label %#{name.lstrip('%')}.tsan_done"
        emit "#{name.lstrip('%')}.tsan_acquire:"
        @indent += 1
        emit "call void @__tsan_acquire(ptr #{ptr})"
        emit "br label %#{name.lstrip('%')}.tsan_done"
        @indent -= 1
        emit "#{name.lstrip('%')}.tsan_done:"
      end
    end

    private def emit_channel_send(inst : ChannelSend, name : String)
      channel = value_ref(inst.channel_ptr)
      val = value_ref(inst.value)

      if @emit_tsan
        emit "call void @__tsan_release(ptr #{channel})"
      end

      emit "call void @__crystal_v2_channel_send(ptr #{channel}, ptr #{val})"
    end

    private def emit_channel_receive(inst : ChannelReceive, name : String)
      channel = value_ref(inst.channel_ptr)

      emit "#{name} = call ptr @__crystal_v2_channel_receive(ptr #{channel})"

      if @emit_tsan
        emit "call void @__tsan_acquire(ptr #{channel})"
      end
    end

    private def emit_channel_close(inst : ChannelClose, name : String)
      channel = value_ref(inst.channel_ptr)

      if @emit_tsan
        emit "call void @__tsan_release(ptr #{channel})"
      end

      emit "call void @__crystal_v2_channel_close(ptr #{channel})"
    end

    # Exception handling - inline setjmp call
    private def emit_try_begin(inst : TryBegin, name : String)
      # Set handler active flag
      emit "store i1 true, ptr @__crystal_exc_handler_active"
      # Call setjmp inline (critical: must be inline, not in a wrapper function)
      emit "#{name} = call i32 @setjmp(ptr @__crystal_exc_jmpbuf)"
    end

    # Exception handling - clear exception handler
    private def emit_try_end(inst : TryEnd, name : String)
      emit "store i1 false, ptr @__crystal_exc_handler_active"
      emit "store ptr null, ptr @__crystal_exc_ptr"
    end

    private def emit_terminator(term : Terminator)
      case term
      when Return
        # TSan: emit function exit before return
        if @emit_tsan
          emit "call void @__tsan_func_exit()"
        end

        if @current_return_type == "void"
          emit "ret void"
        elsif val = term.value
          val_type = @value_types[val]?
          # Check if the value was never defined (MIR bug) - use null/0 default
          # @value_names is only set when instruction is emitted, not by prepass
          # Also check for VOID type - void calls don't emit %rN even if value_names is set
          val_llvm_type = val_type ? @type_mapper.llvm_type(val_type) : nil
          is_undefined = (@value_names[val]?.nil? && @constant_values[val]?.nil?) ||
                         val_llvm_type == "void"
          if is_undefined
            # Undefined value - use safe default
            if @current_return_type == "ptr" || @current_return_type.includes?(".union")
              emit "ret #{@current_return_type} null" if @current_return_type == "ptr"
              if @current_return_type.includes?(".union")
                # Return nil union
                c = @cond_counter
                @cond_counter += 1
                emit "%ret_nil.#{c}.ptr = alloca #{@current_return_type}, align 8"
                emit "%ret_nil.#{c}.type_id_ptr = getelementptr #{@current_return_type}, ptr %ret_nil.#{c}.ptr, i32 0, i32 0"
                emit "store i32 1, ptr %ret_nil.#{c}.type_id_ptr"
                emit "%ret_nil.#{c}.val = load #{@current_return_type}, ptr %ret_nil.#{c}.ptr"
                emit "ret #{@current_return_type} %ret_nil.#{c}.val"
              else
                emit "ret ptr null"
              end
            else
              emit "ret #{@current_return_type} 0"
            end
          else
            val_ref = value_ref(val)
            val_llvm_type = val_type ? @type_mapper.llvm_type(val_type) : "ptr"

          # Check if we need to wrap concrete type in union
          if @current_return_type.includes?(".union") && !val_llvm_type.includes?(".union")
            c = @cond_counter  # Reuse cond_counter for unique naming
            @cond_counter += 1
            emit "%ret#{c}.union_ptr = alloca #{@current_return_type}, align 8"
            emit "%ret#{c}.type_id_ptr = getelementptr #{@current_return_type}, ptr %ret#{c}.union_ptr, i32 0, i32 0"

            # Check if this is a nil/void return - set type_id=1 (nil variant)
            if val_llvm_type == "void" || val_ref == "null"
              emit "store i32 1, ptr %ret#{c}.type_id_ptr"
              # Don't store payload for nil
            else
              # Non-nil value - set type_id=0 and store payload
              emit "store i32 0, ptr %ret#{c}.type_id_ptr"
              emit "%ret#{c}.payload_ptr = getelementptr #{@current_return_type}, ptr %ret#{c}.union_ptr, i32 0, i32 1"
              emit "store #{val_llvm_type} #{val_ref}, ptr %ret#{c}.payload_ptr"
            end
            emit "%ret#{c}.union = load #{@current_return_type}, ptr %ret#{c}.union_ptr"
            emit "ret #{@current_return_type} %ret#{c}.union"
          else
            # For pointer returns, convert integer 0 to null
            # For integer returns, convert null to 0
            # For ptr returns with int value, use inttoptr
            if @current_return_type == "ptr" && val_ref == "0"
              emit "ret ptr null"
            elsif @current_return_type.starts_with?("i") && val_ref == "null"
              emit "ret #{@current_return_type} 0"
            elsif @current_return_type == "ptr" && val_llvm_type && val_llvm_type.starts_with?("i") && !val_llvm_type.includes?(".union")
              # Integer to pointer conversion needed (inttoptr)
              c = @cond_counter
              @cond_counter += 1
              emit "%ret_inttoptr.#{c} = inttoptr #{val_llvm_type} #{val_ref} to ptr"
              emit "ret ptr %ret_inttoptr.#{c}"
            elsif @current_return_type.starts_with?("i") && val_llvm_type == "ptr"
              # Pointer to integer conversion needed (ptrtoint)
              c = @cond_counter
              @cond_counter += 1
              emit "%ret_ptrtoint.#{c} = ptrtoint ptr #{val_ref} to #{@current_return_type}"
              emit "ret #{@current_return_type} %ret_ptrtoint.#{c}"
            elsif @current_return_type.starts_with?("i") && val_llvm_type && val_llvm_type.includes?(".union")
              # Union to integer - extract payload as the integer type
              c = @cond_counter
              @cond_counter += 1
              emit "%ret_union_extract.#{c}.ptr = alloca #{val_llvm_type}, align 8"
              emit "store #{val_llvm_type} #{val_ref}, ptr %ret_union_extract.#{c}.ptr"
              emit "%ret_union_extract.#{c}.payload_ptr = getelementptr #{val_llvm_type}, ptr %ret_union_extract.#{c}.ptr, i32 0, i32 1"
              emit "%ret_union_extract.#{c}.val = load #{@current_return_type}, ptr %ret_union_extract.#{c}.payload_ptr"
              emit "ret #{@current_return_type} %ret_union_extract.#{c}.val"
            else
              emit "ret #{@current_return_type} #{val_ref}"
            end
          end
          end  # End of else block for defined value
        else
          emit "ret void"
        end
      when Jump
        emit "br label %#{@block_names[term.target]}"
      when Branch
        cond = value_ref(term.condition)
        cond_type = @value_types[term.condition]?
        then_block = @block_names[term.then_block]
        else_block = @block_names[term.else_block]

        # Check if condition is a union type - need to extract type_id and compare
        if cond_type && is_union_type?(cond_type)
          # Union layout: { i32 type_id, [N x i8] payload }
          # Extract type_id and compare with 1 (nil variant)
          union_llvm_type = @type_mapper.llvm_type(cond_type)
          c = @cond_counter
          @cond_counter += 1
          emit "%cond#{c}.union_ptr = alloca #{union_llvm_type}, align 8"
          emit "store #{union_llvm_type} #{cond}, ptr %cond#{c}.union_ptr"
          emit "%cond#{c}.type_id_ptr = getelementptr #{union_llvm_type}, ptr %cond#{c}.union_ptr, i32 0, i32 0"
          emit "%cond#{c}.type_id = load i32, ptr %cond#{c}.type_id_ptr"
          # type_id 0 = non-nil (truthy), type_id 1 = nil (falsy)
          emit "%cond#{c}.is_not_nil = icmp ne i32 %cond#{c}.type_id, 1"
          emit "br i1 %cond#{c}.is_not_nil, label %#{then_block}, label %#{else_block}"
        elsif cond_type && @type_mapper.llvm_type(cond_type) == "ptr"
          # Pointer type: compare against null
          c = @cond_counter
          @cond_counter += 1
          emit "%cond#{c}.not_null = icmp ne ptr #{cond}, null"
          emit "br i1 %cond#{c}.not_null, label %#{then_block}, label %#{else_block}"
        elsif cond_type
          cond_llvm_type = @type_mapper.llvm_type(cond_type)
          # Handle void type - treat as ptr (likely nil check)
          if cond_llvm_type == "void"
            c = @cond_counter
            @cond_counter += 1
            emit "%cond#{c}.not_null = icmp ne ptr #{cond}, null"
            emit "br i1 %cond#{c}.not_null, label %#{then_block}, label %#{else_block}"
          elsif cond_llvm_type != "i1"
            # Non-bool type: compare against 0 to get boolean
            c = @cond_counter
            @cond_counter += 1
            emit "%cond#{c}.not_zero = icmp ne #{cond_llvm_type} #{cond}, 0"
            emit "br i1 %cond#{c}.not_zero, label %#{then_block}, label %#{else_block}"
          else
            emit "br i1 #{cond}, label %#{then_block}, label %#{else_block}"
          end
        else
          emit "br i1 #{cond}, label %#{then_block}, label %#{else_block}"
        end
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

    # Check if a type is a union type (for branch condition handling)
    private def is_union_type?(type : TypeRef) : Bool
      # Check MIR module's union_descriptors
      @module.union_descriptors.has_key?(type)
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
