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
      @mangle_cache = {} of String => String
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
        if type.kind.struct? || type.name.starts_with?("StaticArray(")
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
      # IMPORTANT: Check primitive TypeRefs FIRST before registry lookup
      # This prevents primitive types like Float32 from being treated as structs
      # if they happen to be registered in the type registry as class types
      case type_ref
      when TypeRef::VOID    then return "void"
      when TypeRef::NIL     then return "void"
      when TypeRef::BOOL    then return "i1"
      when TypeRef::INT8    then return "i8"
      when TypeRef::INT16   then return "i16"
      when TypeRef::INT32   then return "i32"
      when TypeRef::INT64   then return "i64"
      when TypeRef::INT128  then return "i128"
      when TypeRef::UINT8   then return "i8"
      when TypeRef::UINT16  then return "i16"
      when TypeRef::UINT32  then return "i32"
      when TypeRef::UINT64  then return "i64"
      when TypeRef::UINT128 then return "i128"
      when TypeRef::FLOAT32 then return "float"
      when TypeRef::FLOAT64 then return "double"
      when TypeRef::CHAR    then return "i32"
      when TypeRef::STRING  then return "ptr"
      when TypeRef::SYMBOL  then return "i32"
      when TypeRef::POINTER then return "ptr"
      end

      # For non-primitive types, consult the registry
      if type = @type_registry.get(type_ref)
        # Check if it's a primitive type by NAME (not just TypeRef id)
        # This handles cases where Float32 etc. are registered as class types
        # but should still map to LLVM primitives
        case type.name
        when "Void", "Nil" then return "void"
        when "Bool"        then return "i1"
        when "Int8"        then return "i8"
        when "Int16"       then return "i16"
        when "Int32"       then return "i32"
        when "Int64"       then return "i64"
        when "Int128"      then return "i128"
        when "UInt8"       then return "i8"
        when "UInt16"      then return "i16"
        when "UInt32"      then return "i32"
        when "UInt64"      then return "i64"
        when "UInt128"     then return "i128"
        when "Float32"     then return "float"
        when "Float64"     then return "double"
        when "Char"        then return "i32"
        when "Symbol"      then return "i32"
        end
        compute_llvm_type_for_type(type)
      else
        "ptr"  # Unknown → opaque pointer
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
      when .tuple?                      then "ptr"  # Tuple values are represented by pointer in current ABI
      when .array?                      then compute_array_type(type)
      when .enum?                       then "i32"
      else                                   "ptr"
      end
    end

    private def compute_tuple_type(type : Type) : String
      if elements = type.element_types
        element_types = elements.map { |e|
          t = compute_llvm_type_for_type(e)
          t == "void" ? "i8" : t  # Preserve struct index positions for Nil elements
        }
        return "{}" if element_types.empty?
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
      if cached = @mangle_cache[name]?
        return cached
      end
      result = mangle_name_uncached(name)
      @mangle_cache[name] = result
      result
    end

    private def mangle_name_uncached(name : String) : String
      # LLVM intrinsics must keep their dots (e.g., "llvm.bswap.i32")
      # These are special built-in functions that LLVM recognizes by exact name
      return name if name.starts_with?("llvm.")

      # Escape-encoding for readable, collision-free, portable symbol names.
      # Multi-character tokens must be checked first (longest match).
      result = String::Builder.new(name.bytesize)
      i = 0
      while i < name.bytesize
        # Check multi-char operators first (order matters: longest first)
        remaining = name.bytesize - i
        if remaining >= 3
          three = name[i, 3]
          case three
          when "<=>" then result << "$CMP"; i += 3; next
          when "[]=", "[]?" then result << (three == "[]=" ? "$IDXS" : "$IDXQ"); i += 3; next
          end
        end
        if remaining >= 2
          two = name[i, 2]
          case two
          when "<=" then result << "$LE"; i += 2; next
          when ">=" then result << "$GE"; i += 2; next
          when "==" then result << "$EQ"; i += 2; next
          when "!=" then result << "$NE"; i += 2; next
          when "=~" then result << "$MATCH"; i += 2; next
          when "!~" then result << "$NMATCH"; i += 2; next
          when "<<" then result << "$SHL"; i += 2; next
          when ">>" then result << "$SHR"; i += 2; next
          when "**" then result << "$POW"; i += 2; next
          when "[]" then result << "$IDX"; i += 2; next
          when "->" then result << "$AR"; i += 2; next
          when "::" then result << "$CC"; i += 2; next  # Colon Colon (namespace)
          end
        end
        # Single character
        ch = name.byte_at(i).unsafe_chr
        case ch
        when 'a'..'z', 'A'..'Z', '0'..'9', '_'
          result << ch
        when '<'  then result << "$LT"
        when '>'  then result << "$GT"
        when '+'  then result << "$ADD"
        when '-'  then result << "$SUB"
        when '*'  then result << "$MUL"
        when '/'  then result << "$DIV"
        when '%'  then result << "$MOD"
        when '&'  then result << "$AND"
        when '|'  then result << "$OR"
        when '^'  then result << "$XOR"
        when '~'  then result << "$NOT"
        when '='  then result << "$SET"
        when '!'  then result << "$BANG"
        when '?'  then result << "$Q"
        when '('  then result << "$L"
        when ')'  then result << "$R"
        when ','  then result << "$C"
        when '#'  then result << "$H"
        when '.'  then result << "$D"
        when ' '  then result << "$_"
        when '@'  then result << "$AT"
        when '['  then result << "$BL"  # Bracket Left
        when ']'  then result << "$BR"  # Bracket Right
        when '{'  then result << "$YL"  # brace Left
        when '}'  then result << "$YR"  # brace Right
        when ':'  then result << "$CO"  # COlon
        when ';'  then result << "$SC"  # SemiColon
        when '\'' then result << "$SQ"  # Single Quote
        when '"'  then result << "$DQ"  # Double Quote
        when '\\' then result << "$BS"  # BackSlash
        when '$'  then result << "$$"   # Escape $ itself
        else
          # Fallback: hex encode unknown characters
          result << "$x"
          result << ch.ord.to_s(16).rjust(2, '0')
        end
        i += 1
      end
      result.to_s
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # LLVM IR TEXT GENERATOR
  # ═══════════════════════════════════════════════════════════════════════════

  # Bare iterator method names that are suppressed at call sites.
  # These are yield-based Enumerable/Indexable methods that weren't lowered
  # as intrinsics. The calls pass block procs but the methods are empty stubs —
  # suppressing the calls is safe and avoids LLVM type conflicts.
  BARE_ITERATOR_METHODS = Set{
    "each", "map", "map_with_index", "each_with_index",
    "each_with_index$Int32", "each_entry_with_index", "sum",
  }

  class LLVMIRGenerator
    @module : Module
    @type_mapper : LLVMTypeMapper
    @output : IO::Memory
    @toplevel_output : IO::Memory? = nil  # Route top-level defs here during block buffering
    @indent : Int32
    @value_names : Hash(ValueId, String)
    @block_names : Hash(BlockId, String)
    @current_return_type : String = "void"
    @current_return_type_ref : TypeRef = TypeRef::VOID
    @current_func_name : String = ""
    @current_func_params : Array(Parameter) = [] of Parameter
    @current_slab_frame : Bool = false
    @tsan_needs_func_entry : Bool = false
    @constant_values : Hash(ValueId, String)  # For inlining constants
    @value_types : Hash(ValueId, TypeRef)     # For tracking operand types
    @void_values : Set(ValueId) = Set(ValueId).new  # Values emitted as void (no LLVM result)
    @emitted_functions : Set(String) = Set(String).new  # Track emitted function names to avoid duplicates
    @emitted_function_return_types : Hash(String, String) = {} of String => String  # Track emitted function return types for call-site consistency
    @undefined_externs : Hash(String, String) = {} of String => String  # Track undefined extern calls (name => return_type)
    @called_crystal_functions : Hash(String, {String, Int32, Array(String)}) = {} of String => {String, Int32, Array(String)}  # Track called Crystal functions (name => {return_type, arg_count, arg_types}) for missing declaration generation
    @global_name_mapping : Hash(String, String) = {} of String => String  # Map original global names to renamed names
    @global_declared_types : Hash(String, String) = {} of String => String  # Global name -> declared LLVM type
    @alloc_element_types : Hash(ValueId, TypeRef)  # For GEP element type lookup
    @array_info : Hash(ValueId, {String, Int32})  # Array element_type and size
    @string_constants : Hash(String, String)  # String value -> global name
    @module_singleton_globals : Hash(TypeRef, String)  # Module type -> singleton global name
    @emitted_value_types : Hash(String, String)  # SSA name -> LLVM type (per function)
    @emitted_allocas : Set(ValueId) = Set(ValueId).new  # Track pre-emitted allocas
    @alloc_types : Hash(ValueId, TypeRef) = {} of ValueId => TypeRef  # Track original alloc_type for enum detection
    @inttoptr_value_ids : Set(ValueId) = Set(ValueId).new  # Track values created by inttoptr (packed scalars)
    @addressable_allocas : Hash(ValueId, String) = {} of ValueId => String  # operand_id -> alloca SSA name
    @addressable_alloca_initialized : Set(ValueId) = Set(ValueId).new  # Track which addressable allocas have been stored to
    @pending_allocas : Array({String, String, Int32}) = [] of {String, String, Int32}  # name, type, align
    @string_counter : Int32 = 0
    @cond_counter : Int32 = 0  # For unique branch condition variable names

    C_LIBRARY_FUNCTIONS = Set{
      "printf", "sprintf", "snprintf", "fprintf", "vprintf",
      "vsprintf", "vsnprintf", "vfprintf", "scanf", "sscanf",
      "fscanf", "puts", "fputs", "fgets", "gets",
      "malloc", "calloc", "realloc", "free",
      "memcpy", "memmove", "memset", "memcmp",
      "strlen", "strcpy", "strncpy", "strcat", "strncat",
      "strcmp", "strncmp", "strchr", "strrchr", "strstr",
      "exit", "abort", "atexit", "_exit",
      "setjmp", "longjmp", "sigsetjmp", "siglongjmp",
      "open", "close", "read", "write", "lseek",
      "fopen", "fclose", "fread", "fwrite", "fseek", "ftell",
      "getenv", "setenv", "unsetenv", "system"
    }

    private def mangle_function_name(name : String) : String
      mangled = @type_mapper.mangle_name(name)
      return "__crystal_v2_fn_#{mangled}" if C_LIBRARY_FUNCTIONS.includes?(mangled)
      mangled
    end

    private def unsigned_type_ref?(type_ref : TypeRef) : Bool
      type_ref == TypeRef::UINT8 ||
        type_ref == TypeRef::UINT16 ||
        type_ref == TypeRef::UINT32 ||
        type_ref == TypeRef::UINT64 ||
        type_ref == TypeRef::UINT128
    end

    # String type_id for runtime helpers and string literals
    @string_type_id : Int32 = 16  # TypeRef::STRING.id default, updated during prelude emission

    # Cross-block value tracking for dominance fix
    @value_def_block : Hash(ValueId, BlockId) = {} of ValueId => BlockId  # value → block where defined
    @cross_block_values : Set(ValueId) = Set(ValueId).new  # values that need alloca slots
    @cross_block_slots : Hash(ValueId, String) = {} of ValueId => String  # value → alloca slot name
    @cross_block_slot_types : Hash(ValueId, String) = {} of ValueId => String  # value → slot LLVM type
    @cross_block_slot_type_refs : Hash(ValueId, TypeRef) = {} of ValueId => TypeRef  # value → slot TypeRef (signedness)
    @in_phi_mode : Bool = false  # When true, value_ref returns default instead of emitting load
    @in_phi_block : Bool = false  # When true, we're emitting phi instructions (defer cross-block stores)
    @deferred_phi_stores : Array(String) = [] of String  # Stores to emit after all phis
    @deferred_phi_store_ops : Array({ValueId, String, String}) = [] of {ValueId, String, String}  # {inst_id, value_name, slot_name}

    # Phi-related type conversions: ExternCall values that need zext for phi compatibility
    # Maps value_id -> (from_bits, to_bits) where we need zext from iN to iM
    @phi_zext_conversions : Hash(ValueId, {Int32, Int32}) = {} of ValueId => {Int32, Int32}
    @zext_value_names : Hash(ValueId, String) = {} of ValueId => String  # Extended value names

    # Phi predecessor loads: for cross-block values in phi incomings, we emit loads
    # in predecessor blocks before terminators. Maps (pred_block, value_id) -> loaded_name
    @phi_predecessor_loads : Hash({BlockId, ValueId}, String) = {} of {BlockId, ValueId} => String

    # Phi predecessor conversions: for fixed-type values (params, ExternCalls) that need
    # type conversion for phi compatibility. Maps (pred_block, value_id) -> (converted_name, from_bits, to_bits)
    @phi_predecessor_conversions : Hash({BlockId, ValueId}, {String, Int32, Int32}) = {} of {BlockId, ValueId} => {String, Int32, Int32}
    # Phi predecessor union wraps: for ptr/void values that must be wrapped into union before phi
    # Maps (pred_block, value_id) -> (wrapped_name, union_type_ref, variant_type_id)
    @phi_predecessor_union_wraps : Hash({BlockId, ValueId}, {String, TypeRef, Int32}) = {} of {BlockId, ValueId} => {String, TypeRef, Int32}
    # Phi predecessor union-to-ptr extracts: for union values that must be unwrapped to ptr before phi
    # Maps (pred_block, value_id) -> (extract_name, union_llvm_type)
    @phi_union_to_ptr_extracts : Hash({BlockId, ValueId}, {String, String}) = {} of {BlockId, ValueId} => {String, String}
    # Phi union-to-union converts: for union values that must be reinterpreted as a different union type
    # Maps (pred_block, value_id) -> array of (convert_name, src_union_llvm_type, dst_union_llvm_type)
    # An array is needed because the same value may be used by multiple phi nodes expecting different types
    @phi_union_to_union_converts : Hash({BlockId, ValueId}, Array({String, String, String})) = {} of {BlockId, ValueId} => Array({String, String, String})
    # Phi union payload extracts: for union-typed slot values where phi expects a primitive
    # Maps (pred_block, value_id) -> (extract_name, load_name, target_llvm_type)
    @phi_union_payload_extracts : Hash({BlockId, ValueId}, {String, String, String}) = {} of {BlockId, ValueId} => {String, String, String}
    @current_func_blocks : Hash(BlockId, BasicBlock) = {} of BlockId => BasicBlock
    @current_block_id : BlockId? = nil
    @entry_user_block_id : BlockId = 0_u32  # First user block — dominates all others

    # Track phi nodes that have nil incoming values (for union return type handling)
    # Maps phi value_id -> set of blocks that contribute nil
    @phi_nil_incoming_blocks : Hash(ValueId, Set(BlockId)) = {} of ValueId => Set(BlockId)

    # Track emitted union type definitions to avoid duplicates and ensure forward declarations
    @emitted_union_type_names : Set(String) = Set(String).new

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
    property progress : Bool = false   # Print progress during generation
    property reachability : Bool = false  # Only emit reachable functions (from main) - DISABLED, needs HIR-level implementation
    property no_prelude : Bool = false   # --no-prelude mode: emit C strings instead of Crystal String objects
    property constant_initial_values : Hash(String, Float64 | Int64) = {} of String => (Float64 | Int64)  # Constant literal values for globals (e.g., Math::PI)
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
      @module_singleton_globals = {} of TypeRef => String
      @emitted_value_types = {} of String => String

      # Type metadata
      @type_info_entries = [] of TypeInfoEntry
      @field_info_entries = [] of FieldInfoEntry
      @union_info_entries = [] of UnionInfoEntry
      @union_variant_entries = [] of UnionVariantInfoEntry
      @string_table = IO::Memory.new
      @string_offsets = {} of String => UInt32
    end

    @[AlwaysInline]
    private def sanitize_llvm_local_name(name : String) : String
      return "arg" if name.empty?

      ptr = name.to_unsafe
      n = name.bytesize
      needs_sanitize = false
      i = 0
      while i < n
        b = ptr[i]
        valid = if i == 0
                  (b >= 'a'.ord && b <= 'z'.ord) ||
                    (b >= 'A'.ord && b <= 'Z'.ord) ||
                    b == '_'.ord || b == '.'.ord
                else
                  (b >= 'a'.ord && b <= 'z'.ord) ||
                    (b >= 'A'.ord && b <= 'Z'.ord) ||
                    (b >= '0'.ord && b <= '9'.ord) ||
                    b == '_'.ord || b == '.'.ord
                end
        unless valid
          needs_sanitize = true
          break
        end
        i += 1
      end

      return name unless needs_sanitize

      String.build(n) do |io|
        i = 0
        while i < n
          b = ptr[i]
          valid = if i == 0
                    (b >= 'a'.ord && b <= 'z'.ord) ||
                      (b >= 'A'.ord && b <= 'Z'.ord) ||
                      b == '_'.ord || b == '.'.ord
                  else
                    (b >= 'a'.ord && b <= 'z'.ord) ||
                      (b >= 'A'.ord && b <= 'Z'.ord) ||
                      (b >= '0'.ord && b <= '9'.ord) ||
                      b == '_'.ord || b == '.'.ord
                  end
          io << (valid ? b.unsafe_chr : '_')
          i += 1
        end
      end
    end

    @[AlwaysInline]
    private def current_func_param_type_by_llvm_name(name : String) : TypeRef?
      @current_func_params.each do |param|
        llvm_name = @value_names[param.index]? || sanitize_llvm_local_name(param.name)
        if llvm_name == name
          return param.type == TypeRef::VOID ? TypeRef::POINTER : param.type
        end
      end
      nil
    end

    @[AlwaysInline]
    private def posix_open_flag_creat : Int32
      # Linux/Android use O_CREAT=64, Darwin/BSD use O_CREAT=0x200.
      if @target_triple.includes?("linux") || @target_triple.includes?("android")
        64
      else
        0x200
      end
    end

    @[AlwaysInline]
    private def posix_open_flag_trunc : Int32
      # Linux/Android use O_TRUNC=512, Darwin/BSD use O_TRUNC=0x400.
      if @target_triple.includes?("linux") || @target_triple.includes?("android")
        512
      else
        0x400
      end
    end

    @[AlwaysInline]
    private def posix_open_flag_append : Int32
      # Linux/Android use O_APPEND=1024, Darwin/BSD use O_APPEND=0x8.
      if @target_triple.includes?("linux") || @target_triple.includes?("android")
        1024
      else
        0x8
      end
    end

    @[AlwaysInline]
    private def pointer_sized_int_llvm_type : String
      # Most currently supported targets are 64-bit. Keep 32-bit fallback explicit.
      if @target_triple.includes?("i386") || @target_triple.includes?("armv7") || @target_triple.includes?("wasm32")
        "i32"
      else
        "i64"
      end
    end

    @[AlwaysInline]
    private def pointer_word_bytes_u64 : UInt64
      pointer_sized_int_llvm_type == "i32" ? 4_u64 : 8_u64
    end

    private def container_elem_storage_size_u64(elem_type : Type?) : UInt64
      return pointer_word_bytes_u64 unless elem_type

      is_inline = elem_type.kind.primitive? || elem_type.kind.enum?
      if is_inline && elem_type.size > 0
        return elem_type.size
      end

      # Unions with explicit payload layout are stored inline.
      if elem_type.kind.union? && elem_type.size > pointer_word_bytes_u64
        return elem_type.size
      end

      # Classes/structs/non-inline values are represented as pointers in containers.
      pointer_word_bytes_u64
    end

    def generate : String
      STDERR.puts "  [LLVM] emit_header..." if @progress
      emit_header
      STDERR.puts "  [LLVM] emit_type_definitions..." if @progress
      emit_type_definitions
      STDERR.puts "  [LLVM] emit_runtime_declarations..." if @progress
      emit_runtime_declarations
      STDERR.puts "  [LLVM] emit_union_debug_helpers..." if @progress
      emit_union_debug_helpers
      STDERR.puts "  [LLVM] emit_global_variables..." if @progress
      emit_global_variables

      if @emit_type_metadata
        STDERR.puts "  [LLVM] collect_type_metadata..." if @progress
        collect_type_metadata
        STDERR.puts "  [LLVM] collect_union_metadata..." if @progress
        collect_union_metadata
      end

      # Determine which functions to emit
      functions_to_emit = if @reachability
                            STDERR.puts "  [LLVM] computing reachable functions..." if @progress
                            reachable_ids = compute_reachable_functions
                            @module.functions.select { |f| reachable_ids.includes?(f.id) }
                          else
                            @module.functions
                          end

      # Filter out functions with unresolved type patterns (typeof, unsubstituted type params)
      # These are functions that were partially created but couldn't be monomorphized
      unresolved_patterns = ["typeof("]

      # Build a skip set by FunctionId so we can propagate skips through the call graph.
      skip_ids = Set(FunctionId).new

      # 1) Directly unresolved: function name contains unresolved type patterns.
      functions_to_emit.each do |func|
        skip_ids << func.id if unresolved_patterns.any? { |p| func.name.includes?(p) }
      end

      # 2) Local unresolved: function body references unresolved type patterns via extern calls.
      functions_to_emit.each do |func|
        next if skip_ids.includes?(func.id)
        func.blocks.each do |block|
          block.instructions.each do |inst|
            case inst
            when ExternCall
              if unresolved_patterns.any? { |p| inst.extern_name.includes?(p) }
                skip_ids << func.id
                break
              end
            end
          end
          break if skip_ids.includes?(func.id)
        end
      end

      # 3) Propagate: skip any function that depends on a skipped function.
      # This prevents emitting reachable callers that would otherwise reference missing symbols.
      changed = true
      while changed
        changed = false
        functions_to_emit.each do |func|
          next if skip_ids.includes?(func.id)
          func.blocks.each do |block|
            block.instructions.each do |inst|
              case inst
              when Call
                if skip_ids.includes?(inst.callee)
                  skip_ids << func.id
                  changed = true
                  break
                end
              when RCDecrement
                if destructor_id = inst.destructor
                  if skip_ids.includes?(destructor_id)
                    skip_ids << func.id
                    changed = true
                    break
                  end
                end
              end
            end
            break if skip_ids.includes?(func.id)
          end
        end
      end

      # Apply filter
      functions_to_emit = functions_to_emit.reject { |func| skip_ids.includes?(func.id) }

      # Pre-compute return types for ALL module functions.  This fixes a class of bugs
      # where the MIR return_type is NIL (void) for methods that actually return values
      # (e.g., String? getters encoded as Nil).  By scanning Return instructions we detect
      # functions that have return values and override the type to "ptr" before emission
      # begins.  Both emit_function (function definition) and emit_call (call sites) then
      # see the corrected type, keeping LLVM IR consistent.
      precompute_function_return_types(@module.functions) unless ENV["CRYSTAL_V2_NO_PRECOMPUTE"]?

      total_funcs = functions_to_emit.size
      STDERR.puts "  [LLVM] emitting #{total_funcs} functions (#{@module.functions.size} total, #{@module.functions.size - total_funcs} pruned)..." if @progress
      functions_to_emit.each_with_index do |func, idx|
        if @progress && (idx % 100 == 0 || idx == total_funcs - 1)
          STDERR.puts "    Emitting function #{idx + 1}/#{total_funcs}: #{func.name}"
        end
        begin
          emit_function(func)
        rescue ex : IndexError
          raise "Index error in emit_function for: #{func.name}\n#{ex.message}"
        end
      end

      # Second pass: iteratively emit functions discovered by call sites during the first pass.
      # After emitting reachable functions, @called_crystal_functions may contain functions
      # that exist in the module but weren't reached by the RTA name-resolution.
      # Common issue: call sites use short names (e.g. "HIR$CCAstToHir$Dnew$$...")
      # while module functions use full names (e.g. "Crystal$CCHIR$CCAstToHir$Dnew$$...").
      # Iterate until no new functions are discovered.
      if @reachability
        func_by_mangled = {} of String => Function
        # Suffix index: for "Crystal$CCHIR$CCFoo" also index "$CCHIR$CCFoo", "HIR$CCFoo", "$CCFoo"
        func_by_suffix = {} of String => Function
        @module.functions.each do |f|
          mangled = mangle_function_name(f.name)
          func_by_mangled[mangled] = f
          # Index all $CC-separated suffixes for namespace-prefix matching
          search_from = 0
          while cc_pos = mangled.index("$CC", search_from)
            suffix = mangled[cc_pos + 3..]
            func_by_suffix[suffix] = f unless func_by_suffix.has_key?(suffix)
            search_from = cc_pos + 3
          end
        end

        max_passes = 20
        pass = 0
        loop do
          pass += 1
          break if pass > max_passes

          newly_found = [] of Function
          @called_crystal_functions.each do |name, _|
            next if @emitted_functions.includes?(name)
            next if @undefined_externs.has_key?(name)
            # Try to find this function in the module
            # 1. Exact mangled match
            func = func_by_mangled[name]?
            # 2. Suffix match: call site "HIR$CCAstToHir$Dnew" matches module "Crystal$CCHIR$CCAstToHir$Dnew"
            unless func
              func = func_by_suffix[name]?
            end
            # 3. Strip arg suffix and try suffix match on base
            unless func
              if dd_idx = name.index("$$")
                base = name[0, dd_idx]
                func = func_by_suffix[base]?
              end
            end
            if func
              next if skip_ids.includes?(func.id)
              newly_found << func
            end
          end

          break if newly_found.empty?
          STDERR.puts "  [LLVM] Pass #{pass + 1}: emitting #{newly_found.size} additional functions..." if @progress
          newly_found.each do |func|
            begin
              emit_function(func)
            rescue ex : IndexError
              # Skip functions that fail during emission
            end
          end
        end
      end

      emit_entrypoint_if_needed(functions_to_emit)

      # Emit string constants at end (LLVM allows globals anywhere)
      STDERR.puts "  [LLVM] emit_string_constants..." if @progress
      emit_string_constants

      # Emit varargs stubs for bare iterator methods not already defined
      emit_bare_iterator_stubs

      # Emit declarations for undefined extern calls
      STDERR.puts "  [LLVM] emit_undefined_extern_declarations..." if @progress
      emit_undefined_extern_declarations

      # Emit stubs for any Crystal functions called but not defined.
      # This catches functions that exist in MIR but were skipped during emission
      # (e.g., due to unresolved type patterns or transitive skip propagation).
      emit_missing_crystal_function_stubs

      if @emit_type_metadata
        STDERR.puts "  [LLVM] emit_type_metadata_globals..." if @progress
        emit_type_metadata_globals
        STDERR.puts "  [LLVM] emit_union_metadata_globals..." if @progress
        emit_union_metadata_globals
      end

      # Always emit type name table (needed for self.class at runtime)
      STDERR.puts "  [LLVM] emit_type_name_table..." if @progress
      emit_type_name_table

      STDERR.puts "  [LLVM] finalizing output..." if @progress
      @output.to_s
    end

    # Pre-compute return types for all functions.
    # Detects functions whose MIR return_type is NIL (→ "void") but which actually
    # return values (have Return instructions with a value).  For those functions the
    # return type is corrected to "ptr", ensuring that both the function definition
    # and call sites use a consistent non-void return type.
    #
    # When MIR return_type is NIL the function typically returns a nilable or union
    # type — these are always boxed as pointers.  We always use "ptr" rather than
    # the defining instruction's scalar type (i32/i64) to avoid cross-block slot
    # type mismatches downstream.
    #
    # Two-phase approach:
    # 1. Scan all functions' Return instructions.  For void-returning functions that
    #    have Return(value), set return type to "ptr".
    # 2. Scan all call sites.  If a call instruction has a non-void type for the
    #    callee that is still void, upgrade to "ptr".
    private def precompute_function_return_types(functions : Array(Function))
      # Build function lookup by ID for Call instruction resolution
      func_by_id = {} of FunctionId => Function
      functions.each { |f| func_by_id[f.id] = f }

      # Phase 1: Record the declared return type for every function.
      # We do NOT blindly upgrade void→ptr here because Crystal methods
      # nearly always have Return(value) (last expression) even when void.
      functions.each do |func|
        mangled = mangle_function_name(func.name)
        return_type = @type_mapper.llvm_type(func.return_type)
        @emitted_function_return_types[mangled] = return_type
      end

      # Phase 2: Scan call sites (Call only, not ExternCall) for better type info.
      # When a Call instruction's type is non-void and the callee is still void,
      # upgrade to "ptr".
      if !ENV["CRYSTAL_V2_NO_PRECOMPUTE_P2"]?
        functions.each do |func|
          func.blocks.each do |block|
            block.instructions.each do |inst|
              if inst.is_a?(Call)
                callee_func = func_by_id[inst.callee]?
                next unless callee_func
                callee_mangled = mangle_function_name(callee_func.name)
                call_type = @type_mapper.llvm_type(inst.type)
                next if call_type == "void"
                # Only upgrade void → ptr; don't downgrade existing non-void types
                existing = @emitted_function_return_types[callee_mangled]?
                if existing == "void"
                  @emitted_function_return_types[callee_mangled] = "ptr"
                  STDERR.puts "  [PRECOMPUTE-P2] #{callee_mangled}: void → ptr (call-site type: #{call_type})" if ENV["CRYSTAL_V2_PRECOMPUTE_DEBUG"]?
                end
              end
            end
          end
        end
      end
    end

    # Compute reachable functions from main via transitive call graph
    private def compute_reachable_functions : Set(FunctionId)
      reachable = Set(FunctionId).new
      worklist = [] of FunctionId

      # Find entry functions (Crystal uses __crystal_main as the synthetic entry).
      roots = @module.functions.select { |f| f.name == "main" || f.name == "__crystal_main" }
      if roots.empty?
        # No entry, emit all functions
        @module.functions.each { |f| reachable << f.id }
        return reachable
      end

      roots.each do |root|
        worklist << root.id
        reachable << root.id
      end

      # Build function lookup by ID and by name
      func_by_id = {} of FunctionId => Function
      func_by_name = {} of String => Function
      # Suffix index: handles namespace prefix mismatches where call sites use short
      # names (e.g. "HIR::AstToHir.new") but module has full names ("Crystal::HIR::AstToHir.new")
      func_by_suffix = {} of String => Function
      @module.functions.each do |f|
        func_by_id[f.id] = f
        mangled = @type_mapper.mangle_name(f.name)
        func_by_name[mangled] = f
        func_by_name[f.name] = f
        # Index all $CC-separated (::) suffixes for namespace-prefix matching
        search_from = 0
        while cc_pos = mangled.index("$CC", search_from)
          suffix = mangled[cc_pos + 3..]
          func_by_suffix[suffix] = f unless func_by_suffix.has_key?(suffix)
          search_from = cc_pos + 3
        end
      end

      # Traverse call graph
      while func_id = worklist.shift?
        func = func_by_id[func_id]?
        next unless func  # External function - skip

        # Scan all blocks for Call and other function-referencing instructions
        func.blocks.each do |block|
          block.instructions.each do |inst|
            case inst
            when Call
              callee_id = inst.callee
              unless reachable.includes?(callee_id)
                reachable << callee_id
                worklist << callee_id
              end
            when ExternCall
              # ExternCall uses name - resolve to function ID if it's a defined function
              extern_name = inst.extern_name
              mangled_extern = @type_mapper.mangle_name(extern_name)

              # Try to find matching function by exact name or mangled name
              matching_func = func_by_name[mangled_extern]? || func_by_name[extern_name]?

              # Suffix match: handles namespace prefix mismatches
              # e.g. call to "HIR$CCAstToHir$Dnew" matches "Crystal$CCHIR$CCAstToHir$Dnew"
              unless matching_func
                matching_func = func_by_suffix[mangled_extern]? || func_by_suffix[extern_name]?
              end

              if matching_func
                unless reachable.includes?(matching_func.id)
                  reachable << matching_func.id
                  worklist << matching_func.id
                end
              else
                # DEBUG: log truly unresolved extern calls (not in module)
                if @progress
                  STDERR.puts "  [REACHABILITY] Unresolved extern: #{extern_name} (mangled: #{mangled_extern})"
                end
              end
            when FuncPointer
              # C callback function pointer — resolve by name
              matching_func = func_by_name[inst.func_name]? || func_by_name[@type_mapper.mangle_name(inst.func_name)]?
              if matching_func
                unless reachable.includes?(matching_func.id)
                  reachable << matching_func.id
                  worklist << matching_func.id
                end
              end
            when RCDecrement
              # Destructor is also a function reference
              if destructor_id = inst.destructor
                unless reachable.includes?(destructor_id)
                  reachable << destructor_id
                  worklist << destructor_id
                end
              end
            end
          end
        end
      end

      reachable
    end

    private def emit_undefined_extern_declarations
      return if @undefined_externs.empty?

      # Build set of already declared/defined functions
      already_declared = @emitted_functions.dup
      # Add runtime declarations (from emit_runtime_declarations and emit_string_runtime_functions)
      already_declared << "malloc" << "calloc" << "realloc" << "free"
      already_declared << "printf" << "puts" << "exit" << "abort"
      already_declared << "strlen" << "strcpy" << "strcat" << "sprintf"
      already_declared << "strstr" << "write" << "strchr" << "strtod" << "snprintf"
      already_declared << "strcmp" << "memcmp" << "qsort"
      already_declared << "open" << "lseek" << "read" << "close"
      already_declared << "strtol" << "strtoull"
      already_declared << "setjmp" << "longjmp"
      # Crystal v2 runtime functions
      already_declared << "__crystal_v2_raise" << "__crystal_v2_int_to_string"
      already_declared << "__crystal_v2_string_concat" << "__crystal_v2_string_interpolate" << "__crystal_v2_f64_to_string"
      already_declared << "__crystal_v2_char_to_string" << "__crystal_v2_malloc64"
      already_declared << "__crystal_v2_init_buffer" << "__crystal_v2_string_repeat"
      already_declared << "__crystal_v2_string_index_string" << "__crystal_v2_string_index_char"
      already_declared << "__crystal_v2_string_gsub" << "__crystal_v2_string_gsub_char" << "__crystal_v2_string_byte_slice"
      already_declared << "__crystal_v2_array_join_int32"
      already_declared << "__crystal_v2_string_bytesize" << "__crystal_v2_string_byte_at"
      already_declared << "__crystal_v2_array_new_filled_i32" << "__crystal_v2_array_new_filled_bool"
      already_declared << "__crystal_v2_array_concat"
      already_declared << "__crystal_v2_hash_new"
      already_declared << "__crystal_v2_ptr_copy" << "__crystal_v2_ptr_move"
      already_declared << "__crystal_v2_sort_i32_array" << "__crystal_v2_sort_string_array"
      already_declared << "__crystal_v2_sort_i32_array_dup" << "__crystal_v2_sort_string_array_dup"
      already_declared << "__cmp_i32" << "__cmp_string"
      already_declared << "llvm.memcpy.p0.p0.i32" << "llvm.memcpy.p0.p0.i64" << "llvm.memmove.p0.p0.i64"
      # PCRE2 functions (declared in emit_runtime_declarations)
      already_declared << "pcre2_compile_8" << "pcre2_match_8"
      already_declared << "pcre2_match_data_create_from_pattern_8"
      already_declared << "pcre2_jit_compile_8" << "pcre2_get_ovector_pointer_8"
      already_declared << "pcre2_get_ovector_count_8" << "pcre2_match_data_free_8"
      # C stdlib functions already declared in emit_runtime_declarations
      already_declared << "getcwd" << "realpath"
      # Skip any function starting with __crystal_v2_ (runtime functions)
      runtime_prefix = "__crystal_v2_"

      emit_raw "\n; Forward declarations for undefined external functions\n"
      @undefined_externs.each do |name, return_type|
        # Skip if already emitted or declared
        next if already_declared.includes?(name)
        # Skip runtime functions
        next if name.starts_with?(runtime_prefix)
        already_declared << name

        # If the function also appears in @called_crystal_functions AND is a
        # V2-mangled Crystal function (contains $), prefer the call-site type info
        # (which has correct return type and arg types) over the ExternCall info
        # (which may have void return for unused results).
        # External library functions (GC, libc, etc.) must stay as declare.
        call_info = name.includes?("$") ? @called_crystal_functions[name]? : nil
        if call_info
          better_ret = call_info[0]
          return_type = better_ret if return_type == "void" && better_ret != "void"
        end

        # Tuple#[] — the dead-code stub returns 0 which breaks tuple case/when
        # matching (e.g., `case {num, precision}` in Int#to_s).
        # Implement as i32 element access: ptr + index * 4.
        if name == "$IDX"
          emit_raw "; Tuple#[] — runtime tuple element access (i32 elements)\n"
          emit_raw "define i32 @$IDX(ptr %arg0, i32 %arg1) {\n"
          emit_raw "  %idx64 = sext i32 %arg1 to i64\n"
          emit_raw "  %elem_ptr = getelementptr i32, ptr %arg0, i64 %idx64\n"
          emit_raw "  %value = load i32, ptr %elem_ptr\n"
          emit_raw "  ret i32 %value\n"
          emit_raw "}\n"
          next
        end

        # LLVM intrinsics need proper signatures (not varargs)
        if name.starts_with?("llvm.")
          # Normalize old typed pointer format (p0i8) to opaque pointer format (p0) for LLVM 15+
          normalized_name = name
            .gsub("p0i8.p0i8", "p0.p0")
            .gsub("p0i8", "p0")
          decl = emit_llvm_intrinsic_declaration(normalized_name)
          emit_raw "#{decl}\n" if decl
        elsif call_info
          # Use call-site info for proper arg types and return type
          stub = emit_dead_code_stub(name, call_info[0], call_info[1], call_info[2])
          emit_raw stub if stub
        elsif stub = emit_dead_code_stub(name, return_type)
          # Emit a stub function body for methods on impossible receivers
          # (Nil, Unknown, wrong-type dispatch). These arise from type inference
          # gaps where the compiler emits calls that would never execute at runtime.
          emit_raw stub
        elsif name.matches?(/\Ast_[a-z]/) || name == "st_mtimespec"
          # C struct field accessors (e.g., st_mtimespec from struct stat) that are
          # incorrectly lowered as function calls instead of GEP+load. Provide no-op stubs.
          emit_raw "define #{return_type} @#{name}(...) {\n"
          if return_type == "void"
            emit_raw "  ret void\n"
          elsif return_type == "ptr"
            emit_raw "  ret ptr null\n"
          elsif return_type.starts_with?('i')
            emit_raw "  ret #{return_type} 0\n"
          else
            emit_raw "  ret #{return_type} zeroinitializer\n"
          end
          emit_raw "}\n"
        else
          # External library functions (GC, libc, system) must stay as 'declare' so
          # the linker resolves them to the real implementations. Only V2-mangled
          # Crystal functions (those containing $) get stub bodies.
          if name.includes?("$")
            ret_stmt = case return_type
                       when "void"   then "ret void"
                       when "ptr"    then "ret ptr null"
                       when "i1"     then "ret i1 0"
                       when "i8"     then "ret i8 0"
                       when "i16"    then "ret i16 0"
                       when "i32"    then "ret i32 0"
                       when "i64"    then "ret i64 0"
                       when "float"  then "ret float 0.0"
                       when "double" then "ret double 0.0"
                       else               "ret #{return_type} zeroinitializer"
                       end
            emit_raw "define #{return_type} @#{name}(...) {\n"
            emit_raw "  #{ret_stmt}\n"
            emit_raw "}\n"
          else
            emit_raw "declare #{return_type} @#{name}(...)\n"
          end
        end
      end
    end

    private def emit_missing_crystal_function_stubs
      # Skip functions already declared in emit_runtime_declarations or emit_header
      already_declared = Set(String).new
      already_declared << "getcwd" << "realpath" << "open" << "close" << "read" << "lseek"
      already_declared << "write" << "malloc" << "calloc" << "realloc" << "free" << "memcpy" << "memmove" << "memset"
      already_declared << "strlen" << "strcmp" << "strncmp" << "strstr" << "strerror" << "snprintf" << "exit" << "abort"
      already_declared << "puts" << "printf" << "fwrite" << "fflush"
      missing = @called_crystal_functions.reject { |name, _| @emitted_functions.includes?(name) || @undefined_externs.has_key?(name) || already_declared.includes?(name) }
      return if missing.empty?
      emit_raw "\n; Forward declarations for Crystal functions called but not defined\n"
      missing.each do |name, info|
        return_type = info[0]
        arg_count = info[1]
        arg_types = info[2]
        # Emit a stub function that returns a zero/null value.
        # These are functions the RTA missed or that were skipped during emission.
        if stub = emit_dead_code_stub(name, return_type, arg_count, arg_types)
          emit_raw stub
        else
          # Emit declare with matching arg count and types to avoid type mismatch
          param_list = arg_types.map_with_index { |t, i| t }.join(", ")
          param_list = (0...arg_count).map { |i| "ptr" }.join(", ") if param_list.empty?
          emit_raw "declare #{return_type} @#{name}(#{param_list})\n"
        end
      end
    end

    # Emit a stub function body for methods called on Nil/Unknown/impossible receivers.
    # Returns the LLVM IR string for the stub, or nil if the name doesn't match.
    private def emit_dead_code_stub(name : String, return_type : String, arg_count : Int32 = 0, arg_types : Array(String) = [] of String) : String?
      # Methods on Nil (e.g. Nil$Hzero, Nil$Hto_i, Nil$Hadditive_identity)
      # Methods on Unknown (e.g. Unknown$Hto_i, Unknown$Hto_u32)
      # Union dispatch on Nil|X (e.g. Nil$_$OR_...#call)
      is_nil_method = name.starts_with?("Nil$H") || name.starts_with?("Nil$_$OR$_")
      is_unknown_method = name.starts_with?("Unknown$H")
      # Wrong receiver: Int32#unsafe_fetch makes no sense (Array method on Int32)
      is_wrong_receiver = name == "Int32$Hunsafe_fetch$$Int32"
      # Any V2-mangled method name (contains $) that has no body
      # is dead code from type-inference gaps. Emit a stub to avoid linker errors.
      # Catches: $H (#), $D (.), $CC (::), $$ (param separator), $Q (?), $SHL (<<), etc.
      is_v2_mangled = name.includes?("$")

      # Pointer::Appender#<<(UInt8) — real method, emit proper implementation.
      # Stores byte at current pointer, advances pointer by 1.
      if name == "Pointer$CCAppender$H$SHL$$UInt8"
        return "; Pointer::Appender#<<(UInt8)\n" \
               "define i8 @#{name}(ptr %self, i8 %value) {\n" \
               "  %cur = load ptr, ptr %self\n" \
               "  store i8 %value, ptr %cur\n" \
               "  %next = getelementptr i8, ptr %cur, i32 1\n" \
               "  store ptr %next, ptr %self\n" \
               "  ret i8 %value\n" \
               "}\n"
      end

      # Pointer#address — primitive that returns pointer value as UInt64
      if name.includes?("$Haddress") && name.includes?("Pointer$L")
        return "; Pointer#address (primitive: ptrtoint)\n" \
               "define i64 @#{name}(ptr %self) {\n" \
               "  %addr = ptrtoint ptr %self to i64\n" \
               "  ret i64 %addr\n" \
               "}\n"
      end

      # Reference#object_id — primitive: return pointer address as UInt64
      if name.ends_with?("$Hobject_id") && return_type == "i64"
        return "; Reference#object_id (primitive: ptrtoint)\n" \
               "define i64 @#{name}(ptr %self) {\n" \
               "  %addr = ptrtoint ptr %self to i64\n" \
               "  ret i64 %addr\n" \
               "}\n"
      end

      # Float64#**(Int32) / Float32#**(Int32) — primitive power via llvm.powi intrinsic
      if name == "Float64$H$POW$$Int32"
        return "; Float64#**(Int32) via llvm.powi.f64\n" \
               "define double @#{name}(double %self, i32 %other) {\n" \
               "  %result = call double @llvm.powi.f64(double %self, i32 %other)\n" \
               "  ret double %result\n" \
               "}\n"
      end
      if name == "Float32$H$POW$$Int32"
        return "; Float32#**(Int32) via llvm.powi.f32\n" \
               "define float @#{name}(float %self, i32 %other) {\n" \
               "  %result = call float @llvm.powi.f32(float %self, i32 %other)\n" \
               "  ret float %result\n" \
               "}\n"
      end
      # Float64#**(Float64) / Float32#**(Float32) — via llvm.pow
      if name == "Float64$H$POW$$Float64"
        return "; Float64#**(Float64) via llvm.pow.f64\n" \
               "define double @#{name}(double %self, double %other) {\n" \
               "  %result = call double @llvm.pow.f64(double %self, double %other)\n" \
               "  ret double %result\n" \
               "}\n"
      end
      if name == "Float32$H$POW$$Float32"
        return "; Float32#**(Float32) via llvm.pow.f32\n" \
               "define float @#{name}(float %self, float %other) {\n" \
               "  %result = call float @llvm.pow.f32(float %self, float %other)\n" \
               "  ret float %result\n" \
               "}\n"
      end

      # Primitive integer comparison/arithmetic methods: Int32#>(Int32) etc.
      # These are @[Primitive(:binary)] in stdlib but our compiler treats self as ptr.
      # Pattern: <Type>$H$<OP>$$<ArgType>(ptr %self, <llvm_type> %other)
      if m = name.match(/\A(Int8|Int16|Int32|Int64|Int128|UInt8|UInt16|UInt32|UInt64|UInt128)\$H\$(GT|LT|GE|LE|EQ|NE|ADD|SUB|MUL|AND|OR|XOR)\$\$(.+)\z/)
        recv_type_name = m[1]
        op_name = m[2]
        llvm_type = case recv_type_name
                    when "Int8", "UInt8"   then "i8"
                    when "Int16", "UInt16" then "i16"
                    when "Int32", "UInt32" then "i32"
                    when "Int64", "UInt64" then "i64"
                    when "Int128", "UInt128" then "i128"
                    else "i32"
                    end
        is_signed = recv_type_name.starts_with?("Int") && !recv_type_name.starts_with?("UInt")
        op_ir = case op_name
                when "GT" then is_signed ? "icmp sgt" : "icmp ugt"
                when "LT" then is_signed ? "icmp slt" : "icmp ult"
                when "GE" then is_signed ? "icmp sge" : "icmp uge"
                when "LE" then is_signed ? "icmp sle" : "icmp ule"
                when "EQ" then "icmp eq"
                when "NE" then "icmp ne"
                when "ADD" then "add"
                when "SUB" then "sub"
                when "MUL" then "mul"
                when "AND" then "and"
                when "OR"  then "or"
                when "XOR" then "xor"
                else nil
                end
        if op_ir
          is_cmp = op_ir.starts_with?("icmp")
          ret_type = is_cmp ? "i1" : llvm_type
          return "; #{recv_type_name}##{op_name} primitive\n" \
                 "define #{ret_type} @#{name}(ptr %self, #{llvm_type} %other) {\n" \
                 "  %self_val = ptrtoint ptr %self to #{llvm_type}\n" \
                 "  %result = #{op_ir} #{llvm_type} %self_val, %other\n" \
                 "  ret #{ret_type} %result\n" \
                 "}\n"
        end
      end

      # Int#remainder(Int32) and Int#tdiv(Int32) — also primitive but use self:ptr
      # These call check_div_argument which also has ptr self — handle specifically
      if m = name.match(/\AInt\$H(remainder|tdiv)\$\$(Int8|Int16|Int32|Int64|UInt8|UInt16|UInt32|UInt64)\z/)
        method = m[1]
        arg_type_name = m[2]
        llvm_type = case arg_type_name
                    when "Int8", "UInt8"   then "i8"
                    when "Int16", "UInt16" then "i16"
                    when "Int32", "UInt32" then "i32"
                    when "Int64", "UInt64" then "i64"
                    else "i32"
                    end
        is_signed = !arg_type_name.starts_with?("UInt")
        if method == "remainder"
          div_op = is_signed ? "srem" : "urem"
          return "; Int#remainder(#{arg_type_name}) primitive\n" \
                 "define #{llvm_type} @#{name}(ptr %self, #{llvm_type} %other) {\n" \
                 "  %self_val = ptrtoint ptr %self to #{llvm_type}\n" \
                 "  %result = #{div_op} #{llvm_type} %self_val, %other\n" \
                 "  ret #{llvm_type} %result\n" \
                 "}\n"
        else # tdiv
          div_op = is_signed ? "sdiv" : "udiv"
          return "; Int#tdiv(#{arg_type_name}) primitive\n" \
                 "define ptr @#{name}(ptr %self, #{llvm_type} %other) {\n" \
                 "  %self_val = ptrtoint ptr %self to #{llvm_type}\n" \
                 "  %result = #{div_op} #{llvm_type} %self_val, %other\n" \
                 "  %result_ptr = inttoptr #{llvm_type} %result to ptr\n" \
                 "  ret ptr %result_ptr\n" \
                 "}\n"
        end
      end

      return nil unless is_nil_method || is_unknown_method || is_wrong_receiver || is_v2_mangled

      # For .new constructors returning ptr, allocate memory instead of returning null.
      # This prevents null-pointer crashes in the bootstrap when allocator functions
      # weren't generated by HIR but are called at runtime.
      is_constructor = name.includes?("$Dnew")
      if is_constructor && return_type == "ptr"
        param_list = if !arg_types.empty?
                       arg_types.map_with_index { |t, i| "#{t} %arg#{i}" }.join(", ")
                     else
                       (0...arg_count).map { |i| "ptr %arg#{i}" }.join(", ")
                     end
        # Allocate 512 bytes (zero-initialized) — enough for most Crystal objects.
        # Not properly initialized, but prevents null-ptr crashes during bootstrap.
        return "; allocator stub for: #{name}\n" \
               "define ptr @#{name}(#{param_list}) {\n" \
               "  %mem = call ptr @calloc(i64 1, i64 512)\n" \
               "  ret ptr %mem\n" \
               "}\n"
      end

      # Build a minimal function body that returns a zero/default value
      ret_stmt = case return_type
                 when "void"   then "ret void"
                 when "i1"     then "ret i1 0"
                 when "i8"     then "ret i8 0"
                 when "i16"    then "ret i16 0"
                 when "i32"    then "ret i32 0"
                 when "i64"    then "ret i64 0"
                 when "i128"   then "ret i128 0"
                 when "float"  then "ret float 0.0"
                 when "double" then "ret double 0.0"
                 when "ptr"    then "ret ptr null"
                 else
                   if return_type.includes?(".union")
                     "ret #{return_type} zeroinitializer"
                   else
                     "ret #{return_type} zeroinitializer"
                   end
                 end

      # Build parameter list with proper types from call site instead of varargs (...)
      # This prevents LLVM type mismatches at call sites.
      param_list = if !arg_types.empty?
                     arg_types.map_with_index { |t, i| "#{t} %arg#{i}" }.join(", ")
                   else
                     (0...arg_count).map { |i| "ptr %arg#{i}" }.join(", ")
                   end

      "; stub for dead-code method: #{name}\n" \
      "define #{return_type} @#{name}(#{param_list}) {\n" \
      "  #{ret_stmt}\n" \
      "}\n"
    end

    # Generate proper declaration for LLVM intrinsics
    private def emit_llvm_intrinsic_declaration(name : String) : String?
      case name
      # Bitreverse intrinsics
      when "llvm.bitreverse.i8"   then "declare i8 @llvm.bitreverse.i8(i8)"
      when "llvm.bitreverse.i16"  then "declare i16 @llvm.bitreverse.i16(i16)"
      when "llvm.bitreverse.i32"  then "declare i32 @llvm.bitreverse.i32(i32)"
      when "llvm.bitreverse.i64"  then "declare i64 @llvm.bitreverse.i64(i64)"
      when "llvm.bitreverse.i128" then "declare i128 @llvm.bitreverse.i128(i128)"
      # Byte swap intrinsics
      when "llvm.bswap.i16"  then "declare i16 @llvm.bswap.i16(i16)"
      when "llvm.bswap.i32"  then "declare i32 @llvm.bswap.i32(i32)"
      when "llvm.bswap.i64"  then "declare i64 @llvm.bswap.i64(i64)"
      when "llvm.bswap.i128" then "declare i128 @llvm.bswap.i128(i128)"
      # Population count (ctpop)
      when "llvm.ctpop.i8"   then "declare i8 @llvm.ctpop.i8(i8)"
      when "llvm.ctpop.i16"  then "declare i16 @llvm.ctpop.i16(i16)"
      when "llvm.ctpop.i32"  then "declare i32 @llvm.ctpop.i32(i32)"
      when "llvm.ctpop.i64"  then "declare i64 @llvm.ctpop.i64(i64)"
      when "llvm.ctpop.i128" then "declare i128 @llvm.ctpop.i128(i128)"
      # Count leading zeros
      when "llvm.ctlz.i8"   then "declare i8 @llvm.ctlz.i8(i8, i1)"
      when "llvm.ctlz.i16"  then "declare i16 @llvm.ctlz.i16(i16, i1)"
      when "llvm.ctlz.i32"  then "declare i32 @llvm.ctlz.i32(i32, i1)"
      when "llvm.ctlz.i64"  then "declare i64 @llvm.ctlz.i64(i64, i1)"
      when "llvm.ctlz.i128" then "declare i128 @llvm.ctlz.i128(i128, i1)"
      # Count trailing zeros
      when "llvm.cttz.i8"   then "declare i8 @llvm.cttz.i8(i8, i1)"
      when "llvm.cttz.i16"  then "declare i16 @llvm.cttz.i16(i16, i1)"
      when "llvm.cttz.i32"  then "declare i32 @llvm.cttz.i32(i32, i1)"
      when "llvm.cttz.i64"  then "declare i64 @llvm.cttz.i64(i64, i1)"
      when "llvm.cttz.i128" then "declare i128 @llvm.cttz.i128(i128, i1)"
      # Funnel shift left/right
      when "llvm.fshl.i8"   then "declare i8 @llvm.fshl.i8(i8, i8, i8)"
      when "llvm.fshl.i16"  then "declare i16 @llvm.fshl.i16(i16, i16, i16)"
      when "llvm.fshl.i32"  then "declare i32 @llvm.fshl.i32(i32, i32, i32)"
      when "llvm.fshl.i64"  then "declare i64 @llvm.fshl.i64(i64, i64, i64)"
      when "llvm.fshl.i128" then "declare i128 @llvm.fshl.i128(i128, i128, i128)"
      when "llvm.fshr.i8"   then "declare i8 @llvm.fshr.i8(i8, i8, i8)"
      when "llvm.fshr.i16"  then "declare i16 @llvm.fshr.i16(i16, i16, i16)"
      when "llvm.fshr.i32"  then "declare i32 @llvm.fshr.i32(i32, i32, i32)"
      when "llvm.fshr.i64"  then "declare i64 @llvm.fshr.i64(i64, i64, i64)"
      when "llvm.fshr.i128" then "declare i128 @llvm.fshr.i128(i128, i128, i128)"
      # Math intrinsics (float)
      when "llvm.ceil.f32"  then "declare float @llvm.ceil.f32(float)"
      when "llvm.ceil.f64"  then "declare double @llvm.ceil.f64(double)"
      when "llvm.floor.f32" then "declare float @llvm.floor.f32(float)"
      when "llvm.floor.f64" then "declare double @llvm.floor.f64(double)"
      when "llvm.round.f32" then "declare float @llvm.round.f32(float)"
      when "llvm.round.f64" then "declare double @llvm.round.f64(double)"
      when "llvm.trunc.f32" then "declare float @llvm.trunc.f32(float)"
      when "llvm.trunc.f64" then "declare double @llvm.trunc.f64(double)"
      when "llvm.sqrt.f32"  then "declare float @llvm.sqrt.f32(float)"
      when "llvm.sqrt.f64"  then "declare double @llvm.sqrt.f64(double)"
      when "llvm.fabs.f32"  then "declare float @llvm.fabs.f32(float)"
      when "llvm.fabs.f64"  then "declare double @llvm.fabs.f64(double)"
      when "llvm.sin.f32"   then "declare float @llvm.sin.f32(float)"
      when "llvm.sin.f64"   then "declare double @llvm.sin.f64(double)"
      when "llvm.cos.f32"   then "declare float @llvm.cos.f32(float)"
      when "llvm.cos.f64"   then "declare double @llvm.cos.f64(double)"
      when "llvm.exp.f32"   then "declare float @llvm.exp.f32(float)"
      when "llvm.exp.f64"   then "declare double @llvm.exp.f64(double)"
      when "llvm.exp2.f32"  then "declare float @llvm.exp2.f32(float)"
      when "llvm.exp2.f64"  then "declare double @llvm.exp2.f64(double)"
      when "llvm.log.f32"   then "declare float @llvm.log.f32(float)"
      when "llvm.log.f64"   then "declare double @llvm.log.f64(double)"
      when "llvm.log2.f32"  then "declare float @llvm.log2.f32(float)"
      when "llvm.log2.f64"  then "declare double @llvm.log2.f64(double)"
      when "llvm.log10.f32" then "declare float @llvm.log10.f32(float)"
      when "llvm.log10.f64" then "declare double @llvm.log10.f64(double)"
      when "llvm.rint.f32"  then "declare float @llvm.rint.f32(float)"
      when "llvm.rint.f64"  then "declare double @llvm.rint.f64(double)"
      when "llvm.pow.f32"   then "declare float @llvm.pow.f32(float, float)"
      when "llvm.pow.f64"   then "declare double @llvm.pow.f64(double, double)"
      when "llvm.powi.f32"  then "declare float @llvm.powi.f32(float, i32)"
      when "llvm.powi.f64"  then "declare double @llvm.powi.f64(double, i32)"
      when "llvm.fma.f32"   then "declare float @llvm.fma.f32(float, float, float)"
      when "llvm.fma.f64"   then "declare double @llvm.fma.f64(double, double, double)"
      when "llvm.copysign.f32" then "declare float @llvm.copysign.f32(float, float)"
      when "llvm.copysign.f64" then "declare double @llvm.copysign.f64(double, double)"
      when "llvm.minnum.f32" then "declare float @llvm.minnum.f32(float, float)"
      when "llvm.minnum.f64" then "declare double @llvm.minnum.f64(double, double)"
      when "llvm.maxnum.f32" then "declare float @llvm.maxnum.f32(float, float)"
      when "llvm.maxnum.f64" then "declare double @llvm.maxnum.f64(double, double)"
      # Memory intrinsics (opaque pointer variant for LLVM 15+)
      when "llvm.memcpy.p0.p0.i64"  then "declare void @llvm.memcpy.p0.p0.i64(ptr, ptr, i64, i1)"
      when "llvm.memcpy.p0.p0.i32"  then "declare void @llvm.memcpy.p0.p0.i32(ptr, ptr, i32, i1)"
      when "llvm.memmove.p0.p0.i64" then "declare void @llvm.memmove.p0.p0.i64(ptr, ptr, i64, i1)"
      when "llvm.memmove.p0.p0.i32" then "declare void @llvm.memmove.p0.p0.i32(ptr, ptr, i32, i1)"
      when "llvm.memset.p0.i64"     then "declare void @llvm.memset.p0.i64(ptr, i8, i64, i1)"
      when "llvm.memset.p0.i32"     then "declare void @llvm.memset.p0.i32(ptr, i8, i32, i1)"
      # VA intrinsics
      when "llvm.va_start.p0" then "declare void @llvm.va_start.p0(ptr)"
      when "llvm.va_end.p0"   then "declare void @llvm.va_end.p0(ptr)"
      when "llvm.va_start"    then "declare void @llvm.va_start(ptr)"
      when "llvm.va_end"      then "declare void @llvm.va_end(ptr)"
      # Debug trap
      when "llvm.debugtrap" then "declare void @llvm.debugtrap()"
      # Pause / hint intrinsics
      when "llvm.x86.sse2.pause" then "declare void @llvm.x86.sse2.pause()"
      when "llvm.aarch64.hint"   then "declare void @llvm.aarch64.hint(i32)"
      # Read cycle counter
      when "llvm.readcyclecounter" then "declare i64 @llvm.readcyclecounter()"
      else
        # Unknown intrinsic - emit as varargs (may not work but better than nothing)
        STDERR.puts "[LLVM] Unknown intrinsic: #{name}"
        nil
      end
    end

    # Build arguments for LLVM intrinsic calls with correct types
    # Returns tuple of (args_string, return_type)
    private def emit_llvm_intrinsic_call_args(inst : ExternCall, result_name : String, intrinsic_name : String) : Tuple(String, String)
      # Parse intrinsic signature to get expected types
      sig = parse_intrinsic_signature(intrinsic_name)
      return {"", "void"} unless sig

      param_types, return_type = sig

      # Build args with proper types, loading from pointers if needed
      args = inst.args.map_with_index do |arg_id, idx|
        expected_type = param_types[idx]? || "ptr"
        actual_type_ref = @value_types[arg_id]? || TypeRef::POINTER
        actual_type = @type_mapper.llvm_type(actual_type_ref)
        val = value_ref(arg_id)

        if actual_type == "ptr" && expected_type != "ptr"
          # Need to load the value from the pointer
          load_name = "%intrinsic_load.#{@cond_counter}"
          @cond_counter += 1
          emit "#{load_name} = load #{expected_type}, ptr #{val}"
          "#{expected_type} #{load_name}"
        elsif actual_type == expected_type
          "#{expected_type} #{val}"
        else
          # Type mismatch - try to convert
          if expected_type.starts_with?('i') && actual_type.starts_with?('i')
            # Integer to integer - truncate or extend
            expected_bits = expected_type[1..].to_i? || 32
            actual_bits = actual_type[1..].to_i? || 32
            if expected_bits < actual_bits
              conv_name = "%intrinsic_trunc.#{@cond_counter}"
              @cond_counter += 1
              emit "#{conv_name} = trunc #{actual_type} #{val} to #{expected_type}"
              "#{expected_type} #{conv_name}"
            elsif expected_bits > actual_bits
              conv_name = "%intrinsic_ext.#{@cond_counter}"
              @cond_counter += 1
              emit "#{conv_name} = zext #{actual_type} #{val} to #{expected_type}"
              "#{expected_type} #{conv_name}"
            else
              "#{expected_type} #{val}"
            end
          elsif expected_type == "float" && actual_type == "double"
            conv_name = "%intrinsic_fptrunc.#{@cond_counter}"
            @cond_counter += 1
            emit "#{conv_name} = fptrunc double #{val} to float"
            "float #{conv_name}"
          elsif expected_type == "double" && actual_type == "float"
            conv_name = "%intrinsic_fpext.#{@cond_counter}"
            @cond_counter += 1
            emit "#{conv_name} = fpext float #{val} to double"
            "double #{conv_name}"
          else
            # Fallback - hope for the best
            "#{expected_type} #{val}"
          end
        end
      end.join(", ")

      {args, return_type}
    end

    # Parse LLVM intrinsic name to get parameter types and return type
    private def parse_intrinsic_signature(name : String) : Tuple(Array(String), String)?
      case name
      # Bitreverse intrinsics
      when "llvm.bitreverse.i8"   then {["i8"], "i8"}
      when "llvm.bitreverse.i16"  then {["i16"], "i16"}
      when "llvm.bitreverse.i32"  then {["i32"], "i32"}
      when "llvm.bitreverse.i64"  then {["i64"], "i64"}
      when "llvm.bitreverse.i128" then {["i128"], "i128"}
      # Byte swap intrinsics
      when "llvm.bswap.i16"  then {["i16"], "i16"}
      when "llvm.bswap.i32"  then {["i32"], "i32"}
      when "llvm.bswap.i64"  then {["i64"], "i64"}
      when "llvm.bswap.i128" then {["i128"], "i128"}
      # Population count (ctpop)
      when "llvm.ctpop.i8"   then {["i8"], "i8"}
      when "llvm.ctpop.i16"  then {["i16"], "i16"}
      when "llvm.ctpop.i32"  then {["i32"], "i32"}
      when "llvm.ctpop.i64"  then {["i64"], "i64"}
      when "llvm.ctpop.i128" then {["i128"], "i128"}
      # Count leading zeros
      when "llvm.ctlz.i8"   then {["i8", "i1"], "i8"}
      when "llvm.ctlz.i16"  then {["i16", "i1"], "i16"}
      when "llvm.ctlz.i32"  then {["i32", "i1"], "i32"}
      when "llvm.ctlz.i64"  then {["i64", "i1"], "i64"}
      when "llvm.ctlz.i128" then {["i128", "i1"], "i128"}
      # Count trailing zeros
      when "llvm.cttz.i8"   then {["i8", "i1"], "i8"}
      when "llvm.cttz.i16"  then {["i16", "i1"], "i16"}
      when "llvm.cttz.i32"  then {["i32", "i1"], "i32"}
      when "llvm.cttz.i64"  then {["i64", "i1"], "i64"}
      when "llvm.cttz.i128" then {["i128", "i1"], "i128"}
      # Funnel shift left/right
      when "llvm.fshl.i8"   then {["i8", "i8", "i8"], "i8"}
      when "llvm.fshl.i16"  then {["i16", "i16", "i16"], "i16"}
      when "llvm.fshl.i32"  then {["i32", "i32", "i32"], "i32"}
      when "llvm.fshl.i64"  then {["i64", "i64", "i64"], "i64"}
      when "llvm.fshl.i128" then {["i128", "i128", "i128"], "i128"}
      when "llvm.fshr.i8"   then {["i8", "i8", "i8"], "i8"}
      when "llvm.fshr.i16"  then {["i16", "i16", "i16"], "i16"}
      when "llvm.fshr.i32"  then {["i32", "i32", "i32"], "i32"}
      when "llvm.fshr.i64"  then {["i64", "i64", "i64"], "i64"}
      when "llvm.fshr.i128" then {["i128", "i128", "i128"], "i128"}
      # Math intrinsics (float)
      when "llvm.ceil.f32"  then {["float"], "float"}
      when "llvm.ceil.f64"  then {["double"], "double"}
      when "llvm.floor.f32" then {["float"], "float"}
      when "llvm.floor.f64" then {["double"], "double"}
      when "llvm.round.f32" then {["float"], "float"}
      when "llvm.round.f64" then {["double"], "double"}
      when "llvm.trunc.f32" then {["float"], "float"}
      when "llvm.trunc.f64" then {["double"], "double"}
      when "llvm.sqrt.f32"  then {["float"], "float"}
      when "llvm.sqrt.f64"  then {["double"], "double"}
      when "llvm.fabs.f32"  then {["float"], "float"}
      when "llvm.fabs.f64"  then {["double"], "double"}
      when "llvm.sin.f32"   then {["float"], "float"}
      when "llvm.sin.f64"   then {["double"], "double"}
      when "llvm.cos.f32"   then {["float"], "float"}
      when "llvm.cos.f64"   then {["double"], "double"}
      when "llvm.exp.f32"   then {["float"], "float"}
      when "llvm.exp.f64"   then {["double"], "double"}
      when "llvm.exp2.f32"  then {["float"], "float"}
      when "llvm.exp2.f64"  then {["double"], "double"}
      when "llvm.log.f32"   then {["float"], "float"}
      when "llvm.log.f64"   then {["double"], "double"}
      when "llvm.log2.f32"  then {["float"], "float"}
      when "llvm.log2.f64"  then {["double"], "double"}
      when "llvm.log10.f32" then {["float"], "float"}
      when "llvm.log10.f64" then {["double"], "double"}
      when "llvm.rint.f32"  then {["float"], "float"}
      when "llvm.rint.f64"  then {["double"], "double"}
      when "llvm.pow.f32"   then {["float", "float"], "float"}
      when "llvm.pow.f64"   then {["double", "double"], "double"}
      when "llvm.powi.f32"  then {["float", "i32"], "float"}
      when "llvm.powi.f64"  then {["double", "i32"], "double"}
      when "llvm.fma.f32"   then {["float", "float", "float"], "float"}
      when "llvm.fma.f64"   then {["double", "double", "double"], "double"}
      when "llvm.copysign.f32" then {["float", "float"], "float"}
      when "llvm.copysign.f64" then {["double", "double"], "double"}
      when "llvm.minnum.f32" then {["float", "float"], "float"}
      when "llvm.minnum.f64" then {["double", "double"], "double"}
      when "llvm.maxnum.f32" then {["float", "float"], "float"}
      when "llvm.maxnum.f64" then {["double", "double"], "double"}
      # Memory intrinsics (opaque pointer variant for LLVM 15+)
      when "llvm.memcpy.p0.p0.i64"  then {["ptr", "ptr", "i64", "i1"], "void"}
      when "llvm.memcpy.p0.p0.i32"  then {["ptr", "ptr", "i32", "i1"], "void"}
      when "llvm.memmove.p0.p0.i64" then {["ptr", "ptr", "i64", "i1"], "void"}
      when "llvm.memmove.p0.p0.i32" then {["ptr", "ptr", "i32", "i1"], "void"}
      when "llvm.memset.p0.i64"     then {["ptr", "i8", "i64", "i1"], "void"}
      when "llvm.memset.p0.i32"     then {["ptr", "i8", "i32", "i1"], "void"}
      # VA intrinsics
      when "llvm.va_start.p0" then {["ptr"], "void"}
      when "llvm.va_end.p0"   then {["ptr"], "void"}
      when "llvm.va_start"    then {["ptr"], "void"}
      when "llvm.va_end"      then {["ptr"], "void"}
      # Debug trap
      when "llvm.debugtrap" then {[] of String, "void"}
      # Pause / hint intrinsics
      when "llvm.x86.sse2.pause" then {[] of String, "void"}
      when "llvm.aarch64.hint"   then {["i32"], "void"}
      # Read cycle counter
      when "llvm.readcyclecounter" then {[] of String, "i64"}
      else
        nil
      end
    end

    # Returns receiver name and method core from an extern name like "Type#+" or "Type#+$Int32".
    # Avoids allocations from split.
    @[AlwaysInline]
    private def extract_receiver_and_method(name : String) : Tuple(String?, String)?
      sep_idx = name.index('#') || name.index('.')
      return nil unless sep_idx
      receiver = name[0, sep_idx]
      rest = name[(sep_idx + 1)..]
      if dollar = rest.index('$')
        rest = rest[0, dollar]
      end
      {receiver, rest}
    end

    # Extract the unqualified method core from a possibly-mangled HIR name.
    # Examples: "String#size$Int32" -> "size", "puts$Int32" -> "puts".
    @[AlwaysInline]
    private def method_core_from_name(name : String) : String
      base = name
      if dollar = base.index('$')
        base = base[0, dollar]
      end
      sep_idx = nil
      i = base.bytesize - 1
      while i >= 0
        byte = base.byte_at(i)
        if byte == '#'.ord || byte == '.'.ord
          sep_idx = i
          break
        end
        i -= 1
      end
      sep_idx ? base[(sep_idx + 1)..] : base
    end

    @[AlwaysInline]
    private def suffix_after_dollar(name : String) : String?
      if dollar = name.index('$')
        return name[(dollar + 1)..]
      end
      nil
    end

    @[AlwaysInline]
    private def pointer_constructor_name?(name : String) : Bool
      if parts = extract_receiver_and_method(name)
        receiver, method = parts
        return false unless receiver
        return false unless receiver.starts_with?("Pointer")
        return method == "new" || method == "new!"
      end
      false
    end

    @[AlwaysInline]
    private def operator_method?(name : String) : Bool
      case name
      when "+", "-", "*", "/", "%", "**", "<<", ">>", "&", "|", "^", "<", ">", "<=", ">=", "==", "!="
        true
      else
        false
      end
    end

    @[AlwaysInline]
    private def crystalish_extern_name?(name : String) : Bool
      # Namespaced or receiver-qualified: definitely Crystal.
      return true if name.includes?('#') || name.includes?('.') || name.includes?("::")
      # Type-suffixed or type-like: likely Crystal (Int32/UInt64/etc).
      return true if name.includes?("_Int") || name.includes?("_UInt") || name.includes?("_Float")
      # Uppercase letters are uncommon in C lib symbols, common in Crystal types.
      name.each_byte do |byte|
        return true if byte >= 'A'.ord && byte <= 'Z'.ord
      end
      false
    end

    @[AlwaysInline]
    private def qualified_method_name?(name : String) : Bool
      name.includes?('#') || name.includes?('.') || name.includes?("::")
    end

    # Conservative fuzzy match for unresolved Crystal extern calls:
    # only allow unqualified method names that have an explicit "$..." suffix.
    # This avoids legacy suffix collisions while still covering cases such as
    # "index$UInt8" -> "String#index$UInt8".
    @[AlwaysInline]
    private def extern_fuzzy_match_eligible?(extern_name : String, is_operator : Bool, is_c_lib_function : Bool) : Bool
      return false if is_operator || is_c_lib_function
      return false if qualified_method_name?(extern_name)
      suffix = suffix_after_dollar(extern_name)
      !suffix.nil? && !suffix.empty?
    end

    @[AlwaysInline]
    private def extern_fuzzy_matches_candidate?(extern_name : String, extern_method_core : String, extern_suffix : String, candidate_name : String) : Bool
      return false unless qualified_method_name?(candidate_name)
      return false unless method_core_from_name(candidate_name) == extern_method_core
      candidate_suffix = suffix_after_dollar(candidate_name)
      candidate_suffix == extern_suffix
    end


    private def emit_string_constants
      emit_raw "\n; String constants\n"

      # Check if Crystal String type is available (prelude loaded).
      # If so, emit as Crystal String objects: { i32 type_id, i32 bytesize, i32 length, [N x i8] bytes }
      # Otherwise (--no-prelude), emit as C strings for printf compatibility.
      # NOTE: TypeRegistry pre-registers String as a primitive, so we must also check @no_prelude
      string_mir_type = @no_prelude ? nil : @module.type_registry.get_by_name("String")
      # Use MIR type id directly — must match vdispatch tables and class allocations
      string_type_id = string_mir_type ? string_mir_type.id.to_i32 : 0
      @string_type_id = string_type_id

      if string_mir_type
        # Prelude mode: Crystal String objects
        emit_raw "@.str.empty = private unnamed_addr constant { i32, i32, i32, [1 x i8] } { i32 #{string_type_id}, i32 0, i32 0, [1 x i8] c\"\\00\" }, align 8\n"
        # Error message strings for builtin overrides
        emit_raw "@.str.file_open_error = private unnamed_addr constant { i32, i32, i32, [26 x i8] } { i32 #{string_type_id}, i32 25, i32 25, [26 x i8] c\"Error opening file (open)\\00\" }, align 8\n"
        emit_raw "@.str.dbg_open_label = private unnamed_addr constant [5 x i8] c\"open\\00\", align 1\n"
      else
        # No-prelude mode: C strings
        emit_raw "@.str.empty = private unnamed_addr constant [1 x i8] c\"\\00\", align 1\n"
        emit_raw "@.str.file_open_error = private unnamed_addr constant [26 x i8] c\"Error opening file (open)\\00\", align 1\n"
      end

      return if @string_constants.empty?

      @string_constants.each do |str, global_name|
        # Escape string for LLVM: replace special chars
        escaped = str.gsub("\\", "\\\\")
                    .gsub("\n", "\\0A")
                    .gsub("\r", "\\0D")
                    .gsub("\t", "\\09")
                    .gsub("\"", "\\22")
        len = str.bytesize + 1  # +1 for null terminator
        if string_mir_type
          # Crystal String: { i32 type_id, i32 bytesize, i32 length, [len x i8] bytes_with_null }
          bytesize = str.bytesize
          charsize = str.size  # character count (may differ for multibyte)
          emit_raw "#{global_name} = private unnamed_addr constant { i32, i32, i32, [#{len} x i8] } { i32 #{string_type_id}, i32 #{bytesize}, i32 #{charsize}, [#{len} x i8] c\"#{escaped}\\00\" }, align 8\n"
        else
          # C string
          emit_raw "#{global_name} = private unnamed_addr constant [#{len} x i8] c\"#{escaped}\\00\", align 1\n"
        end
      end
    end

    private def module_singleton_global_for(type_ref : TypeRef) : String
      if existing = @module_singleton_globals[type_ref]?
        return existing
      end

      global_name = "@.module.singleton.#{type_ref.id}"
      @module_singleton_globals[type_ref] = global_name
      global_name
    end

    private def collect_module_singleton_globals
      @module.functions.each do |func|
        func.blocks.each do |block|
          block.instructions.each do |inst|
            next unless inst.is_a?(Constant)
            next unless inst.value.is_a?(Nil)
            next unless @module.module_type?(inst.type)
            module_singleton_global_for(inst.type)
          end
        end
      end
    end

    private def emit_module_singleton_globals
      return if @module_singleton_globals.empty?

      @module_singleton_globals.each do |type_ref, global_name|
        # Runtime module value: pointer to a stable cell whose first i32 is type_id.
        # vdispatch reads this header exactly like class instance type_id.
        emit_raw "#{global_name} = private global i32 #{type_ref.id.to_i32}, align 4\n"
      end
    end

    private def emit_global_variables
      collect_module_singleton_globals

      # Collect all defined globals
      defined_globals = Set(String).new
      @module.globals.each do |global|
        mangled_name = @type_mapper.mangle_name(global.name)
        defined_globals << mangled_name
      end

      extern_globals = @module.extern_globals
      extern_globals.each_key do |name|
        defined_globals << @type_mapper.mangle_name(name)
      end

      # Collect all referenced globals from GlobalLoad/GlobalStore instructions.
      # For each global, prefer the most specific (non-pointer) type — union types
      # from closure cells must not be downgraded to ptr by later GlobalLoad/Store.
      referenced_globals = Hash(String, TypeRef).new
      @module.functions.each do |func|
        func.blocks.each do |block|
          block.instructions.each do |inst|
            case inst
            when GlobalLoad
              mangled = @type_mapper.mangle_name(inst.global_name)
              unless defined_globals.includes?(mangled)
                new_type = inst.type
                existing = referenced_globals[mangled]?
                if existing.nil?
                  referenced_globals[mangled] = new_type
                elsif new_type != TypeRef::VOID && new_type != TypeRef::POINTER
                  # Prefer non-pointer types (e.g., union types for closure cells)
                  referenced_globals[mangled] = new_type
                end
              end
            when GlobalStore
              mangled = @type_mapper.mangle_name(inst.global_name)
              unless defined_globals.includes?(mangled)
                new_type = inst.type
                existing = referenced_globals[mangled]?
                if existing.nil?
                  referenced_globals[mangled] = (new_type != TypeRef::VOID ? new_type : TypeRef::POINTER)
                elsif new_type != TypeRef::VOID && new_type != TypeRef::POINTER
                  # Prefer non-pointer types (e.g., union types for closure cells)
                  referenced_globals[mangled] = new_type
                end
              end
            end
          end
        end
      end

      # Build set of function names to avoid conflict
      function_names = Set(String).new
      @module.functions.each do |func|
        function_names << mangle_function_name(func.name)
      end

      # Emit extern globals first
      extern_globals.each do |name, type_ref|
        llvm_type = @type_mapper.llvm_type(type_ref)
        llvm_type = "ptr" if llvm_type == "void"
        mangled_name = @type_mapper.mangle_name(name)
        emit_raw "@#{mangled_name} = external global #{llvm_type}\n"
      end

      # Emit defined globals (deduplicate — MIR may register the same global multiple times)
      emitted_globals = Set(String).new
      @module.globals.each do |global|
        llvm_type = @type_mapper.llvm_type(global.type)
        llvm_type = "ptr" if llvm_type == "void"
        initial = global.initial_value || 0_i64
        mangled_name = @type_mapper.mangle_name(global.name)
        next if emitted_globals.includes?(mangled_name)
        emitted_globals << mangled_name
        actual_name = mangled_name
        @global_declared_types[mangled_name] = llvm_type
        # Check for constant initial value override
        const_val = @constant_initial_values[mangled_name]?
        # Avoid conflict with function names by prefixing with .global
        if function_names.includes?(mangled_name)
          actual_name = ".global.#{mangled_name}"
          @global_name_mapping[mangled_name] = actual_name
        end
        # Use zeroinitializer for struct/union types, numeric 0 for primitives
        if llvm_type.starts_with?('%') || llvm_type.starts_with?('{')
          emit_raw "@#{actual_name} = global #{llvm_type} zeroinitializer\n"
        elsif llvm_type == "ptr"
          emit_raw "@#{actual_name} = global #{llvm_type} null\n"
        elsif llvm_type == "float" || llvm_type == "double"
          if const_val && initial == 0_i64
            float_value = const_val.is_a?(Float64) ? const_val.to_s : const_val.to_f64.to_s
          else
            float_value = initial.to_f.to_s
          end
          float_value = "0.0" if float_value == "0"
          float_value = "#{float_value}.0" if float_value.matches?(/^-?\d+$/)
          emit_raw "@#{actual_name} = global #{llvm_type} #{float_value}\n"
        else
          if const_val && initial == 0_i64
            int_value = const_val.is_a?(Int64) ? const_val : const_val.to_i64
            emit_raw "@#{actual_name} = global #{llvm_type} #{int_value}\n"
          else
            emit_raw "@#{actual_name} = global #{llvm_type} #{initial}\n"
          end
        end
      end

      # Emit undefined globals (class variables that weren't explicitly defined)
      referenced_globals.each do |name, type_ref|
        # Skip if it conflicts with a function name (likely a method accessor, not a variable)
        next if function_names.includes?(name)
        llvm_type = @type_mapper.llvm_type(type_ref)
        llvm_type = "ptr" if llvm_type == "void"
        @global_declared_types[name] = llvm_type
        # Check for constant initial value (e.g., Math::PI = 3.14159...)
        const_val = @constant_initial_values[name]?
        if llvm_type == "ptr"
          emit_raw "@#{name} = global #{llvm_type} null\n"
        elsif llvm_type.starts_with?('%') || llvm_type.starts_with?('{')
          emit_raw "@#{name} = global #{llvm_type} zeroinitializer\n"
        elsif llvm_type == "float" || llvm_type == "double"
          if const_val
            float_value = const_val.is_a?(Float64) ? const_val.to_s : const_val.to_f64.to_s
            float_value = "#{float_value}.0" if float_value.matches?(/^-?\d+$/)
          else
            float_value = "0.0"
          end
          emit_raw "@#{name} = global #{llvm_type} #{float_value}\n"
        else
          if const_val
            int_value = const_val.is_a?(Int64) ? const_val : const_val.to_i64
            emit_raw "@#{name} = global #{llvm_type} #{int_value}\n"
          else
            emit_raw "@#{name} = global #{llvm_type} 0\n"
          end
        end
      end
      emit_module_singleton_globals

      emit_raw "\n" unless @module.globals.empty? && referenced_globals.empty? && @module_singleton_globals.empty?
    end

    private def emit(s : String)
      @output << ("  " * @indent) << s << "\n"
    end

    private def emit_raw(s : String)
      @output << s
    end

    # Emit text to the top-level output buffer. During block buffering,
    # this routes to the main output to avoid nesting definitions inside functions.
    # When not buffering, this is the same as emit_raw.
    private def emit_toplevel(s : String)
      if tl = @toplevel_output
        tl << s
      else
        @output << s
      end
    end

    private def record_emitted_type(name : String, llvm_type : String)
      return unless name.starts_with?('%')
      @emitted_value_types[name] = llvm_type
      @emitted_value_types[name[1..]] = llvm_type
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

      # Precompute union payload sizes by name to avoid redefinition conflicts.
      union_payload_sizes = {} of String => UInt64
      @module.type_registry.types.each do |type|
        next unless type.kind.union?
        type_name = @type_mapper.mangle_name(type.name)
        max_size = type.variants.try(&.map(&.size).max) || 8_u64
        existing = union_payload_sizes[type_name]? || 0_u64
        union_payload_sizes[type_name] = max_size > existing ? max_size : existing
      end

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
          emit_union_type(type, union_payload_sizes[type_name]?)
          emitted_types << type_name
          @emitted_union_type_names << "#{type_name}.union"
        end
      end

      emit_raw "\n"
    end

    private def emit_struct_type(type : Type)
      name = @type_mapper.mangle_name(type.name)
      fields = type.fields

      if fields && !fields.empty?
        # For value types (structs), use the authoritative MIR size to avoid
        # layout mismatch. Our type mapper maps struct-typed fields to "ptr"
        # (8 bytes), but C lib structs inline them (e.g. LibC::Timespec = 16
        # bytes inside LibC::Stat). Since all field accesses use byte-level GEP,
        # the internal field types don't matter — only the total size does.
        if type.kind.struct? && type.size > 0
          emit_raw "%#{name} = type { [#{type.size} x i8] }\n"
        else
          field_types = [] of String

          # Add vtable ptr as first field for class types (not value types)
          if type.kind.reference?
            field_types << "ptr"  # vtable pointer for classes
          end

          fields.each do |field|
            llvm_type = @type_mapper.llvm_type(field.type_ref)
            # void is not valid for struct fields - use ptr instead (opaque pointer for untyped field)
            llvm_type = "ptr" if llvm_type == "void"
            field_types << llvm_type
          end

          emit_raw "%#{name} = type { #{field_types.join(", ")} }\n"
        end
      else
        # StaticArray(T, N) — emit as [N * elem_size x i8] to ensure correct alloca size.
        # StaticArray has no fields in the MIR registry but needs storage for N elements.
        if type.name.starts_with?("StaticArray(")
          # Prefer authoritative MIR size if available.
          total_bytes = type.size
          if total_bytes == 0
            if m = type.name.match(/StaticArray\((.+),\s*(\d+)\)/)
              elem_name = m[1].strip
              array_count = m[2].to_u64
              elem_mir_type = @module.type_registry.get_by_name(elem_name)
              elem_size = container_elem_storage_size_u64(elem_mir_type)
              total_bytes = elem_size * array_count
            end
          end
          total_bytes = pointer_word_bytes_u64 if total_bytes == 0
          emit_raw "%#{name} = type { [#{total_bytes} x i8] }\n"
        elsif type.kind.reference?
          emit_raw "%#{name} = type { ptr }\n"  # just vtable
        else
          emit_raw "%#{name} = type {}\n"
        end
      end
    end

    private def emit_union_type(type : Type, payload_size : UInt64? = nil)
      name = @type_mapper.mangle_name(type.name)
      # Union = { i32 discriminator, [N x i32] data }
      # Use i32 elements instead of i8 to prevent LLVM's ARM64 ABI lowering from
      # decomposing the payload into individual byte arguments when passed by value.
      # With [N x i8], a 24-byte union gets decomposed into 20 separate i8 register
      # args, corrupting pointer values. With [N x i32], it stays as a few i32 args.
      if type.size > 4
        payload_bytes = type.size - 4
      else
        payload_bytes = payload_size || type.variants.try(&.map(&.size).max) || 8_u64
      end
      # Round up to multiple of 4 bytes for i32 element array
      payload_i32s = (payload_bytes + 3) // 4
      emit_raw "%#{name}.union = type { i32, [#{payload_i32s} x i32] }\n"
    end

    private def emit_runtime_declarations
      # External C library functions
      emit_raw "declare ptr @malloc(i64)\n"
      emit_raw "declare ptr @calloc(i64, i64)\n"
      emit_raw "declare ptr @realloc(ptr, i64)\n"
      emit_raw "declare void @free(ptr)\n"
      emit_raw "declare i32 @printf(ptr, ...)\n"
      emit_raw "declare i32 @puts(ptr)\n"
      emit_raw "declare ptr @gets(ptr)\n"
      emit_raw "declare ptr @fgets(ptr, i32, ptr)\n"
      emit_raw "declare ptr @fputs(ptr, ptr)\n"
      emit_raw "declare void @exit(i32)\n"
      emit_raw "declare void @perror(ptr)\n"
      emit_raw "declare i32 @dprintf(i32, ptr, ...)\n"
      emit_raw "\n"

      # PCRE2 library functions
      emit_raw "declare ptr @pcre2_compile_8(ptr, i64, i32, ptr, ptr, ptr)\n"
      emit_raw "declare i32 @pcre2_match_8(ptr, ptr, i64, i64, i32, ptr, ptr)\n"
      emit_raw "declare ptr @pcre2_match_data_create_from_pattern_8(ptr, ptr)\n"
      emit_raw "declare i32 @pcre2_jit_compile_8(ptr, i32)\n"
      emit_raw "declare ptr @pcre2_get_ovector_pointer_8(ptr)\n"
      emit_raw "declare i32 @pcre2_get_ovector_count_8(ptr)\n"
      emit_raw "declare void @pcre2_match_data_free_8(ptr)\n"
      # Mark as emitted so emit_missing_crystal_function_stubs skips them
      @emitted_functions << "pcre2_compile_8" << "pcre2_match_8"
      @emitted_functions << "pcre2_match_data_create_from_pattern_8"
      @emitted_functions << "pcre2_jit_compile_8" << "pcre2_get_ovector_pointer_8"
      @emitted_functions << "pcre2_get_ovector_count_8" << "pcre2_match_data_free_8"
      emit_raw "\n"

      # Format strings for printing
      emit_raw "@.int_fmt = private constant [4 x i8] c\"%d\\0A\\00\"\n"
      emit_raw "@.int_fmt_no_nl = private constant [3 x i8] c\"%d\\00\"\n"
      emit_raw "@.long_fmt = private constant [5 x i8] c\"%ld\\0A\\00\"\n"
      emit_raw "@.long_fmt_no_nl = private constant [4 x i8] c\"%ld\\00\"\n"
      emit_raw "@.float_fmt_no_nl = private constant [3 x i8] c\"%g\\00\"\n"
      emit_raw "@.float_fmt = private constant [4 x i8] c\"%g\\0A\\00\"\n"
      # bool_to_string constants: Crystal String structs in prelude mode, C strings in no-prelude mode
      if @no_prelude
        emit_raw "@.str.true = private constant [5 x i8] c\"true\\00\", align 1\n"
        emit_raw "@.str.false = private constant [6 x i8] c\"false\\00\", align 1\n"
      else
        emit_raw "@.str.true = private constant { i32, i32, i32, [5 x i8] } { i32 #{@string_type_id}, i32 4, i32 4, [5 x i8] c\"true\\00\" }, align 8\n"
        emit_raw "@.str.false = private constant { i32, i32, i32, [6 x i8] } { i32 #{@string_type_id}, i32 5, i32 5, [6 x i8] c\"false\\00\" }, align 8\n"
      end
      emit_raw "\n"
      open_w = 1 | posix_open_flag_creat | posix_open_flag_trunc
      open_w_plus = 2 | posix_open_flag_creat | posix_open_flag_trunc
      open_a = 1 | posix_open_flag_creat | posix_open_flag_append
      open_a_plus = 2 | posix_open_flag_creat | posix_open_flag_append

      # Memory allocation - use calloc for zero-initialized memory
      # Crystal semantics require zero-init (like GC_MALLOC in original compiler).
      # The .new allocator only sets known fields; union/composite gaps need zeroing.
      #
      # CRITICAL: We call calloc/malloc/realloc through volatile-loaded function pointers.
      # This is necessary because LLVM's interprocedural Attributor pass can infer noalias
      # on our return value (by seeing we call calloc, even through noinline boundaries).
      # With noalias inferred, LLVM's heap-to-stack promotion converts calloc → alloca
      # when the allocation only escapes through a noreturn call (__crystal_v2_raise/longjmp).
      # After longjmp, the stack frame is destroyed and the exception pointer is garbage.
      # Volatile function pointer loads are opaque to LLVM — it can't constant-fold them
      # or infer allocation semantics from an indirect call to an unknown target.
      # In the original Crystal compiler, GC_MALLOC is in a separate shared library (libgc)
      # so LLVM can never see through it. This achieves the same opacity.
      emit_raw "@__crystal_v2_calloc_fn = global ptr @calloc\n"
      emit_raw "@__crystal_v2_malloc_fn = global ptr @malloc\n"
      emit_raw "@__crystal_v2_realloc_fn = global ptr @realloc\n\n"

      emit_raw "define ptr @__crystal_v2_malloc64(i64 %size) noinline {\n"
      emit_raw "  %fn = load volatile ptr, ptr @__crystal_v2_calloc_fn\n"
      emit_raw "  %ptr = call ptr %fn(i64 1, i64 %size)\n"
      emit_raw "  ret ptr %ptr\n"
      emit_raw "}\n\n"

      emit_raw "define ptr @__crystal_v2_realloc64(ptr %old_ptr, i64 %size) noinline {\n"
      emit_raw "  %fn = load volatile ptr, ptr @__crystal_v2_realloc_fn\n"
      emit_raw "  %ptr = call ptr %fn(ptr %old_ptr, i64 %size)\n"
      emit_raw "  ret ptr %ptr\n"
      emit_raw "}\n\n"

      emit_raw "define ptr @__crystal_malloc_atomic64(i64 %size) noinline {\n"
      emit_raw "  %fn = load volatile ptr, ptr @__crystal_v2_malloc_fn\n"
      emit_raw "  %ptr = call ptr %fn(i64 %size)\n"
      emit_raw "  ret ptr %ptr\n"
      emit_raw "}\n\n"

      emit_raw "define ptr @__crystal_realloc64(ptr %ptr, i64 %size) noinline {\n"
      emit_raw "  %fn = load volatile ptr, ptr @__crystal_v2_realloc_fn\n"
      emit_raw "  %new_ptr = call ptr %fn(ptr %ptr, i64 %size)\n"
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

      # Slab allocator - use calloc for zero-init
      emit_raw "define ptr @__crystal_v2_slab_alloc(i32 %size_class) {\n"
      emit_raw "  %size = sext i32 %size_class to i64\n"
      emit_raw "  %shift = shl i64 16, %size\n"
      emit_raw "  %ptr = call ptr @calloc(i64 1, i64 %shift)\n"
      emit_raw "  ret ptr %ptr\n"
      emit_raw "}\n\n"

      emit_raw "define void @__crystal_v2_slab_free(ptr %ptr, i32 %size_class) {\n"
      emit_raw "  call void @free(ptr %ptr)\n"
      emit_raw "  ret void\n"
      emit_raw "}\n\n"

      emit_raw "define void @__crystal_v2_slab_frame_push() {\n"
      emit_raw "  ret void\n"
      emit_raw "}\n\n"

      emit_raw "define void @__crystal_v2_slab_frame_pop() {\n"
      emit_raw "  ret void\n"
      emit_raw "}\n\n"

      # Convert Crystal file mode string to POSIX open flags.
      # Supports: "r", "w", "a" and their '+' variants.
      emit_raw "define i32 @__crystal_v2_mode_to_open_flags(ptr %mode) {\n"
      emit_raw "entry:\n"
      emit_raw "  %mode_cstr = getelementptr i8, ptr %mode, i32 12\n"
      emit_raw "  %m0 = load i8, ptr %mode_cstr\n"
      emit_raw "  switch i8 %m0, label %ret_rdonly [\n"
      emit_raw "    i8 114, label %mode_r\n"
      emit_raw "    i8 119, label %mode_w\n"
      emit_raw "    i8 97, label %mode_a\n"
      emit_raw "  ]\n"
      emit_raw "mode_r:\n"
      emit_raw "  %m1r_ptr = getelementptr i8, ptr %mode_cstr, i32 1\n"
      emit_raw "  %m1r = load i8, ptr %m1r_ptr\n"
      emit_raw "  %r_plus = icmp eq i8 %m1r, 43\n"
      emit_raw "  %r_flags = select i1 %r_plus, i32 2, i32 0\n"
      emit_raw "  ret i32 %r_flags\n"
      emit_raw "mode_w:\n"
      emit_raw "  %m1w_ptr = getelementptr i8, ptr %mode_cstr, i32 1\n"
      emit_raw "  %m1w = load i8, ptr %m1w_ptr\n"
      emit_raw "  %w_plus = icmp eq i8 %m1w, 43\n"
      emit_raw "  %w_flags = select i1 %w_plus, i32 #{open_w_plus}, i32 #{open_w}\n"
      emit_raw "  ret i32 %w_flags\n"
      emit_raw "mode_a:\n"
      emit_raw "  %m1a_ptr = getelementptr i8, ptr %mode_cstr, i32 1\n"
      emit_raw "  %m1a = load i8, ptr %m1a_ptr\n"
      emit_raw "  %a_plus = icmp eq i8 %m1a, 43\n"
      emit_raw "  %a_flags = select i1 %a_plus, i32 #{open_a_plus}, i32 #{open_a}\n"
      emit_raw "  ret i32 %a_flags\n"
      emit_raw "ret_rdonly:\n"
      emit_raw "  ret i32 0\n"
      emit_raw "}\n\n"

      # File.new(path, mode) helper — opens file via POSIX open(), returns ptr to {i32 fd, i1 blocking}
      # Takes plain ptr args (no union by-value) to avoid ARM64 ABI decomposition issues.
      emit_raw "define ptr @__crystal_v2_file_open(ptr %path, ptr %mode) {\n"
      emit_raw "entry:\n"
      emit_raw "  %cstr = getelementptr i8, ptr %path, i32 12\n"
      emit_raw "  %flags = call i32 @__crystal_v2_mode_to_open_flags(ptr %mode)\n"
      emit_raw "  %fd = call i32 (ptr, i32, ...) @open(ptr %cstr, i32 %flags, i32 438)\n"
      emit_raw "  %fd_neg = icmp slt i32 %fd, 0\n"
      emit_raw "  br i1 %fd_neg, label %error, label %success\n"
      emit_raw "error:\n"
      emit_raw "  call void @perror(ptr @.str.dbg_open_label)\n"
      emit_raw "  call void @__crystal_v2_raise_msg(ptr @.str.file_open_error)\n"
      emit_raw "  unreachable\n"
      emit_raw "success:\n"
      emit_raw "  %tup = call ptr @__crystal_v2_malloc64(i64 8)\n"
      emit_raw "  store i32 %fd, ptr %tup\n"
      emit_raw "  %cof_ptr = getelementptr i8, ptr %tup, i32 4\n"
      emit_raw "  store i1 1, ptr %cof_ptr\n"
      emit_raw "  ret ptr %tup\n"
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

      # Float print helper: finds shortest round-trip representation, handles -0.0
      # Uses write(2) syscall directly (like Crystal IO) to preserve output ordering.
      emit_raw "declare ptr @strchr(ptr, i32)\n"
      emit_raw "declare double @strtod(ptr, ptr)\n"
      emit_raw "declare i32 @snprintf(ptr, i64, ptr, ...)\n"
      emit_raw "declare i32 @strcmp(ptr, ptr)\n"
      emit_raw "declare i32 @memcmp(ptr, ptr, i32)\n"
      emit_raw "declare void @qsort(ptr, i64, i64, ptr)\n"
      emit_raw "declare i64 @strtol(ptr, ptr, i32)\n"
      emit_raw "declare i64 @strtoull(ptr, ptr, i32)\n"
      emit_raw "declare double @llvm.copysign.f64(double, double)\n"
      emit_raw "declare double @llvm.fabs.f64(double)\n"
      emit_raw "declare void @llvm.memmove.p0.p0.i64(ptr, ptr, i64, i1)\n"
      emit_raw "declare void @llvm.memcpy.p0.p0.i32(ptr, ptr, i32, i1)\n"
      emit_raw "declare void @llvm.memcpy.p0.p0.i64(ptr, ptr, i64, i1)\n"
      emit_raw "@.str_newline = private constant [2 x i8] c\"\\0A\\00\"\n"
      emit_raw "@.fixed_fmt = private constant [6 x i8] c\"%.17f\\00\"\n"
      emit_raw "@.prec_fmt = private constant [5 x i8] c\"%.*g\\00\"\n"
      emit_raw "@.str_neg_zero = private constant [5 x i8] c\"-0.0\\00\"\n"
      emit_raw "@.str.fmt.d = private constant [3 x i8] c\"%d\\00\"\n\n"

      emit_raw "define void @__crystal_v2_print_float_impl(double %val, i1 %newline) {\n"
      emit_raw "entry:\n"
      emit_raw "  %buf = alloca [64 x i8], align 8\n"
      # Check for negative zero: val == 0.0 && copysign(1.0, val) < 0.0
      emit_raw "  %is_zero = fcmp oeq double %val, 0.0\n"
      emit_raw "  %sign = call double @llvm.copysign.f64(double 1.0, double %val)\n"
      emit_raw "  %is_neg = fcmp olt double %sign, 0.0\n"
      emit_raw "  %is_neg_zero = and i1 %is_zero, %is_neg\n"
      emit_raw "  br i1 %is_neg_zero, label %neg_zero, label %try_format\n"

      # Negative zero: write "-0.0" directly
      emit_raw "neg_zero:\n"
      emit_raw "  call i64 @write(i32 1, ptr @.str_neg_zero, i64 4)\n"
      emit_raw "  br i1 %newline, label %write_nl, label %done\n"

      # Try increasing precision until round-trip matches (shortest representation)
      emit_raw "try_format:\n"
      emit_raw "  br label %try_prec\n"
      emit_raw "try_prec:\n"
      emit_raw "  %prec = phi i32 [1, %try_format], [%next_prec, %try_next]\n"
      emit_raw "  call i32 (ptr, i64, ptr, ...) @snprintf(ptr %buf, i64 64, ptr @.prec_fmt, i32 %prec, double %val)\n"
      emit_raw "  %parsed = call double @strtod(ptr %buf, ptr null)\n"
      emit_raw "  %matches = fcmp oeq double %parsed, %val\n"
      emit_raw "  br i1 %matches, label %found, label %try_next\n"
      emit_raw "try_next:\n"
      emit_raw "  %next_prec = add i32 %prec, 1\n"
      emit_raw "  %too_many = icmp sgt i32 %next_prec, 17\n"
      emit_raw "  br i1 %too_many, label %fallback, label %try_prec\n"
      emit_raw "fallback:\n"
      emit_raw "  call i32 (ptr, i64, ptr, ...) @snprintf(ptr %buf, i64 64, ptr @.prec_fmt, i32 17, double %val)\n"
      emit_raw "  br label %found\n"

      # Found shortest representation - check for scientific notation in normal range
      emit_raw "found:\n"
      emit_raw "  %has_e_f = call ptr @strchr(ptr %buf, i32 101)\n"
      emit_raw "  %is_sci = icmp ne ptr %has_e_f, null\n"
      emit_raw "  br i1 %is_sci, label %check_range, label %check_dot\n"

      # If scientific notation, check if value is in [0.001, 1e15) — use decimal instead
      # Crystal's point_range default is -3..15, meaning exponent 15+ → scientific
      emit_raw "check_range:\n"
      emit_raw "  %abs_val = call double @llvm.fabs.f64(double %val)\n"
      emit_raw "  %ge_min = fcmp oge double %abs_val, 0.001\n"
      emit_raw "  %lt_max = fcmp olt double %abs_val, 1.0e15\n"
      emit_raw "  %in_range = and i1 %ge_min, %lt_max\n"
      emit_raw "  br i1 %in_range, label %reformat_dec, label %sci_ensure_dot\n"

      # Scientific notation: ensure ".0" before 'e' (e.g. "1e+16" → "1.0e+16")
      emit_raw "sci_ensure_dot:\n"
      emit_raw "  %sci_has_dot = call ptr @strchr(ptr %buf, i32 46)\n"
      emit_raw "  %sci_needs_dot = icmp eq ptr %sci_has_dot, null\n"
      emit_raw "  br i1 %sci_needs_dot, label %sci_insert_dot, label %do_print\n"
      # Insert ".0" before 'e': shift tail right by 2, insert ".0"
      emit_raw "sci_insert_dot:\n"
      emit_raw "  %e_pos = ptrtoint ptr %has_e_f to i64\n"
      emit_raw "  %buf_start = ptrtoint ptr %buf to i64\n"
      emit_raw "  %e_offset = sub i64 %e_pos, %buf_start\n"
      emit_raw "  %tail_len = call i64 @strlen(ptr %has_e_f)\n"
      emit_raw "  %tail_len_1 = add i64 %tail_len, 1\n"
      emit_raw "  %dst = getelementptr i8, ptr %has_e_f, i32 2\n"
      emit_raw "  call void @llvm.memmove.p0.p0.i64(ptr %dst, ptr %has_e_f, i64 %tail_len_1, i1 0)\n"
      emit_raw "  store i8 46, ptr %has_e_f\n"
      emit_raw "  %dot_next = getelementptr i8, ptr %has_e_f, i32 1\n"
      emit_raw "  store i8 48, ptr %dot_next\n"
      emit_raw "  br label %do_print\n"

      # Reformat as %.17f and trim trailing zeros
      emit_raw "reformat_dec:\n"
      emit_raw "  call i32 (ptr, i64, ptr, ...) @snprintf(ptr %buf, i64 64, ptr @.fixed_fmt, double %val)\n"
      emit_raw "  %trim_len = call i64 @strlen(ptr %buf)\n"
      emit_raw "  br label %trim_loop\n"
      emit_raw "trim_loop:\n"
      emit_raw "  %tpos = phi i64 [%trim_len, %reformat_dec], [%tprev, %trim_cont]\n"
      emit_raw "  %tprev = sub i64 %tpos, 1\n"
      emit_raw "  %tch_ptr = getelementptr i8, ptr %buf, i64 %tprev\n"
      emit_raw "  %tch = load i8, ptr %tch_ptr\n"
      emit_raw "  %is_tzero = icmp eq i8 %tch, 48\n"
      emit_raw "  br i1 %is_tzero, label %trim_cont, label %trim_check\n"
      emit_raw "trim_cont:\n"
      emit_raw "  br label %trim_loop\n"
      emit_raw "trim_check:\n"
      # If we stopped at '.', keep one zero after it → ".0"
      emit_raw "  %is_tdot = icmp eq i8 %tch, 46\n"
      emit_raw "  br i1 %is_tdot, label %keep_dot_zero, label %trim_end\n"
      emit_raw "keep_dot_zero:\n"
      emit_raw "  %kdz_pos = add i64 %tpos, 1\n"
      emit_raw "  %kdz_ptr = getelementptr i8, ptr %buf, i64 %kdz_pos\n"
      emit_raw "  store i8 0, ptr %kdz_ptr\n"
      emit_raw "  br label %do_print\n"
      emit_raw "trim_end:\n"
      # Null-terminate after last non-zero char
      emit_raw "  %te_ptr = getelementptr i8, ptr %buf, i64 %tpos\n"
      emit_raw "  store i8 0, ptr %te_ptr\n"
      emit_raw "  br label %do_print\n"

      # Check if needs ".0" suffix (no decimal point, no exponent, no special)
      emit_raw "check_dot:\n"
      emit_raw "  %has_dot = call ptr @strchr(ptr %buf, i32 46)\n"
      emit_raw "  %has_n = call ptr @strchr(ptr %buf, i32 110)\n"
      emit_raw "  %has_i = call ptr @strchr(ptr %buf, i32 105)\n"
      emit_raw "  %pd = icmp ne ptr %has_dot, null\n"
      emit_raw "  %pn = icmp ne ptr %has_n, null\n"
      emit_raw "  %pi = icmp ne ptr %has_i, null\n"
      emit_raw "  %or_dn = or i1 %pd, %pn\n"
      emit_raw "  %has_special = or i1 %or_dn, %pi\n"
      emit_raw "  br i1 %has_special, label %do_print, label %append_dot\n"
      emit_raw "append_dot:\n"
      emit_raw "  %len0 = call i64 @strlen(ptr %buf)\n"
      emit_raw "  %end_ptr = getelementptr i8, ptr %buf, i64 %len0\n"
      emit_raw "  store i8 46, ptr %end_ptr\n"
      emit_raw "  %z_ptr = getelementptr i8, ptr %end_ptr, i32 1\n"
      emit_raw "  store i8 48, ptr %z_ptr\n"
      emit_raw "  %n_ptr = getelementptr i8, ptr %z_ptr, i32 1\n"
      emit_raw "  store i8 0, ptr %n_ptr\n"
      emit_raw "  br label %do_print\n"

      # Write the formatted number
      emit_raw "do_print:\n"
      emit_raw "  %len = call i64 @strlen(ptr %buf)\n"
      emit_raw "  call i64 @write(i32 1, ptr %buf, i64 %len)\n"
      emit_raw "  br i1 %newline, label %write_nl, label %done\n"
      emit_raw "write_nl:\n"
      emit_raw "  call i64 @write(i32 1, ptr @.str_newline, i64 1)\n"
      emit_raw "  br label %done\n"
      emit_raw "done:\n"
      emit_raw "  ret void\n"
      emit_raw "}\n\n"

      emit_raw "define void @__crystal_v2_print_float64(double %val) {\n"
      emit_raw "  call void @__crystal_v2_print_float_impl(double %val, i1 0)\n"
      emit_raw "  ret void\n"
      emit_raw "}\n\n"

      emit_raw "define void @__crystal_v2_print_float64_ln(double %val) {\n"
      emit_raw "  call void @__crystal_v2_print_float_impl(double %val, i1 1)\n"
      emit_raw "  ret void\n"
      emit_raw "}\n\n"

      emit_raw "define void @__crystal_v2_print_float32(float %val) {\n"
      emit_raw "  %ext = fpext float %val to double\n"
      emit_raw "  call void @__crystal_v2_print_float_impl(double %ext, i1 0)\n"
      emit_raw "  ret void\n"
      emit_raw "}\n\n"

      emit_raw "define void @__crystal_v2_print_float32_ln(float %val) {\n"
      emit_raw "  %ext = fpext float %val to double\n"
      emit_raw "  call void @__crystal_v2_print_float_impl(double %ext, i1 1)\n"
      emit_raw "  ret void\n"
      emit_raw "}\n\n"


      # String functions - implemented using C library
      emit_raw "declare i64 @strlen(ptr)\n"
      emit_raw "declare ptr @strcpy(ptr, ptr)\n"
      emit_raw "declare ptr @strcat(ptr, ptr)\n"
      emit_raw "declare i32 @sprintf(ptr, ptr, ...)\n"
      emit_raw "declare ptr @strstr(ptr, ptr)\n"
      emit_raw "declare i64 @write(i32, ptr, i64)\n"
      emit_raw "declare i32 @open(ptr, i32, ...)\n"
      emit_raw "declare i64 @lseek(i32, i64, i32)\n"
      emit_raw "declare i64 @read(i32, ptr, i64)\n"
      emit_raw "declare i32 @close(i32)\n"
      emit_raw "declare ptr @getcwd(ptr, i64)\n"
      emit_raw "declare ptr @realpath(ptr, ptr)\n"
      # Boehm GC library functions
      emit_raw "declare void @GC_init()\n"
      emit_raw "declare ptr @GC_malloc(i64)\n"
      emit_raw "declare ptr @GC_malloc_atomic(i64)\n"
      emit_raw "declare ptr @GC_realloc(ptr, i64)\n"
      emit_raw "declare void @GC_set_handle_fork(i32)\n"
      emit_raw "declare void @GC_set_start_callback(ptr)\n"
      emit_raw "declare void @GC_set_warn_proc(ptr)\n"
      emit_raw "declare ptr @GC_get_push_other_roots()\n"
      emit_raw "declare void @GC_set_push_other_roots(ptr)\n"
      emit_raw "declare void @GC_push_all_eager(ptr, ptr)\n"
      emit_raw "declare i32 @GC_get_my_stackbottom(ptr)\n"
      # macOS/POSIX system functions used by Crystal stdlib
      emit_raw "declare i32 @_NSGetExecutablePath(ptr, ptr)\n"
      emit_raw "declare i32 @stat(ptr, ptr)\n"
      emit_raw "declare i32 @stat$INODE64(ptr, ptr)\n"
      emit_raw "declare i32 @lstat(ptr, ptr)\n"
      emit_raw "declare i32 @lstat$INODE64(ptr, ptr)\n"
      emit_raw "declare i32 @fstat(i32, ptr)\n"
      emit_raw "declare i32 @fstat$INODE64(i32, ptr)\n"
      emit_raw "declare ptr @readdir(ptr)\n"
      emit_raw "declare ptr @opendir(ptr)\n"
      emit_raw "declare i32 @closedir(ptr)\n"
      emit_raw "declare i32 @mkdir(ptr, i32)\n"
      emit_raw "declare i32 @rmdir(ptr)\n"
      emit_raw "declare i32 @unlink(ptr)\n"
      emit_raw "declare i32 @rename(ptr, ptr)\n"
      emit_raw "declare i32 @access(ptr, i32)\n"
      emit_raw "declare i32 @chmod(ptr, i32)\n"
      emit_raw "declare i32 @chown(ptr, i32, i32)\n"
      emit_raw "declare ptr @getenv(ptr)\n"
      emit_raw "declare i32 @setenv(ptr, ptr, i32)\n"
      emit_raw "declare i32 @unsetenv(ptr)\n"
      emit_raw "declare i32 @isatty(i32)\n"
      emit_raw "declare i32 @ioctl(i32, i64, ...)\n"
      emit_raw "declare i32 @fcntl(i32, i32, ...)\n"
      emit_raw "declare i32 @pipe(ptr)\n"
      emit_raw "declare i32 @dup2(i32, i32)\n"
      emit_raw "declare i32 @ttyname_r(i32, ptr, i64)\n"
      emit_raw "declare i64 @sysconf(i32)\n"
      emit_raw "declare i32 @posix_spawn(ptr, ptr, ptr, ptr, ptr, ptr)\n"
      emit_raw "declare i32 @posix_spawn_file_actions_init(ptr)\n"
      emit_raw "declare i32 @posix_spawn_file_actions_destroy(ptr)\n"
      emit_raw "declare i32 @posix_spawn_file_actions_addclose(ptr, i32)\n"
      emit_raw "declare i32 @posix_spawn_file_actions_adddup2(ptr, i32, i32)\n"
      emit_raw "declare i32 @posix_spawn_file_actions_addopen(ptr, i32, ptr, i32, i32)\n"
      emit_raw "declare i32 @posix_spawnattr_init(ptr)\n"
      emit_raw "declare i32 @posix_spawnattr_destroy(ptr)\n"
      emit_raw "declare i32 @posix_spawnattr_setflags(ptr, i16)\n"
      emit_raw "declare i32 @waitpid(i32, ptr, i32)\n"
      emit_raw "declare i32 @kill(i32, i32)\n"
      emit_raw "declare i32 @getpid()\n"
      emit_raw "declare i32 @sigaction(i32, ptr, ptr)\n"
      emit_raw "declare i32 @sigemptyset(ptr)\n"
      emit_raw "declare i32 @sigfillset(ptr)\n"
      emit_raw "declare i32 @sigaddset(ptr, i32)\n"
      emit_raw "declare i64 @time(ptr)\n"
      emit_raw "declare ptr @localtime_r(ptr, ptr)\n"
      emit_raw "declare ptr @gmtime_r(ptr, ptr)\n"
      emit_raw "declare i64 @mktime(ptr)\n"
      emit_raw "declare i64 @timegm(ptr)\n"
      emit_raw "declare void @tzset()\n"
      emit_raw "declare i64 @strftime(ptr, i64, ptr, ptr)\n"
      emit_raw "declare i32 @gettimeofday(ptr, ptr)\n"
      emit_raw "declare i32 @clock_gettime(i32, ptr)\n"
      emit_raw "declare i32 @nanosleep(ptr, ptr)\n"
      emit_raw "declare ptr @mmap(ptr, i64, i32, i32, i32, i64)\n"
      emit_raw "declare i32 @munmap(ptr, i64)\n"
      emit_raw "declare i32 @mprotect(ptr, i64, i32)\n"
      emit_raw "declare i32 @kqueue()\n"
      emit_raw "declare i32 @kevent(i32, ptr, i32, ptr, i32, ptr)\n"
      emit_raw "declare ptr @dlopen(ptr, i32)\n"
      emit_raw "declare ptr @dlsym(ptr, ptr)\n"
      emit_raw "declare i32 @dlclose(ptr)\n"
      emit_raw "declare ptr @dlerror()\n"
      emit_raw "declare i32 @pthread_create(ptr, ptr, ptr, ptr)\n"
      emit_raw "declare i32 @pthread_join(ptr, ptr)\n"
      emit_raw "declare ptr @pthread_self()\n"
      emit_raw "declare i32 @pthread_sigmask(i32, ptr, ptr)\n"
      emit_raw "declare i32 @pthread_attr_init(ptr)\n"
      emit_raw "declare i32 @pthread_attr_destroy(ptr)\n"
      emit_raw "declare i32 @pthread_attr_setstack(ptr, ptr, i64)\n"
      emit_raw "declare i32 @pthread_attr_getstacksize(ptr, ptr)\n"
      emit_raw "declare i32 @gethostname(ptr, i64)\n"
      emit_raw "declare i32 @socket(i32, i32, i32)\n"
      emit_raw "declare i32 @connect(i32, ptr, i32)\n"
      emit_raw "declare i32 @bind(i32, ptr, i32)\n"
      emit_raw "declare i32 @listen(i32, i32)\n"
      emit_raw "declare i32 @accept(i32, ptr, ptr)\n"
      emit_raw "declare i32 @setsockopt(i32, i32, i32, ptr, i32)\n"
      emit_raw "declare i32 @getsockopt(i32, i32, i32, ptr, ptr)\n"
      emit_raw "declare i64 @send(i32, ptr, i64, i32)\n"
      emit_raw "declare i64 @recv(i32, ptr, i64, i32)\n"
      emit_raw "declare i32 @shutdown(i32, i32)\n"
      emit_raw "declare i32 @getaddrinfo(ptr, ptr, ptr, ptr)\n"
      emit_raw "declare void @freeaddrinfo(ptr)\n"
      emit_raw "declare ptr @gai_strerror(i32)\n"
      emit_raw "declare ptr @strerror(ptr)\n"
      emit_raw "declare i32 @memset(ptr, i32, i64)\n"
      emit_raw "declare ptr @memcpy(ptr, ptr, i64)\n"
      emit_raw "declare ptr @memmove(ptr, ptr, i64)\n"
      emit_raw "declare i32 @strncmp(ptr, ptr, i64)\n"
      emit_raw "declare i64 @fwrite(ptr, i64, i64, ptr)\n"
      emit_raw "declare i32 @fflush(ptr)\n"
      emit_raw "declare i32 @fclose(ptr)\n"
      emit_raw "declare ptr @fopen(ptr, ptr)\n"
      emit_raw "declare i32 @fileno(ptr)\n"
      emit_raw "declare ptr @fdopen(i32, ptr)\n"
      emit_raw "declare i32 @getrlimit(i32, ptr)\n"
      emit_raw "declare i32 @setrlimit(i32, ptr)\n"
      # Mark C stdlib functions as emitted so emit_undefined_extern_declarations
      # and emit_missing_crystal_function_stubs skip them (prevents duplicate declarations)
      @emitted_functions << "strlen" << "strcpy" << "strcat" << "sprintf" << "strstr"
      @emitted_functions << "write" << "open" << "lseek" << "read" << "close"
      @emitted_functions << "getcwd" << "realpath"
      @emitted_functions << "malloc" << "calloc" << "realloc" << "free"
      @emitted_functions << "printf" << "puts" << "gets" << "fgets" << "fputs"
      @emitted_functions << "exit" << "perror" << "dprintf"
      @emitted_functions << "GC_init" << "GC_malloc" << "GC_malloc_atomic" << "GC_realloc"
      @emitted_functions << "GC_set_handle_fork" << "GC_set_start_callback" << "GC_set_warn_proc"
      @emitted_functions << "GC_get_push_other_roots" << "GC_set_push_other_roots"
      @emitted_functions << "GC_push_all_eager" << "GC_get_my_stackbottom"
      @emitted_functions << "_NSGetExecutablePath"
      @emitted_functions << "stat" << "stat$INODE64" << "lstat" << "lstat$INODE64"
      @emitted_functions << "fstat" << "fstat$INODE64"
      @emitted_functions << "readdir" << "opendir" << "closedir"
      @emitted_functions << "mkdir" << "rmdir" << "unlink" << "rename" << "access" << "chmod" << "chown"
      @emitted_functions << "getenv" << "setenv" << "unsetenv"
      @emitted_functions << "isatty" << "ioctl" << "fcntl" << "pipe" << "dup2" << "ttyname_r" << "sysconf"
      @emitted_functions << "posix_spawn" << "posix_spawn_file_actions_init" << "posix_spawn_file_actions_destroy"
      @emitted_functions << "posix_spawn_file_actions_addclose" << "posix_spawn_file_actions_adddup2"
      @emitted_functions << "posix_spawn_file_actions_addopen"
      @emitted_functions << "posix_spawnattr_init" << "posix_spawnattr_destroy" << "posix_spawnattr_setflags"
      @emitted_functions << "waitpid" << "kill" << "getpid"
      @emitted_functions << "sigaction" << "sigemptyset" << "sigfillset" << "sigaddset"
      @emitted_functions << "time" << "localtime_r" << "gmtime_r" << "mktime" << "timegm" << "tzset"
      @emitted_functions << "strftime" << "gettimeofday" << "clock_gettime" << "nanosleep"
      @emitted_functions << "mmap" << "munmap" << "mprotect"
      @emitted_functions << "kqueue" << "kevent"
      @emitted_functions << "dlopen" << "dlsym" << "dlclose" << "dlerror"
      @emitted_functions << "pthread_create" << "pthread_join" << "pthread_self" << "pthread_sigmask"
      @emitted_functions << "pthread_attr_init" << "pthread_attr_destroy" << "pthread_attr_setstack" << "pthread_attr_getstacksize"
      @emitted_functions << "gethostname" << "socket" << "connect" << "bind" << "listen" << "accept"
      @emitted_functions << "setsockopt" << "getsockopt" << "send" << "recv" << "shutdown"
      @emitted_functions << "getaddrinfo" << "freeaddrinfo" << "gai_strerror"
      @emitted_functions << "strerror" << "memset" << "memcpy" << "memmove" << "strncmp"
      @emitted_functions << "fwrite" << "fflush" << "fclose" << "fopen" << "fileno" << "fdopen"
      @emitted_functions << "getrlimit" << "setrlimit"
      emit_raw "\n"

      # String#includes?(String) — compares byte data via strstr
      # Crystal String layout: { i32 type_id, i32 bytesize, i32 length, [N x i8] data }
      # Data starts at offset 12
      emit_raw "define i1 @__crystal_v2_string_includes_string(ptr %self, ptr %search) {\n"
      emit_raw "  %self_data = getelementptr i8, ptr %self, i32 12\n"
      emit_raw "  %search_data = getelementptr i8, ptr %search, i32 12\n"
      emit_raw "  %result = call ptr @strstr(ptr %self_data, ptr %search_data)\n"
      emit_raw "  %found = icmp ne ptr %result, null\n"
      emit_raw "  ret i1 %found\n"
      emit_raw "}\n\n"

      # String#bytesize → i32 (read from offset 4)
      emit_raw "define i32 @__crystal_v2_string_bytesize(ptr %self) {\n"
      emit_raw "  %bs_ptr = getelementptr i8, ptr %self, i32 4\n"
      emit_raw "  %bs = load i32, ptr %bs_ptr\n"
      emit_raw "  ret i32 %bs\n"
      emit_raw "}\n\n"

      # String byte_at(index) → i32 (read byte at offset 12 + index, zero-extend to i32 for Char)
      emit_raw "define i32 @__crystal_v2_string_byte_at(ptr %self, i32 %idx) {\n"
      emit_raw "  %data = getelementptr i8, ptr %self, i32 12\n"
      emit_raw "  %byte_ptr = getelementptr i8, ptr %data, i32 %idx\n"
      emit_raw "  %byte = load i8, ptr %byte_ptr\n"
      emit_raw "  %ch = zext i8 %byte to i32\n"
      emit_raw "  ret i32 %ch\n"
      emit_raw "}\n\n"

      # String#index(String, offset) → i32 (-1 if not found, byte index otherwise)
      # Uses strstr on data portion, accounts for offset, returns char index
      emit_raw "define i32 @__crystal_v2_string_index_string(ptr %self, ptr %search, i32 %offset) {\n"
      emit_raw "entry:\n"
      emit_raw "  %self_data = getelementptr i8, ptr %self, i32 12\n"
      emit_raw "  %search_data = getelementptr i8, ptr %search, i32 12\n"
      emit_raw "  %start = getelementptr i8, ptr %self_data, i32 %offset\n"
      emit_raw "  %result = call ptr @strstr(ptr %start, ptr %search_data)\n"
      emit_raw "  %found = icmp ne ptr %result, null\n"
      emit_raw "  br i1 %found, label %calc_idx, label %ret_neg\n"
      emit_raw "calc_idx:\n"
      emit_raw "  %self_int = ptrtoint ptr %self_data to i64\n"
      emit_raw "  %result_int = ptrtoint ptr %result to i64\n"
      emit_raw "  %diff = sub i64 %result_int, %self_int\n"
      emit_raw "  %idx = trunc i64 %diff to i32\n"
      emit_raw "  ret i32 %idx\n"
      emit_raw "ret_neg:\n"
      emit_raw "  ret i32 -1\n"
      emit_raw "}\n\n"

      # String#index(Char, offset) → i32 (-1 if not found, byte index otherwise)
      # Scans string data byte-by-byte for the given ASCII char
      emit_raw "define i32 @__crystal_v2_string_index_char(ptr %self, i32 %ch, i32 %offset) {\n"
      emit_raw "entry:\n"
      emit_raw "  %bs_ptr = getelementptr i8, ptr %self, i32 4\n"
      emit_raw "  %bs = load i32, ptr %bs_ptr\n"
      emit_raw "  %self_data = getelementptr i8, ptr %self, i32 12\n"
      emit_raw "  %ch8 = trunc i32 %ch to i8\n"
      emit_raw "  br label %loop\n"
      emit_raw "loop:\n"
      emit_raw "  %i = phi i32 [%offset, %entry], [%next_i, %loop_cont]\n"
      emit_raw "  %done = icmp sge i32 %i, %bs\n"
      emit_raw "  br i1 %done, label %ret_neg, label %check\n"
      emit_raw "check:\n"
      emit_raw "  %ptr = getelementptr i8, ptr %self_data, i32 %i\n"
      emit_raw "  %byte = load i8, ptr %ptr\n"
      emit_raw "  %match = icmp eq i8 %byte, %ch8\n"
      emit_raw "  br i1 %match, label %ret_found, label %loop_cont\n"
      emit_raw "loop_cont:\n"
      emit_raw "  %next_i = add i32 %i, 1\n"
      emit_raw "  br label %loop\n"
      emit_raw "ret_found:\n"
      emit_raw "  ret i32 %i\n"
      emit_raw "ret_neg:\n"
      emit_raw "  ret i32 -1\n"
      emit_raw "}\n\n"

      # String#gsub(String, String) → Crystal String
      # Two-pass: first count occurrences to compute size, then build result
      emit_raw "define ptr @__crystal_v2_string_gsub(ptr %self, ptr %search, ptr %repl) {\n"
      emit_raw "entry:\n"
      # Load self bytesize and data
      emit_raw "  %self_bs_ptr = getelementptr i8, ptr %self, i32 4\n"
      emit_raw "  %self_bs = load i32, ptr %self_bs_ptr\n"
      emit_raw "  %self_data = getelementptr i8, ptr %self, i32 12\n"
      # Load search bytesize and data
      emit_raw "  %search_bs_ptr = getelementptr i8, ptr %search, i32 4\n"
      emit_raw "  %search_bs = load i32, ptr %search_bs_ptr\n"
      emit_raw "  %search_data = getelementptr i8, ptr %search, i32 12\n"
      # Load replacement bytesize and data
      emit_raw "  %repl_bs_ptr = getelementptr i8, ptr %repl, i32 4\n"
      emit_raw "  %repl_bs = load i32, ptr %repl_bs_ptr\n"
      emit_raw "  %repl_data = getelementptr i8, ptr %repl, i32 12\n"
      # Check empty search → return self
      emit_raw "  %search_empty = icmp eq i32 %search_bs, 0\n"
      emit_raw "  br i1 %search_empty, label %return_self, label %count_loop\n"
      emit_raw "return_self:\n"
      emit_raw "  ret ptr %self\n"
      # Pass 1: count occurrences
      emit_raw "count_loop:\n"
      emit_raw "  %ci = phi i32 [0, %entry], [%ci_next, %count_found], [%ci_skip, %count_notfound]\n"
      emit_raw "  %cnt = phi i32 [0, %entry], [%cnt_inc, %count_found], [%cnt, %count_notfound]\n"
      emit_raw "  %ci_end = icmp sge i32 %ci, %self_bs\n"
      emit_raw "  br i1 %ci_end, label %allocate, label %count_check\n"
      emit_raw "count_check:\n"
      emit_raw "  %ci_remain = sub i32 %self_bs, %ci\n"
      emit_raw "  %ci_enough = icmp sge i32 %ci_remain, %search_bs\n"
      emit_raw "  br i1 %ci_enough, label %count_compare, label %allocate\n"
      emit_raw "count_compare:\n"
      emit_raw "  %ci_src = getelementptr i8, ptr %self_data, i32 %ci\n"
      emit_raw "  %ci_cmp = call i32 @memcmp(ptr %ci_src, ptr %search_data, i32 %search_bs)\n"
      emit_raw "  %ci_match = icmp eq i32 %ci_cmp, 0\n"
      emit_raw "  br i1 %ci_match, label %count_found, label %count_notfound\n"
      emit_raw "count_found:\n"
      emit_raw "  %cnt_inc = add i32 %cnt, 1\n"
      emit_raw "  %ci_next = add i32 %ci, %search_bs\n"
      emit_raw "  br label %count_loop\n"
      emit_raw "count_notfound:\n"
      emit_raw "  %ci_skip = add i32 %ci, 1\n"
      emit_raw "  br label %count_loop\n"
      # Allocate result: self_bs - (cnt * search_bs) + (cnt * repl_bs) + 13
      emit_raw "allocate:\n"
      emit_raw "  %cnt2 = phi i32 [%cnt, %count_loop], [%cnt, %count_check]\n"
      emit_raw "  %removed = mul i32 %cnt2, %search_bs\n"
      emit_raw "  %added = mul i32 %cnt2, %repl_bs\n"
      emit_raw "  %result_bs = sub i32 %self_bs, %removed\n"
      emit_raw "  %result_bs2 = add i32 %result_bs, %added\n"
      emit_raw "  %alloc_sz = add i32 %result_bs2, 13\n"
      emit_raw "  %alloc_sz64 = sext i32 %alloc_sz to i64\n"
      emit_raw "  %result = call ptr @__crystal_v2_malloc64(i64 %alloc_sz64)\n"
      # Write Crystal String header
      emit_raw "  store i32 #{@string_type_id}, ptr %result\n"
      emit_raw "  %r_bs = getelementptr i8, ptr %result, i32 4\n"
      emit_raw "  store i32 %result_bs2, ptr %r_bs\n"
      emit_raw "  %r_sz = getelementptr i8, ptr %result, i32 8\n"
      emit_raw "  store i32 %result_bs2, ptr %r_sz\n"
      emit_raw "  %r_data = getelementptr i8, ptr %result, i32 12\n"
      # Pass 2: copy with replacements
      emit_raw "  br label %copy_loop\n"
      emit_raw "copy_loop:\n"
      emit_raw "  %si = phi i32 [0, %allocate], [%si_next, %copy_found], [%si_skip, %copy_notfound]\n"
      emit_raw "  %di = phi i32 [0, %allocate], [%di_next_f, %copy_found], [%di_next_n, %copy_notfound]\n"
      emit_raw "  %si_end = icmp sge i32 %si, %self_bs\n"
      emit_raw "  br i1 %si_end, label %finalize, label %copy_check\n"
      emit_raw "copy_check:\n"
      emit_raw "  %si_remain = sub i32 %self_bs, %si\n"
      emit_raw "  %si_enough = icmp sge i32 %si_remain, %search_bs\n"
      emit_raw "  br i1 %si_enough, label %copy_compare, label %copy_tail\n"
      emit_raw "copy_compare:\n"
      emit_raw "  %si_src = getelementptr i8, ptr %self_data, i32 %si\n"
      emit_raw "  %si_cmp = call i32 @memcmp(ptr %si_src, ptr %search_data, i32 %search_bs)\n"
      emit_raw "  %si_match = icmp eq i32 %si_cmp, 0\n"
      emit_raw "  br i1 %si_match, label %copy_found, label %copy_notfound\n"
      emit_raw "copy_found:\n"
      # Copy replacement
      emit_raw "  %di_dst_f = getelementptr i8, ptr %r_data, i32 %di\n"
      emit_raw "  %repl_bs64 = sext i32 %repl_bs to i64\n"
      emit_raw "  call void @llvm.memcpy.p0.p0.i64(ptr %di_dst_f, ptr %repl_data, i64 %repl_bs64, i1 false)\n"
      emit_raw "  %si_next = add i32 %si, %search_bs\n"
      emit_raw "  %di_next_f = add i32 %di, %repl_bs\n"
      emit_raw "  br label %copy_loop\n"
      emit_raw "copy_notfound:\n"
      # Copy one byte from source
      emit_raw "  %si_src2 = getelementptr i8, ptr %self_data, i32 %si\n"
      emit_raw "  %byte = load i8, ptr %si_src2\n"
      emit_raw "  %di_dst_n = getelementptr i8, ptr %r_data, i32 %di\n"
      emit_raw "  store i8 %byte, ptr %di_dst_n\n"
      emit_raw "  %si_skip = add i32 %si, 1\n"
      emit_raw "  %di_next_n = add i32 %di, 1\n"
      emit_raw "  br label %copy_loop\n"
      # Copy remaining tail bytes (when remaining < search_bs)
      emit_raw "copy_tail:\n"
      emit_raw "  %tail_src = getelementptr i8, ptr %self_data, i32 %si\n"
      emit_raw "  %tail_dst = getelementptr i8, ptr %r_data, i32 %di\n"
      emit_raw "  %tail_len = sub i32 %self_bs, %si\n"
      emit_raw "  %tail_len64 = sext i32 %tail_len to i64\n"
      emit_raw "  call void @llvm.memcpy.p0.p0.i64(ptr %tail_dst, ptr %tail_src, i64 %tail_len64, i1 false)\n"
      emit_raw "  %di_tail = add i32 %di, %tail_len\n"
      emit_raw "  br label %finalize\n"
      emit_raw "finalize:\n"
      emit_raw "  %final_di = phi i32 [%di, %copy_loop], [%di_tail, %copy_tail]\n"
      emit_raw "  %null_pos = getelementptr i8, ptr %r_data, i32 %final_di\n"
      emit_raw "  store i8 0, ptr %null_pos\n"
      emit_raw "  ret ptr %result\n"
      emit_raw "}\n\n"

      # String#gsub(Char, Char) → Crystal String
      # Simple byte-for-byte replacement: iterate string, replace matching bytes
      # Note: only works correctly for ASCII chars (single-byte). Multi-byte chars need more complex handling.
      emit_raw "define ptr @__crystal_v2_string_gsub_char(ptr %self, i32 %search, i32 %repl) {\n"
      emit_raw "entry:\n"
      emit_raw "  %self_bs_ptr = getelementptr i8, ptr %self, i32 4\n"
      emit_raw "  %self_bs = load i32, ptr %self_bs_ptr\n"
      emit_raw "  %self_data = getelementptr i8, ptr %self, i32 12\n"
      # Truncate Char (i32) to i8 for search/replace bytes
      emit_raw "  %search_byte = trunc i32 %search to i8\n"
      emit_raw "  %repl_byte = trunc i32 %repl to i8\n"
      # Allocate result: same size as self (gsub char→char preserves length)
      emit_raw "  %alloc_sz = add i32 %self_bs, 13\n"
      emit_raw "  %alloc_sz64 = sext i32 %alloc_sz to i64\n"
      emit_raw "  %result = call ptr @__crystal_v2_malloc64(i64 %alloc_sz64)\n"
      # Write Crystal String header
      emit_raw "  store i32 #{@string_type_id}, ptr %result\n"
      emit_raw "  %r_bs = getelementptr i8, ptr %result, i32 4\n"
      emit_raw "  store i32 %self_bs, ptr %r_bs\n"
      emit_raw "  %r_sz = getelementptr i8, ptr %result, i32 8\n"
      emit_raw "  store i32 %self_bs, ptr %r_sz\n"
      emit_raw "  %r_data = getelementptr i8, ptr %result, i32 12\n"
      # Loop: copy bytes, replacing matches
      emit_raw "  br label %loop\n"
      emit_raw "loop:\n"
      emit_raw "  %i = phi i32 [0, %entry], [%i_next, %store]\n"
      emit_raw "  %done = icmp sge i32 %i, %self_bs\n"
      emit_raw "  br i1 %done, label %finalize, label %body\n"
      emit_raw "body:\n"
      emit_raw "  %src_ptr = getelementptr i8, ptr %self_data, i32 %i\n"
      emit_raw "  %byte = load i8, ptr %src_ptr\n"
      emit_raw "  %match = icmp eq i8 %byte, %search_byte\n"
      emit_raw "  %out_byte = select i1 %match, i8 %repl_byte, i8 %byte\n"
      emit_raw "  br label %store\n"
      emit_raw "store:\n"
      emit_raw "  %dst_ptr = getelementptr i8, ptr %r_data, i32 %i\n"
      emit_raw "  store i8 %out_byte, ptr %dst_ptr\n"
      emit_raw "  %i_next = add i32 %i, 1\n"
      emit_raw "  br label %loop\n"
      emit_raw "finalize:\n"
      # Null terminate
      emit_raw "  %null_pos = getelementptr i8, ptr %r_data, i32 %self_bs\n"
      emit_raw "  store i8 0, ptr %null_pos\n"
      emit_raw "  ret ptr %result\n"
      emit_raw "}\n\n"

      # String#byte_slice(start, length) → Crystal String
      # Extracts a substring by byte offset and length
      emit_raw "define ptr @__crystal_v2_string_byte_slice(ptr %self, i32 %start, i32 %len) {\n"
      emit_raw "entry:\n"
      emit_raw "  %self_bs_ptr = getelementptr i8, ptr %self, i32 4\n"
      emit_raw "  %self_bs = load i32, ptr %self_bs_ptr\n"
      emit_raw "  %self_data = getelementptr i8, ptr %self, i32 12\n"
      # Clamp start to [0, self_bs]
      emit_raw "  %start_neg = icmp slt i32 %start, 0\n"
      emit_raw "  %start_clamped = select i1 %start_neg, i32 0, i32 %start\n"
      emit_raw "  %start_over = icmp sgt i32 %start_clamped, %self_bs\n"
      emit_raw "  %start_final = select i1 %start_over, i32 %self_bs, i32 %start_clamped\n"
      # Clamp length to available bytes
      emit_raw "  %avail = sub i32 %self_bs, %start_final\n"
      emit_raw "  %len_over = icmp sgt i32 %len, %avail\n"
      emit_raw "  %len_final = select i1 %len_over, i32 %avail, i32 %len\n"
      emit_raw "  %len_neg = icmp slt i32 %len_final, 0\n"
      emit_raw "  %len_safe = select i1 %len_neg, i32 0, i32 %len_final\n"
      # Allocate result Crystal String
      emit_raw "  %alloc_i32 = add i32 %len_safe, 13\n"
      emit_raw "  %alloc_i64 = sext i32 %alloc_i32 to i64\n"
      emit_raw "  %result = call ptr @__crystal_v2_malloc64(i64 %alloc_i64)\n"
      emit_raw "  store i32 #{@string_type_id}, ptr %result\n"
      emit_raw "  %r_bs = getelementptr i8, ptr %result, i32 4\n"
      emit_raw "  store i32 %len_safe, ptr %r_bs\n"
      emit_raw "  %r_sz = getelementptr i8, ptr %result, i32 8\n"
      emit_raw "  store i32 %len_safe, ptr %r_sz\n"
      emit_raw "  %r_data = getelementptr i8, ptr %result, i32 12\n"
      # Copy data
      emit_raw "  %src = getelementptr i8, ptr %self_data, i32 %start_final\n"
      emit_raw "  %len64 = sext i32 %len_safe to i64\n"
      emit_raw "  call void @llvm.memcpy.p0.p0.i64(ptr %r_data, ptr %src, i64 %len64, i1 false)\n"
      # Null-terminate
      emit_raw "  %null_pos = getelementptr i8, ptr %r_data, i32 %len_safe\n"
      emit_raw "  store i8 0, ptr %null_pos\n"
      emit_raw "  ret ptr %result\n"
      emit_raw "}\n\n"

      # String#== — compare two Crystal Strings by content
      # 1. Pointer equality (same object → true)
      # 2. Bytesize comparison (different lengths → false)
      # 3. memcmp on the data bytes
      emit_raw "define i1 @__crystal_v2_string_eq(ptr %a, ptr %b) {\n"
      emit_raw "entry:\n"
      # Guard against null pointers (e.g., deleted hash entries with zeroed keys)
      emit_raw "  %a_null = icmp eq ptr %a, null\n"
      emit_raw "  br i1 %a_null, label %ret_false, label %check_b\n"
      emit_raw "check_b:\n"
      emit_raw "  %b_null = icmp eq ptr %b, null\n"
      emit_raw "  br i1 %b_null, label %ret_false, label %check_same\n"
      emit_raw "check_same:\n"
      emit_raw "  %same = icmp eq ptr %a, %b\n"
      emit_raw "  br i1 %same, label %ret_true, label %check_size\n"
      emit_raw "check_size:\n"
      emit_raw "  %a_bs_ptr = getelementptr i8, ptr %a, i32 4\n"
      emit_raw "  %a_bs = load i32, ptr %a_bs_ptr\n"
      emit_raw "  %b_bs_ptr = getelementptr i8, ptr %b, i32 4\n"
      emit_raw "  %b_bs = load i32, ptr %b_bs_ptr\n"
      emit_raw "  %size_eq = icmp eq i32 %a_bs, %b_bs\n"
      emit_raw "  br i1 %size_eq, label %check_data, label %ret_false\n"
      emit_raw "check_data:\n"
      emit_raw "  %a_data = getelementptr i8, ptr %a, i32 12\n"
      emit_raw "  %b_data = getelementptr i8, ptr %b, i32 12\n"
      emit_raw "  %cmp = call i32 @memcmp(ptr %a_data, ptr %b_data, i32 %a_bs)\n"
      emit_raw "  %eq = icmp eq i32 %cmp, 0\n"
      emit_raw "  br i1 %eq, label %ret_true, label %ret_false\n"
      emit_raw "ret_true:\n"
      emit_raw "  ret i1 1\n"
      emit_raw "ret_false:\n"
      emit_raw "  ret i1 0\n"
      emit_raw "}\n\n"

      # String#to_i — convert Crystal String to Int32 via strtol
      # Data starts at offset 12 in Crystal String layout
      emit_raw "define i32 @__crystal_v2_string_to_i(ptr %self) {\n"
      emit_raw "  %data = getelementptr i8, ptr %self, i32 12\n"
      emit_raw "  %val = call i64 @strtol(ptr %data, ptr null, i32 10)\n"
      emit_raw "  %result = trunc i64 %val to i32\n"
      emit_raw "  ret i32 %result\n"
      emit_raw "}\n\n"

      # String#to_i64 — convert Crystal String to Int64 via strtol
      emit_raw "define i64 @__crystal_v2_string_to_i64(ptr %self) {\n"
      emit_raw "  %data = getelementptr i8, ptr %self, i32 12\n"
      emit_raw "  %val = call i64 @strtol(ptr %data, ptr null, i32 10)\n"
      emit_raw "  ret i64 %val\n"
      emit_raw "}\n\n"

      # String#to_u64 — convert Crystal String to UInt64 via strtoull
      emit_raw "define i64 @__crystal_v2_string_to_u64(ptr %self) {\n"
      emit_raw "  %data = getelementptr i8, ptr %self, i32 12\n"
      emit_raw "  %val = call i64 @strtoull(ptr %data, ptr null, i32 10)\n"
      emit_raw "  ret i64 %val\n"
      emit_raw "}\n\n"

      # String#[](Int32, Int32) — extract substring (start, count)
      # Returns new Crystal String with the specified byte range
      emit_raw "define ptr @__crystal_v2_string_substring(ptr %self, i32 %start, i32 %count) {\n"
      emit_raw "entry:\n"
      emit_raw "  %bs_ptr = getelementptr i8, ptr %self, i32 4\n"
      emit_raw "  %bytesize = load i32, ptr %bs_ptr\n"
      # Clamp start: if negative, add bytesize
      emit_raw "  %neg = icmp slt i32 %start, 0\n"
      emit_raw "  %adj_start = add i32 %start, %bytesize\n"
      emit_raw "  %real_start = select i1 %neg, i32 %adj_start, i32 %start\n"
      # Clamp count: min(count, bytesize - real_start)
      emit_raw "  %remaining = sub i32 %bytesize, %real_start\n"
      emit_raw "  %too_much = icmp sgt i32 %count, %remaining\n"
      emit_raw "  %real_count = select i1 %too_much, i32 %remaining, i32 %count\n"
      # Clamp to non-negative
      emit_raw "  %neg_count = icmp slt i32 %real_count, 0\n"
      emit_raw "  %final_count = select i1 %neg_count, i32 0, i32 %real_count\n"
      # Get data pointer + start
      emit_raw "  %data_base = getelementptr i8, ptr %self, i32 12\n"
      emit_raw "  %data_start = getelementptr i8, ptr %data_base, i32 %real_start\n"
      # Get String type_id from self
      emit_raw "  %tid = load i32, ptr %self\n"
      emit_raw "  %result = call ptr @__crystal_v2_create_substring(ptr %data_start, i32 %final_count, i32 %tid)\n"
      emit_raw "  ret ptr %result\n"
      emit_raw "}\n\n"

      # Array#sum for Int32 — loops over Crystal Array buffer and sums elements
      # Crystal Array layout: { i32 type_id, i32 @size, i32 @capacity, i32 @offset_to_buffer, ptr @buffer }
      emit_raw "define i32 @__crystal_v2_array_sum_int32(ptr %arr) {\n"
      emit_raw "entry:\n"
      emit_raw "  %size_ptr = getelementptr i8, ptr %arr, i32 4\n"
      emit_raw "  %size = load i32, ptr %size_ptr\n"
      emit_raw "  %buf_ptr = getelementptr i8, ptr %arr, i32 16\n"
      emit_raw "  %buffer = load ptr, ptr %buf_ptr\n"
      emit_raw "  %cmp0 = icmp sle i32 %size, 0\n"
      emit_raw "  br i1 %cmp0, label %done, label %loop\n"
      emit_raw "loop:\n"
      emit_raw "  %i = phi i32 [0, %entry], [%i_next, %loop]\n"
      emit_raw "  %sum = phi i32 [0, %entry], [%sum_next, %loop]\n"
      emit_raw "  %elem_ptr = getelementptr i32, ptr %buffer, i32 %i\n"
      emit_raw "  %elem = load i32, ptr %elem_ptr\n"
      emit_raw "  %sum_next = add i32 %sum, %elem\n"
      emit_raw "  %i_next = add i32 %i, 1\n"
      emit_raw "  %cmp = icmp slt i32 %i_next, %size\n"
      emit_raw "  br i1 %cmp, label %loop, label %done\n"
      emit_raw "done:\n"
      emit_raw "  %result = phi i32 [0, %entry], [%sum_next, %loop]\n"
      emit_raw "  ret i32 %result\n"
      emit_raw "}\n\n"

      # Array.new(size, initial_i32_value) → creates filled Crystal Array
      # Creates proper 24-byte Array header with buffer filled with initial value
      emit_raw "define ptr @__crystal_v2_array_new_filled_i32(i32 %size, i32 %val) {\n"
      emit_raw "entry:\n"
      # Allocate 24-byte Crystal Array header
      emit_raw "  %arr = call ptr @__crystal_v2_malloc64(i64 24)\n"
      emit_raw "  store i32 #{array_runtime_type_id_for_element(TypeRef::INT32)}, ptr %arr\n"
      emit_raw "  %sz_ptr = getelementptr i8, ptr %arr, i32 4\n"
      emit_raw "  store i32 %size, ptr %sz_ptr\n"  # @size = size
      emit_raw "  %cap_ptr = getelementptr i8, ptr %arr, i32 8\n"
      emit_raw "  store i32 %size, ptr %cap_ptr\n"  # @capacity = size
      emit_raw "  %otb_ptr = getelementptr i8, ptr %arr, i32 12\n"
      emit_raw "  store i32 0, ptr %otb_ptr\n"  # @offset_to_buffer = 0
      # Allocate element buffer
      emit_raw "  %buf_bytes = mul i32 %size, 4\n"
      emit_raw "  %buf_sz64 = sext i32 %buf_bytes to i64\n"
      emit_raw "  %buf = call ptr @__crystal_v2_malloc64(i64 %buf_sz64)\n"
      emit_raw "  %buf_ptr = getelementptr i8, ptr %arr, i32 16\n"
      emit_raw "  store ptr %buf, ptr %buf_ptr\n"  # @buffer = buf
      # Fill buffer with initial value
      emit_raw "  %is_empty = icmp sle i32 %size, 0\n"
      emit_raw "  br i1 %is_empty, label %done, label %fill_loop\n"
      emit_raw "fill_loop:\n"
      emit_raw "  %i = phi i32 [0, %entry], [%i_next, %fill_loop]\n"
      emit_raw "  %elem_ptr = getelementptr i32, ptr %buf, i32 %i\n"
      emit_raw "  store i32 %val, ptr %elem_ptr\n"
      emit_raw "  %i_next = add i32 %i, 1\n"
      emit_raw "  %fill_done = icmp sge i32 %i_next, %size\n"
      emit_raw "  br i1 %fill_done, label %done, label %fill_loop\n"
      emit_raw "done:\n"
      emit_raw "  ret ptr %arr\n"
      emit_raw "}\n\n"

      # Array.new(size, initial_bool_value) → creates filled Crystal Array with i1 stored as i8
      emit_raw "define ptr @__crystal_v2_array_new_filled_bool(i32 %size, i1 %val) {\n"
      emit_raw "entry:\n"
      emit_raw "  %arr = call ptr @__crystal_v2_malloc64(i64 24)\n"
      emit_raw "  store i32 #{array_runtime_type_id_for_element(TypeRef::BOOL)}, ptr %arr\n"
      emit_raw "  %sz_ptr = getelementptr i8, ptr %arr, i32 4\n"
      emit_raw "  store i32 %size, ptr %sz_ptr\n"
      emit_raw "  %cap_ptr = getelementptr i8, ptr %arr, i32 8\n"
      emit_raw "  store i32 %size, ptr %cap_ptr\n"
      emit_raw "  %otb_ptr = getelementptr i8, ptr %arr, i32 12\n"
      emit_raw "  store i32 0, ptr %otb_ptr\n"
      emit_raw "  %buf_sz64 = sext i32 %size to i64\n"
      emit_raw "  %buf = call ptr @__crystal_v2_malloc64(i64 %buf_sz64)\n"
      emit_raw "  %buf_ptr = getelementptr i8, ptr %arr, i32 16\n"
      emit_raw "  store ptr %buf, ptr %buf_ptr\n"
      emit_raw "  %val8 = zext i1 %val to i8\n"
      emit_raw "  %is_empty = icmp sle i32 %size, 0\n"
      emit_raw "  br i1 %is_empty, label %done, label %fill_loop\n"
      emit_raw "fill_loop:\n"
      emit_raw "  %i = phi i32 [0, %entry], [%i_next, %fill_loop]\n"
      emit_raw "  %elem_ptr = getelementptr i8, ptr %buf, i32 %i\n"
      emit_raw "  store i8 %val8, ptr %elem_ptr\n"
      emit_raw "  %i_next = add i32 %i, 1\n"
      emit_raw "  %fill_done = icmp sge i32 %i_next, %size\n"
      emit_raw "  br i1 %fill_done, label %done, label %fill_loop\n"
      emit_raw "done:\n"
      emit_raw "  ret ptr %arr\n"
      emit_raw "}\n\n"

      # Array#+(other) → new Crystal Array with elements from both arrays
      # Takes elem_size to correctly copy elements of any type (i32=4, ptr=8, etc.)
      emit_raw "define ptr @__crystal_v2_array_concat(ptr %a, ptr %b, i32 %elem_size) {\n"
      emit_raw "entry:\n"
      # Load sizes
      emit_raw "  %a_sz_ptr = getelementptr i8, ptr %a, i32 4\n"
      emit_raw "  %a_sz = load i32, ptr %a_sz_ptr\n"
      emit_raw "  %b_sz_ptr = getelementptr i8, ptr %b, i32 4\n"
      emit_raw "  %b_sz = load i32, ptr %b_sz_ptr\n"
      emit_raw "  %total = add i32 %a_sz, %b_sz\n"
      # Load buffers
      emit_raw "  %a_buf_ptr = getelementptr i8, ptr %a, i32 16\n"
      emit_raw "  %a_buf = load ptr, ptr %a_buf_ptr\n"
      emit_raw "  %b_buf_ptr = getelementptr i8, ptr %b, i32 16\n"
      emit_raw "  %b_buf = load ptr, ptr %b_buf_ptr\n"
      # Allocate new array header (24 bytes)
      emit_raw "  %arr = call ptr @__crystal_v2_malloc64(i64 24)\n"
      emit_raw "  %a_tid = load i32, ptr %a\n"
      emit_raw "  store i32 %a_tid, ptr %arr\n"
      emit_raw "  %sz_ptr = getelementptr i8, ptr %arr, i32 4\n"
      emit_raw "  store i32 %total, ptr %sz_ptr\n"
      emit_raw "  %cap_ptr = getelementptr i8, ptr %arr, i32 8\n"
      emit_raw "  store i32 %total, ptr %cap_ptr\n"
      emit_raw "  %otb_ptr = getelementptr i8, ptr %arr, i32 12\n"
      emit_raw "  store i32 0, ptr %otb_ptr\n"
      # Allocate buffer (elem_size bytes per element)
      emit_raw "  %buf_bytes = mul i32 %total, %elem_size\n"
      emit_raw "  %buf_sz64 = sext i32 %buf_bytes to i64\n"
      emit_raw "  %buf = call ptr @__crystal_v2_malloc64(i64 %buf_sz64)\n"
      emit_raw "  %buf_addr = getelementptr i8, ptr %arr, i32 16\n"
      emit_raw "  store ptr %buf, ptr %buf_addr\n"
      # Copy a's elements
      emit_raw "  %a_bytes = mul i32 %a_sz, %elem_size\n"
      emit_raw "  %a_bytes64 = sext i32 %a_bytes to i64\n"
      emit_raw "  call void @llvm.memcpy.p0.p0.i64(ptr %buf, ptr %a_buf, i64 %a_bytes64, i1 false)\n"
      # Copy b's elements after a's
      emit_raw "  %b_offset = getelementptr i8, ptr %buf, i32 %a_bytes\n"
      emit_raw "  %b_bytes = mul i32 %b_sz, %elem_size\n"
      emit_raw "  %b_bytes64 = sext i32 %b_bytes to i64\n"
      emit_raw "  call void @llvm.memcpy.p0.p0.i64(ptr %b_offset, ptr %b_buf, i64 %b_bytes64, i1 false)\n"
      emit_raw "  ret ptr %arr\n"
      emit_raw "}\n\n"

      # Hash entry access helpers for Hash#each/keys intrinsics
      # Hash layout: offset 8=@entries(ptr) where entries is a pointer table (Entry*[]).
      # hash_offset is provided by HIR from Hash::Entry(K,V) ClassInfo.
      emit_raw "define ptr @__crystal_v2_hash_get_entry_ptr(ptr %hash, i32 %index, i32 %entry_size) {\n"
      emit_raw "  %entries_addr = getelementptr i8, ptr %hash, i32 8\n"
      emit_raw "  %entries = load ptr, ptr %entries_addr\n"
      emit_raw "  %idx64 = sext i32 %index to i64\n"
      emit_raw "  %entry_addr = getelementptr ptr, ptr %entries, i64 %idx64\n"
      emit_raw "  %entry = load ptr, ptr %entry_addr\n"
      emit_raw "  ret ptr %entry\n"
      emit_raw "}\n\n"

      emit_raw "define i1 @__crystal_v2_hash_entry_deleted(ptr %entry, i32 %hash_offset) {\n"
      emit_raw "  %is_null = icmp eq ptr %entry, null\n"
      emit_raw "  br i1 %is_null, label %null_entry, label %check_flag\n"
      emit_raw "null_entry:\n"
      emit_raw "  ret i1 true\n"
      emit_raw "check_flag:\n"
      emit_raw "  %flag_addr = getelementptr i8, ptr %entry, i32 %hash_offset\n"
      emit_raw "  %flag = load i32, ptr %flag_addr\n"
      emit_raw "  %is_deleted = icmp eq i32 %flag, 0\n"
      emit_raw "  ret i1 %is_deleted\n"
      emit_raw "}\n\n"

      # Hash object allocator — bypasses broken constructor overload chains
      # Hash layout (48 bytes total):
      #   offset 0:  type_id (i32)
      #   offset 4:  @size (i32)
      #   offset 8:  @entries (ptr)
      #   offset 16: @indices (ptr)
      #   offset 24: @first (i32)
      #   offset 28: @deleted_count (i32)
      #   offset 32: @indices_bytesize (i8)
      #   offset 33: @indices_size_pow2 (i8)
      #   offset 34: @compare_by_identity (i1)
      #   offset 35-47: padding / @block (ptr)
      emit_raw "define ptr @__crystal_v2_hash_new(i32 %type_id) {\n"
      emit_raw "entry:\n"
      emit_raw "  %raw = call ptr @__crystal_v2_malloc64(i64 48)\n"
      emit_raw "  call void @llvm.memset.p0.i64(ptr %raw, i8 0, i64 48, i1 false)\n"
      emit_raw "  store i32 %type_id, ptr %raw\n"
      emit_raw "  ret ptr %raw\n"
      emit_raw "}\n\n"

      # Pointer copy helper — memcpy with element-size-aware byte count
      # count = number of elements, elem_size = bytes per element
      emit_raw "define ptr @__crystal_v2_ptr_copy(ptr %dest, ptr %src, i32 %count, i32 %elem_size) {\n"
      emit_raw "entry:\n"
      emit_raw "  %bytes = mul i32 %count, %elem_size\n"
      emit_raw "  call void @llvm.memcpy.p0.p0.i32(ptr %dest, ptr %src, i32 %bytes, i1 false)\n"
      emit_raw "  ret ptr %dest\n"
      emit_raw "}\n\n"

      # Pointer move helper — memmove with element-size-aware byte count
      emit_raw "define ptr @__crystal_v2_ptr_move(ptr %dest, ptr %src, i32 %count, i32 %elem_size) {\n"
      emit_raw "entry:\n"
      emit_raw "  %bytes = mul i32 %count, %elem_size\n"
      emit_raw "  call void @llvm.memmove.p0.p0.i32(ptr %dest, ptr %src, i32 %bytes, i1 false)\n"
      emit_raw "  ret ptr %dest\n"
      emit_raw "}\n\n"

      # Int32 comparator for qsort
      emit_raw "define i32 @__cmp_i32(ptr %a, ptr %b) {\n"
      emit_raw "entry:\n"
      emit_raw "  %va = load i32, ptr %a\n"
      emit_raw "  %vb = load i32, ptr %b\n"
      emit_raw "  %lt = icmp slt i32 %va, %vb\n"
      emit_raw "  %gt = icmp sgt i32 %va, %vb\n"
      emit_raw "  %r1 = select i1 %lt, i32 -1, i32 0\n"
      emit_raw "  %r2 = select i1 %gt, i32 1, i32 %r1\n"
      emit_raw "  ret i32 %r2\n"
      emit_raw "}\n\n"

      # String comparator for qsort (compares Crystal String objects by content)
      emit_raw "define i32 @__cmp_string(ptr %a, ptr %b) {\n"
      emit_raw "entry:\n"
      emit_raw "  %pa = load ptr, ptr %a\n"
      emit_raw "  %pb = load ptr, ptr %b\n"
      emit_raw "  %da = getelementptr i8, ptr %pa, i32 12\n"
      emit_raw "  %db = getelementptr i8, ptr %pb, i32 12\n"
      emit_raw "  %r = call i32 @strcmp(ptr %da, ptr %db)\n"
      emit_raw "  ret i32 %r\n"
      emit_raw "}\n\n"

      # Sort Crystal Array(Int32) in-place using qsort
      emit_raw "define void @__crystal_v2_sort_i32_array(ptr %arr) {\n"
      emit_raw "entry:\n"
      emit_raw "  %size_ptr = getelementptr i8, ptr %arr, i32 4\n"
      emit_raw "  %size = load i32, ptr %size_ptr\n"
      emit_raw "  %buf_ptr = getelementptr i8, ptr %arr, i32 16\n"
      emit_raw "  %buf = load ptr, ptr %buf_ptr\n"
      emit_raw "  %size64 = sext i32 %size to i64\n"
      emit_raw "  call void @qsort(ptr %buf, i64 %size64, i64 4, ptr @__cmp_i32)\n"
      emit_raw "  ret void\n"
      emit_raw "}\n\n"

      # Sort Crystal Array(String) in-place using qsort
      emit_raw "define void @__crystal_v2_sort_string_array(ptr %arr) {\n"
      emit_raw "entry:\n"
      emit_raw "  %size_ptr = getelementptr i8, ptr %arr, i32 4\n"
      emit_raw "  %size = load i32, ptr %size_ptr\n"
      emit_raw "  %buf_ptr = getelementptr i8, ptr %arr, i32 16\n"
      emit_raw "  %buf = load ptr, ptr %buf_ptr\n"
      emit_raw "  %size64 = sext i32 %size to i64\n"
      emit_raw "  call void @qsort(ptr %buf, i64 %size64, i64 8, ptr @__cmp_string)\n"
      emit_raw "  ret void\n"
      emit_raw "}\n\n"

      # Dup + sort Crystal Array(Int32): allocate new array, copy, sort, return
      emit_raw "define ptr @__crystal_v2_sort_i32_array_dup(ptr %arr, i32 %type_id) {\n"
      emit_raw "entry:\n"
      emit_raw "  %size_ptr = getelementptr i8, ptr %arr, i32 4\n"
      emit_raw "  %size = load i32, ptr %size_ptr\n"
      emit_raw "  %buf_ptr = getelementptr i8, ptr %arr, i32 16\n"
      emit_raw "  %old_buf = load ptr, ptr %buf_ptr\n"
      # Allocate new Crystal Array (24 bytes)
      emit_raw "  %new_arr = call ptr @__crystal_v2_malloc64(i64 24)\n"
      emit_raw "  store i32 %type_id, ptr %new_arr\n"
      emit_raw "  %new_size_ptr = getelementptr i8, ptr %new_arr, i32 4\n"
      emit_raw "  store i32 %size, ptr %new_size_ptr\n"
      emit_raw "  %new_cap_ptr = getelementptr i8, ptr %new_arr, i32 8\n"
      emit_raw "  store i32 %size, ptr %new_cap_ptr\n"
      emit_raw "  %new_off_ptr = getelementptr i8, ptr %new_arr, i32 12\n"
      emit_raw "  store i32 0, ptr %new_off_ptr\n"
      # Allocate buffer: size * 4 bytes
      emit_raw "  %buf_bytes = mul i32 %size, 4\n"
      emit_raw "  %buf_bytes64 = sext i32 %buf_bytes to i64\n"
      emit_raw "  %new_buf = call ptr @__crystal_v2_malloc64(i64 %buf_bytes64)\n"
      emit_raw "  %new_buf_ptr = getelementptr i8, ptr %new_arr, i32 16\n"
      emit_raw "  store ptr %new_buf, ptr %new_buf_ptr\n"
      # Copy buffer
      emit_raw "  call void @llvm.memcpy.p0.p0.i32(ptr %new_buf, ptr %old_buf, i32 %buf_bytes, i1 false)\n"
      # Sort
      emit_raw "  %size64 = sext i32 %size to i64\n"
      emit_raw "  call void @qsort(ptr %new_buf, i64 %size64, i64 4, ptr @__cmp_i32)\n"
      emit_raw "  ret ptr %new_arr\n"
      emit_raw "}\n\n"

      # Dup + sort Crystal Array(String): allocate new array, copy ptr buffer, sort, return
      emit_raw "define ptr @__crystal_v2_sort_string_array_dup(ptr %arr, i32 %type_id) {\n"
      emit_raw "entry:\n"
      emit_raw "  %size_ptr = getelementptr i8, ptr %arr, i32 4\n"
      emit_raw "  %size = load i32, ptr %size_ptr\n"
      emit_raw "  %buf_ptr = getelementptr i8, ptr %arr, i32 16\n"
      emit_raw "  %old_buf = load ptr, ptr %buf_ptr\n"
      emit_raw "  %new_arr = call ptr @__crystal_v2_malloc64(i64 24)\n"
      emit_raw "  store i32 %type_id, ptr %new_arr\n"
      emit_raw "  %new_size_ptr = getelementptr i8, ptr %new_arr, i32 4\n"
      emit_raw "  store i32 %size, ptr %new_size_ptr\n"
      emit_raw "  %new_cap_ptr = getelementptr i8, ptr %new_arr, i32 8\n"
      emit_raw "  store i32 %size, ptr %new_cap_ptr\n"
      emit_raw "  %new_off_ptr = getelementptr i8, ptr %new_arr, i32 12\n"
      emit_raw "  store i32 0, ptr %new_off_ptr\n"
      emit_raw "  %buf_bytes = mul i32 %size, 8\n"
      emit_raw "  %buf_bytes64 = sext i32 %buf_bytes to i64\n"
      emit_raw "  %new_buf = call ptr @__crystal_v2_malloc64(i64 %buf_bytes64)\n"
      emit_raw "  %new_buf_ptr = getelementptr i8, ptr %new_arr, i32 16\n"
      emit_raw "  store ptr %new_buf, ptr %new_buf_ptr\n"
      emit_raw "  call void @llvm.memcpy.p0.p0.i32(ptr %new_buf, ptr %old_buf, i32 %buf_bytes, i1 false)\n"
      emit_raw "  %size64 = sext i32 %size to i64\n"
      emit_raw "  call void @qsort(ptr %new_buf, i64 %size64, i64 8, ptr @__cmp_string)\n"
      emit_raw "  ret ptr %new_arr\n"
      emit_raw "}\n\n"

      # Array(Int32)#to_s → "[elem, elem, ...]" Crystal String
      # Uses snprintf to build the string in a buffer
      emit_raw "define ptr @__crystal_v2_array_i32_to_string(ptr %arr) {\n"
      emit_raw "entry:\n"
      emit_raw "  %size_ptr = getelementptr i8, ptr %arr, i32 4\n"
      emit_raw "  %size = load i32, ptr %size_ptr\n"
      emit_raw "  %buf_ptr = getelementptr i8, ptr %arr, i32 16\n"
      emit_raw "  %buf = load ptr, ptr %buf_ptr\n"
      # Allocate max buffer: "[" + size * 12 + (size-1) * 2 + "]" + null = ~14*size + 2
      emit_raw "  %max_len_i32 = mul i32 %size, 14\n"
      emit_raw "  %max_len_with_extra = add i32 %max_len_i32, 16\n"
      emit_raw "  %max_len = sext i32 %max_len_with_extra to i64\n"
      emit_raw "  %tmp_buf = call ptr @__crystal_v2_malloc64(i64 %max_len)\n"
      # Start with "["
      emit_raw "  store i8 91, ptr %tmp_buf\n"  # '[' = 91
      emit_raw "  %pos_init = add i32 0, 1\n"
      emit_raw "  br label %loop_cond\n"
      emit_raw "loop_cond:\n"
      emit_raw "  %i = phi i32 [0, %entry], [%i_next, %no_sep]\n"
      emit_raw "  %pos = phi i32 [%pos_init, %entry], [%pos_next, %no_sep]\n"
      emit_raw "  %cmp = icmp slt i32 %i, %size\n"
      emit_raw "  br i1 %cmp, label %loop_body, label %done\n"
      emit_raw "loop_body:\n"
      # Add ", " separator if not first
      emit_raw "  %is_first = icmp eq i32 %i, 0\n"
      emit_raw "  br i1 %is_first, label %no_sep, label %add_sep\n"
      emit_raw "add_sep:\n"
      emit_raw "  %sep_ptr = getelementptr i8, ptr %tmp_buf, i32 %pos\n"
      emit_raw "  store i8 44, ptr %sep_ptr\n"  # ',' = 44
      emit_raw "  %pos_after_comma = add i32 %pos, 1\n"
      emit_raw "  %space_ptr = getelementptr i8, ptr %tmp_buf, i32 %pos_after_comma\n"
      emit_raw "  store i8 32, ptr %space_ptr\n"  # ' ' = 32
      emit_raw "  %pos_after_sep = add i32 %pos, 2\n"
      emit_raw "  br label %no_sep\n"
      emit_raw "no_sep:\n"
      emit_raw "  %pos2 = phi i32 [%pos, %loop_body], [%pos_after_sep, %add_sep]\n"
      # Get element value
      emit_raw "  %elem_ptr = getelementptr i32, ptr %buf, i32 %i\n"
      emit_raw "  %elem = load i32, ptr %elem_ptr\n"
      # snprintf(tmp_buf + pos, remaining, "%d", elem)
      emit_raw "  %write_ptr = getelementptr i8, ptr %tmp_buf, i32 %pos2\n"
      emit_raw "  %remaining = sub i32 %max_len_with_extra, %pos2\n"
      emit_raw "  %remaining64 = sext i32 %remaining to i64\n"
      emit_raw "  %written = call i32 (ptr, i64, ptr, ...) @snprintf(ptr %write_ptr, i64 %remaining64, ptr @.str.fmt.d, i32 %elem)\n"
      emit_raw "  %pos_next = add i32 %pos2, %written\n"
      emit_raw "  %i_next = add i32 %i, 1\n"
      emit_raw "  br label %loop_cond\n"
      emit_raw "done:\n"
      # Add "]"
      emit_raw "  %end_ptr = getelementptr i8, ptr %tmp_buf, i32 %pos\n"
      emit_raw "  store i8 93, ptr %end_ptr\n"  # ']' = 93
      emit_raw "  %total_len = add i32 %pos, 1\n"
      emit_raw "  %null_ptr = getelementptr i8, ptr %tmp_buf, i32 %total_len\n"
      emit_raw "  store i8 0, ptr %null_ptr\n"
      # Create Crystal String
      emit_raw "  %str_type_id = add i32 0, #{TypeRef::STRING.id}\n"
      emit_raw "  %result = call ptr @__crystal_v2_create_substring(ptr %tmp_buf, i32 %total_len, i32 %str_type_id)\n"
      emit_raw "  ret ptr %result\n"
      emit_raw "}\n\n"

      # Array(String)#to_s → "[\"elem\", \"elem\", ...]" Crystal String
      emit_raw "define ptr @__crystal_v2_array_string_to_string(ptr %arr) {\n"
      emit_raw "entry:\n"
      emit_raw "  %size_ptr = getelementptr i8, ptr %arr, i32 4\n"
      emit_raw "  %size = load i32, ptr %size_ptr\n"
      emit_raw "  %buf_ptr = getelementptr i8, ptr %arr, i32 16\n"
      emit_raw "  %buf = load ptr, ptr %buf_ptr\n"
      # Allocate buffer: estimate 64 * size for safety
      emit_raw "  %max_len_i32 = mul i32 %size, 64\n"
      emit_raw "  %max_len_with_extra = add i32 %max_len_i32, 16\n"
      emit_raw "  %max_len = sext i32 %max_len_with_extra to i64\n"
      emit_raw "  %tmp_buf = call ptr @__crystal_v2_malloc64(i64 %max_len)\n"
      # Start with "["
      emit_raw "  store i8 91, ptr %tmp_buf\n"
      emit_raw "  %pos_init = add i32 0, 1\n"
      emit_raw "  br label %loop_cond\n"
      emit_raw "loop_cond:\n"
      emit_raw "  %i = phi i32 [0, %entry], [%i_next, %loop_body_end]\n"
      emit_raw "  %pos = phi i32 [%pos_init, %entry], [%pos_next, %loop_body_end]\n"
      emit_raw "  %cmp = icmp slt i32 %i, %size\n"
      emit_raw "  br i1 %cmp, label %loop_body, label %done\n"
      emit_raw "loop_body:\n"
      # Add ", " separator if not first
      emit_raw "  %is_first = icmp eq i32 %i, 0\n"
      emit_raw "  br i1 %is_first, label %no_sep, label %add_sep\n"
      emit_raw "add_sep:\n"
      emit_raw "  %sep_ptr = getelementptr i8, ptr %tmp_buf, i32 %pos\n"
      emit_raw "  store i8 44, ptr %sep_ptr\n"
      emit_raw "  %pos_after_comma = add i32 %pos, 1\n"
      emit_raw "  %space_ptr = getelementptr i8, ptr %tmp_buf, i32 %pos_after_comma\n"
      emit_raw "  store i8 32, ptr %space_ptr\n"
      emit_raw "  %pos_after_sep = add i32 %pos, 2\n"
      emit_raw "  br label %no_sep\n"
      emit_raw "no_sep:\n"
      emit_raw "  %pos2 = phi i32 [%pos, %loop_body], [%pos_after_sep, %add_sep]\n"
      # Get string element
      emit_raw "  %str_pp = getelementptr ptr, ptr %buf, i32 %i\n"
      emit_raw "  %str = load ptr, ptr %str_pp\n"
      # Get string data (at offset 12) and bytesize (at offset 4)
      emit_raw "  %bs_ptr = getelementptr i8, ptr %str, i32 4\n"
      emit_raw "  %bs = load i32, ptr %bs_ptr\n"
      emit_raw "  %data_ptr = getelementptr i8, ptr %str, i32 12\n"
      # Copy string bytes
      emit_raw "  %write_ptr = getelementptr i8, ptr %tmp_buf, i32 %pos2\n"
      emit_raw "  %bs64 = sext i32 %bs to i64\n"
      emit_raw "  call void @llvm.memcpy.p0.p0.i64(ptr %write_ptr, ptr %data_ptr, i64 %bs64, i1 false)\n"
      emit_raw "  %pos_next = add i32 %pos2, %bs\n"
      emit_raw "  %i_next = add i32 %i, 1\n"
      emit_raw "  br label %loop_body_end\n"
      emit_raw "loop_body_end:\n"
      emit_raw "  br label %loop_cond\n"
      emit_raw "done:\n"
      emit_raw "  %end_ptr = getelementptr i8, ptr %tmp_buf, i32 %pos\n"
      emit_raw "  store i8 93, ptr %end_ptr\n"
      emit_raw "  %total_len = add i32 %pos, 1\n"
      emit_raw "  %null_ptr = getelementptr i8, ptr %tmp_buf, i32 %total_len\n"
      emit_raw "  store i8 0, ptr %null_ptr\n"
      emit_raw "  %str_type_id = add i32 0, #{TypeRef::STRING.id}\n"
      emit_raw "  %result = call ptr @__crystal_v2_create_substring(ptr %tmp_buf, i32 %total_len, i32 %str_type_id)\n"
      emit_raw "  ret ptr %result\n"
      emit_raw "}\n\n"

      # Helper: create Crystal String from raw pointer + length
      emit_raw "define ptr @__crystal_v2_create_substring(ptr %data, i32 %len, i32 %str_tid) {\n"
      emit_raw "entry:\n"
      emit_raw "  %alloc_i32 = add i32 %len, 13\n"
      emit_raw "  %alloc = sext i32 %alloc_i32 to i64\n"
      emit_raw "  %str = call ptr @__crystal_v2_malloc64(i64 %alloc)\n"
      emit_raw "  store i32 %str_tid, ptr %str\n"
      emit_raw "  %bs_ptr = getelementptr i8, ptr %str, i32 4\n"
      emit_raw "  store i32 %len, ptr %bs_ptr\n"
      emit_raw "  %sz_ptr = getelementptr i8, ptr %str, i32 8\n"
      emit_raw "  store i32 %len, ptr %sz_ptr\n"
      emit_raw "  %data_ptr = getelementptr i8, ptr %str, i32 12\n"
      emit_raw "  %len64 = sext i32 %len to i64\n"
      emit_raw "  call void @llvm.memcpy.p0.p0.i64(ptr %data_ptr, ptr %data, i64 %len64, i1 false)\n"
      emit_raw "  %null_pos = getelementptr i8, ptr %data_ptr, i32 %len\n"
      emit_raw "  store i8 0, ptr %null_pos\n"
      emit_raw "  ret ptr %str\n"
      emit_raw "}\n\n"

      # String#split(String) — splits self by separator, returns Crystal Array(String)
      emit_raw "define ptr @__crystal_v2_string_split_string(ptr %self, ptr %sep) {\n"
      emit_raw "entry:\n"
      emit_raw "  %self_bs_ptr = getelementptr i8, ptr %self, i32 4\n"
      emit_raw "  %self_bs = load i32, ptr %self_bs_ptr\n"
      emit_raw "  %self_data = getelementptr i8, ptr %self, i32 12\n"
      emit_raw "  %sep_bs_ptr = getelementptr i8, ptr %sep, i32 4\n"
      emit_raw "  %sep_bs = load i32, ptr %sep_bs_ptr\n"
      emit_raw "  %sep_data = getelementptr i8, ptr %sep, i32 12\n"
      # Count occurrences
      emit_raw "  br label %count_loop\n"
      emit_raw "count_loop:\n"
      emit_raw "  %count = phi i32 [0, %entry], [%count_next, %count_found]\n"
      emit_raw "  %scan = phi ptr [%self_data, %entry], [%scan_next, %count_found]\n"
      emit_raw "  %found = call ptr @strstr(ptr %scan, ptr %sep_data)\n"
      emit_raw "  %is_null = icmp eq ptr %found, null\n"
      emit_raw "  br i1 %is_null, label %count_done, label %count_found\n"
      emit_raw "count_found:\n"
      emit_raw "  %count_next = add i32 %count, 1\n"
      emit_raw "  %scan_next = getelementptr i8, ptr %found, i32 %sep_bs\n"
      emit_raw "  br label %count_loop\n"
      # Allocate Array(String)
      emit_raw "count_done:\n"
      emit_raw "  %num_segs = add i32 %count, 1\n"
      emit_raw "  %arr = call ptr @__crystal_v2_malloc64(i64 24)\n"
      emit_raw "  store i32 #{array_runtime_type_id_for_element(TypeRef::STRING)}, ptr %arr\n"
      emit_raw "  %arr_sz = getelementptr i8, ptr %arr, i32 4\n"
      emit_raw "  store i32 %num_segs, ptr %arr_sz\n"
      emit_raw "  %arr_cap = getelementptr i8, ptr %arr, i32 8\n"
      emit_raw "  store i32 %num_segs, ptr %arr_cap\n"
      emit_raw "  %arr_off = getelementptr i8, ptr %arr, i32 12\n"
      emit_raw "  store i32 0, ptr %arr_off\n"
      emit_raw "  %buf_sz32 = mul i32 %num_segs, 8\n"
      emit_raw "  %buf_sz = sext i32 %buf_sz32 to i64\n"
      emit_raw "  %buf = call ptr @__crystal_v2_malloc64(i64 %buf_sz)\n"
      emit_raw "  %arr_buf = getelementptr i8, ptr %arr, i32 16\n"
      emit_raw "  store ptr %buf, ptr %arr_buf\n"
      # Split loop
      emit_raw "  br label %split_loop\n"
      emit_raw "split_loop:\n"
      emit_raw "  %idx = phi i32 [0, %count_done], [%idx_next, %split_mid]\n"
      emit_raw "  %start = phi ptr [%self_data, %count_done], [%start_next, %split_mid]\n"
      emit_raw "  %sf = call ptr @strstr(ptr %start, ptr %sep_data)\n"
      emit_raw "  %sf_null = icmp eq ptr %sf, null\n"
      emit_raw "  br i1 %sf_null, label %split_last, label %split_mid\n"
      emit_raw "split_mid:\n"
      emit_raw "  %start_i = ptrtoint ptr %start to i64\n"
      emit_raw "  %sf_i = ptrtoint ptr %sf to i64\n"
      emit_raw "  %seg_len64 = sub i64 %sf_i, %start_i\n"
      emit_raw "  %seg_len = trunc i64 %seg_len64 to i32\n"
      emit_raw "  %sub = call ptr @__crystal_v2_create_substring(ptr %start, i32 %seg_len, i32 #{@string_type_id})\n"
      emit_raw "  %slot = getelementptr ptr, ptr %buf, i32 %idx\n"
      emit_raw "  store ptr %sub, ptr %slot\n"
      emit_raw "  %idx_next = add i32 %idx, 1\n"
      emit_raw "  %start_next = getelementptr i8, ptr %sf, i32 %sep_bs\n"
      emit_raw "  br label %split_loop\n"
      # Last segment
      emit_raw "split_last:\n"
      emit_raw "  %self_end = getelementptr i8, ptr %self_data, i32 %self_bs\n"
      emit_raw "  %start_i2 = ptrtoint ptr %start to i64\n"
      emit_raw "  %end_i = ptrtoint ptr %self_end to i64\n"
      emit_raw "  %last_len64 = sub i64 %end_i, %start_i2\n"
      emit_raw "  %last_len = trunc i64 %last_len64 to i32\n"
      emit_raw "  %last_sub = call ptr @__crystal_v2_create_substring(ptr %start, i32 %last_len, i32 #{@string_type_id})\n"
      emit_raw "  %last_slot = getelementptr ptr, ptr %buf, i32 %idx\n"
      emit_raw "  store ptr %last_sub, ptr %last_slot\n"
      emit_raw "  ret ptr %arr\n"
      emit_raw "}\n\n"

      # array_join_string: join Array(String) with separator string
      # Array layout: {i32 type_id, i32 size, i32 cap, i32 offset, ptr buffer}
      # Buffer holds ptr elements (each is a Crystal String)
      emit_raw "define ptr @__crystal_v2_array_join_string(ptr %arr, ptr %sep) {\n"
      emit_raw "entry:\n"
      emit_raw "  %sz_ptr = getelementptr i8, ptr %arr, i32 4\n"
      emit_raw "  %sz = load i32, ptr %sz_ptr\n"
      emit_raw "  %buf_ptr = getelementptr i8, ptr %arr, i32 16\n"
      emit_raw "  %buf = load ptr, ptr %buf_ptr\n"
      emit_raw "  %sep_bs_ptr = getelementptr i8, ptr %sep, i32 4\n"
      emit_raw "  %sep_bs = load i32, ptr %sep_bs_ptr\n"
      emit_raw "  %sep_data = getelementptr i8, ptr %sep, i32 12\n"
      # Empty array → empty string
      emit_raw "  %is_empty = icmp eq i32 %sz, 0\n"
      emit_raw "  br i1 %is_empty, label %ret_empty, label %calc_len\n"
      emit_raw "ret_empty:\n"
      emit_raw "  %empty = call ptr @__crystal_v2_malloc64(i64 13)\n"
      emit_raw "  store i32 #{@string_type_id}, ptr %empty\n"
      emit_raw "  %empty_bs = getelementptr i8, ptr %empty, i32 4\n"
      emit_raw "  store i32 0, ptr %empty_bs\n"
      emit_raw "  %empty_sz = getelementptr i8, ptr %empty, i32 8\n"
      emit_raw "  store i32 0, ptr %empty_sz\n"
      emit_raw "  %empty_d = getelementptr i8, ptr %empty, i32 12\n"
      emit_raw "  store i8 0, ptr %empty_d\n"
      emit_raw "  ret ptr %empty\n"
      # Calculate total length
      emit_raw "calc_len:\n"
      emit_raw "  %sep_count = sub i32 %sz, 1\n"
      emit_raw "  %sep_total = mul i32 %sep_count, %sep_bs\n"
      emit_raw "  br label %len_loop\n"
      emit_raw "len_loop:\n"
      emit_raw "  %li = phi i32 [0, %calc_len], [%li_next, %len_loop]\n"
      emit_raw "  %total = phi i32 [%sep_total, %calc_len], [%total_next, %len_loop]\n"
      emit_raw "  %elem_slot = getelementptr ptr, ptr %buf, i32 %li\n"
      emit_raw "  %elem = load ptr, ptr %elem_slot\n"
      emit_raw "  %elem_bs_ptr = getelementptr i8, ptr %elem, i32 4\n"
      emit_raw "  %elem_bs = load i32, ptr %elem_bs_ptr\n"
      emit_raw "  %total_next = add i32 %total, %elem_bs\n"
      emit_raw "  %li_next = add i32 %li, 1\n"
      emit_raw "  %li_done = icmp eq i32 %li_next, %sz\n"
      emit_raw "  br i1 %li_done, label %alloc_str, label %len_loop\n"
      # Allocate result string
      emit_raw "alloc_str:\n"
      emit_raw "  %alloc_i32 = add i32 %total_next, 13\n"
      emit_raw "  %alloc_i64 = sext i32 %alloc_i32 to i64\n"
      emit_raw "  %result = call ptr @__crystal_v2_malloc64(i64 %alloc_i64)\n"
      emit_raw "  store i32 #{@string_type_id}, ptr %result\n"
      emit_raw "  %r_bs = getelementptr i8, ptr %result, i32 4\n"
      emit_raw "  store i32 %total_next, ptr %r_bs\n"
      emit_raw "  %r_sz = getelementptr i8, ptr %result, i32 8\n"
      emit_raw "  store i32 %total_next, ptr %r_sz\n"
      emit_raw "  %r_data = getelementptr i8, ptr %result, i32 12\n"
      emit_raw "  br label %copy_loop\n"
      # Copy elements with separator
      emit_raw "copy_loop:\n"
      emit_raw "  %ci = phi i32 [0, %alloc_str], [%ci_next, %copy_sep_done]\n"
      emit_raw "  %dest = phi ptr [%r_data, %alloc_str], [%dest3, %copy_sep_done]\n"
      emit_raw "  %ce_slot = getelementptr ptr, ptr %buf, i32 %ci\n"
      emit_raw "  %ce = load ptr, ptr %ce_slot\n"
      emit_raw "  %ce_bs_ptr = getelementptr i8, ptr %ce, i32 4\n"
      emit_raw "  %ce_bs = load i32, ptr %ce_bs_ptr\n"
      emit_raw "  %ce_data = getelementptr i8, ptr %ce, i32 12\n"
      emit_raw "  %ce_bs64 = sext i32 %ce_bs to i64\n"
      emit_raw "  call void @llvm.memcpy.p0.p0.i64(ptr %dest, ptr %ce_data, i64 %ce_bs64, i1 false)\n"
      emit_raw "  %dest2 = getelementptr i8, ptr %dest, i32 %ce_bs\n"
      emit_raw "  %ci_next = add i32 %ci, 1\n"
      emit_raw "  %ci_done = icmp eq i32 %ci_next, %sz\n"
      emit_raw "  br i1 %ci_done, label %finalize, label %copy_sep\n"
      # Copy separator
      emit_raw "copy_sep:\n"
      emit_raw "  %sep_bs64 = sext i32 %sep_bs to i64\n"
      emit_raw "  call void @llvm.memcpy.p0.p0.i64(ptr %dest2, ptr %sep_data, i64 %sep_bs64, i1 false)\n"
      emit_raw "  %dest3 = getelementptr i8, ptr %dest2, i32 %sep_bs\n"
      emit_raw "  br label %copy_sep_done\n"
      emit_raw "copy_sep_done:\n"
      emit_raw "  br label %copy_loop\n"
      # Null-terminate and return
      emit_raw "finalize:\n"
      emit_raw "  store i8 0, ptr %dest2\n"
      emit_raw "  ret ptr %result\n"
      emit_raw "}\n\n"

      # array_join_int32: join Array(Int32) with separator string
      # Converts each i32 element to its string representation, separated by sep
      emit_raw "define ptr @__crystal_v2_array_join_int32(ptr %arr, ptr %sep) {\n"
      emit_raw "entry:\n"
      emit_raw "  %sz_ptr = getelementptr i8, ptr %arr, i32 4\n"
      emit_raw "  %sz = load i32, ptr %sz_ptr\n"
      emit_raw "  %buf_ptr = getelementptr i8, ptr %arr, i32 16\n"
      emit_raw "  %buf = load ptr, ptr %buf_ptr\n"
      emit_raw "  %sep_bs_ptr = getelementptr i8, ptr %sep, i32 4\n"
      emit_raw "  %sep_bs = load i32, ptr %sep_bs_ptr\n"
      emit_raw "  %sep_data = getelementptr i8, ptr %sep, i32 12\n"
      # Empty array → empty string
      emit_raw "  %is_empty = icmp eq i32 %sz, 0\n"
      emit_raw "  br i1 %is_empty, label %ret_empty, label %pass1\n"
      emit_raw "ret_empty:\n"
      emit_raw "  %empty = call ptr @__crystal_v2_malloc64(i64 13)\n"
      emit_raw "  store i32 #{@string_type_id}, ptr %empty\n"
      emit_raw "  %empty_bs = getelementptr i8, ptr %empty, i32 4\n"
      emit_raw "  store i32 0, ptr %empty_bs\n"
      emit_raw "  %empty_sz = getelementptr i8, ptr %empty, i32 8\n"
      emit_raw "  store i32 0, ptr %empty_sz\n"
      emit_raw "  %empty_d = getelementptr i8, ptr %empty, i32 12\n"
      emit_raw "  store i8 0, ptr %empty_d\n"
      emit_raw "  ret ptr %empty\n"
      # Pass 1: convert all ints to strings, store in temp array
      emit_raw "pass1:\n"
      emit_raw "  %sz64 = sext i32 %sz to i64\n"
      emit_raw "  %ptrsz = mul i64 %sz64, 8\n"
      emit_raw "  %strs = call ptr @__crystal_v2_malloc64(i64 %ptrsz)\n"
      emit_raw "  %sep_count = sub i32 %sz, 1\n"
      emit_raw "  %sep_total = mul i32 %sep_count, %sep_bs\n"
      emit_raw "  br label %conv_loop\n"
      emit_raw "conv_loop:\n"
      emit_raw "  %ci = phi i32 [0, %pass1], [%ci_next, %conv_loop]\n"
      emit_raw "  %total = phi i32 [%sep_total, %pass1], [%total_next, %conv_loop]\n"
      emit_raw "  %elem_slot = getelementptr i32, ptr %buf, i32 %ci\n"
      emit_raw "  %elem = load i32, ptr %elem_slot\n"
      emit_raw "  %str = call ptr @__crystal_v2_int_to_string(i32 %elem)\n"
      emit_raw "  %str_slot = getelementptr ptr, ptr %strs, i32 %ci\n"
      emit_raw "  store ptr %str, ptr %str_slot\n"
      emit_raw "  %str_bs_ptr = getelementptr i8, ptr %str, i32 4\n"
      emit_raw "  %str_bs = load i32, ptr %str_bs_ptr\n"
      emit_raw "  %total_next = add i32 %total, %str_bs\n"
      emit_raw "  %ci_next = add i32 %ci, 1\n"
      emit_raw "  %ci_done = icmp eq i32 %ci_next, %sz\n"
      emit_raw "  br i1 %ci_done, label %alloc_str, label %conv_loop\n"
      # Allocate result string
      emit_raw "alloc_str:\n"
      emit_raw "  %alloc_i32 = add i32 %total_next, 13\n"
      emit_raw "  %alloc_i64 = sext i32 %alloc_i32 to i64\n"
      emit_raw "  %result = call ptr @__crystal_v2_malloc64(i64 %alloc_i64)\n"
      emit_raw "  store i32 #{@string_type_id}, ptr %result\n"
      emit_raw "  %r_bs = getelementptr i8, ptr %result, i32 4\n"
      emit_raw "  store i32 %total_next, ptr %r_bs\n"
      emit_raw "  %r_sz = getelementptr i8, ptr %result, i32 8\n"
      emit_raw "  store i32 %total_next, ptr %r_sz\n"
      emit_raw "  %r_data = getelementptr i8, ptr %result, i32 12\n"
      emit_raw "  br label %copy_loop\n"
      # Copy elements with separator
      emit_raw "copy_loop:\n"
      emit_raw "  %xi = phi i32 [0, %alloc_str], [%xi_next, %copy_sep_done]\n"
      emit_raw "  %dest = phi ptr [%r_data, %alloc_str], [%dest3, %copy_sep_done]\n"
      emit_raw "  %xe_slot = getelementptr ptr, ptr %strs, i32 %xi\n"
      emit_raw "  %xe = load ptr, ptr %xe_slot\n"
      emit_raw "  %xe_bs_ptr = getelementptr i8, ptr %xe, i32 4\n"
      emit_raw "  %xe_bs = load i32, ptr %xe_bs_ptr\n"
      emit_raw "  %xe_data = getelementptr i8, ptr %xe, i32 12\n"
      emit_raw "  %xe_bs64 = sext i32 %xe_bs to i64\n"
      emit_raw "  call void @llvm.memcpy.p0.p0.i64(ptr %dest, ptr %xe_data, i64 %xe_bs64, i1 false)\n"
      emit_raw "  %dest2 = getelementptr i8, ptr %dest, i32 %xe_bs\n"
      emit_raw "  %xi_next = add i32 %xi, 1\n"
      emit_raw "  %xi_done = icmp eq i32 %xi_next, %sz\n"
      emit_raw "  br i1 %xi_done, label %finalize, label %copy_sep\n"
      emit_raw "copy_sep:\n"
      emit_raw "  %sep_bs64 = sext i32 %sep_bs to i64\n"
      emit_raw "  call void @llvm.memcpy.p0.p0.i64(ptr %dest2, ptr %sep_data, i64 %sep_bs64, i1 false)\n"
      emit_raw "  %dest3 = getelementptr i8, ptr %dest2, i32 %sep_bs\n"
      emit_raw "  br label %copy_sep_done\n"
      emit_raw "copy_sep_done:\n"
      emit_raw "  br label %copy_loop\n"
      emit_raw "finalize:\n"
      emit_raw "  store i8 0, ptr %dest2\n"
      emit_raw "  ret ptr %result\n"
      emit_raw "}\n\n"

      # int_to_string: allocate buffer and sprintf
      # int_to_string: sprintf to temp buffer, then wrap in Crystal String struct
      emit_raw "define ptr @__crystal_v2_int_to_string(i32 %val) {\n"
      emit_raw "  %tmp = alloca [16 x i8]\n"
      emit_raw "  call i32 (ptr, ptr, ...) @sprintf(ptr %tmp, ptr @.int_fmt_no_nl, i32 %val)\n"
      emit_raw "  %len64 = call i64 @strlen(ptr %tmp)\n"
      emit_raw "  %len = trunc i64 %len64 to i32\n"
      emit_raw "  %alloc_32 = add i32 %len, 13\n"
      emit_raw "  %alloc_64 = sext i32 %alloc_32 to i64\n"
      emit_raw "  %str = call ptr @__crystal_v2_malloc64(i64 %alloc_64)\n"
      emit_raw "  store i32 #{@string_type_id}, ptr %str\n"
      emit_raw "  %bs = getelementptr i8, ptr %str, i32 4\n"
      emit_raw "  store i32 %len, ptr %bs\n"
      emit_raw "  %sz = getelementptr i8, ptr %str, i32 8\n"
      emit_raw "  store i32 %len, ptr %sz\n"
      emit_raw "  %data = getelementptr i8, ptr %str, i32 12\n"
      emit_raw "  %len64c = sext i32 %len to i64\n"
      emit_raw "  call void @llvm.memcpy.p0.p0.i64(ptr %data, ptr %tmp, i64 %len64c, i1 false)\n"
      emit_raw "  %null_pos = getelementptr i8, ptr %data, i32 %len\n"
      emit_raw "  store i8 0, ptr %null_pos\n"
      emit_raw "  ret ptr %str\n"
      emit_raw "}\n\n"

      emit_raw "define ptr @__crystal_v2_int64_to_string(i64 %val) {\n"
      emit_raw "  %tmp = alloca [24 x i8]\n"
      emit_raw "  call i32 (ptr, ptr, ...) @sprintf(ptr %tmp, ptr @.long_fmt_no_nl, i64 %val)\n"
      emit_raw "  %len64 = call i64 @strlen(ptr %tmp)\n"
      emit_raw "  %len = trunc i64 %len64 to i32\n"
      emit_raw "  %alloc_32 = add i32 %len, 13\n"
      emit_raw "  %alloc_64 = sext i32 %alloc_32 to i64\n"
      emit_raw "  %str = call ptr @__crystal_v2_malloc64(i64 %alloc_64)\n"
      emit_raw "  store i32 #{@string_type_id}, ptr %str\n"
      emit_raw "  %bs = getelementptr i8, ptr %str, i32 4\n"
      emit_raw "  store i32 %len, ptr %bs\n"
      emit_raw "  %sz = getelementptr i8, ptr %str, i32 8\n"
      emit_raw "  store i32 %len, ptr %sz\n"
      emit_raw "  %data = getelementptr i8, ptr %str, i32 12\n"
      emit_raw "  %len64c = sext i32 %len to i64\n"
      emit_raw "  call void @llvm.memcpy.p0.p0.i64(ptr %data, ptr %tmp, i64 %len64c, i1 false)\n"
      emit_raw "  %null_pos = getelementptr i8, ptr %data, i32 %len\n"
      emit_raw "  store i8 0, ptr %null_pos\n"
      emit_raw "  ret ptr %str\n"
      emit_raw "}\n\n"

      # File.read — reads entire file into Crystal String
      emit_raw "define ptr @__crystal_v2_file_read(ptr %path) {\n"
      emit_raw "entry:\n"
      emit_raw "  %cstr = getelementptr i8, ptr %path, i32 12\n"
      emit_raw "  %fd = call i32 (ptr, i32, ...) @open(ptr %cstr, i32 0)\n"
      emit_raw "  %fd_bad = icmp slt i32 %fd, 0\n"
      emit_raw "  br i1 %fd_bad, label %err, label %opened\n"
      emit_raw "opened:\n"
      emit_raw "  %size = call i64 @lseek(i32 %fd, i64 0, i32 2)\n"
      emit_raw "  %ignore = call i64 @lseek(i32 %fd, i64 0, i32 0)\n"
      emit_raw "  %size32 = trunc i64 %size to i32\n"
      emit_raw "  %alloc_i32 = add i32 %size32, 13\n"
      emit_raw "  %alloc = sext i32 %alloc_i32 to i64\n"
      emit_raw "  %str = call ptr @__crystal_v2_malloc64(i64 %alloc)\n"
      emit_raw "  store i32 #{@string_type_id}, ptr %str\n"
      emit_raw "  %bs = getelementptr i8, ptr %str, i32 4\n"
      emit_raw "  store i32 %size32, ptr %bs\n"
      emit_raw "  %sz = getelementptr i8, ptr %str, i32 8\n"
      emit_raw "  store i32 %size32, ptr %sz\n"
      emit_raw "  %data = getelementptr i8, ptr %str, i32 12\n"
      emit_raw "  %rsize = call i64 @read(i32 %fd, ptr %data, i64 %size)\n"
      emit_raw "  %null_pos = getelementptr i8, ptr %data, i32 %size32\n"
      emit_raw "  store i8 0, ptr %null_pos\n"
      emit_raw "  %ignore2 = call i32 @close(i32 %fd)\n"
      emit_raw "  ret ptr %str\n"
      emit_raw "err:\n"
      emit_raw "  %empty = call ptr @__crystal_v2_malloc64(i64 13)\n"
      emit_raw "  store i32 #{@string_type_id}, ptr %empty\n"
      emit_raw "  %ebs = getelementptr i8, ptr %empty, i32 4\n"
      emit_raw "  store i32 0, ptr %ebs\n"
      emit_raw "  %esz = getelementptr i8, ptr %empty, i32 8\n"
      emit_raw "  store i32 0, ptr %esz\n"
      emit_raw "  %edata = getelementptr i8, ptr %empty, i32 12\n"
      emit_raw "  store i8 0, ptr %edata\n"
      emit_raw "  ret ptr %empty\n"
      emit_raw "}\n\n"

      # File.write — writes Crystal String to file (returns nil)
      emit_raw "define void @__crystal_v2_file_write(ptr %path, ptr %content) {\n"
      emit_raw "entry:\n"
      emit_raw "  %cstr = getelementptr i8, ptr %path, i32 12\n"
      # O_WRONLY=1 | O_CREAT=0x200 | O_TRUNC=0x400 = 0x601 = 1537
      emit_raw "  %fd = call i32 (ptr, i32, ...) @open(ptr %cstr, i32 1537, i32 420)\n"
      emit_raw "  %fd_bad = icmp slt i32 %fd, 0\n"
      emit_raw "  br i1 %fd_bad, label %done, label %opened\n"
      emit_raw "opened:\n"
      emit_raw "  %bs_ptr = getelementptr i8, ptr %content, i32 4\n"
      emit_raw "  %bs = load i32, ptr %bs_ptr\n"
      emit_raw "  %data = getelementptr i8, ptr %content, i32 12\n"
      emit_raw "  %bs64 = sext i32 %bs to i64\n"
      emit_raw "  %ignore = call i64 @write(i32 %fd, ptr %data, i64 %bs64)\n"
      emit_raw "  %ignore2 = call i32 @close(i32 %fd)\n"
      emit_raw "  br label %done\n"
      emit_raw "done:\n"
      emit_raw "  ret void\n"
      emit_raw "}\n\n"

      # File.dirname — extract directory part of a path string.
      # Implements POSIX dirname logic: strip trailing seps, strip filename, strip trailing seps.
      emit_raw "define ptr @__crystal_v2_file_dirname(ptr %path) {\n"
      emit_raw "entry:\n"
      emit_raw "  %bs_ptr = getelementptr i8, ptr %path, i32 4\n"
      emit_raw "  %len = load i32, ptr %bs_ptr\n"
      emit_raw "  %is_empty = icmp eq i32 %len, 0\n"
      emit_raw "  br i1 %is_empty, label %ret_dot, label %scan_start\n"
      emit_raw "scan_start:\n"
      emit_raw "  %data = getelementptr i8, ptr %path, i32 12\n"
      emit_raw "  %pos0 = sub i32 %len, 1\n"
      emit_raw "  br label %strip_trailing_sep\n"
      # Stage 0: skip trailing separators
      emit_raw "strip_trailing_sep:\n"
      emit_raw "  %p0 = phi i32 [ %pos0, %scan_start ], [ %p0_next, %strip_trailing_sep_next ]\n"
      emit_raw "  %p0_neg = icmp slt i32 %p0, 0\n"
      emit_raw "  br i1 %p0_neg, label %only_seps, label %check_trailing\n"
      emit_raw "check_trailing:\n"
      emit_raw "  %b0_ptr = getelementptr i8, ptr %data, i32 %p0\n"
      emit_raw "  %b0 = load i8, ptr %b0_ptr\n"
      emit_raw "  %is_sep0 = icmp eq i8 %b0, 47\n"  # '/' = 47
      emit_raw "  br i1 %is_sep0, label %strip_trailing_sep_next, label %strip_filename\n"
      emit_raw "strip_trailing_sep_next:\n"
      emit_raw "  %p0_next = sub i32 %p0, 1\n"
      emit_raw "  br label %strip_trailing_sep\n"
      # Stage 1: skip filename (non-separator chars)
      emit_raw "strip_filename:\n"
      emit_raw "  %p1 = phi i32 [ %p0, %check_trailing ], [ %p1_next, %strip_filename_next ]\n"
      emit_raw "  %p1_neg = icmp slt i32 %p1, 0\n"
      emit_raw "  br i1 %p1_neg, label %ret_dot, label %check_filename\n"
      emit_raw "check_filename:\n"
      emit_raw "  %b1_ptr = getelementptr i8, ptr %data, i32 %p1\n"
      emit_raw "  %b1 = load i8, ptr %b1_ptr\n"
      emit_raw "  %is_sep1 = icmp eq i8 %b1, 47\n"
      emit_raw "  br i1 %is_sep1, label %strip_dir_sep, label %strip_filename_next\n"
      emit_raw "strip_filename_next:\n"
      emit_raw "  %p1_next = sub i32 %p1, 1\n"
      emit_raw "  br label %strip_filename\n"
      # Stage 2: skip trailing seps before dirname, stop at non-sep
      emit_raw "strip_dir_sep:\n"
      emit_raw "  %p2 = phi i32 [ %p1, %check_filename ], [ %p2_next, %strip_dir_sep_next ]\n"
      emit_raw "  %p2_neg = icmp slt i32 %p2, 0\n"
      emit_raw "  br i1 %p2_neg, label %root_only, label %check_dir_sep\n"
      emit_raw "check_dir_sep:\n"
      emit_raw "  %b2_ptr = getelementptr i8, ptr %data, i32 %p2\n"
      emit_raw "  %b2 = load i8, ptr %b2_ptr\n"
      emit_raw "  %is_sep2 = icmp eq i8 %b2, 47\n"
      emit_raw "  br i1 %is_sep2, label %strip_dir_sep_next, label %found_dirname\n"
      emit_raw "strip_dir_sep_next:\n"
      emit_raw "  %p2_next = sub i32 %p2, 1\n"
      emit_raw "  br label %strip_dir_sep\n"
      # Found dirname: byte_slice(0, p2+1)
      emit_raw "found_dirname:\n"
      emit_raw "  %end_pos = add i32 %p2, 1\n"
      emit_raw "  br label %make_slice\n"
      # Only separators: return first char ("/")
      emit_raw "only_seps:\n"
      emit_raw "  br label %root_only\n"
      emit_raw "root_only:\n"
      emit_raw "  br label %make_slice_one\n"
      # Build result string for dirname
      emit_raw "make_slice:\n"
      emit_raw "  %alloc_sz = add i32 %end_pos, 13\n"
      emit_raw "  %alloc64 = sext i32 %alloc_sz to i64\n"
      emit_raw "  %result = call ptr @__crystal_v2_malloc64(i64 %alloc64)\n"
      emit_raw "  store i32 #{@string_type_id}, ptr %result\n"
      emit_raw "  %rbs = getelementptr i8, ptr %result, i32 4\n"
      emit_raw "  store i32 %end_pos, ptr %rbs\n"
      emit_raw "  %rsz = getelementptr i8, ptr %result, i32 8\n"
      emit_raw "  store i32 %end_pos, ptr %rsz\n"
      emit_raw "  %rdata = getelementptr i8, ptr %result, i32 12\n"
      emit_raw "  %copy_len = sext i32 %end_pos to i64\n"
      emit_raw "  call void @llvm.memcpy.p0.p0.i64(ptr %rdata, ptr %data, i64 %copy_len, i1 false)\n"
      emit_raw "  %rnull = getelementptr i8, ptr %rdata, i32 %end_pos\n"
      emit_raw "  store i8 0, ptr %rnull\n"
      emit_raw "  ret ptr %result\n"
      # Return "/" (single separator)
      emit_raw "make_slice_one:\n"
      emit_raw "  %r1 = call ptr @__crystal_v2_malloc64(i64 14)\n"
      emit_raw "  store i32 #{@string_type_id}, ptr %r1\n"
      emit_raw "  %r1bs = getelementptr i8, ptr %r1, i32 4\n"
      emit_raw "  store i32 1, ptr %r1bs\n"
      emit_raw "  %r1sz = getelementptr i8, ptr %r1, i32 8\n"
      emit_raw "  store i32 1, ptr %r1sz\n"
      emit_raw "  %r1data = getelementptr i8, ptr %r1, i32 12\n"
      emit_raw "  store i8 47, ptr %r1data\n"
      emit_raw "  %r1null = getelementptr i8, ptr %r1, i32 13\n"
      emit_raw "  store i8 0, ptr %r1null\n"
      emit_raw "  ret ptr %r1\n"
      # Return "." (no parent)
      emit_raw "ret_dot:\n"
      emit_raw "  %rdot = call ptr @__crystal_v2_malloc64(i64 14)\n"
      emit_raw "  store i32 #{@string_type_id}, ptr %rdot\n"
      emit_raw "  %rdbs = getelementptr i8, ptr %rdot, i32 4\n"
      emit_raw "  store i32 1, ptr %rdbs\n"
      emit_raw "  %rdsz = getelementptr i8, ptr %rdot, i32 8\n"
      emit_raw "  store i32 1, ptr %rdsz\n"
      emit_raw "  %rddata = getelementptr i8, ptr %rdot, i32 12\n"
      emit_raw "  store i8 46, ptr %rddata\n"
      emit_raw "  %rdnull = getelementptr i8, ptr %rdot, i32 13\n"
      emit_raw "  store i8 0, ptr %rdnull\n"
      emit_raw "  ret ptr %rdot\n"
      emit_raw "}\n\n"

      # File.expand_path(path, dir) — resolve relative path against dir, normalize via realpath.
      # path: Crystal String, dir: Crystal String (may be empty → use getcwd).
      # Returns Crystal String with absolute, normalized path.
      emit_raw "define ptr @__crystal_v2_file_expand_path(ptr %path, ptr %dir) {\n"
      emit_raw "entry:\n"
      emit_raw "  %buf = alloca [4096 x i8]\n"
      emit_raw "  %buf_ptr = getelementptr [4096 x i8], ptr %buf, i32 0, i32 0\n"
      emit_raw "  %path_data = getelementptr i8, ptr %path, i32 12\n"
      emit_raw "  %path_bs_ptr = getelementptr i8, ptr %path, i32 4\n"
      emit_raw "  %path_bs = load i32, ptr %path_bs_ptr\n"
      # Check if path is absolute (starts with '/')
      emit_raw "  %first_byte_ptr = getelementptr i8, ptr %path_data, i32 0\n"
      emit_raw "  %first_byte = load i8, ptr %first_byte_ptr\n"
      emit_raw "  %is_abs = icmp eq i8 %first_byte, 47\n"
      emit_raw "  br i1 %is_abs, label %try_realpath_abs, label %relative\n"
      # Absolute path: try realpath directly
      emit_raw "try_realpath_abs:\n"
      emit_raw "  %resolved_abs = call ptr @realpath(ptr %path_data, ptr %buf_ptr)\n"
      emit_raw "  %abs_ok = icmp ne ptr %resolved_abs, null\n"
      emit_raw "  br i1 %abs_ok, label %make_string, label %return_path_copy\n"
      # Relative path: build dir/path
      emit_raw "relative:\n"
      emit_raw "  %dir_bs_ptr = getelementptr i8, ptr %dir, i32 4\n"
      emit_raw "  %dir_bs = load i32, ptr %dir_bs_ptr\n"
      emit_raw "  %dir_empty = icmp eq i32 %dir_bs, 0\n"
      emit_raw "  br i1 %dir_empty, label %use_cwd, label %use_dir\n"
      emit_raw "use_cwd:\n"
      emit_raw "  %cwd_buf = alloca [4096 x i8]\n"
      emit_raw "  %cwd_ptr = getelementptr [4096 x i8], ptr %cwd_buf, i32 0, i32 0\n"
      emit_raw "  %cwd_result = call ptr @getcwd(ptr %cwd_ptr, i64 4096)\n"
      emit_raw "  %cwd_len = call i64 @strlen(ptr %cwd_ptr)\n"
      emit_raw "  %cwd_len32 = trunc i64 %cwd_len to i32\n"
      emit_raw "  br label %join_path\n"
      emit_raw "use_dir:\n"
      emit_raw "  %dir_data = getelementptr i8, ptr %dir, i32 12\n"
      emit_raw "  br label %join_path\n"
      emit_raw "join_path:\n"
      emit_raw "  %base_ptr = phi ptr [ %cwd_ptr, %use_cwd ], [ %dir_data, %use_dir ]\n"
      emit_raw "  %base_len = phi i32 [ %cwd_len32, %use_cwd ], [ %dir_bs, %use_dir ]\n"
      # Build joined path in buf: base + "/" + path
      emit_raw "  %join_buf = alloca [8192 x i8]\n"
      emit_raw "  %join_ptr = getelementptr [8192 x i8], ptr %join_buf, i32 0, i32 0\n"
      emit_raw "  %base_len64 = sext i32 %base_len to i64\n"
      emit_raw "  call void @llvm.memcpy.p0.p0.i64(ptr %join_ptr, ptr %base_ptr, i64 %base_len64, i1 false)\n"
      emit_raw "  %sep_pos = getelementptr i8, ptr %join_ptr, i32 %base_len\n"
      emit_raw "  store i8 47, ptr %sep_pos\n"
      emit_raw "  %path_start = add i32 %base_len, 1\n"
      emit_raw "  %path_dest = getelementptr i8, ptr %join_ptr, i32 %path_start\n"
      emit_raw "  %path_bs64 = sext i32 %path_bs to i64\n"
      emit_raw "  call void @llvm.memcpy.p0.p0.i64(ptr %path_dest, ptr %path_data, i64 %path_bs64, i1 false)\n"
      emit_raw "  %total_len = add i32 %path_start, %path_bs\n"
      emit_raw "  %null_pos = getelementptr i8, ptr %join_ptr, i32 %total_len\n"
      emit_raw "  store i8 0, ptr %null_pos\n"
      # Try realpath on joined path
      emit_raw "  %resolved_rel = call ptr @realpath(ptr %join_ptr, ptr %buf_ptr)\n"
      emit_raw "  %rel_ok = icmp ne ptr %resolved_rel, null\n"
      emit_raw "  br i1 %rel_ok, label %make_string, label %return_joined\n"
      # Make Crystal String from resolved path (null-terminated C string in buf)
      emit_raw "make_string:\n"
      emit_raw "  %res_len = call i64 @strlen(ptr %buf_ptr)\n"
      emit_raw "  %res_len32 = trunc i64 %res_len to i32\n"
      emit_raw "  %alloc_i32 = add i32 %res_len32, 13\n"
      emit_raw "  %alloc64 = sext i32 %alloc_i32 to i64\n"
      emit_raw "  %str = call ptr @__crystal_v2_malloc64(i64 %alloc64)\n"
      emit_raw "  store i32 #{@string_type_id}, ptr %str\n"
      emit_raw "  %str_bs = getelementptr i8, ptr %str, i32 4\n"
      emit_raw "  store i32 %res_len32, ptr %str_bs\n"
      emit_raw "  %str_sz = getelementptr i8, ptr %str, i32 8\n"
      emit_raw "  store i32 %res_len32, ptr %str_sz\n"
      emit_raw "  %str_data = getelementptr i8, ptr %str, i32 12\n"
      emit_raw "  %res_len64 = sext i32 %res_len32 to i64\n"
      emit_raw "  call void @llvm.memcpy.p0.p0.i64(ptr %str_data, ptr %buf_ptr, i64 %res_len64, i1 false)\n"
      emit_raw "  %str_null = getelementptr i8, ptr %str_data, i32 %res_len32\n"
      emit_raw "  store i8 0, ptr %str_null\n"
      emit_raw "  ret ptr %str\n"
      # Fallback: return joined path as string (when realpath fails = path doesn't exist)
      emit_raw "return_joined:\n"
      emit_raw "  %j_alloc_i32 = add i32 %total_len, 13\n"
      emit_raw "  %j_alloc64 = sext i32 %j_alloc_i32 to i64\n"
      emit_raw "  %j_str = call ptr @__crystal_v2_malloc64(i64 %j_alloc64)\n"
      emit_raw "  store i32 #{@string_type_id}, ptr %j_str\n"
      emit_raw "  %j_bs = getelementptr i8, ptr %j_str, i32 4\n"
      emit_raw "  store i32 %total_len, ptr %j_bs\n"
      emit_raw "  %j_sz = getelementptr i8, ptr %j_str, i32 8\n"
      emit_raw "  store i32 %total_len, ptr %j_sz\n"
      emit_raw "  %j_data = getelementptr i8, ptr %j_str, i32 12\n"
      emit_raw "  %total_len64 = sext i32 %total_len to i64\n"
      emit_raw "  call void @llvm.memcpy.p0.p0.i64(ptr %j_data, ptr %join_ptr, i64 %total_len64, i1 false)\n"
      emit_raw "  %j_null = getelementptr i8, ptr %j_data, i32 %total_len\n"
      emit_raw "  store i8 0, ptr %j_null\n"
      emit_raw "  ret ptr %j_str\n"
      # Fallback for absolute path when realpath fails: return path as-is
      emit_raw "return_path_copy:\n"
      emit_raw "  %p_alloc_i32 = add i32 %path_bs, 13\n"
      emit_raw "  %p_alloc64 = sext i32 %p_alloc_i32 to i64\n"
      emit_raw "  %p_str = call ptr @__crystal_v2_malloc64(i64 %p_alloc64)\n"
      emit_raw "  store i32 #{@string_type_id}, ptr %p_str\n"
      emit_raw "  %p_bs = getelementptr i8, ptr %p_str, i32 4\n"
      emit_raw "  store i32 %path_bs, ptr %p_bs\n"
      emit_raw "  %p_sz = getelementptr i8, ptr %p_str, i32 8\n"
      emit_raw "  store i32 %path_bs, ptr %p_sz\n"
      emit_raw "  %p_data = getelementptr i8, ptr %p_str, i32 12\n"
      emit_raw "  %p_bs64 = sext i32 %path_bs to i64\n"
      emit_raw "  call void @llvm.memcpy.p0.p0.i64(ptr %p_data, ptr %path_data, i64 %p_bs64, i1 false)\n"
      emit_raw "  %p_null = getelementptr i8, ptr %p_data, i32 %path_bs\n"
      emit_raw "  store i8 0, ptr %p_null\n"
      emit_raw "  ret ptr %p_str\n"
      emit_raw "}\n\n"

      # int64 to i32 (truncate)
      emit_raw "define i32 @__crystal_v2_int64_to_i32(i64 %val) {\n"
      emit_raw "  %trunc = trunc i64 %val to i32\n"
      emit_raw "  ret i32 %trunc\n"
      emit_raw "}\n\n"

      # int32 absolute value
      emit_raw "define i32 @__crystal_v2_int_abs(i32 %val) {\n"
      emit_raw "fn_entry:\n"
      emit_raw "  %neg = icmp slt i32 %val, 0\n"
      emit_raw "  br i1 %neg, label %do_neg, label %done\n"
      emit_raw "do_neg:\n"
      emit_raw "  %negated = sub i32 0, %val\n"
      emit_raw "  br label %done\n"
      emit_raw "done:\n"
      emit_raw "  %result = phi i32 [ %negated, %do_neg ], [ %val, %fn_entry ]\n"
      emit_raw "  ret i32 %result\n"
      emit_raw "}\n\n"

      # Int32 next_power_of_two (bit-trick, clamped for signed)
      # Equivalent to Crystal's Int#next_power_of_two for Int32 (signed)
      emit_raw "define i32 @__crystal_v2_next_power_of_two_i32(i32 %val) {\n"
      emit_raw "fn_entry:\n"
      emit_raw "  %le1 = icmp sle i32 %val, 1\n"
      emit_raw "  br i1 %le1, label %ret_one, label %compute\n"
      emit_raw "compute:\n"
      emit_raw "  %x0 = sub i32 %val, 1\n"
      emit_raw "  %s1 = lshr i32 %x0, 1\n"
      emit_raw "  %x1 = or i32 %x0, %s1\n"
      emit_raw "  %s2 = lshr i32 %x1, 2\n"
      emit_raw "  %x2 = or i32 %x1, %s2\n"
      emit_raw "  %s3 = lshr i32 %x2, 4\n"
      emit_raw "  %x3 = or i32 %x2, %s3\n"
      emit_raw "  %s4 = lshr i32 %x3, 8\n"
      emit_raw "  %x4 = or i32 %x3, %s4\n"
      emit_raw "  %s5 = lshr i32 %x4, 16\n"
      emit_raw "  %x5 = or i32 %x4, %s5\n"
      emit_raw "  %raw = add i32 %x5, 1\n"
      # Clamp to 2^30 = 1073741824 for signed Int32 (bits - 2)
      emit_raw "  %overflow = icmp slt i32 %raw, 0\n"
      emit_raw "  %result = select i1 %overflow, i32 1073741824, i32 %raw\n"
      emit_raw "  ret i32 %result\n"
      emit_raw "ret_one:\n"
      emit_raw "  ret i32 1\n"
      emit_raw "}\n\n"

      # Int32 leading_zeros_count (using LLVM ctlz intrinsic)
      emit_raw "declare i32 @llvm.ctlz.i32(i32, i1)\n"
      emit_raw "define i32 @__crystal_v2_leading_zeros_count_i32(i32 %val) {\n"
      emit_raw "  %result = call i32 @llvm.ctlz.i32(i32 %val, i1 0)\n"
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
      emit_raw "  %tmp = alloca [32 x i8]\n"
      emit_raw "  call i32 (ptr, ptr, ...) @sprintf(ptr %tmp, ptr @.float_fmt_no_nl, double %val)\n"
      emit_raw "  %len64 = call i64 @strlen(ptr %tmp)\n"
      emit_raw "  %len = trunc i64 %len64 to i32\n"
      emit_raw "  %alloc_32 = add i32 %len, 13\n"
      emit_raw "  %alloc_64 = sext i32 %alloc_32 to i64\n"
      emit_raw "  %str = call ptr @__crystal_v2_malloc64(i64 %alloc_64)\n"
      emit_raw "  store i32 #{@string_type_id}, ptr %str\n"
      emit_raw "  %bs = getelementptr i8, ptr %str, i32 4\n"
      emit_raw "  store i32 %len, ptr %bs\n"
      emit_raw "  %sz = getelementptr i8, ptr %str, i32 8\n"
      emit_raw "  store i32 %len, ptr %sz\n"
      emit_raw "  %data = getelementptr i8, ptr %str, i32 12\n"
      emit_raw "  %len64c = sext i32 %len to i64\n"
      emit_raw "  call void @llvm.memcpy.p0.p0.i64(ptr %data, ptr %tmp, i64 %len64c, i1 false)\n"
      emit_raw "  %null_pos = getelementptr i8, ptr %data, i32 %len\n"
      emit_raw "  store i8 0, ptr %null_pos\n"
      emit_raw "  ret ptr %str\n"
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

      # ── char_to_string: Char (i32 codepoint) → Crystal String ──
      emit_raw "define ptr @__crystal_v2_char_to_string(i32 %cp) {\n"
      emit_raw "entry:\n"
      # Determine UTF-8 byte length
      emit_raw "  %is_ascii = icmp ult i32 %cp, 128\n"
      emit_raw "  br i1 %is_ascii, label %ascii, label %check2\n"
      emit_raw "check2:\n"
      emit_raw "  %is_2byte = icmp ult i32 %cp, 2048\n"
      emit_raw "  br i1 %is_2byte, label %utf2, label %check3\n"
      emit_raw "check3:\n"
      emit_raw "  %is_3byte = icmp ult i32 %cp, 65536\n"
      emit_raw "  br i1 %is_3byte, label %utf3, label %utf4\n"
      # ASCII: 1 byte
      emit_raw "ascii:\n"
      emit_raw "  %a_alloc = call ptr @__crystal_v2_malloc64(i64 16)\n"
      emit_raw "  store i32 #{@string_type_id}, ptr %a_alloc\n"
      emit_raw "  %a_bs = getelementptr i8, ptr %a_alloc, i32 4\n"
      emit_raw "  store i32 1, ptr %a_bs\n"
      emit_raw "  %a_sz = getelementptr i8, ptr %a_alloc, i32 8\n"
      emit_raw "  store i32 1, ptr %a_sz\n"
      emit_raw "  %a_data = getelementptr i8, ptr %a_alloc, i32 12\n"
      emit_raw "  %a_byte = trunc i32 %cp to i8\n"
      emit_raw "  store i8 %a_byte, ptr %a_data\n"
      emit_raw "  %a_nul = getelementptr i8, ptr %a_alloc, i32 13\n"
      emit_raw "  store i8 0, ptr %a_nul\n"
      emit_raw "  ret ptr %a_alloc\n"
      # 2-byte UTF-8: 110xxxxx 10xxxxxx
      emit_raw "utf2:\n"
      emit_raw "  %u2_alloc = call ptr @__crystal_v2_malloc64(i64 16)\n"
      emit_raw "  store i32 #{@string_type_id}, ptr %u2_alloc\n"
      emit_raw "  %u2_bs = getelementptr i8, ptr %u2_alloc, i32 4\n"
      emit_raw "  store i32 2, ptr %u2_bs\n"
      emit_raw "  %u2_sz = getelementptr i8, ptr %u2_alloc, i32 8\n"
      emit_raw "  store i32 1, ptr %u2_sz\n"
      emit_raw "  %u2_data = getelementptr i8, ptr %u2_alloc, i32 12\n"
      emit_raw "  %u2_hi = lshr i32 %cp, 6\n"
      emit_raw "  %u2_hi_or = or i32 %u2_hi, 192\n"
      emit_raw "  %u2_b0 = trunc i32 %u2_hi_or to i8\n"
      emit_raw "  store i8 %u2_b0, ptr %u2_data\n"
      emit_raw "  %u2_lo = and i32 %cp, 63\n"
      emit_raw "  %u2_lo_or = or i32 %u2_lo, 128\n"
      emit_raw "  %u2_b1 = trunc i32 %u2_lo_or to i8\n"
      emit_raw "  %u2_d1 = getelementptr i8, ptr %u2_alloc, i32 13\n"
      emit_raw "  store i8 %u2_b1, ptr %u2_d1\n"
      emit_raw "  %u2_nul = getelementptr i8, ptr %u2_alloc, i32 14\n"
      emit_raw "  store i8 0, ptr %u2_nul\n"
      emit_raw "  ret ptr %u2_alloc\n"
      # 3-byte UTF-8: 1110xxxx 10xxxxxx 10xxxxxx
      emit_raw "utf3:\n"
      emit_raw "  %u3_alloc = call ptr @__crystal_v2_malloc64(i64 20)\n"
      emit_raw "  store i32 #{@string_type_id}, ptr %u3_alloc\n"
      emit_raw "  %u3_bs = getelementptr i8, ptr %u3_alloc, i32 4\n"
      emit_raw "  store i32 3, ptr %u3_bs\n"
      emit_raw "  %u3_sz = getelementptr i8, ptr %u3_alloc, i32 8\n"
      emit_raw "  store i32 1, ptr %u3_sz\n"
      emit_raw "  %u3_data = getelementptr i8, ptr %u3_alloc, i32 12\n"
      emit_raw "  %u3_hi = lshr i32 %cp, 12\n"
      emit_raw "  %u3_hi_or = or i32 %u3_hi, 224\n"
      emit_raw "  %u3_b0 = trunc i32 %u3_hi_or to i8\n"
      emit_raw "  store i8 %u3_b0, ptr %u3_data\n"
      emit_raw "  %u3_mid = lshr i32 %cp, 6\n"
      emit_raw "  %u3_mid_and = and i32 %u3_mid, 63\n"
      emit_raw "  %u3_mid_or = or i32 %u3_mid_and, 128\n"
      emit_raw "  %u3_b1 = trunc i32 %u3_mid_or to i8\n"
      emit_raw "  %u3_d1 = getelementptr i8, ptr %u3_alloc, i32 13\n"
      emit_raw "  store i8 %u3_b1, ptr %u3_d1\n"
      emit_raw "  %u3_lo = and i32 %cp, 63\n"
      emit_raw "  %u3_lo_or = or i32 %u3_lo, 128\n"
      emit_raw "  %u3_b2 = trunc i32 %u3_lo_or to i8\n"
      emit_raw "  %u3_d2 = getelementptr i8, ptr %u3_alloc, i32 14\n"
      emit_raw "  store i8 %u3_b2, ptr %u3_d2\n"
      emit_raw "  %u3_nul = getelementptr i8, ptr %u3_alloc, i32 15\n"
      emit_raw "  store i8 0, ptr %u3_nul\n"
      emit_raw "  ret ptr %u3_alloc\n"
      # 4-byte UTF-8: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
      emit_raw "utf4:\n"
      emit_raw "  %u4_alloc = call ptr @__crystal_v2_malloc64(i64 20)\n"
      emit_raw "  store i32 #{@string_type_id}, ptr %u4_alloc\n"
      emit_raw "  %u4_bs = getelementptr i8, ptr %u4_alloc, i32 4\n"
      emit_raw "  store i32 4, ptr %u4_bs\n"
      emit_raw "  %u4_sz = getelementptr i8, ptr %u4_alloc, i32 8\n"
      emit_raw "  store i32 1, ptr %u4_sz\n"
      emit_raw "  %u4_data = getelementptr i8, ptr %u4_alloc, i32 12\n"
      emit_raw "  %u4_hi = lshr i32 %cp, 18\n"
      emit_raw "  %u4_hi_or = or i32 %u4_hi, 240\n"
      emit_raw "  %u4_b0 = trunc i32 %u4_hi_or to i8\n"
      emit_raw "  store i8 %u4_b0, ptr %u4_data\n"
      emit_raw "  %u4_m1 = lshr i32 %cp, 12\n"
      emit_raw "  %u4_m1_and = and i32 %u4_m1, 63\n"
      emit_raw "  %u4_m1_or = or i32 %u4_m1_and, 128\n"
      emit_raw "  %u4_b1 = trunc i32 %u4_m1_or to i8\n"
      emit_raw "  %u4_d1 = getelementptr i8, ptr %u4_alloc, i32 13\n"
      emit_raw "  store i8 %u4_b1, ptr %u4_d1\n"
      emit_raw "  %u4_m2 = lshr i32 %cp, 6\n"
      emit_raw "  %u4_m2_and = and i32 %u4_m2, 63\n"
      emit_raw "  %u4_m2_or = or i32 %u4_m2_and, 128\n"
      emit_raw "  %u4_b2 = trunc i32 %u4_m2_or to i8\n"
      emit_raw "  %u4_d2 = getelementptr i8, ptr %u4_alloc, i32 14\n"
      emit_raw "  store i8 %u4_b2, ptr %u4_d2\n"
      emit_raw "  %u4_lo = and i32 %cp, 63\n"
      emit_raw "  %u4_lo_or = or i32 %u4_lo, 128\n"
      emit_raw "  %u4_b3 = trunc i32 %u4_lo_or to i8\n"
      emit_raw "  %u4_d3 = getelementptr i8, ptr %u4_alloc, i32 15\n"
      emit_raw "  store i8 %u4_b3, ptr %u4_d3\n"
      emit_raw "  %u4_nul = getelementptr i8, ptr %u4_alloc, i32 16\n"
      emit_raw "  store i8 0, ptr %u4_nul\n"
      emit_raw "  ret ptr %u4_alloc\n"
      emit_raw "}\n\n"

      # ── string_concat: two Crystal Strings → one new Crystal String ──
      # Crystal String layout: {type_id:i32, bytesize:i32, size:i32, bytes:[N x i8]}
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
      emit_raw "  %a_bs_ptr = getelementptr i8, ptr %a, i32 4\n"
      emit_raw "  %a_bs = load i32, ptr %a_bs_ptr\n"
      emit_raw "  %b_bs_ptr = getelementptr i8, ptr %b, i32 4\n"
      emit_raw "  %b_bs = load i32, ptr %b_bs_ptr\n"
      emit_raw "  %total = add i32 %a_bs, %b_bs\n"
      emit_raw "  %alloc_32 = add i32 %total, 13\n"
      emit_raw "  %alloc_64 = sext i32 %alloc_32 to i64\n"
      emit_raw "  %buf = call ptr @__crystal_v2_malloc64(i64 %alloc_64)\n"
      emit_raw "  store i32 #{@string_type_id}, ptr %buf\n"
      emit_raw "  %buf_bs = getelementptr i8, ptr %buf, i32 4\n"
      emit_raw "  store i32 %total, ptr %buf_bs\n"
      emit_raw "  %buf_sz = getelementptr i8, ptr %buf, i32 8\n"
      emit_raw "  store i32 %total, ptr %buf_sz\n"
      emit_raw "  %a_data = getelementptr i8, ptr %a, i32 12\n"
      emit_raw "  %buf_data = getelementptr i8, ptr %buf, i32 12\n"
      emit_raw "  %a_bs_64 = sext i32 %a_bs to i64\n"
      emit_raw "  call void @llvm.memcpy.p0.p0.i64(ptr %buf_data, ptr %a_data, i64 %a_bs_64, i1 false)\n"
      emit_raw "  %buf_data2 = getelementptr i8, ptr %buf_data, i32 %a_bs\n"
      emit_raw "  %b_data = getelementptr i8, ptr %b, i32 12\n"
      emit_raw "  %b_bs_64 = sext i32 %b_bs to i64\n"
      emit_raw "  call void @llvm.memcpy.p0.p0.i64(ptr %buf_data2, ptr %b_data, i64 %b_bs_64, i1 false)\n"
      emit_raw "  %null_pos = getelementptr i8, ptr %buf_data, i32 %total\n"
      emit_raw "  store i8 0, ptr %null_pos\n"
      emit_raw "  ret ptr %buf\n"
      emit_raw "}\n\n"

      # ── string_interpolate: N Crystal Strings → one new Crystal String (single alloc) ──
      # Takes a ptr to array of String ptrs and a count.
      # Pass 1: sum bytesizes. Pass 2: alloc once, memcpy all parts.
      emit_raw "define ptr @__crystal_v2_string_interpolate(ptr %parts, i32 %count) {\n"
      emit_raw "entry:\n"
      emit_raw "  %total_ptr = alloca i32\n"
      emit_raw "  store i32 0, ptr %total_ptr\n"
      emit_raw "  %i_ptr = alloca i32\n"
      emit_raw "  store i32 0, ptr %i_ptr\n"
      emit_raw "  br label %sum_loop\n"
      # ── pass 1: compute total bytesize ──
      emit_raw "sum_loop:\n"
      emit_raw "  %si = load i32, ptr %i_ptr\n"
      emit_raw "  %sdone = icmp sge i32 %si, %count\n"
      emit_raw "  br i1 %sdone, label %alloc, label %sum_body\n"
      emit_raw "sum_body:\n"
      emit_raw "  %sp = getelementptr ptr, ptr %parts, i32 %si\n"
      emit_raw "  %spart = load ptr, ptr %sp\n"
      emit_raw "  %snull = icmp eq ptr %spart, null\n"
      emit_raw "  br i1 %snull, label %sum_next, label %sum_add\n"
      emit_raw "sum_add:\n"
      emit_raw "  %sbs_ptr = getelementptr i8, ptr %spart, i32 4\n"
      emit_raw "  %sbs = load i32, ptr %sbs_ptr\n"
      emit_raw "  %sold = load i32, ptr %total_ptr\n"
      emit_raw "  %snew = add i32 %sold, %sbs\n"
      emit_raw "  store i32 %snew, ptr %total_ptr\n"
      emit_raw "  br label %sum_next\n"
      emit_raw "sum_next:\n"
      emit_raw "  %snxi = add i32 %si, 1\n"
      emit_raw "  store i32 %snxi, ptr %i_ptr\n"
      emit_raw "  br label %sum_loop\n"
      # ── allocate result string ──
      emit_raw "alloc:\n"
      emit_raw "  %total = load i32, ptr %total_ptr\n"
      emit_raw "  %a32 = add i32 %total, 13\n"
      emit_raw "  %a64 = sext i32 %a32 to i64\n"
      emit_raw "  %buf = call ptr @__crystal_v2_malloc64(i64 %a64)\n"
      emit_raw "  store i32 #{@string_type_id}, ptr %buf\n"
      emit_raw "  %bbs = getelementptr i8, ptr %buf, i32 4\n"
      emit_raw "  store i32 %total, ptr %bbs\n"
      emit_raw "  %bsz = getelementptr i8, ptr %buf, i32 8\n"
      emit_raw "  store i32 %total, ptr %bsz\n"
      emit_raw "  %off_ptr = alloca i32\n"
      emit_raw "  store i32 0, ptr %off_ptr\n"
      emit_raw "  store i32 0, ptr %i_ptr\n"
      emit_raw "  br label %copy_loop\n"
      # ── pass 2: memcpy each part into buffer ──
      emit_raw "copy_loop:\n"
      emit_raw "  %ci = load i32, ptr %i_ptr\n"
      emit_raw "  %cdone = icmp sge i32 %ci, %count\n"
      emit_raw "  br i1 %cdone, label %finish, label %copy_body\n"
      emit_raw "copy_body:\n"
      emit_raw "  %cp = getelementptr ptr, ptr %parts, i32 %ci\n"
      emit_raw "  %cpart = load ptr, ptr %cp\n"
      emit_raw "  %cnull = icmp eq ptr %cpart, null\n"
      emit_raw "  br i1 %cnull, label %copy_next, label %copy_do\n"
      emit_raw "copy_do:\n"
      emit_raw "  %cbs_ptr = getelementptr i8, ptr %cpart, i32 4\n"
      emit_raw "  %cbs = load i32, ptr %cbs_ptr\n"
      emit_raw "  %cdata = getelementptr i8, ptr %cpart, i32 12\n"
      emit_raw "  %coff = load i32, ptr %off_ptr\n"
      emit_raw "  %dst = getelementptr i8, ptr %buf, i32 12\n"
      emit_raw "  %dst2 = getelementptr i8, ptr %dst, i32 %coff\n"
      emit_raw "  %cbs64 = sext i32 %cbs to i64\n"
      emit_raw "  call void @llvm.memcpy.p0.p0.i64(ptr %dst2, ptr %cdata, i64 %cbs64, i1 false)\n"
      emit_raw "  %noff = add i32 %coff, %cbs\n"
      emit_raw "  store i32 %noff, ptr %off_ptr\n"
      emit_raw "  br label %copy_next\n"
      emit_raw "copy_next:\n"
      emit_raw "  %cnxi = add i32 %ci, 1\n"
      emit_raw "  store i32 %cnxi, ptr %i_ptr\n"
      emit_raw "  br label %copy_loop\n"
      # ── null-terminate and return ──
      emit_raw "finish:\n"
      emit_raw "  %foff = load i32, ptr %off_ptr\n"
      emit_raw "  %ndst = getelementptr i8, ptr %buf, i32 12\n"
      emit_raw "  %ndst2 = getelementptr i8, ptr %ndst, i32 %foff\n"
      emit_raw "  store i8 0, ptr %ndst2\n"
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
      # String repeat: reads Crystal String (header at 0/4/8, data at 12), returns Crystal String
      emit_raw "define ptr @__crystal_v2_string_repeat(ptr %str, i32 %count) {\n"
      emit_raw "entry:\n"
      emit_raw "  %cmp = icmp sle i32 %count, 0\n"
      emit_raw "  br i1 %cmp, label %ret_empty, label %repeat\n"
      # Empty result → return empty Crystal String
      emit_raw "ret_empty:\n"
      emit_raw "  %empty = call ptr @__crystal_v2_malloc64(i64 13)\n"
      emit_raw "  store i32 #{@string_type_id}, ptr %empty\n"
      emit_raw "  %empty_bs = getelementptr i8, ptr %empty, i32 4\n"
      emit_raw "  store i32 0, ptr %empty_bs\n"
      emit_raw "  %empty_sz = getelementptr i8, ptr %empty, i32 8\n"
      emit_raw "  store i32 0, ptr %empty_sz\n"
      emit_raw "  %empty_d = getelementptr i8, ptr %empty, i32 12\n"
      emit_raw "  store i8 0, ptr %empty_d\n"
      emit_raw "  ret ptr %empty\n"
      # Read source string bytesize and data pointer
      emit_raw "repeat:\n"
      emit_raw "  %bs_ptr = getelementptr i8, ptr %str, i32 4\n"
      emit_raw "  %bs = load i32, ptr %bs_ptr\n"
      emit_raw "  %src_data = getelementptr i8, ptr %str, i32 12\n"
      # Calculate total bytesize = bs * count
      emit_raw "  %total = mul i32 %bs, %count\n"
      # Allocate result Crystal String: 12-byte header + total + 1 (null terminator)
      emit_raw "  %alloc_i32 = add i32 %total, 13\n"
      emit_raw "  %alloc_i64 = sext i32 %alloc_i32 to i64\n"
      emit_raw "  %result = call ptr @__crystal_v2_malloc64(i64 %alloc_i64)\n"
      # Write header: type_id, bytesize, charsize
      emit_raw "  store i32 #{@string_type_id}, ptr %result\n"
      emit_raw "  %r_bs = getelementptr i8, ptr %result, i32 4\n"
      emit_raw "  store i32 %total, ptr %r_bs\n"
      emit_raw "  %r_sz = getelementptr i8, ptr %result, i32 8\n"
      emit_raw "  store i32 %total, ptr %r_sz\n"
      emit_raw "  %r_data = getelementptr i8, ptr %result, i32 12\n"
      # Copy source data `count` times using memcpy loop
      emit_raw "  %bs64 = sext i32 %bs to i64\n"
      emit_raw "  br label %loop\n"
      emit_raw "loop:\n"
      emit_raw "  %i = phi i32 [0, %repeat], [%next_i, %loop]\n"
      emit_raw "  %dest = phi ptr [%r_data, %repeat], [%next_dest, %loop]\n"
      emit_raw "  call void @llvm.memcpy.p0.p0.i64(ptr %dest, ptr %src_data, i64 %bs64, i1 false)\n"
      emit_raw "  %next_dest = getelementptr i8, ptr %dest, i32 %bs\n"
      emit_raw "  %next_i = add i32 %i, 1\n"
      emit_raw "  %done = icmp sge i32 %next_i, %count\n"
      emit_raw "  br i1 %done, label %finalize, label %loop\n"
      # Null-terminate and return
      emit_raw "finalize:\n"
      emit_raw "  store i8 0, ptr %next_dest\n"
      emit_raw "  ret ptr %result\n"
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

      # Global exception state — stack of jmp_bufs for nested begin/rescue
      emit_raw "; Global exception handling state (stack-based for nesting)\n"
      emit_raw "@__crystal_exc_jmpbufs = global [64 x %jmp_buf] zeroinitializer\n"
      emit_raw "@__crystal_exc_depth = global i32 0\n"
      emit_raw "@__crystal_exc_ptr = global ptr null\n"
      emit_raw "\n"

      # Helper: get pointer to current jmp_buf (at depth-1)
      emit_raw "define ptr @__crystal_exc_current_jmpbuf() {\n"
      emit_raw "  %depth = load i32, ptr @__crystal_exc_depth\n"
      emit_raw "  %idx = sub i32 %depth, 1\n"
      emit_raw "  %ptr = getelementptr [64 x %jmp_buf], ptr @__crystal_exc_jmpbufs, i32 0, i32 %idx\n"
      emit_raw "  ret ptr %ptr\n"
      emit_raw "}\n\n"

      # End exception handler scope — decrement depth
      emit_raw "define void @__crystal_v2_try_end() {\n"
      emit_raw "  %depth = load i32, ptr @__crystal_exc_depth\n"
      emit_raw "  %new_depth = sub i32 %depth, 1\n"
      emit_raw "  %clamped = call i32 @llvm.smax.i32(i32 %new_depth, i32 0)\n"
      emit_raw "  store i32 %clamped, ptr @__crystal_exc_depth\n"
      emit_raw "  ret void\n"
      emit_raw "}\n"
      emit_raw "declare i32 @llvm.smax.i32(i32, i32)\n\n"

      emit_raw "define void @__crystal_v2_raise(ptr %exc) noinline {\n"
      emit_raw "  store ptr %exc, ptr @__crystal_exc_ptr\n"
      emit_raw "  %depth = load i32, ptr @__crystal_exc_depth\n"
      emit_raw "  %has_handler = icmp sgt i32 %depth, 0\n"
      emit_raw "  br i1 %has_handler, label %do_longjmp, label %no_handler\n"
      emit_raw "do_longjmp:\n"
      emit_raw "  %jmpbuf = call ptr @__crystal_exc_current_jmpbuf()\n"
      emit_raw "  call void @longjmp(ptr %jmpbuf, i32 1)\n"
      emit_raw "  unreachable\n"
      emit_raw "no_handler:\n"
      emit_raw "  call void @abort()\n"
      emit_raw "  unreachable\n"
      emit_raw "}\n\n"

      runtime_error_type = @module.type_registry.get_by_name("RuntimeError")
      exception_type = @module.type_registry.get_by_name("Exception")
      runtime_error_size = runtime_error_type.try(&.size.to_i64) || exception_type.try(&.size.to_i64) || 64_i64
      runtime_error_type_id = runtime_error_type.try(&.id.to_i32) || exception_type.try(&.id.to_i32) || 1_i32
      runtime_error_alloc_size = runtime_error_size + 8_i64 # + refcount header

      emit_raw "define void @__crystal_v2_raise_msg(ptr %msg) noinline {\n"
      emit_raw "  ; Build a minimal RuntimeError object: {type_id, message, ...}\n"
      emit_raw "  ; so rescue variables can safely call Exception#message.\n"
      emit_raw "  %raw = call ptr @__crystal_v2_malloc64(i64 #{runtime_error_alloc_size})\n"
      emit_raw "  store i64 1, ptr %raw, align 8\n"
      emit_raw "  %exc = getelementptr i8, ptr %raw, i64 8\n"
      emit_raw "  call void @llvm.memset.p0.i64(ptr %exc, i8 0, i64 #{runtime_error_size}, i1 false)\n"
      emit_raw "  store i32 #{runtime_error_type_id}, ptr %exc\n"
      emit_raw "  %msg_union.ptr = alloca %Nil$_$OR$_String.union, align 8\n"
      emit_raw "  %msg_union.type_id_ptr = getelementptr %Nil$_$OR$_String.union, ptr %msg_union.ptr, i32 0, i32 0\n"
      emit_raw "  store i32 1, ptr %msg_union.type_id_ptr\n"
      emit_raw "  %msg_union.payload_ptr = getelementptr %Nil$_$OR$_String.union, ptr %msg_union.ptr, i32 0, i32 1\n"
      emit_raw "  store ptr %msg, ptr %msg_union.payload_ptr, align 8\n"
      emit_raw "  %msg_union = load %Nil$_$OR$_String.union, ptr %msg_union.ptr\n"
      emit_raw "  %msg_field = getelementptr i8, ptr %exc, i64 8\n"
      emit_raw "  store %Nil$_$OR$_String.union %msg_union, ptr %msg_field\n"
      emit_raw "  store ptr %exc, ptr @__crystal_exc_ptr\n"
      emit_raw "  %depth = load i32, ptr @__crystal_exc_depth\n"
      emit_raw "  %has_handler = icmp sgt i32 %depth, 0\n"
      emit_raw "  br i1 %has_handler, label %do_longjmp, label %no_handler\n"
      emit_raw "do_longjmp:\n"
      emit_raw "  %jmpbuf = call ptr @__crystal_exc_current_jmpbuf()\n"
      emit_raw "  call void @longjmp(ptr %jmpbuf, i32 1)\n"
      emit_raw "  unreachable\n"
      emit_raw "no_handler:\n"
      emit_raw "  ; No rescue frame: print and abort.\n"
      emit_raw "  %cmsg = getelementptr i8, ptr %msg, i32 12\n"
      emit_raw "  call i32 @puts(ptr %cmsg)\n"
      emit_raw "  call void @abort()\n"
      emit_raw "  unreachable\n"
      emit_raw "}\n\n"

      emit_raw "define void @__crystal_v2_reraise() noinline {\n"
      emit_raw "  %exc = load ptr, ptr @__crystal_exc_ptr\n"
      emit_raw "  %depth = load i32, ptr @__crystal_exc_depth\n"
      emit_raw "  %has_handler = icmp sgt i32 %depth, 0\n"
      emit_raw "  br i1 %has_handler, label %do_longjmp, label %no_handler\n"
      emit_raw "do_longjmp:\n"
      emit_raw "  %jmpbuf = call ptr @__crystal_exc_current_jmpbuf()\n"
      emit_raw "  call void @longjmp(ptr %jmpbuf, i32 1)\n"
      emit_raw "  unreachable\n"
      emit_raw "no_handler:\n"
      emit_raw "  call void @abort()\n"
      emit_raw "  unreachable\n"
      emit_raw "}\n\n"

      emit_raw "define ptr @__crystal_v2_get_exception() {\n"
      emit_raw "  %exc = load ptr, ptr @__crystal_exc_ptr\n"
      emit_raw "  ret ptr %exc\n"
      emit_raw "}\n\n"

      # ═══════════════════════════════════════════════════════════════════════
      # REGEX RUNTIME (PCRE2-based)
      # ═══════════════════════════════════════════════════════════════════════
      # Our regex struct layout: { ptr @code, ptr @match_data }
      # - offset 0: pcre2_code_8* (compiled pattern)
      # - offset 8: pcre2_match_data_8* (reusable match data)

      # PCRE2 external functions are already declared in emit_external_declarations

      # Global: last match data for $~ / capture group access
      emit_raw "@__crystal_v2_last_match_data = global ptr null\n"
      emit_raw "@__crystal_v2_last_match_str = global ptr null\n\n"

      # __crystal_v2_regex_new(pattern_str: Crystal::String*, options: i32) -> ptr
      # Compiles a PCRE2 regex pattern. Returns pointer to {code, match_data} struct.
      emit_raw "define ptr @__crystal_v2_regex_new(ptr %pattern_str, i32 %options) {\n"
      emit_raw "entry:\n"
      emit_raw "  %bs_ptr = getelementptr i8, ptr %pattern_str, i32 4\n"
      emit_raw "  %bytesize = load i32, ptr %bs_ptr\n"
      emit_raw "  %data = getelementptr i8, ptr %pattern_str, i32 12\n"
      emit_raw "  %bs64 = sext i32 %bytesize to i64\n"
      # PCRE2 default options: UTF(0x80000) | DUPNAMES(0x40) | UCP(0x20000) = 0xA0040 = 655424
      emit_raw "  %pcre_opts = or i32 %options, 655424\n"
      emit_raw "  %errorcode = alloca i32\n"
      emit_raw "  %erroroffset = alloca i64\n"
      emit_raw "  %re = call ptr @pcre2_compile_8(ptr %data, i64 %bs64, i32 %pcre_opts, ptr %errorcode, ptr %erroroffset, ptr null)\n"
      emit_raw "  %re_null = icmp eq ptr %re, null\n"
      emit_raw "  br i1 %re_null, label %fail, label %ok\n"
      emit_raw "ok:\n"
      # Allocate 16-byte struct: {code*, match_data*}
      emit_raw "  %regex = call ptr @__crystal_v2_malloc64(i64 16)\n"
      emit_raw "  store ptr %re, ptr %regex\n"
      emit_raw "  %md = call ptr @pcre2_match_data_create_from_pattern_8(ptr %re, ptr null)\n"
      emit_raw "  %md_slot = getelementptr i8, ptr %regex, i32 8\n"
      emit_raw "  store ptr %md, ptr %md_slot\n"
      # JIT compile (ignore errors)
      emit_raw "  call i32 @pcre2_jit_compile_8(ptr %re, i32 1)\n"
      emit_raw "  ret ptr %regex\n"
      emit_raw "fail:\n"
      # Return null on compilation failure
      emit_raw "  ret ptr null\n"
      emit_raw "}\n\n"

      # __crystal_v2_regex_match_q(regex: ptr, str: Crystal::String*) -> i1
      # Returns true if regex matches the string. Sets last match data for capture groups.
      emit_raw "define i1 @__crystal_v2_regex_match_q(ptr %regex, ptr %str) {\n"
      emit_raw "entry:\n"
      emit_raw "  %re_null = icmp eq ptr %regex, null\n"
      emit_raw "  br i1 %re_null, label %ret_false, label %check_str\n"
      emit_raw "check_str:\n"
      emit_raw "  %str_null = icmp eq ptr %str, null\n"
      emit_raw "  br i1 %str_null, label %ret_false, label %do_match\n"
      emit_raw "do_match:\n"
      emit_raw "  %re_ptr = load ptr, ptr %regex\n"
      emit_raw "  %md_slot = getelementptr i8, ptr %regex, i32 8\n"
      emit_raw "  %md = load ptr, ptr %md_slot\n"
      emit_raw "  %bs_ptr = getelementptr i8, ptr %str, i32 4\n"
      emit_raw "  %bytesize = load i32, ptr %bs_ptr\n"
      emit_raw "  %data = getelementptr i8, ptr %str, i32 12\n"
      emit_raw "  %bs64 = sext i32 %bytesize to i64\n"
      emit_raw "  %rc = call i32 @pcre2_match_8(ptr %re_ptr, ptr %data, i64 %bs64, i64 0, i32 0, ptr %md, ptr null)\n"
      emit_raw "  %matched = icmp sge i32 %rc, 0\n"
      # Store last match data for capture group access
      emit_raw "  br i1 %matched, label %store_match, label %ret_false\n"
      emit_raw "store_match:\n"
      emit_raw "  store ptr %md, ptr @__crystal_v2_last_match_data\n"
      emit_raw "  store ptr %str, ptr @__crystal_v2_last_match_str\n"
      emit_raw "  ret i1 1\n"
      emit_raw "ret_false:\n"
      # Clear last match data on failure
      emit_raw "  store ptr null, ptr @__crystal_v2_last_match_data\n"
      emit_raw "  ret i1 0\n"
      emit_raw "}\n\n"

      # __crystal_v2_regex_match_pos(regex: ptr, str: Crystal::String*) -> i32
      # Returns byte position of match start, or -1 if no match.
      emit_raw "define i32 @__crystal_v2_regex_match_pos(ptr %regex, ptr %str) {\n"
      emit_raw "entry:\n"
      emit_raw "  %re_null = icmp eq ptr %regex, null\n"
      emit_raw "  br i1 %re_null, label %ret_neg, label %check_str\n"
      emit_raw "check_str:\n"
      emit_raw "  %str_null = icmp eq ptr %str, null\n"
      emit_raw "  br i1 %str_null, label %ret_neg, label %do_match\n"
      emit_raw "do_match:\n"
      emit_raw "  %re_ptr = load ptr, ptr %regex\n"
      emit_raw "  %md_slot = getelementptr i8, ptr %regex, i32 8\n"
      emit_raw "  %md = load ptr, ptr %md_slot\n"
      emit_raw "  %bs_ptr = getelementptr i8, ptr %str, i32 4\n"
      emit_raw "  %bytesize = load i32, ptr %bs_ptr\n"
      emit_raw "  %data = getelementptr i8, ptr %str, i32 12\n"
      emit_raw "  %bs64 = sext i32 %bytesize to i64\n"
      emit_raw "  %rc = call i32 @pcre2_match_8(ptr %re_ptr, ptr %data, i64 %bs64, i64 0, i32 0, ptr %md, ptr null)\n"
      emit_raw "  %matched = icmp sge i32 %rc, 0\n"
      emit_raw "  br i1 %matched, label %get_pos, label %ret_neg\n"
      emit_raw "get_pos:\n"
      emit_raw "  store ptr %md, ptr @__crystal_v2_last_match_data\n"
      emit_raw "  store ptr %str, ptr @__crystal_v2_last_match_str\n"
      emit_raw "  %ovector = call ptr @pcre2_get_ovector_pointer_8(ptr %md)\n"
      emit_raw "  %start64 = load i64, ptr %ovector\n"
      emit_raw "  %start = trunc i64 %start64 to i32\n"
      emit_raw "  ret i32 %start\n"
      emit_raw "ret_neg:\n"
      emit_raw "  store ptr null, ptr @__crystal_v2_last_match_data\n"
      emit_raw "  ret i32 -1\n"
      emit_raw "}\n\n"

      # __crystal_v2_regex_capture(capture_idx: i32) -> Crystal::String* (or null)
      # Returns the captured substring from the last regex match.
      emit_raw "define ptr @__crystal_v2_regex_capture(i32 %idx) {\n"
      emit_raw "entry:\n"
      emit_raw "  %md = load ptr, ptr @__crystal_v2_last_match_data\n"
      emit_raw "  %md_null = icmp eq ptr %md, null\n"
      emit_raw "  br i1 %md_null, label %ret_null, label %get_ovector\n"
      emit_raw "get_ovector:\n"
      emit_raw "  %ovector = call ptr @pcre2_get_ovector_pointer_8(ptr %md)\n"
      # ovector[idx*2] = start, ovector[idx*2+1] = end (both i64/size_t)
      emit_raw "  %idx2 = mul i32 %idx, 2\n"
      emit_raw "  %start_ptr = getelementptr i64, ptr %ovector, i32 %idx2\n"
      emit_raw "  %start64 = load i64, ptr %start_ptr\n"
      emit_raw "  %idx2p1 = add i32 %idx2, 1\n"
      emit_raw "  %end_ptr = getelementptr i64, ptr %ovector, i32 %idx2p1\n"
      emit_raw "  %end64 = load i64, ptr %end_ptr\n"
      # Check for unmatched group (PCRE2 uses PCRE2_UNSET = ~0)
      emit_raw "  %unset = icmp eq i64 %start64, -1\n"
      emit_raw "  br i1 %unset, label %ret_null, label %extract\n"
      emit_raw "extract:\n"
      emit_raw "  %str = load ptr, ptr @__crystal_v2_last_match_str\n"
      emit_raw "  %str_data = getelementptr i8, ptr %str, i32 12\n"
      emit_raw "  %start = trunc i64 %start64 to i32\n"
      emit_raw "  %end = trunc i64 %end64 to i32\n"
      emit_raw "  %len = sub i32 %end, %start\n"
      emit_raw "  %src = getelementptr i8, ptr %str_data, i32 %start\n"
      # Use existing create_substring helper (needs type_id for String)
      emit_raw "  %result = call ptr @__crystal_v2_create_substring(ptr %src, i32 %len, i32 #{@string_type_id})\n"
      emit_raw "  ret ptr %result\n"
      emit_raw "ret_null:\n"
      emit_raw "  ret ptr null\n"
      emit_raw "}\n\n"

      # __crystal_v2_string_gsub_regex(str: Crystal::String*, regex: ptr, repl: Crystal::String*) -> Crystal::String*
      # Simple gsub: replaces all non-overlapping matches with replacement string.
      emit_raw "define ptr @__crystal_v2_string_gsub_regex(ptr %str, ptr %regex, ptr %repl) {\n"
      emit_raw "entry:\n"
      emit_raw "  %re_null = icmp eq ptr %regex, null\n"
      emit_raw "  br i1 %re_null, label %ret_orig, label %check_str\n"
      emit_raw "check_str:\n"
      emit_raw "  %str_null = icmp eq ptr %str, null\n"
      emit_raw "  br i1 %str_null, label %ret_orig, label %init\n"
      emit_raw "init:\n"
      emit_raw "  %re_ptr = load ptr, ptr %regex\n"
      emit_raw "  %md_slot = getelementptr i8, ptr %regex, i32 8\n"
      emit_raw "  %md = load ptr, ptr %md_slot\n"
      emit_raw "  %str_bs_ptr = getelementptr i8, ptr %str, i32 4\n"
      emit_raw "  %str_bs = load i32, ptr %str_bs_ptr\n"
      emit_raw "  %str_data = getelementptr i8, ptr %str, i32 12\n"
      emit_raw "  %str_bs64 = sext i32 %str_bs to i64\n"
      emit_raw "  %repl_bs_ptr = getelementptr i8, ptr %repl, i32 4\n"
      emit_raw "  %repl_bs = load i32, ptr %repl_bs_ptr\n"
      emit_raw "  %repl_data = getelementptr i8, ptr %repl, i32 12\n"
      # Allocate output buffer (worst case: every char replaced)
      emit_raw "  %max_out = add i32 %str_bs, %str_bs\n"
      emit_raw "  %max_out2 = add i32 %max_out, %repl_bs\n"
      emit_raw "  %max_out3 = add i32 %max_out2, 256\n"
      emit_raw "  %max64 = sext i32 %max_out3 to i64\n"
      emit_raw "  %buf = call ptr @__crystal_v2_malloc64(i64 %max64)\n"
      emit_raw "  br label %loop\n"
      emit_raw "loop:\n"
      emit_raw "  %pos = phi i64 [0, %init], [%next_pos, %copy_rest_of_match]\n"
      emit_raw "  %out_pos = phi i32 [0, %init], [%new_out_pos, %copy_rest_of_match]\n"
      emit_raw "  %rc = call i32 @pcre2_match_8(ptr %re_ptr, ptr %str_data, i64 %str_bs64, i64 %pos, i32 0, ptr %md, ptr null)\n"
      emit_raw "  %has_match = icmp sge i32 %rc, 0\n"
      emit_raw "  br i1 %has_match, label %do_replace, label %finish\n"
      emit_raw "do_replace:\n"
      emit_raw "  %ovector = call ptr @pcre2_get_ovector_pointer_8(ptr %md)\n"
      emit_raw "  %m_start64 = load i64, ptr %ovector\n"
      emit_raw "  %m_end_ptr = getelementptr i64, ptr %ovector, i32 1\n"
      emit_raw "  %m_end64 = load i64, ptr %m_end_ptr\n"
      # Copy pre-match portion
      emit_raw "  %pre_len64 = sub i64 %m_start64, %pos\n"
      emit_raw "  %pre_len = trunc i64 %pre_len64 to i32\n"
      emit_raw "  %pos32 = trunc i64 %pos to i32\n"
      emit_raw "  %src_pre = getelementptr i8, ptr %str_data, i32 %pos32\n"
      emit_raw "  %dst_pre = getelementptr i8, ptr %buf, i32 %out_pos\n"
      emit_raw "  %pre64 = sext i32 %pre_len to i64\n"
      emit_raw "  call void @llvm.memcpy.p0.p0.i64(ptr %dst_pre, ptr %src_pre, i64 %pre64, i1 false)\n"
      emit_raw "  %out_after_pre = add i32 %out_pos, %pre_len\n"
      # Copy replacement
      emit_raw "  %dst_repl = getelementptr i8, ptr %buf, i32 %out_after_pre\n"
      emit_raw "  %repl64 = sext i32 %repl_bs to i64\n"
      emit_raw "  call void @llvm.memcpy.p0.p0.i64(ptr %dst_repl, ptr %repl_data, i64 %repl64, i1 false)\n"
      emit_raw "  %new_out_pos = add i32 %out_after_pre, %repl_bs\n"
      # Advance past match (ensure at least 1 byte progress to avoid infinite loop)
      emit_raw "  %match_len64 = sub i64 %m_end64, %m_start64\n"
      emit_raw "  %zero_len = icmp eq i64 %match_len64, 0\n"
      emit_raw "  %advance = select i1 %zero_len, i64 1, i64 0\n"
      emit_raw "  %next_pos = add i64 %m_end64, %advance\n"
      emit_raw "  br label %copy_rest_of_match\n"
      emit_raw "copy_rest_of_match:\n"
      emit_raw "  %past_end = icmp sge i64 %next_pos, %str_bs64\n"
      emit_raw "  br i1 %past_end, label %finish, label %loop\n"
      emit_raw "finish:\n"
      emit_raw "  %final_out = phi i32 [%out_pos, %loop], [%new_out_pos, %copy_rest_of_match]\n"
      emit_raw "  %final_pos = phi i64 [%pos, %loop], [%next_pos, %copy_rest_of_match]\n"
      # Copy remaining after last match
      emit_raw "  %remain64 = sub i64 %str_bs64, %final_pos\n"
      emit_raw "  %remain = trunc i64 %remain64 to i32\n"
      emit_raw "  %fp32 = trunc i64 %final_pos to i32\n"
      emit_raw "  %src_tail = getelementptr i8, ptr %str_data, i32 %fp32\n"
      emit_raw "  %dst_tail = getelementptr i8, ptr %buf, i32 %final_out\n"
      emit_raw "  %rem64 = sext i32 %remain to i64\n"
      emit_raw "  call void @llvm.memcpy.p0.p0.i64(ptr %dst_tail, ptr %src_tail, i64 %rem64, i1 false)\n"
      emit_raw "  %total_len = add i32 %final_out, %remain\n"
      emit_raw "  %result = call ptr @__crystal_v2_create_substring(ptr %buf, i32 %total_len, i32 #{@string_type_id})\n"
      emit_raw "  ret ptr %result\n"
      emit_raw "ret_orig:\n"
      emit_raw "  ret ptr %str\n"
      emit_raw "}\n\n"

      # __crystal_v2_regex_match(regex: ptr, str: Crystal::String*) -> ptr
      # Returns non-null pointer (the regex struct itself) on match, null on no match.
      # Stores match data globally for capture group access via __crystal_v2_regex_capture.
      emit_raw "define ptr @__crystal_v2_regex_match(ptr %regex, ptr %str) {\n"
      emit_raw "entry:\n"
      emit_raw "  %re_null = icmp eq ptr %regex, null\n"
      emit_raw "  br i1 %re_null, label %ret_null, label %check_str\n"
      emit_raw "check_str:\n"
      emit_raw "  %str_null = icmp eq ptr %str, null\n"
      emit_raw "  br i1 %str_null, label %ret_null, label %do_match\n"
      emit_raw "do_match:\n"
      emit_raw "  %re_ptr = load ptr, ptr %regex\n"
      emit_raw "  %md_slot = getelementptr i8, ptr %regex, i32 8\n"
      emit_raw "  %md = load ptr, ptr %md_slot\n"
      emit_raw "  %bs_ptr = getelementptr i8, ptr %str, i32 4\n"
      emit_raw "  %bytesize = load i32, ptr %bs_ptr\n"
      emit_raw "  %data = getelementptr i8, ptr %str, i32 12\n"
      emit_raw "  %bs64 = sext i32 %bytesize to i64\n"
      emit_raw "  %rc = call i32 @pcre2_match_8(ptr %re_ptr, ptr %data, i64 %bs64, i64 0, i32 0, ptr %md, ptr null)\n"
      emit_raw "  %matched = icmp sge i32 %rc, 0\n"
      emit_raw "  br i1 %matched, label %store_match, label %ret_null\n"
      emit_raw "store_match:\n"
      emit_raw "  store ptr %md, ptr @__crystal_v2_last_match_data\n"
      emit_raw "  store ptr %str, ptr @__crystal_v2_last_match_str\n"
      # Return the regex struct itself as a non-null truthy value
      emit_raw "  ret ptr %regex\n"
      emit_raw "ret_null:\n"
      emit_raw "  store ptr null, ptr @__crystal_v2_last_match_data\n"
      emit_raw "  ret ptr null\n"
      emit_raw "}\n\n"

    end

    # Emit varargs stubs for bare iterator methods that weren't resolved as intrinsics.
    # Called AFTER all regular functions are emitted so we can skip names that have
    # concrete definitions (avoids the LLVM define/define conflict in Stage 2).
    private def emit_bare_iterator_stubs
      BARE_ITERATOR_METHODS.each do |stub_name|
        unless @emitted_functions.includes?(stub_name)
          @emitted_functions << stub_name
          emit_raw "define void @#{stub_name}(...) {\n"
          emit_raw "  ret void\n"
          emit_raw "}\n\n"
        end
      end
    end

    private def emit_entrypoint_if_needed(functions_to_emit : Array(Function))
      has_user_main = functions_to_emit.any? { |f| f.name == "main" }
      has_crystal_main = functions_to_emit.any? { |f| f.name == "__crystal_main" }
      return if has_user_main || !has_crystal_main

      # Entry point: main() calls __crystal_main()
      emit_raw "; Program entry point\n"
      if ENV.has_key?("CRYSTAL_V2_DEBUG_MAIN")
        emit_raw "@.dbg_main_enter = private unnamed_addr constant [13 x i8] c\"[MAIN_ENTER]\\0A\\00\"\n"
        emit_raw "@.dbg_main_exit = private unnamed_addr constant [12 x i8] c\"[MAIN_EXIT]\\0A\\00\"\n"
        emit_raw "define i32 @main(i32 %argc, ptr %argv) {\n"
        emit_raw "  call i64 @write(i32 2, ptr @.dbg_main_enter, i64 12)\n"
        emit_raw "  call void @__crystal_main(i32 %argc, ptr %argv)\n"
        emit_raw "  call i64 @write(i32 2, ptr @.dbg_main_exit, i64 11)\n"
        emit_raw "  ret i32 0\n"
        emit_raw "}\n\n"
      else
        emit_raw "define i32 @main(i32 %argc, ptr %argv) {\n"
        emit_raw "  call void @__crystal_main(i32 %argc, ptr %argv)\n"
        emit_raw "  ret i32 0\n"
        emit_raw "}\n\n"
      end
    end

    # Intercept known-broken stdlib constructors whose bodies don't compile correctly.
    # Returns true if the function was handled (emitted as a runtime helper), false otherwise.
    private def emit_builtin_override(func : Function) : Bool
      mangled = mangle_function_name(func.name)
      case mangled
      when "String$Dnew$$Pointer$LUInt8$R"
        # String.new(chars : UInt8*) — calls strlen then delegates to 3-arg version.
        # The auto-allocator ignores the argument; this override implements the
        # stdlib's self.new(UInt8*) which copies data from the C string.
        emit_raw "; String.new(UInt8*) — runtime override (strlen + copy)\n"
        emit_raw "define ptr @#{mangled}(ptr %chars) {\n"
        emit_raw "entry:\n"
        emit_raw "  %is_null = icmp eq ptr %chars, null\n"
        emit_raw "  br i1 %is_null, label %ret_empty, label %do_strlen\n"
        emit_raw "ret_empty:\n"
        emit_raw "  ret ptr @.str.empty\n"
        emit_raw "do_strlen:\n"
        emit_raw "  %len64 = call i64 @strlen(ptr %chars)\n"
        emit_raw "  %len = trunc i64 %len64 to i32\n"
        emit_raw "  %result = call ptr @String$Dnew$$Pointer$LUInt8$R_Int32_Int32(ptr %chars, i32 %len, i32 %len)\n"
        emit_raw "  ret ptr %result\n"
        emit_raw "}\n\n"
        return true
      when "String$Dnew$$Pointer$LUInt8$R_Int32_Int32"
        # String.new(chars : UInt8*, bytesize : Int32, size : Int32)
        # Allocates a Crystal String, copies data from chars pointer.
        emit_raw "; String.new(UInt8*, Int32, Int32) — runtime override\n"
        emit_raw "define ptr @#{mangled}(ptr %chars, i32 %bytesize, i32 %size) {\n"
        emit_raw "entry:\n"
        emit_raw "  %is_empty = icmp eq i32 %bytesize, 0\n"
        emit_raw "  br i1 %is_empty, label %ret_empty, label %do_alloc\n"
        emit_raw "ret_empty:\n"
        emit_raw "  ret ptr @.str.empty\n"
        emit_raw "do_alloc:\n"
        emit_raw "  %alloc_i32 = add i32 %bytesize, 13\n"
        emit_raw "  %alloc = sext i32 %alloc_i32 to i64\n"
        emit_raw "  %str = call ptr @__crystal_v2_malloc64(i64 %alloc)\n"
        emit_raw "  store i32 #{@string_type_id}, ptr %str\n"
        emit_raw "  %bs_ptr = getelementptr i8, ptr %str, i32 4\n"
        emit_raw "  store i32 %bytesize, ptr %bs_ptr\n"
        emit_raw "  %sz_ptr = getelementptr i8, ptr %str, i32 8\n"
        emit_raw "  store i32 %size, ptr %sz_ptr\n"
        emit_raw "  %data_ptr = getelementptr i8, ptr %str, i32 12\n"
        emit_raw "  %len64 = sext i32 %bytesize to i64\n"
        emit_raw "  call void @llvm.memcpy.p0.p0.i64(ptr %data_ptr, ptr %chars, i64 %len64, i1 false)\n"
        emit_raw "  %null_pos = getelementptr i8, ptr %data_ptr, i32 %bytesize\n"
        emit_raw "  store i8 0, ptr %null_pos\n"
        emit_raw "  ret ptr %str\n"
        emit_raw "}\n\n"
        return true
      when "String$Hbyte_slice$$Int32_Int32"
        # String#byte_slice(start : Int32, count : Int32) : String
        # The stdlib uses String.new(capacity) { block } which we can't compile.
        # Override with direct implementation using our working String.new(UInt8*, Int32, Int32).
        emit_raw "; String#byte_slice(Int32, Int32) — runtime override\n"
        emit_raw "define ptr @#{mangled}(ptr %self, i32 %start, i32 %count) {\n"
        emit_raw "entry:\n"
        # Get bytesize
        emit_raw "  %bs_ptr = getelementptr i8, ptr %self, i32 4\n"
        emit_raw "  %bytesize = load i32, ptr %bs_ptr\n"
        # Handle negative start
        emit_raw "  %start_neg = icmp slt i32 %start, 0\n"
        emit_raw "  br i1 %start_neg, label %fix_start, label %check_bounds\n"
        emit_raw "fix_start:\n"
        emit_raw "  %fixed_start = add i32 %start, %bytesize\n"
        emit_raw "  br label %check_bounds\n"
        emit_raw "check_bounds:\n"
        emit_raw "  %real_start = phi i32 [%fixed_start, %fix_start], [%start, %entry]\n"
        # Clamp: if start < 0 or start >= bytesize, return empty
        emit_raw "  %start_lo = icmp slt i32 %real_start, 0\n"
        emit_raw "  br i1 %start_lo, label %ret_empty, label %check_hi\n"
        emit_raw "check_hi:\n"
        emit_raw "  %start_hi = icmp sge i32 %real_start, %bytesize\n"
        emit_raw "  br i1 %start_hi, label %ret_empty, label %clamp_count\n"
        # Clamp count to remaining bytes
        emit_raw "clamp_count:\n"
        emit_raw "  %remaining = sub i32 %bytesize, %real_start\n"
        emit_raw "  %count_too_big = icmp sgt i32 %count, %remaining\n"
        emit_raw "  %real_count = select i1 %count_too_big, i32 %remaining, i32 %count\n"
        # If count <= 0, return empty
        emit_raw "  %count_zero = icmp sle i32 %real_count, 0\n"
        emit_raw "  br i1 %count_zero, label %ret_empty, label %do_slice\n"
        emit_raw "ret_empty:\n"
        emit_raw "  ret ptr @.str.empty\n"
        emit_raw "do_slice:\n"
        # Get data pointer + start offset
        emit_raw "  %data_base = getelementptr i8, ptr %self, i32 12\n"
        emit_raw "  %src_ptr = getelementptr i8, ptr %data_base, i32 %real_start\n"
        # Call our working String.new(UInt8*, Int32, Int32)
        emit_raw "  %result = call ptr @String$Dnew$$Pointer$LUInt8$R_Int32_Int32(ptr %src_ptr, i32 %real_count, i32 %real_count)\n"
        emit_raw "  ret ptr %result\n"
        emit_raw "}\n\n"
        return true
      when "String$Hbyte_slice$Q$$Int32_Int32"
        # String#byte_slice?(start : Int32, count : Int32) : String?
        # Nilable version — returns nil instead of raising for out-of-bounds.
        emit_raw "; String#byte_slice?(Int32, Int32) — runtime override\n"
        emit_raw "define %Nil$_$OR$_String.union @#{mangled}(ptr %self, i32 %start, i32 %count) {\n"
        emit_raw "entry:\n"
        emit_raw "  %bs_ptr = getelementptr i8, ptr %self, i32 4\n"
        emit_raw "  %bytesize = load i32, ptr %bs_ptr\n"
        # Handle negative start
        emit_raw "  %start_neg = icmp slt i32 %start, 0\n"
        emit_raw "  br i1 %start_neg, label %fix_start, label %check_bounds\n"
        emit_raw "fix_start:\n"
        emit_raw "  %fixed_start = add i32 %start, %bytesize\n"
        emit_raw "  br label %check_bounds\n"
        emit_raw "check_bounds:\n"
        emit_raw "  %real_start = phi i32 [%fixed_start, %fix_start], [%start, %entry]\n"
        # Out-of-bounds → return nil
        emit_raw "  %start_lo = icmp slt i32 %real_start, 0\n"
        emit_raw "  br i1 %start_lo, label %ret_nil, label %check_hi\n"
        emit_raw "check_hi:\n"
        emit_raw "  %start_hi = icmp sgt i32 %real_start, %bytesize\n"
        emit_raw "  br i1 %start_hi, label %ret_nil, label %clamp_count\n"
        emit_raw "clamp_count:\n"
        emit_raw "  %remaining = sub i32 %bytesize, %real_start\n"
        emit_raw "  %count_too_big = icmp sgt i32 %count, %remaining\n"
        emit_raw "  %real_count = select i1 %count_too_big, i32 %remaining, i32 %count\n"
        emit_raw "  %count_zero = icmp sle i32 %real_count, 0\n"
        emit_raw "  br i1 %count_zero, label %ret_empty_str, label %do_slice\n"
        # Return empty string (wrapped in union)
        emit_raw "ret_empty_str:\n"
        emit_raw "  %ret_empty.ptr = alloca %Nil$_$OR$_String.union, align 8\n"
        emit_raw "  %ret_empty.tid = getelementptr %Nil$_$OR$_String.union, ptr %ret_empty.ptr, i32 0, i32 0\n"
        emit_raw "  store i32 1, ptr %ret_empty.tid\n"
        emit_raw "  %ret_empty.val = getelementptr %Nil$_$OR$_String.union, ptr %ret_empty.ptr, i32 0, i32 1\n"
        emit_raw "  store ptr @.str.empty, ptr %ret_empty.val, align 4\n"
        emit_raw "  %ret_empty_union = load %Nil$_$OR$_String.union, ptr %ret_empty.ptr\n"
        emit_raw "  ret %Nil$_$OR$_String.union %ret_empty_union\n"
        # Return nil
        emit_raw "ret_nil:\n"
        emit_raw "  %ret_nil.ptr = alloca %Nil$_$OR$_String.union, align 8\n"
        emit_raw "  %ret_nil.tid = getelementptr %Nil$_$OR$_String.union, ptr %ret_nil.ptr, i32 0, i32 0\n"
        emit_raw "  store i32 0, ptr %ret_nil.tid\n"
        emit_raw "  %ret_nil_union = load %Nil$_$OR$_String.union, ptr %ret_nil.ptr\n"
        emit_raw "  ret %Nil$_$OR$_String.union %ret_nil_union\n"
        emit_raw "do_slice:\n"
        emit_raw "  %data_base = getelementptr i8, ptr %self, i32 12\n"
        emit_raw "  %src_ptr = getelementptr i8, ptr %data_base, i32 %real_start\n"
        emit_raw "  %result = call ptr @String$Dnew$$Pointer$LUInt8$R_Int32_Int32(ptr %src_ptr, i32 %real_count, i32 %real_count)\n"
        emit_raw "  %ret.ptr = alloca %Nil$_$OR$_String.union, align 8\n"
        emit_raw "  %ret.tid = getelementptr %Nil$_$OR$_String.union, ptr %ret.ptr, i32 0, i32 0\n"
        emit_raw "  store i32 1, ptr %ret.tid\n"
        emit_raw "  %ret.val = getelementptr %Nil$_$OR$_String.union, ptr %ret.ptr, i32 0, i32 1\n"
        emit_raw "  store ptr %result, ptr %ret.val, align 4\n"
        emit_raw "  %ret_union = load %Nil$_$OR$_String.union, ptr %ret.ptr\n"
        emit_raw "  ret %Nil$_$OR$_String.union %ret_union\n"
        emit_raw "}\n\n"
        return true
      when "String$Hindex$$Char"
        # String#index(search : Char, offset : Int32 = 0) : Int32?
        # The stdlib calls Slice(UInt8)#fast_index which uses Indexable#size
        # (dead-code stub returning 0). Override with direct forward byte scan.
        union_type = "%Nil$_$OR$_Int32.union"
        emit_raw "; String#index(Char, Int32) — runtime override (forward byte scan)\n"
        emit_raw "define #{union_type} @#{mangled}(ptr %self, i32 %search, i32 %offset) {\n"
        emit_raw "entry:\n"
        emit_raw "  %bs_ptr = getelementptr i8, ptr %self, i32 4\n"
        emit_raw "  %bytesize = load i32, ptr %bs_ptr\n"
        # If offset >= bytesize or offset < 0, return nil
        emit_raw "  %neg = icmp slt i32 %offset, 0\n"
        emit_raw "  br i1 %neg, label %ret_nil, label %check_bounds\n"
        emit_raw "check_bounds:\n"
        emit_raw "  %oob = icmp sge i32 %offset, %bytesize\n"
        emit_raw "  br i1 %oob, label %ret_nil, label %scan\n"
        emit_raw "scan:\n"
        emit_raw "  %data = getelementptr i8, ptr %self, i32 12\n"
        emit_raw "  %byte = trunc i32 %search to i8\n"
        emit_raw "  br label %loop\n"
        emit_raw "loop:\n"
        emit_raw "  %i = phi i32 [%offset, %scan], [%next, %loop_next]\n"
        emit_raw "  %done = icmp sge i32 %i, %bytesize\n"
        emit_raw "  br i1 %done, label %ret_nil, label %check_byte\n"
        emit_raw "check_byte:\n"
        emit_raw "  %p = getelementptr i8, ptr %data, i32 %i\n"
        emit_raw "  %b = load i8, ptr %p\n"
        emit_raw "  %eq = icmp eq i8 %b, %byte\n"
        emit_raw "  br i1 %eq, label %found, label %loop_next\n"
        emit_raw "loop_next:\n"
        emit_raw "  %next = add i32 %i, 1\n"
        emit_raw "  br label %loop\n"
        emit_raw "found:\n"
        emit_raw "  %f_u = alloca #{union_type}, align 8\n"
        emit_raw "  store #{union_type} zeroinitializer, ptr %f_u\n"
        emit_raw "  %f_tid = getelementptr {i32, [8 x i8]}, ptr %f_u, i32 0, i32 0\n"
        emit_raw "  store i32 1, ptr %f_tid\n"
        emit_raw "  %f_pay = getelementptr {i32, [8 x i8]}, ptr %f_u, i32 0, i32 1\n"
        emit_raw "  store i32 %i, ptr %f_pay, align 4\n"
        emit_raw "  %f_val = load #{union_type}, ptr %f_u\n"
        emit_raw "  ret #{union_type} %f_val\n"
        emit_raw "ret_nil:\n"
        emit_raw "  %n_u = alloca #{union_type}, align 8\n"
        emit_raw "  store #{union_type} zeroinitializer, ptr %n_u\n"
        emit_raw "  %n_val = load #{union_type}, ptr %n_u\n"
        emit_raw "  ret #{union_type} %n_val\n"
        emit_raw "}\n\n"
        return true
      when "String$Hrindex$$Char"
        # String#rindex(search : Char, offset : Int32 = size - 1) : Int32?
        # The stdlib calls Slice(UInt8)#rindex which is a dead-code stub.
        # Override with direct backward byte scan for ASCII chars.
        # Always use 2-way union (Nil | Int32). The MIR return type may be
        # polluted by broken Slice#rindex (3-way with Pointer), but we
        # force the correct type here to match the HIR call-site type.
        union_type = "%Nil$_$OR$_Int32.union"
        emit_raw "; String#rindex(Char, Int32) — runtime override (backward byte scan)\n"
        emit_raw "define #{union_type} @#{mangled}(ptr %self, i32 %search, i32 %offset) {\n"
        emit_raw "entry:\n"
        emit_raw "  %bs_ptr = getelementptr i8, ptr %self, i32 4\n"
        emit_raw "  %bytesize = load i32, ptr %bs_ptr\n"
        # Clamp offset to bytesize - 1
        emit_raw "  %max = sub i32 %bytesize, 1\n"
        emit_raw "  %gt = icmp sgt i32 %offset, %max\n"
        emit_raw "  %off = select i1 %gt, i32 %max, i32 %offset\n"
        # If offset < 0, return nil
        emit_raw "  %neg = icmp slt i32 %off, 0\n"
        emit_raw "  br i1 %neg, label %ret_nil, label %scan\n"
        emit_raw "scan:\n"
        emit_raw "  %data = getelementptr i8, ptr %self, i32 12\n"
        emit_raw "  %byte = trunc i32 %search to i8\n"
        emit_raw "  br label %loop\n"
        emit_raw "loop:\n"
        emit_raw "  %i = phi i32 [%off, %scan], [%next, %loop_next]\n"
        emit_raw "  %i_neg = icmp slt i32 %i, 0\n"
        emit_raw "  br i1 %i_neg, label %ret_nil, label %check_byte\n"
        emit_raw "check_byte:\n"
        emit_raw "  %p = getelementptr i8, ptr %data, i32 %i\n"
        emit_raw "  %b = load i8, ptr %p\n"
        emit_raw "  %eq = icmp eq i8 %b, %byte\n"
        emit_raw "  br i1 %eq, label %found, label %loop_next\n"
        emit_raw "loop_next:\n"
        emit_raw "  %next = sub i32 %i, 1\n"
        emit_raw "  br label %loop\n"
        emit_raw "found:\n"
        # Return Int32 (tag=1, payload=i)
        emit_raw "  %f_u = alloca #{union_type}, align 8\n"
        emit_raw "  store #{union_type} zeroinitializer, ptr %f_u\n"
        emit_raw "  %f_tid = getelementptr {i32, [8 x i8]}, ptr %f_u, i32 0, i32 0\n"
        emit_raw "  store i32 1, ptr %f_tid\n"
        emit_raw "  %f_pay = getelementptr {i32, [8 x i8]}, ptr %f_u, i32 0, i32 1\n"
        emit_raw "  store i32 %i, ptr %f_pay, align 4\n"
        emit_raw "  %f_val = load #{union_type}, ptr %f_u\n"
        emit_raw "  ret #{union_type} %f_val\n"
        emit_raw "ret_nil:\n"
        emit_raw "  %n_u = alloca #{union_type}, align 8\n"
        emit_raw "  store #{union_type} zeroinitializer, ptr %n_u\n"
        emit_raw "  %n_val = load #{union_type}, ptr %n_u\n"
        emit_raw "  ret #{union_type} %n_val\n"
        emit_raw "}\n\n"
        return true
      when "Slice$LUInt8$R$Hrindex$$UInt8"
        # Slice(UInt8)#rindex(UInt8) — backward byte scan
        # The generic Slice#rindex instantiation is broken (dead-code stub returning void).
        # IO::FileDescriptor#write calls this to find newlines for flush_on_newline.
        union_type = "%Nil$_$OR$_Int32.union"
        emit_raw "; Slice(UInt8)#rindex(UInt8) — runtime override (backward byte scan)\n"
        # Params: ptr %self (Slice struct), i8 %value (search byte), ptr %offset_ptr (unused)
        emit_raw "define #{union_type} @#{mangled}(ptr %self, i8 %value, ptr %offset_ptr) {\n"
        emit_raw "entry:\n"
        # Load size from Slice offset 0
        emit_raw "  %size = load i32, ptr %self\n"
        emit_raw "  %empty = icmp sle i32 %size, 0\n"
        emit_raw "  br i1 %empty, label %ret_nil, label %load_ptr\n"
        emit_raw "load_ptr:\n"
        # Load buffer pointer from Slice offset 8
        emit_raw "  %buf_addr = getelementptr i8, ptr %self, i32 8\n"
        emit_raw "  %buf = load ptr, ptr %buf_addr\n"
        emit_raw "  %start = sub i32 %size, 1\n"
        emit_raw "  br label %loop\n"
        emit_raw "loop:\n"
        emit_raw "  %i = phi i32 [%start, %load_ptr], [%next, %loop_next]\n"
        emit_raw "  %neg = icmp slt i32 %i, 0\n"
        emit_raw "  br i1 %neg, label %ret_nil, label %check\n"
        emit_raw "check:\n"
        emit_raw "  %p = getelementptr i8, ptr %buf, i32 %i\n"
        emit_raw "  %b = load i8, ptr %p\n"
        emit_raw "  %eq = icmp eq i8 %b, %value\n"
        emit_raw "  br i1 %eq, label %found, label %loop_next\n"
        emit_raw "loop_next:\n"
        emit_raw "  %next = sub i32 %i, 1\n"
        emit_raw "  br label %loop\n"
        emit_raw "found:\n"
        emit_raw "  %f_u = alloca #{union_type}, align 8\n"
        emit_raw "  store #{union_type} zeroinitializer, ptr %f_u\n"
        emit_raw "  %f_tag = getelementptr {i32, [8 x i8]}, ptr %f_u, i32 0, i32 0\n"
        emit_raw "  store i32 1, ptr %f_tag\n"
        emit_raw "  %f_pay = getelementptr {i32, [8 x i8]}, ptr %f_u, i32 0, i32 1\n"
        emit_raw "  store i32 %i, ptr %f_pay, align 4\n"
        emit_raw "  %f_val = load #{union_type}, ptr %f_u\n"
        emit_raw "  ret #{union_type} %f_val\n"
        emit_raw "ret_nil:\n"
        emit_raw "  %n_u = alloca #{union_type}, align 8\n"
        emit_raw "  store #{union_type} zeroinitializer, ptr %n_u\n"
        emit_raw "  %n_val = load #{union_type}, ptr %n_u\n"
        emit_raw "  ret #{union_type} %n_val\n"
        emit_raw "}\n\n"
        return true
      end

      # Slice(UInt8)#[](Range) — runtime multiplex override
      # V2's method resolver sometimes passes raw Int32 indices via inttoptr when it
      # cannot determine arg type (VOID), selecting the Range overload instead of Int32.
      # This override detects at runtime whether the "range" arg is a valid heap pointer
      # (actual Range struct) or a small integer (raw index).
      #
      # Range path: reads @begin (i64) from offset 0, computes count = size - begin
      #   (open-ended range), delegates to Slice#[](Int32, Int32). Returns sub-slice ptr.
      # Int path: directly computes the address of the byte in the slice's data buffer
      #   and returns that pointer (callers do `load i8, ptr %result`).
      if mangled == "Slice$LUInt8$R$H$IDX$$Range"
        emit_raw "; Slice(UInt8)#[](Range) — runtime multiplex override\n"
        emit_raw "define ptr @#{mangled}(ptr %self, ptr %range) {\n"
        emit_raw "entry:\n"
        emit_raw "  %range_int = ptrtoint ptr %range to i64\n"
        # Heap pointers on arm64 macOS are > 0x10000. Raw indices are small.
        emit_raw "  %is_heap = icmp ugt i64 %range_int, 65536\n"
        emit_raw "  br i1 %is_heap, label %range_path, label %int_path\n"
        # Range path: treat as open-ended Range(Int64, Nil) — @begin is i64 at offset 0
        emit_raw "range_path:\n"
        emit_raw "  %size = call i32 @Slice$LUInt8$R$Hsize(ptr %self)\n"
        emit_raw "  %begin_ptr = getelementptr i8, ptr %range, i32 0\n"
        emit_raw "  %begin_i64 = load i64, ptr %begin_ptr\n"
        emit_raw "  %begin_val = trunc i64 %begin_i64 to i32\n"
        # Handle negative begin
        emit_raw "  %begin_neg = icmp slt i32 %begin_val, 0\n"
        emit_raw "  br i1 %begin_neg, label %fix_begin, label %begin_ok\n"
        emit_raw "fix_begin:\n"
        emit_raw "  %begin_fixed = add i32 %begin_val, %size\n"
        emit_raw "  br label %begin_ok\n"
        emit_raw "begin_ok:\n"
        emit_raw "  %start = phi i32 [%begin_val, %range_path], [%begin_fixed, %fix_begin]\n"
        # Open-ended range: count = size - start
        emit_raw "  %count = sub i32 %size, %start\n"
        emit_raw "  %count_neg = icmp slt i32 %count, 0\n"
        emit_raw "  %count_clamped = select i1 %count_neg, i32 0, i32 %count\n"
        emit_raw "  %range_result = call ptr @Slice$LUInt8$R$H$IDX$$Int32_Int32(ptr %self, i32 %start, i32 %count_clamped)\n"
        emit_raw "  ret ptr %range_result\n"
        # Int path: raw Int32 index — return pointer to byte in slice's data buffer.
        # Callers that mistakenly route here do `load i8, ptr %result`, so we must
        # return a real pointer to the byte, not inttoptr(byte_value).
        # Slice layout: offset 0 = size (i32), offset 8 = data ptr (ptr)
        emit_raw "int_path:\n"
        emit_raw "  %index = trunc i64 %range_int to i32\n"
        # Handle negative index
        emit_raw "  %idx_neg = icmp slt i32 %index, 0\n"
        emit_raw "  br i1 %idx_neg, label %fix_idx, label %idx_ok\n"
        emit_raw "fix_idx:\n"
        emit_raw "  %int_size = call i32 @Slice$LUInt8$R$Hsize(ptr %self)\n"
        emit_raw "  %idx_fixed = add i32 %index, %int_size\n"
        emit_raw "  br label %idx_ok\n"
        emit_raw "idx_ok:\n"
        emit_raw "  %final_idx = phi i32 [%index, %int_path], [%idx_fixed, %fix_idx]\n"
        # Load data pointer from Slice struct (offset 8)
        emit_raw "  %data_addr = getelementptr i8, ptr %self, i32 8\n"
        emit_raw "  %data_ptr = load ptr, ptr %data_addr\n"
        # GEP to the indexed byte and return pointer to it
        emit_raw "  %idx_ext = sext i32 %final_idx to i64\n"
        emit_raw "  %byte_addr = getelementptr i8, ptr %data_ptr, i64 %idx_ext\n"
        emit_raw "  ret ptr %byte_addr\n"
        emit_raw "}\n\n"
        return true
      end

      # Arena::Index#valid? — null check for uninitialized __evloop_data
      if mangled == "Crystal$CCEventLoop$CCPolling$CCArena$CCIndex$Hvalid$Q"
        emit_raw "; Arena::Index#valid? — with null self guard\n"
        emit_raw "define i1 @#{mangled}(ptr %self) {\n"
        emit_raw "entry:\n"
        emit_raw "  %is_null = icmp eq ptr %self, null\n"
        emit_raw "  br i1 %is_null, label %ret_false, label %check\n"
        emit_raw "check:\n"
        emit_raw "  %val = load i64, ptr %self\n"
        emit_raw "  %valid = icmp sge i64 %val, 0\n"
        emit_raw "  ret i1 %valid\n"
        emit_raw "ret_false:\n"
        emit_raw "  ret i1 0\n"
        emit_raw "}\n\n"
        return true
      end

      # Crystal::System::File.open — case/in on Errno | Tuple(Handle, Bool) doesn't compile.
      # Override with direct POSIX open() call returning {i32 fd, i1 close_on_finalize}.
      if mangled.starts_with?("Crystal$CCSystem$CCFile$Dopen$$String_String_")
        # Build param string from MIR params using simple p0, p1, ... names
        params_str = func.params.map_with_index { |p, i|
          llvm_t = @type_mapper.llvm_type(p.type)
          llvm_t = "ptr" if llvm_t == "void"
          "#{llvm_t} %p#{i}"
        }.join(", ")

        emit_raw "; Crystal::System::File.open — builtin override (direct POSIX open)\n"
        emit_raw "define ptr @#{mangled}(#{params_str}) {\n"
        emit_raw "entry:\n"
        # p0 = filename (Crystal String), p1 = mode (Crystal String)
        # Get filename C string: Crystal String data starts at offset 12
        emit_raw "  %cstr = getelementptr i8, ptr %p0, i32 12\n"
        # Get open flags from mode string
        emit_raw "  %flags = call i32 @__crystal_v2_mode_to_open_flags(ptr %p1)\n"
        # Call POSIX open(path, flags, 0666)
        emit_raw "  %fd = call i32 (ptr, i32, ...) @open(ptr %cstr, i32 %flags, i32 438)\n"
        emit_raw "  %fd_neg = icmp slt i32 %fd, 0\n"
        emit_raw "  br i1 %fd_neg, label %error, label %success\n"
        emit_raw "error:\n"
        emit_raw "  call void @perror(ptr @.str.dbg_open_label)\n"
        emit_raw "  call void @__crystal_v2_raise_msg(ptr @.str.file_open_error)\n"
        emit_raw "  unreachable\n"
        emit_raw "success:\n"
        # Allocate tuple {i32 fd, i1 close_on_finalize}
        emit_raw "  %tup = call ptr @__crystal_v2_malloc64(i64 8)\n"
        emit_raw "  store i32 %fd, ptr %tup\n"
        emit_raw "  %cof_ptr = getelementptr i8, ptr %tup, i32 4\n"
        emit_raw "  store i1 1, ptr %cof_ptr\n"
        emit_raw "  ret ptr %tup\n"
        emit_raw "}\n\n"
        return true
      end

      # IO::FileDescriptor#read(Slice(UInt8)) — bypass broken buffered IO + event loop stack.
      # Direct read() syscall using fd from the object and buffer from the Slice.
      if mangled == "IO$CCFileDescriptor$Hread$$Slice$LUInt8$R"
        emit_raw "; IO::FileDescriptor#read(Slice(UInt8)) — direct syscall override\n"
        emit_raw "define i32 @#{mangled}(ptr %self, ptr %slice) {\n"
        emit_raw "entry:\n"
        # Load fd: self.@fd is at offset 72 (stored as boxed ptr → i32)
        emit_raw "  %fd_box_ptr = getelementptr i8, ptr %self, i32 72\n"
        emit_raw "  %fd_box = load ptr, ptr %fd_box_ptr\n"
        emit_raw "  %fd = load i32, ptr %fd_box\n"
        # Load slice.@size from offset 0
        emit_raw "  %size_ptr = getelementptr i8, ptr %slice, i32 0\n"
        emit_raw "  %size = load i32, ptr %size_ptr\n"
        # Early return if size == 0
        emit_raw "  %is_zero = icmp eq i32 %size, 0\n"
        emit_raw "  br i1 %is_zero, label %ret_zero, label %do_read\n"
        emit_raw "ret_zero:\n"
        emit_raw "  ret i32 0\n"
        emit_raw "do_read:\n"
        # Load slice.@pointer from offset 8
        emit_raw "  %buf_ptr_ptr = getelementptr i8, ptr %slice, i32 8\n"
        emit_raw "  %buf_ptr = load ptr, ptr %buf_ptr_ptr\n"
        # Call read(fd, buf, size) — returns i64 on macOS
        emit_raw "  %nbytes = call i64 @read(i32 %fd, ptr %buf_ptr, i32 %size)\n"
        # If read returned error (-1), return 0
        emit_raw "  %is_err = icmp slt i64 %nbytes, 0\n"
        emit_raw "  br i1 %is_err, label %ret_zero, label %ret_ok\n"
        emit_raw "ret_ok:\n"
        emit_raw "  %result = trunc i64 %nbytes to i32\n"
        emit_raw "  ret i32 %result\n"
        emit_raw "}\n\n"
        return true
      end

      # IO::FileDescriptor#gets(Char, Bool) — bypass broken buffered IO.
      # Read byte-by-byte using direct syscall until delimiter found.
      # Returns Nil | String union.
      if mangled == "IO$CCFileDescriptor$Hgets$$Char_Bool" ||
         mangled == "File$Hgets$$Char_Bool"
        emit_raw "; #{mangled} — direct syscall gets override\n"
        emit_raw "define %Nil$_$OR$_String.union @#{mangled}(ptr %self, i32 %delimiter, i1 %chomp) {\n"
        emit_raw "entry:\n"
        # Load fd via double-deref at offset 72
        emit_raw "  %fd_box_ptr = getelementptr i8, ptr %self, i32 72\n"
        emit_raw "  %fd_box = load ptr, ptr %fd_box_ptr\n"
        emit_raw "  %fd = load i32, ptr %fd_box\n"
        # Allocate buffer (4096 bytes) and a 1-byte read buffer
        emit_raw "  %buf = call ptr @__crystal_v2_malloc64(i64 4096)\n"
        emit_raw "  %byte_buf = alloca i8\n"
        emit_raw "  %delim_byte = trunc i32 %delimiter to i8\n"
        emit_raw "  br label %loop\n"
        emit_raw "loop:\n"
        emit_raw "  %pos = phi i32 [0, %entry], [%next_pos, %store_byte]\n"
        # Read one byte
        emit_raw "  %nbytes = call i64 @read(i32 %fd, ptr %byte_buf, i32 1)\n"
        emit_raw "  %is_eof = icmp sle i64 %nbytes, 0\n"
        emit_raw "  br i1 %is_eof, label %eof, label %got_byte\n"
        emit_raw "got_byte:\n"
        emit_raw "  %byte = load i8, ptr %byte_buf\n"
        emit_raw "  %is_delim = icmp eq i8 %byte, %delim_byte\n"
        emit_raw "  br i1 %is_delim, label %found_delim, label %store_byte\n"
        emit_raw "store_byte:\n"
        # Store byte in buffer (bounds check: stop at 4095)
        emit_raw "  %buf_slot = getelementptr i8, ptr %buf, i32 %pos\n"
        emit_raw "  store i8 %byte, ptr %buf_slot\n"
        emit_raw "  %next_pos = add i32 %pos, 1\n"
        emit_raw "  %at_limit = icmp sge i32 %next_pos, 4095\n"
        emit_raw "  br i1 %at_limit, label %build_str, label %loop\n"
        emit_raw "found_delim:\n"
        # If chomp, don't include delimiter. If !chomp, include it.
        emit_raw "  br i1 %chomp, label %build_str, label %incl_delim\n"
        emit_raw "incl_delim:\n"
        emit_raw "  %slot2 = getelementptr i8, ptr %buf, i32 %pos\n"
        emit_raw "  store i8 %byte, ptr %slot2\n"
        emit_raw "  %pos_d = add i32 %pos, 1\n"
        emit_raw "  %str2 = call ptr @String$Dnew$$Pointer$LUInt8$R_Int32_Int32(ptr %buf, i32 %pos_d, i32 %pos_d)\n"
        emit_raw "  %u2 = alloca %Nil$_$OR$_String.union, align 8\n"
        emit_raw "  %u2tid = getelementptr %Nil$_$OR$_String.union, ptr %u2, i32 0, i32 0\n"
        emit_raw "  store i32 1, ptr %u2tid\n"
        emit_raw "  %u2pay = getelementptr %Nil$_$OR$_String.union, ptr %u2, i32 0, i32 1\n"
        emit_raw "  store ptr %str2, ptr %u2pay, align 4\n"
        emit_raw "  %rv2 = load %Nil$_$OR$_String.union, ptr %u2\n"
        emit_raw "  ret %Nil$_$OR$_String.union %rv2\n"
        emit_raw "eof:\n"
        # If we read 0 bytes total, return nil
        emit_raw "  %has_data = icmp sgt i32 %pos, 0\n"
        emit_raw "  br i1 %has_data, label %build_str, label %do_nil\n"
        emit_raw "do_nil:\n"
        emit_raw "  %un = alloca %Nil$_$OR$_String.union, align 8\n"
        emit_raw "  %untid = getelementptr %Nil$_$OR$_String.union, ptr %un, i32 0, i32 0\n"
        emit_raw "  store i32 0, ptr %untid\n"
        emit_raw "  %rvn = load %Nil$_$OR$_String.union, ptr %un\n"
        emit_raw "  ret %Nil$_$OR$_String.union %rvn\n"
        emit_raw "build_str:\n"
        # Create Crystal String from buffer using String.new(UInt8*, Int32, Int32)
        emit_raw "  %str1 = call ptr @String$Dnew$$Pointer$LUInt8$R_Int32_Int32(ptr %buf, i32 %pos, i32 %pos)\n"
        emit_raw "  %u1 = alloca %Nil$_$OR$_String.union, align 8\n"
        emit_raw "  %u1tid = getelementptr %Nil$_$OR$_String.union, ptr %u1, i32 0, i32 0\n"
        emit_raw "  store i32 1, ptr %u1tid\n"
        emit_raw "  %u1pay = getelementptr %Nil$_$OR$_String.union, ptr %u1, i32 0, i32 1\n"
        emit_raw "  store ptr %str1, ptr %u1pay, align 4\n"
        emit_raw "  %rv1 = load %Nil$_$OR$_String.union, ptr %u1\n"
        emit_raw "  ret %Nil$_$OR$_String.union %rv1\n"
        emit_raw "}\n\n"
        return true
      end

      # Hash#key_hash for primitive Int32 keys — bypass broken Object#hash vdispatch.
      # Primitives get inttoptr'd to ptr but vdispatch tries to load type_id from the raw address → crash.
      # Fix: compute hash directly using Crystal::Hasher.permute(sext(key, i64)).result.
      if mangled.ends_with?("$Hkey_hash$$Int32") && mangled.includes?("Hash$L")
        emit_raw "; #{mangled} — direct Int32 hash override (bypass vdispatch)\n"
        emit_raw "define i32 @#{mangled}(ptr %self, i32 %key) {\n"
        emit_raw "entry:\n"
        emit_raw "  %hasher = call ptr @Crystal$CCHasher$Dnew(i64 0, i64 0)\n"
        emit_raw "  %key64 = sext i32 %key to i64\n"
        emit_raw "  %hasher2 = call ptr @Crystal$CCHasher$Hpermute$$UInt64(ptr %hasher, i64 %key64)\n"
        emit_raw "  %hash64 = call i64 @Crystal$CCHasher$Hresult(ptr %hasher2)\n"
        emit_raw "  %hash32 = trunc i64 %hash64 to i32\n"
        emit_raw "  %is_zero = icmp eq i32 %hash32, 0\n"
        emit_raw "  br i1 %is_zero, label %ret_max, label %ret_hash\n"
        emit_raw "ret_max:\n"
        emit_raw "  ret i32 -1\n"  # UInt32::MAX = 0xFFFFFFFF = -1 in i32
        emit_raw "ret_hash:\n"
        emit_raw "  ret i32 %hash32\n"
        emit_raw "}\n\n"
        return true
      end

      # Hash#key_hash for other integer key types — same vdispatch bypass.
      # UInt64 keys: already i64, no sign-extend needed.
      if mangled.ends_with?("$Hkey_hash$$UInt64") && mangled.includes?("Hash$L")
        emit_raw "; #{mangled} — direct UInt64 hash override (bypass vdispatch)\n"
        emit_raw "define i32 @#{mangled}(ptr %self, i64 %key) {\n"
        emit_raw "entry:\n"
        emit_raw "  %hasher = call ptr @Crystal$CCHasher$Dnew(i64 0, i64 0)\n"
        emit_raw "  %hasher2 = call ptr @Crystal$CCHasher$Hpermute$$UInt64(ptr %hasher, i64 %key)\n"
        emit_raw "  %hash64 = call i64 @Crystal$CCHasher$Hresult(ptr %hasher2)\n"
        emit_raw "  %hash32 = trunc i64 %hash64 to i32\n"
        emit_raw "  %is_zero = icmp eq i32 %hash32, 0\n"
        emit_raw "  br i1 %is_zero, label %ret_max, label %ret_hash\n"
        emit_raw "ret_max:\n"
        emit_raw "  ret i32 -1\n"
        emit_raw "ret_hash:\n"
        emit_raw "  ret i32 %hash32\n"
        emit_raw "}\n\n"
        return true
      end

      # Int64 keys: already i64.
      if mangled.ends_with?("$Hkey_hash$$Int64") && mangled.includes?("Hash$L")
        emit_raw "; #{mangled} — direct Int64 hash override (bypass vdispatch)\n"
        emit_raw "define i32 @#{mangled}(ptr %self, i64 %key) {\n"
        emit_raw "entry:\n"
        emit_raw "  %hasher = call ptr @Crystal$CCHasher$Dnew(i64 0, i64 0)\n"
        emit_raw "  %hasher2 = call ptr @Crystal$CCHasher$Hpermute$$UInt64(ptr %hasher, i64 %key)\n"
        emit_raw "  %hash64 = call i64 @Crystal$CCHasher$Hresult(ptr %hasher2)\n"
        emit_raw "  %hash32 = trunc i64 %hash64 to i32\n"
        emit_raw "  %is_zero = icmp eq i32 %hash32, 0\n"
        emit_raw "  br i1 %is_zero, label %ret_max, label %ret_hash\n"
        emit_raw "ret_max:\n"
        emit_raw "  ret i32 -1\n"
        emit_raw "ret_hash:\n"
        emit_raw "  ret i32 %hash32\n"
        emit_raw "}\n\n"
        return true
      end

      # UInt32 keys: zext to i64.
      if mangled.ends_with?("$Hkey_hash$$UInt32") && mangled.includes?("Hash$L")
        emit_raw "; #{mangled} — direct UInt32 hash override (bypass vdispatch)\n"
        emit_raw "define i32 @#{mangled}(ptr %self, i32 %key) {\n"
        emit_raw "entry:\n"
        emit_raw "  %hasher = call ptr @Crystal$CCHasher$Dnew(i64 0, i64 0)\n"
        emit_raw "  %key64 = zext i32 %key to i64\n"
        emit_raw "  %hasher2 = call ptr @Crystal$CCHasher$Hpermute$$UInt64(ptr %hasher, i64 %key64)\n"
        emit_raw "  %hash64 = call i64 @Crystal$CCHasher$Hresult(ptr %hasher2)\n"
        emit_raw "  %hash32 = trunc i64 %hash64 to i32\n"
        emit_raw "  %is_zero = icmp eq i32 %hash32, 0\n"
        emit_raw "  br i1 %is_zero, label %ret_max, label %ret_hash\n"
        emit_raw "ret_max:\n"
        emit_raw "  ret i32 -1\n"
        emit_raw "ret_hash:\n"
        emit_raw "  ret i32 %hash32\n"
        emit_raw "}\n\n"
        return true
      end

      # Symbol keys: symbols are i32 IDs.
      if mangled.ends_with?("$Hkey_hash$$Symbol") && mangled.includes?("Hash$L")
        emit_raw "; #{mangled} — direct Symbol hash override (bypass vdispatch)\n"
        emit_raw "define i32 @#{mangled}(ptr %self, i32 %key) {\n"
        emit_raw "entry:\n"
        emit_raw "  %hasher = call ptr @Crystal$CCHasher$Dnew(i64 0, i64 0)\n"
        emit_raw "  %key64 = sext i32 %key to i64\n"
        emit_raw "  %hasher2 = call ptr @Crystal$CCHasher$Hpermute$$UInt64(ptr %hasher, i64 %key64)\n"
        emit_raw "  %hash64 = call i64 @Crystal$CCHasher$Hresult(ptr %hasher2)\n"
        emit_raw "  %hash32 = trunc i64 %hash64 to i32\n"
        emit_raw "  %is_zero = icmp eq i32 %hash32, 0\n"
        emit_raw "  br i1 %is_zero, label %ret_max, label %ret_hash\n"
        emit_raw "ret_max:\n"
        emit_raw "  ret i32 -1\n"
        emit_raw "ret_hash:\n"
        emit_raw "  ret i32 %hash32\n"
        emit_raw "}\n\n"
        return true
      end

      # Hash#key_hash for String keys — bypass broken Object#hash vdispatch for String.
      # String#hash calls hasher.string(self) which uses @bytesize and raw bytes.
      # Override: directly call String's hash infrastructure.
      if mangled.ends_with?("$Hkey_hash$$String") && mangled.includes?("Hash$L")
        emit_raw "; #{mangled} — direct String hash override (bypass vdispatch)\n"
        emit_raw "define i32 @#{mangled}(ptr %self, ptr %key) {\n"
        emit_raw "entry:\n"
        emit_raw "  %hasher = call ptr @Crystal$CCHasher$Dnew(i64 0, i64 0)\n"
        # String hash: read bytesize from offset 4, get data ptr at offset 12
        # Then hash each byte (like Crystal::Hasher#bytes does)
        emit_raw "  %bs_ptr = getelementptr i8, ptr %key, i32 4\n"
        emit_raw "  %bytesize = load i32, ptr %bs_ptr\n"
        emit_raw "  %data = getelementptr i8, ptr %key, i32 12\n"
        emit_raw "  %bs64 = sext i32 %bytesize to i64\n"
        # Call Crystal::Hasher#bytes(ptr, size) if available, otherwise do permute with bytes
        # Simple approach: hash the bytesize and first 8 bytes as i64 chunks
        emit_raw "  br label %hash_loop\n"
        emit_raw "hash_loop:\n"
        emit_raw "  %i = phi i64 [0, %entry], [%i_next, %hash_continue]\n"
        emit_raw "  %h = phi ptr [%hasher, %entry], [%h_next, %hash_continue]\n"
        emit_raw "  %done = icmp sge i64 %i, %bs64\n"
        emit_raw "  br i1 %done, label %hash_finish, label %hash_body\n"
        emit_raw "hash_body:\n"
        emit_raw "  %remaining = sub i64 %bs64, %i\n"
        emit_raw "  %byte_ptr = getelementptr i8, ptr %data, i64 %i\n"
        emit_raw "  %can_read_8 = icmp sge i64 %remaining, 8\n"
        emit_raw "  br i1 %can_read_8, label %read8, label %read1\n"
        emit_raw "read8:\n"
        emit_raw "  %chunk64 = load i64, ptr %byte_ptr\n"
        emit_raw "  %h8 = call ptr @Crystal$CCHasher$Hpermute$$UInt64(ptr %h, i64 %chunk64)\n"
        emit_raw "  %i8_next = add i64 %i, 8\n"
        emit_raw "  br label %hash_continue\n"
        emit_raw "read1:\n"
        emit_raw "  %byte = load i8, ptr %byte_ptr\n"
        emit_raw "  %byte64 = zext i8 %byte to i64\n"
        emit_raw "  %h1 = call ptr @Crystal$CCHasher$Hpermute$$UInt64(ptr %h, i64 %byte64)\n"
        emit_raw "  %i1_next = add i64 %i, 1\n"
        emit_raw "  br label %hash_continue\n"
        emit_raw "hash_continue:\n"
        emit_raw "  %h_next = phi ptr [%h8, %read8], [%h1, %read1]\n"
        emit_raw "  %i_next = phi i64 [%i8_next, %read8], [%i1_next, %read1]\n"
        emit_raw "  br label %hash_loop\n"
        emit_raw "hash_finish:\n"
        # Also permute with the length for better distribution
        emit_raw "  %h_len = call ptr @Crystal$CCHasher$Hpermute$$UInt64(ptr %h, i64 %bs64)\n"
        emit_raw "  %hash64 = call i64 @Crystal$CCHasher$Hresult(ptr %h_len)\n"
        emit_raw "  %hash32 = trunc i64 %hash64 to i32\n"
        emit_raw "  %is_zero = icmp eq i32 %hash32, 0\n"
        emit_raw "  br i1 %is_zero, label %ret_max, label %ret_hash\n"
        emit_raw "ret_max:\n"
        emit_raw "  ret i32 -1\n"
        emit_raw "ret_hash:\n"
        emit_raw "  ret i32 %hash32\n"
        emit_raw "}\n\n"
        return true
      end

      # Hash#key_hash for HIR::TypeRef keys — struct with single id : UInt32 at offset 0.
      # Structs are heap-allocated and passed by pointer; vdispatch fails because structs lack type_id.
      # Fix: load the id field and hash it as UInt32.
      if mangled.ends_with?("$Hkey_hash$$Crystal$CCHIR$CCTypeRef") && mangled.includes?("Hash$L")
        emit_raw "; #{mangled} — direct HIR::TypeRef hash override (bypass vdispatch)\n"
        emit_raw "define i32 @#{mangled}(ptr %self, ptr %key) {\n"
        emit_raw "entry:\n"
        emit_raw "  %hasher = call ptr @Crystal$CCHasher$Dnew(i64 0, i64 0)\n"
        emit_raw "  %id = load i32, ptr %key\n"
        emit_raw "  %id64 = zext i32 %id to i64\n"
        emit_raw "  %hasher2 = call ptr @Crystal$CCHasher$Hpermute$$UInt64(ptr %hasher, i64 %id64)\n"
        emit_raw "  %hash64 = call i64 @Crystal$CCHasher$Hresult(ptr %hasher2)\n"
        emit_raw "  %hash32 = trunc i64 %hash64 to i32\n"
        emit_raw "  %is_zero = icmp eq i32 %hash32, 0\n"
        emit_raw "  br i1 %is_zero, label %ret_max, label %ret_hash\n"
        emit_raw "ret_max:\n"
        emit_raw "  ret i32 -1\n"
        emit_raw "ret_hash:\n"
        emit_raw "  ret i32 %hash32\n"
        emit_raw "}\n\n"
        return true
      end

      # Hash#key_hash for MIR::TypeRef keys — same struct layout as HIR::TypeRef.
      if mangled.ends_with?("$Hkey_hash$$Crystal$CCMIR$CCTypeRef") && mangled.includes?("Hash$L")
        emit_raw "; #{mangled} — direct MIR::TypeRef hash override (bypass vdispatch)\n"
        emit_raw "define i32 @#{mangled}(ptr %self, ptr %key) {\n"
        emit_raw "entry:\n"
        emit_raw "  %hasher = call ptr @Crystal$CCHasher$Dnew(i64 0, i64 0)\n"
        emit_raw "  %id = load i32, ptr %key\n"
        emit_raw "  %id64 = zext i32 %id to i64\n"
        emit_raw "  %hasher2 = call ptr @Crystal$CCHasher$Hpermute$$UInt64(ptr %hasher, i64 %id64)\n"
        emit_raw "  %hash64 = call i64 @Crystal$CCHasher$Hresult(ptr %hasher2)\n"
        emit_raw "  %hash32 = trunc i64 %hash64 to i32\n"
        emit_raw "  %is_zero = icmp eq i32 %hash32, 0\n"
        emit_raw "  br i1 %is_zero, label %ret_max, label %ret_hash\n"
        emit_raw "ret_max:\n"
        emit_raw "  ret i32 -1\n"
        emit_raw "ret_hash:\n"
        emit_raw "  ret i32 %hash32\n"
        emit_raw "}\n\n"
        return true
      end

      # Hash::Entry#deleted? — pointer-slot-safe semantics.
      # In our current ABI Hash entry slots can be zeroed pointers after clear/delete.
      # Treat null entry pointer as deleted and otherwise read @hash by computed offset.
      if mangled.includes?("Hash$CCEntry$L") && mangled.ends_with?("$Hdeleted$Q")
        entry_prefix = mangled.sub("$Hdeleted$Q", "")
        inner = entry_prefix.sub("Hash$CCEntry$L", "")
        inner = inner.ends_with?("$R") ? inner[0...-2] : inner
        sep_idx = inner.index("$C$_")
        key_suffix = sep_idx ? inner[0...sep_idx] : ""
        value_suffix = sep_idx ? inner[(sep_idx + 4)..] : ""

        key_llvm_type = case key_suffix
                        when "String" then "ptr"
                        when "Int8", "UInt8", "Bool" then "i8"
                        when "Int16", "UInt16" then "i16"
                        when "Int32", "UInt32", "Char" then "i32"
                        when "Int64", "UInt64" then "i64"
                        when "Float32" then "float"
                        when "Float64" then "double"
                        else "ptr"
                        end

        value_llvm_type = case value_suffix
                          when "String" then "ptr"
                          when "Int8", "UInt8", "Bool" then "i8"
                          when "Int16", "UInt16" then "i16"
                          when "Int32", "UInt32", "Char" then "i32"
                          when "Int64", "UInt64" then "i64"
                          when "Float32" then "float"
                          when "Float64" then "double"
                          else "ptr"
                          end

        key_size, _key_align = case key_llvm_type
                               when "i1", "i8" then {1, 1}
                               when "i16" then {2, 2}
                               when "i32", "float" then {4, 4}
                               when "i64", "double", "ptr" then {8, 8}
                               else {8, 8}
                               end
        value_size, value_align = case value_llvm_type
                                  when "i1", "i8" then {1, 1}
                                  when "i16" then {2, 2}
                                  when "i32", "float" then {4, 4}
                                  when "i64", "double", "ptr" then {8, 8}
                                  else {8, 8}
                                  end
        value_offset = (key_size + value_align - 1) & ~(value_align - 1)
        hash_offset = (value_offset + value_size + 3) & ~3

        emit_raw "; #{mangled} — null-safe deleted? override\n"
        emit_raw "define i1 @#{mangled}(ptr %self) {\n"
        emit_raw "entry:\n"
        emit_raw "  %is_null = icmp eq ptr %self, null\n"
        emit_raw "  br i1 %is_null, label %ret_true, label %check_hash\n"
        emit_raw "check_hash:\n"
        emit_raw "  %hash_ptr = getelementptr i8, ptr %self, i32 #{hash_offset}\n"
        emit_raw "  %h = load i32, ptr %hash_ptr\n"
        emit_raw "  %is_deleted = icmp eq i32 %h, 0\n"
        emit_raw "  ret i1 %is_deleted\n"
        emit_raw "ret_true:\n"
        emit_raw "  ret i1 true\n"
        emit_raw "}\n\n"
        return true
      end

      # Hash#get_entry — fix stride and dereference pointer.
      # Our compiler heap-allocates Entry structs and stores pointers in the entries array.
      # malloc_entries allocates with stride 8 (ptr size), but get_entry uses sizeof(Entry body).
      # Fix: use stride 8 and load the stored pointer.
      if mangled.includes?("$Hget_index$$Int32") && mangled.includes?("Hash$L")
        # Root-cause fix: disable layout-hardcoded Hash override.
        # This path assumes pointer-based entry storage and injects custom
        # bounds/null semantics that diverge from stdlib value-layout Hash::Entry.
        # Fall back to regular lowering.
        return false

        emit_raw "; #{mangled} — index decode + null-slot validation override\n"
        emit_raw "define i32 @#{mangled}(ptr %self, i32 %index) {\n"
        emit_raw "entry:\n"
        emit_raw "  %indices_ptr_addr = getelementptr i8, ptr %self, i32 16\n"
        emit_raw "  %indices = load ptr, ptr %indices_ptr_addr\n"
        emit_raw "  %indices_is_null = icmp eq ptr %indices, null\n"
        emit_raw "  br i1 %indices_is_null, label %ret_empty, label %check_index_nonneg\n"
        emit_raw "check_index_nonneg:\n"
        emit_raw "  %index_neg = icmp slt i32 %index, 0\n"
        emit_raw "  br i1 %index_neg, label %ret_empty, label %check_index_bound\n"
        emit_raw "check_index_bound:\n"
        emit_raw "  %pow2_ptr = getelementptr i8, ptr %self, i32 33\n"
        emit_raw "  %pow2 = load i8, ptr %pow2_ptr\n"
        emit_raw "  %pow2_i32 = zext i8 %pow2 to i32\n"
        emit_raw "  %indices_size = shl i32 1, %pow2_i32\n"
        emit_raw "  %index_oob = icmp sge i32 %index, %indices_size\n"
        emit_raw "  br i1 %index_oob, label %ret_empty, label %load_kind\n"
        emit_raw "load_kind:\n"
        emit_raw "  %kind_ptr = getelementptr i8, ptr %self, i32 32\n"
        emit_raw "  %kind = load i8, ptr %kind_ptr\n"
        emit_raw "  %is1 = icmp eq i8 %kind, 1\n"
        emit_raw "  br i1 %is1, label %load1, label %check2\n"
        emit_raw "check2:\n"
        emit_raw "  %is2 = icmp eq i8 %kind, 2\n"
        emit_raw "  br i1 %is2, label %load2, label %load4\n"
        emit_raw "load1:\n"
        emit_raw "  %idx64.1 = sext i32 %index to i64\n"
        emit_raw "  %addr1 = getelementptr i8, ptr %indices, i64 %idx64.1\n"
        emit_raw "  %v1 = load i8, ptr %addr1\n"
        emit_raw "  %raw1 = zext i8 %v1 to i32\n"
        emit_raw "  br label %merge_raw\n"
        emit_raw "load2:\n"
        emit_raw "  %idx64.2 = sext i32 %index to i64\n"
        emit_raw "  %indices_i16 = bitcast ptr %indices to ptr\n"
        emit_raw "  %addr2 = getelementptr i16, ptr %indices_i16, i64 %idx64.2\n"
        emit_raw "  %v2 = load i16, ptr %addr2\n"
        emit_raw "  %raw2 = zext i16 %v2 to i32\n"
        emit_raw "  br label %merge_raw\n"
        emit_raw "load4:\n"
        emit_raw "  %idx64.4 = sext i32 %index to i64\n"
        emit_raw "  %indices_i32 = bitcast ptr %indices to ptr\n"
        emit_raw "  %addr4 = getelementptr i32, ptr %indices_i32, i64 %idx64.4\n"
        emit_raw "  %raw4 = load i32, ptr %addr4\n"
        emit_raw "  br label %merge_raw\n"
        emit_raw "merge_raw:\n"
        emit_raw "  %raw = phi i32 [%raw1, %load1], [%raw2, %load2], [%raw4, %load4]\n"
        emit_raw "  %is_empty_raw = icmp eq i32 %raw, 0\n"
        emit_raw "  br i1 %is_empty_raw, label %ret_empty, label %validate_entry\n"
        emit_raw "validate_entry:\n"
        emit_raw "  %entry_index = sub i32 %raw, 1\n"
        emit_raw "  %size_ptr = getelementptr i8, ptr %self, i32 24\n"
        emit_raw "  %size = load i32, ptr %size_ptr\n"
        emit_raw "  %deleted_ptr = getelementptr i8, ptr %self, i32 28\n"
        emit_raw "  %deleted = load i32, ptr %deleted_ptr\n"
        emit_raw "  %entries_size = add i32 %size, %deleted\n"
        emit_raw "  %entry_neg = icmp slt i32 %entry_index, 0\n"
        emit_raw "  br i1 %entry_neg, label %ret_empty, label %check_entry_bound\n"
        emit_raw "check_entry_bound:\n"
        emit_raw "  %entry_oob = icmp sge i32 %entry_index, %entries_size\n"
        emit_raw "  br i1 %entry_oob, label %ret_empty, label %load_entries\n"
        emit_raw "load_entries:\n"
        emit_raw "  %entries_ptr_addr = getelementptr i8, ptr %self, i32 8\n"
        emit_raw "  %entries = load ptr, ptr %entries_ptr_addr\n"
        emit_raw "  %entries_is_null = icmp eq ptr %entries, null\n"
        emit_raw "  br i1 %entries_is_null, label %ret_empty, label %check_slot\n"
        emit_raw "check_slot:\n"
        emit_raw "  %entry_idx64 = sext i32 %entry_index to i64\n"
        emit_raw "  %entry_off = mul i64 %entry_idx64, 8\n"
        emit_raw "  %entry_slot = getelementptr i8, ptr %entries, i64 %entry_off\n"
        emit_raw "  %entry_ptr = load ptr, ptr %entry_slot\n"
        emit_raw "  %entry_is_null = icmp eq ptr %entry_ptr, null\n"
        emit_raw "  br i1 %entry_is_null, label %ret_empty, label %ret_index\n"
        emit_raw "ret_index:\n"
        emit_raw "  ret i32 %entry_index\n"
        emit_raw "ret_empty:\n"
        emit_raw "  ret i32 -1\n"
        emit_raw "}\n\n"
        return true
      end

      if mangled.includes?("$Hget_entry$$Int32") && mangled.includes?("Hash$L")
        # Root-cause fix: disable pointer-slot override.
        # Hash entries are value structs; loading ptr from each slot corrupts
        # entry reads and causes null/invalid receivers in deleted?/matches?.
        return false

        emit_raw "; #{mangled} — fixed stride + pointer dereference override\n"
        emit_raw "define ptr @#{mangled}(ptr %self, i32 %index) {\n"
        emit_raw "entry:\n"
        emit_raw "  %entries_ptr = getelementptr i8, ptr %self, i32 8\n"
        emit_raw "  %entries = load ptr, ptr %entries_ptr\n"
        emit_raw "  %idx64 = sext i32 %index to i64\n"
        emit_raw "  %offset = mul i64 %idx64, 8\n"   # stride = 8 (pointer size)
        emit_raw "  %slot = getelementptr i8, ptr %entries, i64 %offset\n"
        emit_raw "  %entry_ptr = load ptr, ptr %slot\n"  # load the stored pointer
        emit_raw "  ret ptr %entry_ptr\n"
        emit_raw "}\n\n"
        return true
      end

      # Hash#set_entry — fix stride to match malloc_entries (8 bytes per slot).
      if mangled.includes?("$Hset_entry$$Int32_Hash$CCEntry$L")
        # Root-cause fix: disable pointer-slot override.
        # Hash entries are stored by value; storing ptr payload into slots is ABI/layout
        # incompatible and corrupts Hash internals.
        return false

        emit_raw "; #{mangled} — fixed stride override\n"
        emit_raw "define void @#{mangled}(ptr %self, i32 %index, ptr %value) {\n"
        emit_raw "entry:\n"
        emit_raw "  %entries_ptr = getelementptr i8, ptr %self, i32 8\n"
        emit_raw "  %entries = load ptr, ptr %entries_ptr\n"
        emit_raw "  %idx64 = sext i32 %index to i64\n"
        emit_raw "  %offset = mul i64 %idx64, 8\n"
        emit_raw "  %slot = getelementptr i8, ptr %entries, i64 %offset\n"
        emit_raw "  store ptr %value, ptr %slot\n"
        emit_raw "  ret void\n"
        emit_raw "}\n\n"
        return true
      end

      # Hash#[]? — fix return type to be Nil | V union instead of raw V.
      # Hash#[]? override: call find_entry directly and return Nil | V union.
      # The HIR types []? as returning a nilable union (Nil | V), so both the
      # function definition and call site must use the union ABI.
      if mangled.includes?("$H$IDXQ$") && mangled.includes?("Hash$L")
        # Root-cause stabilization: this fast override reconstructs Hash::Entry
        # field layout heuristically (key/value size+alignment), which is not
        # generally sound for all monomorphized key/value combinations.
        # That causes payload corruption and downstream EXC_BAD_ACCESS in stage2.
        # Fall back to regular lowering, which has correct ABI/layout handling.
        return false

        # Extract value type from the Hash mangled name.
        # Hash$L<Key>$C$_<Value>$R → find first $C$_ after Hash$L, value is rest minus final $R
        hash_prefix = mangled.split("$H$IDXQ$").first
        inner = hash_prefix.sub("Hash$L", "")
        # Strip trailing $R (closing Hash)
        inner = inner.ends_with?("$R") ? inner[0...-2] : inner
        # Split on first $C$_ to get key and value parts
        sep_idx = inner.index("$C$_")
        key_type_suffix = if sep_idx
                            inner[0...sep_idx]
                          else
                            ""
                          end
        value_type_str = if sep_idx
                           inner[(sep_idx + 4)..]  # after "$C$_"
                         else
                           ""
                         end
        # Fast []? override is only safe when Hash value payload is a plain scalar/pointer.
        # For nilable/union values (for example V = NamedTuple(...)?) the entry @value field
        # stores a full union object, not a plain pointer. Loading it as ptr corrupts payload
        # (observed as [type_id, type_id, ptr_low, ...] in stage2 crash path).
        return false if value_type_str.includes?("$Q") || value_type_str.includes?("$_$OR$_")

        # Default key LLVM type from parsed generic key suffix.
        key_llvm_type = case key_type_suffix
                        when "String" then "ptr"
                        when "Int32"  then "i32"
                        when "Int64"  then "i64"
                        when "UInt32" then "i32"
                        when "UInt64" then "i64"
                        else               "ptr"
                        end

        # Resolve find_entry using real module functions first (more robust than
        # relying on []? suffix parsing which can include unrelated type markers).
        find_entry_prefix = "#{hash_prefix}$Hfind_entry$$"
        find_entry_name = "#{find_entry_prefix}#{key_type_suffix}"
        find_entry_func = @module.functions.find { |f| mangle_function_name(f.name) == find_entry_name }
        if find_entry_func.nil?
          find_entry_func = @module.functions.find do |f|
            mname = mangle_function_name(f.name)
            mname.starts_with?(find_entry_prefix)
          end
          find_entry_name = mangle_function_name(find_entry_func.name) if find_entry_func
        end

        # Bail out if find_entry function isn't available — can't generate override.
        return false unless find_entry_func || @emitted_functions.includes?(find_entry_name)

        # Prefer the real key parameter type from resolved find_entry signature.
        if find_entry_func && find_entry_func.params.size >= 2
          key_llvm_type = @type_mapper.llvm_type(find_entry_func.params[1].type)
        end
        key_is_ptr = key_llvm_type == "ptr" || key_llvm_type == "i64"

        # Union type name for Nil | Entry (returned by find_entry).
        # Do not synthesize this from hash_prefix: for specialized callsites
        # the key type may stay generic in hash_prefix ("Key") while find_entry
        # return type is concrete (e.g. HTTP::Headers::Key). Reuse the actual
        # declared return type from module metadata when available.
        find_entry_ret_union = begin
          if find_entry_func
            @type_mapper.llvm_type(find_entry_func.return_type)
          end
        rescue
          nil
        end
        entry_type_name = hash_prefix.sub("Hash$L", "Hash$CCEntry$L")
        nil_or_entry_union = find_entry_ret_union || "%Nil$_$OR$_#{entry_type_name}.union"

        # Ensure the entry union type is declared (it may not be in the type registry
        # if Hash::Entry isn't directly referenced in user code).
        if nil_or_entry_union.starts_with?('%') && nil_or_entry_union.ends_with?(".union")
          type_base = nil_or_entry_union[1..]  # strip leading %
          unless @emitted_union_type_names.includes?(type_base)
            emit_raw "%#{type_base} = type { i32, [12 x i8] }\n"
            @emitted_union_type_names << type_base
          end
        end

        # Use the declared function return type for the union ABI.
        # Only keep this fast override for canonical two-variant unions: Nil | V.
        return_desc = @module.get_union_descriptor(func.return_type)
        return false unless return_desc
        nil_variant = return_desc.variants.find { |variant| variant.type_ref == TypeRef::NIL }
        return false unless nil_variant
        non_nil_variants = return_desc.variants.reject { |variant| variant.type_ref == TypeRef::NIL }
        return false unless non_nil_variants.size == 1

        value_variant = non_nil_variants.first
        value_ref = value_variant.type_ref
        value_variant_id = value_variant.type_id
        nil_variant_id = nil_variant.type_id
        value_union_name = @type_mapper.llvm_type(func.return_type)
        value_llvm_type = @type_mapper.llvm_type(value_ref)
        # Complex payloads (structs, tuples, void) are handled by the normal lowering path.
        return false if value_llvm_type.starts_with?("%") || value_llvm_type.starts_with?("[") || value_llvm_type == "void"

        # Compute value offset in Entry body: after @key field, aligned to value type.
        # Entry struct layout: @key at 0, @value at aligned(key_size, value_align), @hash after.
        key_size = key_is_ptr ? 8 : 4
        value_align = case value_llvm_type
                      when "ptr", "i64" then 8
                      when "i1", "i8"  then 1
                      when "i16"        then 2
                      else                   4
                      end
        value_offset = (key_size + value_align - 1) & ~(value_align - 1)

        emit_raw "; #{mangled} — union return override for Hash#[]?\n"
        emit_raw "define #{value_union_name} @#{mangled}(ptr %self, #{key_llvm_type} %key) {\n"
        emit_raw "entry:\n"
        emit_raw "  %entry_union = call #{nil_or_entry_union} @#{find_entry_name}(ptr %self, #{key_llvm_type} %key)\n"
        emit_raw "  %eu_ptr = alloca #{nil_or_entry_union}, align 8\n"
        emit_raw "  store #{nil_or_entry_union} %entry_union, ptr %eu_ptr\n"
        emit_raw "  %tid_ptr = getelementptr #{nil_or_entry_union}, ptr %eu_ptr, i32 0, i32 0\n"
        emit_raw "  %tid = load i32, ptr %tid_ptr\n"
        emit_raw "  %found = icmp ne i32 %tid, 0\n"
        emit_raw "  br i1 %found, label %found_bb, label %notfound_bb\n"
        emit_raw "found_bb:\n"
        emit_raw "  %pay_ptr = getelementptr #{nil_or_entry_union}, ptr %eu_ptr, i32 0, i32 1\n"
        emit_raw "  %entry_p = load ptr, ptr %pay_ptr, align 4\n"
        emit_raw "  %val_ptr = getelementptr i8, ptr %entry_p, i32 #{value_offset}\n"
        emit_raw "  %val = load #{value_llvm_type}, ptr %val_ptr\n"
        # Build result union: type_id = variant_id for V (non-nil), payload = val
        emit_raw "  %result_ptr = alloca #{value_union_name}, align 8\n"
        emit_raw "  store #{value_union_name} zeroinitializer, ptr %result_ptr\n"
        emit_raw "  %rtid = getelementptr #{value_union_name}, ptr %result_ptr, i32 0, i32 0\n"
        emit_raw "  store i32 #{value_variant_id}, ptr %rtid\n"
        emit_raw "  %rpay = getelementptr #{value_union_name}, ptr %result_ptr, i32 0, i32 1\n"
        emit_raw "  store #{value_llvm_type} %val, ptr %rpay, align 4\n"
        emit_raw "  %result_found = load #{value_union_name}, ptr %result_ptr\n"
        emit_raw "  ret #{value_union_name} %result_found\n"
        emit_raw "notfound_bb:\n"
        if nil_variant_id == 0
          emit_raw "  ret #{value_union_name} zeroinitializer\n"
        else
          emit_raw "  %notfound_ptr = alloca #{value_union_name}, align 8\n"
          emit_raw "  store #{value_union_name} zeroinitializer, ptr %notfound_ptr\n"
          emit_raw "  %nftid = getelementptr #{value_union_name}, ptr %notfound_ptr, i32 0, i32 0\n"
          emit_raw "  store i32 #{nil_variant_id}, ptr %nftid\n"
          emit_raw "  %notfound = load #{value_union_name}, ptr %notfound_ptr\n"
          emit_raw "  ret #{value_union_name} %notfound\n"
        end
        emit_raw "}\n\n"
        # Record the actual return type so call sites use the correct ABI
        mangled = mangle_function_name(func.name)
        @emitted_function_return_types[mangled] = value_union_name
        @emitted_functions << mangled
        return true
      end

      # ── Primitive integer/float binary operations ──
      # Crystal defines these via @[Primitive(:binary)] with empty bodies.
      # Our compiler compiles them to trivial stubs (ret 0). Override with correct ops.
      mangled = mangle_function_name(func.name)
      if emit_primitive_binary_override(func, mangled)
        return true
      end

      # ── Number comparison overrides ──
      # Number#<=>, Number#>, Number#< have mutual recursion:
      #   Number#<=> calls Number#> which calls Number#<=> → infinite loop
      # Break the cycle by implementing these with direct icmp.
      # Since Number values are stored as structs with the value at offset 0,
      # we load i32 from both operands and compare directly.
      if mangled == "Number$H$CMP$$Number"
        # Number#<=>(other : Number) : Nil | Int32
        # Returns -1, 0, or 1 (never nil for integer comparisons)
        emit_raw "; Number#<=> — cycle-breaking builtin override\n"
        emit_raw "define %Nil$_$OR$_Int32.union @#{mangled}(ptr %self, ptr %other) {\n"
        emit_raw "entry:\n"
        emit_raw "  %self_val = ptrtoint ptr %self to i32\n"
        emit_raw "  %other_val = ptrtoint ptr %other to i32\n"
        emit_raw "  %gt = icmp sgt i32 %self_val, %other_val\n"
        emit_raw "  br i1 %gt, label %ret_pos, label %check_lt\n"
        emit_raw "check_lt:\n"
        emit_raw "  %lt = icmp slt i32 %self_val, %other_val\n"
        emit_raw "  br i1 %lt, label %ret_neg, label %ret_zero\n"
        emit_raw "ret_pos:\n"
        emit_raw "  %pos_ptr = alloca %Nil$_$OR$_Int32.union, align 8\n"
        emit_raw "  %pos_tid = getelementptr %Nil$_$OR$_Int32.union, ptr %pos_ptr, i32 0, i32 0\n"
        emit_raw "  store i32 1, ptr %pos_tid\n"
        emit_raw "  %pos_pay = getelementptr %Nil$_$OR$_Int32.union, ptr %pos_ptr, i32 0, i32 1\n"
        emit_raw "  store i32 1, ptr %pos_pay, align 4\n"
        emit_raw "  %pos = load %Nil$_$OR$_Int32.union, ptr %pos_ptr\n"
        emit_raw "  ret %Nil$_$OR$_Int32.union %pos\n"
        emit_raw "ret_neg:\n"
        emit_raw "  %neg_ptr = alloca %Nil$_$OR$_Int32.union, align 8\n"
        emit_raw "  %neg_tid = getelementptr %Nil$_$OR$_Int32.union, ptr %neg_ptr, i32 0, i32 0\n"
        emit_raw "  store i32 1, ptr %neg_tid\n"
        emit_raw "  %neg_pay = getelementptr %Nil$_$OR$_Int32.union, ptr %neg_ptr, i32 0, i32 1\n"
        emit_raw "  store i32 -1, ptr %neg_pay, align 4\n"
        emit_raw "  %neg = load %Nil$_$OR$_Int32.union, ptr %neg_ptr\n"
        emit_raw "  ret %Nil$_$OR$_Int32.union %neg\n"
        emit_raw "ret_zero:\n"
        emit_raw "  %zero_ptr = alloca %Nil$_$OR$_Int32.union, align 8\n"
        emit_raw "  %zero_tid = getelementptr %Nil$_$OR$_Int32.union, ptr %zero_ptr, i32 0, i32 0\n"
        emit_raw "  store i32 1, ptr %zero_tid\n"
        emit_raw "  %zero_pay = getelementptr %Nil$_$OR$_Int32.union, ptr %zero_ptr, i32 0, i32 1\n"
        emit_raw "  store i32 0, ptr %zero_pay, align 4\n"
        emit_raw "  %zero = load %Nil$_$OR$_Int32.union, ptr %zero_ptr\n"
        emit_raw "  ret %Nil$_$OR$_Int32.union %zero\n"
        emit_raw "}\n\n"
        return true
      elsif mangled == "Number$H$GT$$Number"
        # Number#>(other : Number) : Bool — direct comparison
        emit_raw "; Number#> — cycle-breaking builtin override\n"
        emit_raw "define i1 @#{mangled}(ptr %self, ptr %other) {\n"
        emit_raw "entry:\n"
        emit_raw "  %self_val = ptrtoint ptr %self to i32\n"
        emit_raw "  %other_val = ptrtoint ptr %other to i32\n"
        emit_raw "  %result = icmp sgt i32 %self_val, %other_val\n"
        emit_raw "  ret i1 %result\n"
        emit_raw "}\n\n"
        return true
      elsif mangled == "Number$H$LT$$Number"
        # Number#<(other : Number) : Bool — direct comparison
        emit_raw "; Number#< — cycle-breaking builtin override\n"
        emit_raw "define i1 @#{mangled}(ptr %self, ptr %other) {\n"
        emit_raw "entry:\n"
        emit_raw "  %self_val = ptrtoint ptr %self to i32\n"
        emit_raw "  %other_val = ptrtoint ptr %other to i32\n"
        emit_raw "  %result = icmp slt i32 %self_val, %other_val\n"
        emit_raw "  ret i1 %result\n"
        emit_raw "}\n\n"
        return true
      elsif mangled == "String$Hmatches$Q$$Regex_Int32_Regex$CCMatchOptions"
        # String#matches?(Regex, pos = 0, options: ...) : Bool
        # stdlib path routes via Regex#match_data and expects the full Regex object layout.
        # Our regex runtime stores {code*, match_data*}, so call PCRE2 directly here.
        emit_raw "; String#matches?(Regex, ...) — runtime override\n"
        emit_raw "define i1 @#{mangled}(ptr %self, ptr %regex, i32 %pos, i32 %options) {\n"
        emit_raw "entry:\n"
        emit_raw "  %self_null = icmp eq ptr %self, null\n"
        emit_raw "  br i1 %self_null, label %ret_false, label %check_regex\n"
        emit_raw "check_regex:\n"
        emit_raw "  %regex_null = icmp eq ptr %regex, null\n"
        emit_raw "  br i1 %regex_null, label %ret_false, label %load_regex\n"
        emit_raw "load_regex:\n"
        emit_raw "  %re = load ptr, ptr %regex\n"
        emit_raw "  %md_slot = getelementptr i8, ptr %regex, i32 8\n"
        emit_raw "  %md = load ptr, ptr %md_slot\n"
        emit_raw "  %re_null = icmp eq ptr %re, null\n"
        emit_raw "  %md_null = icmp eq ptr %md, null\n"
        emit_raw "  %bad_regex = or i1 %re_null, %md_null\n"
        emit_raw "  br i1 %bad_regex, label %ret_false, label %match\n"
        emit_raw "match:\n"
        emit_raw "  %bs_ptr = getelementptr i8, ptr %self, i32 4\n"
        emit_raw "  %bytesize = load i32, ptr %bs_ptr\n"
        emit_raw "  %data = getelementptr i8, ptr %self, i32 12\n"
        emit_raw "  %bs64 = sext i32 %bytesize to i64\n"
        emit_raw "  %pos64 = sext i32 %pos to i64\n"
        emit_raw "  %rc = call i32 @pcre2_match_8(ptr %re, ptr %data, i64 %bs64, i64 %pos64, i32 0, ptr %md, ptr null)\n"
        emit_raw "  %ok = icmp sge i32 %rc, 0\n"
        emit_raw "  ret i1 %ok\n"
        emit_raw "ret_false:\n"
        emit_raw "  ret i1 false\n"
        emit_raw "}\n\n"
        return true
      elsif mangled.starts_with?("String$Hgsub$$Regex")
        # String#gsub(Regex, ...) — redirect to __crystal_v2_string_gsub_regex
        # The Crystal frontend may resolve gsub(Regex, String) to a Hash|NamedTuple
        # overload. We override the entire function to use our PCRE2-based runtime.
        # Extract the replacement string from the union payload (field 1).
        hash_param_type = func.params.size >= 3 ? @type_mapper.llvm_type(func.params[2].type) : "ptr"
        if hash_param_type.includes?(".union")
          emit_raw "; String#gsub(Regex, ...) — runtime override via __crystal_v2_string_gsub_regex\n"
          emit_raw "define ptr @#{mangled}(ptr %self, ptr %pattern, #{hash_param_type} %hash, i32 %options) {\n"
          emit_raw "entry:\n"
          # Extract replacement string from union payload
          emit_raw "  %hash_ptr = alloca #{hash_param_type}, align 8\n"
          emit_raw "  store #{hash_param_type} %hash, ptr %hash_ptr\n"
          emit_raw "  %repl_slot = getelementptr #{hash_param_type}, ptr %hash_ptr, i32 0, i32 1\n"
          emit_raw "  %repl = load ptr, ptr %repl_slot\n"
          emit_raw "  %result = call ptr @__crystal_v2_string_gsub_regex(ptr %self, ptr %pattern, ptr %repl)\n"
          emit_raw "  ret ptr %result\n"
          emit_raw "}\n\n"
        else
          emit_raw "; String#gsub(Regex, ...) — runtime override (non-union)\n"
          emit_raw "define ptr @#{mangled}(ptr %self, ptr %pattern, ptr %repl, i32 %options) {\n"
          emit_raw "entry:\n"
          emit_raw "  %result = call ptr @__crystal_v2_string_gsub_regex(ptr %self, ptr %pattern, ptr %repl)\n"
          emit_raw "  ret ptr %result\n"
          emit_raw "}\n\n"
        end
        return true
      elsif mangled.starts_with?("Regex$Hmatch_at_byte_index")
        # Regex#match_at_byte_index — PCRE2 runtime override
        # Replaces self-recursive stub with actual pcre2_match_8 call.
        # Regex struct layout (from __crystal_v2_regex_new): {code* @0, match_data* @8}
        # MatchData layout: {type_id @0, group_size @8, string @16, ovector @40}
        ret_type = @type_mapper.llvm_type(func.return_type)
        emit_raw "; Regex#match_at_byte_index — PCRE2 runtime override\n"
        emit_raw "define #{ret_type} @#{mangled}(ptr %self, ptr %str, i32 %byte_index, ptr %_options) {\n"
        emit_raw "entry:\n"
        emit_raw "  %re = load ptr, ptr %self\n"
        emit_raw "  %re_null = icmp eq ptr %re, null\n"
        emit_raw "  br i1 %re_null, label %ret_nil, label %check_str\n"
        emit_raw "check_str:\n"
        emit_raw "  %str_null = icmp eq ptr %str, null\n"
        emit_raw "  br i1 %str_null, label %ret_nil, label %do_match\n"
        emit_raw "do_match:\n"
        emit_raw "  %md_slot = getelementptr i8, ptr %self, i32 8\n"
        emit_raw "  %md = load ptr, ptr %md_slot\n"
        emit_raw "  %bs_ptr = getelementptr i8, ptr %str, i32 4\n"
        emit_raw "  %bytesize = load i32, ptr %bs_ptr\n"
        emit_raw "  %data = getelementptr i8, ptr %str, i32 12\n"
        emit_raw "  %bs64 = sext i32 %bytesize to i64\n"
        emit_raw "  %bi64 = sext i32 %byte_index to i64\n"
        emit_raw "  %rc = call i32 @pcre2_match_8(ptr %re, ptr %data, i64 %bs64, i64 %bi64, i32 0, ptr %md, ptr null)\n"
        emit_raw "  %matched = icmp sge i32 %rc, 0\n"
        emit_raw "  br i1 %matched, label %create_md, label %ret_nil\n"
        emit_raw "create_md:\n"
        emit_raw "  %ovcount = call i32 @pcre2_get_ovector_count_8(ptr %md)\n"
        emit_raw "  %group_size = sub i32 %ovcount, 1\n"
        emit_raw "  %ov = call ptr @pcre2_get_ovector_pointer_8(ptr %md)\n"
        # Copy ovector to preserve data across multiple calls
        emit_raw "  %ov_entries = mul i32 %ovcount, 2\n"
        emit_raw "  %ov_bytes_i32 = mul i32 %ov_entries, 8\n"
        emit_raw "  %ov_bytes = sext i32 %ov_bytes_i32 to i64\n"
        emit_raw "  %ov_copy = call ptr @__crystal_v2_malloc64(i64 %ov_bytes)\n"
        emit_raw "  call void @llvm.memcpy.p0.p0.i64(ptr %ov_copy, ptr %ov, i64 %ov_bytes, i1 false)\n"
        # Allocate MatchData (48 bytes)
        emit_raw "  %md_obj = call ptr @__crystal_v2_malloc64(i64 48)\n"
        emit_raw "  store i32 0, ptr %md_obj\n"
        emit_raw "  %gs_ptr = getelementptr i8, ptr %md_obj, i32 8\n"
        emit_raw "  store i32 %group_size, ptr %gs_ptr\n"
        emit_raw "  %str_slot = getelementptr i8, ptr %md_obj, i32 16\n"
        emit_raw "  store ptr %str, ptr %str_slot\n"
        emit_raw "  %ov_slot = getelementptr i8, ptr %md_obj, i32 40\n"
        emit_raw "  store ptr %ov_copy, ptr %ov_slot\n"
        # Wrap in Nil|MatchData union: type_id=1 for MatchData variant
        emit_raw "  %union_ptr = alloca #{ret_type}, align 8\n"
        emit_raw "  store #{ret_type} zeroinitializer, ptr %union_ptr\n"
        emit_raw "  %tid_ptr = getelementptr #{ret_type}, ptr %union_ptr, i32 0, i32 0\n"
        emit_raw "  store i32 1, ptr %tid_ptr\n"
        emit_raw "  %pay_ptr = getelementptr #{ret_type}, ptr %union_ptr, i32 0, i32 1\n"
        emit_raw "  store ptr %md_obj, ptr %pay_ptr\n"
        emit_raw "  %result = load #{ret_type}, ptr %union_ptr\n"
        emit_raw "  ret #{ret_type} %result\n"
        emit_raw "ret_nil:\n"
        emit_raw "  ret #{ret_type} zeroinitializer\n"
        emit_raw "}\n\n"
        return true
      elsif mangled == "Regex$CCMatchData$Hbyte_begin$$Int32"
        # Regex::MatchData#byte_begin(n : Int32) : Int32
        # Reads ovector[n*2] from MatchData. MatchData layout: ovector ptr at offset 40.
        emit_raw "; Regex::MatchData#byte_begin — PCRE2 runtime override\n"
        emit_raw "define i32 @#{mangled}(ptr %self, i32 %n) {\n"
        emit_raw "entry:\n"
        emit_raw "  %ov_slot = getelementptr i8, ptr %self, i32 40\n"
        emit_raw "  %ov = load ptr, ptr %ov_slot\n"
        emit_raw "  %idx = mul i32 %n, 2\n"
        emit_raw "  %idx64 = sext i32 %idx to i64\n"
        emit_raw "  %ptr = getelementptr i64, ptr %ov, i64 %idx64\n"
        emit_raw "  %val64 = load i64, ptr %ptr\n"
        emit_raw "  %val = trunc i64 %val64 to i32\n"
        emit_raw "  ret i32 %val\n"
        emit_raw "}\n\n"
        return true
      elsif mangled == "Regex$CCMatchData$H$IDX$$Int32"
        # Regex::MatchData#[](n : Int32) : String
        # Extracts substring from ovector[n*2..n*2+1]. Uses unsafe_byte_slice.
        emit_raw "; Regex::MatchData#[] — PCRE2 runtime override\n"
        emit_raw "define ptr @#{mangled}(ptr %self, i32 %n) {\n"
        emit_raw "entry:\n"
        # Load string from offset 16
        emit_raw "  %str_slot = getelementptr i8, ptr %self, i32 16\n"
        emit_raw "  %str = load ptr, ptr %str_slot\n"
        # Load ovector from offset 40
        emit_raw "  %ov_slot = getelementptr i8, ptr %self, i32 40\n"
        emit_raw "  %ov = load ptr, ptr %ov_slot\n"
        # Get start = ovector[n*2], end = ovector[n*2+1]
        emit_raw "  %idx_start = mul i32 %n, 2\n"
        emit_raw "  %idx_end_i32 = add i32 %idx_start, 1\n"
        emit_raw "  %idx_start64 = sext i32 %idx_start to i64\n"
        emit_raw "  %idx_end64 = sext i32 %idx_end_i32 to i64\n"
        emit_raw "  %start_ptr = getelementptr i64, ptr %ov, i64 %idx_start64\n"
        emit_raw "  %end_ptr = getelementptr i64, ptr %ov, i64 %idx_end64\n"
        emit_raw "  %start64 = load i64, ptr %start_ptr\n"
        emit_raw "  %end64 = load i64, ptr %end_ptr\n"
        emit_raw "  %start = trunc i64 %start64 to i32\n"
        emit_raw "  %end = trunc i64 %end64 to i32\n"
        emit_raw "  %len = sub i32 %end, %start\n"
        # Create substring using byte_slice
        emit_raw "  %result = call ptr @String$Hbyte_slice$$Int32_Int32(ptr %str, i32 %start, i32 %len)\n"
        emit_raw "  ret ptr %result\n"
        emit_raw "}\n\n"
        return true
      end

      # Time.new(LibC::Timespec, Time::Location) — explicit self.new that adds UNIX_EPOCH
      # The auto-allocator generates the wrong body (passes tv_sec directly as @seconds
      # instead of computing UNIX_EPOCH.total_seconds + tv_sec).
      if mangled == "Time$Dnew$$LibC$CCTimespec_Time$CCLocation"
        emit_raw "; Time.new(LibC::Timespec, Time::Location) — runtime override\n"
        emit_raw "define ptr @#{mangled}(i64 %tv_sec, i32 %tv_nsec, ptr %location) {\n"
        emit_raw "entry:\n"
        # Load UNIX_EPOCH global → Time struct pointer → @seconds at offset 0
        emit_raw "  %epoch_ptr = load ptr, ptr @Time__classvar__UNIX_EPOCH\n"
        emit_raw "  %epoch_is_null = icmp eq ptr %epoch_ptr, null\n"
        emit_raw "  br i1 %epoch_is_null, label %epoch_zero, label %epoch_ok\n"
        emit_raw "epoch_zero:\n"
        emit_raw "  br label %compute\n"
        emit_raw "epoch_ok:\n"
        emit_raw "  %epoch_sec = load i64, ptr %epoch_ptr\n"
        emit_raw "  br label %compute\n"
        emit_raw "compute:\n"
        emit_raw "  %epoch_total = phi i64 [0, %epoch_zero], [%epoch_sec, %epoch_ok]\n"
        emit_raw "  %seconds = add i64 %epoch_total, %tv_sec\n"
        # Bootstrap fallback: route through the always-emitted Int64 constructor.
        # This keeps stage2 codegen valid even when the 3-arg overload isn't lowered.
        emit_raw "  %result = call ptr @Time$Dnew$$Int64(i64 %seconds)\n"
        emit_raw "  ret ptr %result\n"
        emit_raw "}\n\n"
        return true
      end

      false
    end

    # Emit correct primitive binary operation for integer/float methods with
    # empty @[Primitive(:binary)] bodies from the Crystal stdlib.
    private def emit_primitive_binary_override(func : Function, mangled : String) : Bool
      # Match: <Type>$H$<OP>$$<ArgType> patterns
      # Operator mangling: $GT = >, $LT = <, $GE = >=, $LE = <=, $EQ = ==,
      # $NE = !=, $ADD = +, $SUB = -, $MUL = *, $AND = &, $OR = |, $XOR = ^,
      # $SHL = <<, $SHR = >>
      int_types = {"Int8" => {"i8", true}, "Int16" => {"i16", true}, "Int32" => {"i32", true},
                   "Int64" => {"i64", true}, "Int128" => {"i128", true},
                   "UInt8" => {"i8", false}, "UInt16" => {"i16", false}, "UInt32" => {"i32", false},
                   "UInt64" => {"i64", false}, "UInt128" => {"i128", false}}

      # Check if this is a trivial function (2 blocks or less, no meaningful operations)
      is_trivial = func.blocks.size <= 3 && func.blocks.all? { |b| b.instructions.size <= 2 }
      return false unless is_trivial

      # Extract type and operator from mangled name
      int_types.each do |type_name, type_info|
        llvm_type, is_signed = type_info
        prefix = "#{type_name}$H$"

        next unless mangled.starts_with?(prefix)
        rest = mangled[prefix.size..]

        # Match operator
        op_match = nil
        ret_type = ""
        ir_op = ""
        case rest
        when /\AGT\$\$/
          ir_op = is_signed ? "icmp sgt" : "icmp ugt"
          ret_type = "i1"
          op_match = true
        when /\ALT\$\$/
          ir_op = is_signed ? "icmp slt" : "icmp ult"
          ret_type = "i1"
          op_match = true
        when /\AGE\$\$/
          ir_op = is_signed ? "icmp sge" : "icmp uge"
          ret_type = "i1"
          op_match = true
        when /\ALE\$\$/
          ir_op = is_signed ? "icmp sle" : "icmp ule"
          ret_type = "i1"
          op_match = true
        when /\AEQ\$\$/
          ir_op = "icmp eq"
          ret_type = "i1"
          op_match = true
        when /\ANE\$\$/
          ir_op = "icmp ne"
          ret_type = "i1"
          op_match = true
        when /\AADD\$\$/
          ir_op = "add"
          ret_type = llvm_type
          op_match = true
        when /\ASUB\$\$/
          ir_op = "sub"
          ret_type = llvm_type
          op_match = true
        when /\AMUL\$\$/
          ir_op = "mul"
          ret_type = llvm_type
          op_match = true
        when /\AAND\$\$/
          ir_op = "and"
          ret_type = llvm_type
          op_match = true
        when /\AOR\$\$/
          ir_op = "or"
          ret_type = llvm_type
          op_match = true
        when /\AXOR\$\$/
          ir_op = "xor"
          ret_type = llvm_type
          op_match = true
        when /\ASHL\$\$/
          ir_op = "shl"
          ret_type = llvm_type
          op_match = true
        when /\ASHR\$\$/
          ir_op = is_signed ? "ashr" : "lshr"
          ret_type = llvm_type
          op_match = true
        end

        next unless op_match

        # Determine the self type in the function params
        self_llvm = func.params.size >= 1 ? @type_mapper.llvm_type(func.params[0].type) : llvm_type
        other_llvm = func.params.size >= 2 ? @type_mapper.llvm_type(func.params[1].type) : llvm_type

        # If self is ptr (because Int primitive methods declare self as class receiver),
        # we need ptrtoint to extract the actual integer value.
        needs_self_ptrtoint = self_llvm == "ptr"
        actual_self_llvm = needs_self_ptrtoint ? llvm_type : self_llvm

        # Determine the other type's signedness
        other_is_signed = is_signed # default: same as receiver
        if func.params.size >= 2
          other_type_ref = func.params[1].type
          other_is_signed = other_type_ref.id >= TypeRef::INT8.id && other_type_ref.id <= TypeRef::INT128.id
        end

        emit_raw "; #{type_name}##{rest.split("$$").first} primitive override\n"
        emit_raw "define #{ret_type} @#{mangled}(#{self_llvm} %self, #{other_llvm} %other) {\n"

        # Get self value (ptrtoint if needed)
        self_val = if needs_self_ptrtoint
                     emit_raw "  %self_val = ptrtoint ptr %self to #{actual_self_llvm}\n"
                     "%self_val"
                   else
                     "%self"
                   end
        other_val = "%other"

        # Handle type mismatch: extend both to the wider type
        op_type = actual_self_llvm
        if actual_self_llvm != other_llvm
          self_bits = actual_self_llvm[1..].to_i? || 32
          other_bits = other_llvm[1..].to_i? || 32
          if self_bits < other_bits
            op_type = other_llvm
            ext = is_signed ? "sext" : "zext"
            emit_raw "  %self_ext = #{ext} #{actual_self_llvm} #{self_val} to #{other_llvm}\n"
            self_val = "%self_ext"
          elsif other_bits < self_bits
            op_type = actual_self_llvm
            ext = other_is_signed ? "sext" : "zext"
            emit_raw "  %other_ext = #{ext} #{other_llvm} %other to #{actual_self_llvm}\n"
            other_val = "%other_ext"
          end
          # For arithmetic, result type might need to be the wider type
          ret_type = op_type unless ret_type == "i1"
        end

        emit_raw "  %result = #{ir_op} #{op_type} #{self_val}, #{other_val}\n"
        emit_raw "  ret #{ret_type} %result\n"
        emit_raw "}\n\n"
        @emitted_functions << mangled
        @emitted_function_return_types[mangled] = ret_type
        return true
      end

      false
    end

    private def emit_function(func : Function)
      if emit_builtin_override(func)
        # Builtin overrides emit raw LLVM directly and can return before the regular
        # emission path marks the function as emitted. Keep the bookkeeping in sync,
        # otherwise the final "missing function stubs" pass may emit a duplicate
        # definition for an already-defined function.
        mangled_name = mangle_function_name(func.name)
        @emitted_functions << mangled_name
        return_type = @type_mapper.llvm_type(func.return_type)
        @emitted_function_return_types[mangled_name] = return_type
        return
      end

      reset_value_names(func)
      @emitted_allocas.clear
      @addressable_allocas.clear
      @addressable_alloca_initialized.clear
      @value_def_block.clear
      @cross_block_values.clear
      @cross_block_slots.clear
      @cross_block_slot_types.clear
      @cross_block_slot_type_refs.clear
      @phi_predecessor_loads.clear
      @phi_union_to_ptr_extracts.clear
      @phi_union_to_union_converts.clear
      @phi_union_payload_extracts.clear
      @current_func_blocks.clear
      @emitted_value_types.clear

      # Populate block lookup for phi predecessor load emission
      func.blocks.each { |block| @current_func_blocks[block.id] = block }
      @entry_user_block_id = func.blocks.first?.try(&.id) || 0_u32

      # Pre-pass: collect constant values for phi node resolution
      # This ensures forward-referenced constants are available
      prepass_collect_constants(func)

      # Set current func name BEFORE prepass
      @current_func_name = mangle_function_name(func.name)

      # Pre-pass: detect cross-block values that need alloca slots for dominance
      prepass_detect_cross_block_values(func)

      # Pre-pass: infer binary op result types (for widening detection in phi nodes)
      prepass_infer_binary_op_types(func)

      # Function signature
      # Note: void is not valid for parameters, substitute with ptr
      used_param_names = Hash(String, Int32).new(0)
      param_types = func.params.map do |p|
        llvm_type = @type_mapper.llvm_type(p.type)
        llvm_type = "ptr" if llvm_type == "void"

        base_name = sanitize_llvm_local_name(p.name)
        base_name = "arg" if base_name.empty?
        count = used_param_names[base_name]?
        if count
          used_param_names[base_name] = count + 1
          llvm_name = "#{base_name}#{count + 1}"
        else
          used_param_names[base_name] = 0
          llvm_name = base_name
        end

        # Keep param references consistent with the signature.
        @value_names[p.index] = llvm_name

        "#{llvm_type} %#{llvm_name}"
      end
      return_type = @type_mapper.llvm_type(func.return_type)
      # Use pre-computed return type if available.  This corrects void→ptr for methods
      # whose MIR return_type is NIL but which actually return values (detected by
      # scanning Return instructions in precompute_function_return_types).
      mangled_name = @current_func_name
      if return_type == "void"
        if precomputed = @emitted_function_return_types[mangled_name]?
          if precomputed != "void"
            return_type = precomputed
          end
        end
      end
      @current_return_type = return_type  # Store for terminator emission
      @current_return_type_ref = func.return_type
      # @current_func_name already set above before prepass

      @current_func_params = func.params
      @current_slab_frame = func.slab_frame

      # Skip duplicate function definitions
      if @emitted_functions.includes?(mangled_name)
        return
      end
      @emitted_functions << mangled_name
      @emitted_function_return_types[mangled_name] = return_type

      # Pointer#address — primitive: return ptrtoint of self pointer
      if mangled_name.includes?("Pointer$L") && mangled_name.ends_with?("$Haddress")
        emit_raw "define i64 @#{mangled_name}(ptr %self) {\n"
        emit_raw "  %addr = ptrtoint ptr %self to i64\n"
        emit_raw "  ret i64 %addr\n"
        emit_raw "}\n\n"
        return
      end

      # Pointer(T)#bytesize(count) — compute count * element_size explicitly.
      # Some bootstrap paths infer ptr return ABI for this method; in that case
      # return the byte count encoded as a pointer (inttoptr), matching callers.
      if mangled_name.includes?("Pointer$L") && mangled_name.includes?("$Hbytesize$$") && func.params.size >= 2
        elem_size = pointer_word_bytes_u64
        if m = mangled_name.match(/Pointer\$L(.+)\$R\$Hbytesize\$\$/)
          elem_mangled = m[1]
          if elem_type = @module.type_registry.types.find { |t| @type_mapper.mangle_name(t.name) == elem_mangled }
            elem_size = container_elem_storage_size_u64(elem_type)
          elsif elem_mangled == "UInt8" || elem_mangled == "Int8"
            elem_size = 1_u64
          end
        end

        count_param = func.params[1]
        count_name = @value_names[count_param.index]? || "count"
        count_llvm = @type_mapper.llvm_type(count_param.type)
        ptr_int = pointer_sized_int_llvm_type
        ptr_int_bits = ptr_int[1..].to_i? || 64

        emit_raw "define #{return_type} @#{mangled_name}(#{param_types.join(", ")}) {\n"
        emit_raw "entry:\n"

        if count_llvm == ptr_int
          emit_raw "  %count_int = add #{ptr_int} %#{count_name}, 0\n"
        elsif count_llvm == "ptr"
          emit_raw "  %count_int = ptrtoint ptr %#{count_name} to #{ptr_int}\n"
        elsif count_llvm.starts_with?('i')
          count_bits = count_llvm[1..].to_i? || ptr_int_bits
          if count_bits < ptr_int_bits
            ext_op = unsigned_type_ref?(count_param.type) ? "zext" : "sext"
            emit_raw "  %count_int = #{ext_op} #{count_llvm} %#{count_name} to #{ptr_int}\n"
          elsif count_bits > ptr_int_bits
            emit_raw "  %count_int = trunc #{count_llvm} %#{count_name} to #{ptr_int}\n"
          else
            emit_raw "  %count_int = add #{ptr_int} %#{count_name}, 0\n"
          end
        else
          emit_raw "  %count_int = add #{ptr_int} 0, 0\n"
        end

        emit_raw "  %bytes = mul #{ptr_int} %count_int, #{elem_size}\n"

        if return_type == "ptr"
          emit_raw "  %ret_ptr = inttoptr #{ptr_int} %bytes to ptr\n"
          emit_raw "  ret ptr %ret_ptr\n"
        elsif return_type == ptr_int
          emit_raw "  ret #{ptr_int} %bytes\n"
        elsif return_type == "void"
          emit_raw "  ret void\n"
        elsif return_type.starts_with?('i')
          ret_bits = return_type[1..].to_i? || ptr_int_bits
          if ret_bits < ptr_int_bits
            emit_raw "  %ret_int = trunc #{ptr_int} %bytes to #{return_type}\n"
          elsif ret_bits > ptr_int_bits
            emit_raw "  %ret_int = zext #{ptr_int} %bytes to #{return_type}\n"
          else
            emit_raw "  %ret_int = add #{return_type} %bytes, 0\n"
          end
          emit_raw "  ret #{return_type} %ret_int\n"
        else
          emit_raw "  ret #{return_type} zeroinitializer\n"
        end

        emit_raw "}\n\n"
        return
      end

      # Pointer(T)#clear(count) — zero the pointer buffer itself.
      # Important: in our ABI Pointer(Struct) commonly stores pointer slots (8-byte
      # entries), not inline struct payloads. Clearing through the first element pointer
      # corrupts live objects. Always memset(self, 0, count * elem_size).
      if mangled_name.includes?("Pointer$L") && mangled_name.includes?("$Hclear$$") && func.params.size >= 2 && return_type == "void"
        elem_size = pointer_word_bytes_u64
        if m = mangled_name.match(/Pointer\$L(.+)\$R\$Hclear\$\$/)
          elem_mangled = m[1]
          if elem_type = @module.type_registry.types.find { |t| @type_mapper.mangle_name(t.name) == elem_mangled }
            elem_size = container_elem_storage_size_u64(elem_type)
          elsif elem_mangled == "UInt8" || elem_mangled == "Int8"
            elem_size = 1_u64
          end
        end

        count_param = func.params[1]
        count_name = @value_names[count_param.index]? || "count"
        count_llvm = @type_mapper.llvm_type(count_param.type)
        ptr_int = pointer_sized_int_llvm_type
        ptr_int_bits = ptr_int[1..].to_i? || 64

        emit_raw "define void @#{mangled_name}(#{param_types.join(", ")}) {\n"
        emit_raw "entry:\n"

        if count_llvm == ptr_int
          emit_raw "  %count_int = add #{ptr_int} %#{count_name}, 0\n"
        elsif count_llvm == "ptr"
          emit_raw "  %count_int = ptrtoint ptr %#{count_name} to #{ptr_int}\n"
        elsif count_llvm.starts_with?('i')
          count_bits = count_llvm[1..].to_i? || ptr_int_bits
          if count_bits < ptr_int_bits
            ext_op = unsigned_type_ref?(count_param.type) ? "zext" : "sext"
            emit_raw "  %count_int = #{ext_op} #{count_llvm} %#{count_name} to #{ptr_int}\n"
          elsif count_bits > ptr_int_bits
            emit_raw "  %count_int = trunc #{count_llvm} %#{count_name} to #{ptr_int}\n"
          else
            emit_raw "  %count_int = add #{ptr_int} %#{count_name}, 0\n"
          end
        else
          emit_raw "  %count_int = add #{ptr_int} 0, 0\n"
        end

        emit_raw "  %bytes = mul #{ptr_int} %count_int, #{elem_size}\n"
        if ptr_int == "i64"
          emit_raw "  %bytes_i64 = add i64 %bytes, 0\n"
        else
          emit_raw "  %bytes_i64 = zext #{ptr_int} %bytes to i64\n"
        end
        emit_raw "  call void @llvm.memset.p0.i64(ptr %self, i8 0, i64 %bytes_i64, i1 false)\n"
        emit_raw "  ret void\n"
        emit_raw "}\n\n"
        return
      end

      # Proc#call fallback — when HIR/MIR didn't detect the receiver as Proc type,
      # the call goes through normal dispatch to an empty primitive body.
      # A Proc in our compiler is just a function pointer (Proc.new returns the block
      # pointer as-is). So Proc#call(self, args) must invoke self as a function pointer.
      if mangled_name.starts_with?("Proc$Hcall")
        emit_raw "define #{return_type} @#{mangled_name}(#{param_types.join(", ")}) {\n"
        emit_raw "entry:\n"
        # %self (param 0) IS the function pointer — invoke it directly.
        # For zero-arg procs, call with no args. The return value (if any) is
        # discarded when return_type is void, or returned otherwise.
        if return_type == "void"
          emit_raw "  call void %self()\n"
          emit_raw "  ret void\n"
        else
          emit_raw "  %result = call #{return_type} %self()\n"
          emit_raw "  ret #{return_type} %result\n"
        end
        emit_raw "}\n\n"
        return
      end

      # Reference#object_id — primitive: return ptrtoint of self pointer as UInt64
      if mangled_name.ends_with?("$Hobject_id") && return_type == "i64" && param_types.size == 1
        emit_raw "define #{return_type} @#{mangled_name}(#{param_types.join(", ")}) {\n"
        emit_raw "entry:\n"
        emit_raw "  %addr = ptrtoint ptr %self to i64\n"
        emit_raw "  ret i64 %addr\n"
        emit_raw "}\n\n"
        return
      end

      emit_raw "define #{return_type} @#{mangled_name}(#{param_types.join(", ")}) {\n"

      # Emit entry block with hoisted allocas for dominance correctness
      # Use fn_entry to avoid conflict with parameter names like %entry
      emit_raw "fn_entry:\n"
      emit_hoisted_allocas(func)
      if @current_slab_frame
        emit_raw "  call void @__crystal_v2_slab_frame_push()\n"
      end

      # TSan: emit function entry in first block
      @tsan_needs_func_entry = @emit_tsan

      # Prepass: identify which cross-block values need predecessor loads for phi nodes
      # This MUST happen before emitting blocks because block order may differ from CFG order
      prepass_collect_phi_predecessor_loads(func)

      # Prepass: identify which fixed-type values need conversion in predecessor blocks for phi nodes
      prepass_collect_phi_predecessor_conversions(func)
      # Prepass: identify which ptr/void values need union wrapping in predecessor blocks for phi nodes
      prepass_collect_phi_predecessor_union_wraps(func)
      # Prepass: identify which union values need reinterpretation to a different union type for phi nodes
      prepass_collect_phi_union_to_union_converts(func)
      # Prepass: identify which union incoming values need ptr payload extraction
      # in predecessor blocks for ptr-emitted phi nodes.
      prepass_collect_phi_union_to_ptr_extracts(func)
      # Prepass: identify which union-slotted incoming values need scalar payload extraction
      # in predecessor blocks for primitive-typed phi nodes (e.g., phi i32 from union slot).
      prepass_collect_phi_union_payload_extracts(func)

      # Buffer block emission to a temporary output so we can extract non-entry-block
      # allocas and hoist them to the entry block. LLVM treats allocas in non-entry blocks
      # as dynamic stack allocations that grow the stack on every execution and are only
      # freed on function return. Inside loops this causes unbounded stack growth.
      saved_output = @output
      @output = IO::Memory.new
      # Route top-level definitions (stubs, declarations) to the main output
      # during block emission. Without this, they'd end up nested inside the
      # current function which is invalid LLVM IR.
      @toplevel_output = saved_output

      func.blocks.each do |block|
        emit_block(block, func)
      end

      block_ir = @output.to_s
      @output = saved_output
      @toplevel_output = nil

      # Extract alloca instructions from block IR and hoist to entry block.
      # These are scratch allocas for union operations (store→GEP→load patterns)
      # that are safe to allocate once in the entry block and reuse.
      hoisted_allocas = [] of String
      block_ir.each_line do |line|
        stripped = line.lstrip
        if stripped.includes?("= alloca ") && !stripped.starts_with?(';')
          hoisted_allocas << line
        end
      end

      # Emit hoisted allocas in entry block (before the br)
      hoisted_allocas.each do |alloca_line|
        emit_raw alloca_line
        emit_raw "\n"
      end

      # Jump to first user block
      if first_block = func.blocks.first?
        emit_raw "  br label %#{@block_names[first_block.id]}\n"
      end

      # Emit block IR with allocas replaced by no-ops (comments).
      # The alloca SSA names are now defined in the entry block.
      block_ir.each_line do |line|
        stripped = line.lstrip
        if stripped.includes?("= alloca ") && !stripped.starts_with?(';')
          # Replace with comment — the alloca is now in the entry block
          emit_raw "  ; hoisted: #{stripped}\n"
        else
          emit_raw line
          emit_raw "\n"
        end
      end

      emit_raw "}\n\n"
    end

    # Emit all Alloc instructions at function entry for dominance
    private def emit_hoisted_allocas(func : Function)
      func.blocks.each do |block|
        block.instructions.each do |inst|
          next unless inst.is_a?(Alloc)
          # Only hoist stack allocations - heap allocations can stay in place
          next unless inst.strategy == MemoryStrategy::Stack
          name = "%r#{inst.id}"
          @value_names[inst.id] = "r#{inst.id}"
          type = @type_mapper.llvm_alloca_type(inst.alloc_type)
          type = "i8" if type == "void"
          # When LLVM type is just 'ptr' (8 bytes) but MIR needs more space
          # (e.g. Tuples containing union elements), use a byte array alloca
          # to ensure sufficient storage for discriminated union stores.
          if type == "ptr" && inst.size > 8
            type = "[#{inst.size} x i8]"
          end
          emit_raw "  #{name} = alloca #{type}, align #{inst.align}\n"
          @emitted_allocas << inst.id
          @value_types[inst.id] = TypeRef::POINTER
          # Track element type for GEP
          @alloc_element_types[inst.id] = inst.alloc_type
          # Track original alloc_type for enum/struct detection during argument conversion
          @alloc_types[inst.id] = inst.alloc_type
        end
      end

      # Pre-create allocas for AddressOf operands that aren't already stack allocas.
      # This ensures pointerof(x) has a properly-sized alloca in the entry block,
      # and multiple pointerof(x) calls share the same alloca.
      seen_addressof_operands = Set(ValueId).new
      func.blocks.each do |block|
        block.instructions.each do |inst|
          next unless inst.is_a?(AddressOf)
          operand_id = inst.operand
          next if @emitted_allocas.includes?(operand_id)
          next if seen_addressof_operands.includes?(operand_id)
          seen_addressof_operands << operand_id

          operand_type = @value_types[operand_id]? || TypeRef::POINTER
          alloca_type = @type_mapper.llvm_alloca_type(operand_type)
          alloca_type = "i8" if alloca_type == "void"
          alloca_name = "r#{operand_id}.addr"
          emit_raw "  %#{alloca_name} = alloca #{alloca_type}, align 8\n"
          @addressable_allocas[operand_id] = "%#{alloca_name}"
        end
      end

      # Create alloca slots for cross-block values to fix dominance issues
      @cross_block_values.each do |val_id|
        val_type = @value_types[val_id]?
        # Prefer the defining instruction type when it is a union.
        # @value_types can be polluted by later ABI/type adaptation, which may
        # collapse a union value to i32/ptr and break union_is/union_unwrap.
        if def_inst = find_def_inst(val_id)
          def_type = def_inst.type
          def_llvm = @type_mapper.llvm_type(def_type)
          if val_type
            current_llvm = @type_mapper.llvm_type(val_type)
            if def_llvm.includes?(".union") && !current_llvm.includes?(".union")
              val_type = def_type
            end
          else
            val_type = def_type
          end
        end
        next unless val_type
        llvm_type = @type_mapper.llvm_type(val_type)
        if ENV["DEBUG_SLOT_TYPES"]? && func.name.includes?("Path#basename") && val_id == 35
          val_type_name = @module.type_registry.get(val_type).try(&.name) || "unknown"
          STDERR.puts "[SLOT_TYPES] slot val_id=#{val_id} val_type=#{val_type} #{val_type_name} llvm=#{llvm_type}"
        end
        llvm_type = "i64" if llvm_type == "void"  # fallback
        slot_name = "%r#{val_id}.slot"
        @cross_block_slots[val_id] = "r#{val_id}.slot"
        @cross_block_slot_types[val_id] = llvm_type  # Record allocation type for consistent loads
        @cross_block_slot_type_refs[val_id] = val_type
        emit_raw "  #{slot_name} = alloca #{llvm_type}, align 8\n"
        # Initialize to zero/null to avoid undef on unexecuted paths
        init_val = case llvm_type
                   when "ptr" then "null"
                   when "float", "double" then "0.0"
                   when .starts_with?('i') then "0"
                   when .includes?(".union") then "zeroinitializer"
                   else "0"
                   end
        emit_raw "  store #{llvm_type} #{init_val}, ptr #{slot_name}\n"
      end
    end

    # Prepass: identify phi predecessor loads needed for cross-block values
    # This is critical because blocks are emitted in order, but CFG order may differ
    # e.g., bb2 may be emitted before bb7, but bb2's phi needs bb7's predecessor load
    private def prepass_collect_phi_predecessor_loads(func : Function)
      @phi_predecessor_loads.clear

      # For each block with a phi, check if any incoming value is cross-block
      func.blocks.each do |block|
        block.instructions.each do |inst|
          next unless inst.is_a?(Phi)
          phi = inst

          phi.incoming.each do |(pred_block_id, val_id)|
            # Check if value is cross-block (needs slot)
            slot_name = @cross_block_slots[val_id]?
            next unless slot_name

            # Don't add duplicate entries
            next if @phi_predecessor_loads.has_key?({pred_block_id, val_id})

            # Record that this predecessor block needs to emit a load for this value
            load_name = "r#{val_id}.phi_load.#{pred_block_id}"
            @phi_predecessor_loads[{pred_block_id, val_id}] = load_name
          end
        end
      end
    end

    # Prepass: identify phi predecessor conversions needed for fixed-type values (params, ExternCalls)
    # These values have fixed LLVM types and need explicit sext/zext in predecessor blocks
    private def prepass_collect_phi_predecessor_conversions(func : Function)
      @phi_predecessor_conversions.clear

      func.blocks.each do |block|
        block.instructions.each do |inst|
          next unless inst.is_a?(Phi)
          phi = inst

          phi_llvm_type = @type_mapper.llvm_type(phi.type)
          next if phi_llvm_type == "void"
          next unless phi_llvm_type.starts_with?('i') && !phi_llvm_type.includes?('.')
          next if phi_llvm_type == "i1"
          phi_bits = phi_llvm_type[1..-1].to_i? || 32

          phi.incoming.each do |(pred_block_id, val_id)|
            # Don't add duplicate entries
            next if @phi_predecessor_conversions.has_key?({pred_block_id, val_id})

            # Check if this value needs conversion via phi_zext_conversions
            if conversion = @phi_zext_conversions[val_id]?
              from_bits, to_bits = conversion
              conv_name = "r#{val_id}.phi_conv.#{pred_block_id}"
              @phi_predecessor_conversions[{pred_block_id, val_id}] = {conv_name, from_bits, to_bits}
              next
            end

            # Also check for general int width mismatches (e.g. i32 value in i8 phi)
            val_type = @value_types[val_id]?
            next unless val_type
            val_llvm = @type_mapper.llvm_type(val_type)
            next unless val_llvm.starts_with?('i') && !val_llvm.includes?('.')
            val_bits = val_llvm[1..-1].to_i? || 32
            if val_bits != phi_bits
              conv_name = "r#{val_id}.conv.#{pred_block_id}"
              @phi_predecessor_conversions[{pred_block_id, val_id}] = {conv_name, val_bits, phi_bits}
            end
          end
        end
      end
    end

    # Prepass: identify ptr/void values that must be wrapped into unions before phi nodes
    # This prevents ptr-typed phi fallback when a union expects a non-nil payload.
    private def prepass_collect_phi_predecessor_union_wraps(func : Function)
      @phi_predecessor_union_wraps.clear

      func.blocks.each do |block|
        block.instructions.each do |inst|
          next unless inst.is_a?(Phi)
          phi = inst

          phi_llvm_type = @type_mapper.llvm_type(phi.type)
          next unless phi_llvm_type.includes?(".union")

          union_descriptor = @module.get_union_descriptor(phi.type)
          next unless union_descriptor

          phi.incoming.each do |(pred_block_id, val_id)|
            # Don't add duplicate entries
            next if @phi_predecessor_union_wraps.has_key?({pred_block_id, val_id})

            val_type = @value_types[val_id]?
            val_llvm_type = val_type ? @type_mapper.llvm_type(val_type) : nil
            const_val = @constant_values[val_id]?

            # Skip if already union-typed
            next if val_llvm_type && val_llvm_type.includes?(".union")

            # Determine if this value should be wrapped into union
            is_nil_like = (val_type == TypeRef::NIL) || (val_llvm_type == "void") || (const_val == "null")

            variant = nil
            if is_nil_like
              variant = union_descriptor.variants.find { |v| v.type_ref == TypeRef::NIL }
            elsif val_type
              # Prefer exact type_ref match if available
              variant = union_descriptor.variants.find { |v| v.type_ref == val_type }
            end

            # Fallback: if value is ptr-like, map to first ptr-LLVM variant
            if variant.nil? && (val_llvm_type == "ptr" || val_llvm_type.nil?)
              variant = union_descriptor.variants.find do |v|
                v.type_ref != TypeRef::NIL && @type_mapper.llvm_type(v.type_ref) == "ptr"
              end
            end

            next unless variant

            wrap_name = "r#{val_id}.phi_union_wrap.#{pred_block_id}"
            @phi_predecessor_union_wraps[{pred_block_id, val_id}] = {wrap_name, phi.type, variant.type_id}
          end
        end
      end
    end

    # Pre-pass: identify union-typed phi incoming values that have a DIFFERENT union type than the phi.
    # Both types share the same physical layout {i32, [N x i8]} so we reinterpret through memory.
    private def prepass_collect_phi_union_to_union_converts(func : Function)
      @phi_union_to_union_converts.clear

      func.blocks.each do |block|
        block.instructions.each do |inst|
          next unless inst.is_a?(Phi)
          phi = inst

          phi_llvm_type = @type_mapper.llvm_type(phi.type)
          next unless phi_llvm_type.includes?(".union")

          phi.incoming.each do |(pred_block_id, val_id)|
            # Skip if already handled by union wrap prepass
            next if @phi_predecessor_union_wraps.has_key?({pred_block_id, val_id})

            val_type = @value_types[val_id]?
            val_llvm_type = val_type ? @type_mapper.llvm_type(val_type) : nil

            # Only handle union-to-different-union case
            next unless val_llvm_type && val_llvm_type.includes?(".union") && val_llvm_type != phi_llvm_type

            # Check if conversion for this dst type already registered
            existing = @phi_union_to_union_converts[{pred_block_id, val_id}]?
            if existing && existing.any? { |e| e[2] == phi_llvm_type }
              next
            end

            suffix = existing ? ".#{existing.size}" : ""
            convert_name = "r#{val_id}.u2u.#{pred_block_id}#{suffix}"
            arr = @phi_union_to_union_converts[{pred_block_id, val_id}] ||= [] of {String, String, String}
            arr << {convert_name, val_llvm_type, phi_llvm_type}
          end
        end
      end
    end

    # Prepass: identify union-typed incoming values that will feed ptr-emitted phi nodes.
    # These extracts must be known before block emission; otherwise predecessor blocks may
    # already be emitted and `%rN.u2p.B` will become undefined in phi incomings.
    private def prepass_collect_phi_union_to_ptr_extracts(func : Function)
      @phi_union_to_ptr_extracts.clear

      func.blocks.each do |block|
        block.instructions.each do |inst|
          next unless inst.is_a?(Phi)
          phi = inst

          phi_mir_type = @type_mapper.llvm_type(phi.type)
          prepass_phi_type = @value_types[phi.id]?

          emit_ptr_phi = false
          if prepass_phi_type == TypeRef::POINTER && phi_mir_type.includes?(".union")
            emit_ptr_phi = true
          elsif phi_mir_type == "ptr"
            emit_ptr_phi = true
          elsif phi_mir_type.includes?(".union")
            union_descriptor = @module.get_union_descriptor(phi.type)
            emit_ptr_phi = phi.incoming.any? do |(pred_block_id, val_id)|
              next false if @phi_predecessor_union_wraps.has_key?({pred_block_id, val_id})

              val_type = @value_types[val_id]?
              const_val = @constant_values[val_id]?
              val_llvm_type = val_type ? @type_mapper.llvm_type(val_type) : nil

              if val_type && union_descriptor
                next false if union_descriptor.variants.any? { |variant| variant.type_ref == val_type }
              end

              if val_llvm_type != "ptr"
                if def_inst = find_def_inst(val_id)
                  if def_inst.is_a?(UnionUnwrap) && @type_mapper.llvm_type(def_inst.type) == "ptr"
                    next true
                  end
                end
              end

              (val_llvm_type == "ptr") ||
                (val_type.nil? && const_val.nil?) ||
                (const_val == "null")
            end
          end

          next unless emit_ptr_phi

          phi.incoming.each do |(pred_block_id, val_id)|
            # The phi emission normalizes UnionUnwrap values back to their source union
            # (see emit_phi lines 8210+). Account for this: if val is a UnionUnwrap
            # and the phi's MIR type matches the source union, the phi will reference the
            # source union value after normalization. Pre-register u2p for it.
            check_vals = [val_id]
            if phi_mir_type.includes?(".union")
              if uw_inst = find_def_inst(val_id)
                if uw_inst.is_a?(UnionUnwrap)
                  union_val = uw_inst.union_value
                  union_type_ref = @value_types[union_val]?
                  union_llvm_type = union_type_ref ? @type_mapper.llvm_type(union_type_ref) : nil
                  if union_llvm_type == phi_mir_type
                    check_vals << union_val
                  end
                end
              end
            end

            check_vals.each do |vid|
              next if @phi_union_to_ptr_extracts.has_key?({pred_block_id, vid})

              vt = @value_types[vid]?
              slt = @cross_block_slot_types[vid]?

              # Only use @value_types and @cross_block_slot_types — not MIR type,
              # which may differ from the actual emitted type after prepass resolution.
              actual_union_type = nil.as(String?)
              if vt
                vlt = @type_mapper.llvm_type(vt)
                actual_union_type = vlt if vlt.includes?(".union")
              end
              if actual_union_type.nil? && slt && slt.includes?(".union")
                actual_union_type = slt
              end
              if actual_union_type.nil?
                if def_inst = find_def_inst(vid)
                  def_llvm_type = @type_mapper.llvm_type(def_inst.type)
                  if def_llvm_type.includes?(".union")
                    actual_union_type = def_llvm_type
                  elsif def_inst.is_a?(UnionWrap)
                    uw_type = @type_mapper.llvm_type(def_inst.union_type)
                    actual_union_type = uw_type if uw_type.includes?(".union")
                  end
                end
              end
              next unless actual_union_type

              extract_name = "r#{vid}.u2p.#{pred_block_id}"
              @phi_union_to_ptr_extracts[{pred_block_id, vid}] = {extract_name, actual_union_type}
            end
          end
        end
      end
    end

    # Prepass: for primitive-typed phis (i32, i1, etc.) whose incoming values
    # are stored in union-typed cross-block slots, schedule payload extraction
    # in the predecessor block. Without this, the extraction would only be
    # scheduled during phi emission (too late if the predecessor was already emitted).
    private def prepass_collect_phi_union_payload_extracts(func : Function)
      func.blocks.each do |block|
        block.instructions.each do |inst|
          next unless inst.is_a?(Phi)
          phi = inst

          phi_type = @type_mapper.llvm_type(phi.type)
          # Only handle primitive scalar phi types (not union, not ptr, not void —
          # those either have their own prepasses or aren't loadable)
          next if phi_type == "ptr" || phi_type == "void" || phi_type.includes?(".union")

          phi.incoming.each do |(pred_block_id, val_id)|
            # Skip if already registered
            next if @phi_union_payload_extracts.has_key?({pred_block_id, val_id})

            slot_type = @cross_block_slot_types[val_id]?
            next unless slot_type && slot_type.includes?(".union")

            # The value is stored in a union slot but the phi expects a primitive.
            # Schedule extraction of the payload from the union.
            extract_name = "r#{val_id}.phi_extract.#{pred_block_id}"
            load_name = "r#{val_id}.phi_load.#{pred_block_id}"
            @phi_union_payload_extracts[{pred_block_id, val_id}] = {extract_name, load_name, phi_type}
          end
        end
      end
    end

    # Pre-pass to collect constant values AND all value types before emitting IR
    # This resolves forward reference issues with phi nodes
    private def prepass_collect_constants(func : Function)
      func_name = mangle_function_name(func.name)
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
          elsif inst.is_a?(Call)
            # For Call instructions, check the callee's actual return type
            # If callee returns void, mark as void even if inst.type says otherwise
            callee_func = @module.functions.find { |f| f.id == inst.callee }
            if callee_func
              if ENV["DEBUG_SLOT_TYPES"]? && func.name.includes?("Path#basename")
                inst_type_name = @module.type_registry.get(inst.type).try(&.name) || "unknown"
                STDERR.puts "[SLOT_TYPES] func=#{func.name} call_id=#{inst.id} callee=#{callee_func.name}"
                STDERR.puts "[SLOT_TYPES] inst_type=#{inst.type} #{inst_type_name} llvm=#{@type_mapper.llvm_type(inst.type)}"
              end
              callee_ret_type = @type_mapper.llvm_type(callee_func.return_type)
              callee_name = mangle_function_name(callee_func.name)
              # Known void functions (inspect, puts, print, etc.)
              # Check various naming patterns since functions can be mangled differently
              is_known_void = callee_name == "inspect" ||
                              callee_name.includes?("inspect_") ||
                              callee_name.includes?("#inspect") ||
                              callee_name.includes?("_inspect") ||
                              callee_name == "puts" ||
                              callee_name.includes?("puts_") ||
                              callee_name == "print" ||
                              callee_name.includes?("print_") ||
                              callee_name == "p" ||
                              callee_name.ends_with?("_p")
              if callee_ret_type == "void" || is_known_void
                inst_type_str = @type_mapper.llvm_type(inst.type)
                # Only override to VOID when the MIR type is actually void/nil or a pointer fallback.
                # Preserve concrete/non-pointer types (e.g., union/struct/tuple) inferred at callsite.
                if inst.type == TypeRef::VOID || inst.type == TypeRef::NIL || inst_type_str == "ptr"
                  effective_type = TypeRef::VOID
                end
              end
            else
              # Callee not found - check if MIR type is void
              inst_type_str = @type_mapper.llvm_type(inst.type)
              if inst_type_str == "void"
                effective_type = TypeRef::VOID
              end
            end
          elsif inst.is_a?(ExternCall)
            # For ExternCall, look up the actual function return type from module
            # This handles cases like String#index returning Int32|Nil
            mangled_extern_name = @type_mapper.mangle_name(inst.extern_name)
            # Skip suffix matching for:
            # 1. Operator methods (avoid accidental suffix hits)
            # 2. Likely C library functions (simple names without namespace/type hints)
            extern_name = inst.extern_name
            receiver_and_method = extract_receiver_and_method(extern_name)
            is_operator = false
            if receiver_and_method
              _receiver_name, method_core = receiver_and_method
              is_operator = operator_method?(method_core)
            else
              # Bare operator names still count as operator methods.
              is_operator = operator_method?(extern_name)
            end
            is_c_lib_function = !crystalish_extern_name?(extern_name)
            extern_method_core = method_core_from_name(extern_name)
            extern_suffix = suffix_after_dollar(extern_name)
            fuzzy_match_allowed = extern_suffix && extern_fuzzy_match_eligible?(extern_name, is_operator, is_c_lib_function)

            # Search for exact match OR constrained fuzzy match.
            matching_func = @module.functions.find do |f|
              mangled = @type_mapper.mangle_name(f.name)
              next true if mangled == mangled_extern_name || f.name == extern_name
              next false unless fuzzy_match_allowed
              extern_fuzzy_matches_candidate?(extern_name, extern_method_core, extern_suffix.not_nil!, f.name)
            end
            if matching_func
              # Use the actual function return type
              func_ret_type = matching_func.return_type
              func_ret_llvm = @type_mapper.llvm_type(func_ret_type)
              if func_ret_llvm != "void"
                effective_type = func_ret_type
              end
            else
              if effective_type == TypeRef::VOID
                # Default fallback: use inst.type
                extern_type_str = @type_mapper.llvm_type(inst.type)
                if extern_type_str == "void"
                  effective_type = TypeRef::VOID
                end
              end
            end
            # Apply type suffix heuristics AFTER function matching to ensure
            # prepass and emission agree on return types.  The emission path
            # unconditionally applies suffix overrides, so the prepass must too.
            # This fixes slot-type / call-type mismatches for primitive-returning
            # methods on struct types (e.g., Array(UInt32)#unsafe_fetch$Int32
            # where the MIR function returns a struct type mapped to ptr, but
            # the real LLVM return type is i32).
            if suffix2 = suffix_after_dollar(extern_name)
              if !suffix2.includes?('_')
                case suffix2
                when "UInt64", "Int64"
                  effective_type = TypeRef::INT64
                when "UInt32", "Int32"
                  effective_type = TypeRef::INT32
                when "UInt16", "Int16"
                  effective_type = TypeRef::INT16
                when "UInt8", "Int8"
                  effective_type = TypeRef::INT8
                end
              end
            end
          elsif inst.is_a?(BinaryOp)
            if inst.op.eq? || inst.op.ne? || inst.op.lt? || inst.op.le? || inst.op.gt? || inst.op.ge?
              # Comparison ops always return bool
              effective_type = TypeRef::BOOL
            else
            # For BinaryOp, check if operands are union types
            # If so, the emit code will extract integer values, so result will be integer not union
            inst_type_str = @type_mapper.llvm_type(inst.type)
            if inst_type_str.includes?(".union")
              # Check operand types - they're other instruction IDs at this point
              # We'll need to look them up in value_types (already set for earlier instructions)
              left_type = @value_types[inst.left]?
              right_type = @value_types[inst.right]?
              left_llvm = left_type ? @type_mapper.llvm_type(left_type) : nil
              right_llvm = right_type ? @type_mapper.llvm_type(right_type) : nil
              # If either operand is union, emit code will extract to integer
              if (left_llvm && left_llvm.includes?(".union")) || (right_llvm && right_llvm.includes?(".union"))
                # Result will be integer, not union - use i64 as default
                effective_type = TypeRef::INT64
              end
              # NOTE: If inst.type is union but operands are scalars, keep the union type.
              # emit_binary_op will wrap the scalar result back into the union.
            end
            end
          end

          @value_types[inst.id] = effective_type
          if ENV["DEBUG_SLOT_TYPES"]? && func.name.includes?("Path#basename") && inst.id == 35
            inst_type_name = @module.type_registry.get(inst.type).try(&.name) || "unknown"
            eff_type_name = @module.type_registry.get(effective_type).try(&.name) || "unknown"
            STDERR.puts "[SLOT_TYPES] inst_id=#{inst.id} inst_type=#{inst.type} #{inst_type_name} llvm=#{@type_mapper.llvm_type(inst.type)}"
            STDERR.puts "[SLOT_TYPES] effective_type=#{effective_type} #{eff_type_name} llvm=#{@type_mapper.llvm_type(effective_type)}"
          end

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

      # Third pass: USE-BASED type inference for ANY instruction with void type
      # LLVM constraint: if a value is USED, it cannot be void
      # This applies universally to all instructions, not just calls
      used_values = collect_used_values(func)
      usage_contexts = collect_usage_contexts(func)

      func.blocks.each do |block|
        block.instructions.each do |inst|
          # Apply to ALL instructions - any void value that's used needs type inference
          current_type = @value_types[inst.id]?
          next unless current_type
          llvm_type = @type_mapper.llvm_type(current_type)
          next unless llvm_type == "void"

          # Skip Call/ExternCall instructions that are truly void
          # These are genuinely void and any usage is a MIR issue we handle with defaults
          if inst.is_a?(Call)
            callee_func = @module.functions.find { |f| f.id == inst.callee }
            if callee_func && @type_mapper.llvm_type(callee_func.return_type) == "void"
              next  # Genuinely void, don't infer type
            end
          elsif inst.is_a?(ExternCall)
            # ExternCall type comes from the instruction itself
            # But if used in a phi with a specific type, use that type instead
            if phi_type = usage_contexts[inst.id]?
              phi_llvm_type = @type_mapper.llvm_type(phi_type)
              if phi_llvm_type != "void"
                @value_types[inst.id] = phi_type
              end
            end
            next  # Don't fall through to generic inference
          end

          # If this void-typed value is used anywhere, it's not really void
          if used_values.includes?(inst.id)
            # Infer type from usage context
            inferred_type = usage_contexts[inst.id]? || TypeRef::POINTER
            @value_types[inst.id] = inferred_type
          end
        end
      end

      # Fourth pass: Propagate phi types to incoming values (regardless of current type)
      # This handles cases like UInt8___ (returns i8) used in phi expecting i32
      # BUT: For ExternCall instructions AND parameters, record zext/sext conversion needed
      # instead of changing type (their return/LLVM types are fixed)
      fixed_type_ids = Set(ValueId).new
      # Parameters have fixed LLVM types in function signature
      func.params.each do |param|
        fixed_type_ids << param.index
      end
      # ExternCall return types are fixed by the external function
      func.blocks.each do |block|
        block.instructions.each do |inst|
          if inst.is_a?(ExternCall)
            fixed_type_ids << inst.id
          end
        end
      end

      func.blocks.each do |block|
        block.instructions.each do |inst|
          next unless inst.is_a?(Phi)
          phi_type = inst.type
          phi_llvm_type = @type_mapper.llvm_type(phi_type)
          next if phi_llvm_type == "void"
          next unless phi_llvm_type.starts_with?('i') && !phi_llvm_type.includes?('.')
          next if phi_llvm_type == "i1"
          phi_bits = phi_llvm_type[1..-1].to_i? || 32

          inst.incoming.each do |(_, val)|
            current_type = @value_types[val]?
            current_llvm_type = current_type ? @type_mapper.llvm_type(current_type) : "void"

            # If phi expects different int type and both are integers
            if current_llvm_type != phi_llvm_type &&
               current_llvm_type.starts_with?('i') && !current_llvm_type.includes?('.')
              val_bits = current_llvm_type[1..-1].to_i? || 32

              if fixed_type_ids.includes?(val)
                # Fixed-type value (param or ExternCall) - record conversion needed
                if val_bits < phi_bits
                  @phi_zext_conversions[val] = {val_bits, phi_bits}
                elsif val_bits > phi_bits
                  # Need truncation (larger to smaller) - also record it
                  @phi_zext_conversions[val] = {val_bits, phi_bits}
                end
                # Keep the original type - don't overwrite
              else
                # Non-fixed-type - change type as before
                @value_types[val] = phi_type
              end
            end
          end
        end
      end

      # Fifth pass: Propagate union types FROM incoming values TO phi
      # If any incoming value is a union type, the phi must also be union type
      # This handles cases like String#index returning Int32|Nil into phi expecting Int32
      func.blocks.each do |block|
        block.instructions.each do |inst|
          next unless inst.is_a?(Phi)
          phi_type = @value_types[inst.id]? || inst.type
          phi_llvm_type = @type_mapper.llvm_type(phi_type)

          # Check if any incoming value is a union type
          inst.incoming.each do |(_, val)|
            val_type = @value_types[val]?
            next unless val_type
            val_llvm_type = @type_mapper.llvm_type(val_type)

            # If incoming value is union and phi is NOT union, upgrade phi to union
            if val_llvm_type.includes?(".union") && !phi_llvm_type.includes?(".union")
              @value_types[inst.id] = val_type
              phi_type = val_type
              phi_llvm_type = val_llvm_type
            end
          end
        end
      end

      # Sixth pass: Track phi nodes that have NIL-typed incoming values
      # This is used by return emission to correctly set union type_id for nilable returns
      func.blocks.each do |block|
        block.instructions.each do |inst|
          next unless inst.is_a?(Phi)

          nil_blocks = Set(BlockId).new
          inst.incoming.each do |(block_id, val)|
            val_type = @value_types[val]?
            const_val = @constant_values[val]?
            # Check if value is NIL type, VOID type, POINTER type with null value,
            # or a constant that is null
            is_nil_value = val_type == TypeRef::NIL ||
                           val_type == TypeRef::VOID ||
                           const_val == "null" ||
                           (val_type == TypeRef::POINTER && const_val == "null")

            # Also check for Cast instructions that produce ptr from 0 (nil pattern)
            # These have type POINTER but represent nil (inttoptr 0 to ptr)
            if !is_nil_value && val_type == TypeRef::POINTER
              # Look up the instruction that produced this value
              func.blocks.each do |b|
                b.instructions.each do |prod_inst|
                  if prod_inst.id == val && prod_inst.is_a?(Cast)
                    # IntToPtr with source value 0 is nil
                    if prod_inst.kind == CastKind::IntToPtr
                      src_const = @constant_values[prod_inst.value]?
                      if src_const == "0"
                        is_nil_value = true
                      end
                    end
                  end
                end
              end
            end

            if is_nil_value
              nil_blocks << block_id
            end
          end

          if !nil_blocks.empty?
            @phi_nil_incoming_blocks[inst.id] = nil_blocks
          end
        end
      end
    end

    # Collect all value IDs that are used (referenced by other instructions)
    private def collect_used_values(func : Function) : Set(UInt32)
      used = Set(UInt32).new

      func.blocks.each do |block|
        block.instructions.each do |inst|
          # Collect operand IDs based on instruction type
          case inst
          when Phi
            inst.incoming.each { |(_, val)| used << val }
          when BinaryOp
            used << inst.left << inst.right
          when UnaryOp
            used << inst.operand
          when Call
            inst.args.each { |arg| used << arg }
          when ExternCall
            inst.args.each { |arg| used << arg }
          when AddressOf
            used << inst.operand
          when IndirectCall
            inst.args.each { |arg| used << arg }
            used << inst.callee_ptr
          when Store
            used << inst.value << inst.ptr
          when MemCopy
            used << inst.dst << inst.src
          when Load
            used << inst.ptr
          when GetElementPtr
            used << inst.base
            inst.indices.each { |idx| used << idx }
          when Cast
            used << inst.value
          when ArrayGet
            used << inst.array_value << inst.index_value
          when ArraySet
            used << inst.array_value << inst.index_value << inst.value_id
          when ArraySize
            used << inst.array_value
          when ArraySetSize
            used << inst.array_value << inst.size_value
          when ArrayNew
            used << inst.capacity_value
          when ArrayLiteral
            inst.elements.each { |elem| used << elem }
          when UnionIs
            used << inst.union_value
          when UnionUnwrap
            used << inst.union_value
          when UnionWrap
            used << inst.value
          end
        end

        # Also check terminator
        case term = block.terminator
        when Return
          if val = term.value
            used << val
          end
        when Branch
          used << term.condition
        end
      end

      used
    end

    # Collect usage contexts to infer types (ptr vs int types)
    private def collect_usage_contexts(func : Function) : Hash(UInt32, TypeRef)
      contexts = Hash(UInt32, TypeRef).new

      func.blocks.each do |block|
        block.instructions.each do |inst|
          case inst
          when Phi
            phi_type = inst.type
            inst.incoming.each do |(_, val)|
              # If used in phi, inherit phi's type (unless void)
              if @type_mapper.llvm_type(phi_type) != "void"
                contexts[val] ||= phi_type
              else
                contexts[val] ||= TypeRef::POINTER
              end
            end
          when BinaryOp
            # Binary op: infer void operand types from known operand or result type
            left_type = @value_types[inst.left]?
            right_type = @value_types[inst.right]?
            result_type = inst.type
            left_is_void = !left_type || @type_mapper.llvm_type(left_type) == "void"
            right_is_void = !right_type || @type_mapper.llvm_type(right_type) == "void"

            # Determine the best known type to propagate
            inferred = if !left_is_void && left_type
                         left_type
                       elsif !right_is_void && right_type
                         right_type
                       elsif @type_mapper.llvm_type(result_type) != "void" && @type_mapper.llvm_type(result_type) != "i1"
                         # Use result type unless it's void or bool (comparison result)
                         result_type
                       else
                         TypeRef::INT64  # Default for arithmetic ops
                       end

            contexts[inst.left] ||= inferred
            contexts[inst.right] ||= inferred
          when Store
            # store TYPE %val, ptr %ptr → val is TYPE
            val_type = @value_types[inst.value]?
            if val_type && @type_mapper.llvm_type(val_type) != "void"
              contexts[inst.value] ||= val_type
            else
              contexts[inst.value] ||= TypeRef::POINTER
            end
          when MemCopy
            contexts[inst.dst] ||= TypeRef::POINTER
            contexts[inst.src] ||= TypeRef::POINTER
          when Cast
            # Cast to ptr → source might be ptr-like
            if inst.type == TypeRef::POINTER
              contexts[inst.value] ||= TypeRef::POINTER
            end
          end
        end

        # Return value type
        if term = block.terminator.as?(Return)
          if val = term.value
            ret_type = func.return_type
            if @type_mapper.llvm_type(ret_type) != "void"
              contexts[val] ||= ret_type
            end
          end
        end
      end

      contexts
    end

    # Detect values that are defined in one block but used in another
    # without guaranteed dominance. These need alloca slots for correctness.
    private def prepass_detect_cross_block_values(func : Function)
      return if func.blocks.size <= 1  # Single block - no cross-block issues

      entry_block_id = func.blocks.first?.try(&.id) || 0_u32

      # Pass 1: Record which block each value is defined in
      func.blocks.each do |block|
        block.instructions.each do |inst|
          @value_def_block[inst.id] = block.id
        end
      end

      # Pass 2: Find uses and check for cross-block references
      # Focus on Load/ArrayGet results which are commonly problematic
      func.blocks.each do |block|
        block.instructions.each do |inst|
          # Check each operand
          operand_ids = case inst
                        when BinaryOp     then [inst.left, inst.right]
                        when UnaryOp      then [inst.operand]
                        when Cast         then [inst.value]
                        when Call         then inst.args.to_a
                        when ExternCall   then inst.args.to_a
                        when IndirectCall then inst.args.to_a + [inst.callee_ptr]
                        when Store        then [inst.value, inst.ptr]
                        when MemCopy      then [inst.dst, inst.src]
                        when GlobalStore  then [inst.value]
                        when AtomicStore  then [inst.value, inst.ptr]
                        when Load         then [inst.ptr]
                        when GetElementPtr        then [inst.base]
                        when GetElementPtrDynamic then [inst.base, inst.index]
                        when UnionWrap    then [inst.value]
                        when UnionIs      then [inst.union_value]
                        when UnionUnwrap  then [inst.union_value]
                        when UnionTypeIdGet then [inst.union_value]
                        when Select       then [inst.condition, inst.then_value, inst.else_value]
                        when ArrayGet     then [inst.array_value, inst.index_value]
                        when ArraySet     then [inst.array_value, inst.index_value, inst.value_id]
                        when ArraySize    then [inst.array_value]
                        when ArraySetSize then [inst.array_value, inst.size_value]
                        when ArrayNew     then [inst.capacity_value]
                        when ArrayLiteral then inst.elements.to_a
                        when AddressOf    then [inst.operand]
                        when Phi          then inst.incoming.map { |(_, v)| v }
                        when StringInterpolation then inst.parts.to_a
                        else              [] of ValueId
                        end

          operand_ids.each do |op_id|
            def_block = @value_def_block[op_id]?
            next unless def_block

            # Value defined in different block than where it's used
            if def_block != block.id
              next if def_block == entry_block_id

              # Check if definition block is bb0 (block id 0 or 1 typically after entry)
              # Conservative: any non-entry definition used cross-block is suspect
              # Check the instruction type that defines this value
              def_inst = nil
              func.blocks.each do |b|
                def_inst = b.instructions.find { |i| i.id == op_id }
                break if def_inst
              end

              # Flag ALL value-producing instructions that are used cross-block
              # This is more conservative but catches all dominance issues
              # Exclude: Store, Free, RCIncrement, RCDecrement, GlobalStore, AtomicStore (no values)
              # Note: We REMOVED the void check here because:
              # 1. If an instruction's result is used as an operand, it MUST produce a value
              # 2. MIR type might be VOID as placeholder but actual callee returns non-void
              # 3. The usage proves this instruction produces a usable value
              is_non_value_inst = def_inst.is_a?(Store) || def_inst.is_a?(Free) ||
                                  def_inst.is_a?(RCIncrement) || def_inst.is_a?(RCDecrement) ||
                                  def_inst.is_a?(GlobalStore) || def_inst.is_a?(AtomicStore)
              if !is_non_value_inst
                @cross_block_values << op_id
              end
            end
          end
        end

        # Also check phi incoming values for pass-through block issues
        # If phi says [%val, %blockX] but %val is defined in %blockY != %blockX,
        # this is a pass-through situation that causes dominance errors
        block.instructions.each do |inst|
          next unless inst.is_a?(Phi)
          inst.incoming.each do |(from_block, val_id)|
            def_block = @value_def_block[val_id]?
            next unless def_block

            # If the value is defined in a different block than the "from" block,
            # this is a pass-through situation - the value needs an alloca slot
            if def_block != from_block
              # Also exclude parameters (which have no def_block in instructions)
              next if def_block == entry_block_id

              # Find the defining instruction to check its type
              def_inst = nil
              func.blocks.each do |b|
                def_inst = b.instructions.find { |i| i.id == val_id }
                break if def_inst
              end

              # Add to cross_block_values if it's a value-producing instruction.
              #
              # IMPORTANT: Don't trust MIR's `Call#type` here. In many cases it can be `VOID`
              # as a placeholder even when the callee returns a value (the LLVM backend
              # corrects this via `prepass_collect_constants` and `@value_types`).
              if def_inst
                is_non_value_inst = def_inst.is_a?(Store) || def_inst.is_a?(Free) ||
                                    def_inst.is_a?(RCIncrement) || def_inst.is_a?(RCDecrement) ||
                                    def_inst.is_a?(GlobalStore) || def_inst.is_a?(AtomicStore)
                @cross_block_values << val_id unless is_non_value_inst
              end
            end
          end
        end

        # Check terminator
        case term = block.terminator
        when Branch
          cond_id = term.condition
          def_block = @value_def_block[cond_id]?
          if def_block && def_block != block.id && def_block != entry_block_id
            def_inst = nil
            func.blocks.each do |b|
              def_inst = b.instructions.find { |i| i.id == cond_id }
              break if def_inst
            end
            is_non_value_inst = def_inst.is_a?(Store) || def_inst.is_a?(Free) ||
                                def_inst.is_a?(RCIncrement) || def_inst.is_a?(RCDecrement) ||
                                def_inst.is_a?(GlobalStore) || def_inst.is_a?(AtomicStore)
            if !is_non_value_inst
              @cross_block_values << cond_id
            end
          end
        when Return
          # Return value used from different block
          if ret_val = term.value
            def_block = @value_def_block[ret_val]?
            if def_block && def_block != block.id && def_block != entry_block_id
              # Any value-producing instruction that's used cross-block in return
              @cross_block_values << ret_val
            end
          end
        end
      end
    end

    # Pre-pass: Infer binary operation result types considering type widening
    # This is needed for phi nodes that reference binary ops in later blocks
    private def prepass_infer_binary_op_types(func : Function)
      # Multiple passes to handle dependencies between instructions
      # Iterate until no changes occur (fixed point)
      3.times do |pass|
        changed = false
        func.blocks.each do |block|
          block.instructions.each do |inst|
            case inst
            when BinaryOp
              if inst.op.eq? || inst.op.ne? || inst.op.lt? || inst.op.le? || inst.op.gt? || inst.op.ge?
                if @value_types[inst.id]? != TypeRef::BOOL
                  @value_types[inst.id] = TypeRef::BOOL
                  changed = true
                end
                next
              end
              # Check if both operands have known types and compute widened type
              left_type = @value_types[inst.left]? || inst.type
              right_type = @value_types[inst.right]? || inst.type

              # Determine if this is an arithmetic operation
              is_arithmetic = inst.op.add? || inst.op.sub? || inst.op.mul? ||
                              inst.op.div? || inst.op.rem? || inst.op.shl? ||
                              inst.op.shr? || inst.op.and? || inst.op.or? || inst.op.xor?

              # IMPORTANT: Match emit_binary_op behavior
              # - void types default to INT64
              # - ptr types for arithmetic ops must be INT32 or INT64
              effective_type = inst.type
              effective_llvm = @type_mapper.llvm_type(effective_type)
              if effective_llvm == "void"
                effective_type = TypeRef::INT64
              elsif effective_llvm == "ptr" && is_arithmetic
                # Arithmetic ops can't use ptr type - use i32 as default
                effective_type = TypeRef::INT32
              end
              if @type_mapper.llvm_type(left_type) == "void"
                left_type = TypeRef::INT64
              end
              if @type_mapper.llvm_type(right_type) == "void"
                right_type = TypeRef::INT64
              end

              # The MIR BinaryOp type is stored in inst.type
              # But we need to track when widening will occur

              left_llvm = @type_mapper.llvm_type(left_type)
              right_llvm = @type_mapper.llvm_type(right_type)
              result_llvm = @type_mapper.llvm_type(effective_type)

              # Check for int type size mismatch that will cause widening
              if left_llvm.starts_with?('i') && !left_llvm.includes?('.') &&
                 right_llvm.starts_with?('i') && !right_llvm.includes?('.')
                left_bits = left_llvm[1..].to_i? || 32
                right_bits = right_llvm[1..].to_i? || 32
                max_bits = {left_bits, right_bits}.max

                # If widening will occur, record the widened type
                # Propagate unsigned-ness: if either operand is unsigned, result is unsigned
                # (matches Crystal's promotion rules for mixed signed/unsigned arithmetic)
                either_unsigned = unsigned_type_ref?(left_type) || unsigned_type_ref?(right_type)
                declared_bits = result_llvm.starts_with?('i') ? (result_llvm[1..].to_i? || 32) : 32
                if max_bits > declared_bits
                  # Will be widened to max_bits
                  actual_type = if either_unsigned && is_arithmetic
                                  case max_bits
                                  when 8  then TypeRef::UINT8
                                  when 16 then TypeRef::UINT16
                                  when 32 then TypeRef::UINT32
                                  when 64 then TypeRef::UINT64
                                  else TypeRef::UINT64
                                  end
                                else
                                  case max_bits
                                  when 8 then TypeRef::INT8
                                  when 16 then TypeRef::INT16
                                  when 32 then TypeRef::INT32
                                  when 64 then TypeRef::INT64
                                  else TypeRef::INT64
                                  end
                                end
                  if @value_types[inst.id]? != actual_type
                    @value_types[inst.id] = actual_type
                    changed = true
                  end
                else
                  # No widening — use effective type but propagate unsigned-ness
                  result_type_ref = effective_type
                  if either_unsigned && is_arithmetic
                    result_type_ref = case effective_type
                                     when TypeRef::INT8  then TypeRef::UINT8
                                     when TypeRef::INT16 then TypeRef::UINT16
                                     when TypeRef::INT32 then TypeRef::UINT32
                                     when TypeRef::INT64 then TypeRef::UINT64
                                     else effective_type
                                     end
                  end
                  if @value_types[inst.id]? != result_type_ref
                    # Always update since prepass_collect_constants may have set wrong type
                    @value_types[inst.id] = result_type_ref
                    changed = true
                  end
                end
              elsif @value_types[inst.id]? != effective_type
                # Non-int binary op, use effective type (void→INT64)
                @value_types[inst.id] = effective_type
                changed = true
              end
            else
              # For non-BinaryOp, record declared type if not already set
              if !@value_types.has_key?(inst.id)
                @value_types[inst.id] = inst.type
                changed = true
              end
            end
          end
        end
        break unless changed
      end
    end

    private def reset_value_names(func : Function)
      @value_names.clear
      @block_names.clear
      @constant_values.clear
      @value_types.clear
      @void_values.clear
      @array_info.clear
      @alloc_types.clear
      @alloc_element_types.clear
      @inttoptr_value_ids.clear
      @phi_zext_conversions.clear
      @zext_value_names.clear
      @phi_nil_incoming_blocks.clear
      @phi_predecessor_conversions.clear
      @cond_counter = 0  # Reset for each function

      func.params.each do |param|
        @value_names[param.index] = sanitize_llvm_local_name(param.name)
        # Use POINTER for void params (void is not valid for values)
        param_type = param.type == TypeRef::VOID ? TypeRef::POINTER : param.type
        @value_types[param.index] = param_type
        # Parameters typed as ptr may hold packed scalars (inttoptr'd at call site).
        # Mark them so ptr→int conversion uses ptrtoint instead of load.
        if @type_mapper.llvm_type(param_type) == "ptr"
          @inttoptr_value_ids.add(param.index)
        end
      end

      func.blocks.each do |block|
        @block_names[block.id] = "bb#{block.id}"
      end
    end

    private def emit_block(block : BasicBlock, func : Function)
      emit_raw "#{@block_names[block.id]}:\n"
      @indent = 1
      @current_block_id = block.id

      # LLVM requires all phi nodes to be at the top of the basic block.
      # Emit phi nodes first, then other instructions.
      phi_insts = [] of Value
      non_phi_insts = [] of Value
      block.instructions.each do |inst|
        if inst.is_a?(Phi)
          phi_insts << inst
        else
          non_phi_insts << inst
        end
      end

      # Emit phi nodes first (without cross-block stores - those go after all phis)
      @in_phi_block = true
      @deferred_phi_stores.clear
      @deferred_phi_store_ops.clear
      phi_insts.each do |inst|
        emit_instruction(inst, func)
      end
      @in_phi_block = false

      # Now emit deferred stores for cross-block phi values
      @deferred_phi_stores.each do |store_stmt|
        emit store_stmt
      end
      @deferred_phi_stores.clear

      # Emit deferred slot store operations (wrapping + store) that were skipped during PHI emission
      @deferred_phi_store_ops.each do |(inst_id, val_name, slot_name)|
        emit_cross_block_slot_store(inst_id, val_name, slot_name)
      end
      @deferred_phi_store_ops.clear

      # TSan: emit function entry after phi nodes (if first block)
      if @tsan_needs_func_entry
        emit "; TSan function entry"
        emit "%__tsan_func_ptr = bitcast ptr @#{@current_func_name} to ptr"
        emit "call void @__tsan_func_entry(ptr %__tsan_func_ptr)"
        @tsan_needs_func_entry = false
      end

      # Emit non-phi instructions
      non_phi_insts.each do |inst|
        emit_instruction(inst, func)
      end

      # Emit predecessor loads for cross-block phi incoming values
      # This must happen BEFORE the terminator (branch) to successor blocks
      emit_phi_predecessor_loads(block)

      # Emit type conversions for fixed-type values (params, ExternCalls) used in successor phi nodes
      emit_phi_predecessor_conversions(block)
      # Emit union wraps for ptr/void values used in successor union phi nodes
      emit_phi_predecessor_union_wraps(block)
      # Emit union-to-ptr extractions for union values used in successor ptr phi nodes
      emit_phi_union_to_ptr_extracts(block)
      # Emit union-to-union reinterpretations for union values used in successor union phi nodes with different type
      emit_phi_union_to_union_converts(block)
      # Emit union payload extractions for union-slotted values used in primitive phi nodes
      emit_phi_union_payload_extracts(block)

      emit_terminator(block.terminator)
      @indent = 0
      @current_block_id = nil
    end

    # Emit loads from slots for cross-block values used in phi nodes
    # The prepass already identified which (pred_block, val) pairs need loads
    # Now we emit the actual load instructions for this block
    private def emit_phi_predecessor_loads(block : BasicBlock)
      @phi_predecessor_loads.each do |(key, load_name)|
        pred_block_id, val_id = key
        # Only emit loads for THIS block
        next unless pred_block_id == block.id

        # Get the slot for this value
        slot_name = @cross_block_slots[val_id]?
        next unless slot_name

        # Emit load from slot - use the ALLOCATION type, not current @value_types
        # This is critical because @value_types may be updated during emission,
        # but the slot was allocated with the prepass type
        llvm_type = @cross_block_slot_types[val_id]? || "i64"
        emit "%#{load_name} = load #{llvm_type}, ptr %#{slot_name}"
        record_emitted_type("%#{load_name}", llvm_type)
        # Pre-emit an int->ptr view for ptr phis that consume integer-typed slots.
        # This keeps phi incoming values in predecessor blocks and avoids
        # dominance/type issues when ptr values are spill-encoded as integers.
        declared_val_type = @value_types[val_id]?
        if declared_val_type == TypeRef::POINTER &&
           llvm_type.starts_with?('i') &&
           !llvm_type.includes?(".union")
          ptr_view = "%#{load_name}.ptr"
          emit "#{ptr_view} = inttoptr #{llvm_type} %#{load_name} to ptr"
          record_emitted_type(ptr_view, "ptr")
        end
      end
    end

    # Emit type conversions for fixed-type values (params, ExternCalls) used in phi nodes
    # The prepass identified which (pred_block, val) pairs need conversions
    # Emit sext/zext instructions before the block terminator
    private def emit_phi_predecessor_conversions(block : BasicBlock)
      @phi_predecessor_conversions.each do |(key, conv_info)|
        pred_block_id, val_id = key
        # Only emit conversions for THIS block
        next unless pred_block_id == block.id

        conv_name, from_bits, to_bits = conv_info

        # Get the original value reference
        val_ref = value_ref(val_id)

        # Check actual emitted type — value_ref may have already cast from slot type
        actual_type = @emitted_value_types[val_ref]?
        actual_bits = if actual_type && actual_type.starts_with?('i') && !actual_type.includes?('.')
                        actual_type[1..].to_i? || from_bits
                      else
                        from_bits
                      end

        # Skip conversion if value already has the target type
        if actual_bits == to_bits
          emit "%#{conv_name} = add i#{to_bits} #{val_ref}, 0"
        elsif actual_bits < to_bits
          # Widening from actual type
          val_type = @value_types[val_id]?
          is_signed = val_type && (val_type == TypeRef::INT8 || val_type == TypeRef::INT16 ||
                                   val_type == TypeRef::INT32 || val_type == TypeRef::INT64)
          if is_signed
            emit "%#{conv_name} = sext i#{actual_bits} #{val_ref} to i#{to_bits}"
          else
            emit "%#{conv_name} = zext i#{actual_bits} #{val_ref} to i#{to_bits}"
          end
        else
          # Narrowing: use trunc
          emit "%#{conv_name} = trunc i#{actual_bits} #{val_ref} to i#{to_bits}"
        end
      end
    end

    # Emit union wraps in predecessor blocks before terminators
    # This ensures union phi nodes receive correctly-typed union values.
    private def emit_phi_predecessor_union_wraps(block : BasicBlock)
      @phi_predecessor_union_wraps.each do |(key, wrap_info)|
        pred_block_id, val_id = key
        # Only emit wraps for THIS block
        next unless pred_block_id == block.id

        wrap_name, union_type_ref, variant_type_id = wrap_info
        union_type = @type_mapper.llvm_type(union_type_ref)

        # Emit union wrap inline (similar to emit_union_wrap)
        base_name = wrap_name
        val_type = @value_types[val_id]?
        val_llvm_type = val_type ? @type_mapper.llvm_type(val_type) : "ptr"
        val_ref = value_ref(val_id)

        emit "%#{base_name}.ptr = alloca #{union_type}, align 8"
        emit "%#{base_name}.type_id_ptr = getelementptr #{union_type}, ptr %#{base_name}.ptr, i32 0, i32 0"
        emit "store i32 #{variant_type_id}, ptr %#{base_name}.type_id_ptr"
        emit "%#{base_name}.payload_ptr = getelementptr #{union_type}, ptr %#{base_name}.ptr, i32 0, i32 1"
        if val_llvm_type == "void"
          emit "store i8 0, ptr %#{base_name}.payload_ptr, align 4"
        else
          val_ref = "null" if val_llvm_type == "ptr" && val_ref == "0"
          # For tuple types (stack-allocated value types), copy data instead of storing pointer
          phi_variant_vt = false
          phi_variant_size = 0_u64
          if val_llvm_type == "ptr" && val_ref != "null"
            union_mir = @module.type_registry.get(union_type_ref)
            if union_mir && (uvars = union_mir.variants)
              if variant_type_id >= 0 && variant_type_id < uvars.size
                vtype = uvars[variant_type_id]
                if vtype.name.starts_with?("Tuple(") && vtype.is_value_type? && vtype.size > 0
                  phi_variant_vt = true
                  phi_variant_size = vtype.size
                end
              end
            end
          end
          if phi_variant_vt
            emit "call void @llvm.memcpy.p0.p0.i64(ptr %#{base_name}.payload_ptr, ptr #{val_ref}, i64 #{phi_variant_size}, i1 false)"
          else
            emit "store #{val_llvm_type} #{val_ref}, ptr %#{base_name}.payload_ptr, align 4"
          end
        end
        emit "%#{wrap_name} = load #{union_type}, ptr %#{base_name}.ptr"
      end
    end

    # Emit union-to-ptr payload extractions for union values used in successor ptr phi nodes.
    # When a ptr phi has a union-typed incoming value, we extract the ptr payload in the
    # predecessor block. Uses alloca+store+GEP+load to safely extract the pointer from
    # the union struct's payload field (index 1).
    private def emit_phi_union_to_ptr_extracts(block : BasicBlock)
      @phi_union_to_ptr_extracts.each do |(key, info)|
        pred_block_id, val_id = key
        # Only emit extractions for THIS block
        next unless pred_block_id == block.id

        extract_name, union_type = info
        val_ref_str = value_ref(val_id)
        if val_ref_str == "null"
          emit "%#{extract_name} = inttoptr i64 0 to ptr"
          next
        end

        # Check if value_ref already cast the union to ptr (when @value_types differs
        # from slot type). In that case, the ptr is already extracted.
        emitted_type = @emitted_value_types[val_ref_str]?
        if emitted_type == "ptr"
          # Already a ptr — no union extraction needed, just alias
          emit "%#{extract_name} = bitcast ptr #{val_ref_str} to ptr"
          next
        end
        if emitted_type && !emitted_type.includes?(".union")
          if emitted_type.starts_with?('i')
            if val_ref_str == "0"
              emit "%#{extract_name} = inttoptr i64 0 to ptr"
            else
              emit "%#{extract_name}.itp = inttoptr #{emitted_type} #{val_ref_str} to ptr"
              emit "%#{extract_name} = bitcast ptr %#{extract_name}.itp to ptr"
            end
            next
          elsif emitted_type == "double"
            emit "%#{extract_name}.bits = bitcast double #{val_ref_str} to i64"
            emit "%#{extract_name} = inttoptr i64 %#{extract_name}.bits to ptr"
            next
          elsif emitted_type == "float"
            emit "%#{extract_name}.bits = bitcast float #{val_ref_str} to i32"
            emit "%#{extract_name}.ext = zext i32 %#{extract_name}.bits to i64"
            emit "%#{extract_name} = inttoptr i64 %#{extract_name}.ext to ptr"
            next
          end
        end

        # Extract ptr from union: alloca → store → GEP to payload → load ptr
        emit "%#{extract_name}.alloca = alloca #{union_type}, align 8"
        emit "store #{union_type} #{val_ref_str}, ptr %#{extract_name}.alloca"
        emit "%#{extract_name}.pay_ptr = getelementptr #{union_type}, ptr %#{extract_name}.alloca, i32 0, i32 1"
        emit "%#{extract_name} = load ptr, ptr %#{extract_name}.pay_ptr, align 4"
      end
    end

    # Emit union-to-union reinterpretations for union values used in successor union phi nodes
    # where the value's union type differs from the phi's union type. Both union types have the
    # same physical layout {i32, [N x i8]}, so we reinterpret through memory: alloca → store → load.
    private def emit_phi_union_to_union_converts(block : BasicBlock)
      @phi_union_to_union_converts.each do |(key, entries)|
        pred_block_id, val_id = key
        next unless pred_block_id == block.id

        val_ref_str = value_ref(val_id)

        entries.each do |info|
          convert_name, src_union_type, dst_union_type = info

          # Check if value_ref already cast away from union (e.g., union → ptr)
          emitted_type = @emitted_value_types[val_ref_str]?
          if emitted_type && !emitted_type.includes?(".union")
            # value_ref extracted a non-union value; wrap it into the destination union
            emit "%#{convert_name}.alloca = alloca #{dst_union_type}, align 8"
            emit "store #{dst_union_type} zeroinitializer, ptr %#{convert_name}.alloca"
            emit "%#{convert_name}.tid_ptr = getelementptr #{dst_union_type}, ptr %#{convert_name}.alloca, i32 0, i32 0"
            nil_vid = nil_variant_id_for_union_type(dst_union_type) || 0
            emit "store i32 #{nil_vid}, ptr %#{convert_name}.tid_ptr"
            emit "%#{convert_name}.pay_ptr = getelementptr #{dst_union_type}, ptr %#{convert_name}.alloca, i32 0, i32 1"
            emit "store #{emitted_type} #{val_ref_str}, ptr %#{convert_name}.pay_ptr, align 4"
            emit "%#{convert_name} = load #{dst_union_type}, ptr %#{convert_name}.alloca"
          else
            # Convert union: preserve payload, remap type_id across source/destination
            # union variant orderings.
            actual_src = @emitted_value_types[val_ref_str]? || src_union_type
            actual_src = src_union_type unless actual_src.includes?(".union")
            emit "%#{convert_name}.src_ptr = alloca #{actual_src}, align 8"
            emit "store #{actual_src} #{normalize_union_value(val_ref_str, actual_src)}, ptr %#{convert_name}.src_ptr"
            emit "%#{convert_name}.src_type_id_ptr = getelementptr #{actual_src}, ptr %#{convert_name}.src_ptr, i32 0, i32 0"
            emit "%#{convert_name}.src_type_id = load i32, ptr %#{convert_name}.src_type_id_ptr"
            mapped_type_id = emit_union_type_id_remap(actual_src, dst_union_type, "%#{convert_name}.src_type_id", "#{convert_name}.phi_u2u")
            emit "%#{convert_name}.src_payload_ptr = getelementptr #{actual_src}, ptr %#{convert_name}.src_ptr, i32 0, i32 1"
            emit "%#{convert_name}.payload_as_ptr = load ptr, ptr %#{convert_name}.src_payload_ptr, align 4"
            emit "%#{convert_name}.dst_ptr = alloca #{dst_union_type}, align 8"
            emit "%#{convert_name}.dst_type_id_ptr = getelementptr #{dst_union_type}, ptr %#{convert_name}.dst_ptr, i32 0, i32 0"
            emit "store i32 #{mapped_type_id}, ptr %#{convert_name}.dst_type_id_ptr"
            emit "%#{convert_name}.dst_payload_ptr = getelementptr #{dst_union_type}, ptr %#{convert_name}.dst_ptr, i32 0, i32 1"
            emit "store ptr %#{convert_name}.payload_as_ptr, ptr %#{convert_name}.dst_payload_ptr, align 4"
            emit "%#{convert_name} = load #{dst_union_type}, ptr %#{convert_name}.dst_ptr"
          end
        end
      end
    end

    private def emit_phi_union_payload_extracts(block : BasicBlock)
      extracts = @phi_union_payload_extracts
      return if extracts.empty?
      extracts.each do |(key, info)|
        pred_block_id, val_id = key
        # Only emit extractions for THIS block
        next unless pred_block_id == block.id

        extract_name, load_name, target_type = info
        # The load_name loaded the full union struct from the slot.
        # Extract the payload (offset 4 after the type_id i32) as the target primitive type.
        slot_name = @cross_block_slots[val_id]?
        next unless slot_name
        emit "%#{extract_name}.pay_ptr = getelementptr i8, ptr %#{slot_name}, i32 4"
        emit "%#{extract_name} = load #{target_type}, ptr %#{extract_name}.pay_ptr, align 4"
      end
    end

    private def emit_instruction(inst : Value, func : Function)
      name = "%r#{inst.id}"

      # Check if this instruction produces a value (has a result register)
      # Store, Free, RCIncrement, RCDecrement, GlobalStore, AtomicStore don't produce values
      # Also exclude void-returning calls - they emit `call void` without result
      is_void_call = (inst.is_a?(Call) || inst.is_a?(ExternCall) || inst.is_a?(IndirectCall)) && begin
        # Check both prepass type and MIR type — the emission functions use inst.type
        # which may differ from @value_types (prepass can resolve void → ptr).
        mir_llvm_type = @type_mapper.llvm_type(inst.type)
        call_ret_type = @value_types[inst.id]?
        call_llvm_type = call_ret_type ? @type_mapper.llvm_type(call_ret_type) : nil
        call_llvm_type == "void" || mir_llvm_type == "void"
      end
      produces_value = !inst.is_a?(Store) && !inst.is_a?(MemCopy) && !inst.is_a?(Free) &&
                       !inst.is_a?(RCIncrement) && !inst.is_a?(RCDecrement) &&
                       !inst.is_a?(GlobalStore) && !inst.is_a?(AtomicStore) &&
                       !inst.is_a?(TryEnd) && !inst.is_a?(ArraySet) &&
                       !inst.is_a?(ArraySetSize) && !inst.is_a?(Fence) &&
                       !is_void_call

      # Only register value_names for instructions that produce values
      if produces_value
        @value_names[inst.id] = "r#{inst.id}"
        # Preserve prepass types that were propagated from phi nodes
        # Only set MIR type if:
        # - No prepass type exists, OR
        # - Prepass type is void (needs to be overwritten)
        prepass_type = @value_types[inst.id]?
        prepass_llvm = prepass_type ? @type_mapper.llvm_type(prepass_type) : nil
        if prepass_type.nil? || prepass_llvm == "void"
          @value_types[inst.id] = inst.type
        end
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
      when MemCopy
        emit_memcopy(inst)
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
      when AddressOf
        emit_address_of(inst, name)
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
      when ArraySetSize
        emit_array_set_size(inst, name)
      when ArrayNew
        emit_array_new(inst, name)
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
      when FuncPointer
        emit_func_pointer(inst, name)
      end

      # Store to cross-block slot if this value is used across blocks
      # This centralizes the logic that was previously only in emit_load
      # When in phi block, defer EVERYTHING (including wrapping) to after all phis
      if @in_phi_block && produces_value && (slot_name = @cross_block_slots[inst.id]?)
        @deferred_phi_store_ops << {inst.id, name, slot_name}
      elsif produces_value && (slot_name = @cross_block_slots[inst.id]?)
        emit_cross_block_slot_store(inst.id, name, slot_name)
      end
    end

    private def emit_cross_block_slot_store(inst_id : ValueId, name : String, slot_name : String)
      val_type = @value_types[inst_id]?
      return unless val_type
      llvm_type = @type_mapper.llvm_type(val_type)
      llvm_type = "ptr" if llvm_type == "void"
      # Check actual emitted type — the value may have been emitted with a different
      # type than what @value_types reports (e.g., ExternCall returns i32 for
      # Array(UInt32)#unsafe_fetch but MIR type says ptr for struct UInt32).
      # Use the actual emitted type so the slot store can detect and handle mismatches.
      if actual = @emitted_value_types[name]?
        llvm_type = actual if actual != llvm_type
      end
      slot_llvm_type = @cross_block_slot_types[inst_id]?
      # Guard: if MIR reuses value IDs, skip store when types are completely incompatible
      if slot_llvm_type && slot_llvm_type != llvm_type &&
         slot_llvm_type.includes?(".union") && !llvm_type.includes?(".union")
        scalar_compatible = llvm_type == "ptr" || llvm_type.starts_with?('i') ||
                            llvm_type == "double" || llvm_type == "float"
        union_has_float = slot_llvm_type.includes?("Float") || slot_llvm_type.includes?("float")
        union_has_int = slot_llvm_type.includes?("Int") || slot_llvm_type.includes?("UInt")
        value_is_float = llvm_type == "double" || llvm_type == "float"
        value_is_int = llvm_type.starts_with?('i') && !llvm_type.includes?('.')
        if scalar_compatible && !((value_is_float && union_has_float) || (value_is_int && union_has_int) || llvm_type == "ptr")
          slot_llvm_type = nil
        end
      end
      store_val = name
      store_type = llvm_type
      if slot_llvm_type && slot_llvm_type != llvm_type
        base = name.lstrip('%')
        if slot_llvm_type.includes?(".union") && llvm_type.includes?(".union")
          # Union-to-union reinterpretation: both are unions but different named types.
          # Use alloca + store + load to reinterpret through memory.
          # Check actual emitted type — the value may have a different union type than
          # what the type mapper reports (e.g., from a fromslot cast).
          actual_llvm_type = @emitted_value_types[name]? || llvm_type
          actual_llvm_type = llvm_type unless actual_llvm_type.includes?(".union")
          # Alloca with source type so store matches, then load as slot type
          emit "%#{base}.u2u.slot.alloca = alloca #{actual_llvm_type}, align 8"
          emit "store #{actual_llvm_type} #{name}, ptr %#{base}.u2u.slot.alloca"
          emit "%#{base}.u2u.slot.val = load #{slot_llvm_type}, ptr %#{base}.u2u.slot.alloca"
          store_val = "%#{base}.u2u.slot.val"
          store_type = slot_llvm_type
        elsif slot_llvm_type.includes?(".union") && !llvm_type.includes?(".union")
          emit "%#{base}.slot_wrap_ptr = alloca #{slot_llvm_type}, align 8"
          emit "store #{slot_llvm_type} zeroinitializer, ptr %#{base}.slot_wrap_ptr"
          emit "%#{base}.slot_wrap_tid = getelementptr #{slot_llvm_type}, ptr %#{base}.slot_wrap_ptr, i32 0, i32 0"
          emit "store i32 0, ptr %#{base}.slot_wrap_tid"
          emit "%#{base}.slot_wrap_pay = getelementptr #{slot_llvm_type}, ptr %#{base}.slot_wrap_ptr, i32 0, i32 1"
          emit "store #{llvm_type} #{name}, ptr %#{base}.slot_wrap_pay, align 4"
          emit "%#{base}.slot_wrap_val = load #{slot_llvm_type}, ptr %#{base}.slot_wrap_ptr"
          store_val = "%#{base}.slot_wrap_val"
          store_type = slot_llvm_type
        elsif !slot_llvm_type.includes?(".union") && llvm_type.includes?(".union")
          emit "%#{base}.slot_unwrap_ptr = alloca #{llvm_type}, align 8"
          emit "store #{llvm_type} #{name}, ptr %#{base}.slot_unwrap_ptr"
          emit "%#{base}.slot_unwrap_pay = getelementptr #{llvm_type}, ptr %#{base}.slot_unwrap_ptr, i32 0, i32 1"
          emit "%#{base}.slot_unwrap_val = load #{slot_llvm_type}, ptr %#{base}.slot_unwrap_pay, align 4"
          store_val = "%#{base}.slot_unwrap_val"
          store_type = slot_llvm_type
        elsif llvm_type.starts_with?('i') && slot_llvm_type.starts_with?('i') && !llvm_type.includes?('.') && !slot_llvm_type.includes?('.')
          val_bits = llvm_type[1..].to_i? || 64
          slot_bits = slot_llvm_type[1..].to_i? || 64
          if val_bits < slot_bits
            emit "%#{base}.slot_ext = sext #{llvm_type} #{name} to #{slot_llvm_type}"
            store_val = "%#{base}.slot_ext"
          elsif val_bits > slot_bits
            emit "%#{base}.slot_trunc = trunc #{llvm_type} #{name} to #{slot_llvm_type}"
            store_val = "%#{base}.slot_trunc"
          end
          store_type = slot_llvm_type
        elsif llvm_type == "ptr" && slot_llvm_type.starts_with?('i')
          # Decide: ptrtoint (packed scalar) vs load (pointer to data)
          mir_val_type = val_type ? @type_mapper.llvm_type(val_type) : "ptr"
          is_packed = (mir_val_type.starts_with?('i') && !mir_val_type.includes?(".union")) ||
                      mir_val_type == "float" || mir_val_type == "double" ||
                      @inttoptr_value_ids.includes?(inst_id)
          if is_packed
            emit "%#{base}.slot_ptrtoint = ptrtoint ptr #{name} to #{slot_llvm_type}"
            store_val = "%#{base}.slot_ptrtoint"
          else
            emit "%#{base}.slot_load = load #{slot_llvm_type}, ptr #{name}"
            store_val = "%#{base}.slot_load"
          end
          store_type = slot_llvm_type
        elsif llvm_type.starts_with?('i') && slot_llvm_type == "ptr"
          emit "%#{base}.slot_inttoptr = inttoptr #{llvm_type} #{name} to ptr"
          store_val = "%#{base}.slot_inttoptr"
          store_type = slot_llvm_type
        elsif (llvm_type == "double" || llvm_type == "float") && slot_llvm_type.starts_with?('i') && !slot_llvm_type.includes?('.')
          slot_type_ref = @cross_block_slot_type_refs[inst_id]?
          op = slot_type_ref && unsigned_type_ref?(slot_type_ref) ? "fptoui" : "fptosi"
          emit "%#{base}.slot_ftoi = #{op} #{llvm_type} #{name} to #{slot_llvm_type}"
          store_val = "%#{base}.slot_ftoi"
          store_type = slot_llvm_type
        elsif llvm_type.starts_with?('i') && !llvm_type.includes?('.') && (slot_llvm_type == "double" || slot_llvm_type == "float")
          op = unsigned_type_ref?(val_type) ? "uitofp" : "sitofp"
          emit "%#{base}.slot_itof = #{op} #{llvm_type} #{name} to #{slot_llvm_type}"
          store_val = "%#{base}.slot_itof"
          store_type = slot_llvm_type
        elsif llvm_type == "float" && slot_llvm_type == "double"
          emit "%#{base}.slot_fpext = fpext float #{name} to double"
          store_val = "%#{base}.slot_fpext"
          store_type = "double"
        elsif llvm_type == "double" && slot_llvm_type == "float"
          emit "%#{base}.slot_fptrunc = fptrunc double #{name} to float"
          store_val = "%#{base}.slot_fptrunc"
          store_type = "float"
        elsif (llvm_type == "double" || llvm_type == "float") && slot_llvm_type == "ptr"
          if llvm_type == "double"
            emit "%#{base}.slot_float_bits = bitcast double #{name} to i64"
            emit "%#{base}.slot_inttoptr = inttoptr i64 %#{base}.slot_float_bits to ptr"
          else
            emit "%#{base}.slot_float_bits = bitcast float #{name} to i32"
            emit "%#{base}.slot_float_bits_ext = zext i32 %#{base}.slot_float_bits to i64"
            emit "%#{base}.slot_inttoptr = inttoptr i64 %#{base}.slot_float_bits_ext to ptr"
          end
          store_val = "%#{base}.slot_inttoptr"
          store_type = slot_llvm_type
        elsif llvm_type == "ptr" && (slot_llvm_type == "double" || slot_llvm_type == "float")
          emit "%#{base}.slot_ptrtoint = ptrtoint ptr #{name} to i64"
          emit "%#{base}.slot_uitofp = uitofp i64 %#{base}.slot_ptrtoint to #{slot_llvm_type}"
          store_val = "%#{base}.slot_uitofp"
          store_type = slot_llvm_type
        else
          store_type = slot_llvm_type
        end
      end
      emit "store #{store_type} #{store_val}, ptr %#{slot_name}"
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

      # Module literal constants use stable singleton globals, not null pointers.
      # This keeps module-typed virtual dispatch safe (no null type_id load).
      if inst.value.is_a?(Nil) && @module.module_type?(inst.type)
        global_name = module_singleton_global_for(inst.type)
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
      if type.includes?(".union")
        # Union types can't use add instruction - create zeroinit union
        base_name = name.lstrip('%')
        emit "%#{base_name}.ptr = alloca #{type}, align 8"
        # Zero-initialize by setting type_id to 0 (nil variant)
        emit "%#{base_name}.type_id_ptr = getelementptr #{type}, ptr %#{base_name}.ptr, i32 0, i32 0"
        emit "store i32 0, ptr %#{base_name}.type_id_ptr"
        emit "#{name} = load #{type}, ptr %#{base_name}.ptr"
        @value_types[inst.id] = inst.type
      elsif (type == "void" || value == "null" || type == "ptr") &&
            (sa_type = @module.type_registry.get(inst.type)) &&
            sa_type.name.starts_with?("StaticArray(")
        # uninitialized StaticArray(T, N) — emit stack alloca.
        # Parse element type and count from name, look up element size from registry.
        total_bytes = sa_type.size
        if total_bytes == 0
          if m = sa_type.name.match(/StaticArray\((.+),\s*(\d+)\)/)
            elem_name = m[1].strip
            array_count = m[2].to_u64
            elem_mir_type = @module.type_registry.get_by_name(elem_name)
            elem_size = container_elem_storage_size_u64(elem_mir_type)
            total_bytes = elem_size * array_count
          end
        end
        total_bytes = pointer_word_bytes_u64 if total_bytes == 0 # safety fallback
        emit "#{name} = alloca [#{total_bytes} x i8], align 8"
        # Override constant_values so call sites use the alloca ptr, not "null"
        @constant_values[inst.id] = name
        @value_types[inst.id] = TypeRef::POINTER
      elsif type == "void" || value == "null" || type == "ptr"
        # void/null/ptr constants are treated as ptr type in LLVM
        # Must emit real instruction (not comment) so phi nodes can reference it
        emit "#{name} = inttoptr i64 0 to ptr"
        @value_types[inst.id] = TypeRef::POINTER
        @inttoptr_value_ids.add(inst.id)
      elsif type == "double" || type == "float"
        # Float/double constants use fadd - ensure value is proper float literal
        float_value = value == "0" ? "0.0" : value
        float_value = "#{float_value}.0" if float_value.matches?(/^\d+$/)  # Add .0 if just digits
        emit "#{name} = fadd #{type} 0.0, #{float_value}"
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
        # Skip if already emitted in hoisted entry block
        if @emitted_allocas.includes?(inst.id)
          return
        end
        # Use llvm_alloca_type to get actual struct type (not ptr)
        type = @type_mapper.llvm_alloca_type(inst.alloc_type)
        # Guard against void type - use i8 for minimal allocation
        type = "i8" if type == "void"
        # When LLVM type is just 'ptr' but MIR needs more space, use byte array
        if type == "ptr" && inst.size > 8
          type = "[#{inst.size} x i8]"
        end
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
      # Track original alloc_type for enum/struct detection during argument conversion
      @alloc_types[inst.id] = inst.alloc_type
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
        emit "call void @__crystal_v2_slab_free(ptr #{ptr}, i32 0)"
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
      ptr_type_ref = @value_types[inst.ptr]?
      if ptr_type_ref && union_ptr_like?(ptr_type_ref)
        union_llvm = @type_mapper.llvm_type(ptr_type_ref)
        if union_llvm.includes?(".union")
          raw_name = name.starts_with?('%') ? name[1..] : name
          base_name = "#{raw_name}.ptr_union"
          emit "%#{base_name}.ptr = alloca #{union_llvm}, align 8"
          emit "store #{union_llvm} #{normalize_union_value(ptr, union_llvm)}, ptr %#{base_name}.ptr"
          emit "%#{base_name}.payload_ptr = getelementptr #{union_llvm}, ptr %#{base_name}.ptr, i32 0, i32 1"
          emit "%#{base_name}.payload = load ptr, ptr %#{base_name}.payload_ptr, align 4"
          record_emitted_type("%#{base_name}.payload", "ptr")
          ptr = "%#{base_name}.payload"
        end
      end

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
      # Cross-block store is now handled centrally in emit_instruction
    end

    private def union_ptr_like?(type_ref : TypeRef) : Bool
      union_desc = @module.get_union_descriptor(type_ref)
      return false unless union_desc
      variants = union_desc.variants.reject { |v| v.type_ref == TypeRef::NIL }
      return false if variants.empty?
      variants.all? { |v| @type_mapper.llvm_type(v.type_ref) == "ptr" }
    end

    private def emit_store(inst : Store)
      ptr = value_ref(inst.ptr)
      val = value_ref(inst.value)
      # Look up the type of the value being stored
      val_type = @value_types[inst.value]? || TypeRef::POINTER
      val_type_str = @type_mapper.llvm_type(val_type)
      actual_val_type = @emitted_value_types[val]?

      # Fallback: values loaded from cross-block slots are named like %rN.fromslot.*
      # Their real LLVM type is the slot type, not necessarily @value_types[N].
      if !actual_val_type && val.starts_with?('%')
        if match = val.match(/^%r(\d+)\.fromslot/)
          slot_id = ValueId.new(match[1].to_i)
          actual_val_type = @cross_block_slot_types[slot_id]?
        end
      end

      # Fix for VOID types - use a reasonable default type
      if val_type_str == "void"
        if val.starts_with?('%')
          val_type_str = "ptr"  # Value reference, probably a pointer
        else
          val_type_str = "i64"  # Literal value (like "0" for void calls), use i64 as default
        end
      end

      # For pointer types, use "null" instead of "0"
      if val_type_str == "ptr" && val == "0"
        val = "null"
      end

      # For float/double types, ensure values are proper float literals
      if (val_type_str == "float" || val_type_str == "double") && !val.starts_with?('%')
        # Convert integer literals to float literals (e.g., "0" -> "0.0", "3" -> "3.0")
        val = "#{val}.0" if val.matches?(/^-?\d+$/)
      end

      # Reconcile real SSA type with the intended store type.
      # This matters for cross-block slot values where a pointer value can be
      # temporarily represented as an integer slot payload (or vice versa).
      if actual_val_type && actual_val_type != "void" && actual_val_type != val_type_str
        if val.starts_with?('%')
          base = "r#{inst.id}.store_cast"
          if actual_val_type.starts_with?('i') && val_type_str == "ptr"
            emit "%#{base} = inttoptr #{actual_val_type} #{val} to ptr"
            val = "%#{base}"
            val_type_str = "ptr"
          elsif actual_val_type == "ptr" && val_type_str.starts_with?('i')
            emit "%#{base} = ptrtoint ptr #{val} to #{val_type_str}"
            val = "%#{base}"
          elsif actual_val_type.starts_with?('i') && val_type_str.starts_with?('i') &&
                !actual_val_type.includes?('.') && !val_type_str.includes?('.')
            from_bits = actual_val_type[1..].to_i? || 64
            to_bits = val_type_str[1..].to_i? || 64
            if from_bits < to_bits
              emit "%#{base} = sext #{actual_val_type} #{val} to #{val_type_str}"
              val = "%#{base}"
            elsif from_bits > to_bits
              emit "%#{base} = trunc #{actual_val_type} #{val} to #{val_type_str}"
              val = "%#{base}"
            end
          elsif (actual_val_type == "double" || actual_val_type == "float") && val_type_str.starts_with?('i')
            op = unsigned_type_ref?(val_type) ? "fptoui" : "fptosi"
            emit "%#{base} = #{op} #{actual_val_type} #{val} to #{val_type_str}"
            val = "%#{base}"
          elsif actual_val_type.starts_with?('i') && (val_type_str == "double" || val_type_str == "float")
            op = unsigned_type_ref?(val_type) ? "uitofp" : "sitofp"
            emit "%#{base} = #{op} #{actual_val_type} #{val} to #{val_type_str}"
            val = "%#{base}"
          elsif actual_val_type == "float" && val_type_str == "double"
            emit "%#{base} = fpext float #{val} to double"
            val = "%#{base}"
          elsif actual_val_type == "double" && val_type_str == "float"
            emit "%#{base} = fptrunc double #{val} to float"
            val = "%#{base}"
          else
            # Conservative fallback: if conversion is unknown, keep IR type consistent.
            val_type_str = actual_val_type
          end
        else
          # Literal mismatch (rare) - keep IR type consistent.
          val_type_str = actual_val_type
        end
      end

      # Union types can't store raw integer literals like 0/null.
      val = normalize_union_value(val, val_type_str)

      # TSan instrumentation: report write before store
      if @emit_tsan
        tsan_size = tsan_access_size(val_type)
        emit "call void @__tsan_write#{tsan_size}(ptr #{ptr})"
      end

      emit "store #{val_type_str} #{val}, ptr #{ptr}"
    end

    private def emit_memcopy(inst : MemCopy)
      dst = value_ref(inst.dst)
      src = value_ref(inst.src)
      # Skip memcopy from null — this happens when .new default-initializes lib struct
      # fields. The allocation is already zeroed by malloc.
      if src == "null" || src == "0"
        return
      end
      emit "call void @llvm.memcpy.p0.p0.i64(ptr #{dst}, ptr #{src}, i64 #{inst.size}, i1 false)"
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
      base_name = name.lstrip('%')

      # GEP requires a pointer base. If MIR gives us a union/int/float, extract/convert to ptr.
      base_type = @value_types[inst.base]?
      base_type_str = base_type ? @type_mapper.llvm_type(base_type) : "ptr"
      # LLVM pointer constants use `null`, not `0` (only for ptr-typed bases).
      base = "null" if base == "0" && base_type_str == "ptr"
      if base_type_str != "ptr"
        if base_type_str.includes?(".union")
          # Union value - extract payload as ptr
          emit "%#{base_name}.union_ptr = alloca #{base_type_str}, align 8"
          emit "store #{base_type_str} #{normalize_union_value(base, base_type_str)}, ptr %#{base_name}.union_ptr"
          emit "%#{base_name}.payload_ptr = getelementptr #{base_type_str}, ptr %#{base_name}.union_ptr, i32 0, i32 1"
          emit "%#{base_name}.base_ptr = load ptr, ptr %#{base_name}.payload_ptr, align 4"
          base = "%#{base_name}.base_ptr"
        elsif base_type_str.starts_with?('i')
          # Integer to pointer: inttoptr
          emit "%#{base_name}.base_ptr = inttoptr #{base_type_str} #{base} to ptr"
          base = "%#{base_name}.base_ptr"
        elsif base_type_str == "double" || base_type_str == "float"
          # Float to pointer: bitcast to i64 first, then inttoptr
          emit "%#{base_name}.base_int = bitcast #{base_type_str} #{base} to i64"
          emit "%#{base_name}.base_ptr = inttoptr i64 %#{base_name}.base_int to ptr"
          base = "%#{base_name}.base_ptr"
        end
      end

      # Always use byte-level GEP for field access.
      # Our byte offsets (from align_all_class_ivars) are correct, but LLVM struct
      # types may have incorrect field types (e.g. ptr instead of i32), causing
      # struct-level GEP to compute wrong offsets. Byte-level GEP is always safe.
      byte_offset = inst.indices.first? || 0_u32
      # Ensure ptr base is "null" not "0" for valid LLVM IR (dead-code void call returns)
      base = "null" if base == "0"
      emit "#{name} = getelementptr i8, ptr #{base}, i32 #{byte_offset}"
      @value_types[inst.id] = TypeRef::POINTER  # GEP always returns pointer
      record_emitted_type(name, "ptr")
    end

    # Compute LLVM struct field index from byte offset.
    # Each MIR Field stores its byte offset; the LLVM struct fields are emitted
    # in MIR field order with an optional vtable ptr at index 0 for classes.
    # We match by the stored byte offset rather than recomputing sizes, since
    # LLVM alignment differs from our packed byte layout.
    private def compute_field_index(mir_type : Type, byte_offset : UInt32) : Int32
      fields = mir_type.fields
      return 0 unless fields

      class_offset = mir_type.kind.reference? ? 1 : 0

      # For reference types, byte offsets < 4 access the type_id header (i32, LLVM field 0)
      if mir_type.kind.reference? && byte_offset < 4
        return 0
      end

      # Exact match on stored byte offset
      fields.each_with_index do |field, idx|
        if field.offset == byte_offset
          return idx + class_offset
        end
      end

      # Fallback: closest field at or below the target byte offset
      best_idx = 0
      best_offset = 0_u32
      fields.each_with_index do |field, idx|
        if field.offset <= byte_offset && field.offset >= best_offset
          best_idx = idx
          best_offset = field.offset
        end
      end
      best_idx + class_offset
    end

    private def emit_gep_dynamic(inst : GetElementPtrDynamic, name : String)
      base = value_ref(inst.base)
      index = value_ref(inst.index)
      element_type = @type_mapper.llvm_type(inst.element_type)

      # Void is not a valid GEP element type - use i8 (byte pointer arithmetic)
      if element_type == "void"
        element_type = "i8"
      end

      # For tuple element types that map to "ptr" in our ABI, GEP would step
      # by sizeof(ptr)=8 instead of sizeof(tuple). Use byte-level GEP instead.
      # NOTE: Structs are heap-allocated in our ABI — Pointer(Struct) buffers
      # store 8-byte heap pointers, so GEP with ptr stride (8) is correct.
      # Only tuples need the size override since they're stored inline.
      struct_elem_size = inst.element_byte_size  # Explicit size from HIR (for generic structs)
      if struct_elem_size == 0 && element_type == "ptr" && inst.element_type.id > TypeRef::POINTER.id
        if mir_type = @module.type_registry.get(inst.element_type)
          if mir_type.kind.tuple?
            struct_elem_size = mir_type.size
          end
        end
      end

      # Check base type - GEP requires pointer base
      base_type = @value_types[inst.base]?
      base_type_str = base_type ? @type_mapper.llvm_type(base_type) : "ptr"
      # LLVM pointer constants use `null`, not `0` (only for ptr-typed bases).
      base = "null" if base == "0" && base_type_str == "ptr"

      # If base is not a pointer, we have a type mismatch from MIR
      # Convert integer types to pointer using inttoptr
      if base_type_str != "ptr"
        if base_type_str.includes?(".union")
          # Union type - extract payload as ptr
          emit "#{name}.union_ptr = alloca #{base_type_str}, align 8"
          emit "store #{base_type_str} #{normalize_union_value(base, base_type_str)}, ptr #{name}.union_ptr"
          emit "#{name}.payload_ptr = getelementptr #{base_type_str}, ptr #{name}.union_ptr, i32 0, i32 1"
          emit "#{name}.base_ptr = load ptr, ptr #{name}.payload_ptr, align 4"
          base = "#{name}.base_ptr"
        elsif base_type_str.starts_with?('i')
          # Integer to pointer: inttoptr (may be a value used as address)
          emit "#{name}.base_ptr = inttoptr #{base_type_str} #{base} to ptr"
          base = "#{name}.base_ptr"
        elsif base_type_str == "double" || base_type_str == "float"
          # Float to pointer: bitcast to i64 first, then inttoptr
          emit "#{name}.base_int = bitcast #{base_type_str} #{base} to i64"
          emit "#{name}.base_ptr = inttoptr i64 #{name}.base_int to ptr"
          base = "#{name}.base_ptr"
        elsif base_type_str == "void"
          # Void type (e.g., forward-declared call returning void) — treat as null pointer
          base = "null"
        end
        # For other types, hope the value is already ptr (optimistic)
      end
      # Final catch-all: literal "0" as GEP base must be "null" in opaque pointer LLVM IR
      base = "null" if base == "0"

      # Check if index needs to be converted to i64 for GEP
      index_type = @value_types[inst.index]? || TypeRef::INT32
      index_type_str = @type_mapper.llvm_type(index_type)
      if !index_type_str.includes?(".union")
        if emitted_index_type = @emitted_value_types[index]?
          if emitted_index_type.includes?(".union")
            index_type_str = emitted_index_type
          end
        end
      end
      if index_type_str.includes?(".union")
        base_name = name.lstrip('%')
        emit "%#{base_name}.idx_union_ptr = alloca #{index_type_str}, align 8"
        emit "store #{index_type_str} #{normalize_union_value(index, index_type_str)}, ptr %#{base_name}.idx_union_ptr"
        emit "%#{base_name}.idx_type_id_ptr = getelementptr #{index_type_str}, ptr %#{base_name}.idx_union_ptr, i32 0, i32 0"
        emit "%#{base_name}.idx_type_id = load i32, ptr %#{base_name}.idx_type_id_ptr"
        emit "%#{base_name}.idx_payload_ptr = getelementptr #{index_type_str}, ptr %#{base_name}.idx_union_ptr, i32 0, i32 1"

        payload_type = if index_type_str.includes?("Int64") || index_type_str.includes?("UInt64")
                         "i64"
                       else
                         "i32"
                       end
        emit "%#{base_name}.idx_payload = load #{payload_type}, ptr %#{base_name}.idx_payload_ptr, align 4"
        payload64 = "%#{base_name}.idx_payload64"
        if payload_type == "i32"
          emit "#{payload64} = sext i32 %#{base_name}.idx_payload to i64"
        else
          emit "#{payload64} = add i64 %#{base_name}.idx_payload, 0"
        end
        emit "%#{base_name}.idx_is_nil = icmp eq i32 %#{base_name}.idx_type_id, 0"
        emit "%#{base_name}.idx64 = select i1 %#{base_name}.idx_is_nil, i64 0, i64 #{payload64}"
        index = "%#{base_name}.idx64"
        index_type_str = "i64"
      end
      # Normalize index to i64
      idx64 = index  # Will hold i64-typed index
      if index_type_str == "i64"
        # Already i64
      elsif index_type_str == "ptr"
        ext_name = "#{name}.idx64"
        emit "#{ext_name} = ptrtoint ptr #{index} to i64"
        idx64 = ext_name
      elsif index_type_str == "void"
        idx64 = "0"
      elsif index_type_str == "float" || index_type_str == "double"
        ext_name = "#{name}.idx64"
        emit "#{ext_name} = fptosi #{index_type_str} #{index} to i64"
        idx64 = ext_name
      else
        ext_name = "#{name}.idx64"
        bits = nil.as(Int32?)
        if index_type_str.starts_with?('i')
          bits = index_type_str[1..].to_i?
        end
        if bits && bits > 64
          emit "#{ext_name} = trunc #{index_type_str} #{index} to i64"
        else
          is_unsigned = index_type == TypeRef::UINT8 || index_type == TypeRef::UINT16 ||
                        index_type == TypeRef::UINT32 || index_type == TypeRef::UINT64 ||
                        index_type == TypeRef::UINT128
          op = is_unsigned ? "zext" : "sext"
          emit "#{ext_name} = #{op} #{index_type_str} #{index} to i64"
        end
        idx64 = ext_name
      end

      # For struct element types, multiply index by sizeof(struct) and use byte-level GEP.
      # Without this, GEP ptr steps by sizeof(ptr)=8 instead of sizeof(struct).
      if struct_elem_size > 0 && struct_elem_size != 8
        byte_name = "#{name}.byte_offset"
        emit "#{byte_name} = mul i64 #{idx64}, #{struct_elem_size}"
        emit "#{name} = getelementptr i8, ptr #{base}, i64 #{byte_name}"
      else
        emit "#{name} = getelementptr #{element_type}, ptr #{base}, i64 #{idx64}"
      end
      @value_types[inst.id] = TypeRef::POINTER  # GEP always returns pointer
      record_emitted_type(name, "ptr")
    end

    private def emit_binary_op(inst : BinaryOp, name : String)
      result_type = @type_mapper.llvm_type(inst.type)
      union_result_type = result_type.includes?(".union") ? result_type : nil
      wrap_union_result = false
      # Void result type is invalid for binary ops - use i64 as fallback
      if result_type == "void"
        result_type = "i64"
        @value_types[inst.id] = TypeRef::INT64
      end
      left = value_ref(inst.left)
      right = value_ref(inst.right)

      # For comparisons, use operand type; for others, use result type
      # Use lookup_value_llvm_type which checks parameters as well as @value_types
      operand_type = @value_types[inst.left]? || TypeRef::INT32
      right_type = @value_types[inst.right]?
      operand_type_str = lookup_value_llvm_type(inst.left)
      right_type_str = lookup_value_llvm_type(inst.right)

      # Additional fallback: check if value_ref looks like a parameter name (not %r{number})
      # and look up its type from current function params
      if !left.starts_with?("%r") && left.starts_with?('%')
        param_name = left[1..]  # Remove %
        if param_type = current_func_param_type_by_llvm_name(param_name)
          operand_type_str = @type_mapper.llvm_type(param_type)
        end
      end
      if !right.starts_with?("%r") && right.starts_with?('%')
        param_name = right[1..]  # Remove %
        if param_type = current_func_param_type_by_llvm_name(param_name)
          right_type_str = @type_mapper.llvm_type(param_type)
        end
      end

      # If value_ref emitted a casted SSA name, prefer its known LLVM type.
      if emitted_left = @emitted_value_types[left]?
        operand_type_str = emitted_left
      end
      if emitted_right = @emitted_value_types[right]?
        right_type_str = emitted_right
      end

      # Determine operation type
      is_arithmetic = inst.op.add? || inst.op.sub? || inst.op.mul? ||
                      inst.op.div? || inst.op.rem? || inst.op.shl? ||
                      inst.op.shr? || inst.op.and? || inst.op.or? || inst.op.xor?
      is_comparison = inst.op.eq? || inst.op.ne? || inst.op.lt? || inst.op.le? ||
                      inst.op.gt? || inst.op.ge?

      if is_comparison
        left_type = @value_types[inst.left]?
        right_type = @value_types[inst.right]?
        left_is_bool = left_type == TypeRef::BOOL || operand_type_str == "i1"
        right_is_bool = right_type == TypeRef::BOOL || right_type_str == "i1"
        left_is_null = left == "null" || left_type == TypeRef::NIL
        right_is_null = right == "null" || right_type == TypeRef::NIL
        if (left_is_bool && right_is_null) || (right_is_bool && left_is_null)
          const_val = inst.op.eq? ? "0" : "1"
          emit "#{name} = add i1 0, #{const_val}"
          @value_types[inst.id] = TypeRef::BOOL
          return
        end
      end

      # Handle nilable union types in comparisons (e.g., Int32 == Int32?, Int32? == 0)
      # Must check type_id to distinguish nil from concrete values in the union.
      if is_comparison
        left_is_union = operand_type_str.includes?(".union")
        right_is_union = right_type_str.includes?(".union")

        if (left_is_union || right_is_union) && !(left_is_union && right_is_union)
          # One side is union, the other is concrete
          union_type_str = left_is_union ? operand_type_str : right_type_str
          union_val = left_is_union ? left : right
          concrete_val = left_is_union ? right : left
          concrete_type_str = left_is_union ? right_type_str : operand_type_str
          concrete_type_ref = left_is_union ? right_type : left_type

          nil_vid = nil_variant_id_for_union_type(union_type_str)

          if nil_vid != nil
            base_name = name.lstrip('%')

            # Store union to stack and extract type_id
            emit "%#{base_name}.union_ptr = alloca #{union_type_str}, align 8"
            emit "store #{union_type_str} #{normalize_union_value(union_val, union_type_str)}, ptr %#{base_name}.union_ptr"
            emit "%#{base_name}.type_id_ptr = getelementptr #{union_type_str}, ptr %#{base_name}.union_ptr, i32 0, i32 0"
            emit "%#{base_name}.type_id = load i32, ptr %#{base_name}.type_id_ptr"

            # Check if union holds nil
            emit "%#{base_name}.is_nil = icmp eq i32 %#{base_name}.type_id, #{nil_vid}"

            # Is the concrete side nil?
            concrete_is_nil = concrete_val == "null" || concrete_type_ref == TypeRef::NIL

            if concrete_is_nil
              # nil == union or union == nil: result is whether union is nil
              if inst.op.eq?
                emit "#{name} = icmp eq i32 %#{base_name}.type_id, #{nil_vid}"
              else
                emit "#{name} = icmp ne i32 %#{base_name}.type_id, #{nil_vid}"
              end
            else
              # Concrete non-nil vs nilable union: extract payload and compare
              emit "%#{base_name}.payload_ptr = getelementptr #{union_type_str}, ptr %#{base_name}.union_ptr, i32 0, i32 1"

              # Determine comparison type from concrete side
              cmp_type = if concrete_type_str.starts_with?('i') && !concrete_type_str.includes?('.')
                           concrete_type_str
                         elsif concrete_type_str == "ptr"
                           "i64"
                         else
                           "i32"
                         end

              emit "%#{base_name}.payload = load #{cmp_type}, ptr %#{base_name}.payload_ptr, align 4"

              # Coerce concrete value if ptr
              cmp_concrete = concrete_val
              if concrete_type_str == "ptr"
                emit "%#{base_name}.concrete_int = ptrtoint ptr #{concrete_val} to #{cmp_type}"
                cmp_concrete = "%#{base_name}.concrete_int"
              end

              cmp_op = inst.op.eq? ? "eq" : "ne"
              emit "%#{base_name}.payload_cmp = icmp #{cmp_op} #{cmp_type} %#{base_name}.payload, #{cmp_concrete}"

              # If union is nil: eq→false, ne→true. Otherwise use payload comparison.
              nil_result = inst.op.eq? ? "0" : "1"
              emit "#{name} = select i1 %#{base_name}.is_nil, i1 #{nil_result}, i1 %#{base_name}.payload_cmp"
            end

            @value_types[inst.id] = TypeRef::BOOL
            return
          end
        end
      end

      # Guard: arithmetic/shift ops can't use ptr or void type
      # Use operand type instead of defaulting to i64
      # Track if we need to convert result back to ptr
      convert_result_to_ptr = false
      if (result_type == "ptr" || result_type == "void") && is_arithmetic
        convert_result_to_ptr = (result_type == "ptr")
        # Try to use the actual operand type if it's a concrete int type
        # But avoid using union types - use i64 for arithmetic instead
        if operand_type_str != "ptr" && operand_type_str != "void" && !operand_type_str.includes?(".union")
          result_type = operand_type_str
        elsif right_type_str != "ptr" && right_type_str != "void" && !right_type_str.includes?(".union")
          result_type = right_type_str
        else
          result_type = "i64"  # Fallback for ptr/void/union operands
        end
        # Update @value_types to reflect actual emitted type (not MIR's wrong ptr type)
        actual_type_ref = case result_type
                          when "i8" then TypeRef::INT8
                          when "i16" then TypeRef::INT16
                          when "i32" then TypeRef::INT32
                          when "i64" then TypeRef::INT64
                          else TypeRef::INT64
                          end
        @value_types[inst.id] = actual_type_ref
      end

      # Convert ptr operands to appropriate int type for arithmetic/comparison ops
      # Skip for float/double operations
      is_float_op = result_type == "float" || result_type == "double" ||
                    operand_type_str == "float" || operand_type_str == "double" ||
                    right_type_str == "float" || right_type_str == "double"

      # Handle void operands in float operations (void call result used as operand)
      if is_float_op
        # Determine float type from non-void operand or result
        float_type = if operand_type_str == "double" || result_type == "double"
                       "double"
                     elsif right_type_str == "double"
                       "double"
                     elsif operand_type_str == "float" || result_type == "float"
                       "float"
                     elsif right_type_str == "float"
                       "float"
                     else
                       "double"  # Default to double
                     end
        if operand_type_str == "void"
          left = "0.0"
          operand_type_str = float_type
        end
        if right_type_str == "void"
          right = "0.0"
          right_type_str = float_type
        end
        # Handle "null" literals in float operations - convert to 0.0
        if left == "null"
          left = "0.0"
          operand_type_str = float_type
        end
        if right == "null"
          right = "0.0"
          right_type_str = float_type
        end
        # Extract union operands for float ops (treat payload as float/double).
        if operand_type_str.includes?(".union")
          emit "%binop#{inst.id}.left_union_ptr = alloca #{operand_type_str}, align 8"
          emit "store #{operand_type_str} #{normalize_union_value(left, operand_type_str)}, ptr %binop#{inst.id}.left_union_ptr"
          emit "%binop#{inst.id}.left_union_payload_ptr = getelementptr #{operand_type_str}, ptr %binop#{inst.id}.left_union_ptr, i32 0, i32 1"
          emit "%binop#{inst.id}.left_union_val = load #{float_type}, ptr %binop#{inst.id}.left_union_payload_ptr, align 4"
          left = "%binop#{inst.id}.left_union_val"
          operand_type_str = float_type
        end
        if right_type_str.includes?(".union")
          emit "%binop#{inst.id}.right_union_ptr = alloca #{right_type_str}, align 8"
          emit "store #{right_type_str} #{normalize_union_value(right, right_type_str)}, ptr %binop#{inst.id}.right_union_ptr"
          emit "%binop#{inst.id}.right_union_payload_ptr = getelementptr #{right_type_str}, ptr %binop#{inst.id}.right_union_ptr, i32 0, i32 1"
          emit "%binop#{inst.id}.right_union_val = load #{float_type}, ptr %binop#{inst.id}.right_union_payload_ptr, align 4"
          right = "%binop#{inst.id}.right_union_val"
          right_type_str = float_type
        end
        # Convert integer literals to float format for float operations
        # Integer literals are numeric strings without "%" prefix and without "."
        if !left.starts_with?('%') && left =~ /^-?\d+$/ && !left.includes?('.')
          left = "#{left}.0"
          operand_type_str = float_type
        end
        if !right.starts_with?('%') && right =~ /^-?\d+$/ && !right.includes?('.')
          right = "#{right}.0"
          right_type_str = float_type
        end
        # Convert ptr operands to float (ptrtoint then sitofp/uitofp)
        if left.starts_with?('%') && operand_type_str == "ptr"
          emit "%binop#{inst.id}.left_ptrtoint = ptrtoint ptr #{left} to i64"
          emit "%binop#{inst.id}.left_itof = uitofp i64 %binop#{inst.id}.left_ptrtoint to #{float_type}"
          left = "%binop#{inst.id}.left_itof"
          operand_type_str = float_type
        end
        if right.starts_with?('%') && right_type_str == "ptr"
          emit "%binop#{inst.id}.right_ptrtoint = ptrtoint ptr #{right} to i64"
          emit "%binop#{inst.id}.right_itof = uitofp i64 %binop#{inst.id}.right_ptrtoint to #{float_type}"
          right = "%binop#{inst.id}.right_itof"
          right_type_str = float_type
        end
        # Convert integer SSA values to float for float operations
        if left.starts_with?('%') && operand_type_str.starts_with?('i')
          op = unsigned_type_ref?(operand_type) ? "uitofp" : "sitofp"
          emit "%binop#{inst.id}.left_itof = #{op} #{operand_type_str} #{left} to #{float_type}"
          left = "%binop#{inst.id}.left_itof"
          operand_type_str = float_type
        end
        if right.starts_with?('%') && right_type_str.starts_with?('i')
          op = right_type && unsigned_type_ref?(right_type) ? "uitofp" : "sitofp"
          emit "%binop#{inst.id}.right_itof = #{op} #{right_type_str} #{right} to #{float_type}"
          right = "%binop#{inst.id}.right_itof"
          right_type_str = float_type
        end
        # Update result type to match float type
        result_type = float_type
      end

      # Float ops with union result type also need wrapping (parallel to integer wrap logic)
      if union_result_type && is_float_op && is_arithmetic && !wrap_union_result
        wrap_union_result = true
      end

      needs_int_operands = (is_arithmetic || is_comparison) && !is_float_op
      if needs_int_operands
        # Helper to check if type is valid for int conversion
        valid_int = ->(t : String) { t != "ptr" && t != "void" && !t.includes?(".union") && t != "float" && t != "double" }

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
        if operand_type_str == "ptr" || right_type_str == "ptr"
          int_type = pointer_sized_int_llvm_type
        end
        if operand_type_str == "ptr"
          # Convert 0 to null for ptr type
          left_ptr = left == "0" ? "null" : left
          emit "%binop#{inst.id}.left = ptrtoint ptr #{left_ptr} to #{int_type}"
          left = "%binop#{inst.id}.left"
          operand_type_str = int_type
        end
        if right_type_str == "ptr"
          # Convert 0 to null for ptr type
          right_ptr = right == "0" ? "null" : right
          emit "%binop#{inst.id}.right = ptrtoint ptr #{right_ptr} to #{int_type}"
          right = "%binop#{inst.id}.right"
          right_type_str = int_type
        end

        # Handle void operands - replace with 0 of appropriate type (value was likely never defined)
        if operand_type_str == "void"
          left = "0"
          operand_type_str = int_type
        end
        if right_type_str == "void"
          right = "0"
          right_type_str = int_type
        end

        # Handle union operands - extract payload as ptr, then convert to int
        if operand_type_str.includes?(".union")
          emit "%binop#{inst.id}.left_union_ptr = alloca #{operand_type_str}, align 8"
          emit "store #{operand_type_str} #{normalize_union_value(left, operand_type_str)}, ptr %binop#{inst.id}.left_union_ptr"
          emit "%binop#{inst.id}.left_payload_ptr = getelementptr #{operand_type_str}, ptr %binop#{inst.id}.left_union_ptr, i32 0, i32 1"
          emit "%binop#{inst.id}.left_as_ptr = load ptr, ptr %binop#{inst.id}.left_payload_ptr, align 4"
          emit "%binop#{inst.id}.left_as_int = ptrtoint ptr %binop#{inst.id}.left_as_ptr to #{int_type}"
          left = "%binop#{inst.id}.left_as_int"
          operand_type_str = int_type
        end
        if right_type_str.includes?(".union")
          emit "%binop#{inst.id}.right_union_ptr = alloca #{right_type_str}, align 8"
          emit "store #{right_type_str} #{normalize_union_value(right, right_type_str)}, ptr %binop#{inst.id}.right_union_ptr"
          emit "%binop#{inst.id}.right_payload_ptr = getelementptr #{right_type_str}, ptr %binop#{inst.id}.right_union_ptr, i32 0, i32 1"
          emit "%binop#{inst.id}.right_as_ptr = load ptr, ptr %binop#{inst.id}.right_payload_ptr, align 4"
          emit "%binop#{inst.id}.right_as_int = ptrtoint ptr %binop#{inst.id}.right_as_ptr to #{int_type}"
          right = "%binop#{inst.id}.right_as_int"
          right_type_str = int_type
        end

        # After extracting union operands to integers, update result_type if it's also a union
        if union_result_type && is_arithmetic
          # We'll compute the raw arithmetic in an integer type and wrap it back into the union.
          wrap_union_result = true
          result_type = int_type
        end

        # Handle int type size mismatch (e.g., i32 + i64)
        # Only do size matching for actual integer types, not unions or other types
        left_is_int = operand_type_str.starts_with?('i') && !operand_type_str.includes?('.')
        right_is_int = right_type_str.starts_with?('i') && !right_type_str.includes?('.')
        if operand_type_str != right_type_str && left_is_int && right_is_int
          # Convert smaller type to larger type
          # Use zext for unsigned types, sext for signed types
          left_bits = operand_type_str[1..-1].to_i? || 32
          right_bits = right_type_str[1..-1].to_i? || 32
          if left_bits < right_bits
            # Extend left to match right
            left_type = @value_types[inst.left]?
            ext_op = (left_type && unsigned_type_ref?(left_type)) ? "zext" : "sext"
            emit "%binop#{inst.id}.left_ext = #{ext_op} #{operand_type_str} #{left} to #{right_type_str}"
            left = "%binop#{inst.id}.left_ext"
            operand_type_str = right_type_str
            result_type = right_type_str if is_arithmetic
          elsif right_bits < left_bits
            # Extend right to match left
            right_type = @value_types[inst.right]?
            ext_op = (right_type && unsigned_type_ref?(right_type)) ? "zext" : "sext"
            emit "%binop#{inst.id}.right_ext = #{ext_op} #{right_type_str} #{right} to #{operand_type_str}"
            right = "%binop#{inst.id}.right_ext"
            right_type_str = operand_type_str
            result_type = operand_type_str if is_arithmetic
          end
        end

      end

      # Handle float/double type mismatch (MUST be outside needs_int_operands block!)
      # This handles cases like: fmul double %r15, float %max
      if is_float_op && operand_type_str != right_type_str
        left_is_float = operand_type_str == "float" || operand_type_str == "double"
        right_is_float = right_type_str == "float" || right_type_str == "double"
        if left_is_float && right_is_float
          # Convert float to double (always widen to larger type)
          if operand_type_str == "float" && right_type_str == "double"
            emit "%binop#{inst.id}.left_fpext = fpext float #{left} to double"
            left = "%binop#{inst.id}.left_fpext"
            operand_type_str = "double"
            result_type = "double"
          elsif operand_type_str == "double" && right_type_str == "float"
            emit "%binop#{inst.id}.right_fpext = fpext float #{right} to double"
            right = "%binop#{inst.id}.right_fpext"
            right_type_str = "double"
            result_type = "double"
          end
        end
      end

      # Determine signedness from both operands for correct mixed-sign handling.
      # When comparing signed Int32 against unsigned UInt32, UInt32::MAX (0xFFFFFFFF)
      # is -1 in signed i32 → icmp sgt i32 5, -1 = true → wrong!
      # Fix: promote both to wider signed type (i64) before comparing.
      left_is_signed = operand_type.id >= TypeRef::INT8.id && operand_type.id <= TypeRef::INT128.id
      left_is_unsigned = operand_type.id >= TypeRef::UINT8.id && operand_type.id <= TypeRef::UINT128.id
      right_is_signed = right_type ? (right_type.id >= TypeRef::INT8.id && right_type.id <= TypeRef::INT128.id) : left_is_signed
      right_is_unsigned = right_type ? (right_type.id >= TypeRef::UINT8.id && right_type.id <= TypeRef::UINT128.id) : left_is_unsigned

      mixed_sign = (left_is_signed && right_is_unsigned) || (left_is_unsigned && right_is_signed)

      if mixed_sign && is_comparison && operand_type_str.starts_with?('i') && right_type_str.starts_with?('i')
        # Promote both operands to a wider signed type that can represent both ranges
        left_bits = operand_type_str[1..].to_i? || 32
        right_bits = right_type_str[1..].to_i? || 32
        max_bits = {left_bits, right_bits}.max
        wider_bits = max_bits < 64 ? max_bits * 2 : (max_bits < 128 ? 128 : 0)

        if wider_bits > 0
          wider_type = "i#{wider_bits}"
          if left_bits < wider_bits
            emit "%mix.#{inst.id}.left = #{left_is_signed ? "sext" : "zext"} #{operand_type_str} #{left} to #{wider_type}"
            left = "%mix.#{inst.id}.left"
          end
          if right_bits < wider_bits
            emit "%mix.#{inst.id}.right = #{right_is_signed ? "sext" : "zext"} #{right_type_str} #{right} to #{wider_type}"
            right = "%mix.#{inst.id}.right"
          end
          operand_type_str = wider_type
          right_type_str = wider_type
          is_signed = true  # signed comparison on promoted values
        else
          # Can't promote further (i128 vs i128); use unsigned comparison
          is_signed = false
        end
      else
        is_signed = left_is_signed || right_is_signed || (operand_type.id <= TypeRef::INT128.id)
      end

      # Use float operations for float/double types
      op = if is_float_op
             case inst.op
             when .add? then "fadd"
             when .sub? then "fsub"
             when .mul? then "fmul"
             when .div? then "fdiv"
             when .rem? then "frem"
             when .eq?  then "fcmp oeq"
             when .ne?  then "fcmp une"
             when .lt?  then "fcmp olt"
             when .le?  then "fcmp ole"
             when .gt?  then "fcmp ogt"
             when .ge?  then "fcmp oge"
             else            "fadd"
             end
           else
             case inst.op
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
           end

      if op.starts_with?("icmp")
        # Comparisons need both operands to be same type
        # If sizes differ, extend smaller to larger type
        cmp_type = operand_type_str
        if operand_type_str != right_type_str && operand_type_str.starts_with?('i') && right_type_str.starts_with?('i')
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
        record_emitted_type(name, "i1")
      elsif op.starts_with?("fcmp")
        # Float comparisons - use float/double type
        cmp_type = operand_type_str == "double" || right_type_str == "double" ? "double" : "float"
        emit "#{name} = #{op} #{cmp_type} #{left}, #{right}"
        @value_types[inst.id] = TypeRef::BOOL
        record_emitted_type(name, "i1")
      else
        if wrap_union_result
          # Compute raw integer result first, then wrap into the union type expected by MIR.
          raw_name = "%binop#{inst.id}.raw"

          emit "#{raw_name} = #{op} #{result_type} #{left}, #{right}"

          # Pick the union variant matching the actual result type.
          # For float results, pick a float variant; for int results, pick an int variant.
          variant_type_id = 0
          variant_type_ref = TypeRef::INT32
          if descriptor = @module.get_union_descriptor(inst.type)
            is_float_result = result_type == "float" || result_type == "double"
            # Try to find a variant matching the result kind (float or int)
            best_variant = descriptor.variants.find do |v|
              next if v.type_ref == TypeRef::NIL || v.type_ref == TypeRef::VOID
              vt = @type_mapper.llvm_type(v.type_ref)
              is_float_result ? (vt == "float" || vt == "double") : vt.starts_with?('i')
            end
            # Fallback to any non-nil variant
            best_variant ||= descriptor.variants.find { |v| v.type_ref != TypeRef::NIL && v.type_ref != TypeRef::VOID }
            if best_variant
              variant_type_id = best_variant.type_id
              variant_type_ref = best_variant.type_ref
            end
          end

          union_type = union_result_type.not_nil!
          base_name = name.lstrip('%')
          emit "%#{base_name}.ptr = alloca #{union_type}, align 8"
          emit "%#{base_name}.type_id_ptr = getelementptr #{union_type}, ptr %#{base_name}.ptr, i32 0, i32 0"
          emit "store i32 #{variant_type_id}, ptr %#{base_name}.type_id_ptr"

          payload_ptr = "%#{base_name}.payload_ptr"
          emit "#{payload_ptr} = getelementptr #{union_type}, ptr %#{base_name}.ptr, i32 0, i32 1"

          payload_type = @type_mapper.llvm_type(variant_type_ref)
          if payload_type.includes?(".union")
            if ENV.has_key?("DEBUG_UNION_PAYLOAD")
              STDERR.puts "[UNION_PAYLOAD] union=#{union_type} payload=#{payload_type} raw=#{result_type}"
            end
            payload_type = result_type
          end
          payload_val = raw_name
          if payload_type != result_type
            if payload_type.starts_with?('i') && result_type.starts_with?('i')
              payload_bits = payload_type[1..].to_i? || 32
              raw_bits = result_type[1..].to_i? || 32
              if raw_bits > payload_bits
                emit "%binop#{inst.id}.raw_trunc = trunc #{result_type} #{raw_name} to #{payload_type}"
                payload_val = "%binop#{inst.id}.raw_trunc"
              elsif raw_bits < payload_bits
                emit "%binop#{inst.id}.raw_ext = sext #{result_type} #{raw_name} to #{payload_type}"
                payload_val = "%binop#{inst.id}.raw_ext"
              end
            elsif (payload_type == "float" || payload_type == "double") && result_type.starts_with?('i')
              op = (unsigned_type_ref?(operand_type) || (right_type && unsigned_type_ref?(right_type))) ? "uitofp" : "sitofp"
              emit "%binop#{inst.id}.raw_itof = #{op} #{result_type} #{raw_name} to #{payload_type}"
              payload_val = "%binop#{inst.id}.raw_itof"
            elsif payload_type == "ptr" && result_type.starts_with?('i')
              emit "%binop#{inst.id}.raw_inttoptr = inttoptr #{result_type} #{raw_name} to ptr"
              payload_val = "%binop#{inst.id}.raw_inttoptr"
            elsif payload_type.starts_with?('i') && result_type == "ptr"
              emit "%binop#{inst.id}.raw_ptrtoint = ptrtoint ptr #{raw_name} to #{payload_type}"
              payload_val = "%binop#{inst.id}.raw_ptrtoint"
            elsif payload_type.starts_with?('i') && (result_type == "float" || result_type == "double")
              unsigned_payload = unsigned_type_ref?(variant_type_ref)
              op = unsigned_payload ? "fptoui" : "fptosi"
              emit "%binop#{inst.id}.raw_ftoi = #{op} #{result_type} #{raw_name} to #{payload_type}"
              payload_val = "%binop#{inst.id}.raw_ftoi"
            elsif payload_type == "double" && result_type == "float"
              emit "%binop#{inst.id}.raw_fpext = fpext float #{raw_name} to double"
              payload_val = "%binop#{inst.id}.raw_fpext"
            elsif payload_type == "float" && result_type == "double"
              emit "%binop#{inst.id}.raw_fptrunc = fptrunc double #{raw_name} to float"
              payload_val = "%binop#{inst.id}.raw_fptrunc"
            end
          end

          # If no conversion happened and types still mismatch, store using result_type directly.
          # This handles cases where MIR assigns a wrong union type (e.g., Slice|Nil for an fadd)
          # — the payload memory is large enough for any scalar value.
          store_type = payload_type
          if payload_val == raw_name && payload_type != result_type
            store_type = result_type
          end
          emit "store #{store_type} #{payload_val}, ptr #{payload_ptr}, align 4"
          emit "#{name} = load #{union_type}, ptr %#{base_name}.ptr"
          @value_types[inst.id] = inst.type
          record_emitted_type(name, union_type)
          return
        end

        # Ensure operands match result_type for arithmetic ops
        # If operand is larger than result_type, use operand type instead (don't truncate)
        if result_type.starts_with?('i') && result_type != "i1"
          result_bits = result_type[1..].to_i? || 32
          operand_bits = operand_type_str.starts_with?('i') ? (operand_type_str[1..].to_i? || 32) : 0
          right_bits_val = right_type_str.starts_with?('i') ? (right_type_str[1..].to_i? || 32) : 0

          # If operands are larger than result, use the larger operand type
          max_operand_bits = {operand_bits, right_bits_val}.max
          if max_operand_bits > result_bits
            result_type = "i#{max_operand_bits}"
          end

          # Extend smaller operands to result_type
          result_bits = result_type[1..].to_i? || 32
          if operand_type_str.starts_with?('i') && operand_type_str != result_type
            operand_bits = operand_type_str[1..].to_i? || 32
            if operand_bits < result_bits
              emit "%binop#{inst.id}.left_to_result = sext #{operand_type_str} #{left} to #{result_type}"
              left = "%binop#{inst.id}.left_to_result"
            end
          end
          if right_type_str.starts_with?('i') && right_type_str != result_type
            right_bits_check = right_type_str[1..].to_i? || 32
            if right_bits_check < result_bits
              emit "%binop#{inst.id}.right_to_result = sext #{right_type_str} #{right} to #{result_type}"
              right = "%binop#{inst.id}.right_to_result"
            end
          end
        end
        # If MIR expects ptr but we did arithmetic as int, convert back to ptr
        if convert_result_to_ptr && result_type.starts_with?('i')
          emit "%binop#{inst.id}.int_result = #{op} #{result_type} #{left}, #{right}"
          emit "#{name} = inttoptr #{result_type} %binop#{inst.id}.int_result to ptr"
          @value_types[inst.id] = TypeRef::POINTER
          @inttoptr_value_ids.add(inst.id)
          record_emitted_type(name, "ptr")
        else
          emit "#{name} = #{op} #{result_type} #{left}, #{right}"
          # Track actual emitted type for downstream use
          # Preserve unsigned-ness from prepass if either operand was unsigned
          prepass_was_unsigned = if pt = @value_types[inst.id]?
                                   unsigned_type_ref?(pt)
                                 else
                                   false
                                 end
          actual_type = case result_type
                        when "i1" then TypeRef::BOOL
                        when "i8" then prepass_was_unsigned ? TypeRef::UINT8 : TypeRef::INT8
                        when "i16" then prepass_was_unsigned ? TypeRef::UINT16 : TypeRef::INT16
                        when "i32" then prepass_was_unsigned ? TypeRef::UINT32 : TypeRef::INT32
                        when "i64" then prepass_was_unsigned ? TypeRef::UINT64 : TypeRef::INT64
                        when "i128" then prepass_was_unsigned ? TypeRef::UINT128 : TypeRef::INT128
                        when "float" then TypeRef::FLOAT32
                        when "double" then TypeRef::FLOAT64
                        when "ptr" then TypeRef::POINTER
                        else inst.type  # Use MIR type as fallback
                        end
          @value_types[inst.id] = actual_type
          record_emitted_type(name, result_type)
        end
      end
    end

    private def emit_unary_op(inst : UnaryOp, name : String)
      type = @type_mapper.llvm_type(inst.type)
      # Void type is invalid for unary ops - use i64 as fallback
      if type == "void"
        type = "i64"
        @value_types[inst.id] = TypeRef::INT64
      end
      operand = value_ref(inst.operand)
      operand_type = @value_types[inst.operand]? || TypeRef::BOOL
      operand_llvm_type = @type_mapper.llvm_type(operand_type)

      # Also check parameter types for operand (similar to binary op handling)
      if !operand.starts_with?("%r") && operand.starts_with?('%')
        param_name = operand[1..]  # Remove %
        if param_type = current_func_param_type_by_llvm_name(param_name)
          operand_llvm_type = @type_mapper.llvm_type(param_type)
        end
      end
      # Prefer the actually emitted LLVM type when available (for example,
      # when a call result type differs from static/prepass metadata).
      if emitted_operand_type = @emitted_value_types[operand]?
        operand_llvm_type = emitted_operand_type
      end

      case inst.op
      when .neg?
        if operand_llvm_type.includes?(".union")
          # Union negation: extract payload, negate, wrap back into union.
          base_name = name.lstrip('%')
          union_type = operand_llvm_type
          emit "%#{base_name}.neg_union_ptr = alloca #{union_type}, align 8"
          emit "store #{union_type} #{normalize_union_value(operand, union_type)}, ptr %#{base_name}.neg_union_ptr"
          emit "%#{base_name}.neg_payload_ptr = getelementptr #{union_type}, ptr %#{base_name}.neg_union_ptr, i32 0, i32 1"

          variant_type_id = 0
          variant_type_ref = TypeRef::INT32
          payload_type = "i32"
          if descriptor = @module.get_union_descriptor(inst.type)
            # Prefer float variants for negation when present
            float_variant = descriptor.variants.find do |v|
              vt = @type_mapper.llvm_type(v.type_ref)
              vt == "double" || vt == "float"
            end
            if float_variant
              variant_type_id = float_variant.type_id
              variant_type_ref = float_variant.type_ref
              payload_type = @type_mapper.llvm_type(float_variant.type_ref)
            else
              int_variant = descriptor.variants.find do |v|
                vt = @type_mapper.llvm_type(v.type_ref)
                vt.starts_with?('i')
              end
              if int_variant
                variant_type_id = int_variant.type_id
                variant_type_ref = int_variant.type_ref
                payload_type = @type_mapper.llvm_type(int_variant.type_ref)
              end
            end
          end

          emit "%#{base_name}.neg_val = load #{payload_type}, ptr %#{base_name}.neg_payload_ptr, align 4"
          if payload_type == "float" || payload_type == "double"
            emit "%#{base_name}.neg_result = fneg #{payload_type} %#{base_name}.neg_val"
          else
            emit "%#{base_name}.neg_result = sub #{payload_type} 0, %#{base_name}.neg_val"
          end

          emit "%#{base_name}.neg_res_ptr = alloca #{union_type}, align 8"
          emit "%#{base_name}.neg_tid_ptr = getelementptr #{union_type}, ptr %#{base_name}.neg_res_ptr, i32 0, i32 0"
          emit "store i32 #{variant_type_id}, ptr %#{base_name}.neg_tid_ptr"
          emit "%#{base_name}.neg_res_payload_ptr = getelementptr #{union_type}, ptr %#{base_name}.neg_res_ptr, i32 0, i32 1"
          emit "store #{payload_type} %#{base_name}.neg_result, ptr %#{base_name}.neg_res_payload_ptr, align 4"
          emit "#{name} = load #{union_type}, ptr %#{base_name}.neg_res_ptr"
          @value_types[inst.id] = inst.type
          return
        end
        # Handle ptr operand for negation - convert to i64 first
        if operand_llvm_type == "ptr"
          base_name = name.lstrip('%')
          emit "%#{base_name}.ptr_to_int = ptrtoint ptr #{operand} to i64"
          emit "#{name} = sub i64 0, %#{base_name}.ptr_to_int"
          @value_types[inst.id] = TypeRef::INT64  # Negated ptr becomes i64
        elsif operand_llvm_type == "float" || operand_llvm_type == "double"
          # Float negation uses fneg (preserves -0.0 correctly, unlike fsub 0.0)
          emit "#{name} = fneg #{operand_llvm_type} #{operand}"
          @value_types[inst.id] = operand_type
        else
          # Handle type mismatch: if result type is larger than operand type, extend operand
          actual_type = type
          actual_operand = operand
          result_type_ref = operand_type  # Default to preserving operand type

          # If result type is ptr but operand is integer, use operand type
          # Can't do arithmetic on ptr with integer constants
          if type == "ptr" && operand_llvm_type.starts_with?('i')
            actual_type = operand_llvm_type
            result_type_ref = operand_type
          elsif operand_llvm_type != type && operand_llvm_type.starts_with?('i') && type.starts_with?('i')
            operand_bits = operand_llvm_type[1..].to_i? || 32
            type_bits = type[1..].to_i? || 32
            if operand_bits < type_bits
              # Extend operand to match result type
              base_name = name.lstrip('%')
              emit "%#{base_name}.neg_ext = sext #{operand_llvm_type} #{operand} to #{type}"
              actual_operand = "%#{base_name}.neg_ext"
              # Track that result is now the larger type
              result_type_ref = case type
                                when "i64" then TypeRef::INT64
                                when "i32" then TypeRef::INT32
                                when "i16" then TypeRef::INT16
                                when "i8" then TypeRef::INT8
                                else inst.type
                                end
            elsif operand_bits > type_bits
              # Use operand type if larger (shouldn't happen, but be safe)
              actual_type = operand_llvm_type
            end
          end
          emit "#{name} = sub #{actual_type} 0, #{actual_operand}"
          @value_types[inst.id] = result_type_ref
        end
      when .not?
        # NOT needs boolean operand - convert if needed
        if operand_llvm_type != "i1"
          base_name = name.lstrip('%')
          if operand_llvm_type == "void"
            # void operand - treat as false, so NOT returns true
            emit "%#{base_name}.bool = add i1 0, 0"  # false
            emit "#{name} = xor i1 %#{base_name}.bool, 1"  # NOT false = true
          elsif operand_llvm_type == "ptr"
            ptr_operand = operand == "0" ? "null" : operand
            emit "%#{base_name}.bool = icmp ne ptr #{ptr_operand}, null"
            emit "#{name} = xor i1 %#{base_name}.bool, 1"
          elsif operand_llvm_type == "float" || operand_llvm_type == "double"
            emit "%#{base_name}.bool = fcmp one #{operand_llvm_type} #{operand}, 0.0"
            emit "#{name} = xor i1 %#{base_name}.bool, 1"
          elsif operand_llvm_type.includes?(".union")
            # For unions, check if type_id != 0 (0 = nil)
            emit "%#{base_name}.union_ptr = alloca #{operand_llvm_type}, align 8"
            emit "store #{operand_llvm_type} #{normalize_union_value(operand, operand_llvm_type)}, ptr %#{base_name}.union_ptr"
            emit "%#{base_name}.type_id_ptr = getelementptr #{operand_llvm_type}, ptr %#{base_name}.union_ptr, i32 0, i32 0"
            emit "%#{base_name}.type_id = load i32, ptr %#{base_name}.type_id_ptr"
            nil_vid = nil_variant_id_for_union_type(operand_llvm_type) || 0
            emit "%#{base_name}.bool = icmp ne i32 %#{base_name}.type_id, #{nil_vid}"
            emit "#{name} = xor i1 %#{base_name}.bool, 1"
          else
            emit "%#{base_name}.bool = icmp ne #{operand_llvm_type} #{operand}, 0"
            emit "#{name} = xor i1 %#{base_name}.bool, 1"
          end
        else
          emit "#{name} = xor i1 #{operand}, 1"
        end
        @value_types[inst.id] = TypeRef::BOOL  # NOT always returns i1
      when .bit_not?
        # Use operand_llvm_type which is more accurately tracked than inst.type
        actual_type = operand_llvm_type
        actual_type = "i64" if actual_type == "void"  # void operand defaults to i64

        if actual_type == "ptr"
          # Can't xor ptr directly - convert to i64, xor, convert back
          base_name = name.lstrip('%')
          emit "%#{base_name}.ptr_to_int = ptrtoint ptr #{operand} to i64"
          emit "%#{base_name}.xor = xor i64 %#{base_name}.ptr_to_int, -1"
          emit "#{name} = inttoptr i64 %#{base_name}.xor to ptr"
          @value_types[inst.id] = TypeRef::POINTER
          @inttoptr_value_ids.add(inst.id)
        elsif operand_llvm_type == "void"
          # Void operand - use 0 as default value, ~0 = -1
          emit "#{name} = xor i64 0, -1"
          @value_types[inst.id] = TypeRef::INT64
        elsif actual_type.includes?(".union")
          # Union type - extract payload as integer, xor, wrap back
          base_name = name.lstrip('%')
          emit "%#{base_name}.bitnot_union_ptr = alloca #{actual_type}, align 8"
          emit "store #{actual_type} #{operand}, ptr %#{base_name}.bitnot_union_ptr"
          emit "%#{base_name}.bitnot_payload_ptr = getelementptr #{actual_type}, ptr %#{base_name}.bitnot_union_ptr, i32 0, i32 1"
          emit "%#{base_name}.bitnot_val = load i64, ptr %#{base_name}.bitnot_payload_ptr, align 4"
          emit "%#{base_name}.bitnot_result = xor i64 %#{base_name}.bitnot_val, -1"
          # Store result back into union
          emit "%#{base_name}.bitnot_res_ptr = alloca #{actual_type}, align 8"
          emit "%#{base_name}.bitnot_tid_ptr = getelementptr #{actual_type}, ptr %#{base_name}.bitnot_res_ptr, i32 0, i32 0"
          emit "%#{base_name}.bitnot_orig_tid = load i32, ptr %#{base_name}.bitnot_union_ptr"
          emit "store i32 %#{base_name}.bitnot_orig_tid, ptr %#{base_name}.bitnot_tid_ptr"
          emit "%#{base_name}.bitnot_res_payload_ptr = getelementptr #{actual_type}, ptr %#{base_name}.bitnot_res_ptr, i32 0, i32 1"
          emit "store i64 %#{base_name}.bitnot_result, ptr %#{base_name}.bitnot_res_payload_ptr, align 4"
          emit "#{name} = load #{actual_type}, ptr %#{base_name}.bitnot_res_ptr"
          @value_types[inst.id] = operand_type
        else
          emit "#{name} = xor #{actual_type} #{operand}, -1"
          @value_types[inst.id] = operand_type  # bit_not preserves operand type
        end
      end
    end

    private def emit_cast(inst : Cast, name : String)
      # Get source type from value_types registry
      src_type_ref = @value_types[inst.value]? || TypeRef::POINTER
      src_type = @type_mapper.llvm_type(src_type_ref)
      dst_type = @type_mapper.llvm_type(inst.type)
      value = value_ref(inst.value)
      # If the value was emitted earlier with a known LLVM type, prefer it.
      emitted_type = @emitted_value_types[value]?
      if emitted_type
        src_type = emitted_type
        src_type_ref = case emitted_type
                       when "double" then TypeRef::FLOAT64
                       when "float"  then TypeRef::FLOAT32
                       when "i64"    then TypeRef::INT64
                       when "i32"    then TypeRef::INT32
                       when "i16"    then TypeRef::INT16
                       when "i8"     then TypeRef::INT8
                       when "i1"     then TypeRef::BOOL
                       when "ptr"    then TypeRef::POINTER
                       else src_type_ref
                       end
      end
      # If this value is stored in a cross-block slot, prefer the slot's LLVM type.
      # Skip if we already have an emitted cast type for the value.
      if !emitted_type && (slot_type = @cross_block_slot_types[inst.value]?)
        if slot_type != src_type && slot_type != "void"
          src_type = slot_type
          src_type_ref = case slot_type
                         when "double" then TypeRef::FLOAT64
                         when "float"  then TypeRef::FLOAT32
                         when "i64"    then TypeRef::INT64
                         when "i32"    then TypeRef::INT32
                         when "i16"    then TypeRef::INT16
                         when "i8"     then TypeRef::INT8
                         else src_type_ref
                         end
        end
      end

      # Fallback: if the value was loaded from a cross-block slot, recover slot type from the name.
      if !emitted_type && src_type == "ptr" && (match = value.match(/^%r(\d+)\.fromslot/))
        slot_id = ValueId.new(match[1].to_i)
        if slot_type = @cross_block_slot_types[slot_id]?
          if slot_type != "void"
            src_type = slot_type
            src_type_ref = case slot_type
                           when "double" then TypeRef::FLOAT64
                           when "float"  then TypeRef::FLOAT32
                           when "i64"    then TypeRef::INT64
                           when "i32"    then TypeRef::INT32
                           when "i16"    then TypeRef::INT16
                           when "i8"     then TypeRef::INT8
                           else src_type_ref
                           end
          end
        end
      end

      # Void destination: no SSA value should be emitted.
      if dst_type == "void"
        @void_values << inst.id
        @value_types[inst.id] = inst.type
        return
      end

      # No-op cast: emit an identity instruction to define the SSA name.
      if src_type == dst_type
        base_name = name.lstrip('%')
        if dst_type == "ptr"
          emit "#{name} = bitcast ptr #{value} to ptr"
        elsif dst_type == "float"
          emit "#{name} = fadd float #{value}, 0.0"
        elsif dst_type == "double"
          emit "#{name} = fadd double #{value}, 0.0"
        elsif dst_type.starts_with?('i')
          emit "#{name} = add #{dst_type} #{value}, 0"
        else
          emit "%#{base_name}.tmp = alloca #{dst_type}, align 8"
          emit "store #{dst_type} #{value}, ptr %#{base_name}.tmp"
          emit "#{name} = load #{dst_type}, ptr %#{base_name}.tmp"
        end
        record_emitted_type(name, dst_type)
        @value_types[inst.id] = inst.type
        return
      end

      # Special case: casting to union type (e.g., nil → Option(T))
      # Can't bitcast ptr/value to union struct - must construct it
      if dst_type.includes?(".union")
        base_name = name.lstrip('%')
        emit "%#{base_name}.ptr = alloca #{dst_type}, align 8"
        emit "%#{base_name}.type_id_ptr = getelementptr #{dst_type}, ptr %#{base_name}.ptr, i32 0, i32 0"

        # Check if source is null/nil - create nil union
        is_nil_cast = value == "null" || src_type == "void" || src_type_ref == TypeRef::VOID
        variant_type_id = 0
        if desc = @module.union_descriptors[inst.type]?
          if is_nil_cast
            nil_variant = desc.variants.find { |v| v.full_name == "Nil" || v.full_name == "Void" }
            variant_type_id = nil_variant ? nil_variant.type_id : 0
          else
            matched_variant = desc.variants.find { |v| v.type_ref == src_type_ref }
            unless matched_variant
              matched_variant = desc.variants.find do |v|
                @type_mapper.llvm_type(v.type_ref) == src_type
              end
            end
            unless matched_variant
              matched_variant = desc.variants.find { |v| v.full_name != "Nil" && v.full_name != "Void" }
            end
            variant_type_id = matched_variant ? matched_variant.type_id : 1
          end
        else
          variant_type_id = is_nil_cast ? 0 : 1
        end
        if is_nil_cast
          emit "store i32 #{variant_type_id}, ptr %#{base_name}.type_id_ptr"
        else
          emit "store i32 #{variant_type_id}, ptr %#{base_name}.type_id_ptr"
          emit "%#{base_name}.payload_ptr = getelementptr #{dst_type}, ptr %#{base_name}.ptr, i32 0, i32 1"
          emit "store #{src_type} #{value}, ptr %#{base_name}.payload_ptr, align 4"
        end
        emit "#{name} = load #{dst_type}, ptr %#{base_name}.ptr"
        record_emitted_type(name, dst_type)
        @value_types[inst.id] = inst.type
        return
      end

      # Guard: can't cast from void - use null/0 as fallback
      if src_type == "void"
        if dst_type == "ptr" || dst_type.includes?(".union")
          emit "#{name} = inttoptr i64 0 to ptr"
          record_emitted_type(name, "ptr")
          @value_types[inst.id] = TypeRef::POINTER
          @inttoptr_value_ids.add(inst.id)
          return
        else
          # Non-ptr destination - use 0/0.0 as appropriate
          if dst_type == "float" || dst_type == "double"
            emit "#{name} = fadd #{dst_type} 0.0, 0.0"
          else
            emit "#{name} = add #{dst_type} 0, 0"
          end
          record_emitted_type(name, dst_type)
          @value_types[inst.id] = inst.type
          return
        end
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

      # Guard: bitcast can't convert between int and ptr - override op
      is_src_int = src_type.starts_with?('i') && !src_type.includes?(".union")
      is_dst_int = dst_type.starts_with?('i') && !dst_type.includes?(".union")
      if op == "bitcast"
        if is_src_int && dst_type == "ptr"
          op = "inttoptr"
        elsif src_type == "ptr" && is_dst_int
          op = "ptrtoint"
        end
      end

      # Guard: bitcast between different-width integers is invalid in LLVM.
      if op == "bitcast" && is_src_int && is_dst_int && src_type != dst_type
        src_bits = src_type[1..].to_i
        dst_bits = dst_type[1..].to_i
        if dst_bits < src_bits
          op = "trunc"
        else
          signed_src = @module.type_registry.get(src_type_ref).try(&.kind).try(&.signed_integer?) || false
          op = signed_src ? "sext" : "zext"
        end
      end

      # Guard: zext/sext on narrowing casts is invalid; use trunc instead.
      if (op == "sext" || op == "zext") && is_src_int && is_dst_int && src_type != dst_type
        src_bits = src_type[1..].to_i?
        dst_bits = dst_type[1..].to_i?
        if src_bits && dst_bits && dst_bits < src_bits
          op = "trunc"
        end
      end

      # Guard: trunc/bitcast can't be used for float-to-int - use fptosi/fptoui instead
      is_src_float = src_type == "float" || src_type == "double"
      is_dst_float = dst_type == "float" || dst_type == "double"
      # Guard: MIR cast kind may request fp_to_si/fp_to_ui while the actually emitted
      # source value is already integer/pointer (stale type metadata). Emit a valid
      # integer-domain cast from real LLVM source type instead of invalid `fptosi i64`.
      if (op == "fptosi" || op == "fptoui") && !is_src_float
        if is_src_int && is_dst_int && src_type != dst_type
          src_bits = src_type[1..].to_i?
          dst_bits = dst_type[1..].to_i?
          if src_bits && dst_bits
            if dst_bits < src_bits
              op = "trunc"
            elsif dst_bits > src_bits
              signed_src = @module.type_registry.get(src_type_ref).try(&.kind).try(&.signed_integer?) || false
              op = signed_src ? "sext" : "zext"
            else
              op = "bitcast"
            end
          else
            op = "bitcast"
          end
        elsif src_type == "ptr" && is_dst_int
          op = "ptrtoint"
        end
      end
      if (op == "trunc" || op == "bitcast") && is_src_float && is_dst_int
        # Check if destination is unsigned type to choose fptoui vs fptosi
        dst_kind = @module.type_registry.get(inst.type).try(&.kind)
        dst_unsigned = dst_kind && dst_kind.integer? && !dst_kind.signed_integer?
        op = dst_unsigned ? "fptoui" : "fptosi"
      end

      # Guard: int-to-float casts.
      #
      # - For unsafe_as semantics we allow same-width bitcasts (i32<->float, i64<->double).
      # - For width-changing casts (i32->double, i64->float, etc.) bitcast/trunc is invalid;
      #   fall back to numeric conversion.
      if (op == "bitcast" || op == "trunc") && is_src_int && is_dst_float
        src_bits = src_type[1..].to_i?
        dst_bits = dst_type == "float" ? 32 : 64
        needs_numeric = op == "trunc"
        needs_numeric ||= src_bits && src_bits != dst_bits
        if needs_numeric
          signed_src = @module.type_registry.get(src_type_ref).try(&.kind).try(&.signed_integer?) || false
          op = signed_src ? "sitofp" : "uitofp"
        end
      end

      # Guard: float/double to ptr can't be bitcast directly. Bitcast to int bits, then inttoptr.
      if op == "bitcast" && is_src_float && dst_type == "ptr"
        base_name = name.lstrip('%')
        if src_type == "double"
          emit "%#{base_name}.float_bits = bitcast double #{value} to i64"
          emit "#{name} = inttoptr i64 %#{base_name}.float_bits to ptr"
        else
          emit "%#{base_name}.float_bits = bitcast float #{value} to i32"
          emit "%#{base_name}.float_bits.ext = zext i32 %#{base_name}.float_bits to i64"
          emit "#{name} = inttoptr i64 %#{base_name}.float_bits.ext to ptr"
        end
        record_emitted_type(name, "ptr")
        @value_types[inst.id] = TypeRef::POINTER
        return
      end

      # Guard: ptr to float can't be bitcast directly. Convert ptr value to unsigned int, then uitofp.
      if op == "bitcast" && src_type == "ptr" && is_dst_float
        base_name = name.lstrip('%')
        emit "%#{base_name}.ptr_int = ptrtoint ptr #{value} to i64"
        emit "#{name} = uitofp i64 %#{base_name}.ptr_int to #{dst_type}"
        @value_types[inst.id] = inst.type
        return
      end

      # Guard: sext/zext/trunc can't be used with ptr - use ptrtoint instead
      if (op == "sext" || op == "zext" || op == "trunc") && src_type == "ptr"
        # ptr to int - use ptrtoint
        emit "#{name} = ptrtoint ptr #{value} to #{dst_type}"
        @value_types[inst.id] = inst.type
        return
      end

      # Guard: sext/zext/trunc can't be used with void - use default 0
      if (op == "sext" || op == "zext" || op == "trunc") && src_type == "void"
        emit "#{name} = add #{dst_type} 0, 0"
        @value_types[inst.id] = inst.type
        return
      end

      # Guard: can't inttoptr union to ptr - extract payload instead
      if op == "inttoptr" && src_type.includes?(".union") && dst_type == "ptr"
        base_name = name.lstrip('%')
        emit "%#{base_name}.union_ptr = alloca #{src_type}, align 8"
        emit "store #{src_type} #{normalize_union_value(value, src_type)}, ptr %#{base_name}.union_ptr"
        emit "%#{base_name}.payload_ptr = getelementptr #{src_type}, ptr %#{base_name}.union_ptr, i32 0, i32 1"
        emit "#{name} = load ptr, ptr %#{base_name}.payload_ptr, align 4"
        record_emitted_type(name, "ptr")
        @value_types[inst.id] = TypeRef::POINTER
        return
      end

      # Guard: can't bitcast union to ptr - extract payload instead
      if op == "bitcast" && src_type.includes?(".union") && dst_type == "ptr"
        base_name = name.lstrip('%')
        emit "%#{base_name}.union_ptr = alloca #{src_type}, align 8"
        emit "store #{src_type} #{normalize_union_value(value, src_type)}, ptr %#{base_name}.union_ptr"
        emit "%#{base_name}.payload_ptr = getelementptr #{src_type}, ptr %#{base_name}.union_ptr, i32 0, i32 1"
        emit "#{name} = load ptr, ptr %#{base_name}.payload_ptr, align 4"
        record_emitted_type(name, "ptr")
        @value_types[inst.id] = TypeRef::POINTER
        return
      end

      # Guard: union to scalar cast - extract payload as the destination scalar type.
      # This avoids invalid LLVM bitcasts (union struct -> iN/float/double).
      if src_type.includes?(".union")
        is_dst_int = dst_type.starts_with?('i') && !dst_type.includes?(".union")
        is_dst_float = dst_type == "float" || dst_type == "double"
        if is_dst_int || is_dst_float
          base_name = name.lstrip('%')
          emit "%#{base_name}.union_ptr = alloca #{src_type}, align 8"
          emit "store #{src_type} #{normalize_union_value(value, src_type)}, ptr %#{base_name}.union_ptr"
          emit "%#{base_name}.payload_ptr = getelementptr #{src_type}, ptr %#{base_name}.union_ptr, i32 0, i32 1"
          emit "#{name} = load #{dst_type}, ptr %#{base_name}.payload_ptr, align 4"
          record_emitted_type(name, dst_type)
          @value_types[inst.id] = inst.type
          return
        end
      end

      # Guard: detect integer literal used with ptr type (type mismatch)
      # This happens when @value_types defaults to POINTER but actual value is int
      is_int_literal = value.match(/^-?\d+$/) && value != "null"
      if is_int_literal && src_type == "ptr"
        # Actual src type is int, not ptr - correct it
        src_type = "i64"
        src_type_ref = TypeRef::INT64
      end

      # Guard: can't convert to void - just pass through as ptr
      if dst_type == "void"
        if src_type == "ptr"
          # ptr to void conversion: emit identity bitcast (ptr to ptr)
          emit "#{name} = bitcast ptr #{value} to ptr"
          @value_types[inst.id] = TypeRef::POINTER
          return
        else
          # Other types to void: emit null ptr
          emit "#{name} = inttoptr i64 0 to ptr"
          @value_types[inst.id] = TypeRef::POINTER
          return
        end
      end

      # Preserve signedness for narrow int -> ptr casts.
      # Direct `inttoptr i32` on 64-bit targets effectively zero-extends and breaks
      # negative Int32 values in generic Int paths (puts/to_s through Int#internal_to_s).
      if op == "inttoptr" && is_src_int && dst_type == "ptr"
        if src_bits = src_type[1..].to_i?
          if src_bits < 64
            base_name = name.lstrip('%')
            ext_name = "%#{base_name}.inttoptr_ext"
            extend_op = (src_type_ref == TypeRef::BOOL || unsigned_type_ref?(src_type_ref)) ? "zext" : "sext"
            emit "#{ext_name} = #{extend_op} #{src_type} #{value} to i64"
            emit "#{name} = inttoptr i64 #{ext_name} to ptr"
            record_emitted_type(name, "ptr")
            @value_types[inst.id] = TypeRef::POINTER
            @inttoptr_value_ids.add(inst.id)
            return
          end
        end
      end

      # Guard: ptr to float/double - convert pointer VALUE (address) numerically.
      # Never dereference unknown pointers here.
      if src_type == "ptr" && (dst_type == "float" || dst_type == "double")
        base_name = name.lstrip('%')
        emit "%#{base_name}.ptr_int = ptrtoint ptr #{value} to i64"
        emit "#{name} = uitofp i64 %#{base_name}.ptr_int to #{dst_type}"
        @value_types[inst.id] = inst.type
        return
      end

      emit "#{name} = #{op} #{src_type} #{value} to #{dst_type}"
      record_emitted_type(name, dst_type)
      # Track actual destination type for downstream use
      @value_types[inst.id] = inst.type
      # Track inttoptr results for ptr→int conversion decisions
      if op == "inttoptr"
        @inttoptr_value_ids.add(inst.id)
      end
    end

    # Helper to get the phi incoming value reference, checking for predecessor loads first
    # Returns nil if the value should be skipped (handled by special case logic)
    # phi_type is the expected LLVM type of the phi node
    private def phi_incoming_ref(block : BlockId, val : ValueId, phi_type : String) : String?
      # Union wrap in predecessor block (for union phi nodes)
      if phi_type.includes?(".union")
        if wrap_info = @phi_predecessor_union_wraps[{block, val}]?
          wrap_name, _, _ = wrap_info
          return "%#{wrap_name}"
        end
        # Union-to-union reinterpretation in predecessor block
        if convert_entries = @phi_union_to_union_converts[{block, val}]?
          # Find the conversion matching this phi's expected type
          matching = convert_entries.find { |e| e[2] == phi_type }
          if matching
            convert_name, _, _ = matching
            return "%#{convert_name}"
          end
          # No matching conversion — value type may already match phi type,
          # fall through to normal handling
        end
      end
      # Check for predecessor-loaded value (for cross-block SSA fix)
      if pred_load_name = @phi_predecessor_loads[{block, val}]?
        # CRITICAL: Use the SLOT ALLOCATION TYPE, not @value_types[val]
        # @value_types may be updated during emission (e.g., nil constant → ptr),
        # but the slot was allocated with the prepass type. The load will use the slot type.
        slot_llvm_type = @cross_block_slot_types[val]?
        declared_val_type = @value_types[val]?
        # Check type compatibility:
        # - Same type: use the load directly
        # - Both ptr: compatible
        # - Both int (same size): compatible
        # - Union slot, primitive phi: extract payload from union
        # - Otherwise: let the type mismatch handling kick in
        is_compatible = if slot_llvm_type.nil?
                          false  # Unknown slot type, let special handling deal with it
                        elsif slot_llvm_type == phi_type
                          true
                        elsif slot_llvm_type == "ptr" && phi_type == "ptr"
                          true
                        elsif phi_type == "ptr" &&
                              declared_val_type == TypeRef::POINTER &&
                              slot_llvm_type.starts_with?('i') &&
                              !slot_llvm_type.includes?(".union")
                          true
                        elsif slot_llvm_type.starts_with?('i') && phi_type.starts_with?('i') &&
                              !slot_llvm_type.includes?(".union") && !phi_type.includes?(".union")
                          # Same integer width?
                          slot_llvm_type == phi_type
                        else
                          false
                        end
        if is_compatible
          if phi_type == "ptr" &&
             declared_val_type == TypeRef::POINTER &&
             slot_llvm_type &&
             slot_llvm_type.starts_with?('i') &&
             !slot_llvm_type.includes?(".union")
            return "%#{pred_load_name}.ptr"
          end
          return "%#{pred_load_name}"
        end

        # Union slot with primitive phi type: the value was stored as union but the phi
        # expects a primitive (e.g., i32). Schedule extraction from the loaded union.
        if slot_llvm_type && slot_llvm_type.includes?(".union") && !phi_type.includes?(".union")
          # Ptr phis should reuse prepass union->ptr extracts emitted in predecessor blocks.
          # Falling back to late phi_extract names here can produce undefined SSA values
          # when predecessor blocks were already emitted.
          if phi_type == "ptr"
            if extract_info = @phi_union_to_ptr_extracts[{block, val}]?
              return "%#{extract_info[0]}"
            end
            # No prepass extract exists for this predecessor/value pair.
            # Creating it here is too late (predecessor may already be emitted), which
            # leads to undefined `%rN.u2p.B` values in phi incomings.
            return "null"
          end
          extract_name = "r#{val}.phi_extract.#{block}"
          @phi_union_payload_extracts[{block, val}] = {extract_name, pred_load_name, phi_type}
          return "%#{extract_name}"
        end
      end
      nil  # Let caller handle normally (type mismatch handling)
    end

    private def emit_phi(inst : Phi, name : String)
      # Use MIR type for phi - don't upgrade to union as downstream code expects original type
      phi_type = @type_mapper.llvm_type(inst.type)
      prepass_type_ref = @value_types[inst.id]?

      # Helper lambda for safe block name lookup
      block_name = ->(block_id : BlockId) {
        @block_names[block_id]? || "bb#{block_id}"
      }

      incoming_pairs = inst.incoming
      missing_preds = [] of BlockId
      if current_block_id = @current_block_id
        if block = @current_func_blocks[current_block_id]?
          preds = block.predecessors
          if preds.any?
            # Double-validate predecessors: check both computed preds AND
            # that the block's terminator actually targets us. This catches
            # stale predecessors from HIR block splits.
            validated_preds = preds.select do |pred_id|
              if pred_block = @current_func_blocks[pred_id]?
                pred_block.terminator.successors.includes?(current_block_id)
              else
                true  # External/missing block — keep as predecessor
              end
            end
            incoming_map = {} of BlockId => ValueId
            inst.incoming.each { |(block_id, val)| incoming_map[block_id] = val }
            incoming_pairs = [] of Tuple(BlockId, ValueId)
            validated_preds.each do |pred|
              if val = incoming_map[pred]?
                incoming_pairs << {pred, val}
              else
                missing_preds << pred
              end
            end
          else
            # No predecessors computed — validate incoming blocks by checking
            # if each block's terminator actually targets the current block.
            filtered = [] of Tuple(BlockId, ValueId)
            inst.incoming.each do |(block_id, val)|
              if pred_block = @current_func_blocks[block_id]?
                succs = pred_block.terminator.successors
                if succs.includes?(current_block_id)
                  filtered << {block_id, val}
                end
              else
                filtered << {block_id, val}
              end
            end
            incoming_pairs = filtered
          end
        end
      end

      # Filter pass-through incomings where the value is defined in a different block
      # and we don't have a predecessor load. These produce invalid IR (undefined %rN).
      if incoming_pairs.size > 0
        filtered = [] of Tuple(BlockId, ValueId)
        incoming_pairs.each do |(block_id, val_id)|
          def_inst = find_def_inst(val_id)
          val_type = @value_types[val_id]?
          if val_type == TypeRef::VOID && phi_type != "void"
            unless missing_preds.includes?(block_id)
              missing_preds << block_id
            end
            next
          end
          val_emitted = @value_names.has_key?(val_id) || @constant_values.has_key?(val_id)
          if def_inst.nil? && !val_emitted
            unless missing_preds.includes?(block_id)
              missing_preds << block_id
            end
            next
          end
          def_block = @value_def_block[val_id]?
          if def_block && def_block != block_id && !@phi_predecessor_loads.has_key?({block_id, val_id})
            # Entry block values dominate all other blocks — safe to reference directly
            # (they are excluded from cross_block_values/slots for efficiency,
            # but are always available in LLVM IR)
            unless def_block == @entry_user_block_id
              unless missing_preds.includes?(block_id)
                missing_preds << block_id
              end
              next
            end
          end
          filtered << {block_id, val_id}
        end
        incoming_pairs = filtered
      end
      default_phi_value = ->(llvm_type : String) do
        if llvm_type == "ptr"
          "null"
        elsif llvm_type == "float" || llvm_type == "double"
          "0.0"
        elsif llvm_type.includes?(".union")
          "zeroinitializer"
        else
          "0"
        end
      end

      append_missing = ->(entries : Array(String), llvm_type : String) do
        missing_preds.each do |pred|
          entries << "[#{default_phi_value.call(llvm_type)}, %#{block_name.call(pred)}]"
        end
      end

      # Normalize union phi incoming values:
      # If an incoming value is a UnionUnwrap payload and the phi expects the original union,
      # use the union value instead of the payload ptr to avoid ptr-typed phi mismatches.
      if phi_type.includes?(".union")
        incoming_pairs = incoming_pairs.map do |(block, val)|
          if def_inst = find_def_inst(val)
            if def_inst.is_a?(UnionUnwrap)
              union_val = def_inst.union_value
              union_type_ref = @value_types[union_val]?
              union_llvm_type = union_type_ref ? @type_mapper.llvm_type(union_type_ref) : nil
              if union_llvm_type == phi_type
                {block, union_val}
              else
                {block, val}
              end
            else
              {block, val}
            end
          else
            {block, val}
          end
        end
      end

      # Enable phi mode to prevent value_ref from emitting loads
      # (phi nodes must be grouped at top of basic block)
      @in_phi_mode = true

      # Check if prepass already marked this phi as ptr (union→ptr conversion)
      # Note: prepass_type_ref already retrieved above
      if prepass_type_ref == TypeRef::POINTER && phi_type.includes?(".union")
        # Prepass determined this union phi should be ptr - emit as ptr phi
        incoming = incoming_pairs.map do |(block, val)|
          # Check for predecessor load first (cross-block SSA fix)
          if pred_ref = phi_incoming_ref(block, val, "ptr")
            next "[#{pred_ref}, %#{block_name.call(block)}]"
          end
            val_type = @value_types[val]?
            val_type_str = val_type ? @type_mapper.llvm_type(val_type) : nil
            if val_type_str && val_type_str.includes?(".union")
              if extract_info = @phi_union_to_ptr_extracts[{block, val}]?
                "[%#{extract_info[0]}, %#{block_name.call(block)}]"
              else
                "[null, %#{block_name.call(block)}]"
              end
            elsif val_type_str && val_type_str.starts_with?('i') && !val_type_str.includes?(".union")
              # Int value can't be used in ptr phi - use null
              "[null, %#{block_name.call(block)}]"
          else
            # Check if value was emitted before using value_ref
            val_emitted = @value_names.has_key?(val)
            val_is_const = @constant_values.has_key?(val)
            if !val_emitted && !val_is_const
              # Undefined value in prepass ptr phi - use null
              "[null, %#{block_name.call(block)}]"
            else
              ref = value_ref(val)
              "[#{ref}, %#{block_name.call(block)}]"
            end
          end
        end
        append_missing.call(incoming, "ptr")
        emit "#{name} = phi ptr #{incoming.join(", ")}"
        record_emitted_type(name, "ptr")
        # @value_types already set by prepass
        @in_phi_mode = false
        return
      end

      # Void type phi nodes are invalid in LLVM - emit as ptr with null values
      if phi_type == "void"
        # Emit as ptr phi with null values so the register exists for downstream use
        incoming = incoming_pairs.map do |(block, val)|
          "[null, %#{block_name.call(block)}]"
        end
        append_missing.call(incoming, "ptr")
        emit "#{name} = phi ptr #{incoming.join(", ")}"
        record_emitted_type(name, "ptr")
        @value_types[inst.id] = TypeRef::POINTER
        @in_phi_mode = false
        return
      end

      is_int_type = phi_type.starts_with?('i') && phi_type != "i1" && !phi_type.includes?(".union")
      is_bool_type = phi_type == "i1"
      is_ptr_type = phi_type == "ptr"
      is_union_type = phi_type.includes?(".union")
      is_float_type = phi_type == "float" || phi_type == "double"

      # Check if i1 phi has type-mismatched incoming values (union, ptr, larger int, float)
      if is_bool_type
        has_mismatched_incoming = incoming_pairs.any? do |(block, val)|
          val_type = @value_types[val]?
          val_type_str = val_type ? @type_mapper.llvm_type(val_type) : nil
          val_type_str && val_type_str != "i1" && val_type_str != "void"
        end
        if has_mismatched_incoming
          incoming = incoming_pairs.map do |(block, val)|
            # Check for predecessor load first (cross-block SSA fix) - but only if types match
            pred_ref = phi_incoming_ref(block, val, "i1")
            if pred_ref
              next "[#{pred_ref}, %#{block_name.call(block)}]"
            end
            val_type = @value_types[val]?
            val_type_str = val_type ? @type_mapper.llvm_type(val_type) : nil
            if val_type_str && val_type_str.includes?(".union")
              # Union value - use 0 (false) as default
              "[0, %#{block_name.call(block)}]"
            elsif val_type_str == "ptr" || val_type_str == "void"
              # Ptr/void value flowing into i1 phi - use 0 (type mismatch)
              "[0, %#{block_name.call(block)}]"
            elsif val_type_str && val_type_str.starts_with?('i') && val_type_str != "i1"
              # Larger int (i8, i16, i32, i64) flowing into i1 phi - use 0 (type mismatch)
              # Can't truncate in phi node, so use 0 as safe default
              "[0, %#{block_name.call(block)}]"
            elsif val_type_str == "float" || val_type_str == "double"
              # Float/double value flowing into i1 phi - use 0 (type mismatch)
              "[0, %#{block_name.call(block)}]"
            else
              # Check if value was emitted before using value_ref
              val_emitted = @value_names.has_key?(val)
              val_is_const = @constant_values.has_key?(val)
              if !val_emitted && !val_is_const
                # Undefined value in bool phi - use 0
                "[0, %#{block_name.call(block)}]"
              else
                ref = value_ref(val)
                # Guard against null and float literals for non-ptr phi
                ref = "0" if ref == "null" || ref.includes?('.')
                "[#{ref}, %#{block_name.call(block)}]"
              end
            end
          end
          append_missing.call(incoming, "i1")
          emit "#{name} = phi i1 #{incoming.join(", ")}"
          record_emitted_type(name, "i1")
          @value_types[inst.id] = TypeRef::BOOL
          @in_phi_mode = false
          return
        end
      end

      # Check if int phi (i8, i16, i32, i64) has type-mismatched incoming values
      # This handles: 1) int size mismatches 2) union values flowing into int phi
      # 3) values with no type (forward references to calls)
      if is_int_type
        phi_bits = phi_type[1..-1].to_i? || 32
        has_mismatched_int_incoming = incoming_pairs.any? do |(block, val)|
          val_type = @value_types[val]?
          # No type = unknown, treat as potentially mismatched for non-emitted values
          if !val_type
            val_emitted = @value_names.has_key?(val)
            val_is_const = @constant_values.has_key?(val)
            next true if !val_emitted && !val_is_const  # Unknown forward ref
            next false  # Emitted value without type - let it through
          end
          val_type_str = @type_mapper.llvm_type(val_type)
          # Check for union type mismatch
          next true if val_type_str.includes?(".union")
          # Check for void type
          next true if val_type_str == "void"
          # Check for ptr type in int phi
          next true if val_type_str == "ptr"
          # Check for float/double type in int phi
          next true if val_type_str == "float" || val_type_str == "double"
          next false unless val_type_str.starts_with?('i') && !val_type_str.includes?(".union")
          val_bits = val_type_str[1..-1].to_i? || 32
          val_bits != phi_bits
        end
        if has_mismatched_int_incoming
          incoming = incoming_pairs.map do |(block, val)|
            # Check for predecessor load first (cross-block SSA fix)
            if pred_ref = phi_incoming_ref(block, val, phi_type)
              next "[#{pred_ref}, %#{block_name.call(block)}]"
            end
            val_type = @value_types[val]?
            val_type_str = val_type ? @type_mapper.llvm_type(val_type) : nil
            if val_type_str && val_type_str.includes?(".union")
              # Union value flowing into int phi - use 0 as default
              # This happens with e.g. String#index returning Int32|Nil
              "[0, %#{block_name.call(block)}]"
            elsif val_type_str && val_type_str.starts_with?('i') && !val_type_str.includes?(".union")
              val_bits = val_type_str[1..-1].to_i? || 32
              if val_bits != phi_bits
                # Type size mismatch - check if prepass recorded a conversion for this value
                # First check for predecessor conversion (for params and fixed-type values)
                if pred_conv = @phi_predecessor_conversions[{block, val}]?
                  conv_name, _, _ = pred_conv
                  "[%#{conv_name}, %#{block_name.call(block)}]"
                elsif @phi_zext_conversions.has_key?(val)
                  # Construct the expected zext name (will be emitted by emit_extern_call)
                  zext_name = "%r#{val}.zext"
                  "[#{zext_name}, %#{block_name.call(block)}]"
                else
                  # No conversion planned - use 0 as fallback
                  "[0, %#{block_name.call(block)}]"
                end
              else
                ref = value_ref(val)
                ref = "0" if ref == "null"
                "[#{ref}, %#{block_name.call(block)}]"
              end
            elsif val_type_str == "float" || val_type_str == "double"
              # Float/double value flowing into int phi - use 0 as fallback
              "[0, %#{block_name.call(block)}]"
            elsif val_type_str.nil? || val_type_str == "void" || val_type_str == "ptr"
              # No type, void, or ptr value flowing into int phi - use 0
              "[0, %#{block_name.call(block)}]"
            else
              # Check if value was emitted
              val_emitted = @value_names.has_key?(val)
              val_is_const = @constant_values.has_key?(val)
              if !val_emitted && !val_is_const
                "[0, %#{block_name.call(block)}]"
              else
                ref = value_ref(val)
                ref = "0" if ref == "null"
                "[#{ref}, %#{block_name.call(block)}]"
              end
            end
          end
          append_missing.call(incoming, phi_type)
          emit "#{name} = phi #{phi_type} #{incoming.join(", ")}"
          record_emitted_type(name, phi_type)
          @value_types[inst.id] = inst.type
          @in_phi_mode = false
          return
        end
      end

      # Check if ptr phi has incompatible incoming (union, int, float) - use null for them
      # Can't extract/convert in current block for phi, so use null (lossy but compiles)
      if is_ptr_type
        has_incompatible_incoming = incoming_pairs.any? do |(block, val)|
          val_type = @value_types[val]?
          next false unless val_type
          val_type_str = @type_mapper.llvm_type(val_type)
          # Union, int (including i1 bool), float/double, or void are incompatible with ptr
          # void values don't emit LLVM %rN - they're from void function calls
          val_type_str.includes?(".union") ||
            val_type_str.starts_with?('i') ||
            val_type_str == "float" || val_type_str == "double" ||
            val_type_str == "void"
        end
        if has_incompatible_incoming
          incoming = incoming_pairs.map do |(block, val)|
            # Check for predecessor load first (cross-block SSA fix)
            if pred_ref = phi_incoming_ref(block, val, "ptr")
              next "[#{pred_ref}, %#{block_name.call(block)}]"
            end
            val_type = @value_types[val]?
            val_type_str = val_type ? @type_mapper.llvm_type(val_type) : nil
            if val_type_str && val_type_str.includes?(".union")
              if extract_info = @phi_union_to_ptr_extracts[{block, val}]?
                "[%#{extract_info[0]}, %#{block_name.call(block)}]"
              else
                "[null, %#{block_name.call(block)}]"
              end
            elsif val_type_str && val_type_str.starts_with?('i') && !val_type_str.includes?(".union")
              # Int value in ptr phi - use null
              "[null, %#{block_name.call(block)}]"
            elsif val_type_str == "float" || val_type_str == "double"
              # Float/double value in ptr phi - use null
              "[null, %#{block_name.call(block)}]"
            elsif val_type_str == "void"
              # Void value (from void call) can't be used in ptr phi - use null
              "[null, %#{block_name.call(block)}]"
            else
              # Check if value was emitted before using value_ref
              val_emitted = @value_names.has_key?(val)
              val_is_const = @constant_values.has_key?(val)
              if !val_emitted && !val_is_const
                # Undefined value in ptr phi - use null
                "[null, %#{block_name.call(block)}]"
              else
                ref = value_ref(val)
                "[#{ref}, %#{block_name.call(block)}]"
              end
            end
          end
          append_missing.call(incoming, "ptr")
          emit "#{name} = phi ptr #{incoming.join(", ")}"
          record_emitted_type(name, "ptr")
          @value_types[inst.id] = TypeRef::POINTER
          @in_phi_mode = false
          return
        end
      end

      # Check if any incoming value is ptr OR unknown type when phi expects union
      # Unknown types might be forward-referenced ptrs, so emit as ptr phi to be safe
      # This is a MIR type mismatch - handle by emitting as ptr phi with null for union values
      if is_union_type
        union_descriptor = @module.get_union_descriptor(inst.type)
        has_ptr_or_unknown_incoming = incoming_pairs.any? do |(block, val)|
          # If we have a predecessor union wrap for this incoming, it's handled
          next false if @phi_predecessor_union_wraps.has_key?({block, val})
          val_type = @value_types[val]?
          const_val = @constant_values[val]?
          val_llvm_type = val_type ? @type_mapper.llvm_type(val_type) : nil
          emitted_llvm_type = @emitted_value_types["%r#{val}"]?
          slot_llvm_type = @cross_block_slot_types[val]?
          effective_llvm_type = if slot_llvm_type && slot_llvm_type.includes?(".union")
                                  slot_llvm_type
                                elsif emitted_llvm_type && emitted_llvm_type.includes?(".union")
                                  emitted_llvm_type
                                else
                                  val_llvm_type
                                end

          # If we already know this incoming as a union value (via slot/emitted type),
          # it must not trigger ptr-phi fallback.
          if effective_llvm_type && effective_llvm_type.includes?(".union")
            next false
          end

          if val_type && union_descriptor
            # If the incoming type is a known union variant, don't treat ptr as a mismatch.
            next false if union_descriptor.variants.any? { |variant| variant.type_ref == val_type }
          end
          if effective_llvm_type != "ptr"
            def_inst = find_def_inst(val)
            if def_inst && def_inst.is_a?(UnionUnwrap) && @type_mapper.llvm_type(def_inst.type) == "ptr"
              next true
            end
          end
          # Has ptr type, OR has no known type (forward reference that might be ptr)
          # But exclude constants which have known values
          # Also check if the incoming value is from a block that might have ptr phis
          (effective_llvm_type == "ptr") ||
          (val_type.nil? && const_val.nil?) ||
          # If const_val is "null", it's being used as ptr
          (const_val == "null")
        end
        if has_ptr_or_unknown_incoming
          # Emit as ptr phi - for union/int values use null (lossy but compiles)
          # This handles MIR bugs where array literal (ptr) and union flow to same phi
          incoming = incoming_pairs.map do |(block, val)|
            # Check for predecessor load first (cross-block SSA fix)
            if pred_ref = phi_incoming_ref(block, val, "ptr")
              next "[#{pred_ref}, %#{block_name.call(block)}]"
            end
            val_type = @value_types[val]?
            val_type_str = val_type ? @type_mapper.llvm_type(val_type) : nil
            if val_type_str && val_type_str.includes?(".union")
              def_inst = find_def_inst(val)
              if def_inst && def_inst.is_a?(UnionUnwrap) && @type_mapper.llvm_type(def_inst.type) == "ptr"
                # UnionUnwrap produced a ptr value; keep it.
                "[#{value_ref(val)}, %#{block_name.call(block)}]"
              else
                # Use prepass extract if present; creating it here is too late and can
                # produce undefined `%rN.u2p.B` when predecessor block is already emitted.
                if extract_info = @phi_union_to_ptr_extracts[{block, val}]?
                  "[%#{extract_info[0]}, %#{block_name.call(block)}]"
                else
                  "[null, %#{block_name.call(block)}]"
                end
              end
            elsif val_type_str && val_type_str.starts_with?('i') && !val_type_str.includes?(".union")
              # Int value can't be used in ptr phi - use null
              "[null, %#{block_name.call(block)}]"
            else
              # Check if value was emitted before using value_ref
              val_emitted = @value_names.has_key?(val)
              val_is_const = @constant_values.has_key?(val)
              if !val_emitted && !val_is_const
                # Undefined value in ptr phi - use null
                "[null, %#{block_name.call(block)}]"
              else
                ref = value_ref(val)
                # For unknown types, assume they're ptr-compatible
                "[#{ref}, %#{block_name.call(block)}]"
              end
            end
          end
          append_missing.call(incoming, "ptr")
          emit "#{name} = phi ptr #{incoming.join(", ")}"
          record_emitted_type(name, "ptr")
          @value_types[inst.id] = TypeRef::POINTER
          @in_phi_mode = false
          return
        end

        # Check if any incoming value is a non-union type (like plain i32) when phi expects union
        # OR if any incoming value is a DIFFERENT union type
        # This happens when MIR doesn't properly wrap values in unions before phi
        has_non_union_incoming = incoming_pairs.any? do |(block, val)|
          val_type = @value_types[val]?
          val_llvm_type = val_type ? @type_mapper.llvm_type(val_type) : nil
          # Value has a type but it's not a union and not ptr (ptr handled above)
          val_llvm_type && !val_llvm_type.includes?(".union") && val_llvm_type != "ptr"
        end
        # Also check for different union types (e.g., UInt8___Nil.union vs String___Nil.union)
        has_different_union_incoming = incoming_pairs.any? do |(block, val)|
          val_type = @value_types[val]?
          val_llvm_type = val_type ? @type_mapper.llvm_type(val_type) : nil
          # Check MIR-mapped type
          if val_llvm_type && val_llvm_type.includes?(".union") && val_llvm_type != phi_type
            next true
          end
          # Also check actual emitted type — function call results may have a different
          # LLVM union type name than what the MIR type mapper produces (e.g., ClassInfo|Void
          # vs Nil|ClassInfo for nilable returns from Hash#[]?)
          emitted = @emitted_value_types["%r#{val}"]?
          if emitted && emitted.includes?(".union") && emitted != phi_type
            next true
          end
          # Check cross-block slot type — when value crosses blocks, its slot stores the
          # actual emitted type which may differ from the MIR-mapped type
          slot_type = @cross_block_slot_types[val]?
          if slot_type && slot_type.includes?(".union") && slot_type != phi_type
            next true
          end
          false
        end
        if has_non_union_incoming || has_different_union_incoming
          # Emit union phi with proper conversion for mismatched incoming values
          # For different union types: defer alloca+store+load reinterpret to predecessor block
          # For non-union types: use zeroinitializer (nil case)
          incoming = incoming_pairs.map do |(block, val)|
            # Check for predecessor load first (cross-block SSA fix)
            if pred_ref = phi_incoming_ref(block, val, phi_type)
              next "[#{pred_ref}, %#{block_name.call(block)}]"
            end
            val_type = @value_types[val]?
            val_type_str = val_type ? @type_mapper.llvm_type(val_type) : nil
            emitted_type = @emitted_value_types["%r#{val}"]?
            slot_type = @cross_block_slot_types[val]?
            effective_val_type = if emitted_type && emitted_type.includes?(".union")
                                   emitted_type
                                 elsif slot_type && slot_type.includes?(".union")
                                   slot_type
                                 else
                                   val_type_str
                                 end

            if effective_val_type && effective_val_type == phi_type
              # Same union type - use value directly
              ref = value_ref(val)
              # If value_ref returned "null", convert to zeroinitializer for union
              ref = "zeroinitializer" if ref == "null"
              "[#{ref}, %#{block_name.call(block)}]"
            elsif effective_val_type && effective_val_type.includes?(".union") && effective_val_type != phi_type
              # Different union type - defer reinterpret to predecessor block
              existing = @phi_union_to_union_converts[{block, val}]?
              if existing && (match = existing.find { |e| e[2] == phi_type })
                convert_name = match[0]
              else
                suffix = existing ? ".#{existing.size}" : ""
                convert_name = "r#{val}.u2u.#{block}#{suffix}"
                arr = @phi_union_to_union_converts[{block, val}] ||= [] of {String, String, String}
                arr << {convert_name, effective_val_type, phi_type}
              end
              "[%#{convert_name}, %#{block_name.call(block)}]"
            else
              # Non-union type mismatch - use zeroinitializer (nil case)
              "[zeroinitializer, %#{block_name.call(block)}]"
            end
          end
          append_missing.call(incoming, phi_type)
          emit "#{name} = phi #{phi_type} #{incoming.join(", ")}"
          record_emitted_type(name, phi_type)
          @value_types[inst.id] = inst.type
          @in_phi_mode = false
          return
        end
      end

      incoming = incoming_pairs.map do |(block, val)|
        # FIRST: Check if we have a predecessor-loaded value for cross-block SSA fix
        # This handles cases where phi says [%val, %predBlock] but %val is defined
        # in a different block (pass-through situation causing SSA dominance errors)
        if pred_ref = phi_incoming_ref(block, val, phi_type)
          next "[#{pred_ref}, %#{block_name.call(block)}]"
        end

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

        # Check if this is a forward reference (value defined but not yet emitted)
        # Prepass collects all types, but we must ensure the value has a real def.
        # IMPORTANT: void values are NOT valid forward references - they don't emit %rN
        def_inst = find_def_inst(val)
        is_forward_ref = !val_emitted && !val_is_const && def_inst && @value_types.has_key?(val) && !is_void_value

        if (!val_emitted || is_void_value) && !val_is_const && !is_forward_ref
          # Truly undefined value - use safe default based on phi type
          if is_union_type
            "[zeroinitializer, %#{block_name.call(block)}]"
          elsif is_ptr_type
            "[null, %#{block_name.call(block)}]"
          elsif is_int_type || is_bool_type
            "[0, %#{block_name.call(block)}]"
          elsif is_float_type
            "[0.0, %#{block_name.call(block)}]"
          else
            "[null, %#{block_name.call(block)}]"
          end
        elsif is_forward_ref
          # Forward reference from loop back-edge - use %r#{val} which will be defined later
          # BUT: verify type compatibility first. If the forward-referenced value has a
          # different type than the phi, we can't use it directly (LLVM will reject the
          # type mismatch). Fall through to type mismatch handling instead.
          fwd_val_type = @value_types[val]?
          fwd_val_type_str = fwd_val_type ? @type_mapper.llvm_type(fwd_val_type) : nil
          if fwd_val_type_str && fwd_val_type_str != phi_type &&
             !(fwd_val_type_str == "ptr" && phi_type == "ptr")
            # Type mismatch — use safe default
            if is_union_type
              "[zeroinitializer, %#{block_name.call(block)}]"
            elsif is_ptr_type
              "[null, %#{block_name.call(block)}]"
            elsif is_int_type || is_bool_type
              "[0, %#{block_name.call(block)}]"
            elsif is_float_type
              "[0.0, %#{block_name.call(block)}]"
            else
              "[null, %#{block_name.call(block)}]"
            end
          else
            "[%r#{val}, %#{block_name.call(block)}]"
          end
        elsif const_val == "null" && is_union_type
          # null constant in union phi - use zeroinitializer
          "[zeroinitializer, %#{block_name.call(block)}]"
        elsif const_val == "null" && (is_int_type || is_bool_type)
          "[0, %#{block_name.call(block)}]"
        elsif const_val == "null" && is_float_type
          # null flowing into float phi - use 0.0
          "[0.0, %#{block_name.call(block)}]"
        elsif const_val == "0" && is_ptr_type
          "[null, %#{block_name.call(block)}]"
        else
          # Check for type mismatch
          val_type = @value_types[val]?
          val_type_str = val_type ? @type_mapper.llvm_type(val_type) : nil

          if (is_int_type || is_bool_type) && val_type_str && val_type_str.includes?(".union")
            # Union flowing into int/bool phi - use 0 (nil case)
            "[0, %#{block_name.call(block)}]"
          elsif (is_int_type || is_bool_type) && (val_type_str == "ptr" || val_type_str == "void")
            # Ptr/void flowing into int/bool phi - use 0 (type mismatch from MIR)
            "[0, %#{block_name.call(block)}]"
          elsif is_int_type && (val_type_str == "double" || val_type_str == "float")
            # Float/double flowing into int phi - use 0 as safe default
            # We can't bitcast here because it would be in wrong block
            "[0, %#{block_name.call(block)}]"
          elsif is_int_type && val_type_str && val_type_str.starts_with?('i') && val_type_str != phi_type
            # Integer width mismatch (e.g., i64 into i32 phi) - use 0 as safe default
            # We can't emit trunc/ext in phi, would need to be in source block
            "[0, %#{block_name.call(block)}]"
          elsif (is_int_type || is_bool_type) && val_type_str.nil?
            # Unknown type flowing into int/bool phi
            ref = value_ref(val)
            if ref == "null"
              # Null value can't flow into int/bool phi
              "[0, %#{block_name.call(block)}]"
            else
              # Allow %r* forward references - these are valid in phi nodes for loop back-edges
              "[#{ref}, %#{block_name.call(block)}]"
            end
          elsif is_ptr_type && val_type_str && val_type_str.starts_with?('i') && !val_type_str.includes?(".union")
            # Int flowing into ptr phi - use null (type mismatch from MIR)
            "[null, %#{block_name.call(block)}]"
          elsif is_float_type && (val_type_str == "ptr" || val_type_str == "void")
            # Ptr/void flowing into float phi - use 0.0 (type mismatch from MIR)
            "[0.0, %#{block_name.call(block)}]"
          elsif is_float_type && val_type_str && (val_type_str == "float" || val_type_str == "double") && val_type_str != phi_type
            # float↔double mismatch in phi - use 0.0 as safe default
            # Can't emit fpext/fptrunc in phi, would need to be in source block
            "[0.0, %#{block_name.call(block)}]"
          elsif is_float_type && val_type_str && val_type_str.starts_with?('i')
            # Int flowing into float phi - use 0.0 (type mismatch from MIR)
            "[0.0, %#{block_name.call(block)}]"
          else
            ref = value_ref(val)
            # Check if value_ref returned "null"
            if ref == "null" && (is_int_type || is_bool_type)
              ref = "0"
            elsif ref == "null" && is_float_type
              # null flowing into float phi - use 0.0
              ref = "0.0"
            elsif ref == "null" && is_union_type
              # null flowing into union phi - use zeroinitializer
              ref = "zeroinitializer"
            elsif (ref == "0" || ref.starts_with?("%r")) && is_ptr_type && val_type_str && val_type_str.starts_with?('i')
              # Int value flowing into ptr phi
              ref = "null"
            end
            "[#{ref}, %#{block_name.call(block)}]"
          end
        end
      end
      append_missing.call(incoming, phi_type)
      emit "#{name} = phi #{phi_type} #{incoming.join(", ")}"
      record_emitted_type(name, phi_type)
      @value_types[inst.id] = inst.type
      @in_phi_mode = false
    end

    private def emit_select(inst : Select, name : String)
      type = @type_mapper.llvm_type(inst.type)
      cond = value_ref(inst.condition)
      then_val = value_ref(inst.then_value)
      else_val = value_ref(inst.else_value)
      # For non-pointer types, convert "null" to appropriate zero value
      if type != "ptr" && !type.includes?(".union")
        if type == "double" || type == "float"
          then_val = "0.0" if then_val == "null"
          else_val = "0.0" if else_val == "null"
        else
          then_val = "0" if then_val == "null"
          else_val = "0" if else_val == "null"
        end
      end
      emit "#{name} = select i1 #{cond}, #{type} #{then_val}, #{type} #{else_val}"
      @value_types[inst.id] = inst.type
    end

    private def emit_call(inst : Call, name : String, func : Function)
      # Look up callee function for name and param types
      callee_func = @module.functions.find { |f| f.id == inst.callee }
      callee_name = if callee_func
                      mangle_function_name(callee_func.name)
                    else
                      # Undefined internal function - track for declaration
                      undefined_name = "func#{inst.callee}"
                      ret_type = @type_mapper.llvm_type(inst.type)
                      ret_type = "ptr" if ret_type == "void"  # Use ptr for void to be safe
                      @undefined_externs[undefined_name] = ret_type unless @undefined_externs.has_key?(undefined_name)
                      undefined_name
                    end

      raw_callee_name = callee_func.try(&.name)

      # Keep direct self-recursive calls intact. Overload retargeting must be resolved
      # in HIR/MIR; backend-level substitution can silently redirect to wrong overloads.

      # Intercept Array#sum() with no args (only self) to avoid infinite recursion
      # from overload name collision (sum() and sum(initial) get same mangled name)
      if callee_name.includes?("$Hsum") && inst.args.size == 1
        # Extract element type from Array$L<Type>$R
        if callee_name =~ /Array\$L(Int32|UInt32)\$R\$Hsum$/
          elem_type = $1
          self_arg = value_ref(inst.args[0])
          case elem_type
          when "Int32", "UInt32"
            emit "#{name} = call i32 @__crystal_v2_array_sum_int32(ptr #{self_arg})"
            @value_types[inst.id] = TypeRef::INT32
            return
          end
        end
      end

      # Intercept Int#abs / Number#abs when self is a primitive integer.
      # The generic abs function takes ptr self, but for primitive ints we inline:
      #   abs(x) = x < 0 ? -x : x
      if callee_name == "Int$Habs" || callee_name == "Number$Habs"
        if inst.args.size >= 1
          self_id = inst.args[0]
          self_val = value_ref(self_id)
          self_type = lookup_value_llvm_type(self_id)
          if self_type.starts_with?('i') && !self_type.includes?('.')
            zero = "0"
            cmp_name = "%abs_cmp.#{inst.id}"
            neg_name = "%abs_neg.#{inst.id}"
            emit "#{cmp_name} = icmp slt #{self_type} #{self_val}, #{zero}"
            emit "#{neg_name} = sub #{self_type} #{zero}, #{self_val}"
            emit "#{name} = select i1 #{cmp_name}, #{self_type} #{neg_name}, #{self_type} #{self_val}"
            @value_types[inst.id] = @value_types[self_id]? || TypeRef::INT32
            return
          end
        end
      end

      # Intercept Int#next_power_of_two — abstract Int method crashes at runtime
      # because is_a?(Int::Signed) tries to load type_id from raw integer value.
      # Redirect to our bit-trick intrinsic.
      if callee_name == "Int$Hnext_power_of_two" || callee_name == "Int32$Hnext_power_of_two"
        if inst.args.size >= 1
          self_id = inst.args[0]
          self_val = value_ref(self_id)
          self_type = lookup_value_llvm_type(self_id)
          if self_type == "ptr"
            # Raw int encoded as ptr — ptrtoint first
            int_name = "%npt_int.#{inst.id}"
            emit "#{int_name} = ptrtoint ptr #{self_val} to i32"
            emit "#{name} = call i32 @__crystal_v2_next_power_of_two_i32(i32 #{int_name})"
          else
            emit "#{name} = call i32 @__crystal_v2_next_power_of_two_i32(i32 #{self_val})"
          end
          @value_types[inst.id] = TypeRef::INT32
          return
        end
      end

      # Intercept Int#leading_zeros_count — same issue as next_power_of_two
      if callee_name == "Int$Hleading_zeros_count" || callee_name == "Int32$Hleading_zeros_count"
        if inst.args.size >= 1
          self_id = inst.args[0]
          self_val = value_ref(self_id)
          self_type = lookup_value_llvm_type(self_id)
          if self_type == "ptr"
            int_name = "%lzc_int.#{inst.id}"
            emit "#{int_name} = ptrtoint ptr #{self_val} to i32"
            emit "#{name} = call i32 @__crystal_v2_leading_zeros_count_i32(i32 #{int_name})"
          else
            emit "#{name} = call i32 @__crystal_v2_leading_zeros_count_i32(i32 #{self_val})"
          end
          @value_types[inst.id] = TypeRef::INT32
          return
        end
      end

      # Intercept String#index(String, offset) — stdlib compilation produces wrong method
      # calls (IO::FileDescriptor#tell) and contaminated union return types.
      # Redirect to our strstr-based runtime helper.
      if callee_name == "String$Hindex$$String" || callee_name == "String$Hindex$$String_Int32"
        if inst.args.size >= 2
          self_val = value_ref(inst.args[0])
          search_val = value_ref(inst.args[1])
          offset_val = if inst.args.size >= 3
                          value_ref(inst.args[2])
                        else
                          "0"
                        end
          # Call helper: returns i32 (-1 = not found, else byte index)
          idx_name = "%str_idx.#{inst.id}"
          emit "#{idx_name} = call i32 @__crystal_v2_string_index_string(ptr #{self_val}, ptr #{search_val}, i32 #{offset_val})"
          # Build Nil|Int32 union: {i32 type_id, [8 x i8] payload}
          # type_id=0 for Nil, type_id=1 for Int32 (local convention)
          cmp_name = "%str_idx_nil.#{inst.id}"
          emit "#{cmp_name} = icmp eq i32 #{idx_name}, -1"
          union_ptr = "%str_idx_union.#{inst.id}"
          emit "#{union_ptr} = alloca {i32, [8 x i8]}, align 8"
          tid_ptr = "%str_idx_tid.#{inst.id}"
          emit "#{tid_ptr} = getelementptr {i32, [8 x i8]}, ptr #{union_ptr}, i32 0, i32 0"
          payload_ptr = "%str_idx_pay.#{inst.id}"
          emit "#{payload_ptr} = getelementptr {i32, [8 x i8]}, ptr #{union_ptr}, i32 0, i32 1"
          # Store nil (0) or non-nil (1) type_id
          tid_val = "%str_idx_tidv.#{inst.id}"
          emit "#{tid_val} = select i1 #{cmp_name}, i32 0, i32 1"
          emit "store i32 #{tid_val}, ptr #{tid_ptr}"
          # Store payload (the index value) — only meaningful when non-nil
          emit "store i32 #{idx_name}, ptr #{payload_ptr}, align 4"
          # Load as the expected return type
          ret_type = @type_mapper.llvm_type(inst.type)
          if ret_type == "ptr"
            emit "#{name} = bitcast ptr #{union_ptr} to ptr"
          else
            emit "#{name} = load #{ret_type}, ptr #{union_ptr}"
          end
          record_emitted_type(name, ret_type)
          @value_types[inst.id] = (ret_type == "ptr" ? TypeRef::POINTER : inst.type)
          return
        end
      end

      if raw_callee_name && pointer_constructor_name?(raw_callee_name) && inst.args.size == 1
        arg_id = inst.args[0]
        arg = value_ref(arg_id)
        arg_type = lookup_value_llvm_type(arg_id)
        if arg_type == "ptr"
          emit "#{name} = bitcast ptr #{arg} to ptr"
        elsif arg_type.starts_with?('i')
          emit "#{name} = inttoptr #{arg_type} #{arg} to ptr"
          @inttoptr_value_ids.add(inst.id)
        else
          emit "#{name} = bitcast ptr #{arg} to ptr"
        end
        record_emitted_type(name, "ptr")
        @value_types[inst.id] = TypeRef::POINTER
        return
      end

      # Use callee's return type instead of inst.type for correct ABI
      # But if callee returns void and inst.type is not void, use inst.type
      # (this handles cases where method resolution found wrong overload)
      inst_return_type = @type_mapper.llvm_type(inst.type)
      # Check if we already emitted this function with a DIFFERENT return type
      # (e.g., []? override returns union but MIR callee says i32)
      emitted_ret = @emitted_function_return_types[callee_name]?
      needs_ptr_to_union_wrap = false
      ptr_to_union_target_type = ""
      needs_union_to_union_wrap = false
      union_to_union_target_type = ""
      return_type = if emitted_ret && emitted_ret != "void"
                      # Use the ACTUAL emitted function's return type for ABI correctness
                      emitted_ret
                    elsif callee_func
                      callee_ret = @type_mapper.llvm_type(callee_func.return_type)
                      # If inst.type is a union (nilable) but callee returns ptr (not union),
                      # use callee's actual return type for ABI correctness, then wrap the
                      # ptr result into a union struct after the call. Using the union type
                      # directly would cause an LLVM IR type mismatch (function returns ptr
                      # but call declares union struct return).
                      if inst_return_type.includes?(".union") && callee_ret == "ptr" && inst_return_type.includes?("Nil")
                        # Callee returns ptr (nullable reference) but caller expects Nil|X union.
                        # Only apply for nilable unions (Nil | X) where null ptr = nil.
                        # Use ptr for the call, then wrap into union after.
                        needs_ptr_to_union_wrap = true
                        ptr_to_union_target_type = inst_return_type
                        callee_ret
                      elsif inst_return_type.includes?(".union") && !callee_ret.includes?(".union")
                        # Keep callee ABI return type. Emitting a call with union return against
                        # a non-union function declaration produces invalid LLVM IR.
                        callee_ret
                      elsif inst_return_type.includes?(".union") && callee_ret.includes?(".union") &&
                            inst_return_type != callee_ret
                        # ABI stays callee union type; immediately reinterpret the result
                        # to the expected union type for downstream SSA/type consistency.
                        needs_union_to_union_wrap = true
                        union_to_union_target_type = inst_return_type
                        callee_ret
                      # Prefer inst.type when callee returns void but MIR expects a value
                      elsif callee_ret == "void" && inst_return_type != "void"
                        inst_return_type
                      else
                        callee_ret
                      end
                    else
                      if inst_return_type == "void"
                        "ptr"
                      else
                        inst_return_type
                      end
                    end

      # IMPORTANT: Check if prepass determined a different type (e.g., from phi usage)
      # Prepass type takes precedence when phi expects a specific type
      needs_union_abi_fix = false
      union_abi_desired_type = ""
      prepass_type = @value_types[inst.id]?
      if prepass_type
        prepass_llvm_type = @type_mapper.llvm_type(prepass_type)
        if prepass_llvm_type != "void" && prepass_llvm_type != return_type
          if callee_func
            if return_type.includes?(".union") && !prepass_llvm_type.includes?(".union")
              # ABI fix: callee returns union but MIR/prepass expects simpler type.
              # Keep the union return type for the call instruction (ABI correctness),
              # then extract the payload to the desired type after the call.
              needs_union_abi_fix = true
              union_abi_desired_type = prepass_llvm_type
            elsif needs_ptr_to_union_wrap
              # Keep callee ABI type (ptr), wrapping will produce the expected union.
            else
              # For known callees, never override ABI return type from prepass hints.
              # Hints can still be applied via post-call adaptation paths.
            end
          elsif needs_ptr_to_union_wrap
            # Don't let prepass override return_type back to union — we need to use
            # the callee's actual return type (ptr) for ABI correctness, then wrap.
            # Keep return_type as ptr; the wrapping will produce the union type.
          else
            return_type = prepass_llvm_type
          end
        end
      end

      # Final fallback: use prepass-determined type or minimal heuristics for constructors
      # Main type inference is done in prepass via use-based analysis
      if return_type == "void"
        # Check if prepass already determined this call returns a value (use-based inference)
        prepass_type = @value_types[inst.id]?
        if prepass_type && @type_mapper.llvm_type(prepass_type) != "void"
          return_type = @type_mapper.llvm_type(prepass_type)
        else
          # Minimal fallback: constructors and common conversion methods.
          # Use the method core (no receiver/typed suffix) to avoid mangling assumptions.
          method_core = method_core_from_name(raw_callee_name || callee_name)
          ptr_returning_methods = ["new", "allocate", "clone", "dup", "tap"]
          i64_returning_methods = ["to_i64", "to_u64"]
          i32_returning_methods = ["size", "length", "count", "hash", "to_i32", "to_i", "ord", "chr"]
          i16_returning_methods = ["to_i16", "to_u16"] of String
          i8_returning_methods = ["to_i8", "to_u8"] of String
          f32_returning_methods = ["to_f32"] of String
          f64_returning_methods = ["to_f64", "to_f"]

          returns_ptr = ptr_returning_methods.includes?(method_core)
          returns_i64 = i64_returning_methods.includes?(method_core)
          returns_i32 = i32_returning_methods.includes?(method_core)
          returns_i16 = i16_returning_methods.includes?(method_core)
          returns_i8 = i8_returning_methods.includes?(method_core)
          returns_f32 = f32_returning_methods.includes?(method_core)
          returns_f64 = f64_returning_methods.includes?(method_core)

          is_bang_method = method_core.ends_with?('!')
          returns_bool = !is_bang_method && method_core.ends_with?('?')

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
        elsif returns_f32
          return_type = "float"
          @value_types[inst.id] = TypeRef::FLOAT32
        elsif returns_f64
          return_type = "double"
          @value_types[inst.id] = TypeRef::FLOAT64
        end
        end  # end else for prepass_type check
      end

      # Format arguments with proper types, handling type coercion where needed
      # Use callee function params only if param count matches arg count.
      # For class methods (self.foo), MIR call includes self as first arg but the
      # callee definition doesn't. Strip the leading self arg when count is off by 1.
      call_args = inst.args
      if callee_func && callee_func.params.size == inst.args.size - 1 && inst.args.size > 0
        # Off-by-1 arg count is the hallmark of class method self arg mismatch.
        # The MIR call includes self (void/ptr/union) but the callee definition doesn't.
        # Strip the leading self arg regardless of its LLVM type.
        call_args = inst.args[1..]
      end
      # Struct argument expansion: when the callee has more params than the call has args,
      # a struct-typed argument may need to be expanded into its individual fields.
      # Example: call has (Timespec_ptr, Location_ptr) but callee expects (i64, i32, ptr)
      # because the auto-generated .new took the inner overload's flattened signature.
      # Detect by finding a ptr arg where the callee expects consecutive scalar params.
      if callee_func && call_args.size < callee_func.params.size
        param_count = callee_func.params.size
        extra_params = param_count - call_args.size
        expanded = [] of ValueId
        param_idx = 0
        call_args.each do |arg_id|
          break if param_idx >= param_count
          arg_type_ref = @value_types[arg_id]? || TypeRef::POINTER
          arg_llvm = @type_mapper.llvm_type(arg_type_ref)
          param_llvm = @type_mapper.llvm_type(callee_func.params[param_idx].type)

          # Check if arg is a ptr but callee expects a scalar — struct expansion needed.
          # The ptr arg points to inline struct data whose fields match consecutive params.
          if arg_llvm == "ptr" && param_llvm != "ptr" && !param_llvm.includes?(".union") &&
             param_idx + extra_params < param_count
            # Count how many consecutive scalar params could come from this struct.
            # Stop when we hit a ptr or union param (that's the next real arg).
            n_fields = 1
            while param_idx + n_fields < param_count
              next_param_llvm = @type_mapper.llvm_type(callee_func.params[param_idx + n_fields].type)
              break if next_param_llvm == "ptr" || next_param_llvm.includes?(".union")
              n_fields += 1
            end
            # Only expand if this absorbs the extra params gap
            if n_fields > 1 && n_fields <= extra_params + 1
              base_val = value_ref(arg_id)
              byte_offset = 0
              n_fields.times do |fi|
                field_llvm = @type_mapper.llvm_type(callee_func.params[param_idx].type)
                c = @cond_counter
                @cond_counter += 1
                field_ptr_name = "%struct_expand.#{c}.ptr"
                field_val_name = "%struct_expand.#{c}.val"
                emit "#{field_ptr_name} = getelementptr i8, ptr #{base_val}, i32 #{byte_offset}"
                if field_llvm == "void"
                  # Can't load void; use ptr null as placeholder for Nil-typed fields
                  field_llvm = "ptr"
                  emit "#{field_val_name} = inttoptr i64 0 to ptr"
                else
                  emit "#{field_val_name} = load #{field_llvm}, ptr #{field_ptr_name}"
                end
                synthetic_id = ValueId.new(900000_u32 + c.to_u32)
                @value_names[synthetic_id] = field_val_name.lstrip('%')
                @value_types[synthetic_id] = callee_func.params[param_idx].type
                record_emitted_type(field_val_name, field_llvm)
                expanded << synthetic_id
                # Advance byte offset by field size
                byte_offset += case field_llvm
                               when "i8"  then 1
                               when "i16" then 2
                               when "i32" then 4
                               when "i64" then 8
                               when "float" then 4
                               when "double" then 8
                               else 8
                               end
                param_idx += 1
              end
              next  # Don't add the original struct arg
            end
          end
          expanded << arg_id
          param_idx += 1
        end
        call_args = expanded if expanded.size > call_args.size
      end

      # Pad missing args with default values or zero/null to avoid UB from arg count mismatch.
      # This handles cases where callers omit default parameters (e.g., .new with defaults).
      pad_args_extra = nil.as(Array(String)?)
      if callee_func && call_args.size < callee_func.params.size
        extra = [] of String
        (call_args.size...callee_func.params.size).each do |i|
          param = callee_func.params[i]
          param_llvm = @type_mapper.llvm_type(param.type)
          pad_val = if default_val = param.default_value
                      # Use the stored default literal value from the function definition.
                      # Guard against type mismatches (e.g., Bool default with ptr LLVM type).
                      case param_llvm
                      when "i1"
                        default_val == "true" ? "i1 1" : "i1 0"
                      when "ptr"
                        # String default: create string constant; otherwise null
                        if param.type == TypeRef::STRING && default_val != "true" && default_val != "false"
                          str_global = get_or_create_string_global(default_val)
                          "ptr #{str_global}"
                        else
                          "ptr null"
                        end
                      when "float"
                        "float #{default_val.to_f? ? default_val : "0.0"}"
                      when "double"
                        "double #{default_val.to_f? ? default_val : "0.0"}"
                      when .includes?(".union")
                        "#{param_llvm} zeroinitializer"
                      when "void"
                        # Nil/void param with default - convert to ptr null
                        "ptr null"
                      else
                        # For integer types, validate the default is numeric
                        if default_val == "true"
                          "#{param_llvm} 1"
                        elsif default_val == "false"
                          "#{param_llvm} 0"
                        else
                          # Strip Crystal numeric suffixes (e.g., "0_u64" → "0", "8_u32" → "8")
                          clean_val = default_val.gsub(/_(?:i|u)(?:8|16|32|64|128)|_f(?:32|64)\z/, "")
                          "#{param_llvm} #{clean_val}"
                        end
                      end
                    else
                      case param_llvm
                      when "i1"    then "i1 0"
                      when "ptr"
                        # Check if this is an Array type - create empty array instead of null
                        # This handles default parameters like `transitions = [] of T`
                        param_type_info = @module.type_registry.get(param.type)
                        if param_type_info && param_type_info.name.starts_with?("Array(")
                          # Create an inline empty array: {type_id, size=0, cap=0, offset=0, buf=null}
                          c = @cond_counter
                          @cond_counter += 1
                          type_id = param.type.id.to_i32
                          emit "%empty_arr.#{c} = call ptr @__crystal_v2_malloc64(i64 24)"
                          emit "%empty_arr.#{c}.tid = getelementptr i8, ptr %empty_arr.#{c}, i32 0"
                          emit "store i32 #{type_id}, ptr %empty_arr.#{c}.tid"
                          emit "%empty_arr.#{c}.sz = getelementptr i8, ptr %empty_arr.#{c}, i32 4"
                          emit "store i32 0, ptr %empty_arr.#{c}.sz"
                          emit "%empty_arr.#{c}.cap = getelementptr i8, ptr %empty_arr.#{c}, i32 8"
                          emit "store i32 0, ptr %empty_arr.#{c}.cap"
                          emit "%empty_arr.#{c}.off = getelementptr i8, ptr %empty_arr.#{c}, i32 12"
                          emit "store i32 0, ptr %empty_arr.#{c}.off"
                          emit "%empty_arr.#{c}.buf = getelementptr i8, ptr %empty_arr.#{c}, i32 16"
                          emit "store ptr null, ptr %empty_arr.#{c}.buf"
                          "ptr %empty_arr.#{c}"
                        else
                          "ptr null"
                        end
                      when "void"  then "ptr null"
                      when .includes?(".union") then "#{param_llvm} zeroinitializer"
                      when "float" then "float 0.0"
                      when "double" then "double 0.0"
                      else "#{param_llvm} 0"
                      end
                    end
          extra << pad_val
        end
        pad_args_extra = extra unless extra.empty?
      end
      use_callee_params = callee_func && call_args.size <= callee_func.params.size
      # Debug void args
      has_void_arg = call_args.any? { |a| @type_mapper.llvm_type(@value_types[a]? || TypeRef::POINTER) == "void" }
      args = if use_callee_params && callee_func
               call_args.map_with_index { |a, i|
                 param_type = callee_func.params[i].type
                 expected_llvm_type = @type_mapper.llvm_type(param_type)
                 actual_type = @value_types[a]? || TypeRef::POINTER
                 actual_llvm_type = @type_mapper.llvm_type(actual_type)
                 # Save MIR-derived type before emitted-type override (for ptr→int decisions)
                 mir_llvm_type = actual_llvm_type
                 # If the slot is ptr but value_types maps to a non-ptr type,
                 # value_ref will load as ptr and then cast to the value type.
                 # Only override to ptr if value_types also agrees it's ptr (or is unknown).
                 if @cross_block_slot_types[a]? == "ptr" && (actual_llvm_type == "ptr" || actual_llvm_type == "void")
                   actual_type = TypeRef::POINTER
                   actual_llvm_type = "ptr"
                 end
                 # Prefer already-emitted LLVM type when available. @value_types can lag
                 # behind after ABI-preserving adaptations and fromslot casts.
                 if val_name = @value_names[a]?
                   if emitted_actual = (@emitted_value_types["%#{val_name}"]? || @emitted_value_types[val_name]?)
                     actual_llvm_type = emitted_actual
                   elsif param_actual = current_func_param_type_by_llvm_name(val_name)
                     actual_llvm_type = @type_mapper.llvm_type(param_actual)
                   end
                 end
                 # Guard against void - not valid for function arguments
                 # Remember if original was void so we can use null instead of value_ref
                 original_was_void = actual_llvm_type == "void"
                 expected_llvm_type = "ptr" if expected_llvm_type == "void"
                 actual_llvm_type = "ptr" if actual_llvm_type == "void"

                 # If original type was void, use null instead of value reference
                 if original_was_void
                   "ptr null"
                 # If types match, use directly (but handle union 0/null and float literals specially)
                 elsif expected_llvm_type == actual_llvm_type
                   val = value_ref(a)
                   val_key = val.starts_with?('%') ? val[1..] : val
                   emitted_actual = @emitted_value_types[val]? || @emitted_value_types[val_key]? || actual_llvm_type
                   def_actual_llvm = if def_inst = find_def_inst(a)
                                       @type_mapper.llvm_type(def_inst.type)
                                     else
                                       actual_llvm_type
                                     end
                   if (expected_llvm_type == "float" || expected_llvm_type == "double") &&
                      (emitted_actual == "ptr" || def_actual_llvm == "ptr")
                     # value_ref can return a ptr SSA even when MIR says float/double.
                     # Decode packed scalar bits before passing to the callee.
                     c = @cond_counter
                     @cond_counter += 1
                     emit "%eq_ptr_to_fp.#{c}.bits64 = ptrtoint ptr #{val} to i64"
                     if expected_llvm_type == "double"
                       emit "%eq_ptr_to_fp.#{c}.val = bitcast i64 %eq_ptr_to_fp.#{c}.bits64 to double"
                     else
                       emit "%eq_ptr_to_fp.#{c}.bits32 = trunc i64 %eq_ptr_to_fp.#{c}.bits64 to i32"
                       emit "%eq_ptr_to_fp.#{c}.val = bitcast i32 %eq_ptr_to_fp.#{c}.bits32 to float"
                     end
                     "#{expected_llvm_type} %eq_ptr_to_fp.#{c}.val"
                   elsif expected_llvm_type.includes?(".union") &&
                      emitted_actual.includes?(".union") &&
                      (emitted_actual != expected_llvm_type || union_type_id_remap_needed?(actual_type, param_type))
                     c = @cond_counter
                     @cond_counter += 1
                     emit "%union_conv.eq.#{c}.src_ptr = alloca #{emitted_actual}, align 8"
                     emit "store #{emitted_actual} #{normalize_union_value(val, emitted_actual)}, ptr %union_conv.eq.#{c}.src_ptr"
                     emit "%union_conv.eq.#{c}.src_type_id_ptr = getelementptr #{emitted_actual}, ptr %union_conv.eq.#{c}.src_ptr, i32 0, i32 0"
                     emit "%union_conv.eq.#{c}.type_id = load i32, ptr %union_conv.eq.#{c}.src_type_id_ptr"
                     mapped_type_id = emit_union_type_id_remap(
                       emitted_actual,
                       expected_llvm_type,
                       "%union_conv.eq.#{c}.type_id",
                       "union_conv.eq.#{c}",
                       actual_type,
                       param_type
                     )
                     emit "%union_conv.eq.#{c}.src_payload_ptr = getelementptr #{emitted_actual}, ptr %union_conv.eq.#{c}.src_ptr, i32 0, i32 1"
                     emit "%union_conv.eq.#{c}.payload_as_ptr = load ptr, ptr %union_conv.eq.#{c}.src_payload_ptr, align 4"
                     emit "%union_conv.eq.#{c}.dst_ptr = alloca #{expected_llvm_type}, align 8"
                     emit "%union_conv.eq.#{c}.dst_type_id_ptr = getelementptr #{expected_llvm_type}, ptr %union_conv.eq.#{c}.dst_ptr, i32 0, i32 0"
                     emit "store i32 #{mapped_type_id}, ptr %union_conv.eq.#{c}.dst_type_id_ptr"
                     emit "%union_conv.eq.#{c}.dst_payload_ptr = getelementptr #{expected_llvm_type}, ptr %union_conv.eq.#{c}.dst_ptr, i32 0, i32 1"
                     emit "store ptr %union_conv.eq.#{c}.payload_as_ptr, ptr %union_conv.eq.#{c}.dst_payload_ptr, align 4"
                     emit "%union_conv.eq.#{c}.val = load #{expected_llvm_type}, ptr %union_conv.eq.#{c}.dst_ptr"
                     "#{expected_llvm_type} %union_conv.eq.#{c}.val"
                   # Union types need zeroinitializer instead of 0 or null
                   elsif expected_llvm_type.includes?(".union") && (val == "0" || val == "null")
                     "#{expected_llvm_type} zeroinitializer"
                   # Float types need 0.0 instead of 0
                   elsif (expected_llvm_type == "float" || expected_llvm_type == "double") && val == "0"
                     "#{expected_llvm_type} 0.0"
                   # Pointer types need null instead of 0
                   elsif expected_llvm_type == "ptr" && val == "0"
                     "ptr null"
                   else
                     "#{expected_llvm_type} #{val}"
                   end
                elsif expected_llvm_type == "ptr" && actual_llvm_type.starts_with?('%') && actual_llvm_type.includes?(".union")
                  # Coerce union to ptr: two strategies depending on context.
                  c = @cond_counter
                  @cond_counter += 1
                  val_ref = value_ref(a)
                  emitted_actual_llvm = @emitted_value_types[val_ref]? || actual_llvm_type
                  union_storage_type = emitted_actual_llvm.includes?(".union") ? emitted_actual_llvm : actual_llvm_type
                  def_inst = find_def_inst(a)
                  inst_llvm_type = def_inst ? @type_mapper.llvm_type(def_inst.type) : nil
                  if inst_llvm_type == "ptr" || @cross_block_slot_types[a]? == "ptr"
                    "ptr #{val_ref}"
                  else
                    # Callee expects ptr (concrete reference type) but we have a union value.
                    # Extract the payload (field 1 of the union struct) as a ptr.
                    # MIR type narrowing guarantees the active variant is a reference type
                    # at this point, so the payload is always a valid pointer.
                    temp_alloca = "%alloca.#{c}"
                    emit "#{temp_alloca} = alloca #{union_storage_type}, align 8"
                    emit "store #{union_storage_type} #{normalize_union_value(val_ref, union_storage_type)}, ptr #{temp_alloca}"
                    emit "%payload_ptr.#{c} = getelementptr #{union_storage_type}, ptr #{temp_alloca}, i32 0, i32 1"
                    emit "%payload_val.#{c} = load ptr, ptr %payload_ptr.#{c}, align 4"
                    "ptr %payload_val.#{c}"
                  end
                 elsif expected_llvm_type == "ptr" && actual_llvm_type == "i1"
                   # Bool to ptr - likely string context, convert bool to string
                   val = value_ref(a)
                   c = @cond_counter
                   @cond_counter += 1
                   emit "%bool_to_str.#{c} = call ptr @__crystal_v2_bool_to_string(i1 #{val})"
                   "ptr %bool_to_str.#{c}"
                elsif expected_llvm_type == "ptr" && (actual_llvm_type.starts_with?('i') || actual_llvm_type == "ptr")
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
                       if src_bits = actual_llvm_type[1..].to_i?
                         if src_bits < 64
                           ext_name = "#{temp_ptr}.ext"
                           ext_op = (actual_type == TypeRef::BOOL || unsigned_type_ref?(actual_type)) ? "zext" : "sext"
                           emit "#{ext_name} = #{ext_op} #{actual_llvm_type} #{val} to i64"
                           emit "#{temp_ptr} = inttoptr i64 #{ext_name} to ptr"
                         else
                           emit "#{temp_ptr} = inttoptr #{actual_llvm_type} #{val} to ptr"
                         end
                       else
                         emit "#{temp_ptr} = inttoptr #{actual_llvm_type} #{val} to ptr"
                       end
                       "ptr #{temp_ptr}"
                     end
                   end
                 elsif expected_llvm_type == "ptr" && (actual_llvm_type == "double" || actual_llvm_type == "float")
                   # Float to ptr conversion: bitcast float bits to int, then inttoptr
                   val = value_ref(a)
                   c = @cond_counter
                   @cond_counter += 1
                   if actual_llvm_type == "double"
                     emit "%fptoptr.#{c}.bits = bitcast double #{val} to i64"
                     emit "%fptoptr.#{c} = inttoptr i64 %fptoptr.#{c}.bits to ptr"
                   else
                     emit "%fptoptr.#{c}.bits = bitcast float #{val} to i32"
                     emit "%fptoptr.#{c}.ext = zext i32 %fptoptr.#{c}.bits to i64"
                     emit "%fptoptr.#{c} = inttoptr i64 %fptoptr.#{c}.ext to ptr"
                   end
                   "ptr %fptoptr.#{c}"
                elsif (expected_llvm_type == "double" || expected_llvm_type == "float") && actual_llvm_type == "ptr"
                  # Packed scalar in ptr form (common for union payloads): decode bit-pattern.
                  # Float64: ptr -> i64 -> bitcast to double
                  # Float32: ptr -> i64 -> trunc i32 -> bitcast to float
                  val = value_ref(a)
                  c = @cond_counter
                  @cond_counter += 1
                  emit "%ptr_to_fp.#{c}.bits64 = ptrtoint ptr #{val} to i64"
                  if expected_llvm_type == "double"
                    emit "%ptr_to_fp.#{c}.val = bitcast i64 %ptr_to_fp.#{c}.bits64 to double"
                  else
                    emit "%ptr_to_fp.#{c}.bits32 = trunc i64 %ptr_to_fp.#{c}.bits64 to i32"
                    emit "%ptr_to_fp.#{c}.val = bitcast i32 %ptr_to_fp.#{c}.bits32 to float"
                  end
                  "#{expected_llvm_type} %ptr_to_fp.#{c}.val"
                elsif (expected_llvm_type == "double" || expected_llvm_type == "float") && actual_llvm_type.starts_with?('i')
                   # Int to float conversion: signed or unsigned
                   val = value_ref(a)
                   c = @cond_counter
                   @cond_counter += 1
                   op = unsigned_type_ref?(actual_type) ? "uitofp" : "sitofp"
                   emit "%itofp.#{c} = #{op} #{actual_llvm_type} #{val} to #{expected_llvm_type}"
                   "#{expected_llvm_type} %itofp.#{c}"
                 elsif expected_llvm_type.starts_with?('i') && (actual_llvm_type == "float" || actual_llvm_type == "double")
                   # Float to int conversion: fptosi/fptoui based on signedness
                   val = value_ref(a)
                   c = @cond_counter
                   @cond_counter += 1
                   unsigned = param_type == TypeRef::UINT8 || param_type == TypeRef::UINT16 ||
                              param_type == TypeRef::UINT32 || param_type == TypeRef::UINT64 ||
                              param_type == TypeRef::UINT128
                   op = unsigned ? "fptoui" : "fptosi"
                   emit "%fpto_int.#{c} = #{op} #{actual_llvm_type} #{val} to #{expected_llvm_type}"
                   "#{expected_llvm_type} %fpto_int.#{c}"
                 elsif expected_llvm_type == "i1" && actual_llvm_type.starts_with?('i') && actual_llvm_type != "i1"
                   # Larger int to i1 (bool) - truncate
                   val = value_ref(a)
                   c = @cond_counter
                   @cond_counter += 1
                   emit "%trunc_to_i1.#{c} = trunc #{actual_llvm_type} #{val} to i1"
                   "i1 %trunc_to_i1.#{c}"
                 elsif expected_llvm_type == "i32" && actual_llvm_type == "i64"
                   # i64 to i32 - truncate
                   val = value_ref(a)
                   c = @cond_counter
                   @cond_counter += 1
                   emit "%trunc_to_i32.#{c} = trunc i64 #{val} to i32"
                   "i32 %trunc_to_i32.#{c}"
                 elsif expected_llvm_type == "i64" && (actual_llvm_type == "i32" || actual_llvm_type == "i16" || actual_llvm_type == "i8")
                   # Smaller int to i64 - sign extend
                   val = value_ref(a)
                   c = @cond_counter
                   @cond_counter += 1
                   emit "%sext_to_i64.#{c} = sext #{actual_llvm_type} #{val} to i64"
                   "i64 %sext_to_i64.#{c}"
                 elsif expected_llvm_type == "i32" && (actual_llvm_type == "i16" || actual_llvm_type == "i8")
                   # Smaller int to i32 - sign extend
                   val = value_ref(a)
                   c = @cond_counter
                   @cond_counter += 1
                   emit "%sext_to_i32.#{c} = sext #{actual_llvm_type} #{val} to i32"
                   "i32 %sext_to_i32.#{c}"
                 elsif expected_llvm_type == "i16" && actual_llvm_type == "i8"
                   # i8 to i16 - sign extend
                   val = value_ref(a)
                   c = @cond_counter
                   @cond_counter += 1
                   emit "%sext_to_i16.#{c} = sext i8 #{val} to i16"
                   "i16 %sext_to_i16.#{c}"
                 elsif expected_llvm_type == "i16" && (actual_llvm_type == "i32" || actual_llvm_type == "i64")
                   # Larger int to i16 - truncate
                   val = value_ref(a)
                   c = @cond_counter
                   @cond_counter += 1
                   emit "%trunc_to_i16.#{c} = trunc #{actual_llvm_type} #{val} to i16"
                   "i16 %trunc_to_i16.#{c}"
                 elsif expected_llvm_type == "i8" && (actual_llvm_type == "i16" || actual_llvm_type == "i32" || actual_llvm_type == "i64")
                   # Larger int to i8 - truncate
                   val = value_ref(a)
                   c = @cond_counter
                   @cond_counter += 1
                   emit "%trunc_to_i8.#{c} = trunc #{actual_llvm_type} #{val} to i8"
                   "i8 %trunc_to_i8.#{c}"
                 elsif expected_llvm_type.starts_with?('i') && actual_llvm_type.starts_with?('i') && expected_llvm_type != actual_llvm_type
                   expected_bits = expected_llvm_type[1..].to_i?
                   actual_bits = actual_llvm_type[1..].to_i?
                   if expected_bits && actual_bits
                     val = value_ref(a)
                     c = @cond_counter
                     @cond_counter += 1
                     if expected_bits > actual_bits
                       unsigned = param_type == TypeRef::UINT8 || param_type == TypeRef::UINT16 ||
                                  param_type == TypeRef::UINT32 || param_type == TypeRef::UINT64 ||
                                  param_type == TypeRef::UINT128
                       op = unsigned ? "zext" : "sext"
                       emit "%int_ext.#{c} = #{op} #{actual_llvm_type} #{val} to #{expected_llvm_type}"
                       "#{expected_llvm_type} %int_ext.#{c}"
                     else
                       emit "%int_trunc.#{c} = trunc #{actual_llvm_type} #{val} to #{expected_llvm_type}"
                       "#{expected_llvm_type} %int_trunc.#{c}"
                     end
                   else
                     "#{expected_llvm_type} #{value_ref(a)}"
                   end
                 elsif expected_llvm_type.starts_with?('i') && is_union_llvm_type?(actual_llvm_type)
                   # Coerce union to int: extract payload as int
                   # Union layout: { type_id : i32, payload : [N x i8] }
                   # Payload starts at offset 4, so max guaranteed alignment is 4
                   c = @cond_counter
                   @cond_counter += 1
                   val = value_ref(a)
                   emit "%union_to_int.#{c}.ptr = alloca #{actual_llvm_type}, align 8"
                   emit "store #{actual_llvm_type} #{val}, ptr %union_to_int.#{c}.ptr"
                   emit "%union_to_int.#{c}.payload_ptr = getelementptr #{actual_llvm_type}, ptr %union_to_int.#{c}.ptr, i32 0, i32 1"
                   emit "%union_to_int.#{c}.val = load #{expected_llvm_type}, ptr %union_to_int.#{c}.payload_ptr, align 4"
                   "#{expected_llvm_type} %union_to_int.#{c}.val"
                 elsif (expected_llvm_type == "float" || expected_llvm_type == "double") && is_union_llvm_type?(actual_llvm_type)
                   # Coerce union to float/double: extract payload as float
                   # Union layout: { type_id : i32, payload : [N x i8] }
                   # Payload starts at offset 4, so max guaranteed alignment is 4
                   # On ARM64, unaligned 8-byte loads require explicit align annotation
                   c = @cond_counter
                   @cond_counter += 1
                   val = value_ref(a)
                   emit "%union_to_fp.#{c}.ptr = alloca #{actual_llvm_type}, align 8"
                   emit "store #{actual_llvm_type} #{val}, ptr %union_to_fp.#{c}.ptr"
                   emit "%union_to_fp.#{c}.payload_ptr = getelementptr #{actual_llvm_type}, ptr %union_to_fp.#{c}.ptr, i32 0, i32 1"
                   emit "%union_to_fp.#{c}.val = load #{expected_llvm_type}, ptr %union_to_fp.#{c}.payload_ptr, align 4"
                   "#{expected_llvm_type} %union_to_fp.#{c}.val"
                 elsif expected_llvm_type.starts_with?('i') && actual_llvm_type == "ptr"
                   # Ptr to int conversion needed (ptrtoint)
                   val = value_ref(a)
                   # value_ref may have already cast from ptr to int (e.g., slot was ptr
                   # but @value_types says int). Check the actual emitted type.
                   emitted_type = @emitted_value_types[val]?
                   if emitted_type && emitted_type == expected_llvm_type
                     "#{expected_llvm_type} #{val}"
                   elsif emitted_type && emitted_type.starts_with?('i') && !emitted_type.includes?(".union")
                     # Already an integer, but might need widening/narrowing
                     if emitted_type == expected_llvm_type
                       "#{expected_llvm_type} #{val}"
                     else
                       c = @cond_counter
                       @cond_counter += 1
                       src_bits = emitted_type[1..].to_i? || 32
                       dst_bits = expected_llvm_type[1..].to_i? || 32
                       if dst_bits < src_bits
                         emit "%int_trunc.#{c} = trunc #{emitted_type} #{val} to #{expected_llvm_type}"
                         "#{expected_llvm_type} %int_trunc.#{c}"
                       elsif dst_bits > src_bits
                         emit "%int_ext.#{c} = sext #{emitted_type} #{val} to #{expected_llvm_type}"
                         "#{expected_llvm_type} %int_ext.#{c}"
                       else
                         "#{expected_llvm_type} #{val}"
                       end
                     end
                   else
                     # Decide: ptrtoint (packed scalar) vs load (pointer to data).
                     # Three signals identify packed scalars:
                     # 1. MIR type is scalar (i32,i8,etc) but emitted as ptr → packed via inttoptr
                     # 2. Value created by explicit inttoptr instruction
                     # 3. Value from an alloc'd enum/struct (already tracked in @alloc_types)
                     # Everything else (GEP results, allocas, loaded pointers) → load the data.
                     c = @cond_counter
                     @cond_counter += 1
                     is_packed_scalar = (mir_llvm_type.starts_with?('i') && !mir_llvm_type.includes?(".union")) ||
                                        mir_llvm_type == "float" || mir_llvm_type == "double" ||
                                        @inttoptr_value_ids.includes?(a)
                     if is_packed_scalar
                       temp_int = "%ptrtoint.#{c}"
                       emit "#{temp_int} = ptrtoint ptr #{val} to #{expected_llvm_type}"
                     else
                       # Pointer to data (GEP result, alloca, etc.) — load the value
                       temp_int = "%load_from_ptr.#{c}"
                       emit "#{temp_int} = load #{expected_llvm_type}, ptr #{val}"
                     end
                     "#{expected_llvm_type} #{temp_int}"
                   end
                 elsif is_union_llvm_type?(expected_llvm_type) &&
                       (actual_llvm_type.starts_with?('i') || actual_llvm_type == "float" || actual_llvm_type == "double")
                   # Scalar to union conversion - wrap scalar payload with correct variant type_id
                   val = value_ref(a)
                   c = @cond_counter
                   @cond_counter += 1
                   # Look up correct variant type_id from union descriptor
                   scalar_variant_type_id = if desc = @module.union_descriptors[param_type]?
                     matching_variant = desc.variants.find { |v| v.type_ref == actual_type }
                     if matching_variant
                       matching_variant.type_id
                     else
                       # Fallback: find first non-Nil variant
                       non_nil = desc.variants.find { |v| v.full_name != "Nil" }
                       non_nil ? non_nil.type_id : 0
                     end
                   else
                     0
                   end
                   emit "%scalar_to_union.#{c}.ptr = alloca #{expected_llvm_type}, align 8"
                   emit "%scalar_to_union.#{c}.type_id_ptr = getelementptr #{expected_llvm_type}, ptr %scalar_to_union.#{c}.ptr, i32 0, i32 0"
                   emit "store i32 #{scalar_variant_type_id}, ptr %scalar_to_union.#{c}.type_id_ptr"
                   emit "%scalar_to_union.#{c}.payload_ptr = getelementptr #{expected_llvm_type}, ptr %scalar_to_union.#{c}.ptr, i32 0, i32 1"
                   # Payload at offset 4, use align 4 for ARM64 compatibility
                   emit "store #{actual_llvm_type} #{val}, ptr %scalar_to_union.#{c}.payload_ptr, align 4"
                   emit "%scalar_to_union.#{c}.val = load #{expected_llvm_type}, ptr %scalar_to_union.#{c}.ptr"
                   "#{expected_llvm_type} %scalar_to_union.#{c}.val"
                 elsif is_union_llvm_type?(expected_llvm_type) && actual_llvm_type == "ptr"
                   # Ptr to union conversion - wrap ptr in union with correct variant type_id
                   val = value_ref(a)
                   c = @cond_counter
                   @cond_counter += 1
                   # Look up correct variant type_id from union descriptor
                   variant_type_id = if val == "null"
                     if desc = @module.union_descriptors[param_type]?
                       nil_variant = desc.variants.find { |v| v.full_name == "Nil" }
                       nil_variant ? nil_variant.type_id : 0
                     else
                       0
                     end
                   else
                     if desc = @module.union_descriptors[param_type]?
                       matching_variant = desc.variants.find { |v| v.type_ref == actual_type }
                       if matching_variant
                         matching_variant.type_id
                       else
                         # Fallback: find first non-Nil variant
                         non_nil = desc.variants.find { |v| v.full_name != "Nil" }
                         non_nil ? non_nil.type_id : 1
                       end
                     else
                       1
                     end
                   end
                   if val == "null"
                     emit "%ptr_to_union.#{c}.ptr = alloca #{expected_llvm_type}, align 8"
                     emit "%ptr_to_union.#{c}.type_id_ptr = getelementptr #{expected_llvm_type}, ptr %ptr_to_union.#{c}.ptr, i32 0, i32 0"
                     emit "store i32 #{variant_type_id}, ptr %ptr_to_union.#{c}.type_id_ptr"
                     emit "%ptr_to_union.#{c}.val = load #{expected_llvm_type}, ptr %ptr_to_union.#{c}.ptr"
                     "#{expected_llvm_type} %ptr_to_union.#{c}.val"
                   else
                     emit "%ptr_to_union.#{c}.ptr = alloca #{expected_llvm_type}, align 8"
                     emit "%ptr_to_union.#{c}.type_id_ptr = getelementptr #{expected_llvm_type}, ptr %ptr_to_union.#{c}.ptr, i32 0, i32 0"
                     emit "store i32 #{variant_type_id}, ptr %ptr_to_union.#{c}.type_id_ptr"
                     emit "%ptr_to_union.#{c}.payload_ptr = getelementptr #{expected_llvm_type}, ptr %ptr_to_union.#{c}.ptr, i32 0, i32 1"
                     # Use null instead of 0 for pointer types
                     ptr_val = val == "0" ? "null" : val
                     emit "store ptr #{ptr_val}, ptr %ptr_to_union.#{c}.payload_ptr, align 4"
                     emit "%ptr_to_union.#{c}.val = load #{expected_llvm_type}, ptr %ptr_to_union.#{c}.ptr"
                     "#{expected_llvm_type} %ptr_to_union.#{c}.val"
                   end
                 elsif expected_llvm_type.includes?(".union") && actual_llvm_type.includes?(".union") && expected_llvm_type != actual_llvm_type
                   # Different union types - extract from actual, store to expected
                   # This handles cases like Bool___Nil.union → ValueId___Nil.union
                   val = value_ref(a)
                   actual_union_type = @emitted_value_types[val]? || actual_llvm_type
                   actual_union_type = actual_llvm_type unless actual_union_type.includes?(".union")
                   c = @cond_counter
                   @cond_counter += 1
                   # Extract type_id from actual union
                   emit "%union_conv.#{c}.src_ptr = alloca #{actual_union_type}, align 8"
                   emit "store #{actual_union_type} #{normalize_union_value(val, actual_union_type)}, ptr %union_conv.#{c}.src_ptr"
                   emit "%union_conv.#{c}.src_type_id_ptr = getelementptr #{actual_union_type}, ptr %union_conv.#{c}.src_ptr, i32 0, i32 0"
                   emit "%union_conv.#{c}.type_id = load i32, ptr %union_conv.#{c}.src_type_id_ptr"
                   mapped_type_id = emit_union_type_id_remap(
                     actual_union_type,
                     expected_llvm_type,
                     "%union_conv.#{c}.type_id",
                     "union_conv.#{c}",
                     actual_type,
                     param_type
                   )
                   emit "%union_conv.#{c}.src_payload_ptr = getelementptr #{actual_union_type}, ptr %union_conv.#{c}.src_ptr, i32 0, i32 1"
                   emit "%union_conv.#{c}.payload_as_ptr = load ptr, ptr %union_conv.#{c}.src_payload_ptr, align 4"
                   # Store into expected union
                   emit "%union_conv.#{c}.dst_ptr = alloca #{expected_llvm_type}, align 8"
                   emit "%union_conv.#{c}.dst_type_id_ptr = getelementptr #{expected_llvm_type}, ptr %union_conv.#{c}.dst_ptr, i32 0, i32 0"
                   emit "store i32 #{mapped_type_id}, ptr %union_conv.#{c}.dst_type_id_ptr"
                   emit "%union_conv.#{c}.dst_payload_ptr = getelementptr #{expected_llvm_type}, ptr %union_conv.#{c}.dst_ptr, i32 0, i32 1"
                   emit "store ptr %union_conv.#{c}.payload_as_ptr, ptr %union_conv.#{c}.dst_payload_ptr, align 4"
                   emit "%union_conv.#{c}.val = load #{expected_llvm_type}, ptr %union_conv.#{c}.dst_ptr"
                   "#{expected_llvm_type} %union_conv.#{c}.val"
                 elsif expected_llvm_type == "float" && actual_llvm_type == "double"
                   # double to float - truncate
                   val = value_ref(a)
                   c = @cond_counter
                   @cond_counter += 1
                   emit "%fptrunc.#{c} = fptrunc double #{val} to float"
                   "float %fptrunc.#{c}"
                 elsif expected_llvm_type == "double" && actual_llvm_type == "float"
                   # float to double - extend
                   val = value_ref(a)
                   c = @cond_counter
                   @cond_counter += 1
                   emit "%fpext.#{c} = fpext float #{val} to double"
                   "double %fpext.#{c}"
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
                   # For union types, use zeroinitializer instead of 0
                   elsif expected_llvm_type.includes?(".union") && (val == "0" || val == "null")
                     "#{expected_llvm_type} zeroinitializer"
                   # For float types, convert 0 to 0.0
                   elsif (expected_llvm_type == "float" || expected_llvm_type == "double") && val == "0"
                     "#{expected_llvm_type} 0.0"
                   else
                     "#{expected_llvm_type} #{val}"
                   end
                 end
               }.join(", ")
             else
               # Fallback: use actual argument types from value registry
               call_args.map { |a|
                 arg_type = @value_types[a]? || TypeRef::POINTER
                 arg_llvm_type = @type_mapper.llvm_type(arg_type)
                 # Guard against void argument type - use ptr null
                 if arg_llvm_type == "void"
                   "ptr null"
                 else
                   val = value_ref(a)
                   # For union types, use zeroinitializer instead of 0
                   if arg_llvm_type.includes?(".union") && (val == "0" || val == "null")
                     "#{arg_llvm_type} zeroinitializer"
                   else
                     "#{arg_llvm_type} #{val}"
                   end
                 end
               }.join(", ")
             end

      # Append zero-value padding for missing default args
      if pad_args_extra
        if args.empty?
          args = pad_args_extra.join(", ")
        else
          args = args + ", " + pad_args_extra.join(", ")
        end
      end

      if return_type == "void"
        emit "call void @#{callee_name}(#{args})"
        # Mark as void so value_ref returns a safe default for downstream uses
        @void_values << inst.id
      elsif needs_ptr_to_union_wrap
        # ABI fix: callee returns ptr but caller expects union struct.
        # For nilable reference types, the callee uses ptr (null = nil, non-null = value)
        # but downstream code expects a union struct {i32 type_id, [N x i32] payload}.
        # Emit call with ptr return, then wrap into union.
        @value_names[inst.id] ||= "r#{inst.id}"
        c = @cond_counter
        @cond_counter += 1
        call_reg = "%ptr2union.#{c}.raw"
        emit "#{call_reg} = call #{return_type} @#{callee_name}(#{args})"
        record_emitted_type(call_reg, return_type)
        emit "%ptr2union.#{c}.alloca = alloca #{ptr_to_union_target_type}, align 8"
        emit "store #{ptr_to_union_target_type} zeroinitializer, ptr %ptr2union.#{c}.alloca"
        # Determine type_id: 0 = nil (ptr is null), 1 = non-nil
        emit "%ptr2union.#{c}.is_nil = icmp eq ptr #{call_reg}, null"
        emit "%ptr2union.#{c}.tid = select i1 %ptr2union.#{c}.is_nil, i32 0, i32 1"
        emit "%ptr2union.#{c}.tid_ptr = getelementptr #{ptr_to_union_target_type}, ptr %ptr2union.#{c}.alloca, i32 0, i32 0"
        emit "store i32 %ptr2union.#{c}.tid, ptr %ptr2union.#{c}.tid_ptr"
        emit "%ptr2union.#{c}.pay_ptr = getelementptr #{ptr_to_union_target_type}, ptr %ptr2union.#{c}.alloca, i32 0, i32 1"
        emit "store ptr #{call_reg}, ptr %ptr2union.#{c}.pay_ptr, align 4"
        emit "#{name} = load #{ptr_to_union_target_type}, ptr %ptr2union.#{c}.alloca"
        record_emitted_type(name, ptr_to_union_target_type)
        @value_types[inst.id] = inst.type
      elsif needs_union_to_union_wrap
        # ABI fix: callee returns one union layout, caller/prepass expects another.
        # Preserve payload and remap type_id across variant orderings.
        @value_names[inst.id] ||= "r#{inst.id}"
        c = @cond_counter
        @cond_counter += 1
        call_reg = "%u2u_call.#{c}.raw"
        emit "#{call_reg} = call #{return_type} @#{callee_name}(#{args})"
        record_emitted_type(call_reg, return_type)
        emit "%u2u_call.#{c}.src = alloca #{return_type}, align 8"
        emit "store #{return_type} #{call_reg}, ptr %u2u_call.#{c}.src"
        emit "%u2u_call.#{c}.src_type_id_ptr = getelementptr #{return_type}, ptr %u2u_call.#{c}.src, i32 0, i32 0"
        emit "%u2u_call.#{c}.src_type_id = load i32, ptr %u2u_call.#{c}.src_type_id_ptr"
        mapped_type_id = emit_union_type_id_remap(return_type, union_to_union_target_type, "%u2u_call.#{c}.src_type_id", "u2u_call.#{c}")
        emit "%u2u_call.#{c}.src_payload_ptr = getelementptr #{return_type}, ptr %u2u_call.#{c}.src, i32 0, i32 1"
        emit "%u2u_call.#{c}.payload_as_ptr = load ptr, ptr %u2u_call.#{c}.src_payload_ptr, align 4"
        emit "%u2u_call.#{c}.dst = alloca #{union_to_union_target_type}, align 8"
        emit "%u2u_call.#{c}.dst_type_id_ptr = getelementptr #{union_to_union_target_type}, ptr %u2u_call.#{c}.dst, i32 0, i32 0"
        emit "store i32 #{mapped_type_id}, ptr %u2u_call.#{c}.dst_type_id_ptr"
        emit "%u2u_call.#{c}.dst_payload_ptr = getelementptr #{union_to_union_target_type}, ptr %u2u_call.#{c}.dst, i32 0, i32 1"
        emit "store ptr %u2u_call.#{c}.payload_as_ptr, ptr %u2u_call.#{c}.dst_payload_ptr, align 4"
        emit "#{name} = load #{union_to_union_target_type}, ptr %u2u_call.#{c}.dst"
        record_emitted_type(name, union_to_union_target_type)
        @value_types[inst.id] = inst.type
      elsif needs_union_abi_fix
        # Ensure value_names is registered (may have been skipped if prepass wrongly marked void)
        @value_names[inst.id] ||= "r#{inst.id}"
        # ABI fix: emit call with correct union return type, then extract payload
        # as the simpler type that downstream code expects.
        c = @cond_counter
        @cond_counter += 1
        call_reg = "%union_abi.#{c}.call"
        emit "#{call_reg} = call #{return_type} @#{callee_name}(#{args})"
        record_emitted_type(call_reg, return_type)
        emit "%union_abi.#{c}.alloca = alloca #{return_type}, align 8"
        emit "store #{return_type} #{call_reg}, ptr %union_abi.#{c}.alloca"
        emit "%union_abi.#{c}.payload = getelementptr #{return_type}, ptr %union_abi.#{c}.alloca, i32 0, i32 1"
        emit "#{name} = load #{union_abi_desired_type}, ptr %union_abi.#{c}.payload, align 4"
        record_emitted_type(name, union_abi_desired_type)
        @value_types[inst.id] = prepass_type.not_nil!
      else
        # Ensure value_names is registered (may have been skipped if prepass wrongly marked void)
        @value_names[inst.id] ||= "r#{inst.id}"
        emit "#{name} = call #{return_type} @#{callee_name}(#{args})"
        record_emitted_type(name, return_type)
        # Update value_types to match EMITTED return type (not callee's return type)
        # This is critical because prepass may have determined a different type
        if return_type.includes?(".union")
          # For union types, find matching TypeRef for the emitted union
          # Use prepass_type if it matches, otherwise try to find from callee
          resolved_union_type = nil.as(TypeRef?)
          if prepass_type && @type_mapper.llvm_type(prepass_type).includes?(".union")
            resolved_union_type = prepass_type
          elsif callee_func && @type_mapper.llvm_type(callee_func.return_type).includes?(".union")
            resolved_union_type = callee_func.return_type
          else
            # Fallback to inst.type if it's a union
            if @type_mapper.llvm_type(inst.type).includes?(".union")
              resolved_union_type = inst.type
            else
              resolved_union_type = find_type_ref_for_llvm_type(return_type)
            end
          end
          @value_types[inst.id] = resolved_union_type if resolved_union_type
        else
          # If the MIR type is a tuple or StaticArray, preserve it even if ABI uses ptr.
          if tuple_type = @module.type_registry.get(inst.type)
            if tuple_type.kind.tuple? || tuple_type.name.starts_with?("StaticArray(")
              @value_types[inst.id] = inst.type
            else
              actual_type_ref = case return_type
                                when "i1" then TypeRef::BOOL
                                when "i8" then TypeRef::INT8
                                when "i16" then TypeRef::INT16
                                when "i32" then TypeRef::INT32
                                when "i64" then TypeRef::INT64
                                when "i128" then TypeRef::INT128
                                when "float" then TypeRef::FLOAT32
                                when "double" then TypeRef::FLOAT64
                                when "ptr" then TypeRef::POINTER
                                else TypeRef::POINTER  # Default fallback
                                end
              @value_types[inst.id] = actual_type_ref
            end
          else
            actual_type_ref = case return_type
                              when "i1" then TypeRef::BOOL
                              when "i8" then TypeRef::INT8
                              when "i16" then TypeRef::INT16
                              when "i32" then TypeRef::INT32
                              when "i64" then TypeRef::INT64
                              when "i128" then TypeRef::INT128
                              when "float" then TypeRef::FLOAT32
                              when "double" then TypeRef::FLOAT64
                              when "ptr" then TypeRef::POINTER
                              else TypeRef::POINTER  # Default fallback
                              end
            @value_types[inst.id] = actual_type_ref
          end
        end
      end
      # Track called function for forward declaration if missing at end of IR gen
      arg_type_strs = inst.args.map do |a|
        at = @value_types[a]?
        at ? @type_mapper.llvm_type(at) : "ptr"
      end.reject { |t| t == "void" }
      @called_crystal_functions[callee_name] ||= {(return_type == "void" ? "ptr" : return_type), arg_type_strs.size, arg_type_strs}
    end

    private def emit_indirect_call(inst : IndirectCall, name : String)
      return_type = @type_mapper.llvm_type(inst.type)
      callee = value_ref(inst.callee_ptr)
      # Handle args: union types need special handling - pass ptr to slot/alloca, not loaded value.
      # Non-union values should be passed by value with their LLVM type.
      arg_strs = inst.args.compact_map do |a|
        arg_type = @value_types[a]?
        next if arg_type == TypeRef::VOID
        if arg_type
          arg_llvm_type = @type_mapper.llvm_type(arg_type)
          # Nil-like arguments map to LLVM "void" in our type mapper, but LLVM
          # function arguments cannot be typed as void. Pass them as ptr values.
          if arg_llvm_type == "void"
            val = value_ref(a)
            if val == "0" || val == "null"
              "ptr null"
            else
              "ptr #{val}"
            end
          elsif arg_llvm_type.includes?(".union")
            # For union types, check if we have a cross-block slot to use
            if slot_name = @cross_block_slots[a]?
              "ptr %#{slot_name}"
            else
              # No slot - need to alloca and store the union value
              temp_slot = "%indirect_union_arg.#{@cond_counter}"
              @cond_counter += 1
              emit "#{temp_slot} = alloca #{arg_llvm_type}, align 8"
              emit "store #{arg_llvm_type} #{value_ref(a)}, ptr #{temp_slot}"
              "ptr #{temp_slot}"
            end
          else
            "#{arg_llvm_type} #{value_ref(a)}"
          end
        else
          "ptr #{value_ref(a)}"
        end
      end
      args = arg_strs.join(", ")
      if return_type == "void"
        emit "call void #{callee}(#{args})"
        # Mark as void so value_ref returns a safe default for downstream uses
        @void_values << inst.id
      else
        emit "#{name} = call #{return_type} #{callee}(#{args})"
        # Track actual return type for downstream use
        actual_type = case return_type
                      when "i1" then TypeRef::BOOL
                      when "i8" then TypeRef::INT8
                      when "i16" then TypeRef::INT16
                      when "i32" then TypeRef::INT32
                      when "i64" then TypeRef::INT64
                      when "i128" then TypeRef::INT128
                      when "float" then TypeRef::FLOAT32
                      when "double" then TypeRef::FLOAT64
                      when "ptr" then TypeRef::POINTER
                      else inst.type  # Fallback to MIR type
                      end
        @value_types[inst.id] = actual_type
      end
    end

    private def find_type_ref_for_llvm_type(llvm_type : String) : TypeRef?
      @module.type_registry.types.each do |type|
        ref = TypeRef.new(type.id)
        return ref if @type_mapper.llvm_type(ref) == llvm_type
      end
      nil
    end

    private def emit_extern_call(inst : ExternCall, name : String)
      # Intercept __crystal_v2_select_ptr(is_a_bool, obj) → select i1, ptr, ptr null
      # Used by as?() implementation: returns obj if type matches, null if not.
      if inst.extern_name == "__crystal_v2_select_ptr" && inst.args.size == 2
        cond_val = value_ref(inst.args[0])
        obj_val = value_ref(inst.args[1])
        cond_type = lookup_value_llvm_type(inst.args[0])
        if cond_type != "i1"
          cond_i1 = "%as_q_cond.#{inst.id}"
          emit "#{cond_i1} = trunc #{cond_type} #{cond_val} to i1"
          cond_val = cond_i1
        end
        # If obj is a union struct (not ptr), extract the payload pointer first
        obj_type_ref = @value_types[inst.args[1]]?
        obj_llvm_type = obj_type_ref ? @type_mapper.llvm_type(obj_type_ref) : "ptr"
        if obj_llvm_type.includes?(".union")
          # Extract payload ptr from union: alloca union → store → gep to payload → load ptr
          base = name.lstrip('%')
          emit "%#{base}.sel_union = alloca #{obj_llvm_type}, align 8"
          emit "store #{obj_llvm_type} #{obj_val}, ptr %#{base}.sel_union"
          emit "%#{base}.sel_pay = getelementptr #{obj_llvm_type}, ptr %#{base}.sel_union, i32 0, i32 1"
          emit "%#{base}.sel_ptr = load ptr, ptr %#{base}.sel_pay, align 4"
          obj_val = "%#{base}.sel_ptr"
        elsif obj_llvm_type != "ptr" && obj_llvm_type.starts_with?('i')
          # Integer value (e.g., i32 ExprId) used in select_ptr — cast to ptr
          base = name.lstrip('%')
          if obj_llvm_type == "i64"
            emit "%#{base}.sel_itp = inttoptr i64 #{obj_val} to ptr"
          else
            emit "%#{base}.sel_ext = sext #{obj_llvm_type} #{obj_val} to i64"
            emit "%#{base}.sel_itp = inttoptr i64 %#{base}.sel_ext to ptr"
          end
          obj_val = "%#{base}.sel_itp"
        end
        emit "#{name} = select i1 #{cond_val}, ptr #{obj_val}, ptr null"
        # Always register as POINTER — the select returns ptr (payload extracted above if union)
        @value_types[inst.id] = TypeRef::POINTER
        return
      end

      if debug_extern_filter = ENV["DEBUG_EXTERN_CALL"]?
        if debug_extern_filter.empty? || debug_extern_filter == "1" || inst.extern_name.includes?(debug_extern_filter)
          STDERR.puts "[EXTERN_CALL] extern_name=#{inst.extern_name} args=#{inst.args.size}"
        end
      end
      return_type = @type_mapper.llvm_type(inst.type)

      # IMPORTANT: Check if prepass determined a different type (e.g., from phi usage)
      # Prepass type takes precedence over inst.type when phi expects a specific type
      prepass_type = @value_types[inst.id]?
      if prepass_type
        prepass_llvm_type = @type_mapper.llvm_type(prepass_type)
        if prepass_llvm_type != "void" && prepass_llvm_type != return_type
          return_type = prepass_llvm_type
        end
      end

      cast_fixed_arg = ->(actual_type : String, actual_type_ref : TypeRef?, value : String, expected_type : String, expected_type_ref : TypeRef?) : String {
        if expected_type == "void"
          expected_type = "ptr"
        end
        if actual_type == "void"
          actual_type = "ptr"
          value = "null"
        end
        return value if actual_type == expected_type

        c = @cond_counter
        @cond_counter += 1
        cast_name = "%varargs_cast.#{c}"

        if actual_type.includes?(".union") && expected_type.includes?(".union")
          src_ptr = "%varargs_u2u.#{c}.src_ptr"
          dst_val = "%varargs_u2u.#{c}.val"
          emit "#{src_ptr} = alloca #{actual_type}, align 8"
          emit "store #{actual_type} #{normalize_union_value(value, actual_type)}, ptr #{src_ptr}"
          emit "#{dst_val} = load #{expected_type}, ptr #{src_ptr}"
          return dst_val
        end

        if actual_type == "ptr" && expected_type.starts_with?('i')
          emit "#{cast_name} = ptrtoint ptr #{value} to #{expected_type}"
          return cast_name
        end

        if expected_type == "ptr" && actual_type.starts_with?('i')
          emit "#{cast_name} = inttoptr #{actual_type} #{value} to ptr"
          return cast_name
        end

        if actual_type == "ptr" && (expected_type == "double" || expected_type == "float")
          emit "#{cast_name}.int = ptrtoint ptr #{value} to i64"
          emit "#{cast_name} = uitofp i64 #{cast_name}.int to #{expected_type}"
          return cast_name
        end

        if expected_type == "ptr" && (actual_type == "double" || actual_type == "float")
          if actual_type == "double"
            emit "#{cast_name}.bits = bitcast double #{value} to i64"
            emit "#{cast_name} = inttoptr i64 #{cast_name}.bits to ptr"
          else
            emit "#{cast_name}.bits = bitcast float #{value} to i32"
            emit "#{cast_name}.ext = zext i32 #{cast_name}.bits to i64"
            emit "#{cast_name} = inttoptr i64 #{cast_name}.ext to ptr"
          end
          return cast_name
        end

        # Int <-> float conversions (avoid invalid LLVM bitcasts)
        if (expected_type == "double" || expected_type == "float") && actual_type.starts_with?('i')
          op = (actual_type_ref && unsigned_type_ref?(actual_type_ref)) ? "uitofp" : "sitofp"
          emit "#{cast_name} = #{op} #{actual_type} #{value} to #{expected_type}"
          return cast_name
        end

        if expected_type.starts_with?('i') && (actual_type == "double" || actual_type == "float")
          op = (expected_type_ref && unsigned_type_ref?(expected_type_ref)) ? "fptoui" : "fptosi"
          emit "#{cast_name} = #{op} #{actual_type} #{value} to #{expected_type}"
          return cast_name
        end

        if expected_type == "double" && actual_type == "float"
          emit "#{cast_name} = fpext float #{value} to double"
          return cast_name
        end

        if expected_type == "float" && actual_type == "double"
          emit "#{cast_name} = fptrunc double #{value} to float"
          return cast_name
        end

        if actual_type.starts_with?('i') && expected_type.starts_with?('i')
          actual_bits = actual_type[1..].to_i?
          expected_bits = expected_type[1..].to_i?
          if actual_bits && expected_bits
            if actual_bits < expected_bits
              ext_op = (actual_type_ref && unsigned_type_ref?(actual_type_ref)) ? "zext" : "sext"
              emit "#{cast_name} = #{ext_op} #{actual_type} #{value} to #{expected_type}"
            elsif actual_bits > expected_bits
              emit "#{cast_name} = trunc #{actual_type} #{value} to #{expected_type}"
            else
              emit "#{cast_name} = bitcast #{actual_type} #{value} to #{expected_type}"
            end
          else
            emit "#{cast_name} = bitcast #{actual_type} #{value} to #{expected_type}"
          end
          return cast_name
        end

        emit "#{cast_name} = bitcast #{actual_type} #{value} to #{expected_type}"
        cast_name
      }

      arg_entries = inst.args.map do |arg_id|
        arg_type_ref = @value_types[arg_id]? || TypeRef::POINTER
        arg_type = @type_mapper.llvm_type(arg_type_ref)

        if arg_type == "void"
          {"ptr", "null", nil.as(TypeRef?)}
        else
          val = value_ref(arg_id)
          if emitted_arg_type = @emitted_value_types[val]?
            arg_type = emitted_arg_type
          end
          if arg_type.includes?(".union") && (val == "0" || val == "null")
            {arg_type, "zeroinitializer", arg_type_ref.as(TypeRef?)}
          elsif arg_type == "ptr" && val == "0"
            {"ptr", "null", arg_type_ref.as(TypeRef?)}
          else
            {arg_type, val, arg_type_ref.as(TypeRef?)}
          end
        end
      end

      # Mangle the extern name to be a valid LLVM identifier.
      # Preserve '$' for platform-specific C symbols like realpath$DARWIN_EXTSN.
      mangled_extern_name = @type_mapper.mangle_name(inst.extern_name)
      if inst.extern_name.includes?('$') &&
         !inst.extern_name.includes?("::") &&
         !inst.extern_name.includes?('#') &&
         !inst.extern_name.includes?('.') &&
         inst.extern_name.matches?(/\A[a-zA-Z0-9_$]+\z/)
        mangled_extern_name = inst.extern_name
      end

      # Handle Pointer.new / Pointer.new! - convert integer to pointer
      if pointer_constructor_name?(inst.extern_name) && inst.args.size == 1
        arg_id = inst.args[0]
        arg = value_ref(arg_id)
        arg_type = lookup_value_llvm_type(arg_id)
        if arg_type == "ptr"
          emit "#{name} = bitcast ptr #{arg} to ptr"
        elsif arg_type.starts_with?('i')
          emit "#{name} = inttoptr #{arg_type} #{arg} to ptr"
          @inttoptr_value_ids.add(inst.id)
        else
          emit "#{name} = bitcast ptr #{arg} to ptr"
        end
        record_emitted_type(name, "ptr")
        @value_types[inst.id] = TypeRef::POINTER
        return
      end

      # Special handling for LLVM intrinsics - need correct argument types
      if mangled_extern_name.starts_with?("llvm.")
        # Normalize old typed pointer format (p0i8) to opaque pointer format (p0) for LLVM 15+
        normalized_intrinsic = mangled_extern_name
          .gsub("p0i8.p0i8", "p0.p0")  # memcpy/memmove: llvm.memcpy.p0i8.p0i8.i32 → llvm.memcpy.p0.p0.i32
          .gsub("p0i8", "p0")          # memset: llvm.memset.p0i8.i32 → llvm.memset.p0.i32
        args, return_type = emit_llvm_intrinsic_call_args(inst, name, normalized_intrinsic)
        if return_type == "void"
          emit "call void @#{normalized_intrinsic}(#{args})"
          @void_values << inst.id
        else
          emit "#{name} = call #{return_type} @#{normalized_intrinsic}(#{args})"
          actual_type = case return_type
                        when "i1" then TypeRef::BOOL
                        when "i8" then TypeRef::INT8
                        when "i16" then TypeRef::INT16
                        when "i32" then TypeRef::INT32
                        when "i64" then TypeRef::INT64
                        when "i128" then TypeRef::INT128
                        when "float" then TypeRef::FLOAT32
                        when "double" then TypeRef::FLOAT64
                        when "ptr" then TypeRef::POINTER
                        else inst.type
                        end
          @value_types[inst.id] = actual_type
        end
        return
      end

      # Handle nil? and not_nil! intrinsics inline
      # These are Crystal intrinsics that should be inlined, not external calls
      if mangled_extern_name == "nil_" && inst.args.size == 1
        arg_id = inst.args[0]
        arg_type = @value_types[arg_id]? || TypeRef::POINTER
        arg_llvm_type = @type_mapper.llvm_type(arg_type)
        arg_val = value_ref(arg_id)

        if arg_llvm_type == "ptr"
          if @cross_block_slot_types[arg_id]? == "i1"
            arg_type = TypeRef::BOOL
            arg_llvm_type = "i1"
            @value_types[arg_id] = TypeRef::BOOL
          end
          if def_inst = find_def_inst(arg_id)
            if def_inst.type == TypeRef::BOOL || @type_mapper.llvm_type(def_inst.type) == "i1"
              arg_type = TypeRef::BOOL
              arg_llvm_type = "i1"
              @value_types[arg_id] = TypeRef::BOOL
            elsif def_inst.is_a?(BinaryOp) &&
               (def_inst.op.eq? || def_inst.op.ne? || def_inst.op.lt? || def_inst.op.le? || def_inst.op.gt? || def_inst.op.ge?)
              arg_type = TypeRef::BOOL
              arg_llvm_type = "i1"
              @value_types[arg_id] = TypeRef::BOOL
            end
          end
        end

        if arg_llvm_type.includes?(".union")
          # Union type: extract type tag and compare to NIL (type_id = 1)
          c = @cond_counter
          @cond_counter += 1
          # Union is { i32 type_id, [N x i8] payload }
          # Store union to memory to extract type_id
          emit "%nil_check_alloca.#{c} = alloca #{arg_llvm_type}, align 8"
          emit "store #{arg_llvm_type} #{normalize_union_value(arg_val, arg_llvm_type)}, ptr %nil_check_alloca.#{c}"
          emit "%nil_check_tag_ptr.#{c} = getelementptr #{arg_llvm_type}, ptr %nil_check_alloca.#{c}, i32 0, i32 0"
          emit "%nil_check_tag.#{c} = load i32, ptr %nil_check_tag_ptr.#{c}"
          emit "#{name} = icmp eq i32 %nil_check_tag.#{c}, 1"  # 1 = NIL type_id
        elsif arg_llvm_type == "ptr"
          # Pointer type: compare to null
          emit "#{name} = icmp eq ptr #{arg_val}, null"
        else
          # Primitive type (i32, i64, etc.): cannot be nil, always false
          emit "#{name} = add i1 0, 0"  # false
        end
        @value_types[inst.id] = TypeRef::BOOL
        @value_names[inst.id] = "r#{inst.id}"  # Register for later value_ref lookups
        return
      end

      if mangled_extern_name == "not_nil_" && inst.args.size == 1
        arg_id = inst.args[0]
        arg_type = @value_types[arg_id]? || TypeRef::POINTER
        arg_llvm_type = @type_mapper.llvm_type(arg_type)
        arg_val = value_ref(arg_id)

        if arg_llvm_type == "ptr"
          if @cross_block_slot_types[arg_id]? == "i1"
            arg_type = TypeRef::BOOL
            arg_llvm_type = "i1"
            @value_types[arg_id] = TypeRef::BOOL
          end
          if def_inst = find_def_inst(arg_id)
            if def_inst.type == TypeRef::BOOL || @type_mapper.llvm_type(def_inst.type) == "i1"
              arg_type = TypeRef::BOOL
              arg_llvm_type = "i1"
              @value_types[arg_id] = TypeRef::BOOL
            elsif def_inst.is_a?(BinaryOp) &&
               (def_inst.op.eq? || def_inst.op.ne? || def_inst.op.lt? || def_inst.op.le? || def_inst.op.gt? || def_inst.op.ge?)
              arg_type = TypeRef::BOOL
              arg_llvm_type = "i1"
              @value_types[arg_id] = TypeRef::BOOL
            end
          end
        end

        if arg_llvm_type.includes?(".union")
          # Union type: extract the payload value
          # Union is { i32 type_id, [N x i8] payload }
          # Determine the non-nil type from union name (e.g., %Int32___Nil.union -> i32)
          result_type = "i32"  # Default to i32
          if arg_llvm_type.includes?("Int64")
            result_type = "i64"
          elsif arg_llvm_type.includes?("Float64") || arg_llvm_type.includes?("Double")
            result_type = "double"
          elsif arg_llvm_type.includes?("Float32")
            result_type = "float"
          elsif arg_llvm_type.includes?("String") || arg_llvm_type.includes?("Pointer") || arg_llvm_type.includes?("Array")
            result_type = "ptr"
          end

          c = @cond_counter
          @cond_counter += 1
          emit "%not_nil_alloca.#{c} = alloca #{arg_llvm_type}, align 8"
          emit "store #{arg_llvm_type} #{normalize_union_value(arg_val, arg_llvm_type)}, ptr %not_nil_alloca.#{c}"
          emit "%not_nil_payload_ptr.#{c} = getelementptr #{arg_llvm_type}, ptr %not_nil_alloca.#{c}, i32 0, i32 1"
          emit "#{name} = load #{result_type}, ptr %not_nil_payload_ptr.#{c}, align 4"

          result_type_ref = case result_type
                            when "i32" then TypeRef::INT32
                            when "i64" then TypeRef::INT64
                            when "float" then TypeRef::FLOAT32
                            when "double" then TypeRef::FLOAT64
                            when "ptr" then TypeRef::POINTER
                            else TypeRef::INT32
                            end
          @value_types[inst.id] = result_type_ref
          @value_names[inst.id] = "r#{inst.id}"  # Register for later value_ref lookups
        else
          # Non-union type: just return the value itself
          # Guard against void type - void values are nil, return null ptr instead
          if arg_llvm_type == "void"
            emit "#{name} = inttoptr i64 0 to ptr"
            @value_types[inst.id] = TypeRef::POINTER
          else
            emit "#{name} = bitcast #{arg_llvm_type} #{arg_val} to #{arg_llvm_type}"
            @value_types[inst.id] = arg_type
          end
          @value_names[inst.id] = "r#{inst.id}"  # Register for later value_ref lookups
        end
        return
      end

      # Handle to_u64!/to_i64!/to_i32! on numeric union types (e.g., Int8 | Int16 | Int32#to_u64!)
      # These are sign/zero extensions of primitive values and should be inlined
      if (mangled_extern_name.includes?("to_u64_") || mangled_extern_name.includes?("to_i64_")) && inst.args.size == 1
        arg_id = inst.args[0]
        arg_type = @value_types[arg_id]? || TypeRef::POINTER
        arg_llvm_type = @type_mapper.llvm_type(arg_type)
        arg_val = value_ref(arg_id)
        target_unsigned = mangled_extern_name.includes?("to_u64_")
        ext_op = target_unsigned ? "zext" : "sext"
        target_type_ref = target_unsigned ? TypeRef::UINT64 : TypeRef::INT64

        c = @cond_counter
        @cond_counter += 1

        if arg_llvm_type.includes?(".union")
          # Union of numeric types - extract payload and sign-extend to i64
          # Union is { i32 type_id, [N x i8] payload }
          # For numeric unions like Int8|Int16|Int32, just load the payload as i64
          emit "%to_i64_alloca.#{c} = alloca #{arg_llvm_type}, align 8"
          emit "store #{arg_llvm_type} #{normalize_union_value(arg_val, arg_llvm_type)}, ptr %to_i64_alloca.#{c}"
          emit "%to_i64_payload_ptr.#{c} = getelementptr #{arg_llvm_type}, ptr %to_i64_alloca.#{c}, i32 0, i32 1"
          emit "%to_i64_val.#{c} = load i32, ptr %to_i64_payload_ptr.#{c}, align 4"  # Load as i32 (covers Int8/16/32)
          emit "#{name} = #{ext_op} i32 %to_i64_val.#{c} to i64"
          @value_types[inst.id] = target_type_ref
        elsif arg_llvm_type == "i64"
          emit "#{name} = add i64 #{arg_val}, 0"
          @value_types[inst.id] = target_type_ref
        elsif arg_llvm_type.starts_with?('i')
          # Primitive int type - sign or zero extend depending on target conversion.
          emit "#{name} = #{ext_op} #{arg_llvm_type} #{arg_val} to i64"
          @value_types[inst.id] = target_type_ref
        else
          # Fallback - treat as i32 and extend
          emit "#{name} = #{ext_op} i32 #{arg_val} to i64"
          @value_types[inst.id] = target_type_ref
        end
        @value_names[inst.id] = "r#{inst.id}"
        return
      end

      if (mangled_extern_name.includes?("to_u32_") || mangled_extern_name.includes?("to_u_") || mangled_extern_name.includes?("to_i32_") || mangled_extern_name.includes?("to_i_")) && inst.args.size == 1
        arg_id = inst.args[0]
        arg_type = @value_types[arg_id]? || TypeRef::POINTER
        arg_llvm_type = @type_mapper.llvm_type(arg_type)
        arg_val = value_ref(arg_id)
        target_unsigned = mangled_extern_name.includes?("to_u32_") || mangled_extern_name.includes?("to_u_")
        ext_op = target_unsigned ? "zext" : "sext"
        target_type_ref = target_unsigned ? TypeRef::UINT32 : TypeRef::INT32

        c = @cond_counter
        @cond_counter += 1

        if arg_llvm_type.includes?(".union")
          # Union of numeric types - extract payload as i32
          emit "%to_i32_alloca.#{c} = alloca #{arg_llvm_type}, align 8"
          emit "store #{arg_llvm_type} #{normalize_union_value(arg_val, arg_llvm_type)}, ptr %to_i32_alloca.#{c}"
          emit "%to_i32_payload_ptr.#{c} = getelementptr #{arg_llvm_type}, ptr %to_i32_alloca.#{c}, i32 0, i32 1"
          emit "#{name} = load i32, ptr %to_i32_payload_ptr.#{c}, align 4"
          @value_types[inst.id] = target_type_ref
        elsif arg_llvm_type == "i64"
          emit "#{name} = trunc i64 #{arg_val} to i32"
          @value_types[inst.id] = target_type_ref
        elsif arg_llvm_type == "i32"
          emit "#{name} = add i32 #{arg_val}, 0"
          @value_types[inst.id] = target_type_ref
        elsif arg_llvm_type.starts_with?('i')
          emit "#{name} = #{ext_op} #{arg_llvm_type} #{arg_val} to i32"
          @value_types[inst.id] = target_type_ref
        elsif arg_llvm_type == "ptr"
          emit "#{name} = ptrtoint ptr #{arg_val} to i32"
          @value_types[inst.id] = target_type_ref
        else
          emit "#{name} = add i32 0, 0"  # Fallback to 0
          @value_types[inst.id] = target_type_ref
        end
        @value_names[inst.id] = "r#{inst.id}"
        return
      end

      # Handle ascii_alphanumeric? on Char - check if A-Z, a-z, or 0-9
      # ASCII ranges: A-Z = 65-90, a-z = 97-122, 0-9 = 48-57
      if mangled_extern_name == "ascii_alphanumeric_" && inst.args.size == 1
        arg_id = inst.args[0]
        arg_val = value_ref(arg_id)
        id = inst.id
        # Check if char is in any of the alphanumeric ranges using unique temp names
        # Check uppercase (65 <= c <= 90)
        emit "%aa_up_ge.#{id} = icmp uge i32 #{arg_val}, 65"
        emit "%aa_up_le.#{id} = icmp ule i32 #{arg_val}, 90"
        emit "%aa_upper.#{id} = and i1 %aa_up_ge.#{id}, %aa_up_le.#{id}"
        # Check lowercase (97 <= c <= 122)
        emit "%aa_lo_ge.#{id} = icmp uge i32 #{arg_val}, 97"
        emit "%aa_lo_le.#{id} = icmp ule i32 #{arg_val}, 122"
        emit "%aa_lower.#{id} = and i1 %aa_lo_ge.#{id}, %aa_lo_le.#{id}"
        # Check digit (48 <= c <= 57)
        emit "%aa_dg_ge.#{id} = icmp uge i32 #{arg_val}, 48"
        emit "%aa_dg_le.#{id} = icmp ule i32 #{arg_val}, 57"
        emit "%aa_digit.#{id} = and i1 %aa_dg_ge.#{id}, %aa_dg_le.#{id}"
        # Combine all three checks with OR
        emit "%aa_letter.#{id} = or i1 %aa_upper.#{id}, %aa_lower.#{id}"
        emit "#{name} = or i1 %aa_letter.#{id}, %aa_digit.#{id}"
        @value_types[inst.id] = TypeRef::BOOL
        @value_names[inst.id] = "r#{inst.id}"
        return
      end

      # Handle none?/zero? on numeric types - compare with 0
      # This handles flags enums like Unicode::CaseOptions#none?
      if mangled_extern_name.ends_with?("_none_") && inst.args.size == 1
        arg_id = inst.args[0]
        arg_type = @value_types[arg_id]? || TypeRef::INT32
        arg_llvm_type = @type_mapper.llvm_type(arg_type)
        arg_val = value_ref(arg_id)

        if arg_llvm_type.starts_with?('i')
          emit "#{name} = icmp eq #{arg_llvm_type} #{arg_val}, 0"
          @value_types[inst.id] = TypeRef::BOOL
          @value_names[inst.id] = "r#{inst.id}"
          return
        end
      end

      # If the mangled name doesn't match any defined function, try to find a match with namespace prefix
      # Search in MIR module's functions using strict matching:
      # - exact name/mangled name
      # - constrained fuzzy match for unresolved unqualified Crystal method names with explicit suffix
      extern_name = inst.extern_name
      receiver_and_method = extract_receiver_and_method(extern_name)
      is_operator = if receiver_and_method
                      _receiver_name, method_core = receiver_and_method
                      operator_method?(method_core)
                    else
                      operator_method?(extern_name)
                    end
      is_c_lib_function = !crystalish_extern_name?(extern_name)
      extern_method_core = method_core_from_name(extern_name)
      extern_suffix = suffix_after_dollar(extern_name)
      fuzzy_match_allowed = extern_suffix && extern_fuzzy_match_eligible?(extern_name, is_operator, is_c_lib_function)

      matching_func = @module.functions.find do |f|
        mangled = @type_mapper.mangle_name(f.name)
        next true if mangled == mangled_extern_name || f.name == extern_name
        next false unless fuzzy_match_allowed
        extern_fuzzy_matches_candidate?(extern_name, extern_method_core, extern_suffix.not_nil!, f.name)
      end
      if matching_func
        mangled_extern_name = @type_mapper.mangle_name(matching_func.name)
      end

      # Type suffix heuristics - apply BEFORE void check since MIR might have wrong ptr type
      # Methods with type suffix in HIR (e.g., "unsafe_shr$UInt64").
      if suffix = suffix_after_dollar(extern_name)
        if !suffix.includes?('_')
          case suffix
          when "UInt64", "Int64"
            return_type = "i64"
            @value_types[inst.id] = TypeRef::INT64
          when "UInt32", "Int32"
            return_type = "i32"
            @value_types[inst.id] = TypeRef::INT32
          when "UInt16", "Int16"
            return_type = "i16"
            @value_types[inst.id] = TypeRef::INT16
          when "UInt8", "Int8"
            return_type = "i8"
            @value_types[inst.id] = TypeRef::INT8
          end
        end
      end

      if !matching_func
        # Function not found - track for later declaration
        @undefined_externs[mangled_extern_name] = return_type unless @undefined_externs.has_key?(mangled_extern_name)
      end

      # Fallback: use prepass-determined type or minimal heuristics for constructors
      # Main type inference is done in prepass via use-based analysis
      if return_type == "void"
        # Check if prepass already determined this call returns a value (use-based inference)
        prepass_type = @value_types[inst.id]?
        if prepass_type && @type_mapper.llvm_type(prepass_type) != "void"
          return_type = @type_mapper.llvm_type(prepass_type)
        else
          # Minimal fallback: constructors and common conversion methods.
          method_core = method_core_from_name(extern_name)
          ptr_returning_methods = ["new", "allocate", "clone", "dup", "tap"]
          i64_returning_methods = ["to_i64", "to_u64"]
          i32_returning_methods = ["size", "length", "count", "hash", "to_i32", "to_i", "ord", "chr"]
          i16_returning_methods = ["to_i16", "to_u16"] of String
          i8_returning_methods = ["to_i8", "to_u8"] of String

        returns_ptr = ptr_returning_methods.includes?(method_core)

        # Check for arithmetic operators on typed receivers: use receiver type when method is "+" or "*".
        if receiver_and_method = extract_receiver_and_method(inst.extern_name)
          receiver_name, method_core = receiver_and_method
          if (method_core == "+" || method_core == "*") && receiver_name
            case receiver_name
            when "UInt8", "Int8"
              return_type = "i8"
              @value_types[inst.id] = TypeRef::INT8
            when "UInt16", "Int16"
              return_type = "i16"
              @value_types[inst.id] = TypeRef::INT16
            when "UInt32", "Int32"
              return_type = "i32"
              @value_types[inst.id] = TypeRef::INT32
            when "UInt64", "Int64"
              return_type = "i64"
              @value_types[inst.id] = TypeRef::INT64
            end
          elsif (method_core == "+" || method_core == "*") && inst.args.size > 0
            # Standalone operators: use first argument type when available.
            first_arg_type = @value_types[inst.args[0]]?
            if first_arg_type
              first_arg_llvm = @type_mapper.llvm_type(first_arg_type)
              if first_arg_llvm.starts_with?('i') && !first_arg_llvm.includes?('.')
                return_type = first_arg_llvm
                @value_types[inst.id] = first_arg_type
              end
            end
          end
        end

        # Check for type conversion methods (to_u64, to_i64, etc.)
        is_u64_conversion = method_core == "to_u64"
        is_i64_conversion = method_core == "to_i64"
        if is_u64_conversion || is_i64_conversion
          return_type = "i64"
          @value_types[inst.id] = TypeRef::INT64
        end

        # Check for i64-returning methods
        returns_i64 = i64_returning_methods.includes?(method_core)

        # Check for i32-returning methods
        returns_i32 = i32_returning_methods.includes?(method_core)

        # Check for i16-returning methods
        returns_i16 = i16_returning_methods.includes?(method_core)

        # Check for i8-returning methods
        returns_i8 = i8_returning_methods.includes?(method_core)

        # Predicate methods (ending in ?) return Bool, unless it's a bang method.
        is_bang_method = method_core.ends_with?('!')
        returns_bool = !is_bang_method && method_core.ends_with?('?')

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
        end  # end else for prepass_type check
      end

      # Known varargs functions need explicit signatures because we currently declare unknown
      # varargs as `declare <ret> @name(...)` (no fixed params). For correct ABI lowering,
      # the number and types of *fixed* parameters must match the real C prototype.
      #
      # IMPORTANT: Do not treat all arguments as fixed. That breaks `va_start` offsets and
      # results in garbage reads for functions like `printf`.
      varargs_signatures = {
        # stdio
        "printf"    => ["ptr"],
        "fprintf"   => ["ptr", "ptr"],
        "sprintf"   => ["ptr", "ptr"],
        "snprintf"  => ["ptr", "i64", "ptr"],
        "scanf"     => ["ptr"],
        "sscanf"    => ["ptr", "ptr"],
        "fscanf"    => ["ptr", "ptr"],
        "vprintf"   => ["ptr", "ptr"],
        "vfprintf"  => ["ptr", "ptr", "ptr"],
        "vsprintf"  => ["ptr", "ptr", "ptr"],
        "vsnprintf" => ["ptr", "i64", "ptr", "ptr"],
        # unix
        "open"      => ["ptr", "i32"],
        "fcntl"     => ["i32", "i32"],
        "ioctl"     => ["i32", "i64"],
        # exec*
        "execl"     => ["ptr", "ptr"],
        "execle"    => ["ptr", "ptr"],
        "execlp"    => ["ptr", "ptr"],
      } of String => Array(String)
      fixed_sig = varargs_signatures[mangled_extern_name]?

      if fixed_sig
        fixed_sig.each_with_index do |expected_type, idx|
          break if idx >= arg_entries.size
          actual_type, actual_val, actual_type_ref = arg_entries[idx]
          coerced_val = cast_fixed_arg.call(actual_type, actual_type_ref, actual_val, expected_type, nil)
          arg_entries[idx] = {expected_type, coerced_val, actual_type_ref}
        end
      elsif matching_func
        matching_func.params.each_with_index do |param, idx|
          break if idx >= arg_entries.size
          expected_type = @type_mapper.llvm_type(param.type)
          expected_type = "ptr" if expected_type == "void"
          actual_type, actual_val, actual_type_ref = arg_entries[idx]
          coerced_val = cast_fixed_arg.call(actual_type, actual_type_ref, actual_val, expected_type, param.type)
          arg_entries[idx] = {expected_type, coerced_val, actual_type_ref}
        end
      end

      args = arg_entries.map { |(t, v, _)| "#{t} #{v}" }.join(", ")

      if return_type == "void"
        emit "call void @#{mangled_extern_name}(#{args})"
        # Mark as void so value_ref returns a safe default for downstream uses
        @void_values << inst.id
      elsif fixed_sig
        sig_prefix = fixed_sig.join(", ")
        emit "#{name} = call #{return_type} (#{sig_prefix}, ...) @#{mangled_extern_name}(#{args})"
        record_emitted_type(name, return_type)
      else
        emit "#{name} = call #{return_type} @#{mangled_extern_name}(#{args})"
        record_emitted_type(name, return_type)
        # Track type for downstream use (if not already set by pattern matching above)
        unless @value_types.has_key?(inst.id)
          actual_type = case return_type
                        when "i1" then TypeRef::BOOL
                        when "i8" then TypeRef::INT8
                        when "i16" then TypeRef::INT16
                        when "i32" then TypeRef::INT32
                        when "i64" then TypeRef::INT64
                        when "i128" then TypeRef::INT128
                        when "float" then TypeRef::FLOAT32
                        when "double" then TypeRef::FLOAT64
                        when "ptr" then TypeRef::POINTER
                        else inst.type  # Fallback to MIR type
                        end
          @value_types[inst.id] = actual_type
        end
      end

      # If prepass detected this ExternCall needs zext/trunc for phi compatibility, emit it now
      if (conversion = @phi_zext_conversions[inst.id]?)
        from_bits, to_bits = conversion
        zext_name = "#{name}.zext"
        cast_op = from_bits < to_bits ? "zext" : "trunc"
        emit "#{zext_name} = #{cast_op} i#{from_bits} #{name} to i#{to_bits}"
        @zext_value_names[inst.id] = zext_name
      end

      # Track called function for forward declaration if missing at end of IR gen.
      # This catches ExternCall targets that exist in @module.functions (so NOT added
      # to @undefined_externs) but whose bodies were never emitted by RTA.
      extern_arg_types = inst.args.map do |a|
        at = @value_types[a]?
        at ? @type_mapper.llvm_type(at) : "ptr"
      end.reject { |t| t == "void" }
      @called_crystal_functions[mangled_extern_name] ||= {(return_type == "void" ? "ptr" : return_type), extern_arg_types.size, extern_arg_types}
    end

    private def emit_address_of(inst : AddressOf, name : String)
      # Get address of a value (pointerof)
      # For variables that already have stack allocas, return the alloca directly.
      # This ensures pointerof(x) returns the ACTUAL address of x, not a copy.

      # Case 1: Operand is already a stack alloca (from Alloc(Stack)) → return it directly
      if @emitted_allocas.includes?(inst.operand)
        operand_ref = value_ref(inst.operand)
        emit "#{name} = getelementptr i8, ptr #{operand_ref}, i64 0"
        @value_types[inst.id] = TypeRef::POINTER
        return
      end

      # Case 2: Hoisted addressable alloca exists (from pre-scan in emit_hoisted_allocas)
      if alloca_ref = @addressable_allocas[inst.operand]?
        # Store the operand value on first use (subsequent uses reuse same alloca)
        unless @addressable_alloca_initialized.includes?(inst.operand)
          operand_type = @value_types[inst.operand]? || TypeRef::POINTER
          llvm_type = @type_mapper.llvm_type(operand_type)
          operand_ref = value_ref(inst.operand)
          if llvm_type == "void"
            llvm_type = "ptr"
            operand_ref = "null"
          end
          store_val = (llvm_type == "ptr" && operand_ref == "0") ? "null" : operand_ref
          emit "store #{llvm_type} #{store_val}, ptr #{alloca_ref}"
          @addressable_alloca_initialized << inst.operand
        end
        emit "#{name} = getelementptr i8, ptr #{alloca_ref}, i64 0"
        @value_types[inst.id] = TypeRef::POINTER
        return
      end

      # Case 3: Fallback — create inline alloca (should rarely be hit with hoisting)
      operand_type = @value_types[inst.operand]? || TypeRef::POINTER
      llvm_type = @type_mapper.llvm_type(operand_type)
      operand_ref = value_ref(inst.operand)
      if llvm_type == "void"
        llvm_type = "ptr"
        operand_ref = "null"
      end
      alloca_ref = "#{name}.addr"
      emit "#{alloca_ref} = alloca #{llvm_type}"
      store_val = (llvm_type == "ptr" && operand_ref == "0") ? "null" : operand_ref
      emit "store #{llvm_type} #{store_val}, ptr #{alloca_ref}"
      emit "#{name} = getelementptr i8, ptr #{alloca_ref}, i64 0"
      @value_types[inst.id] = TypeRef::POINTER
    end

    private def emit_global_load(inst : GlobalLoad, name : String)
      llvm_type = @type_mapper.llvm_type(inst.type)
      mangled_global = @type_mapper.mangle_name(inst.global_name)
      # Use renamed global if it was renamed to avoid function name conflict
      actual_global = @global_name_mapping[mangled_global]? || mangled_global
      # Can't load void - use ptr instead
      if llvm_type == "void"
        llvm_type = "ptr"
        @value_types[inst.id] = TypeRef::POINTER
      else
        @value_types[inst.id] = inst.type
      end
      # If the global is declared as a union but we want to load as ptr,
      # GEP to the payload field (index 1) and load the pointer from there.
      decl_type = @global_declared_types[actual_global]? || @global_declared_types[mangled_global]?
      if decl_type && decl_type.includes?(".union") && llvm_type == "ptr"
        base_name = name.lstrip('%')
        emit "%#{base_name}.pay_ptr = getelementptr #{decl_type}, ptr @#{actual_global}, i32 0, i32 1"
        emit "#{name} = load ptr, ptr %#{base_name}.pay_ptr, align 4"
      else
        emit "#{name} = load #{llvm_type}, ptr @#{actual_global}"
      end
    end

    private def emit_global_store(inst : GlobalStore, name : String)
      # Get value type from the stored value
      val = value_ref(inst.value)
      llvm_type = @type_mapper.llvm_type(inst.type)

      # Get actual type of the value being stored
      val_type = @value_types[inst.value]?
      val_type_str = val_type ? @type_mapper.llvm_type(val_type) : nil
      actual_val_type = @emitted_value_types[val]? || val_type_str

      # Fallback: values loaded from cross-block slots are named like %rN.fromslot.*
      # Their real LLVM type is the slot type, not always @value_types[N].
      if val.starts_with?('%')
        if match = val.match(/^%r(\d+)\.fromslot/)
          slot_id = ValueId.new(match[1].to_i)
          if slot_type = @cross_block_slot_types[slot_id]?
            actual_val_type = slot_type
          end
        end
      end

      # Can't store void - use ptr instead (typically for nil assignment)
      if llvm_type == "void"
        llvm_type = "ptr"
        val = "null" if val == "null" || val.starts_with?("%r") # likely void value
      end

      # Early check: if the value is a union and the global is declared as the same
      # union type, upgrade llvm_type to match. This prevents the union→ptr extraction
      # below from destroying union values that should be stored whole (e.g., closure cells).
      if llvm_type == "ptr" && actual_val_type && actual_val_type.includes?(".union")
        mangled_check = @type_mapper.mangle_name(inst.global_name)
        actual_check = @global_name_mapping[mangled_check]? || mangled_check
        decl_check = @global_declared_types[actual_check]? || @global_declared_types[mangled_check]?
        if decl_check && decl_check.includes?(".union")
          # Global is declared as union — use the union type for the store
          llvm_type = decl_check
        end
      end

      # Handle type mismatches for union types
      if llvm_type.includes?(".union")
        if actual_val_type && actual_val_type.includes?(".union") && actual_val_type != llvm_type
          # Both are unions but different types - reinterpret through memory
          c = @cond_counter
          @cond_counter += 1
          emit "%gs_conv.#{c}.ptr = alloca #{actual_val_type}, align 8"
          emit "store #{actual_val_type} #{normalize_union_value(val, actual_val_type)}, ptr %gs_conv.#{c}.ptr"
          emit "%gs_conv.#{c}.result = load #{llvm_type}, ptr %gs_conv.#{c}.ptr"
          val = "%gs_conv.#{c}.result"
        elsif actual_val_type.nil? || !actual_val_type.includes?(".union")
          # Value is not a union (or type unknown) - use zeroinitializer for the union
          val = "zeroinitializer"
        end
        # Union stores cannot use scalar null/0 literals directly.
        val = normalize_union_value(val, llvm_type)
      end

      # Reconcile union→ptr: value is union-typed but global expects ptr — extract payload
      if llvm_type == "ptr" && actual_val_type && actual_val_type.includes?(".union") && val.starts_with?('%')
        c = @cond_counter
        @cond_counter += 1
        emit "%gs_u2p.#{c}.ptr = alloca #{actual_val_type}, align 8"
        emit "store #{actual_val_type} #{normalize_union_value(val, actual_val_type)}, ptr %gs_u2p.#{c}.ptr"
        emit "%gs_u2p.#{c}.pay = getelementptr #{actual_val_type}, ptr %gs_u2p.#{c}.ptr, i32 0, i32 1"
        emit "%gs_u2p.#{c}.val = load ptr, ptr %gs_u2p.#{c}.pay, align 4"
        val = "%gs_u2p.#{c}.val"
        actual_val_type = "ptr"
      end

      # Reconcile scalar pointer/integer mismatches using emitted SSA type.
      if actual_val_type && val.starts_with?('%')
        if llvm_type == "ptr" && actual_val_type.starts_with?('i') && !actual_val_type.includes?(".union")
          c = @cond_counter
          @cond_counter += 1
          emit "%global_inttoptr.#{c} = inttoptr #{actual_val_type} #{val} to ptr"
          val = "%global_inttoptr.#{c}"
        elsif llvm_type.starts_with?('i') && actual_val_type == "ptr"
          c = @cond_counter
          @cond_counter += 1
          emit "%global_ptrtoint.#{c} = ptrtoint ptr #{val} to #{llvm_type}"
          val = "%global_ptrtoint.#{c}"
        elsif llvm_type.starts_with?('i') && actual_val_type.starts_with?('i') &&
              !llvm_type.includes?('.') && !actual_val_type.includes?('.')
          from_bits = actual_val_type[1..].to_i? || 64
          to_bits = llvm_type[1..].to_i? || 64
          if from_bits < to_bits
            c = @cond_counter
            @cond_counter += 1
            emit "%global_iexpand.#{c} = sext #{actual_val_type} #{val} to #{llvm_type}"
            val = "%global_iexpand.#{c}"
          elsif from_bits > to_bits
            c = @cond_counter
            @cond_counter += 1
            emit "%global_itrunc.#{c} = trunc #{actual_val_type} #{val} to #{llvm_type}"
            val = "%global_itrunc.#{c}"
          end
        end
      end

      # For ptr type, convert integer constants to null or inttoptr
      if llvm_type == "ptr" && val.matches?(/^\d+$/)
        if val == "0"
          val = "null"
        else
          c = @cond_counter
          @cond_counter += 1
          emit "%global_inttoptr.#{c} = inttoptr i64 #{val} to ptr"
          val = "%global_inttoptr.#{c}"
        end
      end
      mangled_global = @type_mapper.mangle_name(inst.global_name)
      # Use renamed global if it was renamed to avoid function name conflict
      actual_global = @global_name_mapping[mangled_global]? || mangled_global

      # Reconcile store type with global's declared type to prevent type mismatches
      declared_type = @global_declared_types[actual_global]? || @global_declared_types[mangled_global]?
      if declared_type && declared_type != llvm_type
        if declared_type == "ptr" && llvm_type.includes?(".union")
          # Store type is union but global is ptr — extract pointer payload
          c = @cond_counter
          @cond_counter += 1
          emit "%gs_decl_u2p.#{c}.ptr = alloca #{llvm_type}, align 8"
          emit "store #{llvm_type} #{normalize_union_value(val, llvm_type)}, ptr %gs_decl_u2p.#{c}.ptr"
          emit "%gs_decl_u2p.#{c}.pay = getelementptr #{llvm_type}, ptr %gs_decl_u2p.#{c}.ptr, i32 0, i32 1"
          emit "%gs_decl_u2p.#{c}.val = load ptr, ptr %gs_decl_u2p.#{c}.pay, align 4"
          val = "%gs_decl_u2p.#{c}.val"
          llvm_type = "ptr"
        elsif declared_type.includes?(".union") && llvm_type == "ptr"
          # Store type is ptr but global is union — wrap ptr into union through memory
          if val.starts_with?('%')
            c = @cond_counter
            @cond_counter += 1
            emit "%gs_p2u.#{c}.ptr = alloca #{declared_type}, align 8"
            emit "store #{declared_type} zeroinitializer, ptr %gs_p2u.#{c}.ptr"
            # Store ptr into payload area (field 1 of union, after type_id)
            emit "%gs_p2u.#{c}.pay = getelementptr #{declared_type}, ptr %gs_p2u.#{c}.ptr, i32 0, i32 1"
            emit "store ptr #{val}, ptr %gs_p2u.#{c}.pay"
            emit "%gs_p2u.#{c}.val = load #{declared_type}, ptr %gs_p2u.#{c}.ptr"
            val = "%gs_p2u.#{c}.val"
          else
            val = normalize_union_value(val, declared_type)
          end
          llvm_type = declared_type
        elsif declared_type.includes?(".union") && llvm_type.includes?(".union")
          # Both unions but different — reinterpret through memory
          c = @cond_counter
          @cond_counter += 1
          emit "%gs_decl_u2u.#{c}.ptr = alloca #{llvm_type}, align 8"
          emit "store #{llvm_type} #{normalize_union_value(val, llvm_type)}, ptr %gs_decl_u2u.#{c}.ptr"
          emit "%gs_decl_u2u.#{c}.val = load #{declared_type}, ptr %gs_decl_u2u.#{c}.ptr"
          val = "%gs_decl_u2u.#{c}.val"
          llvm_type = declared_type
        end
      end

      emit "store #{llvm_type} #{val}, ptr @#{actual_global}"
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
      unless union_type.includes?(".union")
        # Guard: union wrap on a non-union LLVM type. Treat as a cast to the target type.
        if ENV.has_key?("DEBUG_UNION_WRAP")
          src_type = @value_types[inst.value]? || TypeRef::POINTER
          STDERR.puts "[UNION_WRAP] non-union target=#{union_type} src=#{@type_mapper.llvm_type(src_type)}"
        end
        emit_cast(Cast.new(inst.id, inst.union_type, CastKind::Bitcast, inst.value), name)
        return
      end

      # Guard: if value is already the same union type, just pass it through (avoid double-wrap).
      # This handles cases where MIR has redundant UnionWrap(UnionWrap(x)).
      val_type = @value_types[inst.value]? || TypeRef::POINTER
      val_type_str = @type_mapper.llvm_type(val_type)
      if val_type_str == union_type
        # Store through alloca to create a named value (LLVM can't alias aggregates)
        val = value_ref(inst.value)
        emit "%#{base_name}.passthru = alloca #{union_type}, align 8"
        emit "store #{union_type} #{normalize_union_value(val, union_type)}, ptr %#{base_name}.passthru"
        emit "#{name} = load #{union_type}, ptr %#{base_name}.passthru"
        return
      end
      emit "%#{base_name}.ptr = alloca #{union_type}, align 8"

      # Resolve value early so we can check for null before storing type_id
      val_type = @value_types[inst.value]? || TypeRef::POINTER
      val_type_str = @type_mapper.llvm_type(val_type)
      val = value_ref(inst.value)
      if emitted_val_type = @emitted_value_types[val]?
        # Trust the actually emitted LLVM type when it disagrees with MIR hints.
        # This avoids invalid stores where @value_types is stale (e.g. narrowed
        # union hint but value register still holds a wider union aggregate).
        val_type_str = emitted_val_type unless emitted_val_type == "void"
      end
      val = "null" if val_type_str == "ptr" && val == "0"

      # 2. Store type_id discriminator
      # When wrapping a ptr into a nilable union, the ptr might be null (Nil).
      # A prior union ABI extraction may have stripped the type_id, so we must
      # check for null at runtime and set the correct Nil variant type_id.
      emit "%#{base_name}.type_id_ptr = getelementptr #{union_type}, ptr %#{base_name}.ptr, i32 0, i32 0"
      nil_vid = nil_variant_id_for_union_type(union_type)
      if val_type_str == "ptr" && nil_vid != nil && inst.variant_type_id != nil_vid
        emit "%#{base_name}.is_null = icmp eq ptr #{val}, null"
        emit "%#{base_name}.type_id = select i1 %#{base_name}.is_null, i32 #{nil_vid}, i32 #{inst.variant_type_id}"
        emit "store i32 %#{base_name}.type_id, ptr %#{base_name}.type_id_ptr"
      else
        emit "store i32 #{inst.variant_type_id}, ptr %#{base_name}.type_id_ptr"
      end

      # 3. Store value in payload (skip for void/nil types - they have no payload)
      emit "%#{base_name}.payload_ptr = getelementptr #{union_type}, ptr %#{base_name}.ptr, i32 0, i32 1"
      if val_type_str == "void"
        emit "store i8 0, ptr %#{base_name}.payload_ptr, align 4"
      else
        # If the selected union variant is itself a union type, but the wrapped
        # value is a different union layout, normalize by bitcasting through a
        # temporary alloca so the store type matches the payload type exactly.
        expected_variant_llvm : String? = nil
        if union_desc = @module.get_union_descriptor(inst.union_type)
          if variant_desc = union_desc.variants.find { |v| v.type_id == inst.variant_type_id }
            expected_variant_llvm = @type_mapper.llvm_type(variant_desc.type_ref)
          end
        end
        if expected_union = expected_variant_llvm
          if expected_union.includes?(".union") && val_type_str.includes?(".union") && expected_union != val_type_str
            emit "%#{base_name}.payload_cast = alloca #{val_type_str}, align 8"
            emit "store #{val_type_str} #{val}, ptr %#{base_name}.payload_cast"
            emit "%#{base_name}.payload_cast_val = load #{expected_union}, ptr %#{base_name}.payload_cast"
            val = "%#{base_name}.payload_cast_val"
            val_type_str = expected_union
          end
        end

        # For tuple types (stack-allocated value types stored via ptr), copy data instead of storing pointer.
        # Only tuples need this — other structs are heap-allocated so their pointer IS the value.
        variant_is_value_type = false
        variant_size = 0_u64
        if val_type_str == "ptr" && val != "null"
          union_mir_type = @module.type_registry.get(inst.union_type)
          if union_mir_type && (uvariants = union_mir_type.variants)
            if inst.variant_type_id >= 0 && inst.variant_type_id < uvariants.size
              vtype = uvariants[inst.variant_type_id]
              if vtype.name.starts_with?("Tuple(") && vtype.is_value_type? && vtype.size > 0
                variant_is_value_type = true
                variant_size = vtype.size
              end
            end
          end
        end
        if variant_is_value_type
          emit "call void @llvm.memcpy.p0.p0.i64(ptr %#{base_name}.payload_ptr, ptr #{val}, i64 #{variant_size}, i1 false)"
        else
          emit "store #{val_type_str} #{val}, ptr %#{base_name}.payload_ptr, align 4"
        end
      end

      # 4. Load the completed union value from stack
      emit "#{name} = load #{union_type}, ptr %#{base_name}.ptr"
    end

    private def emit_union_unwrap(inst : UnionUnwrap, name : String)
      # Get payload from union, assuming type_id matches
      # Union may be passed by value - need to store to stack first
      union_val = value_ref(inst.union_value)
      def_union_type_ref = find_def_inst(inst.union_value).try(&.type)
      union_type_ref = if def_union_type_ref && @type_mapper.llvm_type(def_union_type_ref).includes?(".union")
                         def_union_type_ref
                       else
                         @value_types[inst.union_value]? || def_union_type_ref || TypeRef::POINTER
                       end
      static_union_type = @type_mapper.llvm_type(union_type_ref)
      slot_union_type = @cross_block_slot_types[inst.union_value]?
      emitted_union_type = @emitted_value_types[union_val]?
      def_union_type = def_union_type_ref ? @type_mapper.llvm_type(def_union_type_ref) : nil
      union_type = if emitted_union_type && emitted_union_type.includes?(".union")
                     emitted_union_type
                   elsif slot_union_type && slot_union_type.includes?(".union")
                     slot_union_type
                   elsif def_union_type && def_union_type.includes?(".union")
                     def_union_type
                   else
                     static_union_type
                   end
      result_type = @type_mapper.llvm_type(inst.type)
      base_name = name.lstrip('%')

      # value_ref can cast cross-block union values to ptr when @value_types was
      # polluted to POINTER. For UnionUnwrap we need the raw union struct payload.
      if union_type.includes?(".union")
        emitted_from_ref = @emitted_value_types[union_val]?
        unless emitted_from_ref && emitted_from_ref.includes?(".union")
          if slot_name = @cross_block_slots[inst.union_value]?
            c = @cond_counter
            @cond_counter += 1
            raw_union_val = "%#{base_name}.raw_union.#{c}"
            emit "#{raw_union_val} = load #{union_type}, ptr %#{slot_name}"
            record_emitted_type(raw_union_val, union_type)
            union_val = raw_union_val
          end
        end
      end

      # Void result type cannot be loaded - just emit a null ptr placeholder
      if result_type == "void"
        emit "#{name} = bitcast ptr null to ptr"
        @value_types[inst.id] = TypeRef::POINTER
        return
      end

      # Check if LLVM type is actually a union struct (not just ptr)
      if union_type.includes?(".union")
        # Store union value to stack to get pointer for GEP
        emit "%#{base_name}.union_ptr = alloca #{union_type}, align 8"
        # Prefer the actual emitted union type for the value. If it differs from the
        # expected union_type, reinterpret through memory before storing.
        actual_union_val_type = @emitted_value_types[union_val]? || union_type
        store_val = if actual_union_val_type.includes?(".union") && actual_union_val_type != union_type
                      emit "%#{base_name}.union_conv_ptr = alloca #{actual_union_val_type}, align 8"
                      emit "store #{actual_union_val_type} #{normalize_union_value(union_val, actual_union_val_type)}, ptr %#{base_name}.union_conv_ptr"
                      emit "%#{base_name}.union_conv = load #{union_type}, ptr %#{base_name}.union_conv_ptr"
                      "%#{base_name}.union_conv"
                    else
                      # Use zeroinitializer for integer literal 0 with struct types
                      (union_val == "0" || union_val == "null") ? "zeroinitializer" : union_val
                    end
        emit "store #{union_type} #{store_val}, ptr %#{base_name}.union_ptr"

        # Check if variant is a tuple type (stack-allocated value type) that needs memcpy from payload
        unwrap_vt = false
        unwrap_vt_size = 0_u64
        if result_type == "ptr"
          union_mir = @module.type_registry.get(union_type_ref)
          if union_mir && (uvars = union_mir.variants)
            if inst.variant_type_id >= 0 && inst.variant_type_id < uvars.size
              vtype = uvars[inst.variant_type_id]
              if vtype.name.starts_with?("Tuple(") && vtype.is_value_type? && vtype.size > 0
                unwrap_vt = true
                unwrap_vt_size = vtype.size
              end
            end
          end
        end

        if inst.safe
          # Safe unwrap: check type_id first, return null/zero on mismatch
          emit "%#{base_name}.type_id_ptr = getelementptr #{union_type}, ptr %#{base_name}.union_ptr, i32 0, i32 0"
          emit "%#{base_name}.actual_type_id = load i32, ptr %#{base_name}.type_id_ptr"
          emit "%#{base_name}.type_match = icmp eq i32 %#{base_name}.actual_type_id, #{inst.variant_type_id}"
          emit "%#{base_name}.payload_ptr = getelementptr #{union_type}, ptr %#{base_name}.union_ptr, i32 0, i32 1"
          if unwrap_vt
            # Value type: alloca + memcpy from payload
            emit "%#{base_name}.vt_alloca = alloca i8, i64 #{unwrap_vt_size}, align 8"
            emit "call void @llvm.memcpy.p0.p0.i64(ptr %#{base_name}.vt_alloca, ptr %#{base_name}.payload_ptr, i64 #{unwrap_vt_size}, i1 false)"
            emit "#{name} = select i1 %#{base_name}.type_match, ptr %#{base_name}.vt_alloca, ptr null"
          else
            emit "%#{base_name}.payload = load #{result_type}, ptr %#{base_name}.payload_ptr, align 4"
            # Select null/zero if type doesn't match
            emit "#{name} = select i1 %#{base_name}.type_match, #{result_type} %#{base_name}.payload, #{result_type} zeroinitializer"
          end
        else
          # Unsafe unwrap: just load payload (UB if type_id doesn't match)
          emit "%#{base_name}.payload_ptr = getelementptr #{union_type}, ptr %#{base_name}.union_ptr, i32 0, i32 1"
          if unwrap_vt
            # Value type: alloca + memcpy from payload
            emit "#{name} = alloca i8, i64 #{unwrap_vt_size}, align 8"
            emit "call void @llvm.memcpy.p0.p0.i64(ptr #{name}, ptr %#{base_name}.payload_ptr, i64 #{unwrap_vt_size}, i1 false)"
          else
            emit "#{name} = load #{result_type}, ptr %#{base_name}.payload_ptr, align 4"
          end
        end
      else
        # Not a union struct - just use the value directly
        actual_union_val_type = @emitted_value_types[union_val]? || union_type
        # Check if union_val is an integer literal (type mismatch from defaulting to POINTER)
        is_int_literal = union_val.match(/^-?\d+$/) && union_val != "null"

        if is_int_literal
          # union_val is an int literal, use it directly for int result types
          if result_type.starts_with?('i')
            emit "#{name} = add #{result_type} #{union_val}, 0"
          elsif result_type == "ptr"
            emit "#{name} = inttoptr i64 #{union_val} to ptr"
          else
            emit "#{name} = add i64 #{union_val}, 0"
          end
        elsif result_type == "ptr"
          if actual_union_val_type == "ptr"
            emit "#{name} = bitcast ptr #{union_val} to ptr"
          elsif actual_union_val_type.starts_with?('i') && !actual_union_val_type.includes?(".union")
            emit "#{name} = inttoptr #{actual_union_val_type} #{union_val} to ptr"
          elsif actual_union_val_type == "double"
            emit "%#{base_name}.bits = bitcast double #{union_val} to i64"
            emit "#{name} = inttoptr i64 %#{base_name}.bits to ptr"
          elsif actual_union_val_type == "float"
            emit "%#{base_name}.bits = bitcast float #{union_val} to i32"
            emit "%#{base_name}.bits.ext = zext i32 %#{base_name}.bits to i64"
            emit "#{name} = inttoptr i64 %#{base_name}.bits.ext to ptr"
          else
            emit "#{name} = bitcast ptr #{union_val} to ptr"
          end
        elsif result_type.starts_with?('i')
          if actual_union_val_type == "ptr"
            emit "#{name} = ptrtoint ptr #{union_val} to #{result_type}"
          elsif actual_union_val_type.starts_with?('i') && !actual_union_val_type.includes?(".union")
            if actual_union_val_type == result_type
              emit "#{name} = add #{result_type} #{union_val}, 0"
            else
              src_bits = actual_union_val_type[1..].to_i?
              dst_bits = result_type[1..].to_i?
              if src_bits && dst_bits
                if dst_bits < src_bits
                  emit "#{name} = trunc #{actual_union_val_type} #{union_val} to #{result_type}"
                elsif dst_bits > src_bits
                  ext_op = unsigned_type_ref?(inst.type) ? "zext" : "sext"
                  emit "#{name} = #{ext_op} #{actual_union_val_type} #{union_val} to #{result_type}"
                else
                  emit "#{name} = add #{result_type} #{union_val}, 0"
                end
              else
                emit "#{name} = add #{result_type} 0, 0"
              end
            end
          else
            emit "#{name} = add #{result_type} 0, 0"
          end
        elsif result_type == actual_union_val_type
          if result_type == "float"
            emit "#{name} = fadd float #{union_val}, 0.0"
          elsif result_type == "double"
            emit "#{name} = fadd double #{union_val}, 0.0"
          else
            emit "#{name} = bitcast ptr #{union_val} to ptr"
          end
        else
          # Fallback: bitcast
          emit "#{name} = bitcast ptr #{union_val} to ptr"
        end
      end

      record_emitted_type(name, result_type)
      @value_types[inst.id] = inst.type
    end

    private def emit_union_type_id_get(inst : UnionTypeIdGet, name : String)
      # Load type_id from union
      # Union may be passed by value - need to store to stack first
      union_val = value_ref(inst.union_value)
      def_union_type_ref = find_def_inst(inst.union_value).try(&.type)
      union_type_ref = if def_union_type_ref && @type_mapper.llvm_type(def_union_type_ref).includes?(".union")
                         def_union_type_ref
                       else
                         @value_types[inst.union_value]? || def_union_type_ref || TypeRef::POINTER
                       end
      static_union_type = @type_mapper.llvm_type(union_type_ref)
      slot_union_type = @cross_block_slot_types[inst.union_value]?
      emitted_union_type = @emitted_value_types[union_val]?
      def_union_type = def_union_type_ref ? @type_mapper.llvm_type(def_union_type_ref) : nil
      union_type = if emitted_union_type && emitted_union_type.includes?(".union")
                     emitted_union_type
                   elsif slot_union_type && slot_union_type.includes?(".union")
                     slot_union_type
                   elsif def_union_type && def_union_type.includes?(".union")
                     def_union_type
                   else
                     static_union_type
                   end
      base_name = name.lstrip('%')

      # value_ref can degrade a cross-block union value to ptr via fromslot casts.
      # For union type_id checks we need the raw union struct value.
      if union_type.includes?(".union")
        emitted_from_ref = @emitted_value_types[union_val]?
        unless emitted_from_ref && emitted_from_ref.includes?(".union")
          if slot_name = @cross_block_slots[inst.union_value]?
            c = @cond_counter
            @cond_counter += 1
            raw_union_val = "%#{base_name}.raw_union.#{c}"
            emit "#{raw_union_val} = load #{union_type}, ptr %#{slot_name}"
            record_emitted_type(raw_union_val, union_type)
            union_val = raw_union_val
          end
        end
      end

      # Check if LLVM type is actually a union struct (not just ptr)
      if union_type.includes?(".union")
        # Store union value to stack to get pointer for GEP
        emit "%#{base_name}.union_ptr = alloca #{union_type}, align 8"
        store_val = coerce_union_value_for_type("#{base_name}.typeid", union_val, union_type)
        emit "store #{union_type} #{store_val}, ptr %#{base_name}.union_ptr"

        emit "%#{base_name}.type_id_ptr = getelementptr #{union_type}, ptr %#{base_name}.union_ptr, i32 0, i32 0"
        emit "#{name} = load i32, ptr %#{base_name}.type_id_ptr"
      else
        # Not a union struct - determine type_id from ptr null check
        # type_id 0 = nil, type_id 1+ = non-nil
        # Handle case where union_val is an integer literal
        if def_inst = find_def_inst(inst.union_value)
          if def_inst.type == TypeRef::BOOL || @type_mapper.llvm_type(def_inst.type) == "i1"
            emit "#{name} = add i32 0, 0"
            return
          end
        end
        if union_type_ref == TypeRef::BOOL ||
           union_type_ref == TypeRef::INT8 || union_type_ref == TypeRef::UINT8 ||
           union_type_ref == TypeRef::INT16 || union_type_ref == TypeRef::UINT16 ||
           union_type_ref == TypeRef::INT32 || union_type_ref == TypeRef::UINT32 ||
           union_type_ref == TypeRef::INT64 || union_type_ref == TypeRef::UINT64 ||
           union_type_ref == TypeRef::INT128 || union_type_ref == TypeRef::UINT128 ||
           union_type_ref == TypeRef::FLOAT32 || union_type_ref == TypeRef::FLOAT64 ||
           union_type_ref == TypeRef::CHAR || union_type_ref == TypeRef::SYMBOL
          emit "#{name} = add i32 0, 0"
          return
        end
        ptr_val = union_val
        if union_val =~ /^\d+$/ || union_val == "null"
          if union_val == "0" || union_val == "null"
            ptr_val = "null"
          else
            emit "%#{base_name}.inttoptr = inttoptr i64 #{union_val} to ptr"
            ptr_val = "%#{base_name}.inttoptr"
          end
        end
        emit "%#{base_name}.is_null = icmp eq ptr #{ptr_val}, null"
        emit "#{name} = select i1 %#{base_name}.is_null, i32 0, i32 1"
      end
    end

    private def emit_union_is(inst : UnionIs, name : String)
      # Check if union is specific variant
      # Union may be passed by value (from load) - need to store to stack first
      union_val = value_ref(inst.union_value)
      def_union_type_ref = find_def_inst(inst.union_value).try(&.type)
      union_type_ref = if def_union_type_ref && @type_mapper.llvm_type(def_union_type_ref).includes?(".union")
                         def_union_type_ref
                       else
                         @value_types[inst.union_value]? || def_union_type_ref || TypeRef::POINTER
                       end
      static_union_type = @type_mapper.llvm_type(union_type_ref)
      slot_union_type = @cross_block_slot_types[inst.union_value]?
      emitted_union_type = @emitted_value_types[union_val]?
      def_union_type = def_union_type_ref ? @type_mapper.llvm_type(def_union_type_ref) : nil
      union_type = if emitted_union_type && emitted_union_type.includes?(".union")
                     emitted_union_type
                   elsif slot_union_type && slot_union_type.includes?(".union")
                     slot_union_type
                   elsif def_union_type && def_union_type.includes?(".union")
                     def_union_type
                   else
                     static_union_type
                   end
      base_name = name.lstrip('%')

      # value_ref can degrade a cross-block union value to ptr via fromslot casts.
      # For union variant checks we need the raw union struct value.
      if union_type.includes?(".union")
        emitted_from_ref = @emitted_value_types[union_val]?
        unless emitted_from_ref && emitted_from_ref.includes?(".union")
          if slot_name = @cross_block_slots[inst.union_value]?
            c = @cond_counter
            @cond_counter += 1
            raw_union_val = "%#{base_name}.raw_union.#{c}"
            emit "#{raw_union_val} = load #{union_type}, ptr %#{slot_name}"
            record_emitted_type(raw_union_val, union_type)
            union_val = raw_union_val
          end
        end
      end

      # Check if LLVM type is actually a union struct (not just ptr)
      if union_type.includes?(".union")
        # Store union value to stack to get pointer for GEP
        emit "%#{base_name}.union_ptr = alloca #{union_type}, align 8"
        store_val = coerce_union_value_for_type("#{base_name}.is", union_val, union_type)
        emit "store #{union_type} #{store_val}, ptr %#{base_name}.union_ptr"

        # Get type_id from stored union
        emit "%#{base_name}.type_id_ptr = getelementptr #{union_type}, ptr %#{base_name}.union_ptr, i32 0, i32 0"
        emit "%#{base_name}.actual_type_id = load i32, ptr %#{base_name}.type_id_ptr"
        emit "#{name} = icmp eq i32 %#{base_name}.actual_type_id, #{inst.variant_type_id}"
      else
        # Not a union struct - compare pointer against null
        # variant_type_id 0 = nil, 1+ = non-nil (matches union variant table)
        # Handle case where union_val is an integer literal (convert to ptr first)
        if def_inst = find_def_inst(inst.union_value)
          if def_inst.type == TypeRef::BOOL || @type_mapper.llvm_type(def_inst.type) == "i1"
            const_val = inst.variant_type_id == 0 ? "0" : "1"
            emit "#{name} = add i1 0, #{const_val}"
            return
          end
        end
        if union_type_ref == TypeRef::BOOL ||
           union_type_ref == TypeRef::INT8 || union_type_ref == TypeRef::UINT8 ||
           union_type_ref == TypeRef::INT16 || union_type_ref == TypeRef::UINT16 ||
           union_type_ref == TypeRef::INT32 || union_type_ref == TypeRef::UINT32 ||
           union_type_ref == TypeRef::INT64 || union_type_ref == TypeRef::UINT64 ||
           union_type_ref == TypeRef::INT128 || union_type_ref == TypeRef::UINT128 ||
           union_type_ref == TypeRef::FLOAT32 || union_type_ref == TypeRef::FLOAT64 ||
           union_type_ref == TypeRef::CHAR || union_type_ref == TypeRef::SYMBOL
          const_val = inst.variant_type_id == 0 ? "0" : "1"
          emit "#{name} = add i1 0, #{const_val}"
          return
        end
        ptr_val = union_val
        if union_val =~ /^\d+$/ || union_val == "null"
          # Integer literal or null - convert to ptr for comparison
          if union_val == "0" || union_val == "null"
            ptr_val = "null"
          else
            # Non-zero integer literal - convert to ptr via inttoptr
            emit "%#{base_name}.inttoptr = inttoptr i64 #{union_val} to ptr"
            ptr_val = "%#{base_name}.inttoptr"
          end
        end
        if inst.variant_type_id == 0
          # Checking if value IS variant 0 (nil): ptr == null
          emit "#{name} = icmp eq ptr #{ptr_val}, null"
        else
          # Checking if value IS a non-nil variant: ptr != null
          emit "#{name} = icmp ne ptr #{ptr_val}, null"
        end
      end
    end

    # ─────────────────────────────────────────────────────────────────────────
    # Array Operations
    # ─────────────────────────────────────────────────────────────────────────

    # Resolve runtime type_id for Array(T) from the registered MIR type table.
    # Returns 0 when the specialized Array(T) type is not available.
    private def array_runtime_type_id_for_element(element_type_ref : TypeRef) : Int32
      if elem_type = @module.type_registry.get(element_type_ref)
        array_name = String.build(elem_type.name.bytesize + 7) do |io|
          io << "Array("
          io << elem_type.name
          io << ')'
        end
        if array_type = @module.type_registry.get_by_name(array_name)
          return array_type.id.to_i32
        end
        if ENV["DEBUG_ARRAY_TID"]?
          STDERR.puts "[ARRAY_TID] miss array_name=#{array_name} elem_ref=#{element_type_ref.id} elem=#{elem_type.name}"
        end
      elsif ENV["DEBUG_ARRAY_TID"]?
        STDERR.puts "[ARRAY_TID] no_elem_type elem_ref=#{element_type_ref.id}"
      end
      0_i32
    end

    private def emit_array_literal(inst : ArrayLiteral, name : String)
      base_name = name.lstrip('%')
      element_type = @type_mapper.llvm_type(inst.element_type)
      # Void is not valid for array elements - use ptr instead
      element_type = "ptr" if element_type == "void"
      size = inst.size
      array_type_id = array_runtime_type_id_for_element(inst.element_type)

      # For empty array literals (size 0), try to call Array(T).new(0) constructor
      # to get a proper heap-allocated Array object. This is critical when the
      # array escapes (e.g. stored in classvars) since stack alloca would be invalid.
      if size == 0
        # Construct the Array(T).new(Int32) constructor name from element type
        elem_type_obj = @module.type_registry.get(inst.element_type)
        elem_name = elem_type_obj.try(&.name)
        if elem_name && !elem_name.empty? && elem_name != "Unknown" && elem_name != "Void"
          array_class_name = "Array(#{elem_name})"
          ctor_name = @type_mapper.mangle_name(array_class_name + ".new") + "$$Int32"
          # Check if this constructor exists in the module
          ctor_func = @module.functions.find { |f| mangle_function_name(f.name) == ctor_name }
          if ctor_func
            emit "#{name} = call ptr @#{ctor_name}(i32 0)"
            @called_crystal_functions[ctor_name] ||= {"ptr", 1, ["i32"] of String}
            # Some constructor paths still leave array type_id as 0; patch it here
            # so module-method vdispatch on Indexable works for empty arrays.
            if array_type_id != 0
              emit "%#{base_name}.tid_fix_ptr = getelementptr i8, ptr #{name}, i32 0"
              emit "store i32 #{array_type_id}, ptr %#{base_name}.tid_fix_ptr"
            end
            @array_info[inst.id] = {element_type, size}
            return
          end
        end
      end

      # Create proper Array object matching Crystal class layout:
      # { type_id: i32, @size: i32, @capacity: i32, @offset_factor: i32, @buffer: ptr }
      # Byte offsets: 0, 4, 8, 12, 16. Total: 24 bytes.
      # Buffer is heap-allocated so resize/push works correctly.

      # Compute element size in bytes for buffer allocation
      elem_byte_size = case element_type
                       when "i1", "i8"  then 1
                       when "i16"       then 2
                       when "i32"       then 4
                       when "i64"       then 8
                       when "i128"      then 16
                       when "float"     then 4
                       when "double"    then 8
                       when "ptr"       then 8
                       else
                         if element_type.includes?(".union")
                           # Union types: {i32, [N x i8]} — get size from type definition
                           union_type_info = @module.type_registry.get(inst.element_type)
                           union_type_info.try(&.size) || 16
                         else
                           8 # default to pointer size
                         end
                       end

      capacity = size < 4 ? 4 : size  # minimum capacity like Crystal's Array

      # Allocate Array object (24 bytes) based on memory strategy.
      # Stack = alloca (fast, auto-freed), anything else = heap (survives function).
      if inst.strategy == MIR::MemoryStrategy::Stack
        emit "%#{base_name}.ptr = alloca { i32, i32, i32, i32, ptr }, align 8"
      else
        emit "%#{base_name}.ptr = call ptr @__crystal_v2_malloc64(i64 24)"
      end

      # Store type_id at offset 0
      emit "%#{base_name}.tid_ptr = getelementptr i8, ptr %#{base_name}.ptr, i32 0"
      emit "store i32 #{array_type_id}, ptr %#{base_name}.tid_ptr"

      # Store @size at offset 4
      emit "%#{base_name}.size_ptr = getelementptr i8, ptr %#{base_name}.ptr, i32 4"
      emit "store i32 #{size}, ptr %#{base_name}.size_ptr"

      # Store @capacity at offset 8
      emit "%#{base_name}.cap_ptr = getelementptr i8, ptr %#{base_name}.ptr, i32 8"
      emit "store i32 #{capacity}, ptr %#{base_name}.cap_ptr"

      # Store @offset_factor at offset 12
      emit "%#{base_name}.off_ptr = getelementptr i8, ptr %#{base_name}.ptr, i32 12"
      emit "store i32 0, ptr %#{base_name}.off_ptr"

      # Heap-allocate buffer (capacity * elem_size bytes, zero-initialized via calloc)
      buffer_bytes = capacity * elem_byte_size
      emit "%#{base_name}.buf = call ptr @__crystal_v2_malloc64(i64 #{buffer_bytes})"

      # Store @buffer at offset 16
      emit "%#{base_name}.buf_field_ptr = getelementptr i8, ptr %#{base_name}.ptr, i32 16"
      emit "store ptr %#{base_name}.buf, ptr %#{base_name}.buf_field_ptr"

      # Store elements into the heap buffer
      original_element_type = @type_mapper.llvm_type(inst.element_type)
      # If element type is void, try to infer from actual element values.
      # This handles cases like [Zone::UTC] where the constant reference type
      # is not resolved during type inference but the actual value is a ptr.
      if original_element_type == "void" && inst.elements.size > 0
        inst.elements.each do |eid|
          if et = @value_types[eid]?
            inferred = @type_mapper.llvm_type(et)
            if inferred != "void"
              original_element_type = inferred
              break
            end
          end
        end
        # Fallback: if all elements are still void, use ptr (class instances are ptrs)
        original_element_type = "ptr" if original_element_type == "void"
      end
      inst.elements.each_with_index do |elem_id, idx|
        emit "%#{base_name}.elem#{idx}_ptr = getelementptr #{element_type}, ptr %#{base_name}.buf, i32 #{idx}"
        # If original element was void, store null; otherwise store actual value
        if original_element_type == "void"
          emit "store ptr null, ptr %#{base_name}.elem#{idx}_ptr"
        else
          elem_val = value_ref(elem_id)
          # Check actual element value type and convert if needed
          actual_elem_type = @value_types[elem_id]?
          actual_elem_type_str = actual_elem_type ? @type_mapper.llvm_type(actual_elem_type) : nil
          if actual_elem_type_str && actual_elem_type_str != element_type
            # Type mismatch - need conversion
            is_elem_int = element_type.starts_with?('i') && !element_type.includes?(".union")
            is_actual_ptr = actual_elem_type_str == "ptr"
            is_elem_union = element_type.includes?(".union")
            if is_elem_union && is_actual_ptr
              # ptr → union: wrap ptr in union with type_id=0 (non-nil)
              emit "%#{base_name}.elem#{idx}_union_ptr = alloca #{element_type}, align 8"
              emit "%#{base_name}.elem#{idx}_type_id_ptr = getelementptr #{element_type}, ptr %#{base_name}.elem#{idx}_union_ptr, i32 0, i32 0"
              emit "store i32 0, ptr %#{base_name}.elem#{idx}_type_id_ptr"
              emit "%#{base_name}.elem#{idx}_payload_ptr = getelementptr #{element_type}, ptr %#{base_name}.elem#{idx}_union_ptr, i32 0, i32 1"
              emit "store ptr #{elem_val}, ptr %#{base_name}.elem#{idx}_payload_ptr, align 4"
              emit "%#{base_name}.elem#{idx}_union = load #{element_type}, ptr %#{base_name}.elem#{idx}_union_ptr"
              elem_val = "%#{base_name}.elem#{idx}_union"
            elsif is_elem_int && is_actual_ptr
              # ptr → int: use ptrtoint
              emit "%#{base_name}.elem#{idx}_conv = ptrtoint ptr #{elem_val} to #{element_type}"
              elem_val = "%#{base_name}.elem#{idx}_conv"
            elsif element_type == "ptr" && actual_elem_type_str.includes?(".union")
              # union → ptr: extract pointer from union payload
              emit "%#{base_name}.elem#{idx}_union_ptr = alloca #{actual_elem_type_str}, align 8"
              emit "store #{actual_elem_type_str} #{normalize_union_value(elem_val, actual_elem_type_str)}, ptr %#{base_name}.elem#{idx}_union_ptr"
              emit "%#{base_name}.elem#{idx}_payload_ptr = getelementptr #{actual_elem_type_str}, ptr %#{base_name}.elem#{idx}_union_ptr, i32 0, i32 1"
              emit "%#{base_name}.elem#{idx}_val_ptr = load ptr, ptr %#{base_name}.elem#{idx}_payload_ptr, align 4"
              elem_val = "%#{base_name}.elem#{idx}_val_ptr"
            elsif element_type == "ptr" && actual_elem_type_str.starts_with?('i') && !actual_elem_type_str.includes?(".union")
              # int → ptr: use inttoptr, but convert 0 to null directly
              if elem_val == "0"
                elem_val = "null"
              else
                emit "%#{base_name}.elem#{idx}_conv = inttoptr #{actual_elem_type_str} #{elem_val} to ptr"
                elem_val = "%#{base_name}.elem#{idx}_conv"
              end
            elsif is_elem_union && actual_elem_type_str.includes?(".union")
              # union → union (different types, same physical layout): reinterpret through memory
              emit "%#{base_name}.elem#{idx}_u2u_ptr = alloca #{actual_elem_type_str}, align 8"
              emit "store #{actual_elem_type_str} #{normalize_union_value(elem_val, actual_elem_type_str)}, ptr %#{base_name}.elem#{idx}_u2u_ptr"
              emit "%#{base_name}.elem#{idx}_u2u = load #{element_type}, ptr %#{base_name}.elem#{idx}_u2u_ptr"
              elem_val = "%#{base_name}.elem#{idx}_u2u"
            elsif is_elem_int && actual_elem_type_str.includes?(".union")
              # union → int: extract scalar payload from union
              emit "%#{base_name}.elem#{idx}_u2i_ptr = alloca #{actual_elem_type_str}, align 8"
              emit "store #{actual_elem_type_str} #{normalize_union_value(elem_val, actual_elem_type_str)}, ptr %#{base_name}.elem#{idx}_u2i_ptr"
              emit "%#{base_name}.elem#{idx}_pay_ptr = getelementptr #{actual_elem_type_str}, ptr %#{base_name}.elem#{idx}_u2i_ptr, i32 0, i32 1"
              emit "%#{base_name}.elem#{idx}_u2i = load #{element_type}, ptr %#{base_name}.elem#{idx}_pay_ptr, align 4"
              elem_val = "%#{base_name}.elem#{idx}_u2i"
            end
          end
          # For ptr type, convert 0 to null
          if element_type == "ptr" && elem_val == "0"
            elem_val = "null"
          end
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
      if array_value_type == TypeRef::POINTER
        if alloc_elem = @alloc_element_types[inst.array_value]?
          array_value_type = alloc_elem
        end
      end
      if array_value_type
        array_llvm_type = @type_mapper.llvm_type(array_value_type)
        actual_val_type = lookup_value_llvm_type(inst.array_value, "")
        if array_llvm_type.includes?(".union")
          # Extract pointer from union payload
          emit "%#{base_name}.union_ptr = alloca #{array_llvm_type}, align 8"
          union_val = array_ptr
          actual_val_type = lookup_value_llvm_type(inst.array_value, "")
          if actual_val_type == "ptr"
            emit "%#{base_name}.union_val = load #{array_llvm_type}, ptr #{array_ptr}"
            union_val = "%#{base_name}.union_val"
          end
          emit "store #{array_llvm_type} #{normalize_union_value(union_val, array_llvm_type)}, ptr %#{base_name}.union_ptr"
          emit "%#{base_name}.payload_ptr = getelementptr #{array_llvm_type}, ptr %#{base_name}.union_ptr, i32 0, i32 1"
          emit "%#{base_name}.arr_ptr = load ptr, ptr %#{base_name}.payload_ptr, align 4"
          array_ptr = "%#{base_name}.arr_ptr"
        elsif array_llvm_type != "ptr" && !array_llvm_type.starts_with?('%') && !array_llvm_type.starts_with?('[') && actual_val_type != "ptr"
          # Non-ptr, non-struct type (e.g., i1, i32) - this is a MIR type inference issue
          # Use inttoptr conversion as fallback
          if array_llvm_type == "i1" || array_llvm_type == "i8" || array_llvm_type == "i16" || array_llvm_type == "i32"
            emit "%#{base_name}.ext = zext #{array_llvm_type} #{array_ptr} to i64"
            emit "%#{base_name}.arr_ptr = inttoptr i64 %#{base_name}.ext to ptr"
          elsif array_llvm_type == "i64"
            emit "%#{base_name}.arr_ptr = inttoptr i64 #{array_ptr} to ptr"
          else
            # Unknown type - use null as safest fallback
            emit "%#{base_name}.arr_ptr = inttoptr i64 0 to ptr"
          end
          array_ptr = "%#{base_name}.arr_ptr"
        end
      end

      # Check if this is a Tuple — tuples store elements inline, no @size field.
      # Return compile-time element count instead of reading from memory.
      # IMPORTANT: for values produced by ArrayLiteral/ArrayNew we must keep
      # array object layout semantics even if type inference is imprecise.
      if array_value_type && !@array_info.has_key?(inst.array_value)
        tuple_type_ref = array_value_type
        if array_value_type == TypeRef::POINTER
          if alloc_elem = @alloc_element_types[inst.array_value]?
            tuple_type_ref = alloc_elem
          end
        end
        if union_desc = @module.get_union_descriptor(tuple_type_ref)
          if tuple_variant = union_desc.variants.find { |v| (t = @module.type_registry.get(v.type_ref)) && t.kind.tuple? }
            tuple_type_ref = tuple_variant.type_ref
          end
        end
        if tuple_type = @module.type_registry.get(tuple_type_ref)
          if tuple_type.kind.tuple?
            element_count = tuple_type.element_types.try(&.size) || 0
            emit "#{name} = add i32 0, #{element_count}"
            @value_types[inst.id] = TypeRef::INT32
            return
          end
        end
        # Also handle bare Tuple struct
        bare_type = @module.type_registry.get(array_value_type)
        if bare_type && bare_type.name == "Tuple" && bare_type.kind.struct?
          element_count = bare_type.element_types.try(&.size) || 0
          emit "#{name} = add i32 0, #{element_count}"
          @value_types[inst.id] = TypeRef::INT32
          return
        end
      end

      # Get size from array struct field 1 (byte offset 4), matching Crystal class layout
      emit "%#{base_name}.size_ptr = getelementptr { i32, i32, [0 x i32] }, ptr #{array_ptr}, i32 0, i32 1"
      emit "#{name} = load i32, ptr %#{base_name}.size_ptr"

      # Update @value_types - size is always i32
      @value_types[inst.id] = TypeRef::INT32
    end

    private def emit_array_set_size(inst : ArraySetSize, name : String)
      base_name = name.lstrip('%')
      array_ptr = value_ref(inst.array_value)
      size_val = value_ref(inst.size_value)

      # Store new size to array struct field 1 (byte offset 4)
      emit "%#{base_name}.size_ptr = getelementptr { i32, i32, [0 x i32] }, ptr #{array_ptr}, i32 0, i32 1"
      emit "store i32 #{size_val}, ptr %#{base_name}.size_ptr"
    end

    private def emit_array_new(inst : ArrayNew, name : String)
      base_name = name.lstrip('%')
      capacity_val = value_ref(inst.capacity_value)
      element_type = @type_mapper.llvm_type(inst.element_type_ref)
      element_type = "ptr" if element_type == "void"
      array_type_id = array_runtime_type_id_for_element(inst.element_type_ref)
      elem_size = case element_type
                  when "i1", "i8"   then 1
                  when "i16"        then 2
                  when "i32", "float" then 4
                  when "i64", "double", "ptr" then 8
                  when "i128"       then 16
                  else                   8 # default for complex types
                  end

      # Allocate proper Crystal Array object (24 bytes):
      #   offset 0:  type_id (i32), offset 4: @size (i32), offset 8: @capacity (i32),
      #   offset 12: @offset_to_buffer (i32), offset 16: @buffer (ptr)
      emit "%#{base_name}.arr = call ptr @__crystal_v2_malloc64(i64 24)"

      # Set runtime type_id for Array(T) when known.
      emit "store i32 #{array_type_id}, ptr %#{base_name}.arr"

      # Set @size = 0 at offset 4
      emit "%#{base_name}.size_ptr = getelementptr i8, ptr %#{base_name}.arr, i32 4"
      emit "store i32 0, ptr %#{base_name}.size_ptr"

      # Set @capacity at offset 8
      emit "%#{base_name}.cap_ptr = getelementptr i8, ptr %#{base_name}.arr, i32 8"
      emit "store i32 #{capacity_val}, ptr %#{base_name}.cap_ptr"

      # Set @offset_to_buffer = 0 at offset 12
      emit "%#{base_name}.otb_ptr = getelementptr i8, ptr %#{base_name}.arr, i32 12"
      emit "store i32 0, ptr %#{base_name}.otb_ptr"

      # Allocate buffer for elements
      emit "%#{base_name}.elem_bytes = mul i32 #{capacity_val}, #{elem_size}"
      emit "%#{base_name}.buf_size = sext i32 %#{base_name}.elem_bytes to i64"
      emit "%#{base_name}.buf = call ptr @__crystal_v2_malloc64(i64 %#{base_name}.buf_size)"

      # Set @buffer at offset 16
      emit "%#{base_name}.buf_addr = getelementptr i8, ptr %#{base_name}.arr, i32 16"
      emit "store ptr %#{base_name}.buf, ptr %#{base_name}.buf_addr"

      # Alias so %name resolves to the Array pointer
      emit "#{name} = bitcast ptr %#{base_name}.arr to ptr"

      # The emitted value is always ptr — ensure type tracking reflects this
      # so that cross-block slot stores do proper union wrapping if needed
      @value_types[inst.id] = TypeRef::POINTER

      # Register as array for IndexGet/IndexSet/ArraySize
      @array_info[inst.id] = {element_type, 0}
    end

    private def emit_array_get(inst : ArrayGet, name : String)
      base_name = name.lstrip('%')
      array_ptr = value_ref(inst.array_value)
      index = value_ref(inst.index_value)
      element_type = @type_mapper.llvm_type(inst.element_type)

      # If the array value is actually a tuple value (struct), use extractvalue
      actual_array_llvm = lookup_value_llvm_type(inst.array_value, "")
      if actual_array_llvm.starts_with?('{')
        idx_const = nil
        if !index.starts_with?('%') && index != "null"
          idx_const = index.to_i?
        end
        if idx_const
          emit "#{name} = extractvalue #{actual_array_llvm} #{array_ptr}, #{idx_const}"
          @value_types[inst.id] = inst.element_type
          return
        end
      end

      # Check if array value is a union type - need to extract ptr from payload
      array_value_type = @value_types[inst.array_value]?
      if array_value_type == TypeRef::POINTER
        if alloc_elem = @alloc_element_types[inst.array_value]?
          array_value_type = alloc_elem
        end
      end
      if array_value_type
        array_llvm_type = @type_mapper.llvm_type(array_value_type)
        actual_val_type = lookup_value_llvm_type(inst.array_value, "")
        if array_llvm_type.includes?(".union")
          # Extract value from union payload.
          # Both tuples and arrays/classes are reference types in our compiler —
          # the payload holds a pointer to the heap-allocated object.
          emit "%#{base_name}.union_ptr = alloca #{array_llvm_type}, align 8"
          union_val = array_ptr
          actual_val_type = lookup_value_llvm_type(inst.array_value, "")
          if actual_val_type == "ptr"
            emit "%#{base_name}.union_val = load #{array_llvm_type}, ptr #{array_ptr}"
            union_val = "%#{base_name}.union_val"
          end
          emit "store #{array_llvm_type} #{normalize_union_value(union_val, array_llvm_type)}, ptr %#{base_name}.union_ptr"
          emit "%#{base_name}.payload_ptr = getelementptr #{array_llvm_type}, ptr %#{base_name}.union_ptr, i32 0, i32 1"
          emit "%#{base_name}.arr_ptr = load ptr, ptr %#{base_name}.payload_ptr, align 4"
          array_ptr = "%#{base_name}.arr_ptr"
        elsif array_llvm_type != "ptr" && !array_llvm_type.starts_with?('%') && !array_llvm_type.starts_with?('[') && actual_val_type != "ptr"
          # Non-ptr, non-struct type (e.g., i1, i32) - this is a MIR type inference issue
          # Use inttoptr conversion as fallback
          if array_llvm_type == "i1" || array_llvm_type == "i8" || array_llvm_type == "i16" || array_llvm_type == "i32"
            emit "%#{base_name}.ext = zext #{array_llvm_type} #{array_ptr} to i64"
            emit "%#{base_name}.arr_ptr = inttoptr i64 %#{base_name}.ext to ptr"
          elsif array_llvm_type == "i64"
            emit "%#{base_name}.arr_ptr = inttoptr i64 #{array_ptr} to ptr"
          else
            # Unknown type - use null as safest fallback
            emit "%#{base_name}.arr_ptr = inttoptr i64 0 to ptr"
          end
          array_ptr = "%#{base_name}.arr_ptr"
        end
      end

      # Tuple element access: use struct GEP with constant index instead of array layout.
      # IMPORTANT: for known Array values, always use array buffer layout.
      if array_value_type && !@array_info.has_key?(inst.array_value)
        tuple_type_ref = array_value_type
        if array_value_type == TypeRef::POINTER
          if alloc_elem = @alloc_element_types[inst.array_value]?
            tuple_type_ref = alloc_elem
          end
        end
        if union_desc = @module.get_union_descriptor(tuple_type_ref)
          if tuple_variant = union_desc.variants.find { |v| (t = @module.type_registry.get(v.type_ref)) && t.kind.tuple? }
            tuple_type_ref = tuple_variant.type_ref
          end
        end
        if tuple_type = @module.type_registry.get(tuple_type_ref)
          if tuple_type.kind.tuple?
            idx_const = nil
            if !index.starts_with?('%') && index != "null"
              idx_const = index.to_i?
            end
            if idx_const
              element_count = tuple_type.element_types.try(&.size) || 0
              if idx_const >= 0 && idx_const < element_count
                # Use byte-level GEP with MIR type sizes/alignment to match how tuple
                # stores compute offsets (hir_to_mir.cr:722-731). Struct-level GEP uses
                # LLVM type sizes which differ (e.g., String is ptr=8 in LLVM but size=12 in MIR).
                elements = tuple_type.element_types.not_nil!
                byte_offset = 0_u64
                elements.each_with_index do |elem, i|
                  # Reference types (classes, structs) are heap-allocated and stored
                  # as pointers in tuples — use pointer size, not full struct size.
                  # Union types may need more than pointer size for their discriminated repr.
                  is_inline = elem.kind.primitive? || elem.kind.enum?
                  elem_size = if is_inline && elem.size > 0
                                elem.size
                              elsif elem.kind.union? && elem.size > 8
                                elem.size.to_u64
                              else
                                8_u64
                              end
                  elem_align = if is_inline && elem.alignment > 0
                                 elem.alignment
                               else
                                 8_u32
                               end
                  byte_offset = (byte_offset + elem_align - 1) & ~(elem_align.to_u64 - 1)
                  break if i == idx_const
                  byte_offset += elem_size
                end
                emit "%#{base_name}.elem_ptr = getelementptr i8, ptr #{array_ptr}, i32 #{byte_offset}"
              else
                # Index exceeds detected tuple element count — type tracking mismatch.
                # Fall back to byte-level GEP using element size.
                elem_size = case element_type
                            when "i8" then 1
                            when "i16" then 2
                            when "i32", "float" then 4
                            else 8
                            end
                byte_offset = idx_const.to_i64 * elem_size.to_i64
                emit "%#{base_name}.elem_ptr = getelementptr i8, ptr #{array_ptr}, i64 #{byte_offset}"
              end
              emit "#{name} = load #{element_type}, ptr %#{base_name}.elem_ptr"
              @value_types[inst.id] = inst.element_type
              return
            else
              # Variable (runtime) index on a Tuple.
              # Tuples store elements inline (not via a buf pointer like Array).
              # Compute byte offsets for each element, then use a switch to select
              # the right GEP based on the runtime index value.
              elements = tuple_type.element_types
              element_count = elements.try(&.size) || 0
              if element_count > 0 && elements
                # Compute byte offsets for each element
                offsets = [] of UInt64
                byte_offset = 0_u64
                elements.each_with_index do |elem, i|
                  is_inline = elem.kind.primitive? || elem.kind.enum?
                  elem_size = if is_inline && elem.size > 0
                                elem.size
                              elsif elem.kind.union? && elem.size > 8
                                elem.size.to_u64
                              else
                                8_u64
                              end
                  elem_align = if is_inline && elem.alignment > 0
                                 elem.alignment
                               else
                                 8_u32
                               end
                  byte_offset = (byte_offset + elem_align - 1) & ~(elem_align.to_u64 - 1)
                  offsets << byte_offset
                  byte_offset += elem_size
                end

                # Check if all offsets are uniformly spaced (homogeneous elements).
                # If so, use a simple multiply instead of a switch.
                stride = offsets.size >= 2 ? offsets[1] - offsets[0] : (offsets.first? || 8_u64)
                uniform = offsets.each_with_index.all? { |(off, i)| off == i.to_u64 * stride }

                # Ensure index is i32
                var_index = index
                if var_index.starts_with?('@')
                  # Global reference used as index — likely a value_ref mismatch.
                  # Load as pointer-sized int and truncate to i32.
                  emit "%#{base_name}.gidx_raw = ptrtoint ptr #{var_index} to i64"
                  emit "%#{base_name}.tidx = trunc i64 %#{base_name}.gidx_raw to i32"
                  var_index = "%#{base_name}.tidx"
                elsif var_index.starts_with?('%')
                  idx_type = @value_types[inst.index_value]?
                  idx_llvm = idx_type ? @type_mapper.llvm_type(idx_type) : "i32"
                  # Also check actual emitted type — GEP results are ptr even if MIR says i32
                  actual_emitted = @emitted_value_types[var_index]?
                  if actual_emitted && actual_emitted.includes?(".union")
                    idx_llvm = actual_emitted
                  end
                  idx_llvm = "ptr" if actual_emitted == "ptr"
                  if idx_llvm.includes?(".union")
                    emit "%#{base_name}.gidx_union_ptr = alloca #{idx_llvm}, align 8"
                    emit "store #{idx_llvm} #{normalize_union_value(var_index, idx_llvm)}, ptr %#{base_name}.gidx_union_ptr"
                    emit "%#{base_name}.gidx_payload_ptr = getelementptr #{idx_llvm}, ptr %#{base_name}.gidx_union_ptr, i32 0, i32 1"
                    if idx_llvm.includes?("Int64") || idx_llvm.includes?("UInt64") || idx_llvm.includes?("Pointer")
                      emit "%#{base_name}.gidx_payload = load i64, ptr %#{base_name}.gidx_payload_ptr, align 4"
                      emit "%#{base_name}.tidx = trunc i64 %#{base_name}.gidx_payload to i32"
                    else
                      emit "%#{base_name}.tidx = load i32, ptr %#{base_name}.gidx_payload_ptr, align 4"
                    end
                    var_index = "%#{base_name}.tidx"
                  elsif idx_llvm == "ptr"
                    # Pointer or non-integer used as index — convert via ptrtoint
                    emit "%#{base_name}.gidx_raw = ptrtoint ptr #{var_index} to i64"
                    emit "%#{base_name}.tidx = trunc i64 %#{base_name}.gidx_raw to i32"
                    var_index = "%#{base_name}.tidx"
                  elsif idx_llvm.includes?(".")
                    emit "%#{base_name}.gidx_raw = ptrtoint ptr #{var_index} to i64"
                    emit "%#{base_name}.tidx = trunc i64 %#{base_name}.gidx_raw to i32"
                    var_index = "%#{base_name}.tidx"
                  elsif idx_llvm == "i64"
                    emit "%#{base_name}.tidx = trunc i64 #{var_index} to i32"
                    var_index = "%#{base_name}.tidx"
                  elsif idx_llvm != "i32" && idx_llvm.starts_with?('i')
                    # Guard: verify value is actually integer-typed, not ptr
                    emitted_ty = @emitted_value_types[var_index]?
                    if emitted_ty == "ptr"
                      emit "%#{base_name}.gidx_raw = ptrtoint ptr #{var_index} to i64"
                      emit "%#{base_name}.tidx = trunc i64 %#{base_name}.gidx_raw to i32"
                    else
                      emit "%#{base_name}.tidx = sext #{idx_llvm} #{var_index} to i32"
                    end
                    var_index = "%#{base_name}.tidx"
                  end
                end

                if uniform
                  # Uniform stride: byte_offset = index * stride
                  emit "%#{base_name}.byte_off = mul i32 #{var_index}, #{stride}"
                  emit "%#{base_name}.elem_ptr = getelementptr i8, ptr #{array_ptr}, i32 %#{base_name}.byte_off"
                else
                  # Heterogeneous: use cascading selects to map index → byte offset
                  # sel0 = (idx==0) ? off_0 : off_last
                  # sel1 = (idx==1) ? off_1 : sel0
                  # sel2 = (idx==2) ? off_2 : sel1
                  # ...
                  default_off = offsets.last
                  prev = "#{default_off}"
                  offsets.each_with_index do |off, i|
                    emit "%#{base_name}.cmp#{i} = icmp eq i32 #{var_index}, #{i}"
                    emit "%#{base_name}.sel#{i} = select i1 %#{base_name}.cmp#{i}, i32 #{off}, i32 #{prev}"
                    prev = "%#{base_name}.sel#{i}"
                  end
                  emit "%#{base_name}.elem_ptr = getelementptr i8, ptr #{array_ptr}, i32 #{prev}"
                end
                emit "#{name} = load #{element_type}, ptr %#{base_name}.elem_ptr"
                @value_types[inst.id] = inst.element_type
                return
              end
            end
          end
        end
      end

      # Handle bare Tuple struct (generic Tuple without concrete type params in registry).
      # When Tuple#includes? is compiled as a standalone method, self is typed as bare
      # Struct(Tuple) instead of concrete Tuple(Char, Char). Use inline byte-offset access.
      if array_value_type
        bare_type = @module.type_registry.get(array_value_type)
        if bare_type && bare_type.name == "Tuple" && bare_type.kind.struct?
          idx_const = nil
          if !index.starts_with?('%') && index != "null"
            idx_const = index.to_i?
          end
          elem_size = case element_type
                      when "i1", "i8" then 1
                      when "i16"      then 2
                      when "i32", "float" then 4
                      when "i64", "double", "ptr" then 8
                      else 8
                      end
          if idx_const
            byte_offset = idx_const.to_i64 * elem_size.to_i64
            emit "%#{base_name}.elem_ptr = getelementptr i8, ptr #{array_ptr}, i64 #{byte_offset}"
            emit "#{name} = load #{element_type}, ptr %#{base_name}.elem_ptr"
            @value_types[inst.id] = inst.element_type
            return
          else
            # Variable index on bare Tuple: all elements assumed same size
            var_index = index
            if var_index.starts_with?('@')
              emit "%#{base_name}.gidx_raw = ptrtoint ptr #{var_index} to i64"
              emit "%#{base_name}.tidx = trunc i64 %#{base_name}.gidx_raw to i32"
              var_index = "%#{base_name}.tidx"
            end
            emit "%#{base_name}.byte_off = mul i32 #{var_index}, #{elem_size}"
            emit "%#{base_name}.elem_ptr = getelementptr i8, ptr #{array_ptr}, i32 %#{base_name}.byte_off"
            emit "#{name} = load #{element_type}, ptr %#{base_name}.elem_ptr"
            @value_types[inst.id] = inst.element_type
            return
          end
        end
      end

      # Check if index needs type conversion for i32 GEP index
      index_type = @value_types[inst.index_value]?
      if index_type
        index_llvm = @type_mapper.llvm_type(index_type)
        if !index_llvm.includes?(".union")
          if emitted_index_llvm = @emitted_value_types[index]?
            if emitted_index_llvm.includes?(".union")
              index_llvm = emitted_index_llvm
            end
          end
        end
        if index_llvm.includes?(".union")
          # Union type index - extract payload as i32
          emit "%#{base_name}.idx_union_ptr = alloca #{index_llvm}, align 8"
          emit "store #{index_llvm} #{normalize_union_value(index, index_llvm)}, ptr %#{base_name}.idx_union_ptr"
          emit "%#{base_name}.idx_payload_ptr = getelementptr #{index_llvm}, ptr %#{base_name}.idx_union_ptr, i32 0, i32 1"
          # Check if payload is likely pointer or integer based on union name
          if index_llvm.includes?("Pointer") || index_llvm.includes?("Int64")
            emit "%#{base_name}.idx_payload = load i64, ptr %#{base_name}.idx_payload_ptr, align 4"
            emit "%#{base_name}.idx_int = trunc i64 %#{base_name}.idx_payload to i32"
          else
            emit "%#{base_name}.idx_int = load i32, ptr %#{base_name}.idx_payload_ptr, align 4"
          end
          index = "%#{base_name}.idx_int"
        elsif index_llvm == "ptr"
          emit "%#{base_name}.idx_int = ptrtoint ptr #{index} to i32"
          index = "%#{base_name}.idx_int"
        elsif index_llvm == "i1" || index_llvm == "i8" || index_llvm == "i16"
          emit "%#{base_name}.idx_int = zext #{index_llvm} #{index} to i32"
          index = "%#{base_name}.idx_int"
        elsif index_llvm == "i64"
          emit "%#{base_name}.idx_int = trunc i64 #{index} to i32"
          index = "%#{base_name}.idx_int"
        elsif index_llvm == "i128"
          emit "%#{base_name}.idx_int = trunc i128 #{index} to i32"
          index = "%#{base_name}.idx_int"
        elsif index_llvm == "double"
          emit "%#{base_name}.idx_int = fptosi double #{index} to i32"
          index = "%#{base_name}.idx_int"
        elsif index_llvm == "float"
          emit "%#{base_name}.idx_int = fptosi float #{index} to i32"
          index = "%#{base_name}.idx_int"
        end
      end
      # Guard against null index (MIR type mismatch) - default to 0
      index = "0" if index == "null"

      # StaticArray: data is inline (no buffer pointer), use direct element GEP
      is_static_array = false
      if array_value_type
        sa_type = @module.type_registry.get(array_value_type)
        if sa_type && sa_type.name.starts_with?("StaticArray(")
          is_static_array = true
        end
      end
      if is_static_array
        # StaticArray stores elements inline at offset 0
        emit "%#{base_name}.elem_ptr = getelementptr #{element_type}, ptr #{array_ptr}, i32 #{index}"
        emit "#{name} = load #{element_type}, ptr %#{base_name}.elem_ptr"
      else
        # Load buffer pointer from Crystal Array layout (offset 16 = @buffer field)
        emit "%#{base_name}.buf_addr = getelementptr i8, ptr #{array_ptr}, i32 16"
        emit "%#{base_name}.buf = load ptr, ptr %#{base_name}.buf_addr"
        # Get element from buffer
        emit "%#{base_name}.elem_ptr = getelementptr #{element_type}, ptr %#{base_name}.buf, i32 #{index}"
        emit "#{name} = load #{element_type}, ptr %#{base_name}.elem_ptr"
      end

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
          emit "store #{array_llvm_type} #{normalize_union_value(array_ptr, array_llvm_type)}, ptr %#{base_name}.union_ptr"
          emit "%#{base_name}.payload_ptr = getelementptr #{array_llvm_type}, ptr %#{base_name}.union_ptr, i32 0, i32 1"
          emit "%#{base_name}.arr_ptr = load ptr, ptr %#{base_name}.payload_ptr, align 4"
          array_ptr = "%#{base_name}.arr_ptr"
        elsif array_llvm_type != "ptr" && !array_llvm_type.starts_with?('%')
          # Non-ptr, non-struct type (e.g., i1, i32) - this is a MIR type inference issue
          # Use inttoptr conversion as fallback
          if array_llvm_type == "i1" || array_llvm_type == "i8" || array_llvm_type == "i16" || array_llvm_type == "i32"
            emit "%#{base_name}.ext = zext #{array_llvm_type} #{array_ptr} to i64"
            emit "%#{base_name}.arr_ptr = inttoptr i64 %#{base_name}.ext to ptr"
          elsif array_llvm_type == "i64"
            emit "%#{base_name}.arr_ptr = inttoptr i64 #{array_ptr} to ptr"
          else
            # Unknown type - use null as safest fallback
            emit "%#{base_name}.arr_ptr = inttoptr i64 0 to ptr"
          end
          array_ptr = "%#{base_name}.arr_ptr"
        end
      end

      # Check if index needs type conversion for i32 GEP index
      index_type = @value_types[inst.index_value]?
      if index_type
        index_llvm = @type_mapper.llvm_type(index_type)
        if !index_llvm.includes?(".union")
          if emitted_index_llvm = @emitted_value_types[index]?
            if emitted_index_llvm.includes?(".union")
              index_llvm = emitted_index_llvm
            end
          end
        end
        if index_llvm.includes?(".union")
          # Union type index - extract payload as i32
          emit "%#{base_name}.idx_union_ptr = alloca #{index_llvm}, align 8"
          emit "store #{index_llvm} #{normalize_union_value(index, index_llvm)}, ptr %#{base_name}.idx_union_ptr"
          emit "%#{base_name}.idx_payload_ptr = getelementptr #{index_llvm}, ptr %#{base_name}.idx_union_ptr, i32 0, i32 1"
          if index_llvm.includes?("Pointer") || index_llvm.includes?("Int64")
            emit "%#{base_name}.idx_payload = load i64, ptr %#{base_name}.idx_payload_ptr, align 4"
            emit "%#{base_name}.idx_int = trunc i64 %#{base_name}.idx_payload to i32"
          else
            emit "%#{base_name}.idx_int = load i32, ptr %#{base_name}.idx_payload_ptr, align 4"
          end
          index = "%#{base_name}.idx_int"
        elsif index_llvm == "ptr"
          emit "%#{base_name}.idx_int = ptrtoint ptr #{index} to i32"
          index = "%#{base_name}.idx_int"
        elsif index_llvm == "i1" || index_llvm == "i8" || index_llvm == "i16"
          emit "%#{base_name}.idx_int = zext #{index_llvm} #{index} to i32"
          index = "%#{base_name}.idx_int"
        elsif index_llvm == "i64"
          emit "%#{base_name}.idx_int = trunc i64 #{index} to i32"
          index = "%#{base_name}.idx_int"
        elsif index_llvm == "i128"
          emit "%#{base_name}.idx_int = trunc i128 #{index} to i32"
          index = "%#{base_name}.idx_int"
        elsif index_llvm == "double"
          emit "%#{base_name}.idx_int = fptosi double #{index} to i32"
          index = "%#{base_name}.idx_int"
        elsif index_llvm == "float"
          emit "%#{base_name}.idx_int = fptosi float #{index} to i32"
          index = "%#{base_name}.idx_int"
        end
      end
      # Guard against null index (MIR type mismatch) - default to 0
      index = "0" if index == "null"

      # Check if value type matches element type and convert if needed
      value_type = @value_types[inst.value_id]?
      actual_value_type = value_type ? @type_mapper.llvm_type(value_type) : element_type
      if actual_value_type == "ptr" && element_type == "i32"
        # Convert ptr to i32
        emit "%#{base_name}.val_int = ptrtoint ptr #{value} to i32"
        value = "%#{base_name}.val_int"
      elsif actual_value_type == "ptr" && element_type != "ptr" && !element_type.includes?(".union")
        # Convert ptr to target integer type
        emit "%#{base_name}.val_cast = ptrtoint ptr #{value} to #{element_type}"
        value = "%#{base_name}.val_cast"
      elsif actual_value_type == "i1" && element_type.starts_with?('i') && element_type != "i1"
        # Convert bool (i1) to larger integer type (zext)
        emit "%#{base_name}.val_ext = zext i1 #{value} to #{element_type}"
        value = "%#{base_name}.val_ext"
      elsif actual_value_type.starts_with?('i') && !actual_value_type.includes?(".union") && element_type == "ptr"
        # Convert integer to ptr (inttoptr)
        emit "%#{base_name}.val_ptr = inttoptr #{actual_value_type} #{value} to ptr"
        value = "%#{base_name}.val_ptr"
      elsif actual_value_type.includes?(".union") && element_type == "ptr"
        # Union → ptr: extract pointer from union payload
        emit "%#{base_name}.val_union_ptr = alloca #{actual_value_type}, align 8"
        emit "store #{actual_value_type} #{normalize_union_value(value, actual_value_type)}, ptr %#{base_name}.val_union_ptr"
        emit "%#{base_name}.val_payload_ptr = getelementptr #{actual_value_type}, ptr %#{base_name}.val_union_ptr, i32 0, i32 1"
        emit "%#{base_name}.val_ptr = load ptr, ptr %#{base_name}.val_payload_ptr, align 4"
        value = "%#{base_name}.val_ptr"
      elsif actual_value_type.includes?(".union") && element_type.includes?(".union") && element_type != actual_value_type
        # Union → different union type (same physical layout): reinterpret through memory
        emit "%#{base_name}.val_u2u_ptr = alloca #{actual_value_type}, align 8"
        emit "store #{actual_value_type} #{normalize_union_value(value, actual_value_type)}, ptr %#{base_name}.val_u2u_ptr"
        emit "%#{base_name}.val_u2u = load #{element_type}, ptr %#{base_name}.val_u2u_ptr"
        value = "%#{base_name}.val_u2u"
      elsif actual_value_type.includes?(".union") && !element_type.includes?(".union") && element_type != actual_value_type
        # Union → non-union type: extract payload and convert
        emit "%#{base_name}.val_union_ptr = alloca #{actual_value_type}, align 8"
        emit "store #{actual_value_type} #{normalize_union_value(value, actual_value_type)}, ptr %#{base_name}.val_union_ptr"
        emit "%#{base_name}.val_payload_ptr = getelementptr #{actual_value_type}, ptr %#{base_name}.val_union_ptr, i32 0, i32 1"
        emit "%#{base_name}.val_conv = load #{element_type}, ptr %#{base_name}.val_payload_ptr, align 4"
        value = "%#{base_name}.val_conv"
      elsif actual_value_type == "ptr" && element_type.includes?(".union")
        # ptr → union: wrap pointer in union with type_id=0
        emit "%#{base_name}.val_union_ptr = alloca #{element_type}, align 8"
        emit "%#{base_name}.val_tid_ptr = getelementptr #{element_type}, ptr %#{base_name}.val_union_ptr, i32 0, i32 0"
        emit "store i32 0, ptr %#{base_name}.val_tid_ptr"
        emit "%#{base_name}.val_pay_ptr = getelementptr #{element_type}, ptr %#{base_name}.val_union_ptr, i32 0, i32 1"
        emit "store ptr #{value}, ptr %#{base_name}.val_pay_ptr, align 4"
        emit "%#{base_name}.val_union = load #{element_type}, ptr %#{base_name}.val_union_ptr"
        value = "%#{base_name}.val_union"
      elsif actual_value_type.starts_with?('i') && element_type.includes?(".union")
        # primitive int → union: wrap in union struct (e.g., enum value into Nil|Enum union)
        # Determine type_id: look up union descriptor for variant matching the value type
        val_type_ref = @value_types[inst.value_id]?
        val_type_id = 0
        if val_type_ref
          union_desc = @module.get_union_descriptor(inst.element_type)
          if union_desc
            union_desc.variants.each do |v|
              if v.type_ref == val_type_ref
                val_type_id = v.type_id.to_i32
                break
              end
            end
          end
          val_type_id = val_type_ref.id.to_i32 if val_type_id == 0
        end
        emit "%#{base_name}.val_union_ptr = alloca #{element_type}, align 8"
        emit "call void @llvm.memset.p0.i64(ptr %#{base_name}.val_union_ptr, i8 0, i64 16, i1 false)"
        emit "%#{base_name}.val_tid_ptr = getelementptr #{element_type}, ptr %#{base_name}.val_union_ptr, i32 0, i32 0"
        emit "store i32 #{val_type_id}, ptr %#{base_name}.val_tid_ptr"
        emit "%#{base_name}.val_pay_ptr = getelementptr #{element_type}, ptr %#{base_name}.val_union_ptr, i32 0, i32 1"
        emit "store #{actual_value_type} #{value}, ptr %#{base_name}.val_pay_ptr, align 4"
        emit "%#{base_name}.val_union = load #{element_type}, ptr %#{base_name}.val_union_ptr"
        value = "%#{base_name}.val_union"
      elsif actual_value_type.starts_with?('i') && element_type.starts_with?('i') && actual_value_type != element_type
        # Integer width mismatch - use sext or trunc
        actual_bits = actual_value_type[1..].to_i? || 32
        element_bits = element_type[1..].to_i? || 32
        if actual_bits < element_bits
          emit "%#{base_name}.val_ext = sext #{actual_value_type} #{value} to #{element_type}"
          value = "%#{base_name}.val_ext"
        else
          emit "%#{base_name}.val_trunc = trunc #{actual_value_type} #{value} to #{element_type}"
          value = "%#{base_name}.val_trunc"
        end
      end

      # LLVM pointer constants must use `null` (not `0`)
      if element_type == "ptr" && value == "0"
        value = "null"
      end

      # Detect StaticArray — data is stored inline (no buffer pointer)
      is_static_array = false
      if array_value_type
        sa_type = @module.type_registry.get(array_value_type)
        if sa_type && sa_type.name.starts_with?("StaticArray(")
          is_static_array = true
        end
      end

      if is_static_array
        # StaticArray stores elements inline at offset 0
        emit "%#{base_name}.elem_ptr = getelementptr #{element_type}, ptr #{array_ptr}, i32 #{index}"
        emit "store #{element_type} #{value}, ptr %#{base_name}.elem_ptr"
      else
        # Load buffer pointer from Crystal Array layout (offset 16 = @buffer field)
        emit "%#{base_name}.buf_addr = getelementptr i8, ptr #{array_ptr}, i32 16"
        emit "%#{base_name}.buf = load ptr, ptr %#{base_name}.buf_addr"
        # Set element in buffer
        emit "%#{base_name}.elem_ptr = getelementptr #{element_type}, ptr %#{base_name}.buf, i32 #{index}"
        emit "store #{element_type} #{value}, ptr %#{base_name}.elem_ptr"
      end
    end

    private def emit_string_interpolation(inst : StringInterpolation, name : String)
      base_name = name.lstrip('%')

      # Convert each part to string ptr, handling type conversion
      string_parts = [] of String
      inst.parts.each_with_index do |part_id, idx|
        part_type = @value_types[part_id]?
        # Use HIR-level part_types for Char/Int32 disambiguation when available
        if hir_pt = inst.part_types.try(&.[idx]?)
          if hir_pt == TypeRef::CHAR && (part_type == TypeRef::INT32 || part_type.nil?)
            part_type = TypeRef::CHAR
          end
        end
        part_llvm_type_check = part_type ? @type_mapper.llvm_type(part_type) : nil

        # Check for void type - void calls don't produce a value, use empty string
        if part_llvm_type_check == "void"
          string_parts << "@.str.empty"
          next
        end

        part_ref = value_ref(part_id)

        # Check if part is an Array (stored as ptr but tracked in @array_info)
        if arr_info = @array_info[part_id]?
          elem_llvm_type = arr_info[0]  # LLVM type: "i32", "ptr", etc.
          helper = case elem_llvm_type
                   when "i32", "i16", "i8"
                     "__crystal_v2_array_i32_to_string"
                   when "ptr"
                     "__crystal_v2_array_string_to_string"
                   else
                     "__crystal_v2_array_i32_to_string"  # fallback
                   end
          emit "%#{base_name}.conv#{idx} = call ptr @#{helper}(ptr #{part_ref})"
          string_parts << "%#{base_name}.conv#{idx}"
          next
        end

        # Check if part is already a string (ptr type from string literal)
        if part_type == TypeRef::STRING || part_type == TypeRef::POINTER || part_type.nil?
          string_parts << part_ref
        elsif part_type == TypeRef::CHAR
          # Convert char (i32 codepoint) to string
          emit "%#{base_name}.conv#{idx} = call ptr @__crystal_v2_char_to_string(i32 #{part_ref})"
          string_parts << "%#{base_name}.conv#{idx}"
        elsif part_type == TypeRef::INT32 || part_type == TypeRef::UINT32
          # Convert int32 to string
          emit "%#{base_name}.conv#{idx} = call ptr @__crystal_v2_int_to_string(i32 #{part_ref})"
          string_parts << "%#{base_name}.conv#{idx}"
        elsif part_type == TypeRef::INT64 || part_type == TypeRef::UINT64
          # Convert int64 to string
          emit "%#{base_name}.conv#{idx} = call ptr @__crystal_v2_int64_to_string(i64 #{part_ref})"
          string_parts << "%#{base_name}.conv#{idx}"
        elsif part_type == TypeRef::INT128 || part_type == TypeRef::UINT128
          # Convert int128 to string (truncate to i64 — sufficient for practical values like unix_ns)
          emit "%#{base_name}.trunc#{idx} = trunc i128 #{part_ref} to i64"
          emit "%#{base_name}.conv#{idx} = call ptr @__crystal_v2_int64_to_string(i64 %#{base_name}.trunc#{idx})"
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
          elsif part_llvm_type == "i128"
            emit "%#{base_name}.trunc#{idx} = trunc i128 #{part_ref} to i64"
            emit "%#{base_name}.conv#{idx} = call ptr @__crystal_v2_int64_to_string(i64 %#{base_name}.trunc#{idx})"
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
          elsif part_llvm_type == "double"
            emit "%#{base_name}.conv#{idx} = call ptr @__crystal_v2_f64_to_string(double #{part_ref})"
            string_parts << "%#{base_name}.conv#{idx}"
          elsif part_llvm_type == "float"
            # Convert float to double first
            emit "%#{base_name}.ext#{idx} = fpext float #{part_ref} to double"
            emit "%#{base_name}.conv#{idx} = call ptr @__crystal_v2_f64_to_string(double %#{base_name}.ext#{idx})"
            string_parts << "%#{base_name}.conv#{idx}"
          elsif part_llvm_type.includes?(".union")
            # Union type — must check payload type, not always assume ptr.
            # For Nil|Int32, payload is i32; for Nil|String, payload is ptr.
            # IMPORTANT: The actual slot type may differ from the MIR TypeRef.
            # If the slot stores a primitive (not a union), use the slot type instead.
            actual_slot_type = @cross_block_slot_types[part_id]?
            if actual_slot_type && !actual_slot_type.includes?(".union") && actual_slot_type != part_llvm_type
              # Slot is a primitive — use the primitive conversion path instead of union
              case actual_slot_type
              when "i32"
                emit "%#{base_name}.conv#{idx} = call ptr @__crystal_v2_int_to_string(i32 #{part_ref})"
              when "i64"
                emit "%#{base_name}.conv#{idx} = call ptr @__crystal_v2_int64_to_string(i64 #{part_ref})"
              when "i1"
                emit "%#{base_name}.conv#{idx} = call ptr @__crystal_v2_bool_to_string(i1 #{part_ref})"
              when "ptr"
                string_parts << part_ref
                next
              else
                # Fallback: treat as ptr
                string_parts << part_ref
                next
              end
              string_parts << "%#{base_name}.conv#{idx}"
              next
            end
            # The actual emitted type of the value may differ from part_llvm_type
            # (e.g., Hash#[] returns %String$_$OR$_UInt32.union but caller expects %Nil$_$OR$_String.union).
            # Use the emitted type for the store to avoid LLVM type mismatch errors.
            store_union_type = @emitted_value_types[part_ref]?
            store_union_type = part_llvm_type unless store_union_type && store_union_type.includes?(".union")
            emit "%#{base_name}.union_ptr#{idx} = alloca #{part_llvm_type}, align 8"
            emit "store #{store_union_type} #{normalize_union_value(part_ref, store_union_type)}, ptr %#{base_name}.union_ptr#{idx}"
            emit "%#{base_name}.tid_ptr#{idx} = getelementptr #{part_llvm_type}, ptr %#{base_name}.union_ptr#{idx}, i32 0, i32 0"
            emit "%#{base_name}.tid#{idx} = load i32, ptr %#{base_name}.tid_ptr#{idx}"
            emit "%#{base_name}.payload_ptr#{idx} = getelementptr #{part_llvm_type}, ptr %#{base_name}.union_ptr#{idx}, i32 0, i32 1"

            # Determine payload type from union LLVM type name
            # If last $OR$ member is Int32/UInt32, payload might be i32
            payload_is_int32 = part_llvm_type.ends_with?("_Int32.union") ||
                               part_llvm_type.ends_with?("_UInt32.union")
            payload_is_int64 = part_llvm_type.ends_with?("_Int64.union") ||
                               part_llvm_type.ends_with?("_UInt64.union")
            payload_is_bool = part_llvm_type.ends_with?("_Bool.union")

            # Use branchless select to avoid splitting basic blocks (breaks phi predecessors).
            emit "%#{base_name}.is_nil#{idx} = icmp eq i32 %#{base_name}.tid#{idx}, 0"
            if payload_is_int32
              emit "%#{base_name}.i32_val#{idx} = load i32, ptr %#{base_name}.payload_ptr#{idx}, align 4"
              emit "%#{base_name}.i32_str#{idx} = call ptr @__crystal_v2_int_to_string(i32 %#{base_name}.i32_val#{idx})"
              emit "%#{base_name}.conv#{idx} = select i1 %#{base_name}.is_nil#{idx}, ptr @.str.empty, ptr %#{base_name}.i32_str#{idx}"
              string_parts << "%#{base_name}.conv#{idx}"
            elsif payload_is_int64
              emit "%#{base_name}.i64_val#{idx} = load i64, ptr %#{base_name}.payload_ptr#{idx}, align 4"
              emit "%#{base_name}.i64_str#{idx} = call ptr @__crystal_v2_int64_to_string(i64 %#{base_name}.i64_val#{idx})"
              emit "%#{base_name}.conv#{idx} = select i1 %#{base_name}.is_nil#{idx}, ptr @.str.empty, ptr %#{base_name}.i64_str#{idx}"
              string_parts << "%#{base_name}.conv#{idx}"
            elsif payload_is_bool
              emit "%#{base_name}.bool_val#{idx} = load i8, ptr %#{base_name}.payload_ptr#{idx}, align 4"
              emit "%#{base_name}.bool_i1#{idx} = trunc i8 %#{base_name}.bool_val#{idx} to i1"
              emit "%#{base_name}.bool_str#{idx} = call ptr @__crystal_v2_bool_to_string(i1 %#{base_name}.bool_i1#{idx})"
              emit "%#{base_name}.conv#{idx} = select i1 %#{base_name}.is_nil#{idx}, ptr @.str.empty, ptr %#{base_name}.bool_str#{idx}"
              string_parts << "%#{base_name}.conv#{idx}"
            else
              # Payload is ptr (String, Array, or class instance) — load always safe from alloca
              emit "%#{base_name}.str_ptr#{idx} = load ptr, ptr %#{base_name}.payload_ptr#{idx}, align 4"
              emit "%#{base_name}.conv#{idx} = select i1 %#{base_name}.is_nil#{idx}, ptr @.str.empty, ptr %#{base_name}.str_ptr#{idx}"
              string_parts << "%#{base_name}.conv#{idx}"
            end
          else
            # Check if this is an Array type — build "[elem, elem, ...]" string
            arr_type_name = part_type ? (@module.type_registry.get(part_type).try(&.name) || "") : ""
            if arr_type_name.starts_with?("Array(")
              # Extract element type name from "Array(Int32)" etc.
              elem_name = arr_type_name[6, arr_type_name.size - 7]
              helper = case elem_name
                       when "Int32", "UInt32", "Int16", "UInt16", "Int8", "UInt8"
                         "__crystal_v2_array_i32_to_string"
                       when "String"
                         "__crystal_v2_array_string_to_string"
                       else
                         "__crystal_v2_array_i32_to_string"  # fallback for int-like types
                       end
              emit "%#{base_name}.conv#{idx} = call ptr @#{helper}(ptr #{part_ref})"
              string_parts << "%#{base_name}.conv#{idx}"
            else
              # Fallback - treat as ptr
              string_parts << part_ref
            end
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
        # 3+ parts: alloca array, store parts, call single-alloc __crystal_v2_string_interpolate
        n = string_parts.size
        emit "%#{base_name}.arr = alloca [#{n} x ptr]"
        string_parts.each_with_index do |part, i|
          emit "%#{base_name}.slot#{i} = getelementptr [#{n} x ptr], ptr %#{base_name}.arr, i32 0, i32 #{i}"
          emit "store ptr #{part}, ptr %#{base_name}.slot#{i}"
        end
        emit "#{name} = call ptr @__crystal_v2_string_interpolate(ptr %#{base_name}.arr, i32 #{n})"
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

    # Exception handling - push jmp_buf and inline setjmp
    private def emit_try_begin(inst : TryBegin, name : String)
      base = name.lstrip('%')
      # Push: increment depth, compute jmpbuf pointer, setjmp
      emit "%#{base}.depth = load i32, ptr @__crystal_exc_depth"
      emit "%#{base}.new_depth = add i32 %#{base}.depth, 1"
      emit "store i32 %#{base}.new_depth, ptr @__crystal_exc_depth"
      emit "%#{base}.jmpbuf = getelementptr [64 x %jmp_buf], ptr @__crystal_exc_jmpbufs, i32 0, i32 %#{base}.depth"
      # Call setjmp inline (critical: must be inline, not in a wrapper function)
      emit "#{name} = call i32 @setjmp(ptr %#{base}.jmpbuf)"
    end

    # Exception handling - pop jmp_buf (decrement depth)
    private def emit_try_end(inst : TryEnd, name : String)
      emit "call void @__crystal_v2_try_end()"
    end

    private def emit_func_pointer(inst : FuncPointer, name : String)
      mangled = mangle_function_name(inst.func_name)
      # In LLVM IR, a function name @foo is a valid ptr value
      @constant_values[inst.id] = "@#{mangled}"
      @value_types[inst.id] = TypeRef::POINTER
      # Emit a bitcast to materialize it as a register (needed for phi/cross-block use)
      emit "#{name} = bitcast ptr @#{mangled} to ptr"
      # Track as called function so iterative RTA pass emits it
      @called_crystal_functions[mangled] ||= {"ptr", 0, [] of String}
    end

    private def emit_terminator(term : Terminator)
      case term
      when Return
        if @current_slab_frame
          emit "call void @__crystal_v2_slab_frame_pop()"
        end
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
          is_undefined = (@value_names[val]?.nil? && @constant_values[val]?.nil? && @cross_block_slots[val]?.nil?) ||
                         val_llvm_type == "void"
          if is_undefined
            # Undefined value - use safe default
            if @current_return_type == "ptr"
              emit "ret ptr null"
            elsif @current_return_type.includes?(".union")
              # Return a nil union. Use the correct Nil variant type_id
              # (not always 0 — depends on variant ordering in the union).
              nil_vid = nil_variant_id_for_union_type(@current_return_type)
              if nil_vid && nil_vid == 0
                emit "ret #{@current_return_type} zeroinitializer"
              elsif nil_vid
                c = @cond_counter
                @cond_counter += 1
                emit "%ret_nil#{c}.ptr = alloca #{@current_return_type}, align 8"
                emit "store #{@current_return_type} zeroinitializer, ptr %ret_nil#{c}.ptr"
                emit "%ret_nil#{c}.tid = getelementptr #{@current_return_type}, ptr %ret_nil#{c}.ptr, i32 0, i32 0"
                emit "store i32 #{nil_vid}, ptr %ret_nil#{c}.tid"
                emit "%ret_nil#{c}.val = load #{@current_return_type}, ptr %ret_nil#{c}.ptr"
                emit "ret #{@current_return_type} %ret_nil#{c}.val"
              else
                # No Nil variant found — use zeroinitializer as fallback
                emit "ret #{@current_return_type} zeroinitializer"
              end
            else
              # Use 0.0 for float types, 0 for others
              if @current_return_type == "double" || @current_return_type == "float"
                emit "ret #{@current_return_type} 0.0"
              else
                emit "ret #{@current_return_type} 0"
              end
            end
          else
            val_ref = value_ref(val)
            val_llvm_type = val_type ? @type_mapper.llvm_type(val_type) : "ptr"
            # value_ref may resolve to a register that was emitted with a different LLVM type
            # than @value_types predicts (e.g., due ABI-preserving call emission). Prefer the
            # real emitted type to avoid invalid `ret <type> <value>` pairs.
            if actual_emitted_type = @emitted_value_types[val_ref]?
              val_llvm_type = actual_emitted_type
            end

            if @current_return_type.includes?(".union") && val_ref == "null"
              # Preserve nil semantics — use the correct Nil variant type_id.
              nil_vid = nil_variant_id_for_union_type(@current_return_type)
              if nil_vid && nil_vid == 0
                emit "ret #{@current_return_type} zeroinitializer"
              elsif nil_vid
                c = @cond_counter
                @cond_counter += 1
                emit "%ret_nil#{c}.ptr = alloca #{@current_return_type}, align 8"
                emit "store #{@current_return_type} zeroinitializer, ptr %ret_nil#{c}.ptr"
                emit "%ret_nil#{c}.tid = getelementptr #{@current_return_type}, ptr %ret_nil#{c}.ptr, i32 0, i32 0"
                emit "store i32 #{nil_vid}, ptr %ret_nil#{c}.tid"
                emit "%ret_nil#{c}.val = load #{@current_return_type}, ptr %ret_nil#{c}.ptr"
                emit "ret #{@current_return_type} %ret_nil#{c}.val"
              else
                emit "ret #{@current_return_type} zeroinitializer"
              end
              return
            end

          # Check if we need to wrap concrete type in union
          if @current_return_type.includes?(".union") && !val_llvm_type.includes?(".union")
            c = @cond_counter  # Reuse cond_counter for unique naming
            @cond_counter += 1
            emit "%ret#{c}.union_ptr = alloca #{@current_return_type}, align 8"
            emit "%ret#{c}.type_id_ptr = getelementptr #{@current_return_type}, ptr %ret#{c}.union_ptr, i32 0, i32 0"

            # Determine the correct Nil variant type_id for this union
            # (not always 0 — depends on variant ordering).
            ret_nil_vid = nil_variant_id_for_union_type(@current_return_type) || 0
            ret_nonnil_vid = ret_nil_vid == 0 ? 1 : 0

            if val_llvm_type == "void" || val_ref == "null"
              # Nil/void return — set correct Nil variant type_id
              emit "store #{@current_return_type} zeroinitializer, ptr %ret#{c}.union_ptr"
              emit "store i32 #{ret_nil_vid}, ptr %ret#{c}.type_id_ptr"
              # Don't store payload for nil
            elsif @phi_nil_incoming_blocks.has_key?(val) && val_llvm_type.starts_with?('i')
              # Value comes from a phi that has nil incoming - emit conditional type_id
              # When nil flows into an integer phi, it becomes 0, so check if val == 0
              # TODO: This heuristic assumes non-nil values are never 0, which may not always be true
              emit "%ret#{c}.is_nil = icmp eq #{val_llvm_type} #{val_ref}, 0"
              emit "%ret#{c}.type_id = select i1 %ret#{c}.is_nil, i32 #{ret_nil_vid}, i32 #{ret_nonnil_vid}"
              emit "store i32 %ret#{c}.type_id, ptr %ret#{c}.type_id_ptr"
              emit "%ret#{c}.payload_ptr = getelementptr #{@current_return_type}, ptr %ret#{c}.union_ptr, i32 0, i32 1"
              emit "store #{val_llvm_type} #{val_ref}, ptr %ret#{c}.payload_ptr, align 4"
            elsif val_llvm_type == "ptr"
              # Pointer value being wrapped into union — the pointer might be null
              # (from a union Nil variant that was extracted by the ABI fix).
              # Emit a runtime null check to set the correct type_id.
              emit "%ret#{c}.is_null = icmp eq ptr #{val_ref}, null"
              emit "%ret#{c}.type_id = select i1 %ret#{c}.is_null, i32 #{ret_nil_vid}, i32 #{ret_nonnil_vid}"
              emit "store i32 %ret#{c}.type_id, ptr %ret#{c}.type_id_ptr"
              emit "%ret#{c}.payload_ptr = getelementptr #{@current_return_type}, ptr %ret#{c}.union_ptr, i32 0, i32 1"
              # For tuple types, copy data instead of storing pointer
              ret_vt_size = 0_u64
              union_mir = @module.type_registry.get(@current_return_type_ref)
              if union_mir && (uvars = union_mir.variants) && uvars.size > 1
                vtype = uvars[1] # variant 1 = non-nil (matches type_id=1)
                if vtype.name.starts_with?("Tuple(") && vtype.is_value_type? && vtype.size > 0
                  ret_vt_size = vtype.size
                end
              end
              if ret_vt_size > 0
                emit "call void @llvm.memcpy.p0.p0.i64(ptr %ret#{c}.payload_ptr, ptr #{val_ref}, i64 #{ret_vt_size}, i1 false)"
              else
                emit "store ptr #{val_ref}, ptr %ret#{c}.payload_ptr, align 4"
              end
            else
              # Non-nil, non-pointer value - set type_id=1 and store payload
              emit "store i32 1, ptr %ret#{c}.type_id_ptr"
              emit "%ret#{c}.payload_ptr = getelementptr #{@current_return_type}, ptr %ret#{c}.union_ptr, i32 0, i32 1"
              emit "store #{val_llvm_type} #{val_ref}, ptr %ret#{c}.payload_ptr, align 4"
            end
            emit "%ret#{c}.union = load #{@current_return_type}, ptr %ret#{c}.union_ptr"
            emit "ret #{@current_return_type} %ret#{c}.union"
          else
            # For pointer returns, convert integer 0 to null
            # For integer returns, convert null to 0
            # For ptr returns with int value, use inttoptr
            if @current_return_type == "ptr" && val_ref == "0"
              emit "ret ptr null"
            elsif @current_return_type.starts_with?('i') && val_ref == "null"
              emit "ret #{@current_return_type} 0"
            elsif @current_return_type == "ptr" && val_llvm_type && val_llvm_type.starts_with?('i') && !val_llvm_type.includes?(".union")
              # Integer to pointer conversion needed (inttoptr)
              c = @cond_counter
              @cond_counter += 1
              emit "%ret_inttoptr.#{c} = inttoptr #{val_llvm_type} #{val_ref} to ptr"
              emit "ret ptr %ret_inttoptr.#{c}"
            elsif @current_return_type.starts_with?('i') && val_llvm_type == "ptr"
              # Pointer to integer conversion — decide ptrtoint vs load.
              # If MIR type is scalar or value was created by inttoptr → packed scalar → ptrtoint
              # Otherwise (GEP, alloca, etc.) → pointer to data → load
              c = @cond_counter
              @cond_counter += 1
              mir_ret_type = val_type ? @type_mapper.llvm_type(val_type) : "ptr"
              is_packed = (mir_ret_type.starts_with?('i') && !mir_ret_type.includes?(".union")) ||
                          mir_ret_type == "float" || mir_ret_type == "double" ||
                          (val && @inttoptr_value_ids.includes?(val))
              if is_packed
                emit "%ret_ptrtoint.#{c} = ptrtoint ptr #{val_ref} to #{@current_return_type}"
                emit "ret #{@current_return_type} %ret_ptrtoint.#{c}"
              else
                emit "%ret_load.#{c} = load #{@current_return_type}, ptr #{val_ref}"
                emit "ret #{@current_return_type} %ret_load.#{c}"
              end
            elsif @current_return_type.starts_with?('i') && val_llvm_type && val_llvm_type.includes?(".union")
              # Union to integer - extract payload as the integer type
              c = @cond_counter
              @cond_counter += 1
              emit "%ret_union_extract.#{c}.ptr = alloca #{val_llvm_type}, align 8"
              emit "store #{val_llvm_type} #{val_ref}, ptr %ret_union_extract.#{c}.ptr"
              emit "%ret_union_extract.#{c}.payload_ptr = getelementptr #{val_llvm_type}, ptr %ret_union_extract.#{c}.ptr, i32 0, i32 1"
              emit "%ret_union_extract.#{c}.val = load #{@current_return_type}, ptr %ret_union_extract.#{c}.payload_ptr, align 4"
              emit "ret #{@current_return_type} %ret_union_extract.#{c}.val"
            elsif @current_return_type == "ptr" && val_llvm_type && val_llvm_type.includes?(".union")
              # Union to pointer - extract payload as ptr
              c = @cond_counter
              @cond_counter += 1
              emit "%ret_union_to_ptr.#{c}.ptr = alloca #{val_llvm_type}, align 8"
              emit "store #{val_llvm_type} #{val_ref}, ptr %ret_union_to_ptr.#{c}.ptr"
              emit "%ret_union_to_ptr.#{c}.payload_ptr = getelementptr #{val_llvm_type}, ptr %ret_union_to_ptr.#{c}.ptr, i32 0, i32 1"
              emit "%ret_union_to_ptr.#{c}.val = load ptr, ptr %ret_union_to_ptr.#{c}.payload_ptr, align 4"
              emit "ret ptr %ret_union_to_ptr.#{c}.val"
            elsif (@current_return_type == "float" || @current_return_type == "double") && val_llvm_type && val_llvm_type.includes?(".union")
              # Union to float/double - extract payload as the float type
              c = @cond_counter
              @cond_counter += 1
              emit "%ret_union_to_float.#{c}.ptr = alloca #{val_llvm_type}, align 8"
              emit "store #{val_llvm_type} #{val_ref}, ptr %ret_union_to_float.#{c}.ptr"
              emit "%ret_union_to_float.#{c}.payload_ptr = getelementptr #{val_llvm_type}, ptr %ret_union_to_float.#{c}.ptr, i32 0, i32 1"
              emit "%ret_union_to_float.#{c}.val = load #{@current_return_type}, ptr %ret_union_to_float.#{c}.payload_ptr, align 4"
              emit "ret #{@current_return_type} %ret_union_to_float.#{c}.val"
            elsif @current_return_type.includes?(".union") && val_llvm_type && val_llvm_type.includes?(".union") && @current_return_type != val_llvm_type
              # Union to different union - preserve payload and remap type_id.
              c = @cond_counter
              @cond_counter += 1
              # Store source union to get its pointers
              emit "%ret_union_conv.#{c}.src_ptr = alloca #{val_llvm_type}, align 8"
              emit "store #{val_llvm_type} #{val_ref}, ptr %ret_union_conv.#{c}.src_ptr"
              # Get source type_id
              emit "%ret_union_conv.#{c}.src_type_ptr = getelementptr #{val_llvm_type}, ptr %ret_union_conv.#{c}.src_ptr, i32 0, i32 0"
              emit "%ret_union_conv.#{c}.type_id = load i32, ptr %ret_union_conv.#{c}.src_type_ptr"
              mapped_type_id = emit_union_type_id_remap(val_llvm_type, @current_return_type, "%ret_union_conv.#{c}.type_id", "ret_union_conv.#{c}")
              # Create destination union
              emit "%ret_union_conv.#{c}.dst_ptr = alloca #{@current_return_type}, align 8"
              # Store type_id to destination
              emit "%ret_union_conv.#{c}.dst_type_ptr = getelementptr #{@current_return_type}, ptr %ret_union_conv.#{c}.dst_ptr, i32 0, i32 0"
              emit "store i32 #{mapped_type_id}, ptr %ret_union_conv.#{c}.dst_type_ptr"
              # Copy payload bytes from source to destination (use memcpy or byte-wise copy via ptr)
              emit "%ret_union_conv.#{c}.src_payload_ptr = getelementptr #{val_llvm_type}, ptr %ret_union_conv.#{c}.src_ptr, i32 0, i32 1"
              emit "%ret_union_conv.#{c}.dst_payload_ptr = getelementptr #{@current_return_type}, ptr %ret_union_conv.#{c}.dst_ptr, i32 0, i32 1"
              emit "%ret_union_conv.#{c}.payload_as_ptr = load ptr, ptr %ret_union_conv.#{c}.src_payload_ptr, align 4"
              emit "store ptr %ret_union_conv.#{c}.payload_as_ptr, ptr %ret_union_conv.#{c}.dst_payload_ptr, align 4"
              # Load and return destination union
              emit "%ret_union_conv.#{c}.result = load #{@current_return_type}, ptr %ret_union_conv.#{c}.dst_ptr"
              emit "ret #{@current_return_type} %ret_union_conv.#{c}.result"
            elsif @current_return_type.starts_with?('i') && val_llvm_type && val_llvm_type.starts_with?('i') && @current_return_type != val_llvm_type
              # Integer width mismatch - need sext or trunc
              ret_bits = @current_return_type[1..].to_i
              val_bits = val_llvm_type[1..].to_i
              c = @cond_counter
              @cond_counter += 1
              if ret_bits > val_bits
                emit "%ret_sext.#{c} = sext #{val_llvm_type} #{val_ref} to #{@current_return_type}"
                emit "ret #{@current_return_type} %ret_sext.#{c}"
              else
                emit "%ret_trunc.#{c} = trunc #{val_llvm_type} #{val_ref} to #{@current_return_type}"
                emit "ret #{@current_return_type} %ret_trunc.#{c}"
              end
            elsif (@current_return_type == "double" || @current_return_type == "float") && val_llvm_type && val_llvm_type.starts_with?('i')
              # Integer to float conversion (signed/unsigned)
              c = @cond_counter
              @cond_counter += 1
              op = (val_type && unsigned_type_ref?(val_type)) ? "uitofp" : "sitofp"
              emit "%ret_itof.#{c} = #{op} #{val_llvm_type} #{val_ref} to #{@current_return_type}"
              emit "ret #{@current_return_type} %ret_itof.#{c}"
            elsif val_llvm_type && (val_llvm_type == "double" || val_llvm_type == "float") && @current_return_type.starts_with?('i')
              # Float to integer conversion (signed/unsigned)
              c = @cond_counter
              @cond_counter += 1
              unsigned = unsigned_type_ref?(@current_return_type_ref)
              op = unsigned ? "fptoui" : "fptosi"
              emit "%ret_ftoi.#{c} = #{op} #{val_llvm_type} #{val_ref} to #{@current_return_type}"
              emit "ret #{@current_return_type} %ret_ftoi.#{c}"
            elsif (@current_return_type == "double" || @current_return_type == "float") && val_llvm_type == "ptr"
              # Pointer to float conversion - interpret pointer value as unsigned and convert.
              # Avoids dereferencing unknown pointers from incomplete bodies.
              c = @cond_counter
              @cond_counter += 1
              emit "%ret_ptr_to_float.#{c}.int = ptrtoint ptr #{val_ref} to i64"
              emit "%ret_ptr_to_float.#{c} = uitofp i64 %ret_ptr_to_float.#{c}.int to #{@current_return_type}"
              emit "ret #{@current_return_type} %ret_ptr_to_float.#{c}"
            elsif @current_return_type == "ptr" && val_llvm_type == "double"
              # Double to ptr conversion - bitcast to i64 then inttoptr
              # This happens with abstract numeric types like Int that return `: self`
              c = @cond_counter
              @cond_counter += 1
              emit "%ret_float_to_ptr.#{c}.bits = bitcast double #{val_ref} to i64"
              emit "%ret_float_to_ptr.#{c} = inttoptr i64 %ret_float_to_ptr.#{c}.bits to ptr"
              emit "ret ptr %ret_float_to_ptr.#{c}"
            elsif @current_return_type == "ptr" && val_llvm_type == "float"
              # Float to ptr conversion - bitcast to i32 then inttoptr
              c = @cond_counter
              @cond_counter += 1
              emit "%ret_float_to_ptr.#{c}.bits = bitcast float #{val_ref} to i32"
              emit "%ret_float_to_ptr.#{c} = inttoptr i32 %ret_float_to_ptr.#{c}.bits to ptr"
              emit "ret ptr %ret_float_to_ptr.#{c}"
            elsif @current_return_type == "float" && val_llvm_type == "double"
              # Double to float conversion (truncate)
              c = @cond_counter
              @cond_counter += 1
              emit "%ret_fptrunc.#{c} = fptrunc double #{val_ref} to float"
              emit "ret float %ret_fptrunc.#{c}"
            elsif @current_return_type == "double" && val_llvm_type == "float"
              # Float to double conversion (extend)
              c = @cond_counter
              @cond_counter += 1
              emit "%ret_fpext.#{c} = fpext float #{val_ref} to double"
              emit "ret double %ret_fpext.#{c}"
            else
              if @current_return_type.includes?(".union") && (val_ref == "0" || val_ref == "null")
                nil_vid = nil_variant_id_for_union_type(@current_return_type)
                if nil_vid && nil_vid != 0
                  c = @cond_counter
                  @cond_counter += 1
                  emit "%ret_nil_lit.#{c}.ptr = alloca #{@current_return_type}, align 8"
                  emit "store #{@current_return_type} zeroinitializer, ptr %ret_nil_lit.#{c}.ptr"
                  emit "%ret_nil_lit.#{c}.tid = getelementptr #{@current_return_type}, ptr %ret_nil_lit.#{c}.ptr, i32 0, i32 0"
                  emit "store i32 #{nil_vid}, ptr %ret_nil_lit.#{c}.tid"
                  emit "%ret_nil_lit.#{c}.val = load #{@current_return_type}, ptr %ret_nil_lit.#{c}.ptr"
                  emit "ret #{@current_return_type} %ret_nil_lit.#{c}.val"
                else
                  emit "ret #{@current_return_type} zeroinitializer"
                end
                return
              end
              # Handle literal values for return types
              actual_val = if (@current_return_type == "double" || @current_return_type == "float") && (val_ref == "0" || val_ref == "null")
                             "0.0"
                           elsif @current_return_type == "ptr" && val_ref == "0"
                             "null"
                           elsif @current_return_type.starts_with?('i') && val_ref == "null"
                             "0"
                           else
                             val_ref
                           end
              emit "ret #{@current_return_type} #{actual_val}"
            end
          end
          end  # End of else block for defined value
        else
          # No return value specified - emit appropriate return based on function return type
          if @current_return_type == "void"
            emit "ret void"
          elsif @current_return_type == "ptr"
            emit "ret ptr null"
          elsif @current_return_type.starts_with?('i')
            emit "ret #{@current_return_type} 0"
          elsif @current_return_type.includes?(".union")
            # Return nil union (type_id = 0, matches variant table convention)
            c = @cond_counter
            @cond_counter += 1
            emit "%ret_nil_union.#{c}.ptr = alloca #{@current_return_type}, align 8"
            emit "%ret_nil_union.#{c}.type_id_ptr = getelementptr #{@current_return_type}, ptr %ret_nil_union.#{c}.ptr, i32 0, i32 0"
            emit "store i32 0, ptr %ret_nil_union.#{c}.type_id_ptr"
            emit "%ret_nil_union.#{c}.val = load #{@current_return_type}, ptr %ret_nil_union.#{c}.ptr"
            emit "ret #{@current_return_type} %ret_nil_union.#{c}.val"
          elsif @current_return_type == "double" || @current_return_type == "float"
            emit "ret #{@current_return_type} 0.0"
          else
            # Fallback - try ptr null
            emit "ret ptr null"
          end
        end
      when Jump
        emit "br label %#{@block_names[term.target]}"
      when Branch
        cond = value_ref(term.condition)
        cond_type = @value_types[term.condition]?
        then_block = @block_names[term.then_block]
        else_block = @block_names[term.else_block]

        # If the condition comes from a cross-block slot, prefer the slot's LLVM type.
        slot_llvm_type = @cross_block_slot_types[term.condition]?

        # Check the actual emitted type of cond — value_ref may have already extracted
        # a ptr from a union slot (when @value_types differs from slot type).
        # In that case, use the emitted type, not the slot type.
        actual_cond_type = @emitted_value_types[cond]?

        # Check if condition is a union type - need to extract type_id and compare
        # Also verify LLVM type is actually a union struct (not just ptr)
        union_llvm_type = if actual_cond_type
                            # value_ref already cast the value — use the actual emitted type
                            actual_cond_type.includes?(".union") ? actual_cond_type : nil
                          else
                            slot_llvm_type || (cond_type ? @type_mapper.llvm_type(cond_type) : nil)
                          end
        if union_llvm_type && union_llvm_type.includes?(".union")
          # Union layout: { i32 type_id, [N x i8] payload }.
          # Nil is not guaranteed to have variant_id=0, so resolve it from type.
          c = @cond_counter
          @cond_counter += 1
          nil_vid = nil_variant_id_for_union_type(union_llvm_type) || 0
          emit "%cond#{c}.union_ptr = alloca #{union_llvm_type}, align 8"
          emit "store #{union_llvm_type} #{normalize_union_value(cond, union_llvm_type)}, ptr %cond#{c}.union_ptr"
          emit "%cond#{c}.type_id_ptr = getelementptr #{union_llvm_type}, ptr %cond#{c}.union_ptr, i32 0, i32 0"
          emit "%cond#{c}.type_id = load i32, ptr %cond#{c}.type_id_ptr"
          emit "%cond#{c}.is_not_nil = icmp ne i32 %cond#{c}.type_id, #{nil_vid}"
          emit "br i1 %cond#{c}.is_not_nil, label %#{then_block}, label %#{else_block}"
        elsif (actual_cond_type == "ptr") ||
              (cond_type && @type_mapper.llvm_type(cond_type) == "ptr" && (slot_llvm_type.nil? || slot_llvm_type == "ptr"))
          # Pointer type: compare against null
          c = @cond_counter
          @cond_counter += 1
          cond_ptr = cond == "0" ? "null" : cond
          emit "%cond#{c}.not_null = icmp ne ptr #{cond_ptr}, null"
          emit "br i1 %cond#{c}.not_null, label %#{then_block}, label %#{else_block}"
        elsif cond_type
          cond_llvm_type = @type_mapper.llvm_type(cond_type)
          # Handle void type - void condition means the value was never defined
          # (e.g., void call used as condition). Treat as falsy (branch to else)
          if cond_llvm_type == "void"
            emit "; void condition - treating as falsy"
            emit "br label %#{else_block}"
          elsif cond_llvm_type != "i1"
            # Non-bool type: compare against 0 to get boolean
            c = @cond_counter
            @cond_counter += 1
            if cond_llvm_type == "double" || cond_llvm_type == "float"
              emit "%cond#{c}.not_zero = fcmp one #{cond_llvm_type} #{cond}, 0.0"
            elsif cond_llvm_type == "ptr"
              emit "%cond#{c}.not_zero = icmp ne ptr #{cond}, null"
            else
              emit "%cond#{c}.not_zero = icmp ne #{cond_llvm_type} #{cond}, 0"
            end
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
        val_type = @value_types[term.value]? || TypeRef::INT64
        val_llvm_type = @type_mapper.llvm_type(val_type)
        emit "switch #{val_llvm_type} #{val}, label %#{default} ["
        @indent += 1
        # Deduplicate case values — LLVM requires unique switch case values.
        # MIR may produce duplicates when multiple subtypes share the same type_id.
        seen_cases = Set(Int64).new
        term.cases.each do |(case_val, block)|
          next if seen_cases.includes?(case_val)
          seen_cases << case_val
          emit "#{val_llvm_type} #{case_val}, label %#{@block_names[block]}"
        end
        @indent -= 1
        emit "]"
      when Unreachable
        # For void functions, a block with Unreachable terminator (the default)
        # may simply be missing an explicit Return. Emit ret void to prevent
        # trap/crash when the block is actually reached at runtime (e.g., GC callbacks).
        if @current_return_type == "void"
          emit "ret void"
        else
          emit "unreachable"
        end
      end
    end

    private def value_ref(id : ValueId) : String
      # Check if it's a constant (inline the value)
      if const_val = @constant_values[id]?
        # If this constant was made addressable via pointerof() and the alloca has been
        # initialized, the value may have been modified through the pointer (e.g.,
        # copy_from writes into the alloca). Load from the alloca to get current value.
        if !@in_phi_mode && @addressable_alloca_initialized.includes?(id)
          if alloca_name = @addressable_allocas[id]?
            val_type = @value_types[id]?
            llvm_type = val_type ? @type_mapper.llvm_type(val_type) : "i32"
            if llvm_type != "void" && llvm_type != "ptr"
              temp = "%r#{id}.addrload.#{@cond_counter}"
              @cond_counter += 1
              emit "#{temp} = load #{llvm_type}, ptr #{alloca_name}"
              return temp
            end
          end
        end
        if const_val == "0"
          if val_type = @value_types[id]?
            llvm_type = @type_mapper.llvm_type(val_type)
            return "null" if llvm_type == "ptr"
          end
        end
        return const_val
      end
      # Check if this was a void call - return safe default literal
      if @void_values.includes?(id)
        return "0"
      end
      # For cross-block values in phi mode, use direct value reference.
      # Phi nodes are specifically designed to handle values from different paths.
      # Only load from slot for non-phi uses (when @in_phi_mode is false).
      if slot_name = @cross_block_slots[id]?
        if @in_phi_mode
          # In phi mode, use the direct value if it was emitted
          if name = @value_names[id]?
            return "%#{name}"
          end
          # Value not emitted yet (forward reference from loop back-edge)
          # For phi nodes, return the name that WILL be assigned to this value
          # LLVM phi nodes support forward references
          return "%r#{id}"
        end
        # Non-phi use: load from slot to handle dominance issues
        val_type = @value_types[id]?
        slot_type_ref = @cross_block_slot_type_refs[id]?
        llvm_type = @cross_block_slot_types[id]? ||
          (val_type ? @type_mapper.llvm_type(val_type) : "i64")
        llvm_type = "i64" if llvm_type == "void"
        temp_name = "%r#{id}.fromslot.#{@cond_counter}"
        @cond_counter += 1
        emit "#{temp_name} = load #{llvm_type}, ptr %#{slot_name}"
        record_emitted_type(temp_name, llvm_type)
        # If the slot type differs from the expected value type, insert a cast.
        expected_type = val_type ? @type_mapper.llvm_type(val_type) : llvm_type
        if expected_type != llvm_type && expected_type != "void"
          cast_name = "%r#{id}.fromslot.cast.#{@cond_counter}"
          @cond_counter += 1
          if llvm_type.starts_with?('i') && expected_type.starts_with?('i')
            src_bits = llvm_type[1..].to_i?
            dst_bits = expected_type[1..].to_i?
            if src_bits && dst_bits
              if dst_bits < src_bits
                emit "#{cast_name} = trunc #{llvm_type} #{temp_name} to #{expected_type}"
              elsif dst_bits > src_bits
                ext_op = (slot_type_ref && unsigned_type_ref?(slot_type_ref)) ? "zext" : "sext"
                emit "#{cast_name} = #{ext_op} #{llvm_type} #{temp_name} to #{expected_type}"
              else
                emit "#{cast_name} = add #{expected_type} #{temp_name}, 0"
              end
              record_emitted_type(cast_name, expected_type)
              return cast_name
            end
          elsif llvm_type.starts_with?('i') && (expected_type == "float" || expected_type == "double")
            op = (val_type && unsigned_type_ref?(val_type)) ? "uitofp" : "sitofp"
            emit "#{cast_name} = #{op} #{llvm_type} #{temp_name} to #{expected_type}"
            record_emitted_type(cast_name, expected_type)
            return cast_name
          elsif (llvm_type == "float" || llvm_type == "double") && expected_type.starts_with?('i')
            op = (val_type && unsigned_type_ref?(val_type)) ? "fptoui" : "fptosi"
            emit "#{cast_name} = #{op} #{llvm_type} #{temp_name} to #{expected_type}"
            record_emitted_type(cast_name, expected_type)
            return cast_name
          elsif llvm_type == "float" && expected_type == "double"
            emit "#{cast_name} = fpext float #{temp_name} to double"
            record_emitted_type(cast_name, expected_type)
            return cast_name
          elsif llvm_type == "double" && expected_type == "float"
            emit "#{cast_name} = fptrunc double #{temp_name} to float"
            record_emitted_type(cast_name, expected_type)
            return cast_name
          elsif expected_type == "ptr" && llvm_type.starts_with?('i')
            emit "#{cast_name} = inttoptr #{llvm_type} #{temp_name} to ptr"
            record_emitted_type(cast_name, "ptr")
            return cast_name
          elsif llvm_type == "ptr" && expected_type.starts_with?('i')
            emit "#{cast_name} = ptrtoint ptr #{temp_name} to #{expected_type}"
            record_emitted_type(cast_name, expected_type)
            return cast_name
          elsif expected_type == "ptr" && llvm_type.includes?(".union")
            # Extract pointer payload from union value.
            union_ptr = "%r#{id}.fromslot.union_ptr.#{@cond_counter}"
            @cond_counter += 1
            emit "#{union_ptr} = alloca #{llvm_type}, align 8"
            emit "store #{llvm_type} #{temp_name}, ptr #{union_ptr}"
            emit "#{cast_name}.payload_ptr = getelementptr #{llvm_type}, ptr #{union_ptr}, i32 0, i32 1"
            emit "#{cast_name} = load ptr, ptr #{cast_name}.payload_ptr, align 4"
            record_emitted_type(cast_name, "ptr")
            return cast_name
          elsif (expected_type == "double" || expected_type == "float" || (expected_type.starts_with?('i') && !expected_type.includes?('.'))) && llvm_type.includes?(".union")
            # Extract scalar payload from union value.
            union_ptr = "%r#{id}.fromslot.union_ptr.#{@cond_counter}"
            @cond_counter += 1
            emit "#{union_ptr} = alloca #{llvm_type}, align 8"
            emit "store #{llvm_type} #{temp_name}, ptr #{union_ptr}"
            emit "#{cast_name}.payload_ptr = getelementptr #{llvm_type}, ptr #{union_ptr}, i32 0, i32 1"
            emit "#{cast_name} = load #{expected_type}, ptr #{cast_name}.payload_ptr, align 4"
            record_emitted_type(cast_name, expected_type)
            return cast_name
          elsif llvm_type.includes?(".union") && expected_type.includes?(".union") && llvm_type != expected_type
            # Different union types with same layout - reinterpret through memory
            # (LLVM doesn't allow bitcast between aggregate types)
            u2u_ptr = "%r#{id}.fromslot.u2u.#{@cond_counter}"
            @cond_counter += 1
            emit "#{u2u_ptr} = alloca #{expected_type}, align 8"
            emit "store #{llvm_type} #{temp_name}, ptr #{u2u_ptr}"
            emit "#{cast_name} = load #{expected_type}, ptr #{u2u_ptr}"
            record_emitted_type(cast_name, expected_type)
            return cast_name
          end
        end
        return temp_name
      end
      # Otherwise reference by name
      if name = @value_names[id]?
        "%#{name}"
      else
        # Value was never emitted yet.
        # In phi mode: return forward reference (LLVM phi nodes support this for loop back-edges)
        # Outside phi mode: return default value
        if @in_phi_mode
          # Forward reference for phi node from loop back-edge
          # The value will be assigned to %r#{id} when its block is emitted
          return "%r#{id}"
        end
        # Not in phi mode - this is likely an unreachable instruction or
        # a call that was skipped. Check if we know the type and return appropriate default.
        val_type = @value_types[id]?
        if val_type
          llvm_type = @type_mapper.llvm_type(val_type)
          if llvm_type == "ptr" || llvm_type == "void"
            return "null"
          elsif llvm_type.includes?(".union")
            # Can't use zeroinitializer inline, emit a placeholder that will cause obvious error
            return "null"
          elsif llvm_type == "float" || llvm_type == "double"
            return "0.0"
          else
            return "0"
          end
        else
          # No type info either - use null as safest default for pointer context
          # This happens when instruction was never emitted (skipped due to void type)
          return "null"
        end
      end
    end

    private def find_def_inst(id : ValueId)
      if block_id = @value_def_block[id]?
        if block = @current_func_blocks[block_id]?
          return block.instructions.find { |inst| inst.id == id }
        end
      end
      nil
    end

    # Check if LLVM type string represents a union type
    # Union types are named with ".union" suffix (e.g., "%Int32_$OR$_Nil.union")
    # This is more robust than simple string matching - checks for the pattern
    # at the end of a type reference to avoid false positives with user types
    private def is_union_llvm_type?(llvm_type : String) : Bool
      # Union types end with ".union" or ".union}" for struct fields
      llvm_type.ends_with?(".union") || llvm_type.ends_with?(".union}")
    end

    private def tuple_struct_llvm_type(type : Type) : String
      if elements = type.element_types
        element_types = elements.map { |e|
          t = @type_mapper.llvm_type(e)
          t == "void" ? "i8" : t  # Preserve struct index positions for Nil elements
        }
        return "{}" if element_types.empty?
        "{ #{element_types.join(", ")} }"
      else
        "{}"
      end
    end

    # Look up the LLVM type string for a value, with fallback to parameter lookup
    private def lookup_value_llvm_type(id : ValueId, default : String = "i32") : String
      # First check @value_types
      if type_ref = @value_types[id]?
        return @type_mapper.llvm_type(type_ref)
      end

      # Check if value corresponds to a parameter by checking value_names
      if name = @value_names[id]?
        if param_type = current_func_param_type_by_llvm_name(name)
          return @type_mapper.llvm_type(param_type)
        end
      end

      # Fallback
      default
    end

    # Normalize a value for storing into a union type
    # LLVM requires zeroinitializer for struct types, not integer literals like 0
    private def normalize_union_value(val : String, type_str : String) : String
      if type_str.includes?(".union") && (val == "0" || val == "null")
        "zeroinitializer"
      else
        val
      end
    end

    private def union_variant_tokens_for_llvm_union(llvm_type : String) : Array(String)
      name = llvm_type.lstrip('%').chomp(".union")
      return [] of String if name.empty?
      name.split("$_$OR$_")
    end

    private def union_type_id_remap_pairs_from_descriptors(
      src_union_ref : TypeRef,
      dst_union_ref : TypeRef,
    ) : Array({Int32, Int32})?
      src_desc = @module.get_union_descriptor(src_union_ref)
      dst_desc = @module.get_union_descriptor(dst_union_ref)
      return nil unless src_desc && dst_desc

      dst_by_name = {} of String => Int32
      dst_desc.variants.each do |variant|
        dst_by_name[variant.full_name] = variant.type_id unless dst_by_name.has_key?(variant.full_name)
      end

      remap_pairs = [] of {Int32, Int32}
      src_desc.variants.each do |variant|
        dst_id = dst_by_name[variant.full_name]?
        if dst_id.nil?
          if by_ref = dst_desc.variants.find { |v| v.type_ref == variant.type_ref }
            dst_id = by_ref.type_id
          end
        end
        remap_pairs << {variant.type_id, dst_id || variant.type_id}
      end

      remap_pairs
    end

    private def union_type_id_remap_needed?(src_union_ref : TypeRef, dst_union_ref : TypeRef) : Bool
      return false if src_union_ref == dst_union_ref
      return false unless is_union_type?(src_union_ref) && is_union_type?(dst_union_ref)
      pairs = union_type_id_remap_pairs_from_descriptors(src_union_ref, dst_union_ref)
      return false unless pairs
      pairs.any? { |(src_idx, dst_idx)| src_idx != dst_idx }
    end

    private def emit_union_type_id_remap(
      src_union_type : String,
      dst_union_type : String,
      src_type_id_value : String,
      temp_prefix : String,
      src_union_ref : TypeRef? = nil,
      dst_union_ref : TypeRef? = nil,
    ) : String
      remap_pairs = nil.as(Array({Int32, Int32})?)
      if src_union_ref && dst_union_ref
        remap_pairs = union_type_id_remap_pairs_from_descriptors(src_union_ref, dst_union_ref)
      end

      if remap_pairs.nil?
        return src_type_id_value if src_union_type == dst_union_type

        src_variants = union_variant_tokens_for_llvm_union(src_union_type)
        dst_variants = union_variant_tokens_for_llvm_union(dst_union_type)
        return src_type_id_value if src_variants.empty? || dst_variants.empty?

        dst_index_by_name = {} of String => Int32
        dst_variants.each_with_index do |variant, idx|
          dst_index_by_name[variant] = idx.to_i32 unless dst_index_by_name.has_key?(variant)
        end

        pairs = [] of {Int32, Int32}
        src_variants.each_with_index do |variant, src_idx|
          dst_idx = dst_index_by_name[variant]? || src_idx.to_i32
          pairs << {src_idx.to_i32, dst_idx}
        end
        remap_pairs = pairs
      end

      return src_type_id_value if remap_pairs.all? { |(src_idx, dst_idx)| src_idx == dst_idx }

      mapped = src_type_id_value
      remap_pairs.each_with_index do |(src_idx, dst_idx), idx|
        next if src_idx == dst_idx
        cmp_reg = "%#{temp_prefix}.tid_cmp#{idx}"
        sel_reg = "%#{temp_prefix}.tid_map#{idx}"
        emit "#{cmp_reg} = icmp eq i32 #{mapped}, #{src_idx}"
        emit "#{sel_reg} = select i1 #{cmp_reg}, i32 #{dst_idx}, i32 #{mapped}"
        mapped = sel_reg
      end

      mapped
    end

    # Find the variant index (type_id) for Nil within a union LLVM type name.
    # Union type names list variants alphabetically separated by $_$OR$_.
    # Returns nil if the union doesn't contain a Nil variant.
    private def nil_variant_id_for_union_type(llvm_type : String) : Int32?
      variants = union_variant_tokens_for_llvm_union(llvm_type)
      variants.each_with_index do |v, i|
        # Strip generic args ($L...$R) and namespace separators ($CC)
        base = v.split("$L").first.split("$CC").last
        return i if base == "Nil"
      end
      nil
    end

    # Find first non-Nil variant index for a union LLVM type name.
    private def first_non_nil_variant_id_for_union_type(llvm_type : String) : Int32?
      variants = union_variant_tokens_for_llvm_union(llvm_type)
      variants.each_with_index do |v, i|
        base = v.split("$L").first.split("$CC").last
        return i unless base == "Nil"
      end
      nil
    end

    # Coerce a union value to a specific union LLVM type when the emitted source type
    # differs from the expected destination union type.
    private def coerce_union_value_for_type(base_name : String, val : String, expected_union_type : String) : String
      actual_union_type = @emitted_value_types[val]?
      if actual_union_type.nil?
        if m = val.match(/^%r(\d+)/)
          vid = ValueId.new(m[1].to_u32)
          if tref = @value_types[vid]?
            actual_union_type = @type_mapper.llvm_type(tref)
          end
        end
      end
      if actual_union_type.nil? && (val == "0" || val == "null")
        actual_union_type = "ptr"
      end
      actual_union_type ||= expected_union_type
      if actual_union_type.includes?(".union") && actual_union_type != expected_union_type
        emit "%#{base_name}.u2u_src_ptr = alloca #{actual_union_type}, align 8"
        emit "store #{actual_union_type} #{normalize_union_value(val, actual_union_type)}, ptr %#{base_name}.u2u_src_ptr"
        emit "%#{base_name}.u2u_src_type_id_ptr = getelementptr #{actual_union_type}, ptr %#{base_name}.u2u_src_ptr, i32 0, i32 0"
        emit "%#{base_name}.u2u_src_type_id = load i32, ptr %#{base_name}.u2u_src_type_id_ptr"
        mapped_type_id = emit_union_type_id_remap(actual_union_type, expected_union_type, "%#{base_name}.u2u_src_type_id", "#{base_name}.u2u")
        emit "%#{base_name}.u2u_src_payload_ptr = getelementptr #{actual_union_type}, ptr %#{base_name}.u2u_src_ptr, i32 0, i32 1"
        emit "%#{base_name}.u2u_payload_as_ptr = load ptr, ptr %#{base_name}.u2u_src_payload_ptr, align 4"
        emit "%#{base_name}.u2u_dst_ptr = alloca #{expected_union_type}, align 8"
        emit "%#{base_name}.u2u_dst_type_id_ptr = getelementptr #{expected_union_type}, ptr %#{base_name}.u2u_dst_ptr, i32 0, i32 0"
        emit "store i32 #{mapped_type_id}, ptr %#{base_name}.u2u_dst_type_id_ptr"
        emit "%#{base_name}.u2u_dst_payload_ptr = getelementptr #{expected_union_type}, ptr %#{base_name}.u2u_dst_ptr, i32 0, i32 1"
        emit "store ptr %#{base_name}.u2u_payload_as_ptr, ptr %#{base_name}.u2u_dst_payload_ptr, align 4"
        emit "%#{base_name}.u2u_val = load #{expected_union_type}, ptr %#{base_name}.u2u_dst_ptr"
        "%#{base_name}.u2u_val"
      elsif expected_union_type.includes?(".union") && !actual_union_type.includes?(".union")
        # Scalar/pointer to union wrapping.
        nil_variant = nil_variant_id_for_union_type(expected_union_type) || 0
        non_nil_variant = first_non_nil_variant_id_for_union_type(expected_union_type) || (nil_variant == 0 ? 1 : 0)
        emit "%#{base_name}.s2u_ptr = alloca #{expected_union_type}, align 8"
        emit "store #{expected_union_type} zeroinitializer, ptr %#{base_name}.s2u_ptr"
        emit "%#{base_name}.s2u_type_id_ptr = getelementptr #{expected_union_type}, ptr %#{base_name}.s2u_ptr, i32 0, i32 0"

        if val == "null" || val == "0"
          emit "store i32 #{nil_variant}, ptr %#{base_name}.s2u_type_id_ptr"
        elsif actual_union_type == "ptr"
          emit "%#{base_name}.s2u_is_nil = icmp eq ptr #{val}, null"
          emit "%#{base_name}.s2u_type_id = select i1 %#{base_name}.s2u_is_nil, i32 #{nil_variant}, i32 #{non_nil_variant}"
          emit "store i32 %#{base_name}.s2u_type_id, ptr %#{base_name}.s2u_type_id_ptr"
          emit "%#{base_name}.s2u_payload_ptr = getelementptr #{expected_union_type}, ptr %#{base_name}.s2u_ptr, i32 0, i32 1"
          emit "store ptr #{val}, ptr %#{base_name}.s2u_payload_ptr, align 4"
        else
          emit "store i32 #{non_nil_variant}, ptr %#{base_name}.s2u_type_id_ptr"
          emit "%#{base_name}.s2u_payload_ptr = getelementptr #{expected_union_type}, ptr %#{base_name}.s2u_ptr, i32 0, i32 1"
          if actual_union_type == "void"
            emit "store i8 0, ptr %#{base_name}.s2u_payload_ptr, align 4"
          else
            emit "store #{actual_union_type} #{val}, ptr %#{base_name}.s2u_payload_ptr, align 4"
          end
        end

        emit "%#{base_name}.s2u_val = load #{expected_union_type}, ptr %#{base_name}.s2u_ptr"
        "%#{base_name}.s2u_val"
      else
        normalize_union_value(val, expected_union_type)
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
    # TYPE NAME TABLE (runtime type_id → class name lookup)
    # ═══════════════════════════════════════════════════════════════════════

    private def emit_type_name_table
      string_type_id = @string_type_id || 0

      # Collect all type_id → name mappings
      # Start with primitive types (IDs 0-18)
      type_names = {} of UInt32 => String
      primitive_names = {
         0_u32 => "Void",    1_u32 => "Nil",      2_u32 => "Bool",
         3_u32 => "Int8",    4_u32 => "Int16",     5_u32 => "Int32",
         6_u32 => "Int64",   7_u32 => "Int128",    8_u32 => "UInt8",
         9_u32 => "UInt16", 10_u32 => "UInt32",   11_u32 => "UInt64",
        12_u32 => "UInt128",13_u32 => "Float32",  14_u32 => "Float64",
        15_u32 => "Char",   16_u32 => "String",   17_u32 => "Symbol",
        18_u32 => "Pointer",
      }
      primitive_names.each { |id, name| type_names[id] = name }

      # Add types from the type registry
      @type_info_entries.each do |entry|
        # Look up name from string table using name_offset
        name = read_string_from_table(entry.name_offset)
        type_names[entry.type_id] = name unless name.empty?
      end

      return if type_names.empty?

      max_id = type_names.keys.max
      table_size = max_id + 1

      emit_raw "\n; Type name table: type_id → Crystal String pointer (for self.class)\n"

      # Emit Crystal String constants for each type name
      type_names.each do |tid, name|
        escaped = name.gsub("\\", "\\\\").gsub("\"", "\\22")
        len = name.bytesize + 1 # +1 for null terminator
        bytesize = name.bytesize
        charsize = name.size
        emit_raw "@.str.typename.#{tid} = private unnamed_addr constant { i32, i32, i32, [#{len} x i8] } { i32 #{string_type_id}, i32 #{bytesize}, i32 #{charsize}, [#{len} x i8] c\"#{escaped}\\00\" }, align 8\n"
      end

      # Emit the "Unknown" fallback string
      emit_raw "@.str.unknown_type = private unnamed_addr constant { i32, i32, i32, [8 x i8] } { i32 #{string_type_id}, i32 7, i32 7, [8 x i8] c\"Unknown\\00\" }, align 8\n"

      # Emit the table: array of ptrs indexed by type_id
      emit_raw "@__crystal_type_name_table = private constant [#{table_size} x ptr] [\n"
      table_size.times do |i|
        comma = i < table_size - 1 ? "," : ""
        if type_names.has_key?(i.to_u32)
          emit_raw "  ptr @.str.typename.#{i}#{comma}\n"
        else
          emit_raw "  ptr null#{comma}\n"
        end
      end
      emit_raw "]\n"

      # Emit the lookup function
      emit_raw "\ndefine ptr @__crystal_v2_type_name(i32 %tid) {\n"
      emit_raw "entry:\n"
      emit_raw "  %inbounds = icmp ult i32 %tid, #{table_size}\n"
      emit_raw "  br i1 %inbounds, label %lookup, label %unknown\n"
      emit_raw "lookup:\n"
      emit_raw "  %slot = getelementptr [#{table_size} x ptr], ptr @__crystal_type_name_table, i32 0, i32 %tid\n"
      emit_raw "  %str = load ptr, ptr %slot\n"
      emit_raw "  %nonnull = icmp ne ptr %str, null\n"
      emit_raw "  br i1 %nonnull, label %done, label %unknown\n"
      emit_raw "done:\n"
      emit_raw "  ret ptr %str\n"
      emit_raw "unknown:\n"
      emit_raw "  ret ptr @.str.unknown_type\n"
      emit_raw "}\n"
    end

    private def read_string_from_table(offset : UInt32) : String
      bytes = @string_table.to_slice
      return "" if offset >= bytes.size
      end_pos = offset
      while end_pos < bytes.size && bytes[end_pos] != 0
        end_pos += 1
      end
      String.new(bytes[offset...end_pos])
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
