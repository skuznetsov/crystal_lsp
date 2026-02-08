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

  class LLVMIRGenerator
    @module : Module
    @type_mapper : LLVMTypeMapper
    @output : IO::Memory
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
    @undefined_externs : Hash(String, String) = {} of String => String  # Track undefined extern calls (name => return_type)
    @global_name_mapping : Hash(String, String) = {} of String => String  # Map original global names to renamed names
    @alloc_element_types : Hash(ValueId, TypeRef)  # For GEP element type lookup
    @array_info : Hash(ValueId, {String, Int32})  # Array element_type and size
    @string_constants : Hash(String, String)  # String value -> global name
    @emitted_value_types : Hash(String, String)  # SSA name -> LLVM type (per function)
    @emitted_allocas : Set(ValueId) = Set(ValueId).new  # Track pre-emitted allocas
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

    # Cross-block value tracking for dominance fix
    @value_def_block : Hash(ValueId, BlockId) = {} of ValueId => BlockId  # value → block where defined
    @cross_block_values : Set(ValueId) = Set(ValueId).new  # values that need alloca slots
    @cross_block_slots : Hash(ValueId, String) = {} of ValueId => String  # value → alloca slot name
    @cross_block_slot_types : Hash(ValueId, String) = {} of ValueId => String  # value → slot LLVM type
    @cross_block_slot_type_refs : Hash(ValueId, TypeRef) = {} of ValueId => TypeRef  # value → slot TypeRef (signedness)
    @in_phi_mode : Bool = false  # When true, value_ref returns default instead of emitting load
    @in_phi_block : Bool = false  # When true, we're emitting phi instructions (defer cross-block stores)
    @deferred_phi_stores : Array(String) = [] of String  # Stores to emit after all phis

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
    @current_func_blocks : Hash(BlockId, BasicBlock) = {} of BlockId => BasicBlock
    @current_block_id : BlockId? = nil

    # Track phi nodes that have nil incoming values (for union return type handling)
    # Maps phi value_id -> set of blocks that contribute nil
    @phi_nil_incoming_blocks : Hash(ValueId, Set(BlockId)) = {} of ValueId => Set(BlockId)

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
      @emitted_value_types = {} of String => String

      # Type metadata
      @type_info_entries = [] of TypeInfoEntry
      @field_info_entries = [] of FieldInfoEntry
      @union_info_entries = [] of UnionInfoEntry
      @union_variant_entries = [] of UnionVariantInfoEntry
      @string_table = IO::Memory.new
      @string_offsets = {} of String => UInt32
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
      unresolved_patterns = ["typeof(", "typeof_"]

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

      emit_entrypoint_if_needed(functions_to_emit)

      # Emit string constants at end (LLVM allows globals anywhere)
      STDERR.puts "  [LLVM] emit_string_constants..." if @progress
      emit_string_constants

      # Emit declarations for undefined extern calls
      STDERR.puts "  [LLVM] emit_undefined_extern_declarations..." if @progress
      emit_undefined_extern_declarations

      if @emit_type_metadata
        STDERR.puts "  [LLVM] emit_type_metadata_globals..." if @progress
        emit_type_metadata_globals
        STDERR.puts "  [LLVM] emit_union_metadata_globals..." if @progress
        emit_union_metadata_globals
      end

      STDERR.puts "  [LLVM] finalizing output..." if @progress
      @output.to_s
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
      @module.functions.each do |f|
        func_by_id[f.id] = f
        mangled = @type_mapper.mangle_name(f.name)
        func_by_name[mangled] = f
        func_by_name[f.name] = f
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

              # Note: Disabled suffix matching as it causes false positives
              # Functions should be found by exact name match only

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
      already_declared << "setjmp" << "longjmp"
      # Crystal v2 runtime functions
      already_declared << "__crystal_v2_raise" << "__crystal_v2_int_to_string"
      already_declared << "__crystal_v2_string_concat" << "__crystal_v2_f64_to_string"
      already_declared << "__crystal_v2_char_to_string" << "__crystal_v2_malloc64"
      already_declared << "__crystal_v2_init_buffer" << "__crystal_v2_string_repeat"
      # Skip any function starting with __crystal_v2_ (runtime functions)
      runtime_prefix = "__crystal_v2_"

      emit_raw "\n; Forward declarations for undefined external functions\n"
      @undefined_externs.each do |name, return_type|
        # Skip if already emitted or declared
        next if already_declared.includes?(name)
        # Skip runtime functions
        next if name.starts_with?(runtime_prefix)
        already_declared << name

        # LLVM intrinsics need proper signatures (not varargs)
        if name.starts_with?("llvm.")
          # Normalize old typed pointer format (p0i8) to opaque pointer format (p0) for LLVM 15+
          normalized_name = name
            .gsub("p0i8.p0i8", "p0.p0")
            .gsub("p0i8", "p0")
          decl = emit_llvm_intrinsic_declaration(normalized_name)
          emit_raw "#{decl}\n" if decl
        elsif stub = emit_dead_code_stub(name, return_type)
          # Emit a stub function body for methods on impossible receivers
          # (Nil, Unknown, wrong-type dispatch). These arise from type inference
          # gaps where the compiler emits calls that would never execute at runtime.
          emit_raw stub
        else
          # Declare with varargs to accept any arguments
          emit_raw "declare #{return_type} @#{name}(...)\n"
        end
      end
    end

    # Emit a stub function body for methods called on Nil/Unknown/impossible receivers.
    # Returns the LLVM IR string for the stub, or nil if the name doesn't match.
    private def emit_dead_code_stub(name : String, return_type : String) : String?
      # Methods on Nil (e.g. Nil$Hzero, Nil$Hto_i, Nil$Hadditive_identity)
      # Methods on Unknown (e.g. Unknown$Hto_i, Unknown$Hto_u32)
      # Union dispatch on Nil|X (e.g. Nil$_$OR_...#call)
      is_nil_method = name.starts_with?("Nil$H") || name.starts_with?("Nil$_$OR$_")
      is_unknown_method = name.starts_with?("Unknown$H")
      # Wrong receiver: Int32#unsafe_fetch makes no sense (Array method on Int32)
      is_wrong_receiver = name == "Int32$Hunsafe_fetch$$Int32"

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

      return nil unless is_nil_method || is_unknown_method || is_wrong_receiver

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

      "; stub for dead-code method: #{name}\n" \
      "define #{return_type} @#{name}(...) {\n" \
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
          if expected_type.starts_with?("i") && actual_type.starts_with?("i")
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
      return true if name.includes?("#") || name.includes?(".") || name.includes?("::")
      # Type-suffixed or type-like: likely Crystal (Int32/UInt64/etc).
      return true if name.includes?("_Int") || name.includes?("_UInt") || name.includes?("_Float")
      # Uppercase letters are uncommon in C lib symbols, common in Crystal types.
      name.each_byte do |byte|
        return true if byte >= 'A'.ord && byte <= 'Z'.ord
      end
      false
    end


    private def emit_string_constants
      emit_raw "\n; String constants\n"
      # Always emit empty string constant (used for void interpolation parts)
      emit_raw "@.str.empty = private unnamed_addr constant [1 x i8] c\"\\00\", align 1\n"

      return if @string_constants.empty?

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

      # Collect all referenced globals from GlobalLoad/GlobalStore instructions
      referenced_globals = Hash(String, TypeRef).new
      @module.functions.each do |func|
        func.blocks.each do |block|
          block.instructions.each do |inst|
            case inst
            when GlobalLoad
              mangled = @type_mapper.mangle_name(inst.global_name)
              unless defined_globals.includes?(mangled)
                referenced_globals[mangled] = inst.type
              end
            when GlobalStore
              mangled = @type_mapper.mangle_name(inst.global_name)
              unless defined_globals.includes?(mangled)
                # For GlobalStore, we use value type if available, else ptr
                referenced_globals[mangled] ||= TypeRef::POINTER
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

      # Emit defined globals
      @module.globals.each do |global|
        llvm_type = @type_mapper.llvm_type(global.type)
        llvm_type = "ptr" if llvm_type == "void"
        initial = global.initial_value || 0_i64
        mangled_name = @type_mapper.mangle_name(global.name)
        actual_name = mangled_name
        # Avoid conflict with function names by prefixing with .global
        if function_names.includes?(mangled_name)
          actual_name = ".global.#{mangled_name}"
          @global_name_mapping[mangled_name] = actual_name
        end
        # Use zeroinitializer for struct/union types, numeric 0 for primitives
        if llvm_type.starts_with?("%") || llvm_type.starts_with?("{")
          emit_raw "@#{actual_name} = global #{llvm_type} zeroinitializer\n"
        elsif llvm_type == "ptr"
          emit_raw "@#{actual_name} = global #{llvm_type} null\n"
        elsif llvm_type == "float" || llvm_type == "double"
          float_value = initial.to_f.to_s
          float_value = "0.0" if float_value == "0"
          float_value = "#{float_value}.0" if float_value.matches?(/^-?\d+$/)
          emit_raw "@#{actual_name} = global #{llvm_type} #{float_value}\n"
        else
          emit_raw "@#{actual_name} = global #{llvm_type} #{initial}\n"
        end
      end

      # Emit undefined globals (class variables that weren't explicitly defined)
      referenced_globals.each do |name, type_ref|
        # Skip if it conflicts with a function name (likely a method accessor, not a variable)
        next if function_names.includes?(name)
        llvm_type = @type_mapper.llvm_type(type_ref)
        llvm_type = "ptr" if llvm_type == "void"
        # Use zeroinitializer for struct/union types, null for pointers, 0 for primitives
        if llvm_type == "ptr"
          emit_raw "@#{name} = global #{llvm_type} null\n"
        elsif llvm_type.starts_with?("%") || llvm_type.starts_with?("{")
          emit_raw "@#{name} = global #{llvm_type} zeroinitializer\n"
        elsif llvm_type == "float" || llvm_type == "double"
          emit_raw "@#{name} = global #{llvm_type} 0.0\n"
        else
          emit_raw "@#{name} = global #{llvm_type} 0\n"
        end
      end

      emit_raw "\n" unless @module.globals.empty? && referenced_globals.empty?
    end

    private def emit(s : String)
      @output << ("  " * @indent) << s << "\n"
    end

    private def emit_raw(s : String)
      @output << s
    end

    private def record_emitted_type(name : String, llvm_type : String)
      return unless name.starts_with?("%")
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
          llvm_type = @type_mapper.llvm_type(field.type_ref)
          # void is not valid for struct fields - use ptr instead (opaque pointer for untyped field)
          llvm_type = "ptr" if llvm_type == "void"
          field_types << llvm_type
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

    private def emit_union_type(type : Type, payload_size : UInt64? = nil)
      name = @type_mapper.mangle_name(type.name)
      # Union = { i32 discriminator, [max_size x i8] data }
      max_size = payload_size || type.variants.try(&.map(&.size).max) || 8_u64
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
      emit_raw "declare ptr @gets(ptr)\n"
      emit_raw "declare ptr @fgets(ptr, i32, ptr)\n"
      emit_raw "declare ptr @fputs(ptr, ptr)\n"
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

      # Memory allocation - use calloc for zero-initialized memory
      # Crystal semantics require zero-init (like GC_MALLOC in original compiler).
      # The .new allocator only sets known fields; union/composite gaps need zeroing.
      emit_raw "define ptr @__crystal_v2_malloc64(i64 %size) {\n"
      emit_raw "  %ptr = call ptr @calloc(i64 1, i64 %size)\n"
      emit_raw "  ret ptr %ptr\n"
      emit_raw "}\n\n"

      emit_raw "define ptr @__crystal_v2_realloc64(ptr %old_ptr, i64 %size) {\n"
      emit_raw "  %ptr = call ptr @realloc(ptr %old_ptr, i64 %size)\n"
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

    private def emit_entrypoint_if_needed(functions_to_emit : Array(Function))
      has_user_main = functions_to_emit.any? { |f| f.name == "main" }
      has_crystal_main = functions_to_emit.any? { |f| f.name == "__crystal_main" }
      return if has_user_main || !has_crystal_main

      # Entry point: main() calls __crystal_main()
      emit_raw "; Program entry point\n"
      emit_raw "define i32 @main(i32 %argc, ptr %argv) {\n"
      emit_raw "  call void @__crystal_main(i32 %argc, ptr %argv)\n"
      emit_raw "  ret i32 0\n"
      emit_raw "}\n\n"
    end

    private def emit_function(func : Function)
      reset_value_names(func)
      @emitted_allocas.clear
      @value_def_block.clear
      @cross_block_values.clear
      @cross_block_slots.clear
      @cross_block_slot_types.clear
      @cross_block_slot_type_refs.clear
      @phi_predecessor_loads.clear
      @current_func_blocks.clear
      @emitted_value_types.clear

      # Populate block lookup for phi predecessor load emission
      func.blocks.each { |block| @current_func_blocks[block.id] = block }

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

        base_name = p.name
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
      @current_return_type = return_type  # Store for terminator emission
      @current_return_type_ref = func.return_type
      # @current_func_name already set above before prepass

      mangled_name = @current_func_name
      @current_func_params = func.params
      @current_slab_frame = func.slab_frame

      # Skip duplicate function definitions
      if @emitted_functions.includes?(mangled_name)
        return
      end
      @emitted_functions << mangled_name

      emit_raw "define #{return_type} @#{mangled_name}(#{param_types.join(", ")}) {\n"

      # Emit entry block with hoisted allocas for dominance correctness
      # Use fn_entry to avoid conflict with parameter names like %entry
      emit_raw "fn_entry:\n"
      emit_hoisted_allocas(func)
      if @current_slab_frame
        emit_raw "  call void @__crystal_v2_slab_frame_push()\n"
      end
      # Jump to first user block
      if first_block = func.blocks.first?
        emit_raw "  br label %#{@block_names[first_block.id]}\n"
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

      func.blocks.each do |block|
        emit_block(block, func)
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
          emit_raw "  #{name} = alloca #{type}, align #{inst.align}\n"
          @emitted_allocas << inst.id
          @value_types[inst.id] = TypeRef::POINTER
          # Track element type for GEP
          @alloc_element_types[inst.id] = inst.alloc_type
        end
      end

      # Create alloca slots for cross-block values to fix dominance issues
      @cross_block_values.each do |val_id|
        val_type = @value_types[val_id]?
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
                   when .starts_with?("i") then "0"
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
          next unless phi_llvm_type.starts_with?("i") && !phi_llvm_type.includes?(".")
          next if phi_llvm_type == "i1"
          phi_bits = phi_llvm_type[1..-1].to_i? || 32

          phi.incoming.each do |(pred_block_id, val_id)|
            # Check if this value needs conversion
            conversion = @phi_zext_conversions[val_id]?
            next unless conversion

            from_bits, to_bits = conversion

            # Don't add duplicate entries
            next if @phi_predecessor_conversions.has_key?({pred_block_id, val_id})

            # Record that this predecessor block needs to emit a conversion for this value
            conv_name = "r#{val_id}.phi_conv.#{pred_block_id}"
            @phi_predecessor_conversions[{pred_block_id, val_id}] = {conv_name, from_bits, to_bits}
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
            # Search for exact match OR suffix match (for non-arithmetic, non-C-lib operators)
            matching_func = @module.functions.find do |f|
              mangled = @type_mapper.mangle_name(f.name)
              if is_operator || is_c_lib_function
                # Only exact match for arithmetic operators and C lib functions
                mangled == mangled_extern_name
              else
                # Exact match OR suffix match (e.g., index$UInt8 matches String#index$UInt8)
                mangled == mangled_extern_name ||
                mangled.ends_with?(mangled_extern_name)
              end
            end
            if matching_func
              # Use the actual function return type
              func_ret_type = matching_func.return_type
              func_ret_llvm = @type_mapper.llvm_type(func_ret_type)
              if func_ret_llvm != "void"
                effective_type = func_ret_type
              end
            else
              # Fallback: apply type suffix heuristics for method names with type suffixes
              if suffix = suffix_after_dollar(extern_name)
                if !suffix.includes?("_")
                  case suffix
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
              if effective_type == TypeRef::VOID
                # Default fallback: use inst.type
                extern_type_str = @type_mapper.llvm_type(inst.type)
                if extern_type_str == "void"
                  effective_type = TypeRef::VOID
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
          next unless phi_llvm_type.starts_with?("i") && !phi_llvm_type.includes?(".")
          next if phi_llvm_type == "i1"
          phi_bits = phi_llvm_type[1..-1].to_i? || 32

          inst.incoming.each do |(_, val)|
            current_type = @value_types[val]?
            current_llvm_type = current_type ? @type_mapper.llvm_type(current_type) : "void"

            # If phi expects different int type and both are integers
            if current_llvm_type != phi_llvm_type &&
               current_llvm_type.starts_with?("i") && !current_llvm_type.includes?(".")
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
                        when Load         then [inst.ptr]
                        when GetElementPtr        then [inst.base]
                        when GetElementPtrDynamic then [inst.base, inst.index]
                        when UnionWrap    then [inst.value]
                        when Select       then [inst.condition, inst.then_value, inst.else_value]
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
            if def_inst.is_a?(BinaryOp) || def_inst.is_a?(Load)
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
              if left_llvm.starts_with?("i") && !left_llvm.includes?(".") &&
                 right_llvm.starts_with?("i") && !right_llvm.includes?(".")
                left_bits = left_llvm[1..].to_i? || 32
                right_bits = right_llvm[1..].to_i? || 32
                max_bits = {left_bits, right_bits}.max

                # If widening will occur, record the widened type
                declared_bits = result_llvm.starts_with?("i") ? (result_llvm[1..].to_i? || 32) : 32
                if max_bits > declared_bits
                  # Will be widened to max_bits
                  actual_type = case max_bits
                                when 8 then TypeRef::INT8
                                when 16 then TypeRef::INT16
                                when 32 then TypeRef::INT32
                                when 64 then TypeRef::INT64
                                else TypeRef::INT64
                                end
                  if @value_types[inst.id]? != actual_type
                    @value_types[inst.id] = actual_type
                    changed = true
                  end
                elsif @value_types[inst.id]? != effective_type
                  # No widening, use effective type (void→INT64)
                  # Always update since prepass_collect_constants may have set wrong type
                  @value_types[inst.id] = effective_type
                  changed = true
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
      @phi_zext_conversions.clear
      @zext_value_names.clear
      @phi_nil_incoming_blocks.clear
      @phi_predecessor_conversions.clear
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
      phi_insts.each do |inst|
        emit_instruction(inst, func)
      end
      @in_phi_block = false

      # Now emit deferred stores for cross-block phi values
      @deferred_phi_stores.each do |store_stmt|
        emit store_stmt
      end
      @deferred_phi_stores.clear

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

        # Emit the appropriate conversion instruction
        if from_bits < to_bits
          # Widening: use sext for signed, zext for unsigned
          # Check if the type is signed based on @value_types
          val_type = @value_types[val_id]?
          is_signed = val_type && (val_type == TypeRef::INT8 || val_type == TypeRef::INT16 ||
                                   val_type == TypeRef::INT32 || val_type == TypeRef::INT64)
          if is_signed
            emit "%#{conv_name} = sext i#{from_bits} #{val_ref} to i#{to_bits}"
          else
            emit "%#{conv_name} = zext i#{from_bits} #{val_ref} to i#{to_bits}"
          end
        else
          # Narrowing: use trunc
          emit "%#{conv_name} = trunc i#{from_bits} #{val_ref} to i#{to_bits}"
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
          emit "store #{val_llvm_type} #{val_ref}, ptr %#{base_name}.payload_ptr, align 4"
        end
        emit "%#{wrap_name} = load #{union_type}, ptr %#{base_name}.ptr"
      end
    end

    private def emit_instruction(inst : Value, func : Function)
      name = "%r#{inst.id}"

      # Check if this instruction produces a value (has a result register)
      # Store, Free, RCIncrement, RCDecrement, GlobalStore, AtomicStore don't produce values
      # Also exclude void-returning calls - they emit `call void` without result
      is_void_call = (inst.is_a?(Call) || inst.is_a?(ExternCall)) && begin
        call_ret_type = @value_types[inst.id]?
        call_llvm_type = call_ret_type ? @type_mapper.llvm_type(call_ret_type) : nil
        call_llvm_type == "void"
      end
      produces_value = !inst.is_a?(Store) && !inst.is_a?(Free) &&
                       !inst.is_a?(RCIncrement) && !inst.is_a?(RCDecrement) &&
                       !inst.is_a?(GlobalStore) && !inst.is_a?(AtomicStore) &&
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

      # Store to cross-block slot if this value is used across blocks
      # This centralizes the logic that was previously only in emit_load
      if produces_value && (slot_name = @cross_block_slots[inst.id]?)
        val_type = @value_types[inst.id]? || inst.type
        llvm_type = @type_mapper.llvm_type(val_type)
        llvm_type = "ptr" if llvm_type == "void"
        slot_llvm_type = @cross_block_slot_types[inst.id]?
        # Guard: if MIR reuses value IDs, skip store when types are completely incompatible
        # (e.g., double result into Slice|Nil slot)
        if slot_llvm_type && slot_llvm_type != llvm_type &&
           slot_llvm_type.includes?(".union") && !llvm_type.includes?(".union")
          # Check if the scalar type could reasonably be a payload of the union
          # Union payload occupies field 1 with size [N x i8]
          # A double (8 bytes) fits in [8 x i8], i32 fits in [4 x i8], ptr fits in [8 x i8]
          # Skip store if the union name doesn't relate to the scalar type
          scalar_compatible = llvm_type == "ptr" || llvm_type.starts_with?("i") ||
                              llvm_type == "double" || llvm_type == "float"
          # Check if the union name contains a type related to the scalar
          union_has_float = slot_llvm_type.includes?("Float") || slot_llvm_type.includes?("float")
          union_has_int = slot_llvm_type.includes?("Int") || slot_llvm_type.includes?("UInt")
          value_is_float = llvm_type == "double" || llvm_type == "float"
          value_is_int = llvm_type.starts_with?("i") && !llvm_type.includes?(".")
          if scalar_compatible && !((value_is_float && union_has_float) || (value_is_int && union_has_int) || llvm_type == "ptr")
            # Incompatible types — MIR likely reused this value ID. Skip store.
            slot_llvm_type = nil  # Signal to skip
          end
        end
        store_val = name
        store_type = llvm_type
        # If the slot type differs from the value type, convert value to match slot type.
        # The slot type is fixed from prepass; loads always use the slot type.
        if slot_llvm_type && slot_llvm_type != llvm_type
          base = name.lstrip('%')
          if slot_llvm_type.includes?(".union") && !llvm_type.includes?(".union")
            # Scalar → union: wrap value in union struct
            emit "%#{base}.slot_wrap_ptr = alloca #{slot_llvm_type}, align 8"
            emit "store #{slot_llvm_type} zeroinitializer, ptr %#{base}.slot_wrap_ptr"
            emit "%#{base}.slot_wrap_tid = getelementptr #{slot_llvm_type}, ptr %#{base}.slot_wrap_ptr, i32 0, i32 0"
            emit "store i32 0, ptr %#{base}.slot_wrap_tid"
            emit "%#{base}.slot_wrap_pay = getelementptr #{slot_llvm_type}, ptr %#{base}.slot_wrap_ptr, i32 0, i32 1"
            emit "store #{llvm_type} #{name}, ptr %#{base}.slot_wrap_pay"
            emit "%#{base}.slot_wrap_val = load #{slot_llvm_type}, ptr %#{base}.slot_wrap_ptr"
            store_val = "%#{base}.slot_wrap_val"
            store_type = slot_llvm_type
          elsif !slot_llvm_type.includes?(".union") && llvm_type.includes?(".union")
            # Union → scalar: extract payload
            emit "%#{base}.slot_unwrap_ptr = alloca #{llvm_type}, align 8"
            emit "store #{llvm_type} #{name}, ptr %#{base}.slot_unwrap_ptr"
            emit "%#{base}.slot_unwrap_pay = getelementptr #{llvm_type}, ptr %#{base}.slot_unwrap_ptr, i32 0, i32 1"
            emit "%#{base}.slot_unwrap_val = load #{slot_llvm_type}, ptr %#{base}.slot_unwrap_pay"
            store_val = "%#{base}.slot_unwrap_val"
            store_type = slot_llvm_type
          elsif llvm_type.starts_with?("i") && slot_llvm_type.starts_with?("i") && !llvm_type.includes?(".") && !slot_llvm_type.includes?(".")
            # Integer width mismatch: sext/trunc
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
          elsif llvm_type == "ptr" && slot_llvm_type.starts_with?("i")
            emit "%#{base}.slot_ptrtoint = ptrtoint ptr #{name} to #{slot_llvm_type}"
            store_val = "%#{base}.slot_ptrtoint"
            store_type = slot_llvm_type
          elsif llvm_type.starts_with?("i") && slot_llvm_type == "ptr"
            emit "%#{base}.slot_inttoptr = inttoptr #{llvm_type} #{name} to ptr"
            store_val = "%#{base}.slot_inttoptr"
            store_type = slot_llvm_type
          elsif (llvm_type == "double" || llvm_type == "float") && slot_llvm_type.starts_with?("i") && !slot_llvm_type.includes?(".")
            # Float → int: pick signedness based on the slot type.
            slot_type_ref = @cross_block_slot_type_refs[inst.id]?
            op = slot_type_ref && unsigned_type_ref?(slot_type_ref) ? "fptoui" : "fptosi"
            emit "%#{base}.slot_ftoi = #{op} #{llvm_type} #{name} to #{slot_llvm_type}"
            store_val = "%#{base}.slot_ftoi"
            store_type = slot_llvm_type
          elsif llvm_type.starts_with?("i") && !llvm_type.includes?(".") && (slot_llvm_type == "double" || slot_llvm_type == "float")
            # Int → float: use uitofp for unsigned types
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
            # Float → ptr: preserve bit pattern (bitcast), then inttoptr.
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
            # Fallback: store as the slot type (may cause issues but avoids crash)
            store_type = slot_llvm_type
          end
        end
        store_stmt = "store #{store_type} #{store_val}, ptr %#{slot_name}"
        if @in_phi_block
          # Defer store until after all phi nodes (LLVM requires phis to be grouped at top)
          @deferred_phi_stores << store_stmt
        else
          emit store_stmt
        end
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
      if type.includes?(".union")
        # Union types can't use add instruction - create zeroinit union
        base_name = name.lstrip('%')
        emit "%#{base_name}.ptr = alloca #{type}, align 8"
        # Zero-initialize by setting type_id to 1 (nil variant)
        emit "%#{base_name}.type_id_ptr = getelementptr #{type}, ptr %#{base_name}.ptr, i32 0, i32 0"
        emit "store i32 1, ptr %#{base_name}.type_id_ptr"
        emit "#{name} = load #{type}, ptr %#{base_name}.ptr"
        @value_types[inst.id] = inst.type
      elsif (type == "void" || value == "null" || type == "ptr") &&
            (sa_type = @module.type_registry.get(inst.type)) &&
            sa_type.name.starts_with?("StaticArray(")
        # uninitialized StaticArray(T, N) — emit stack alloca.
        # Parse element type and count from name, look up element size from registry.
        total_bytes = 0_u64
        if m = sa_type.name.match(/StaticArray\((.+),\s*(\d+)\)/)
          elem_name = m[1].strip
          array_count = m[2].to_u64
          elem_mir_type = @module.type_registry.get_by_name(elem_name)
          elem_size = elem_mir_type ? elem_mir_type.size : 8_u64 # default to pointer size
          total_bytes = elem_size * array_count
        end
        total_bytes = 8_u64 if total_bytes == 0 # safety fallback
        emit "#{name} = alloca [#{total_bytes} x i8], align 8"
        # Override constant_values so call sites use the alloca ptr, not "null"
        @constant_values[inst.id] = name
        @value_types[inst.id] = TypeRef::POINTER
      elsif type == "void" || value == "null" || type == "ptr"
        # void/null/ptr constants are treated as ptr type in LLVM
        # Must emit real instruction (not comment) so phi nodes can reference it
        emit "#{name} = inttoptr i64 0 to ptr"
        @value_types[inst.id] = TypeRef::POINTER
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
          raw_name = name.starts_with?("%") ? name[1..] : name
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

      # Fix for VOID types - use a reasonable default type
      if val_type_str == "void"
        if val.starts_with?("%")
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
      if (val_type_str == "float" || val_type_str == "double") && !val.starts_with?("%")
        # Convert integer literals to float literals (e.g., "0" -> "0.0", "3" -> "3.0")
        val = "#{val}.0" if val.matches?(/^-?\d+$/)
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
        elsif base_type_str.starts_with?("i")
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

      # Check if base is a struct or reference type (from registered types)
      base_value_type = @value_types[inst.base]?
      if base_value_type && (mir_type = @module.type_registry.get(base_value_type))
        has_fields = (mir_type.fields && !mir_type.fields.not_nil!.empty?) || mir_type.kind.reference?
        if has_fields && (mir_type.kind.struct? || mir_type.kind.reference?)
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

    # Compute LLVM struct field index from byte offset.
    # Each MIR Field stores its byte offset; the LLVM struct fields are emitted
    # in MIR field order with an optional vtable ptr at index 0 for classes.
    # We match by the stored byte offset rather than recomputing sizes, since
    # LLVM alignment differs from our packed byte layout.
    private def compute_field_index(mir_type : Type, byte_offset : UInt32) : Int32
      fields = mir_type.fields
      return 0 unless fields

      class_offset = mir_type.kind.reference? ? 1 : 0

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
        elsif base_type_str.starts_with?("i")
          # Integer to pointer: inttoptr (may be a value used as address)
          emit "#{name}.base_ptr = inttoptr #{base_type_str} #{base} to ptr"
          base = "#{name}.base_ptr"
        elsif base_type_str == "double" || base_type_str == "float"
          # Float to pointer: bitcast to i64 first, then inttoptr
          emit "#{name}.base_int = bitcast #{base_type_str} #{base} to i64"
          emit "#{name}.base_ptr = inttoptr i64 #{name}.base_int to ptr"
          base = "#{name}.base_ptr"
        end
        # For other types, hope the value is already ptr (optimistic)
      end

      # Check if index needs to be converted to i64 for GEP
      index_type = @value_types[inst.index]? || TypeRef::INT32
      index_type_str = @type_mapper.llvm_type(index_type)
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
        emit "%#{base_name}.idx_is_nil = icmp eq i32 %#{base_name}.idx_type_id, 1"
        emit "%#{base_name}.idx64 = select i1 %#{base_name}.idx_is_nil, i64 0, i64 #{payload64}"
        index = "%#{base_name}.idx64"
        index_type_str = "i64"
      end
      if index_type_str == "i64"
        emit "#{name} = getelementptr #{element_type}, ptr #{base}, i64 #{index}"
      elsif index_type_str == "ptr"
        # Convert ptr to i64 with ptrtoint
        ext_name = "#{name}.idx64"
        emit "#{ext_name} = ptrtoint ptr #{index} to i64"
        emit "#{name} = getelementptr #{element_type}, ptr #{base}, i64 #{ext_name}"
      elsif index_type_str == "void"
        # Void index - use 0 as default
        emit "#{name} = getelementptr #{element_type}, ptr #{base}, i64 0"
      elsif index_type_str == "float" || index_type_str == "double"
        # Convert float to i64 with fptosi
        ext_name = "#{name}.idx64"
        emit "#{ext_name} = fptosi #{index_type_str} #{index} to i64"
        emit "#{name} = getelementptr #{element_type}, ptr #{base}, i64 #{ext_name}"
      else
        # Convert integer index to i64 (extend or truncate based on width).
        ext_name = "#{name}.idx64"
        bits = nil.as(Int32?)
        if index_type_str.starts_with?("i")
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
        emit "#{name} = getelementptr #{element_type}, ptr #{base}, i64 #{ext_name}"
      end
      @value_types[inst.id] = TypeRef::POINTER  # GEP always returns pointer
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
      if !left.starts_with?("%r") && left.starts_with?("%")
        param_name = left[1..]  # Remove %
        @current_func_params.each do |param|
          if param.name == param_name
            param_type = param.type == TypeRef::VOID ? TypeRef::POINTER : param.type
            operand_type_str = @type_mapper.llvm_type(param_type)
            # STDERR.puts "[BINOP-PARAM-LEFT] left=#{left}, param_name=#{param_name}, found_type=#{operand_type_str}"
            break
          end
        end
      end
      if !right.starts_with?("%r") && right.starts_with?("%")
        param_name = right[1..]  # Remove %
        @current_func_params.each do |param|
          if param.name == param_name
            param_type = param.type == TypeRef::VOID ? TypeRef::POINTER : param.type
            right_type_str = @type_mapper.llvm_type(param_type)
            # STDERR.puts "[BINOP-PARAM-RIGHT] right=#{right}, param_name=#{param_name}, found_type=#{right_type_str}"
            break
          end
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

      # Handle union types in comparisons - check type_id for nil comparison
      if is_comparison && operand_type_str.includes?(".union")
        base_name = name.lstrip('%')
        # Union comparison: compare type_id (0=non-nil, 1=nil)
        # For == 0 or != 0, compare type_id to 0 (non-nil check)
        emit "%#{base_name}.union_ptr = alloca #{operand_type_str}, align 8"
        emit "store #{operand_type_str} #{normalize_union_value(left, operand_type_str)}, ptr %#{base_name}.union_ptr"
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
        if !left.starts_with?("%") && left =~ /^-?\d+$/ && !left.includes?(".")
          left = "#{left}.0"
          operand_type_str = float_type
        end
        if !right.starts_with?("%") && right =~ /^-?\d+$/ && !right.includes?(".")
          right = "#{right}.0"
          right_type_str = float_type
        end
        # Convert ptr operands to float (ptrtoint then sitofp/uitofp)
        if left.starts_with?("%") && operand_type_str == "ptr"
          emit "%binop#{inst.id}.left_ptrtoint = ptrtoint ptr #{left} to i64"
          emit "%binop#{inst.id}.left_itof = uitofp i64 %binop#{inst.id}.left_ptrtoint to #{float_type}"
          left = "%binop#{inst.id}.left_itof"
          operand_type_str = float_type
        end
        if right.starts_with?("%") && right_type_str == "ptr"
          emit "%binop#{inst.id}.right_ptrtoint = ptrtoint ptr #{right} to i64"
          emit "%binop#{inst.id}.right_itof = uitofp i64 %binop#{inst.id}.right_ptrtoint to #{float_type}"
          right = "%binop#{inst.id}.right_itof"
          right_type_str = float_type
        end
        # Convert integer SSA values to float for float operations
        if left.starts_with?("%") && operand_type_str.starts_with?("i")
          op = unsigned_type_ref?(operand_type) ? "uitofp" : "sitofp"
          emit "%binop#{inst.id}.left_itof = #{op} #{operand_type_str} #{left} to #{float_type}"
          left = "%binop#{inst.id}.left_itof"
          operand_type_str = float_type
        end
        if right.starts_with?("%") && right_type_str.starts_with?("i")
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
        left_is_int = operand_type_str.starts_with?("i") && !operand_type_str.includes?(".")
        right_is_int = right_type_str.starts_with?("i") && !right_type_str.includes?(".")
        if operand_type_str != right_type_str && left_is_int && right_is_int
          # Convert smaller type to larger type
          left_bits = operand_type_str[1..-1].to_i? || 32
          right_bits = right_type_str[1..-1].to_i? || 32
          if left_bits < right_bits
            # Extend left to match right
            emit "%binop#{inst.id}.left_ext = sext #{operand_type_str} #{left} to #{right_type_str}"
            left = "%binop#{inst.id}.left_ext"
            operand_type_str = right_type_str
            result_type = right_type_str if is_arithmetic
          elsif right_bits < left_bits
            # Extend right to match left
            emit "%binop#{inst.id}.right_ext = sext #{right_type_str} #{right} to #{operand_type_str}"
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

      is_signed = operand_type.id <= TypeRef::INT128.id

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
      elsif op.starts_with?("fcmp")
        # Float comparisons - use float/double type
        cmp_type = operand_type_str == "double" || right_type_str == "double" ? "double" : "float"
        emit "#{name} = #{op} #{cmp_type} #{left}, #{right}"
        @value_types[inst.id] = TypeRef::BOOL
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
              is_float_result ? (vt == "float" || vt == "double") : vt.starts_with?("i")
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
            if payload_type.starts_with?("i") && result_type.starts_with?("i")
              payload_bits = payload_type[1..].to_i? || 32
              raw_bits = result_type[1..].to_i? || 32
              if raw_bits > payload_bits
                emit "%binop#{inst.id}.raw_trunc = trunc #{result_type} #{raw_name} to #{payload_type}"
                payload_val = "%binop#{inst.id}.raw_trunc"
              elsif raw_bits < payload_bits
                emit "%binop#{inst.id}.raw_ext = sext #{result_type} #{raw_name} to #{payload_type}"
                payload_val = "%binop#{inst.id}.raw_ext"
              end
            elsif (payload_type == "float" || payload_type == "double") && result_type.starts_with?("i")
              op = (unsigned_type_ref?(operand_type) || (right_type && unsigned_type_ref?(right_type))) ? "uitofp" : "sitofp"
              emit "%binop#{inst.id}.raw_itof = #{op} #{result_type} #{raw_name} to #{payload_type}"
              payload_val = "%binop#{inst.id}.raw_itof"
            elsif payload_type == "ptr" && result_type.starts_with?("i")
              emit "%binop#{inst.id}.raw_inttoptr = inttoptr #{result_type} #{raw_name} to ptr"
              payload_val = "%binop#{inst.id}.raw_inttoptr"
            elsif payload_type.starts_with?("i") && result_type == "ptr"
              emit "%binop#{inst.id}.raw_ptrtoint = ptrtoint ptr #{raw_name} to #{payload_type}"
              payload_val = "%binop#{inst.id}.raw_ptrtoint"
            elsif payload_type.starts_with?("i") && (result_type == "float" || result_type == "double")
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
          return
        end

        # Ensure operands match result_type for arithmetic ops
        # If operand is larger than result_type, use operand type instead (don't truncate)
        if result_type.starts_with?("i") && result_type != "i1"
          result_bits = result_type[1..].to_i? || 32
          operand_bits = operand_type_str.starts_with?("i") ? (operand_type_str[1..].to_i? || 32) : 0
          right_bits_val = right_type_str.starts_with?("i") ? (right_type_str[1..].to_i? || 32) : 0

          # If operands are larger than result, use the larger operand type
          max_operand_bits = {operand_bits, right_bits_val}.max
          if max_operand_bits > result_bits
            result_type = "i#{max_operand_bits}"
          end

          # Extend smaller operands to result_type
          result_bits = result_type[1..].to_i? || 32
          if operand_type_str.starts_with?("i") && operand_type_str != result_type
            operand_bits = operand_type_str[1..].to_i? || 32
            if operand_bits < result_bits
              emit "%binop#{inst.id}.left_to_result = sext #{operand_type_str} #{left} to #{result_type}"
              left = "%binop#{inst.id}.left_to_result"
            end
          end
          if right_type_str.starts_with?("i") && right_type_str != result_type
            right_bits_check = right_type_str[1..].to_i? || 32
            if right_bits_check < result_bits
              emit "%binop#{inst.id}.right_to_result = sext #{right_type_str} #{right} to #{result_type}"
              right = "%binop#{inst.id}.right_to_result"
            end
          end
        end
        # If MIR expects ptr but we did arithmetic as int, convert back to ptr
        if convert_result_to_ptr && result_type.starts_with?("i")
          emit "%binop#{inst.id}.int_result = #{op} #{result_type} #{left}, #{right}"
          emit "#{name} = inttoptr #{result_type} %binop#{inst.id}.int_result to ptr"
          @value_types[inst.id] = TypeRef::POINTER
        else
          emit "#{name} = #{op} #{result_type} #{left}, #{right}"
          # Track actual emitted type for downstream use
          actual_type = case result_type
                        when "i1" then TypeRef::BOOL
                        when "i8" then TypeRef::INT8
                        when "i16" then TypeRef::INT16
                        when "i32" then TypeRef::INT32
                        when "i64" then TypeRef::INT64
                        when "i128" then TypeRef::INT128
                        when "float" then TypeRef::FLOAT32
                        when "double" then TypeRef::FLOAT64
                        when "ptr" then TypeRef::POINTER
                        else inst.type  # Use MIR type as fallback
                        end
          @value_types[inst.id] = actual_type
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
      if !operand.starts_with?("%r") && operand.starts_with?("%")
        param_name = operand[1..]  # Remove %
        @current_func_params.each do |param|
          if param.name == param_name
            param_type = param.type == TypeRef::VOID ? TypeRef::POINTER : param.type
            operand_llvm_type = @type_mapper.llvm_type(param_type)
            break
          end
        end
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
                vt.starts_with?("i")
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
            emit "%#{base_name}.neg_result = fsub #{payload_type} 0.0, %#{base_name}.neg_val"
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
          # Float negation uses fsub with 0.0
          emit "#{name} = fsub #{operand_llvm_type} 0.0, #{operand}"
          @value_types[inst.id] = operand_type
        else
          # Handle type mismatch: if result type is larger than operand type, extend operand
          actual_type = type
          actual_operand = operand
          result_type_ref = operand_type  # Default to preserving operand type

          # If result type is ptr but operand is integer, use operand type
          # Can't do arithmetic on ptr with integer constants
          if type == "ptr" && operand_llvm_type.starts_with?("i")
            actual_type = operand_llvm_type
            result_type_ref = operand_type
          elsif operand_llvm_type != type && operand_llvm_type.starts_with?("i") && type.starts_with?("i")
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
            # For unions, check if type_id != 1 (1 = nil)
            emit "%#{base_name}.union_ptr = alloca #{operand_llvm_type}, align 8"
            emit "store #{operand_llvm_type} #{normalize_union_value(operand, operand_llvm_type)}, ptr %#{base_name}.union_ptr"
            emit "%#{base_name}.type_id_ptr = getelementptr #{operand_llvm_type}, ptr %#{base_name}.union_ptr, i32 0, i32 0"
            emit "%#{base_name}.type_id = load i32, ptr %#{base_name}.type_id_ptr"
            emit "%#{base_name}.bool = icmp ne i32 %#{base_name}.type_id, 1"
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
      if ENV["DEBUG_CAST_SLOT"]? && value.includes?(".fromslot.cast")
        emitted_dbg = @emitted_value_types[value]?
        STDERR.puts "[CAST_SLOT] func=#{@current_func_name} name=#{name} value=#{value} src=#{src_type} dst=#{dst_type} emitted=#{emitted_dbg}"
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
        elsif dst_type.starts_with?("i")
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
        if is_nil_cast
          emit "store i32 1, ptr %#{base_name}.type_id_ptr"  # 1 = nil
        else
          # Non-nil value - store type_id=0 and payload
          emit "store i32 0, ptr %#{base_name}.type_id_ptr"  # 0 = non-nil
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
      is_src_int = src_type.starts_with?("i") && !src_type.includes?(".union")
      is_dst_int = dst_type.starts_with?("i") && !dst_type.includes?(".union")
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
        is_dst_int = dst_type.starts_with?("i") && !dst_type.includes?(".union")
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

      # Guard: ptr to float/double - can't bitcast directly, must load from ptr
      # This happens when value is a pointer to float/double and we need the value itself
      if src_type == "ptr" && (dst_type == "float" || dst_type == "double")
        emit "#{name} = load #{dst_type}, ptr #{value}"
        @value_types[inst.id] = inst.type
        return
      end

      emit "#{name} = #{op} #{src_type} #{value} to #{dst_type}"
      record_emitted_type(name, dst_type)
      # Track actual destination type for downstream use
      @value_types[inst.id] = inst.type
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
      end
      # Check for predecessor-loaded value (for cross-block SSA fix)
      if pred_load_name = @phi_predecessor_loads[{block, val}]?
        # CRITICAL: Use the SLOT ALLOCATION TYPE, not @value_types[val]
        # @value_types may be updated during emission (e.g., nil constant → ptr),
        # but the slot was allocated with the prepass type. The load will use the slot type.
        slot_llvm_type = @cross_block_slot_types[val]?
        # Check type compatibility:
        # - Same type: use the load directly
        # - Both ptr: compatible
        # - Both int (same size): compatible
        # - Otherwise: let the type mismatch handling kick in
        is_compatible = if slot_llvm_type.nil?
                          false  # Unknown slot type, let special handling deal with it
                        elsif slot_llvm_type == phi_type
                          true
                        elsif slot_llvm_type == "ptr" && phi_type == "ptr"
                          true
                        elsif slot_llvm_type.starts_with?("i") && phi_type.starts_with?("i") &&
                              !slot_llvm_type.includes?(".union") && !phi_type.includes?(".union")
                          # Same integer width?
                          slot_llvm_type == phi_type
                        else
                          false
                        end
        return "%#{pred_load_name}" if is_compatible
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
            incoming_map = {} of BlockId => ValueId
            inst.incoming.each { |(block_id, val)| incoming_map[block_id] = val }
            incoming_pairs = [] of Tuple(BlockId, ValueId)
            preds.each do |pred|
              if val = incoming_map[pred]?
                incoming_pairs << {pred, val}
              else
                missing_preds << pred
              end
            end
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
            unless missing_preds.includes?(block_id)
              missing_preds << block_id
            end
            next
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
            # Union value can't be used in ptr phi - use null
            "[null, %#{block_name.call(block)}]"
          elsif val_type_str && val_type_str.starts_with?("i") && !val_type_str.includes?(".union")
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
        @value_types[inst.id] = TypeRef::POINTER
        @in_phi_mode = false
        return
      end

      is_int_type = phi_type.starts_with?("i") && phi_type != "i1" && !phi_type.includes?(".union")
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
            elsif val_type_str && val_type_str.starts_with?("i") && val_type_str != "i1"
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
                ref = "0" if ref == "null" || ref.includes?(".")
                "[#{ref}, %#{block_name.call(block)}]"
              end
            end
          end
          append_missing.call(incoming, "i1")
          emit "#{name} = phi i1 #{incoming.join(", ")}"
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
          next false unless val_type_str.starts_with?("i") && !val_type_str.includes?(".union")
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
            elsif val_type_str && val_type_str.starts_with?("i") && !val_type_str.includes?(".union")
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
            val_type_str.starts_with?("i") ||
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
              # Union value can't be directly used in ptr phi - use null
              "[null, %#{block_name.call(block)}]"
            elsif val_type_str && val_type_str.starts_with?("i") && !val_type_str.includes?(".union")
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
          if val_type && union_descriptor
            # If the incoming type is a known union variant, don't treat ptr as a mismatch.
            next false if union_descriptor.variants.any? { |variant| variant.type_ref == val_type }
          end
          if val_llvm_type != "ptr"
            def_inst = find_def_inst(val)
            if def_inst && def_inst.is_a?(UnionUnwrap) && @type_mapper.llvm_type(def_inst.type) == "ptr"
              next true
            end
          end
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
                # Union value can't be used in ptr phi - use null
                # This is lossy but allows compilation to proceed
                "[null, %#{block_name.call(block)}]"
              end
            elsif val_type_str && val_type_str.starts_with?("i") && !val_type_str.includes?(".union")
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
          # Value is a union but different from phi type
          val_llvm_type && val_llvm_type.includes?(".union") && val_llvm_type != phi_type
        end
        if has_non_union_incoming || has_different_union_incoming
          # Emit union phi with zeroinitializer for non-union or different union values
          # We can't convert between union types in phi instruction, so use zeroinitializer (nil case)
          incoming = incoming_pairs.map do |(block, val)|
            # Check for predecessor load first (cross-block SSA fix)
            if pred_ref = phi_incoming_ref(block, val, phi_type)
              next "[#{pred_ref}, %#{block_name.call(block)}]"
            end
            val_type = @value_types[val]?
            val_type_str = val_type ? @type_mapper.llvm_type(val_type) : nil
            if val_type_str && val_type_str == phi_type
              # Same union type - use value directly
              ref = value_ref(val)
              # If value_ref returned "null", convert to zeroinitializer for union
              ref = "zeroinitializer" if ref == "null"
              "[#{ref}, %#{block_name.call(block)}]"
            else
              # Type mismatch (non-union or different union) - use zeroinitializer (nil)
              "[zeroinitializer, %#{block_name.call(block)}]"
            end
          end
          append_missing.call(incoming, phi_type)
          emit "#{name} = phi #{phi_type} #{incoming.join(", ")}"
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
          "[%r#{val}, %#{block_name.call(block)}]"
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
          elsif is_int_type && val_type_str && val_type_str.starts_with?("i") && val_type_str != phi_type
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
          elsif is_ptr_type && val_type_str && val_type_str.starts_with?("i") && !val_type_str.includes?(".union")
            # Int flowing into ptr phi - use null (type mismatch from MIR)
            "[null, %#{block_name.call(block)}]"
          elsif is_float_type && (val_type_str == "ptr" || val_type_str == "void")
            # Ptr/void flowing into float phi - use 0.0 (type mismatch from MIR)
            "[0.0, %#{block_name.call(block)}]"
          elsif is_float_type && val_type_str && (val_type_str == "float" || val_type_str == "double") && val_type_str != phi_type
            # float↔double mismatch in phi - use 0.0 as safe default
            # Can't emit fpext/fptrunc in phi, would need to be in source block
            "[0.0, %#{block_name.call(block)}]"
          elsif is_float_type && val_type_str && val_type_str.starts_with?("i")
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
            elsif (ref == "0" || ref.starts_with?("%r")) && is_ptr_type && val_type_str && val_type_str.starts_with?("i")
              # Int value flowing into ptr phi
              ref = "null"
            end
            "[#{ref}, %#{block_name.call(block)}]"
          end
        end
      end
      append_missing.call(incoming, phi_type)
      emit "#{name} = phi #{phi_type} #{incoming.join(", ")}"
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
      if raw_callee_name && pointer_constructor_name?(raw_callee_name) && inst.args.size == 1
        arg_id = inst.args[0]
        arg = value_ref(arg_id)
        arg_type = lookup_value_llvm_type(arg_id)
        if arg_type == "ptr"
          emit "#{name} = bitcast ptr #{arg} to ptr"
        elsif arg_type.starts_with?("i")
          emit "#{name} = inttoptr #{arg_type} #{arg} to ptr"
        else
          emit "#{name} = bitcast ptr #{arg} to ptr"
        end
        @value_types[inst.id] = TypeRef::POINTER
        return
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

      # IMPORTANT: Check if prepass determined a different type (e.g., from phi usage)
      # Prepass type takes precedence when phi expects a specific type
      prepass_type = @value_types[inst.id]?
      if prepass_type
        prepass_llvm_type = @type_mapper.llvm_type(prepass_type)
        if prepass_llvm_type != "void" && prepass_llvm_type != return_type
          return_type = prepass_llvm_type
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
        first_arg_type = @value_types[inst.args[0]]?
        first_arg_llvm = first_arg_type ? @type_mapper.llvm_type(first_arg_type) : "void"
        if first_arg_llvm == "void" || first_arg_llvm == "ptr"
          # Likely a self arg for a class method — drop it
          call_args = inst.args[1..]
        end
      end
      # Pad missing args with zero/null defaults to avoid UB from arg count mismatch.
      # This handles cases where callers omit default parameters (e.g., .new with defaults).
      pad_args_extra = nil.as(Array(String)?)
      if callee_func && call_args.size < callee_func.params.size
        extra = [] of String
        (call_args.size...callee_func.params.size).each do |i|
          param = callee_func.params[i]
          param_llvm = @type_mapper.llvm_type(param.type)
          pad_val = case param_llvm
                    when "i1"    then "i1 0"
                    when "ptr"   then "ptr null"
                    when "void"  then "ptr null"
                    when .includes?(".union") then "#{param_llvm} zeroinitializer"
                    when "float" then "float 0.0"
                    when "double" then "double 0.0"
                    else "#{param_llvm} 0"
                    end
          extra << pad_val
        end
        pad_args_extra = extra unless extra.empty?
      end
      use_callee_params = callee_func && callee_func.params.size == call_args.size
      # Debug void args
      has_void_arg = call_args.any? { |a| @type_mapper.llvm_type(@value_types[a]? || TypeRef::POINTER) == "void" }
      # STDERR.puts "[CALL-DEBUG] #{callee_name}, use_callee_params=#{use_callee_params}, has_void_arg=#{has_void_arg}" if has_void_arg
      args = if use_callee_params && callee_func
               call_args.map_with_index { |a, i|
                 param_type = callee_func.params[i].type
                 expected_llvm_type = @type_mapper.llvm_type(param_type)
                 actual_type = @value_types[a]? || TypeRef::POINTER
                 actual_llvm_type = @type_mapper.llvm_type(actual_type)
                 if @cross_block_slot_types[a]? == "ptr"
                   actual_type = TypeRef::POINTER
                   actual_llvm_type = "ptr"
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
                   # Union types need zeroinitializer instead of 0 or null
                   if expected_llvm_type.includes?(".union") && (val == "0" || val == "null")
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
                 elsif expected_llvm_type == "ptr" && actual_llvm_type.starts_with?("%") && actual_llvm_type.includes?(".union")
                   # Coerce union to ptr: extract payload and interpret as ptr
                   # Union layout: { type_id : i32, payload : [8 x i8] }
                   c = @cond_counter
                   @cond_counter += 1
                   temp_alloca = "%alloca.#{c}"
                   temp_ptr = "%ptr.#{c}"
                   temp_load = "%load.#{c}"
                   def_inst = find_def_inst(a)
                   inst_llvm_type = def_inst ? @type_mapper.llvm_type(def_inst.type) : nil
                   if ENV.has_key?("DEBUG_UNION_ARG")
                     STDERR.puts "[UNION_ARG] callee=#{callee_name} arg=#{a} expected=#{expected_llvm_type} actual=#{actual_llvm_type} val=#{value_ref(a)} def=#{def_inst.class.name if def_inst} def_type=#{def_inst.try(&.type)} def_llvm=#{inst_llvm_type}"
                   end
                   if inst_llvm_type == "ptr" || @cross_block_slot_types[a]? == "ptr"
                     "ptr #{value_ref(a)}"
                   else
                     emit "#{temp_alloca} = alloca #{actual_llvm_type}, align 8"
                     emit "store #{actual_llvm_type} #{normalize_union_value(value_ref(a), actual_llvm_type)}, ptr #{temp_alloca}"
                     emit "#{temp_ptr} = getelementptr #{actual_llvm_type}, ptr #{temp_alloca}, i32 0, i32 1"
                     emit "#{temp_load} = load ptr, ptr #{temp_ptr}"
                     "ptr #{temp_load}"
                   end
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
                  # Ptr to float conversion: ptrtoint, then unsigned int→float
                  val = value_ref(a)
                  c = @cond_counter
                  @cond_counter += 1
                  emit "%ptrtofp.#{c}.int = ptrtoint ptr #{val} to i64"
                  emit "%ptrtofp.#{c} = uitofp i64 %ptrtofp.#{c}.int to #{expected_llvm_type}"
                  "#{expected_llvm_type} %ptrtofp.#{c}"
                elsif (expected_llvm_type == "double" || expected_llvm_type == "float") && actual_llvm_type.starts_with?("i")
                   # Int to float conversion: signed or unsigned
                   val = value_ref(a)
                   c = @cond_counter
                   @cond_counter += 1
                   op = unsigned_type_ref?(actual_type) ? "uitofp" : "sitofp"
                   emit "%itofp.#{c} = #{op} #{actual_llvm_type} #{val} to #{expected_llvm_type}"
                   "#{expected_llvm_type} %itofp.#{c}"
                 elsif expected_llvm_type.starts_with?("i") && (actual_llvm_type == "float" || actual_llvm_type == "double")
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
                 elsif expected_llvm_type == "i1" && actual_llvm_type.starts_with?("i") && actual_llvm_type != "i1"
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
                 elsif expected_llvm_type.starts_with?("i") && actual_llvm_type.starts_with?("i") && expected_llvm_type != actual_llvm_type
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
                 elsif expected_llvm_type.starts_with?("i") && is_union_llvm_type?(actual_llvm_type)
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
                 elsif expected_llvm_type.starts_with?("i") && actual_llvm_type == "ptr"
                   # Ptr to int conversion needed (ptrtoint)
                   val = value_ref(a)
                   c = @cond_counter
                   @cond_counter += 1
                   temp_int = "%ptrtoint.#{c}"
                   emit "#{temp_int} = ptrtoint ptr #{val} to #{expected_llvm_type}"
                   "#{expected_llvm_type} #{temp_int}"
                 elsif is_union_llvm_type?(expected_llvm_type) &&
                       (actual_llvm_type.starts_with?("i") || actual_llvm_type == "float" || actual_llvm_type == "double")
                   # Scalar to union conversion - wrap scalar payload with type_id=0 (non-nil)
                   val = value_ref(a)
                   c = @cond_counter
                   @cond_counter += 1
                   emit "%scalar_to_union.#{c}.ptr = alloca #{expected_llvm_type}, align 8"
                   emit "%scalar_to_union.#{c}.type_id_ptr = getelementptr #{expected_llvm_type}, ptr %scalar_to_union.#{c}.ptr, i32 0, i32 0"
                   emit "store i32 0, ptr %scalar_to_union.#{c}.type_id_ptr"
                   emit "%scalar_to_union.#{c}.payload_ptr = getelementptr #{expected_llvm_type}, ptr %scalar_to_union.#{c}.ptr, i32 0, i32 1"
                   # Payload at offset 4, use align 4 for ARM64 compatibility
                   emit "store #{actual_llvm_type} #{val}, ptr %scalar_to_union.#{c}.payload_ptr, align 4"
                   emit "%scalar_to_union.#{c}.val = load #{expected_llvm_type}, ptr %scalar_to_union.#{c}.ptr"
                   "#{expected_llvm_type} %scalar_to_union.#{c}.val"
                 elsif is_union_llvm_type?(expected_llvm_type) && actual_llvm_type == "ptr"
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
                   c = @cond_counter
                   @cond_counter += 1
                   # Extract type_id from actual union
                   emit "%union_conv.#{c}.src_ptr = alloca #{actual_llvm_type}, align 8"
                   emit "store #{actual_llvm_type} #{val}, ptr %union_conv.#{c}.src_ptr"
                   emit "%union_conv.#{c}.src_type_id_ptr = getelementptr #{actual_llvm_type}, ptr %union_conv.#{c}.src_ptr, i32 0, i32 0"
                   emit "%union_conv.#{c}.type_id = load i32, ptr %union_conv.#{c}.src_type_id_ptr"
                   emit "%union_conv.#{c}.src_payload_ptr = getelementptr #{actual_llvm_type}, ptr %union_conv.#{c}.src_ptr, i32 0, i32 1"
                   emit "%union_conv.#{c}.payload_as_ptr = load ptr, ptr %union_conv.#{c}.src_payload_ptr, align 4"
                   # Store into expected union
                   emit "%union_conv.#{c}.dst_ptr = alloca #{expected_llvm_type}, align 8"
                   emit "%union_conv.#{c}.dst_type_id_ptr = getelementptr #{expected_llvm_type}, ptr %union_conv.#{c}.dst_ptr, i32 0, i32 0"
                   emit "store i32 %union_conv.#{c}.type_id, ptr %union_conv.#{c}.dst_type_id_ptr"
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
      else
        emit "#{name} = call #{return_type} @#{callee_name}(#{args})"
        record_emitted_type(name, return_type)
        # Update value_types to match EMITTED return type (not callee's return type)
        # This is critical because prepass may have determined a different type
        if return_type.includes?(".union")
          # For union types, find matching TypeRef for the emitted union
          # Use prepass_type if it matches, otherwise try to find from callee
          if prepass_type && @type_mapper.llvm_type(prepass_type).includes?(".union")
            @value_types[inst.id] = prepass_type
          elsif callee_func && @type_mapper.llvm_type(callee_func.return_type).includes?(".union")
            @value_types[inst.id] = callee_func.return_type
          else
            # Fallback to inst.type if it's a union
            if @type_mapper.llvm_type(inst.type).includes?(".union")
              @value_types[inst.id] = inst.type
            end
          end
        else
          # If the MIR type is a tuple, preserve it even if ABI uses ptr.
          if tuple_type = @module.type_registry.get(inst.type)
            if tuple_type.kind.tuple?
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
          if arg_llvm_type.includes?(".union")
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

    private def emit_extern_call(inst : ExternCall, name : String)
      if ENV.has_key?("DEBUG_EXTERN_CALL") && inst.extern_name.includes?("byte_range")
        STDERR.puts "[EXTERN_CALL] extern_name=#{inst.extern_name} args=#{inst.args.size}"
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

      cast_fixed_arg = ->(actual_type : String, value : String, expected_type : String) : String {
        return value if actual_type == expected_type

        c = @cond_counter
        @cond_counter += 1
        cast_name = "%varargs_cast.#{c}"

        if actual_type == "ptr" && expected_type.starts_with?("i")
          emit "#{cast_name} = ptrtoint ptr #{value} to #{expected_type}"
          return cast_name
        end

        if expected_type == "ptr" && actual_type.starts_with?("i")
          emit "#{cast_name} = inttoptr #{actual_type} #{value} to ptr"
          return cast_name
        end

        # Int <-> float conversions (avoid invalid LLVM bitcasts)
        if (expected_type == "double" || expected_type == "float") && actual_type.starts_with?("i")
          emit "#{cast_name} = sitofp #{actual_type} #{value} to #{expected_type}"
          return cast_name
        end

        if expected_type.starts_with?("i") && (actual_type == "double" || actual_type == "float")
          emit "#{cast_name} = fptosi #{actual_type} #{value} to #{expected_type}"
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

        if actual_type.starts_with?("i") && expected_type.starts_with?("i")
          actual_bits = actual_type[1..].to_i?
          expected_bits = expected_type[1..].to_i?
          if actual_bits && expected_bits
            if actual_bits < expected_bits
              emit "#{cast_name} = sext #{actual_type} #{value} to #{expected_type}"
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
          {"ptr", "null"}
        else
          val = value_ref(arg_id)
          if arg_type.includes?(".union") && (val == "0" || val == "null")
            {arg_type, "zeroinitializer"}
          elsif arg_type == "ptr" && val == "0"
            {"ptr", "null"}
          else
            {arg_type, val}
          end
        end
      end

      # Mangle the extern name to be a valid LLVM identifier.
      # Preserve '$' for platform-specific C symbols like realpath$DARWIN_EXTSN.
      mangled_extern_name = @type_mapper.mangle_name(inst.extern_name)
      if inst.extern_name.includes?("$") &&
         !inst.extern_name.includes?("::") &&
         !inst.extern_name.includes?("#") &&
         !inst.extern_name.includes?(".") &&
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
        elsif arg_type.starts_with?("i")
          emit "#{name} = inttoptr #{arg_type} #{arg} to ptr"
        else
          emit "#{name} = bitcast ptr #{arg} to ptr"
        end
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
          emit "#{name} = sext i32 %to_i64_val.#{c} to i64"
          @value_types[inst.id] = TypeRef::INT64
        elsif arg_llvm_type == "i64"
          emit "#{name} = add i64 #{arg_val}, 0"
          @value_types[inst.id] = TypeRef::INT64
        elsif arg_llvm_type.starts_with?("i")
          # Primitive int type - just sign extend
          emit "#{name} = sext #{arg_llvm_type} #{arg_val} to i64"
          @value_types[inst.id] = TypeRef::INT64
        else
          # Fallback - treat as i32 and extend
          emit "#{name} = sext i32 #{arg_val} to i64"
          @value_types[inst.id] = TypeRef::INT64
        end
        @value_names[inst.id] = "r#{inst.id}"
        return
      end

      if (mangled_extern_name.includes?("to_u32_") || mangled_extern_name.includes?("to_i32_") || mangled_extern_name.includes?("to_i_")) && inst.args.size == 1
        arg_id = inst.args[0]
        arg_type = @value_types[arg_id]? || TypeRef::POINTER
        arg_llvm_type = @type_mapper.llvm_type(arg_type)
        arg_val = value_ref(arg_id)

        c = @cond_counter
        @cond_counter += 1

        if arg_llvm_type.includes?(".union")
          # Union of numeric types - extract payload as i32
          emit "%to_i32_alloca.#{c} = alloca #{arg_llvm_type}, align 8"
          emit "store #{arg_llvm_type} #{normalize_union_value(arg_val, arg_llvm_type)}, ptr %to_i32_alloca.#{c}"
          emit "%to_i32_payload_ptr.#{c} = getelementptr #{arg_llvm_type}, ptr %to_i32_alloca.#{c}, i32 0, i32 1"
          emit "#{name} = load i32, ptr %to_i32_payload_ptr.#{c}, align 4"
          @value_types[inst.id] = TypeRef::INT32
        elsif arg_llvm_type == "i64"
          emit "#{name} = trunc i64 #{arg_val} to i32"
          @value_types[inst.id] = TypeRef::INT32
        elsif arg_llvm_type == "i32"
          emit "#{name} = add i32 #{arg_val}, 0"
          @value_types[inst.id] = TypeRef::INT32
        elsif arg_llvm_type.starts_with?("i")
          emit "#{name} = sext #{arg_llvm_type} #{arg_val} to i32"
          @value_types[inst.id] = TypeRef::INT32
        elsif arg_llvm_type == "ptr"
          emit "#{name} = ptrtoint ptr #{arg_val} to i32"
          @value_types[inst.id] = TypeRef::INT32
        else
          emit "#{name} = add i32 0, 0"  # Fallback to 0
          @value_types[inst.id] = TypeRef::INT32
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

        if arg_llvm_type.starts_with?("i")
          emit "#{name} = icmp eq #{arg_llvm_type} #{arg_val}, 0"
          @value_types[inst.id] = TypeRef::BOOL
          @value_names[inst.id] = "r#{inst.id}"
          return
        end
      end

      # If the mangled name doesn't match any defined function, try to find a match with namespace prefix
      # Search in MIR module's functions - but only exact matches or proper namespace matches
      # Note: Suffix matching is disabled as it causes false positives (e.g., matching initialize to unrelated methods)
      matching_func = @module.functions.find do |f|
        mangled = @type_mapper.mangle_name(f.name)
        # Only match if:
        # 1. Exact match, or
        # 2. Function name is namespace-prefixed version: "Namespace__method" == "method"
        mangled == mangled_extern_name ||
        (mangled_extern_name.includes?("#") || mangled_extern_name.includes?(".")) && mangled == mangled_extern_name
      end
      if matching_func
        mangled_extern_name = @type_mapper.mangle_name(matching_func.name)
      end

      # Type suffix heuristics - apply BEFORE void check since MIR might have wrong ptr type
      # Methods with type suffix in HIR (e.g., "unsafe_shr$UInt64").
      extern_name = inst.extern_name
      if suffix = suffix_after_dollar(extern_name)
        if !suffix.includes?("_")
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
              if first_arg_llvm.starts_with?("i") && !first_arg_llvm.includes?(".")
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
          actual_type, actual_val = arg_entries[idx]
          coerced_val = cast_fixed_arg.call(actual_type, actual_val, expected_type)
          arg_entries[idx] = {expected_type, coerced_val}
        end
      end

      args = arg_entries.map { |(t, v)| "#{t} #{v}" }.join(", ")

      if return_type == "void"
        emit "call void @#{mangled_extern_name}(#{args})"
        # Mark as void so value_ref returns a safe default for downstream uses
        @void_values << inst.id
      elsif fixed_sig
        sig_prefix = fixed_sig.join(", ")
        emit "#{name} = call #{return_type} (#{sig_prefix}, ...) @#{mangled_extern_name}(#{args})"
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
                        when "i128" then TypeRef::INT128
                        when "float" then TypeRef::FLOAT32
                        when "double" then TypeRef::FLOAT64
                        when "ptr" then TypeRef::POINTER
                        else inst.type  # Fallback to MIR type
                        end
          @value_types[inst.id] = actual_type
        end
      end

      # If prepass detected this ExternCall needs zext for phi compatibility, emit it now
      if (conversion = @phi_zext_conversions[inst.id]?)
        from_bits, to_bits = conversion
        zext_name = "#{name}.zext"
        emit "#{zext_name} = zext i#{from_bits} #{name} to i#{to_bits}"
        @zext_value_names[inst.id] = zext_name
      end
    end

    private def emit_address_of(inst : AddressOf, name : String)
      # Get address of a value (pointerof)
      # We need to alloca storage for the value, store it, and return the pointer
      operand_type = @value_types[inst.operand]? || TypeRef::POINTER
      llvm_type = @type_mapper.llvm_type(operand_type)
      operand_ref = value_ref(inst.operand)

      # Can't alloca void - use ptr instead
      if llvm_type == "void"
        llvm_type = "ptr"
        operand_ref = "null"  # void has no value, use null for ptr
      end

      # Allocate space for the value and store it
      emit "#{name}.alloca = alloca #{llvm_type}"
      # For pointer types, convert 0 to null
      store_val = (llvm_type == "ptr" && operand_ref == "0") ? "null" : operand_ref
      emit "store #{llvm_type} #{store_val}, ptr #{name}.alloca"
      # Return the pointer
      emit "#{name} = bitcast ptr #{name}.alloca to ptr"
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
      emit "#{name} = load #{llvm_type}, ptr @#{actual_global}"
    end

    private def emit_global_store(inst : GlobalStore, name : String)
      # Get value type from the stored value
      val = value_ref(inst.value)
      llvm_type = @type_mapper.llvm_type(inst.type)

      # Get actual type of the value being stored
      val_type = @value_types[inst.value]?
      val_type_str = val_type ? @type_mapper.llvm_type(val_type) : nil

      # Can't store void - use ptr instead (typically for nil assignment)
      if llvm_type == "void"
        llvm_type = "ptr"
        val = "null" if val == "null" || val.starts_with?("%r") # likely void value
      end

      # Handle type mismatches for union types
      if llvm_type.includes?(".union")
        if val_type_str && val_type_str.includes?(".union") && val_type_str != llvm_type
          # Both are unions but different types - reinterpret through memory
          c = @cond_counter
          @cond_counter += 1
          emit "%gs_conv.#{c}.ptr = alloca #{val_type_str}, align 8"
          emit "store #{val_type_str} #{val}, ptr %gs_conv.#{c}.ptr"
          emit "%gs_conv.#{c}.result = load #{llvm_type}, ptr %gs_conv.#{c}.ptr"
          val = "%gs_conv.#{c}.result"
        elsif val_type_str.nil? || !val_type_str.includes?(".union")
          # Value is not a union (or type unknown) - use zeroinitializer for the union
          val = "zeroinitializer"
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
      emit "%#{base_name}.ptr = alloca #{union_type}, align 8"

      # 2. Store type_id discriminator
      emit "%#{base_name}.type_id_ptr = getelementptr #{union_type}, ptr %#{base_name}.ptr, i32 0, i32 0"
      emit "store i32 #{inst.variant_type_id}, ptr %#{base_name}.type_id_ptr"

      # 3. Store value in payload (skip for void/nil types - they have no payload)
      val_type = @value_types[inst.value]? || TypeRef::POINTER
      val_type_str = @type_mapper.llvm_type(val_type)
      val = value_ref(inst.value)
      emit "%#{base_name}.payload_ptr = getelementptr #{union_type}, ptr %#{base_name}.ptr, i32 0, i32 1"
      if val_type_str == "void"
        emit "store i8 0, ptr %#{base_name}.payload_ptr, align 4"
      else
        val = "null" if val_type_str == "ptr" && val == "0"
        emit "store #{val_type_str} #{val}, ptr %#{base_name}.payload_ptr, align 4"
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
        # Use zeroinitializer for integer literal 0 with struct types
        store_val = (union_val == "0" || union_val == "null") ? "zeroinitializer" : union_val
        emit "store #{union_type} #{store_val}, ptr %#{base_name}.union_ptr"

        if inst.safe
          # Safe unwrap: check type_id first, return null/zero on mismatch
          emit "%#{base_name}.type_id_ptr = getelementptr #{union_type}, ptr %#{base_name}.union_ptr, i32 0, i32 0"
          emit "%#{base_name}.actual_type_id = load i32, ptr %#{base_name}.type_id_ptr"
          emit "%#{base_name}.type_match = icmp eq i32 %#{base_name}.actual_type_id, #{inst.variant_type_id}"
          emit "%#{base_name}.payload_ptr = getelementptr #{union_type}, ptr %#{base_name}.union_ptr, i32 0, i32 1"
          emit "%#{base_name}.payload = load #{result_type}, ptr %#{base_name}.payload_ptr, align 4"
          # Select null/zero if type doesn't match
          emit "#{name} = select i1 %#{base_name}.type_match, #{result_type} %#{base_name}.payload, #{result_type} zeroinitializer"
        else
          # Unsafe unwrap: just load payload (UB if type_id doesn't match)
          emit "%#{base_name}.payload_ptr = getelementptr #{union_type}, ptr %#{base_name}.union_ptr, i32 0, i32 1"
          emit "#{name} = load #{result_type}, ptr %#{base_name}.payload_ptr, align 4"
        end
      else
        # Not a union struct - just use the value directly
        # Check if union_val is an integer literal (type mismatch from defaulting to POINTER)
        is_int_literal = union_val.match(/^-?\d+$/) && union_val != "null"

        if is_int_literal
          # union_val is an int literal, use it directly for int result types
          if result_type.starts_with?("i")
            emit "#{name} = add #{result_type} #{union_val}, 0"
          elsif result_type == "ptr"
            emit "#{name} = inttoptr i64 #{union_val} to ptr"
          else
            emit "#{name} = add i64 #{union_val}, 0"
          end
        elsif result_type == "ptr" || result_type == union_type
          emit "#{name} = bitcast ptr #{union_val} to ptr"
        elsif result_type.starts_with?("i")
          # Need to convert ptr to int
          emit "#{name} = ptrtoint ptr #{union_val} to #{result_type}"
        else
          # Fallback: bitcast
          emit "#{name} = bitcast ptr #{union_val} to ptr"
        end
      end
    end

    private def emit_union_type_id_get(inst : UnionTypeIdGet, name : String)
      # Load type_id from union
      # Union may be passed by value - need to store to stack first
      union_val = value_ref(inst.union_value)
      union_type_ref = @value_types[inst.union_value]? || TypeRef::POINTER
      union_type = @type_mapper.llvm_type(union_type_ref)
      base_name = name.lstrip('%')

      # Check if LLVM type is actually a union struct (not just ptr)
      if union_type.includes?(".union")
        # Store union value to stack to get pointer for GEP
        emit "%#{base_name}.union_ptr = alloca #{union_type}, align 8"
        # Use zeroinitializer for integer literal 0 with struct types
        store_val = (union_val == "0" || union_val == "null") ? "zeroinitializer" : union_val
        emit "store #{union_type} #{store_val}, ptr %#{base_name}.union_ptr"

        emit "%#{base_name}.type_id_ptr = getelementptr #{union_type}, ptr %#{base_name}.union_ptr, i32 0, i32 0"
        emit "#{name} = load i32, ptr %#{base_name}.type_id_ptr"
      else
        # Not a union struct - determine type_id from ptr null check
        # type_id 0 = non-nil, type_id 1 = nil
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
        emit "#{name} = select i1 %#{base_name}.is_null, i32 1, i32 0"
      end
    end

    private def emit_union_is(inst : UnionIs, name : String)
      # Check if union is specific variant
      # Union may be passed by value (from load) - need to store to stack first
      union_val = value_ref(inst.union_value)
      union_type_ref = @value_types[inst.union_value]? || TypeRef::POINTER
      union_type = @type_mapper.llvm_type(union_type_ref)
      base_name = name.lstrip('%')

      # Check if LLVM type is actually a union struct (not just ptr)
      if union_type.includes?(".union")
        # Store union value to stack to get pointer for GEP
        emit "%#{base_name}.union_ptr = alloca #{union_type}, align 8"
        # Use zeroinitializer for integer literal 0 with struct types
        store_val = (union_val == "0" || union_val == "null") ? "zeroinitializer" : union_val
        emit "store #{union_type} #{store_val}, ptr %#{base_name}.union_ptr"

        # Get type_id from stored union
        emit "%#{base_name}.type_id_ptr = getelementptr #{union_type}, ptr %#{base_name}.union_ptr, i32 0, i32 0"
        emit "%#{base_name}.actual_type_id = load i32, ptr %#{base_name}.type_id_ptr"
        emit "#{name} = icmp eq i32 %#{base_name}.actual_type_id, #{inst.variant_type_id}"
      else
        # Not a union struct - compare pointer against null
        # variant_type_id 0 typically means non-nil, 1 means nil
        # Handle case where union_val is an integer literal (convert to ptr first)
        if def_inst = find_def_inst(inst.union_value)
          if def_inst.type == TypeRef::BOOL || @type_mapper.llvm_type(def_inst.type) == "i1"
            const_val = inst.variant_type_id == 1 ? "0" : "1"
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
          const_val = inst.variant_type_id == 1 ? "0" : "1"
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
          # Checking for non-nil variant: ptr != null
          emit "#{name} = icmp ne ptr #{ptr_val}, null"
        else
          # Checking for nil variant: ptr == null
          emit "#{name} = icmp eq ptr #{ptr_val}, null"
        end
      end
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
            @array_info[inst.id] = {element_type, size}
            return
          end
        end
      end

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
          # Check actual element value type and convert if needed
          actual_elem_type = @value_types[elem_id]?
          actual_elem_type_str = actual_elem_type ? @type_mapper.llvm_type(actual_elem_type) : nil
          if actual_elem_type_str && actual_elem_type_str != element_type
            # Type mismatch - need conversion
            is_elem_int = element_type.starts_with?("i") && !element_type.includes?(".union")
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
            elsif element_type == "ptr" && actual_elem_type_str.starts_with?("i") && !actual_elem_type_str.includes?(".union")
              # int → ptr: use inttoptr, but convert 0 to null directly
              if elem_val == "0"
                elem_val = "null"
              else
                emit "%#{base_name}.elem#{idx}_conv = inttoptr #{actual_elem_type_str} #{elem_val} to ptr"
                elem_val = "%#{base_name}.elem#{idx}_conv"
              end
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
        elsif array_llvm_type != "ptr" && !array_llvm_type.starts_with?("%") && !array_llvm_type.starts_with?("[") && actual_val_type != "ptr"
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

      # If the array value is actually a tuple value (struct), use extractvalue
      actual_array_llvm = lookup_value_llvm_type(inst.array_value, "")
      if actual_array_llvm.starts_with?("{")
        idx_const = nil
        if !index.starts_with?("%") && index != "null"
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
        elsif array_llvm_type != "ptr" && !array_llvm_type.starts_with?("%") && !array_llvm_type.starts_with?("[") && actual_val_type != "ptr"
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
      if array_value_type
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
            if !index.starts_with?("%") && index != "null"
              idx_const = index.to_i?
            end
            if idx_const
              element_count = tuple_type.element_types.try(&.size) || 0
              if idx_const >= 0 && idx_const < element_count
                tuple_struct = tuple_struct_llvm_type(tuple_type)
                emit "%#{base_name}.elem_ptr = getelementptr #{tuple_struct}, ptr #{array_ptr}, i32 0, i32 #{idx_const}"
              else
                # Index exceeds detected tuple element count — type tracking mismatch.
                # Fall back to byte-level GEP using element size.
                elem_size = case element_type
                            when "i8" then 1
                            when "i16" then 2
                            when "i32", "float" then 4
                            else 8
                            end
                byte_offset = idx_const * elem_size
                emit "%#{base_name}.elem_ptr = getelementptr i8, ptr #{array_ptr}, i32 #{byte_offset}"
              end
              emit "#{name} = load #{element_type}, ptr %#{base_name}.elem_ptr"
              @value_types[inst.id] = inst.element_type
              return
            end
          end
        end
      end

      # Check if index needs type conversion for i32 GEP index
      index_type = @value_types[inst.index_value]?
      if index_type
        index_llvm = @type_mapper.llvm_type(index_type)
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
          emit "store #{array_llvm_type} #{normalize_union_value(array_ptr, array_llvm_type)}, ptr %#{base_name}.union_ptr"
          emit "%#{base_name}.payload_ptr = getelementptr #{array_llvm_type}, ptr %#{base_name}.union_ptr, i32 0, i32 1"
          emit "%#{base_name}.arr_ptr = load ptr, ptr %#{base_name}.payload_ptr, align 4"
          array_ptr = "%#{base_name}.arr_ptr"
        elsif array_llvm_type != "ptr" && !array_llvm_type.starts_with?("%")
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
      elsif actual_value_type == "i1" && element_type.starts_with?("i") && element_type != "i1"
        # Convert bool (i1) to larger integer type (zext)
        emit "%#{base_name}.val_ext = zext i1 #{value} to #{element_type}"
        value = "%#{base_name}.val_ext"
      elsif actual_value_type.starts_with?("i") && !actual_value_type.includes?(".union") && element_type == "ptr"
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
      elsif actual_value_type.includes?(".union") && element_type != actual_value_type
        # Union → different type: extract payload and convert
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
        emit "store ptr #{value}, ptr %#{base_name}.val_pay_ptr"
        emit "%#{base_name}.val_union = load #{element_type}, ptr %#{base_name}.val_union_ptr"
        value = "%#{base_name}.val_union"
      elsif actual_value_type.starts_with?("i") && element_type.starts_with?("i") && actual_value_type != element_type
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

      # Get element pointer from data array (second field) and store
      emit "%#{base_name}.elem_ptr = getelementptr { i32, [0 x #{element_type}] }, ptr #{array_ptr}, i32 0, i32 1, i32 #{index}"
      emit "store #{element_type} #{value}, ptr %#{base_name}.elem_ptr"
    end

    private def emit_string_interpolation(inst : StringInterpolation, name : String)
      base_name = name.lstrip('%')

      # Convert each part to string ptr, handling type conversion
      string_parts = [] of String
      inst.parts.each_with_index do |part_id, idx|
        part_type = @value_types[part_id]?
        part_llvm_type_check = part_type ? @type_mapper.llvm_type(part_type) : nil

        # Check for void type - void calls don't produce a value, use empty string
        if part_llvm_type_check == "void"
          string_parts << "@.str.empty"
          next
        end

        part_ref = value_ref(part_id)

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
          elsif part_llvm_type == "double"
            emit "%#{base_name}.conv#{idx} = call ptr @__crystal_v2_f64_to_string(double #{part_ref})"
            string_parts << "%#{base_name}.conv#{idx}"
          elsif part_llvm_type == "float"
            # Convert float to double first
            emit "%#{base_name}.ext#{idx} = fpext float #{part_ref} to double"
            emit "%#{base_name}.conv#{idx} = call ptr @__crystal_v2_f64_to_string(double %#{base_name}.ext#{idx})"
            string_parts << "%#{base_name}.conv#{idx}"
          elsif part_llvm_type.includes?(".union")
            # Union type (e.g. String | Nil) - extract ptr from payload
            emit "%#{base_name}.union_ptr#{idx} = alloca #{part_llvm_type}, align 8"
            emit "store #{part_llvm_type} #{normalize_union_value(part_ref, part_llvm_type)}, ptr %#{base_name}.union_ptr#{idx}"
            emit "%#{base_name}.payload_ptr#{idx} = getelementptr #{part_llvm_type}, ptr %#{base_name}.union_ptr#{idx}, i32 0, i32 1"
            emit "%#{base_name}.str_ptr#{idx} = load ptr, ptr %#{base_name}.payload_ptr#{idx}, align 4"
            string_parts << "%#{base_name}.str_ptr#{idx}"
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
          is_undefined = (@value_names[val]?.nil? && @constant_values[val]?.nil?) ||
                         val_llvm_type == "void"
          if is_undefined
            # Undefined value - use safe default
            if @current_return_type == "ptr"
              emit "ret ptr null"
            elsif @current_return_type.includes?(".union")
              # Return nil union
              c = @cond_counter
              @cond_counter += 1
              emit "%ret_nil.#{c}.ptr = alloca #{@current_return_type}, align 8"
              emit "%ret_nil.#{c}.type_id_ptr = getelementptr #{@current_return_type}, ptr %ret_nil.#{c}.ptr, i32 0, i32 0"
              emit "store i32 1, ptr %ret_nil.#{c}.type_id_ptr"
              emit "%ret_nil.#{c}.val = load #{@current_return_type}, ptr %ret_nil.#{c}.ptr"
              emit "ret #{@current_return_type} %ret_nil.#{c}.val"
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

            if @current_return_type.includes?(".union") && val_ref == "null"
              c = @cond_counter
              @cond_counter += 1
              emit "%ret_nil_union.#{c}.ptr = alloca #{@current_return_type}, align 8"
              emit "%ret_nil_union.#{c}.type_id_ptr = getelementptr #{@current_return_type}, ptr %ret_nil_union.#{c}.ptr, i32 0, i32 0"
              emit "store i32 1, ptr %ret_nil_union.#{c}.type_id_ptr"
              emit "%ret_nil_union.#{c}.val = load #{@current_return_type}, ptr %ret_nil_union.#{c}.ptr"
              emit "ret #{@current_return_type} %ret_nil_union.#{c}.val"
              return
            end

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
            elsif @phi_nil_incoming_blocks.has_key?(val) && val_llvm_type.starts_with?("i")
              # Value comes from a phi that has nil incoming - emit conditional type_id
              # When nil flows into an integer phi, it becomes 0, so check if val == 0
              # TODO: This heuristic assumes non-nil values are never 0, which may not always be true
              emit "%ret#{c}.is_nil = icmp eq #{val_llvm_type} #{val_ref}, 0"
              emit "%ret#{c}.type_id = select i1 %ret#{c}.is_nil, i32 1, i32 0"
              emit "store i32 %ret#{c}.type_id, ptr %ret#{c}.type_id_ptr"
              emit "%ret#{c}.payload_ptr = getelementptr #{@current_return_type}, ptr %ret#{c}.union_ptr, i32 0, i32 1"
              emit "store #{val_llvm_type} #{val_ref}, ptr %ret#{c}.payload_ptr, align 4"
            else
              # Non-nil value - set type_id=0 and store payload
              emit "store i32 0, ptr %ret#{c}.type_id_ptr"
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
              # Union to different union - copy type_id and payload to new union struct
              c = @cond_counter
              @cond_counter += 1
              # Store source union to get its pointers
              emit "%ret_union_conv.#{c}.src_ptr = alloca #{val_llvm_type}, align 8"
              emit "store #{val_llvm_type} #{val_ref}, ptr %ret_union_conv.#{c}.src_ptr"
              # Get source type_id
              emit "%ret_union_conv.#{c}.src_type_ptr = getelementptr #{val_llvm_type}, ptr %ret_union_conv.#{c}.src_ptr, i32 0, i32 0"
              emit "%ret_union_conv.#{c}.type_id = load i32, ptr %ret_union_conv.#{c}.src_type_ptr"
              # Create destination union
              emit "%ret_union_conv.#{c}.dst_ptr = alloca #{@current_return_type}, align 8"
              # Store type_id to destination
              emit "%ret_union_conv.#{c}.dst_type_ptr = getelementptr #{@current_return_type}, ptr %ret_union_conv.#{c}.dst_ptr, i32 0, i32 0"
              emit "store i32 %ret_union_conv.#{c}.type_id, ptr %ret_union_conv.#{c}.dst_type_ptr"
              # Copy payload bytes from source to destination (use memcpy or byte-wise copy via ptr)
              emit "%ret_union_conv.#{c}.src_payload_ptr = getelementptr #{val_llvm_type}, ptr %ret_union_conv.#{c}.src_ptr, i32 0, i32 1"
              emit "%ret_union_conv.#{c}.dst_payload_ptr = getelementptr #{@current_return_type}, ptr %ret_union_conv.#{c}.dst_ptr, i32 0, i32 1"
              emit "%ret_union_conv.#{c}.payload_as_ptr = load ptr, ptr %ret_union_conv.#{c}.src_payload_ptr, align 4"
              emit "store ptr %ret_union_conv.#{c}.payload_as_ptr, ptr %ret_union_conv.#{c}.dst_payload_ptr, align 4"
              # Load and return destination union
              emit "%ret_union_conv.#{c}.result = load #{@current_return_type}, ptr %ret_union_conv.#{c}.dst_ptr"
              emit "ret #{@current_return_type} %ret_union_conv.#{c}.result"
            elsif @current_return_type.starts_with?("i") && val_llvm_type && val_llvm_type.starts_with?("i") && @current_return_type != val_llvm_type
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
            elsif (@current_return_type == "double" || @current_return_type == "float") && val_llvm_type && val_llvm_type.starts_with?("i")
              # Integer to float conversion (signed/unsigned)
              c = @cond_counter
              @cond_counter += 1
              op = (val_type && unsigned_type_ref?(val_type)) ? "uitofp" : "sitofp"
              emit "%ret_itof.#{c} = #{op} #{val_llvm_type} #{val_ref} to #{@current_return_type}"
              emit "ret #{@current_return_type} %ret_itof.#{c}"
            elsif val_llvm_type && (val_llvm_type == "double" || val_llvm_type == "float") && @current_return_type.starts_with?("i")
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
              # Handle literal values for return types
              actual_val = if (@current_return_type == "double" || @current_return_type == "float") && (val_ref == "0" || val_ref == "null")
                             "0.0"
                           elsif @current_return_type == "ptr" && val_ref == "0"
                             "null"
                           elsif @current_return_type.starts_with?("i") && val_ref == "null"
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
          elsif @current_return_type.starts_with?("i")
            emit "ret #{@current_return_type} 0"
          elsif @current_return_type.includes?(".union")
            # Return nil union (type_id = 1)
            c = @cond_counter
            @cond_counter += 1
            emit "%ret_nil_union.#{c}.ptr = alloca #{@current_return_type}, align 8"
            emit "%ret_nil_union.#{c}.type_id_ptr = getelementptr #{@current_return_type}, ptr %ret_nil_union.#{c}.ptr, i32 0, i32 0"
            emit "store i32 1, ptr %ret_nil_union.#{c}.type_id_ptr"
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

        # Check if condition is a union type - need to extract type_id and compare
        # Also verify LLVM type is actually a union struct (not just ptr)
        union_llvm_type = slot_llvm_type || (cond_type ? @type_mapper.llvm_type(cond_type) : nil)
        if union_llvm_type && union_llvm_type.includes?(".union")
          # Union layout: { i32 type_id, [N x i8] payload }
          # Extract type_id and compare with 1 (nil variant)
          c = @cond_counter
          @cond_counter += 1
          emit "%cond#{c}.union_ptr = alloca #{union_llvm_type}, align 8"
          emit "store #{union_llvm_type} #{normalize_union_value(cond, union_llvm_type)}, ptr %cond#{c}.union_ptr"
          emit "%cond#{c}.type_id_ptr = getelementptr #{union_llvm_type}, ptr %cond#{c}.union_ptr, i32 0, i32 0"
          emit "%cond#{c}.type_id = load i32, ptr %cond#{c}.type_id_ptr"
          # type_id 0 = non-nil (truthy), type_id 1 = nil (falsy)
          emit "%cond#{c}.is_not_nil = icmp ne i32 %cond#{c}.type_id, 1"
          emit "br i1 %cond#{c}.is_not_nil, label %#{then_block}, label %#{else_block}"
        elsif cond_type && @type_mapper.llvm_type(cond_type) == "ptr" && (slot_llvm_type.nil? || slot_llvm_type == "ptr")
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
        term.cases.each do |(case_val, block)|
          emit "#{val_llvm_type} #{case_val}, label %#{@block_names[block]}"
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
        llvm_type = @cross_block_slot_types[id]? ||
          (val_type ? @type_mapper.llvm_type(val_type) : "i64")
        llvm_type = "i64" if llvm_type == "void"
        temp_name = "%r#{id}.fromslot.#{@cond_counter}"
        @cond_counter += 1
        emit "#{temp_name} = load #{llvm_type}, ptr %#{slot_name}"
        # If the slot type differs from the expected value type, insert a cast.
        expected_type = val_type ? @type_mapper.llvm_type(val_type) : llvm_type
        if expected_type != llvm_type && expected_type != "void"
          cast_name = "%r#{id}.fromslot.cast.#{@cond_counter}"
          @cond_counter += 1
          if llvm_type.starts_with?("i") && expected_type.starts_with?("i")
            src_bits = llvm_type[1..].to_i?
            dst_bits = expected_type[1..].to_i?
            if src_bits && dst_bits
              if dst_bits < src_bits
                emit "#{cast_name} = trunc #{llvm_type} #{temp_name} to #{expected_type}"
              elsif dst_bits > src_bits
                emit "#{cast_name} = sext #{llvm_type} #{temp_name} to #{expected_type}"
              else
                emit "#{cast_name} = add #{expected_type} #{temp_name}, 0"
              end
              record_emitted_type(cast_name, expected_type)
              return cast_name
            end
          elsif llvm_type.starts_with?("i") && (expected_type == "float" || expected_type == "double")
            op = (val_type && unsigned_type_ref?(val_type)) ? "uitofp" : "sitofp"
            emit "#{cast_name} = #{op} #{llvm_type} #{temp_name} to #{expected_type}"
            record_emitted_type(cast_name, expected_type)
            return cast_name
          elsif (llvm_type == "float" || llvm_type == "double") && expected_type.starts_with?("i")
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
          elsif expected_type == "ptr" && llvm_type.starts_with?("i")
            emit "#{cast_name} = inttoptr #{llvm_type} #{temp_name} to ptr"
            record_emitted_type(cast_name, "ptr")
            return cast_name
          elsif llvm_type == "ptr" && expected_type.starts_with?("i")
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
          elsif (expected_type == "double" || expected_type == "float" || (expected_type.starts_with?("i") && !expected_type.includes?("."))) && llvm_type.includes?(".union")
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
            # Different union types - bitcast through ptr (they have same layout)
            emit "#{cast_name} = bitcast #{llvm_type} #{temp_name} to #{expected_type}"
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
        # Look up parameter by name
        @current_func_params.each do |param|
          if param.name == name
            param_type = param.type == TypeRef::VOID ? TypeRef::POINTER : param.type
            return @type_mapper.llvm_type(param_type)
          end
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
