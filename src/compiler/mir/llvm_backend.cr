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
    @current_func_params : Array(Parameter) = [] of Parameter
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
    @emitted_allocas : Set(ValueId) = Set(ValueId).new  # Track pre-emitted allocas
    @pending_allocas : Array({String, String, Int32}) = [] of {String, String, Int32}  # name, type, align
    @string_counter : Int32 = 0
    @cond_counter : Int32 = 0  # For unique branch condition variable names

    # Cross-block value tracking for dominance fix
    @value_def_block : Hash(ValueId, BlockId) = {} of ValueId => BlockId  # value → block where defined
    @cross_block_values : Set(ValueId) = Set(ValueId).new  # values that need alloca slots
    @cross_block_slots : Hash(ValueId, String) = {} of ValueId => String  # value → alloca slot name
    @in_phi_mode : Bool = false  # When true, value_ref returns default instead of emitting load

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

      # Find main function
      main_func = @module.functions.find { |f| f.name == "main" }
      if main_func
        worklist << main_func.id
        reachable << main_func.id
      else
        # No main, emit all functions
        @module.functions.each { |f| reachable << f.id }
        return reachable
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
        # Declare with varargs to accept any arguments
        emit_raw "declare #{return_type} @#{name}(...)\n"
      end
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
        function_names << @type_mapper.mangle_name(func.name)
      end

      # Emit defined globals
      @module.globals.each do |global|
        llvm_type = @type_mapper.llvm_type(global.type)
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

      # Memory allocation - just wrap malloc
      emit_raw "define ptr @__crystal_v2_malloc64(i64 %size) {\n"
      emit_raw "  %ptr = call ptr @malloc(i64 %size)\n"
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

      # Entry point: main() calls __crystal_main()
      emit_raw "; Program entry point\n"
      emit_raw "define i32 @main(i32 %argc, ptr %argv) {\n"
      emit_raw "  call void @__crystal_main(i32 %argc, ptr %argv)\n"
      emit_raw "  ret i32 0\n"
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
      @emitted_allocas.clear
      @value_def_block.clear
      @cross_block_values.clear
      @cross_block_slots.clear

      # Pre-pass: collect constant values for phi node resolution
      # This ensures forward-referenced constants are available
      prepass_collect_constants(func)

      # Pre-pass: detect cross-block values that need alloca slots for dominance
      prepass_detect_cross_block_values(func)

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
      @current_func_params = func.params

      # Skip functions that would conflict with C library declarations
      c_library_functions = Set{"printf", "sprintf", "snprintf", "fprintf", "vprintf",
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
                                "getenv", "setenv", "unsetenv", "system"}
      if c_library_functions.includes?(mangled_name)
        return
      end

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
      # Jump to first user block
      if first_block = func.blocks.first?
        emit_raw "  br label %#{@block_names[first_block.id]}\n"
      end

      # TSan: emit function entry in first block
      @tsan_needs_func_entry = @emit_tsan

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
        llvm_type = "i64" if llvm_type == "void"  # fallback
        slot_name = "%r#{val_id}.slot"
        @cross_block_slots[val_id] = "r#{val_id}.slot"
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
          elsif inst.is_a?(Call)
            # For Call instructions, check the callee's actual return type
            # If callee returns void, mark as void even if inst.type says otherwise
            callee_func = @module.functions.find { |f| f.id == inst.callee }
            if callee_func
              callee_ret_type = @type_mapper.llvm_type(callee_func.return_type)
              callee_name = @type_mapper.mangle_name(callee_func.name)
              # DEBUG: check what callee names look like for low IDs
              if inst.id < 50
                # STDERR.puts "[PREPASS-CALLEE-FOUND] id=#{inst.id}, callee_name=#{callee_name}, ret=#{callee_ret_type}"
              end
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
              # DEBUG: trace inspect calls
              if callee_name.includes?("inspect")
                # STDERR.puts "[PREPASS-INSPECT] id=#{inst.id}, callee=#{callee_name}, callee_ret=#{callee_ret_type}, is_known=#{is_known_void}, inst_type=#{@type_mapper.llvm_type(inst.type)}"
              end
              if callee_ret_type == "void" || is_known_void
                effective_type = TypeRef::VOID
              end
            else
              # Callee not found - check if MIR type is void or name suggests void
              inst_type_str = @type_mapper.llvm_type(inst.type)
              # DEBUG: trace callee not found cases
              # STDERR.puts "[PREPASS-NO-CALLEE] id=#{inst.id}, callee_id=#{inst.callee}, inst_type=#{inst_type_str}"
              if inst_type_str == "void"
                effective_type = TypeRef::VOID
              end
            end
          elsif inst.is_a?(ExternCall)
            # For ExternCall, the return type is typically determined at emit time
            # If MIR type is void, it's void; otherwise check extern declaration if available
            extern_type_str = @type_mapper.llvm_type(inst.type)
            # DEBUG: trace all ExternCall (disabled)
            # if inst.extern_name.includes?("inspect") || inst.extern_name.includes?("puts") || inst.extern_name.includes?("print")
            #   STDERR.puts "[PREPASS-EXTERN] id=#{inst.id}, name=#{inst.extern_name}, type=#{extern_type_str}"
            # end
            if extern_type_str == "void"
              effective_type = TypeRef::VOID
            end
          elsif inst.is_a?(BinaryOp)
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
            end
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
            # ExternCall type comes from the instruction itself (already set to void in prepass)
            # If it was marked void by prepass, it's genuinely void
            next  # ExternCall with void type is genuinely void
          end

          # If this void-typed value is used anywhere, it's not really void
          if used_values.includes?(inst.id)
            # Infer type from usage context
            inferred_type = usage_contexts[inst.id]? || TypeRef::POINTER
            @value_types[inst.id] = inferred_type
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
                        else              [] of ValueId
                        end

          operand_ids.each do |op_id|
            def_block = @value_def_block[op_id]?
            next unless def_block

            # Value defined in different block than where it's used
            if def_block != block.id
              # Check if definition block is entry block - entry dominates all
              next if def_block == entry_block_id

              # Check if definition block is bb0 (block id 0 or 1 typically after entry)
              # Conservative: any non-entry definition used cross-block is suspect
              # Check the instruction type that defines this value
              def_inst = nil
              func.blocks.each do |b|
                def_inst = b.instructions.find { |i| i.id == op_id }
                break if def_inst
              end

              # Flag instructions that commonly cause cross-block domination issues
              # Includes: Load, ArrayGet, GetElementPtr, GetElementPtrDynamic, Call, ExternCall
              # But exclude void-returning calls (they don't produce values to track)
              is_void_call = (def_inst.is_a?(Call) || def_inst.is_a?(ExternCall)) &&
                             def_inst.type == TypeRef::VOID
              if !is_void_call &&
                 (def_inst.is_a?(Load) || def_inst.is_a?(ArrayGet) || def_inst.is_a?(GetElementPtr) ||
                  def_inst.is_a?(GetElementPtrDynamic) || def_inst.is_a?(Call) || def_inst.is_a?(ExternCall))
                @cross_block_values << op_id
              end
            end
          end
        end

        # Also check phi incoming values
        block.instructions.each do |inst|
          next unless inst.is_a?(Phi)
          inst.incoming.each do |(from_block, val_id)|
            def_block = @value_def_block[val_id]?
            next unless def_block
            # Phi use is in current block, but value may be defined elsewhere
            # This is usually fine for phi, but check for loop-carried values
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
        end
      end
    end

    private def reset_value_names(func : Function)
      @value_names.clear
      @block_names.clear
      @constant_values.clear
      @value_types.clear
      @void_values.clear
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

      # Emit phi nodes first
      phi_insts.each do |inst|
        emit_instruction(inst, func)
      end

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

      emit_terminator(block.terminator)
      @indent = 0
    end

    private def emit_instruction(inst : Value, func : Function)
      name = "%r#{inst.id}"

      # Check if this instruction produces a value (has a result register)
      # Store, Free, RCIncrement, RCDecrement, GlobalStore, AtomicStore don't produce values
      produces_value = !inst.is_a?(Store) && !inst.is_a?(Free) &&
                       !inst.is_a?(RCIncrement) && !inst.is_a?(RCDecrement) &&
                       !inst.is_a?(GlobalStore) && !inst.is_a?(AtomicStore)

      # Only register value_names for instructions that produce values
      if produces_value
        @value_names[inst.id] = "r#{inst.id}"
        # Preserve prepass type for phi nodes that were converted to ptr
        # For other instructions, set the MIR type (will be overwritten by emit_* if needed)
        unless inst.is_a?(Phi) && @value_types[inst.id]? == TypeRef::POINTER
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
        emit "store #{llvm_type} #{name}, ptr %#{slot_name}"
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
      elsif type == "double" || type == "float"
        # Float/double constants use fadd
        emit "#{name} = fadd #{type} 0.0, #{value}"
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
      # Cross-block store is now handled centrally in emit_instruction
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

      # Void is not a valid GEP element type - use i8 (byte pointer arithmetic)
      if element_type == "void"
        element_type = "i8"
      end

      # Check base type - GEP requires pointer base
      base_type = @value_types[inst.base]?
      base_type_str = base_type ? @type_mapper.llvm_type(base_type) : "ptr"

      # If base is not a pointer, we have a type mismatch from MIR
      # Convert integer types to pointer using inttoptr
      if base_type_str != "ptr"
        if base_type_str.includes?(".union")
          # Union type - extract payload as ptr
          emit "#{name}.union_ptr = alloca #{base_type_str}, align 8"
          emit "store #{base_type_str} #{normalize_union_value(base, base_type_str)}, ptr #{name}.union_ptr"
          emit "#{name}.payload_ptr = getelementptr #{base_type_str}, ptr #{name}.union_ptr, i32 0, i32 1"
          emit "#{name}.base_ptr = load ptr, ptr #{name}.payload_ptr"
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
        # Convert integer type to i64 with sign extension
        ext_name = "#{name}.idx64"
        emit "#{ext_name} = sext #{index_type_str} #{index} to i64"
        emit "#{name} = getelementptr #{element_type}, ptr #{base}, i64 #{ext_name}"
      end
      @value_types[inst.id] = TypeRef::POINTER  # GEP always returns pointer
    end

    private def emit_binary_op(inst : BinaryOp, name : String)
      result_type = @type_mapper.llvm_type(inst.type)
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
        # Convert ptr operands to float (ptrtoint then sitofp)
        if left.starts_with?("%") && operand_type_str == "ptr"
          emit "%binop#{inst.id}.left_ptrtoint = ptrtoint ptr #{left} to i64"
          emit "%binop#{inst.id}.left_itof = sitofp i64 %binop#{inst.id}.left_ptrtoint to #{float_type}"
          left = "%binop#{inst.id}.left_itof"
          operand_type_str = float_type
        end
        if right.starts_with?("%") && right_type_str == "ptr"
          emit "%binop#{inst.id}.right_ptrtoint = ptrtoint ptr #{right} to i64"
          emit "%binop#{inst.id}.right_itof = sitofp i64 %binop#{inst.id}.right_ptrtoint to #{float_type}"
          right = "%binop#{inst.id}.right_itof"
          right_type_str = float_type
        end
        # Convert integer SSA values to float for float operations
        if left.starts_with?("%") && operand_type_str.starts_with?("i")
          emit "%binop#{inst.id}.left_itof = sitofp #{operand_type_str} #{left} to #{float_type}"
          left = "%binop#{inst.id}.left_itof"
          operand_type_str = float_type
        end
        if right.starts_with?("%") && right_type_str.starts_with?("i")
          emit "%binop#{inst.id}.right_itof = sitofp #{right_type_str} #{right} to #{float_type}"
          right = "%binop#{inst.id}.right_itof"
          right_type_str = float_type
        end
        # Update result type to match float type
        result_type = float_type
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
          emit "%binop#{inst.id}.left_as_ptr = load ptr, ptr %binop#{inst.id}.left_payload_ptr"
          emit "%binop#{inst.id}.left_as_int = ptrtoint ptr %binop#{inst.id}.left_as_ptr to #{int_type}"
          left = "%binop#{inst.id}.left_as_int"
          operand_type_str = int_type
        end
        if right_type_str.includes?(".union")
          emit "%binop#{inst.id}.right_union_ptr = alloca #{right_type_str}, align 8"
          emit "store #{right_type_str} #{normalize_union_value(right, right_type_str)}, ptr %binop#{inst.id}.right_union_ptr"
          emit "%binop#{inst.id}.right_payload_ptr = getelementptr #{right_type_str}, ptr %binop#{inst.id}.right_union_ptr, i32 0, i32 1"
          emit "%binop#{inst.id}.right_as_ptr = load ptr, ptr %binop#{inst.id}.right_payload_ptr"
          emit "%binop#{inst.id}.right_as_int = ptrtoint ptr %binop#{inst.id}.right_as_ptr to #{int_type}"
          right = "%binop#{inst.id}.right_as_int"
          right_type_str = int_type
        end

        # After extracting union operands to integers, update result_type if it's also a union
        if result_type.includes?(".union") && is_arithmetic
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
            emit "%#{base_name}.bool = icmp ne ptr #{operand}, null"
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

      # Guard: can't cast from void - use null/0 as fallback
      if src_type == "void"
        if dst_type == "ptr" || dst_type.includes?(".union")
          emit "#{name} = inttoptr i64 0 to ptr"
          @value_types[inst.id] = TypeRef::POINTER
          return
        else
          # Non-ptr destination - use 0
          emit "#{name} = add #{dst_type} 0, 0"
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
        emit "#{name} = load ptr, ptr %#{base_name}.payload_ptr"
        @value_types[inst.id] = TypeRef::POINTER
        return
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

      emit "#{name} = #{op} #{src_type} #{value} to #{dst_type}"
      # Track actual destination type for downstream use
      @value_types[inst.id] = inst.type
    end

    private def emit_phi(inst : Phi, name : String)
      phi_type = @type_mapper.llvm_type(inst.type)

      # Enable phi mode to prevent value_ref from emitting loads
      # (phi nodes must be grouped at top of basic block)
      @in_phi_mode = true

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
            # Check if value was emitted before using value_ref
            val_emitted = @value_names.has_key?(val)
            val_is_const = @constant_values.has_key?(val)
            if !val_emitted && !val_is_const
              # Undefined value in prepass ptr phi - use null
              "[null, %#{@block_names[block]}]"
            else
              ref = value_ref(val)
              "[#{ref}, %#{@block_names[block]}]"
            end
          end
        end
        emit "#{name} = phi ptr #{incoming.join(", ")}"
        # @value_types already set by prepass
        @in_phi_mode = false
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
        has_mismatched_incoming = inst.incoming.any? do |(block, val)|
          val_type = @value_types[val]?
          val_type_str = val_type ? @type_mapper.llvm_type(val_type) : nil
          val_type_str && val_type_str != "i1" && val_type_str != "void"
        end
        if has_mismatched_incoming
          incoming = inst.incoming.map do |(block, val)|
            val_type = @value_types[val]?
            val_type_str = val_type ? @type_mapper.llvm_type(val_type) : nil
            if val_type_str && val_type_str.includes?(".union")
              # Union value - use 0 (false) as default
              "[0, %#{@block_names[block]}]"
            elsif val_type_str == "ptr" || val_type_str == "void"
              # Ptr/void value flowing into i1 phi - use 0 (type mismatch)
              "[0, %#{@block_names[block]}]"
            elsif val_type_str && val_type_str.starts_with?("i") && val_type_str != "i1"
              # Larger int (i8, i16, i32, i64) flowing into i1 phi - use 0 (type mismatch)
              # Can't truncate in phi node, so use 0 as safe default
              "[0, %#{@block_names[block]}]"
            elsif val_type_str == "float" || val_type_str == "double"
              # Float/double value flowing into i1 phi - use 0 (type mismatch)
              "[0, %#{@block_names[block]}]"
            else
              # Check if value was emitted before using value_ref
              val_emitted = @value_names.has_key?(val)
              val_is_const = @constant_values.has_key?(val)
              if !val_emitted && !val_is_const
                # Undefined value in bool phi - use 0
                "[0, %#{@block_names[block]}]"
              else
                ref = value_ref(val)
                # Guard against null and float literals for non-ptr phi
                ref = "0" if ref == "null" || ref.includes?(".")
                "[#{ref}, %#{@block_names[block]}]"
              end
            end
          end
          emit "#{name} = phi i1 #{incoming.join(", ")}"
          @value_types[inst.id] = TypeRef::BOOL
          @in_phi_mode = false
          return
        end
      end

      # Check if ptr phi has incompatible incoming (union, int, float) - use null for them
      # Can't extract/convert in current block for phi, so use null (lossy but compiles)
      if is_ptr_type
        has_incompatible_incoming = inst.incoming.any? do |(block, val)|
          val_type = @value_types[val]?
          next false unless val_type
          val_type_str = @type_mapper.llvm_type(val_type)
          # Union, int, or float/double are incompatible with ptr
          val_type_str.includes?(".union") ||
            (val_type_str.starts_with?("i") && val_type_str != "i1") ||
            val_type_str == "float" || val_type_str == "double"
        end
        if has_incompatible_incoming
          incoming = inst.incoming.map do |(block, val)|
            val_type = @value_types[val]?
            val_type_str = val_type ? @type_mapper.llvm_type(val_type) : nil
            if val_type_str && val_type_str.includes?(".union")
              # Union value can't be directly used in ptr phi - use null
              "[null, %#{@block_names[block]}]"
            elsif val_type_str && val_type_str.starts_with?("i") && !val_type_str.includes?(".union")
              # Int value in ptr phi - use null
              "[null, %#{@block_names[block]}]"
            elsif val_type_str == "float" || val_type_str == "double"
              # Float/double value in ptr phi - use null
              "[null, %#{@block_names[block]}]"
            else
              # Check if value was emitted before using value_ref
              val_emitted = @value_names.has_key?(val)
              val_is_const = @constant_values.has_key?(val)
              if !val_emitted && !val_is_const
                # Undefined value in ptr phi - use null
                "[null, %#{@block_names[block]}]"
              else
                ref = value_ref(val)
                "[#{ref}, %#{@block_names[block]}]"
              end
            end
          end
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
          # Debug: check if this is emitting wrong type
          if @current_func_name.includes?("backreferences")
            STDERR.puts "[DEBUG-BACKREF] inst.id=#{inst.id} phi_type=#{phi_type} has_ptr_or_unknown=true"
          end
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
              # Check if value was emitted before using value_ref
              val_emitted = @value_names.has_key?(val)
              val_is_const = @constant_values.has_key?(val)
              if !val_emitted && !val_is_const
                # Undefined value in ptr phi - use null
                "[null, %#{@block_names[block]}]"
              else
                ref = value_ref(val)
                # For unknown types, assume they're ptr-compatible
                "[#{ref}, %#{@block_names[block]}]"
              end
            end
          end
          emit "#{name} = phi ptr #{incoming.join(", ")}"
          @value_types[inst.id] = TypeRef::POINTER
          @in_phi_mode = false
          return
        end

        # Check if any incoming value is a non-union type (like plain i32) when phi expects union
        # OR if any incoming value is a DIFFERENT union type
        # This happens when MIR doesn't properly wrap values in unions before phi
        has_non_union_incoming = inst.incoming.any? do |(block, val)|
          val_type = @value_types[val]?
          val_llvm_type = val_type ? @type_mapper.llvm_type(val_type) : nil
          # Value has a type but it's not a union and not ptr (ptr handled above)
          val_llvm_type && !val_llvm_type.includes?(".union") && val_llvm_type != "ptr"
        end
        # Also check for different union types (e.g., UInt8___Nil.union vs String___Nil.union)
        has_different_union_incoming = inst.incoming.any? do |(block, val)|
          val_type = @value_types[val]?
          val_llvm_type = val_type ? @type_mapper.llvm_type(val_type) : nil
          # Value is a union but different from phi type
          val_llvm_type && val_llvm_type.includes?(".union") && val_llvm_type != phi_type
        end
        if has_non_union_incoming || has_different_union_incoming
          # Debug
          if @current_func_name.includes?("backreferences")
            STDERR.puts "[DEBUG-BACKREF2] inst.id=#{inst.id} has_non_union=#{has_non_union_incoming} has_diff_union=#{has_different_union_incoming}"
          end
          # Emit union phi with zeroinitializer for non-union or different union values
          # We can't convert between union types in phi instruction, so use zeroinitializer (nil case)
          incoming = inst.incoming.map do |(block, val)|
            val_type = @value_types[val]?
            val_type_str = val_type ? @type_mapper.llvm_type(val_type) : nil
            if val_type_str && val_type_str == phi_type
              # Same union type - use value directly
              ref = value_ref(val)
              # If value_ref returned "null", convert to zeroinitializer for union
              ref = "zeroinitializer" if ref == "null"
              if @current_func_name.includes?("backreferences")
                STDERR.puts "[DEBUG-BACKREF2]   block=#{block} val=#{val} val_type_str=#{val_type_str} ref=#{ref}"
              end
              "[#{ref}, %#{@block_names[block]}]"
            else
              # Type mismatch (non-union or different union) - use zeroinitializer (nil)
              if @current_func_name.includes?("backreferences")
                STDERR.puts "[DEBUG-BACKREF2]   block=#{block} val=#{val} val_type_str=#{val_type_str} -> zeroinitializer"
              end
              "[zeroinitializer, %#{@block_names[block]}]"
            end
          end
          emit "#{name} = phi #{phi_type} #{incoming.join(", ")}"
          @value_types[inst.id] = inst.type
          @in_phi_mode = false
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

        # Debug: trace union phi values that produce null
        ref_preview = if val_is_const
                        const_val
                      elsif val_emitted
                        "%r#{val}"
                      else
                        "undefined"
                      end
        if is_union_type && (ref_preview == "null" || ref_preview == "undefined")
          STDERR.puts "[DEBUG-PHI-NULL] func=#{@current_func_name} inst.id=#{inst.id} block=#{block} val=#{val} const_val=#{const_val} val_emitted=#{val_emitted} val_is_const=#{val_is_const} is_void=#{is_void_value}"
        end

        if (!val_emitted || is_void_value) && !val_is_const
          # Use safe default based on phi type
          if is_union_type
            "[zeroinitializer, %#{@block_names[block]}]"
          elsif is_ptr_type
            "[null, %#{@block_names[block]}]"
          elsif is_int_type || is_bool_type
            "[0, %#{@block_names[block]}]"
          elsif is_float_type
            "[0.0, %#{@block_names[block]}]"
          else
            "[null, %#{@block_names[block]}]"
          end
        elsif const_val == "null" && is_union_type
          # null constant in union phi - use zeroinitializer
          "[zeroinitializer, %#{@block_names[block]}]"
        elsif const_val == "null" && (is_int_type || is_bool_type)
          "[0, %#{@block_names[block]}]"
        elsif const_val == "null" && is_float_type
          # null flowing into float phi - use 0.0
          "[0.0, %#{@block_names[block]}]"
        elsif const_val == "0" && is_ptr_type
          "[null, %#{@block_names[block]}]"
        else
          # Check for type mismatch
          val_type = @value_types[val]?
          val_type_str = val_type ? @type_mapper.llvm_type(val_type) : nil

          if (is_int_type || is_bool_type) && val_type_str && val_type_str.includes?(".union")
            # Union flowing into int/bool phi - use 0 (nil case)
            "[0, %#{@block_names[block]}]"
          elsif (is_int_type || is_bool_type) && (val_type_str == "ptr" || val_type_str == "void")
            # Ptr/void flowing into int/bool phi - use 0 (type mismatch from MIR)
            "[0, %#{@block_names[block]}]"
          elsif is_int_type && (val_type_str == "double" || val_type_str == "float")
            # Float/double flowing into int phi - use 0 as safe default
            # We can't bitcast here because it would be in wrong block
            "[0, %#{@block_names[block]}]"
          elsif is_int_type && val_type_str && val_type_str.starts_with?("i") && val_type_str != phi_type
            # Integer width mismatch (e.g., i64 into i32 phi) - use 0 as safe default
            # We can't emit trunc/ext in phi, would need to be in source block
            "[0, %#{@block_names[block]}]"
          elsif (is_int_type || is_bool_type) && val_type_str.nil?
            # Unknown type flowing into int/bool phi - check if value_ref looks like ptr
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
          elsif is_float_type && (val_type_str == "ptr" || val_type_str == "void")
            # Ptr/void flowing into float phi - use 0.0 (type mismatch from MIR)
            "[0.0, %#{@block_names[block]}]"
          elsif is_float_type && val_type_str && (val_type_str == "float" || val_type_str == "double") && val_type_str != phi_type
            # float↔double mismatch in phi - use 0.0 as safe default
            # Can't emit fpext/fptrunc in phi, would need to be in source block
            "[0.0, %#{@block_names[block]}]"
          elsif is_float_type && val_type_str && val_type_str.starts_with?("i")
            # Int flowing into float phi - use 0.0 (type mismatch from MIR)
            "[0.0, %#{@block_names[block]}]"
          else
            ref = value_ref(val)
            # Debug: trace unexpected else branch for bool phi
            if is_bool_type && (ref == "null" || val_type_str == "ptr")
              STDERR.puts "[PHI-BOOL-DEBUG] phi=#{inst.id}, val=#{val}, ref=#{ref}, val_type_str=#{val_type_str}"
            end
            # Also check if value_ref returned "null"
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
            "[#{ref}, %#{@block_names[block]}]"
          end
        end
      end
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
                      @type_mapper.mangle_name(callee_func.name)
                    else
                      # Undefined internal function - track for declaration
                      undefined_name = "func#{inst.callee}"
                      ret_type = @type_mapper.llvm_type(inst.type)
                      ret_type = "ptr" if ret_type == "void"  # Use ptr for void to be safe
                      @undefined_externs[undefined_name] = ret_type unless @undefined_externs.has_key?(undefined_name)
                      undefined_name
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
      # Final fallback: use prepass-determined type or minimal heuristics for constructors
      # Main type inference is done in prepass via use-based analysis
      if return_type == "void"
        # Check if prepass already determined this call returns a value (use-based inference)
        prepass_type = @value_types[inst.id]?
        if prepass_type && @type_mapper.llvm_type(prepass_type) != "void"
          return_type = @type_mapper.llvm_type(prepass_type)
        else
          # Minimal fallback: constructors and some methods that return their argument
          ptr_returning_methods = ["new", "allocate", "clone", "dup", "tap"]
          # Methods that return i64
          i64_returning_methods = ["to_i64", "to_u64"]
          # Methods that return i32 (includes chr which returns Char, a 32-bit Unicode codepoint)
          i32_returning_methods = ["size", "length", "count", "hash", "to_i32", "to_i", "ord", "chr"]
          # Type conversion methods
          i16_returning_methods = ["to_i16", "to_u16"] of String
          i8_returning_methods = ["to_i8", "to_u8"] of String
          f32_returning_methods = ["to_f32"] of String
          # Methods that return float64
          f64_returning_methods = ["to_f64", "to_f"]

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
        returns_f32 = f32_returning_methods.any? { |m| matches_name.call(m, callee_name) }
        returns_f64 = f64_returning_methods.any? { |m| matches_name.call(m, callee_name) }
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
      # Use callee function params only if param count matches arg count
      use_callee_params = callee_func && callee_func.params.size == inst.args.size
      # Debug void args
      has_void_arg = inst.args.any? { |a| @type_mapper.llvm_type(@value_types[a]? || TypeRef::POINTER) == "void" }
      # STDERR.puts "[CALL-DEBUG] #{callee_name}, use_callee_params=#{use_callee_params}, has_void_arg=#{has_void_arg}" if has_void_arg
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
                   emit "#{temp_alloca} = alloca #{actual_llvm_type}, align 8"
                   emit "store #{actual_llvm_type} #{normalize_union_value(value_ref(a), actual_llvm_type)}, ptr #{temp_alloca}"
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
                   # Ptr to float conversion: ptrtoint, then bitcast to float
                   val = value_ref(a)
                   c = @cond_counter
                   @cond_counter += 1
                   if expected_llvm_type == "double"
                     emit "%ptrtofp.#{c}.int = ptrtoint ptr #{val} to i64"
                     emit "%ptrtofp.#{c} = bitcast i64 %ptrtofp.#{c}.int to double"
                   else
                     emit "%ptrtofp.#{c}.int = ptrtoint ptr #{val} to i32"
                     emit "%ptrtofp.#{c} = bitcast i32 %ptrtofp.#{c}.int to float"
                   end
                   "#{expected_llvm_type} %ptrtofp.#{c}"
                 elsif (expected_llvm_type == "double" || expected_llvm_type == "float") && actual_llvm_type.starts_with?("i")
                   # Int to float conversion: sitofp (signed int to floating point)
                   val = value_ref(a)
                   c = @cond_counter
                   @cond_counter += 1
                   emit "%itofp.#{c} = sitofp #{actual_llvm_type} #{val} to #{expected_llvm_type}"
                   "#{expected_llvm_type} %itofp.#{c}"
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
                     # Use null instead of 0 for pointer types
                     ptr_val = val == "0" ? "null" : val
                     emit "store ptr #{ptr_val}, ptr %ptr_to_union.#{c}.payload_ptr"
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
                   emit "%union_conv.#{c}.payload_as_ptr = load ptr, ptr %union_conv.#{c}.src_payload_ptr"
                   # Store into expected union
                   emit "%union_conv.#{c}.dst_ptr = alloca #{expected_llvm_type}, align 8"
                   emit "%union_conv.#{c}.dst_type_id_ptr = getelementptr #{expected_llvm_type}, ptr %union_conv.#{c}.dst_ptr, i32 0, i32 0"
                   emit "store i32 %union_conv.#{c}.type_id, ptr %union_conv.#{c}.dst_type_id_ptr"
                   emit "%union_conv.#{c}.dst_payload_ptr = getelementptr #{expected_llvm_type}, ptr %union_conv.#{c}.dst_ptr, i32 0, i32 1"
                   emit "store ptr %union_conv.#{c}.payload_as_ptr, ptr %union_conv.#{c}.dst_payload_ptr"
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
               inst.args.map { |a|
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

      if return_type == "void"
        emit "call void @#{callee_name}(#{args})"
        # Mark as void so value_ref returns a safe default for downstream uses
        @void_values << inst.id
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

    private def emit_indirect_call(inst : IndirectCall, name : String)
      return_type = @type_mapper.llvm_type(inst.type)
      callee = value_ref(inst.callee_ptr)
      args = inst.args.map { |a| "ptr #{value_ref(a)}" }.join(", ")
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
      return_type = @type_mapper.llvm_type(inst.type)

      # Format arguments using actual types from value_types registry
      args = inst.args.map do |a|
        arg_type_ref = @value_types[a]? || TypeRef::POINTER
        arg_type = @type_mapper.llvm_type(arg_type_ref)
        # Guard against void argument type - use ptr null instead
        if arg_type == "void"
          "ptr null"
        else
          val = value_ref(a)
          # For union types, use zeroinitializer instead of 0
          if arg_type.includes?(".union") && (val == "0" || val == "null")
            "#{arg_type} zeroinitializer"
          elsif arg_type == "ptr" && val == "0"
            # ptr 0 is invalid - use ptr null
            "ptr null"
          else
            "#{arg_type} #{val}"
          end
        end
      end.join(", ")

      # Mangle the extern name to be a valid LLVM identifier
      mangled_extern_name = @type_mapper.mangle_name(inst.extern_name)

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
        old_name = mangled_extern_name
        mangled_extern_name = @type_mapper.mangle_name(matching_func.name)
        if mangled_extern_name.includes?("ArenaLike")
          STDERR.puts "[EXTERN-RESOLVED] #{old_name} -> #{mangled_extern_name}"
        end
      else
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
          # Minimal fallback: constructors and some methods that return their argument
          ptr_returning_methods = ["new", "allocate", "clone", "dup", "tap"]
          # Methods that return i64
          i64_returning_methods = ["to_i64", "to_u64"]
          # Methods that return i32 (includes chr which returns Char, a 32-bit Unicode codepoint)
          i32_returning_methods = ["size", "length", "count", "hash", "to_i32", "to_i", "ord", "chr"]
          # Type conversion methods
          i16_returning_methods = ["to_i16", "to_u16"] of String
          i8_returning_methods = ["to_i8", "to_u8"] of String

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
        end  # end else for prepass_type check
      end

      if return_type == "void"
        emit "call void @#{mangled_extern_name}(#{args})"
        # Mark as void so value_ref returns a safe default for downstream uses
        @void_values << inst.id
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
        if val_type_str.nil? || !val_type_str.includes?(".union")
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
          emit "%#{base_name}.payload = load #{result_type}, ptr %#{base_name}.payload_ptr"
          # Select null/zero if type doesn't match
          emit "#{name} = select i1 %#{base_name}.type_match, #{result_type} %#{base_name}.payload, #{result_type} zeroinitializer"
        else
          # Unsafe unwrap: just load payload (UB if type_id doesn't match)
          emit "%#{base_name}.payload_ptr = getelementptr #{union_type}, ptr %#{base_name}.union_ptr, i32 0, i32 1"
          emit "#{name} = load #{result_type}, ptr %#{base_name}.payload_ptr"
        end
      else
        # Not a union struct - just use the value directly
        # If result_type is ptr, use the union_val as-is
        if result_type == "ptr" || result_type == union_type
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
        emit "%#{base_name}.is_null = icmp eq ptr #{union_val}, null"
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
        if inst.variant_type_id == 0
          # Checking for non-nil variant: ptr != null
          emit "#{name} = icmp ne ptr #{union_val}, null"
        else
          # Checking for nil variant: ptr == null
          emit "#{name} = icmp eq ptr #{union_val}, null"
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
              emit "store ptr #{elem_val}, ptr %#{base_name}.elem#{idx}_payload_ptr"
              emit "%#{base_name}.elem#{idx}_union = load #{element_type}, ptr %#{base_name}.elem#{idx}_union_ptr"
              elem_val = "%#{base_name}.elem#{idx}_union"
            elsif is_elem_int && is_actual_ptr
              # ptr → int: use ptrtoint
              emit "%#{base_name}.elem#{idx}_conv = ptrtoint ptr #{elem_val} to #{element_type}"
              elem_val = "%#{base_name}.elem#{idx}_conv"
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
      if array_value_type
        array_llvm_type = @type_mapper.llvm_type(array_value_type)
        if array_llvm_type.includes?(".union")
          # Extract pointer from union payload
          emit "%#{base_name}.union_ptr = alloca #{array_llvm_type}, align 8"
          emit "store #{array_llvm_type} #{normalize_union_value(array_ptr, array_llvm_type)}, ptr %#{base_name}.union_ptr"
          emit "%#{base_name}.payload_ptr = getelementptr #{array_llvm_type}, ptr %#{base_name}.union_ptr, i32 0, i32 1"
          emit "%#{base_name}.arr_ptr = load ptr, ptr %#{base_name}.payload_ptr"
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
          emit "store #{array_llvm_type} #{normalize_union_value(array_ptr, array_llvm_type)}, ptr %#{base_name}.union_ptr"
          emit "%#{base_name}.payload_ptr = getelementptr #{array_llvm_type}, ptr %#{base_name}.union_ptr, i32 0, i32 1"
          emit "%#{base_name}.arr_ptr = load ptr, ptr %#{base_name}.payload_ptr"
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
        if index_llvm == "ptr"
          emit "%#{base_name}.idx_int = ptrtoint ptr #{index} to i32"
          index = "%#{base_name}.idx_int"
        elsif index_llvm == "i64"
          emit "%#{base_name}.idx_int = trunc i64 #{index} to i32"
          index = "%#{base_name}.idx_int"
        end
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
          emit "store #{array_llvm_type} #{normalize_union_value(array_ptr, array_llvm_type)}, ptr %#{base_name}.union_ptr"
          emit "%#{base_name}.payload_ptr = getelementptr #{array_llvm_type}, ptr %#{base_name}.union_ptr, i32 0, i32 1"
          emit "%#{base_name}.arr_ptr = load ptr, ptr %#{base_name}.payload_ptr"
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
        if index_llvm == "ptr"
          emit "%#{base_name}.idx_int = ptrtoint ptr #{index} to i32"
          index = "%#{base_name}.idx_int"
        elsif index_llvm == "i64"
          emit "%#{base_name}.idx_int = trunc i64 #{index} to i32"
          index = "%#{base_name}.idx_int"
        end
      end

      # Check if value type matches element type and convert if needed
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
      elsif actual_value_type == "i1" && element_type.starts_with?("i") && element_type != "i1"
        # Convert bool (i1) to larger integer type (zext)
        emit "%#{base_name}.val_ext = zext i1 #{value} to #{element_type}"
        value = "%#{base_name}.val_ext"
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
            emit "%#{base_name}.str_ptr#{idx} = load ptr, ptr %#{base_name}.payload_ptr#{idx}"
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
            elsif @current_return_type == "ptr" && val_llvm_type && val_llvm_type.includes?(".union")
              # Union to pointer - extract payload as ptr
              c = @cond_counter
              @cond_counter += 1
              emit "%ret_union_to_ptr.#{c}.ptr = alloca #{val_llvm_type}, align 8"
              emit "store #{val_llvm_type} #{val_ref}, ptr %ret_union_to_ptr.#{c}.ptr"
              emit "%ret_union_to_ptr.#{c}.payload_ptr = getelementptr #{val_llvm_type}, ptr %ret_union_to_ptr.#{c}.ptr, i32 0, i32 1"
              emit "%ret_union_to_ptr.#{c}.val = load ptr, ptr %ret_union_to_ptr.#{c}.payload_ptr"
              emit "ret ptr %ret_union_to_ptr.#{c}.val"
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
              emit "%ret_union_conv.#{c}.payload_as_ptr = load ptr, ptr %ret_union_conv.#{c}.src_payload_ptr"
              emit "store ptr %ret_union_conv.#{c}.payload_as_ptr, ptr %ret_union_conv.#{c}.dst_payload_ptr"
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
              # Integer to float conversion
              c = @cond_counter
              @cond_counter += 1
              emit "%ret_itof.#{c} = sitofp #{val_llvm_type} #{val_ref} to #{@current_return_type}"
              emit "ret #{@current_return_type} %ret_itof.#{c}"
            elsif val_llvm_type && (val_llvm_type == "double" || val_llvm_type == "float") && @current_return_type.starts_with?("i")
              # Float to integer conversion
              c = @cond_counter
              @cond_counter += 1
              emit "%ret_ftoi.#{c} = fptosi #{val_llvm_type} #{val_ref} to #{@current_return_type}"
              emit "ret #{@current_return_type} %ret_ftoi.#{c}"
            elsif (@current_return_type == "double" || @current_return_type == "float") && val_llvm_type == "ptr"
              # Pointer to float conversion - try to load from pointer or return 0.0
              # This often happens with incomplete function bodies
              c = @cond_counter
              @cond_counter += 1
              emit "%ret_ptr_to_float.#{c} = load #{@current_return_type}, ptr #{val_ref}"
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

        # Check if condition is a union type - need to extract type_id and compare
        # Also verify LLVM type is actually a union struct (not just ptr)
        union_llvm_type = cond_type ? @type_mapper.llvm_type(cond_type) : nil
        if cond_type && is_union_type?(cond_type) && union_llvm_type && union_llvm_type.includes?(".union")
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
        elsif cond_type && @type_mapper.llvm_type(cond_type) == "ptr"
          # Pointer type: compare against null
          c = @cond_counter
          @cond_counter += 1
          emit "%cond#{c}.not_null = icmp ne ptr #{cond}, null"
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
          # Value not emitted - return safe default for this phi incoming
          val_type = @value_types[id]?
          llvm_type = val_type ? @type_mapper.llvm_type(val_type) : "i64"
          case llvm_type
          when "ptr" then return "null"
          when "float", "double" then return "0.0"
          when .includes?(".union") then return "zeroinitializer"
          else return "0"
          end
        end
        # Non-phi use: load from slot to handle dominance issues
        val_type = @value_types[id]?
        llvm_type = val_type ? @type_mapper.llvm_type(val_type) : "i64"
        llvm_type = "i64" if llvm_type == "void"
        temp_name = "%r#{id}.fromslot.#{@cond_counter}"
        @cond_counter += 1
        emit "#{temp_name} = load #{llvm_type}, ptr %#{slot_name}"
        return temp_name
      end
      # Otherwise reference by name
      if name = @value_names[id]?
        "%#{name}"
      else
        # Value was never emitted - this is likely an unreachable instruction or
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
