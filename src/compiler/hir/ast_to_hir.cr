# AST to HIR Lowering
#
# Converts Crystal AST (from parser) to High-Level IR for analysis.
# This is the first stage of the codegen pipeline.
#
# See docs/codegen_architecture.md for full specification.

require "./hir"
require "../frontend/ast"
require "../mir/mir"

module Crystal::HIR
  # Error raised during AST to HIR conversion
  class LoweringError < Exception
    getter node : CrystalV2::Compiler::Frontend::Node?
    getter details : String

    def initialize(@details : String, @node : CrystalV2::Compiler::Frontend::Node? = nil)
      super(@details)
    end
  end

  # Context for lowering a single function
  class LoweringContext
    getter function : Function
    getter module : Module
    getter arena : CrystalV2::Compiler::Frontend::ArenaLike

    # Current block being built
    property current_block : BlockId

    # Variable name → ValueId mapping per scope
    @locals : Hash(String, ValueId)

    # Scope stack for nested scopes
    @scope_stack : Array(ScopeId)

    # Type cache
    @type_cache : Hash(String, TypeRef)

    # Value → Type mapping for type inference
    @value_types : Hash(ValueId, TypeRef)

    def initialize(@function : Function, @module : Module, @arena)
      @current_block = @function.entry_block
      @locals = {} of String => ValueId
      @scope_stack = [@function.scopes[0].id]  # Function scope
      @type_cache = {} of String => TypeRef
      @value_types = {} of ValueId => TypeRef
    end

    # Get current scope
    def current_scope : ScopeId
      @scope_stack.last
    end

    # Push new scope
    def push_scope(kind : ScopeKind) : ScopeId
      scope_id = @function.create_scope(kind, current_scope)
      @scope_stack << scope_id
      scope_id
    end

    # Pop scope
    def pop_scope : ScopeId
      @scope_stack.pop
    end

    # Create new block in current scope
    def create_block : BlockId
      @function.create_block(current_scope)
    end

    # Get block by ID
    def get_block(id : BlockId) : Block
      @function.get_block(id)
    end

    # Switch to a different block
    def switch_to_block(id : BlockId)
      @current_block = id
    end

    # Get current block ID
    def current_block_id : BlockId
      @current_block
    end

    # Add instruction to current block
    def emit(value : Value) : Value
      get_block(@current_block).add(value)
      @value_types[value.id] = value.type  # Track type for inference
      value
    end

    # Add instruction to a specific block (used for inserting before phi nodes)
    def emit_to_block(block_id : BlockId, value : Value) : Value
      get_block(block_id).add(value)
      @value_types[value.id] = value.type
      value
    end

    # Look up the type of a value by ID
    def type_of(id : ValueId) : TypeRef
      @value_types[id]? || TypeRef::VOID
    end

    # Register type for a value (used for params not emitted via emit)
    def register_type(id : ValueId, type : TypeRef)
      @value_types[id] = type
    end

    # Set terminator for current block
    def terminate(term : Terminator)
      get_block(@current_block).terminator = term
    end

    # Next value ID
    def next_id : ValueId
      @function.next_value_id
    end

    # Register local variable
    def register_local(name : String, value_id : ValueId)
      @locals[name] = value_id
      @function.get_scope(current_scope).add_local(value_id)
    end

    # Lookup local variable
    def lookup_local(name : String) : ValueId?
      @locals[name]?
    end

    # Save current locals state (for branching)
    def save_locals : Hash(String, ValueId)
      @locals.dup
    end

    # Restore locals state (for else branch)
    def restore_locals(saved : Hash(String, ValueId))
      @locals = saved.dup
    end

    # Get all current locals
    def all_locals : Hash(String, ValueId)
      @locals
    end

    # Get or create type ref
    def get_type(name : String) : TypeRef
      @type_cache[name]? || begin
        type_ref = case name
                   when "Void"           then TypeRef::VOID
                   when "Nil"            then TypeRef::NIL
                   when "Bool"           then TypeRef::BOOL
                   when "Int8"           then TypeRef::INT8
                   when "Int16"          then TypeRef::INT16
                   when "Int32"          then TypeRef::INT32
                   when "Int64"          then TypeRef::INT64
                   when "Int128"         then TypeRef::INT128
                   when "UInt8"          then TypeRef::UINT8
                   when "UInt16"         then TypeRef::UINT16
                   when "UInt32"         then TypeRef::UINT32
                   when "UInt64"         then TypeRef::UINT64
                   when "UInt128"        then TypeRef::UINT128
                   when "Float32"        then TypeRef::FLOAT32
                   when "Float64"        then TypeRef::FLOAT64
                   when "Char"           then TypeRef::CHAR
                   when "String"         then TypeRef::STRING
                   when "Symbol"         then TypeRef::SYMBOL
                   else
                     # User-defined type
                     @module.intern_type(TypeDescriptor.new(TypeKind::Class, name))
                   end
        @type_cache[name] = type_ref
        type_ref
      end
    end
  end

  # Instance variable info for class layout
  record IVarInfo, name : String, type : TypeRef, offset : Int32

  # Class variable info
  record ClassVarInfo, name : String, type : TypeRef, initial_value : Int64?

  # Class type info (is_struct=true for value types)
  record ClassInfo, name : String, type_ref : TypeRef, ivars : Array(IVarInfo), class_vars : Array(ClassVarInfo), size : Int32, is_struct : Bool = false, parent_name : String? = nil

  # Class-level accessor entry (class_getter/class_setter/class_property)
  record ClassAccessorEntry,
    owner : String,
    spec : CrystalV2::Compiler::Frontend::AccessorSpec,
    arena : CrystalV2::Compiler::Frontend::ArenaLike,
    kind : Symbol

  # Generic class template (not yet specialized)
  record GenericClassTemplate,
    name : String,                                    # Base name like "Box"
    type_params : Array(String),                      # ["T"] or ["K", "V"]
    node : CrystalV2::Compiler::Frontend::ClassNode,  # Original AST node for re-lowering
    arena : CrystalV2::Compiler::Frontend::ArenaLike, # Arena for looking up body ExprIds
    is_struct : Bool = false

  # Main AST to HIR converter
  class AstToHir
    alias AstNode = CrystalV2::Compiler::Frontend::Node
    alias ExprId = CrystalV2::Compiler::Frontend::ExprId

    # Platform-specific LibC type aliases (fallback for unevaluated macro conditionals)
    # On 64-bit systems (aarch64-darwin, x86_64-*):
    # Includes both LibC:: prefixed and bare names for direct use
    LIBC_TYPE_ALIASES = {
      # With LibC:: prefix
      "LibC::Char"      => "UInt8",
      "LibC::UChar"     => "UInt8",
      "LibC::SChar"     => "Int8",
      "LibC::Short"     => "Int16",
      "LibC::UShort"    => "UInt16",
      "LibC::Int"       => "Int32",
      "LibC::UInt"      => "UInt32",
      "LibC::Long"      => "Int64",      # 64-bit
      "LibC::ULong"     => "UInt64",     # 64-bit
      "LibC::LongLong"  => "Int64",
      "LibC::ULongLong" => "UInt64",
      "LibC::Float"     => "Float32",
      "LibC::Double"    => "Float64",
      "LibC::SizeT"     => "UInt64",     # 64-bit: ULong -> UInt64
      "LibC::SSizeT"    => "Int64",      # 64-bit: Long -> Int64
      "LibC::PtrDiffT"  => "Int64",      # 64-bit: Long -> Int64
      "LibC::OffT"      => "Int64",      # 64-bit
      "LibC::ModeT"     => "UInt16",     # darwin
      "LibC::PidT"      => "Int32",
      "LibC::UidT"      => "UInt32",
      "LibC::GidT"      => "UInt32",
      "LibC::TimeT"     => "Int64",      # 64-bit
      "LibC::ClockT"    => "UInt64",     # darwin
      # Without LibC:: prefix (for macro conditionals that define these)
      "Char"     => "UInt8",
      "UChar"    => "UInt8",
      "SChar"    => "Int8",
      "Short"    => "Int16",
      "UShort"   => "UInt16",
      "Int"      => "Int32",
      "UInt"     => "UInt32",
      "Long"     => "Int64",             # 64-bit
      "ULong"    => "UInt64",            # 64-bit
      "SizeT"    => "UInt64",            # 64-bit
      "SSizeT"   => "Int64",             # 64-bit
    }

    # Top-level user-defined `def main` is renamed to avoid clashing with the C entrypoint.
    TOP_LEVEL_MAIN_BASE = "__crystal_user_main"
    # Marker for top-level `fun` definitions (C ABI). Stored in DefNode.receiver.
    FUN_DEF_RECEIVER = "__fun__"

    getter module : Module
    property arena : CrystalV2::Compiler::Frontend::ArenaLike

    # Pre-registered function signatures for forward reference support
    @function_types : Hash(String, TypeRef)

    # Index of function base names (without $ type suffix) for fast prefix lookups
    # Maps base name -> true (existence check)
    @function_base_names : Set(String)

    # Cached return type for a function base name (without $ suffix).
    # This is used for method resolution when only the base name is known.
    @function_base_return_types : Hash(String, TypeRef)

    # Class type information
    getter class_info : Hash(String, ClassInfo)

    # Initialize parameters for each class (for new() generation)
    @init_params : Hash(String, Array({String, TypeRef}))

    # Current class being lowered (for ivar access)
    @current_class : String?

    # Union type descriptors for debug info (keyed by MIR::TypeRef)
    getter union_descriptors : Hash(MIR::TypeRef, MIR::UnionDescriptor)

    # AST of function definitions for inline expansion
    @function_defs : Hash(String, CrystalV2::Compiler::Frontend::DefNode)
    @function_def_arenas : Hash(String, CrystalV2::Compiler::Frontend::ArenaLike)

    # Functions that contain yield (candidates for inline)
    @yield_functions : Set(String)

    # Track functions lowered lazily to avoid re-entrancy/duplication.
    @lowered_functions : Set(String)
    @lowering_functions : Set(String)
    # Call-site argument types for lazily lowered functions (mangled name -> arg types).
    @pending_arg_types : Hash(String, Array(TypeRef))

    # Generic class templates (base name -> template)
    @generic_templates : Hash(String, GenericClassTemplate)

    # Already monomorphized generic classes (specialized name -> true)
    @monomorphized : Set(String)

    # Debug counters
    @template_reg_counter : Int32?
    @mono_counter : Int32?

    # Pending monomorphizations (deferred until after all templates are registered)
    @pending_monomorphizations : Array({String, Array(String), String})
    @defer_monomorphization : Bool

    # Current type parameter substitutions for generic lowering
    @type_param_map : Hash(String, String)

    # Current method name being lowered (for super calls)
    @current_method : String?
    # Track whether the current method is a class/module method (self.)
    @current_method_is_class : Bool

    # Locals available for resolving typeof(...) in type positions (per def)
    @current_typeof_locals : Hash(String, TypeRef)?

    # Macro definitions (name -> MacroDefNode)
    @macro_defs : Hash(String, CrystalV2::Compiler::Frontend::MacroDefNode)

    # Class-level accessors (full method name -> entry)
    @class_accessor_entries : Hash(String, ClassAccessorEntry)

    # Module AST definitions (name -> list of {node, arena}) for mixin expansion.
    # Many stdlib "modules" provide instance methods meant to be included into classes/structs.
    @module_defs : Hash(String, Array({CrystalV2::Compiler::Frontend::ModuleNode, CrystalV2::Compiler::Frontend::ArenaLike}))
    # Track concrete types that include a module for module-typed receiver fallback.
    @module_includers : Hash(String, Set(String))
    # Reverse mapping: track which modules each class includes (for method lookup)
    @class_included_modules : Hash(String, Set(String))

    # Type aliases (alias_name -> target_type_name)
    @type_aliases : Hash(String, String)

    # Track which allocators have been generated (to avoid duplicates for reopened classes)
    @generated_allocators : Set(String)

    # Type cache to prevent infinite recursion in type_ref_for_name/create_union_type
    @type_cache : Hash(String, TypeRef)

    # Temporary arena switching context for cross-file yield inlining:
    # {caller_arena, callee_arena}
    @inline_arenas : {CrystalV2::Compiler::Frontend::ArenaLike, CrystalV2::Compiler::Frontend::ArenaLike}? = nil

    # While inlining yield-functions, we must preserve caller locals for the block body.
    # Otherwise callee locals (especially `self`) can leak into the caller and break ivar access.
    @inline_caller_locals_stack : Array(Hash(String, ValueId)) = [] of Hash(String, ValueId)
    # Preserve caller class/method so block bodies resolve unqualified calls in caller scope.
    @inline_caller_class_stack : Array(String?) = [] of String?
    @inline_caller_method_stack : Array(String?) = [] of String?
    @inline_caller_method_is_class_stack : Array(Bool) = [] of Bool
    # Loop-carried locals for inline yield contexts (used to keep phi-bound values stable).
    @inline_loop_vars_stack : Array(Set(String)) = [] of Set(String)

    # While inlining yield-functions, substitute `yield` with the call-site block body.
    # Use a stack to support nested inlining (a block body may itself contain `yield`).
    @inline_yield_block_stack : Array(CrystalV2::Compiler::Frontend::BlockNode) = [] of CrystalV2::Compiler::Frontend::BlockNode
    @inline_yield_block_arena_stack : Array(CrystalV2::Compiler::Frontend::ArenaLike) = [] of CrystalV2::Compiler::Frontend::ArenaLike

    # Track currently inlined yield-functions to avoid infinite inline recursion on stdlib code.
    @inline_yield_name_stack : Array(String) = [] of String

    # Captures computed for block literals (body_block -> captures).
    @block_captures : Hash(BlockId, Array(CapturedVar)) = {} of BlockId => Array(CapturedVar)

    # Track declared type names for locals (used to resolve module-typed receivers).
    @current_typeof_local_names : Hash(String, String)?

    # Track top-level `def main` so we can remap calls and avoid entrypoint collisions.
    @top_level_main_defined : Bool

    def initialize(@arena, module_name : String = "main")
      @module = Module.new(module_name)
      @function_types = {} of String => TypeRef
      @function_base_names = Set(String).new
      @function_base_return_types = {} of String => TypeRef
      @class_info = {} of String => ClassInfo
      @init_params = {} of String => Array({String, TypeRef})
      @current_class = nil
      @current_method = nil
      @current_method_is_class = false
      @current_typeof_locals = nil
      @union_descriptors = {} of MIR::TypeRef => MIR::UnionDescriptor
      @function_defs = {} of String => CrystalV2::Compiler::Frontend::DefNode
      @function_def_arenas = {} of String => CrystalV2::Compiler::Frontend::ArenaLike
      @yield_functions = Set(String).new
      @lowered_functions = Set(String).new
      @lowering_functions = Set(String).new
      @pending_arg_types = {} of String => Array(TypeRef)
      @generic_templates = {} of String => GenericClassTemplate
      @monomorphized = Set(String).new
      @pending_monomorphizations = [] of {String, Array(String), String}
      @defer_monomorphization = true  # Start in deferred mode
      @type_param_map = {} of String => String
      @macro_defs = {} of String => CrystalV2::Compiler::Frontend::MacroDefNode
      @class_accessor_entries = {} of String => ClassAccessorEntry
      @module_defs = {} of String => Array({CrystalV2::Compiler::Frontend::ModuleNode, CrystalV2::Compiler::Frontend::ArenaLike})
      @module_includers = {} of String => Set(String)
      @class_included_modules = {} of String => Set(String)
      @type_aliases = {} of String => String
      @generated_allocators = Set(String).new
      @type_cache = {} of String => TypeRef
      @current_typeof_local_names = nil
      @top_level_main_defined = false
      @block_captures = {} of BlockId => Array(CapturedVar)
    end

    private def fun_def?(node : CrystalV2::Compiler::Frontend::DefNode) : Bool
      if recv = node.receiver
        String.new(recv) == FUN_DEF_RECEIVER
      else
        false
      end
    end

    private def record_module_inclusion(module_name : String, class_name : String) : Nil
      set = @module_includers[module_name]? || begin
        new_set = Set(String).new
        @module_includers[module_name] = new_set
        new_set
      end
      set.add(class_name)
      # Also record reverse mapping (class -> modules it includes)
      class_set = @class_included_modules[class_name]? || begin
        new_set = Set(String).new
        @class_included_modules[class_name] = new_set
        new_set
      end
      class_set.add(module_name)
    end

    private def named_only_separator?(param : CrystalV2::Compiler::Frontend::Parameter) : Bool
      param.is_splat && param.name.nil? && param.external_name.nil?
    end

    # Register a function type and maintain the base name index
    private def register_function_type(full_name : String, return_type : TypeRef)
      @function_types[full_name] = return_type
      # Extract base name (without $ type suffix) for fast lookups
      base_name = full_name.split("$").first
      @function_base_names.add(base_name)
      # Cache a representative return type for the base name.
      # Prefer a non-VOID return type when available.
      if return_type != TypeRef::VOID
        cached = @function_base_return_types[base_name]?
        if cached.nil? || cached == TypeRef::VOID
          @function_base_return_types[base_name] = return_type
        end
      end
    end

    # Check if a function exists with given base name (fast O(1) lookup)
    private def has_function_base?(base_name : String) : Bool
      @function_base_names.includes?(base_name)
    end

    # Get class info by name
    def get_class_info(name : String) : ClassInfo?
      @class_info[name]?
    end

    # Enum info: name -> {member_name -> value}
    @enum_info : Hash(String, Hash(String, Int64))?

    # Register an enum type (pass 1)
    def register_enum(node : CrystalV2::Compiler::Frontend::EnumNode)
      enum_name = String.new(node.name)
      @enum_info ||= {} of String => Hash(String, Int64)

      members = {} of String => Int64
      current_value = 0_i64

      node.members.each do |member|
        member_name = String.new(member.name)
        # If member has explicit value, use it
        if val_id = member.value
          val_node = @arena[val_id]
          if val_node.is_a?(CrystalV2::Compiler::Frontend::NumberNode)
            current_value = String.new(val_node.value).to_i64? || current_value
          end
        end
        members[member_name] = current_value
        current_value += 1
      end

      @enum_info.not_nil![enum_name] = members
    end

    # Register an enum type with explicit name (for nested enums)
    def register_enum_with_name(node : CrystalV2::Compiler::Frontend::EnumNode, full_enum_name : String)
      @enum_info ||= {} of String => Hash(String, Int64)

      members = {} of String => Int64
      current_value = 0_i64

      node.members.each do |member|
        member_name = String.new(member.name)
        # If member has explicit value, use it
        if val_id = member.value
          val_node = @arena[val_id]
          if val_node.is_a?(CrystalV2::Compiler::Frontend::NumberNode)
            current_value = String.new(val_node.value).to_i64? || current_value
          end
        end
        members[member_name] = current_value
        current_value += 1
      end

      @enum_info.not_nil![full_enum_name] = members
      # Also register by short name for lookups
      short_name = String.new(node.name)
      @enum_info.not_nil![short_name] = members
    end

    # Register a macro definition (pass 1)
    def register_macro(node : CrystalV2::Compiler::Frontend::MacroDefNode)
      macro_name = String.new(node.name)
      @macro_defs[macro_name] = node
    end

    # Register a type alias (pass 1)
    # e.g., alias HIR = Crystal::HIR
    def register_alias(node : CrystalV2::Compiler::Frontend::AliasNode)
      alias_name = String.new(node.name)
      target_name = resolve_alias_target(String.new(node.value))
      register_type_alias(alias_name, target_name)
    end

    private def register_type_alias(alias_name : String, target_name : String)
      @type_aliases[alias_name] = target_name
      @type_cache.delete(alias_name)
    end

    # Register a C library binding (pass 1)
    # e.g., lib LibC ... end
    def register_lib(node : CrystalV2::Compiler::Frontend::LibNode)
      lib_name = String.new(node.name)

      if body = node.body
        body.each do |expr_id|
          body_node = @arena[expr_id]
          case body_node
          when CrystalV2::Compiler::Frontend::FunNode
            # Register external function
            register_extern_fun(lib_name, body_node)
          when CrystalV2::Compiler::Frontend::AliasNode
            # type aliases within lib
            alias_name = String.new(body_node.name)
            old_class = @current_class
            @current_class = lib_name
            target_name = resolve_alias_target(String.new(body_node.value))
            @current_class = old_class
            full_alias_name = "#{lib_name}::#{alias_name}"
            register_type_alias(full_alias_name, target_name)
            register_type_alias(alias_name, target_name)
          when CrystalV2::Compiler::Frontend::EnumNode
            # Enums within lib - register with lib prefix
            enum_name = String.new(body_node.name)
            full_enum_name = "#{lib_name}::#{enum_name}"
            register_enum_with_name(body_node, full_enum_name)
          when CrystalV2::Compiler::Frontend::StructNode
            # Structs within lib - skip for now
          # Annotations, nested libs, etc. - ignored for now
          end
        end
      end
    end

    # Expand a macro call inline
    # For simple macros, parse the body text as Crystal code and lower it
    private def expand_macro(ctx : LoweringContext, macro_def : CrystalV2::Compiler::Frontend::MacroDefNode, args : Array(ExprId)) : ValueId
      body_node = @arena[macro_def.body]

      case body_node
      when CrystalV2::Compiler::Frontend::MacroLiteralNode
        # First pass: collect macro parameter names from Expression pieces
        # In order of first appearance (first unique identifier = first param)
        param_names = [] of String
        body_node.pieces.each do |piece|
          if piece.kind == CrystalV2::Compiler::Frontend::MacroPiece::Kind::Expression
            if expr_id = piece.expr
              param_name = extract_macro_param_name(@arena[expr_id])
              if param_name && !param_names.includes?(param_name)
                param_names << param_name
              end
            end
          end
        end

        # Build parameter -> argument value mapping
        param_values = {} of String => String
        param_names.each_with_index do |name, idx|
          if idx < args.size
            # Stringify the argument node
            arg_node = @arena[args[idx]]
            param_values[name] = stringify_ast_node(arg_node)
          end
        end

        # Second pass: expand pieces with parameter substitution
        expanded_text = String.build do |io|
          body_node.pieces.each do |piece|
            case piece.kind
            when CrystalV2::Compiler::Frontend::MacroPiece::Kind::Text
              if text = piece.text
                io << text
              end
            when CrystalV2::Compiler::Frontend::MacroPiece::Kind::Expression
              # {{ expr }} - substitute parameter or evaluate expression
              if expr_id = piece.expr
                expr_node = @arena[expr_id]
                param_name = extract_macro_param_name(expr_node)
                if param_name && (value = param_values[param_name]?)
                  io << value
                else
                  # Not a parameter - try to stringify the expression
                  io << stringify_ast_node(expr_node)
                end
              end
            else
              # Control structures - handle in future
            end
          end
        end

        # Parse the expanded text as Crystal code
        expanded_text = expanded_text.strip
        if expanded_text.empty?
          # Empty macro - return void
          noop = Literal.new(ctx.next_id, TypeRef::VOID, nil)
          ctx.emit(noop)
          return noop.id
        end

        lexer = CrystalV2::Compiler::Frontend::Lexer.new(expanded_text)
        parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
        program = parser.parse_program

        if program.roots.empty?
          noop = Literal.new(ctx.next_id, TypeRef::VOID, nil)
          ctx.emit(noop)
          return noop.id
        end

        # Lower the parsed expression using the macro's arena
        old_arena = @arena
        @arena = program.arena
        begin
          # Lower all expressions, return last one
          last_id : ValueId? = nil
          program.roots.each do |expr_id|
            last_id = lower_expr(ctx, expr_id)
          end
          last_id || begin
            noop = Literal.new(ctx.next_id, TypeRef::VOID, nil)
            ctx.emit(noop)
            noop.id
          end
        ensure
          @arena = old_arena
        end
      else
        # Unsupported macro body type
        noop = Literal.new(ctx.next_id, TypeRef::VOID, nil)
        ctx.emit(noop)
        noop.id
      end
    end

    # Extract parameter name from macro expression node
    private def extract_macro_param_name(node) : String?
      case node
      when CrystalV2::Compiler::Frontend::MacroExpressionNode
        inner = @arena[node.expression]
        extract_macro_param_name(inner)
      when CrystalV2::Compiler::Frontend::IdentifierNode
        String.new(node.name)
      else
        nil
      end
    end

    # Stringify an AST node for macro expansion
    private def stringify_ast_node(node) : String
      case node
      when CrystalV2::Compiler::Frontend::NumberNode
        String.new(node.value)
      when CrystalV2::Compiler::Frontend::StringNode
        # Return with quotes for string literals
        "\"#{String.new(node.value)}\""
      when CrystalV2::Compiler::Frontend::IdentifierNode
        String.new(node.name)
      when CrystalV2::Compiler::Frontend::MacroExpressionNode
        stringify_ast_node(@arena[node.expression])
      when CrystalV2::Compiler::Frontend::BinaryNode
        left = stringify_ast_node(@arena[node.left])
        right = stringify_ast_node(@arena[node.right])
        "#{left} #{String.new(node.operator)} #{right}"
      when CrystalV2::Compiler::Frontend::CallNode
        # Simple call stringify
        callee = @arena[node.callee]
        name = case callee
               when CrystalV2::Compiler::Frontend::IdentifierNode
                 String.new(callee.name)
               else
                 "?"
               end
        args_str = node.args.map { |arg| stringify_ast_node(@arena[arg]) }.join(", ")
        "#{name}(#{args_str})"
      else
        ""
      end
    end

    private def resolve_path_like_name(expr_id : ExprId) : String?
      return nil if expr_id.invalid?

      node = @arena[expr_id]
      case node
      when CrystalV2::Compiler::Frontend::IdentifierNode
        String.new(node.name)
      when CrystalV2::Compiler::Frontend::ConstantNode
        String.new(node.name)
      when CrystalV2::Compiler::Frontend::PathNode
        left = node.left
        left_name = left ? resolve_path_like_name(left.not_nil!) : nil
        right_name = resolve_path_like_name(node.right)
        return nil unless right_name
        left_name ? "#{left_name}::#{right_name}" : right_name
      when CrystalV2::Compiler::Frontend::GenericNode
        resolve_path_like_name(node.base_type)
      else
        nil
      end
    end

    private def stringify_type_expr(expr_id : ExprId) : String?
      return nil if expr_id.invalid?

      node = @arena[expr_id]
      case node
      when CrystalV2::Compiler::Frontend::TypeofNode
        inner = node.args.first?
        inner ? resolve_typeof_expr(inner) : "Pointer(Void)"
      when CrystalV2::Compiler::Frontend::IdentifierNode
        name = String.new(node.name)
        @type_param_map[name]? || name
      when CrystalV2::Compiler::Frontend::ConstantNode
        name = String.new(node.name)
        @type_param_map[name]? || name
      when CrystalV2::Compiler::Frontend::PathNode
        left = node.left
        left_name = left ? stringify_type_expr(left.not_nil!) : nil
        right_name = stringify_type_expr(node.right)
        return nil unless right_name
        left_name ? "#{left_name}::#{right_name}" : right_name
      when CrystalV2::Compiler::Frontend::GenericNode
        base = stringify_type_expr(node.base_type)
        return nil unless base
        args = [] of String
        node.type_args.each do |arg|
          if str = stringify_type_expr(arg)
            args << str
          end
        end
        "#{base}(#{args.join(", ")})"
      when CrystalV2::Compiler::Frontend::UnaryNode
        base = stringify_type_expr(node.operand)
        return nil unless base
        op = String.new(node.operator)
        case op
        when "?"
          "#{base}?"
        when "*", "**"
          "#{base}#{op}"
        else
          nil
        end
      when CrystalV2::Compiler::Frontend::BinaryNode
        op = String.new(node.operator)
        return nil unless op == "|"
        left = stringify_type_expr(node.left)
        right = stringify_type_expr(node.right)
        return nil unless left && right
        "#{left} | #{right}"
      else
        nil
      end
    end

    private def normalize_typeof_type_name(type_name : String) : String
      return "Pointer(Void)" if type_name.empty? || type_name == "Void" || type_name == "Unknown" || type_name.includes?("|")
      type_name
    end

    private def resolve_typeof_expr(expr_id : ExprId) : String
      node = @arena[expr_id]
      case node
      when CrystalV2::Compiler::Frontend::IdentifierNode
        resolve_typeof_inner(String.new(node.name))
      when CrystalV2::Compiler::Frontend::InstanceVarNode
        resolve_typeof_inner(String.new(node.name))
      when CrystalV2::Compiler::Frontend::ClassVarNode
        resolve_typeof_inner(String.new(node.name))
      when CrystalV2::Compiler::Frontend::SelfNode
        resolve_typeof_inner("self")
      else
        "Pointer(Void)"
      end
    end

    private def resolve_typeof_inner(expr : String) : String
      expr = expr.strip
      if expr == "self"
        return normalize_typeof_type_name(@current_class || "Pointer(Void)")
      end

      if locals = @current_typeof_locals
        if type_ref = locals[expr]?
          return normalize_typeof_type_name(get_type_name_from_ref(type_ref))
        end
      end

      if class_name = @current_class
        if info = @class_info[class_name]?
          if expr.starts_with?("@@")
            cvar_name = expr.lstrip('@')
            if cvar = info.class_vars.find { |cv| cv.name == cvar_name }
              return normalize_typeof_type_name(get_type_name_from_ref(cvar.type))
            end
          elsif expr.starts_with?("@")
            if ivar = info.ivars.find { |iv| iv.name == expr }
              return normalize_typeof_type_name(get_type_name_from_ref(ivar.type))
            end
          end
        end
      end

      drop_nil = false
      if expr.ends_with?(".not_nil!")
        drop_nil = true
        expr = expr[0, expr.size - 9].strip
      end

      index_suffix = nil
      if expr.ends_with?("]")
        if match = expr.match(/^(.*)\[(\d+)\]\s*$/)
          expr = match[1].strip
          index_suffix = match[2].to_i
        end
      end

      if resolved = resolve_element_type_expression(expr)
        resolved = apply_index_to_type_name(resolved, index_suffix) if index_suffix
        resolved = drop_nil_from_union(resolved) if drop_nil
        return normalize_typeof_type_name(resolved)
      end

      # Allow simple constant/path typeof usage in type contexts (no local scope needed).
      if expr.includes?("::") || (expr.size > 0 && expr[0].uppercase?)
        resolved = resolve_type_alias_chain(expr)
        if !resolved.includes?("::")
          resolved = resolve_class_name_in_context(resolved)
        end
        if @class_info.has_key?(resolved) || @module_defs.has_key?(resolved)
          return normalize_typeof_type_name(resolved)
        end
      end

      "Pointer(Void)"
    end

    ELEMENT_TYPE_PREFIXES = ["Enumerable.element_type", "::Enumerable.element_type", "Indexable.element_type",
                             "::Indexable.element_type", "Iterator.element_type", "::Iterator.element_type",
                             "Iterable.element_type", "::Iterable.element_type"] of String

    private def resolve_element_type_expression(expr : String) : String?
      prefix = ELEMENT_TYPE_PREFIXES.find { |p| expr.starts_with?(p) }
      return nil unless prefix

      rest = expr[prefix.size, expr.size - prefix.size].strip
      arg_str = if rest.starts_with?("(")
                  extract_balanced_paren_content(rest)
                else
                  rest
                end
      return nil if arg_str.nil? || arg_str.empty?

      inner_type = resolve_typeof_inner(arg_str.not_nil!)
      if inner_type == "Pointer(Void)" && arg_str.not_nil!.size > 0 && arg_str.not_nil![0].uppercase?
        inner_type = arg_str.not_nil!
      end

      element_type_for_type_name(inner_type)
    end

    private def extract_balanced_paren_content(expr : String) : String?
      return nil unless expr.starts_with?("(")
      depth = 0
      i = 0
      while i < expr.bytesize
        ch = expr.byte_at(i).unsafe_chr
        case ch
        when '('
          depth += 1
        when ')'
          depth -= 1
          if depth == 0
            return expr[1, i - 1].strip
          end
        end
        i += 1
      end
      nil
    end

    private def element_type_for_type_name(type_name : String) : String?
      name = type_name.strip
      if name.includes?("|")
        variants = name.split("|").map(&.strip)
        element_variants = variants.compact_map { |v| element_type_for_type_name(v) }
        uniq = element_variants.uniq
        return uniq.join(" | ") unless uniq.empty?
      end

      if name.ends_with?("?")
        base = name[0, name.size - 1]
        return element_type_for_type_name(base)
      end

      paren = name.index('(')
      if paren && name.ends_with?(")")
        base = name[0, paren]
        params_str = name[paren + 1, name.size - paren - 2]
        args = split_generic_type_args(params_str)
        case base
        when "Array", "StaticArray", "Slice", "Deque", "Set", "Indexable", "Enumerable", "Iterator", "Iterable", "Range"
          return args.first?
        when "Hash"
          return "Tuple(#{args[0]}, #{args[1]})" if args.size >= 2
        end
      end

      nil
    end

    private def apply_index_to_type_name(type_name : String, index : Int32?) : String
      return type_name unless index
      name = type_name.strip
      if name.includes?("|")
        variants = name.split("|").map(&.strip)
        indexed = variants.compact_map { |v| apply_index_to_type_name(v, index) }
        uniq = indexed.uniq
        return uniq.join(" | ") unless uniq.empty?
      end

      if name.starts_with?("Tuple(") && name.ends_with?(")")
        params_str = name[6, name.size - 7]
        args = split_generic_type_args(params_str)
        return args[index]? || type_name
      end

      if name.starts_with?("Array(") && name.ends_with?(")")
        params_str = name[6, name.size - 7]
        args = split_generic_type_args(params_str)
        return args.first? || type_name
      end

      type_name
    end

    private def drop_nil_from_union(type_name : String) : String
      return type_name unless type_name.includes?("|")
      parts = type_name.split("|").map(&.strip)
      filtered = parts.reject { |p| p == "Nil" }
      return "Nil" if filtered.empty?
      filtered.uniq.join(" | ")
    end

    private def resolve_typeof_in_type_string(type_name : String) : String
      return type_name unless type_name.includes?("typeof(")

      output = String.build do |io|
        i = 0
        while i < type_name.bytesize
          if i + 7 <= type_name.bytesize && type_name.byte_slice(i, 7) == "typeof("
            start = i + 7
            depth = 1
            j = start
            while j < type_name.bytesize && depth > 0
              ch = type_name[j]
              if ch == '('
                depth += 1
              elsif ch == ')'
                depth -= 1
              end
              j += 1
            end
            if depth != 0
              io << type_name[i]
              i += 1
              next
            end
            inner = type_name.byte_slice(start, j - start - 1)
            io << resolve_typeof_inner(inner)
            i = j
          else
            io << type_name[i]
            i += 1
          end
        end
      end

      output
    end

    private def resolve_alias_target(target_name : String) : String
      resolved = normalize_declared_type_name(target_name)
      return target_name if resolved.includes?("Pointer(Void)") || resolved.includes?("Unknown")
      resolved
    end

    private def normalize_declared_type_name(type_name : String) : String
      resolved = resolve_typeof_in_type_string(type_name)
      @type_param_map.each do |param, actual|
        resolved = substitute_type_param(resolved, param, actual)
      end
      resolved
    end

    private def update_typeof_local(name : String, type_ref : TypeRef) : Nil
      return unless locals = @current_typeof_locals
      locals[name] = type_ref
    end

    private def update_typeof_local_name(name : String, type_name : String) : Nil
      return unless locals = @current_typeof_local_names
      locals[name] = normalize_declared_type_name(type_name)
    end

    private def concrete_type_name_for(type_ref : TypeRef) : String?
      return nil if type_ref == TypeRef::VOID

      # Prefer direct primitive names to avoid missing descriptors.
      primitive_name = case type_ref
                       when TypeRef::NIL     then "Nil"
                       when TypeRef::BOOL    then "Bool"
                       when TypeRef::INT8    then "Int8"
                       when TypeRef::INT16   then "Int16"
                       when TypeRef::INT32   then "Int32"
                       when TypeRef::INT64   then "Int64"
                       when TypeRef::INT128  then "Int128"
                       when TypeRef::UINT8   then "UInt8"
                       when TypeRef::UINT16  then "UInt16"
                       when TypeRef::UINT32  then "UInt32"
                       when TypeRef::UINT64  then "UInt64"
                       when TypeRef::UINT128 then "UInt128"
                       when TypeRef::FLOAT32 then "Float32"
                       when TypeRef::FLOAT64 then "Float64"
                       when TypeRef::CHAR    then "Char"
                       when TypeRef::STRING  then "String"
                       when TypeRef::SYMBOL  then "Symbol"
                       when TypeRef::POINTER then "Pointer"
                       else nil
                       end
      return primitive_name if primitive_name

      if desc = @module.get_type_descriptor(type_ref)
        return nil if desc.kind.in?(TypeKind::Module, TypeKind::Union)
        return desc.name
      end

      nil
    end

    private def lookup_typeof_local_name(name : String) : String?
      @current_typeof_local_names.try(&.[name]?)
    end

    private def with_arena(arena : CrystalV2::Compiler::Frontend::ArenaLike, &)
      old_arena = @arena
      @arena = arena
      begin
        yield
      ensure
        @arena = old_arena
      end
    end

    private def with_type_param_map(extra : Hash(String, String), &)
      old_map = @type_param_map
      @type_param_map = old_map.merge(extra)
      begin
        yield
      ensure
        @type_param_map = old_map
      end
    end

    private def include_type_param_map(
      module_node : CrystalV2::Compiler::Frontend::ModuleNode,
      include_target : ExprId,
      include_arena : CrystalV2::Compiler::Frontend::ArenaLike = @arena
    ) : Hash(String, String)
      extra = {} of String => String
      type_params = module_node.type_params
      return extra unless type_params

      arg_strings = [] of String
      target_node = include_arena[include_target]
      return extra unless target_node.is_a?(CrystalV2::Compiler::Frontend::GenericNode)
      with_arena(include_arena) do
        target_node.type_args.each do |arg|
          if str = stringify_type_expr(arg)
            arg_strings << str
          end
        end
      end
      return extra if arg_strings.empty?

      type_params.each_with_index do |tp, idx|
        break if idx >= arg_strings.size
        extra[String.new(tp)] = arg_strings[idx]
      end
      extra
    end

    private def unwrap_visibility_member(member)
      while member.is_a?(CrystalV2::Compiler::Frontend::VisibilityModifierNode)
        member = @arena[member.expression]
      end
      member
    end

    private def collect_defined_instance_method_full_names(class_name : String, body : Array(ExprId)) : Set(String)
      defined = Set(String).new
      body.each do |expr_id|
        member = unwrap_visibility_member(@arena[expr_id])
        case member
        when CrystalV2::Compiler::Frontend::DefNode
          next if member.is_abstract
          next if (recv = member.receiver) && String.new(recv) == "self"

          method_name = String.new(member.name)
          base_name = "#{class_name}##{method_name}"

          param_types = [] of TypeRef
          has_block = false
          if params = member.params
            params.each do |param|
              next if named_only_separator?(param)
              if param.is_block
                has_block = true
                next
              end
              if ta = param.type_annotation
                param_types << type_ref_for_name(String.new(ta))
              else
                param_types << TypeRef::VOID
              end
            end
          end

          full_name = mangle_function_name(base_name, param_types, has_block)
          defined << full_name
        when CrystalV2::Compiler::Frontend::GetterNode
          member.specs.each do |spec|
            accessor_name = String.new(spec.name)
            base_name = "#{class_name}##{accessor_name}"
            full_name = mangle_function_name(base_name, [] of TypeRef)
            defined << full_name
          end
        when CrystalV2::Compiler::Frontend::SetterNode
          member.specs.each do |spec|
            accessor_name = String.new(spec.name)
            base_name = "#{class_name}##{accessor_name}="
            param_type = if ta = spec.type_annotation
                           type_ref_for_name(String.new(ta))
                         else
                           TypeRef::VOID
                         end
            full_name = mangle_function_name(base_name, [param_type])
            defined << full_name
          end
        when CrystalV2::Compiler::Frontend::PropertyNode
          member.specs.each do |spec|
            accessor_name = String.new(spec.name)
            getter_base = "#{class_name}##{accessor_name}"
            setter_base = "#{class_name}##{accessor_name}="
            getter_full = mangle_function_name(getter_base, [] of TypeRef)
            param_type = if ta = spec.type_annotation
                           type_ref_for_name(String.new(ta))
                         else
                           TypeRef::VOID
                         end
            setter_full = mangle_function_name(setter_base, [param_type])
            defined << getter_full
            defined << setter_full
          end
        end
      end
      defined
    end

    private def register_accessor_from_module(
      class_name : String,
      spec : CrystalV2::Compiler::Frontend::AccessorSpec,
      ivars : Array(IVarInfo),
      offset : Int32,
      defined_full_names : Set(String),
      is_struct : Bool,
      include_getter : Bool,
      include_setter : Bool
    ) : Int32
      accessor_name = String.new(spec.name)
      ivar_name = "@#{accessor_name}"
      ivar_type = if ta = spec.type_annotation
                    type_ref_for_name(String.new(ta))
                  else
                    TypeRef::VOID
                  end

      unless ivars.any? { |iv| iv.name == ivar_name }
        ivars << IVarInfo.new(ivar_name, ivar_type, offset)
        offset += type_size(ivar_type)
      end

      if include_getter
        getter_name = "#{class_name}##{accessor_name}"
        getter_full = mangle_function_name(getter_name, [] of TypeRef)
        unless defined_full_names.includes?(getter_full)
          register_function_type(getter_full, ivar_type)
        end
      end

      if include_setter
        setter_name = "#{class_name}##{accessor_name}="
        setter_full = mangle_function_name(setter_name, [ivar_type])
        unless defined_full_names.includes?(setter_full)
          setter_return = is_struct ? TypeRef::VOID : ivar_type
          register_function_type(setter_full, setter_return)
        end
      end

      offset
    end

    private def register_module_instance_methods_for(
      class_name : String,
      include_node : CrystalV2::Compiler::Frontend::IncludeNode,
      defined_full_names : Set(String),
      visited : Set(String),
      ivars : Array(IVarInfo),
      offset : Int32,
      is_struct : Bool
    ) : Int32
      module_full_name = resolve_path_like_name(include_node.target)
      return offset unless module_full_name
      record_module_inclusion(module_full_name, class_name)
      return offset if visited.includes?(module_full_name)
      visited << module_full_name

      defs = @module_defs[module_full_name]?
      return offset unless defs
      include_arena = @arena
      defs.each do |mod_node, mod_arena|
        with_arena(mod_arena) do
          extra_map = include_type_param_map(mod_node, include_node.target, include_arena)
          with_type_param_map(extra_map) do
            if body = mod_node.body
              body.each do |member_id|
                member = unwrap_visibility_member(@arena[member_id])
                case member
                when CrystalV2::Compiler::Frontend::IncludeNode
                  offset = register_module_instance_methods_for(
                    class_name,
                    member,
                    defined_full_names,
                    visited,
                    ivars,
                    offset,
                    is_struct
                  )
                when CrystalV2::Compiler::Frontend::DefNode
                  next if (recv = member.receiver) && String.new(recv) == "self"
                  next if member.is_abstract

                  method_name = String.new(member.name)
                  base_name = "#{class_name}##{method_name}"

                  return_type = if rt = member.return_type
                                  rt_name = String.new(rt)
                                  inferred = module_like_type_name?(rt_name) ? infer_concrete_return_type_from_body(member, class_name) : nil
                                  inferred || type_ref_for_name(rt_name)
                                elsif method_name.ends_with?("?")
                                  TypeRef::BOOL
                                else
                                  TypeRef::VOID
                                end

                  param_types = [] of TypeRef
                  has_block = false
                  if params = member.params
                    params.each do |param|
                      next if named_only_separator?(param)
                      if param.is_block
                        has_block = true
                        next
                      end
                      param_type = if ta = param.type_annotation
                                     type_ref_for_name(String.new(ta))
                                   else
                                     TypeRef::VOID
                                   end
                      param_types << param_type
                    end
                  end

                  full_name = mangle_function_name(base_name, param_types, has_block)
                  next if defined_full_names.includes?(full_name)

                  register_function_type(full_name, return_type)
                  @function_defs[full_name] = member
                  @function_def_arenas[full_name] = @arena

                  if body = member.body
                    if contains_yield?(body)
                      @yield_functions.add(full_name)
                      unless @function_defs.has_key?(base_name)
                        @function_defs[base_name] = member
                        @function_def_arenas[base_name] = @arena
                      end
                      @function_defs[full_name] = member
                      @function_def_arenas[full_name] = @arena
                    end
                  end
                when CrystalV2::Compiler::Frontend::GetterNode
                  member.specs.each do |spec|
                    offset = register_accessor_from_module(
                      class_name,
                      spec,
                      ivars,
                      offset,
                      defined_full_names,
                      is_struct,
                      true,
                      false
                    )
                  end
                when CrystalV2::Compiler::Frontend::SetterNode
                  member.specs.each do |spec|
                    offset = register_accessor_from_module(
                      class_name,
                      spec,
                      ivars,
                      offset,
                      defined_full_names,
                      is_struct,
                      false,
                      true
                    )
                  end
                when CrystalV2::Compiler::Frontend::PropertyNode
                  member.specs.each do |spec|
                    offset = register_accessor_from_module(
                      class_name,
                      spec,
                      ivars,
                      offset,
                      defined_full_names,
                      is_struct,
                      true,
                      true
                    )
                  end
                end
              end
            end
          end
        end
      end
      offset
    end

    private def lower_module_instance_methods_for(
      class_name : String,
      class_info : ClassInfo,
      include_node : CrystalV2::Compiler::Frontend::IncludeNode,
      defined_full_names : Set(String),
      visited : Set(String)
    )
      module_full_name = resolve_path_like_name(include_node.target)
      return unless module_full_name
      record_module_inclusion(module_full_name, class_name)
      return if visited.includes?(module_full_name)
      visited << module_full_name

      defs = @module_defs[module_full_name]? || return
      include_arena = @arena
      defs.each do |mod_node, mod_arena|
        with_arena(mod_arena) do
          extra_map = include_type_param_map(mod_node, include_node.target, include_arena)
          with_type_param_map(extra_map) do
            if body = mod_node.body
              body.each do |member_id|
                member = unwrap_visibility_member(@arena[member_id])
                case member
                when CrystalV2::Compiler::Frontend::IncludeNode
                  lower_module_instance_methods_for(class_name, class_info, member, defined_full_names, visited)
                when CrystalV2::Compiler::Frontend::DefNode
                  next if (recv = member.receiver) && String.new(recv) == "self"
                  next if member.is_abstract

                  method_name = String.new(member.name)
                  base_name = "#{class_name}##{method_name}"

                  param_types = [] of TypeRef
                  has_block = false
                  if params = member.params
                    params.each do |param|
                      next if named_only_separator?(param)
                      if param.is_block
                        has_block = true
                        next
                      end
                      if ta = param.type_annotation
                        param_types << type_ref_for_name(String.new(ta))
                      else
                        param_types << TypeRef::VOID
                      end
                    end
                  end

                  full_name = mangle_function_name(base_name, param_types, has_block)
                  next if defined_full_names.includes?(full_name)
                  next if @module.has_function?(full_name)

                  lower_method(class_name, class_info, member)
                when CrystalV2::Compiler::Frontend::GetterNode
                  member.specs.each do |spec|
                    generate_getter_method(class_name, class_info, spec)
                  end
                when CrystalV2::Compiler::Frontend::SetterNode
                  member.specs.each do |spec|
                    generate_setter_method(class_name, class_info, spec)
                  end
                when CrystalV2::Compiler::Frontend::PropertyNode
                  member.specs.each do |spec|
                    generate_getter_method(class_name, class_info, spec)
                    generate_setter_method(class_name, class_info, spec)
                  end
                end
              end
            end
          end
        end
      end
    end

    private def module_like_type_name?(name : String) : Bool
      base = if paren = name.index('(')
               name[0, paren]
             else
               name
             end
      @module_defs.has_key?(base)
    end

    NILABLE_QUERY_METHODS = ["[]?", "at?", "first?", "last?", "pop?", "shift?"] of String

    # Some stdlib methods end in `?` but return a value (typically `T?` / `V?`) rather than `Bool`.
    # We need correct return types early (during signature registration) so callers lowered before the
    # callee body still get a stable type (avoids emitting `i1` where `i32`/union is expected).
    private def infer_unannotated_query_return_type(method_name : String, self_type : TypeRef) : TypeRef?
      return nil unless NILABLE_QUERY_METHODS.includes?(method_name)

      desc = @module.get_type_descriptor(self_type)
      return nil unless desc

      case desc.kind
      when TypeKind::Array
        elem = desc.type_params.first?
        return nil unless elem
        create_union_type_for_nullable(elem)
      when TypeKind::Hash
        value = desc.type_params[1]?
        return nil unless value
        create_union_type_for_nullable(value)
      else
        nil
      end
    end

    private def infer_concrete_return_type_from_body(
      node : CrystalV2::Compiler::Frontend::DefNode,
      self_type_name : String? = nil
    ) : TypeRef?
      body = node.body
      return nil unless body && !body.empty?

      # Use the last expression as a heuristic return (handles simple multi-line bodies).
      expr_id = body.last
      loop do
        expr_node = @arena[expr_id]
        case expr_node
        when CrystalV2::Compiler::Frontend::GroupingNode
          expr_id = expr_node.expression
        when CrystalV2::Compiler::Frontend::MacroExpressionNode
          expr_id = expr_node.expression
        when CrystalV2::Compiler::Frontend::ReturnNode
          value = expr_node.value
          return nil unless value
          expr_id = value
        else
          break
        end
      end

      if inferred = infer_type_from_expr(expr_id, self_type_name)
        return inferred
      end

      expr_node = @arena[expr_id]
      case expr_node
      when CrystalV2::Compiler::Frontend::IdentifierNode
        name = String.new(expr_node.name)
        if inferred = infer_local_type_from_body(body, name, self_type_name)
          return inferred
        end
      when CrystalV2::Compiler::Frontend::AssignNode
        return infer_type_from_expr(expr_node.value, self_type_name)
      when CrystalV2::Compiler::Frontend::TypeDeclarationNode
        return type_ref_for_name(String.new(expr_node.declared_type))
      end

      nil
    end

    private def infer_type_from_expr(expr_id : ExprId, self_type_name : String?) : TypeRef?
      expr_node = @arena[expr_id]
      case expr_node
      when CrystalV2::Compiler::Frontend::GroupingNode
        return infer_type_from_expr(expr_node.expression, self_type_name)
      when CrystalV2::Compiler::Frontend::MacroExpressionNode
        return infer_type_from_expr(expr_node.expression, self_type_name)
      when CrystalV2::Compiler::Frontend::ReturnNode
        value = expr_node.value
        return nil unless value
        return infer_type_from_expr(value, self_type_name)
      when CrystalV2::Compiler::Frontend::SelfNode
        return type_ref_for_name(self_type_name) if self_type_name
      when CrystalV2::Compiler::Frontend::IdentifierNode
        if self_type_name && String.new(expr_node.name) == "self"
          return type_ref_for_name(self_type_name)
        end
      when CrystalV2::Compiler::Frontend::InstanceVarNode
        if self_type_name
          if info = @class_info[self_type_name]?
            ivar_name = String.new(expr_node.name)
            if ivar = info.ivars.find { |iv| iv.name == ivar_name }
              return ivar.type
            end
          end
        end
      when CrystalV2::Compiler::Frontend::CallNode
        callee_node = @arena[expr_node.callee]
        if callee_node.is_a?(CrystalV2::Compiler::Frontend::MemberAccessNode)
          member_name = String.new(callee_node.member)
          if member_name == "new"
            if type_str = stringify_type_expr(callee_node.object)
              return type_ref_for_name(type_str)
            end
          end
        end
      end

      nil
    end

    private def infer_local_type_from_body(
      body : Array(ExprId),
      name : String,
      self_type_name : String?
    ) : TypeRef?
      body.reverse_each do |expr_id|
        expr_node = @arena[expr_id]
        case expr_node
        when CrystalV2::Compiler::Frontend::AssignNode
          target = @arena[expr_node.target]
          if target.is_a?(CrystalV2::Compiler::Frontend::IdentifierNode) &&
             String.new(target.name) == name
            return infer_type_from_expr(expr_node.value, self_type_name)
          end
        when CrystalV2::Compiler::Frontend::TypeDeclarationNode
          if String.new(expr_node.name) == name
            return type_ref_for_name(String.new(expr_node.declared_type))
          end
        end
      end

      nil
    end

    # Register a module and its methods (pass 1)
    # Modules are like classes but with only class methods (self.method)
    # Also handles nested classes: module Foo; class Bar; end; end -> Foo::Bar
    def register_module(node : CrystalV2::Compiler::Frontend::ModuleNode)
      module_name = String.new(node.name)
      if ENV.has_key?("DEBUG_NESTED_CLASS") && (module_name == "IO" || module_name.includes?("FileDescriptor"))
        STDERR.puts "[DEBUG_MODULE] Processing module: #{module_name}, body_size=#{node.body.try(&.size) || 0}"
      end

      # Keep module AST around for mixin expansion (`include Foo` in classes/structs).
      (@module_defs[module_name] ||= [] of {CrystalV2::Compiler::Frontend::ModuleNode, CrystalV2::Compiler::Frontend::ArenaLike}) << {node, @arena}

      # Register module methods (def self.foo) and nested classes
      if body = node.body
        # PASS 1: Register aliases and nested modules first (so they're available for function type resolution)
        body.each do |expr_id|
          member = unwrap_visibility_member(@arena[expr_id])
          if member.is_a?(CrystalV2::Compiler::Frontend::AliasNode)
            alias_name = String.new(member.name)
            old_class = @current_class
            @current_class = module_name
            target_name = resolve_alias_target(String.new(member.value))
            @current_class = old_class
            full_alias_name = "#{module_name}::#{alias_name}"
            register_type_alias(full_alias_name, target_name)
            register_type_alias(alias_name, target_name)
            if ENV.has_key?("DEBUG_ALIAS")
              STDERR.puts "[ALIAS] Registered (module): #{full_alias_name} => #{target_name}, also: #{alias_name} => #{target_name}"
            end
          elsif member.is_a?(CrystalV2::Compiler::Frontend::ModuleNode)
            # Nested module: Foo::Bar (as module)
            nested_name = String.new(member.name)
            full_nested_name = "#{module_name}::#{nested_name}"
            register_nested_module(member, full_nested_name)
          elsif member.is_a?(CrystalV2::Compiler::Frontend::ClassNode)
            # Register class/struct type alias and any aliases inside the class
            class_name = String.new(member.name)
            full_class_name = "#{module_name}::#{class_name}"
            register_type_alias(full_class_name, full_class_name)
            # Also register short name -> full name for local resolution (for both classes and structs)
            register_type_alias(class_name, full_class_name)
            register_class_aliases(member, full_class_name)
          elsif member.is_a?(CrystalV2::Compiler::Frontend::StructNode)
            # Register struct type alias - both short and full names (for actual StructNode type)
            struct_name = String.new(member.name)
            full_struct_name = "#{module_name}::#{struct_name}"
            register_type_alias(full_struct_name, full_struct_name)
            register_type_alias(struct_name, full_struct_name)
          end
        end
        # PASS 2: Register functions and classes (now that aliases are available)
        body.each do |expr_id|
          member = unwrap_visibility_member(@arena[expr_id])
          case member
          when CrystalV2::Compiler::Frontend::DefNode
            method_name = String.new(member.name)
            # In Crystal, `def self.foo` defines a module (class) method,
            # while `def foo` defines an instance method meant to be mixed in via `include`.
            is_class_method = if recv = member.receiver
                                String.new(recv) == "self"
                              else
                                false
                              end
            next unless is_class_method
            base_name = "#{module_name}.#{method_name}"
            return_type = if rt = member.return_type
                            rt_name = String.new(rt)
                            inferred = module_like_type_name?(rt_name) ? infer_concrete_return_type_from_body(member) : nil
                            inferred || type_ref_for_name(rt_name)
                          elsif method_name.ends_with?("?")
                            TypeRef::BOOL
                          else
                            infer_concrete_return_type_from_body(member) || TypeRef::VOID
                          end
            param_types = [] of TypeRef
            has_block = false
            if params = member.params
              params.each do |param|
                next if named_only_separator?(param)
                if param.is_block
                  has_block = true
                  next
                end
                param_type = if ta = param.type_annotation
                               type_ref_for_name(String.new(ta))
                             else
                               TypeRef::VOID
                             end
                param_types << param_type
              end
            end
            full_name = mangle_function_name(base_name, param_types, has_block)
            register_function_type(full_name, return_type)
            @function_defs[full_name] = member
            @function_def_arenas[full_name] = @arena

            # Track yield-functions for inline expansion (module methods).
            if body = member.body
              if contains_yield?(body)
                @yield_functions.add(full_name)
                unless @function_defs.has_key?(base_name)
                  @function_defs[base_name] = member
                  @function_def_arenas[base_name] = @arena
                end
                @function_defs[full_name] = member
                @function_def_arenas[full_name] = @arena
              end
            end
          when CrystalV2::Compiler::Frontend::GetterNode
            next unless member.is_class?
            member.specs.each do |spec|
              register_class_accessor_entry(module_name, spec, :getter)
            end
          when CrystalV2::Compiler::Frontend::SetterNode
            next unless member.is_class?
            member.specs.each do |spec|
              register_class_accessor_entry(module_name, spec, :setter)
            end
          when CrystalV2::Compiler::Frontend::PropertyNode
            next unless member.is_class?
            member.specs.each do |spec|
              register_class_accessor_entry(module_name, spec, :getter)
              register_class_accessor_entry(module_name, spec, :setter)
            end
          when CrystalV2::Compiler::Frontend::ClassNode
            class_name = String.new(member.name)
            full_class_name = "#{module_name}::#{class_name}"
            if ENV.has_key?("DEBUG_NESTED_CLASS") && full_class_name.includes?("FileDescriptor")
              STDERR.puts "[DEBUG_NESTED_CLASS] Registering nested class: #{full_class_name}"
            end
            register_class_with_name(member, full_class_name)
          when CrystalV2::Compiler::Frontend::EnumNode
            enum_name = String.new(member.name)
            full_enum_name = "#{module_name}::#{enum_name}"
            register_enum_with_name(member, full_enum_name)
          when CrystalV2::Compiler::Frontend::StructNode
            struct_name = String.new(member.name)
            full_struct_name = "#{module_name}::#{struct_name}"
            register_struct_with_name(member, full_struct_name)
          end
        end
      end
    end

    # Register a nested module with full path
    private def register_nested_module(node : CrystalV2::Compiler::Frontend::ModuleNode, full_name : String)
      # Keep nested module AST around for mixin expansion.
      (@module_defs[full_name] ||= [] of {CrystalV2::Compiler::Frontend::ModuleNode, CrystalV2::Compiler::Frontend::ArenaLike}) << {node, @arena}

      if body = node.body
        # PASS 1: Register aliases first (so they're available for function type resolution)
        body.each do |expr_id|
          member = unwrap_visibility_member(@arena[expr_id])
          if member.is_a?(CrystalV2::Compiler::Frontend::AliasNode)
            alias_name = String.new(member.name)
            old_class = @current_class
            @current_class = full_name
            target_name = resolve_alias_target(String.new(member.value))
            @current_class = old_class
            full_alias_name = "#{full_name}::#{alias_name}"
            register_type_alias(full_alias_name, target_name)
            # Also register short name for local resolution within module
            register_type_alias(alias_name, target_name)
            if ENV.has_key?("DEBUG_ALIAS")
              STDERR.puts "[ALIAS] Registered: #{full_alias_name} => #{target_name}, also: #{alias_name} => #{target_name}"
            end
          elsif member.is_a?(CrystalV2::Compiler::Frontend::ModuleNode)
            # Recursively register nested module aliases first
            nested_name = String.new(member.name)
            full_nested_name = "#{full_name}::#{nested_name}"
            register_nested_module(member, full_nested_name)
          elsif member.is_a?(CrystalV2::Compiler::Frontend::ClassNode)
            # Register class/struct type alias and any aliases inside the class
            class_name = String.new(member.name)
            full_class_name = "#{full_name}::#{class_name}"
            register_type_alias(full_class_name, full_class_name)
            # Also register short name -> full name for local resolution (for both classes and structs)
            register_type_alias(class_name, full_class_name)
            register_class_aliases(member, full_class_name)
          elsif member.is_a?(CrystalV2::Compiler::Frontend::StructNode)
            # Register struct type alias - both short and full names
            struct_name = String.new(member.name)
            full_struct_name = "#{full_name}::#{struct_name}"
            register_type_alias(full_struct_name, full_struct_name)
            # Also register short name -> full name for local resolution
            register_type_alias(struct_name, full_struct_name)
          end
        end
        # PASS 2: Register functions and other members (now that aliases are available)
        body.each do |expr_id|
          member = unwrap_visibility_member(@arena[expr_id])
          case member
          when CrystalV2::Compiler::Frontend::DefNode
            method_name = String.new(member.name)
            is_class_method = if recv = member.receiver
                                String.new(recv) == "self"
                              else
                                false
                              end
            next unless is_class_method
            base_name = "#{full_name}.#{method_name}"
            return_type = if rt = member.return_type
                            rt_name = String.new(rt)
                            inferred = module_like_type_name?(rt_name) ? infer_concrete_return_type_from_body(member) : nil
                            inferred || type_ref_for_name(rt_name)
                          elsif method_name.ends_with?("?")
                            TypeRef::BOOL
                          else
                            infer_concrete_return_type_from_body(member) || TypeRef::VOID
                          end
            param_types = [] of TypeRef
            has_block = false
            if params = member.params
              params.each do |param|
                next if named_only_separator?(param)
                if param.is_block
                  has_block = true
                  next
                end
                param_type = if ta = param.type_annotation
                               type_ref_for_name(String.new(ta))
                             else
                               TypeRef::VOID
                             end
                param_types << param_type
              end
            end
            full_method_name = mangle_function_name(base_name, param_types, has_block)
            register_function_type(full_method_name, return_type)
            @function_defs[full_method_name] = member
            @function_def_arenas[full_method_name] = @arena

            # Track yield-functions for inline expansion (nested module methods).
            if body = member.body
              if contains_yield?(body)
                @yield_functions.add(full_method_name)
                unless @function_defs.has_key?(base_name)
                  @function_defs[base_name] = member
                  @function_def_arenas[base_name] = @arena
                end
                @function_defs[full_method_name] = member
                @function_def_arenas[full_method_name] = @arena
              end
            end
          when CrystalV2::Compiler::Frontend::ClassNode
            class_name = String.new(member.name)
            full_class_name = "#{full_name}::#{class_name}"
            register_class_with_name(member, full_class_name)
          when CrystalV2::Compiler::Frontend::EnumNode
            enum_name = String.new(member.name)
            full_enum_name = "#{full_name}::#{enum_name}"
            register_enum_with_name(member, full_enum_name)
          when CrystalV2::Compiler::Frontend::StructNode
            struct_name = String.new(member.name)
            full_struct_name = "#{full_name}::#{struct_name}"
            register_struct_with_name(member, full_struct_name)
          end
        end
      end
    end

    # Register aliases inside a class (for nested alias-first processing)
    private def register_class_aliases(node : CrystalV2::Compiler::Frontend::ClassNode, class_name : String)
      if body = node.body
        body.each do |expr_id|
          member = unwrap_visibility_member(@arena[expr_id])
          if member.is_a?(CrystalV2::Compiler::Frontend::AliasNode)
            alias_name = String.new(member.name)
            old_class = @current_class
            @current_class = class_name
            target_name = resolve_alias_target(String.new(member.value))
            @current_class = old_class
            full_alias_name = "#{class_name}::#{alias_name}"
            register_type_alias(full_alias_name, target_name)
            register_type_alias(alias_name, target_name)
            if ENV.has_key?("DEBUG_ALIAS")
              STDERR.puts "[ALIAS] Registered (class): #{full_alias_name} => #{target_name}, also: #{alias_name} => #{target_name}"
            end
          end
        end
      end
    end

    # Lower a module's methods and nested classes (pass 3)
    def lower_module(node : CrystalV2::Compiler::Frontend::ModuleNode)
      module_name = String.new(node.name)
      lower_module_with_name(node, module_name)
    end

    # Lower a module with a specific name prefix
    private def lower_module_with_name(node : CrystalV2::Compiler::Frontend::ModuleNode, module_name : String)
      if ENV.has_key?("DEBUG_NESTED_CLASS") && (module_name == "IO" || module_name.includes?("FileDescriptor"))
        STDERR.puts "[DEBUG_LOWER_MOD] lower_module_with_name: #{module_name}, body_size=#{node.body.try(&.size) || 0}"
      end
      if body = node.body
        body.each_with_index do |expr_id, idx|
          member = unwrap_visibility_member(@arena[expr_id])
          if ENV.has_key?("DEBUG_NESTED_CLASS") && (module_name == "IO" || module_name.includes?("FileDescriptor"))
            STDERR.puts "[DEBUG_LOWER_MOD] #{module_name} member #{idx}: #{member.class}"
          end
          case member
          when CrystalV2::Compiler::Frontend::DefNode
            method_name = String.new(member.name)
            is_class_method = if recv = member.receiver
                                String.new(recv) == "self"
                              else
                                false
                              end
            next unless is_class_method
            STDERR.puts "      [#{module_name}] Method #{idx}: #{method_name}" if ENV["HIR_DEBUG"]?
            STDERR.flush if ENV["HIR_DEBUG"]?
            lower_module_method(module_name, member)
          when CrystalV2::Compiler::Frontend::GetterNode
            next unless member.is_class?
            member.specs.each do |spec|
              generate_class_getter_method(module_name, spec, @arena)
            end
          when CrystalV2::Compiler::Frontend::SetterNode
            next unless member.is_class?
            member.specs.each do |spec|
              generate_class_setter_method(module_name, spec)
            end
          when CrystalV2::Compiler::Frontend::PropertyNode
            next unless member.is_class?
            member.specs.each do |spec|
              generate_class_getter_method(module_name, spec, @arena)
              generate_class_setter_method(module_name, spec)
            end
          when CrystalV2::Compiler::Frontend::ClassNode
            # Lower nested class with full name
            class_name = String.new(member.name)
            full_class_name = "#{module_name}::#{class_name}"
            if ENV.has_key?("DEBUG_NESTED_CLASS") && (module_name == "IO" || class_name.includes?("FileDescriptor"))
              STDERR.puts "[DEBUG_LOWER_MOD] lowering nested class: #{full_class_name}"
            end
            lower_class_with_name(member, full_class_name)
          when CrystalV2::Compiler::Frontend::ModuleNode
            # Recursively lower nested module
            nested_name = String.new(member.name)
            full_nested_name = "#{module_name}::#{nested_name}"
            lower_module_with_name(member, full_nested_name)
          when CrystalV2::Compiler::Frontend::StructNode
            # Lower nested struct with full name
            struct_name = String.new(member.name)
            full_struct_name = "#{module_name}::#{struct_name}"
            lower_struct_with_name(member, full_struct_name)
          end
        end
      end
    end

    # Lower a module method (static function)
    private def lower_module_method(
      module_name : String,
      node : CrystalV2::Compiler::Frontend::DefNode,
      call_arg_types : Array(TypeRef)? = nil,
      full_name_override : String? = nil
    )
      method_name = String.new(node.name)
      base_name = "#{module_name}.#{method_name}"

      old_class = @current_class
      old_method = @current_method
      old_method_is_class = @current_method_is_class
      @current_class = module_name
      @current_method = method_name
      @current_method_is_class = true

      return_type = if rt = node.return_type
                      type_ref_for_name(String.new(rt))
                    else
                      TypeRef::VOID
                    end

      # Collect parameter types for name mangling
      param_infos = [] of Tuple(String, TypeRef)
      param_types = [] of TypeRef
      has_block = false
      param_type_map = {} of String => TypeRef
      old_typeof_locals = @current_typeof_locals
      old_typeof_local_names = @current_typeof_local_names
      @current_typeof_locals = param_type_map
      @current_typeof_local_names = {} of String => String
      call_types = call_arg_types || [] of TypeRef
      call_index = 0
      splat_param_info_index : Int32? = nil
      splat_param_types_index : Int32? = nil
      splat_param_name : String? = nil
      splat_param_has_annotation = false

      if params = node.params
        params.each do |param|
          next if named_only_separator?(param)
          param_name = param.name.nil? ? "_" : String.new(param.name.not_nil!)
          param_type = if ta = param.type_annotation
                         type_ref_for_name(String.new(ta))
                       else
                         TypeRef::VOID
                       end
          if param_type == TypeRef::VOID && !param.is_block && !param.is_splat && !param.is_double_splat
            if call_index < call_types.size
              inferred = call_types[call_index]
              param_type = inferred if inferred != TypeRef::VOID
            end
          end
          if !param.is_block && !param.is_splat && !param.is_double_splat && call_index < call_types.size
            param_type = refine_param_type_from_call(param_type, call_types[call_index])
          end
          param_type_map[param_name] = param_type
          param_infos << {param_name, param_type}
          if ta = param.type_annotation
            update_typeof_local_name(param_name, String.new(ta))
          end
          if param.is_block
            has_block = true
          else
            if param.is_splat || param.is_double_splat
              splat_param_info_index = param_infos.size - 1
              splat_param_types_index = param_types.size
              splat_param_name = param_name
              splat_param_has_annotation = !param.type_annotation.nil?
            else
              call_index += 1
            end
            param_types << param_type
          end
        end
      end

      if splat_param_name && !call_types.empty? && !splat_param_has_annotation
        remaining = call_types[call_index..-1]? || [] of TypeRef
        splat_type = if remaining.size == 1
                       remaining[0]
                     else
                       tuple_type_from_arg_types(remaining)
                     end
        if splat_type != TypeRef::VOID
          param_type_map[splat_param_name.not_nil!] = splat_type
          if idx = splat_param_info_index
            param_infos[idx] = {splat_param_name.not_nil!, splat_type}
          end
          if idx = splat_param_types_index
            param_types[idx] = splat_type
          end
        end
      end

      # Mangle function name with parameter types
      full_name = full_name_override || mangle_function_name(base_name, param_types, has_block)

      func = @module.create_function(full_name, return_type)
      ctx = LoweringContext.new(func, @module, @arena)

      # Lower parameters (no self for module methods)
      param_infos.each do |(param_name, param_type)|
        hir_param = func.add_param(param_name, param_type)
        ctx.register_local(param_name, hir_param.id)
        ctx.register_type(hir_param.id, param_type)
      end

      # Lower body
      last_value : ValueId? = nil
      if body = node.body
        body.each do |expr_id|
          last_value = lower_expr(ctx, expr_id)
        end
      end

      # Infer return type from the last expression for unannotated module methods.
      if node.return_type.nil? && last_value
        inferred = ctx.type_of(last_value)
        if inferred != TypeRef::VOID && inferred != return_type
          return_type = inferred
          func.return_type = inferred
        end
      end

      register_function_type(full_name, return_type)

      @current_typeof_locals = old_typeof_locals
      @current_typeof_local_names = old_typeof_local_names

      # Restore current class
      @current_class = old_class
      @current_method = old_method
      @current_method_is_class = old_method_is_class || false

      # Add implicit return if not already terminated
      # BUT don't add return after raise (which sets Unreachable terminator)
      block = ctx.get_block(ctx.current_block)
      block_has_raise = block.instructions.any? { |inst| inst.is_a?(Raise) }
      if block.terminator.is_a?(Unreachable) && !block_has_raise
        block.terminator = Return.new(last_value)
      end
    end

    # Register a class type and its methods (pass 1)
    def register_class(node : CrystalV2::Compiler::Frontend::ClassNode)
      class_name = String.new(node.name)
      if ENV.has_key?("DEBUG_NESTED_CLASS") && (class_name == "IO" || class_name.includes?("FileDescriptor"))
        STDERR.puts "[DEBUG_CLASS_REG] register_class called: #{class_name}"
      end
      register_class_with_name(node, class_name)
    end

    # Register a class with a specific name (for nested classes like Foo::Bar)
    def register_class_with_name(node : CrystalV2::Compiler::Frontend::ClassNode, class_name : String)
      is_struct = node.is_struct == true

      # Check if this is a generic class (has type parameters)
      if type_params = node.type_params
        if type_params.size > 0
          # Store as generic template - don't create ClassInfo yet
          # Keep the template with the LARGEST body (main definition, not reopenings)
          new_body_size = node.body.try(&.size) || 0
          if existing = @generic_templates[class_name]?
            existing_body_size = existing.node.body.try(&.size) || 0
            if new_body_size <= existing_body_size
              return  # Existing template has larger body, skip this reopening
            end
          end
          param_names = type_params.map { |p| String.new(p) }
          @generic_templates[class_name] = GenericClassTemplate.new(
            class_name, param_names, node, @arena, is_struct
          )
          return  # Don't register as concrete class
        end
      end

      # Non-generic class - proceed with normal registration
      register_concrete_class(node, class_name, is_struct)
    end

    # Register a concrete (non-generic or specialized) class
    private def register_concrete_class(node : CrystalV2::Compiler::Frontend::ClassNode, class_name : String, is_struct : Bool)
      # Check if class already exists (class reopening)
      existing_info = @class_info[class_name]?

      # Collect instance variables and their types
      ivars = [] of IVarInfo
      class_vars = [] of ClassVarInfo
      # Struct has no type_id header (value type), class starts at 8 for header
      offset = is_struct ? 0 : 8

      # If class already exists, preserve existing ivars and class_vars
      if existing_info
        existing_info.ivars.each { |iv| ivars << iv.dup }
        existing_info.class_vars.each { |cv| class_vars << cv.dup }
        offset = existing_info.size
      end

      # Inheritance: copy parent ivars first (to preserve layout)
      # Only do this for new class definitions, not reopened classes
      parent_name : String? = nil
      if !existing_info && (super_name_slice = node.super_name)
        parent_name = String.new(super_name_slice)
        if parent_info = @class_info[parent_name]?
          # Copy all ivars from parent, preserving their offsets
          parent_info.ivars.each do |parent_ivar|
            ivars << parent_ivar.dup
          end
          # Start child ivars after parent ivars
          offset = parent_info.size
        end
      elsif existing_info
        parent_name = existing_info.parent_name
      end

      # Also find initialize to get constructor parameters
      init_params = [] of {String, TypeRef}

      if body = node.body
        # PASS 0: Register nested types first so method signatures and bodies can resolve them
        # (e.g., Dir::EntryIterator used as `EntryIterator` inside Dir).
        body.each do |expr_id|
          member = unwrap_visibility_member(@arena[expr_id])
          case member
          when CrystalV2::Compiler::Frontend::ClassNode
            nested_name = String.new(member.name)
            full_nested_name = "#{class_name}::#{nested_name}"
            register_class_with_name(member, full_nested_name)
          when CrystalV2::Compiler::Frontend::StructNode
            nested_name = String.new(member.name)
            full_nested_name = "#{class_name}::#{nested_name}"
            register_struct_with_name(member, full_nested_name)
          when CrystalV2::Compiler::Frontend::EnumNode
            enum_name = String.new(member.name)
            full_enum_name = "#{class_name}::#{enum_name}"
            register_enum_with_name(member, full_enum_name)
          when CrystalV2::Compiler::Frontend::ModuleNode
            nested_name = String.new(member.name)
            full_nested_name = "#{class_name}::#{nested_name}"
            register_nested_module(member, full_nested_name)
          end
        end

        defined_instance_method_full_names = collect_defined_instance_method_full_names(class_name, body)
        include_nodes = [] of CrystalV2::Compiler::Frontend::IncludeNode

        old_class = @current_class
        @current_class = class_name
        begin
        body.each do |expr_id|
          member = unwrap_visibility_member(@arena[expr_id])
          case member
          when CrystalV2::Compiler::Frontend::IncludeNode
            include_nodes << member
          when CrystalV2::Compiler::Frontend::InstanceVarDeclNode
            # Instance variable declaration: @value : Int32
            ivar_name = String.new(member.name)
            ivar_type = type_ref_for_name(String.new(member.type))
            ivars << IVarInfo.new(ivar_name, ivar_type, offset)
            offset += type_size(ivar_type)

          when CrystalV2::Compiler::Frontend::ClassVarDeclNode
            # Class variable declaration: @@total : Int32 = 0
            # Name includes @@ prefix, strip it
            raw_name = String.new(member.name)
            cvar_name = raw_name.lstrip('@')
            cvar_type = type_ref_for_name(String.new(member.type))
            # Get initial value if present (only supporting literal integers for now)
            initial_value : Int64? = nil
            if val_id = member.value
              val_node = @arena[val_id]
              if val_node.is_a?(CrystalV2::Compiler::Frontend::NumberNode)
                # Parse the number from its text representation
                num_str = String.new(val_node.value)
                initial_value = num_str.to_i64?
              end
            end
            class_vars << ClassVarInfo.new(cvar_name, cvar_type, initial_value)

          when CrystalV2::Compiler::Frontend::DefNode
            # Register method signature
            method_name = String.new(member.name)
            # Check if this is a class method (def self.method) or instance method (def method)
            is_class_method = if recv = member.receiver
                                String.new(recv) == "self"
                              else
                                false
                              end
            # Use . for class methods, # for instance methods
            base_name = if is_class_method
                          "#{class_name}.#{method_name}"
                        else
                          "#{class_name}##{method_name}"
                        end
            return_type = if rt = member.return_type
                            rt_name = String.new(rt)
                            inferred = module_like_type_name?(rt_name) ? infer_concrete_return_type_from_body(member, class_name) : nil
                            inferred || type_ref_for_name(rt_name)
                          elsif method_name.ends_with?("?")
                            self_type = type_ref_for_name(class_name)
                            infer_unannotated_query_return_type(method_name, self_type) || TypeRef::BOOL
                          else
                            # Try to infer return type from getter-style methods (single ivar access)
                            inferred = infer_getter_return_type(member, ivars)
                            inferred || TypeRef::VOID
                          end
            # Collect parameter types for mangling
            method_param_types = [] of TypeRef
            has_block = false
      if params = member.params
        params.each do |param|
          next if named_only_separator?(param)
          if param.is_block
            has_block = true
            next
          end
                param_type = if ta = param.type_annotation
                               type_ref_for_name(String.new(ta))
                             else
                               TypeRef::VOID
                             end
                method_param_types << param_type
              end
            end
            full_name = mangle_function_name(base_name, method_param_types, has_block)
            if ENV.has_key?("DEBUG_NESTED_CLASS") && class_name.includes?("FileDescriptor")
              STDERR.puts "[DEBUG_METHOD_REG] #{class_name}: #{method_name} -> #{full_name} (class_method=#{is_class_method})"
            end
            register_function_type(full_name, return_type)
            @function_defs[full_name] = member
            @function_def_arenas[full_name] = @arena

            # Track yield-functions for inline expansion.
            # Note: MIR lowering removes yield-containing functions (inline-only), so we must inline
            # them at call sites. We key by both base and mangled names so resolution can find them.
            if body = member.body
              if contains_yield?(body)
                @yield_functions.add(full_name)
                unless @function_defs.has_key?(base_name)
                  @function_defs[base_name] = member
                  @function_def_arenas[base_name] = @arena
                end
                @function_defs[full_name] = member
                @function_def_arenas[full_name] = @arena
              end
            end

            # Capture initialize parameters for new()
            # Also extract ivars from shorthand: def initialize(@value : T)
            # Note: Only capture from FIRST initialize (for multiple overloads, each gets its own mangled name)
            if method_name == "initialize" && init_params.empty?
              if params = member.params
                params.each do |param|
                  next if named_only_separator?(param)
                  param_name = param.name.nil? ? "_" : String.new(param.name.not_nil!)
                  param_type = if ta = param.type_annotation
                                 type_ref_for_name(String.new(ta))
                               else
                                 TypeRef::VOID
                               end

                  # Handle instance variable shorthand: @value : T
                  # Note: parser stores name WITHOUT @ prefix when is_instance_var=true
                  if param.is_instance_var
                    ivar_name = "@#{param_name}"  # Add @ prefix for ivar storage
                    # Check if ivar already declared
                    unless ivars.any? { |iv| iv.name == ivar_name }
                      ivars << IVarInfo.new(ivar_name, param_type, offset)
                      offset += type_size(param_type)
                    end
                    # For new() params, use name without @
                    init_params << {param_name, param_type}
                  else
                    init_params << {param_name, param_type}
                  end
                end
              end
            end

          when CrystalV2::Compiler::Frontend::GetterNode
            # Getter declarations: getter name : Type
            # Creates @name ivar and def name; @name; end method
            if member.is_class?
              member.specs.each do |spec|
                register_class_accessor_entry(class_name, spec, :getter)
              end
            else
              specs = member.specs
              specs.each do |spec|
                accessor_name = String.new(spec.name)
                ivar_name = "@#{accessor_name}"
                ivar_type = if ta = spec.type_annotation
                              type_ref_for_name(String.new(ta))
                            else
                              TypeRef::VOID
                            end
                # Register ivar if not already declared
                unless ivars.any? { |iv| iv.name == ivar_name }
                  ivars << IVarInfo.new(ivar_name, ivar_type, offset)
                  offset += type_size(ivar_type)
                end
                # Register getter method: def name : Type
                getter_name = "#{class_name}##{accessor_name}"
                full_name = mangle_function_name(getter_name, [] of TypeRef)
                register_function_type(full_name, ivar_type)
              end
            end

          when CrystalV2::Compiler::Frontend::SetterNode
            # Setter declarations: setter name : Type
            # Creates @name ivar and def name=(value : Type); @name = value; end
            if member.is_class?
              member.specs.each do |spec|
                register_class_accessor_entry(class_name, spec, :setter)
              end
            else
              specs = member.specs
              specs.each do |spec|
                accessor_name = String.new(spec.name)
                ivar_name = "@#{accessor_name}"
                ivar_type = if ta = spec.type_annotation
                              type_ref_for_name(String.new(ta))
                            else
                              TypeRef::VOID
                            end
                # Register ivar if not already declared
                unless ivars.any? { |iv| iv.name == ivar_name }
                  ivars << IVarInfo.new(ivar_name, ivar_type, offset)
                  offset += type_size(ivar_type)
                end
                # Register setter method: def name=(value : Type) : Type
                setter_name = "#{class_name}##{accessor_name}="
                full_name = mangle_function_name(setter_name, [ivar_type])
                register_function_type(full_name, ivar_type)
              end
            end

          when CrystalV2::Compiler::Frontend::PropertyNode
            # Property declarations: property name : Type
            # Creates both getter and setter
            if member.is_class?
              member.specs.each do |spec|
                register_class_accessor_entry(class_name, spec, :getter)
                register_class_accessor_entry(class_name, spec, :setter)
              end
            else
              specs = member.specs
              specs.each do |spec|
                accessor_name = String.new(spec.name)
                ivar_name = "@#{accessor_name}"
                ivar_type = if ta = spec.type_annotation
                              type_ref_for_name(String.new(ta))
                            else
                              TypeRef::VOID
                            end
                # Register ivar if not already declared
                unless ivars.any? { |iv| iv.name == ivar_name }
                  ivars << IVarInfo.new(ivar_name, ivar_type, offset)
                  offset += type_size(ivar_type)
                end
                # Register getter method
                getter_name = "#{class_name}##{accessor_name}"
                getter_full = mangle_function_name(getter_name, [] of TypeRef)
                register_function_type(getter_full, ivar_type)
                # Register setter method
                setter_name = "#{class_name}##{accessor_name}="
                setter_full = mangle_function_name(setter_name, [ivar_type])
                register_function_type(setter_full, ivar_type)
              end
            end

          when CrystalV2::Compiler::Frontend::AssignNode
            # Handle ivar assignments at class body level: @vec = SomeType.new
            target_node = @arena[member.target]
            if target_node.is_a?(CrystalV2::Compiler::Frontend::InstanceVarNode)
              ivar_name = String.new(target_node.name)
              # Infer type from the assigned value
              value_node = @arena[member.value]
              if ENV.has_key?("DEBUG_IVAR_REG")
                STDERR.puts "[IVAR_REG] #{class_name}##{ivar_name} value_node=#{value_node.class}"
              end
              ivar_type = infer_type_from_class_ivar_assign(value_node)
              if ENV.has_key?("DEBUG_IVAR_REG")
                STDERR.puts "[IVAR_REG] #{class_name}##{ivar_name} inferred type=#{ivar_type.id}"
              end
              unless ivars.any? { |iv| iv.name == ivar_name }
                ivars << IVarInfo.new(ivar_name, ivar_type, offset)
                offset += type_size(ivar_type)
              end
            end
          when CrystalV2::Compiler::Frontend::AliasNode
            # Type alias within class: alias Handle = Int32
            alias_name = String.new(member.name)
            target_name = resolve_alias_target(String.new(member.value))
            full_alias_name = "#{class_name}::#{alias_name}"
            register_type_alias(full_alias_name, target_name)
            # Also register short name for local resolution within class
            register_type_alias(alias_name, target_name)
          end
        end

        # Expand module mixins: register included module instance method signatures.
        visited_modules = Set(String).new
        include_nodes.each do |inc|
          offset = register_module_instance_methods_for(
            class_name,
            inc,
            defined_instance_method_full_names,
            visited_modules,
            ivars,
            offset,
            is_struct
          )
        end
        ensure
          @current_class = old_class
        end
      end

      # Create class/struct type (reuse existing type_ref for reopened classes)
      # IMPORTANT: Use well-known TypeRefs for primitive types, not intern_type
      type_kind = is_struct ? TypeKind::Struct : TypeKind::Class
      type_ref = if existing_info
                   existing_info.type_ref
                 else
                   # Check for primitive types - use well-known TypeRefs
                   case class_name
                   when "Bool"    then TypeRef::BOOL
                   when "Int8"    then TypeRef::INT8
                   when "Int16"   then TypeRef::INT16
                   when "Int32"   then TypeRef::INT32
                   when "Int64"   then TypeRef::INT64
                   when "Int128"  then TypeRef::INT128
                   when "UInt8"   then TypeRef::UINT8
                   when "UInt16"  then TypeRef::UINT16
                   when "UInt32"  then TypeRef::UINT32
                   when "UInt64"  then TypeRef::UINT64
                   when "UInt128" then TypeRef::UINT128
                   when "Float32" then TypeRef::FLOAT32
                   when "Float64" then TypeRef::FLOAT64
                   when "Char"    then TypeRef::CHAR
                   when "String"  then TypeRef::STRING
                   when "Symbol"  then TypeRef::SYMBOL
                   when "Nil"     then TypeRef::NIL
                   else
                     # For generic instantiations, reuse type_ref_for_name so the TypeRef id
                     # matches annotations and preserves Hash/Array kinds.
                     if class_name.includes?("(")
                       type_ref_for_name(class_name)
                     else
                       @module.intern_type(TypeDescriptor.new(type_kind, class_name))
                     end
                   end
                 end
      @class_info[class_name] = ClassInfo.new(class_name, type_ref, ivars, class_vars, offset, is_struct, parent_name)

      # Store initialize params for allocator generation
      # If child class has no initialize, inherit from parent
      @init_params ||= {} of String => Array({String, TypeRef})
      if init_params.empty? && parent_name
        if parent_init_params = @init_params.not_nil![parent_name]?
          init_params = parent_init_params
        end
      end
      @init_params.not_nil![class_name] = init_params

      # Register "new" allocator function
      register_function_type("#{class_name}.new", type_ref)
    end

    # Register a struct type and its methods (pass 1)
    # Structs are value types, similar to classes but without inheritance
    def register_struct(node : CrystalV2::Compiler::Frontend::StructNode)
      struct_name = String.new(node.name)
      register_struct_with_name(node, struct_name)
    end

    # Register a struct with a specific name (for nested structs like Foo::Bar)
    def register_struct_with_name(node : CrystalV2::Compiler::Frontend::StructNode, struct_name : String)
      # Collect instance variables and their types
      # Structs have no type_id header (value type), so offset starts at 0
      ivars = [] of IVarInfo
      class_vars = [] of ClassVarInfo
      offset = 0

      # Check if struct already exists (reopening)
      if existing_info = @class_info[struct_name]?
        existing_info.ivars.each { |iv| ivars << iv.dup }
        existing_info.class_vars.each { |cv| class_vars << cv.dup }
        offset = existing_info.size
      end

      # Initialize constructor params
      init_params = [] of {String, TypeRef}

      if body = node.body
        # PASS 0: Register nested types first so method signatures and bodies can resolve them
        body.each do |expr_id|
          member = unwrap_visibility_member(@arena[expr_id])
          case member
          when CrystalV2::Compiler::Frontend::ClassNode
            nested_name = String.new(member.name)
            full_nested_name = "#{struct_name}::#{nested_name}"
            register_class_with_name(member, full_nested_name)
          when CrystalV2::Compiler::Frontend::StructNode
            nested_name = String.new(member.name)
            full_nested_name = "#{struct_name}::#{nested_name}"
            register_struct_with_name(member, full_nested_name)
          when CrystalV2::Compiler::Frontend::EnumNode
            enum_name = String.new(member.name)
            full_enum_name = "#{struct_name}::#{enum_name}"
            register_enum_with_name(member, full_enum_name)
          when CrystalV2::Compiler::Frontend::ModuleNode
            nested_name = String.new(member.name)
            full_nested_name = "#{struct_name}::#{nested_name}"
            register_nested_module(member, full_nested_name)
          end
        end

        defined_instance_method_full_names = collect_defined_instance_method_full_names(struct_name, body)
        include_nodes = [] of CrystalV2::Compiler::Frontend::IncludeNode
        body.each do |expr_id|
          member = unwrap_visibility_member(@arena[expr_id])
          include_nodes << member if member.is_a?(CrystalV2::Compiler::Frontend::IncludeNode)
        end

        body.each do |expr_id|
          member = unwrap_visibility_member(@arena[expr_id])
          case member
          when CrystalV2::Compiler::Frontend::InstanceVarDeclNode
            ivar_name = String.new(member.name)
            ivar_type = type_ref_for_name(String.new(member.type))
            ivars << IVarInfo.new(ivar_name, ivar_type, offset)
            offset += type_size(ivar_type)

          when CrystalV2::Compiler::Frontend::ClassVarDeclNode
            raw_name = String.new(member.name)
            cvar_name = raw_name.lstrip('@')
            cvar_type = type_ref_for_name(String.new(member.type))
            initial_value : Int64? = nil
            if val_id = member.value
              val_node = @arena[val_id]
              if val_node.is_a?(CrystalV2::Compiler::Frontend::NumberNode)
                num_str = String.new(val_node.value)
                initial_value = num_str.to_i64?
              end
            end
            class_vars << ClassVarInfo.new(cvar_name, cvar_type, initial_value)

          when CrystalV2::Compiler::Frontend::DefNode
            method_name = String.new(member.name)
            is_class_method = if recv = member.receiver
                                String.new(recv) == "self"
                              else
                                false
                              end
            base_name = if is_class_method
                          "#{struct_name}.#{method_name}"
                        else
                          "#{struct_name}##{method_name}"
                        end
            param_types = [] of TypeRef
            has_block = false
            if params = member.params
              params.each do |param|
                next if named_only_separator?(param)
                if param.is_block
                  has_block = true
                  next
                end
                if param.is_instance_var
                  param_name = "@#{String.new(param.name.not_nil!)}"
                  existing_ivar = ivars.find { |iv| iv.name == param_name }
                  if existing_ivar
                    param_types << existing_ivar.type
                  elsif ta = param.type_annotation
                    param_types << type_ref_for_name(String.new(ta))
                  else
                    param_types << TypeRef::VOID
                  end
                elsif ta = param.type_annotation
                  param_types << type_ref_for_name(String.new(ta))
                else
                  param_types << TypeRef::VOID
                end
              end
            end
            full_name = mangle_function_name(base_name, param_types, has_block)
            return_type = if rta = member.return_type
                           type_ref_for_name(String.new(rta))
                         else
                           TypeRef::VOID
                         end
            register_function_type(full_name, return_type)
            @function_defs[full_name] = member
            @function_def_arenas[full_name] = @arena

            # Track yield-functions for inline expansion (struct methods).
            # MIR lowering removes yield-containing functions (inline-only), so we must inline them.
            if body = member.body
              if contains_yield?(body)
                @yield_functions.add(full_name)
                unless @function_defs.has_key?(base_name)
                  @function_defs[base_name] = member
                  @function_def_arenas[base_name] = @arena
                end
                @function_defs[full_name] = member
                @function_def_arenas[full_name] = @arena
              end
            end

            if method_name == "initialize"
              if params = member.params
                params.each do |param|
                  next if param.is_block
                  next if named_only_separator?(param)
                  if param.is_instance_var
                    param_name = String.new(param.name.not_nil!)
                    ivar_name = "@#{param_name}"
                    ivar_type = if ta = param.type_annotation
                                  type_ref_for_name(String.new(ta))
                                else
                                  existing_iv = ivars.find { |iv| iv.name == ivar_name }
                                  existing_iv ? existing_iv.type : TypeRef::VOID
                                end
                    unless ivars.any? { |iv| iv.name == ivar_name }
                      ivars << IVarInfo.new(ivar_name, ivar_type, offset)
                      offset += type_size(ivar_type)
                    end
                    init_params << {param_name, ivar_type}
                  elsif ta = param.type_annotation
                    param_name = String.new(param.name.not_nil!)
                    param_type = type_ref_for_name(String.new(ta))
                    init_params << {param_name, param_type}
                  end
                end
              end
            end

          when CrystalV2::Compiler::Frontend::GetterNode
            if member.is_class?
              member.specs.each do |spec|
                register_class_accessor_entry(struct_name, spec, :getter)
              end
            else
              member.specs.each do |spec|
                accessor_name = String.new(spec.name)
                ivar_name = "@#{accessor_name}"
                ivar_type = if ta = spec.type_annotation
                              type_ref_for_name(String.new(ta))
                            else
                              TypeRef::VOID
                            end
                unless ivars.any? { |iv| iv.name == ivar_name }
                  ivars << IVarInfo.new(ivar_name, ivar_type, offset)
                  offset += type_size(ivar_type)
                end
                getter_name = "#{struct_name}##{accessor_name}"
                full_name = mangle_function_name(getter_name, [] of TypeRef)
                register_function_type(full_name, ivar_type)
              end
            end

          when CrystalV2::Compiler::Frontend::SetterNode
            if member.is_class?
              member.specs.each do |spec|
                register_class_accessor_entry(struct_name, spec, :setter)
              end
            else
              member.specs.each do |spec|
                accessor_name = String.new(spec.name)
                ivar_name = "@#{accessor_name}"
                ivar_type = if ta = spec.type_annotation
                              type_ref_for_name(String.new(ta))
                            else
                              TypeRef::VOID
                            end
                unless ivars.any? { |iv| iv.name == ivar_name }
                  ivars << IVarInfo.new(ivar_name, ivar_type, offset)
                  offset += type_size(ivar_type)
                end
                setter_name = "#{struct_name}##{accessor_name}="
                full_name = mangle_function_name(setter_name, [ivar_type])
                register_function_type(full_name, TypeRef::VOID)
              end
            end

          when CrystalV2::Compiler::Frontend::PropertyNode
            if member.is_class?
              member.specs.each do |spec|
                register_class_accessor_entry(struct_name, spec, :getter)
                register_class_accessor_entry(struct_name, spec, :setter)
              end
            else
              member.specs.each do |spec|
                accessor_name = String.new(spec.name)
                ivar_name = "@#{accessor_name}"
                ivar_type = if ta = spec.type_annotation
                              type_ref_for_name(String.new(ta))
                            else
                              TypeRef::VOID
                            end
                unless ivars.any? { |iv| iv.name == ivar_name }
                  ivars << IVarInfo.new(ivar_name, ivar_type, offset)
                  offset += type_size(ivar_type)
                end
                getter_name = "#{struct_name}##{accessor_name}"
                getter_full = mangle_function_name(getter_name, [] of TypeRef)
                register_function_type(getter_full, ivar_type)
                setter_name = "#{struct_name}##{accessor_name}="
                setter_full = mangle_function_name(setter_name, [ivar_type])
                register_function_type(setter_full, TypeRef::VOID)
              end
            end
          end
        end

        # Expand module mixins: register included module instance method signatures.
        visited_modules = Set(String).new
        include_nodes.each do |inc|
          offset = register_module_instance_methods_for(
            struct_name,
            inc,
            defined_instance_method_full_names,
            visited_modules,
            ivars,
            offset,
            true
          )
        end
      end

      # Calculate final size
      size = offset > 0 ? offset : 1
      type_ref = type_ref_for_name(struct_name)

      # Create struct info (is_struct = true)
      @class_info[struct_name] = ClassInfo.new(struct_name, type_ref, ivars, class_vars, size, true, nil)

      @init_params.not_nil![struct_name] = init_params
      register_function_type("#{struct_name}.new", type_ref)
    end

    # Flush all pending monomorphizations (call after all templates are registered)
    def flush_pending_monomorphizations
      @defer_monomorphization = false
      pending = @pending_monomorphizations.dup
      @pending_monomorphizations.clear

      pending.each do |(base_name, type_args, specialized_name)|
        next if @monomorphized.includes?(specialized_name)
        monomorphize_generic_class(base_name, type_args, specialized_name)
      end
    end

    # Monomorphize a generic class: create specialized version with concrete types
    private def monomorphize_generic_class(base_name : String, type_args : Array(String), specialized_name : String)
      template = @generic_templates[base_name]?
      return unless template

      # Double-check inside the method (belt and suspenders)
      if @monomorphized.includes?(specialized_name)
        return
      end

      # Skip if type_args contain unresolved type parameters
      # These indicate incomplete substitution and would cause infinite recursion
      # NOTE: unions like `String | Nil` are concrete and must be allowed here.
      unresolved_token_re = /(?:^|[^A-Za-z0-9_:])(K2|V2|K|V|T|U|L|W)(?:$|[^A-Za-z0-9_:])/
      has_unresolved = type_args.any? do |arg|
        # typeof(...) in type positions is not fully resolved during bootstrap.
        next true if arg.includes?("typeof(")
        # Detect remaining generic placeholders as standalone tokens (avoid false positives like "ValueId").
        arg.matches?(unresolved_token_re)
      end
      if has_unresolved
        if ENV.has_key?("DEBUG_MONO")
          STDERR.puts "[MONO] Skipping unresolved: #{specialized_name}"
        end
        return
      end

      # Mark as monomorphized BEFORE processing to prevent infinite recursion
      # (e.g., Array(String) method calls something that creates Array(String) again)
      @monomorphized.add(specialized_name)

      # Check arity matches
      if template.type_params.size != type_args.size
        raise "Generic #{base_name} expects #{template.type_params.size} type args, got #{type_args.size}"
      end

      # Set up type parameter substitutions: T => Int32, etc.
      old_map = @type_param_map.dup
      template.type_params.each_with_index do |param, i|
        @type_param_map[param] = type_args[i]
      end

      # Switch to the template's arena for AST node lookup
      old_arena = @arena
      @arena = template.arena

      # Register the specialized class using the template's AST node
      # The type_ref_for_name calls will now substitute T => Int32
      register_concrete_class(template.node, specialized_name, template.is_struct)

      # Generate allocator and methods for the specialized class
      if class_info = @class_info[specialized_name]?
        # Set current class for ivar lookup
        old_class = @current_class
        @current_class = specialized_name

        generate_allocator(specialized_name, class_info)

        # Lower methods with type substitutions
        if body = template.node.body
          body.each do |expr_id|
            member = @arena[expr_id]
            case member
            when CrystalV2::Compiler::Frontend::DefNode
              lower_method(specialized_name, class_info, member)
            when CrystalV2::Compiler::Frontend::GetterNode
              # Generate synthetic getter methods
              member.specs.each do |spec|
                generate_getter_method(specialized_name, class_info, spec)
              end
            when CrystalV2::Compiler::Frontend::SetterNode
              # Generate synthetic setter methods
              member.specs.each do |spec|
                generate_setter_method(specialized_name, class_info, spec)
              end
            when CrystalV2::Compiler::Frontend::PropertyNode
              # Generate both getter and setter methods
              member.specs.each do |spec|
                generate_getter_method(specialized_name, class_info, spec)
                generate_setter_method(specialized_name, class_info, spec)
              end
            end
          end
        end

        @current_class = old_class
      end

      # Restore arena and type param map
      @arena = old_arena
      @type_param_map = old_map
      @monomorphized.add(specialized_name)
    end

    # Lower a class and all its methods (pass 3)
    def lower_class(node : CrystalV2::Compiler::Frontend::ClassNode)
      class_name = String.new(node.name)
      lower_class_with_name(node, class_name)
    end

    # Lower a class with a specific name (for nested classes like Foo::Bar)
    def lower_class_with_name(node : CrystalV2::Compiler::Frontend::ClassNode, class_name : String)
      # Skip generic class templates - they're lowered on-demand during monomorphization
      if @generic_templates.has_key?(class_name)
        return
      end

      class_info = @class_info[class_name]? || return
      old_class = @current_class
      @current_class = class_name

      # Generate allocator function: ClassName.new
      generate_allocator(class_name, class_info)

      # Lower each method
      if body = node.body
        # Lower nested types first (classes/structs/modules inside the class body).
        # These can be referenced unqualified from within the class (e.g., `EntryIterator.new` inside `Dir`).
        body.each do |expr_id|
          member = unwrap_visibility_member(@arena[expr_id])
          case member
          when CrystalV2::Compiler::Frontend::ClassNode
            nested_name = String.new(member.name)
            lower_class_with_name(member, "#{class_name}::#{nested_name}")
          when CrystalV2::Compiler::Frontend::StructNode
            nested_name = String.new(member.name)
            lower_struct_with_name(member, "#{class_name}::#{nested_name}")
          when CrystalV2::Compiler::Frontend::ModuleNode
            nested_name = String.new(member.name)
            lower_module_with_name(member, "#{class_name}::#{nested_name}")
          end
        end

        defined_full_names = collect_defined_instance_method_full_names(class_name, body)
        include_nodes = [] of CrystalV2::Compiler::Frontend::IncludeNode

        body.each do |expr_id|
          member = unwrap_visibility_member(@arena[expr_id])
          include_nodes << member if member.is_a?(CrystalV2::Compiler::Frontend::IncludeNode)
        end

        visited_modules = Set(String).new
        include_nodes.each do |inc|
          lower_module_instance_methods_for(class_name, class_info, inc, defined_full_names, visited_modules)
        end

        body.each do |expr_id|
          member = unwrap_visibility_member(@arena[expr_id])
          case member
          when CrystalV2::Compiler::Frontend::IncludeNode
            # Handled above via mixin expansion.
          when CrystalV2::Compiler::Frontend::DefNode
            lower_method(class_name, class_info, member)
          when CrystalV2::Compiler::Frontend::GetterNode
            # Generate synthetic getter methods
            if member.is_class?
              member.specs.each do |spec|
                generate_class_getter_method(class_name, spec, @arena)
              end
            else
              member.specs.each do |spec|
                generate_getter_method(class_name, class_info, spec)
              end
            end
          when CrystalV2::Compiler::Frontend::SetterNode
            # Generate synthetic setter methods
            if member.is_class?
              member.specs.each do |spec|
                generate_class_setter_method(class_name, spec)
              end
            else
              member.specs.each do |spec|
                generate_setter_method(class_name, class_info, spec)
              end
            end
          when CrystalV2::Compiler::Frontend::PropertyNode
            # Generate both getter and setter methods
            if member.is_class?
              member.specs.each do |spec|
                generate_class_getter_method(class_name, spec, @arena)
                generate_class_setter_method(class_name, spec)
              end
            else
              member.specs.each do |spec|
                generate_getter_method(class_name, class_info, spec)
                generate_setter_method(class_name, class_info, spec)
              end
            end
          end
        end
      end

      @current_class = old_class
    end

    # Lower a struct and all its methods (pass 3)
    def lower_struct(node : CrystalV2::Compiler::Frontend::StructNode)
      struct_name = String.new(node.name)
      lower_struct_with_name(node, struct_name)
    end

    # Lower a struct with a specific name (for nested structs like Foo::Bar)
    def lower_struct_with_name(node : CrystalV2::Compiler::Frontend::StructNode, struct_name : String)
      struct_info = @class_info[struct_name]? || return
      old_class = @current_class
      @current_class = struct_name

      # Generate allocator function: StructName.new
      generate_allocator(struct_name, struct_info)

      # Lower each method
      if body = node.body
        # Lower nested types first (classes/structs/modules inside the struct body).
        body.each do |expr_id|
          member = unwrap_visibility_member(@arena[expr_id])
          case member
          when CrystalV2::Compiler::Frontend::ClassNode
            nested_name = String.new(member.name)
            lower_class_with_name(member, "#{struct_name}::#{nested_name}")
          when CrystalV2::Compiler::Frontend::StructNode
            nested_name = String.new(member.name)
            lower_struct_with_name(member, "#{struct_name}::#{nested_name}")
          when CrystalV2::Compiler::Frontend::ModuleNode
            nested_name = String.new(member.name)
            lower_module_with_name(member, "#{struct_name}::#{nested_name}")
          end
        end

        defined_full_names = collect_defined_instance_method_full_names(struct_name, body)
        include_nodes = [] of CrystalV2::Compiler::Frontend::IncludeNode

        body.each do |expr_id|
          member = unwrap_visibility_member(@arena[expr_id])
          include_nodes << member if member.is_a?(CrystalV2::Compiler::Frontend::IncludeNode)
        end

        visited_modules = Set(String).new
        include_nodes.each do |inc|
          lower_module_instance_methods_for(struct_name, struct_info, inc, defined_full_names, visited_modules)
        end

        body.each do |expr_id|
          member = unwrap_visibility_member(@arena[expr_id])
          case member
          when CrystalV2::Compiler::Frontend::IncludeNode
            # Handled above via mixin expansion.
          when CrystalV2::Compiler::Frontend::DefNode
            lower_method(struct_name, struct_info, member)
          when CrystalV2::Compiler::Frontend::GetterNode
            # Generate synthetic getter methods
            if member.is_class?
              member.specs.each do |spec|
                generate_class_getter_method(struct_name, spec, @arena)
              end
            else
              member.specs.each do |spec|
                generate_getter_method(struct_name, struct_info, spec)
              end
            end
          when CrystalV2::Compiler::Frontend::SetterNode
            # Generate synthetic setter methods
            if member.is_class?
              member.specs.each do |spec|
                generate_class_setter_method(struct_name, spec)
              end
            else
              member.specs.each do |spec|
                generate_setter_method(struct_name, struct_info, spec)
              end
            end
          when CrystalV2::Compiler::Frontend::PropertyNode
            # Generate both getter and setter methods
            if member.is_class?
              member.specs.each do |spec|
                generate_class_getter_method(struct_name, spec, @arena)
                generate_class_setter_method(struct_name, spec)
              end
            else
              member.specs.each do |spec|
                generate_getter_method(struct_name, struct_info, spec)
                generate_setter_method(struct_name, struct_info, spec)
              end
            end
          end
        end
      end

      @current_class = old_class
    end

    # Generate allocator: ClassName.new(...) -> allocates and returns instance
    private def generate_allocator(class_name : String, class_info : ClassInfo)
      func_name = "#{class_name}.new"

      # Skip Pointer types - they're primitive types with no allocator
      if class_name.starts_with?("Pointer(") || class_name.starts_with?("Pointer_")
        return
      end

      # Skip primitive number types - they don't need allocators
      # (their .new methods convert from other types, not allocate)
      case class_name
      when "Int8", "Int16", "Int32", "Int64", "Int128",
           "UInt8", "UInt16", "UInt32", "UInt64", "UInt128",
           "Float32", "Float64", "Bool", "Char"
        return
      end

      # Skip if allocator already generated (for reopened classes)
      return if @generated_allocators.includes?(class_name)
      @generated_allocators.add(class_name)

      # Return type is the class type (semantically)
      # LLVM backend converts to ptr for ABI
      func = @module.create_function(func_name, class_info.type_ref)
      ctx = LoweringContext.new(func, @module, @arena)

      # Get initialize parameters for this class
      init_params = @init_params[class_name]? || [] of {String, TypeRef}

      # Add parameters to new() that match initialize()
      param_ids = [] of ValueId
      init_params.each do |param_name, param_type|
        hir_param = func.add_param(param_name, param_type)
        ctx.register_local(param_name, hir_param.id)
        ctx.register_type(hir_param.id, param_type)
        param_ids << hir_param.id
      end

      # Allocate object (struct=stack, class=heap determined by escape analysis)
      alloc = Allocate.new(ctx.next_id, class_info.type_ref, [] of ValueId, class_info.is_struct)
      ctx.emit(alloc)
      ctx.register_type(alloc.id, class_info.type_ref)

      # Initialize instance variables to zero/default
      class_info.ivars.each do |ivar|
        # Check if this is a union type by looking up the type descriptor
        type_desc = @module.get_type_descriptor(ivar.type)
        if type_desc && type_desc.kind == TypeKind::Union
          # Union types will be initialized in initialize()
          # We can't create a simple zero literal for unions
          next
        end

        # Use nil for pointer types, 0 for others
        # Note: Pointer types might be registered as Class kind but with name starting with "Pointer"
        is_pointer = ivar.type == TypeRef::POINTER ||
                     (type_desc && type_desc.kind == TypeKind::Pointer) ||
                     (type_desc && type_desc.name.starts_with?("Pointer"))
        default_value : (Int64 | Nil) = if is_pointer
                                          nil
                                        else
                                          0_i64
                                        end
        default_val = Literal.new(ctx.next_id, ivar.type, default_value)
        ctx.emit(default_val)
        ivar_store = FieldSet.new(ctx.next_id, TypeRef::VOID, alloc.id, ivar.name, default_val.id, ivar.offset)
        ctx.emit(ivar_store)
      end

      # Call initialize if it exists, passing through the parameters
      # Use inheritance-aware resolution to find initialize in parent classes
      init_base_name = resolve_method_with_inheritance(class_name, "initialize")
      if init_base_name
        # Mangle the initialize call with parameter types
        init_param_types = init_params.map { |_, t| t }
        init_name = mangle_function_name(init_base_name, init_param_types)
        lower_function_if_needed(init_name)
        init_call = Call.new(ctx.next_id, TypeRef::VOID, alloc.id, init_name, param_ids)
        ctx.emit(init_call)
      end

      # Return allocated object
      ctx.terminate(Return.new(alloc.id))
    end

    # Generate synthetic getter method: def name; @name; end
    private def generate_getter_method(class_name : String, class_info : ClassInfo, spec : CrystalV2::Compiler::Frontend::AccessorSpec)
      accessor_name = String.new(spec.name)
      ivar_name = "@#{accessor_name}"
      ivar_info = class_info.ivars.find { |iv| iv.name == ivar_name }
      return unless ivar_info

      generate_getter_method_for_ivar(class_name, class_info, ivar_info)
    end

    # Generate synthetic setter method: def name=(value); @name = value; end
    private def generate_setter_method(class_name : String, class_info : ClassInfo, spec : CrystalV2::Compiler::Frontend::AccessorSpec)
      accessor_name = String.new(spec.name)
      ivar_name = "@#{accessor_name}"
      ivar_info = class_info.ivars.find { |iv| iv.name == ivar_name }
      return unless ivar_info

      generate_setter_method_for_ivar(class_name, class_info, ivar_info)
    end

    private def generate_getter_method_for_ivar(class_name : String, class_info : ClassInfo, ivar_info : IVarInfo)
      accessor_name = ivar_info.name.lstrip('@')
      ivar_type = ivar_info.type

      base_name = "#{class_name}##{accessor_name}"
      func_name = mangle_function_name(base_name, [] of TypeRef)
      return if @module.has_function?(func_name)

      func = @module.create_function(func_name, ivar_type)
      ctx = LoweringContext.new(func, @module, @arena)

      self_param = func.add_param("self", class_info.type_ref)
      ctx.register_local("self", self_param.id)
      ctx.register_type(self_param.id, class_info.type_ref)

      field_get = FieldGet.new(ctx.next_id, ivar_type, self_param.id, ivar_info.name, ivar_info.offset)
      ctx.emit(field_get)
      ctx.register_type(field_get.id, ivar_type)

      ctx.terminate(Return.new(field_get.id))
    end

    private def generate_setter_method_for_ivar(class_name : String, class_info : ClassInfo, ivar_info : IVarInfo)
      accessor_name = ivar_info.name.lstrip('@')
      ivar_type = ivar_info.type

      base_name = "#{class_name}##{accessor_name}="
      func_name = mangle_function_name(base_name, [ivar_type])
      return if @module.has_function?(func_name)

      func = @module.create_function(func_name, ivar_type)
      ctx = LoweringContext.new(func, @module, @arena)

      self_param = func.add_param("self", class_info.type_ref)
      ctx.register_local("self", self_param.id)
      ctx.register_type(self_param.id, class_info.type_ref)

      value_param = func.add_param("value", ivar_type)
      ctx.register_local("value", value_param.id)
      ctx.register_type(value_param.id, ivar_type)

      field_set = FieldSet.new(ctx.next_id, TypeRef::VOID, self_param.id, ivar_info.name, value_param.id, ivar_info.offset)
      ctx.emit(field_set)

      ctx.terminate(Return.new(value_param.id))
    end

    private def generate_class_getter_method(owner_name : String, spec : CrystalV2::Compiler::Frontend::AccessorSpec, arena : CrystalV2::Compiler::Frontend::ArenaLike)
      accessor_name = String.new(spec.name)
      old_class = @current_class
      old_method = @current_method
      old_method_is_class = @current_method_is_class

      base_name = "#{owner_name}.#{accessor_name}"
      full_name = mangle_function_name(base_name, [] of TypeRef)
      return if @module.has_function?(full_name)

      @current_class = owner_name
      @current_method = accessor_name
      @current_method_is_class = true

      return_type = begin
        if ta = spec.type_annotation
          type_ref_for_name(String.new(ta))
        elsif accessor_name.ends_with?("?")
          TypeRef::BOOL
        elsif default_value = spec.default_value
          inferred = infer_type_from_expr(default_value, owner_name)
          inferred || TypeRef::VOID
        else
          TypeRef::VOID
        end
      end

      func = @module.create_function(full_name, return_type)
      ctx = LoweringContext.new(func, @module, arena)

      if default_value = spec.default_value
        flag_name = class_accessor_init_flag_name(accessor_name)
        flag_get = ClassVarGet.new(ctx.next_id, TypeRef::BOOL, owner_name, flag_name)
        ctx.emit(flag_get)

        then_block = ctx.create_block
        else_block = ctx.create_block
        merge_block = ctx.create_block
        ctx.terminate(Branch.new(flag_get.id, then_block, else_block))

        ctx.switch_to_block(then_block)
        cached_get = ClassVarGet.new(ctx.next_id, return_type, owner_name, accessor_name)
        ctx.emit(cached_get)
        ctx.terminate(Jump.new(merge_block))

        ctx.switch_to_block(else_block)
        init_value = with_arena(arena) { lower_accessor_default_value(ctx, default_value) }
        class_var_set = ClassVarSet.new(ctx.next_id, return_type, owner_name, accessor_name, init_value)
        ctx.emit(class_var_set)
        flag_lit = Literal.new(ctx.next_id, TypeRef::BOOL, true)
        ctx.emit(flag_lit)
        flag_set = ClassVarSet.new(ctx.next_id, TypeRef::BOOL, owner_name, flag_name, flag_lit.id)
        ctx.emit(flag_set)
        ctx.terminate(Jump.new(merge_block))

        ctx.switch_to_block(merge_block)
        phi = Phi.new(ctx.next_id, return_type)
        phi.add_incoming(then_block, cached_get.id)
        phi.add_incoming(else_block, init_value)
        ctx.emit(phi)
        ctx.terminate(Return.new(phi.id))
      else
        value_id = ClassVarGet.new(ctx.next_id, return_type, owner_name, accessor_name)
        ctx.emit(value_id)
        ctx.terminate(Return.new(value_id.id))
      end
    ensure
      @current_class = old_class
      @current_method = old_method
      @current_method_is_class = old_method_is_class || false
    end

    private def generate_class_setter_method(owner_name : String, spec : CrystalV2::Compiler::Frontend::AccessorSpec)
      accessor_name = String.new(spec.name)
      param_type = if ta = spec.type_annotation
                     type_ref_for_name(String.new(ta))
                   else
                     TypeRef::VOID
                   end
      base_name = "#{owner_name}.#{accessor_name}="
      full_name = mangle_function_name(base_name, [param_type])
      return if @module.has_function?(full_name)

      func = @module.create_function(full_name, param_type)
      ctx = LoweringContext.new(func, @module, @arena)
      value_param = func.add_param("value", param_type)
      ctx.register_local("value", value_param.id)
      ctx.register_type(value_param.id, param_type)

      class_var_set = ClassVarSet.new(ctx.next_id, param_type, owner_name, accessor_name, value_param.id)
      ctx.emit(class_var_set)
      flag_name = class_accessor_init_flag_name(accessor_name)
      flag_lit = Literal.new(ctx.next_id, TypeRef::BOOL, true)
      ctx.emit(flag_lit)
      flag_set = ClassVarSet.new(ctx.next_id, TypeRef::BOOL, owner_name, flag_name, flag_lit.id)
      ctx.emit(flag_set)
      ctx.terminate(Return.new(value_param.id))
    end

    private def class_accessor_init_flag_name(accessor_name : String) : String
      "__class_accessor_init_#{accessor_name}"
    end

    private def lower_accessor_default_value(ctx : LoweringContext, default_value : CrystalV2::Compiler::Frontend::ExprId) : ValueId
      default_node = @arena[default_value]
      case default_node
      when CrystalV2::Compiler::Frontend::BlockNode
        if default_node.body.empty?
          nil_value(ctx)
        else
          lower_body(ctx, default_node.body)
        end
      else
        lower_expr(ctx, default_value)
      end
    end

    private def default_literal_for_type(ctx : LoweringContext, type_ref : TypeRef) : ValueId
      case type_ref
      when TypeRef::BOOL
        lit = Literal.new(ctx.next_id, type_ref, false)
        ctx.emit(lit)
        lit.id
      when TypeRef::INT8, TypeRef::INT16, TypeRef::INT32, TypeRef::INT64, TypeRef::INT128,
           TypeRef::UINT8, TypeRef::UINT16, TypeRef::UINT32, TypeRef::UINT64, TypeRef::UINT128
        lit = Literal.new(ctx.next_id, type_ref, 0_i64)
        ctx.emit(lit)
        lit.id
      when TypeRef::FLOAT32, TypeRef::FLOAT64
        lit = Literal.new(ctx.next_id, type_ref, 0.0_f64)
        ctx.emit(lit)
        lit.id
      when TypeRef::CHAR
        lit = Literal.new(ctx.next_id, type_ref, '\0')
        ctx.emit(lit)
        lit.id
      when TypeRef::NIL
        nil_value(ctx)
      else
        type_desc = @module.get_type_descriptor(type_ref)
        if type_desc && type_desc.kind == TypeKind::Struct
          alloc = Allocate.new(ctx.next_id, type_ref, [] of ValueId, true)
          ctx.emit(alloc)
          alloc.id
        else
          lit = Literal.new(ctx.next_id, type_ref, nil)
          ctx.emit(lit)
          lit.id
        end
      end
    end

    # Lower a method within a class
    private def lower_method(
      class_name : String,
      class_info : ClassInfo,
      node : CrystalV2::Compiler::Frontend::DefNode,
      call_arg_types : Array(TypeRef)? = nil,
      full_name_override : String? = nil
    )
      method_name = String.new(node.name)

      # Check if this is a class method (def self.method_name)
      is_class_method = if recv = node.receiver
                          String.new(recv) == "self"
                        else
                          false
                        end

      if ENV.has_key?("DEBUG_NESTED_CLASS") && (class_name.includes?("FileDescriptor") && method_name.includes?("from_stdio"))
        sep = is_class_method ? "." : "#"
        STDERR.puts "[DEBUG_LOWER_METHOD] #{class_name}#{sep}#{method_name} (class_method=#{is_class_method}, receiver=#{node.receiver})"
      end

      # Class methods use "." separator, instance methods use "#"
      base_name = if is_class_method
                    "#{class_name}.#{method_name}"
                  else
                    "#{class_name}##{method_name}"
                  end

      # Skip abstract methods - they have no implementation
      if node.is_abstract
        return
      end

      # Skip methods for Pointer types - they're all primitives handled at call sites
      if class_name.starts_with?("Pointer(") || class_name.starts_with?("Pointer_")
        return
      end

      # Track current method for super calls
      old_method = @current_method
      old_method_is_class = @current_method_is_class
      @current_method = method_name
      @current_method_is_class = is_class_method

      # Collect parameter types first for name mangling
      param_infos = [] of Tuple(String, TypeRef, Bool)  # (name, type, is_instance_var)
      param_types = [] of TypeRef
      has_block = false
      param_type_map = {} of String => TypeRef
      old_typeof_locals = @current_typeof_locals
      old_typeof_local_names = @current_typeof_local_names
      @current_typeof_locals = param_type_map
      @current_typeof_local_names = {} of String => String
      call_types = call_arg_types || [] of TypeRef
      call_index = 0
      splat_param_info_index : Int32? = nil
      splat_param_types_index : Int32? = nil
      splat_param_name : String? = nil
      splat_param_has_annotation = false

      if params = node.params
        params.each do |param|
          next if named_only_separator?(param)
          param_name = param.name.nil? ? "_" : String.new(param.name.not_nil!)
          param_type = if ta = param.type_annotation
                         type_ref_for_name(String.new(ta))
                       else
                         TypeRef::VOID
                       end
          if param_type == TypeRef::VOID && !param.is_block && !param.is_splat && !param.is_double_splat
            if call_index < call_types.size
              inferred = call_types[call_index]
              param_type = inferred if inferred != TypeRef::VOID
            end
          end
          if !param.is_block && !param.is_splat && !param.is_double_splat && call_index < call_types.size
            param_type = refine_param_type_from_call(param_type, call_types[call_index])
          end
          param_type_map[param_name] = param_type
          param_infos << {param_name, param_type, param.is_instance_var}
          if ta = param.type_annotation
            update_typeof_local_name(param_name, String.new(ta))
          end
          if param.is_block
            has_block = true
          else
            if param.is_splat || param.is_double_splat
              splat_param_info_index = param_infos.size - 1
              splat_param_types_index = param_types.size
              splat_param_name = param_name
              splat_param_has_annotation = !param.type_annotation.nil?
            else
              call_index += 1
            end
            param_types << param_type
          end
        end
      end

      if splat_param_name && !call_types.empty? && !splat_param_has_annotation
        remaining = call_types[call_index..-1]? || [] of TypeRef
        splat_type = if remaining.size == 1
                       remaining[0]
                     else
                       tuple_type_from_arg_types(remaining)
                     end
        if splat_type != TypeRef::VOID
          param_type_map[splat_param_name.not_nil!] = splat_type
          if idx = splat_param_info_index
            param_infos[idx] = {splat_param_name.not_nil!, splat_type, param_infos[idx][2]}
          end
          if idx = splat_param_types_index
            param_types[idx] = splat_type
          end
        end
      end

      return_type = if rt = node.return_type
                      rt_name = String.new(rt)
                      # "self" in return type means "the current class type"
                      if rt_name == "self"
                        class_info.type_ref
                      elsif module_like_type_name?(rt_name)
                        inferred = infer_concrete_return_type_from_body(node, class_name)
                        inferred || type_ref_for_name(rt_name)
                      else
                        type_ref_for_name(rt_name)
                      end
                    elsif method_name.ends_with?("?")
                      infer_unannotated_query_return_type(method_name, class_info.type_ref) || TypeRef::BOOL
                    else
                      # Try to infer return type from getter-style methods (single ivar access)
                      inferred = infer_getter_return_type(node, class_info.ivars)
                      inferred || TypeRef::VOID
                    end

      # Mangle function name with parameter types for overloading
      full_name = full_name_override || mangle_function_name(base_name, param_types, has_block)

      func = @module.create_function(full_name, return_type)
      ctx = LoweringContext.new(func, @module, @arena)

      # Add implicit 'self' parameter first
      # For primitive types (Int32, Bool, etc.), use primitive TypeRef so LLVM passes by value
      # For structs with fields, use class_info.type_ref (passed as pointer)
      self_type = primitive_self_type(class_name) || class_info.type_ref
      self_param = func.add_param("self", self_type)
      ctx.register_local("self", self_param.id)
      ctx.register_type(self_param.id, self_type)

      # Lower explicit parameters
      # Track @param style for auto-assignment
      auto_assign_params = [] of Tuple(String, ValueId, Int32)  # (ivar_name, param_value_id, offset)

      param_infos.each do |(param_name, param_type, is_instance_var)|
        # If a method parameter references a concrete generic instantiation (e.g., `Hash(String, ValueId)`),
        # ensure it is monomorphized before lowering the method body so calls on the value can resolve.
        ensure_monomorphized_type(param_type) unless param_type == TypeRef::VOID

        hir_param = func.add_param(param_name, param_type)
        ctx.register_local(param_name, hir_param.id)
        ctx.register_type(hir_param.id, param_type)

        # Check for @param syntax (auto-assignment to ivar via is_instance_var flag)
        if is_instance_var
          ivar_name = "@#{param_name}"  # Add @ prefix for ivar lookup
          ivar_offset = get_ivar_offset(ivar_name)
          auto_assign_params << {ivar_name, hir_param.id, ivar_offset}
        end
      end

      # Emit auto-assignments for @param style parameters
      auto_assign_params.each do |(ivar_name, param_id, offset)|
        self_id = emit_self(ctx)
        param_type = ctx.type_of(param_id)
        field_set = FieldSet.new(ctx.next_id, TypeRef::VOID, self_id, ivar_name, param_id, offset)
        ctx.emit(field_set)
      end

      # Lower body
      last_value : ValueId? = nil
      if body = node.body
        body.each do |expr_id|
          last_value = lower_expr(ctx, expr_id)
        end
      end

      # Infer return type from the last expression for unannotated methods.
      # This is a pragmatic bootstrap improvement for stdlib-style combinators and nilable query methods
      # that often omit return annotations (e.g., `Hash#[]?`, `Array#first?`).
      if node.return_type.nil? && last_value
        inferred = ctx.type_of(last_value)
        if inferred != TypeRef::VOID && inferred != return_type
          return_type = inferred
          func.return_type = inferred
        end
      end

      # Ensure the return type map matches the lowered function.
      register_function_type(full_name, return_type)

      # Add implicit return if not already terminated
      # BUT don't add return after raise (which sets Unreachable terminator)
      block = ctx.get_block(ctx.current_block)
      block_has_raise = block.instructions.any? { |inst| inst.is_a?(Raise) }
      if block.terminator.is_a?(Unreachable) && !block_has_raise
        block.terminator = Return.new(last_value)
      end

      @current_typeof_locals = old_typeof_locals
      @current_typeof_local_names = old_typeof_local_names

      # Restore previous method context
      @current_method = old_method
      @current_method_is_class = old_method_is_class || false
    end

    # Get primitive TypeRef for self if class_name is a primitive type
    # Returns nil if class_name is not a primitive (use class_info.type_ref instead)
    private def primitive_self_type(class_name : String) : TypeRef?
      case class_name
      when "Bool"    then TypeRef::BOOL
      when "Int8"    then TypeRef::INT8
      when "Int16"   then TypeRef::INT16
      when "Int32"   then TypeRef::INT32
      when "Int64"   then TypeRef::INT64
      when "Int128"  then TypeRef::INT128
      when "UInt8"   then TypeRef::UINT8
      when "UInt16"  then TypeRef::UINT16
      when "UInt32"  then TypeRef::UINT32
      when "UInt64"  then TypeRef::UINT64
      when "UInt128" then TypeRef::UINT128
      when "Float32" then TypeRef::FLOAT32
      when "Float64" then TypeRef::FLOAT64
      when "Char"    then TypeRef::CHAR
      else                nil
      end
    end

    # Get type name for mangling (converts TypeRef to short string)
    private def type_name_for_mangling(type : TypeRef) : String
      case type
      when TypeRef::VOID    then "Void"
      when TypeRef::NIL     then "Nil"
      when TypeRef::BOOL    then "Bool"
      when TypeRef::INT8    then "Int8"
      when TypeRef::INT16   then "Int16"
      when TypeRef::INT32   then "Int32"
      when TypeRef::INT64   then "Int64"
      when TypeRef::INT128  then "Int128"
      when TypeRef::UINT8   then "UInt8"
      when TypeRef::UINT16  then "UInt16"
      when TypeRef::UINT32  then "UInt32"
      when TypeRef::UINT64  then "UInt64"
      when TypeRef::UINT128 then "UInt128"
      when TypeRef::FLOAT32 then "Float32"
      when TypeRef::FLOAT64 then "Float64"
      when TypeRef::CHAR    then "Char"
      when TypeRef::STRING  then "String"
      when TypeRef::SYMBOL  then "Symbol"
      when TypeRef::POINTER then "Pointer"
      else
        # User-defined type - look up name from module's type descriptors
        if desc = @module.get_type_descriptor(type)
          desc.name
        else
          "T#{type.id}"  # Fallback to type ID
        end
      end
    end

    private def tuple_type_from_arg_types(arg_types : Array(TypeRef)) : TypeRef
      return TypeRef::VOID if arg_types.empty?
      return TypeRef::VOID if arg_types.any? { |t| t == TypeRef::VOID }

      parts = arg_types.map { |t| type_name_for_mangling(t) }
      type_ref_for_name("Tuple(#{parts.join(", ")})")
    end

    private def refine_param_type_from_call(param_type : TypeRef, call_type : TypeRef) : TypeRef
      return param_type if call_type == TypeRef::VOID
      return param_type if param_type == call_type

      param_desc = @module.get_type_descriptor(param_type)
      call_desc = @module.get_type_descriptor(call_type)
      return param_type unless param_desc && call_desc

      param_name = param_desc.name
      return param_type if param_name.includes?("(")

      call_base = call_desc.name.split("(", 2).first
      return call_type if call_base == param_name && call_desc.name.includes?("(")

      param_type
    end

    # Map operator method name to BinaryOp for primitive inlining
    # Returns nil if the method is not a binary operator
    private def binary_op_for_method(method_name : String) : BinaryOp?
      case method_name
      when "+"   then BinaryOp::Add
      when "-"   then BinaryOp::Sub
      when "*"   then BinaryOp::Mul
      when "/"   then BinaryOp::Div
      when "%"   then BinaryOp::Mod
      when "&"   then BinaryOp::BitAnd
      when "|"   then BinaryOp::BitOr
      when "^"   then BinaryOp::BitXor
      when "<<"  then BinaryOp::Shl
      when ">>"  then BinaryOp::Shr
      when "=="  then BinaryOp::Eq
      when "!="  then BinaryOp::Ne
      when "<"   then BinaryOp::Lt
      when "<="  then BinaryOp::Le
      when ">"   then BinaryOp::Gt
      when ">="  then BinaryOp::Ge
      when "&&"  then BinaryOp::And
      when "||"  then BinaryOp::Or
      # Wrapping operators - map to same ops (LLVM integer ops already wrap)
      when "&+"  then BinaryOp::Add
      when "&-"  then BinaryOp::Sub
      when "&*"  then BinaryOp::Mul
      else            nil
      end
    end

    # Check if a TypeRef is a numeric primitive type (supports binary ops)
    private def numeric_primitive?(type : TypeRef) : Bool
      case type
      when TypeRef::INT8, TypeRef::INT16, TypeRef::INT32, TypeRef::INT64, TypeRef::INT128,
           TypeRef::UINT8, TypeRef::UINT16, TypeRef::UINT32, TypeRef::UINT64, TypeRef::UINT128,
           TypeRef::FLOAT32, TypeRef::FLOAT64, TypeRef::CHAR
        true
      else
        false
      end
    end

    # Extract element type from Pointer(T) class name or method name
    # "Pointer(Int32)" or "Pointer(Int32).malloc" -> TypeRef::INT32
    private def pointer_element_type(class_name : String) : TypeRef
      # Extract type argument from "Pointer(T)" format (may have .method suffix)
      if match = class_name.match(/^Pointer\(([^)]+)\)/)
        type_arg = match[1]
        case type_arg
        when "Int8"    then TypeRef::INT8
        when "Int16"   then TypeRef::INT16
        when "Int32"   then TypeRef::INT32
        when "Int64"   then TypeRef::INT64
        when "Int128"  then TypeRef::INT128
        when "UInt8"   then TypeRef::UINT8
        when "UInt16"  then TypeRef::UINT16
        when "UInt32"  then TypeRef::UINT32
        when "UInt64"  then TypeRef::UINT64
        when "UInt128" then TypeRef::UINT128
        when "Float32" then TypeRef::FLOAT32
        when "Float64" then TypeRef::FLOAT64
        when "Bool"    then TypeRef::BOOL
        when "Char"    then TypeRef::CHAR
        when "Pointer" then TypeRef::POINTER
        else
          # User-defined type - look it up in class_info
          if info = @class_info[type_arg]?
            info.type_ref
          else
            TypeRef::VOID  # Unknown type
          end
        end
      else
        TypeRef::VOID
      end
    end

    # Mangle function name with parameter types for overloading.
    #
    # Examples:
    # - "IO.print" + [String] -> "IO.print$String"
    # - "Int32#+ " + [Int32] -> "Int32#+$Int32"
    # - "Int32#downto" + [] + has_block=true -> "Int32#downto$block"
    #
    # Note: Using $ instead of : because LLVM doesn't support : in identifiers.
    private def mangle_function_name(base_name : String, param_types : Array(TypeRef), has_block : Bool = false) : String
      # Filter out VOID types (untyped parameters don't provide overload info)
      typed_params = param_types.reject { |t| t == TypeRef::VOID }

      suffix = typed_params.map { |t| type_name_for_mangling(t) }.join("_")
      if has_block
        suffix = suffix.empty? ? "block" : "#{suffix}_block"
      end

      suffix.empty? ? base_name : "#{base_name}$#{suffix}"
    end

    # Resolve method call for a receiver type and method name
    # Returns the properly mangled method name that should be used in the Call node
    private def resolve_method_call(ctx : LoweringContext, receiver_id : ValueId, method_name : String, arg_types : Array(TypeRef)) : String
      receiver_type = ctx.type_of(receiver_id)
      type_desc = @module.get_type_descriptor(receiver_type)

      # Get the class name from the type descriptor
      class_name = type_desc.try(&.name) || ""

      # Build the base method name as ClassName#method
      base_method_name = class_name.empty? ? method_name : "#{class_name}##{method_name}"

      # Mangle with argument types
      mangled_name = mangle_function_name(base_method_name, arg_types)

      # Try to find the function in the module.
      #
      # IMPORTANT: Don't use return type as an existence check.
      # Many valid methods return `Nil` (VOID) in our IR, and treating VOID as "missing"
      # causes qualified calls to degrade into unqualified extern calls.
      if @function_types.has_key?(mangled_name)
        return mangled_name
      end

      # If any overload exists for the base name, return the base name and let
      # the MIR lowering do fuzzy matching to pick a concrete overload.
      if !class_name.empty? && has_function_base?(base_method_name)
        return base_method_name
      end

      # If the receiver is module-like (e.g., Iterator(T)), resolve only when a unique includer
      # matches the call signature. Avoid heuristic picks that can point to wrong types.
      if !class_name.empty? && module_like_type_name?(class_name)
        if resolved = resolve_module_typed_method(method_name, arg_types, class_name, false, @current_class)
          return resolved
        end
      end

      # Search through all class info for matching method (O(n) fallback).
      # Only do this when the receiver type is unknown (no descriptor name).
      if class_name.empty?
        @class_info.each do |klass_name, info|
          test_base = "#{klass_name}##{method_name}"
          test_mangled = mangle_function_name(test_base, arg_types)
          if @function_types.has_key?(test_mangled)
            return test_mangled
          elsif has_function_base?(test_base)
            return test_base
          end
        end
      end

      # For special operators like <<, try common receivers with relaxed type matching
      if method_name == "<<" && class_name.empty?
        # Try Array(T)#<< with various element types
        ["String", "Int32", "Pointer"].each do |elem_type|
          array_class = "Array(#{elem_type})"
          test_base = "#{array_class}#<<"
          test_mangled = mangle_function_name(test_base, arg_types)
          if @function_types[test_mangled]?
            return test_mangled
          end
          # Also try without arg types mangling
          @function_types.each_key do |key|
            if key.starts_with?("#{test_base}$")
              return key
            end
          end
        end
        # Try IO#<<
        ["IO", "IO::Memory", "String::Builder"].each do |io_class|
          test_base = "#{io_class}#<<"
          @function_types.each_key do |key|
            if key.starts_with?("#{test_base}$") || key == test_base
              return key
            end
          end
        end
      end

      # Fallback to mangled name
      mangled_name
    end

    private def module_receiver_type_name(node : CrystalV2::Compiler::Frontend::MemberAccessNode) : String?
      obj_node = @arena[node.object]
      case obj_node
      when CrystalV2::Compiler::Frontend::IdentifierNode
        name = String.new(obj_node.name)
        if type_name = lookup_typeof_local_name(name)
          return type_name if module_like_type_name?(type_name)
        end
      end
      nil
    end

    private def resolve_module_typed_method(
      method_name : String,
      arg_types : Array(TypeRef),
      module_type_name : String,
      has_block : Bool,
      prefer_class : String?
    ) : String?
      module_base = if paren = module_type_name.index('(')
                      module_type_name[0, paren]
                    else
                      module_type_name
                    end
      includers = @module_includers[module_base]?
      return nil unless includers && !includers.empty?

      candidates = includers.to_a
      if prefer_class
        candidates.delete(prefer_class)
        candidates.sort!
        candidates.unshift(prefer_class)
      else
        candidates.sort!
      end

      matches = [] of String
      arg_count = arg_types.size
      candidates.each do |candidate|
        base = "#{candidate}##{method_name}"
        # Filter by arity/defaults first to avoid picking mismatched overloads.
        if entry = lookup_function_def_for_call(base, arg_count, has_block)
          def_node = entry[1]
          next unless params_compatible_with_args?(def_node, arg_types)
          mangled = mangle_function_name(base, arg_types, has_block)
          if @function_types.has_key?(mangled) || @module.has_function?(mangled)
            matches << mangled
          elsif has_function_base?(base)
            matches << base
          end
        end
      end

      return matches.first if matches.size == 1
      if prefer_class
        preferred = matches.find { |name| name.starts_with?("#{prefer_class}#") }
        return preferred if preferred
      end
      nil
    end

    private def params_compatible_with_args?(
      def_node : CrystalV2::Compiler::Frontend::DefNode,
      arg_types : Array(TypeRef)
    ) : Bool
      params = def_node.params
      return true unless params

      arg_idx = 0
      params.each do |param|
        next if param.is_block
        next if named_only_separator?(param)
        if param.is_splat || param.is_double_splat
          return true
        end
        break if arg_idx >= arg_types.size

        param_type_name = param.type_annotation ? String.new(param.type_annotation.not_nil!) : ""
        param_type = if param_type_name.empty?
                       TypeRef::VOID
                     else
                       type_ref_for_name(param_type_name)
                     end
        arg_type = arg_types[arg_idx]

        if !param_type_name.empty?
          resolved_name = resolve_type_alias_chain(param_type_name)
          if module_like_type_name?(resolved_name)
            return false if primitive_type?(arg_type)
            arg_idx += 1
            next
          end
        end

        if param_type != TypeRef::VOID && arg_type != TypeRef::VOID
          if param_type != arg_type && !needs_union_coercion?(arg_type, param_type)
            return false
          end
        end

        arg_idx += 1
      end

      true
    end

    private def primitive_type?(type : TypeRef) : Bool
      case type
      when TypeRef::VOID, TypeRef::NIL, TypeRef::BOOL, TypeRef::CHAR,
           TypeRef::INT8, TypeRef::INT16, TypeRef::INT32, TypeRef::INT64, TypeRef::INT128,
           TypeRef::UINT8, TypeRef::UINT16, TypeRef::UINT32, TypeRef::UINT64, TypeRef::UINT128,
           TypeRef::FLOAT32, TypeRef::FLOAT64
        true
      else
        false
      end
    end

    # Fallback for yield-function inlining when receiver type is unknown (often due to untyped params).
    # Tries to find a unique yield method by name + arity.
    private def find_yield_method_fallback(method_name : String, arg_count : Int32) : String?
      instance_suffix = "##{method_name}"
      class_suffix = ".#{method_name}"

      candidates = [] of String
      seen_defs = Set(UInt64).new
      @yield_functions.each do |name|
        base = name.split("$").first
        next unless base.ends_with?(instance_suffix) || base.ends_with?(class_suffix)
        func_def = @function_defs[name]?
        func_def ||= @function_defs[base]?
        next unless func_def
        def_id = func_def.object_id
        next if seen_defs.includes?(def_id)
        non_block_params = if params = func_def.params
                             params.count { |p| !p.is_block && !named_only_separator?(p) }
                           else
                             0
                           end
        next unless non_block_params == arg_count
        candidates << name
        seen_defs << def_id
      end

      return candidates.first if candidates.size == 1

      # Multiple candidates - prefer Object methods (base class) over more specialized ones
      # This handles cases like Object#tap vs Iterator#tap
      if candidates.size > 1
        object_candidate = candidates.find { |c| c.starts_with?("Object#") }
        return object_candidate if object_candidate
        # Fallback: return first candidate anyway (better than nothing)
        return candidates.first
      end
      nil
    end

    # Infer return type for getter-style methods (single ivar access in body)
    # Returns the ivar type if the method body is just "@x", nil otherwise
    private def infer_getter_return_type(node : CrystalV2::Compiler::Frontend::DefNode, ivars : Array(IVarInfo)) : TypeRef?
      body = node.body
      return nil unless body && body.size == 1

      # Single expression in body - check if it's an ivar access
      body_node = @arena[body[0]]
      case body_node
      when CrystalV2::Compiler::Frontend::InstanceVarNode
        # Body is just "@x" - find the ivar type
        ivar_name = String.new(body_node.name)
        ivar_info = ivars.find { |iv| iv.name == ivar_name }
        return ivar_info.type if ivar_info && ivar_info.type != TypeRef::VOID
      end
      nil
    end

    # Helper to get type size in bytes
    private def type_size(type : TypeRef) : Int32
      case type
      when TypeRef::BOOL, TypeRef::INT8, TypeRef::UINT8
        1
      when TypeRef::INT16, TypeRef::UINT16
        2
      when TypeRef::INT32, TypeRef::UINT32, TypeRef::FLOAT32, TypeRef::CHAR
        4
      when TypeRef::INT64, TypeRef::UINT64, TypeRef::FLOAT64
        8
      when TypeRef::INT128, TypeRef::UINT128
        16
      when TypeRef::VOID, TypeRef::NIL
        0  # Nil/Void has no storage size
      else
        # Check if it's a union type we've registered
        mir_type_ref = MIR::TypeRef.new(type.id)
        if descriptor = @union_descriptors[mir_type_ref]?
          descriptor.total_size
        else
          8  # Pointer size for reference types
        end
      end
    end

    # Register a function signature (for forward reference support)
    # Call this for all functions before lowering any function bodies
    def register_function(node : CrystalV2::Compiler::Frontend::DefNode)
      base_name = String.new(node.name)
      if base_name == "main" && @current_class.nil? && !fun_def?(node)
        base_name = TOP_LEVEL_MAIN_BASE
        @top_level_main_defined = true
      end
      param_types = [] of TypeRef
      param_type_map = {} of String => TypeRef
      has_block = false

      old_typeof_locals = @current_typeof_locals
      @current_typeof_locals = param_type_map
      if params = node.params
        params.each do |param|
          next if named_only_separator?(param)
          if param.is_block
            has_block = true
            next
          end
          param_name = param.name.nil? ? "_" : String.new(param.name.not_nil!)
          param_type = if ta = param.type_annotation
                         type_ref_for_name(String.new(ta))
                       else
                         TypeRef::VOID
                       end
          param_type_map[param_name] = param_type
          param_types << param_type
        end
      end

      return_type = if rt = node.return_type
                      type_ref_for_name(String.new(rt))
                    else
                      TypeRef::VOID
                    end
      @current_typeof_locals = old_typeof_locals

      # Register with mangled name
      full_name = mangle_function_name(base_name, param_types, has_block)
      register_function_type(full_name, return_type)

      # Also register with base name for fallback lookup
      # (when function is not overloaded, we look up by base name)
      register_function_type(base_name, return_type)

      # Store AST for potential inline expansion (use mangled name)
      @function_defs[full_name] = node
      @function_def_arenas[full_name] = @arena

      # Check if function contains yield
      if body = node.body
        if contains_yield?(body)
          @yield_functions.add(full_name)
        end
      end
    end

    # Check if expression list contains yield
    private def contains_yield?(body : Array(ExprId)) : Bool
      body.any? { |expr_id| contains_yield_in_expr?(expr_id) }
    end

    private def contains_yield_in_expr?(expr_id : ExprId) : Bool
      return false if expr_id.invalid?
      node = @arena[expr_id]
      case node
      when CrystalV2::Compiler::Frontend::YieldNode
        true
      when CrystalV2::Compiler::Frontend::AssignNode
        contains_yield_in_expr?(node.target) || contains_yield_in_expr?(node.value)
      when CrystalV2::Compiler::Frontend::MemberAccessNode
        contains_yield_in_expr?(node.object)
      when CrystalV2::Compiler::Frontend::CallNode
        if callee_id = node.callee
          return true if contains_yield_in_expr?(callee_id)
        end
        node.args.each do |arg|
          return true if contains_yield_in_expr?(arg)
        end
        if block_id = node.block
          return true if contains_yield_in_expr?(block_id)
        end
        if named = node.named_args
          named.each do |na|
            return true if contains_yield_in_expr?(na.value)
          end
        end
        false
      when CrystalV2::Compiler::Frontend::UnaryNode
        contains_yield_in_expr?(node.operand)
      when CrystalV2::Compiler::Frontend::BinaryNode
        contains_yield_in_expr?(node.left) || contains_yield_in_expr?(node.right)
      when CrystalV2::Compiler::Frontend::GroupingNode
        contains_yield_in_expr?(node.expression)
      when CrystalV2::Compiler::Frontend::MacroExpressionNode
        contains_yield_in_expr?(node.expression)
      when CrystalV2::Compiler::Frontend::ConstantNode
        contains_yield_in_expr?(node.value)
      when CrystalV2::Compiler::Frontend::IfNode
        contains_yield_in_expr?(node.condition) ||
          contains_yield?(node.then_body) ||
          (node.else_body ? contains_yield?(node.else_body.not_nil!) : false)
      when CrystalV2::Compiler::Frontend::UnlessNode
        contains_yield_in_expr?(node.condition) ||
          contains_yield?(node.then_branch) ||
          (node.else_branch ? contains_yield?(node.else_branch.not_nil!) : false)
      when CrystalV2::Compiler::Frontend::WhileNode
        contains_yield_in_expr?(node.condition) || contains_yield?(node.body)
      when CrystalV2::Compiler::Frontend::UntilNode
        contains_yield_in_expr?(node.condition) || contains_yield?(node.body)
      when CrystalV2::Compiler::Frontend::LoopNode
        contains_yield?(node.body)
      when CrystalV2::Compiler::Frontend::BlockNode
        contains_yield?(node.body)
      when CrystalV2::Compiler::Frontend::ProcLiteralNode
        contains_yield?(node.body)
      when CrystalV2::Compiler::Frontend::CaseNode
        node.when_branches.any? { |w| contains_yield?(w.body) } ||
          (node.else_branch ? contains_yield?(node.else_branch.not_nil!) : false)
      when CrystalV2::Compiler::Frontend::IndexNode
        return true if contains_yield_in_expr?(node.object)
        node.indexes.any? { |idx| contains_yield_in_expr?(idx) }
      when CrystalV2::Compiler::Frontend::RangeNode
        contains_yield_in_expr?(node.begin_expr) || contains_yield_in_expr?(node.end_expr)
      else
        false
      end
    end

    # Check if a module method exists (with any signature - for module detection)
    private def is_module_method?(module_name : String, method_name : String) : Bool
      base_name = "#{module_name}.#{method_name}"
      # O(1) lookup: check exact match or mangled version exists
      @function_types.has_key?(base_name) || has_function_base?(base_name)
    end

    # Look up return type of a function by name
    private def get_function_return_type(name : String) : TypeRef
      # First check pre-registered signatures (for forward references)
      if type = @function_types[name]?
        return type
      end
      # If this is a base name (no $ suffix), use cached return type if available.
      if cached = @function_base_return_types[name]?
        return cached
      end
      # Fall back to already-lowered functions
      @module.functions.each do |func|
        return func.return_type if func.name == name
      end
      TypeRef::VOID
    end

    # Get instance variable offset from current class
    private def get_ivar_offset(name : String) : Int32
      if class_name = @current_class
        if class_info = @class_info[class_name]?
          class_info.ivars.each do |ivar|
            return ivar.offset if ivar.name == name
          end
        end
      end
      0  # Default offset
    end

    # Get instance variable type from current class
    private def get_ivar_type(name : String) : TypeRef?
      if class_name = @current_class
        if class_info = @class_info[class_name]?
          class_info.ivars.each do |ivar|
            return ivar.type if ivar.name == name
          end
        end
      end
      nil
    end

    # Resolve method name with inheritance: look in class and all parent classes
    # Resolve a short class name to its fully qualified name using current context
    # E.g., if @current_class is "CrystalV2::Compiler::Frontend::Span" and name is "Span",
    # returns "CrystalV2::Compiler::Frontend::Span"
    private def resolve_class_name_in_context(name : String) : String
      # Prefer the current namespace first (nested types shadow outer/global ones).
      if current = @current_class
        # Extract namespace: "Foo::Bar::Baz" -> "Foo::Bar"
        parts = current.split("::")

        # Try full namespace first (e.g., Foo::Bar::Baz::Name), then increasingly shorter
        # This handles cases where we're inside module Foo::Bar and reference Name
        # which should resolve to Foo::Bar::Name before trying Foo::Name
        while parts.size > 0
          qualified_name = (parts + [name]).join("::")
          if @class_info.has_key?(qualified_name)
            return qualified_name
          end
          parts.pop
        end

        # All namespace prefixes tried and failed - continue with other checks below
      end

      # Fall back to a top-level type
      return name if @class_info.has_key?(name)

      # Also try the exact class name if current class matches
      # E.g., inside Span, "Span" should resolve to the same class
      if current = @current_class
        # Check if name is the last component of current class
        last_part = current.split("::").last
        if last_part == name && @class_info.has_key?(current)
          return current
        end
      end

      # Fallback to original name
      name
    end

    private def resolve_type_alias_chain(name : String) : String
      resolved = @type_aliases[name]? || LIBC_TYPE_ALIASES[name]? || name
      depth = 0
      while (next_resolved = @type_aliases[resolved]? || LIBC_TYPE_ALIASES[resolved]?) && next_resolved != resolved && depth < 10
        resolved = next_resolved
        depth += 1
      end
      resolved
    end

    # Split a generic type argument list like "String, Array(Int32), Hash(K, V)"
    # into top-level arguments, respecting nested parentheses.
    private def split_generic_type_args(params_str : String) : Array(String)
      args = [] of String
      depth = 0
      start = 0
      i = 0
      while i < params_str.bytesize
        ch = params_str.byte_at(i).unsafe_chr
        case ch
        when '(', '{', '['
          depth += 1
        when ')', '}', ']'
          depth -= 1 if depth > 0
        when ','
          if depth == 0
            part = params_str[start, i - start].strip
            args << part unless part.empty?
            start = i + 1
          end
        end
        i += 1
      end
      tail = params_str[start, params_str.size - start].strip
      args << tail unless tail.empty?
      args
    end

    private def find_top_level_arrow(name : String) : Int32?
      depth = 0
      i = 0
      while i + 1 < name.bytesize
        ch = name.byte_at(i).unsafe_chr
        case ch
        when '(', '{', '['
          depth += 1
        when ')', '}', ']'
          depth -= 1 if depth > 0
        when '-'
          if depth == 0 && name.byte_at(i + 1).unsafe_chr == '>'
            return i
          end
        end
        i += 1
      end
      nil
    end

    private def extract_proc_return_type_name(type_name : String) : String?
      stripped = type_name.strip
      if stripped.starts_with?("Proc(") && stripped.ends_with?(")")
        inner = stripped[5, stripped.size - 6]
        args = split_generic_type_args(inner)
        if ret = args.last?
          ret = ret.strip
          return ret unless ret.empty?
        end
      end
      if arrow_index = find_top_level_arrow(stripped)
        right = stripped[arrow_index + 2, stripped.size - arrow_index - 2].strip
        return right unless right.empty?
      end
      nil
    end

    private def substitute_type_param(type_name : String, param_name : String, actual_name : String) : String
      return type_name if param_name.empty? || actual_name.empty?
      pattern = /(^|[^A-Za-z0-9_:])#{Regex.escape(param_name)}([^A-Za-z0-9_:]|$)/
      type_name.gsub(pattern, "\\1#{actual_name}\\2")
    end

    private def block_return_type_name(ctx : LoweringContext, block_id : BlockId) : String?
      block = ctx.get_block(block_id)
      term = block.terminator
      return nil unless term.is_a?(Return)
      value_id = term.value
      return nil unless value_id
      type_ref = ctx.type_of(value_id)
      return nil if type_ref == TypeRef::VOID
      type_name = get_type_name_from_ref(type_ref)
      return nil if type_name == "Void" || type_name == "Unknown"
      type_name
    end

    private def resolve_block_dependent_return_type(
      mangled_method_name : String,
      base_method_name : String,
      block_return_name : String
    ) : TypeRef?
      return nil if block_return_name.empty?

      func_def = @function_defs[mangled_method_name]? || @function_defs[base_method_name]?
      return nil unless func_def

      return_type_slice = func_def.return_type
      return nil unless return_type_slice
      return_type_name = String.new(return_type_slice)
      return nil if return_type_name.empty?

      block_param = func_def.params.try(&.find(&.is_block))
      return nil unless block_param

      block_type = block_param.type_annotation
      return nil unless block_type

      block_type_name = String.new(block_type)
      type_param_name = extract_proc_return_type_name(block_type_name)
      return nil unless type_param_name && !type_param_name.empty?

      substituted = substitute_type_param(return_type_name, type_param_name, block_return_name)
      return nil if substituted == return_type_name

      type_ref_for_name(substituted)
    end

    private def intern_proc_type(type_names : Array(String)) : TypeRef
      names = type_names
      names = ["Nil"] if names.empty?
      type_refs = names.map { |tn| type_ref_for_name(tn) }
      @module.intern_type(TypeDescriptor.new(TypeKind::Proc, "Proc", type_refs))
    end

    private def proc_type_ref_for_name(name : String) : TypeRef?
      stripped = name.strip
      return nil if stripped.empty?

      if stripped.starts_with?("Proc(") && stripped.ends_with?(")")
        inner = stripped[5, stripped.size - 6]
        return intern_proc_type(split_generic_type_args(inner))
      end

      if arrow_index = find_top_level_arrow(stripped)
        left = stripped[0, arrow_index].strip
        right = stripped[arrow_index + 2, stripped.size - arrow_index - 2].strip
        args = left.empty? ? [] of String : split_generic_type_args(left)
        ret_name = right.empty? ? "Nil" : right
        return intern_proc_type(args + [ret_name])
      end

      nil
    end

    # Ensure a generic instantiation referenced via a type annotation (e.g. `Hash(String, ValueId)`)
    # is monomorphized before lowering code that calls methods on it.
    private def ensure_monomorphized_type(type_ref : TypeRef) : Nil
      type_desc = @module.get_type_descriptor(type_ref)
      return unless type_desc

      name = type_desc.name
      paren = name.index('(')
      return unless paren

      base = name[0, paren]
      return if base == "Pointer"
      template = @generic_templates[base]?
      return unless template
      return if @monomorphized.includes?(name)

      params_str = name[paren + 1, name.size - paren - 2]
      type_args = split_generic_type_args(params_str)
      return unless template.type_params.size == type_args.size

      monomorphize_generic_class(base, type_args, name)
    end

    # Returns the fully qualified method name (e.g., "Animal#age" or "Animal#age:Int32") or nil if not found
    # Note: Returns the base name without mangling - caller should mangle with actual arg types
    private def resolve_method_with_inheritance(class_name : String, method_name : String) : String?
      current = class_name
      visited = Set(String).new
      while true
        break if visited.includes?(current)
        visited << current
        test_name = "#{current}##{method_name}"
        # O(1) lookup: check exact match first, then check if base name exists
        if @function_types.has_key?(test_name) || has_function_base?(test_name)
          return test_name  # Return base name - caller will mangle
        end
        # Also check included modules for this class
        if included = @class_included_modules[current]?
          included.each do |module_name|
            # Strip generic params for module lookup (Indexable(T) -> Indexable)
            base_module = module_name.split('(').first
            module_method = "#{base_module}##{method_name}"
            if @function_types.has_key?(module_method) || has_function_base?(module_method)
              # Return with class prefix so it gets lowered for this class
              return test_name
            end
          end
        end
        # Try parent class
        if info = @class_info[current]?
          if parent = info.parent_name
            current = parent
          else
            break
          end
        else
          break
        end
      end
      nil
    end

    # Infer type argument for generic class constructor call
    # E.g., Array.new(size, value) -> infer T from value's type
    #       Array.new(size) { block } -> infer T from block's return type
    private def infer_generic_type_arg(class_name : String, args : Array(CrystalV2::Compiler::Frontend::ExprId)?, block : CrystalV2::Compiler::Frontend::ExprId?, ctx : LoweringContext) : String?
      # For Array.new(size, initial_value), infer from initial_value (second arg)
      if class_name == "Array" && args && args.size >= 2
        value_arg = @arena[args[1]]
        return infer_type_from_expr(value_arg)
      end

      # For Array.new(size) { block }, infer from block's return type
      if class_name == "Array" && block
        block_node = @arena[block]
        if block_node.is_a?(CrystalV2::Compiler::Frontend::BlockNode)
          if body = block_node.body
            # Look at last expression in block to infer return type
            last_expr_id = body.last?
            if last_expr_id
              last_expr = @arena[last_expr_id]
              return infer_type_from_expr(last_expr)
            end
          end
        end
      end

      # For Slice.new(pointer, size, ...), infer from first arg's pointed-to type
      if class_name == "Slice" && args && args.size >= 1
        pointer_arg_id = args[0]
        pointer_arg = @arena[pointer_arg_id]
        # Try to get the pointer's element type
        # First check if it's an ivar access (like @buffer which is Pointer(UInt8))
        if pointer_arg.is_a?(CrystalV2::Compiler::Frontend::InstanceVarNode)
          ivar_name = String.new(pointer_arg.name)
          # Look up ivar type in current class
          if current_class_name = @current_class
            if class_info = @class_info[current_class_name]?
              if ivar_info = class_info.ivars.find { |iv| iv.name == ivar_name }
                ivar_type = ivar_info.type
                # If it's a Pointer type, extract element type
                if type_desc = @module.get_type_descriptor(ivar_type)
                  type_str = type_desc.name
                  if type_str.starts_with?("Pointer(") && type_str.ends_with?(")")
                    element_type = type_str[8...-1]  # Extract "UInt8" from "Pointer(UInt8)"
                    return element_type
                  end
                end
              end
            end
          end
        end
        # Default to UInt8 for Slice (Bytes is Slice(UInt8))
        # This is the most common case and avoids side effects from lower_expr
        return "UInt8"
      end

      # For Hash.new, return nil (needs two type params - more complex)
      nil
    end

    # Infer type name from an expression AST node
    private def infer_type_from_expr(node) : String?
      case node
      when CrystalV2::Compiler::Frontend::StringNode, CrystalV2::Compiler::Frontend::StringInterpolationNode
        "String"
      when CrystalV2::Compiler::Frontend::NumberNode
        # Check if it's a float (has decimal point or exponent)
        num_str = String.new(node.value)
        if num_str.includes?('.') || num_str.includes?('e') || num_str.includes?('E')
          "Float64"
        else
          "Int32"
        end
      when CrystalV2::Compiler::Frontend::BoolNode
        "Bool"
      when CrystalV2::Compiler::Frontend::CharNode
        "Char"
      when CrystalV2::Compiler::Frontend::SymbolNode
        "Symbol"
      when CrystalV2::Compiler::Frontend::NilNode
        "Nil"
      when CrystalV2::Compiler::Frontend::CallNode
        # For calls like String.new(...), try to get return type
        callee = node.callee
        callee_node = @arena[callee]
        case callee_node
        when CrystalV2::Compiler::Frontend::MemberAccessNode
          obj_node = @arena[callee_node.object]
          member_name = String.new(callee_node.member)
          if obj_node.is_a?(CrystalV2::Compiler::Frontend::ConstantNode) && member_name == "new"
            # ClassName.new() returns ClassName
            return String.new(obj_node.name)
          elsif obj_node.is_a?(CrystalV2::Compiler::Frontend::IdentifierNode) && member_name == "new"
            name = String.new(obj_node.name)
            if name[0].uppercase?
              return name
            end
          end
        end
        nil
      else
        nil
      end
    end

    # Infer type from class-level ivar assignment: @vec = SomeType.new
    # Used during class registration (pass 2) to handle implicit ivars
    private def infer_type_from_class_ivar_assign(value_node) : TypeRef
      case value_node
      when CrystalV2::Compiler::Frontend::MemberAccessNode
        # Direct member access: SomeType.new (without call parens)
        obj_node = @arena[value_node.object]
        member_name = String.new(value_node.member)
        if ENV.has_key?("DEBUG_TYPE_INFER")
          STDERR.puts "[TYPE_INFER] Direct MemberAccess member=#{member_name}, obj_node type: #{obj_node.class}"
        end
        if member_name == "new"
          type_name = extract_type_name_from_node(obj_node)
          if ENV.has_key?("DEBUG_TYPE_INFER")
            STDERR.puts "[TYPE_INFER] extracted type_name=#{type_name || "nil"}"
          end
          if type_name
            return type_ref_for_name(type_name)
          end
        end
      when CrystalV2::Compiler::Frontend::CallNode
        # For calls like SomeClass.new() or GenericClass(T).new()
        callee = value_node.callee
        callee_node = @arena[callee]
        if ENV.has_key?("DEBUG_TYPE_INFER")
          STDERR.puts "[TYPE_INFER] CallNode callee_node type: #{callee_node.class}"
        end
        case callee_node
        when CrystalV2::Compiler::Frontend::MemberAccessNode
          obj_node = @arena[callee_node.object]
          member_name = String.new(callee_node.member)
          if ENV.has_key?("DEBUG_TYPE_INFER")
            STDERR.puts "[TYPE_INFER] MemberAccess member=#{member_name}, obj_node type: #{obj_node.class}"
          end
          if member_name == "new"
            type_name = extract_type_name_from_node(obj_node)
            if ENV.has_key?("DEBUG_TYPE_INFER")
              STDERR.puts "[TYPE_INFER] extracted type_name=#{type_name || "nil"}"
            end
            if type_name
              return type_ref_for_name(type_name)
            end
          end
        end
      when CrystalV2::Compiler::Frontend::StringNode, CrystalV2::Compiler::Frontend::StringInterpolationNode
        return TypeRef::STRING
      when CrystalV2::Compiler::Frontend::NumberNode
        num_str = String.new(value_node.value)
        if num_str.includes?('.') || num_str.includes?('e') || num_str.includes?('E')
          return TypeRef::FLOAT64
        else
          return TypeRef::INT32
        end
      when CrystalV2::Compiler::Frontend::BoolNode
        return TypeRef::BOOL
      when CrystalV2::Compiler::Frontend::CharNode
        return TypeRef::CHAR
      when CrystalV2::Compiler::Frontend::NilNode
        return TypeRef::VOID
      when CrystalV2::Compiler::Frontend::ArrayLiteralNode
        # Array literal - infer element type from first element
        if elements = value_node.elements
          if first_id = elements.first?
            first_node = @arena[first_id]
            if elem_type = infer_type_from_expr(first_node)
              return type_ref_for_name("Array(#{elem_type})")
            end
          end
        end
        return type_ref_for_name("Array(String)")  # Default
      end
      TypeRef::VOID  # Default to VOID if we can't infer
    end

    # Extract type name from AST node (ConstantNode, GenericNode, etc.)
    private def extract_type_name_from_node(node) : String?
      case node
      when CrystalV2::Compiler::Frontend::ConstantNode
        String.new(node.name)
      when CrystalV2::Compiler::Frontend::IdentifierNode
        # Identifiers can be type names (uppercase) or constants used as type args
        String.new(node.name)
      when CrystalV2::Compiler::Frontend::GenericNode
        # GenericClass(T, U) -> "GenericClass(T, U)"
        base_node = @arena[node.base_type]
        base_name = extract_type_name_from_node(base_node)
        if base_name && (type_args = node.type_args)
          # Resolve type args (may be type params like T that need substitution)
          arg_names = type_args.map do |arg_id|
            arg_node = @arena[arg_id]
            extract_type_name_from_node(arg_node) || "UNKNOWN"
          end
          "#{base_name}(#{arg_names.join(", ")})"
        else
          base_name
        end
      when CrystalV2::Compiler::Frontend::MemberAccessNode
        # Nested::Class - reconstruct qualified name
        obj_node = @arena[node.object]
        member_name = String.new(node.member)
        if obj_name = extract_type_name_from_node(obj_node)
          "#{obj_name}::#{member_name}"
        else
          member_name
        end
      else
        nil
      end
    end

    # Check if a type is a union type
    private def is_union_type?(type_ref : TypeRef) : Bool
      if type_desc = @module.get_type_descriptor(type_ref)
        type_desc.kind == TypeKind::Union
      else
        false
      end
    end

    # Get variant type_id for a value being assigned to union
    # Returns the index of the matching variant, or -1 if not found
    private def get_union_variant_id(union_type : TypeRef, value_type : TypeRef) : Int32
      mir_union_ref = hir_to_mir_type_ref(union_type)
      if descriptor = @union_descriptors[mir_union_ref]?
        mir_value_ref = hir_to_mir_type_ref(value_type)
        descriptor.variants.each_with_index do |variant, idx|
          if variant.type_ref == mir_value_ref
            return idx
          end
        end
      end
      -1
    end

    # Find an existing union type in the module that can represent all required types.
    # Prefer the smallest matching union (fewest variants) to avoid needlessly wide phis.
    private def find_covering_union_type(required_types : Array(TypeRef)) : TypeRef?
      required_mir = required_types.map { |t| hir_to_mir_type_ref(t) }.uniq

      best_ref : TypeRef? = nil
      best_variant_count = Int32::MAX

      @module.types.each_with_index do |desc, idx|
        next unless desc.kind == TypeKind::Union || desc.name.includes?("___")

        hir_union_ref = TypeRef.new(TypeRef::FIRST_USER_TYPE + idx.to_u32)
        mir_union_ref = hir_to_mir_type_ref(hir_union_ref)
        descriptor = @union_descriptors[mir_union_ref]?
        next unless descriptor

        variants = descriptor.variants.map(&.type_ref)
        next unless required_mir.all? { |rt| variants.includes?(rt) }

        if variants.size < best_variant_count
          best_variant_count = variants.size
          best_ref = hir_union_ref
        end
      end

      best_ref
    end

    # Get class variable type from current class
    private def get_class_var_type(name : String) : TypeRef
      if class_name = @current_class
        if class_info = @class_info[class_name]?
          class_info.class_vars.each do |cvar|
            return cvar.type if cvar.name == name
          end
        end
      end
      TypeRef::VOID
    end

    # Lower a function definition
    def lower_def(
      node : CrystalV2::Compiler::Frontend::DefNode,
      call_arg_types : Array(TypeRef)? = nil,
      full_name_override : String? = nil
    ) : Function
      base_name = String.new(node.name)
      if base_name == "main" && @current_class.nil? && !fun_def?(node)
        base_name = TOP_LEVEL_MAIN_BASE
        @top_level_main_defined = true
      end

      # Lower parameters
      param_infos = [] of Tuple(String, TypeRef)
      param_types = [] of TypeRef
      has_block = false
      param_type_map = {} of String => TypeRef
      old_typeof_locals = @current_typeof_locals
      old_typeof_local_names = @current_typeof_local_names
      @current_typeof_locals = param_type_map
      @current_typeof_local_names = {} of String => String
      call_types = call_arg_types || [] of TypeRef
      call_index = 0
      splat_param_info_index : Int32? = nil
      splat_param_types_index : Int32? = nil
      splat_param_name : String? = nil
      splat_param_has_annotation = false

      if params = node.params
        params.each do |param|
          next if named_only_separator?(param)
          param_name = param.name.nil? ? "_" : String.new(param.name.not_nil!)
          param_type = if ta = param.type_annotation
                         type_ref_for_name(String.new(ta))
                       else
                         TypeRef::VOID  # Unknown type
                       end
          if param_type == TypeRef::VOID && !param.is_block && !param.is_splat && !param.is_double_splat
            if call_index < call_types.size
              inferred = call_types[call_index]
              param_type = inferred if inferred != TypeRef::VOID
            end
          end
          if !param.is_block && !param.is_splat && !param.is_double_splat && call_index < call_types.size
            param_type = refine_param_type_from_call(param_type, call_types[call_index])
          end

          param_type_map[param_name] = param_type
          param_infos << {param_name, param_type}
          if ta = param.type_annotation
            update_typeof_local_name(param_name, String.new(ta))
          end
          if param.is_block
            has_block = true
          else
            if param.is_splat || param.is_double_splat
              splat_param_info_index = param_infos.size - 1
              splat_param_types_index = param_types.size
              splat_param_name = param_name
              splat_param_has_annotation = !param.type_annotation.nil?
            else
              call_index += 1
            end
            param_types << param_type
          end
        end
      end

      if splat_param_name && !call_types.empty? && !splat_param_has_annotation
        remaining = call_types[call_index..-1]? || [] of TypeRef
        splat_type = if remaining.size == 1
                       remaining[0]
                     else
                       tuple_type_from_arg_types(remaining)
                     end
        if splat_type != TypeRef::VOID
          param_type_map[splat_param_name.not_nil!] = splat_type
          if idx = splat_param_info_index
            param_infos[idx] = {splat_param_name.not_nil!, splat_type}
          end
          if idx = splat_param_types_index
            param_types[idx] = splat_type
          end
        end
      end

      # Determine return type (default to Void if not specified)
      return_type = if rt = node.return_type
                      rt_string = String.new(rt)
                      type_ref_for_name(rt_string)
                    elsif base_name.ends_with?("?")
                      TypeRef::BOOL
                    else
                      TypeRef::VOID
                    end

      # Top-level functions support overloading, so use mangled names consistently.
      full_name = full_name_override || mangle_function_name(base_name, param_types, has_block)

      # Idempotency: avoid lowering the same function twice (can happen with conditional defs).
      if existing = @module.functions.find { |f| f.name == full_name }
        return existing
      end

      # Ensure function type is registered even when caller skipped register_function (e.g. conditional defs).
      register_function_type(full_name, return_type) unless @function_types[full_name]?
      register_function_type(base_name, return_type) unless @function_types[base_name]?

      # Keep AST around for signatureHelp/named args and for yield inlining.
      unless @function_defs.has_key?(base_name)
        @function_defs[base_name] = node
        @function_def_arenas[base_name] = @arena
      end
      @function_defs[full_name] = node
      @function_def_arenas[full_name] = @arena

      func = @module.create_function(full_name, return_type)
      ctx = LoweringContext.new(func, @module, @arena)

      param_infos.each do |(param_name, param_type)|
        hir_param = func.add_param(param_name, param_type)
        ctx.register_local(param_name, hir_param.id)
        ctx.register_type(hir_param.id, param_type)  # Track param type for inference
      end

      # Lower body
      last_value : ValueId? = nil
      if body = node.body
        body.each do |expr_id|
          last_value = lower_expr(ctx, expr_id)
        end
      end

      # Add implicit return if not already terminated
      # BUT don't add return after raise (which sets Unreachable terminator)
      block = ctx.get_block(ctx.current_block)
      block_has_raise = block.instructions.any? { |inst| inst.is_a?(Raise) }
      if block.terminator.is_a?(Unreachable) && !block_has_raise
        block.terminator = Return.new(last_value)
      end

      # Infer return type from last expression if not explicitly specified
      # This handles methods with implicit returns like `def root_buffer; @buffer - @offset; end`
      if return_type == TypeRef::VOID && last_value
        inferred_type = ctx.type_of(last_value)
        if inferred_type != TypeRef::VOID
          func.return_type = inferred_type
          # Update function type registry to match
          register_function_type(full_name, inferred_type)
          register_function_type(base_name, inferred_type)
        end
      end

      @current_typeof_locals = old_typeof_locals
      @current_typeof_local_names = old_typeof_local_names

      func
    end

    # Lower top-level expressions into a synthetic main function
    # Note: Named __crystal_main because stdlib's fun main calls LibCrystalMain.__crystal_main
    def lower_main(main_exprs : Array(Tuple(CrystalV2::Compiler::Frontend::ExprId, CrystalV2::Compiler::Frontend::ArenaLike))) : Function
      # Create __crystal_main function with void return type
      # Signature: fun __crystal_main(argc : Int32, argv : UInt8**)
      func = @module.create_function("__crystal_main", TypeRef::VOID)

      # Add parameters to match lib declaration
      argc_param = func.add_param("argc", TypeRef::INT32)
      # UInt8** = pointer to pointer to UInt8, use generic POINTER type
      argv_param = func.add_param("argv", TypeRef::POINTER)

      ctx = LoweringContext.new(func, @module, @arena)

      # Register parameters in context for potential use
      ctx.register_local("argc", argc_param.id)
      ctx.register_local("argv", argv_param.id)

      # Lower each top-level expression in order
      last_value : ValueId? = nil
      main_exprs.each do |expr_id, arena|
        # Switch arena context for this expression
        @arena = arena
        last_value = lower_expr(ctx, expr_id)
      end

      # Return void (stdlib's fun main handles the return value)
      block = ctx.get_block(ctx.current_block)
      if block.terminator.is_a?(Unreachable)
        block.terminator = Return.new(nil)
      end

      func
    end

    # Lower a synthetic __crystal_main that calls a user-defined main.
    # Used when there are no top-level expressions (no implicit main body).
    def lower_main_from_def(node : CrystalV2::Compiler::Frontend::DefNode) : Function
      if existing = @module.functions.find { |f| f.name == "__crystal_main" }
        return existing
      end

      # Ensure user main is lowered (lazy lowering in CLI).
      lower_def(node)

      # Create __crystal_main(argc, argv)
      func = @module.create_function("__crystal_main", TypeRef::VOID)
      argc_param = func.add_param("argc", TypeRef::INT32)
      argv_param = func.add_param("argv", TypeRef::POINTER)

      ctx = LoweringContext.new(func, @module, @arena)
      ctx.register_local("argc", argc_param.id)
      ctx.register_local("argv", argv_param.id)
      ctx.register_type(argc_param.id, TypeRef::INT32)
      ctx.register_type(argv_param.id, TypeRef::POINTER)

      # Build the call to main with argc/argv if requested by the signature.
      param_types = [] of TypeRef
      has_block = false
      if params = node.params
        params.each do |param|
          next if named_only_separator?(param)
          if param.is_block
            has_block = true
            next
          end
          param_types << (param.type_annotation ? type_ref_for_name(String.new(param.type_annotation.not_nil!)) : TypeRef::VOID)
        end
      end

      main_base = if String.new(node.name) == "main" && @current_class.nil? && !fun_def?(node)
                    @top_level_main_defined = true
                    TOP_LEVEL_MAIN_BASE
                  else
                    "main"
                  end
      main_name = mangle_function_name(main_base, param_types, has_block)
      return_type = get_function_return_type(main_name)
      args = [] of ValueId
      args << argc_param.id if param_types.size >= 1
      args << argv_param.id if param_types.size >= 2
      call = Call.new(ctx.next_id, return_type, nil, main_name, args)
      ctx.emit(call)

      ctx.terminate(Return.new(nil))
      func
    end

    # Lower a single expression, returns ValueId of result
    def lower_expr(ctx : LoweringContext, expr_id : ExprId) : ValueId
      node = @arena[expr_id]
      lower_node(ctx, node)
    end

    # Lower an AST node to HIR
    def lower_node(ctx : LoweringContext, node : AstNode) : ValueId
      case node
      # ═══════════════════════════════════════════════════════════════════
      # LITERALS
      # ═══════════════════════════════════════════════════════════════════

      when CrystalV2::Compiler::Frontend::NumberNode
        lower_number(ctx, node)

      when CrystalV2::Compiler::Frontend::StringNode
        lower_string(ctx, node)

      when CrystalV2::Compiler::Frontend::StringInterpolationNode
        lower_string_interpolation(ctx, node)

      when CrystalV2::Compiler::Frontend::RegexNode
        lower_regex(ctx, node)

      when CrystalV2::Compiler::Frontend::CharNode
        lower_char(ctx, node)

      when CrystalV2::Compiler::Frontend::BoolNode
        lower_bool(ctx, node)

      when CrystalV2::Compiler::Frontend::NilNode
        lower_nil(ctx, node)

      when CrystalV2::Compiler::Frontend::SymbolNode
        lower_symbol(ctx, node)

      # ═══════════════════════════════════════════════════════════════════
      # VARIABLES
      # ═══════════════════════════════════════════════════════════════════

      when CrystalV2::Compiler::Frontend::IdentifierNode
        lower_identifier(ctx, node)

      when CrystalV2::Compiler::Frontend::InstanceVarNode
        lower_instance_var(ctx, node)

      when CrystalV2::Compiler::Frontend::ClassVarNode
        lower_class_var(ctx, node)

      when CrystalV2::Compiler::Frontend::SelfNode
        lower_self(ctx, node)

      when CrystalV2::Compiler::Frontend::ImplicitObjNode
        # Implicit object (like .foo) is treated as self in current context
        lower_self_implicit(ctx, node)

      when CrystalV2::Compiler::Frontend::SuperNode
        lower_super(ctx, node)

      when CrystalV2::Compiler::Frontend::PreviousDefNode
        lower_previous_def(ctx, node)

      when CrystalV2::Compiler::Frontend::GlobalNode
        lower_global(ctx, node)

      # ═══════════════════════════════════════════════════════════════════
      # OPERATIONS
      # ═══════════════════════════════════════════════════════════════════

      when CrystalV2::Compiler::Frontend::BinaryNode
        lower_binary(ctx, node)

      when CrystalV2::Compiler::Frontend::UnaryNode
        lower_unary(ctx, node)

      # ═══════════════════════════════════════════════════════════════════
      # CONTROL FLOW
      # ═══════════════════════════════════════════════════════════════════

      when CrystalV2::Compiler::Frontend::IfNode
        lower_if(ctx, node)

      when CrystalV2::Compiler::Frontend::UnlessNode
        lower_unless(ctx, node)

      when CrystalV2::Compiler::Frontend::WhileNode
        lower_while(ctx, node)

      when CrystalV2::Compiler::Frontend::UntilNode
        lower_until(ctx, node)

      when CrystalV2::Compiler::Frontend::TernaryNode
        lower_ternary(ctx, node)

      when CrystalV2::Compiler::Frontend::CaseNode
        lower_case(ctx, node)

      # ═══════════════════════════════════════════════════════════════════
      # FUNCTION-RELATED
      # ═══════════════════════════════════════════════════════════════════

      when CrystalV2::Compiler::Frontend::ReturnNode
        lower_return(ctx, node)

      when CrystalV2::Compiler::Frontend::YieldNode
        if blk = @inline_yield_block_stack.last?
          inline_block_body(ctx, node, blk)
        else
          lower_yield(ctx, node)
        end

      when CrystalV2::Compiler::Frontend::BreakNode
        lower_break(ctx, node)

      when CrystalV2::Compiler::Frontend::NextNode
        lower_next(ctx, node)

      when CrystalV2::Compiler::Frontend::BeginNode
        lower_begin(ctx, node)

      when CrystalV2::Compiler::Frontend::RaiseNode
        lower_raise(ctx, node)

      # ═══════════════════════════════════════════════════════════════════
      # CALLS
      # ═══════════════════════════════════════════════════════════════════

      when CrystalV2::Compiler::Frontend::CallNode
        lower_call(ctx, node)

      when CrystalV2::Compiler::Frontend::IndexNode
        lower_index(ctx, node)

      when CrystalV2::Compiler::Frontend::MemberAccessNode
        lower_member_access(ctx, node)

      # ═══════════════════════════════════════════════════════════════════
      # ASSIGNMENT
      # ═══════════════════════════════════════════════════════════════════

      when CrystalV2::Compiler::Frontend::AssignNode
        lower_assign(ctx, node)

      when CrystalV2::Compiler::Frontend::MultipleAssignNode
        lower_multiple_assign(ctx, node)

      # ═══════════════════════════════════════════════════════════════════
      # CLOSURES
      # ═══════════════════════════════════════════════════════════════════

      when CrystalV2::Compiler::Frontend::BlockNode
        lower_block(ctx, node)

      when CrystalV2::Compiler::Frontend::ProcLiteralNode
        lower_proc_literal(ctx, node)

      # ═══════════════════════════════════════════════════════════════════
      # COLLECTIONS
      # ═══════════════════════════════════════════════════════════════════

      when CrystalV2::Compiler::Frontend::ArrayLiteralNode
        lower_array_literal(ctx, node)

      when CrystalV2::Compiler::Frontend::HashLiteralNode
        lower_hash_literal(ctx, node)

      when CrystalV2::Compiler::Frontend::TupleLiteralNode
        lower_tuple_literal(ctx, node)

      when CrystalV2::Compiler::Frontend::NamedTupleLiteralNode
        lower_named_tuple_literal(ctx, node)

      when CrystalV2::Compiler::Frontend::RangeNode
        lower_range(ctx, node)

      # ═══════════════════════════════════════════════════════════════════
      # TYPE OPERATIONS
      # ═══════════════════════════════════════════════════════════════════

      when CrystalV2::Compiler::Frontend::AsNode
        lower_as(ctx, node)

      when CrystalV2::Compiler::Frontend::AsQuestionNode
        lower_as_question(ctx, node)

      when CrystalV2::Compiler::Frontend::IsANode
        lower_is_a(ctx, node)

      when CrystalV2::Compiler::Frontend::RespondsToNode
        lower_responds_to(ctx, node)

      # ═══════════════════════════════════════════════════════════════════
      # MISC
      # ═══════════════════════════════════════════════════════════════════

      when CrystalV2::Compiler::Frontend::GroupingNode
        # Just unwrap grouping
        lower_expr(ctx, node.expression)

      when CrystalV2::Compiler::Frontend::SplatNode
        # Lower the inner expression (splat semantics handled at call site)
        lower_expr(ctx, node.expr)

      when CrystalV2::Compiler::Frontend::PathNode
        lower_path(ctx, node)

      when CrystalV2::Compiler::Frontend::GenericNode
        # Generic type like Array(Int32) - lower as type reference for use as receiver
        lower_generic_type_ref(ctx, node)

      when CrystalV2::Compiler::Frontend::UninitializedNode
        # uninitialized Type - returns undefined value of given type
        lower_uninitialized(ctx, node)

      when CrystalV2::Compiler::Frontend::TypeDeclarationNode
        # x : Type = value - local variable with type annotation
        lower_type_declaration(ctx, node)

      when CrystalV2::Compiler::Frontend::LoopNode
        # loop do ... end - infinite loop (exits via break)
        lower_loop(ctx, node)

      when CrystalV2::Compiler::Frontend::MacroLiteralNode,
           CrystalV2::Compiler::Frontend::MacroVarNode,
           CrystalV2::Compiler::Frontend::MacroIfNode,
           CrystalV2::Compiler::Frontend::MacroForNode,
           CrystalV2::Compiler::Frontend::MacroExpressionNode
        # Macro nodes are not lowered directly - they are expanded first
        # Return nil for any macro content encountered
        nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
        ctx.emit(nil_lit)
        nil_lit.id

      when CrystalV2::Compiler::Frontend::PointerofNode
        lower_pointerof(ctx, node)

      when CrystalV2::Compiler::Frontend::AnnotationNode
        # Annotations like @[Link("c")] - store for later processing
        # For now, just return nil (annotations are metadata, not values)
        lower_annotation(ctx, node)

      when CrystalV2::Compiler::Frontend::LibNode
        # lib LibC ... end - C library bindings
        lower_lib(ctx, node)

      when CrystalV2::Compiler::Frontend::FunNode
        # fun malloc(size : Int64) : Void* - external C function
        lower_fun(ctx, node)

      when CrystalV2::Compiler::Frontend::DefNode
        # Top-level method definition
        lower_top_level_def(ctx, node)

      when CrystalV2::Compiler::Frontend::TypeofNode
        # typeof(x) - returns type at compile time, for runtime just return nil
        lower_typeof(ctx, node)

      when CrystalV2::Compiler::Frontend::SizeofNode
        # sizeof(T) - returns size of type
        lower_sizeof(ctx, node)

      when CrystalV2::Compiler::Frontend::OffsetofNode
        # offsetof(T, @field) - returns field offset
        lower_offsetof(ctx, node)

      when CrystalV2::Compiler::Frontend::MacroDefNode
        # Macro definitions - skip at runtime, just return nil
        nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
        ctx.emit(nil_lit)
        nil_lit.id

      when CrystalV2::Compiler::Frontend::ModuleNode
        # Module definitions inside expressions - skip, just return nil
        nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
        ctx.emit(nil_lit)
        nil_lit.id

      when CrystalV2::Compiler::Frontend::ClassNode
        # Class definitions inside expressions - skip, just return nil
        nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
        ctx.emit(nil_lit)
        nil_lit.id

      when CrystalV2::Compiler::Frontend::RequireNode
        # Require directives - already processed, return nil
        nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
        ctx.emit(nil_lit)
        nil_lit.id

      when CrystalV2::Compiler::Frontend::GlobalVarDeclNode
        # Global variable declaration ($name : Type) - declaration only, return nil
        nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
        ctx.emit(nil_lit)
        nil_lit.id

      when CrystalV2::Compiler::Frontend::AnnotationDefNode
        # Annotation definition (annotation Foo) - declaration only, return nil
        nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
        ctx.emit(nil_lit)
        nil_lit.id

      when CrystalV2::Compiler::Frontend::ConstantNode
        # Constant definition (FOO = value) - evaluate and return the value
        value_id = lower_expr(ctx, node.value)
        value_id

      when CrystalV2::Compiler::Frontend::VisibilityModifierNode
        # Visibility modifier (private, protected) - just lower the target expression
        lower_expr(ctx, node.expression)

      when CrystalV2::Compiler::Frontend::OutNode
        # out x - creates a pointer to a local variable for C functions
        # The variable name is in node.identifier
        var_name = String.new(node.identifier)
        # Create the local variable if it doesn't exist
        var_id = ctx.lookup_local(var_name)
        if var_id.nil?
          # Create uninitialized variable
          alloc = Allocate.new(ctx.next_id, TypeRef::POINTER, [] of ValueId, true)
          ctx.emit(alloc)
          ctx.register_local(var_name, alloc.id)
          var_id = alloc.id
        end
        # Get address of the variable
        ptr = AddressOf.new(ctx.next_id, TypeRef::POINTER, var_id)
        ctx.emit(ptr)
        ptr.id

      when CrystalV2::Compiler::Frontend::EnumNode
        # Enum declarations are processed during registration phase
        # Just return nil literal during lowering
        nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
        ctx.emit(nil_lit)
        nil_lit.id

      when CrystalV2::Compiler::Frontend::StructNode
        # Struct declarations are processed during registration phase
        nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
        ctx.emit(nil_lit)
        nil_lit.id

      when CrystalV2::Compiler::Frontend::GetterNode,
           CrystalV2::Compiler::Frontend::SetterNode,
           CrystalV2::Compiler::Frontend::PropertyNode
        # Accessor declarations are processed during class registration/generation
        # If encountered during expression lowering, return nil
        nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
        ctx.emit(nil_lit)
        nil_lit.id

      else
        raise LoweringError.new("Unsupported AST node type: #{node.class}", node)
      end
    end

    # ═══════════════════════════════════════════════════════════════════════
    # LITERAL LOWERING
    # ═══════════════════════════════════════════════════════════════════════

    private def lower_number(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::NumberNode) : ValueId
      type = case node.kind
             when .i8?   then TypeRef::INT8
             when .i16?  then TypeRef::INT16
             when .i32?  then TypeRef::INT32
             when .i64?  then TypeRef::INT64
             when .i128? then TypeRef::INT128
             when .u8?   then TypeRef::UINT8
             when .u16?  then TypeRef::UINT16
             when .u32?  then TypeRef::UINT32
             when .u64?  then TypeRef::UINT64
             when .u128? then TypeRef::UINT128
             when .f32?  then TypeRef::FLOAT32
             when .f64?  then TypeRef::FLOAT64
             else             TypeRef::INT32
             end

      # Remove underscores and type suffixes (42_000i64 -> 42000)
      value_str = String.new(node.value).gsub('_', "")
      # Strip type suffix (i8, i16, i32, i64, i128, u8, u16, u32, u64, u128, f32, f64)
      value_str = value_str.gsub(/[iuf]\d+$/, "")

      # Handle empty string case (malformed number)
      if value_str.empty?
        value = 0_i64
      elsif node.kind.f32? || node.kind.f64?
        value = value_str.to_f64
      else
        # Handle hex (0x), binary (0b), and octal (0o)
        # Use UInt64 for parsing to handle large hex values, then cast to Int64
        raw = if value_str.starts_with?("0x") || value_str.starts_with?("0X")
                digits = value_str[2..]
                digits.empty? ? 0_u64 : digits.to_u64(16)
              elsif value_str.starts_with?("0b") || value_str.starts_with?("0B")
                digits = value_str[2..]
                digits.empty? ? 0_u64 : digits.to_u64(2)
              elsif value_str.starts_with?("0o") || value_str.starts_with?("0O")
                digits = value_str[2..]
                digits.empty? ? 0_u64 : digits.to_u64(8)
              else
                # Try to parse as UInt64 first (handles numbers > Int64::MAX)
                value_str.to_u64? || value_str.to_i64?.try(&.to_u64!) || 0_u64
              end
        value = raw.to_i64!  # Bitcast to Int64
      end

      lit = Literal.new(ctx.next_id, type, value)
      ctx.emit(lit)
      lit.id
    end

    private def lower_string(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::StringNode) : ValueId
      str = String.new(node.value)
      lit = Literal.new(ctx.next_id, TypeRef::STRING, str)
      ctx.emit(lit)
      lit.id
    end

    private def lower_regex(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::RegexNode) : ValueId
      # Regex literal /pattern/
      # For now, store as a string pattern - full regex support requires PCRE bindings
      pattern = String.new(node.pattern)
      regex_type = ctx.get_type("Regex")
      lit = Literal.new(ctx.next_id, regex_type, pattern)
      ctx.emit(lit)
      lit.id
    end

    private def lower_type_declaration(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::TypeDeclarationNode) : ValueId
      # x : Type = value - local variable declaration with type annotation
      var_name = String.new(node.name)
      type_name = String.new(node.declared_type)
      type_ref = type_ref_for_name(type_name)

      if value_id = node.value
        # Has initial value: x : Type = value
        value = lower_expr(ctx, value_id)
        # Register as local variable
        ctx.register_local(var_name, value)
        value_type = ctx.type_of(value)
        # If the declared type is module-like and the initializer is concrete,
        # keep the concrete type for call resolution to avoid includer heuristics.
        if module_like_type_name?(type_name) && value_type != TypeRef::VOID
          ctx.register_type(value, value_type)
          update_typeof_local(var_name, value_type)
          if concrete_name = concrete_type_name_for(value_type)
            update_typeof_local_name(var_name, concrete_name)
          else
            update_typeof_local_name(var_name, type_name)
          end
        else
          ctx.register_type(value, type_ref)
          update_typeof_local(var_name, type_ref)
          update_typeof_local_name(var_name, type_name)
        end
        value
      else
        # No initial value: x : Type (uninitialized)
        # Create undefined value for the type
        undefined_value : Int64 | Float64 | String | Bool | Nil = case type_ref
          when TypeRef::BOOL then false
          when TypeRef::FLOAT32, TypeRef::FLOAT64 then 0.0
          when TypeRef::STRING, TypeRef::POINTER then nil
          else 0_i64
        end

        lit = Literal.new(ctx.next_id, type_ref, undefined_value)
        ctx.emit(lit)
        ctx.register_local(var_name, lit.id)
        ctx.register_type(lit.id, type_ref)
        update_typeof_local(var_name, type_ref)
        update_typeof_local_name(var_name, type_name)
        lit.id
      end
    end

    private def lower_uninitialized(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::UninitializedNode) : ValueId
      # uninitialized Type - returns undefined value of given type
      # Extract type name from the type expression
      type_node = @arena[node.type]
      type_name = case type_node
                  when CrystalV2::Compiler::Frontend::IdentifierNode
                    String.new(type_node.name)
                  when CrystalV2::Compiler::Frontend::ConstantNode
                    String.new(type_node.name)
                  when CrystalV2::Compiler::Frontend::PathNode
                    collect_path_string(type_node)
                  when CrystalV2::Compiler::Frontend::GenericNode
                    # Generic type - extract base and type args
                    base = @arena[type_node.base_type]
                    base_name = case base
                                when CrystalV2::Compiler::Frontend::IdentifierNode
                                  String.new(base.name)
                                when CrystalV2::Compiler::Frontend::ConstantNode
                                  String.new(base.name)
                                else
                                  "Unknown"
                                end
                    type_args = type_node.type_args.map do |arg_id|
                      arg = @arena[arg_id]
                      case arg
                      when CrystalV2::Compiler::Frontend::IdentifierNode
                        String.new(arg.name)
                      when CrystalV2::Compiler::Frontend::ConstantNode
                        String.new(arg.name)
                      else
                        "Unknown"
                      end
                    end
                    "#{base_name}(#{type_args.join(", ")})"
                  when CrystalV2::Compiler::Frontend::IndexNode
                    # Static array type: UInt8[256] -> StaticArray(UInt8, 256)
                    base = @arena[type_node.object]
                    base_name = case base
                                when CrystalV2::Compiler::Frontend::IdentifierNode
                                  String.new(base.name)
                                when CrystalV2::Compiler::Frontend::ConstantNode
                                  String.new(base.name)
                                else
                                  "Unknown"
                                end
                    # Get size from first index
                    if type_node.indexes.size > 0
                      size_node = @arena[type_node.indexes[0]]
                      size_str = case size_node
                                 when CrystalV2::Compiler::Frontend::NumberNode
                                   String.new(size_node.value)
                                 else
                                   "0"
                                 end
                      "StaticArray(#{base_name}, #{size_str})"
                    else
                      "StaticArray(#{base_name}, 0)"
                    end
                  else
                    "Unknown"
                  end

      type_ref = type_ref_for_name(type_name)

      # Create an undefined literal (value doesn't matter, it's uninitialized)
      # For numeric types, use 0; for pointers, use null
      undefined_value : Int64 | Float64 | String | Bool | Nil = case type_ref
        when TypeRef::BOOL then false
        when TypeRef::FLOAT32, TypeRef::FLOAT64 then 0.0
        when TypeRef::STRING, TypeRef::POINTER then nil
        else 0_i64
      end

      lit = Literal.new(ctx.next_id, type_ref, undefined_value)
      ctx.emit(lit)
      ctx.register_type(lit.id, type_ref)
      lit.id
    end

    # ═══════════════════════════════════════════════════════════════════════
    # LIB BINDINGS (C FFI)
    # ═══════════════════════════════════════════════════════════════════════

    private def lower_annotation(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::AnnotationNode) : ValueId
      # Annotations like @[Link("c")] are metadata
      # Store Link annotations for the linker
      # node.name is an ExprId pointing to IdentifierNode or PathNode
      annotation_name = resolve_annotation_name(node.name)

      if annotation_name == "Link"
        # Extract library name from annotation arguments
        # Positional args: @[Link("c")]
        node.args.each do |arg_id|
          arg_node = @arena[arg_id]
          if arg_node.is_a?(CrystalV2::Compiler::Frontend::StringNode)
            lib_name = String.new(arg_node.value)
            @module.add_link_library(lib_name)
          end
        end

        # Named args: @[Link(framework: "Cocoa")] or @[Link(pkg_config: "libfoo")]
        if named_args = node.named_args
          named_args.each do |named_arg|
            value_node = @arena[named_arg.value]
            if value_node.is_a?(CrystalV2::Compiler::Frontend::StringNode)
              named_lib_name = String.new(value_node.value)
              named_key = String.new(named_arg.name)
              prefix = case named_key
                       when "pkg_config" then "pkg_config:"
                       when "framework"  then "framework:"
                       when "dll"        then "dll:"
                       else "#{named_key}:"
                       end
              @module.add_link_library("#{prefix}#{named_lib_name}")
            end
          end
        end
      end

      # Annotations don't produce values
      nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
      ctx.emit(nil_lit)
      nil_lit.id
    end

    private def lower_lib(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::LibNode) : ValueId
      # lib LibC ... end - C library bindings
      # Process the body to register external functions
      lib_name = String.new(node.name)

      if body = node.body
        body.each do |expr_id|
          body_node = @arena[expr_id]
          case body_node
          when CrystalV2::Compiler::Frontend::FunNode
            # Register external function
            register_extern_fun(lib_name, body_node)
          when CrystalV2::Compiler::Frontend::AnnotationNode
            # Process annotations within lib (e.g., @[Link])
            lower_annotation(ctx, body_node)
          when CrystalV2::Compiler::Frontend::AliasNode
            # type aliases within lib - register for later resolution
            alias_name = String.new(body_node.name)
            old_class = @current_class
            @current_class = lib_name
            target_name = resolve_alias_target(String.new(body_node.value))
            @current_class = old_class
            full_alias_name = "#{lib_name}::#{alias_name}"
            register_type_alias(full_alias_name, target_name)
            register_type_alias(alias_name, target_name)
          when CrystalV2::Compiler::Frontend::EnumNode
            # Enums within lib - register with lib prefix
            enum_name = String.new(body_node.name)
            full_enum_name = "#{lib_name}::#{enum_name}"
            register_enum_with_name(body_node, full_enum_name)
          when CrystalV2::Compiler::Frontend::StructNode
            # Structs within lib - skip for now (handled separately)
          else
            # Other declarations - process recursively
            lower_node(ctx, body_node)
          end
        end
      end

      # Lib declarations don't produce values
      nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
      ctx.emit(nil_lit)
      nil_lit.id
    end

    private def lower_fun(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::FunNode) : ValueId
      # fun malloc(size : Int64) : Void* - external C function
      # Register in current lib context (or global if not in lib)
      register_extern_fun(nil, node)

      # Fun declarations don't produce values
      nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
      ctx.emit(nil_lit)
      nil_lit.id
    end

    private def register_extern_fun(lib_name : String?, node : CrystalV2::Compiler::Frontend::FunNode)
      fun_name = String.new(node.name)
      real_name = node.real_name ? String.new(node.real_name.not_nil!) : fun_name

      # Top-level `fun main` in the stdlib is a C-ABI entrypoint in the real compiler.
      # Our bootstrap uses a synthetic wrapper main, so skip registering it as an extern
      # to avoid LLVM redefinition (declare + define).
      if lib_name.nil? && fun_name == "main"
        return
      end

      # Build parameter types
      param_types = [] of TypeRef
      if params = node.params
        params.each do |param|
          if type_ann = param.type_annotation
            type_name = String.new(type_ann)
            param_types << type_ref_for_c_type(type_name)
          else
            param_types << TypeRef::POINTER  # Default to pointer for untyped params
          end
        end
      end

      # Return type
      return_type = if ret = node.return_type
                      type_ref_for_c_type(String.new(ret))
                    else
                      TypeRef::VOID
                    end

      # Register the external function
      @module.add_extern_function(ExternFunction.new(
        name: fun_name,
        real_name: real_name,
        lib_name: lib_name,
        param_types: param_types,
        return_type: return_type,
        varargs: node.varargs
      ))
    end

    # Convert Crystal type notation to C-compatible TypeRef
    private def type_ref_for_c_type(type_name : String) : TypeRef
      case type_name
      when "Void", "Void*", "Pointer(Void)", "Pointer"
        TypeRef::POINTER
      when "Int8", "SChar"
        TypeRef::INT8
      when "UInt8", "Char", "UChar"
        TypeRef::UINT8
      when "Int16", "Short"
        TypeRef::INT16
      when "UInt16", "UShort"
        TypeRef::UINT16
      when "Int32", "Int"
        TypeRef::INT32
      when "UInt32", "UInt"
        TypeRef::UINT32
      when "Int64", "Long", "LongLong"
        TypeRef::INT64
      when "UInt64", "ULong", "ULongLong", "SizeT"
        TypeRef::UINT64
      when "Float32", "Float"
        TypeRef::FLOAT32
      when "Float64", "Double"
        TypeRef::FLOAT64
      when "Bool"
        TypeRef::BOOL
      when "NoReturn"
        TypeRef::VOID  # NoReturn functions still have void return in LLVM
      when .ends_with?("*")
        TypeRef::POINTER
      when .starts_with?("Pointer(")
        TypeRef::POINTER
      else
        TypeRef::POINTER  # Default to pointer for unknown types
      end
    end

    # ═══════════════════════════════════════════════════════════════════════
    # METAPROGRAMMING (typeof, sizeof, etc.)
    # ═══════════════════════════════════════════════════════════════════════

    private def lower_typeof(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::TypeofNode) : ValueId
      # typeof(x) returns the type of x at compile time
      # At runtime, we evaluate the expressions for side effects and return a type placeholder
      # For now, just lower the args and return a nil (type info is compile-time only)
      node.args.each do |arg_id|
        lower_expr(ctx, arg_id)
      end
      nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
      ctx.emit(nil_lit)
      nil_lit.id
    end

    private def lower_sizeof(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::SizeofNode) : ValueId
      # sizeof(T) returns the size of type T in bytes
      # For basic types, we can compute this at compile time
      size = 8_i64  # Default pointer size
      if node.args.size > 0
        type_node = @arena[node.args.first]
        size = compute_type_size(type_node)
      end
      lit = Literal.new(ctx.next_id, TypeRef::INT32, size)
      ctx.emit(lit)
      lit.id
    end

    private def lower_offsetof(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::OffsetofNode) : ValueId
      # offsetof(T, @field) returns the byte offset of a field
      offset = 0_i64  # Default
      lit = Literal.new(ctx.next_id, TypeRef::INT32, offset)
      ctx.emit(lit)
      lit.id
    end

    private def compute_type_size(type_node : CrystalV2::Compiler::Frontend::Node) : Int64
      case type_node
      when CrystalV2::Compiler::Frontend::IdentifierNode
        name = String.new(type_node.name)
        case name
        when "Int8", "UInt8", "Bool" then 1_i64
        when "Int16", "UInt16"       then 2_i64
        when "Int32", "UInt32", "Float32", "Char" then 4_i64
        when "Int64", "UInt64", "Float64" then 8_i64
        when "Int128", "UInt128"     then 16_i64
        else 8_i64  # Pointer/reference size
        end
      when CrystalV2::Compiler::Frontend::ConstantNode
        name = String.new(type_node.name)
        case name
        when "Int8", "UInt8", "Bool" then 1_i64
        when "Int16", "UInt16"       then 2_i64
        when "Int32", "UInt32", "Float32", "Char" then 4_i64
        when "Int64", "UInt64", "Float64" then 8_i64
        when "Int128", "UInt128"     then 16_i64
        else 8_i64
        end
      else
        8_i64  # Default pointer size
      end
    end

    # Lower top-level method definition
    private def lower_top_level_def(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::DefNode) : ValueId
      # Top-level methods are global functions
      method_name = String.new(node.name)

      # Skip if already registered
      if @function_defs.has_key?(method_name)
        nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
        ctx.emit(nil_lit)
        return nil_lit.id
      end

      # Determine return type
      return_type = if rt = node.return_type
                      type_ref_for_name(String.new(rt))
                    elsif method_name.ends_with?("?")
                      TypeRef::BOOL
                    else
                      TypeRef::VOID
                    end

      # Create function
      func = @module.create_function(method_name, return_type)
      @function_defs[method_name] = node
      @function_def_arenas[method_name] = @arena

      # Register return type
      register_function_type(method_name, return_type)

      # Method definitions don't produce a value
      nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
      ctx.emit(nil_lit)
      nil_lit.id
    end

    # Lower pointerof(x) to get address of a variable
    private def lower_pointerof(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::PointerofNode) : ValueId
      # pointerof takes one argument - the variable/expression to get address of
      if node.args.empty?
        # Return null pointer if no args
        nil_lit = Literal.new(ctx.next_id, TypeRef::POINTER, nil)
        ctx.emit(nil_lit)
        return nil_lit.id
      end

      # Lower the operand (the thing we're getting a pointer to)
      operand_id = lower_expr(ctx, node.args.first)

      # Create AddressOf instruction
      addr_of = AddressOf.new(ctx.next_id, TypeRef::POINTER, operand_id)
      ctx.emit(addr_of)
      ctx.register_type(addr_of.id, TypeRef::POINTER)
      addr_of.id
    end

    # Emit a call to an external C function
    private def emit_extern_call(ctx : LoweringContext, extern_func : ExternFunction, arg_ids : Array(ValueId)) : ValueId
      # Emit an ExternCall instruction with the real C function name
      extern_call = ExternCall.new(
        ctx.next_id,
        extern_func.return_type,
        extern_func.real_name,  # Use the real C name
        arg_ids,
        extern_func.varargs
      )
      ctx.emit(extern_call)
      ctx.register_type(extern_call.id, extern_func.return_type)
      extern_call.id
    end

    # Resolve annotation name from ExprId to string
    private def resolve_annotation_name(name_expr : CrystalV2::Compiler::Frontend::ExprId) : String
      name_node = @arena[name_expr]
      case name_node
      when CrystalV2::Compiler::Frontend::IdentifierNode
        String.new(name_node.name)
      when CrystalV2::Compiler::Frontend::PathNode
        # For paths like JSON::Field, extract the last part
        resolve_annotation_name(name_node.right)
      else
        "Unknown"
      end
    end

    private def lower_string_interpolation(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::StringInterpolationNode) : ValueId
      # String interpolation "Hello #{x}!" becomes:
      # 1. Build string by concatenating parts
      # 2. For each Text piece: create string literal
      # 3. For each Expression piece: convert to string and concat
      #
      # For now, implement simple version that calls __crystal_v2_string_interpolate
      # with all parts as arguments

      parts = [] of ValueId

      node.pieces.each do |piece|
        case piece.kind
        when .text?
          # Simple text piece - create string literal
          text = piece.text || ""
          lit = Literal.new(ctx.next_id, TypeRef::STRING, text)
          ctx.emit(lit)
          parts << lit.id
        when .expression?
          # Expression piece - lower the expression
          if expr_id = piece.expr
            val_id = lower_expr(ctx, expr_id)
            parts << val_id
          end
        end
      end

      # For simple case with one text part, return it directly
      if parts.size == 1 && node.pieces.first?.try(&.kind.text?)
        return parts.first
      end

      # Create StringInterpolation HIR instruction
      interp = StringInterpolation.new(ctx.next_id, parts)
      ctx.emit(interp)
      interp.id
    end

    private def lower_char(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::CharNode) : ValueId
      # Convert Slice(UInt8) to Char - first char of the slice
      char_value = String.new(node.value)[0]? || '\0'
      lit = Literal.new(ctx.next_id, TypeRef::CHAR, char_value)
      ctx.emit(lit)
      lit.id
    end

    private def lower_bool(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::BoolNode) : ValueId
      lit = Literal.new(ctx.next_id, TypeRef::BOOL, node.value)
      ctx.emit(lit)
      lit.id
    end

    private def lower_nil(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::NilNode) : ValueId
      lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
      ctx.emit(lit)
      lit.id
    end

    private def lower_symbol(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::SymbolNode) : ValueId
      str = String.new(node.name)
      lit = Literal.new(ctx.next_id, TypeRef::SYMBOL, str)
      ctx.emit(lit)
      lit.id
    end

    # ═══════════════════════════════════════════════════════════════════════
    # VARIABLE LOWERING
    # ═══════════════════════════════════════════════════════════════════════

    private def lower_identifier(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::IdentifierNode) : ValueId
      name = String.new(node.name)

      # Check if it's a local variable first
      if local_id = ctx.lookup_local(name)
        # Return a copy/reference to the local
        copy = Copy.new(ctx.next_id, ctx.type_of(local_id), local_id)
        ctx.emit(copy)
        return copy.id
      end

      # Inside a class: check if it's a method call on self (e.g., getter without parens)
      if current_class = @current_class
        if name == "ord" && current_class == "Char"
          self_id = emit_self(ctx)
          cast = Cast.new(ctx.next_id, TypeRef::INT32, self_id, TypeRef::INT32)
          ctx.emit(cast)
          ctx.register_type(cast.id, TypeRef::INT32)
          return cast.id
        end

        if name == "unsafe_chr"
          case current_class
          when "Int8", "Int16", "Int32", "Int64", "Int128",
               "UInt8", "UInt16", "UInt32", "UInt64", "UInt128"
            self_id = emit_self(ctx)
            cast = Cast.new(ctx.next_id, TypeRef::CHAR, self_id, TypeRef::CHAR)
            ctx.emit(cast)
            ctx.register_type(cast.id, TypeRef::CHAR)
            return cast.id
          end
        end

        # Check if method exists in current class (with inheritance)
        class_method_base = resolve_method_with_inheritance(current_class, name)
        if class_method_base
          # This is a method call on self with no arguments
          self_id = emit_self(ctx)
          full_name = mangle_function_name(class_method_base, [] of TypeRef)
          return_type = @function_types[full_name]? || TypeRef::VOID
          lower_function_if_needed(full_name)
          call = Call.new(ctx.next_id, return_type, self_id, full_name, [] of ValueId)
          ctx.emit(call)
          ctx.register_type(call.id, return_type)
          return call.id
        end

        # Module/class method call without parens (e.g., class_getter inside module)
        module_method_base = "#{current_class}.#{name}"
        if @function_types.has_key?(module_method_base) || has_function_base?(module_method_base) || @class_accessor_entries.has_key?(module_method_base)
          full_name = mangle_function_name(module_method_base, [] of TypeRef)
          return_type = @function_types[full_name]? || @function_types[module_method_base]? || TypeRef::VOID
          lower_function_if_needed(full_name)
          call = Call.new(ctx.next_id, return_type, nil, full_name, [] of ValueId)
          ctx.emit(call)
          ctx.register_type(call.id, return_type)
          return call.id
        end
      end

      # Top-level: treat `main` as a function call when a top-level main is defined.
      if @current_class.nil? && name == "main" && @top_level_main_defined
        full_name = mangle_function_name(TOP_LEVEL_MAIN_BASE, [] of TypeRef)
        return_type = @function_types[full_name]? || TypeRef::VOID
        lower_function_if_needed(full_name)
        call = Call.new(ctx.next_id, return_type, nil, full_name, [] of ValueId)
        ctx.emit(call)
        ctx.register_type(call.id, return_type)
        return call.id
      end

      # Otherwise create a new local (first use)
      local = Local.new(ctx.next_id, TypeRef::VOID, name, ctx.current_scope)
      ctx.emit(local)
      ctx.register_local(name, local.id)
      local.id
    end

    private def lower_instance_var(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::InstanceVarNode) : ValueId
      name = String.new(node.name)

      # Get the type and offset of the instance variable from current class
      ivar_type = TypeRef::VOID
      ivar_offset = 0
      if class_name = @current_class
        if class_info = @class_info[class_name]?
          class_info.ivars.each do |ivar|
            if ivar.name == name
              ivar_type = ivar.type
              ivar_offset = ivar.offset
              break
            end
          end
          # Debug: log ivar lookup results (commented out for performance)
          # if ENV.has_key?("DEBUG_IVAR") && ivar_type == TypeRef::VOID
          #   ivar_names = class_info.ivars.map { |iv| "#{iv.name}:#{iv.type.id}" }.join(", ")
          #   found_ivar = class_info.ivars.find { |iv| iv.name == name }
          #   if found_ivar
          #     STDERR.puts "[IVAR] #{class_name}##{name} FOUND but type is VOID (id=#{found_ivar.type.id})"
          #   else
          #     STDERR.puts "[IVAR] #{class_name}##{name} NOT FOUND in ivars: [#{ivar_names}]"
          #   end
          # end
        end
      end

      # If ivar type is still VOID, try to find a getter method as fallback
      # This handles cases where ivars are defined implicitly (e.g., @ivar = value in initialize)
      if ivar_type == TypeRef::VOID && (class_name = @current_class)
        # Try to find getter method: @bytesize -> bytesize()
        accessor_name = name.lchop('@')
        # IMPORTANT: Don't call getter if we're inside that getter method!
        # This prevents infinite recursion in `def x; @x; end`
        current_method_name = @current_method
        is_inside_getter = current_method_name && current_method_name == accessor_name

        unless is_inside_getter
          getter_base = resolve_method_with_inheritance(class_name, accessor_name)
          if getter_base
            # Found a getter method - emit as method call instead of field get
            self_id = emit_self(ctx)
            full_name = mangle_function_name(getter_base, [] of TypeRef)
            return_type = @function_types[full_name]? || TypeRef::VOID
            call = Call.new(ctx.next_id, return_type, self_id, full_name, [] of ValueId)
            ctx.emit(call)
            ctx.register_type(call.id, return_type)
            return call.id
          end
        end
      end

      # Instance var access is a field get on self
      self_id = emit_self(ctx)
      field_get = FieldGet.new(ctx.next_id, ivar_type, self_id, name, ivar_offset)
      ctx.emit(field_get)
      ctx.register_type(field_get.id, ivar_type)  # Register type for is_a?/case checks
      field_get.id
    end

    private def lower_class_var(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::ClassVarNode) : ValueId
      # Name includes @@ prefix, strip it
      raw_name = String.new(node.name)
      name = raw_name.lstrip('@')
      cvar_type = get_class_var_type(name)
      class_name = @current_class || ""
      class_var_get = ClassVarGet.new(ctx.next_id, cvar_type, class_name, name)
      ctx.emit(class_var_get)
      ctx.register_type(class_var_get.id, cvar_type)
      class_var_get.id
    end

    private def lower_self(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::SelfNode) : ValueId
      emit_self(ctx)
    end

    # Lower implicit object (like .foo syntax) - treated as self
    private def lower_self_implicit(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::ImplicitObjNode) : ValueId
      emit_self(ctx)
    end

    # Lower super call - calls parent class method with same name
    private def lower_super(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::SuperNode) : ValueId
      class_name = @current_class
      method_name = @current_method

      unless class_name && method_name
        # Fallback: return void
        void_lit = Literal.new(ctx.next_id, TypeRef::VOID, 0_i64)
        ctx.emit(void_lit)
        return void_lit.id
      end

      # Find parent class
      class_info = @class_info[class_name]?
      parent_name = class_info.try(&.parent_name)

      unless parent_name
        # No parent - return void
        void_lit = Literal.new(ctx.next_id, TypeRef::VOID, 0_i64)
        ctx.emit(void_lit)
        return void_lit.id
      end

      # Lower arguments first to get their types
      args = if node_args = node.args
               node_args.map { |arg| lower_expr(ctx, arg) }
             else
               # If no args, forward current method's parameters
               # Get them from the function context (skip 'self' param at index 0)
               ctx.function.params[1..].map(&.id)
             end

      # Get argument types for mangling
      arg_types = args.map { |arg| ctx.type_of(arg) }

      # Find the method in parent class with proper mangling
      base_method_name = "#{parent_name}##{method_name}"
      super_method_name = mangle_function_name(base_method_name, arg_types)

      # Try to find the actual function definition - it might have more parameters with defaults
      actual_func_name = super_method_name
      actual_func_def : CrystalV2::Compiler::Frontend::DefNode? = nil

      # First try exact match
      if func_def = @function_defs[super_method_name]?
        actual_func_def = func_def
      elsif func_def = @function_defs[base_method_name]?
        actual_func_def = func_def
        actual_func_name = base_method_name
      else
        # Search for mangled variant (method with more params than we're passing)
        mangled_prefix = "#{base_method_name}$"
        @function_defs.each_key do |key|
          if key.starts_with?(mangled_prefix)
            actual_func_def = @function_defs[key]
            actual_func_name = key
            break
          end
        end
      end

      # If we found a method with more parameters, fill in defaults for missing args
      if actual_func_def && actual_func_name != super_method_name
        if params = actual_func_def.params
          # Count non-block, non-separator parameters
          param_count = params.count { |p| !p.is_block && !named_only_separator?(p) }

          if param_count > args.size
            # Need to fill in default values for missing arguments
            param_idx = 0
            params.each do |param|
              next if param.is_block || named_only_separator?(param)
              if param_idx >= args.size
                # This parameter needs a default value
                if default_val = param.default_value
                  default_id = lower_expr(ctx, default_val)
                  args << default_id
                else
                  # No default - use nil
                  nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
                  ctx.emit(nil_lit)
                  ctx.register_type(nil_lit.id, TypeRef::NIL)
                  args << nil_lit.id
                end
              end
              param_idx += 1
            end
          end
        end
        super_method_name = actual_func_name
      end

      # Get return type from mangled name
      return_type = @function_types[super_method_name]? || TypeRef::VOID

      # Get self for the call
      self_id = emit_self(ctx)

      # Ensure parent method is lowered
      if ENV.has_key?("DEBUG_SUPER")
        STDERR.puts "[DEBUG_SUPER] lower_super: class=#{class_name} method=#{method_name}"
        STDERR.puts "[DEBUG_SUPER]   parent=#{parent_name}"
        STDERR.puts "[DEBUG_SUPER]   base_method_name=#{base_method_name}"
        STDERR.puts "[DEBUG_SUPER]   super_method_name=#{super_method_name}"
        STDERR.puts "[DEBUG_SUPER]   function_defs.has_key?(super)=#{@function_defs.has_key?(super_method_name)}"
        STDERR.puts "[DEBUG_SUPER]   function_defs.has_key?(base)=#{@function_defs.has_key?(base_method_name)}"
      end
      lower_function_if_needed(super_method_name)

      # Call parent method
      call = Call.new(ctx.next_id, return_type, self_id, super_method_name, args)
      ctx.emit(call)
      ctx.register_type(call.id, return_type)
      call.id
    end

    private def lower_previous_def(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::PreviousDefNode) : ValueId
      # previous_def calls the previous definition of the current method
      # This is used when reopening classes/methods
      class_name = @current_class
      method_name = @current_method

      unless class_name && method_name
        # No current method context - return nil
        nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
        ctx.emit(nil_lit)
        return nil_lit.id
      end

      # Lower arguments first to get their types
      args = if node_args = node.args
               node_args.map { |arg| lower_expr(ctx, arg) }
             else
               # If no args, forward current method's parameters
               # Get them from the function context (skip 'self' param at index 0)
               ctx.function.params[1..].map(&.id)
             end

      # Get argument types for mangling
      arg_types = args.map { |arg| ctx.type_of(arg) }

      # For previous_def, we call the same method with a _previous suffix
      # The actual linking will resolve this (or fail if no previous def exists)
      base_method_name = "#{class_name}##{method_name}_previous"
      previous_method_name = mangle_function_name(base_method_name, arg_types)

      # Get return type (fallback to VOID if unknown)
      return_type = @function_types[previous_method_name]? || TypeRef::VOID

      # Get self for the call
      self_id = emit_self(ctx)

      # Call previous method definition
      call = Call.new(ctx.next_id, return_type, self_id, previous_method_name, args)
      ctx.emit(call)
      ctx.register_type(call.id, return_type)
      call.id
    end

    # Collect full path string from PathNode (e.g., Foo::Bar::Baz -> "Foo::Bar::Baz")
    private def collect_path_string(node : CrystalV2::Compiler::Frontend::PathNode) : String
      parts = [] of String

      # Get left part recursively
      if left_id = node.left
        left_node = @arena[left_id]
        case left_node
        when CrystalV2::Compiler::Frontend::PathNode
          # Recursively collect left path
          parts << collect_path_string(left_node)
        when CrystalV2::Compiler::Frontend::IdentifierNode
          parts << String.new(left_node.name)
        when CrystalV2::Compiler::Frontend::ConstantNode
          parts << String.new(left_node.name)
        end
      end

      # Get right part
      right_node = @arena[node.right]
      case right_node
      when CrystalV2::Compiler::Frontend::IdentifierNode
        parts << String.new(right_node.name)
      when CrystalV2::Compiler::Frontend::ConstantNode
        parts << String.new(right_node.name)
      end

      parts.join("::")
    end

    # Lower path expression (e.g., Color::Green for enums, or Module::Constant)
    private def lower_path(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::PathNode) : ValueId
      # Extract left and right parts
      # For Color::Green: left = Color (IdentifierNode), right = Green (IdentifierNode)
      left_name : String? = nil
      if left_id = node.left
        left_node = @arena[left_id]
        left_name = case left_node
                    when CrystalV2::Compiler::Frontend::IdentifierNode
                      String.new(left_node.name)
                    when CrystalV2::Compiler::Frontend::ConstantNode
                      String.new(left_node.name)
                    when CrystalV2::Compiler::Frontend::PathNode
                      # Nested path like A::B::C - recurse to get full path
                      # For now, just get the rightmost part
                      right_of_left = @arena[left_node.right]
                      case right_of_left
                      when CrystalV2::Compiler::Frontend::IdentifierNode
                        String.new(right_of_left.name)
                      else
                        nil
                      end
                    else
                      nil
                    end
      end

      right_node = @arena[node.right]
      right_name = case right_node
                   when CrystalV2::Compiler::Frontend::IdentifierNode
                     String.new(right_node.name)
                   when CrystalV2::Compiler::Frontend::ConstantNode
                     String.new(right_node.name)
                   else
                     nil
                   end

      # Check if this is an enum value access
      if left_name && right_name
        if enum_info = @enum_info
          if members = enum_info[left_name]?
            if value = members[right_name]?
              # Found enum value - emit as Int32 literal
              lit = Literal.new(ctx.next_id, TypeRef::INT32, value)
              ctx.emit(lit)
              return lit.id
            end
          end
        end
      end

      # Fallback: treat as constant or module access (for future expansion)
      # For now, just return 0
      lit = Literal.new(ctx.next_id, TypeRef::INT32, 0_i64)
      ctx.emit(lit)
      lit.id
    end

    private def lower_global(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::GlobalNode) : ValueId
      name = String.new(node.name)
      # Global variables are like class vars at top level
      class_var_get = ClassVarGet.new(ctx.next_id, TypeRef::VOID, "$", name)
      class_var_get.lifetime = LifetimeTag::GlobalEscape
      ctx.emit(class_var_get)
      class_var_get.id
    end

    private def emit_self(ctx : LoweringContext) : ValueId
      # Check if we have a 'self' local
      if self_id = ctx.lookup_local("self")
        return self_id
      end

      # Create implicit self parameter
      local = Local.new(ctx.next_id, TypeRef::VOID, "self", ctx.current_scope, mutable: false)
      ctx.emit(local)
      ctx.register_local("self", local.id)
      local.id
    end

    # Lower a generic type reference like Array(Int32), Hash(String, Int32)
    # This is used when calling static methods like Array(Int32).new
    private def lower_generic_type_ref(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::GenericNode) : ValueId
      # Extract base type name
      base_name = resolve_path_like_name(node.base_type) || "Unknown"
      base_name = resolve_type_alias_chain(base_name)

      # Extract type arguments, substituting any type parameters
      normalize_typeof_name = ->(type_name : String) : String {
        if type_name == "Void" || type_name == "Unknown" || type_name.includes?("|")
          "Pointer(Void)"
        else
          type_name
        end
      }

      type_args = node.type_args.map do |arg_id|
        arg_node = @arena[arg_id]
        arg_name = case arg_node
                   when CrystalV2::Compiler::Frontend::TypeofNode
                     inner = arg_node.args.first?
                     inner ? resolve_typeof_expr(inner) : "Pointer(Void)"
                   else
                     stringify_type_expr(arg_id) || "Unknown"
                   end
        arg_name = resolve_typeof_in_type_string(arg_name)
        arg_name = normalize_typeof_name.call(arg_name)
        # Substitute type parameter if applicable
        @type_param_map[arg_name]? || arg_name
      end

      # Create specialized class name like Array(Int32)
      class_name = "#{base_name}(#{type_args.join(", ")})"

      # Monomorphize if needed
      if !@monomorphized.includes?(class_name)
        monomorphize_generic_class(base_name, type_args, class_name)
      end

      # Return a type reference literal (for use as receiver in static calls)
      # We use a nil literal with special type tracking
      type_ref = @class_info[class_name]?.try(&.type_ref) || TypeRef::VOID
      lit = Literal.new(ctx.next_id, type_ref, nil)
      ctx.emit(lit)
      ctx.register_type(lit.id, type_ref)
      lit.id
    end

    # ═══════════════════════════════════════════════════════════════════════
    # BINARY/UNARY OPERATIONS
    # ═══════════════════════════════════════════════════════════════════════

    private def lower_binary(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::BinaryNode) : ValueId
      op_str = node.operator_string
      if op_str == "&&" || op_str == "||"
        return lower_short_circuit(ctx, node, op_str)
      end

      left_id = lower_expr(ctx, node.left)
      right_id = lower_expr(ctx, node.right)

      # Check for pointer arithmetic: ptr + n or ptr - n
      left_type = ctx.type_of(left_id)
      if left_type == TypeRef::POINTER && (op_str == "+" || op_str == "-")
        offset_id = right_id
        # For subtraction, negate the offset
        if op_str == "-"
          neg_one = Literal.new(ctx.next_id, TypeRef::INT32, -1_i64)
          ctx.emit(neg_one)
          ctx.register_type(neg_one.id, TypeRef::INT32)
          neg_offset = BinaryOperation.new(ctx.next_id, TypeRef::INT32, BinaryOp::Mul, offset_id, neg_one.id)
          ctx.emit(neg_offset)
          ctx.register_type(neg_offset.id, TypeRef::INT32)
          offset_id = neg_offset.id
        end
        element_type = TypeRef::INT32  # TODO: track pointer element type
        add_node = PointerAdd.new(ctx.next_id, TypeRef::POINTER, left_id, offset_id, element_type)
        ctx.emit(add_node)
        ctx.register_type(add_node.id, TypeRef::POINTER)
        return add_node.id
      end

      # Check for string concatenation: String + String -> StringConcat
      left_type = ctx.type_of(left_id)
      if left_type == TypeRef::STRING && op_str == "+"
        # String concatenation - emit as StringInterpolation with two parts
        interp = StringInterpolation.new(ctx.next_id, [left_id, right_id])
        ctx.emit(interp)
        return interp.id
      end

      # Check for string repetition: String * Int -> __crystal_v2_string_repeat
      if (left_type == TypeRef::STRING || left_type == TypeRef::POINTER) && op_str == "*"
        # String repetition - emit as runtime call
        call = Call.new(ctx.next_id, TypeRef::POINTER, nil, "__crystal_v2_string_repeat", [left_id, right_id])
        ctx.emit(call)
        ctx.register_type(call.id, TypeRef::POINTER)
        return call.id
      end

      # Check for shovel operator << on non-integer types (IO, Array, etc.)
      # If left operand is pointer or union type and operator is <<, emit as method call
      # This handles io << value and arr << elem, which are NOT bit-shift but append
      is_integer_type = left_type.id >= TypeRef::INT8.id && left_type.id <= TypeRef::INT128.id
      if !is_integer_type && op_str == "<<"
        # Emit as method call: left.<<(right)
        # The method is registered as "#<<" in the function_types
        ensure_monomorphized_type(left_type) unless left_type == TypeRef::VOID
        right_type = ctx.type_of(right_id)
        type_desc = @module.get_type_descriptor(left_type)
        class_name = type_desc.try(&.name) || ""
        base_method_name = class_name.empty? ? "<<" : "#{class_name}#<<"
        primary_mangled_name = mangle_function_name(base_method_name, [right_type])
        # Debug: log the resolution attempt
        if ENV.has_key?("DEBUG_SHOVEL")
          type_desc = @module.get_type_descriptor(left_type)
          STDERR.puts "[SHOVEL] left_type=#{left_type.id}, type_desc=#{type_desc.try(&.name) || "nil"}, right_type=#{right_type.id}"
        end
        method_name = resolve_method_call(ctx, left_id, "<<", [right_type])
        if ENV.has_key?("DEBUG_SHOVEL")
          STDERR.puts "[SHOVEL] resolved to: #{method_name}"
        end
        remember_callsite_arg_types(primary_mangled_name, [right_type])
        if method_name != primary_mangled_name
          remember_callsite_arg_types(method_name, [right_type])
        end
        lower_function_if_needed(primary_mangled_name)
        if method_name != primary_mangled_name
          lower_function_if_needed(method_name)
        end
        call = Call.new(ctx.next_id, left_type, left_id, method_name, [right_id])
        ctx.emit(call)
        ctx.register_type(call.id, left_type)
        return call.id
      end

      op = case op_str
           when "+"   then BinaryOp::Add
           when "-"   then BinaryOp::Sub
           when "*"   then BinaryOp::Mul
           when "/"   then BinaryOp::Div
           when "//"  then BinaryOp::Div  # Floor division - use Div (truncation) for now
           when "%"   then BinaryOp::Mod
           when "&"   then BinaryOp::BitAnd
           when "|"   then BinaryOp::BitOr
           when "^"   then BinaryOp::BitXor
           when "<<"  then BinaryOp::Shl
           when ">>"  then BinaryOp::Shr
           when "=="  then BinaryOp::Eq
           when "!="  then BinaryOp::Ne
           when "<"   then BinaryOp::Lt
           when "<="  then BinaryOp::Le
           when ">"   then BinaryOp::Gt
           when ">="  then BinaryOp::Ge
           # Wrapping operators - map to same ops (LLVM integer ops already wrap)
           when "&+"  then BinaryOp::Add
           when "&-"  then BinaryOp::Sub
           when "&*"  then BinaryOp::Mul
           else
             # Unknown operator - emit as method call
             return emit_binary_call(ctx, left_id, op_str, right_id)
           end

      result_type = if op.eq? || op.ne? || op.lt? || op.le? || op.gt? || op.ge? || op.and? || op.or?
                      TypeRef::BOOL
                    else
                      # For arithmetic ops, infer type from left operand
                      ctx.type_of(left_id)
                    end

      binop = BinaryOperation.new(ctx.next_id, result_type, op, left_id, right_id)
      ctx.emit(binop)
      binop.id
    end

    # Lower short-circuiting || and && with value semantics (returns last evaluated value).
    private def lower_short_circuit(
      ctx : LoweringContext,
      node : CrystalV2::Compiler::Frontend::BinaryNode,
      op_str : String
    ) : ValueId
      left_id = lower_expr(ctx, node.left)
      left_type = ctx.type_of(left_id)

      cond_id = lower_truthy_check(ctx, left_id, left_type)

      pre_branch_locals = ctx.save_locals

      then_block = ctx.create_block
      else_block = ctx.create_block
      merge_block = ctx.create_block

      ctx.terminate(Branch.new(cond_id, then_block, else_block))

      # then: left is truthy
      ctx.current_block = then_block
      ctx.restore_locals(pre_branch_locals)
      then_value = if op_str == "||"
                     left_id
                   else
                     lower_expr(ctx, node.right)
                   end
      then_exit = ctx.current_block
      then_locals = ctx.save_locals
      then_block_data = ctx.get_block(ctx.current_block)
      then_has_noreturn = then_block_data.instructions.any? { |inst| inst.is_a?(Raise) }
      then_flows_to_merge = then_block_data.terminator.is_a?(Unreachable) && !then_has_noreturn
      if then_flows_to_merge
        ctx.terminate(Jump.new(merge_block))
      end

      # else: left is falsy
      ctx.current_block = else_block
      ctx.restore_locals(pre_branch_locals)
      else_value = if op_str == "||"
                     lower_expr(ctx, node.right)
                   else
                     left_id
                   end
      else_exit = ctx.current_block
      else_locals = ctx.save_locals
      else_block_data = ctx.get_block(ctx.current_block)
      else_has_noreturn = else_block_data.instructions.any? { |inst| inst.is_a?(Raise) }
      else_flows_to_merge = else_block_data.terminator.is_a?(Unreachable) && !else_has_noreturn
      if else_flows_to_merge
        ctx.terminate(Jump.new(merge_block))
      end

      ctx.current_block = merge_block

      if then_flows_to_merge || else_flows_to_merge
        if then_flows_to_merge && else_flows_to_merge
          # Merge locals and result value
          merge_branch_locals(ctx, pre_branch_locals, then_locals, else_locals, then_exit, else_exit)

          then_type = ctx.type_of(then_value)
          else_type = ctx.type_of(else_value)
          phi_type = union_type_for_values(then_type, else_type)

          if phi_type == TypeRef::VOID || phi_type == TypeRef::NIL
            nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
            ctx.emit(nil_lit)
            return nil_lit.id
          end

          phi = Phi.new(ctx.next_id, phi_type)
          phi.add_incoming(then_exit, then_value)
          phi.add_incoming(else_exit, else_value)
          ctx.emit(phi)
          ctx.register_type(phi.id, phi_type)
          return phi.id
        elsif then_flows_to_merge
          then_locals.each { |name, val| ctx.register_local(name, val) }
          return then_value
        else
          else_locals.each { |name, val| ctx.register_local(name, val) }
          return else_value
        end
      end

      nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
      ctx.emit(nil_lit)
      nil_lit.id
    end

    private def union_type_for_values(left_type : TypeRef, right_type : TypeRef) : TypeRef
      return left_type if left_type == right_type
      left_name = get_type_name_from_ref(left_type)
      right_name = get_type_name_from_ref(right_type)
      create_union_type("#{left_name} | #{right_name}")
    end

    private def lower_truthy_check(ctx : LoweringContext, value_id : ValueId, value_type : TypeRef) : ValueId
      if value_type == TypeRef::BOOL
        return value_id
      end

      if value_type == TypeRef::NIL || value_type == TypeRef::VOID
        lit = Literal.new(ctx.next_id, TypeRef::BOOL, false)
        ctx.emit(lit)
        return lit.id
      end

      if value_type == TypeRef::POINTER
        nil_val = Literal.new(ctx.next_id, TypeRef::POINTER, 0_i64)
        ctx.emit(nil_val)
        ne_check = BinaryOperation.new(ctx.next_id, TypeRef::BOOL, BinaryOp::Ne, value_id, nil_val.id)
        ctx.emit(ne_check)
        return ne_check.id
      end

      if is_union_or_nilable_type?(value_type)
        is_nil = lower_nil_check_intrinsic(ctx, value_id, value_type)
        not_nil = UnaryOperation.new(ctx.next_id, TypeRef::BOOL, UnaryOp::Not, is_nil)
        ctx.emit(not_nil)
        ctx.register_type(not_nil.id, TypeRef::BOOL)
        return not_nil.id
      end

      # Non-nilable, non-bool types are always truthy.
      lit = Literal.new(ctx.next_id, TypeRef::BOOL, true)
      ctx.emit(lit)
      lit.id
    end

    private def emit_binary_call(ctx : LoweringContext, left : ValueId, op : String, right : ValueId) : ValueId
      # Qualify method name with receiver's class
      right_type = ctx.type_of(right)
      left_type = ctx.type_of(left)
      ensure_monomorphized_type(left_type) unless left_type == TypeRef::VOID
      type_desc = @module.get_type_descriptor(left_type)
      class_name = type_desc.try(&.name) || ""
      base_method_name = class_name.empty? ? op : "#{class_name}##{op}"
      primary_mangled_name = mangle_function_name(base_method_name, [right_type])
      method_name = resolve_method_call(ctx, left, op, [right_type])
      remember_callsite_arg_types(primary_mangled_name, [right_type])
      if method_name != primary_mangled_name
        remember_callsite_arg_types(method_name, [right_type])
      end
      lower_function_if_needed(primary_mangled_name)
      if method_name != primary_mangled_name
        lower_function_if_needed(method_name)
      end
      call = Call.new(ctx.next_id, TypeRef::VOID, left, method_name, [right])
      ctx.emit(call)
      call.id
    end

    private def lower_unary(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::UnaryNode) : ValueId
      operand_id = lower_expr(ctx, node.operand)

      op_str = String.new(node.operator)
      op = case op_str
           when "-" then UnaryOp::Neg
           when "!" then UnaryOp::Not
           when "~" then UnaryOp::BitNot
           else
             # Unknown unary op - emit as method call
             # Qualify method name with receiver's class
             operand_type = ctx.type_of(operand_id)
             ensure_monomorphized_type(operand_type) unless operand_type == TypeRef::VOID
             type_desc = @module.get_type_descriptor(operand_type)
             class_name = type_desc.try(&.name) || ""
             base_method_name = class_name.empty? ? op_str : "#{class_name}##{op_str}"
             primary_mangled_name = mangle_function_name(base_method_name, [] of TypeRef)
             method_name = resolve_method_call(ctx, operand_id, op_str, [] of TypeRef)
             remember_callsite_arg_types(primary_mangled_name, [] of TypeRef)
             if method_name != primary_mangled_name
               remember_callsite_arg_types(method_name, [] of TypeRef)
             end
             lower_function_if_needed(primary_mangled_name)
             if method_name != primary_mangled_name
               lower_function_if_needed(method_name)
             end
             call = Call.new(ctx.next_id, TypeRef::VOID, operand_id, method_name, [] of ValueId)
             ctx.emit(call)
             return call.id
           end

      # Determine result type based on operation
      result_type = case op
                    when UnaryOp::Not
                      TypeRef::BOOL
                    when UnaryOp::Neg, UnaryOp::BitNot
                      # Negation and bitwise not preserve operand type
                      ctx.type_of(operand_id)
                    else
                      ctx.type_of(operand_id)
                    end
      unop = UnaryOperation.new(ctx.next_id, result_type, op, operand_id)
      ctx.emit(unop)
      unop.id
    end

    # ═══════════════════════════════════════════════════════════════════════
    # CONTROL FLOW
    # ═══════════════════════════════════════════════════════════════════════

    private def lower_if(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::IfNode) : ValueId
      merge_block = ctx.create_block

      # Save locals state before branching
      pre_branch_locals = ctx.save_locals

      # Collect all branches: (exit_block, value, locals, flows_to_merge)
      branches = [] of {BlockId, ValueId, Hash(String, ValueId), Bool}

      # Build the chain: if -> elsif1 -> elsif2 -> ... -> else
      # Each test that fails jumps to the next test block (or final else block)
      elsifs = node.elsifs
      has_elsifs = elsifs && !elsifs.empty?

      # Create blocks for the chain
      then_block = ctx.create_block
      next_test_block = if has_elsifs
                          ctx.create_block  # First elsif test
                        else
                          ctx.create_block  # Direct else block
                        end

      # Lower main condition and branch
      cond_id = lower_expr(ctx, node.condition)
      ctx.terminate(Branch.new(cond_id, then_block, next_test_block))

      # Process "then" branch
      ctx.current_block = then_block
      ctx.push_scope(ScopeKind::Block)
      then_value = lower_body(ctx, node.then_body)
      then_exit_block = ctx.current_block
      then_locals = ctx.save_locals
      ctx.pop_scope

      then_block_data = ctx.get_block(ctx.current_block)
      then_has_noreturn = then_block_data.instructions.any? { |inst| inst.is_a?(Raise) || inst.is_a?(Return) }
      then_flows_to_merge = then_block_data.terminator.is_a?(Unreachable) && !then_has_noreturn
      if then_flows_to_merge
        ctx.terminate(Jump.new(merge_block))
      end
      branches << {then_exit_block, then_value, then_locals, then_flows_to_merge}

      # Process elsif branches
      if elsifs && !elsifs.empty?
        elsifs.each_with_index do |elsif_branch, idx|
          # Restore locals for this test (each elsif sees pre-branch state)
          ctx.restore_locals(pre_branch_locals)

          # We're now in the test block for this elsif
          ctx.current_block = next_test_block

          # Lower elsif condition
          elsif_cond_id = lower_expr(ctx, elsif_branch.condition)

          # Create body block and next block
          elsif_body_block = ctx.create_block
          is_last_elsif = (idx == elsifs.size - 1)
          next_test_block = if is_last_elsif
                              ctx.create_block  # Final else block
                            else
                              ctx.create_block  # Next elsif test
                            end

          ctx.terminate(Branch.new(elsif_cond_id, elsif_body_block, next_test_block))

          # Process elsif body
          ctx.current_block = elsif_body_block
          ctx.push_scope(ScopeKind::Block)
          elsif_value = lower_body(ctx, elsif_branch.body)
          elsif_exit_block = ctx.current_block
          elsif_locals = ctx.save_locals
          ctx.pop_scope

          elsif_block_data = ctx.get_block(ctx.current_block)
          elsif_has_noreturn = elsif_block_data.instructions.any? { |inst| inst.is_a?(Raise) || inst.is_a?(Return) }
          elsif_flows_to_merge = elsif_block_data.terminator.is_a?(Unreachable) && !elsif_has_noreturn
          if elsif_flows_to_merge
            ctx.terminate(Jump.new(merge_block))
          end
          branches << {elsif_exit_block, elsif_value, elsif_locals, elsif_flows_to_merge}
        end
      end

      # Process final else branch
      ctx.restore_locals(pre_branch_locals)
      ctx.current_block = next_test_block
      ctx.push_scope(ScopeKind::Block)
      else_value = if else_body = node.else_body
                     if else_body.empty?
                       nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
                       ctx.emit(nil_lit)
                       nil_lit.id
                     else
                       lower_body(ctx, else_body)
                     end
                   else
                     # No else: produce nil
                     nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
                     ctx.emit(nil_lit)
                     nil_lit.id
                   end
      else_exit_block = ctx.current_block
      else_locals = ctx.save_locals
      ctx.pop_scope

      else_block_data = ctx.get_block(ctx.current_block)
      else_has_noreturn = else_block_data.instructions.any? { |inst| inst.is_a?(Raise) || inst.is_a?(Return) }
      else_flows_to_merge = else_block_data.terminator.is_a?(Unreachable) && !else_has_noreturn
      if else_flows_to_merge
        ctx.terminate(Jump.new(merge_block))
      end
      branches << {else_exit_block, else_value, else_locals, else_flows_to_merge}

      # Merge block
      ctx.current_block = merge_block

      # Count flowing branches
      flowing_branches = branches.select { |_, _, _, flows| flows }

      if flowing_branches.empty?
        # All branches return/raise - emit nil placeholder
        nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
        ctx.emit(nil_lit)
        return nil_lit.id
      elsif flowing_branches.size == 1
        # Only one branch flows - use its value and locals
        exit_block, value, locals, _ = flowing_branches.first
        locals.each { |name, val| ctx.register_local(name, val) }
        return value
      else
        # Multiple branches flow - create phi
        # Use first flowing branch's type as phi type (simplified)
        first_value = flowing_branches.first[1]
        phi_type = ctx.type_of(first_value)

        # Don't create phi for void/nil types
        if phi_type == TypeRef::VOID || phi_type == TypeRef::NIL
          nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
          ctx.emit(nil_lit)
          return nil_lit.id
        end

        phi = Phi.new(ctx.next_id, phi_type)
        flowing_branches.each do |exit_block, value, _, _|
          phi.add_incoming(exit_block, value)
        end
        ctx.emit(phi)
        return phi.id
      end
    end

    # Merge locals from two branches, creating phi nodes where needed
    private def merge_branch_locals(ctx : LoweringContext,
                                    pre_locals : Hash(String, ValueId),
                                    then_locals : Hash(String, ValueId),
                                    else_locals : Hash(String, ValueId),
                                    then_block : BlockId,
                                    else_block : BlockId)
      # Find all variables that exist in either branch or existed before
      all_vars = (then_locals.keys + else_locals.keys + pre_locals.keys).uniq

      all_vars.each do |var_name|
        then_val = then_locals[var_name]?
        else_val = else_locals[var_name]?
        pre_val = pre_locals[var_name]?

        # Use pre-branch value if branch didn't define the variable
        then_val ||= pre_val
        else_val ||= pre_val

        # If variable is new in only one branch (not in pre_locals), use nil for other
        # Crystal semantics: variables assigned in one branch are nilable outside
        if then_val && !else_val
          # Create nil literal for else branch
          nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
          ctx.emit_to_block(else_block, nil_lit)
          else_val = nil_lit.id
        elsif else_val && !then_val
          # Create nil literal for then branch
          nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
          ctx.emit_to_block(then_block, nil_lit)
          then_val = nil_lit.id
        end

        # Skip if variable doesn't exist in either branch (and didn't exist before)
        next unless then_val && else_val

        # If both branches have the same value, no phi needed
        if then_val == else_val
          ctx.register_local(var_name, then_val)
          next
        end

        # Create phi to merge the values
        var_type = ctx.type_of(then_val)
        merge_phi = Phi.new(ctx.next_id, var_type)
        merge_phi.add_incoming(then_block, then_val)
        merge_phi.add_incoming(else_block, else_val)
        ctx.emit(merge_phi)
        ctx.register_local(var_name, merge_phi.id)
      end
    end

    # Merge locals from N branches (for case expressions), creating phi nodes where needed
    # branch_info: Array of (BlockId, Hash(String, ValueId)) for each branch
    private def merge_case_locals(ctx : LoweringContext,
                                  pre_locals : Hash(String, ValueId),
                                  branch_info : Array(Tuple(BlockId, Hash(String, ValueId))))
      return if branch_info.empty?

      # Collect all variable names across all branches
      all_vars = Set(String).new
      branch_info.each { |(_, locals)| all_vars.concat(locals.keys) }

      all_vars.each do |var_name|
        pre_val = pre_locals[var_name]?

        # Collect values from all branches that have this variable
        branch_values = [] of Tuple(BlockId, ValueId)
        branch_info.each do |(block, locals)|
          if val = locals[var_name]?
            branch_values << {block, val}
          elsif pre_val
            # Branch didn't modify, use pre-case value
            branch_values << {block, pre_val}
          end
        end

        # Skip if variable doesn't exist in all branches and didn't exist before
        next if branch_values.size != branch_info.size

        # If all branches have the same value, no phi needed
        first_val = branch_values.first[1]
        if branch_values.all? { |(_, v)| v == first_val }
          ctx.register_local(var_name, first_val)
          next
        end

        # Create phi to merge values from all branches
        var_type = ctx.type_of(first_val)
        merge_phi = Phi.new(ctx.next_id, var_type)
        branch_values.each { |(blk, val)| merge_phi.add_incoming(blk, val) }
        ctx.emit(merge_phi)
        ctx.register_local(var_name, merge_phi.id)
      end
    end

    private def lower_unless(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::UnlessNode) : ValueId
      # Unless is just if with inverted condition
      cond_id = lower_expr(ctx, node.condition)

      # Save locals state before branching
      pre_branch_locals = ctx.save_locals

      # Negate condition
      neg_cond = UnaryOperation.new(ctx.next_id, TypeRef::BOOL, UnaryOp::Not, cond_id)
      ctx.emit(neg_cond)

      then_block = ctx.create_block
      else_block = ctx.create_block
      merge_block = ctx.create_block

      ctx.terminate(Branch.new(neg_cond.id, then_block, else_block))

      # Then (was unless body)
      ctx.current_block = then_block
      ctx.restore_locals(pre_branch_locals)
      ctx.push_scope(ScopeKind::Block)
      then_value = lower_body(ctx, node.then_branch)
      then_exit = ctx.current_block
      ctx.pop_scope
      then_locals = ctx.save_locals

      # Check if then branch flows to merge (not terminated by return/raise)
      then_block_data = ctx.get_block(ctx.current_block)
      then_has_noreturn = then_block_data.instructions.any? { |inst| inst.is_a?(Raise) }
      then_flows_to_merge = then_block_data.terminator.is_a?(Unreachable) && !then_has_noreturn
      if then_flows_to_merge
        ctx.terminate(Jump.new(merge_block))
      end

      # Else branch (if any)
      ctx.current_block = else_block
      ctx.restore_locals(pre_branch_locals)
      else_value = if else_branch = node.else_branch
                     lower_body(ctx, else_branch)
                   else
                     nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
                     ctx.emit(nil_lit)
                     nil_lit.id
                   end
      else_exit = ctx.current_block
      else_locals = ctx.save_locals

      # Check if else branch flows to merge
      else_block_data = ctx.get_block(ctx.current_block)
      else_has_noreturn = else_block_data.instructions.any? { |inst| inst.is_a?(Raise) }
      else_flows_to_merge = else_block_data.terminator.is_a?(Unreachable) && !else_has_noreturn
      if else_flows_to_merge
        ctx.terminate(Jump.new(merge_block))
      end

      # Merge
      ctx.current_block = merge_block

      # Only create phi if at least one branch flows to merge
      if then_flows_to_merge || else_flows_to_merge
        if then_flows_to_merge && else_flows_to_merge
          # Merge locals from both branches
          merge_branch_locals(ctx, pre_branch_locals, then_locals, else_locals,
                              then_exit, else_exit)

          phi_type = ctx.type_of(then_value)

          # Don't create phi for void types - LLVM doesn't allow phi void
          if phi_type == TypeRef::VOID || phi_type == TypeRef::NIL
            nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
            ctx.emit(nil_lit)
            return nil_lit.id
          end

          phi = Phi.new(ctx.next_id, phi_type)
          phi.add_incoming(then_exit, then_value)
          phi.add_incoming(else_exit, else_value)
          ctx.emit(phi)
          return phi.id
        elsif then_flows_to_merge
          # Only then flows - use then_value, then_locals
          then_locals.each { |name, val| ctx.register_local(name, val) }
          return then_value
        else
          # Only else flows - use else_value, else_locals
          else_locals.each { |name, val| ctx.register_local(name, val) }
          return else_value
        end
      end

      # Neither branch flows to merge - emit nil placeholder
      nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
      ctx.emit(nil_lit)
      nil_lit.id
    end

    private def lower_while(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::WhileNode) : ValueId
      # Collect variables that might be assigned in the loop body
      # We need phi nodes at the loop header for these
      assigned_vars = collect_assigned_vars(node.body)
      inline_vars = Set(String).new

      # Save the initial values of variables before the loop
      pre_loop_block = ctx.current_block
      initial_values = {} of String => ValueId
      assigned_vars.each do |var_name|
        if val = lookup_local_for_phi(ctx, var_name, inline_vars)
          initial_values[var_name] = val
        end
      end

      cond_block = ctx.create_block
      body_block = ctx.create_block
      exit_block = ctx.create_block

      # Jump to condition check
      ctx.terminate(Jump.new(cond_block))

      # Condition block - create phi nodes for mutable variables
      ctx.current_block = cond_block
      phi_nodes = {} of String => Phi
      assigned_vars.each do |var_name|
        if initial_val = initial_values[var_name]?
          var_type = ctx.type_of(initial_val)
          phi = Phi.new(ctx.next_id, var_type)
          # Add incoming from pre-loop block
          phi.add_incoming(pre_loop_block, initial_val)
          ctx.emit(phi)
          phi_nodes[var_name] = phi
          # Update local to point to phi
          ctx.register_local(var_name, phi.id)
          if inline_vars.includes?(var_name)
            @inline_caller_locals_stack[-1][var_name] = phi.id
          end
        end
      end

      cond_id = lower_expr(ctx, node.condition)
      ctx.terminate(Branch.new(cond_id, body_block, exit_block))

      # Body block
      ctx.current_block = body_block
      ctx.push_scope(ScopeKind::Loop)
      pushed_inline = false
      if !inline_vars.empty?
        @inline_loop_vars_stack << inline_vars
        pushed_inline = true
      end
      begin
        lower_body(ctx, node.body)
      ensure
        @inline_loop_vars_stack.pop? if pushed_inline
      end
      body_exit_block = ctx.current_block
      ctx.pop_scope

      # After body execution, get updated values and patch phi nodes
      assigned_vars.each do |var_name|
        if phi = phi_nodes[var_name]?
          if updated_val = ctx.lookup_local(var_name)
            # Add incoming from body block (the updated value)
            phi.add_incoming(body_exit_block, updated_val)
            # Reset local to point back to phi for next iteration
            ctx.register_local(var_name, phi.id)
          end
        end
      end

      if ctx.get_block(ctx.current_block).terminator.is_a?(Unreachable)
        ctx.terminate(Jump.new(cond_block))  # Loop back
      end

      # Exit block - locals should still point to phi nodes
      ctx.current_block = exit_block

      # Restore phi values for use after the loop
      phi_nodes.each do |var_name, phi|
        ctx.register_local(var_name, phi.id)
      end

      nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
      ctx.emit(nil_lit)
      nil_lit.id
    end

    # ═══════════════════════════════════════════════════════════════════════
    # LOOP (infinite loop) LOWERING
    # ═══════════════════════════════════════════════════════════════════════
    private def lower_loop(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::LoopNode) : ValueId
      # Collect variables that might be assigned in the loop body
      assigned_vars = collect_assigned_vars(node.body)
      inline_vars = Set(String).new

      # Save the initial values of variables before the loop
      pre_loop_block = ctx.current_block
      initial_values = {} of String => ValueId
      assigned_vars.each do |var_name|
        if val = lookup_local_for_phi(ctx, var_name, inline_vars)
          initial_values[var_name] = val
        end
      end

      # For infinite loop, we just need body and exit blocks
      body_block = ctx.create_block
      exit_block = ctx.create_block

      # Jump directly to body
      ctx.terminate(Jump.new(body_block))

      # Body block - create phi nodes for mutable variables
      ctx.current_block = body_block
      phi_nodes = {} of String => Phi
      assigned_vars.each do |var_name|
        if initial_val = initial_values[var_name]?
          var_type = ctx.type_of(initial_val)
          phi = Phi.new(ctx.next_id, var_type)
          phi.add_incoming(pre_loop_block, initial_val)
          ctx.emit(phi)
          phi_nodes[var_name] = phi
          ctx.register_local(var_name, phi.id)
          if inline_vars.includes?(var_name)
            @inline_caller_locals_stack[-1][var_name] = phi.id
          end
        end
      end

      ctx.push_scope(ScopeKind::Loop)
      pushed_inline = false
      if !inline_vars.empty?
        @inline_loop_vars_stack << inline_vars
        pushed_inline = true
      end
      begin
        lower_body(ctx, node.body)
      ensure
        @inline_loop_vars_stack.pop? if pushed_inline
      end
      body_exit_block = ctx.current_block
      ctx.pop_scope

      # After body execution, update phi nodes for next iteration
      assigned_vars.each do |var_name|
        if phi = phi_nodes[var_name]?
          if updated_val = ctx.lookup_local(var_name)
            phi.add_incoming(body_exit_block, updated_val)
            ctx.register_local(var_name, phi.id)
          end
        end
      end

      # Loop back unconditionally (break will jump to exit_block)
      if ctx.get_block(ctx.current_block).terminator.is_a?(Unreachable)
        ctx.terminate(Jump.new(body_block))
      end

      # Exit block (reached via break)
      ctx.current_block = exit_block

      # Restore phi values for use after the loop
      phi_nodes.each do |var_name, phi|
        ctx.register_local(var_name, phi.id)
      end

      nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
      ctx.emit(nil_lit)
      nil_lit.id
    end

    # Collect variable names that are assigned in a list of expressions
    private def collect_assigned_vars(body : Array(ExprId), visited_blocks : Set(UInt64)? = nil) : Array(String)
      visited_blocks ||= Set(UInt64).new
      vars = [] of String
      body.each do |expr_id|
        collect_assigned_vars_in_expr(expr_id, vars, visited_blocks)
      end
      vars.uniq
    end

    private def collect_assigned_vars_in_expr(expr_id : ExprId, vars : Array(String), visited_blocks : Set(UInt64))
      node = @arena[expr_id]
      case node
      when CrystalV2::Compiler::Frontend::AssignNode
        target = @arena[node.target]
        if target.is_a?(CrystalV2::Compiler::Frontend::IdentifierNode)
          vars << String.new(target.name)
        end
        # Also check the value side for nested assignments
        collect_assigned_vars_in_expr(node.value, vars, visited_blocks)

      when CrystalV2::Compiler::Frontend::MultipleAssignNode
        node.targets.each do |target_id|
          target = @arena[target_id]
          if target.is_a?(CrystalV2::Compiler::Frontend::IdentifierNode)
            vars << String.new(target.name)
          end
        end
        collect_assigned_vars_in_expr(node.value, vars, visited_blocks)

      when CrystalV2::Compiler::Frontend::TypeDeclarationNode
        vars << String.new(node.name)
        if value = node.value
          collect_assigned_vars_in_expr(value, vars, visited_blocks)
        end

      when CrystalV2::Compiler::Frontend::WhileNode
        # Nested while - check its body
        collect_assigned_vars(node.body, visited_blocks).each { |v| vars << v }

      when CrystalV2::Compiler::Frontend::LoopNode
        # Nested loop - check its body
        collect_assigned_vars(node.body, visited_blocks).each { |v| vars << v }

      when CrystalV2::Compiler::Frontend::IfNode
        # Check all branches
        collect_assigned_vars(node.then_body, visited_blocks).each { |v| vars << v }
        if else_body = node.else_body
          collect_assigned_vars(else_body, visited_blocks).each { |v| vars << v }
        end

      when CrystalV2::Compiler::Frontend::UnlessNode
        # Check all branches
        collect_assigned_vars(node.then_branch, visited_blocks).each { |v| vars << v }
        if else_body = node.else_branch
          collect_assigned_vars(else_body, visited_blocks).each { |v| vars << v }
        end

      when CrystalV2::Compiler::Frontend::CaseNode
        # Check all when branches and else
        node.when_branches.each do |when_branch|
          collect_assigned_vars(when_branch.body, visited_blocks).each { |v| vars << v }
        end
        if else_body = node.else_branch
          collect_assigned_vars(else_body, visited_blocks).each { |v| vars << v }
        end

      when CrystalV2::Compiler::Frontend::BinaryNode
        collect_assigned_vars_in_expr(node.left, vars, visited_blocks)
        collect_assigned_vars_in_expr(node.right, vars, visited_blocks)

      when CrystalV2::Compiler::Frontend::CallNode
        node.args.each { |arg| collect_assigned_vars_in_expr(arg, vars, visited_blocks) }

      when CrystalV2::Compiler::Frontend::GroupingNode
        collect_assigned_vars_in_expr(node.expression, vars, visited_blocks)

      when CrystalV2::Compiler::Frontend::YieldNode
        if inline_block = @inline_yield_block_stack.last?
          block_id = inline_block.object_id
          unless visited_blocks.includes?(block_id)
            visited_blocks.add(block_id)
            old_arena = @arena
            if block_arena = @inline_yield_block_arena_stack.last?
              @arena = block_arena
            end
            begin
              block_vars = collect_assigned_vars(inline_block.body, visited_blocks)
            ensure
              @arena = old_arena
            end
            if params = inline_block.params
              param_names = params.compact_map { |param| param.name ? String.new(param.name.not_nil!) : nil }
              block_vars.reject! { |name| param_names.includes?(name) }
            end
            block_vars.each { |v| vars << v }
          end
        end
      end
    end

    private def lookup_local_for_phi(ctx : LoweringContext, name : String, inline_vars : Set(String)) : ValueId?
      if val = ctx.lookup_local(name)
        return val
      end

      if caller_locals = @inline_caller_locals_stack.last?
        if val = caller_locals[name]?
          inline_vars.add(name)
          return val
        end
      end

      nil
    end

    private def inline_loop_vars_union : Set(String)?
      return nil if @inline_loop_vars_stack.empty?
      union = Set(String).new
      @inline_loop_vars_stack.each do |set|
        set.each { |name| union.add(name) }
      end
      union
    end

    private def lower_until(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::UntilNode) : ValueId
      # Until is while with inverted condition
      cond_block = ctx.create_block
      body_block = ctx.create_block
      exit_block = ctx.create_block

      ctx.terminate(Jump.new(cond_block))

      ctx.current_block = cond_block
      cond_id = lower_expr(ctx, node.condition)
      # Negate condition
      neg_cond = UnaryOperation.new(ctx.next_id, TypeRef::BOOL, UnaryOp::Not, cond_id)
      ctx.emit(neg_cond)
      ctx.terminate(Branch.new(neg_cond.id, body_block, exit_block))

      ctx.current_block = body_block
      ctx.push_scope(ScopeKind::Loop)
      lower_body(ctx, node.body)
      ctx.pop_scope

      if ctx.get_block(ctx.current_block).terminator.is_a?(Unreachable)
        ctx.terminate(Jump.new(cond_block))
      end

      ctx.current_block = exit_block
      nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
      ctx.emit(nil_lit)
      nil_lit.id
    end

    private def lower_ternary(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::TernaryNode) : ValueId
      cond_id = lower_expr(ctx, node.condition)

      # Save locals before branching
      pre_branch_locals = ctx.save_locals

      then_block = ctx.create_block
      else_block = ctx.create_block
      merge_block = ctx.create_block

      ctx.terminate(Branch.new(cond_id, then_block, else_block))

      ctx.current_block = then_block
      ctx.restore_locals(pre_branch_locals)
      then_value = lower_expr(ctx, node.true_branch)
      then_exit = ctx.current_block
      then_locals = ctx.save_locals

      # Check if then branch flows to merge
      then_block_data = ctx.get_block(ctx.current_block)
      then_has_noreturn = then_block_data.instructions.any? { |inst| inst.is_a?(Raise) }
      then_flows_to_merge = then_block_data.terminator.is_a?(Unreachable) && !then_has_noreturn
      if then_flows_to_merge
        ctx.terminate(Jump.new(merge_block))
      end

      ctx.current_block = else_block
      ctx.restore_locals(pre_branch_locals)
      else_value = lower_expr(ctx, node.false_branch)
      else_exit = ctx.current_block
      else_locals = ctx.save_locals

      # Check if else branch flows to merge
      else_block_data = ctx.get_block(ctx.current_block)
      else_has_noreturn = else_block_data.instructions.any? { |inst| inst.is_a?(Raise) }
      else_flows_to_merge = else_block_data.terminator.is_a?(Unreachable) && !else_has_noreturn
      if else_flows_to_merge
        ctx.terminate(Jump.new(merge_block))
      end

      ctx.current_block = merge_block

      # Only create phi if at least one branch flows to merge
      if then_flows_to_merge || else_flows_to_merge
        if then_flows_to_merge && else_flows_to_merge
          # Merge locals from both branches
          merge_branch_locals(ctx, pre_branch_locals, then_locals, else_locals,
                              then_exit, else_exit)

          phi_type = ctx.type_of(then_value)

          # Don't create phi for void types - LLVM doesn't allow phi void
          if phi_type == TypeRef::VOID || phi_type == TypeRef::NIL
            nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
            ctx.emit(nil_lit)
            return nil_lit.id
          end

          phi = Phi.new(ctx.next_id, phi_type)
          phi.add_incoming(then_exit, then_value)
          phi.add_incoming(else_exit, else_value)
          ctx.emit(phi)
          return phi.id
        elsif then_flows_to_merge
          then_locals.each { |name, val| ctx.register_local(name, val) }
          return then_value
        else
          else_locals.each { |name, val| ctx.register_local(name, val) }
          return else_value
        end
      end

      # Neither branch flows - return nil placeholder
      nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
      ctx.emit(nil_lit)
      nil_lit.id
    end

    # Emit comparison for case/when using appropriate === semantics
    # Returns ValueId of boolean result
    private def emit_case_comparison(ctx : LoweringContext, subject_id : ValueId, cond_expr : ExprId) : ValueId
      cond_node = @arena[cond_expr]

      case cond_node
      when CrystalV2::Compiler::Frontend::NumberNode,
           CrystalV2::Compiler::Frontend::BoolNode,
           CrystalV2::Compiler::Frontend::NilNode,
           CrystalV2::Compiler::Frontend::CharNode
        # Primitive literals: direct equality comparison (optimized)
        cond_val = lower_expr(ctx, cond_expr)
        eq = BinaryOperation.new(ctx.next_id, TypeRef::BOOL, BinaryOp::Eq, subject_id, cond_val)
        ctx.emit(eq)
        eq.id

      when CrystalV2::Compiler::Frontend::ConstantNode
        # Could be a type name (Int32, String) → is_a? check
        # Or a constant value → equality
        const_name = String.new(cond_node.name)
        if is_type_name?(const_name)
          # Type check: subject.is_a?(ConstName)
          check_type = type_ref_for_name(const_name)
          subject_type = ctx.type_of(subject_id)

          # If subject is union type, use UnionIs
          if is_union_type?(subject_type)
            variant_id = get_union_variant_id(subject_type, check_type)
            if variant_id >= 0
              union_is = UnionIs.new(ctx.next_id, subject_id, variant_id)
              ctx.emit(union_is)
              return union_is.id
            end
          end

          # Regular is_a? check
          is_a = IsA.new(ctx.next_id, subject_id, check_type)
          ctx.emit(is_a)
          is_a.id
        else
          # Constant value - equality
          cond_val = lower_expr(ctx, cond_expr)
          eq = BinaryOperation.new(ctx.next_id, TypeRef::BOOL, BinaryOp::Eq, subject_id, cond_val)
          ctx.emit(eq)
          eq.id
        end

      when CrystalV2::Compiler::Frontend::RangeNode
        # Range: call Range#=== or Range#includes?
        # For now, expand to: subject >= begin && subject <= end (or < for exclusive)
        range_begin = lower_expr(ctx, cond_node.begin_expr)
        range_end = lower_expr(ctx, cond_node.end_expr)

        gte = BinaryOperation.new(ctx.next_id, TypeRef::BOOL, BinaryOp::Ge, subject_id, range_begin)
        ctx.emit(gte)

        cmp_op = cond_node.exclusive ? BinaryOp::Lt : BinaryOp::Le
        lte = BinaryOperation.new(ctx.next_id, TypeRef::BOOL, cmp_op, subject_id, range_end)
        ctx.emit(lte)

        and_op = BinaryOperation.new(ctx.next_id, TypeRef::BOOL, BinaryOp::And, gte.id, lte.id)
        ctx.emit(and_op)
        and_op.id

      when CrystalV2::Compiler::Frontend::IdentifierNode
        # Could be a type name (Int32, String) or variable
        ident_name = String.new(cond_node.name)
        if is_type_name?(ident_name)
          # Type check: subject.is_a?(IdentName)
          check_type = type_ref_for_name(ident_name)
          subject_type = ctx.type_of(subject_id)

          # If subject is union type, use UnionIs
          if is_union_type?(subject_type)
            variant_id = get_union_variant_id(subject_type, check_type)
            if variant_id >= 0
              union_is = UnionIs.new(ctx.next_id, subject_id, variant_id)
              ctx.emit(union_is)
              return union_is.id
            end
          end

          # Regular is_a? check
          is_a = IsA.new(ctx.next_id, subject_id, check_type)
          ctx.emit(is_a)
          is_a.id
        else
          # Variable - equality
          cond_val = lower_expr(ctx, cond_expr)
          eq = BinaryOperation.new(ctx.next_id, TypeRef::BOOL, BinaryOp::Eq, subject_id, cond_val)
          ctx.emit(eq)
          eq.id
        end

      else
        # Default: call === method (when we have method calls working)
        # For now, fall back to equality
        cond_val = lower_expr(ctx, cond_expr)
        eq = BinaryOperation.new(ctx.next_id, TypeRef::BOOL, BinaryOp::Eq, subject_id, cond_val)
        ctx.emit(eq)
        eq.id
      end
    end

    # Check if a name refers to a type (starts with uppercase)
    private def is_type_name?(name : String) : Bool
      return false if name.empty?
      first_char = name[0]
      first_char.uppercase? && @class_info.has_key?(name) ||
        ["Int32", "Int64", "String", "Bool", "Nil", "Float64"].includes?(name)
    end

    private def lower_case(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::CaseNode) : ValueId
      # Save locals state before case for proper phi merging
      pre_case_locals = ctx.save_locals

      # Lower case subject
      subject_id = if subj = node.value
                     lower_expr(ctx, subj)
                   else
                     nil
                   end

      merge_block = ctx.create_block
      incoming = [] of Tuple(BlockId, ValueId)
      branch_locals = [] of Tuple(BlockId, Hash(String, ValueId))  # Track locals for each branch

      # Process each when branch
      node.when_branches.each_with_index do |when_branch, idx|
        when_block = ctx.create_block
        next_block = ctx.create_block

        # Build condition (any match)
        if subject_id
          # Match subject against when values using appropriate === semantics
          conds = when_branch.conditions.map do |cond_expr|
            emit_case_comparison(ctx, subject_id.not_nil!, cond_expr)
          end

          # Combine with OR
          combined = conds.reduce do |acc, c|
            or_op = BinaryOperation.new(ctx.next_id, TypeRef::BOOL, BinaryOp::Or, acc, c)
            ctx.emit(or_op)
            or_op.id
          end

          ctx.terminate(Branch.new(combined, when_block, next_block))
        else
          # No subject: conditions are boolean
          cond_val = lower_expr(ctx, when_branch.conditions.first)
          ctx.terminate(Branch.new(cond_val, when_block, next_block))
        end

        # When body - restore locals before each branch
        ctx.current_block = when_block
        ctx.restore_locals(pre_case_locals)
        ctx.push_scope(ScopeKind::Block)
        result = lower_body(ctx, when_branch.body)
        exit_block = ctx.current_block
        ctx.pop_scope

        # Check if branch flows to merge
        when_block_data = ctx.get_block(ctx.current_block)
        when_has_noreturn = when_block_data.instructions.any? { |inst| inst.is_a?(Raise) }
        when_flows_to_merge = when_block_data.terminator.is_a?(Unreachable) && !when_has_noreturn

        if when_flows_to_merge
          # Save branch locals before jumping to merge (only if flowing)
          branch_locals << {exit_block, ctx.save_locals}
          ctx.terminate(Jump.new(merge_block))
          incoming << {exit_block, result}
        end

        ctx.current_block = next_block
      end

      # Else branch - restore locals before else branch
      ctx.restore_locals(pre_case_locals)
      ctx.push_scope(ScopeKind::Block)
      else_result = if else_body = node.else_branch
                      if else_body.empty?
                        nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
                        ctx.emit(nil_lit)
                        nil_lit.id
                      else
                        lower_body(ctx, else_body)
                      end
                    else
                      nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
                      ctx.emit(nil_lit)
                      nil_lit.id
                    end
      else_exit = ctx.current_block
      ctx.pop_scope

      # Check if else branch flows to merge
      else_block_data = ctx.get_block(ctx.current_block)
      else_has_noreturn = else_block_data.instructions.any? { |inst| inst.is_a?(Raise) }
      else_flows_to_merge = else_block_data.terminator.is_a?(Unreachable) && !else_has_noreturn

      if else_flows_to_merge
        # Save else branch locals (only if flowing)
        branch_locals << {else_exit, ctx.save_locals}
        ctx.terminate(Jump.new(merge_block))
        incoming << {else_exit, else_result}
      end

      # Merge
      ctx.current_block = merge_block

      # Only merge locals and create phi if at least one branch flows
      if incoming.empty?
        # No branches flow to merge - emit nil placeholder
        nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
        ctx.emit(nil_lit)
        return nil_lit.id
      end

      # Merge locals from branches that flow to merge
      merge_case_locals(ctx, pre_case_locals, branch_locals)

      value_types = incoming.map { |(_, val)| ctx.type_of(val) }.reject { |t| t == TypeRef::VOID }.uniq
      phi_type = value_types.first? || TypeRef::VOID

      # If branch value types differ, prefer a union type that covers them.
      # This avoids invalid IR when merging mixed types (e.g., Bool/Float64/Int64),
      # and matches Crystal semantics (case expression returns a union).
      if value_types.size > 1 && !value_types.all? { |t| numeric_primitive?(t) }
        if union_ref = find_covering_union_type(value_types)
          phi_type = union_ref
        else
          union_name = value_types.map { |t| get_type_name_from_ref(t) }.uniq.join(" | ")
          phi_type = create_union_type(union_name)
        end
      end

      # Coerce incoming values to the chosen phi type when needed.
      #
      # NOTE: We must insert conversions into the predecessor blocks (not the merge block),
      # otherwise the SSA value won't dominate the phi edge.
      coerced_incoming = incoming.map do |(blk, val)|
        val_type = ctx.type_of(val)
        if val_type == phi_type
          {blk, val}
        elsif is_union_type?(phi_type)
          variant_id = get_union_variant_id(phi_type, val_type)
          if variant_id >= 0
            wrap = UnionWrap.new(ctx.next_id, phi_type, val, variant_id)
            ctx.emit_to_block(blk, wrap)
            {blk, wrap.id}
          else
            {blk, val}
          end
        elsif numeric_primitive?(val_type) && numeric_primitive?(phi_type)
          cast = Cast.new(ctx.next_id, phi_type, val, phi_type, safe: false)
          ctx.emit_to_block(blk, cast)
          {blk, cast.id}
        else
          {blk, val}
        end
      end

      # Don't create phi for void types - LLVM doesn't allow phi void
      if phi_type == TypeRef::VOID || phi_type == TypeRef::NIL
        nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
        ctx.emit(nil_lit)
        return nil_lit.id
      end

      # If only one branch flows, no phi needed
      if coerced_incoming.size == 1
        return coerced_incoming.first[1]
      end

      phi = Phi.new(ctx.next_id, phi_type)
      coerced_incoming.each { |(blk, val)| phi.add_incoming(blk, val) }
      ctx.emit(phi)
      phi.id
    end

    # ═══════════════════════════════════════════════════════════════════════
    # FUNCTION-RELATED
    # ═══════════════════════════════════════════════════════════════════════

    private def lower_return(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::ReturnNode) : ValueId
      value_id = if val = node.value
                   lower_expr(ctx, val)
                 else
                   nil
                 end

      ctx.terminate(Return.new(value_id))

      # Return a dummy value (code after return is unreachable)
      nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
      ctx.emit(nil_lit)
      nil_lit.id
    end

    private def lower_yield(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::YieldNode) : ValueId
      args = if node_args = node.args
               node_args.map { |arg| lower_expr(ctx, arg) }
             else
               [] of ValueId
             end

      y = Yield.new(ctx.next_id, TypeRef::VOID, args)
      ctx.emit(y)
      y.id
    end

    private def lower_break(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::BreakNode) : ValueId
      # Break needs special handling - for now emit as unreachable
      # TODO: proper break with loop exit block tracking
      ctx.terminate(Unreachable.new)
      nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
      ctx.emit(nil_lit)
      nil_lit.id
    end

    private def lower_next(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::NextNode) : ValueId
      # Next needs special handling - for now emit as unreachable
      # TODO: proper next with loop continue block tracking
      ctx.terminate(Unreachable.new)
      nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
      ctx.emit(nil_lit)
      nil_lit.id
    end

    # ═══════════════════════════════════════════════════════════════════════
    # EXCEPTION HANDLING
    # ═══════════════════════════════════════════════════════════════════════

    # Lower begin/rescue/ensure block
    # Structure:
    #   begin
    #     body...
    #   rescue ex : ExceptionType
    #     handler...
    #   else
    #     else_body...
    #   ensure
    #     cleanup...
    #   end
    private def lower_begin(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::BeginNode) : ValueId
      # Exception handling using setjmp/longjmp:
      # 1. Call TryBegin (setjmp) - returns 0 for normal, non-zero for exception
      # 2. Branch based on result
      # 3. Body path: execute body, call TryEnd
      # 4. Rescue path: execute rescue handlers, call TryEnd
      # 5. Ensure path: always executed

      has_rescue = !node.rescue_clauses.nil? && !node.rescue_clauses.not_nil!.empty?
      has_else = !node.else_body.nil? && !node.else_body.not_nil!.empty?
      has_ensure = !node.ensure_body.nil? && !node.ensure_body.not_nil!.empty?

      # Create blocks
      body_block = ctx.create_block
      rescue_block = ctx.create_block if has_rescue
      else_block = ctx.create_block if has_else
      ensure_block = ctx.create_block if has_ensure
      exit_block = ctx.create_block

      # If we have rescue clauses, set up exception handling
      if has_rescue
        # Call TryBegin - returns 0 for normal path, non-zero for exception
        try_begin = TryBegin.new(ctx.next_id)
        ctx.emit(try_begin)
        ctx.register_type(try_begin.id, TypeRef::INT32)

        # Compare with 0
        zero = Literal.new(ctx.next_id, TypeRef::INT32, 0_i64)
        ctx.emit(zero)
        ctx.register_type(zero.id, TypeRef::INT32)

        cmp = BinaryOperation.new(ctx.next_id, TypeRef::BOOL, BinaryOp::Eq, try_begin.id, zero.id)
        ctx.emit(cmp)
        ctx.register_type(cmp.id, TypeRef::BOOL)

        # Branch: if result == 0, normal path (body); else exception path (rescue)
        ctx.terminate(Branch.new(cmp.id, body_block, rescue_block.not_nil!))
      else
        # No rescue - just jump to body
        ctx.terminate(Jump.new(body_block))
      end

      ctx.switch_to_block(body_block)

      # Lower body
      result_id : ValueId = ctx.next_id
      node.body.each do |expr_id|
        result_id = lower_expr(ctx, expr_id)
      end

      # After body, call TryEnd if we have rescue handlers
      if has_rescue
        try_end = TryEnd.new(ctx.next_id)
        ctx.emit(try_end)
      end

      # After body, jump to else (if exists) or ensure (if exists) or exit
      after_body_target = else_block || ensure_block || exit_block
      ctx.terminate(Jump.new(after_body_target))

      # Lower rescue clauses if any
      if has_rescue
        rescue_clauses = node.rescue_clauses.not_nil!
        ctx.switch_to_block(rescue_block.not_nil!)

        # For now, just execute the first rescue clause's body
        # TODO: proper exception type matching
        rescue_result : ValueId = ctx.next_id
        rescue_clauses.each_with_index do |clause, idx|
          # If clause has variable name, create local for exception
          if var_name = clause.variable_name
            exc_var = Local.new(ctx.next_id, TypeRef::POINTER, String.new(var_name), ctx.current_scope, true)
            ctx.emit(exc_var)
            ctx.register_local(String.new(var_name), exc_var.id)
            ctx.register_type(exc_var.id, TypeRef::POINTER)

            # Get exception value
            get_exc = GetException.new(ctx.next_id, TypeRef::POINTER)
            ctx.emit(get_exc)
            ctx.register_type(get_exc.id, TypeRef::POINTER)

            # Copy to variable
            copy = Copy.new(ctx.next_id, TypeRef::POINTER, get_exc.id)
            ctx.emit(copy)
            ctx.register_type(copy.id, TypeRef::POINTER)
          end

          # Lower rescue body
          clause.body.each do |expr_id|
            rescue_result = lower_expr(ctx, expr_id)
          end

          # Only handle first clause for now
          break
        end

        # Call TryEnd after rescue
        try_end = TryEnd.new(ctx.next_id)
        ctx.emit(try_end)

        # After rescue, jump to ensure or exit
        after_rescue_target = ensure_block || exit_block
        ctx.terminate(Jump.new(after_rescue_target))
      end

      # Lower else block if any
      if has_else
        else_body = node.else_body.not_nil!
        ctx.switch_to_block(else_block.not_nil!)
        else_body.each do |expr_id|
          result_id = lower_expr(ctx, expr_id)
        end
        after_else_target = ensure_block || exit_block
        ctx.terminate(Jump.new(after_else_target))
      end

      # Lower ensure block if any
      if has_ensure
        ensure_body = node.ensure_body.not_nil!
        ctx.switch_to_block(ensure_block.not_nil!)
        ensure_body.each do |expr_id|
          lower_expr(ctx, expr_id)  # ensure result is discarded
        end
        ctx.terminate(Jump.new(exit_block))
      end

      # Continue from exit block
      ctx.switch_to_block(exit_block)

      result_id
    end

    # Lower raise statement
    private def lower_raise(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::RaiseNode) : ValueId
      exc_value : ValueId? = nil
      exc_message : String? = nil

      if val_id = node.value
        val_node = @arena[val_id]
        # Check if it's a string literal for simple raise "message"
        if val_node.is_a?(CrystalV2::Compiler::Frontend::StringNode)
          exc_message = String.new(val_node.value)
        else
          # Lower the exception value
          exc_value = lower_expr(ctx, val_id)
        end
      end

      # Emit raise instruction
      raise_inst = Raise.new(ctx.next_id, exc_value, exc_message)
      ctx.emit(raise_inst)

      # Raise is a terminator - nothing executes after
      ctx.terminate(Unreachable.new)

      raise_inst.id
    end

    # ═══════════════════════════════════════════════════════════════════════
    # CALLS
    # ═══════════════════════════════════════════════════════════════════════

    private def remember_callsite_arg_types(name : String, arg_types : Array(TypeRef)) : Nil
      return if name.empty?
      @pending_arg_types[name] = arg_types.dup
    end

    private def lower_function_if_needed(name : String) : Nil
      return if name.empty?
      return if @yield_functions.includes?(name)
      return if @module.has_function?(name)
      return if @lowering_functions.includes?(name)

      target_name = name
      func_def = @function_defs[target_name]?
      arena = @function_def_arenas[target_name]?
      unless func_def
        if maybe_generate_class_accessor_for_name(name)
          return
        end
        if maybe_generate_accessor_for_name(name)
          return
        end
        base_name = name.split("$").first
        if base_name != name
          func_def = @function_defs[base_name]?
          arena = @function_def_arenas[base_name]? if func_def
          target_name = base_name if func_def
        end
        # If still not found, search for any mangled variant of the base name
        # This handles methods with default parameters where call-site arg count < defined param count
        unless func_def
          mangled_prefix = "#{base_name}$"
          if ENV.has_key?("DEBUG_LOOKUP")
            STDERR.puts "[DEBUG_LOOKUP] Searching for prefix '#{mangled_prefix}' for name '#{name}'"
          end
          @function_defs.each_key do |key|
            if key.starts_with?(mangled_prefix)
              if ENV.has_key?("DEBUG_LOOKUP")
                STDERR.puts "[DEBUG_LOOKUP]   Found match: '#{key}'"
              end
              func_def = @function_defs[key]
              arena = @function_def_arenas[key]
              target_name = key
              break
            end
          end
          if ENV.has_key?("DEBUG_LOOKUP") && !func_def
            STDERR.puts "[DEBUG_LOOKUP]   No match found for '#{mangled_prefix}'"
          end
        end
      end
      return unless func_def
      return if @yield_functions.includes?(target_name)
      return if @lowering_functions.includes?(target_name)

      call_arg_types = @pending_arg_types[name]? || @pending_arg_types[target_name]?
      @pending_arg_types.delete(name) if @pending_arg_types.has_key?(name)
      @pending_arg_types.delete(target_name) if @pending_arg_types.has_key?(target_name)

      @lowering_functions.add(target_name)
      begin
        with_arena(arena || @arena) do
          if target_name.includes?("#")
            owner = target_name.split("#", 2)[0]
            if class_info = @class_info[owner]?
              old_class = @current_class
              @current_class = owner
              lower_method(owner, class_info, func_def, call_arg_types, target_name)
              @current_class = old_class
            end
          elsif target_name.includes?(".")
            owner = target_name.split(".", 2)[0]
            if class_info = @class_info[owner]?
              old_class = @current_class
              @current_class = owner
              lower_method(owner, class_info, func_def, call_arg_types, target_name)
              @current_class = old_class
            else
              lower_module_method(owner, func_def, call_arg_types, target_name)
            end
          else
            lower_def(func_def, call_arg_types, target_name)
          end
        end
      ensure
        @lowering_functions.delete(target_name)
        @lowered_functions.add(target_name)
      end
    end

    private def maybe_generate_accessor_for_name(name : String) : Bool
      base_name = name.split("$", 2)[0]
      return false unless base_name.includes?("#")

      owner, method_name = base_name.split("#", 2)
      return false if owner.empty? || method_name.empty?

      class_info = @class_info[owner]?
      return false unless class_info

      if method_name.ends_with?("=")
        accessor = method_name[0, method_name.size - 1]
        ivar_name = "@#{accessor}"
        if ivar_info = class_info.ivars.find { |iv| iv.name == ivar_name }
          expected_name = mangle_function_name(base_name, [ivar_info.type])
          return false if name.includes?("$") && expected_name != name
          generate_setter_method_for_ivar(owner, class_info, ivar_info)
          return true
        end
      else
        ivar_name = "@#{method_name}"
        if ivar_info = class_info.ivars.find { |iv| iv.name == ivar_name }
          expected_name = mangle_function_name(base_name, [] of TypeRef)
          return false if name.includes?("$") && expected_name != name
          generate_getter_method_for_ivar(owner, class_info, ivar_info)
          return true
        end
      end

      false
    end

    private def register_class_accessor_entry(
      owner_name : String,
      spec : CrystalV2::Compiler::Frontend::AccessorSpec,
      kind : Symbol
    ) : Nil
      accessor_name = String.new(spec.name)

      case kind
      when :getter
        return_type = if ta = spec.type_annotation
                        type_ref_for_name(String.new(ta))
                      elsif accessor_name.ends_with?("?")
                        TypeRef::BOOL
                      elsif default_value = spec.default_value
                        infer_type_from_expr(default_value, owner_name) || TypeRef::VOID
                      else
                        TypeRef::VOID
                      end
        base_name = "#{owner_name}.#{accessor_name}"
        full_name = mangle_function_name(base_name, [] of TypeRef)
        register_function_type(full_name, return_type)
        entry = ClassAccessorEntry.new(owner_name, spec, @arena, :getter)
        @class_accessor_entries[full_name] = entry
        @class_accessor_entries[base_name] = entry
      when :setter
        param_type = if ta = spec.type_annotation
                       type_ref_for_name(String.new(ta))
                     else
                       TypeRef::VOID
                     end
        base_name = "#{owner_name}.#{accessor_name}="
        full_name = mangle_function_name(base_name, [param_type])
        register_function_type(full_name, param_type)
        entry = ClassAccessorEntry.new(owner_name, spec, @arena, :setter)
        @class_accessor_entries[full_name] = entry
        @class_accessor_entries[base_name] = entry
      end
    end

    private def maybe_generate_class_accessor_for_name(name : String) : Bool
      entry = @class_accessor_entries[name]?
      unless entry
        base_name = name.split("$", 2)[0]
        entry = @class_accessor_entries[base_name]?
      end
      return false unless entry

      with_arena(entry.arena) do
        case entry.kind
        when :getter
          generate_class_getter_method(entry.owner, entry.spec, entry.arena)
        when :setter
          generate_class_setter_method(entry.owner, entry.spec)
        end
      end
      true
    end

    private def ensure_accessor_method(
      ctx : LoweringContext,
      receiver_id : ValueId,
      method_name : String
    ) : Tuple(TypeRef, String)?
      receiver_type = ctx.type_of(receiver_id)
      class_name = get_type_name_from_ref(receiver_type)
      class_info = @class_info[class_name]?
      return nil unless class_info

      if method_name.ends_with?("=")
        accessor_name = method_name[0, method_name.size - 1]
        ivar_name = "@#{accessor_name}"
        if ivar_info = class_info.ivars.find { |iv| iv.name == ivar_name }
          func_name = mangle_function_name("#{class_name}##{accessor_name}=", [ivar_info.type])
          generate_setter_method_for_ivar(class_name, class_info, ivar_info) unless @module.has_function?(func_name)
          return {ivar_info.type, func_name}
        end
      else
        ivar_name = "@#{method_name}"
        if ivar_info = class_info.ivars.find { |iv| iv.name == ivar_name }
          func_name = mangle_function_name("#{class_name}##{method_name}", [] of TypeRef)
          generate_getter_method_for_ivar(class_name, class_info, ivar_info) unless @module.has_function?(func_name)
          return {ivar_info.type, func_name}
        end
      end

      nil
    end

    private def lower_call(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::CallNode) : ValueId
      # CallNode has callee (ExprId) which can be:
      # - IdentifierNode: simple function call like foo() or ClassName.new()
      # - MemberAccessNode: method call like obj.method()
      # - Other: chained/complex calls

      callee_node = @arena[node.callee]

      receiver_id : ValueId? = nil
      method_name : String
      full_method_name : String? = nil

      case callee_node
      when CrystalV2::Compiler::Frontend::IdentifierNode
        # Simple function call: foo()
        method_name = String.new(callee_node.name)

        # Check if this is a macro call - expand inline instead of generating Call
        if macro_def = @macro_defs[method_name]?
          return expand_macro(ctx, macro_def, node.args)
        end

        # If inside a class/module, check if this is a method call on self or module
        if current = @current_class
          # Check if method exists in current class (instance method: Class#method)
          class_method_name = "#{current}##{method_name}"
          # O(1) lookup: check exact match or mangled version exists
          has_class_method = @function_types.has_key?(class_method_name) || has_function_base?(class_method_name)
          if has_class_method
            # This is a method call on self - set receiver to self
            receiver_id = emit_self(ctx)
            full_method_name = class_method_name
          else
            # Also check for module-style method (Module.method)
            module_method_name = "#{current}.#{method_name}"
            # O(1) lookup: check exact match or mangled version exists
            has_module_method = @function_types.has_key?(module_method_name) || has_function_base?(module_method_name)
            if has_module_method
              # This is a module method call (no receiver)
              receiver_id = nil
              full_method_name = module_method_name
            else
              receiver_id = nil
            end
          end
        else
          receiver_id = nil
        end

      when CrystalV2::Compiler::Frontend::MemberAccessNode
        # Could be method call: obj.method() or class method: ClassName.new()
        obj_node = @arena[callee_node.object]
        method_name = String.new(callee_node.member)

        # Intrinsic: `x.upto(y).each { ... }` / `x.downto(y).each { ... }`
        # Prefer lowering directly via the yield-based overload to avoid iterator types like
        # `UptoIterator(typeof(self), typeof(to))` which are not yet fully monomorphized in codegen.
        if method_name == "each"
          if blk_expr = node.block
            blk_node = @arena[blk_expr]
            if blk_node.is_a?(CrystalV2::Compiler::Frontend::BlockNode) && obj_node.is_a?(CrystalV2::Compiler::Frontend::CallNode)
              inner_call = obj_node
              if inner_call.block.nil?
                inner_callee = @arena[inner_call.callee]
                if inner_callee.is_a?(CrystalV2::Compiler::Frontend::MemberAccessNode)
                  inner_method = String.new(inner_callee.member)
                  if inner_method == "upto" || inner_method == "downto"
                    inner_receiver_id = lower_expr(ctx, inner_callee.object)
                    inner_args = if inner_call.named_args
                                   # Named args for upto/downto are not expected; fall back to positional.
                                   expand_splat_args(ctx, inner_call.args)
                                 else
                                   expand_splat_args(ctx, inner_call.args)
                                 end
                    if yield_key = find_yield_method_fallback(inner_method, inner_args.size)
                      if func_def = @function_defs[yield_key]?
                        callee_arena = @function_def_arenas[yield_key]? || @arena
                        return inline_yield_function(ctx, func_def, yield_key, inner_receiver_id, inner_args, blk_node, callee_arena)
                      end
                    end
                  end
                end
              end
            end
          end
        end

        # Check if it's a class/module method call (ClassName.new() or Module.method())
        # Can be ConstantNode, IdentifierNode starting with uppercase, or GenericNode
        class_name_str : String? = nil
        if obj_node.is_a?(CrystalV2::Compiler::Frontend::ConstantNode)
          name = String.new(obj_node.name)
          # Resolve type alias with chain resolution (check both @type_aliases and LIBC_TYPE_ALIASES)
          resolved = @type_aliases[name]? || LIBC_TYPE_ALIASES[name]? || name
          # Chain resolve (e.g., LibCrypto::ULong -> LibC::ULong -> UInt64) - max 10 iterations to prevent cycles
          depth = 0
          while (next_resolved = @type_aliases[resolved]? || LIBC_TYPE_ALIASES[resolved]?) && next_resolved != resolved && depth < 10
            resolved = next_resolved
            depth += 1
          end
          class_name_str = resolved
          # If the short name isn't a known class, try to resolve using current namespace
          unless @class_info.has_key?(class_name_str) || resolved != name
            class_name_str = resolve_class_name_in_context(name)
          end
        elsif obj_node.is_a?(CrystalV2::Compiler::Frontend::IdentifierNode)
          name = String.new(obj_node.name)
          # Resolve type alias with chain resolution (check both @type_aliases and LIBC_TYPE_ALIASES)
          resolved_name = @type_aliases[name]? || LIBC_TYPE_ALIASES[name]? || name
          # Chain resolve (e.g., LibCrypto::ULong -> LibC::ULong -> UInt64) - max 10 iterations
          depth = 0
          while (next_resolved = @type_aliases[resolved_name]? || LIBC_TYPE_ALIASES[resolved_name]?) && next_resolved != resolved_name && depth < 10
            resolved_name = next_resolved
            depth += 1
          end
          # Check if it's a class name (starts with uppercase and is known class)
          # OR a module name (check if Module.method exists in function_types)
          if resolved_name[0].uppercase?
            # Prefer nested types in the current namespace over top-level types.
            resolved_name = resolve_class_name_in_context(resolved_name)
            if @class_info.has_key?(resolved_name)
              class_name_str = resolved_name
            elsif is_module_method?(resolved_name, method_name)
              # It's a module method call
              class_name_str = resolved_name
            elsif @generic_templates.has_key?(resolved_name) && method_name == "new"
              # Calling .new on a generic template (e.g., Array.new, Hash.new)
              # Try to infer type argument from constructor arguments or block
              inferred_type = infer_generic_type_arg(resolved_name, node.args, node.block, ctx)
              if inferred_type
                specialized_name = "#{resolved_name}(#{inferred_type})"
                # Monomorphize if not already done
                if !@monomorphized.includes?(specialized_name)
                  monomorphize_generic_class(resolved_name, [inferred_type], specialized_name)
                end
                class_name_str = specialized_name
              else
                # Can't infer type - use fallback or report error
                # For now, use String as default for Array (common case)
                if resolved_name == "Array"
                  specialized_name = "Array(String)"
                  if !@monomorphized.includes?(specialized_name)
                    monomorphize_generic_class(resolved_name, ["String"], specialized_name)
                  end
                  class_name_str = specialized_name
                end
              end
            else
              # For primitive types and aliases not in class_info, use the resolved name directly
              # This handles cases like ULong -> UInt64 where UInt64 isn't registered as a class
              class_name_str = resolved_name
            end
          end
        elsif obj_node.is_a?(CrystalV2::Compiler::Frontend::GenericNode)
          # Generic type like Box(Int32).new()
          base_name = resolve_path_like_name(obj_node.base_type)
          if base_name
            base_name = resolve_type_alias_chain(base_name)
            normalize_typeof_name = ->(type_name : String) : String {
              if type_name == "Void" || type_name == "Unknown" || type_name.includes?("|")
                "Pointer(Void)"
              else
                type_name
              end
            }

            type_args = obj_node.type_args.map do |arg_id|
              arg_node = @arena[arg_id]
              arg_name = case arg_node
                         when CrystalV2::Compiler::Frontend::TypeofNode
                           inner = arg_node.args.first?
                           inner ? resolve_typeof_expr(inner) : "Pointer(Void)"
                         else
                           stringify_type_expr(arg_id) || "Unknown"
                         end
              arg_name = resolve_typeof_in_type_string(arg_name)
              arg_name = normalize_typeof_name.call(arg_name)
              @type_param_map[arg_name]? || arg_name
            end

            # Create specialized class name like Box(Int32)
            class_name_str = "#{base_name}(#{type_args.join(", ")})"

            # Monomorphize generic class if not already done
            if !@monomorphized.includes?(class_name_str)
              monomorphize_generic_class(base_name, type_args, class_name_str)
            end
          end
        elsif obj_node.is_a?(CrystalV2::Compiler::Frontend::PathNode)
          # Path like Foo::Bar for nested classes/modules
          full_path = collect_path_string(obj_node)
          # Check if this path is a known class
          if @class_info.has_key?(full_path)
            class_name_str = full_path
          elsif @type_aliases.has_key?(full_path) || LIBC_TYPE_ALIASES.has_key?(full_path)
            # Resolve type alias with chain resolution
            resolved = @type_aliases[full_path]? || LIBC_TYPE_ALIASES[full_path]? || full_path
            # Chain resolve if needed (e.g., LibCrypto::ULong -> LibC::ULong -> UInt64) - max 10 iterations
            depth = 0
            while (next_resolved = @type_aliases[resolved]? || LIBC_TYPE_ALIASES[resolved]?) && next_resolved != resolved && depth < 10
              resolved = next_resolved
              depth += 1
            end
            class_name_str = resolved
          else
            # Even if not in class_info, treat path as class name for class method calls
            # This handles nested classes/modules that may not be fully registered
            class_name_str = full_path
          end
        end

        if class_name_str
          # Check if this is a lib function call (e.g., LibC.puts)
          if extern_func = @module.get_extern_function(class_name_str, method_name)
            # This is a call to an extern C function
            # Lower args and emit extern call with real C name
            arg_ids = expand_splat_args(ctx, node.args)
            return emit_extern_call(ctx, extern_func, arg_ids)
          end
          # Class method call like Counter.new()
          full_method_name = "#{class_name_str}.#{method_name}"
          receiver_id = nil  # Static call, no receiver
          if method_name == "new"
            if class_info = @class_info[class_name_str]?
              generate_allocator(class_name_str, class_info)
            end
          end
        else
          # Instance method call like c.increment()
          receiver_id = lower_expr(ctx, callee_node.object)

          # Try to determine the class from receiver's type
          receiver_type = ctx.type_of(receiver_id)
          ensure_monomorphized_type(receiver_type) unless receiver_type == TypeRef::VOID
          if receiver_type.id > 0
            # Look up class name from type, then resolve method with inheritance
            @class_info.each do |name, info|
              if info.type_ref.id == receiver_type.id
                # Use inheritance-aware method resolution
                full_method_name = resolve_method_with_inheritance(name, method_name)
                break
              end
            end

            # NOTE: Avoid call-site monomorphization here: it can explode compilation time by creating many
            # specialized types reachable only through transient receiver types. Prefer monomorphization from
            # explicit annotations (see `ensure_monomorphized_type`) and constructor calls.
          end
        end

      else
        # Complex callee (e.g., another call result being called)
        # Lower callee as receiver and use "call" as synthetic method name
        receiver_id = lower_node(ctx, callee_node)
        method_name = "call"
      end

      # Handle named arguments by reordering them to match parameter positions
      # Also expand splat arguments (*array -> individual elements)
      has_block_call = !node.block.nil?
      args = if named_args = node.named_args
               reorder_named_args(ctx, node.args, named_args, method_name, full_method_name, has_block_call)
             else
               expand_splat_args(ctx, node.args)
             end
      args = apply_default_args(ctx, args, method_name, full_method_name, has_block_call)
      args = pack_splat_args_for_call(ctx, args, method_name, full_method_name, has_block_call)

      # Handle .times { |i| body } intrinsic BEFORE lowering block
      if method_name == "times" && receiver_id
        if blk_expr = node.block
          blk_node = @arena[blk_expr]
          if blk_node.is_a?(CrystalV2::Compiler::Frontend::BlockNode)
            return lower_times_intrinsic(ctx, receiver_id, blk_node)
          end
        end
      end

      # Handle nil? intrinsic for union types (T | Nil)
      if method_name == "nil?" && receiver_id
        receiver_type = ctx.type_of(receiver_id)
        # Try to get declared type from local variable if receiver is a Load
        declared_type = receiver_type
        if callee_node.is_a?(CrystalV2::Compiler::Frontend::MemberAccessNode)
          obj = @arena[callee_node.object]
          if obj.is_a?(CrystalV2::Compiler::Frontend::IdentifierNode)
            var_name = String.new(obj.name)
            if local_id = ctx.lookup_local(var_name)
              local_type = ctx.type_of(local_id)
              if @module.get_type_descriptor(local_type)
                declared_type = local_type
              end
            end
          end
        end
        # Check if receiver is a union type (has variants) or nilable
        if is_union_or_nilable_type?(declared_type)
          return lower_nil_check_intrinsic(ctx, receiver_id, declared_type)
        end
        # Non-union nil? is a constant for primitives and Nil, and a pointer check for pointers.
        if declared_type == TypeRef::NIL
          lit = Literal.new(ctx.next_id, TypeRef::BOOL, true)
          ctx.emit(lit)
          return lit.id
        elsif declared_type == TypeRef::POINTER
          nil_val = Literal.new(ctx.next_id, TypeRef::POINTER, 0_i64)
          ctx.emit(nil_val)
          eq_check = BinaryOperation.new(ctx.next_id, TypeRef::BOOL, BinaryOp::Eq, receiver_id, nil_val.id)
          ctx.emit(eq_check)
          ctx.register_type(eq_check.id, TypeRef::BOOL)
          return eq_check.id
        else
          lit = Literal.new(ctx.next_id, TypeRef::BOOL, false)
          ctx.emit(lit)
          return lit.id
        end
      end

      # Handle not_nil! intrinsic for union types - extracts non-nil value
      if method_name == "not_nil!" && receiver_id
        receiver_type = ctx.type_of(receiver_id)
        # Try to get declared type from local variable if receiver is a Load
        declared_type = receiver_type
        if callee_node.is_a?(CrystalV2::Compiler::Frontend::MemberAccessNode)
          obj = @arena[callee_node.object]
          if obj.is_a?(CrystalV2::Compiler::Frontend::IdentifierNode)
            var_name = String.new(obj.name)
            if local_id = ctx.lookup_local(var_name)
              local_type = ctx.type_of(local_id)
              if type_desc = @module.get_type_descriptor(local_type)
                declared_type = local_type
              end
            end
          end
        end
        if is_union_or_nilable_type?(declared_type)
          return lower_not_nil_intrinsic(ctx, receiver_id, declared_type)
        end
      end

      # Handle Range#each { |i| body } and Array#each { |x| body } intrinsics
      if method_name == "each"
        # Check if callee is (range).each - MemberAccessNode on RangeNode
        if callee_node.is_a?(CrystalV2::Compiler::Frontend::MemberAccessNode)
          inner_obj = @arena[callee_node.object]
          # Unwrap GroupingNode: (1..3) creates GroupingNode around RangeNode
          if inner_obj.is_a?(CrystalV2::Compiler::Frontend::GroupingNode)
            inner_obj = @arena[inner_obj.expression]
          end
          if inner_obj.is_a?(CrystalV2::Compiler::Frontend::RangeNode)
            if blk_expr = node.block
              blk_node = @arena[blk_expr]
              if blk_node.is_a?(CrystalV2::Compiler::Frontend::BlockNode)
                return lower_range_each_intrinsic(ctx, inner_obj, blk_node)
              end
            end
          end
          # Array#each intrinsic - check if inner_obj is ArrayLiteralNode or identifier
          if inner_obj.is_a?(CrystalV2::Compiler::Frontend::ArrayLiteralNode)
            if blk_expr = node.block
              blk_node = @arena[blk_expr]
              if blk_node.is_a?(CrystalV2::Compiler::Frontend::BlockNode)
                # Lower array first, then call array_each
                array_id = lower_array_literal(ctx, inner_obj)
                return lower_array_each_intrinsic(ctx, array_id, inner_obj.elements.size, blk_node)
              end
            end
          end
        end
        # arr.each where arr is a variable (receiver_id set)
        if receiver_id
          if blk_expr = node.block
            blk_node = @arena[blk_expr]
            if blk_node.is_a?(CrystalV2::Compiler::Frontend::BlockNode)
              # Check if receiver has known array size (from register_type)
              # For now, use dynamic size via ArraySize
              return lower_array_each_dynamic(ctx, receiver_id, blk_node)
            end
          end
        end
      end

      # Handle Array#each_with_index { |elem, idx| ... } intrinsic
      if method_name == "each_with_index"
        if receiver_id
          if blk_expr = node.block
            blk_node = @arena[blk_expr]
            if blk_node.is_a?(CrystalV2::Compiler::Frontend::BlockNode)
              return lower_array_each_with_index_dynamic(ctx, receiver_id, blk_node)
            end
          end
        end
      end

      # Handle Array#map { |x| expr } intrinsic
      if method_name == "map"
        if callee_node.is_a?(CrystalV2::Compiler::Frontend::MemberAccessNode)
          inner_obj = @arena[callee_node.object]
          # Array literal: [1, 2, 3].map { |x| x * 2 }
          if inner_obj.is_a?(CrystalV2::Compiler::Frontend::ArrayLiteralNode)
            if blk_expr = node.block
              blk_node = @arena[blk_expr]
              if blk_node.is_a?(CrystalV2::Compiler::Frontend::BlockNode)
                array_id = lower_array_literal(ctx, inner_obj)
                return lower_array_map_intrinsic(ctx, array_id, inner_obj.elements.size, blk_node)
              end
            end
          end
        end
        # arr.map where arr is a variable (receiver_id set)
        if receiver_id
          if blk_expr = node.block
            blk_node = @arena[blk_expr]
            if blk_node.is_a?(CrystalV2::Compiler::Frontend::BlockNode)
              receiver_type = ctx.type_of(receiver_id)
              type_desc = @module.get_type_descriptor(receiver_type)
              is_array_type = type_desc && (type_desc.kind == TypeKind::Array ||
                type_desc.name.starts_with?("Array") ||
                type_desc.name.starts_with?("StaticArray"))
              return lower_array_map_dynamic(ctx, receiver_id, blk_node) if is_array_type
            end
          end
        end
      end

      # Handle Array#select { |x| condition } intrinsic
      if method_name == "select"
        if callee_node.is_a?(CrystalV2::Compiler::Frontend::MemberAccessNode)
          inner_obj = @arena[callee_node.object]
          # Array literal: [1, 2, 3].select { |x| x > 1 }
          if inner_obj.is_a?(CrystalV2::Compiler::Frontend::ArrayLiteralNode)
            if blk_expr = node.block
              blk_node = @arena[blk_expr]
              if blk_node.is_a?(CrystalV2::Compiler::Frontend::BlockNode)
                # Pass element expressions for compile-time predicate evaluation
                return lower_array_select_intrinsic_with_ast(ctx, inner_obj, blk_node)
              end
            end
          end
        end
        # arr.select where arr is a variable (receiver_id set)
        if receiver_id
          if blk_expr = node.block
            blk_node = @arena[blk_expr]
            if blk_node.is_a?(CrystalV2::Compiler::Frontend::BlockNode)
              return lower_array_select_dynamic(ctx, receiver_id, blk_node)
            end
          end
        end
      end

      # Handle Array#reduce { |acc, elem| ... } intrinsic
      if method_name == "reduce"
        if receiver_id
          if blk_expr = node.block
            blk_node = @arena[blk_expr]
            if blk_node.is_a?(CrystalV2::Compiler::Frontend::BlockNode)
              return lower_array_reduce_dynamic(ctx, receiver_id, blk_node)
            end
          end
        end
      end

      # Collect argument types for name mangling (overloading support)
      arg_types = args.map { |arg_id| ctx.type_of(arg_id) }

      # Compute mangled name based on base name + argument types
      # If no explicit receiver and we're inside a class, try class#method first
      base_method_name = if full_method_name
                           full_method_name
                         elsif receiver_id.nil? && (current = @current_class)
                           # Bare method call inside a class/module - could be self.method()
                           # Use class method separator when in a class method context.
                           sep = @current_method_is_class ? "." : "#"
                           "#{current}#{sep}#{method_name}"
                         else
                           method_name
                         end
      if receiver_id.nil? && full_method_name.nil? && method_name == "main" && @top_level_main_defined
        base_method_name = TOP_LEVEL_MAIN_BASE
      end
      mangled_method_name = mangle_function_name(base_method_name, arg_types)
      if has_block_call
        mangled_with_block = mangle_function_name(base_method_name, arg_types, true)
        if @function_types.has_key?(mangled_with_block) || @yield_functions.includes?(mangled_with_block) || @module.has_function?(mangled_with_block)
          mangled_method_name = mangled_with_block
        end
      end

      if full_method_name && !@function_defs.has_key?(mangled_method_name)
        if entry = lookup_function_def_for_call(full_method_name, args.size, has_block_call)
          mangled_method_name = entry[0]
          base_method_name = mangled_method_name.split("$").first
        end
      end

      if receiver_id && full_method_name.nil? && callee_node.is_a?(CrystalV2::Compiler::Frontend::MemberAccessNode)
        receiver_type = ctx.type_of(receiver_id)
        if type_desc = @module.get_type_descriptor(receiver_type)
          if module_like_type_name?(type_desc.name)
            if module_type_name = module_receiver_type_name(callee_node)
              if resolved = resolve_module_typed_method(method_name, arg_types, module_type_name, has_block_call, @current_class)
                mangled_method_name = resolved
                base_method_name = resolved.split("$").first
              end
            end
          end
        end
      end
      primary_mangled_name = mangled_method_name

      # Handle yield-functions with inline expansion FIRST (before lowering block)
      # Must check with mangled name since that's how yield functions are registered
      #
      # Note: Blocks can be either:
      # 1. In node.block field: for { |x| ... } syntax
      # 2. In node.args: for &.something or &block syntax (passed as argument)
      blk_expr = node.block
      blk_from_args = false

      # Check if block is passed as an argument (for &.something syntax)
      if blk_expr.nil? && node.args.size > 0
        last_arg = @arena[node.args.last]
        if last_arg.is_a?(CrystalV2::Compiler::Frontend::BlockNode) ||
           last_arg.is_a?(CrystalV2::Compiler::Frontend::ProcLiteralNode)
          blk_expr = node.args.last
          blk_from_args = true
        end
      end

      if blk_expr
        blk_node = @arena[blk_expr]
        # Handle both BlockNode (block syntax) and ProcLiteralNode (&.something syntax)
        is_block_node = blk_node.is_a?(CrystalV2::Compiler::Frontend::BlockNode)
        is_proc_node = blk_node.is_a?(CrystalV2::Compiler::Frontend::ProcLiteralNode)

        if is_block_node || is_proc_node
          # For Object#tap, inline directly when we have a receiver
          # tap yields self to the block then returns self
          if receiver_id && method_name == "tap"
            if is_block_node
              block_cast = blk_node.as(CrystalV2::Compiler::Frontend::BlockNode)
              return inline_tap_with_block(ctx, receiver_id, block_cast)
            elsif is_proc_node
              proc_node = blk_node.as(CrystalV2::Compiler::Frontend::ProcLiteralNode)
              return inline_tap_with_proc(ctx, receiver_id, proc_node)
            end
          end

          if is_block_node
            block_cast = blk_node.as(CrystalV2::Compiler::Frontend::BlockNode)
            # Adjust args - remove block from args if it was taken from there
            call_args = blk_from_args ? args[0...-1] : args

            # Check if this is a call to a yield-function using mangled name
            if @yield_functions.includes?(mangled_method_name)
              if func_def = @function_defs[mangled_method_name]?
                callee_arena = @function_def_arenas[mangled_method_name]? || @arena
                return inline_yield_function(ctx, func_def, mangled_method_name, receiver_id, call_args, block_cast, callee_arena)
              end
            end
            # Also try base method name (for functions without overloading)
            if @yield_functions.includes?(base_method_name)
              if func_def = @function_defs[base_method_name]?
                callee_arena = @function_def_arenas[base_method_name]? || @arena
                return inline_yield_function(ctx, func_def, base_method_name, receiver_id, call_args, block_cast, callee_arena)
              end
            end

            # Fallback: try to find yield method by name + arity.
            # This handles inherited methods like Object#tap called on any class.
            # Example: `fd.tap { |x| x.something }` where tap is defined in Object.
            if receiver_id
              if yield_key = find_yield_method_fallback(method_name, call_args.size)
                if func_def = @function_defs[yield_key]?
                  callee_arena = @function_def_arenas[yield_key]? || @arena
                  return inline_yield_function(ctx, func_def, yield_key, receiver_id, call_args, block_cast, callee_arena)
                end
              end
            end
          end
        end
      end

      # Handle String.build { |io| ... } intrinsic
      # This is a common pattern that builds a string using an IO-like builder
      if full_method_name == "String.build"
        if blk_expr = node.block
          blk_node = @arena[blk_expr]
          if blk_node.is_a?(CrystalV2::Compiler::Frontend::BlockNode)
            return lower_string_build_intrinsic(ctx, blk_node)
          end
        end
      end

      # Check for block (ExprId -> must lower to BlockNode) - for non-inline calls only
      # This is after yield function check so we don't emit dead block code
      block_id = if blk_expr = node.block
                   blk_node = @arena[blk_expr]
                   if blk_node.is_a?(CrystalV2::Compiler::Frontend::BlockNode)
                     lower_block_to_block_id(ctx, blk_node)
                   else
                     # Block is some other expression - should not happen in well-formed AST
                     nil
                   end
                 else
                   nil
                 end

      # Try to infer return type using mangled name first, fallback to base name
      # For non-overloaded functions, prefer base name since that's how they're registered in HIR module
      return_type = get_function_return_type(mangled_method_name)

      base_signature_exists = @function_types.has_key?(base_method_name)
      if base_signature_exists && !@function_types.has_key?(mangled_method_name) && mangled_method_name != base_method_name
        return_type = get_function_return_type(base_method_name)
        mangled_method_name = base_method_name
      end

      # Check if function exists in module by base name (for user-defined functions)
      base_func_exists = @module.functions.any? { |f| f.name == base_method_name }

      if base_func_exists
        # Function exists with base name - use that (no mangling needed for simple functions)
        return_type = get_function_return_type(base_method_name) if return_type == TypeRef::VOID
        mangled_method_name = base_method_name
      elsif return_type == TypeRef::VOID && mangled_method_name != base_method_name
        # Try unmangled name as fallback
        return_type = get_function_return_type(base_method_name)
        if return_type != TypeRef::VOID
          mangled_method_name = base_method_name
        end
      end

      if block_id
        if block_return_name = block_return_type_name(ctx, block_id)
          if inferred = resolve_block_dependent_return_type(mangled_method_name, base_method_name, block_return_name)
            return_type = inferred
          end
        end
      end

      # If still not found and receiver_id is set, try to find method in any class.
      # NOTE: Only do this when we couldn't resolve the receiver class; otherwise it can
      # incorrectly bind to a different specialization (e.g., Hash(Int32, Bool)#[]?).
      if return_type == TypeRef::VOID && receiver_id && full_method_name.nil?
        receiver_type = ctx.type_of(receiver_id)
        type_desc = @module.get_type_descriptor(receiver_type)
        if receiver_type == TypeRef::VOID || type_desc.nil?
          @class_info.each do |class_name, info|
            test_base = "#{class_name}##{method_name}"
            test_mangled = mangle_function_name(test_base, arg_types)
            if type = @function_types[test_mangled]?
              return_type = type
              mangled_method_name = test_mangled
              break
            elsif type = @function_types[test_base]?
              return_type = type
              mangled_method_name = test_base
              break
            end
          end
        end
      end

      # Ensure synthetic accessors exist for direct ivar access.
      if return_type == TypeRef::VOID && receiver_id
        if accessor = ensure_accessor_method(ctx, receiver_id, method_name)
          return_type = accessor[0]
          mangled_method_name = accessor[1]
        end
      end

      # Check for primitive binary operator inlining
      # When calling methods like Int32#+ on primitive types, emit BinaryOperation instead of Call
      if receiver_id && args.size == 1
        receiver_type = ctx.type_of(receiver_id)
        if numeric_primitive?(receiver_type)
          if bin_op = binary_op_for_method(method_name)
            # Emit native binary operation instead of method call
            # Return type is same as receiver type for arithmetic, bool for comparisons
            result_type = case bin_op
                          when BinaryOp::Eq, BinaryOp::Ne, BinaryOp::Lt, BinaryOp::Le,
                               BinaryOp::Gt, BinaryOp::Ge, BinaryOp::And, BinaryOp::Or
                            TypeRef::BOOL
                          else
                            receiver_type
                          end
            bin_node = BinaryOperation.new(ctx.next_id, result_type, bin_op, receiver_id, args[0])
            ctx.emit(bin_node)
            return bin_node.id
          end
        end
      end

      if receiver_id && method_name == "ord" && args.empty?
        receiver_type = ctx.type_of(receiver_id)
        if receiver_type == TypeRef::CHAR
          cast = Cast.new(ctx.next_id, TypeRef::INT32, receiver_id, TypeRef::INT32)
          ctx.emit(cast)
          ctx.register_type(cast.id, TypeRef::INT32)
          return cast.id
        end
      end

      if receiver_id && method_name == "unsafe_chr" && args.empty?
        receiver_type = ctx.type_of(receiver_id)
        case receiver_type
        when TypeRef::INT8, TypeRef::INT16, TypeRef::INT32, TypeRef::INT64, TypeRef::INT128,
             TypeRef::UINT8, TypeRef::UINT16, TypeRef::UINT32, TypeRef::UINT64, TypeRef::UINT128
          cast = Cast.new(ctx.next_id, TypeRef::CHAR, receiver_id, TypeRef::CHAR)
          ctx.emit(cast)
          ctx.register_type(cast.id, TypeRef::CHAR)
          return cast.id
        end
      end

      # Check for pointer primitive operations
      # Pointer(T).malloc(count) -> PointerMalloc
      if full_method_name && full_method_name.starts_with?("Pointer(") && method_name == "malloc" && args.size == 1
        element_type = pointer_element_type(full_method_name)
        malloc_node = PointerMalloc.new(ctx.next_id, TypeRef::POINTER, element_type, args[0])
        ctx.emit(malloc_node)
        ctx.register_type(malloc_node.id, TypeRef::POINTER)
        return malloc_node.id
      end

      if full_method_name && full_method_name.starts_with?("Pointer(") && method_name == "null" && args.empty?
        zero = Literal.new(ctx.next_id, TypeRef::INT64, 0_i64)
        ctx.emit(zero)
        ctx.register_type(zero.id, TypeRef::INT64)
        cast = Cast.new(ctx.next_id, TypeRef::POINTER, zero.id, TypeRef::POINTER)
        ctx.emit(cast)
        ctx.register_type(cast.id, TypeRef::POINTER)
        return cast.id
      end

      # ptr.value or ptr[index] -> PointerLoad
      if receiver_id && (method_name == "value" || method_name == "[]")
        receiver_type = ctx.type_of(receiver_id)
        recv_type_desc = @module.get_type_descriptor(receiver_type)
        is_pointer_type = receiver_type == TypeRef::POINTER ||
                          (recv_type_desc && recv_type_desc.name.starts_with?("Pointer"))
        if ENV.has_key?("DEBUG_PTR_VALUE")
          STDERR.puts "[DEBUG_PTR_VALUE] method=#{method_name} receiver_type.id=#{receiver_type.id} desc=#{recv_type_desc.try(&.name)} is_pointer=#{is_pointer_type}"
        end
        if is_pointer_type
          index_id = if method_name == "[]" && args.size == 1
                       args[0]
                     else
                       nil
                     end
          # Return the dereferenced type from Pointer(T) -> T
          deref_type = if recv_type_desc && recv_type_desc.name.starts_with?("Pointer(") && recv_type_desc.name.ends_with?(")")
                         element_type_name = recv_type_desc.name[8...-1]
                         type_ref_for_name(element_type_name)
                       else
                         TypeRef::INT32  # Fallback for untyped pointers
                       end
          load_node = PointerLoad.new(ctx.next_id, deref_type, receiver_id, index_id)
          ctx.emit(load_node)
          ctx.register_type(load_node.id, deref_type)
          return load_node.id
        end
      end

      # ptr.value= or ptr[index]= -> PointerStore
      if receiver_id && (method_name == "value=" || method_name == "[]=")
        receiver_type = ctx.type_of(receiver_id)
        recv_type_desc = @module.get_type_descriptor(receiver_type)
        is_pointer_type = receiver_type == TypeRef::POINTER ||
                          (recv_type_desc && recv_type_desc.name.starts_with?("Pointer"))
        if is_pointer_type
          if method_name == "value=" && args.size == 1
            store_node = PointerStore.new(ctx.next_id, TypeRef::VOID, receiver_id, args[0], nil)
            ctx.emit(store_node)
            return store_node.id
          elsif method_name == "[]=" && args.size == 2
            store_node = PointerStore.new(ctx.next_id, TypeRef::VOID, receiver_id, args[1], args[0])
            ctx.emit(store_node)
            return store_node.id
          end
        end
      end

      # ptr + offset or ptr - offset -> PointerAdd
      if receiver_id && (method_name == "+" || method_name == "-") && args.size == 1
        receiver_type = ctx.type_of(receiver_id)
        recv_type_desc = @module.get_type_descriptor(receiver_type)
        is_pointer_type = receiver_type == TypeRef::POINTER ||
                          (recv_type_desc && recv_type_desc.name.starts_with?("Pointer"))
        if is_pointer_type
          offset_id = args[0]
          # For subtraction, negate the offset
          if method_name == "-"
            neg_one = Literal.new(ctx.next_id, TypeRef::INT32, -1_i64)
            ctx.emit(neg_one)
            ctx.register_type(neg_one.id, TypeRef::INT32)
            neg_offset = BinaryOperation.new(ctx.next_id, TypeRef::INT32, BinaryOp::Mul, offset_id, neg_one.id)
            ctx.emit(neg_offset)
            ctx.register_type(neg_offset.id, TypeRef::INT32)
            offset_id = neg_offset.id
          end
          element_type = TypeRef::INT32  # TODO: track pointer element type
          add_node = PointerAdd.new(ctx.next_id, TypeRef::POINTER, receiver_id, offset_id, element_type)
          ctx.emit(add_node)
          ctx.register_type(add_node.id, TypeRef::POINTER)
          return add_node.id
        end
      end

      # ptr.realloc(new_count) -> PointerRealloc
      if receiver_id && method_name == "realloc" && args.size == 1
        receiver_type = ctx.type_of(receiver_id)
        recv_type_desc = @module.get_type_descriptor(receiver_type)
        is_pointer_type = receiver_type == TypeRef::POINTER ||
                          (recv_type_desc && recv_type_desc.name.starts_with?("Pointer"))
        if is_pointer_type
          realloc_node = PointerRealloc.new(ctx.next_id, TypeRef::POINTER, receiver_id, args[0])
          ctx.emit(realloc_node)
          ctx.register_type(realloc_node.id, TypeRef::POINTER)
          return realloc_node.id
        end
      end

      # For allocator calls (ClassName.new), ensure return type is the class type
      # This handles cases where the mangled name wasn't found in @function_types
      if method_name == "new" && full_method_name && return_type == TypeRef::VOID
        # Extract class name from "ClassName.new"
        class_name = full_method_name.rchop(".new")
        if class_info = @class_info[class_name]?
          return_type = class_info.type_ref
        else
          # Unknown class (probably from stdlib) - use pointer type as fallback
          # This ensures exception types like ArgumentError work correctly
          return_type = TypeRef::POINTER
        end
      end

      # For method calls that return void but are likely returning the receiver or a value,
      # use pointer type as fallback to avoid void type errors in LLVM
      if return_type == TypeRef::VOID && receiver_id
        # Methods that typically return self or a collection (stdlib methods)
        methods_returning_self_or_value = ["to_a", "to_s", "map", "select", "reduce", "each",
                                           "first", "last", "dup", "clone", "cover",
                                           "compact", "flatten", "sort", "reverse", "uniq",
                                           "join", "split", "strip", "chomp", "chars",
                                           "keys", "values", "value",  # 'value' is common getter
                                           "lines", "bytes", "codepoints", "graphemes",
                                           "rstrip", "lstrip", "downcase", "upcase", "capitalize",
                                           "gsub", "sub", "tr", "delete", "squeeze",
                                           "rjust", "ljust", "center", "each_line",
                                           "each_with_index", "map_with_index", "select_with_index",
                                           "index", "rindex", "find", "find_index", "count",
                                           "sum", "product", "min", "max", "minmax", "sample",
                                           "take", "drop", "take_while", "drop_while",
                                           "group_by", "partition", "zip", "transpose",
                                           "shuffle", "rotate", "pop", "shift", "slice",
                                           "to_slice", "to_unsafe", "to_h", "to_set", "copy_from"]
        if methods_returning_self_or_value.includes?(method_name)
          return_type = TypeRef::POINTER
        end
      end

      # Methods that return the same type as the receiver
      methods_returning_receiver_type = ["tap", "clamp", "abs", "ceil", "floor", "round", "truncate"]
      if return_type == TypeRef::VOID && receiver_id && methods_returning_receiver_type.includes?(method_name)
        return_type = ctx.type_of(receiver_id)
      end

      # For unqualified method calls (no class prefix in the call name),
      # if return type is still void, use pointer as fallback
      # This handles stdlib methods that aren't defined in the bootstrap sources
      if return_type == TypeRef::VOID && receiver_id && !mangled_method_name.includes?("#") && !mangled_method_name.includes?(".")
        return_type = TypeRef::POINTER
      end

      # For class method calls (no receiver), handle common builder patterns
      # These methods return a new object (typically the class's instance type)
      if return_type == TypeRef::VOID && receiver_id.nil?
        class_methods_returning_value = ["build", "new", "create", "from", "parse", "load", "open"]
        if class_methods_returning_value.includes?(method_name)
          return_type = TypeRef::POINTER
        end
      end

      # For module method calls (within the same module), if return type is still void,
      # try to find the method's declared return type or use POINTER fallback
      # Module methods returning String/Array/etc. should not be void
      if return_type == TypeRef::VOID && receiver_id.nil? && @current_class
        # Check if it's a method that typically returns a value (not a side-effect only method)
        # Methods with "build_", "get_", "create_", "make_", "extract_", "format_" prefixes
        # typically return values
        if method_name.starts_with?("build_") || method_name.starts_with?("get_") ||
           method_name.starts_with?("create_") || method_name.starts_with?("make_") ||
           method_name.starts_with?("extract_") || method_name.starts_with?("format_") ||
           method_name.starts_with?("parse_") || method_name.starts_with?("to_") ||
           method_name.ends_with?("_lines") || method_name.ends_with?("_string") ||
           method_name.ends_with?("_snippet") || method_name.ends_with?("_range") ||
           method_name.ends_with?("_gutter") || method_name.ends_with?("_segment")
          return_type = TypeRef::POINTER
        end
      end

      call_virtual = false
      if receiver_id
        if type_desc = @module.get_type_descriptor(ctx.type_of(receiver_id))
          call_virtual = type_desc.kind.in?(TypeKind::Union, TypeKind::Module)
        end
      end

      # Lazily lower target function bodies (avoid full stdlib lowering).
      remember_callsite_arg_types(primary_mangled_name, arg_types)
      if mangled_method_name != primary_mangled_name
        remember_callsite_arg_types(mangled_method_name, arg_types)
      end
      lower_function_if_needed(primary_mangled_name)
      if mangled_method_name != primary_mangled_name
        lower_function_if_needed(mangled_method_name)
      end

      # Coerce arguments to union types if needed
      # This handles cases like passing Int32 to a parameter of type Int32 | Nil
      args = coerce_args_to_param_types(ctx, args, mangled_method_name)

      call = Call.new(ctx.next_id, return_type, receiver_id, mangled_method_name, args, block_id, call_virtual)
      ctx.emit(call)
      ctx.register_type(call.id, return_type)
      call.id
    end

    # Expand splat arguments in a call
    # *array becomes individual elements at compile time (if array is literal)
    private def expand_splat_args(ctx : LoweringContext, arg_exprs : Array(ExprId)) : Array(ValueId)
      result = [] of ValueId

      arg_exprs.each do |arg_expr|
        arg_node = @arena[arg_expr]

        if arg_node.is_a?(CrystalV2::Compiler::Frontend::SplatNode)
          # Splat - try to expand if inner is array literal
          inner_node = @arena[arg_node.expr]
          if inner_node.is_a?(CrystalV2::Compiler::Frontend::ArrayLiteralNode)
            # Expand array elements as individual arguments
            inner_node.elements.each do |elem_id|
              result << lower_expr(ctx, elem_id)
            end
          else
            # Non-literal splat - just pass through (runtime behavior not fully supported)
            result << lower_expr(ctx, arg_node.expr)
          end
        else
          result << lower_expr(ctx, arg_expr)
        end
      end

      result
    end

    private def pack_splat_args_for_call(
      ctx : LoweringContext,
      args : Array(ValueId),
      method_name : String,
      full_method_name : String?,
      has_block_call : Bool
    ) : Array(ValueId)
      func_name = if full_method_name
                    full_method_name
                  elsif current = @current_class
                    sep = @current_method_is_class ? "." : "#"
                    "#{current}#{sep}#{method_name}"
                  else
                    method_name
                  end
      entry = lookup_function_def_for_call(func_name, args.size, has_block_call)
      return args unless entry

      func_def = entry[1]
      return args unless params = func_def.params

      splat_index : Int32? = nil
      splat_is_last = true
      param_index = 0
      saw_splat = false
      params.each do |param|
        next if param.is_block || named_only_separator?(param)
        if param.is_splat && !param.is_double_splat
          splat_index = param_index
          saw_splat = true
        elsif saw_splat
          splat_is_last = false
          break
        end
        param_index += 1
      end

      return args unless splat_index && splat_is_last
      return args if args.size <= splat_index

      fixed = args[0, splat_index]
      splat_args = args[splat_index..-1] || [] of ValueId
      return args if splat_args.empty?

      splat_types = splat_args.map { |arg_id| ctx.type_of(arg_id) }
      tuple_type = tuple_type_from_arg_types(splat_types)
      return args if tuple_type == TypeRef::VOID

      tuple_alloc = Allocate.new(ctx.next_id, tuple_type, splat_args)
      ctx.emit(tuple_alloc)

      fixed + [tuple_alloc.id]
    end

    # Coerce arguments to match parameter types (e.g., wrap concrete types in unions)
    # This is needed when passing Int32 to a parameter of type Int32 | Nil
    private def coerce_args_to_param_types(
      ctx : LoweringContext,
      args : Array(ValueId),
      method_name : String
    ) : Array(ValueId)
      # Find the target function to get parameter types
      target_func = @module.functions.find { |f| f.name == method_name }

      # If not found, try fuzzy match (for mangled names)
      unless target_func
        base_name = method_name.split("$").first
        target_func = @module.functions.find { |f| f.name.split("$").first == base_name }
      end

      # Try another fuzzy match: method name may have different type suffix but same base
      unless target_func
        # Look for functions that match the method base (class#method or Module.method)
        if method_name.includes?("#") || method_name.includes?(".")
          # Extract class and method parts: "Class.method$Types" -> "Class", "method"
          separator = method_name.includes?("#") ? "#" : "."
          parts = method_name.split(separator, 2)
          if parts.size == 2
            class_part = parts[0]
            method_with_types = parts[1]
            # Extract just the method name (before $type suffix)
            method_part = method_with_types.split("$").first
            # Match: same class, same method name, possibly different type suffixes
            target_func = @module.functions.find do |f|
              if f.name.includes?(separator)
                f_parts = f.name.split(separator, 2)
                f_parts.size == 2 &&
                  f_parts[0] == class_part &&
                  f_parts[1].split("$").first == method_part
              else
                false
              end
            end
          end
        end
      end

      return args unless target_func

      # Receiver counts as first implicit parameter for instance methods
      # The function params include self for instance methods
      params = target_func.params

      # Coerce each argument
      result = [] of ValueId
      args.each_with_index do |arg_id, idx|
        param = params[idx]?
        if param.nil?
          # No more params - just pass through
          result << arg_id
          next
        end

        arg_type = ctx.type_of(arg_id)
        param_type = param.type

        # Check if we need to coerce: arg is concrete type, param is union containing that type
        if needs_union_coercion?(arg_type, param_type)
          # Determine variant id: 0 for the concrete type, 1 for Nil typically
          variant_id = get_union_variant_id(param_type, arg_type)
          wrap = UnionWrap.new(ctx.next_id, param_type, arg_id, variant_id)
          ctx.emit(wrap)
          ctx.register_type(wrap.id, param_type)
          result << wrap.id
        else
          result << arg_id
        end
      end

      result
    end

    # Check if arg_type needs to be wrapped into param_type union
    private def needs_union_coercion?(arg_type : TypeRef, param_type : TypeRef) : Bool
      # Quick check: same type, no coercion needed
      return false if arg_type == param_type

      # Check if param_type is a union type (has type descriptor with union marker)
      if type_desc = @module.get_type_descriptor(param_type)
        if type_desc.kind == TypeKind::Union
          # Check if arg_type is one of the union variants
          # For now, assume simple unions like Int32 | Nil
          # The arg_type should be a non-union type that's part of the union
          return true if arg_type != param_type && !is_union_type?(arg_type)
        end
      end

      # Also check by type naming convention: types with "___" are usually unions
      # e.g., Int32___Nil is Int32 | Nil
      if param_type.id > 0
        @module.types.each_with_index do |desc, idx|
          if TypeRef.new(TypeRef::FIRST_USER_TYPE + idx.to_u32) == param_type
            if desc.name.includes?("___") || desc.kind == TypeKind::Union
              # Param is union type - check if arg is a concrete type
              return !is_union_type?(arg_type)
            end
          end
        end
      end

      false
    end

    # Check if a type is a union type
    private def is_union_type?(type : TypeRef) : Bool
      if type_desc = @module.get_type_descriptor(type)
        return type_desc.kind == TypeKind::Union || type_desc.name.includes?("___")
      end
      false
    end

    # Check if a type is a nilable Int32 union (Int32 | Nil)
    private def is_nilable_int32_union?(type : TypeRef) : Bool
      if type_desc = @module.get_type_descriptor(type)
        # Check if it's a union with Int32 in the name (Int32___Nil or Int32 | Nil)
        if type_desc.kind == TypeKind::Union || type_desc.name.includes?("___")
          name = type_desc.name
          return name.includes?("Int32") && (name.includes?("Nil") || name.includes?("nil"))
        end
      end
      false
    end

    private def lookup_function_def_for_call(
      func_name : String,
      arg_count : Int32,
      has_block : Bool
    ) : Tuple(String, CrystalV2::Compiler::Frontend::DefNode)?
      if func_def = @function_defs[func_name]?
        return {func_name, func_def}
      end

      best : CrystalV2::Compiler::Frontend::DefNode? = nil
      best_name : String? = nil
      best_param_count = Int32::MAX

      @function_defs.each do |name, def_node|
        next unless name == func_name || name.starts_with?("#{func_name}$")
        params = def_node.params
        next unless params

        if has_block
          next unless params.any?(&.is_block)
        else
          next if params.any?(&.is_block)
        end

        param_count = params.count { |p| !p.is_block && !named_only_separator?(p) }
        has_splat = params.any? { |p| p.is_splat && !named_only_separator?(p) }
        has_double_splat = params.any? { |p| p.is_double_splat }
        required = params.count do |p|
          !p.is_block && !named_only_separator?(p) && p.default_value.nil? && !p.is_splat && !p.is_double_splat
        end

        next if arg_count < required
        next if arg_count > param_count && !has_splat && !has_double_splat

        if param_count < best_param_count
          best = def_node
          best_name = name
          best_param_count = param_count
        end
      end

      return nil unless best && best_name
      {best_name, best}
    end

    # Reorder named arguments to match parameter positions
    private def reorder_named_args(
      ctx : LoweringContext,
      positional_args : Array(ExprId),
      named_args : Array(CrystalV2::Compiler::Frontend::NamedArgument),
      method_name : String,
      full_method_name : String?,
      has_block_call : Bool
    ) : Array(ValueId)
      # First, lower positional args
      result = positional_args.map { |arg| lower_expr(ctx, arg) }
      provided = Array(Bool).new(result.size, true)

      # Get parameter names from function definition
      func_name = full_method_name || method_name
      func_entry = lookup_function_def_for_call(func_name, positional_args.size + named_args.size, has_block_call)
      func_def = func_entry ? func_entry[1] : nil
      def_arena = func_entry ? (@function_def_arenas[func_entry[0]]? || @arena) : @arena

      if func_def && (params = func_def.params)
        param_call_names = [] of String
        param_local_names = [] of String
        param_defaults = [] of ExprId?
        param_types = [] of TypeRef
        params.each do |p|
          next if p.is_block
          next if named_only_separator?(p)
          call_name = if ext = p.external_name
                        String.new(ext)
                      elsif name = p.name
                        String.new(name)
                      else
                        ""
                      end
          local_name = p.name ? String.new(p.name.not_nil!) : ""
          param_call_names << call_name
          param_local_names << local_name
          param_defaults << p.default_value
          if ta = p.type_annotation
            param_types << type_ref_for_name(String.new(ta))
          else
            param_types << TypeRef::VOID
          end
        end

        # Process named args
        named_args.each do |named_arg|
          arg_name = String.new(named_arg.name)
          arg_value = lower_expr(ctx, named_arg.value)

          # Find position of this parameter
          idx = param_call_names.index(arg_name)
          if idx
            # Extend result array if needed
            while result.size <= idx
              # Fill with nil placeholder (will be replaced)
              nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
              ctx.emit(nil_lit)
              result << nil_lit.id
              provided << false
            end
            result[idx] = arg_value
            provided[idx] = true
          else
            # Unknown parameter name - just append
            result << arg_value
            provided << true
          end
        end

        # Fill missing args with defaults (evaluate in a param-local context)
        if param_defaults.any?
          while result.size < param_call_names.size
            nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
            ctx.emit(nil_lit)
            result << nil_lit.id
            provided << false
          end

          saved_locals = ctx.save_locals
          begin
            locals = ctx.all_locals
            param_local_names.each_with_index do |name, idx|
              next if name.empty?
              if idx < result.size && provided[idx]
                locals[name] = result[idx]
              end
            end

            param_defaults.each_with_index do |default_expr, idx|
              next if provided[idx]
              next unless default_expr
              default_id = with_arena(def_arena) { lower_expr(ctx, default_expr) }
              if param_types[idx] != TypeRef::VOID
                ctx.register_type(default_id, param_types[idx])
              end
              result[idx] = default_id
              provided[idx] = true
              name = param_local_names[idx]
              locals[name] = default_id unless name.empty?
            end
          ensure
            ctx.restore_locals(saved_locals)
          end
        end
      else
        # No function definition found - just append named args in order
        named_args.each do |named_arg|
          result << lower_expr(ctx, named_arg.value)
        end
      end

      result
    end

    private def apply_default_args(
      ctx : LoweringContext,
      args : Array(ValueId),
      method_name : String,
      full_method_name : String?,
      has_block_call : Bool
    ) : Array(ValueId)
      func_name = full_method_name || method_name
      func_entry = lookup_function_def_for_call(func_name, args.size, has_block_call)
      return args unless func_entry
      func_def = func_entry[1]
      def_arena = @function_def_arenas[func_entry[0]]? || @arena
      params = func_def.params
      return args unless params

      param_local_names = [] of String
      param_defaults = [] of ExprId?
      param_types = [] of TypeRef

      params.each do |p|
        next if p.is_block
        next if named_only_separator?(p)
        local_name = p.name ? String.new(p.name.not_nil!) : ""
        param_local_names << local_name
        param_defaults << p.default_value
        if ta = p.type_annotation
          param_types << type_ref_for_name(String.new(ta))
        else
          param_types << TypeRef::VOID
        end
      end

      return args if args.size >= param_defaults.size

      saved_locals = ctx.save_locals
      begin
        locals = ctx.all_locals
        param_local_names.each_with_index do |name, idx|
          break if idx >= args.size
          locals[name] = args[idx] unless name.empty?
        end

        idx = args.size
        while idx < param_defaults.size
          default_expr = param_defaults[idx]
          break unless default_expr
          default_id = with_arena(def_arena) { lower_expr(ctx, default_expr) }
          if param_types[idx] != TypeRef::VOID
            ctx.register_type(default_id, param_types[idx])
          end
          args << default_id
          local_name = param_local_names[idx]
          locals[local_name] = default_id unless local_name.empty?
          idx += 1
        end
      ensure
        ctx.restore_locals(saved_locals)
      end

      args
    end

    # Intrinsic: n.times { |i| body }
    # Expands to: i = 0; while i < n { body; i += 1 }
    # Uses phi nodes for loop variable AND mutable external variables
    private def lower_times_intrinsic(ctx : LoweringContext, count_id : ValueId, block : CrystalV2::Compiler::Frontend::BlockNode) : ValueId
      # Get block param name (default to "i" if not specified)
      param_name = if params = block.params
                     if first_param = params.first?
                       if pname = first_param.name
                         String.new(pname)
                       else
                         "__times_i"
                       end
                     else
                       "__times_i"
                     end
                   else
                     "__times_i"
                   end

      # Collect variables that might be assigned in the block body (same as while loop)
      assigned_vars = collect_assigned_vars(block.body)
      # Remove the block parameter from assigned vars - it's handled separately
      assigned_vars = assigned_vars.reject { |v| v == param_name }
      inline_vars = Set(String).new

      # Save initial values of mutable variables before the loop
      entry_block = ctx.current_block
      initial_values = {} of String => ValueId
      assigned_vars.each do |var_name|
        if val = lookup_local_for_phi(ctx, var_name, inline_vars)
          initial_values[var_name] = val
        end
      end

      # Initial counter value
      zero = Literal.new(ctx.next_id, TypeRef::INT32, 0_i64)
      ctx.emit(zero)

      # Create blocks
      cond_block = ctx.create_block
      body_block = ctx.create_block
      incr_block = ctx.create_block
      exit_block = ctx.create_block

      # Jump to condition
      ctx.terminate(Jump.new(cond_block))

      # Condition block with phi for counter
      ctx.current_block = cond_block
      counter_phi = Phi.new(ctx.next_id, TypeRef::INT32)
      counter_phi.add_incoming(entry_block, zero.id)
      ctx.emit(counter_phi)

      # Create phi nodes for mutable external variables (same pattern as lower_while)
      phi_nodes = {} of String => Phi
      assigned_vars.each do |var_name|
        if initial_val = initial_values[var_name]?
          var_type = ctx.type_of(initial_val)
          phi = Phi.new(ctx.next_id, var_type)
          phi.add_incoming(entry_block, initial_val)
          ctx.emit(phi)
          phi_nodes[var_name] = phi
          ctx.register_local(var_name, phi.id)
          if inline_vars.includes?(var_name)
            @inline_caller_locals_stack[-1][var_name] = phi.id
          end
        end
      end

      # Register counter for use in body
      ctx.register_local(param_name, counter_phi.id)
      ctx.register_type(counter_phi.id, TypeRef::INT32)

      # Compare: i < n
      cmp = BinaryOperation.new(ctx.next_id, TypeRef::BOOL, BinaryOp::Lt, counter_phi.id, count_id)
      ctx.emit(cmp)
      ctx.terminate(Branch.new(cmp.id, body_block, exit_block))

      # Body block
      ctx.current_block = body_block
      ctx.push_scope(ScopeKind::Block)

      # Lower block body
      pushed_inline = false
      if !inline_vars.empty?
        @inline_loop_vars_stack << inline_vars
        pushed_inline = true
      end
      begin
        lower_body(ctx, block.body)
      ensure
        @inline_loop_vars_stack.pop? if pushed_inline
      end
      body_exit_block = ctx.current_block
      ctx.pop_scope

      # Jump to increment block
      ctx.terminate(Jump.new(incr_block))

      # Increment block: i + 1
      ctx.current_block = incr_block
      one = Literal.new(ctx.next_id, TypeRef::INT32, 1_i64)
      ctx.emit(one)
      new_i = BinaryOperation.new(ctx.next_id, TypeRef::INT32, BinaryOp::Add, counter_phi.id, one.id)
      ctx.emit(new_i)

      # Add incoming to counter phi from increment block
      counter_phi.add_incoming(incr_block, new_i.id)

      # Patch mutable variable phi nodes - incoming from incr_block (the actual predecessor of cond_block)
      assigned_vars.each do |var_name|
        if phi = phi_nodes[var_name]?
          if updated_val = ctx.lookup_local(var_name)
            # Add incoming from incr_block (not body_block!)
            phi.add_incoming(incr_block, updated_val)
          end
        end
      end

      # Jump back to condition
      ctx.terminate(Jump.new(cond_block))

      # Exit block - restore phi values for use after the loop
      ctx.current_block = exit_block
      phi_nodes.each do |var_name, phi|
        ctx.register_local(var_name, phi.id)
      end

      nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
      ctx.emit(nil_lit)
      nil_lit.id
    end

    # Intrinsic: (begin..end).each { |i| body } or (begin...end).each { |i| body }
    # Expands to: i = begin; while i <= end (or < for exclusive) { body; i += 1 }
    private def lower_range_each_intrinsic(
      ctx : LoweringContext,
      range : CrystalV2::Compiler::Frontend::RangeNode,
      block : CrystalV2::Compiler::Frontend::BlockNode
    ) : ValueId
      # Get block param name
      param_name = if params = block.params
                     if first_param = params.first?
                       if pname = first_param.name
                         String.new(pname)
                       else
                         "__range_i"
                       end
                     else
                       "__range_i"
                     end
                   else
                     "__range_i"
                   end

      # Lower range bounds
      begin_id = lower_expr(ctx, range.begin_expr)
      end_id = lower_expr(ctx, range.end_expr)

      # Collect mutable vars (same as times)
      assigned_vars = collect_assigned_vars(block.body)
      assigned_vars = assigned_vars.reject { |v| v == param_name }
      inline_vars = Set(String).new

      entry_block = ctx.current_block
      initial_values = {} of String => ValueId
      assigned_vars.each do |var_name|
        if val = lookup_local_for_phi(ctx, var_name, inline_vars)
          initial_values[var_name] = val
        end
      end

      # Create blocks
      cond_block = ctx.create_block
      body_block = ctx.create_block
      incr_block = ctx.create_block
      exit_block = ctx.create_block

      ctx.terminate(Jump.new(cond_block))

      # Condition block with phi
      ctx.current_block = cond_block
      counter_phi = Phi.new(ctx.next_id, TypeRef::INT32)
      counter_phi.add_incoming(entry_block, begin_id)
      ctx.emit(counter_phi)

      # Phi for mutable vars
      phi_nodes = {} of String => Phi
      assigned_vars.each do |var_name|
        if initial_val = initial_values[var_name]?
          var_type = ctx.type_of(initial_val)
          phi = Phi.new(ctx.next_id, var_type)
          phi.add_incoming(entry_block, initial_val)
          ctx.emit(phi)
          phi_nodes[var_name] = phi
          ctx.register_local(var_name, phi.id)
          if inline_vars.includes?(var_name)
            @inline_caller_locals_stack[-1][var_name] = phi.id
          end
        end
      end

      ctx.register_local(param_name, counter_phi.id)
      ctx.register_type(counter_phi.id, TypeRef::INT32)

      # Compare: i <= end (inclusive) or i < end (exclusive)
      cmp_op = range.exclusive ? BinaryOp::Lt : BinaryOp::Le
      cmp = BinaryOperation.new(ctx.next_id, TypeRef::BOOL, cmp_op, counter_phi.id, end_id)
      ctx.emit(cmp)
      ctx.terminate(Branch.new(cmp.id, body_block, exit_block))

      # Body block
      ctx.current_block = body_block
      ctx.push_scope(ScopeKind::Block)
      pushed_inline = false
      if !inline_vars.empty?
        @inline_loop_vars_stack << inline_vars
        pushed_inline = true
      end
      begin
        lower_body(ctx, block.body)
      ensure
        @inline_loop_vars_stack.pop? if pushed_inline
      end
      ctx.pop_scope
      ctx.terminate(Jump.new(incr_block))

      # Increment block
      ctx.current_block = incr_block
      one = Literal.new(ctx.next_id, TypeRef::INT32, 1_i64)
      ctx.emit(one)
      new_i = BinaryOperation.new(ctx.next_id, TypeRef::INT32, BinaryOp::Add, counter_phi.id, one.id)
      ctx.emit(new_i)

      counter_phi.add_incoming(incr_block, new_i.id)

      # Patch mutable var phis
      assigned_vars.each do |var_name|
        if phi = phi_nodes[var_name]?
          if updated_val = ctx.lookup_local(var_name)
            phi.add_incoming(incr_block, updated_val)
          end
        end
      end

      ctx.terminate(Jump.new(cond_block))

      # Exit block
      ctx.current_block = exit_block
      phi_nodes.each do |var_name, phi|
        ctx.register_local(var_name, phi.id)
      end

      nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
      ctx.emit(nil_lit)
      nil_lit.id
    end

    # Intrinsic: arr.each { |x| body } for static array with known size
    private def lower_array_each_intrinsic(
      ctx : LoweringContext,
      array_id : ValueId,
      array_size : Int32,
      block : CrystalV2::Compiler::Frontend::BlockNode
    ) : ValueId
      # Get block param name
      param_name = if params = block.params
                     if first_param = params.first?
                       if pname = first_param.name
                         String.new(pname)
                       else
                         "__arr_elem"
                       end
                     else
                       "__arr_elem"
                     end
                   else
                     "__arr_elem"
                   end

      # Collect mutable vars
      assigned_vars = collect_assigned_vars(block.body)
      assigned_vars = assigned_vars.reject { |v| v == param_name }
      inline_vars = Set(String).new

      entry_block = ctx.current_block
      initial_values = {} of String => ValueId
      assigned_vars.each do |var_name|
        if val = lookup_local_for_phi(ctx, var_name, inline_vars)
          initial_values[var_name] = val
        end
      end

      # Emit zero in entry block BEFORE jump (required for phi SSA)
      zero = Literal.new(ctx.next_id, TypeRef::INT32, 0_i64)
      ctx.emit(zero)

      # Create blocks
      cond_block = ctx.create_block
      body_block = ctx.create_block
      incr_block = ctx.create_block
      exit_block = ctx.create_block

      ctx.terminate(Jump.new(cond_block))

      # Condition block with index phi
      ctx.current_block = cond_block
      index_phi = Phi.new(ctx.next_id, TypeRef::INT32)
      index_phi.add_incoming(entry_block, zero.id)
      ctx.emit(index_phi)

      # Phi for mutable vars
      phi_nodes = {} of String => Phi
      assigned_vars.each do |var_name|
        if initial_val = initial_values[var_name]?
          var_type = ctx.type_of(initial_val)
          phi = Phi.new(ctx.next_id, var_type)
          phi.add_incoming(entry_block, initial_val)
          ctx.emit(phi)
          phi_nodes[var_name] = phi
          ctx.register_local(var_name, phi.id)
          if inline_vars.includes?(var_name)
            @inline_caller_locals_stack[-1][var_name] = phi.id
          end
        end
      end

      # Compare: i < size
      size_lit = Literal.new(ctx.next_id, TypeRef::INT32, array_size.to_i64)
      ctx.emit(size_lit)
      cmp = BinaryOperation.new(ctx.next_id, TypeRef::BOOL, BinaryOp::Lt, index_phi.id, size_lit.id)
      ctx.emit(cmp)
      ctx.terminate(Branch.new(cmp.id, body_block, exit_block))

      # Body block
      ctx.current_block = body_block
      ctx.push_scope(ScopeKind::Block)

      # Get element: arr[i]
      element_type = ctx.type_of(array_id)
      if element_type == TypeRef::VOID
        element_type = TypeRef::INT32
      end
      index_get = IndexGet.new(ctx.next_id, element_type, array_id, index_phi.id)
      ctx.emit(index_get)
      ctx.register_type(index_get.id, element_type)

      # Handle tuple destructuring: |(a, b, ...)| vs single param |x|
      if params = block.params
        if params.size > 1
          # Tuple destructuring - extract each element
          params.each_with_index do |param, idx|
            if pname = param.name
              name = String.new(pname)
              idx_lit = Literal.new(ctx.next_id, TypeRef::INT32, idx.to_i64)
              ctx.emit(idx_lit)
              elem_extract = IndexGet.new(ctx.next_id, TypeRef::POINTER, index_get.id, idx_lit.id)
              ctx.emit(elem_extract)
              ctx.register_type(elem_extract.id, TypeRef::POINTER)
              ctx.register_local(name, elem_extract.id)
            end
          end
        else
          # Single parameter - bind whole element
          ctx.register_local(param_name, index_get.id)
        end
      else
        ctx.register_local(param_name, index_get.id)
      end

      pushed_inline = false
      if !inline_vars.empty?
        @inline_loop_vars_stack << inline_vars
        pushed_inline = true
      end
      begin
        lower_body(ctx, block.body)
      ensure
        @inline_loop_vars_stack.pop? if pushed_inline
      end
      ctx.pop_scope
      ctx.terminate(Jump.new(incr_block))

      # Increment block
      ctx.current_block = incr_block
      one = Literal.new(ctx.next_id, TypeRef::INT32, 1_i64)
      ctx.emit(one)
      new_i = BinaryOperation.new(ctx.next_id, TypeRef::INT32, BinaryOp::Add, index_phi.id, one.id)
      ctx.emit(new_i)

      index_phi.add_incoming(incr_block, new_i.id)

      # Patch mutable var phis
      assigned_vars.each do |var_name|
        if phi = phi_nodes[var_name]?
          if updated_val = ctx.lookup_local(var_name)
            phi.add_incoming(incr_block, updated_val)
          end
        end
      end

      ctx.terminate(Jump.new(cond_block))

      # Exit block
      ctx.current_block = exit_block
      phi_nodes.each do |var_name, phi|
        ctx.register_local(var_name, phi.id)
      end

      nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
      ctx.emit(nil_lit)
      nil_lit.id
    end

    # Dynamic array each - gets size at runtime via ArraySize
    private def lower_array_each_dynamic(
      ctx : LoweringContext,
      array_id : ValueId,
      block : CrystalV2::Compiler::Frontend::BlockNode
    ) : ValueId
      # Get block param name
      param_name = if params = block.params
                     if first_param = params.first?
                       if pname = first_param.name
                         String.new(pname)
                       else
                         "__arr_elem"
                       end
                     else
                       "__arr_elem"
                     end
                   else
                     "__arr_elem"
                   end

      # Collect mutable vars
      assigned_vars = collect_assigned_vars(block.body)
      assigned_vars = assigned_vars.reject { |v| v == param_name }
      inline_vars = Set(String).new

      entry_block = ctx.current_block
      initial_values = {} of String => ValueId
      assigned_vars.each do |var_name|
        if val = lookup_local_for_phi(ctx, var_name, inline_vars)
          initial_values[var_name] = val
        end
      end

      # Emit zero in entry block BEFORE jump (required for phi SSA)
      zero = Literal.new(ctx.next_id, TypeRef::INT32, 0_i64)
      ctx.emit(zero)

      # Create blocks
      cond_block = ctx.create_block
      body_block = ctx.create_block
      incr_block = ctx.create_block
      exit_block = ctx.create_block

      ctx.terminate(Jump.new(cond_block))

      # Condition block with index phi
      ctx.current_block = cond_block
      index_phi = Phi.new(ctx.next_id, TypeRef::INT32)
      index_phi.add_incoming(entry_block, zero.id)
      ctx.emit(index_phi)

      # Phi for mutable vars
      phi_nodes = {} of String => Phi
      assigned_vars.each do |var_name|
        if initial_val = initial_values[var_name]?
          var_type = ctx.type_of(initial_val)
          phi = Phi.new(ctx.next_id, var_type)
          phi.add_incoming(entry_block, initial_val)
          ctx.emit(phi)
          phi_nodes[var_name] = phi
          ctx.register_local(var_name, phi.id)
          if inline_vars.includes?(var_name)
            @inline_caller_locals_stack[-1][var_name] = phi.id
          end
        end
      end

      # Get array size dynamically
      size_val = ArraySize.new(ctx.next_id, TypeRef::INT32, array_id)
      ctx.emit(size_val)

      # Compare: i < size
      cmp = BinaryOperation.new(ctx.next_id, TypeRef::BOOL, BinaryOp::Lt, index_phi.id, size_val.id)
      ctx.emit(cmp)
      ctx.terminate(Branch.new(cmp.id, body_block, exit_block))

      # Body block
      ctx.current_block = body_block
      ctx.push_scope(ScopeKind::Block)

      # Get element: arr[i]
      element_type = ctx.type_of(array_id)
      if element_type == TypeRef::VOID
        element_type = TypeRef::INT32
      end
      index_get = IndexGet.new(ctx.next_id, element_type, array_id, index_phi.id)
      ctx.emit(index_get)
      ctx.register_type(index_get.id, element_type)

      # Handle tuple destructuring: |(a, b, ...)| vs single param |x|
      if params = block.params
        if params.size > 1
          # Tuple destructuring - extract each element
          params.each_with_index do |param, idx|
            if pname = param.name
              name = String.new(pname)
              idx_lit = Literal.new(ctx.next_id, TypeRef::INT32, idx.to_i64)
              ctx.emit(idx_lit)
              elem_extract = IndexGet.new(ctx.next_id, TypeRef::POINTER, index_get.id, idx_lit.id)
              ctx.emit(elem_extract)
              ctx.register_type(elem_extract.id, TypeRef::POINTER)
              ctx.register_local(name, elem_extract.id)
            end
          end
        else
          # Single parameter - bind whole element
          ctx.register_local(param_name, index_get.id)
        end
      else
        ctx.register_local(param_name, index_get.id)
      end

      pushed_inline = false
      if !inline_vars.empty?
        @inline_loop_vars_stack << inline_vars
        pushed_inline = true
      end
      begin
        lower_body(ctx, block.body)
      ensure
        @inline_loop_vars_stack.pop? if pushed_inline
      end
      ctx.pop_scope
      ctx.terminate(Jump.new(incr_block))

      # Increment block
      ctx.current_block = incr_block
      one = Literal.new(ctx.next_id, TypeRef::INT32, 1_i64)
      ctx.emit(one)
      new_i = BinaryOperation.new(ctx.next_id, TypeRef::INT32, BinaryOp::Add, index_phi.id, one.id)
      ctx.emit(new_i)

      index_phi.add_incoming(incr_block, new_i.id)

      # Patch mutable var phis
      assigned_vars.each do |var_name|
        if phi = phi_nodes[var_name]?
          if updated_val = ctx.lookup_local(var_name)
            phi.add_incoming(incr_block, updated_val)
          end
        end
      end

      ctx.terminate(Jump.new(cond_block))

      # Exit block
      ctx.current_block = exit_block
      phi_nodes.each do |var_name, phi|
        ctx.register_local(var_name, phi.id)
      end

      nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
      ctx.emit(nil_lit)
      nil_lit.id
    end

    # Array each_with_index intrinsic - iterates with element and index
    private def lower_array_each_with_index_dynamic(
      ctx : LoweringContext,
      array_id : ValueId,
      block : CrystalV2::Compiler::Frontend::BlockNode
    ) : ValueId
      # Get block param names (element, index)
      elem_param_name = "__arr_elem"
      index_param_name = "__arr_idx"
      if params = block.params
        if first_param = params[0]?
          if pname = first_param.name
            elem_param_name = String.new(pname)
          end
        end
        if second_param = params[1]?
          if pname = second_param.name
            index_param_name = String.new(pname)
          end
        end
      end

      # Collect mutable vars
      assigned_vars = collect_assigned_vars(block.body)
      assigned_vars = assigned_vars.reject { |v| v == elem_param_name || v == index_param_name }
      inline_vars = Set(String).new

      entry_block = ctx.current_block
      initial_values = {} of String => ValueId
      assigned_vars.each do |var_name|
        if val = lookup_local_for_phi(ctx, var_name, inline_vars)
          initial_values[var_name] = val
        end
      end

      # Emit zero in entry block BEFORE jump (required for phi SSA)
      zero = Literal.new(ctx.next_id, TypeRef::INT32, 0_i64)
      ctx.emit(zero)

      # Create blocks
      cond_block = ctx.create_block
      body_block = ctx.create_block
      incr_block = ctx.create_block
      exit_block = ctx.create_block

      ctx.terminate(Jump.new(cond_block))

      # Condition block with index phi
      ctx.current_block = cond_block
      index_phi = Phi.new(ctx.next_id, TypeRef::INT32)
      index_phi.add_incoming(entry_block, zero.id)
      ctx.emit(index_phi)

      # Phi for mutable vars
      phi_nodes = {} of String => Phi
      assigned_vars.each do |var_name|
        if initial_val = initial_values[var_name]?
          var_type = ctx.type_of(initial_val)
          phi = Phi.new(ctx.next_id, var_type)
          phi.add_incoming(entry_block, initial_val)
          ctx.emit(phi)
          phi_nodes[var_name] = phi
          ctx.register_local(var_name, phi.id)
          if inline_vars.includes?(var_name)
            @inline_caller_locals_stack[-1][var_name] = phi.id
          end
        end
      end

      # Get array size dynamically
      size_val = ArraySize.new(ctx.next_id, TypeRef::INT32, array_id)
      ctx.emit(size_val)

      # Compare: i < size
      cmp = BinaryOperation.new(ctx.next_id, TypeRef::BOOL, BinaryOp::Lt, index_phi.id, size_val.id)
      ctx.emit(cmp)
      ctx.terminate(Branch.new(cmp.id, body_block, exit_block))

      # Body block
      ctx.current_block = body_block
      ctx.push_scope(ScopeKind::Block)

      # Get element: arr[i] - element type should be POINTER for array of strings/objects
      element_type = TypeRef::POINTER  # Arrays typically contain objects
      index_get = IndexGet.new(ctx.next_id, element_type, array_id, index_phi.id)
      ctx.emit(index_get)
      ctx.register_type(index_get.id, element_type)
      ctx.register_local(elem_param_name, index_get.id)

      # Register index parameter with INT32 type
      ctx.register_local(index_param_name, index_phi.id)
      ctx.register_type(index_phi.id, TypeRef::INT32)

      pushed_inline = false
      if !inline_vars.empty?
        @inline_loop_vars_stack << inline_vars
        pushed_inline = true
      end
      begin
        lower_body(ctx, block.body)
      ensure
        @inline_loop_vars_stack.pop? if pushed_inline
      end
      ctx.pop_scope
      ctx.terminate(Jump.new(incr_block))

      # Increment block
      ctx.current_block = incr_block
      one = Literal.new(ctx.next_id, TypeRef::INT32, 1_i64)
      ctx.emit(one)
      new_i = BinaryOperation.new(ctx.next_id, TypeRef::INT32, BinaryOp::Add, index_phi.id, one.id)
      ctx.emit(new_i)

      index_phi.add_incoming(incr_block, new_i.id)

      # Patch mutable var phis
      assigned_vars.each do |var_name|
        if phi = phi_nodes[var_name]?
          if updated_val = ctx.lookup_local(var_name)
            phi.add_incoming(incr_block, updated_val)
          end
        end
      end

      ctx.terminate(Jump.new(cond_block))

      # Exit block
      ctx.current_block = exit_block
      phi_nodes.each do |var_name, phi|
        ctx.register_local(var_name, phi.id)
      end

      nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
      ctx.emit(nil_lit)
      nil_lit.id
    end

    # Array map intrinsic - creates new array with transformed elements (compile-time size)
    # Uses inline expansion for small arrays, creating ArrayLiteral with transformed values
    private def lower_array_map_intrinsic(
      ctx : LoweringContext,
      array_id : ValueId,
      array_size : Int32,
      block : CrystalV2::Compiler::Frontend::BlockNode
    ) : ValueId
      # Get block param name
      param_name = if params = block.params
                     if first_param = params.first?
                       if pname = first_param.name
                         String.new(pname)
                       else
                         "__map_elem"
                       end
                     else
                       "__map_elem"
                     end
                   else
                     "__map_elem"
                   end

      # Get element type from source array
      source_element_type = TypeRef::INT32  # Default to Int32

      # Collect transformed values
      transformed_values = [] of ValueId
      result_element_type = TypeRef::INT32  # Will be updated based on first result

      (0...array_size).each do |i|
        # Get element: arr[i]
        index_lit = Literal.new(ctx.next_id, TypeRef::INT32, i.to_i64)
        ctx.emit(index_lit)
        ctx.register_type(index_lit.id, TypeRef::INT32)

        index_get = IndexGet.new(ctx.next_id, source_element_type, array_id, index_lit.id)
        ctx.emit(index_get)
        ctx.register_type(index_get.id, source_element_type)

        # Bind block parameter
        ctx.push_scope(ScopeKind::Block)
        ctx.register_local(param_name, index_get.id)
        ctx.register_type(index_get.id, source_element_type)

        # Lower block body to get transformed value
        result_value = lower_body(ctx, block.body)
        ctx.pop_scope

        if result_value
          transformed_values << result_value
          # Track element type from first result
          if i == 0
            result_element_type = ctx.type_of(result_value)
          end
        end
      end

      # Create new ArrayLiteral with transformed values
      arr_lit = ArrayLiteral.new(ctx.next_id, result_element_type, transformed_values)
      ctx.emit(arr_lit)
      ctx.register_type(arr_lit.id, TypeRef::POINTER)  # Arrays are pointers
      arr_lit.id
    end

    # Dynamic array map - for arrays with runtime-determined size
    # Currently falls back to returning the source array (not fully implemented)
    private def lower_array_map_dynamic(
      ctx : LoweringContext,
      array_id : ValueId,
      block : CrystalV2::Compiler::Frontend::BlockNode
    ) : ValueId
      # TODO: Implement dynamic map with runtime allocation
      # For now, just process inline similar to static case but use ArraySize
      param_name = if params = block.params
                     if first_param = params.first?
                       if pname = first_param.name
                         String.new(pname)
                       else
                         "__map_elem"
                       end
                     else
                       "__map_elem"
                     end
                   else
                     "__map_elem"
                   end

      element_type = TypeRef::INT32

      # Get size dynamically
      size_val = ArraySize.new(ctx.next_id, TypeRef::INT32, array_id)
      ctx.emit(size_val)

      entry_block = ctx.current_block
      zero = Literal.new(ctx.next_id, TypeRef::INT32, 0_i64)
      ctx.emit(zero)

      cond_block = ctx.create_block
      body_block = ctx.create_block
      incr_block = ctx.create_block
      exit_block = ctx.create_block

      ctx.terminate(Jump.new(cond_block))

      ctx.current_block = cond_block
      index_phi = Phi.new(ctx.next_id, TypeRef::INT32)
      index_phi.add_incoming(entry_block, zero.id)
      ctx.emit(index_phi)

      cmp = BinaryOperation.new(ctx.next_id, TypeRef::BOOL, BinaryOp::Lt, index_phi.id, size_val.id)
      ctx.emit(cmp)
      ctx.terminate(Branch.new(cmp.id, body_block, exit_block))

      ctx.current_block = body_block
      ctx.push_scope(ScopeKind::Block)

      index_get = IndexGet.new(ctx.next_id, element_type, array_id, index_phi.id)
      ctx.emit(index_get)
      ctx.register_type(index_get.id, element_type)
      ctx.register_local(param_name, index_get.id)

      # Lower block body (transformed value stored in-place for now)
      result_value = lower_body(ctx, block.body)
      ctx.pop_scope

      # Store transformed value back to source array (in-place map)
      if result_value
        index_set = IndexSet.new(ctx.next_id, element_type, array_id, index_phi.id, result_value)
        ctx.emit(index_set)
      end

      ctx.terminate(Jump.new(incr_block))

      ctx.current_block = incr_block
      one = Literal.new(ctx.next_id, TypeRef::INT32, 1_i64)
      ctx.emit(one)
      new_i = BinaryOperation.new(ctx.next_id, TypeRef::INT32, BinaryOp::Add, index_phi.id, one.id)
      ctx.emit(new_i)
      index_phi.add_incoming(incr_block, new_i.id)
      ctx.terminate(Jump.new(cond_block))

      ctx.current_block = exit_block
      # Return the modified source array
      array_id
    end

    # Select intrinsic for compile-time sized arrays with AST access
    # For [1, 2, 3].select { |x| x > 1 }, evaluates predicate at compile-time
    private def lower_array_select_intrinsic_with_ast(
      ctx : LoweringContext,
      array_literal : CrystalV2::Compiler::Frontend::ArrayLiteralNode,
      block : CrystalV2::Compiler::Frontend::BlockNode
    ) : ValueId
      # Get block param name
      param_name = if params = block.params
                     if first_param = params.first?
                       if pname = first_param.name
                         String.new(pname)
                       else
                         "__select_elem"
                       end
                     else
                       "__select_elem"
                     end
                   else
                     "__select_elem"
                   end

      source_element_type = TypeRef::INT32  # Default to Int32

      # Try to evaluate predicate at compile-time for each element
      # Collect indices of elements that pass the predicate
      selected_indices = [] of Int32

      array_literal.elements.each_with_index do |elem_expr_id, i|
        elem_node = @arena[elem_expr_id]

        # Try to get compile-time value of element
        elem_value = extract_compile_time_int(elem_node)
        if elem_value
          # Try to evaluate predicate at compile time
          if evaluate_predicate_at_compile_time(param_name, elem_value, block)
            selected_indices << i
          end
        else
          # Can't evaluate at compile time - include element (conservative)
          selected_indices << i
        end
      end

      # Now lower only the selected elements into a new array
      selected_values = [] of ValueId
      selected_indices.each do |i|
        elem_expr_id = array_literal.elements[i]
        elem_val = lower_expr(ctx, elem_expr_id)
        selected_values << elem_val
      end

      # Create result array with only selected elements
      arr_lit = ArrayLiteral.new(ctx.next_id, source_element_type, selected_values)
      ctx.emit(arr_lit)
      ctx.register_type(arr_lit.id, TypeRef::POINTER)
      arr_lit.id
    end

    # Try to extract compile-time integer value from AST node
    private def extract_compile_time_int(node : CrystalV2::Compiler::Frontend::Node) : Int64?
      case node
      when CrystalV2::Compiler::Frontend::NumberNode
        # NumberNode stores value as Slice(UInt8)
        str_val = String.new(node.value)
        str_val.to_i64?
      else
        nil
      end
    end

    # Try to evaluate a simple predicate at compile time
    # Supports: x > n, x < n, x >= n, x <= n, x == n, x != n
    private def evaluate_predicate_at_compile_time(param_name : String, param_value : Int64, block : CrystalV2::Compiler::Frontend::BlockNode) : Bool
      # Get block body - should be a single expression
      return true if block.body.empty?

      body_expr_id = block.body.last
      body_node = @arena[body_expr_id]

      case body_node
      when CrystalV2::Compiler::Frontend::BinaryNode
        # For x > 2, structure is:
        # BinaryNode { operator: ">", left: x, right: 2 }
        op_name = String.new(body_node.operator)

        # Get left operand (should be our parameter)
        left_node = @arena[body_node.left]
        return true unless left_node.is_a?(CrystalV2::Compiler::Frontend::IdentifierNode)

        # Check if left is our parameter
        left_name = String.new(left_node.name)
        return true unless left_name == param_name

        # Get right operand (should be a number)
        right_node = @arena[body_node.right]
        compare_value = extract_compile_time_int(right_node)
        return true unless compare_value

        # Evaluate comparison
        case op_name
        when ">"  then param_value > compare_value
        when "<"  then param_value < compare_value
        when ">=" then param_value >= compare_value
        when "<=" then param_value <= compare_value
        when "==" then param_value == compare_value
        when "!=" then param_value != compare_value
        else
          true  # Unknown op - include element
        end
      when CrystalV2::Compiler::Frontend::CallNode
        # For method-style comparisons: x.>(2)
        callee_node = @arena[body_node.callee]

        case callee_node
        when CrystalV2::Compiler::Frontend::MemberAccessNode
          # Get operator name from member
          op_name = String.new(callee_node.member)

          # Get receiver (object of member access)
          receiver_node = @arena[callee_node.object]
          return true unless receiver_node.is_a?(CrystalV2::Compiler::Frontend::IdentifierNode)

          # Check if receiver is our parameter
          recv_name = String.new(receiver_node.name)
          return true unless recv_name == param_name

          # Get comparison value from args
          args = body_node.args
          return true if args.empty?

          arg_node = @arena[args.first]
          compare_value = extract_compile_time_int(arg_node)
          return true unless compare_value

          # Evaluate comparison
          case op_name
          when ">"  then param_value > compare_value
          when "<"  then param_value < compare_value
          when ">=" then param_value >= compare_value
          when "<=" then param_value <= compare_value
          when "==" then param_value == compare_value
          when "!=" then param_value != compare_value
          else
            true  # Unknown op - include element
          end
        else
          true  # Unknown callee structure
        end
      else
        true  # Can't evaluate - include element conservatively
      end
    end

    # Dynamic array select - for arrays with runtime-determined size
    # TODO: Implement full dynamic select with proper allocation
    # For now, falls back to map-style in-place filtering
    private def lower_array_select_dynamic(
      ctx : LoweringContext,
      array_id : ValueId,
      block : CrystalV2::Compiler::Frontend::BlockNode
    ) : ValueId
      # For dynamic select, we need runtime allocation which is not yet supported
      # Fall back to returning original array (partial implementation)
      # This works for cases where the array is only read, not modified
      array_id
    end

    # Lower Array#reduce { |acc, elem| ... } intrinsic
    # Reduces array to single value by iterating with accumulator
    private def lower_array_reduce_dynamic(
      ctx : LoweringContext,
      array_id : ValueId,
      block : CrystalV2::Compiler::Frontend::BlockNode
    ) : ValueId
      # Extract block parameter names (acc, elem)
      acc_name = "__reduce_acc"
      elem_name = "__reduce_elem"
      if params = block.params
        if params.size >= 1 && (pname = params[0].name)
          acc_name = String.new(pname)
        end
        if params.size >= 2 && (pname = params[1].name)
          elem_name = String.new(pname)
        end
      end

      # Element type - use POINTER since we're typically working with objects
      element_type = TypeRef::POINTER

      # Get array size
      size_val = ArraySize.new(ctx.next_id, TypeRef::INT32, array_id)
      ctx.emit(size_val)

      # Entry block - initialize accumulator with first element
      entry_block = ctx.current_block
      one = Literal.new(ctx.next_id, TypeRef::INT32, 1_i64)
      ctx.emit(one)

      # Get first element as initial accumulator
      zero = Literal.new(ctx.next_id, TypeRef::INT32, 0_i64)
      ctx.emit(zero)
      first_elem = IndexGet.new(ctx.next_id, element_type, array_id, zero.id)
      ctx.emit(first_elem)
      ctx.register_type(first_elem.id, element_type)

      # Create blocks: cond, body, exit
      cond_block = ctx.create_block
      body_block = ctx.create_block
      exit_block = ctx.create_block

      ctx.terminate(Jump.new(cond_block))

      # Condition block - check if index < size
      ctx.current_block = cond_block

      # PHI for index (starts at 1, since we already used element 0)
      index_phi = Phi.new(ctx.next_id, TypeRef::INT32)
      index_phi.add_incoming(entry_block, one.id)
      ctx.emit(index_phi)

      # PHI for accumulator
      acc_phi = Phi.new(ctx.next_id, element_type)
      acc_phi.add_incoming(entry_block, first_elem.id)
      ctx.emit(acc_phi)
      ctx.register_type(acc_phi.id, element_type)

      # Compare index < size
      cmp = BinaryOperation.new(ctx.next_id, TypeRef::BOOL, BinaryOp::Lt, index_phi.id, size_val.id)
      ctx.emit(cmp)
      ctx.terminate(Branch.new(cmp.id, body_block, exit_block))

      # Body block - execute reduce operation
      ctx.current_block = body_block
      ctx.push_scope(ScopeKind::Block)

      # Get current element
      curr_elem = IndexGet.new(ctx.next_id, element_type, array_id, index_phi.id)
      ctx.emit(curr_elem)
      ctx.register_type(curr_elem.id, element_type)

      # Register block parameters
      ctx.register_local(acc_name, acc_phi.id)
      ctx.register_local(elem_name, curr_elem.id)

      # Lower block body - result becomes new accumulator
      new_acc = lower_body(ctx, block.body)
      ctx.pop_scope

      # Increment index
      incr = BinaryOperation.new(ctx.next_id, TypeRef::INT32, BinaryOp::Add, index_phi.id, one.id)
      ctx.emit(incr)
      index_phi.add_incoming(body_block, incr.id)

      # Update accumulator PHI
      if new_acc
        acc_phi.add_incoming(body_block, new_acc)
      else
        acc_phi.add_incoming(body_block, acc_phi.id)
      end

      ctx.terminate(Jump.new(cond_block))

      # Exit block - return final accumulator
      ctx.current_block = exit_block
      ctx.register_type(acc_phi.id, element_type)
      acc_phi.id
    end

    # Handle String.build { |io| ... } intrinsic
    # This creates a StringBuilder, passes it to the block, and returns the final string
    private def lower_string_build_intrinsic(
      ctx : LoweringContext,
      block : CrystalV2::Compiler::Frontend::BlockNode
    ) : ValueId
      # Extract block parameter name (typically "io")
      io_name = "io"
      if params = block.params
        if first_param = params.first?
          if pname = first_param.name
            io_name = String.new(pname)
          end
        end
      end

      # Allocate a string buffer (StringBuilder)
      # For bootstrap, we use a simple approach: allocate a buffer via malloc
      # The buffer pointer serves as both the StringBuilder and the IO-like object
      buffer_size = Literal.new(ctx.next_id, TypeRef::INT64, 4096_i64)
      ctx.emit(buffer_size)
      ctx.register_type(buffer_size.id, TypeRef::INT64)

      # Call malloc to allocate the buffer
      malloc_call = Call.new(ctx.next_id, TypeRef::POINTER, nil, "__crystal_v2_malloc64", [buffer_size.id])
      ctx.emit(malloc_call)
      ctx.register_type(malloc_call.id, TypeRef::POINTER)

      # Initialize buffer with empty string (write null terminator at start)
      # Call __crystal_v2_init_buffer to write '\0' at position 0
      init_call = Call.new(ctx.next_id, TypeRef::VOID, nil, "__crystal_v2_init_buffer", [malloc_call.id])
      ctx.emit(init_call)

      # Create a scope for the block
      ctx.push_scope(ScopeKind::Block)

      # Register the block parameter "io" as the buffer pointer
      ctx.register_local(io_name, malloc_call.id)

      # Lower the block body
      # The block will contain operations like io << "text"
      last_value = lower_body(ctx, block.body)

      ctx.pop_scope

      # Return the buffer as the result string
      # In a real implementation, this would call StringBuilder#to_s
      # For bootstrap, we return the buffer directly (it's already a string pointer)
      malloc_call.id
    end

    # Inline Object#tap with a BlockNode
    # tap yields self to the block then returns self
    # Example: fd.tap { |x| x.flush_on_newline=(true) }
    #   => fd.flush_on_newline=(true); fd
    private def inline_tap_with_block(
      ctx : LoweringContext,
      receiver_id : ValueId,
      block_node : CrystalV2::Compiler::Frontend::BlockNode
    ) : ValueId
      ctx.push_scope(ScopeKind::Block)

      # Bind block params to receiver_id
      if params = block_node.params
        params.each_with_index do |param, i|
          param_name = if param.name
                         String.new(param.name.not_nil!)
                       else
                         "_block_param_#{i}"
                       end
          ctx.register_local(param_name, receiver_id)
          ctx.register_type(receiver_id, ctx.type_of(receiver_id))
        end
      end

      # Execute block body
      if body = block_node.body
        body.each do |expr_id|
          lower_expr(ctx, expr_id)
        end
      end

      ctx.pop_scope
      # tap returns self
      receiver_id
    end

    # Inline Object#tap with a ProcLiteralNode (&.something syntax)
    # tap yields self to the block then returns self
    # Example: fd.tap(&.flush_on_newline=(true))
    #   => fd.flush_on_newline=(true); fd
    private def inline_tap_with_proc(
      ctx : LoweringContext,
      receiver_id : ValueId,
      proc_node : CrystalV2::Compiler::Frontend::ProcLiteralNode
    ) : ValueId
      ctx.push_scope(ScopeKind::Block)

      # The proc body is like: x.flush_on_newline=(true)
      # where x is the implicit block param that should be bound to receiver_id
      if body = proc_node.body
        # For short block syntax &.method, the body is a single call where
        # the first param becomes the receiver. We need to evaluate it with
        # the receiver bound as the implicit block param.
        if params = proc_node.params
          # Bind each block param - for &.method, there's typically one implicit param
          params.each_with_index do |param, i|
            param_name = if param.name
                           String.new(param.name.not_nil!)
                         else
                           "_block_param_#{i}"
                         end
            ctx.register_local(param_name, receiver_id)
            ctx.register_type(receiver_id, ctx.type_of(receiver_id))
          end
        end

        # If no explicit params, the &.method syntax uses the receiver directly
        # The body should be evaluated in current context
        body.each do |expr_id|
          lower_expr(ctx, expr_id)
        end
      end

      ctx.pop_scope
      # tap returns self
      receiver_id
    end

    # Inline a yield-function call with block
    # Transforms: func(args) { |params| block_body }
    # Into: inline func body, replacing yield with block_body
    private def inline_yield_function(
      ctx : LoweringContext,
      func_def : CrystalV2::Compiler::Frontend::DefNode,
      inline_key : String,
      receiver_id : ValueId?,
      call_args : Array(ValueId),
      block : CrystalV2::Compiler::Frontend::BlockNode,
      callee_arena : CrystalV2::Compiler::Frontend::ArenaLike
    ) : ValueId
      ctx.push_scope(ScopeKind::Block)

      caller_arena = @arena

      # Prevent infinite recursion / runaway stack usage in aggressive yield inlining.
      # This can happen in stdlib where yield is used deeply (or recursively).
      max_depth = (ENV["INLINE_YIELD_MAX_DEPTH"]? || "256").to_i
      if @inline_yield_name_stack.size >= max_depth || @inline_yield_name_stack.includes?(inline_key)
        if ENV.has_key?("DEBUG_YIELD_INLINE")
          STDERR.puts "[INLINE_YIELD] skipping inline: #{inline_key} (depth=#{@inline_yield_name_stack.size}, max=#{max_depth})"
        end

        return_type = get_function_return_type(inline_key)
        block_id = lower_block_to_block_id(ctx, block)
        call = Call.new(ctx.next_id, return_type, receiver_id, inline_key, call_args, block_id)
        ctx.emit(call)
        ctx.register_type(call.id, return_type)
        ctx.pop_scope
        return call.id
      end

      old_inline_arenas = @inline_arenas
      @inline_arenas = {caller_arena, callee_arena}
      @arena = callee_arena

      # Isolate callee locals from caller locals, but keep caller locals available for block bodies.
      caller_locals = ctx.save_locals
      @inline_caller_locals_stack << caller_locals
      ctx.restore_locals({} of String => ValueId)

      begin
        pushed_name = false
        pushed_block = false

        @inline_yield_name_stack << inline_key
        pushed_name = true
        @inline_yield_block_stack << block
        @inline_yield_block_arena_stack << caller_arena
        pushed_block = true

        old_current_class = @current_class
        old_current_method = @current_method
        old_current_method_is_class = @current_method_is_class
        @inline_caller_class_stack << old_current_class
        @inline_caller_method_stack << old_current_method
        @inline_caller_method_is_class_stack << old_current_method_is_class
        base_inline_name = inline_key.split("$", 2)[0]
        if base_inline_name.includes?("#")
          owner, method = base_inline_name.split("#", 2)
          unless owner.empty?
            @current_class = owner
            @current_method = method unless method.empty?
            @current_method_is_class = false
          end
        elsif base_inline_name.includes?(".")
          owner, method = base_inline_name.split(".", 2)
          unless owner.empty?
            @current_class = owner
            @current_method = method unless method.empty?
            @current_method_is_class = true
          end
        end

        # If inlining an instance method, bind the receiver as `self`.
        if receiver_id
          ctx.register_local("self", receiver_id)
          ctx.register_type(receiver_id, ctx.type_of(receiver_id))
        end

        # Bind function parameters to call arguments
        if params = func_def.params
          params.each_with_index do |param, idx|
            if pname = param.name
              param_name = String.new(pname)
              if idx < call_args.size
                ctx.register_local(param_name, call_args[idx])
              end
            end
          end
        end

        # Lower function body with yield substitution
        result_value = nil_value(ctx)
        if body = func_def.body
          result_value = lower_body(ctx, body)
        end
        result_value
      ensure
        @current_class = old_current_class
        @current_method = old_current_method
        @current_method_is_class = old_current_method_is_class || false
        @inline_caller_class_stack.pop?
        @inline_caller_method_stack.pop?
        @inline_caller_method_is_class_stack.pop?
        @inline_yield_block_stack.pop? if pushed_block
        @inline_yield_block_arena_stack.pop? if pushed_block
        @inline_yield_name_stack.pop? if pushed_name
        # Restore caller locals (including any mutations made inside the inlined block body).
        if restored = @inline_caller_locals_stack.pop?
          ctx.restore_locals(restored)
        end
        @arena = caller_arena
        @inline_arenas = old_inline_arenas
        ctx.pop_scope
      end
    end

    # Inline block body in place of yield
    private def inline_block_body(
      ctx : LoweringContext,
      yield_node : CrystalV2::Compiler::Frontend::YieldNode,
      block : CrystalV2::Compiler::Frontend::BlockNode
    ) : ValueId
      # Lower yield arguments
      yield_args = if args = yield_node.args
                     args.map { |arg| lower_expr(ctx, arg) }
                   else
                     [] of ValueId
                   end

      # Lower block body
      # For inlined yield-functions, the block body must run in the *caller* lexical scope
      # (caller locals, caller `self`). Otherwise ivar access inside the block can target
      # the callee receiver (e.g. `tap` receiver) and generate invalid IR.
      result = if caller_locals = @inline_caller_locals_stack.last?
        old_inline_class = @current_class
        old_inline_method = @current_method
        old_inline_method_is_class = @current_method_is_class
        @current_class = @inline_caller_class_stack.last?
        @current_method = @inline_caller_method_stack.last?
        @current_method_is_class = @inline_caller_method_is_class_stack.last? || false
        begin
          saved_callee_locals = ctx.save_locals
          ctx.restore_locals(caller_locals)

          caller_locals_before_params = ctx.save_locals
          param_names = [] of String
          if params = block.params
            params.each do |param|
              if pname = param.name
                param_names << String.new(pname)
              end
            end
          end

          # Bind block parameters to yield arguments (in caller scope).
          param_names.each_with_index do |param_name, idx|
            next unless idx < yield_args.size
            ctx.register_local(param_name, yield_args[idx])
            arg_id = yield_args[idx]
            ctx.register_type(arg_id, ctx.type_of(arg_id))
          end

          # Ensure block body is lowered in the caller arena, even when the callee comes from another file.
          body_result = begin
            # The block body belongs to the *caller* and may itself contain `yield`.
            # Temporarily disable the current inlined-yield substitution so nested `yield`
            # in the block body can bind to an outer inlining context (if any).
            popped_block = @inline_yield_block_stack.pop?
            popped_arena = @inline_yield_block_arena_stack.pop?
            if popped_block && popped_block.object_id != block.object_id
              # Unexpected mismatch; restore stacks and continue without popping.
              @inline_yield_block_stack << popped_block
              @inline_yield_block_arena_stack << popped_arena if popped_arena
              popped_block = nil
              popped_arena = nil
            end

            old_arena = @arena
            begin
              @arena = popped_arena || old_arena
              begin
                lower_body(ctx, block.body)
              ensure
                @arena = old_arena
              end
            ensure
              if popped_block
                @inline_yield_block_stack << popped_block
                if restored_arena = popped_arena
                  @inline_yield_block_arena_stack << restored_arena
                else
                  @inline_yield_block_arena_stack << old_arena
                end
              end
            end
          end

          caller_locals_after = ctx.save_locals
          # Block parameters must not leak outside the block.
          param_names.each do |name|
            if prev = caller_locals_before_params[name]?
              caller_locals_after[name] = prev
            else
              caller_locals_after.delete(name)
            end
          end

          # If we're in an inline loop context, keep phi-bound locals stable across iterations.
          if loop_vars = inline_loop_vars_union
            loop_vars.each do |name|
              if prev = caller_locals[name]?
                caller_locals_after[name] = prev
              else
                caller_locals_after.delete(name)
              end
            end
          end
          @inline_caller_locals_stack[-1] = caller_locals_after

          ctx.restore_locals(saved_callee_locals)
          body_result
        ensure
          @current_class = old_inline_class
          @current_method = old_inline_method
          @current_method_is_class = old_inline_method_is_class || false
        end
      else
        # No yield inlining context; just lower block normally.
        if arenas = @inline_arenas
          candidate = arenas[0]
          chosen = candidate
          unless block.body.empty?
            max_index = block.body.max_of(&.index)
            if max_index < 0 || max_index >= candidate.size
              chosen = @arena
            end
          end

          old_arena = @arena
          @arena = chosen
          begin
            lower_body(ctx, block.body)
          ensure
            @arena = old_arena
          end
        else
          lower_body(ctx, block.body)
        end
      end

      # Propagate taints from arguments to result if result is present
      if result
        yield_args.each do |arg_id|
          arg_type = ctx.type_of(arg_id)
          ctx.register_type(result, arg_type) if arg_type && arg_type != TypeRef::VOID
        end
      end

      result
    end

    # Helper to create nil value
    private def nil_value(ctx : LoweringContext) : ValueId
      nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
      ctx.emit(nil_lit)
      nil_lit.id
    end

    private def lower_index(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::IndexNode) : ValueId
      object_id = lower_expr(ctx, node.object)

      # Check if any index is a Range - if so, this is a slice operation
      # Need to check BEFORE lowering the indices so we can handle Range specially
      if node.indexes.size == 1
        idx_node = @arena[node.indexes.first]
        if idx_node.is_a?(CrystalV2::Compiler::Frontend::RangeNode)
          # Array slice: arr[start..end] -> call Array#[] with range, returns Array
          # For bootstrap, emit as a method call to the slice variant
          start_id = lower_expr(ctx, idx_node.begin_expr)
          end_id = lower_expr(ctx, idx_node.end_expr)

          # Emit a call to an intrinsic that creates a slice
          # For now, create a new array and copy elements
          # Qualify method name with receiver's class
          start_type = ctx.type_of(start_id)
          end_type = ctx.type_of(end_id)
          method_name = resolve_method_call(ctx, object_id, "[]", [start_type, end_type])
          call = Call.new(ctx.next_id, TypeRef::POINTER, object_id, method_name, [start_id, end_id])
          ctx.emit(call)
          ctx.register_type(call.id, TypeRef::POINTER)
          return call.id
        end
      end

      # IndexNode has indexes (Array) - can be multi-dimensional like arr[1, 2]
      index_ids = node.indexes.map { |idx| lower_expr(ctx, idx) }

      # Check if this is an array by looking at the object node (ArrayLiteral check)
      # This is necessary because arrays are typed as POINTER but should use IndexGet
      obj_node = @arena[node.object]
      is_array_literal = obj_node.is_a?(CrystalV2::Compiler::Frontend::ArrayLiteralNode)
      # Also check if object is an identifier that was assigned an array
      if !is_array_literal && obj_node.is_a?(CrystalV2::Compiler::Frontend::IdentifierNode)
        # Check if the emitted HIR instruction is an ArrayLiteral
        # (This is a simple check - for now just assume POINTER type without explicit Pointer(T) is an array)
        object_type = ctx.type_of(object_id)
        type_desc = @module.get_type_descriptor(object_type)
        # If it's POINTER but NOT Pointer(T), treat as array
        if object_type == TypeRef::POINTER && !(type_desc && type_desc.name.starts_with?("Pointer"))
          is_array_literal = true
        end
      end

      # Array indexing: use IndexGet for element access
      if is_array_literal && index_ids.size == 1
        element_type = TypeRef::INT32  # Default element type
        index_get = IndexGet.new(ctx.next_id, element_type, object_id, index_ids.first)
        ctx.emit(index_get)
        ctx.register_type(index_get.id, element_type)
        return index_get.id
      end

      # Check if this is pointer indexing (ptr[i])
      object_type = ctx.type_of(object_id)
      type_desc = @module.get_type_descriptor(object_type)
      is_pointer_type = object_type == TypeRef::POINTER ||
                        (type_desc && type_desc.kind == TypeKind::Pointer) ||
                        (type_desc && type_desc.name.starts_with?("Pointer"))
      if is_pointer_type && index_ids.size == 1
        # Pointer indexing: ptr[i] -> PointerLoad with index
        # Extract element type from Pointer(T) if available
        deref_type = if type_desc && type_desc.name.starts_with?("Pointer(")
                       elem_name = type_desc.name[8, type_desc.name.size - 9]
                       type_ref_for_name(elem_name)
                     else
                       TypeRef::INT32
                     end
        load_node = PointerLoad.new(ctx.next_id, deref_type, object_id, index_ids.first)
        ctx.emit(load_node)
        ctx.register_type(load_node.id, deref_type)
        return load_node.id
      end

      # Check if this is an array-like type (which uses IndexGet for element access)
      is_array_type = type_desc && (type_desc.kind == TypeKind::Array ||
                                     type_desc.name.starts_with?("Array") ||
                                     type_desc.name.starts_with?("StaticArray") ||
                                     type_desc.name.starts_with?("Slice"))

      if is_array_type && index_ids.size == 1
        # Array element access: arr[i] -> IndexGet
        # Prefer the array's element type from the interned TypeDescriptor params.
        # Arrays are represented as POINTER at runtime, so we must carry element type explicitly.
        element_type = type_desc.not_nil!.type_params.first? || TypeRef::INT32
        element_type = TypeRef::INT32 if element_type == TypeRef::VOID

        index_get = IndexGet.new(ctx.next_id, element_type, object_id, index_ids.first)
        ctx.emit(index_get)
        ctx.register_type(index_get.id, element_type)
        index_get.id
      else
        # Everything else (classes like Hash, custom types): call [] method
        # Resolve the method name properly (with class name and mangling)
        arg_types = index_ids.map { |idx| ctx.type_of(idx) }
        method_name = resolve_method_call(ctx, object_id, "[]", arg_types)
        return_type = get_function_return_type(method_name)
        # Fallback: [] typically returns a value (element or subslice)
        if return_type == TypeRef::VOID
          return_type = TypeRef::POINTER
        end
        call = Call.new(ctx.next_id, return_type, object_id, method_name, index_ids)
        ctx.emit(call)
        ctx.register_type(call.id, return_type)
        call.id
      end
    end

    private def lower_member_access(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::MemberAccessNode) : ValueId
      obj_node = @arena[node.object]
      member_name = String.new(node.member)

      # Check if this is a class/module static call like Counter.new (without parens)
      # Similar logic to lower_call for MemberAccessNode
      class_name_str : String? = nil

      if obj_node.is_a?(CrystalV2::Compiler::Frontend::ConstantNode)
        name = String.new(obj_node.name)
        # Resolve type alias if exists (check both @type_aliases and LIBC_TYPE_ALIASES)
        resolved_name = @type_aliases[name]? || LIBC_TYPE_ALIASES[name]? || name
        # Chain resolve if needed (e.g., LibCrypto::ULong -> LibC::ULong -> UInt64) - max 10 iterations
        depth = 0
        while (next_resolved = @type_aliases[resolved_name]? || LIBC_TYPE_ALIASES[resolved_name]?) && next_resolved != resolved_name && depth < 10
          resolved_name = next_resolved
          depth += 1
        end
        class_name_str = resolved_name
      elsif obj_node.is_a?(CrystalV2::Compiler::Frontend::IdentifierNode)
        name = String.new(obj_node.name)
        # Resolve type alias if exists (check both @type_aliases and LIBC_TYPE_ALIASES)
        resolved_name = @type_aliases[name]? || LIBC_TYPE_ALIASES[name]? || name
        # Chain resolve if needed - max 10 iterations
        depth = 0
        while (next_resolved = @type_aliases[resolved_name]? || LIBC_TYPE_ALIASES[resolved_name]?) && next_resolved != resolved_name && depth < 10
          resolved_name = next_resolved
          depth += 1
        end
        if resolved_name[0].uppercase?
          resolved_name = resolve_class_name_in_context(resolved_name)
          if @class_info.has_key?(resolved_name)
            class_name_str = resolved_name
          end
        end
      elsif obj_node.is_a?(CrystalV2::Compiler::Frontend::GenericNode)
        # Generic type like Hash(Int32, Int32).new
        base_name = resolve_path_like_name(obj_node.base_type)
        if base_name
          base_name = resolve_type_alias_chain(base_name)
          normalize_typeof_name = ->(type_name : String) : String {
            if type_name == "Void" || type_name == "Unknown" || type_name.includes?("|")
              "Pointer(Void)"
            else
              type_name
            end
          }

          type_args = obj_node.type_args.map do |arg_id|
            arg_name = stringify_type_expr(arg_id) || "Unknown"
            arg_name = resolve_typeof_in_type_string(arg_name)
            arg_name = normalize_typeof_name.call(arg_name)
            @type_param_map[arg_name]? || arg_name
          end
          # Create specialized class name like Hash(Int32, Int32)
          class_name_str = "#{base_name}(#{type_args.join(", ")})"
          # Monomorphize generic class if not already done
          if !@monomorphized.includes?(class_name_str)
            monomorphize_generic_class(base_name, type_args, class_name_str)
          end
        end
      end

      # If it's a static class call (like Counter.new), emit as static call
      if class_name_str
        full_method_name = "#{class_name_str}.#{member_name}"
        if member_name == "new"
          if class_info = @class_info[class_name_str]?
            generate_allocator(class_name_str, class_info)
          end
        end

        if member_name == "null" && full_method_name.starts_with?("Pointer(")
          zero = Literal.new(ctx.next_id, TypeRef::INT64, 0_i64)
          ctx.emit(zero)
          ctx.register_type(zero.id, TypeRef::INT64)
          cast = Cast.new(ctx.next_id, TypeRef::POINTER, zero.id, TypeRef::POINTER)
          ctx.emit(cast)
          ctx.register_type(cast.id, TypeRef::POINTER)
          return cast.id
        end

        args = apply_default_args(ctx, [] of ValueId, member_name, full_method_name, false)
        arg_types = args.map { |arg_id| ctx.type_of(arg_id) }
        mangled_name = mangle_function_name(full_method_name, arg_types)
        actual_name = if @function_types.has_key?(mangled_name) || @module.has_function?(mangled_name)
                        mangled_name
                      elsif has_function_base?(full_method_name)
                        full_method_name
                      else
                        mangled_name
                      end

        return_type = get_function_return_type(actual_name)
        # For .new, use class type_ref as return type
        if member_name == "new" && return_type == TypeRef::VOID
          if class_info = @class_info[class_name_str]?
            return_type = class_info.type_ref
          else
            return_type = TypeRef::POINTER
          end
        end

        lower_function_if_needed(mangled_name)
        if actual_name != mangled_name
          lower_function_if_needed(actual_name)
        end

        args = coerce_args_to_param_types(ctx, args, actual_name)
        call = Call.new(ctx.next_id, return_type, nil, actual_name, args)
        ctx.emit(call)
        ctx.register_type(call.id, return_type)
        return call.id
      end

      # Otherwise it's an instance method call - evaluate object first
      object_id = lower_expr(ctx, node.object)

      # Check for pointer.value -> PointerLoad
      receiver_type = ctx.type_of(object_id)
      ensure_monomorphized_type(receiver_type) unless receiver_type == TypeRef::VOID

      # Handle nil? intrinsic for union types (T | Nil)
      if member_name == "nil?" && is_union_or_nilable_type?(receiver_type)
        return lower_nil_check_intrinsic(ctx, object_id, receiver_type)
      end

      # Handle not_nil! intrinsic for union types
      if member_name == "not_nil!" && is_union_or_nilable_type?(receiver_type)
        return lower_not_nil_intrinsic(ctx, object_id, receiver_type)
      end

      if receiver_type == TypeRef::CHAR && member_name == "ord"
        cast = Cast.new(ctx.next_id, TypeRef::INT32, object_id, TypeRef::INT32)
        ctx.emit(cast)
        ctx.register_type(cast.id, TypeRef::INT32)
        return cast.id
      end

      if member_name == "unsafe_chr"
        case receiver_type
        when TypeRef::INT8, TypeRef::INT16, TypeRef::INT32, TypeRef::INT64, TypeRef::INT128,
             TypeRef::UINT8, TypeRef::UINT16, TypeRef::UINT32, TypeRef::UINT64, TypeRef::UINT128
          cast = Cast.new(ctx.next_id, TypeRef::CHAR, object_id, TypeRef::CHAR)
          ctx.emit(cast)
          ctx.register_type(cast.id, TypeRef::CHAR)
          return cast.id
        end
      end

      # Handle pointer.value -> PointerLoad for typed pointers (Pointer(T))
      type_desc = @module.get_type_descriptor(receiver_type)
      is_pointer_type = receiver_type == TypeRef::POINTER ||
                        (type_desc && type_desc.name.starts_with?("Pointer"))
      if is_pointer_type && member_name == "value"
        # Return the dereferenced type from Pointer(T) -> T
        deref_type = if type_desc && type_desc.name.starts_with?("Pointer(") && type_desc.name.ends_with?(")")
                       element_type_name = type_desc.name[8...-1]
                       type_ref_for_name(element_type_name)
                     else
                       TypeRef::INT32  # Fallback for untyped pointers
                     end
        load_node = PointerLoad.new(ctx.next_id, deref_type, object_id, nil)
        ctx.emit(load_node)
        ctx.register_type(load_node.id, deref_type)
        return load_node.id
      end

      # Check for enum.value -> return the enum value as-is (enums are stored as Int32)
      if receiver_type == TypeRef::INT32 && member_name == "value"
        return object_id
      end

      # Special handling for union types containing primitives (like Int32 | Nil)
      # When calling methods like .to_s on a union, we need to unwrap the value first
      if is_nilable_int32_union?(receiver_type) && member_name == "to_s"
        # Unwrap Int32 from the union (assuming it's not nil - caller should have checked)
        unwrap = UnionUnwrap.new(ctx.next_id, TypeRef::INT32, object_id, 0, false)  # variant 0 = Int32
        ctx.emit(unwrap)
        ctx.register_type(unwrap.id, TypeRef::INT32)
        # Call __crystal_v2_int_to_string on the unwrapped value
        call = Call.new(ctx.next_id, TypeRef::POINTER, nil, "__crystal_v2_int_to_string", [unwrap.id])
        ctx.emit(call)
        ctx.register_type(call.id, TypeRef::POINTER)
        return call.id
      end

      # Special handling for primitive type methods (Int32, Bool, etc.)
      # These are NOT classes so class_info lookup will fail
      # We use Call with nil receiver and __crystal_v2_* method name, which hir_to_mir
      # automatically converts to ExternCall
      if receiver_type == TypeRef::INT32
        case member_name
        when "to_s"
          # Call __crystal_v2_int_to_string intrinsic
          call = Call.new(ctx.next_id, TypeRef::POINTER, nil, "__crystal_v2_int_to_string", [object_id])
          ctx.emit(call)
          ctx.register_type(call.id, TypeRef::POINTER)
          return call.id
        when "abs"
          # For abs, we could emit inline: (x < 0) ? -x : x, or extern call
          # For now, emit as extern
          call = Call.new(ctx.next_id, TypeRef::INT32, nil, "__crystal_v2_int_abs", [object_id])
          ctx.emit(call)
          ctx.register_type(call.id, TypeRef::INT32)
          return call.id
        when "to_i", "to_i32"
          # Int32.to_i is identity
          return object_id
        when "to_i64"
          # Sign-extend to i64
          call = Call.new(ctx.next_id, TypeRef::INT64, nil, "__crystal_v2_int_to_i64", [object_id])
          ctx.emit(call)
          ctx.register_type(call.id, TypeRef::INT64)
          return call.id
        when "to_f", "to_f64"
          call = Call.new(ctx.next_id, TypeRef::FLOAT64, nil, "__crystal_v2_int_to_f64", [object_id])
          ctx.emit(call)
          ctx.register_type(call.id, TypeRef::FLOAT64)
          return call.id
        when "times"
          # Int32#times with block - handle as intrinsic loop
          # Will be handled in lower_call for blocks, skip here
        end
      elsif receiver_type == TypeRef::INT64
        case member_name
        when "to_s"
          call = Call.new(ctx.next_id, TypeRef::POINTER, nil, "__crystal_v2_int64_to_string", [object_id])
          ctx.emit(call)
          ctx.register_type(call.id, TypeRef::POINTER)
          return call.id
        when "to_i", "to_i32"
          # Truncate to i32
          call = Call.new(ctx.next_id, TypeRef::INT32, nil, "__crystal_v2_int64_to_i32", [object_id])
          ctx.emit(call)
          ctx.register_type(call.id, TypeRef::INT32)
          return call.id
        when "to_i64"
          return object_id
        end
      elsif receiver_type == TypeRef::FLOAT64
        case member_name
        when "to_s"
          call = Call.new(ctx.next_id, TypeRef::POINTER, nil, "__crystal_v2_f64_to_string", [object_id])
          ctx.emit(call)
          ctx.register_type(call.id, TypeRef::POINTER)
          return call.id
        when "to_i", "to_i32"
          call = Call.new(ctx.next_id, TypeRef::INT32, nil, "__crystal_v2_f64_to_i32", [object_id])
          ctx.emit(call)
          ctx.register_type(call.id, TypeRef::INT32)
          return call.id
        when "to_i64"
          call = Call.new(ctx.next_id, TypeRef::INT64, nil, "__crystal_v2_f64_to_i64", [object_id])
          ctx.emit(call)
          ctx.register_type(call.id, TypeRef::INT64)
          return call.id
        end
      elsif receiver_type == TypeRef::BOOL
        case member_name
        when "to_s"
          call = Call.new(ctx.next_id, TypeRef::POINTER, nil, "__crystal_v2_bool_to_string", [object_id])
          ctx.emit(call)
          ctx.register_type(call.id, TypeRef::POINTER)
          return call.id
        end
      end

      # Determine receiver type to find the correct method
      resolved_method_name : String? = nil
      return_type = TypeRef::VOID

      # Try to find method by receiver type with inheritance support
      if receiver_type.id > 0
        @class_info.each do |class_name, info|
          if info.type_ref.id == receiver_type.id
            # Use inheritance-aware method resolution
            if base_method = resolve_method_with_inheritance(class_name, member_name)
              resolved_method_name = base_method
              return_type = get_function_return_type(base_method)
            end
            break
          end
        end
      end

      # Fallback 1: Try to match by type descriptor name (when type_ref IDs don't match)
      if resolved_method_name.nil? && receiver_type.id > 0
        if type_desc = @module.get_type_descriptor(receiver_type)
          type_name = type_desc.name
          # Try full name first
          if @class_info.has_key?(type_name)
            if base_method = resolve_method_with_inheritance(type_name, member_name)
              resolved_method_name = base_method
              return_type = get_function_return_type(base_method)
            end
          else
            # Try to find a class that ends with the type name (handle namespacing)
            # e.g., type_name="Span" matches "CrystalV2::Compiler::Frontend::Span"
            @class_info.each do |class_name, info|
              if class_name.ends_with?("::#{type_name}") || class_name == type_name
                if base_method = resolve_method_with_inheritance(class_name, member_name)
                  resolved_method_name = base_method
                  return_type = get_function_return_type(base_method)
                  break
                end
              end
            end
          end
        end
      end

      # Fallback 2: search all classes for this method (only when receiver type is unknown)
      if resolved_method_name.nil? && receiver_type.id == 0
        @class_info.each do |class_name, info|
          test_name = "#{class_name}##{member_name}"
          if has_function_base?(test_name)
            resolved_method_name = test_name
            return_type = get_function_return_type(test_name)
            break
          end
        end
      end

      base_method_name = resolved_method_name
      if base_method_name.nil?
        base_method_name = resolve_method_call(ctx, object_id, member_name, [] of TypeRef)
        if dollar = base_method_name.index('$')
          base_method_name = base_method_name[0, dollar]
        end
      end

      args = apply_default_args(ctx, [] of ValueId, member_name, base_method_name, false)
      arg_types = args.map { |arg_id| ctx.type_of(arg_id) }

      actual_name = if resolved_method_name
                      mangled_name = mangle_function_name(resolved_method_name, arg_types)
                      if @function_types.has_key?(mangled_name) || @module.has_function?(mangled_name)
                        mangled_name
                      elsif has_function_base?(resolved_method_name)
                        resolved_method_name
                      else
                        mangled_name
                      end
                    else
                      resolve_method_call(ctx, object_id, member_name, arg_types)
                    end

      # Special handling for Tuple#size - return compile-time constant based on type parameters
      if member_name == "size"
        if type_desc = @module.get_type_descriptor(receiver_type)
          type_name = type_desc.name
          if type_name.starts_with?("Tuple(") && type_name.ends_with?(")")
            # Count the number of type parameters in Tuple(T1, T2, ...)
            # by counting commas + 1 (handling nested generics)
            inner = type_name[6...-1]  # Strip "Tuple(" and ")"
            tuple_size = 0
            depth = 0
            unless inner.empty?
              tuple_size = 1
              inner.each_char do |c|
                case c
                when '(' then depth += 1
                when ')' then depth -= 1
                when ','
                  tuple_size += 1 if depth == 0
                end
              end
            end
            lit = Literal.new(ctx.next_id, TypeRef::INT32, tuple_size.to_i64)
            ctx.emit(lit)
            ctx.register_type(lit.id, TypeRef::INT32)
            return lit.id
          end
        end
      end

      return_type = get_function_return_type(actual_name)
      if return_type == TypeRef::VOID && actual_name != base_method_name
        base_return = get_function_return_type(base_method_name)
        if base_return != TypeRef::VOID
          return_type = base_return
          actual_name = base_method_name
        end
      end

      # Fallback for stdlib methods that should return a value (like to_a, map, etc.)
      # Same logic as in lower_call for consistency
      if return_type == TypeRef::VOID
        # Methods returning Bool (predicate methods)
        methods_returning_bool = ["empty?", "any?", "all?", "none?", "includes?",
                                  "starts_with?", "ends_with?", "blank?", "present?",
                                  "valid?", "nil?", "is_a?", "responds_to?"]
        # Methods returning Int32
        methods_returning_int32 = ["size", "length", "count", "bytesize", "hash",
                                   "index", "rindex", "ord"]
        if methods_returning_bool.includes?(member_name)
          return_type = TypeRef::BOOL
        elsif methods_returning_int32.includes?(member_name)
          return_type = TypeRef::INT32
        else
          methods_returning_self_or_value = ["to_a", "to_s", "map", "select", "reduce", "each",
                                             "first", "last", "dup", "clone", "cover",
                                             "compact", "flatten", "sort", "reverse", "uniq",
                                             "join", "split", "strip", "chomp", "chars",
                                             "keys", "values", "value",  # 'value' is common getter
                                             "lines", "bytes", "codepoints", "graphemes",
                                             "rstrip", "lstrip", "downcase", "upcase", "capitalize",
                                             "gsub", "sub", "tr", "delete", "squeeze",
                                             "rjust", "ljust", "center", "each_line",
                                             "each_with_index", "map_with_index", "select_with_index",
                                             "find", "find_index",
                                             "sum", "product", "min", "max", "minmax", "sample",
                                             "take", "drop", "take_while", "drop_while",
                                             "group_by", "partition", "zip", "transpose",
                                             "shuffle", "rotate", "pop", "shift", "slice",
                                             "to_slice", "to_unsafe", "to_h", "to_set", "copy_from"]
          if methods_returning_self_or_value.includes?(member_name)
            return_type = TypeRef::POINTER
          end
        end
      end

      if return_type == TypeRef::VOID
        methods_returning_receiver_type = ["tap", "clamp", "abs", "ceil", "floor", "round", "truncate"]
        if methods_returning_receiver_type.includes?(member_name)
          return_type = ctx.type_of(object_id)
        end
      end

      call_virtual = false
      if type_desc = @module.get_type_descriptor(ctx.type_of(object_id))
        call_virtual = type_desc.kind.in?(TypeKind::Union, TypeKind::Module)
      end

      primary_name = if resolved_method_name
                       mangle_function_name(resolved_method_name, arg_types)
                     else
                       actual_name
                     end

      lower_function_if_needed(primary_name)
      if actual_name != primary_name
        lower_function_if_needed(actual_name)
      end

      args = coerce_args_to_param_types(ctx, args, actual_name)
      call = Call.new(ctx.next_id, return_type, object_id, actual_name, args, nil, call_virtual)
      ctx.emit(call)
      ctx.register_type(call.id, return_type)
      call.id
    end

    # ═══════════════════════════════════════════════════════════════════════
    # ASSIGNMENT
    # ═══════════════════════════════════════════════════════════════════════

    private def lower_assign(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::AssignNode) : ValueId
      value_id = lower_expr(ctx, node.value)
      target_node = @arena[node.target]

      case target_node
      when CrystalV2::Compiler::Frontend::IdentifierNode
        name = String.new(target_node.name)
        value_type = ctx.type_of(value_id)
        if existing = ctx.lookup_local(name)
          # Reassignment
          copy = Copy.new(ctx.next_id, value_type, value_id)
          ctx.emit(copy)
          ctx.register_local(name, copy.id)
          update_typeof_local(name, value_type)
          if concrete_name = concrete_type_name_for(value_type)
            existing_name = lookup_typeof_local_name(name)
            if existing_name.nil? || module_like_type_name?(existing_name)
              update_typeof_local_name(name, concrete_name)
            end
          end
          copy.id
        else
          # New variable
          local = Local.new(ctx.next_id, value_type, name, ctx.current_scope)
          ctx.emit(local)
          ctx.register_local(name, value_id)
          # Also emit copy
          copy = Copy.new(ctx.next_id, value_type, value_id)
          ctx.emit(copy)
          ctx.register_local(name, copy.id)
          update_typeof_local(name, value_type)
          if concrete_name = concrete_type_name_for(value_type)
            update_typeof_local_name(name, concrete_name)
          end
          copy.id
        end

      when CrystalV2::Compiler::Frontend::InstanceVarNode
        name = String.new(target_node.name)
        ivar_offset = get_ivar_offset(name)
        ivar_type = get_ivar_type(name)
        self_id = emit_self(ctx)

        # Check if ivar is a union type - need to wrap the value
        if ivar_type && is_union_type?(ivar_type)
          # Get the type of the value being assigned
          value_type = ctx.type_of(value_id)
          variant_id = get_union_variant_id(ivar_type, value_type)

          if variant_id >= 0
            # Wrap value into union with type_id
            union_wrap = UnionWrap.new(ctx.next_id, ivar_type, value_id, variant_id)
            ctx.emit(union_wrap)
            ctx.register_type(union_wrap.id, ivar_type)

            # Store the wrapped union value
            field_set = FieldSet.new(ctx.next_id, TypeRef::VOID, self_id, name, union_wrap.id, ivar_offset)
            ctx.emit(field_set)
            return field_set.id
          end
        end

        # Regular (non-union) field assignment
        field_set = FieldSet.new(ctx.next_id, TypeRef::VOID, self_id, name, value_id, ivar_offset)
        ctx.emit(field_set)
        field_set.id

      when CrystalV2::Compiler::Frontend::ClassVarNode
        # Name includes @@ prefix, strip it
        raw_name = String.new(target_node.name)
        name = raw_name.lstrip('@')
        cvar_type = get_class_var_type(name)
        class_name = @current_class || ""
        class_var_set = ClassVarSet.new(ctx.next_id, cvar_type, class_name, name, value_id)
        ctx.emit(class_var_set)
        class_var_set.id

      when CrystalV2::Compiler::Frontend::IndexNode
        object_id = lower_expr(ctx, target_node.object)
        index_ids = target_node.indexes.map { |idx| lower_expr(ctx, idx) }

        # Check if this is pointer indexing (ptr[i] = val)
        object_type = ctx.type_of(object_id)
        type_desc = @module.get_type_descriptor(object_type)
        is_pointer_type = object_type == TypeRef::POINTER ||
                          (type_desc && type_desc.kind == TypeKind::Pointer) ||
                          (type_desc && type_desc.name.starts_with?("Pointer"))

        if is_pointer_type && index_ids.size == 1
          # Pointer store: ptr[i] = val -> PointerStore with index
          store_type = ctx.type_of(value_id)
          store_node = PointerStore.new(ctx.next_id, store_type, object_id, value_id, index_ids.first)
          ctx.emit(store_node)
          return value_id
        end

        # Check if this is an array-like type (which uses IndexSet for element assignment)
        is_array_type = type_desc && (type_desc.kind == TypeKind::Array ||
                                       type_desc.name.starts_with?("Array") ||
                                       type_desc.name.starts_with?("StaticArray") ||
                                       type_desc.name.starts_with?("Slice"))

        if is_array_type && index_ids.size == 1
          # Array element assignment: arr[i] = val -> IndexSet
          element_type = type_desc.not_nil!.type_params.first? || ctx.type_of(value_id)
          element_type = TypeRef::INT32 if element_type == TypeRef::VOID
          index_set = IndexSet.new(ctx.next_id, element_type, object_id, index_ids.first, value_id)
          ctx.emit(index_set)
          value_id
        else
          # Everything else (classes like Hash, custom types): call []= method
          # Resolve the method name properly (with class name and mangling)
          all_args = index_ids + [value_id]
          arg_types = all_args.map { |arg| ctx.type_of(arg) }
          method_name = resolve_method_call(ctx, object_id, "[]=", arg_types)
          return_type = get_function_return_type(method_name)
          call = Call.new(ctx.next_id, return_type, object_id, method_name, all_args)
          ctx.emit(call)
          ctx.register_type(call.id, return_type)
          call.id
        end

      when CrystalV2::Compiler::Frontend::MemberAccessNode
        # obj.field = value -> call setter method or direct field set
        object_id = lower_expr(ctx, target_node.object)
        field_name = String.new(target_node.member)

        # Get the object's type to resolve the setter method
        object_type = ctx.type_of(object_id)
        type_desc = @module.get_type_descriptor(object_type)

        # Try direct field access if we know the class layout
        class_name = type_desc ? type_desc.name : nil
        if class_name && @class_info.has_key?(class_name)
          class_info = @class_info[class_name]

          # Check if this is a known field (ivar)
          ivar_name = "@#{field_name}"
          if ivar_info = class_info.ivars.find { |iv| iv.name == ivar_name }
            # Direct field set
            field_set = FieldSet.new(ctx.next_id, TypeRef::VOID, object_id, ivar_name, value_id, ivar_info.offset)
            ctx.emit(field_set)
            return field_set.id
          end
        end

        # Fallback to setter method call: obj.field=(value)
        setter_name = "#{field_name}="
        arg_types = [ctx.type_of(value_id)]
        method_name = resolve_method_call(ctx, object_id, setter_name, arg_types)
        return_type = get_function_return_type(method_name)
        call = Call.new(ctx.next_id, return_type, object_id, method_name, [value_id])
        ctx.emit(call)
        ctx.register_type(call.id, return_type)
        call.id

      when CrystalV2::Compiler::Frontend::GlobalNode
        # Global variable assignment: $name = value
        name = String.new(target_node.name)
        value_type = ctx.type_of(value_id)
        class_var_set = ClassVarSet.new(ctx.next_id, value_type, "$", name, value_id)
        ctx.emit(class_var_set)
        class_var_set.id

      else
        raise LoweringError.new("Unsupported assignment target: #{target_node.class}", target_node)
      end
    end

    private def lower_multiple_assign(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::MultipleAssignNode) : ValueId
      # MultipleAssignNode has a single value (destructured)
      # e.g., a, b, c = expr  where expr is a tuple/array
      rhs_id = lower_expr(ctx, node.value)

      # For each target, emit index operation to destructure
      node.targets.each_with_index do |target_expr, idx|
        target_node = @arena[target_expr]

        # Index into the RHS to get this element
        index_lit = Literal.new(ctx.next_id, TypeRef::INT32, idx.to_i64)
        ctx.emit(index_lit)
        element_id = IndexGet.new(ctx.next_id, TypeRef::VOID, rhs_id, index_lit.id)
        ctx.emit(element_id)

        case target_node
        when CrystalV2::Compiler::Frontend::IdentifierNode
          name = String.new(target_node.name)
          local = Local.new(ctx.next_id, TypeRef::VOID, name, ctx.current_scope)
          ctx.emit(local)
          ctx.register_local(name, element_id.id)

        when CrystalV2::Compiler::Frontend::InstanceVarNode
          name = String.new(target_node.name)
          ivar_offset = get_ivar_offset(name)
          self_id = emit_self(ctx)
          field_set = FieldSet.new(ctx.next_id, TypeRef::VOID, self_id, name, element_id.id, ivar_offset)
          ctx.emit(field_set)

        else
          # Other target types can be added as needed
        end
      end

      rhs_id
    end

    # ═══════════════════════════════════════════════════════════════════════
    # CLOSURES
    # ═══════════════════════════════════════════════════════════════════════

    private def compute_block_captures(
      ctx : LoweringContext,
      closure_scope : ScopeId,
      saved_locals : Hash(String, ValueId),
      assigned_vars : Set(String)
    ) : Array(CapturedVar)
      return [] of CapturedVar if saved_locals.empty?

      value_to_name = {} of ValueId => String
      saved_locals.each { |name, id| value_to_name[id] = name }

      captured_names = Set(String).new
      ctx.function.blocks.each do |block|
        next unless scope_within?(ctx.function, block.scope, closure_scope)
        block.instructions.each do |inst|
          each_operand_for_capture(inst) do |operand|
            if name = value_to_name[operand]?
              captured_names << name
            end
          end
        end
      end

      captures = [] of CapturedVar
      captured_names.each do |name|
        value_id = saved_locals[name]
        by_ref = assigned_vars.includes?(name)
        captures << CapturedVar.new(value_id, name, by_ref)
      end
      captures
    end

    private def scope_within?(function : Function, scope_id : ScopeId, root_scope : ScopeId) : Bool
      current = scope_id
      loop do
        return true if current == root_scope
        scope = function.get_scope(current)
        parent = scope.parent
        return false unless parent
        current = parent
      end
    end

    private def each_operand_for_capture(value : Value, &block : ValueId ->)
      case value
      when Copy
        yield value.source
      when BinaryOperation
        yield value.left
        yield value.right
      when UnaryOperation
        yield value.operand
      when Call
        if recv = value.receiver
          yield recv
        end
        value.args.each { |arg| yield arg }
      when FieldGet
        yield value.object
      when FieldSet
        yield value.object
        yield value.value
      when IndexGet
        yield value.object
        yield value.index
      when IndexSet
        yield value.object
        yield value.index
        yield value.value
      when MakeClosure
        value.captures.each { |cap| yield cap.value_id }
      when Yield
        value.args.each { |arg| yield arg }
      when Phi
        value.incoming.each { |(_, val)| yield val }
      when Cast
        yield value.value
      when IsA
        yield value.value
      when Allocate
        value.constructor_args.each { |arg| yield arg }
      end
    end

    private def lower_block(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::BlockNode) : ValueId
      block_id = lower_block_to_block_id(ctx, node)

      # Create MakeClosure
      captures = @block_captures.delete(block_id) || [] of CapturedVar

      closure = MakeClosure.new(ctx.next_id, TypeRef::VOID, block_id, captures)
      ctx.emit(closure)
      closure.id
    end

    private def lower_block_to_block_id(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::BlockNode) : BlockId
      saved_block = ctx.current_block
      # Save locals before lowering block body - block-local vars shouldn't leak
      saved_locals = ctx.save_locals
      assigned_vars = collect_assigned_vars(node.body).to_set
      if params = node.params
        params.each do |param|
          next unless param_name = param.name
          assigned_vars.delete(String.new(param_name))
        end
      end
      old_typeof_locals = @current_typeof_locals
      old_typeof_local_names = @current_typeof_local_names
      @current_typeof_locals = old_typeof_locals ? old_typeof_locals.dup : old_typeof_locals
      @current_typeof_local_names = old_typeof_local_names ? old_typeof_local_names.dup : old_typeof_local_names

      ctx.push_scope(ScopeKind::Closure)
      closure_scope = ctx.current_scope
      body_block = ctx.create_block
      ctx.current_block = body_block

      # Add block parameters (params can be nil)
      # Default to POINTER type since block parameters are typically objects (IO, etc.)
      if params = node.params
        params.each_with_index do |param, idx|
          if param_name = param.name
            name = String.new(param_name)
            param_type = if ta = param.type_annotation
                           type_ref_for_name(String.new(ta))
                         else
                           TypeRef::POINTER  # Default to pointer for block params
                         end
            param_val = Parameter.new(ctx.next_id, param_type, idx, name)
            ctx.emit(param_val)
            ctx.register_local(name, param_val.id)
            ctx.register_type(param_val.id, param_type)
            update_typeof_local(name, param_type)
            if ta = param.type_annotation
              update_typeof_local_name(name, String.new(ta))
            end
          end
        end
      end

      # Lower body
      last_value = lower_body(ctx, node.body)
      ctx.pop_scope

      @block_captures[body_block] = compute_block_captures(ctx, closure_scope, saved_locals, assigned_vars)

      # Implicit return from block
      if ctx.get_block(ctx.current_block).terminator.is_a?(Unreachable)
        ctx.terminate(Return.new(last_value))
      end

      ctx.current_block = saved_block
      # Restore locals - block-local vars shouldn't pollute outer scope
      ctx.restore_locals(saved_locals)
      @current_typeof_locals = old_typeof_locals
      @current_typeof_local_names = old_typeof_local_names
      body_block
    end

    private def lower_proc_literal(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::ProcLiteralNode) : ValueId
      saved_block = ctx.current_block
      saved_locals = ctx.save_locals
      assigned_vars = collect_assigned_vars(node.body).to_set
      old_typeof_locals = @current_typeof_locals
      old_typeof_local_names = @current_typeof_local_names
      @current_typeof_locals = old_typeof_locals ? old_typeof_locals.dup : old_typeof_locals
      @current_typeof_local_names = old_typeof_local_names ? old_typeof_local_names.dup : old_typeof_local_names

      ctx.push_scope(ScopeKind::Closure)
      closure_scope = ctx.current_scope
      body_block = ctx.create_block
      ctx.current_block = body_block

      # Add proc parameters (params can be nil)
      if params = node.params
        params.each_with_index do |param, idx|
          if param_name = param.name
            name = String.new(param_name)
            param_type = if ta = param.type_annotation
                           type_ref_for_name(String.new(ta))
                         else
                           TypeRef::VOID
                         end
            param_val = Parameter.new(ctx.next_id, param_type, idx, name)
            ctx.emit(param_val)
            ctx.register_local(name, param_val.id)
            ctx.register_type(param_val.id, param_type)
            update_typeof_local(name, param_type)
            if ta = param.type_annotation
              update_typeof_local_name(name, String.new(ta))
            end
            assigned_vars.delete(name)
          end
        end
      end

      # Lower body
      last_value = lower_body(ctx, node.body)
      ctx.pop_scope

      if ctx.get_block(ctx.current_block).terminator.is_a?(Unreachable)
        ctx.terminate(Return.new(last_value))
      end

      ctx.current_block = saved_block
      @current_typeof_locals = old_typeof_locals
      @current_typeof_local_names = old_typeof_local_names

      # Create MakeClosure
      captures = compute_block_captures(ctx, closure_scope, saved_locals, assigned_vars)
      closure = MakeClosure.new(ctx.next_id, TypeRef::VOID, body_block, captures)
      closure.lifetime = LifetimeTag::HeapEscape  # Procs typically escape
      ctx.emit(closure)
      closure.id
    end

    # ═══════════════════════════════════════════════════════════════════════
    # COLLECTIONS
    # ═══════════════════════════════════════════════════════════════════════

    private def lower_array_literal(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::ArrayLiteralNode) : ValueId
      element_ids = node.elements.map { |e| lower_expr(ctx, e) }

      # Determine element type from explicit `of` type, or from first element (or Int32 default)
      element_type = if of_type = node.of_type
                       if type_str = stringify_type_expr(of_type)
                         type_ref_for_name(type_str)
                       else
                         element_ids.size > 0 ? ctx.type_of(element_ids.first) : TypeRef::INT32
                       end
                     elsif element_ids.size > 0
                       ctx.type_of(element_ids.first)
                     else
                       TypeRef::INT32
                     end

      # Create ArrayLiteral instruction (not generic Allocate)
      arr = ArrayLiteral.new(ctx.next_id, element_type, element_ids)
      arr.lifetime = LifetimeTag::StackLocal  # Default to stack until escape analysis
      ctx.emit(arr)
      # Register as POINTER type - arrays are pointer-like for indexing purposes
      ctx.register_type(arr.id, TypeRef::POINTER)
      arr.id
    end

    private def lower_hash_literal(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::HashLiteralNode) : ValueId
      # Lower key-value pairs
      args = [] of ValueId
      node.entries.each do |entry|
        key_id = lower_expr(ctx, entry.key)
        value_id = lower_expr(ctx, entry.value)
        args << key_id
        args << value_id
      end

      hash_type = ctx.get_type("Hash")
      alloc = Allocate.new(ctx.next_id, hash_type, args)
      ctx.emit(alloc)
      alloc.id
    end

    private def lower_tuple_literal(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::TupleLiteralNode) : ValueId
      element_ids = node.elements.map { |e| lower_expr(ctx, e) }

      tuple_type = ctx.get_type("Tuple")
      alloc = Allocate.new(ctx.next_id, tuple_type, element_ids)
      ctx.emit(alloc)
      alloc.id
    end

    private def lower_named_tuple_literal(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::NamedTupleLiteralNode) : ValueId
      # NamedTuple is like a Tuple but with named fields
      # For now, we treat it similarly to a Tuple - lowering values in order
      # The key names are available for type-level operations
      element_ids = node.entries.map { |entry| lower_expr(ctx, entry.value) }

      # Create a NamedTuple type based on keys
      # For simplicity, use NamedTuple as the type (real Crystal has specialized types)
      named_tuple_type = ctx.get_type("NamedTuple")
      alloc = Allocate.new(ctx.next_id, named_tuple_type, element_ids)
      ctx.emit(alloc)
      alloc.id
    end

    private def lower_range(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::RangeNode) : ValueId
      begin_id = lower_expr(ctx, node.begin_expr)
      end_id = lower_expr(ctx, node.end_expr)

      # Include exclusive flag as third argument
      excl_lit = Literal.new(ctx.next_id, TypeRef::BOOL, node.exclusive)
      ctx.emit(excl_lit)

      range_type = ctx.get_type("Range")
      alloc = Allocate.new(ctx.next_id, range_type, [begin_id, end_id, excl_lit.id])
      ctx.emit(alloc)
      alloc.id
    end

    # ═══════════════════════════════════════════════════════════════════════
    # TYPE OPERATIONS
    # ═══════════════════════════════════════════════════════════════════════

    private def lower_as(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::AsNode) : ValueId
      value_id = lower_expr(ctx, node.expression)
      # target_type is Slice(UInt8) - type name as bytes
      target_type = type_ref_for_name(String.new(node.target_type))

      # Check if value is union type - use UnionUnwrap instead of Cast
      value_type = ctx.type_of(value_id)
      if is_union_type?(value_type)
        variant_id = get_union_variant_id(value_type, target_type)
        if variant_id >= 0
          # Unsafe unwrap - assumes type_id matches (caller should have checked with is_a?)
          unwrap = UnionUnwrap.new(ctx.next_id, target_type, value_id, variant_id, safe: false)
          ctx.emit(unwrap)
          return unwrap.id
        end
      end

      # Regular cast for non-union types
      cast = Cast.new(ctx.next_id, target_type, value_id, target_type, safe: false)
      ctx.emit(cast)
      cast.id
    end

    private def lower_as_question(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::AsQuestionNode) : ValueId
      value_id = lower_expr(ctx, node.expression)
      # target_type is Slice(UInt8) - type name as bytes
      target_type = type_ref_for_name(String.new(node.target_type))

      cast = Cast.new(ctx.next_id, target_type, value_id, target_type, safe: true)
      ctx.emit(cast)
      cast.id
    end

    private def lower_is_a(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::IsANode) : ValueId
      value_id = lower_expr(ctx, node.expression)
      # target_type is Slice(UInt8) - type name as bytes
      check_type = type_ref_for_name(String.new(node.target_type))

      # Check if value is a union type - use UnionIs for runtime type check
      value_type = ctx.type_of(value_id)
      if is_union_type?(value_type)
        # Get the variant_type_id for the check_type within this union
        variant_id = get_union_variant_id(value_type, check_type)
        if variant_id >= 0
          union_is = UnionIs.new(ctx.next_id, value_id, variant_id)
          ctx.emit(union_is)
          return union_is.id
        end
      end

      # Regular is_a check for non-union types
      is_a = IsA.new(ctx.next_id, value_id, check_type)
      ctx.emit(is_a)
      is_a.id
    end

    private def lower_responds_to(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::RespondsToNode) : ValueId
      value_id = lower_expr(ctx, node.expression)
      value_type = ctx.type_of(value_id)

      # Get method name from the symbol literal
      method_name_node = @arena[node.method_name]
      method_name = case method_name_node
                    when CrystalV2::Compiler::Frontend::SymbolNode
                      String.new(method_name_node.name)
                    else
                      ""
                    end

      # Try to determine at compile time if the type responds to this method
      type_desc = @module.get_type_descriptor(value_type)
      class_name = type_desc ? type_desc.name : nil

      result = false
      if class_name && !method_name.empty?
        # Check if the class has this method
        # O(1) lookup: check exact match or mangled version exists
        full_method = "#{class_name}.#{method_name}"
        result = @function_types.has_key?(full_method) || has_function_base?(full_method)
      end

      # Return compile-time constant boolean
      bool_lit = Literal.new(ctx.next_id, TypeRef::BOOL, result)
      ctx.emit(bool_lit)
      bool_lit.id
    end

    # ═══════════════════════════════════════════════════════════════════════
    # HELPERS
    # ═══════════════════════════════════════════════════════════════════════

    private def lower_body(ctx : LoweringContext, body : Array(ExprId)) : ValueId
      last_value : ValueId? = nil
      body.each do |expr_id|
        last_value = lower_expr(ctx, expr_id)
      end

      last_value || begin
        nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
        ctx.emit(nil_lit)
        nil_lit.id
      end
    end

    private def get_type_name(expr_id : ExprId) : String
      node = @arena[expr_id]
      case node
      when CrystalV2::Compiler::Frontend::PathNode
        String.new(node.names.first)
      when CrystalV2::Compiler::Frontend::GenericNode
        String.new(node.name)
      when CrystalV2::Compiler::Frontend::IdentifierNode
        String.new(node.name)
      else
        "Unknown"
      end
    end

    private def type_ref_for_name(name : String) : TypeRef
      # Normalize union type names: "Int32|Nil" -> "Int32 | Nil"
      # This ensures consistent caching regardless of spacing
      normalized_name = if name.includes?("|")
        name.split("|").map(&.strip).join(" | ")
      else
        name
      end

      lookup_name = normalized_name
      if lookup_name.includes?("typeof(")
        lookup_name = resolve_typeof_in_type_string(lookup_name)
      end
      # Resolve type parameters before cache and namespace resolution.
      if substitution = @type_param_map[lookup_name]?
        result = type_ref_for_name(substitution)
        @type_cache[lookup_name] = result
        return result
      end
      # Resolve unqualified type names in the current namespace before cache lookup.
      # This avoids poisoning the cache with short names that can resolve differently per scope.
      if !lookup_name.includes?("|") && !lookup_name.includes?("::") && lookup_name.size > 0 && lookup_name[0].uppercase?
        lookup_name = resolve_class_name_in_context(lookup_name)
      end

      if cached = @type_cache[lookup_name]?
        return cached
      end

      if proc_ref = proc_type_ref_for_name(lookup_name)
        @type_cache[lookup_name] = proc_ref
        return proc_ref
      end

      # Normalize tuple literal types: "{A, B}" -> "Tuple(A, B)"
      # Do this before union handling so inner unions are preserved.
      if lookup_name.starts_with?("{") && lookup_name.ends_with?("}")
        inner = lookup_name[1, lookup_name.size - 2]
        tuple_args = split_generic_type_args(inner)
        tuple_name = "Tuple(#{tuple_args.join(", ")})"
        result = type_ref_for_name(tuple_name)
        @type_cache[lookup_name] = result
        return result
      end

      # Check for union type syntax: "Type1 | Type2" or "Type1|Type2" (parser may not add spaces)
      # NOTE: Don't set placeholder here - create_union_type handles its own caching
      if lookup_name.includes?("|")
        result = create_union_type(lookup_name)
        @type_cache[lookup_name] = result
        return result
      end

      # Mark as being processed with placeholder to break cycles (BEFORE any recursion)
      # Only for non-union types - union types are handled by create_union_type
      @type_cache[lookup_name] = TypeRef::VOID

      # Handle nullable type suffix: "T?" means "T | Nil"
      if lookup_name.ends_with?("?")
        base_name = lookup_name[0, lookup_name.size - 1]
        # Substitute type parameter if present
        base_name = @type_param_map[base_name]? || base_name
        union_name = "#{base_name} | Nil"
        result = create_union_type(union_name)
        @type_cache[lookup_name] = result
        return result
      end

      # Check if this is a type alias (but not self-referencing)
      if alias_target = @type_aliases[lookup_name]?
        if alias_target != lookup_name
          result = type_ref_for_name(alias_target)
          @type_cache[lookup_name] = result
          return result
        end
      end

      # Check LibC type aliases (platform-specific fallback)
      if libc_target = LIBC_TYPE_ALIASES[lookup_name]?
        result = type_ref_for_name(libc_target)
        @type_cache[lookup_name] = result
        return result
      end

      # Handle generic types like Pointer(K), Array(T) - substitute type parameters
      if lookup_name.includes?("(") && lookup_name.includes?(")")
        # Parse generic: "Pointer(K)" -> base="Pointer", params=["K"]
        paren_start = lookup_name.index('(').not_nil!
        base_name = lookup_name[0, paren_start]
        params_str = lookup_name[paren_start + 1, lookup_name.size - paren_start - 2]

        # Substitute each type parameter
        substituted_params = split_generic_type_args(params_str).map do |param|
          param = param.strip
          @type_param_map[param]? || param
        end

        # Reconstruct with substituted params
        substituted_name = "#{base_name}(#{substituted_params.join(", ")})"
        if substituted_name != lookup_name
          # Types changed - recurse with new name
          return type_ref_for_name(substituted_name)
        end

        # Intern a parameterized type descriptor so downstream passes can recover element/key/value types.
        # This is critical for arrays/hashes/pointers where runtime representation is "ptr", but codegen
        # still needs the generic parameters for correct loads/stores.
        type_params = substituted_params.map { |p| type_ref_for_name(p) }
        type_kind = case base_name
                    when "Array", "StaticArray" then TypeKind::Array
                    when "Hash"                then TypeKind::Hash
                    when "Pointer"             then TypeKind::Pointer
                    else                            TypeKind::Generic
                    end
        result = @module.intern_type(TypeDescriptor.new(type_kind, substituted_name, type_params))
        @type_cache[lookup_name] = result

        # Trigger monomorphization if this is a generic class/struct template
        # This ensures included module methods get registered for the specialized type
        if template = @generic_templates[base_name]?
          unless @monomorphized.includes?(substituted_name)
            if @defer_monomorphization
              # Queue for later - templates may not be fully registered yet
              @pending_monomorphizations << {base_name, substituted_params, substituted_name}
            else
              monomorphize_generic_class(base_name, substituted_params, substituted_name)
            end
          end
        end

        return result
      end

      # Handle pointer types (Void*, Pointer(T), T*)
      # IMPORTANT: This must be checked BEFORE the case statement for "Void"
      if lookup_name.ends_with?("*")
        # T* syntax -> pointer type
        @type_cache[lookup_name] = TypeRef::POINTER
        return TypeRef::POINTER
      end

      if lookup_name == "Pointer"
        # Pointer(T) or just Pointer -> pointer type
        @type_cache[lookup_name] = TypeRef::POINTER
        return TypeRef::POINTER
      end

      result = case lookup_name
               when "Void"    then TypeRef::VOID
               when "Nil"     then TypeRef::NIL
               when "Bool"    then TypeRef::BOOL
               when "Int8"    then TypeRef::INT8
               when "Int16"   then TypeRef::INT16
               when "Int32"   then TypeRef::INT32
               when "Int64"   then TypeRef::INT64
               when "Int128"  then TypeRef::INT128
               when "UInt8"   then TypeRef::UINT8
               when "UInt16"  then TypeRef::UINT16
               when "UInt32"  then TypeRef::UINT32
               when "UInt64"  then TypeRef::UINT64
               when "UInt128" then TypeRef::UINT128
               when "Float32" then TypeRef::FLOAT32
               when "Float64" then TypeRef::FLOAT64
               when "Char"    then TypeRef::CHAR
               when "String"  then TypeRef::STRING
               when "Symbol"  then TypeRef::SYMBOL
               # Abstract/stdlib types that should be treated as pointers
               when "IO"         then TypeRef::POINTER
               when "Object"     then TypeRef::POINTER
               when "Reference"  then TypeRef::POINTER
               when "Exception"  then TypeRef::POINTER
               when "Enumerable" then TypeRef::POINTER
               when "Indexable"  then TypeRef::POINTER
               when "Comparable" then TypeRef::POINTER
               when "Iterable"   then TypeRef::POINTER
               else
                 # Check if this is an enum type - enums are stored as Int32
                 if enum_info = @enum_info
                   if enum_info.has_key?(lookup_name)
                     @type_cache[lookup_name] = TypeRef::INT32
                     return TypeRef::INT32
                   end
                 end

                 # Prefer already-registered concrete types to preserve kind (struct vs class).
                 if info = @class_info[lookup_name]?
                   @type_cache[lookup_name] = info.type_ref
                   return info.type_ref
                 end

                 @module.intern_type(TypeDescriptor.new(TypeKind::Class, lookup_name))
               end
      # Cache the result to avoid VOID placeholder being returned on subsequent calls
      @type_cache[lookup_name] = result
      result
    end

    # Create a union type from "Type1 | Type2 | Type3" syntax
    private def create_union_type(name : String) : TypeRef
      # Check cache first to prevent infinite recursion
      if cached = @type_cache[name]?
        return cached
      end
      # Mark as being processed with placeholder to break cycles
      @type_cache[name] = TypeRef::VOID

      # Parse variant type names (handle both "Type1 | Type2" and "Type1|Type2")
      variant_names = name.split("|").map(&.strip)

      # Get TypeRefs for each variant (recursive to handle nested unions)
      variant_refs = variant_names.map { |vn| type_ref_for_name(vn) }

      # Calculate union layout
      variants = [] of MIR::UnionVariantDescriptor
      max_size = 0
      max_align = 4  # Minimum alignment for type_id

      variant_refs.each_with_index do |vref, idx|
        vsize = type_size(vref)
        valign = type_alignment(vref)
        max_size = {max_size, vsize}.max
        max_align = {max_align, valign}.max

        # Convert HIR::TypeRef to MIR::TypeRef using proper ID mapping
        mir_type_ref = hir_to_mir_type_ref(vref)

        variants << MIR::UnionVariantDescriptor.new(
          type_id: idx,
          type_ref: mir_type_ref,
          full_name: variant_names[idx],
          size: vsize,
          alignment: valign,
          field_offsets: nil
        )
      end

      # Total size: header (4 bytes for type_id) + padding + max payload
      payload_offset = ((4 + max_align - 1) // max_align) * max_align
      total_size = payload_offset + max_size

      # Create union type and register descriptor
      type_ref = @module.intern_type(TypeDescriptor.new(TypeKind::Union, name))

      # Convert to MIR::TypeRef for the descriptor key
      mir_union_type_ref = hir_to_mir_type_ref(type_ref)

      # Create union descriptor
      descriptor = MIR::UnionDescriptor.new(
        name: name,
        variants: variants,
        total_size: total_size,
        alignment: max_align
      )

      # Store descriptor for LLVM backend
      @union_descriptors[mir_union_type_ref] = descriptor

      # Update cache with real value (replacing placeholder)
      @type_cache[name] = type_ref

      type_ref
    end

    # Create a nullable union type (T | Nil) from a concrete type
    private def create_union_type_for_nullable(concrete_type : TypeRef) : TypeRef
      # Get the name of the concrete type
      type_name = get_type_name_from_ref(concrete_type)
      union_name = "#{type_name} | Nil"

      # Create the union type via the string-based method
      create_union_type(union_name)
    end

    # Get the type name from a TypeRef
    private def get_type_name_from_ref(type_ref : TypeRef) : String
      case type_ref
      when TypeRef::VOID    then "Void"
      when TypeRef::BOOL    then "Bool"
      when TypeRef::INT8    then "Int8"
      when TypeRef::INT16   then "Int16"
      when TypeRef::INT32   then "Int32"
      when TypeRef::INT64   then "Int64"
      when TypeRef::INT128  then "Int128"
      when TypeRef::UINT8   then "UInt8"
      when TypeRef::UINT16  then "UInt16"
      when TypeRef::UINT32  then "UInt32"
      when TypeRef::UINT64  then "UInt64"
      when TypeRef::UINT128 then "UInt128"
      when TypeRef::FLOAT32 then "Float32"
      when TypeRef::FLOAT64 then "Float64"
      when TypeRef::CHAR    then "Char"
      when TypeRef::STRING  then "String"
      when TypeRef::NIL     then "Nil"
      when TypeRef::SYMBOL  then "Symbol"
      when TypeRef::POINTER then "Pointer"
      else
        # Look up in type registry
        if desc = @module.get_type_descriptor(type_ref)
          desc.name
        else
          "Unknown"
        end
      end
    end

    # Convert HIR::TypeRef to MIR::TypeRef with proper ID mapping
    # HIR and MIR have different primitive type IDs
    private def hir_to_mir_type_ref(hir_type : TypeRef) : MIR::TypeRef
      case hir_type
      when TypeRef::VOID    then MIR::TypeRef::VOID
      when TypeRef::BOOL    then MIR::TypeRef::BOOL
      when TypeRef::INT8    then MIR::TypeRef::INT8
      when TypeRef::INT16   then MIR::TypeRef::INT16
      when TypeRef::INT32   then MIR::TypeRef::INT32
      when TypeRef::INT64   then MIR::TypeRef::INT64
      when TypeRef::INT128  then MIR::TypeRef::INT128
      when TypeRef::UINT8   then MIR::TypeRef::UINT8
      when TypeRef::UINT16  then MIR::TypeRef::UINT16
      when TypeRef::UINT32  then MIR::TypeRef::UINT32
      when TypeRef::UINT64  then MIR::TypeRef::UINT64
      when TypeRef::UINT128 then MIR::TypeRef::UINT128
      when TypeRef::FLOAT32 then MIR::TypeRef::FLOAT32
      when TypeRef::FLOAT64 then MIR::TypeRef::FLOAT64
      when TypeRef::CHAR    then MIR::TypeRef::CHAR
      when TypeRef::STRING  then MIR::TypeRef::STRING
      when TypeRef::NIL     then MIR::TypeRef::NIL
      when TypeRef::SYMBOL  then MIR::TypeRef::SYMBOL
      when TypeRef::POINTER then MIR::TypeRef::POINTER
      else
        # User-defined types: offset by 20 (matching hir_to_mir.cr convert_type)
        MIR::TypeRef.new(hir_type.id + 20_u32)
      end
    end

    # Get type alignment in bytes
    private def type_alignment(type : TypeRef) : Int32
      case type
      when TypeRef::BOOL, TypeRef::INT8, TypeRef::UINT8
        1
      when TypeRef::INT16, TypeRef::UINT16
        2
      when TypeRef::INT32, TypeRef::UINT32, TypeRef::FLOAT32, TypeRef::CHAR
        4
      when TypeRef::INT64, TypeRef::UINT64, TypeRef::FLOAT64
        8
      when TypeRef::INT128, TypeRef::UINT128
        16
      else
        8  # Pointer alignment for reference types
      end
    end

    private def type_ref_for_expr(ctx : LoweringContext, expr_id : ExprId) : TypeRef
      name = get_type_name(expr_id)
      type_ref_for_name(name)
    end

    # Check if a type is a union type or nilable (T | Nil)
    private def is_union_or_nilable_type?(type : TypeRef) : Bool
      is_union_type?(type)
    end

    # Intrinsic: value.nil? for union types
    # Checks if the union's type tag indicates Nil variant
    private def lower_nil_check_intrinsic(ctx : LoweringContext, value_id : ValueId, value_type : TypeRef) : ValueId
      nil_variant_id = get_union_variant_id(value_type, TypeRef::NIL)
      if nil_variant_id < 0
        # Fallback: if we can't resolve a union variant, conservatively return false.
        # This should be rare (union descriptors are registered during AST->HIR conversion).
        lit = Literal.new(ctx.next_id, TypeRef::BOOL, false)
        ctx.emit(lit)
        return lit.id
      end

      # Emit UnionIs instruction to check if value is Nil variant
      union_is = UnionIs.new(ctx.next_id, value_id, nil_variant_id)
      ctx.emit(union_is)
      ctx.register_type(union_is.id, TypeRef::BOOL)
      union_is.id
    end

    # Intrinsic: value.not_nil! for union types
    # Extracts the non-nil value from a union (asserts it's not nil)
    private def lower_not_nil_intrinsic(ctx : LoweringContext, value_id : ValueId, value_type : TypeRef) : ValueId
      # Determine the non-nil type from the union descriptor
      non_nil_type = TypeRef::INT32  # Default fallback for Int32 | Nil

      # Try to get the actual non-nil type from the union descriptor
      mir_union_ref = hir_to_mir_type_ref(value_type)
      if descriptor = @union_descriptors[mir_union_ref]?
        descriptor.variants.each do |variant|
          if variant.type_ref != MIR::TypeRef::NIL
            # Convert MIR type back to HIR type (rough approximation)
            non_nil_type = case variant.type_ref
                           when MIR::TypeRef::INT32  then TypeRef::INT32
                           when MIR::TypeRef::INT64  then TypeRef::INT64
                           when MIR::TypeRef::FLOAT64 then TypeRef::FLOAT64
                           when MIR::TypeRef::BOOL   then TypeRef::BOOL
                           when MIR::TypeRef::POINTER then TypeRef::POINTER
                           else TypeRef::POINTER
                           end
            break
          end
        end
      end

      # Extract using UnionUnwrap (variant 0 is the non-nil type)
      unwrap = UnionUnwrap.new(ctx.next_id, non_nil_type, value_id, 0_i32, false)
      ctx.emit(unwrap)
      ctx.register_type(unwrap.id, non_nil_type)
      unwrap.id
    end
  end
end
