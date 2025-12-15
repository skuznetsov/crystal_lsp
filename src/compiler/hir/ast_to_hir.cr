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
                   when "Void", "Nil"    then TypeRef::VOID
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

    getter module : Module
    property arena : CrystalV2::Compiler::Frontend::ArenaLike

    # Pre-registered function signatures for forward reference support
    @function_types : Hash(String, TypeRef)

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

    # Functions that contain yield (candidates for inline)
    @yield_functions : Set(String)

    # Generic class templates (base name -> template)
    @generic_templates : Hash(String, GenericClassTemplate)

    # Already monomorphized generic classes (specialized name -> true)
    @monomorphized : Set(String)

    # Current type parameter substitutions for generic lowering
    @type_param_map : Hash(String, String)

    # Current method name being lowered (for super calls)
    @current_method : String?

    # Macro definitions (name -> MacroDefNode)
    @macro_defs : Hash(String, CrystalV2::Compiler::Frontend::MacroDefNode)

    # Type aliases (alias_name -> target_type_name)
    @type_aliases : Hash(String, String)

    # Track which allocators have been generated (to avoid duplicates for reopened classes)
    @generated_allocators : Set(String)

    def initialize(@arena, module_name : String = "main")
      @module = Module.new(module_name)
      @function_types = {} of String => TypeRef
      @class_info = {} of String => ClassInfo
      @init_params = {} of String => Array({String, TypeRef})
      @current_class = nil
      @current_method = nil
      @union_descriptors = {} of MIR::TypeRef => MIR::UnionDescriptor
      @function_defs = {} of String => CrystalV2::Compiler::Frontend::DefNode
      @yield_functions = Set(String).new
      @generic_templates = {} of String => GenericClassTemplate
      @monomorphized = Set(String).new
      @type_param_map = {} of String => String
      @macro_defs = {} of String => CrystalV2::Compiler::Frontend::MacroDefNode
      @type_aliases = {} of String => String
      @generated_allocators = Set(String).new
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
      target_name = String.new(node.value)
      @type_aliases[alias_name] = target_name
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

    # Register a module and its methods (pass 1)
    # Modules are like classes but with only class methods (self.method)
    # Also handles nested classes: module Foo; class Bar; end; end -> Foo::Bar
    def register_module(node : CrystalV2::Compiler::Frontend::ModuleNode)
      module_name = String.new(node.name)

      # Register module methods (def self.foo) and nested classes
      if body = node.body
        body.each do |expr_id|
          member = @arena[expr_id]
          case member
          when CrystalV2::Compiler::Frontend::DefNode
            method_name = String.new(member.name)
            # Module methods are always class methods: Module.method
            base_name = "#{module_name}.#{method_name}"
            return_type = if rt = member.return_type
                            type_ref_for_name(String.new(rt))
                          elsif method_name.ends_with?("?")
                            # Predicate methods (ending in ?) return Bool
                            TypeRef::BOOL
                          else
                            TypeRef::VOID
                          end
            # Collect parameter types for mangling
            param_types = [] of TypeRef
            if params = member.params
              params.each do |param|
                param_type = if ta = param.type_annotation
                               type_ref_for_name(String.new(ta))
                             else
                               TypeRef::VOID
                             end
                param_types << param_type
              end
            end
            full_name = mangle_function_name(base_name, param_types)
            @function_types[full_name] = return_type
          when CrystalV2::Compiler::Frontend::ClassNode
            # Nested class: Foo::Bar
            class_name = String.new(member.name)
            full_class_name = "#{module_name}::#{class_name}"
            # Create alias for the nested class name
            @type_aliases[full_class_name] = full_class_name
            # Register the class with its full name
            register_class_with_name(member, full_class_name)
          when CrystalV2::Compiler::Frontend::ModuleNode
            # Nested module: Foo::Bar (as module)
            nested_name = String.new(member.name)
            full_nested_name = "#{module_name}::#{nested_name}"
            # Recursively register nested module
            register_nested_module(member, full_nested_name)
          when CrystalV2::Compiler::Frontend::EnumNode
            # Nested enum: Foo::Bar
            enum_name = String.new(member.name)
            full_enum_name = "#{module_name}::#{enum_name}"
            register_enum_with_name(member, full_enum_name)
          end
        end
      end
    end

    # Register a nested module with full path
    private def register_nested_module(node : CrystalV2::Compiler::Frontend::ModuleNode, full_name : String)
      if body = node.body
        body.each do |expr_id|
          member = @arena[expr_id]
          case member
          when CrystalV2::Compiler::Frontend::DefNode
            method_name = String.new(member.name)
            base_name = "#{full_name}.#{method_name}"
            return_type = if rt = member.return_type
                            type_ref_for_name(String.new(rt))
                          elsif method_name.ends_with?("?")
                            # Predicate methods (ending in ?) return Bool
                            TypeRef::BOOL
                          else
                            TypeRef::VOID
                          end
            param_types = [] of TypeRef
            if params = member.params
              params.each do |param|
                param_type = if ta = param.type_annotation
                               type_ref_for_name(String.new(ta))
                             else
                               TypeRef::VOID
                             end
                param_types << param_type
              end
            end
            full_method_name = mangle_function_name(base_name, param_types)
            @function_types[full_method_name] = return_type
          when CrystalV2::Compiler::Frontend::ClassNode
            class_name = String.new(member.name)
            full_class_name = "#{full_name}::#{class_name}"
            @type_aliases[full_class_name] = full_class_name
            register_class_with_name(member, full_class_name)
          when CrystalV2::Compiler::Frontend::ModuleNode
            nested_name = String.new(member.name)
            full_nested_name = "#{full_name}::#{nested_name}"
            register_nested_module(member, full_nested_name)
          when CrystalV2::Compiler::Frontend::EnumNode
            enum_name = String.new(member.name)
            full_enum_name = "#{full_name}::#{enum_name}"
            register_enum_with_name(member, full_enum_name)
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
      if body = node.body
        body.each do |expr_id|
          member = @arena[expr_id]
          case member
          when CrystalV2::Compiler::Frontend::DefNode
            lower_module_method(module_name, member)
          when CrystalV2::Compiler::Frontend::ClassNode
            # Lower nested class with full name
            class_name = String.new(member.name)
            full_class_name = "#{module_name}::#{class_name}"
            lower_class_with_name(member, full_class_name)
          when CrystalV2::Compiler::Frontend::ModuleNode
            # Recursively lower nested module
            nested_name = String.new(member.name)
            full_nested_name = "#{module_name}::#{nested_name}"
            lower_module_with_name(member, full_nested_name)
          end
        end
      end
    end

    # Lower a module method (static function)
    private def lower_module_method(module_name : String, node : CrystalV2::Compiler::Frontend::DefNode)
      method_name = String.new(node.name)
      base_name = "#{module_name}.#{method_name}"

      return_type = if rt = node.return_type
                      type_ref_for_name(String.new(rt))
                    else
                      TypeRef::VOID
                    end

      # Collect parameter types for name mangling
      param_infos = [] of Tuple(String, TypeRef)
      if params = node.params
        params.each do |param|
          param_name = param.name.nil? ? "_" : String.new(param.name.not_nil!)
          param_type = if ta = param.type_annotation
                         type_ref_for_name(String.new(ta))
                       else
                         TypeRef::VOID
                       end
          param_infos << {param_name, param_type}
        end
      end

      # Mangle function name with parameter types
      param_types = param_infos.map { |_, t| t }
      full_name = mangle_function_name(base_name, param_types)

      func = @module.create_function(full_name, return_type)
      ctx = LoweringContext.new(func, @module, @arena)

      # Lower parameters (no self for module methods)
      param_infos.each do |(param_name, param_type)|
        hir_param = func.add_param(param_name, param_type)
        ctx.register_local(param_name, hir_param.id)
        ctx.register_type(hir_param.id, param_type)
      end

      # Set current class to module name for method resolution in body
      old_class = @current_class
      @current_class = module_name

      # Lower body
      last_value : ValueId? = nil
      if body = node.body
        body.each do |expr_id|
          last_value = lower_expr(ctx, expr_id)
        end
      end

      # Restore current class
      @current_class = old_class

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
      register_class_with_name(node, class_name)
    end

    # Register a class with a specific name (for nested classes like Foo::Bar)
    def register_class_with_name(node : CrystalV2::Compiler::Frontend::ClassNode, class_name : String)
      is_struct = node.is_struct == true

      # Check if this is a generic class (has type parameters)
      if type_params = node.type_params
        if type_params.size > 0
          # Store as generic template - don't create ClassInfo yet
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
        body.each do |expr_id|
          member = @arena[expr_id]
          case member
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
            base_name = "#{class_name}##{method_name}"
            return_type = if rt = member.return_type
                            type_ref_for_name(String.new(rt))
                          elsif method_name.ends_with?("?")
                            # Predicate methods (ending in ?) return Bool
                            TypeRef::BOOL
                          else
                            TypeRef::VOID
                          end
            # Collect parameter types for mangling
            method_param_types = [] of TypeRef
            if params = member.params
              params.each do |param|
                param_type = if ta = param.type_annotation
                               type_ref_for_name(String.new(ta))
                             else
                               TypeRef::VOID
                             end
                method_param_types << param_type
              end
            end
            full_name = mangle_function_name(base_name, method_param_types)
            @function_types[full_name] = return_type

            # Capture initialize parameters for new()
            # Also extract ivars from shorthand: def initialize(@value : T)
            # Note: Only capture from FIRST initialize (for multiple overloads, each gets its own mangled name)
            if method_name == "initialize" && init_params.empty?
              if params = member.params
                params.each do |param|
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
              @function_types[full_name] = ivar_type
            end

          when CrystalV2::Compiler::Frontend::SetterNode
            # Setter declarations: setter name : Type
            # Creates @name ivar and def name=(value : Type); @name = value; end
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
              @function_types[full_name] = ivar_type
            end

          when CrystalV2::Compiler::Frontend::PropertyNode
            # Property declarations: property name : Type
            # Creates both getter and setter
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
              @function_types[getter_full] = ivar_type
              # Register setter method
              setter_name = "#{class_name}##{accessor_name}="
              setter_full = mangle_function_name(setter_name, [ivar_type])
              @function_types[setter_full] = ivar_type
            end
          end
        end
      end

      # Create class/struct type (reuse existing type_ref for reopened classes)
      type_kind = is_struct ? TypeKind::Struct : TypeKind::Class
      type_ref = if existing_info
                   existing_info.type_ref
                 else
                   @module.intern_type(TypeDescriptor.new(type_kind, class_name))
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
      @function_types["#{class_name}.new"] = type_ref
    end

    # Monomorphize a generic class: create specialized version with concrete types
    private def monomorphize_generic_class(base_name : String, type_args : Array(String), specialized_name : String)
      template = @generic_templates[base_name]?
      return unless template

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
            if member.is_a?(CrystalV2::Compiler::Frontend::DefNode)
              lower_method(specialized_name, class_info, member)
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
      @current_class = class_name

      # Generate allocator function: ClassName.new
      generate_allocator(class_name, class_info)

      # Lower each method
      if body = node.body
        body.each do |expr_id|
          member = @arena[expr_id]
          case member
          when CrystalV2::Compiler::Frontend::DefNode
            lower_method(class_name, class_info, member)
          when CrystalV2::Compiler::Frontend::GetterNode
            # Generate synthetic getter methods
            member.specs.each do |spec|
              generate_getter_method(class_name, class_info, spec)
            end
          when CrystalV2::Compiler::Frontend::SetterNode
            # Generate synthetic setter methods
            member.specs.each do |spec|
              generate_setter_method(class_name, class_info, spec)
            end
          when CrystalV2::Compiler::Frontend::PropertyNode
            # Generate both getter and setter methods
            member.specs.each do |spec|
              generate_getter_method(class_name, class_info, spec)
              generate_setter_method(class_name, class_info, spec)
            end
          end
        end
      end

      @current_class = nil
    end

    # Generate allocator: ClassName.new(...) -> allocates and returns instance
    private def generate_allocator(class_name : String, class_info : ClassInfo)
      func_name = "#{class_name}.new"

      # Skip Pointer types - they're primitive types with no allocator
      if class_name.starts_with?("Pointer(") || class_name.starts_with?("Pointer_")
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

      # Find ivar info
      ivar_info = class_info.ivars.find { |iv| iv.name == ivar_name }
      return unless ivar_info

      ivar_type = ivar_info.type

      # Create mangled function name
      base_name = "#{class_name}##{accessor_name}"
      func_name = mangle_function_name(base_name, [] of TypeRef)

      # Skip if already generated
      return if @module.has_function?(func_name)

      # Create function returning the ivar type
      func = @module.create_function(func_name, ivar_type)
      ctx = LoweringContext.new(func, @module, @arena)

      # Add self parameter
      self_param = func.add_param("self", class_info.type_ref)
      ctx.register_local("self", self_param.id)
      ctx.register_type(self_param.id, class_info.type_ref)

      # Read the ivar
      field_get = FieldGet.new(ctx.next_id, ivar_type, self_param.id, ivar_name, ivar_info.offset)
      ctx.emit(field_get)
      ctx.register_type(field_get.id, ivar_type)

      # Return the value
      ctx.terminate(Return.new(field_get.id))
    end

    # Generate synthetic setter method: def name=(value); @name = value; end
    private def generate_setter_method(class_name : String, class_info : ClassInfo, spec : CrystalV2::Compiler::Frontend::AccessorSpec)
      accessor_name = String.new(spec.name)
      ivar_name = "@#{accessor_name}"

      # Find ivar info
      ivar_info = class_info.ivars.find { |iv| iv.name == ivar_name }
      return unless ivar_info

      ivar_type = ivar_info.type

      # Create mangled function name with parameter type
      base_name = "#{class_name}##{accessor_name}="
      func_name = mangle_function_name(base_name, [ivar_type])

      # Skip if already generated
      return if @module.has_function?(func_name)

      # Create function returning the ivar type (returns the set value)
      func = @module.create_function(func_name, ivar_type)
      ctx = LoweringContext.new(func, @module, @arena)

      # Add self parameter
      self_param = func.add_param("self", class_info.type_ref)
      ctx.register_local("self", self_param.id)
      ctx.register_type(self_param.id, class_info.type_ref)

      # Add value parameter
      value_param = func.add_param("value", ivar_type)
      ctx.register_local("value", value_param.id)
      ctx.register_type(value_param.id, ivar_type)

      # Write to ivar
      field_set = FieldSet.new(ctx.next_id, TypeRef::VOID, self_param.id, ivar_name, value_param.id, ivar_info.offset)
      ctx.emit(field_set)

      # Return the value
      ctx.terminate(Return.new(value_param.id))
    end

    # Lower a method within a class
    private def lower_method(class_name : String, class_info : ClassInfo, node : CrystalV2::Compiler::Frontend::DefNode)
      method_name = String.new(node.name)
      base_name = "#{class_name}##{method_name}"

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
      @current_method = method_name

      return_type = if rt = node.return_type
                      type_ref_for_name(String.new(rt))
                    elsif method_name.ends_with?("?")
                      # Predicate methods (ending in ?) return Bool
                      TypeRef::BOOL
                    else
                      TypeRef::VOID
                    end

      # Collect parameter types first for name mangling
      param_infos = [] of Tuple(String, TypeRef, Bool)  # (name, type, is_instance_var)
      if params = node.params
        params.each do |param|
          param_name = param.name.nil? ? "_" : String.new(param.name.not_nil!)
          param_type = if ta = param.type_annotation
                         type_ref_for_name(String.new(ta))
                       else
                         TypeRef::VOID
                       end
          param_infos << {param_name, param_type, param.is_instance_var}
        end
      end

      # Mangle function name with parameter types for overloading
      param_types = param_infos.map { |_, t, _| t }
      full_name = mangle_function_name(base_name, param_types)

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

      # Add implicit return if not already terminated
      # BUT don't add return after raise (which sets Unreachable terminator)
      block = ctx.get_block(ctx.current_block)
      block_has_raise = block.instructions.any? { |inst| inst.is_a?(Raise) }
      if block.terminator.is_a?(Unreachable) && !block_has_raise
        block.terminator = Return.new(last_value)
      end

      # Restore previous method context
      @current_method = old_method
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

    # Mangle function name with parameter types for overloading
    # Example: "IO.print" + [String] -> "IO.print$String"
    # Example: "Int32#++" + [Int32] -> "Int32#+$Int32"
    # Note: Using $ instead of : because LLVM doesn't support : in identifiers
    private def mangle_function_name(base_name : String, param_types : Array(TypeRef)) : String
      return base_name if param_types.empty?
      type_suffix = param_types.map { |t| type_name_for_mangling(t) }.join("_")
      "#{base_name}$#{type_suffix}"
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

      # Try to find the function in the module
      # First try the mangled name, then try variations
      return_type = get_function_return_type(mangled_name)
      if return_type != TypeRef::VOID
        return mangled_name
      end

      # Try base method name without mangling
      return_type = get_function_return_type(base_method_name)
      if return_type != TypeRef::VOID
        return base_method_name
      end

      # Search through all class info for matching method
      @class_info.each do |klass_name, info|
        test_base = "#{klass_name}##{method_name}"
        test_mangled = mangle_function_name(test_base, arg_types)
        if @function_types[test_mangled]?
          return test_mangled
        elsif @function_types[test_base]?
          return test_base
        end
      end

      # Fallback to mangled name
      mangled_name
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
      return_type = if rt = node.return_type
                      type_ref_for_name(String.new(rt))
                    else
                      TypeRef::VOID
                    end

      # Collect parameter types for name mangling
      param_types = [] of TypeRef
      if params = node.params
        params.each do |param|
          param_type = if ta = param.type_annotation
                         type_ref_for_name(String.new(ta))
                       else
                         TypeRef::VOID
                       end
          param_types << param_type
        end
      end

      # Register with mangled name
      full_name = mangle_function_name(base_name, param_types)
      @function_types[full_name] = return_type

      # Also register with base name for fallback lookup
      # (when function is not overloaded, we look up by base name)
      @function_types[base_name] = return_type

      # Store AST for potential inline expansion (use mangled name)
      @function_defs[full_name] = node

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
      node = @arena[expr_id]
      case node
      when CrystalV2::Compiler::Frontend::YieldNode
        true
      when CrystalV2::Compiler::Frontend::IfNode
        contains_yield?(node.then_body) ||
          (node.else_body ? contains_yield?(node.else_body.not_nil!) : false)
      when CrystalV2::Compiler::Frontend::WhileNode
        contains_yield?(node.body)
      when CrystalV2::Compiler::Frontend::LoopNode
        contains_yield?(node.body)
      when CrystalV2::Compiler::Frontend::BlockNode
        contains_yield?(node.body)
      when CrystalV2::Compiler::Frontend::CaseNode
        node.when_branches.any? { |w| contains_yield?(w.body) } ||
          (node.else_branch ? contains_yield?(node.else_branch.not_nil!) : false)
      else
        false
      end
    end

    # Check if a module method exists (with any signature - for module detection)
    private def is_module_method?(module_name : String, method_name : String) : Bool
      prefix = "#{module_name}.#{method_name}"
      @function_types.keys.any? { |key| key.starts_with?(prefix) }
    end

    # Look up return type of a function by name
    private def get_function_return_type(name : String) : TypeRef
      # First check pre-registered signatures (for forward references)
      if type = @function_types[name]?
        return type
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
      # First check if it's directly in class_info
      return name if @class_info.has_key?(name)

      # Try to resolve using current class's namespace
      if current = @current_class
        # Extract namespace: "Foo::Bar::Baz" -> "Foo::Bar"
        parts = current.split("::")
        # Try increasingly shorter prefixes
        while parts.size > 0
          parts.pop
          qualified_name = (parts + [name]).join("::")
          if @class_info.has_key?(qualified_name)
            return qualified_name
          end
        end
      end

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

    # Returns the fully qualified method name (e.g., "Animal#age" or "Animal#age:Int32") or nil if not found
    # Note: Returns the base name without mangling - caller should mangle with actual arg types
    private def resolve_method_with_inheritance(class_name : String, method_name : String) : String?
      current = class_name
      while true
        test_name = "#{current}##{method_name}"
        # Check for any function with this prefix (handles mangled names)
        found = @function_types.keys.find { |key| key.starts_with?(test_name) }
        if found
          return test_name  # Return base name - caller will mangle
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
    def lower_def(node : CrystalV2::Compiler::Frontend::DefNode) : Function
      name = String.new(node.name)

      # Determine return type (default to Void if not specified)
      return_type = if rt = node.return_type
                      type_ref_for_name(String.new(rt))
                    else
                      TypeRef::VOID
                    end

      func = @module.create_function(name, return_type)
      ctx = LoweringContext.new(func, @module, @arena)

      # Lower parameters
      if params = node.params
        params.each do |param|
          param_name = param.name.nil? ? "_" : String.new(param.name.not_nil!)
          param_type = if ta = param.type_annotation
                         type_ref_for_name(String.new(ta))
                       else
                         TypeRef::VOID  # Unknown type
                       end

          hir_param = func.add_param(param_name, param_type)
          ctx.register_local(param_name, hir_param.id)
          ctx.register_type(hir_param.id, param_type)  # Track param type for inference
        end
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

      func
    end

    # Lower top-level expressions into a synthetic main function
    def lower_main(main_exprs : Array(Tuple(CrystalV2::Compiler::Frontend::ExprId, CrystalV2::Compiler::Frontend::ArenaLike))) : Function
      # Create main function with Int32 return type (standard C main)
      func = @module.create_function("main", TypeRef::INT32)
      ctx = LoweringContext.new(func, @module, @arena)

      # Lower each top-level expression in order
      last_value : ValueId? = nil
      main_exprs.each do |expr_id, arena|
        # Switch arena context for this expression
        @arena = arena
        last_value = lower_expr(ctx, expr_id)
      end

      # Return 0 for success (main returns int)
      block = ctx.get_block(ctx.current_block)
      if block.terminator.is_a?(Unreachable)
        zero = Literal.new(ctx.next_id, TypeRef::INT32, 0_i64)
        ctx.emit(zero)
        block.terminator = Return.new(zero.id)
      end

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
        lower_yield(ctx, node)

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
      value = if node.kind.f32? || node.kind.f64?
                value_str.to_f64
              else
                # Handle hex (0x), binary (0b), and octal (0o)
                if value_str.starts_with?("0x") || value_str.starts_with?("0X")
                  value_str[2..].to_i64(16)
                elsif value_str.starts_with?("0b") || value_str.starts_with?("0B")
                  value_str[2..].to_i64(2)
                elsif value_str.starts_with?("0o") || value_str.starts_with?("0O")
                  value_str[2..].to_i64(8)
                else
                  value_str.to_i64
                end
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
        ctx.register_type(value, type_ref)
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
        # Check if method exists in current class (with inheritance)
        class_method_base = resolve_method_with_inheritance(current_class, name)
        if class_method_base
          # This is a method call on self with no arguments
          self_id = emit_self(ctx)
          full_name = mangle_function_name(class_method_base, [] of TypeRef)
          return_type = @function_types[full_name]? || TypeRef::VOID
          call = Call.new(ctx.next_id, return_type, self_id, full_name, [] of ValueId)
          ctx.emit(call)
          ctx.register_type(call.id, return_type)
          return call.id
        end
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

      # Get return type from mangled name
      return_type = @function_types[super_method_name]? || TypeRef::VOID

      # Get self for the call
      self_id = emit_self(ctx)

      # Call parent method
      call = Call.new(ctx.next_id, return_type, self_id, super_method_name, args)
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
      base_node = @arena[node.base_type]
      base_name = case base_node
                  when CrystalV2::Compiler::Frontend::ConstantNode
                    String.new(base_node.name)
                  when CrystalV2::Compiler::Frontend::IdentifierNode
                    String.new(base_node.name)
                  else
                    "Unknown"
                  end

      # Extract type arguments, substituting any type parameters
      type_args = node.type_args.map do |arg_id|
        arg_node = @arena[arg_id]
        arg_name = case arg_node
                   when CrystalV2::Compiler::Frontend::ConstantNode
                     String.new(arg_node.name)
                   when CrystalV2::Compiler::Frontend::IdentifierNode
                     String.new(arg_node.name)
                   else
                     "Unknown"
                   end
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
      left_id = lower_expr(ctx, node.left)
      right_id = lower_expr(ctx, node.right)

      # Check for pointer arithmetic: ptr + n or ptr - n
      left_type = ctx.type_of(left_id)
      op_str = node.operator_string
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
        # Use mangled name "shovel" since << is not valid LLVM identifier
        call = Call.new(ctx.next_id, left_type, left_id, "shovel", [right_id])
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
           when "&&"  then BinaryOp::And
           when "||"  then BinaryOp::Or
           else
             # Unknown operator - emit as method call
             return emit_binary_call(ctx, left_id, op_str, right_id)
           end

      # For && and ||, both operands must be boolean
      # If they're not, convert them (pointer types become nil-check, others assume truthy)
      if op == BinaryOp::And || op == BinaryOp::Or
        if left_type == TypeRef::POINTER
          # Pointer type: check if not null
          nil_val = Literal.new(ctx.next_id, TypeRef::POINTER, 0_i64)
          ctx.emit(nil_val)
          ne_check = BinaryOperation.new(ctx.next_id, TypeRef::BOOL, BinaryOp::Ne, left_id, nil_val.id)
          ctx.emit(ne_check)
          left_id = ne_check.id
        elsif left_type != TypeRef::BOOL
          # Union or other types: just emit true constant for now (simplified)
          # TODO: proper union nil-check by examining type_id field
          true_val = Literal.new(ctx.next_id, TypeRef::BOOL, 1_i64)
          ctx.emit(true_val)
          left_id = true_val.id
        end
        right_type = ctx.type_of(right_id)
        if right_type == TypeRef::POINTER
          nil_val = Literal.new(ctx.next_id, TypeRef::POINTER, 0_i64)
          ctx.emit(nil_val)
          ne_check = BinaryOperation.new(ctx.next_id, TypeRef::BOOL, BinaryOp::Ne, right_id, nil_val.id)
          ctx.emit(ne_check)
          right_id = ne_check.id
        elsif right_type != TypeRef::BOOL
          true_val = Literal.new(ctx.next_id, TypeRef::BOOL, 1_i64)
          ctx.emit(true_val)
          right_id = true_val.id
        end
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

    private def emit_binary_call(ctx : LoweringContext, left : ValueId, op : String, right : ValueId) : ValueId
      call = Call.new(ctx.next_id, TypeRef::VOID, left, op, [right])
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
             call = Call.new(ctx.next_id, TypeRef::VOID, operand_id, op_str, [] of ValueId)
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
      cond_id = lower_expr(ctx, node.condition)

      then_block = ctx.create_block
      else_block = ctx.create_block
      merge_block = ctx.create_block

      # Save locals state before branching
      pre_branch_locals = ctx.save_locals

      # Branch on condition
      ctx.terminate(Branch.new(cond_id, then_block, else_block))

      # Then branch
      ctx.current_block = then_block
      ctx.push_scope(ScopeKind::Block)
      then_value = lower_body(ctx, node.then_body)
      then_exit_block = ctx.current_block
      then_locals = ctx.save_locals
      ctx.pop_scope

      # Only jump if block isn't already terminated (e.g., by return or raise)
      # Check if block ends with a noreturn instruction (Raise, Return, etc.)
      then_block_data = ctx.get_block(ctx.current_block)
      then_has_noreturn = then_block_data.instructions.any? { |inst| inst.is_a?(Raise) || inst.is_a?(Return) }
      then_flows_to_merge = then_block_data.terminator.is_a?(Unreachable) && !then_has_noreturn
      if then_flows_to_merge
        ctx.terminate(Jump.new(merge_block))
      end

      # Restore locals for else branch (else branch sees pre-branch state)
      ctx.restore_locals(pre_branch_locals)

      # Else branch
      ctx.current_block = else_block
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

      # Check if else block has noreturn instruction
      else_block_data = ctx.get_block(ctx.current_block)
      else_has_noreturn = else_block_data.instructions.any? { |inst| inst.is_a?(Raise) || inst.is_a?(Return) }
      else_flows_to_merge = else_block_data.terminator.is_a?(Unreachable) && !else_has_noreturn
      if else_flows_to_merge
        ctx.terminate(Jump.new(merge_block))
      end

      # Merge block with phi for the if expression value
      ctx.current_block = merge_block

      # Only create phi if at least one branch flows to merge
      if then_flows_to_merge || else_flows_to_merge
        if then_flows_to_merge && else_flows_to_merge
          # Both branches flow to merge - need phi (unless void type)
          then_type = ctx.type_of(then_value)
          else_type = ctx.type_of(else_value)

          # Determine phi type - handle type unification
          phi_type : TypeRef
          actual_then_value = then_value
          actual_else_value = else_value

          if then_type == else_type
            # Same types - simple case
            phi_type = then_type
          elsif else_type == TypeRef::NIL && then_type != TypeRef::VOID
            # Else is nil, then is concrete type -> create union T | Nil
            phi_type = create_union_type_for_nullable(then_type)

            # Wrap then value in union (variant 0 = concrete type)
            then_wrap = UnionWrap.new(ctx.next_id, phi_type, then_value, 0)
            ctx.emit_to_block(then_exit_block, then_wrap)
            ctx.register_type(then_wrap.id, phi_type)
            actual_then_value = then_wrap.id

            # Wrap nil in union (variant 1 = Nil)
            nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
            ctx.emit_to_block(else_exit_block, nil_lit)
            nil_wrap = UnionWrap.new(ctx.next_id, phi_type, nil_lit.id, 1)
            ctx.emit_to_block(else_exit_block, nil_wrap)
            ctx.register_type(nil_wrap.id, phi_type)
            actual_else_value = nil_wrap.id
          elsif then_type == TypeRef::NIL && else_type != TypeRef::VOID
            # Then is nil, else is concrete type -> create union T | Nil
            phi_type = create_union_type_for_nullable(else_type)

            # Wrap nil in union (variant 1 = Nil)
            nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
            ctx.emit_to_block(then_exit_block, nil_lit)
            nil_wrap = UnionWrap.new(ctx.next_id, phi_type, nil_lit.id, 1)
            ctx.emit_to_block(then_exit_block, nil_wrap)
            ctx.register_type(nil_wrap.id, phi_type)
            actual_then_value = nil_wrap.id

            # Wrap else value in union (variant 0 = concrete type)
            else_wrap = UnionWrap.new(ctx.next_id, phi_type, else_value, 0)
            ctx.emit_to_block(else_exit_block, else_wrap)
            ctx.register_type(else_wrap.id, phi_type)
            actual_else_value = else_wrap.id
          else
            # Different non-nil types - just use then_type for now
            # TODO: proper type unification
            phi_type = then_type
          end

          # Don't create phi for void types - LLVM doesn't allow phi void
          if phi_type == TypeRef::VOID || phi_type == TypeRef::NIL
            merge_branch_locals(ctx, pre_branch_locals, then_locals, else_locals,
                                then_exit_block, else_exit_block)
            # Return nil literal as the result of void if-expression
            nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
            ctx.emit(nil_lit)
            return nil_lit.id
          end

          phi = Phi.new(ctx.next_id, phi_type)
          phi.add_incoming(then_exit_block, actual_then_value)
          phi.add_incoming(else_exit_block, actual_else_value)
          ctx.emit(phi)

          # Merge locals from both branches
          merge_branch_locals(ctx, pre_branch_locals, then_locals, else_locals,
                              then_exit_block, else_exit_block)
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

      # Neither branch flows to merge (both return) - emit nil as placeholder
      nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
      ctx.emit(nil_lit)
      nil_lit.id
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

      # Save the initial values of variables before the loop
      pre_loop_block = ctx.current_block
      initial_values = {} of String => ValueId
      assigned_vars.each do |var_name|
        if val = ctx.lookup_local(var_name)
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
        end
      end

      cond_id = lower_expr(ctx, node.condition)
      ctx.terminate(Branch.new(cond_id, body_block, exit_block))

      # Body block
      ctx.current_block = body_block
      ctx.push_scope(ScopeKind::Loop)
      lower_body(ctx, node.body)
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

      # Save the initial values of variables before the loop
      pre_loop_block = ctx.current_block
      initial_values = {} of String => ValueId
      assigned_vars.each do |var_name|
        if val = ctx.lookup_local(var_name)
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
        end
      end

      ctx.push_scope(ScopeKind::Loop)
      lower_body(ctx, node.body)
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
    private def collect_assigned_vars(body : Array(ExprId)) : Array(String)
      vars = [] of String
      body.each do |expr_id|
        collect_assigned_vars_in_expr(expr_id, vars)
      end
      vars.uniq
    end

    private def collect_assigned_vars_in_expr(expr_id : ExprId, vars : Array(String))
      node = @arena[expr_id]
      case node
      when CrystalV2::Compiler::Frontend::AssignNode
        target = @arena[node.target]
        if target.is_a?(CrystalV2::Compiler::Frontend::IdentifierNode)
          vars << String.new(target.name)
        end
        # Also check the value side for nested assignments
        collect_assigned_vars_in_expr(node.value, vars)

      when CrystalV2::Compiler::Frontend::WhileNode
        # Nested while - check its body
        collect_assigned_vars(node.body).each { |v| vars << v }

      when CrystalV2::Compiler::Frontend::LoopNode
        # Nested loop - check its body
        collect_assigned_vars(node.body).each { |v| vars << v }

      when CrystalV2::Compiler::Frontend::IfNode
        # Check all branches
        collect_assigned_vars(node.then_body).each { |v| vars << v }
        if else_body = node.else_body
          collect_assigned_vars(else_body).each { |v| vars << v }
        end

      when CrystalV2::Compiler::Frontend::UnlessNode
        # Check all branches
        collect_assigned_vars(node.then_branch).each { |v| vars << v }
        if else_body = node.else_branch
          collect_assigned_vars(else_body).each { |v| vars << v }
        end

      when CrystalV2::Compiler::Frontend::CaseNode
        # Check all when branches and else
        node.when_branches.each do |when_branch|
          collect_assigned_vars(when_branch.body).each { |v| vars << v }
        end
        if else_body = node.else_branch
          collect_assigned_vars(else_body).each { |v| vars << v }
        end

      when CrystalV2::Compiler::Frontend::BinaryNode
        collect_assigned_vars_in_expr(node.left, vars)
        collect_assigned_vars_in_expr(node.right, vars)

      when CrystalV2::Compiler::Frontend::CallNode
        node.args.each { |arg| collect_assigned_vars_in_expr(arg, vars) }

      when CrystalV2::Compiler::Frontend::GroupingNode
        collect_assigned_vars_in_expr(node.expression, vars)
      end
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

      phi_type = incoming.first?.try { |(_, val)| ctx.type_of(val) } || TypeRef::VOID

      # Don't create phi for void types - LLVM doesn't allow phi void
      if phi_type == TypeRef::VOID || phi_type == TypeRef::NIL
        nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
        ctx.emit(nil_lit)
        return nil_lit.id
      end

      # If only one branch flows, no phi needed
      if incoming.size == 1
        return incoming.first[1]
      end

      phi = Phi.new(ctx.next_id, phi_type)
      incoming.each { |(blk, val)| phi.add_incoming(blk, val) }
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
          has_class_method = @function_types.keys.any? { |k| k.starts_with?(class_method_name) }
          if has_class_method
            # This is a method call on self - set receiver to self
            receiver_id = emit_self(ctx)
            full_method_name = class_method_name
          else
            # Also check for module-style method (Module.method)
            module_method_name = "#{current}.#{method_name}"
            has_module_method = @function_types.keys.any? { |k| k.starts_with?(module_method_name) }
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

        # Check if it's a class/module method call (ClassName.new() or Module.method())
        # Can be ConstantNode, IdentifierNode starting with uppercase, or GenericNode
        class_name_str : String? = nil
        if obj_node.is_a?(CrystalV2::Compiler::Frontend::ConstantNode)
          name = String.new(obj_node.name)
          # Resolve type alias if exists, then check class_info
          class_name_str = @type_aliases[name]? || name
          # If the short name isn't a known class, try to resolve using current namespace
          unless @class_info.has_key?(class_name_str)
            class_name_str = resolve_class_name_in_context(name)
          end
        elsif obj_node.is_a?(CrystalV2::Compiler::Frontend::IdentifierNode)
          name = String.new(obj_node.name)
          # Resolve type alias if exists
          resolved_name = @type_aliases[name]? || name
          # Check if it's a class name (starts with uppercase and is known class)
          # OR a module name (check if Module.method exists in function_types)
          if resolved_name[0].uppercase?
            if @class_info.has_key?(resolved_name)
              class_name_str = resolved_name
            elsif is_module_method?(resolved_name, method_name)
              # It's a module method call
              class_name_str = resolved_name
            else
              # Try to resolve using current namespace
              class_name_str = resolve_class_name_in_context(name)
            end
          end
        elsif obj_node.is_a?(CrystalV2::Compiler::Frontend::GenericNode)
          # Generic type like Box(Int32).new()
          # Extract base type name and type arguments
          base_node = @arena[obj_node.base_type]
          base_name = case base_node
                      when CrystalV2::Compiler::Frontend::ConstantNode
                        String.new(base_node.name)
                      when CrystalV2::Compiler::Frontend::IdentifierNode
                        String.new(base_node.name)
                      else
                        nil
                      end
          if base_name
            # Extract type argument names, substituting type parameters
            type_args = obj_node.type_args.map do |arg_id|
              arg_node = @arena[arg_id]
              arg_name = case arg_node
                         when CrystalV2::Compiler::Frontend::ConstantNode
                           String.new(arg_node.name)
                         when CrystalV2::Compiler::Frontend::IdentifierNode
                           String.new(arg_node.name)
                         else
                           "Unknown"
                         end
              # Substitute type parameter if applicable
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
          elsif @type_aliases.has_key?(full_path)
            class_name_str = @type_aliases[full_path]
          end
        end

        if class_name_str
          # Class method call like Counter.new()
          full_method_name = "#{class_name_str}.#{method_name}"
          receiver_id = nil  # Static call, no receiver
        else
          # Instance method call like c.increment()
          receiver_id = lower_expr(ctx, callee_node.object)

          # Try to determine the class from receiver's type
          receiver_type = ctx.type_of(receiver_id)
          if receiver_type.id > 0
            # Look up class name from type, then resolve method with inheritance
            @class_info.each do |name, info|
              if info.type_ref.id == receiver_type.id
                # Use inheritance-aware method resolution
                full_method_name = resolve_method_with_inheritance(name, method_name)
                break
              end
            end
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
      args = if named_args = node.named_args
               reorder_named_args(ctx, node.args, named_args, method_name, full_method_name)
             else
               expand_splat_args(ctx, node.args)
             end

      # Handle .times { |i| body } intrinsic BEFORE lowering block
      if method_name == "times" && receiver_id
        if blk_expr = node.block
          blk_node = @arena[blk_expr]
          if blk_node.is_a?(CrystalV2::Compiler::Frontend::BlockNode)
            return lower_times_intrinsic(ctx, receiver_id, blk_node)
          end
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
              return lower_array_map_dynamic(ctx, receiver_id, blk_node)
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
                           # Bare method call inside a class - could be self.method()
                           # Try current class first
                           "#{current}##{method_name}"
                         else
                           method_name
                         end
      mangled_method_name = mangle_function_name(base_method_name, arg_types)

      # Handle yield-functions with inline expansion FIRST (before lowering block)
      # Must check with mangled name since that's how yield functions are registered
      if blk_expr = node.block
        blk_node = @arena[blk_expr]
        if blk_node.is_a?(CrystalV2::Compiler::Frontend::BlockNode)
          # Check if this is a call to a yield-function using mangled name
          if @yield_functions.includes?(mangled_method_name)
            if func_def = @function_defs[mangled_method_name]?
              return inline_yield_function(ctx, func_def, args, blk_node)
            end
          end
          # Also try base method name (for functions without overloading)
          if @yield_functions.includes?(base_method_name)
            if func_def = @function_defs[base_method_name]?
              return inline_yield_function(ctx, func_def, args, blk_node)
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

      # If still not found and receiver_id is set, try to find method in any class
      # NOTE: This fallback doesn't use inheritance - it's a last resort
      if return_type == TypeRef::VOID && receiver_id
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

      # Check for pointer primitive operations
      # Pointer(T).malloc(count) -> PointerMalloc
      if full_method_name && full_method_name.starts_with?("Pointer(") && method_name == "malloc" && args.size == 1
        element_type = pointer_element_type(full_method_name)
        malloc_node = PointerMalloc.new(ctx.next_id, TypeRef::POINTER, element_type, args[0])
        ctx.emit(malloc_node)
        ctx.register_type(malloc_node.id, TypeRef::POINTER)
        return malloc_node.id
      end

      # ptr.value or ptr[index] -> PointerLoad
      if receiver_id && (method_name == "value" || method_name == "[]")
        receiver_type = ctx.type_of(receiver_id)
        if receiver_type == TypeRef::POINTER
          index_id = if method_name == "[]" && args.size == 1
                       args[0]
                     else
                       nil
                     end
          # Return the dereferenced type (we use INT32 as default for untyped pointers)
          deref_type = TypeRef::INT32  # TODO: track pointer element type
          load_node = PointerLoad.new(ctx.next_id, deref_type, receiver_id, index_id)
          ctx.emit(load_node)
          ctx.register_type(load_node.id, deref_type)
          return load_node.id
        end
      end

      # ptr.value= or ptr[index]= -> PointerStore
      if receiver_id && (method_name == "value=" || method_name == "[]=")
        receiver_type = ctx.type_of(receiver_id)
        if receiver_type == TypeRef::POINTER
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
        if receiver_type == TypeRef::POINTER
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
        if receiver_type == TypeRef::POINTER
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
                                           "first", "last", "dup", "clone", "cover", "tap",
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
      methods_returning_receiver_type = ["clamp", "abs", "ceil", "floor", "round", "truncate"]
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

      # Coerce arguments to union types if needed
      # This handles cases like passing Int32 to a parameter of type Int32 | Nil
      args = coerce_args_to_param_types(ctx, args, mangled_method_name)

      call = Call.new(ctx.next_id, return_type, receiver_id, mangled_method_name, args, block_id)
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
          variant_id = get_union_variant_id(arg_type, param_type)
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

    # Get the variant ID for a concrete type within a union
    private def get_union_variant_id(concrete_type : TypeRef, union_type : TypeRef) : Int32
      # For unions like Int32 | Nil:
      # - variant 0 = Int32 (or other concrete type)
      # - variant 1 = Nil
      # This matches our convention in if/else lowering
      if concrete_type == TypeRef::NIL
        1  # Nil is always variant 1
      else
        0  # Concrete types are variant 0
      end
    end

    # Reorder named arguments to match parameter positions
    private def reorder_named_args(
      ctx : LoweringContext,
      positional_args : Array(ExprId),
      named_args : Array(CrystalV2::Compiler::Frontend::NamedArgument),
      method_name : String,
      full_method_name : String?
    ) : Array(ValueId)
      # First, lower positional args
      result = positional_args.map { |arg| lower_expr(ctx, arg) }

      # Get parameter names from function definition
      func_name = full_method_name || method_name
      func_def = @function_defs[func_name]?

      if func_def && (params = func_def.params)
        param_names = params.map do |p|
          if name = p.name
            String.new(name)
          else
            ""
          end
        end

        # Process named args
        named_args.each do |named_arg|
          arg_name = String.new(named_arg.name)
          arg_value = lower_expr(ctx, named_arg.value)

          # Find position of this parameter
          idx = param_names.index(arg_name)
          if idx
            # Extend result array if needed
            while result.size <= idx
              # Fill with nil placeholder (will be replaced)
              nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
              ctx.emit(nil_lit)
              result << nil_lit.id
            end
            result[idx] = arg_value
          else
            # Unknown parameter name - just append
            result << arg_value
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

      # Save initial values of mutable variables before the loop
      entry_block = ctx.current_block
      initial_values = {} of String => ValueId
      assigned_vars.each do |var_name|
        if val = ctx.lookup_local(var_name)
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
      lower_body(ctx, block.body)
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

      entry_block = ctx.current_block
      initial_values = {} of String => ValueId
      assigned_vars.each do |var_name|
        if val = ctx.lookup_local(var_name)
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
      lower_body(ctx, block.body)
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

      entry_block = ctx.current_block
      initial_values = {} of String => ValueId
      assigned_vars.each do |var_name|
        if val = ctx.lookup_local(var_name)
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

      lower_body(ctx, block.body)
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

      entry_block = ctx.current_block
      initial_values = {} of String => ValueId
      assigned_vars.each do |var_name|
        if val = ctx.lookup_local(var_name)
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

      lower_body(ctx, block.body)
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

      entry_block = ctx.current_block
      initial_values = {} of String => ValueId
      assigned_vars.each do |var_name|
        if val = ctx.lookup_local(var_name)
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

      lower_body(ctx, block.body)
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

    # Inline a yield-function call with block
    # Transforms: func(args) { |params| block_body }
    # Into: inline func body, replacing yield with block_body
    private def inline_yield_function(
      ctx : LoweringContext,
      func_def : CrystalV2::Compiler::Frontend::DefNode,
      call_args : Array(ValueId),
      block : CrystalV2::Compiler::Frontend::BlockNode
    ) : ValueId
      ctx.push_scope(ScopeKind::Block)

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
        result_value = lower_body_with_yield_substitution(ctx, body, block)
      end

      ctx.pop_scope
      result_value
    end

    # Lower body expressions, substituting yield with block body
    private def lower_body_with_yield_substitution(
      ctx : LoweringContext,
      body : Array(ExprId),
      block : CrystalV2::Compiler::Frontend::BlockNode
    ) : ValueId
      last_value = nil_value(ctx)
      body.each do |expr_id|
        last_value = lower_expr_with_yield_substitution(ctx, expr_id, block)
      end
      last_value
    end

    # Lower single expression, substituting yield
    private def lower_expr_with_yield_substitution(
      ctx : LoweringContext,
      expr_id : ExprId,
      block : CrystalV2::Compiler::Frontend::BlockNode
    ) : ValueId
      node = @arena[expr_id]

      case node
      when CrystalV2::Compiler::Frontend::YieldNode
        # YIELD SUBSTITUTION: Replace yield with block body
        inline_block_body(ctx, node, block)
      else
        # Regular lowering
        lower_node(ctx, node)
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

      # Bind block parameters to yield arguments
      if params = block.params
        params.each_with_index do |param, idx|
          if pname = param.name
            param_name = String.new(pname)
            if idx < yield_args.size
              ctx.register_local(param_name, yield_args[idx])
              # Propagate taints/lifetime for inline block params to support RC/ThreadShared decisions
              arg_id = yield_args[idx]
              ctx.register_type(arg_id, ctx.type_of(arg_id))
            end
          end
        end
      end

      # Lower block body
      result = lower_body(ctx, block.body)

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
          call = Call.new(ctx.next_id, TypeRef::POINTER, object_id, "[]", [start_id, end_id])
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

      # Check if this is an array type (which uses IndexGet for element access)
      is_array_type = type_desc && (type_desc.kind == TypeKind::Array ||
                                     type_desc.name.starts_with?("Array") ||
                                     type_desc.name.starts_with?("StaticArray"))

      if is_array_type && index_ids.size == 1
        # Array element access: arr[i] -> IndexGet
        element_type = object_type
        if element_type == TypeRef::VOID
          element_type = TypeRef::INT32  # Default for untyped arrays
        end

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
        # Resolve type alias if exists
        resolved_name = @type_aliases[name]? || name
        class_name_str = resolved_name
      elsif obj_node.is_a?(CrystalV2::Compiler::Frontend::IdentifierNode)
        name = String.new(obj_node.name)
        # Resolve type alias if exists
        resolved_name = @type_aliases[name]? || name
        if resolved_name[0].uppercase? && @class_info.has_key?(resolved_name)
          class_name_str = resolved_name
        end
      elsif obj_node.is_a?(CrystalV2::Compiler::Frontend::GenericNode)
        # Generic type like Hash(Int32, Int32).new
        base_node = @arena[obj_node.base_type]
        base_name = case base_node
                    when CrystalV2::Compiler::Frontend::ConstantNode
                      String.new(base_node.name)
                    when CrystalV2::Compiler::Frontend::IdentifierNode
                      String.new(base_node.name)
                    else
                      nil
                    end
        if base_name
          # Extract type argument names, substituting type parameters
          type_args = obj_node.type_args.map do |arg_id|
            arg_node = @arena[arg_id]
            arg_name = case arg_node
                       when CrystalV2::Compiler::Frontend::ConstantNode
                         String.new(arg_node.name)
                       when CrystalV2::Compiler::Frontend::IdentifierNode
                         String.new(arg_node.name)
                       else
                         "Unknown"
                       end
            # Substitute type parameter if applicable
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
        return_type = @function_types[full_method_name]? || TypeRef::VOID
        # For .new, use class type_ref as return type
        if member_name == "new" && return_type == TypeRef::VOID
          if class_info = @class_info[class_name_str]?
            return_type = class_info.type_ref
          end
        end
        call = Call.new(ctx.next_id, return_type, nil, full_method_name, [] of ValueId)
        ctx.emit(call)
        ctx.register_type(call.id, return_type)
        return call.id
      end

      # Otherwise it's an instance method call - evaluate object first
      object_id = lower_expr(ctx, node.object)

      # Check for pointer.value -> PointerLoad
      receiver_type = ctx.type_of(object_id)
      if receiver_type == TypeRef::POINTER && member_name == "value"
        deref_type = TypeRef::INT32  # TODO: track actual element type
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
              if type = @function_types[base_method]?
                resolved_method_name = base_method
                return_type = type
              end
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
              if type = @function_types[base_method]?
                resolved_method_name = base_method
                return_type = type
              end
            end
          else
            # Try to find a class that ends with the type name (handle namespacing)
            # e.g., type_name="Span" matches "CrystalV2::Compiler::Frontend::Span"
            @class_info.each do |class_name, info|
              if class_name.ends_with?("::#{type_name}") || class_name == type_name
                if base_method = resolve_method_with_inheritance(class_name, member_name)
                  if type = @function_types[base_method]?
                    resolved_method_name = base_method
                    return_type = type
                    break
                  end
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
          if type = @function_types[test_name]?
            resolved_method_name = test_name
            return_type = type
            break
          end
        end
      end

      actual_name = resolved_method_name || member_name

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
                                             "first", "last", "dup", "clone", "cover", "tap",
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

      call = Call.new(ctx.next_id, return_type, object_id, actual_name, [] of ValueId)
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
          store_node = PointerStore.new(ctx.next_id, TypeRef::VOID, object_id, value_id, index_ids.first)
          ctx.emit(store_node)
          return store_node.id
        end

        # Check if this is an array type (which uses IndexSet for element assignment)
        is_array_type = type_desc && (type_desc.kind == TypeKind::Array ||
                                       type_desc.name.starts_with?("Array") ||
                                       type_desc.name.starts_with?("StaticArray"))

        if is_array_type && index_ids.size == 1
          # Array element assignment: arr[i] = val -> IndexSet
          index_set = IndexSet.new(ctx.next_id, TypeRef::VOID, object_id, index_ids.first, value_id)
          ctx.emit(index_set)
          index_set.id
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

    private def lower_block(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::BlockNode) : ValueId
      block_id = lower_block_to_block_id(ctx, node)

      # Create MakeClosure
      # For now, capture analysis is deferred to escape analysis phase
      captures = [] of CapturedVar  # Will be filled by escape analysis

      closure = MakeClosure.new(ctx.next_id, TypeRef::VOID, block_id, captures)
      ctx.emit(closure)
      closure.id
    end

    private def lower_block_to_block_id(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::BlockNode) : BlockId
      body_block = ctx.create_block
      saved_block = ctx.current_block
      # Save locals before lowering block body - block-local vars shouldn't leak
      saved_locals = ctx.save_locals

      ctx.current_block = body_block
      ctx.push_scope(ScopeKind::Closure)

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
          end
        end
      end

      # Lower body
      last_value = lower_body(ctx, node.body)
      ctx.pop_scope

      # Implicit return from block
      if ctx.get_block(ctx.current_block).terminator.is_a?(Unreachable)
        ctx.terminate(Return.new(last_value))
      end

      ctx.current_block = saved_block
      # Restore locals - block-local vars shouldn't pollute outer scope
      ctx.restore_locals(saved_locals)
      body_block
    end

    private def lower_proc_literal(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::ProcLiteralNode) : ValueId
      body_block = ctx.create_block
      saved_block = ctx.current_block

      ctx.current_block = body_block
      ctx.push_scope(ScopeKind::Closure)

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

      # Create MakeClosure
      captures = [] of CapturedVar
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

      # Determine element type from first element (or Int32 default)
      element_type = if element_ids.size > 0
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
        full_method = "#{class_name}.#{method_name}"
        result = @function_types.keys.any? { |k| k.starts_with?(full_method) }
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
      # Check for union type syntax: "Type1 | Type2" or "Type1|Type2" (parser may not add spaces)
      if name.includes?("|")
        return create_union_type(name)
      end

      # Handle nullable type suffix: "T?" means "T | Nil"
      if name.ends_with?("?")
        base_name = name[0, name.size - 1]
        # Substitute type parameter if present
        base_name = @type_param_map[base_name]? || base_name
        return create_union_type("#{base_name} | Nil")
      end

      # Check if this is a type parameter that should be substituted
      if substitution = @type_param_map[name]?
        return type_ref_for_name(substitution)
      end

      # Check if this is a type alias (but not self-referencing)
      if alias_target = @type_aliases[name]?
        if alias_target != name
          return type_ref_for_name(alias_target)
        end
      end

      # Handle generic types like Pointer(K), Array(T) - substitute type parameters
      if name.includes?("(") && name.includes?(")")
        # Parse generic: "Pointer(K)" -> base="Pointer", params=["K"]
        paren_start = name.index('(').not_nil!
        base_name = name[0, paren_start]
        params_str = name[paren_start + 1, name.size - paren_start - 2]

        # Substitute each type parameter
        substituted_params = params_str.split(",").map do |param|
          param = param.strip
          @type_param_map[param]? || param
        end

        # Reconstruct with substituted params
        substituted_name = "#{base_name}(#{substituted_params.join(", ")})"
        if substituted_name != name
          # Types changed - recurse with new name
          return type_ref_for_name(substituted_name)
        end
      end

      case name
      when "Void", "Nil"    then TypeRef::VOID
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
      # Abstract/stdlib types that should be treated as pointers
      when "IO"             then TypeRef::POINTER
      when "Object"         then TypeRef::POINTER
      when "Reference"      then TypeRef::POINTER
      when "Exception"      then TypeRef::POINTER
      when "Enumerable"     then TypeRef::POINTER
      when "Indexable"      then TypeRef::POINTER
      when "Comparable"     then TypeRef::POINTER
      when "Iterable"       then TypeRef::POINTER
      else
        # Check if this is an enum type - enums are stored as Int32
        if enum_info = @enum_info
          if enum_info.has_key?(name)
            return TypeRef::INT32
          end
        end
        @module.intern_type(TypeDescriptor.new(TypeKind::Class, name))
      end
    end

    # Create a union type from "Type1 | Type2 | Type3" syntax
    private def create_union_type(name : String) : TypeRef
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
  end
end
