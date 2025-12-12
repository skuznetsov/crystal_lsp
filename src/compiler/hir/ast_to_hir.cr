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

    # Register a macro definition (pass 1)
    def register_macro(node : CrystalV2::Compiler::Frontend::MacroDefNode)
      macro_name = String.new(node.name)
      @macro_defs[macro_name] = node
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
    def register_module(node : CrystalV2::Compiler::Frontend::ModuleNode)
      module_name = String.new(node.name)

      # Register module methods (def self.foo)
      if body = node.body
        body.each do |expr_id|
          member = @arena[expr_id]
          if member.is_a?(CrystalV2::Compiler::Frontend::DefNode)
            method_name = String.new(member.name)
            # Module methods are always class methods: Module.method
            base_name = "#{module_name}.#{method_name}"
            return_type = if rt = member.return_type
                            type_ref_for_name(String.new(rt))
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
          end
        end
      end
    end

    # Lower a module's methods (pass 3)
    def lower_module(node : CrystalV2::Compiler::Frontend::ModuleNode)
      module_name = String.new(node.name)

      if body = node.body
        body.each do |expr_id|
          member = @arena[expr_id]
          if member.is_a?(CrystalV2::Compiler::Frontend::DefNode)
            lower_module_method(module_name, member)
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

      # Lower body
      last_value : ValueId? = nil
      if body = node.body
        body.each do |expr_id|
          last_value = lower_expr(ctx, expr_id)
        end
      end

      # Add implicit return
      block = ctx.get_block(ctx.current_block)
      if block.terminator.is_a?(Unreachable)
        block.terminator = Return.new(last_value)
      end
    end

    # Register a class type and its methods (pass 1)
    def register_class(node : CrystalV2::Compiler::Frontend::ClassNode)
      class_name = String.new(node.name)
      is_struct = node.is_struct == true

      # Check if this is a generic class (has type parameters)
      if type_params = node.type_params
        if type_params.size > 0
          # Store as generic template - don't create ClassInfo yet
          param_names = type_params.map { |p| String.new(p) }
          @generic_templates[class_name] = GenericClassTemplate.new(
            class_name, param_names, node, is_struct
          )
          return  # Don't register as concrete class
        end
      end

      # Non-generic class - proceed with normal registration
      register_concrete_class(node, class_name, is_struct)
    end

    # Register a concrete (non-generic or specialized) class
    private def register_concrete_class(node : CrystalV2::Compiler::Frontend::ClassNode, class_name : String, is_struct : Bool)
      # Collect instance variables and their types
      ivars = [] of IVarInfo
      class_vars = [] of ClassVarInfo
      # Struct has no type_id header (value type), class starts at 8 for header
      offset = is_struct ? 0 : 8

      # Inheritance: copy parent ivars first (to preserve layout)
      parent_name : String? = nil
      if super_name_slice = node.super_name
        parent_name = String.new(super_name_slice)
        if parent_info = @class_info[parent_name]?
          # Copy all ivars from parent, preserving their offsets
          parent_info.ivars.each do |parent_ivar|
            ivars << parent_ivar.dup
          end
          # Start child ivars after parent ivars
          offset = parent_info.size
        end
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
            if method_name == "initialize"
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
          end
        end
      end

      # Create class/struct type
      type_kind = is_struct ? TypeKind::Struct : TypeKind::Class
      type_ref = @module.intern_type(TypeDescriptor.new(type_kind, class_name))
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

      # Restore old type param map
      @type_param_map = old_map
      @monomorphized.add(specialized_name)
    end

    # Lower a class and all its methods (pass 3)
    def lower_class(node : CrystalV2::Compiler::Frontend::ClassNode)
      class_name = String.new(node.name)

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
          if member.is_a?(CrystalV2::Compiler::Frontend::DefNode)
            lower_method(class_name, class_info, member)
          end
        end
      end

      @current_class = nil
    end

    # Generate allocator: ClassName.new(...) -> allocates and returns instance
    private def generate_allocator(class_name : String, class_info : ClassInfo)
      func_name = "#{class_name}.new"
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
        if type_desc = @module.get_type_descriptor(ivar.type)
          if type_desc.kind == TypeKind::Union
            # Union types will be initialized in initialize()
            # We can't create a simple zero literal for unions
            next
          end
        end

        default_val = Literal.new(ctx.next_id, ivar.type, 0_i64)
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

    # Lower a method within a class
    private def lower_method(class_name : String, class_info : ClassInfo, node : CrystalV2::Compiler::Frontend::DefNode)
      method_name = String.new(node.name)
      base_name = "#{class_name}##{method_name}"

      # Track current method for super calls
      old_method = @current_method
      @current_method = method_name

      return_type = if rt = node.return_type
                      type_ref_for_name(String.new(rt))
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
      block = ctx.get_block(ctx.current_block)
      if block.terminator.is_a?(Unreachable)
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
    # Example: "IO.print" + [String] -> "IO.print:String"
    # Example: "Int32#++" + [Int32] -> "Int32#+:Int32"
    private def mangle_function_name(base_name : String, param_types : Array(TypeRef)) : String
      return base_name if param_types.empty?
      type_suffix = param_types.map { |t| type_name_for_mangling(t) }.join(",")
      "#{base_name}:#{type_suffix}"
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
      block = ctx.get_block(ctx.current_block)
      if block.terminator.is_a?(Unreachable)
        block.terminator = Return.new(last_value)
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
                value_str.to_i64
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

      # Check if it's a local variable
      if local_id = ctx.lookup_local(name)
        # Return a copy/reference to the local
        copy = Copy.new(ctx.next_id, ctx.type_of(local_id), local_id)
        ctx.emit(copy)
        return copy.id
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

      # Find the method in parent class (not using inheritance - we want specifically parent's version)
      super_method_name = "#{parent_name}##{method_name}"

      # Get return type
      return_type = @function_types[super_method_name]? || TypeRef::VOID

      # Lower arguments if provided
      args = if node_args = node.args
               node_args.map { |arg| lower_expr(ctx, arg) }
             else
               # If no args, forward current method's parameters
               # Get them from the function context (skip 'self' param at index 0)
               ctx.function.params[1..].map(&.id)
             end

      # Get self for the call
      self_id = emit_self(ctx)

      # Call parent method
      call = Call.new(ctx.next_id, return_type, self_id, super_method_name, args)
      ctx.emit(call)
      call.id
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

      # Extract type arguments
      type_args = node.type_args.map do |arg_id|
        arg_node = @arena[arg_id]
        case arg_node
        when CrystalV2::Compiler::Frontend::ConstantNode
          String.new(arg_node.name)
        when CrystalV2::Compiler::Frontend::IdentifierNode
          String.new(arg_node.name)
        else
          "Unknown"
        end
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

      op = case op_str
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

      result_type = op.not? ? TypeRef::BOOL : TypeRef::VOID
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

      # Only jump if block isn't already terminated (e.g., by return)
      then_flows_to_merge = ctx.get_block(ctx.current_block).terminator.is_a?(Unreachable)
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

      else_flows_to_merge = ctx.get_block(ctx.current_block).terminator.is_a?(Unreachable)
      if else_flows_to_merge
        ctx.terminate(Jump.new(merge_block))
      end

      # Merge block with phi for the if expression value
      ctx.current_block = merge_block

      # Only create phi if at least one branch flows to merge
      if then_flows_to_merge || else_flows_to_merge
        if then_flows_to_merge && else_flows_to_merge
          # Both branches flow to merge - need phi (unless void type)
          phi_type = ctx.type_of(then_value)

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
          phi.add_incoming(then_exit_block, then_value)
          phi.add_incoming(else_exit_block, else_value)
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
      # Find all variables that exist in either branch
      all_vars = (then_locals.keys + else_locals.keys).uniq

      all_vars.each do |var_name|
        then_val = then_locals[var_name]?
        else_val = else_locals[var_name]?
        pre_val = pre_locals[var_name]?

        # Skip if variable didn't exist before and doesn't exist in both branches
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

    private def lower_unless(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::UnlessNode) : ValueId
      # Unless is just if with inverted condition
      cond_id = lower_expr(ctx, node.condition)

      # Negate condition
      neg_cond = UnaryOperation.new(ctx.next_id, TypeRef::BOOL, UnaryOp::Not, cond_id)
      ctx.emit(neg_cond)

      then_block = ctx.create_block
      else_block = ctx.create_block
      merge_block = ctx.create_block

      ctx.terminate(Branch.new(neg_cond.id, then_block, else_block))

      # Then (was unless body)
      ctx.current_block = then_block
      ctx.push_scope(ScopeKind::Block)
      then_value = lower_body(ctx, node.then_branch)
      then_exit = ctx.current_block
      ctx.pop_scope

      if ctx.get_block(ctx.current_block).terminator.is_a?(Unreachable)
        ctx.terminate(Jump.new(merge_block))
      end

      # Else branch (if any)
      ctx.current_block = else_block
      else_value = if else_branch = node.else_branch
                     lower_body(ctx, else_branch)
                   else
                     nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
                     ctx.emit(nil_lit)
                     nil_lit.id
                   end
      else_exit = ctx.current_block
      ctx.terminate(Jump.new(merge_block))

      # Merge
      ctx.current_block = merge_block
      phi_type = ctx.type_of(then_value)  # Infer type from incoming value

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
      phi.id
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

      when CrystalV2::Compiler::Frontend::IfNode
        # Check all branches
        collect_assigned_vars(node.then_body).each { |v| vars << v }
        if else_body = node.else_body
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

      then_block = ctx.create_block
      else_block = ctx.create_block
      merge_block = ctx.create_block

      ctx.terminate(Branch.new(cond_id, then_block, else_block))

      ctx.current_block = then_block
      then_value = lower_expr(ctx, node.true_branch)
      then_exit = ctx.current_block
      ctx.terminate(Jump.new(merge_block))

      ctx.current_block = else_block
      else_value = lower_expr(ctx, node.false_branch)
      else_exit = ctx.current_block
      ctx.terminate(Jump.new(merge_block))

      ctx.current_block = merge_block
      phi_type = ctx.type_of(then_value)  # Infer type from incoming value

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
      phi.id
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
      # Lower case subject
      subject_id = if subj = node.value
                     lower_expr(ctx, subj)
                   else
                     nil
                   end

      merge_block = ctx.create_block
      incoming = [] of Tuple(BlockId, ValueId)

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

        # When body
        ctx.current_block = when_block
        ctx.push_scope(ScopeKind::Block)
        result = lower_body(ctx, when_branch.body)
        exit_block = ctx.current_block
        ctx.pop_scope

        if ctx.get_block(ctx.current_block).terminator.is_a?(Unreachable)
          ctx.terminate(Jump.new(merge_block))
        end
        incoming << {exit_block, result}

        ctx.current_block = next_block
      end

      # Else branch
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
      ctx.terminate(Jump.new(merge_block))
      incoming << {else_exit, else_result}

      # Merge
      ctx.current_block = merge_block
      phi_type = incoming.first?.try { |(_, val)| ctx.type_of(val) } || TypeRef::VOID

      # Don't create phi for void types - LLVM doesn't allow phi void
      if phi_type == TypeRef::VOID || phi_type == TypeRef::NIL
        nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
        ctx.emit(nil_lit)
        return nil_lit.id
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
      # For now, we implement a simplified version using control flow:
      # 1. Execute body
      # 2. If raise was called, jump to rescue block
      # 3. Execute ensure block (always)
      # 4. Return result

      # Create blocks
      body_block = ctx.create_block
      rescue_block = ctx.create_block if node.rescue_clauses
      else_block = ctx.create_block if node.else_body
      ensure_block = ctx.create_block if node.ensure_body
      exit_block = ctx.create_block

      # Jump to body
      ctx.terminate(Jump.new(body_block))
      ctx.switch_to_block(body_block)

      # Lower body
      result_id : ValueId = ctx.next_id
      node.body.each do |expr_id|
        result_id = lower_expr(ctx, expr_id)
      end

      # After body, jump to else (if exists) or ensure (if exists) or exit
      after_body_target = else_block || ensure_block || exit_block
      ctx.terminate(Jump.new(after_body_target))

      # Lower rescue clauses if any
      if rescue_clauses = node.rescue_clauses
        ctx.switch_to_block(rescue_block.not_nil!)

        # For now, just execute the first rescue clause's body
        # TODO: proper exception type matching
        rescue_result : ValueId = ctx.next_id
        rescue_clauses.each_with_index do |clause, idx|
          # If clause has variable name, create local for exception
          if var_name = clause.variable_name
            exc_var = Local.new(ctx.next_id, TypeRef::VOID, String.new(var_name), ctx.current_scope, true)
            ctx.emit(exc_var)
            # Get exception value
            get_exc = GetException.new(ctx.next_id, TypeRef::VOID)
            ctx.emit(get_exc)
            # Copy to variable
            copy = Copy.new(ctx.next_id, TypeRef::VOID, get_exc.id)
            ctx.emit(copy)
          end

          # Lower rescue body
          clause.body.each do |expr_id|
            rescue_result = lower_expr(ctx, expr_id)
          end

          # Only handle first clause for now
          break
        end

        # After rescue, jump to ensure or exit
        after_rescue_target = ensure_block || exit_block
        ctx.terminate(Jump.new(after_rescue_target))
      end

      # Lower else block if any
      if else_body = node.else_body
        ctx.switch_to_block(else_block.not_nil!)
        else_body.each do |expr_id|
          result_id = lower_expr(ctx, expr_id)
        end
        after_else_target = ensure_block || exit_block
        ctx.terminate(Jump.new(after_else_target))
      end

      # Lower ensure block if any
      if ensure_body = node.ensure_body
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
        receiver_id = nil

        # Check if this is a macro call - expand inline instead of generating Call
        if macro_def = @macro_defs[method_name]?
          return expand_macro(ctx, macro_def, node.args)
        end

      when CrystalV2::Compiler::Frontend::MemberAccessNode
        # Could be method call: obj.method() or class method: ClassName.new()
        obj_node = @arena[callee_node.object]
        method_name = String.new(callee_node.member)

        # Check if it's a class/module method call (ClassName.new() or Module.method())
        # Can be ConstantNode, IdentifierNode starting with uppercase, or GenericNode
        class_name_str : String? = nil
        if obj_node.is_a?(CrystalV2::Compiler::Frontend::ConstantNode)
          class_name_str = String.new(obj_node.name)
        elsif obj_node.is_a?(CrystalV2::Compiler::Frontend::IdentifierNode)
          name = String.new(obj_node.name)
          # Check if it's a class name (starts with uppercase and is known class)
          # OR a module name (check if Module.method exists in function_types)
          if name[0].uppercase?
            if @class_info.has_key?(name)
              class_name_str = name
            elsif is_module_method?(name, method_name)
              # It's a module method call
              class_name_str = name
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
            # Extract type argument names
            type_args = obj_node.type_args.map do |arg_id|
              arg_node = @arena[arg_id]
              case arg_node
              when CrystalV2::Compiler::Frontend::ConstantNode
                String.new(arg_node.name)
              when CrystalV2::Compiler::Frontend::IdentifierNode
                String.new(arg_node.name)
              else
                "Unknown"
              end
            end

            # Create specialized class name like Box(Int32)
            class_name_str = "#{base_name}(#{type_args.join(", ")})"

            # Monomorphize generic class if not already done
            if !@monomorphized.includes?(class_name_str)
              monomorphize_generic_class(base_name, type_args, class_name_str)
            end
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

      # Handle yield-functions with inline expansion
      if blk_expr = node.block
        blk_node = @arena[blk_expr]
        if blk_node.is_a?(CrystalV2::Compiler::Frontend::BlockNode)
          # Check if this is a call to a yield-function
          if @yield_functions.includes?(method_name)
            if func_def = @function_defs[method_name]?
              return inline_yield_function(ctx, func_def, args, blk_node)
            end
          end
        end
      end

      # Check for block (ExprId -> must lower to BlockNode) - fallback for non-inline
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

      # Collect argument types for name mangling (overloading support)
      arg_types = args.map { |arg_id| ctx.type_of(arg_id) }

      # Compute mangled name based on base name + argument types
      base_method_name = full_method_name || method_name
      mangled_method_name = mangle_function_name(base_method_name, arg_types)

      # Try to infer return type using mangled name first, fallback to base name
      return_type = get_function_return_type(mangled_method_name)
      if return_type == TypeRef::VOID && mangled_method_name != base_method_name
        # Try unmangled name as fallback (for non-overloaded functions)
        return_type = get_function_return_type(base_method_name)
        if return_type != TypeRef::VOID
          # Function exists without mangling - use unmangled name
          mangled_method_name = base_method_name
        end
      end

      # If still not found and receiver_id is set, try to find method in any class
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

      call = Call.new(ctx.next_id, return_type, receiver_id, mangled_method_name, args, block_id)
      ctx.emit(call)
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
      ctx.register_local(param_name, index_get.id)

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
      ctx.register_local(param_name, index_get.id)

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
      # IndexNode has indexes (Array) - can be multi-dimensional like arr[1, 2]
      index_ids = node.indexes.map { |idx| lower_expr(ctx, idx) }

      # Check if this is pointer indexing (ptr[i])
      object_type = ctx.type_of(object_id)
      if object_type == TypeRef::POINTER && index_ids.size == 1
        # Pointer indexing: ptr[i] -> PointerLoad with index
        # TODO: track actual element type from Pointer(T)
        deref_type = TypeRef::INT32
        load_node = PointerLoad.new(ctx.next_id, deref_type, object_id, index_ids.first)
        ctx.emit(load_node)
        ctx.register_type(load_node.id, deref_type)
        return load_node.id
      end

      # For single index, use IndexGet directly
      # For multiple indexes, chain them or use as tuple
      if index_ids.size == 1
        # Get element type from array if tracked
        element_type = object_type
        if element_type == TypeRef::VOID
          element_type = TypeRef::INT32  # Default for untyped arrays
        end

        index_get = IndexGet.new(ctx.next_id, element_type, object_id, index_ids.first)
        ctx.emit(index_get)
        ctx.register_type(index_get.id, element_type)
        index_get.id
      else
        # Multi-dimensional: emit as call to []
        call = Call.new(ctx.next_id, TypeRef::VOID, object_id, "[]", index_ids)
        ctx.emit(call)
        call.id
      end
    end

    private def lower_member_access(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::MemberAccessNode) : ValueId
      object_id = lower_expr(ctx, node.object)
      member_name = String.new(node.member)

      # Check for pointer.value -> PointerLoad
      receiver_type = ctx.type_of(object_id)
      if receiver_type == TypeRef::POINTER && member_name == "value"
        deref_type = TypeRef::INT32  # TODO: track actual element type
        load_node = PointerLoad.new(ctx.next_id, deref_type, object_id, nil)
        ctx.emit(load_node)
        ctx.register_type(load_node.id, deref_type)
        return load_node.id
      end

      # Determine receiver type to find the correct method
      full_method_name : String? = nil
      return_type = TypeRef::VOID

      # Try to find method by receiver type
      if receiver_type.id > 0
        @class_info.each do |class_name, info|
          if info.type_ref.id == receiver_type.id
            test_name = "#{class_name}##{member_name}"
            if type = @function_types[test_name]?
              full_method_name = test_name
              return_type = type
            end
            break
          end
        end
      end

      # Fallback: search all classes for this method
      if full_method_name.nil?
        @class_info.each do |class_name, info|
          test_name = "#{class_name}##{member_name}"
          if type = @function_types[test_name]?
            full_method_name = test_name
            return_type = type
            break
          end
        end
      end

      actual_name = full_method_name || member_name
      call = Call.new(ctx.next_id, return_type, object_id, actual_name, [] of ValueId)
      ctx.emit(call)
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
        if object_type == TypeRef::POINTER && index_ids.size == 1
          # Pointer store: ptr[i] = val -> PointerStore with index
          store_node = PointerStore.new(ctx.next_id, TypeRef::VOID, object_id, value_id, index_ids.first)
          ctx.emit(store_node)
          return store_node.id
        end

        if index_ids.size == 1
          index_set = IndexSet.new(ctx.next_id, TypeRef::VOID, object_id, index_ids.first, value_id)
          ctx.emit(index_set)
          index_set.id
        else
          # Multi-index assignment: emit as call to []=
          call = Call.new(ctx.next_id, TypeRef::VOID, object_id, "[]=", index_ids + [value_id])
          ctx.emit(call)
          call.id
        end

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

      ctx.current_block = body_block
      ctx.push_scope(ScopeKind::Closure)

      # Add block parameters (params can be nil)
      if params = node.params
        params.each_with_index do |param, idx|
          if param_name = param.name
            name = String.new(param_name)
            param_val = Parameter.new(ctx.next_id, TypeRef::VOID, idx, name)
            ctx.emit(param_val)
            ctx.register_local(name, param_val.id)
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
      ctx.register_type(arr.id, element_type)  # element type for .each
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

      # Check if this is a type parameter that should be substituted
      if substitution = @type_param_map[name]?
        return type_ref_for_name(substitution)
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
      else
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
