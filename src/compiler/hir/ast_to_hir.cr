# AST to HIR Lowering
#
# Converts Crystal AST (from parser) to High-Level IR for analysis.
# This is the first stage of the codegen pipeline.
#
# See docs/codegen_architecture.md for full specification.

require "./hir"
require "../frontend/ast"

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

    def initialize(@function : Function, @module : Module, @arena)
      @current_block = @function.entry_block
      @locals = {} of String => ValueId
      @scope_stack = [@function.scopes[0].id]  # Function scope
      @type_cache = {} of String => TypeRef
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

    # Add instruction to current block
    def emit(value : Value) : Value
      get_block(@current_block).add(value)
      value
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

  # Main AST to HIR converter
  class AstToHir
    alias AstNode = CrystalV2::Compiler::Frontend::Node
    alias ExprId = CrystalV2::Compiler::Frontend::ExprId

    getter module : Module
    @arena : CrystalV2::Compiler::Frontend::ArenaLike

    def initialize(@arena, module_name : String = "main")
      @module = Module.new(module_name)
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
        lower_expr(ctx, node.expression)

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

      value_str = String.new(node.value)
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
        copy = Copy.new(ctx.next_id, TypeRef::VOID, local_id)  # Type will be refined
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
      # Instance var access is a field get on self
      self_id = emit_self(ctx)
      field_get = FieldGet.new(ctx.next_id, TypeRef::VOID, self_id, name)
      ctx.emit(field_get)
      field_get.id
    end

    private def lower_class_var(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::ClassVarNode) : ValueId
      name = String.new(node.name)
      class_var_get = ClassVarGet.new(ctx.next_id, TypeRef::VOID, "", name)  # class_name filled later
      ctx.emit(class_var_get)
      class_var_get.id
    end

    private def lower_self(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::SelfNode) : ValueId
      emit_self(ctx)
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

    # ═══════════════════════════════════════════════════════════════════════
    # BINARY/UNARY OPERATIONS
    # ═══════════════════════════════════════════════════════════════════════

    private def lower_binary(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::BinaryNode) : ValueId
      left_id = lower_expr(ctx, node.left)
      right_id = lower_expr(ctx, node.right)

      op = case node.operator_string
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
             return emit_binary_call(ctx, left_id, node.operator_string, right_id)
           end

      result_type = if op.eq? || op.ne? || op.lt? || op.le? || op.gt? || op.ge? || op.and? || op.or?
                      TypeRef::BOOL
                    else
                      TypeRef::VOID  # Will be refined by type inference
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

      # Branch on condition
      ctx.terminate(Branch.new(cond_id, then_block, else_block))

      # Then branch
      ctx.current_block = then_block
      ctx.push_scope(ScopeKind::Block)
      then_value = lower_body(ctx, node.then_body)
      then_exit_block = ctx.current_block
      ctx.pop_scope

      # Only jump if block isn't already terminated
      if ctx.get_block(ctx.current_block).terminator.is_a?(Unreachable)
        ctx.terminate(Jump.new(merge_block))
      end

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
      ctx.pop_scope

      if ctx.get_block(ctx.current_block).terminator.is_a?(Unreachable)
        ctx.terminate(Jump.new(merge_block))
      end

      # Merge block with phi
      ctx.current_block = merge_block
      phi = Phi.new(ctx.next_id, TypeRef::VOID)
      phi.add_incoming(then_exit_block, then_value)
      phi.add_incoming(else_exit_block, else_value)
      ctx.emit(phi)
      phi.id
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
      phi = Phi.new(ctx.next_id, TypeRef::VOID)
      phi.add_incoming(then_exit, then_value)
      phi.add_incoming(else_exit, else_value)
      ctx.emit(phi)
      phi.id
    end

    private def lower_while(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::WhileNode) : ValueId
      cond_block = ctx.create_block
      body_block = ctx.create_block
      exit_block = ctx.create_block

      # Jump to condition check
      ctx.terminate(Jump.new(cond_block))

      # Condition block
      ctx.current_block = cond_block
      cond_id = lower_expr(ctx, node.condition)
      ctx.terminate(Branch.new(cond_id, body_block, exit_block))

      # Body block
      ctx.current_block = body_block
      ctx.push_scope(ScopeKind::Loop)
      lower_body(ctx, node.body)
      ctx.pop_scope

      if ctx.get_block(ctx.current_block).terminator.is_a?(Unreachable)
        ctx.terminate(Jump.new(cond_block))  # Loop back
      end

      # Exit block
      ctx.current_block = exit_block
      nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
      ctx.emit(nil_lit)
      nil_lit.id
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
      phi = Phi.new(ctx.next_id, TypeRef::VOID)
      phi.add_incoming(then_exit, then_value)
      phi.add_incoming(else_exit, else_value)
      ctx.emit(phi)
      phi.id
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
          # Match subject against when values
          conds = when_branch.conditions.map do |cond_expr|
            cond_val = lower_expr(ctx, cond_expr)
            eq = BinaryOperation.new(ctx.next_id, TypeRef::BOOL, BinaryOp::Eq, subject_id.not_nil!, cond_val)
            ctx.emit(eq)
            eq.id
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
      phi = Phi.new(ctx.next_id, TypeRef::VOID)
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
      yield_val = Yield.new(ctx.next_id, TypeRef::VOID, args)
      ctx.emit(yield_val)
      yield_val.id
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
    # CALLS
    # ═══════════════════════════════════════════════════════════════════════

    private def lower_call(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::CallNode) : ValueId
      receiver_id = if recv = node.receiver
                      lower_expr(ctx, recv)
                    else
                      nil
                    end

      args = node.args.map { |arg| lower_expr(ctx, arg) }
      method_name = String.new(node.name)

      # Check for block
      block_id = if blk = node.block
                   lower_block_to_block_id(ctx, blk)
                 else
                   nil
                 end

      call = Call.new(ctx.next_id, TypeRef::VOID, receiver_id, method_name, args, block_id)
      ctx.emit(call)
      call.id
    end

    private def lower_index(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::IndexNode) : ValueId
      object_id = lower_expr(ctx, node.object)
      index_id = lower_expr(ctx, node.index)

      index_get = IndexGet.new(ctx.next_id, TypeRef::VOID, object_id, index_id)
      ctx.emit(index_get)
      index_get.id
    end

    private def lower_member_access(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::MemberAccessNode) : ValueId
      object_id = lower_expr(ctx, node.object)
      member_name = String.new(node.member)

      # Member access is a call without arguments
      call = Call.new(ctx.next_id, TypeRef::VOID, object_id, member_name, [] of ValueId)
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
        if existing = ctx.lookup_local(name)
          # Reassignment
          copy = Copy.new(ctx.next_id, TypeRef::VOID, value_id)
          ctx.emit(copy)
          ctx.register_local(name, copy.id)
          copy.id
        else
          # New variable
          local = Local.new(ctx.next_id, TypeRef::VOID, name, ctx.current_scope)
          ctx.emit(local)
          ctx.register_local(name, value_id)
          # Also emit copy
          copy = Copy.new(ctx.next_id, TypeRef::VOID, value_id)
          ctx.emit(copy)
          ctx.register_local(name, copy.id)
          copy.id
        end

      when CrystalV2::Compiler::Frontend::InstanceVarNode
        name = String.new(target_node.name)
        self_id = emit_self(ctx)
        field_set = FieldSet.new(ctx.next_id, TypeRef::VOID, self_id, name, value_id)
        ctx.emit(field_set)
        field_set.id

      when CrystalV2::Compiler::Frontend::ClassVarNode
        name = String.new(target_node.name)
        class_var_set = ClassVarSet.new(ctx.next_id, TypeRef::VOID, "", name, value_id)
        ctx.emit(class_var_set)
        class_var_set.id

      when CrystalV2::Compiler::Frontend::IndexNode
        object_id = lower_expr(ctx, target_node.object)
        index_id = lower_expr(ctx, target_node.index)
        index_set = IndexSet.new(ctx.next_id, TypeRef::VOID, object_id, index_id, value_id)
        ctx.emit(index_set)
        index_set.id

      else
        raise LoweringError.new("Unsupported assignment target: #{target_node.class}", target_node)
      end
    end

    private def lower_multiple_assign(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::MultipleAssignNode) : ValueId
      # Lower all values first
      values = node.values.map { |v| lower_expr(ctx, v) }

      # Assign to each target
      node.targets.each_with_index do |target_expr, idx|
        target_node = @arena[target_expr]
        value_id = values[idx]? || values.last  # Splat handling

        case target_node
        when CrystalV2::Compiler::Frontend::IdentifierNode
          name = String.new(target_node.name)
          local = Local.new(ctx.next_id, TypeRef::VOID, name, ctx.current_scope)
          ctx.emit(local)
          ctx.register_local(name, value_id)
        else
          # Handle other target types
        end
      end

      # Return last value
      values.last? || begin
        nil_lit = Literal.new(ctx.next_id, TypeRef::NIL, nil)
        ctx.emit(nil_lit)
        nil_lit.id
      end
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

      # Add block parameters
      node.params.each do |param|
        if param_name = param.name
          name = String.new(param_name)
          param_val = Parameter.new(ctx.next_id, TypeRef::VOID, 0, name)
          ctx.emit(param_val)
          ctx.register_local(name, param_val.id)
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

      # Add proc parameters
      node.params.each_with_index do |param, idx|
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

      # Allocate array
      array_type = ctx.get_type("Array")
      alloc = Allocate.new(ctx.next_id, array_type, element_ids)
      ctx.emit(alloc)
      alloc.id
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
      from_id = lower_expr(ctx, node.from)
      to_id = lower_expr(ctx, node.to)

      range_type = ctx.get_type("Range")
      alloc = Allocate.new(ctx.next_id, range_type, [from_id, to_id])
      ctx.emit(alloc)
      alloc.id
    end

    # ═══════════════════════════════════════════════════════════════════════
    # TYPE OPERATIONS
    # ═══════════════════════════════════════════════════════════════════════

    private def lower_as(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::AsNode) : ValueId
      value_id = lower_expr(ctx, node.expression)
      target_type = type_ref_for_expr(ctx, node.type_node)

      cast = Cast.new(ctx.next_id, target_type, value_id, target_type, safe: false)
      ctx.emit(cast)
      cast.id
    end

    private def lower_as_question(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::AsQuestionNode) : ValueId
      value_id = lower_expr(ctx, node.expression)
      target_type = type_ref_for_expr(ctx, node.type_node)

      cast = Cast.new(ctx.next_id, target_type, value_id, target_type, safe: true)
      ctx.emit(cast)
      cast.id
    end

    private def lower_is_a(ctx : LoweringContext, node : CrystalV2::Compiler::Frontend::IsANode) : ValueId
      value_id = lower_expr(ctx, node.expression)
      check_type = type_ref_for_expr(ctx, node.type_node)

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

    private def type_ref_for_expr(ctx : LoweringContext, expr_id : ExprId) : TypeRef
      name = get_type_name(expr_id)
      type_ref_for_name(name)
    end
  end
end
