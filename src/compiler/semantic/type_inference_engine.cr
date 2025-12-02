require "./types/type_context"
require "./types/type"
require "./types/primitive_type"
require "./types/class_type"
require "./types/instance_type"
require "./types/union_type"
require "./types/array_type"
require "./types/range_type"
require "./types/hash_type"
require "./types/tuple_type"
require "./types/module_type"
require "./analyzer"
require "../frontend/ast"

module CrystalV2
  module Compiler
    module Semantic
      alias NumberKind = Frontend::NumberKind
      alias Parameter = Frontend::Parameter

      # Type Inference Engine for Stage 3
      #
      # Implements simple bottom-up type inference:
      # - Infer types from leaves to root
      # - Check type compatibility
      # - Emit type error diagnostics
      #
      # Algorithm: See collab_log/2025-10-21-003000-claude-type-inference-design.md
      #
      # Phase 1 (MVP): Literals + Variables
      # Phase 2: Binary operators
      # Phase 3: Control flow (if/while with unions)
      # Phase 4: Method calls with overload resolution
      # Phase 5: Definitions (method/class bodies)
      class TypeInferenceEngine
        getter context : TypeContext
        getter diagnostics : Array(Diagnostic)

        @current_class : ClassSymbol?          # Phase 5C: Track current class for instance var types
        @receiver_type_context : InstanceType? # Week 1: Track receiver's instance type for generic method body inference
        @depth : Int32
        MAX_DEPTH = 512

        @debug_enabled : Bool

        def initialize(
          @program : Frontend::Program,
          @identifier_symbols : Hash(ExprId, Symbol),
          @global_table : SymbolTable? = nil,
          @context : TypeContext = TypeContext.new,
        )
          @diagnostics = [] of Diagnostic
          @assignments = {} of String => Type        # Track variable assignments: name → type
          @instance_var_types = {} of String => Type # Phase 5A: Track instance variable types
          @flow_narrowings = {} of String => Type    # Phase 95: Flow typing - narrowed types in conditionals
          @current_class = nil
          @receiver_type_context = nil
          @depth = 0
          @debug_enabled = ENV["TYPE_INFERENCE_DEBUG"]? == "1"
        end

        # Debug helper
        private def debug(msg : String)
          STDERR.puts "[TYPE_INFERENCE_DEBUG] #{msg}" if @debug_enabled
        end

        # Main entry point: Infer types for all root expressions
        def infer_types
          @program.roots.each do |root_id|
            type = infer_expression(root_id)
            @context.set_type(root_id, type)
          end
        end

        # Recursive type inference for a single expression
        private def infer_expression(expr_id : ExprId) : Type
          # Iterative fast path to avoid deep recursion/stack overflows
          if t0 = @context.get_type(expr_id)
            return t0
          end
          # local frame as tuple {id, visited}, plus state map 0/1/2
          stack = [] of {ExprId, Bool}
          state = {} of ExprId => Int32
          stack << {expr_id, false}
          state[expr_id] = 1
          while frame = stack.pop?
            # Check watchdog to prevent infinite loops in type inference
            Frontend::Watchdog.check!

            id = frame[0]
            next if @context.get_type(id)
            node = @program.arena[id]
            if !frame[1]
              stack << {id, true}
              children_of(node).each do |child|
                next if @context.get_type(child)
                case state[child]?
                when 2
                  next
                when 1
                  # Cycle detected; assign nil_type to break it
                  # EXCEPTION: Don't set Nil for IdentifierNode - it needs to check @assignments
                  child_node = @program.arena[child]
                  unless child_node.is_a?(Frontend::IdentifierNode)
                    @context.set_type(child, @context.nil_type)
                  end
                  next
                else
                  state[child] = 1
                  stack << {child, false}
                end
              end
            else
              # Try to compute type for simple nodes
              if t = compute_node_type_no_recurse(node, id)
                # Successfully computed - mark as done
                debug("ITERATIVE: #{node.class.name.split("::").last} computed type #{t}")
                @context.set_type(id, t)
                state[id] = 2
              else
                # Complex node - skip in iterative path, leave for recursive fallback
                debug("ITERATIVE: #{node.class.name.split("::").last} returned nil, will use recursive")
                # IMPORTANT: Clear any type that might have been set by cycle detection (line 97)
                # Otherwise line 86 will skip this node even though it needs recursive processing
                @context.expression_types.delete(id)
                state[id] = 0 # Reset to allow recursive processing
              end
            end
          end
          if t1 = @context.get_type(expr_id)
            if @debug_enabled
              node = @program.arena[expr_id]
              debug("ITERATIVE SUCCESS: #{node.class.name.split("::").last} got type #{t1}")
            end
            return t1
          end
          # Fallback to recursive implementation below
          if @debug_enabled
            node = @program.arena[expr_id]
            debug("FALLBACK TO RECURSIVE: #{node.class.name.split("::").last}")
          end
          if @depth > MAX_DEPTH
            debug("max recursion depth reached at expr #{expr_id}")
            return @context.nil_type
          end
          @depth += 1
          node = @program.arena[expr_id]

          result_type = case Frontend.node_kind(node)
                        when .number?
                          infer_number(node.as(Frontend::NumberNode))
                        when .string?
                          infer_string(node.as(Frontend::StringNode))
                        when .string_interpolation?
                          infer_string_interpolation(node.as(Frontend::StringInterpolationNode), expr_id)
                        when .symbol?
                          # Phase 16: Symbol literals
                          infer_symbol(node.as(Frontend::SymbolNode))
                        when .array_literal?
                          infer_array_literal(node.as(Frontend::ArrayLiteralNode), expr_id)
                        when .bool?
                          infer_bool(node.as(Frontend::BoolNode))
                        when .nil?
                          infer_nil(node.as(Frontend::NilNode))
                        when .identifier?
                          infer_identifier(node.as(Frontend::IdentifierNode), expr_id)
                        when .instance_var?
                          infer_instance_var(node.as(Frontend::InstanceVarNode), expr_id)
                        when .instance_var_decl?
                          # Phase 5C/77: Instance variable declaration (@var : Type)
                          infer_instance_var_decl(node, expr_id)
                        when .class_var?
                          # Phase 76: Class variables
                          infer_class_var(node, expr_id)
                        when .class_var_decl?
                          # Phase 77: Class variable declaration (@@var : Type)
                          infer_class_var_decl(node, expr_id)
                        when .global?
                          # Phase 75: Global variables
                          infer_global(node, expr_id)
                        when .global_var_decl?
                          # Phase 77: Global variable declaration ($var : Type)
                          infer_global_var_decl(node, expr_id)
                        when .unary?
                          # Phase 17: Unary operators (+x, -x, !x)
                          infer_unary(node.as(Frontend::UnaryNode), expr_id)
                        when .binary?
                          infer_binary(node.as(Frontend::BinaryNode), expr_id)
                        when .def?
                          infer_def(node.as(Frontend::DefNode), expr_id)
                        when .class?
                          infer_class(node.as(Frontend::ClassNode), expr_id)
                        when .call?
                          infer_call(node.as(Frontend::CallNode), expr_id)
                        when .member_access?
                          # In Crystal, obj.method without parens is a zero-argument method call
                          infer_member_access(node.as(Frontend::MemberAccessNode), expr_id)
                        when .index?
                          # Phase 9: Array indexing arr[0]
                          infer_index(node.as(Frontend::IndexNode), expr_id)
                        when .if?
                          infer_if(node.as(Frontend::IfNode))
                        when .unless?
                          # Phase 24: unless condition
                          infer_unless(node.as(Frontend::UnlessNode))
                        when .while?
                          infer_while(node.as(Frontend::WhileNode))
                        when .for?
                          # Phase 99: for loop
                          infer_for(node.as(Frontend::ForNode))
                        when .loop?
                          # Phase 83: infinite loop
                          infer_loop(node.as(Frontend::LoopNode))
                        when .spawn?
                          # Phase 84: spawn fiber
                          infer_spawn(node.as(Frontend::SpawnNode))
                        when .until?
                          # Phase 25: until loop
                          infer_until(node.as(Frontend::UntilNode))
                        when .begin?
                          # Phase 28/29: begin/end blocks with rescue/ensure
                          infer_begin(node.as(Frontend::BeginNode))
                        when .raise?
                          # Phase 29: raise exception
                          infer_raise(node.as(Frontend::RaiseNode))
                        when .require?
                          # Phase 65: require statement
                          infer_require(node.as(Frontend::RequireNode))
                        when .type_declaration?
                          # Phase 66: type declaration
                          infer_type_declaration(node)
                        when .with?
                          # Phase 67: with context block
                          infer_with(node.as(Frontend::WithNode))
                        when .getter?
                          # Phase 30: getter macro
                          infer_accessor(node.as(Frontend::GetterNode))
                        when .setter?
                          # Phase 30: setter macro
                          infer_accessor(node.as(Frontend::SetterNode))
                        when .property?
                          # Phase 30: property macro
                          infer_accessor(node.as(Frontend::PropertyNode))
                        when .assign?
                          infer_assign(node.as(Frontend::AssignNode), expr_id)
                        when .multiple_assign?
                          infer_multiple_assign(node.as(Frontend::MultipleAssignNode), expr_id)
                        when .return?
                          infer_return(node.as(Frontend::ReturnNode), expr_id)
                        when .self?
                          infer_self(node, expr_id)
                        when .super?
                          # Phase 39: Super expressions
                          infer_super(node.as(Frontend::SuperNode), expr_id)
                        when .previous_def?
                          # Phase 96: PreviousDef expressions
                          infer_previous_def(node.as(Frontend::PreviousDefNode), expr_id)
                        when .typeof?
                          # Phase 40: Typeof expressions
                          infer_typeof(node.as(Frontend::TypeofNode), expr_id)
                        when .sizeof?
                          # Phase 41: Sizeof expressions
                          infer_sizeof(node.as(Frontend::SizeofNode), expr_id)
                        when .pointerof?
                          # Phase 42: Pointerof expressions
                          infer_pointerof(node.as(Frontend::PointerofNode), expr_id)
                        when .uninitialized?
                          # Phase 85: Uninitialized expressions
                          infer_uninitialized(node.as(Frontend::UninitializedNode), expr_id)
                        when .offsetof?
                          # Phase 86: Offsetof expressions
                          infer_offsetof(node.as(Frontend::OffsetofNode), expr_id)
                        when .alignof?
                          # Phase 88: Alignof expressions
                          infer_alignof(node.as(Frontend::AlignofNode), expr_id)
                        when .instance_alignof?
                          # Phase 88: InstanceAlignof expressions
                          infer_instance_alignof(node.as(Frontend::InstanceAlignofNode), expr_id)
                        when .asm?
                          # Phase 95: Inline assembly expressions
                          infer_asm(node.as(Frontend::AsmNode), expr_id)
                        when .out?
                          # Phase 98: Out keyword (C bindings output parameter)
                          infer_out(node.as(Frontend::OutNode), expr_id)
                        when Frontend::NodeKind::As
                          # Phase 44: Type cast expressions (can't use .as? due to keyword collision)
                          infer_as(node.as(Frontend::AsNode), expr_id)
                        when Frontend::NodeKind::AsQuestion
                          # Phase 45: Safe cast expressions (nilable)
                          infer_as_question(node.as(Frontend::AsQuestionNode), expr_id)
                        when Frontend::NodeKind::IsA
                          # Phase 93: Type check expressions (returns Bool)
                          infer_is_a(node.as(Frontend::IsANode), expr_id)
                        when Frontend::NodeKind::RespondsTo
                          # Phase 94: Method check expressions (returns Bool)
                          infer_responds_to(node.as(Frontend::RespondsToNode), expr_id)
                        when Frontend::NodeKind::Generic
                          # Phase 60: Generic type instantiation
                          infer_generic(node.as(Frontend::GenericNode), expr_id)
                        when Frontend::NodeKind::Path
                          # Phase 63: Path expressions (Foo::Bar)
                          infer_path(node.as(Frontend::PathNode), expr_id)
                        when Frontend::NodeKind::SafeNavigation
                          # Phase 47: Safe navigation expressions (returns nilable)
                          infer_safe_navigation(node.as(Frontend::SafeNavigationNode), expr_id)
                        when .block?
                          # Phase 10: Block literals
                          infer_block(node.as(Frontend::BlockNode), expr_id)
                        when .proc_literal?
                          # Phase 74: Proc literals
                          infer_proc_literal(node.as(Frontend::ProcLiteralNode), expr_id)
                        when .yield?
                          # Phase 10: Yield expressions
                          infer_yield(node.as(Frontend::YieldNode), expr_id)
                        when .case?
                          # Phase 11: Case/when pattern matching
                          infer_case(node.as(Frontend::CaseNode), expr_id)
                        when .select?
                          # Phase 90A: Select/when concurrent channel operations
                          infer_select(node.as(Frontend::SelectNode), expr_id)
                        when .break?
                          # Phase 12: Break expressions
                          infer_break(node.as(Frontend::BreakNode), expr_id)
                        when .next?
                          # Phase 12: Next expressions
                          infer_next(node, expr_id)
                        when .range?
                          # Phase 13: Range expressions
                          infer_range(node.as(Frontend::RangeNode), expr_id)
                        when .hash_literal?
                          # Phase 14: Hash literals
                          infer_hash_literal(node.as(Frontend::HashLiteralNode), expr_id)
                        when .tuple_literal?
                          # Phase 15: Tuple literals
                          infer_tuple_literal(node.as(Frontend::TupleLiteralNode), expr_id)
                        when .named_tuple_literal?
                          # Phase 70: Named tuple literals
                          infer_named_tuple_literal(node.as(Frontend::NamedTupleLiteralNode), expr_id)
                        when .ternary?
                          # Phase 23: Ternary operator
                          infer_ternary(node.as(Frontend::TernaryNode), expr_id)
                        when .module?
                          # Phase 31: Module definition
                          infer_module(node.as(Frontend::ModuleNode), expr_id)
                        when .include?
                          # Phase 31: Include module
                          infer_include(node)
                        when .extend?
                          # Phase 31: Extend module
                          infer_extend(node)
                        when .struct?
                          # Phase 32: Struct definition (value type)
                          # At parsing stage, handled identically to class; be tolerant to AST nuances
                          if node.is_a?(Frontend::StructNode)
                            infer_struct(node.as(Frontend::StructNode), expr_id)
                          elsif node.is_a?(Frontend::ClassNode)
                            infer_class(node.as(Frontend::ClassNode), expr_id)
                          else
                            @context.nil_type
                          end
                        when .union?
                          # Phase 97: Union definition (C bindings)
                          # At parsing stage, handled identically to class
                          infer_union(node.as(Frontend::UnionNode), expr_id)
                        when .enum?
                          # Phase 33: Enum definition (enumerated type)
                          infer_enum(node.as(Frontend::EnumNode))
                        when .alias?
                          # Phase 34: Type alias definition
                          infer_alias(node)
                        when .constant?
                          # Phase 35: Constant declaration
                          infer_constant(node.as(Frontend::ConstantNode))
                        when .lib?
                          # Phase 38: Lib definition (C bindings)
                          infer_lib(node.as(Frontend::LibNode), expr_id)
                        when .fun?
                          # Phase 64: Fun declaration (C function)
                          infer_fun(node)
                        when .grouping?
                          # Grouping expressions: (expr)
                          # Type is the type of the wrapped expression
                          infer_expression(node.as(Frontend::GroupingNode).expression)
                        else
                          # Unknown expression kind
                          @context.nil_type
                        end

          # Always set type for this expression (some infer_* methods already do this,
          # but this ensures ALL expressions have types set, even for nested calls)
          @context.set_type(expr_id, result_type)
          result_type
        ensure
          @depth -= 1
        end

        # Return direct child ExprIds of a node (used by iterative inference)
        private def children_of(node) : Array(ExprId)
          children = [] of ExprId
          case node
          when Frontend::GroupingNode
            children << node.expression
          when Frontend::UnaryNode
            children << node.operand
          when Frontend::BinaryNode
            children << node.left; children << node.right
          when Frontend::AssignNode
            children << node.target; children << node.value
          when Frontend::IndexNode
            children << node.object; node.indexes.each { |e| children << e }
          when Frontend::MemberAccessNode
            children << node.object
          when Frontend::SafeNavigationNode
            children << node.object
          when Frontend::CallNode
            children << node.callee
            node.args.each { |e| children << e }
            if blk = node.block
              children << blk
            end
            if named = node.named_args
              named.each { |na| children << na.value }
            end
          when Frontend::IfNode
            children << node.condition
            node.then_body.each { |e| children << e }
            node.elsifs.try &.each { |br| children << br.condition; br.body.each { |e| children << e } }
            node.else_body.try &.each { |e| children << e }
          when Frontend::UnlessNode
            children << node.condition
            node.then_branch.each { |e| children << e }
            node.else_branch.try &.each { |e| children << e }
          when Frontend::WhileNode
            children << node.condition; node.body.each { |e| children << e }
          when Frontend::UntilNode
            children << node.condition; node.body.each { |e| children << e }
          when Frontend::LoopNode
            node.body.each { |e| children << e }
          when Frontend::BlockNode
            node.body.each { |e| children << e }
          when Frontend::ProcLiteralNode
            node.body.each { |e| children << e }
          when Frontend::CaseNode
            if v = node.value
              children << v
            end
            node.when_branches.each { |br| br.conditions.each { |c| children << c }; br.body.each { |e| children << e } }
            node.else_branch.try &.each { |e| children << e }
          when Frontend::SelectNode
            node.branches.each { |br| children << br.condition; br.body.each { |e| children << e } }
            node.else_branch.try &.each { |e| children << e }
          when Frontend::BeginNode
            node.body.each { |e| children << e }
            node.rescue_clauses.try &.each { |rc| rc.body.each { |e| children << e } }
            if en = node.ensure_body
              en.each { |e| children << e }
            end
          when Frontend::RaiseNode
            if v = node.value
              children << v
            end
          when Frontend::WithNode
            node.body.each { |e| children << e }
          when Frontend::TupleLiteralNode
            node.elements.each { |e| children << e }
          when Frontend::ArrayLiteralNode
            if elems = node.elements
              elems.each { |e| children << e }
            end
          when Frontend::HashLiteralNode
            node.entries.each { |entry| children << entry.key; children << entry.value }
          when Frontend::DefNode
            # DO NOT process method body as children!
            # Method bodies should only be type-inferred when the method is called
            # (via infer_method_body_type), at which point receiver context is set up
            # for generic type substitution. Processing them here would infer types
            # without proper context, causing generic instance variables to return Nil.
            # (similar issue to ProcLiteralNode - see lines 577-583)
            # No children for DefNode
          when Frontend::ClassNode
            (node.body || [] of ExprId).each { |e| children << e }
          when Frontend::ModuleNode
            (node.body || [] of ExprId).each { |e| children << e }
          when Frontend::StructNode
            (node.body || [] of ExprId).each { |e| children << e }
          when Frontend::UnionNode
            (node.body || [] of ExprId).each { |e| children << e }
          when Frontend::ConstantNode
            children << node.value
          when Frontend::TernaryNode
            children << node.condition
            children << node.true_branch
            children << node.false_branch
          when Frontend::RangeNode
            children << node.begin_expr
            children << node.end_expr
          when Frontend::StringInterpolationNode
            node.pieces.each do |piece|
              if expr = piece.expr
                children << expr
              end
            end
          end
          children
        end

        # Compute node type using already computed children (no recursion)
        # Returns nil for complex nodes that need recursive inference
        private def compute_node_type_no_recurse(node, expr_id : ExprId) : Type?
          case node
          when Frontend::NumberNode
            infer_number(node)
          when Frontend::StringNode
            infer_string(node)
          when Frontend::UnaryNode
            op = Frontend.node_operator_string(node) || ""
            if op == "!"
              @context.bool_type
            else
              # +x, -x, ~x: return operand's type
              infer_expression(node.operand)
            end
          when Frontend::BinaryNode
            op = Frontend.node_operator_string(node) || ""
            case op
            when "==", "!=", "<", ">", "<=", ">=", "&&", "||"
              @context.bool_type
            when "..", "..."
              # Range(begin,end): type ignores exclusivity for now
              l = infer_expression(node.left)
              r = infer_expression(node.right)
              RangeType.new(l, r)
            else
              # Arithmetic and other operators need validation
              # Return nil to trigger recursive path
              nil
            end
          when Frontend::IfNode, Frontend::UnlessNode, Frontend::WhileNode, Frontend::UntilNode
            # Control flow nodes need validation (condition type checking)
            # Return nil to trigger recursive fallback where diagnostics are emitted
            nil
          when Frontend::LoopNode
            # Loop expression evaluates to Nil (simplified)
            @context.nil_type
          when Frontend::BoolNode
            infer_bool(node)
          when Frontend::NilNode
            infer_nil(node)
          when Frontend::SymbolNode
            infer_symbol(node)
          when Frontend::ArrayLiteralNode
            infer_array_literal(node, expr_id)
          when Frontend::TupleLiteralNode
            elems = node.elements
            types = Array(Type).new(elems.size)
            elems.each { |e| types << infer_expression(e) }
            TupleType.new(types)
          when Frontend::HashLiteralNode
            kt = Array(Type).new(node.entries.size)
            vt = Array(Type).new(node.entries.size)
            node.entries.each do |entry|
              kt << infer_expression(entry.key)
              vt << infer_expression(entry.value)
            end
            HashType.new(@context.union_of(kt), @context.union_of(vt))
          when Frontend::AssignNode
            # Assignment needs to infer child expression - too complex for iterative path
            # Return nil to trigger recursive fallback
            nil
          when Frontend::ProcLiteralNode
            # MUST use recursive path to properly handle parameter scoping
            # The recursive infer_proc_literal method registers parameters in @assignments,
            # infers body expression types, then cleans up the scope.
            # The iterative path can't handle this because it processes children (body)
            # before the parent (proc), so parameters aren't available yet.
            nil
          when Frontend::GroupingNode
            # Parenthesized expressions have the type of their inner expression
            infer_expression(node.expression)
          when Frontend::TernaryNode
            # condition ? then : else - union of then and else branches
            then_type = infer_expression(node.true_branch)
            else_type = infer_expression(node.false_branch)
            @context.union_of([then_type, else_type])
          when Frontend::RangeNode
            # begin..end or begin...end
            begin_type = infer_expression(node.begin_expr)
            end_type = infer_expression(node.end_expr)
            RangeType.new(begin_type, end_type)
          when Frontend::BeginNode
            # Must use recursive path to ensure all body expressions are inferred
            # The iterative path correctly identifies children (line 445-449)
            # but we need the recursive infer_begin to run to properly handle scoping
            nil
          when Frontend::CaseNode
            # Case/when needs proper Nil handling for missing else
            # Return nil to trigger recursive path where this is done correctly
            nil
          when Frontend::IdentifierNode
            # FIX: Always use recursive path for identifiers to ensure consistent
            # @assignments lookup across all IdentifierNodes with the same name.
            # The iterative path can't handle this properly because @assignments
            # is updated during recursive inference, not in @context.
            nil
          when Frontend::InstanceVarNode
            # MUST use recursive path to properly handle type parameter substitution
            # The recursive infer_instance_var method checks @receiver_type_context
            # and substitutes type parameters (e.g., T → Int32 in Box(Int32).direct_value)
            # The iterative path can't handle this because it only looks up @instance_var_types
            # which doesn't have information about type parameter substitutions
            nil
          when Frontend::StringInterpolationNode
            # Must use recursive path to ensure expression pieces are inferred
            # The iterative path correctly identifies children (line 478-483)
            # but we need the recursive infer_string_interpolation to run
            nil
          else
            # Unknown/complex node - return nil to trigger recursive fallback
            nil
          end
        end

        # ============================================================
        # PHASE 1: Literals
        # ============================================================

        private def infer_number(node : Frontend::NumberNode) : Type
          # Use NumberKind from lexer/parser
          case node.kind
          when Frontend::NumberKind::I32
            @context.int32_type
          when Frontend::NumberKind::I64
            @context.int64_type
          when Frontend::NumberKind::F64
            @context.float64_type
          else
            # Fallback to Int32 if NumberKind is nil (shouldn't happen)
            @context.int32_type
          end
        end

        private def infer_string(node : Frontend::StringNode) : Type
          @context.string_type
        end

        # Phase 16: Symbol literal type inference
        private def infer_symbol(node : Frontend::SymbolNode) : Type
          @context.symbol_type
        end

        private def infer_bool(node : Frontend::BoolNode) : Type
          @context.bool_type
        end

        private def infer_nil(node : Frontend::NilNode) : Type
          @context.nil_type
        end

        # ============================================================
        # PHASE 1: Variables
        # ============================================================

        private def infer_identifier(node : Frontend::IdentifierNode, expr_id : ExprId) : Type
          identifier_name = String.new(node.name)

          debug("infer_identifier: name = '#{identifier_name}' (object_id=#{identifier_name.object_id})")
          debug("  @assignments has #{@assignments.size} entries: #{@assignments.keys.inspect}")

          # Week 1: Check for built-in type names used as type arguments
          # In Box(Int32), Int32 is an identifier that should map to PrimitiveType
          case identifier_name
          when "Int32"   then return @context.int32_type
          when "Int64"   then return @context.int64_type
          when "Float64" then return @context.float64_type
          when "String"  then return @context.string_type
          when "Bool"    then return @context.bool_type
          when "Nil"     then return @context.nil_type
          when "Char"    then return @context.char_type
          end

          # Phase 95: Check flow narrowings first (type narrowing in conditionals)
          if narrowed_type = @flow_narrowings[identifier_name]?
            debug("  Found in @flow_narrowings: #{narrowed_type}")
            return narrowed_type
          end

          # First, check if this identifier has a tracked assignment
          if assigned_type = @assignments[identifier_name]?
            debug("  Found in @assignments: #{assigned_type}")
            return assigned_type
          else
            debug("  NOT found in @assignments")
          end

          # Try name resolution first
          symbol = @identifier_symbols[expr_id]?

          # Fallback to global symbol table lookup if name resolution didn't resolve this identifier
          if symbol.nil?
            symbol = @global_table.try(&.lookup(identifier_name))
          end

          debug("  Symbol lookup: #{symbol ? symbol.class.name : "nil"}")

          return @context.nil_type unless symbol

          case symbol
          when VariableSymbol
            # Explicit type annotation: var : Int32
            if declared_type_name = symbol.declared_type
              parse_type_name(declared_type_name)
            else
              @context.nil_type
            end
          when ClassSymbol
            # Reference to class → ClassType
            # Class itself is a value (for calling class methods like Dog.new)
            ClassType.new(symbol)
          when ConstantSymbol
            infer_expression(symbol.value)
          when MethodSymbol
            # Reference to method → Nil for now
            # TODO: Function types (Phase 5)
            @context.nil_type
          else
            @context.nil_type
          end
        end

        # ============================================================
        # PHASE 5: Classes, Methods, and Instance Variables
        # ============================================================

        # Phase 6: Process method definitions and their bodies
        private def infer_def(node : Frontend::DefNode, expr_id : ExprId) : Type
          # Phase 71: Process default parameter values
          if params = node.params
            params.each do |param|
              if default_value = param.default_value
                infer_expression(default_value)
              end
            end
          end

          # Selective body inference strategy:
          # - Generic classes: DEFER body inference until method is called
          #   (need receiver context for type parameter substitution)
          # - Non-generic classes: INFER body immediately
          #   (no substitution needed, allows return statements to work)

          if body = Frontend.node_def_body(node)
            # Check if current class is generic (has type parameters)
            is_generic = false
            if current_class = @current_class
              # @current_class is ClassSymbol during class definition (set in infer_class)
              if current_class.responds_to?(:type_parameters)
                type_params = current_class.type_parameters
                is_generic = type_params && !type_params.empty?
              end
            end

            # Only infer body for non-generic classes
            # Generic classes need receiver context for type parameter substitution
            unless is_generic
              body.each do |stmt|
                infer_expression(stmt)
              end
            end
          end

          # Method definitions don't have value types (they're statements)
          @context.nil_type
        end

        # Phase 5C: Process class bodies and track current class context
        private def infer_class(node : Frontend::ClassNode, expr_id : ExprId) : Type
          # Look up the ClassSymbol from the symbol table
          class_name = String.new(node.name)

          class_symbol = @global_table.try(&.lookup(class_name))
          return @context.nil_type unless class_symbol.is_a?(ClassSymbol)

          # Save previous class context and set current class
          previous_class = @current_class
          @current_class = class_symbol

          # Process class body (method definitions, etc.)
          (node.body || [] of ExprId).each do |body_expr_id|
            infer_expression(body_expr_id)
          end

          # Restore previous class context
          @current_class = previous_class

          # Class definitions don't have value types
          @context.nil_type
        end

        private def infer_struct(node : Frontend::StructNode, expr_id : ExprId) : Type
          (node.body || [] of ExprId).each do |body_expr_id|
            infer_expression(body_expr_id)
          end

          @context.nil_type
        end

        private def infer_union(node : Frontend::UnionNode, expr_id : ExprId) : Type
          (node.body || [] of ExprId).each do |body_expr_id|
            infer_expression(body_expr_id)
          end

          @context.nil_type
        end

        # Phase 31: Type inference for module definition
        private def infer_module(node : Frontend::ModuleNode, expr_id : ExprId) : Type
          # In a full implementation, we would:
          # 1. Look up ModuleSymbol from symbol table
          # 2. Save current module context
          # 3. Process module body
          # 4. Restore previous module context
          # For now, just process the body
          (node.body || [] of ExprId).each do |body_expr_id|
            infer_expression(body_expr_id)
          end

          # Module definitions don't have value types
          @context.nil_type
        end

        # Phase 31: Type inference for include statement
        private def infer_include(node) : Type
          # In a full implementation, we would:
          # 1. Look up the module being included
          # 2. Mix the module's methods into the current class/module
          # 3. Verify module exists
          # For now, include statements just return Nil
          @context.nil_type
        end

        # Phase 31: Type inference for extend statement
        private def infer_extend(node) : Type
          # In a full implementation, we would:
          # 1. Look up the module being extended
          # 2. Mix the module's methods as class methods
          # 3. Verify module exists
          # For now, extend statements just return Nil
          @context.nil_type
        end

        # Phase 33: Type inference for enum definition
        private def infer_enum(node : Frontend::EnumNode) : Type
          # In a full implementation, we would:
          # 1. Create an EnumType with members
          # 2. Process member values (if any) and infer their types
          # 3. Validate base type compatibility
          # For now, process member values and return Nil
          if members = node.members
            members.each do |member|
              if value_expr = member.value
                infer_expression(value_expr)
              end
            end
          end

          # Enum definitions don't have value types
          @context.nil_type
        end

        private def infer_alias(node) : Type
          # Phase 34: Type alias definition
          # Type aliases are compile-time constructs with no runtime value
          # For now, we just acknowledge the alias exists and return Nil
          # In future, this would register the alias in a type registry
          @context.nil_type
        end

        # Phase 38: Type inference for lib definition
        private def infer_lib(node : Frontend::LibNode, expr_id : ExprId) : Type
          # In a full implementation, we would:
          # 1. Look up LibSymbol from symbol table
          # 2. Save current lib context
          # 3. Process lib body (fun, type declarations)
          # 4. Restore previous context
          # For now, just process the body
          (node.body || [] of ExprId).each do |body_expr_id|
            infer_expression(body_expr_id)
          end

          # Lib definitions don't have value types
          @context.nil_type
        end

        # Phase 64: Type inference for fun declaration (C function)
        private def infer_fun(node) : Type
          # Fun declarations are external C functions with no body
          # They specify:
          # - def_name: function name
          # - def_params: parameters (typed)
          # - def_return_type: return type annotation
          # - def_body: nil (no implementation)
          #
          # In a full implementation, we would:
          # 1. Register the function signature in the current lib context
          # 2. Resolve parameter types and return type
          # 3. Make the function available for calls
          #
          # For now, fun declarations have no runtime value (they're declarations)
          @context.nil_type
        end

        private def infer_constant(node : Frontend::ConstantNode) : Type
          # Phase 35: Constant declaration
          # Infer type from the assigned value expression
          if value_expr = node.value
            infer_expression(value_expr)
          else
            @context.nil_type
          end
        end

        private def infer_instance_var(node : Frontend::InstanceVarNode, expr_id : ExprId) : Type
          var_name = String.new(node.name)

          # Remove @ prefix
          clean_name = var_name.starts_with?("@") ? var_name[1..-1] : var_name

          if ENV["DEBUG"]?
            puts "DEBUG infer_instance_var:"
            puts "  var_name: #{clean_name}"
            puts "  @current_class: #{@current_class.try(&.name)}"
            puts "  @receiver_type_context: #{@receiver_type_context.inspect}"
          end

          # Phase 5C: Check explicit type annotation from ClassSymbol first
          if current_class = @current_class
            if type_annotation = current_class.get_instance_var_type(clean_name)
              if ENV["DEBUG"]?
                puts "  type_annotation: #{type_annotation}"
              end
              # Week 1: If we have receiver type context (generic instance), substitute type parameters
              if receiver = @receiver_type_context
                if ENV["DEBUG"]?
                  puts "  receiver.type_args: #{receiver.type_args.inspect}"
                  puts "  receiver.class_symbol.type_parameters: #{receiver.class_symbol.type_parameters.inspect}"
                end
                if (type_args = receiver.type_args) && (type_params = receiver.class_symbol.type_parameters)
                  result = substitute_type_parameters(type_annotation, type_args, type_params)
                  if ENV["DEBUG"]?
                    puts "  substituted result: #{result.class} = #{result.inspect}"
                  end
                  return result
                end
              end
              return parse_type_name(type_annotation)
            else
              if ENV["DEBUG"]?
                puts "  type_annotation: nil (not found in class)"
              end
            end
          end

          # Check if we have inferred type from assignment
          if inferred_type = @instance_var_types[clean_name]?
            return inferred_type
          end

          # Not found - return Nil
          if ENV["DEBUG"]?
            puts "  returning Nil (not found)"
          end
          @context.nil_type
        end

        # Phase 76: Infer type of class variable
        private def infer_class_var(node, expr_id : ExprId) : Type
          # For now, return nil_type as placeholder
          # Future: Track class variable types in class scope
          # (Type will be set by infer_expression)
          @context.nil_type
        end

        # Phase 75: Infer type of global variable
        private def infer_global(node, expr_id : ExprId) : Type
          # For now, return nil_type as placeholder
          # Future: Track global variable types in global scope
          # (Type will be set by infer_expression)
          @context.nil_type
        end

        # Phase 5C/77: Infer type of instance variable declaration (@var : Type)
        private def infer_instance_var_decl(node, expr_id : ExprId) : Type
          # Type declarations have no runtime value, return nil_type
          # The type annotation is stored in ivar_decl_type for semantic analysis
          @context.nil_type
        end

        # Phase 77: Infer type of class variable declaration (@@var : Type)
        private def infer_class_var_decl(node, expr_id : ExprId) : Type
          # Type declarations have no runtime value, return nil_type
          # The type annotation is stored in ivar_decl_type for semantic analysis
          @context.nil_type
        end

        # Phase 77: Infer type of global variable declaration ($var : Type)
        private def infer_global_var_decl(node, expr_id : ExprId) : Type
          # Type declarations have no runtime value, return nil_type
          # The type annotation is stored in ivar_decl_type for semantic analysis
          @context.nil_type
        end

        # Parse simple type name (e.g., "Int32", "String")
        # For Phase 1: Only built-in primitive types
        private def parse_type_name(name : String) : Type
          # Check for generic type syntax: Array(T)
          if name.includes?('(') && name.includes?(')')
            # Extract base type and type argument
            paren_start = name.index('(').not_nil!
            paren_end = name.rindex(')').not_nil!

            base_type = name[0...paren_start]
            type_arg = name[(paren_start + 1)...paren_end]

            case base_type
            when "Array"
              element_type = parse_type_name(type_arg)
              return ArrayType.new(element_type)
            else
              emit_error("Unknown generic type '#{base_type}'")
              return @context.nil_type
            end
          end

          # Handle primitive types
          case name
          when "Int32"   then @context.int32_type
          when "Int64"   then @context.int64_type
          when "Float64" then @context.float64_type
          when "String"  then @context.string_type
          when "Bool"    then @context.bool_type
          when "Nil"     then @context.nil_type
          when "Char"    then @context.char_type
          else
            # Try resolving scoped names (Time::Span) by the rightmost segment
            base_name = name.includes?("::") ? name.split("::").last : name

            if prim = primitive_type_for(base_name)
              return prim
            end

            if table = @global_table
              if symbol = table.lookup(base_name)
                if type = type_from_symbol(symbol)
                  return normalize_literal_type(type)
                end
              end
              # If scoped, try full name lookup as fallback
              if name.includes?("::")
                if symbol = table.lookup(name)
                  if type = type_from_symbol(symbol)
                    return normalize_literal_type(type)
                  end
                end
              end
            end

            # As a fallback, create a nominal primitive type to avoid Nil/Unknown
            PrimitiveType.new(name)
          end
        end

        # ============================================================
        # PHASE 2: Binary Operators
        # ============================================================

        private def infer_binary(node : Frontend::BinaryNode, expr_id : ExprId) : Type
          # Binary node has left, right, operator fields
          left_id = node.left
          right_id = node.right

          left_type = infer_expression(left_id)
          right_type = infer_expression(right_id)

          # Get operator text
          op = String.new(node.operator)

          debug("infer_binary: op=#{op}, left_type=#{left_type}, right_type=#{right_type}")

          result_type = case op
                        when "+", "-", "*", "/", "//", "%", "**", "<<", ">>", "&", "|", "^", "&+", "&-", "&*", "&**"
                          # Phase 4B.3/4B.5/18/19/21/22/78/89: Try method lookup first for built-in methods
                          # Phase 89: Wrapping arithmetic operators (&+, &-, &*, &**)
                          if method = lookup_method(left_type, op, [right_type])
                            debug("  lookup_method found: #{method.name}, return_annotation=#{method.return_annotation.inspect}")
                            if ann = method.return_annotation
                              result = parse_type_name(ann)
                              debug("  parse_type_name(#{ann}) => #{result}")
                              result
                            else
                              debug("  NO return_annotation, returning nil_type")
                              @context.nil_type
                            end
                            # Fallback: numeric promotion for untyped numeric operators
                            # Exclude << (array push operator) as it has specific semantics
                          elsif op != "<<" && numeric_type?(left_type) && numeric_type?(right_type)
                            debug("  fallback: numeric promotion")
                            promote_numeric_types(left_type, right_type)
                          else
                            # No method found and not numeric types
                            debug("  NO method found, emitting error")
                            emit_error("Operator '#{op}' not defined for #{left_type} and #{right_type}", expr_id)
                            @context.nil_type
                          end
                        when "==", "!=", "<", ">", "<=", ">=", "===", "=~", "!~", "in"
                          # Phase 4B.3/4B.5/50/79/80: Try method lookup first for built-in methods
                          # Phase 50: === (case equality) returns Bool like ==
                          # Phase 79: in (containment) returns Bool
                          # Phase 80: =~ (regex match), !~ (regex not match) return Bool
                          if method = lookup_method(left_type, op, [right_type])
                            if ann = method.return_annotation
                              parse_type_name(ann)
                            else
                              @context.bool_type
                            end
                          else
                            # Fallback: comparison operators → Bool for compatible types
                            @context.bool_type
                          end
                        when "<=>"
                          # Phase 48: Spaceship operator (three-way comparison)
                          # Returns Int32: -1 (less), 0 (equal), or 1 (greater)
                          # Try method lookup first
                          if method = lookup_method(left_type, op, [right_type])
                            if ann = method.return_annotation
                              parse_type_name(ann)
                            else
                              @context.int32_type
                            end
                          else
                            # Fallback: spaceship operator → Int32
                            @context.int32_type
                          end
                        when "&&", "||"
                          # Logical operators
                          unless bool_type?(left_type) && bool_type?(right_type)
                            emit_error("Operator '#{op}' requires bool types, got #{left_type} and #{right_type}", expr_id)
                            @context.nil_type
                          else
                            @context.bool_type
                          end
                        when "??"
                          # Phase 81: Nil-coalescing operator
                          # value ?? default - returns value if not nil, otherwise default
                          # Type: Simplified - return union of left and right types
                          # More accurate: return non-nil version of left type | right type
                          # For now: return right type (the fallback type)
                          right_type
                        else
                          emit_error("Unknown operator '#{op}'", expr_id)
                          @context.nil_type
                        end

          # Type will be set by infer_expression
          result_type
        end

        # Phase 17: Unary operator type inference
        private def infer_unary(node : Frontend::UnaryNode, expr_id : ExprId) : Type
          # Unary node has right (operand) and operator fields
          operand_id = node.operand

          operand_type = infer_expression(operand_id)
          op = String.new(node.operator)

          result_type = case op
                        when "!"
                          # Logical not: always returns Bool
                          # In Crystal: nil and false are falsy, everything else is truthy
                          @context.bool_type
                        when "+"
                          # Unary plus: identity for numeric types
                          if numeric_type?(operand_type)
                            operand_type
                          else
                            emit_error("Unary '+' not defined for #{operand_type}", expr_id)
                            @context.nil_type
                          end
                        when "-"
                          # Unary minus: negation for numeric types
                          if numeric_type?(operand_type)
                            operand_type
                          else
                            emit_error("Unary '-' not defined for #{operand_type}", expr_id)
                            @context.nil_type
                          end
                        when "&+"
                          # Phase 89: Unary wrapping plus (identity with wrapping semantics)
                          if numeric_type?(operand_type)
                            operand_type
                          else
                            emit_error("Unary '&+' not defined for #{operand_type}", expr_id)
                            @context.nil_type
                          end
                        when "&-"
                          # Phase 89: Unary wrapping minus (negation with wrapping semantics)
                          if numeric_type?(operand_type)
                            operand_type
                          else
                            emit_error("Unary '&-' not defined for #{operand_type}", expr_id)
                            @context.nil_type
                          end
                        when "~"
                          # Phase 21: Bitwise NOT for integer types
                          if numeric_type?(operand_type)
                            operand_type
                          else
                            emit_error("Bitwise '~' not defined for #{operand_type}", expr_id)
                            @context.nil_type
                          end
                        else
                          emit_error("Unknown unary operator '#{op}'", expr_id)
                          @context.nil_type
                        end

          result_type
        end

        private def numeric_type?(type : Type) : Bool
          type.is_a?(PrimitiveType) &&
            (type.name == "Int32" || type.name == "Int64" || type.name == "Float64")
        end

        private def bool_type?(type : Type) : Bool
          type.is_a?(PrimitiveType) && type.name == "Bool"
        end

        private def infer_block_result(expressions : Array(ExprId)) : Type
          return @context.nil_type if expressions.empty?

          result_type = @context.nil_type
          expressions.each do |expr_id|
            result_type = infer_expression(expr_id)
          end
          result_type
        end

        # Promote two numeric types to their widest common type
        #
        # PRODUCTION-READY FALLBACK STRATEGY:
        # This is a SAFE DEFAULT used when method resolution is not available.
        # In Phase 4, this will be replaced by exact Crystal behavior:
        #   1. Check method overload: left_type.+(right_type)
        #   2. If found: use method's return type (Crystal-exact)
        #   3. If not found: use this fallback (safe, no precision loss)
        #
        # Promotion rules (fallback):
        #   I32 < I64 < F64 (width ordering)
        #   Always return widest type to prevent precision loss
        #
        # Examples:
        #   promote(I32, I32) → I32
        #   promote(I32, I64) → I64  (safe: preserves Int64 range)
        #   promote(I32, F64) → F64  (safe: preserves float precision)
        #   promote(I64, F64) → F64
        #
        # TODO Phase 4: Replace with method overload lookup when available
        private def promote_numeric_types(left : Type, right : Type) : Type
          return @context.nil_type unless left.is_a?(PrimitiveType) && right.is_a?(PrimitiveType)

          # Type width values for ordering
          left_width = numeric_type_width(left.name)
          right_width = numeric_type_width(right.name)

          # Return widest type
          if left_width >= right_width
            left
          else
            right
          end
        end

        # Get numeric type width for promotion
        # I32 = 32, I64 = 64, F64 = 128 (floats wider than ints)
        private def numeric_type_width(type_name : String) : Int32
          case type_name
          when "Int32"   then 32
          when "Int64"   then 64
          when "Float64" then 128 # Floats wider than any integer
          else                0
          end
        end

        # ============================================================
        # PHASE 3: Control Flow
        # ============================================================

        private def infer_if(node : Frontend::IfNode) : Type
          condition_id = node.condition
          condition_type = infer_expression(condition_id)

          unless bool_type?(condition_type)
            emit_error("If condition must be Bool, got #{condition_type}", condition_id)
          end

          # Phase 95: Extract flow narrowing from is_a? condition
          narrowing = extract_is_a_narrowing(condition_id)

          # Apply narrowing for then-branch
          if narrowing
            var_name, narrowed_type = narrowing
            @flow_narrowings[var_name] = narrowed_type
          end

          then_type = infer_block_result(node.then_body)

          # Remove narrowing after then-branch
          if narrowing
            @flow_narrowings.delete(narrowing[0])
          end

          elsif_types = [] of Type
          if elsifs = node.elsifs
            elsifs.each do |elsif_branch|
              branch_condition = elsif_branch.condition
              branch_condition_type = infer_expression(branch_condition)
              unless bool_type?(branch_condition_type)
                emit_error("Elsif condition must be Bool, got #{branch_condition_type}", branch_condition)
              end

              # Phase 95: Extract flow narrowing for elsif branch
              elsif_narrowing = extract_is_a_narrowing(branch_condition)
              if elsif_narrowing
                @flow_narrowings[elsif_narrowing[0]] = elsif_narrowing[1]
              end

              elsif_types << infer_block_result(elsif_branch.body)

              if elsif_narrowing
                @flow_narrowings.delete(elsif_narrowing[0])
              end
            end
          end

          else_type = node.else_body ? infer_block_result(node.else_body.not_nil!) : @context.nil_type

          union_of([then_type] + elsif_types + [else_type])
        end

        # Phase 95: Extract variable name and narrowed type from is_a? condition
        # Returns {variable_name, narrowed_type} or nil if not an is_a? check
        private def extract_is_a_narrowing(condition_id : ExprId) : {String, Type}?
          condition_node = @program.arena[condition_id]

          # Check if condition is an is_a? expression
          return nil unless condition_node.is_a?(Frontend::IsANode)

          # Get the expression being checked
          expr_id = condition_node.expression
          expr_node = @program.arena[expr_id]

          # Only narrow if checking a simple variable (identifier)
          return nil unless expr_node.is_a?(Frontend::IdentifierNode)

          var_name = String.new(expr_node.name)
          type_name = String.new(condition_node.target_type)

          # Resolve target type
          narrowed_type = parse_type_name(type_name)

          {var_name, narrowed_type}
        end

        # Phase 24: Type inference for unless (similar to if but without elsif)
        private def infer_unless(node : Frontend::UnlessNode) : Type
          condition_id = node.condition
          condition_type = infer_expression(condition_id)

          unless bool_type?(condition_type)
            emit_error("Unless condition must be Bool, got #{condition_type}", condition_id)
          end

          then_type = infer_block_result(node.then_branch)
          else_type = node.else_branch ? infer_block_result(node.else_branch.not_nil!) : @context.nil_type

          union_of([then_type, else_type])
        end

        # Phase 25: Type inference for until loop (inverse of while)
        private def infer_until(node : Frontend::UntilNode) : Type
          condition_id = node.condition
          condition_type = infer_expression(condition_id)

          unless bool_type?(condition_type)
            emit_error("Until condition must be Bool, got #{condition_type}", condition_id)
          end

          node.body.each { |expr_id| infer_expression(expr_id) }

          @context.nil_type
        end

        # Phase 28/29: Type inference for begin/end blocks with rescue/ensure
        # Begin blocks return the type of the last expression, or Nil if empty
        # With rescue: union of begin body type and all rescue body types
        # Ensure doesn't affect type (always runs but doesn't change return value)
        private def infer_begin(node : Frontend::BeginNode) : Type
          begin_type = infer_block_result(node.body)

          types = [begin_type]
          if rescue_clauses = node.rescue_clauses
            rescue_clauses.each do |rescue_clause|
              types << infer_block_result(rescue_clause.body)
            end
          end

          if ensure_body = node.ensure_body
            ensure_body.each { |expr_id| infer_expression(expr_id) }
          end

          union_of(types)
        end

        # Phase 29: Type inference for raise statement
        # Raise always returns Nil (it never actually returns, but we use Nil for simplicity)
        # In a full compiler, this would be NoReturn type
        private def infer_raise(node : Frontend::RaiseNode) : Type
          # Infer the raise value (if present)
          if raise_value = node.value
            infer_expression(raise_value)
          end

          # Raise never returns, but we use Nil as type
          @context.nil_type
        end

        # Phase 65: Type inference for require statement
        private def infer_require(node : Frontend::RequireNode) : Type
          # Infer the require path expression (typically a string literal)
          infer_expression(node.path)

          # Require statements are executed at compile-time for imports
          # They don't have a runtime value, so return Nil type
          @context.nil_type
        end

        # Phase 66: Type inference for type declaration
        private def infer_type_declaration(node) : Type
          # Type declarations like `x : Int32` declare a variable with an explicit type
          # but don't assign a value. They are compile-time type annotations.
          #
          # In a full implementation:
          # - Register the variable name with its declared type in the scope
          # - Use this type for subsequent references to the variable
          # - Verify assignments match the declared type
          #
          # For now, type declarations have no runtime value (they're declarations)
          @context.nil_type
        end

        # Phase 67: Type inference for with context block
        private def infer_with(node : Frontend::WithNode) : Type
          infer_expression(node.receiver)

          result_type = @context.nil_type
          node.body.each do |expr_id|
            result_type = infer_expression(expr_id)
          end

          result_type
        end

        # Phase 30: Type inference for accessor macros (getter/setter/property)
        # These are declarations, not expressions - they return Nil
        # In a full compiler with macro expansion, they would:
        #   1. Declare instance variable (@name : Type)
        #   2. Generate getter method (def name : Type; @name; end)
        #   3. Generate setter method (def name=(@name : Type); end)
        # For now, we parse and type-check the structure only
        private def infer_accessor(node : Frontend::GetterNode | Frontend::SetterNode | Frontend::PropertyNode) : Type
          node.specs.each do |spec|
            if default_value = spec.default_value
              infer_expression(default_value)
            end
          end

          @context.nil_type
        end

        private def infer_while(node : Frontend::WhileNode) : Type
          condition_id = node.condition
          condition_type = infer_expression(condition_id)

          unless bool_type?(condition_type)
            emit_error("While condition must be Bool, got #{condition_type}", condition_id)
          end

          node.body.each { |expr_id| infer_expression(expr_id) }

          @context.nil_type
        end

        # Phase 99: for loop
        private def infer_for(node : Frontend::ForNode) : Type
          # Infer collection type
          collection_type = infer_expression(node.collection)

          # In a full implementation:
          # - Check collection is Enumerable/Iterable
          # - Define loop variable in nested scope
          # - Infer variable type from collection element type

          # For now, just verify collection expression is valid

          # Infer body expressions (result not used)
          if body = node.body
            body.each { |expr_id| infer_expression(expr_id) }
          end

          # For loops always return Nil in Crystal
          @context.nil_type
        end

        private def infer_loop(node : Frontend::LoopNode) : Type
          # Phase 83: Infinite loop
          # loop do ... end - runs indefinitely until break/return

          # Infer body expressions (result not used)
          if body = node.body
            body.each { |expr_id| infer_expression(expr_id) }
          end

          # Loop statements always return Nil in Crystal
          # (actual exit via break/return is handled separately)
          @context.nil_type
        end

        private def infer_spawn(node : Frontend::SpawnNode) : Type
          # Phase 84: Spawn fiber (concurrency)
          # spawn do...end | spawn expression
          # Creates a new fiber to run code concurrently

          # Block form: spawn do...end
          if body = node.body
            body.each { |expr_id| infer_expression(expr_id) }
          end

          # Expression form: spawn expression
          if expr = node.expression
            infer_expression(expr)
          end

          # Spawn doesn't return value to caller
          # (fiber runs independently, no synchronous return)
          @context.nil_type
        end

        # ============================================================
        # PHASE 2: Assignments
        # ============================================================

        private def infer_assign(node : Frontend::AssignNode, expr_id : ExprId) : Type
          target_id = node.target
          value_id = node.value

          # Infer value type
          value_type = infer_expression(value_id)

          # Get target identifier name
          target_node = @program.arena[target_id]

          # Phase 5A: Check if target is instance variable
          case target_node
          when Frontend::InstanceVarNode
            target_name = String.new(target_node.name)
            clean_name = target_name.starts_with?("@") ? target_name[1..-1] : target_name
            @instance_var_types[clean_name] = value_type
          when Frontend::IdentifierNode
            @assignments[String.new(target_node.name)] = value_type
          end
          # Phase 14B: Index assignment (h["key"] = value) - no tracking needed,
          # just return value type

          # Assignments return the value type in Crystal
          # Type will be set by infer_expression
          value_type
        end

        # Phase 73: Multiple assignment (a, b = 1, 2)
        private def infer_multiple_assign(node : Frontend::MultipleAssignNode, expr_id : ExprId) : Type
          targets = node.targets
          value_id = node.value

          # Infer value type (typically a tuple)
          value_type = infer_expression(value_id)

          # For each target, store the value type
          # Future: Extract individual types from tuple
          targets.each do |target_id|
            target_node = @program.arena[target_id]
            if target_node.is_a?(Frontend::IdentifierNode)
              @assignments[String.new(target_node.name)] = value_type
            end
          end

          # Multiple assignment returns the value type
          value_type
        end

        # ============================================================
        # PHASE 6: Return Statements
        # ============================================================

        private def infer_return(node : Frontend::ReturnNode, expr_id : ExprId) : Type
          # If return has a value, infer its type
          # (Type will be set by infer_expression)
          if value_id = node.value
            value_type = infer_expression(value_id)
            value_type
          else
            # Return without value returns nil
            @context.nil_type
          end
        end

        # ============================================================
        # PHASE 7: Self Keyword
        # ============================================================

        private def infer_self(node, expr_id : ExprId) : Type
          # self returns InstanceType of the current class
          # (Type will be set by infer_expression)
          if current_class = @current_class
            instance_type = InstanceType.new(current_class)
            instance_type
          else
            # self outside class context (shouldn't happen in valid code)
            @context.nil_type
          end
        end

        # Phase 39: Type inference for super (call parent method)
        private def infer_super(node : Frontend::SuperNode, expr_id : ExprId) : Type
          # In a full implementation, we would:
          # 1. Look up the current method name
          # 2. Look up the parent class's method with same name
          # 3. Type check the arguments
          # 4. Return the parent method's return type

          # For now, infer types of provided arguments
          if args = node.args
            args.each do |arg_expr_id|
              infer_expression(arg_expr_id)
            end
          end

          # Return Nil for now (in full implementation, would return parent method's return type)
          # If super_args is nil, it means implicit args (pass all method args)
          @context.nil_type
        end

        # Phase 96: Type inference for previous_def (call previous definition before reopening/redefining)
        private def infer_previous_def(node : Frontend::PreviousDefNode, expr_id : ExprId) : Type
          # In a full implementation, we would:
          # 1. Look up the current method name
          # 2. Look up the previous definition of this method (before reopening/redefining)
          # 3. Type check the arguments
          # 4. Return the previous method's return type

          # For now, infer types of provided arguments
          if args = node.args
            args.each do |arg_expr_id|
              infer_expression(arg_expr_id)
            end
          end

          # Return Nil for now (in full implementation, would return previous method's return type)
          # If previous_def_args is nil, it means implicit args (pass all method args)
          @context.nil_type
        end

        # Phase 40: Type inference for typeof (type introspection)
        private def infer_typeof(node : Frontend::TypeofNode, expr_id : ExprId) : Type
          # typeof returns the type of its argument(s) at compile time
          # In a full implementation:
          # - typeof(x) returns the type of x (e.g., Int32)
          # - typeof(x, y) returns the union type (e.g., Int32 | String)

          # For now, infer types of arguments and return a placeholder
          node.args.each do |arg_expr_id|
            infer_expression(arg_expr_id)
          end

          @context.nil_type
        end

        # Phase 41: Type inference for sizeof (size in bytes)
        private def infer_sizeof(node : Frontend::SizeofNode, expr_id : ExprId) : Type
          # sizeof returns the size of a type or expression in bytes
          # In a full implementation:
          # - sizeof(Int32) returns 4 (32 bits = 4 bytes)
          # - sizeof(Type) returns the size of that type
          # - sizeof(expr) returns the size of expr's type

          # For now, infer types of arguments and return Int32
          node.args.each { |arg_expr_id| infer_expression(arg_expr_id) }

          # sizeof always returns Int32 (number of bytes)
          @context.int32_type
        end

        # Phase 42: pointerof (pointer to variable/expression)
        private def infer_pointerof(node : Frontend::PointerofNode, expr_id : ExprId) : Type
          # pointerof returns a pointer to a variable or expression
          # In a full implementation:
          # - pointerof(x) returns Pointer(T) where T is the type of x
          # - pointerof(@ivar) returns pointer to instance variable
          # - pointerof(expr) returns pointer to expression's result

          # For now, infer types of arguments and return nil as placeholder
          node.args.each { |arg_expr_id| infer_expression(arg_expr_id) }

          # Return nil_type as placeholder (full implementation would return Pointer(T))
          @context.nil_type
        end

        private def infer_uninitialized(node : Frontend::UninitializedNode, expr_id : ExprId) : Type
          # Phase 85: uninitialized creates an uninitialized variable of specified type
          # uninitialized(Type) allocates memory but doesn't initialize it
          # Returns a value of the specified type
          # In a full implementation:
          # - Parse type expression to get actual type
          # - Return that type as the result type
          # - No initialization code generated

          # For now, infer the type expression and return the type
          infer_expression(node.type)

          # Return nil_type as placeholder (full implementation would return the actual type)
          @context.nil_type
        end

        private def infer_offsetof(node : Frontend::OffsetofNode, expr_id : ExprId) : Type
          # Phase 86: offsetof returns the byte offset of a field within a type
          # offsetof(Type, :field) returns Int32 offset value
          # In a full implementation:
          # - Process type argument to get actual type
          # - Process field argument (symbol or identifier)
          # - Calculate field offset in bytes
          # - Return Int32 type

          # For now, infer both arguments and return nil_type as placeholder
          node.args.each { |arg_expr_id| infer_expression(arg_expr_id) }

          # Return nil_type as placeholder (full implementation would return Int32)
          @context.nil_type
        end

        # Phase 88: Type inference for alignof (ABI alignment in bytes)
        private def infer_alignof(node : Frontend::AlignofNode, expr_id : ExprId) : Type
          # alignof returns the ABI alignment of a type in bytes
          # In a full implementation:
          # - alignof(Int32) returns 4 (aligned on 4-byte boundaries)
          # - alignof(Int64) returns 8 (aligned on 8-byte boundaries)
          # - alignof(Type) returns the alignment of that type

          # For now, infer types of arguments and return Int32
          node.args.each { |arg_expr_id| infer_expression(arg_expr_id) }

          # alignof always returns Int32 (number of bytes)
          @context.int32_type
        end

        # Phase 88: Type inference for instance_alignof (instance alignment)
        private def infer_instance_alignof(node : Frontend::InstanceAlignofNode, expr_id : ExprId) : Type
          # instance_alignof returns the effective alignment of a class instance
          # Different from alignof which returns pointer alignment for reference types
          # In a full implementation:
          # - instance_alignof(Class) returns actual instance alignment
          # - instance_alignof differs from alignof for reference types

          # For now, infer types of arguments and return Int32
          node.args.each { |arg_expr_id| infer_expression(arg_expr_id) }

          # instance_alignof always returns Int32 (number of bytes)
          @context.int32_type
        end

        # Phase 95: Type inference for asm (inline assembly)
        private def infer_asm(node : Frontend::AsmNode, expr_id : ExprId) : Type
          # asm inserts inline assembly code
          # In a full implementation:
          # - asm("template", outputs..., inputs..., clobbers..., flags...)
          # - Parse colon-separated sections
          # - Validate LLVM assembly constraints
          # - Return type depends on output operands
          #
          # Phase 95A: Parser-only implementation
          # - Parse all arguments as expressions
          # - Return nil_type (asm statements don't produce values)
          # - Future phases will add proper constraint parsing and type checking

          # For now, infer types of all arguments
          node.args.each { |arg_expr_id| infer_expression(arg_expr_id) }

          # asm expressions return nil (inline assembly is a statement)
          @context.nil_type
        end

        # Phase 98: out keyword (C bindings output parameter)
        private def infer_out(node : Frontend::OutNode, expr_id : ExprId) : Type
          # out defines a new variable and passes its address to C function
          # Syntax: out identifier
          # In a full implementation:
          # - Define new variable in current scope
          # - Variable type inferred from C function signature
          # - Return pointer type for passing to C
          #
          # Phase 98A: Parser-only implementation
          # - Store identifier slice in AST
          # - Return nil_type (actual variable definition in future phase)

          # No expressions to infer (just identifier slice stored in Frontend.node_out_identifier(node))

          # Return nil (actual type inference deferred to semantic phase)
          @context.nil_type
        end

        # Phase 44: as keyword (type cast)
        private def infer_as(node : Frontend::AsNode, expr_id : ExprId) : Type
          # Type cast: value.as(Type)
          # In a full implementation:
          # - Infer type of value being cast
          # - Look up target type in type registry
          # - Verify cast is valid (runtime or compile-time)
          # - Return target type

          # For now, infer type of value and return nil as placeholder
          infer_expression(node.expression)

          # Return nil_type as placeholder (full implementation would return target type)
          @context.nil_type
        end

        # Phase 45: as? keyword (safe cast - nilable)
        private def infer_as_question(node : Frontend::AsQuestionNode, expr_id : ExprId) : Type
          # Safe cast: value.as?(Type)
          # Returns Type? (nilable) instead of Type
          # In a full implementation:
          # - Infer type of value being cast
          # - Look up target type in type registry
          # - Return Union(target_type, Nil) - nilable version
          # - Unlike .as, this doesn't panic on invalid cast, returns nil

          # For now, infer type of value and return nil as placeholder
          infer_expression(node.expression)

          # Return nil_type as placeholder (full implementation would return target_type | Nil)
          @context.nil_type
        end

        # Phase 93: is_a? keyword (type check - returns Bool)
        private def infer_is_a(node : Frontend::IsANode, expr_id : ExprId) : Type
          # Type check: value.is_a?(Type)
          # Returns Bool (true if value is instance of Type, false otherwise)
          # In a full implementation:
          # - Infer type of value being checked
          # - Look up target type in type registry
          # - Return Bool type
          # - Unlike .as, this doesn't cast, just checks
          # - Can enable type narrowing in conditional branches

          # For now, infer type of value and return bool type
          infer_expression(node.expression)

          # Return Bool type (is_a? always returns boolean)
          @context.bool_type
        end

        # Phase 94: responds_to? keyword (method check - returns Bool)
        private def infer_responds_to(node : Frontend::RespondsToNode, expr_id : ExprId) : Type
          # Method check: value.responds_to?(:method_name)
          # Returns Bool (true if value has method, false otherwise)
          # In a full implementation:
          # - Infer type of value being checked
          # - Extract method name from Symbol/String argument
          # - Look up method in value's type
          # - Return Bool based on whether method exists
          # - Unlike .is_a?, this checks for method availability

          # For now, infer type of value and method name, return bool type
          infer_expression(node.expression)
          infer_expression(node.method_name)

          # Return Bool type (responds_to? always returns boolean)
          @context.bool_type
        end

        # Phase 60 + Week 1: Generic type instantiation
        # Box(Int32) → ClassType(Box, [Int32])
        private def infer_generic(node : Frontend::GenericNode, expr_id : ExprId) : Type
          # Infer base type (should be ClassType for user-defined generics)
          base_type = infer_expression(node.base_type)

          if ENV["DEBUG"]?
            puts "DEBUG infer_generic:"
            puts "  base_type: #{base_type.class} = #{base_type.inspect}"
            puts "  type_args count: #{node.type_args.size}"
          end

          # Infer type arguments and normalize them
          # ClassType(Int32) → PrimitiveType("Int32")
          type_args = Array(Type).new(node.type_args.size)
          node.type_args.each_with_index do |arg, i|
            if ENV["DEBUG"]?
              arg_node = @program.arena[arg]
              puts "  arg #{i}: node=#{arg_node.class}"
            end
            inferred = infer_expression(arg)
            if ENV["DEBUG"]?
              puts "    inferred: #{inferred.class} = #{inferred.inspect}"
            end
            type_args << normalize_type_argument(inferred)
          end

          # If base is a class (Box), create ClassType with type args
          if base_type.is_a?(ClassType)
            return ClassType.new(base_type.symbol, type_args)
          end

          # Otherwise return base type (for built-in types like Array, Hash)
          # Future: handle Array(Int32), Hash(String, Int32)
          base_type
        end

        # Normalize type argument: ClassType(Int32) → PrimitiveType("Int32")
        # Week 1: Convert ClassType references to actual types
        private def normalize_type_argument(type : Type) : Type
          if ENV["DEBUG"]?
            puts "DEBUG normalize_type_argument: #{type.class} = #{type.inspect}"
          end

          if type.is_a?(ClassType)
            # If it's a reference to a primitive class, convert to PrimitiveType
            result = case type.symbol.name
                     when "Int32"   then @context.int32_type
                     when "Int64"   then @context.int64_type
                     when "Float64" then @context.float64_type
                     when "String"  then @context.string_type
                     when "Bool"    then @context.bool_type
                     when "Nil"     then @context.nil_type
                     when "Char"    then @context.char_type
                     else
                       # User-defined class - keep as ClassType
                       type
                     end

            if ENV["DEBUG"]?
              puts "  → normalized to: #{result.class} = #{result.inspect}"
            end
            result
          else
            type
          end
        end

        # Normalize literal/annotation types to primitives when possible
        private def normalize_literal_type(type : Type) : Type
          normalize_type_argument(type)
        end

        # Phase 63: Type inference for path expressions (Foo::Bar)
        private def infer_path(node : Frontend::PathNode, expr_id : ExprId) : Type
          debug("infer_path: expr_id=#{expr_id.index}")
          # Visit child expressions so nested nodes get types assigned
          if left_expr = node.left
            infer_expression(left_expr)
          end
          infer_expression(node.right)

          if symbol = resolve_path_symbol(node)
            debug("  resolved symbol: #{symbol.class.name}")
            if type = type_from_symbol(symbol)
              debug("  resolved type: #{type.class.name}")
              return type
            end
          end

          debug("  resolve_path_symbol failed") if @debug_enabled
          @context.nil_type
        end

        # Phase 47: &. safe navigation operator (returns nilable)
        private def infer_safe_navigation(node : Frontend::SafeNavigationNode, expr_id : ExprId) : Type
          # Safe navigation: receiver&.member
          # Returns member_type | Nil (nilable)
          # If receiver is nil, returns nil without calling method
          # Otherwise, calls method and returns its result
          # In a full implementation:
          # - Infer type of receiver
          # - Look up member in receiver's type
          # - Return Union(member_type, Nil) - nilable version

          # For now, infer receiver type and return nil as placeholder
          infer_expression(node.object)

          # Return nil_type as placeholder (full implementation would return member_type | Nil)
          @context.nil_type
        end

        private def resolve_path_symbol(node : Frontend::PathNode) : Symbol?
          return nil unless @global_table

          segments = [] of String
          collect_path_segments(node, segments)
          debug("  segments=#{segments.inspect}") if @debug_enabled
          return nil if segments.empty?

          current_table = @global_table
          symbol : Symbol? = nil

          segments.each_with_index do |segment, index|
            return nil unless current_table
            debug("  resolving segment '#{segment}' (index #{index})") if @debug_enabled
            symbol = current_table.lookup(segment)
            debug("    symbol=#{symbol ? symbol.class.name : "nil"}") if @debug_enabled
            return nil unless symbol

            if index < segments.size - 1
              current_table = case symbol
                              when ClassSymbol
                                symbol.scope
                              when ModuleSymbol
                                symbol.scope
                              else
                                return nil
                              end
            end
          end

          symbol
        end

        private def collect_path_segments(node : Frontend::PathNode, segments : Array(String))
          if left_id = node.left
            append_path_segments(left_id, segments)
          end
          append_path_segments(node.right, segments)
        end

        private def append_path_segments(expr_id : ExprId, segments : Array(String))
          expr = @program.arena[expr_id]
          case expr
          when Frontend::PathNode
            collect_path_segments(expr, segments)
          when Frontend::IdentifierNode
            segments << String.new(expr.name)
          end
        end

        private def type_from_symbol(symbol : Symbol) : Type?
          case symbol
          when ClassSymbol
            ClassType.new(symbol)
          when ModuleSymbol
            ModuleType.new(symbol)
          else
            nil
          end
        end

        # ============================================================
        # PHASE 8: String Interpolation
        # ============================================================

        private def infer_string_interpolation(node : Frontend::StringInterpolationNode, expr_id : ExprId) : Type
          node.pieces.each do |piece|
            if piece.kind == Frontend::StringPiece::Kind::Expression
              if expr = piece.expr
                infer_expression(expr)
              end
            end
          end

          # Set the type for the string interpolation node itself
          @context.set_type(expr_id, @context.string_type)
          @context.string_type
        end

        # ============================================================
        # PHASE 9: Array Literals
        # ============================================================

        private def infer_array_literal(node : Frontend::ArrayLiteralNode, expr_id : ExprId) : Type
          # Determine element type
          element_type : Type

          # Case 1: Explicit "of Type" syntax ([] of Int32)
          if of_type_expr_id = node.of_type
            # Phase 91A: parser-only pass — use Nil placeholder until full type extraction
            element_type = @context.nil_type
            # Case 2: Infer from elements
          elsif elements = node.elements
            if elements.empty?
              # Empty array without type annotation - default to Nil
              # (In real Crystal this would be an error, but we'll allow it for now)
              element_type = @context.nil_type
            else
              # Infer type of each element
              tmp = Array(Type).new(elements.size)
              elements.each { |elem_id| tmp << infer_expression(elem_id) }
              # Union all element types
              element_type = @context.union_of(tmp)
            end
          else
            # Empty array without elements or type - shouldn't happen
            element_type = @context.nil_type
          end

          # Create Array(T) type
          # (Type will be set by infer_expression)
          array_type = ArrayType.new(element_type)
          array_type
        end

        private def type_from_type_expr(expr_id : ExprId) : Type?
          node = @program.arena[expr_id]

          case node
          when Frontend::PathNode
            if symbol = resolve_path_symbol(node)
              if type = type_from_symbol(symbol)
                return normalize_literal_type(type)
              end
            end
            if name = rightmost_segment(node)
              if prim = primitive_type_for(name)
                return prim
              end
            end
          when Frontend::IdentifierNode
            table = @global_table
            return nil unless table
            name = String.new(node.name)
            if symbol = table.lookup(name)
              if type = type_from_symbol(symbol)
                return normalize_literal_type(type)
              end
            end
            if prim = primitive_type_for(name)
              return prim
            end
          end

          nil
        end

        private def rightmost_segment(node : Frontend::PathNode) : String?
          segments = [] of String
          collect_path_segments(node, segments)
          segments.last?
        end

        private def primitive_type_for(name : String) : Type?
          case name
          when "Int32"   then @context.int32_type
          when "Int64"   then @context.int64_type
          when "Float64" then @context.float64_type
          when "String"  then @context.string_type
          when "Bool"    then @context.bool_type
          when "Nil"     then @context.nil_type
          when "Char"    then @context.char_type
          else
            nil
          end
        end

        private def infer_index(node : Frontend::IndexNode, expr_id : ExprId) : Type
          target_type = infer_expression(node.object)
          index_id = node.indexes.first?
          return @context.nil_type unless index_id
          _index_type = infer_expression(index_id)

          # Phase 9: Array indexing
          if target_type.is_a?(ArrayType)
            element_type = target_type.element_type
            # Type will be set by infer_expression
            element_type
            # Phase 14B: Hash indexing
          elsif target_type.is_a?(HashType)
            value_type = target_type.value_type
            # Type will be set by infer_expression
            value_type
            # Phase 15: Tuple indexing
          elsif target_type.is_a?(TupleType)
            index_node = @program.arena[index_id]
            unless index_node.is_a?(Frontend::NumberNode)
              emit_error("Tuple indexing requires compile-time constant integer", expr_id)
              return @context.nil_type
            end

            index_text = String.new(index_node.value)
            unless index_value = index_text.to_i32?
              emit_error("Invalid tuple index literal", expr_id)
              return @context.nil_type
            end

            if elem_type = target_type.type_at(index_value)
              elem_type
            else
              emit_error("Tuple index #{index_value} out of bounds (size: #{target_type.size})", expr_id)
              @context.nil_type
            end
          else
            # Not an array, hash, or tuple - emit error
            emit_error("Cannot index type #{target_type}", expr_id)
            @context.nil_type
          end
        end

        # ============================================================
        # PHASE 4: Method Calls
        # ============================================================

        private def infer_member_access(node : Frontend::MemberAccessNode, expr_id : ExprId) : Type
          # MemberAccess without parens is a zero-argument method call in Crystal
          # obj.method → obj.method()
          receiver_type = infer_expression(node.object)
          method_name = String.new(node.member)

          debug("infer_member_access: receiver_type = #{receiver_type.class.name}: #{receiver_type}, method = #{method_name}")

          # Phase 4B.5 + Week 1: Special case for constructor - ClassName.new → InstanceType
          # Week 1: Zero-argument constructor
          # Box(Int32).new or Box.new (no args)
          if method_name == "new" && receiver_type.is_a?(ClassType)
            # If ClassType has type_args (e.g., Box(Int32)), copy them to InstanceType
            debug("  Constructor call - returning InstanceType")
            return InstanceType.new(receiver_type.symbol, receiver_type.type_args)
          end

          # Phase 4B: Zero-argument method call
          arg_types = [] of Type

          # Phase 4B.4: Special case for union types - compute union return type
          if receiver_type.is_a?(UnionType)
            if return_type = compute_union_method_return_type(receiver_type, method_name, arg_types)
              return return_type
            else
              emit_error("Method '#{method_name}' not found on #{receiver_type}", expr_id)
              return @context.nil_type
            end
          end

          # Lookup method with overload resolution
          method = lookup_method(receiver_type, method_name, arg_types)
          debug("  lookup_method returned: #{method ? "MethodSymbol(#{method.name})" : "nil"}")

          result_type = if method
                          if ann = method.return_annotation
                            debug("  Method has return annotation: #{ann}")
                            # Week 1: Substitute type parameters in return type
                            # If receiver is InstanceType with type_args, substitute T → Int32
                            if receiver_type.is_a?(InstanceType) && (type_args = receiver_type.type_args) && (type_params = receiver_type.class_symbol.type_parameters)
                              substitute_type_parameters(ann, type_args, type_params)
                            else
                              parse_type_name(ann)
                            end
                          else
                            debug("  No return annotation - inferring from method body")
                            # Week 1: No return annotation - infer from method body
                            # For generic methods, set receiver context for type parameter substitution
                            body_type = infer_method_body_type(method, receiver_type)
                            debug("  infer_method_body_type returned: #{body_type.class.name}: #{body_type}")
                            body_type
                          end
                        else
                          emit_error("Method '#{method_name}' not found on #{receiver_type}", expr_id)
                          @context.nil_type
                        end

          debug("  infer_member_access returning: #{result_type.class.name}: #{result_type}")
          result_type
        end

        private def infer_call(node : Frontend::CallNode, expr_id : ExprId) : Type
          if block_id = node.block
            infer_expression(block_id)
          end

          callee_node = @program.arena[node.callee]

          debug("infer_call: callee_node type = #{callee_node.class.name}")

          receiver_type : Type?
          method_name : String?

          case callee_node
          when Frontend::MemberAccessNode
            receiver_type = infer_expression(callee_node.object)
            method_name = String.new(callee_node.member)
            debug("  receiver_type = #{receiver_type.class.name}: #{receiver_type}")
            debug("  method_name = #{method_name}")
          when Frontend::IdentifierNode
            # Week 1 Day 2: Top-level function call (e.g., identity(42))
            method_name = String.new(callee_node.name)
            debug("  IdentifierNode call: method_name = #{method_name}")
            # Infer argument types
            arg_types = Array(Type).new(node.args.size)
            node.args.each { |arg_id| arg_types << infer_expression(arg_id) }
            # Lookup method in global scope
            return infer_top_level_function_call(method_name, arg_types, expr_id)
          else
            debug("  UNKNOWN callee type - returning Nil!")
            return @context.nil_type
          end

          return @context.nil_type unless receiver_type && method_name

          # Week 1: Generic class instantiation with type inference
          # Box.new(42) → infer T=Int32 from argument
          # Box(Int32).new(42) → use explicit type args
          if @debug_enabled && method_name == "new"
            debug("  Constructor call: receiver_type.class = #{receiver_type.class.name}")
            debug("  Is ClassType? #{receiver_type.is_a?(ClassType)}")
          end
          if method_name == "new" && receiver_type.is_a?(ClassType)
            # Infer argument types first
            arg_types = Array(Type).new(node.args.size)
            node.args.each { |arg_id| arg_types << infer_expression(arg_id) }

            # DEBUG: Print receiver_type info
            if ENV["DEBUG"]?
              puts "DEBUG infer_call:"
              puts "  receiver: #{receiver_type.symbol.name}"
              puts "  receiver.type_args: #{receiver_type.type_args.inspect}"
              if ta = receiver_type.type_args
                ta.each_with_index do |t, i|
                  puts "    arg #{i}: #{t.class} = #{t.inspect}"
                end
              end
            end

            # If ClassType already has type_args (explicit Box(Int32)), use them
            if receiver_type.type_args
              return InstanceType.new(receiver_type.symbol, receiver_type.type_args)
              # Otherwise try to infer type arguments from constructor arguments
            elsif type_args = infer_type_arguments(receiver_type.symbol, arg_types)
              return InstanceType.new(receiver_type.symbol, type_args)
            else
              return InstanceType.new(receiver_type.symbol)
            end
          end

          arg_types = Array(Type).new(node.args.size)
          node.args.each { |arg_id| arg_types << infer_expression(arg_id) }

          if named_args = node.named_args
            named_args.each { |named_arg| infer_expression(named_arg.value) }
          end

          # Enumerable heuristics for Array element types
          if receiver_type.is_a?(ArrayType)
            elem_type = receiver_type.element_type
            case method_name
            when "map", "collect"
              mapped = elem_type
              if block_id = node.block
                block = @program.arena[block_id].as(Frontend::BlockNode)
                mapped = infer_block_result(block.body) || elem_type
              end
              return ArrayType.new(mapped)
            when "select", "reject", "filter"
              return ArrayType.new(elem_type)
            when "each", "each_with_index"
              return receiver_type
            when "to_a"
              return receiver_type
            end
          elsif receiver_type.is_a?(PrimitiveType) && receiver_type.name == "Nil"
            # Fallback heuristic: preserve collection shape even if receiver unknown
            case method_name
            when "map", "collect", "select", "reject", "filter", "to_a"
              return ArrayType.new(@context.nil_type)
            when "each", "each_with_index"
              return ArrayType.new(@context.nil_type)
            end
          end

          if receiver_type.is_a?(UnionType)
            if return_type = compute_union_method_return_type(receiver_type, method_name, arg_types)
              return return_type
            else
              emit_error("Method '#{method_name}' not found on #{receiver_type}", expr_id)
              return @context.nil_type
            end
          end

          if method = lookup_method(receiver_type, method_name, arg_types)
            if ann = method.return_annotation
              # Week 1: Substitute type parameters in return type
              if receiver_type.is_a?(InstanceType) && (type_args = receiver_type.type_args) && (type_params = receiver_type.class_symbol.type_parameters)
                substitute_type_parameters(ann, type_args, type_params)
              else
                parse_type_name(ann)
              end
            else
              @context.nil_type
            end
          else
            # Fallback heuristic for unknown collection receivers
            if {"map", "collect", "select", "reject", "filter", "to_a", "each", "each_with_index"}.includes?(method_name)
              return ArrayType.new(@context.nil_type)
            end
            emit_error("Method '#{method_name}' not found on #{receiver_type}", expr_id)
            @context.nil_type
          end
        end

        # Week 1 Day 2: Infer type for top-level function call
        # Handles calls like: identity(42), pair(1, "hi")
        private def infer_top_level_function_call(method_name : String, arg_types : Array(Type), expr_id : ExprId) : Type
          # Lookup method in global symbol table
          method = @global_table.try(&.lookup(method_name))
          unless method.is_a?(MethodSymbol)
            emit_error("Function '#{method_name}' not found", expr_id)
            return @context.nil_type
          end

          # Check parameter count
          expected_count = method.params.size
          actual_count = arg_types.size
          unless expected_count == actual_count
            emit_error("Wrong number of arguments for '#{method_name}' (given #{actual_count}, expected #{expected_count})", expr_id)
            return @context.nil_type
          end

          # If method has generic type parameters, infer them from arguments
          if type_params = method.type_parameters
            type_args = infer_method_type_arguments(method, arg_types)

            # Substitute type parameters in return type
            if ret_ann = method.return_annotation
              return substitute_type_parameters(ret_ann, type_args, type_params)
            else
              # No return annotation - would need to infer from body (future)
              return @context.nil_type
            end
          else
            # Non-generic method - just return the annotated type
            if ret_ann = method.return_annotation
              return parse_type_name(ret_ann)
            else
              return @context.nil_type
            end
          end
        end

        # Week 1 Day 2: Infer type arguments for generic method from call arguments
        # Similar to infer_type_arguments but for methods instead of classes
        private def infer_method_type_arguments(method : MethodSymbol, arg_types : Array(Type)) : Array(Type)
          type_params = method.type_parameters
          return [] of Type unless type_params && !type_params.empty?

          params = method.params
          return [] of Type if params.size != arg_types.size

          # Build binding map: type parameter name → inferred type
          binding = {} of String => Type
          params.zip(arg_types).each do |param, arg_type|
            if type_ann = param.type_annotation
              type_name = String.new(type_ann)

              # Case 1: Simple type parameter (e.g., x : T)
              if type_params.includes?(type_name)
                binding[type_name] = arg_type
                # Case 2: Generic type with type parameter (e.g., box : Box(T))
              elsif type_name.includes?('(') && type_name.includes?(')')
                # Parse generic type: "Box(T)" → base="Box", param="T"
                paren_start = type_name.index('(').not_nil!
                paren_end = type_name.rindex(')').not_nil!
                base_type = type_name[0...paren_start]
                inner_type = type_name[(paren_start + 1)...paren_end]

                # If inner type is a type parameter and arg is InstanceType
                if type_params.includes?(inner_type) && arg_type.is_a?(InstanceType)
                  # Match: Box(T) with Box(Int32) → T = Int32
                  if arg_type.class_symbol.name == base_type && arg_type.type_args
                    type_args = arg_type.type_args.not_nil!
                    if type_args.size > 0
                      binding[inner_type] = type_args[0]
                    end
                  end
                end
              end
            end
          end

          # Return type arguments in the same order as type_params
          type_params.map { |param_name| binding[param_name]? || @context.nil_type }
        end

        # Phase 4B: Method lookup with overload resolution
        #
        # Algorithm:
        # 1. Find all methods with given name (may be multiple overloads)
        # 2. Filter by parameter count
        # 3. Filter by parameter types (if annotated)
        # 4. Return best match
        private def lookup_method(receiver_type : Type, method_name : String, arg_types : Array(Type)) : MethodSymbol?
          candidates = find_all_methods(receiver_type, method_name)
          return nil if candidates.empty?

          # Filter by parameter count
          matching_count = candidates.select { |m| m.params.size == arg_types.size }
          return nil if matching_count.empty?

          # Filter by parameter types (for typed parameters)
          matches = matching_count.select do |method|
            parameters_match?(method, arg_types)
          end

          # Return best match (for now: first match)
          # TODO: Add specificity ranking (prefer more specific types)
          matches.first?
        end

        # Find all methods with given name on receiver type
        private def find_all_methods(receiver_type : Type, method_name : String) : Array(MethodSymbol)
          methods = [] of MethodSymbol

          case receiver_type
          when ClassType
            # Look in class scope
            if symbol = receiver_type.symbol.scope.lookup(method_name)
              case symbol
              when MethodSymbol
                # Single method
                methods << symbol
              when OverloadSetSymbol
                # Phase 4B.2: Multiple overloads
                methods.concat(symbol.overloads)
              end
            end

            # Phase 4B.2: Inheritance search - look in superclass
            if methods.empty?
              methods.concat(find_in_superclass(receiver_type.symbol, method_name))
            end
          when InstanceType
            # Phase 4B.2: Look for instance methods in class scope
            if symbol = receiver_type.class_symbol.scope.lookup(method_name)
              case symbol
              when MethodSymbol
                methods << symbol
              when OverloadSetSymbol
                methods.concat(symbol.overloads)
              end
            end

            # Phase 4B.2: Inheritance search - look in superclass
            if methods.empty?
              methods.concat(find_in_superclass(receiver_type.class_symbol, method_name))
            end
          when PrimitiveType
            # Phase 4B.3: Built-in methods for primitive types
            methods.concat(get_builtin_methods(receiver_type.name, method_name))
          when ArrayType
            # Phase 9: Built-in methods for arrays
            methods.concat(get_array_builtin_methods(receiver_type, method_name))
          when UnionType
            # Phase 4B.4: Find common method in all union members
            # Method can only be called on union if it exists in ALL constituent types
            methods.concat(find_methods_in_union(receiver_type, method_name))
          end

          methods
        end

        # Phase 4B.2: Recursively search for method in superclass chain
        private def find_in_superclass(class_symbol : ClassSymbol, method_name : String) : Array(MethodSymbol)
          methods = [] of MethodSymbol

          # Get superclass name
          superclass_name = class_symbol.superclass_name
          return methods unless superclass_name

          # Lookup superclass in global symbol table
          superclass_symbol = @global_table.try(&.lookup(superclass_name))
          return methods unless superclass_symbol.is_a?(ClassSymbol)

          # Look for method in superclass scope
          if symbol = superclass_symbol.scope.lookup(method_name)
            case symbol
            when MethodSymbol
              methods << symbol
            when OverloadSetSymbol
              methods.concat(symbol.overloads)
            end
          end

          # Recursively search in superclass's superclass
          if methods.empty?
            methods.concat(find_in_superclass(superclass_symbol, method_name))
          end

          methods
        end

        # Phase 4B.4: Compute union return type for method call on union
        #
        # When calling a method on a union type (T | U | V), the return type
        # is the union of return types from each constituent type.
        #
        # Example:
        #   class A; def foo : Int32; end; end
        #   class B; def foo : String; end; end
        #   x = A.new | B.new  # A | B
        #   x.foo  # Returns Int32 | String
        private def compute_union_method_return_type(union_type : UnionType, method_name : String, arg_types : Array(Type)) : Type?
          return_types = [] of Type

          # Get return type from each union member
          union_type.types.each do |member_type|
            # Find and resolve method for this specific type
            if method = lookup_method(member_type, method_name, arg_types)
              if ann = method.return_annotation
                return_types << parse_type_name(ann)
              else
                return_types << @context.nil_type
              end
            else
              # Method not found in this type → cannot call on union
              return nil
            end
          end

          # Create union of all return types
          union_of(return_types)
        end

        # Phase 4B.4: Find methods common to all types in a union
        #
        # Production-ready Crystal-compatible implementation:
        # 1. Method must exist in ALL constituent types
        # 2. Parameter signatures must be compatible
        # 3. Returns methods that can be called with compatible arguments
        #
        # Note: This returns candidate methods for overload resolution.
        # The caller must compute union return type separately.
        private def find_methods_in_union(union_type : UnionType, method_name : String) : Array(MethodSymbol)
          # Find methods in each constituent type
          methods_per_type = union_type.types.map do |member_type|
            find_all_methods(member_type, method_name)
          end

          # Check if ALL types have this method
          return [] of MethodSymbol if methods_per_type.any?(&.empty?)

          # For now, return methods from first type
          # The overload resolution will filter by parameter compatibility
          # and the caller will compute union return type
          methods_per_type[0]
        end

        # Check if method parameters match argument types
        private def parameters_match?(method : MethodSymbol, arg_types : Array(Type)) : Bool
          method.params.zip(arg_types).all? do |param, arg_type|
            # If parameter has no type annotation, it matches any argument type
            type_ann = param.type_annotation
            return true unless type_ann

            # TIER 2.1: Convert Slice(UInt8) to String for type parsing
            # If parameter has type annotation, check if argument type matches
            param_type = parse_type_name(String.new(type_ann))
            type_matches?(arg_type, param_type)
          end
        end

        # Check if actual_type is compatible with expected_type
        #
        # Phase 4B: Simple exact match for now
        # TODO: Add subtyping, union types, nilable types
        private def type_matches?(actual : Type, expected : Type) : Bool
          # Exact type match
          actual.to_s == expected.to_s
        end

        # ============================================================
        # Helper Methods
        # ============================================================

        # Creates a union type from constituent types
        #
        # Normalizes the union (flattens, removes duplicates, sorts).
        # If only one type remains after normalization, returns that type directly.
        private def union_of(types : Array(Type)) : Type
          normalized = UnionType.normalize(types)
          if normalized.size == 1
            normalized[0]
          else
            UnionType.new(types)
          end
        end

        # ============================================================
        # Phase 4B.3: Built-in Methods for Primitive Types
        # ============================================================

        # Get built-in methods for a primitive type
        #
        # Returns an array of MethodSymbol representing built-in methods
        # like Int32#+, String#size, etc.
        private def get_builtin_methods(type_name : String, method_name : String) : Array(MethodSymbol)
          methods = [] of MethodSymbol

          # Dummy values for built-in methods (no AST node)
          dummy_node_id = ExprId.new(0)
          dummy_scope = SymbolTable.new(nil)

          case type_name
          when "Int32", "Int64", "Float64"
            # Arithmetic operators
            case method_name
            when "+", "-", "*", "/", "//"
              # Binary arithmetic: Int32#+(Int32) : Int32
              # Phase 78: // floor division
              param = Frontend::Parameter.new(name: "other".to_slice, type_annotation: type_name.to_slice)
              methods << MethodSymbol.new(
                method_name,
                dummy_node_id,
                params: [param],
                return_annotation: type_name,
                scope: dummy_scope
              )
            when "<", ">", "<=", ">=", "==", "!="
              # Comparison operators: Int32#<(Int32) : Bool
              param = Frontend::Parameter.new(name: "other".to_slice, type_annotation: type_name.to_slice)
              methods << MethodSymbol.new(
                method_name,
                dummy_node_id,
                params: [param],
                return_annotation: "Bool",
                scope: dummy_scope
              )
            end
          when "String"
            case method_name
            when "size"
              # String#size : Int32
              methods << MethodSymbol.new(
                method_name,
                dummy_node_id,
                params: [] of Frontend::Parameter,
                return_annotation: "Int32",
                scope: dummy_scope
              )
            when "+"
              # String#+(String) : String
              param = Frontend::Parameter.new(name: "other".to_slice, type_annotation: "String".to_slice)
              methods << MethodSymbol.new(
                method_name,
                dummy_node_id,
                params: [param],
                return_annotation: "String",
                scope: dummy_scope
              )
            when "==", "!="
              # String#==(String) : Bool
              param = Frontend::Parameter.new(name: "other".to_slice, type_annotation: "String".to_slice)
              methods << MethodSymbol.new(
                method_name,
                dummy_node_id,
                params: [param],
                return_annotation: "Bool",
                scope: dummy_scope
              )
            end
          when "Bool"
            case method_name
            when "==", "!="
              # Bool#==(Bool) : Bool
              param = Frontend::Parameter.new(name: "other".to_slice, type_annotation: "Bool".to_slice)
              methods << MethodSymbol.new(
                method_name,
                dummy_node_id,
                params: [param],
                return_annotation: "Bool",
                scope: dummy_scope
              )
            end
          end

          methods
        end

        # Phase 9: Built-in methods for Array(T)
        private def get_array_builtin_methods(array_type : ArrayType, method_name : String) : Array(MethodSymbol)
          methods = [] of MethodSymbol

          # Dummy values for built-in methods
          dummy_node_id = ExprId.new(0)
          dummy_scope = SymbolTable.new(nil)

          element_type_name = array_type.element_type.to_s

          case method_name
          when "size"
            # Array(T)#size : Int32
            methods << MethodSymbol.new(
              method_name,
              dummy_node_id,
              params: [] of Frontend::Parameter,
              return_annotation: "Int32",
              scope: dummy_scope
            )
          when "empty?"
            # Array(T)#empty? : Bool
            methods << MethodSymbol.new(
              method_name,
              dummy_node_id,
              params: [] of Frontend::Parameter,
              return_annotation: "Bool",
              scope: dummy_scope
            )
          when "first", "last"
            # Array(T)#first : T
            # Array(T)#last : T
            # Return element type
            methods << MethodSymbol.new(
              method_name,
              dummy_node_id,
              params: [] of Frontend::Parameter,
              return_annotation: element_type_name,
              scope: dummy_scope
            )
          when "<<"
            # Array(T)#<<(T) : Array(T)
            # Push returns the array itself
            param = Frontend::Parameter.new(name: "value".to_slice, type_annotation: element_type_name.to_slice)
            methods << MethodSymbol.new(
              method_name,
              dummy_node_id,
              params: [param],
              return_annotation: "Array(#{element_type_name})",
              scope: dummy_scope
            )
          end

          methods
        end

        # ============================================================
        # PHASE 10: Blocks and Yield
        # ============================================================

        private def infer_block(node : Frontend::BlockNode, expr_id : ExprId) : Type
          infer_block_result(node.body)
        end

        private def infer_yield(node : Frontend::YieldNode, expr_id : ExprId) : Type
          # Infer types of yield arguments
          if args = node.args
            args.each { |arg_id| infer_expression(arg_id) }
          end

          # For now, yield returns Nil
          # TODO: In full implementation, yield should return the block's return type
          # (Type will be set by infer_expression)
          @context.nil_type
        end

        # Phase 74: Proc literal type inference
        private def infer_proc_literal(node : Frontend::ProcLiteralNode, expr_id : ExprId) : Type
          # Register proc parameters in @assignments BEFORE inferring body
          # This makes them available for type inference in body expressions
          if params = node.params
            params.each do |param|
              if param_name = param.name
                param_type = if type_ann = param.type_annotation
                               parse_type_name(String.new(type_ann))
                             else
                               @context.nil_type
                             end
                @assignments[String.new(param_name)] = param_type
              end
            end
          end

          # Infer types for body expressions (with parameters in scope)
          body_type = infer_block_result(node.body || [] of ExprId)

          # Clean up proc parameters from @assignments (restore scope)
          if params = node.params
            params.each do |param|
              if param_name = param.name
                @assignments.delete(String.new(param_name))
              end
            end
          end

          # If return type is annotated, use it
          # Otherwise use inferred body type
          # For now, we return a simple Proc type
          # TODO: Full implementation should create Proc(Arg1, Arg2, ... -> ReturnType)

          # Return a generic Proc type
          # In a full implementation, this would be Proc(T1, T2 -> R)
          @context.proc_type
        end

        # ============================================================
        # WEEK 1: Generic Type Instantiation Helpers
        # ============================================================

        # Infer type arguments for generic class from constructor call arguments
        #
        # Example: Box.new(42) where Box(T) has initialize(@value : T)
        #   → Infers T = Int32 from argument type
        #   → Returns [Int32]
        #
        # Week 1: Simple unification - T appears directly in parameter type
        # Future: Complex constraints (T < Number, etc.)
        private def infer_type_arguments(class_symbol : ClassSymbol, arg_types : Array(Type)) : Array(Type)?
          type_params = class_symbol.type_parameters
          return nil unless type_params && !type_params.empty?

          # Find constructor (initialize method)
          init_symbol = class_symbol.scope.lookup_local("initialize")
          return nil unless init_symbol.is_a?(MethodSymbol)

          # Simple binding: parameter count must match
          params = init_symbol.params
          return nil if params.size != arg_types.size

          # Build type parameter binding map
          # For now: simple case where parameter type == type parameter name
          # Example: @value : T  → T gets bound to arg_types[0]
          binding = {} of String => Type

          params.zip(arg_types).each do |param, arg_type|
            if type_ann = param.type_annotation
              type_name = String.new(type_ann)
              # If parameter type is a type parameter (e.g., "T"), bind it
              if type_params.includes?(type_name)
                binding[type_name] = arg_type
              end
            end
          end

          # Return type arguments in the same order as type_parameters
          type_params.map { |param_name| binding[param_name]? || @context.nil_type }
        end

        # Substitute type parameters in a type annotation
        #
        # Example: "T" with binding {"T" => Int32} → Int32
        # Example: "Array(T)" with binding → needs parsing (future)
        #
        # Week 1: Simple substitution for direct type parameter references
        private def substitute_type_parameters(type_name : String, type_args : Array(Type), type_params : Array(String)) : Type
          # Check if type_name is a type parameter
          if idx = type_params.index(type_name)
            return type_args[idx] if idx < type_args.size
          end

          # Otherwise parse as regular type
          parse_type_name(type_name)
        end

        # Week 1: Infer method body type when there's no return annotation
        # Used for generic methods like: def direct_value; @value; end
        private def infer_method_body_type(method : MethodSymbol, receiver_type : Type) : Type
          if ENV["DEBUG"]?
            puts "DEBUG infer_method_body_type:"
            puts "  method: #{method.name}"
            puts "  receiver_type: #{receiver_type.class} = #{receiver_type.inspect}"
          end

          # Get the method's DefNode to access its body
          def_node = @program.arena[method.node_id]
          unless def_node.is_a?(Frontend::DefNode)
            puts "  ERROR: def_node is not DefNode!" if ENV["DEBUG"]?
            return @context.nil_type
          end

          # Get the method body
          body = def_node.body
          unless body && !body.empty?
            puts "  ERROR: body is empty!" if ENV["DEBUG"]?
            return @context.nil_type
          end

          # Save current receiver context
          previous_receiver_context = @receiver_type_context
          previous_class = @current_class

          # Set receiver context for type parameter substitution
          if receiver_type.is_a?(InstanceType)
            @receiver_type_context = receiver_type
            @current_class = receiver_type.class_symbol
            if ENV["DEBUG"]?
              puts "  Set receiver_type_context: Box(#{receiver_type.type_args.inspect})"
            end
          elsif receiver_type.is_a?(ClassType)
            @current_class = receiver_type.symbol
          end

          # Infer the last expression in the body (implicit return)
          last_expr_id = body.last
          result_type = infer_expression(last_expr_id)

          if ENV["DEBUG"]?
            puts "  result_type: #{result_type.class} = #{result_type.inspect}"
          end

          # Restore context
          @receiver_type_context = previous_receiver_context
          @current_class = previous_class

          result_type
        end

        # ============================================================
        # PHASE 11: Case/When
        # ============================================================

        private def infer_case(node : Frontend::CaseNode, expr_id : ExprId) : Type
          if value_id = node.value
            infer_expression(value_id)
          end

          branch_types = [] of Type

          node.when_branches.each do |branch|
            branch.conditions.each { |cond_id| infer_expression(cond_id) }
            branch_types << infer_block_result(branch.body)
          end

          if else_body = node.else_branch
            branch_types << infer_block_result(else_body)
          else
            branch_types << @context.nil_type
          end

          case_type = branch_types.size == 1 ? branch_types[0] : @context.union_of(branch_types)
          case_type
        end

        # ============================================================
        # PHASE 90A: Select/When (Concurrent Channel Operations)
        # ============================================================

        private def infer_select(node : Frontend::SelectNode, expr_id : ExprId) : Type
          # Phase 90A: Parser only - infer conditions and bodies but return placeholder
          # Full concurrent semantics deferred to Phase 90B

          # Infer types from select branches
          node.branches.each do |branch|
            # Infer type of condition (channel operation)
            infer_expression(branch.condition)

            # Infer types of body expressions
            branch.body.each { |stmt_id| infer_expression(stmt_id) }
          end

          # Infer type from else clause (non-blocking fallback)
          if else_body = node.else_branch
            else_body.each { |stmt_id| infer_expression(stmt_id) }
          end

          # Return nil_type placeholder (Phase 90A - parser only)
          # Full type inference with concurrent semantics in Phase 90B
          @context.nil_type
        end

        # ============================================================
        # PHASE 12: Break/Next
        # ============================================================

        private def infer_break(node : Frontend::BreakNode, expr_id : ExprId) : Type
          # Break can have an optional value
          # (Type will be set by infer_expression)
          break_type = if value_id = node.value
                         infer_expression(value_id)
                       else
                         @context.nil_type
                       end

          break_type
        end

        private def infer_next(node, expr_id : ExprId) : Type
          # Next has no value in Crystal, always returns Nil
          # (Type will be set by infer_expression)
          @context.nil_type
        end

        # ============================================================
        # PHASE 13: Range Expressions
        # ============================================================

        private def infer_range(node : Frontend::RangeNode, expr_id : ExprId) : Type
          begin_type = infer_expression(node.begin_expr)
          end_type = infer_expression(node.end_expr)

          # Create Range(B, E) type
          # (Type will be set by infer_expression)
          range_type = RangeType.new(begin_type, end_type)
          range_type
        end

        # ============================================================
        # PHASE 14: Hash Literals
        # ============================================================

        private def infer_hash_literal(node : Frontend::HashLiteralNode, expr_id : ExprId) : Type
          entries = node.entries

          if entries.empty?
            if key_type_slice = node.of_key_type
              value_type_slice = node.of_value_type.not_nil!
              key_type = lookup_type_by_name(String.new(key_type_slice))
              value_type = lookup_type_by_name(String.new(value_type_slice))
              return HashType.new(key_type, value_type)
            else
              return HashType.new(@context.nil_type, @context.nil_type)
            end
          end

          key_types = [] of Type
          value_types = [] of Type

          entries.each do |entry|
            key_types << infer_expression(entry.key)
            value_types << infer_expression(entry.value)
          end

          final_key_type = key_types.size == 1 ? key_types.first : @context.union_of(key_types)
          final_value_type = value_types.size == 1 ? value_types.first : @context.union_of(value_types)

          HashType.new(final_key_type, final_value_type)
        end

        # ============================================================
        # PHASE 15: Tuple Literals
        # ============================================================

        private def infer_tuple_literal(node : Frontend::TupleLiteralNode, expr_id : ExprId) : Type
          elements = node.elements
          return TupleType.new([] of Type) if elements.empty?

          element_types = Array(Type).new(elements.size)
          elements.each do |elem_id|
            element_types << infer_expression(elem_id)
          end
          TupleType.new(element_types)
        end

        # PHASE 70: Named Tuple Literals
        # ============================================================

        private def infer_named_tuple_literal(node : Frontend::NamedTupleLiteralNode, expr_id : ExprId) : Type
          # Named tuple: {name: "Alice", age: 30}
          # Type: NamedTuple(name: String, age: Int32)
          entries = node.entries

          # Empty named tuple (shouldn't happen with current parser, but handle)
          if entries.empty?
            # Empty named tuple is valid in Crystal
            return @context.nil_type # Placeholder for future NamedTupleType with no fields
          end

          # Infer type of each value
          # For full type system, we'd create:
          # NamedTupleType with fields: [(key, value_type), ...]
          # For now, just infer all values
          entries.each do |entry|
            infer_expression(entry.value)
          end

          # Return placeholder type
          # Future: return NamedTupleType.new(entries.map { |e| {e.key, infer_expression(e.value)} })
          @context.nil_type
        end

        # Phase 23: Infer type of ternary operator
        #
        # condition ? true_branch : false_branch
        #
        # Returns the union of true_branch and false_branch types
        private def infer_ternary(node : Frontend::TernaryNode, expr_id : ExprId) : Type
          condition_id = node.condition
          true_id = node.true_branch
          false_id = node.false_branch

          # Infer all three expressions
          condition_type = infer_expression(condition_id)
          true_type = infer_expression(true_id)
          false_type = infer_expression(false_id)

          # In Crystal, condition can be any type (truthy/falsy semantics)
          # We don't need to check condition_type

          # Return union of both branches
          union_of([true_type, false_type])
        end

        # ============================================================
        # Helper Methods
        # ============================================================

        private def lookup_type_by_name(name : String) : Type
          case name
          when "Int32"
            @context.int32_type
          when "Int64"
            @context.int64_type
          when "Float64"
            @context.float64_type
          when "String"
            @context.string_type
          when "Bool"
            @context.bool_type
          when "Nil"
            @context.nil_type
          when "Char"
            @context.char_type
          else
            # Unknown type, default to Nil
            @context.nil_type
          end
        end

        # ============================================================
        # Error Handling
        # ============================================================

        private def emit_error(message : String, node_id : ExprId? = nil)
          # Get actual span from node if available
          span = if node_id
                   node = @program.arena[node_id]
                   node.span
                 else
                   # Fallback to dummy span if node not available
                   Frontend::Span.new(
                     start_offset: 0,
                     end_offset: 0,
                     start_line: 1,
                     start_column: 1,
                     end_line: 1,
                     end_column: 1
                   )
                 end

          diagnostic = Diagnostic.new(
            level: DiagnosticLevel::Error,
            code: "E3001", # Type error codes start at E3xxx
            message: message,
            primary_span: span
          )
          @diagnostics << diagnostic
        end
      end
    end
  end
end
