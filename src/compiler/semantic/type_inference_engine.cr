require "./types/type_context"
require "./types/type_index"
require "./types/type"
require "./types/primitive_type"
require "./types/class_type"
require "./types/instance_type"
require "./types/union_type"
require "./types/array_type"
require "./types/range_type"
require "./types/hash_type"
require "./types/tuple_type"
require "./types/named_tuple_type"
require "./types/proc_type"
require "./types/pointer_type"
require "./types/module_type"
require "./types/enum_type"
require "./types/virtual_type"
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
        @current_module : ModuleSymbol?        # Track current module for resolving module (self) methods
        @receiver_type_context : InstanceType? # Week 1: Track receiver's instance type for generic method body inference
        @depth : Int32
        MAX_DEPTH = 512

        @debug_enabled : Bool

        # Cache method body return types to avoid repeated/recursive inference loops
        @method_body_cache : Hash(MethodSymbol, Type) = {} of MethodSymbol => Type
        @method_body_in_progress : Set(MethodSymbol) = Set(MethodSymbol).new

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
          @current_module = nil
          @receiver_type_context = nil
          @depth = 0
          @debug_enabled = ENV["TYPE_INFERENCE_DEBUG"]? == "1"
        end

        # Debug helper
        private def debug(msg : String)
          STDERR.puts "[TYPE_INFERENCE_DEBUG] #{msg}" if @debug_enabled
        end

        # Centralized watchdog guard to ensure we don't miss deadlines inside
        # long-running helper methods.
        private def guard_watchdog!
          Frontend::Watchdog.check!
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
          # Guard against runaway inference when watchdog is enabled
          Frontend::Watchdog.check!

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

          # Re-check inside the recursive path to catch long-running branches
          Frontend::Watchdog.check!

          result_type = if node.is_a?(Frontend::SplatNode)
                          @context.nil_type
                        else
                          case Frontend.node_kind(node)
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

          end

          # Always set type for this expression (some infer_* methods already do this,
          # but this ensures ALL expressions have types set, even for nested calls)
          @context.set_type(expr_id, result_type)
          result_type
        rescue OverflowError
          debug("overflow while inferring expr #{expr_id}") if @debug_enabled
          @context.nil_type
        ensure
          @depth -= 1
        end

        private def infer_splat(node : Frontend::SplatNode, expr_id : ExprId) : Type
          # Placeholder until splat typing is implemented; keep inference total.
          @context.nil_type
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
            # DO NOT process block body as children in iterative phase!
            # Block parameters need to be set up by the caller (e.g., try, map, each)
            # before the body can be correctly typed. Processing here would infer
            # block param identifiers as Nil before they're properly bound.
            # The recursive infer_block path handles this correctly.
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
            # OPTIMIZATION: For large arrays, only add sample elements to stack
            # Full array inference will be done in infer_array_literal with sampling
            if elems = node.elements
              if elems.size > 10
                # Large array - only sample first 3 elements for stack processing
                3.times { |i| children << elems[i] if i < elems.size }
              else
                # Small array - process all elements
                elems.each { |e| children << e }
              end
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
          # Ensure long-running per-node work still honors the watchdog
          Frontend::Watchdog.check!

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
          when Frontend::CharNode
            @context.char_type
          when Frontend::RegexNode
            @context.regex_type
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
          # Use NumberKind from lexer/parser - Phase 103C: full numeric type support
          case node.kind
          when Frontend::NumberKind::I8
            @context.int8_type
          when Frontend::NumberKind::I16
            @context.int16_type
          when Frontend::NumberKind::I32
            @context.int32_type
          when Frontend::NumberKind::I64
            @context.int64_type
          when Frontend::NumberKind::I128
            @context.int128_type
          when Frontend::NumberKind::U8
            @context.uint8_type
          when Frontend::NumberKind::U16
            @context.uint16_type
          when Frontend::NumberKind::U32
            @context.uint32_type
          when Frontend::NumberKind::U64
            @context.uint64_type
          when Frontend::NumberKind::U128
            @context.uint128_type
          when Frontend::NumberKind::F32
            @context.float32_type
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

          debug("infer_identifier: name = '#{identifier_name}' (expr_id=#{expr_id} object_id=#{identifier_name.object_id})")
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
          when ModuleSymbol
            # Reference to module → ModuleType
            # Module itself is a value (for calling module methods like Utils.helper)
            ModuleType.new(symbol)
          when EnumSymbol
            # Reference to enum → EnumType
            EnumType.new(symbol)
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
          guard_watchdog!

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
          guard_watchdog!

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
          guard_watchdog!

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
          module_name = String.new(node.name)
          previous_module = @current_module

          # Prefer nested lookup inside current module, otherwise fall back to global table
          if previous_module
            if sym = previous_module.scope.lookup(module_name)
              @current_module = sym if sym.is_a?(ModuleSymbol)
            end
          end
          if @current_module.nil?
            if sym = @global_table.try(&.lookup(module_name))
              @current_module = sym if sym.is_a?(ModuleSymbol)
            end
          end

          (node.body || [] of ExprId).each do |body_expr_id|
            infer_expression(body_expr_id)
          end

          # Restore previous module context
          @current_module = previous_module

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
          guard_watchdog!

          # Handle nilable syntax: T? = T | Nil
          if name.ends_with?("?") && name.size > 1
            base_name = name[0...-1]  # Remove trailing ?
            base_type = parse_type_name(base_name)
            return union_of([base_type, @context.nil_type])
          end

          # Handle union syntax: T | U | V
          if name.includes?(" | ")
            parts = name.split(" | ").map(&.strip)
            types = parts.map { |p| parse_type_name(p) }
            return union_of(types)
          end

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
            # Try resolving scoped names (Time::Span) by full path
            if symbol = resolve_scoped_symbol(name)
              case symbol
              when ClassSymbol
                return InstanceType.new(symbol)
              when ModuleSymbol
                return PrimitiveType.new(name)
              end
            end
            # Try finding class by last segment anywhere in global table
            if symbol = find_class_symbol_by_suffix(name)
              return InstanceType.new(symbol)
            end

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
            end

            # As a fallback, create a nominal primitive type to avoid Nil/Unknown
            PrimitiveType.new(name)
          end
        end

        # Resolve a scoped name like Folding::Core::Protein against the global symbol table.
        private def resolve_scoped_symbol(name : String) : Symbol?
          guard_watchdog!

          return nil unless table = @global_table
          segments = name.split("::")
          return nil if segments.empty?
          current_table = table
          current_symbol : Symbol? = nil

          segments.each do |seg|
            sym = current_table.try(&.lookup(seg))
            return nil unless sym
            current_symbol = sym
            case sym
            when ModuleSymbol
              current_table = sym.scope
            when ClassSymbol
              current_table = sym.scope
            when EnumSymbol
              current_table = sym.scope
            else
              current_table = nil
            end
          end

          current_symbol
        end

        # Find a class symbol by matching the rightmost segment across global table (fallback).
        private def find_class_symbol_by_suffix(name : String) : ClassSymbol?
          guard_watchdog!

          suffix = name.includes?("::") ? name.split("::").last : name
          return nil unless suffix
          return nil unless table = @global_table
          queue = [table]
          visited = Set(SymbolTable).new
          max_nodes = 2000
          nodes_seen = 0
          while current = queue.shift?
            guard_watchdog!
            next if visited.includes?(current)
            visited << current
            nodes_seen += 1
            break if nodes_seen > max_nodes

            current.each_local_symbol do |_k, sym|
              case sym
              when ClassSymbol
                return sym if sym.name == suffix
                queue << sym.scope if sym.scope
              when ModuleSymbol
                queue << sym.scope if sym.scope
              end
            end
            current.included_modules.each do |mod|
              queue << mod.scope
            end
          end
          nil
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
          guard_watchdog!
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
          guard_watchdog!

          condition_id = node.condition
          condition_type = infer_expression(condition_id)

          # Crystal allows any type as condition (truthy check: nil and false are falsy)
          # No need to require Bool type

          # Phase 95: Extract flow narrowing from is_a? condition
          narrowing = extract_is_a_narrowing(condition_id)

          # Phase 96: Extract nil narrowing from truthy check (if x where x : T | Nil)
          nil_narrowing = narrowing.nil? ? extract_nil_narrowing(condition_id, condition_type) : nil

          # Apply narrowing for then-branch
          if narrowing
            var_name, narrowed_type = narrowing
            @flow_narrowings[var_name] = narrowed_type
          elsif nil_narrowing
            var_name, narrowed_type = nil_narrowing
            @flow_narrowings[var_name] = narrowed_type
          end

          then_type = infer_block_result(node.then_body)

          # Remove narrowing after then-branch
          if narrowing
            @flow_narrowings.delete(narrowing[0])
          elsif nil_narrowing
            @flow_narrowings.delete(nil_narrowing[0])
          end

          elsif_types = [] of Type
          if elsifs = node.elsifs
            elsifs.each do |elsif_branch|
              branch_condition = elsif_branch.condition
              branch_condition_type = infer_expression(branch_condition)

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

          # Phase 96: Apply negative narrowing for else branch
          else_narrowing = compute_else_narrowing(narrowing, nil_narrowing, condition_type)
          if else_narrowing
            var_name, narrowed_type = else_narrowing
            @flow_narrowings[var_name] = narrowed_type
          end

          else_type = node.else_body ? infer_block_result(node.else_body.not_nil!) : @context.nil_type

          # Remove else narrowing
          if else_narrowing
            @flow_narrowings.delete(else_narrowing[0])
          end

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

        # Phase 96: Extract nil narrowing from truthy check
        # If condition is a simple identifier with nilable type, narrow to non-nil in then branch
        # Returns {variable_name, narrowed_type} or nil if not applicable
        private def extract_nil_narrowing(condition_id : ExprId, condition_type : Type) : {String, Type}?
          condition_node = @program.arena[condition_id]

          # Only narrow if condition is a simple variable (identifier)
          return nil unless condition_node.is_a?(Frontend::IdentifierNode)

          var_name = String.new(condition_node.name)

          # Check if type includes Nil
          narrowed = remove_nil_from_type(condition_type)
          return nil if narrowed == condition_type  # No nil to remove

          {var_name, narrowed}
        end

        # Phase 96: Remove Nil from a type, returning the narrowed type
        private def remove_nil_from_type(type : Type) : Type
          case type
          when UnionType
            non_nil_types = type.types.reject { |t| t.to_s == "Nil" }
            if non_nil_types.empty?
              type  # All types were Nil, keep as is
            elsif non_nil_types.size == 1
              non_nil_types.first
            else
              UnionType.new(non_nil_types)
            end
          else
            type  # Not a union, no narrowing possible
          end
        end

        # Phase 96: Compute negative narrowing for else branch
        # After is_a? check, else branch has remaining types
        # After truthy check, else branch has Nil (or false for Bool)
        private def compute_else_narrowing(
          is_a_narrowing : {String, Type}?,
          nil_narrowing : {String, Type}?,
          condition_type : Type
        ) : {String, Type}?
          # Case 1: is_a? check - else branch gets remaining types
          if is_a_narrowing
            var_name, matched_type = is_a_narrowing
            # Get original type of variable (before narrowing)
            original_type = lookup_variable_type(var_name)
            return nil unless original_type

            remaining = subtract_type_from_union(original_type, matched_type)
            return nil if remaining == original_type  # No change
            return {var_name, remaining}
          end

          # Case 2: truthy check - else branch gets Nil
          if nil_narrowing
            var_name, _ = nil_narrowing
            return {var_name, @context.nil_type}
          end

          nil
        end

        # Phase 96: Subtract a type from a union, returning remaining types
        private def subtract_type_from_union(original : Type, to_remove : Type) : Type
          case original
          when UnionType
            remaining = original.types.reject { |t| t == to_remove || t.to_s == to_remove.to_s }
            if remaining.empty?
              original  # Can't remove all types
            elsif remaining.size == 1
              remaining.first
            else
              UnionType.new(remaining)
            end
          else
            original  # Not a union, can't subtract
          end
        end

        # Phase 96: Lookup original variable type (before any narrowing)
        private def lookup_variable_type(var_name : String) : Type?
          # Check identifier symbols for local variable or method parameters
          @identifier_symbols.each do |expr_id, symbol|
            case symbol
            when VariableSymbol
              if symbol.name == var_name
                # Try to get inferred type from the type context
                if type = @context.get_type(expr_id)
                  return type
                end
              end
            when MethodSymbol
              # Check method parameters
              symbol.params.each do |param|
                if (param_name = param.name) && String.new(param_name) == var_name && (param_type = param.type_annotation)
                  return parse_type_name(String.new(param_type))
                end
              end
            end
          end

          nil
        end

        # Phase 24: Type inference for unless (similar to if but without elsif)
        private def infer_unless(node : Frontend::UnlessNode) : Type
          guard_watchdog!

          condition_id = node.condition
          condition_type = infer_expression(condition_id)

          # Crystal allows any type as condition (truthy check)
          # No need to require Bool type

          then_type = infer_block_result(node.then_branch)
          else_type = node.else_branch ? infer_block_result(node.else_branch.not_nil!) : @context.nil_type

          union_of([then_type, else_type])
        end

        # Phase 25: Type inference for until loop (inverse of while)
        private def infer_until(node : Frontend::UntilNode) : Type
          guard_watchdog!

          condition_id = node.condition
          condition_type = infer_expression(condition_id)

          # Crystal allows any type as condition (truthy check)
          # No need to require Bool type

          node.body.each { |expr_id| infer_expression(expr_id) }

          @context.nil_type
        end

        # Phase 28/29: Type inference for begin/end blocks with rescue/ensure
        # Begin blocks return the type of the last expression, or Nil if empty
        # With rescue: union of begin body type and all rescue body types
        # Ensure doesn't affect type (always runs but doesn't change return value)
        private def infer_begin(node : Frontend::BeginNode) : Type
          guard_watchdog!

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

        # Phase 66/103C: Type inference for type declaration
        # Handles: x : Type and x : Type = value
        private def infer_type_declaration(node) : Type
          guard_watchdog!

          decl = node.as(Frontend::TypeDeclarationNode)
          var_name = String.new(decl.name)
          type_name = String.new(decl.declared_type)

          # Resolve the declared type
          declared_type = lookup_type_by_name(type_name) || PrimitiveType.new(type_name)

          # If there's an initial value, infer its type (for verification)
          if value_id = decl.value
            infer_expression(value_id)
          end

          # Register variable with declared type for subsequent references
          @assignments[var_name] = declared_type

          # The declaration itself returns Nil (it's a statement, not an expression)
          @context.nil_type
        end

        # Phase 67: Type inference for with context block
        private def infer_with(node : Frontend::WithNode) : Type
          guard_watchdog!

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
          guard_watchdog!

          node.specs.each do |spec|
            if default_value = spec.default_value
              infer_expression(default_value)
            end
          end

          @context.nil_type
        end

        private def infer_while(node : Frontend::WhileNode) : Type
          guard_watchdog!

          condition_id = node.condition
          condition_type = infer_expression(condition_id)

          # Crystal allows any type as condition (truthy check)
          # No need to require Bool type

          node.body.each { |expr_id| infer_expression(expr_id) }

          @context.nil_type
        end

        # Phase 99: for loop
        private def infer_for(node : Frontend::ForNode) : Type
          guard_watchdog!

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
          guard_watchdog!

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
          guard_watchdog!

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
          guard_watchdog!

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
            @context.set_type(target_id, value_type)
          when Frontend::IdentifierNode
            @assignments[String.new(target_node.name)] = value_type
            @context.set_type(target_id, value_type)
          end
          # Phase 14B: Index assignment (h["key"] = value) - no tracking needed,
          # just return value type

          # Assignments return the value type in Crystal
          # Type will be set by infer_expression
          value_type
        end

        # Phase 73: Multiple assignment (a, b = 1, 2)
        private def infer_multiple_assign(node : Frontend::MultipleAssignNode, expr_id : ExprId) : Type
          guard_watchdog!

          targets = node.targets
          value_id = node.value

          # Infer value type (typically a tuple)
          value_type = infer_expression(value_id)

          # Phase 103B: Extract individual types from tuple for each target
          targets.each_with_index do |target_id, idx|
            target_node = @program.arena[target_id]
            if target_node.is_a?(Frontend::IdentifierNode)
              # Extract type for this position
              element_type = case value_type
                             when TupleType
                               # Get type at this index from tuple
                               value_type.element_types[idx]? || @context.nil_type
                             when ArrayType
                               # All elements have same type
                               value_type.element_type
                             else
                               # Fallback to the whole type if not destructurable
                               value_type
                             end
              @assignments[String.new(target_node.name)] = element_type
              @context.set_type(target_id, element_type)
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
          # pointerof(x) returns Pointer(T) where T is the type of x

          # Phase 103C: Infer pointed-to type and return Pointer(T)
          if arg_id = node.args.first?
            pointed_type = infer_expression(arg_id)
            PointerType.new(pointed_type)
          else
            @context.nil_type
          end
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
          # Returns the target type

          # Phase 103C: Resolve target type from type name
          infer_expression(node.expression)
          target_type_name = String.new(node.target_type)

          # Try to resolve the target type
          if target_type = lookup_type_by_name(target_type_name)
            target_type
          else
            # If can't resolve, create a primitive type placeholder
            PrimitiveType.new(target_type_name)
          end
        end

        # Phase 45: as? keyword (safe cast - nilable)
        private def infer_as_question(node : Frontend::AsQuestionNode, expr_id : ExprId) : Type
          # Safe cast: value.as?(Type)
          # Returns Type | Nil (nilable)

          # Phase 103C: Resolve target type and make nilable
          infer_expression(node.expression)
          target_type_name = String.new(node.target_type)

          # Try to resolve the target type
          target_type = if resolved = lookup_type_by_name(target_type_name)
            resolved
          else
            PrimitiveType.new(target_type_name)
          end

          # Return nilable version (target_type | Nil)
          @context.union_of([target_type, @context.nil_type])
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

          # Phase 102: Check for enum member access (Color::Red)
          if enum_type = resolve_enum_member_access(node)
            return enum_type
          end

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

        # Phase 102: Resolve enum member access like Color::Red → EnumType(Color)
        private def resolve_enum_member_access(node : Frontend::PathNode) : Type?
          return nil unless @global_table

          segments = [] of String
          collect_path_segments(node, segments)
          return nil if segments.size < 2

          # Try to find an enum in the path (e.g., Color in Color::Red)
          # Check all prefixes to find enum
          (1...(segments.size)).each do |split|
            prefix = segments[0...split]
            suffix = segments[split..-1]

            # Look up prefix as symbol path
            current_table = @global_table
            enum_symbol : EnumSymbol? = nil

            prefix.each_with_index do |seg, idx|
              return nil unless current_table
              symbol = current_table.lookup(seg)
              return nil unless symbol

              if idx == prefix.size - 1
                # Last segment - check if it's an enum
                if symbol.is_a?(EnumSymbol)
                  enum_symbol = symbol
                end
              else
                # Intermediate - navigate scope
                current_table = case symbol
                when ModuleSymbol then symbol.scope
                when ClassSymbol then symbol.scope
                else nil
                end
              end
            end

            # If we found an enum and suffix is a single member name
            if enum_symbol && suffix.size == 1
              member_name = suffix[0]
              if enum_symbol.members.has_key?(member_name)
                # Return the EnumType (value is an instance of the enum)
                return EnumType.new(enum_symbol)
              end
            end
          end

          nil
        end

        # Phase 47: &. safe navigation operator (returns nilable)
        private def infer_safe_navigation(node : Frontend::SafeNavigationNode, expr_id : ExprId) : Type
          # Safe navigation: receiver&.member
          # Returns member_type | Nil (nilable)
          # If receiver is nil, returns nil without calling method
          # Otherwise, calls method and returns its result

          # Phase 103C: Infer receiver and method, return nilable result
          receiver_type = infer_expression(node.object)
          method_name = String.new(node.member)

          # Strip Nil from receiver for method lookup (since we're safe-navigating)
          non_nil_type = case receiver_type
                         when UnionType
                           # Remove Nil from union for method lookup
                           non_nil_types = receiver_type.types.reject { |t| t.is_a?(PrimitiveType) && t.name == "Nil" }
                           if non_nil_types.size == 1
                             non_nil_types.first
                           elsif non_nil_types.empty?
                             @context.nil_type
                           else
                             UnionType.new(non_nil_types)
                           end
                         else
                           receiver_type
                         end

          # Find method on non-nil type
          methods = find_all_methods(non_nil_type, method_name)

          if method = methods.first?
            result_type = if return_annotation = method.return_annotation
              lookup_type_by_name(return_annotation) || @context.nil_type
            else
              @context.nil_type
            end
            # Return nilable result
            @context.union_of([result_type, @context.nil_type])
          else
            # Method not found - return Nil
            @context.nil_type
          end
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
                              when EnumSymbol
                                # Phase 102: For enum, return scope but last segment will be member lookup
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
          # FAST PATH: Skip element inference entirely when type is explicit
          if of_type_expr_id = node.of_type
            # Phase 103B: Resolve the type expression
            if resolved_type = type_from_type_expr(of_type_expr_id)
              element_type = resolved_type
            else
              # Fallback to Nil if type resolution fails
              element_type = @context.nil_type
            end
            # Case 2: Infer from elements
          elsif elements = node.elements
            if elements.empty?
              # Empty array without type annotation - default to Nil
              # (In real Crystal this would be an error, but we'll allow it for now)
              element_type = @context.nil_type
            else
              # OPTIMIZATION: For large arrays of uniform literals, sample first few elements
              # This handles cases like CACHE = [0x81ceb32c..._u64, ...] with 600+ elements
              if elements.size > 10
                # Sample first 3 elements
                sample_types = Array(Type).new(3)
                3.times do |i|
                  break if i >= elements.size
                  sample_types << infer_expression(elements[i])
                end

                # Check if all samples are the same primitive type
                first_type = sample_types.first?
                if first_type && sample_types.all? { |t| t == first_type } && first_type.is_a?(PrimitiveType)
                  # All samples are same primitive - assume entire array is uniform
                  STDERR.puts("[TI DEBUG] Large array (#{elements.size} elements) - sampled 3, all #{first_type}") if @debug_enabled
                  element_type = first_type
                else
                  # Mixed types - fall back to full inference
                  STDERR.puts("[TI DEBUG] Large array (#{elements.size} elements) - mixed types, full inference") if @debug_enabled
                  tmp = Array(Type).new(elements.size)
                  elements.each { |elem_id| tmp << infer_expression(elem_id) }
                  element_type = @context.union_of(tmp)
                end
              else
                # Small array - infer all elements
                tmp = Array(Type).new(elements.size)
                elements.each { |elem_id| tmp << infer_expression(elem_id) }
                element_type = @context.union_of(tmp)
              end
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
          elsif target_type.is_a?(NamedTupleType)
            # Phase 102: Named tuple symbol access - nt[:key]
            index_node = @program.arena[index_id]
            key_name = case index_node
            when Frontend::SymbolNode
              # Symbol name may include leading colon, strip it
              sym_name = String.new(index_node.name)
              sym_name.lstrip(':')
            when Frontend::StringNode
              String.new(index_node.value)
            else
              emit_error("NamedTuple indexing requires symbol or string key", expr_id)
              return @context.nil_type
            end

            if value_type = target_type.type_for(key_name)
              value_type
            else
              emit_error("NamedTuple has no key '#{key_name}'", expr_id)
              @context.nil_type
            end
          elsif target_type.is_a?(PrimitiveType) && target_type.name == "String"
            # Phase 103C: String indexing
            # String[Int32] → Char, String[Range] → String
            index_type = infer_expression(index_id)
            if index_type.is_a?(RangeType)
              @context.string_type  # String range slice returns String
            else
              @context.char_type  # String[Int32] returns Char
            end
          else
            # Not an array, hash, tuple, named tuple, or string - emit error
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
          # If receiver is a nominal primitive (unknown class), try resolving to class type
          if receiver_type.is_a?(PrimitiveType) && receiver_type.name.includes?("::")
            if sym = resolve_scoped_symbol(receiver_type.name)
              if sym.is_a?(ClassSymbol)
                receiver_type = InstanceType.new(sym)
              end
            end
          end
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

          # Phase 103C: Special handling for universal methods on union types
          # These methods need to see the full union, not be computed per-member
          if receiver_type.is_a?(UnionType)
            case method_name
            when "not_nil!"
              # Strip Nil from union
              non_nil = receiver_type.types.reject { |t| t.is_a?(PrimitiveType) && t.name == "Nil" }
              result = if non_nil.size == 1
                         non_nil.first
                       elsif non_nil.empty?
                         @context.nil_type
                       else
                         UnionType.new(non_nil)
                       end
              @context.set_type(expr_id, result)
              return result
            when "nil?"
              result = @context.bool_type
              @context.set_type(expr_id, result)
              return result
            end
          end

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
                          # Heuristic: for arrays, some methods are no-ops on type
                          if receiver_type.is_a?(ArrayType) && {"to_a", "each", "each_with_index", "map", "collect", "select", "reject", "filter"}.includes?(method_name)
                            receiver_type
                          else
                            @context.nil_type
                          end
                        end

          @context.set_type(expr_id, result_type)
          debug("  infer_member_access returning: #{result_type.class.name}: #{result_type}")
          result_type
        end

        private def infer_call(node : Frontend::CallNode, expr_id : ExprId) : Type
          guard_watchdog!

          # NOTE: Block inference is deferred for methods that need special handling
          # (like `try` on union types) where the block parameter type depends on receiver

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

            # Helper to ensure block is inferred before returning
            infer_block_if_present = ->(result : Type) {
              if block_id = node.block
                infer_expression(block_id)
              end
              result
            }

            # If resolver already bound this identifier to a method (incl. self/class methods), use it
            if symbol = @identifier_symbols[node.callee]?
              debug("  identifier_symbols hit: #{symbol.class.name}") if @debug_enabled
              if @debug_enabled && method_name == "total_energy_breakdown"
                debug("  identifier_symbols[callee]=#{symbol.class.name}")
              end
              if symbol.is_a?(MethodSymbol)
                # Phase 100: Handle generic methods
                if type_params = symbol.type_parameters
                  type_args = infer_method_type_arguments(symbol, arg_types)
                  if ret_ann = symbol.return_annotation
                    return infer_block_if_present.call(substitute_type_parameters(ret_ann, type_args, type_params))
                  end
                elsif ann = symbol.return_annotation
                  return infer_block_if_present.call(parse_type_name(ann))
                end
              elsif symbol.is_a?(ClassSymbol)
                # Calling a class name without .new is likely a missing resolution; fall through
              end
            end
            # Try current class/module scope for class methods (implicit self)
            if current_class = @current_class
              if sym = current_class.scope.lookup(method_name)
                case sym
                when MethodSymbol
                  if ann = sym.return_annotation
                    debug("  current_class scope hit for #{method_name}") if @debug_enabled
                    return infer_block_if_present.call(parse_type_name(ann))
                  end
                when OverloadSetSymbol
                  if overload = sym.overloads.first?
                    if ann = overload.return_annotation
                      debug("  current_class overload hit for #{method_name}") if @debug_enabled
                      return infer_block_if_present.call(parse_type_name(ann))
                    end
                  end
                end
              end
            end
            # Try current module scope (def self.* inside a module)
            if current_module = @current_module
              if sym = current_module.scope.lookup(method_name)
                case sym
                when MethodSymbol
                  if ann = sym.return_annotation
                    debug("  current_module scope hit for #{method_name}") if @debug_enabled
                    return infer_block_if_present.call(parse_type_name(ann))
                  end
                when OverloadSetSymbol
                  if overload = sym.overloads.first?
                    if ann = overload.return_annotation
                      debug("  current_module overload hit for #{method_name}") if @debug_enabled
                      return infer_block_if_present.call(parse_type_name(ann))
                    end
                  end
                end
              end
            end
            # Fallback: search global table for a module/class method with this name
            if method = find_method_in_scope(@global_table, method_name)
              debug("  find_method_in_scope hit for #{method_name}: #{method.class.name}") if @debug_enabled
              if ann = method.return_annotation
                return infer_block_if_present.call(parse_type_name(ann))
              end
            end
            # Lookup method in global scope
            result = infer_top_level_function_call(method_name, arg_types, expr_id)
            return infer_block_if_present.call(result)
          else
            debug("  UNKNOWN callee type - returning Nil!")
            return @context.nil_type
          end

          return @context.nil_type unless receiver_type && method_name

          # If receiver is unknown (Nil placeholder), use heuristics and skip lookup
          if receiver_type.is_a?(PrimitiveType) && receiver_type.name == "Nil"
            case method_name
            when "map", "collect", "select", "reject", "filter", "to_a", "each", "each_with_index"
              return ArrayType.new(@context.nil_type)
            else
              return @context.nil_type
            end
          end

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
              # Find block - can be in node.block or node.args
              map_block_node : Frontend::BlockNode? = nil
              if blk_id = node.block
                map_block_node = @program.arena[blk_id].as(Frontend::BlockNode)
              elsif node.args.first?
                arg_node = @program.arena[node.args.first]
                if arg_node.is_a?(Frontend::BlockNode)
                  map_block_node = arg_node
                end
              end

              if block = map_block_node
                # Set up block parameter with element type
                if (params = block.params) && (first_param = params.first?) && (name_slice = first_param.name)
                  param_name = String.new(name_slice)
                  old_assignment = @assignments[param_name]?
                  @assignments[param_name] = elem_type
                  # Clear cached types
                  block.body.each do |body_id|
                    @context.expression_types.delete(body_id)
                    body_node = @program.arena[body_id]
                    case body_node
                    when Frontend::BinaryNode
                      @context.expression_types.delete(body_node.left)
                      @context.expression_types.delete(body_node.right)
                    when Frontend::MemberAccessNode
                      @context.expression_types.delete(body_node.object)
                    end
                  end
                  mapped = infer_block_result(block.body) || elem_type
                  if old_assignment
                    @assignments[param_name] = old_assignment
                  else
                    @assignments.delete(param_name)
                  end
                end
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

          # Phase 103C: Special handling for universal methods on union types in calls
          if receiver_type.is_a?(UnionType)
            case method_name
            when "try"
              # try(&block) on nilable: call block with non-nil value, return block_result | Nil
              non_nil = receiver_type.types.reject { |t| t.is_a?(PrimitiveType) && t.name == "Nil" }
              non_nil_type = if non_nil.size == 1
                               non_nil.first
                             elsif non_nil.empty?
                               @context.nil_type
                             else
                               UnionType.new(non_nil)
                             end
              # Try to infer block result type
              block_result = @context.nil_type
              # Short block form &.method is passed as first arg (BlockNode), not via node.block
              block_node : Frontend::BlockNode? = nil
              if blk_id = node.block
                block_node = @program.arena[blk_id].as(Frontend::BlockNode)
              elsif node.args.first?
                arg_node = @program.arena[node.args.first]
                if arg_node.is_a?(Frontend::BlockNode)
                  block_node = arg_node
                end
              end

              if block = block_node
                # Block receives the non-nil type as implicit parameter
                if (params = block.params) && (first_param = params.first?) && (name_slice = first_param.name)
                  param_name = String.new(name_slice)
                  old_assignment = @assignments[param_name]?
                  @assignments[param_name] = non_nil_type
                  # Clear any cached types for the param identifier
                  block.body.each do |body_id|
                    @context.expression_types.delete(body_id)
                    # Also clear nested expressions that might reference the param
                    inner_node = @program.arena[body_id]
                    if inner_node.is_a?(Frontend::MemberAccessNode)
                      @context.expression_types.delete(inner_node.object)
                    end
                  end
                  block_result = infer_block_result(block.body) || @context.nil_type
                  if old_assignment
                    @assignments[param_name] = old_assignment
                  else
                    @assignments.delete(param_name)
                  end
                else
                  # Short block form &.method without explicit param
                  if body_id = block.body.first?
                    body_node = @program.arena[body_id]
                    if body_node.is_a?(Frontend::MemberAccessNode)
                      member_name = String.new(body_node.member)
                      if method = lookup_method(non_nil_type, member_name, [] of Type)
                        if ann = method.return_annotation
                          block_result = parse_type_name(ann)
                        end
                      end
                    end
                  end
                end
              end
              return @context.union_of([block_result, @context.nil_type])
            when "not_nil!"
              # Strip Nil from union
              non_nil = receiver_type.types.reject { |t| t.is_a?(PrimitiveType) && t.name == "Nil" }
              result = if non_nil.size == 1
                         non_nil.first
                       elsif non_nil.empty?
                         @context.nil_type
                       else
                         UnionType.new(non_nil)
                       end
              return result
            when "nil?"
              return @context.bool_type
            end

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
            # Ensure block type is inferred even for unknown methods
            if block_id = node.block
              infer_expression(block_id)
            end
            @context.nil_type
          end
        end

        # Helper: Ensure block is inferred for call with block
        private def ensure_block_inferred(node : Frontend::CallNode)
          if block_id = node.block
            infer_expression(block_id)
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

          # Check parameter count (accounting for default values, splat, double_splat)
          actual_count = arg_types.size
          required_count = count_required_params(method.params)
          has_splat = method.params.any? { |p| p.is_splat || p.is_double_splat }
          max_count = has_splat ? Int32::MAX : method.params.count { |p| !p.is_block }

          unless actual_count >= required_count && actual_count <= max_count
            if required_count == max_count
              emit_error("Wrong number of arguments for '#{method_name}' (given #{actual_count}, expected #{required_count})", expr_id)
            else
              emit_error("Wrong number of arguments for '#{method_name}' (given #{actual_count}, expected #{required_count}..#{max_count})", expr_id)
            end
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
          required_count = count_required_params(params)
          # Allow fewer args than params if defaults exist
          return [] of Type if arg_types.size < required_count || arg_types.size > params.size

          # Build binding map: type parameter name → inferred type
          binding = {} of String => Type
          arg_types.each_with_index do |arg_type, i|
            param = params[i]
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

        # Depth-first search for a method name in the given scope (SymbolTable)
        private def find_method_in_scope(scope : SymbolTable?, name : String) : MethodSymbol?
          return nil unless scope
          if sym = scope.lookup(name)
            case sym
            when MethodSymbol
              return sym
            when OverloadSetSymbol
              return sym.overloads.first?
            end
          end
          scope.each_local_symbol do |_sym_name, sym|
            case sym
            when ModuleSymbol, ClassSymbol
              if found = find_method_in_scope(sym.scope, name)
                return found
              end
            end
          end
          nil
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

          # Filter by parameter count (accounting for default values, splat, double_splat)
          actual_count = arg_types.size
          matching_count = candidates.select do |m|
            required = count_required_params(m.params)
            has_splat = m.params.any? { |p| p.is_splat || p.is_double_splat }
            max = has_splat ? Int32::MAX : m.params.count { |p| !p.is_block }
            actual_count >= required && actual_count <= max
          end
          return nil if matching_count.empty?

          # Filter by parameter types (for typed parameters)
          matches = matching_count.select do |method|
            parameters_match?(method, arg_types)
          end

          return nil if matches.empty?
          return matches.first if matches.size == 1

          # Phase 98: Specificity ranking - prefer more specific overload
          #
          # Sort by specificity score (highest first) and return best match
          # Ties are resolved by order of definition (first defined wins)
          matches.max_by { |m| specificity_score(m, arg_types) }
        end

        # Find all methods with given name on receiver type
        private def find_all_methods(receiver_type : Type, method_name : String) : Array(MethodSymbol)
          methods = [] of MethodSymbol

          case receiver_type
          when ClassType
            # Look in class_scope for class methods (def self.*)
            if symbol = receiver_type.symbol.class_scope.lookup(method_name)
              case symbol
              when MethodSymbol
                # Single method
                methods << symbol
              when OverloadSetSymbol
                # Phase 4B.2: Multiple overloads
                methods.concat(symbol.overloads)
              end
            end

            # Phase 4B.2: Inheritance search - look in superclass class_scope
            if methods.empty?
              methods.concat(find_in_superclass(receiver_type.symbol, method_name, class_methods: true))
            end
          when ModuleType
            # Phase 102: Look for module methods (def self.* inside module)
            # Module methods are stored in the module's scope
            if symbol = receiver_type.symbol.scope.lookup(method_name)
              case symbol
              when MethodSymbol
                methods << symbol
              when OverloadSetSymbol
                methods.concat(symbol.overloads)
              end
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

            # If class scope is empty (symbols not loaded), try global table using class name
            if methods.empty? && (table = @global_table)
              if global_sym = table.lookup(receiver_type.class_symbol.name)
                if global_sym.is_a?(ClassSymbol)
                  if sym = global_sym.scope.lookup(method_name)
                    case sym
                    when MethodSymbol
                      methods << sym
                    when OverloadSetSymbol
                      methods.concat(sym.overloads)
                    end
                  end
                end
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
          when HashType
            # Phase 103C: Built-in methods for hashes
            methods.concat(get_hash_builtin_methods(receiver_type, method_name))
          when UnionType
            # Phase 4B.4: Find common method in all union members
            # Method can only be called on union if it exists in ALL constituent types
            methods.concat(find_methods_in_union(receiver_type, method_name))
          when VirtualType
            # Phase 99: Virtual type method dispatch
            # Find method in base class and all known subclasses
            methods.concat(find_methods_in_virtual(receiver_type, method_name))
          when ProcType
            # Phase 101: Built-in methods for procs
            methods.concat(get_proc_builtin_methods(receiver_type, method_name))
          end

          # Phase 103C: Universal methods available on all types
          if methods.empty?
            methods.concat(get_universal_methods(receiver_type, method_name))
          end

          methods
        end

        # Phase 103C: Universal methods available on all types (Object methods)
        private def get_universal_methods(receiver_type : Type, method_name : String) : Array(MethodSymbol)
          methods = [] of MethodSymbol
          dummy_node_id = ExprId.new(0)
          dummy_scope = SymbolTable.new(nil)

          case method_name
          when "nil?"
            # T#nil? : Bool - returns true only if receiver is Nil
            methods << MethodSymbol.new(method_name, dummy_node_id, params: [] of Frontend::Parameter, return_annotation: "Bool", scope: dummy_scope)
          when "not_nil!"
            # T#not_nil! : T - returns receiver with Nil stripped from union, or same type
            # For union types with Nil, return the non-nil part
            return_type = case receiver_type
                          when UnionType
                            non_nil = receiver_type.types.reject { |t| t.is_a?(PrimitiveType) && t.name == "Nil" }
                            if non_nil.size == 1
                              non_nil.first.to_s
                            elsif non_nil.empty?
                              "Nil"
                            else
                              non_nil.map(&.to_s).join(" | ")
                            end
                          else
                            receiver_type.to_s
                          end
            methods << MethodSymbol.new(method_name, dummy_node_id, params: [] of Frontend::Parameter, return_annotation: return_type, scope: dummy_scope)
          when "to_s"
            methods << MethodSymbol.new(method_name, dummy_node_id, params: [] of Frontend::Parameter, return_annotation: "String", scope: dummy_scope)
          when "hash"
            methods << MethodSymbol.new(method_name, dummy_node_id, params: [] of Frontend::Parameter, return_annotation: "UInt64", scope: dummy_scope)
          when "class"
            methods << MethodSymbol.new(method_name, dummy_node_id, params: [] of Frontend::Parameter, return_annotation: "Class", scope: dummy_scope)
          when "=~"
            # Pattern matching operator
            param = Frontend::Parameter.new(name: "pattern".to_slice, type_annotation: "Regex".to_slice)
            methods << MethodSymbol.new(method_name, dummy_node_id, params: [param], return_annotation: "Bool", scope: dummy_scope)
          when "try"
            # T#try(&block) : U | Nil - calls block with self, returns result or nil
            # For nilable types, returns nil if receiver is nil
            methods << MethodSymbol.new(method_name, dummy_node_id, params: [] of Frontend::Parameter, return_annotation: "Nil", scope: dummy_scope)
          end

          methods
        end

        # Phase 4B.2: Recursively search for method in superclass chain and included modules
        # class_methods: true searches in class_scope (for def self.*), false searches in scope (for instance methods)
        private def find_in_superclass(class_symbol : ClassSymbol, method_name : String, *, class_methods : Bool = false, visited = Set(ClassSymbol).new) : Array(MethodSymbol)
          guard_watchdog!

          return [] of MethodSymbol if visited.includes?(class_symbol)
          visited << class_symbol

          methods = [] of MethodSymbol

          # First, search in included modules (MRO: included modules come before superclass)
          target_scope = class_methods ? class_symbol.class_scope : class_symbol.scope
          target_scope.included_modules.each do |mod_symbol|
            if symbol = mod_symbol.scope.lookup_local(method_name)
              case symbol
              when MethodSymbol
                methods << symbol
              when OverloadSetSymbol
                methods.concat(symbol.overloads)
              end
            end
          end

          # If found in modules, return early
          return methods unless methods.empty?

          # Get superclass name
          superclass_name = class_symbol.superclass_name
          return methods unless superclass_name

          # Lookup superclass in global symbol table
          superclass_symbol = @global_table.try(&.lookup(superclass_name))
          return methods unless superclass_symbol.is_a?(ClassSymbol)

          # Look for method in superclass scope (class_scope for class methods, scope for instance methods)
          super_target_scope = class_methods ? superclass_symbol.class_scope : superclass_symbol.scope
          if symbol = super_target_scope.lookup(method_name)
            case symbol
            when MethodSymbol
              methods << symbol
            when OverloadSetSymbol
              methods.concat(symbol.overloads)
            end
          end

          # Recursively search in superclass's superclass (and its included modules)
          if methods.empty?
            methods.concat(find_in_superclass(superclass_symbol, method_name, class_methods: class_methods, visited: visited))
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

        # Phase 99: Find methods in virtual type (base class + all known subclasses)
        #
        # Virtual type method dispatch collects methods from:
        # 1. The base class itself
        # 2. All known subclasses (for polymorphic dispatch)
        #
        # Returns methods from base class (subclass overrides have same signature)
        private def find_methods_in_virtual(virtual_type : VirtualType, method_name : String) : Array(MethodSymbol)
          methods = [] of MethodSymbol
          base_class = virtual_type.base_class

          # Find method in base class
          if symbol = base_class.scope.lookup(method_name)
            case symbol
            when MethodSymbol
              methods << symbol
            when OverloadSetSymbol
              methods.concat(symbol.overloads)
            end
          end

          # If not found in base, search superclass chain
          if methods.empty?
            methods.concat(find_in_superclass(base_class, method_name))
          end

          # For virtual dispatch, we need to also check subclasses
          # to ensure the method exists and compute union return type
          # The subclass methods are collected but not returned separately
          # (caller computes union of return types from all overrides)
          if (table = @global_table) && !methods.empty?
            subclass_methods = find_method_overrides_in_subclasses(base_class, method_name, table)
            # We don't add subclass methods to candidates (same signature as base)
            # but we could track them for return type union computation
          end

          methods
        end

        # Find all overrides of a method in known subclasses
        #
        # Used for virtual type return type computation
        private def find_method_overrides_in_subclasses(base_class : ClassSymbol, method_name : String, table : SymbolTable) : Array(MethodSymbol)
          overrides = [] of MethodSymbol
          visited = Set(String).new
          visited << base_class.name

          # Scan all symbols in global table to find subclasses
          # (This is O(n) but cached; could be optimized with subclass index)
          table.each_local_symbol do |name, sym|
            next if visited.includes?(name)
            next unless sym.is_a?(ClassSymbol)

            # Check if this is a subclass of base_class
            if is_subclass_of?(sym, base_class.name, table, visited)
              # Look for method override
              if method_sym = sym.scope.lookup(method_name)
                case method_sym
                when MethodSymbol
                  overrides << method_sym
                when OverloadSetSymbol
                  overrides.concat(method_sym.overloads)
                end
              end
            end
          end

          overrides
        end

        # Check if class_symbol is a subclass of parent_name
        private def is_subclass_of?(class_symbol : ClassSymbol, parent_name : String, table : SymbolTable, visited : Set(String)) : Bool
          current = class_symbol.superclass_name
          while current
            return true if current == parent_name
            if visited.includes?(current)
              return false
            end
            visited << current

            if sym = table.lookup(current)
              if sym.is_a?(ClassSymbol)
                current = sym.superclass_name
              else
                break
              end
            else
              break
            end
          end
          false
        end

        # Check if method parameters match argument types
        # Accounts for default parameter values
        private def parameters_match?(method : MethodSymbol, arg_types : Array(Type)) : Bool
          params = method.params
          required_count = count_required_params(params)

          # Check argument count (allow fewer args if defaults exist)
          return false if arg_types.size < required_count
          return false if arg_types.size > params.size

          # Check each provided argument matches its parameter
          arg_types.each_with_index do |arg_type, i|
            param = params[i]
            type_ann = param.type_annotation
            next unless type_ann  # No annotation means any type matches

            param_type = parse_type_name(String.new(type_ann))
            return false unless type_matches?(arg_type, param_type)
          end

          true
        end

        # Check if actual_type is compatible with expected_type
        #
        # Phase 98: Enhanced type matching with subtyping and union support
        #
        # Matching rules:
        # 1. Exact match (Int32 matches Int32)
        # 2. Subtype match (Child matches Parent)
        # 3. Union member match (String matches String | Int32)
        # 4. Nil matches nilable type (Nil matches T?)
        private def type_matches?(actual : Type, expected : Type) : Bool
          # Exact type match
          return true if actual.to_s == expected.to_s

          # If expected is a union, actual must match at least one member
          if expected.is_a?(UnionType)
            return expected.types.any? { |t| type_matches?(actual, t) }
          end

          # If actual is a union, all members must match expected
          if actual.is_a?(UnionType)
            return actual.types.all? { |t| type_matches?(t, expected) }
          end

          # Subtype check: actual is subtype of expected
          is_subtype?(actual, expected)
        end

        # Check if child_type is a subtype of parent_type (class inheritance)
        private def is_subtype?(child : Type, parent : Type) : Bool
          child_name = case child
                       when InstanceType  then child.class_symbol.name
                       when ClassType     then child.symbol.name
                       when PrimitiveType then child.name
                       when VirtualType   then child.base_class.name
                       else return false
                       end

          parent_name = case parent
                        when InstanceType  then parent.class_symbol.name
                        when ClassType     then parent.symbol.name
                        when PrimitiveType then parent.name
                        when VirtualType   then parent.base_class.name
                        else return false
                        end

          # Walk the inheritance chain
          current_name = child_name
          visited = Set(String).new

          while current_name && !visited.includes?(current_name)
            return true if current_name == parent_name
            visited << current_name

            # Look up class in global table to find superclass
            if (table = @global_table) && (sym = table.lookup(current_name))
              if sym.is_a?(ClassSymbol)
                current_name = sym.superclass_name
              else
                break
              end
            else
              break
            end
          end

          false
        end

        # Calculate specificity score for a method match
        #
        # Higher score = more specific match
        # Used to select the best overload when multiple match
        #
        # Scoring:
        # - Exact type match: 2 points per parameter
        # - Subtype match: 1 point per parameter
        # - Union/generic match: 0 points per parameter
        private def specificity_score(method : MethodSymbol, arg_types : Array(Type)) : Int32
          score = 0
          method.params.zip(arg_types) do |param, arg_type|
            type_ann = param.type_annotation
            next unless type_ann

            param_type = parse_type_name(String.new(type_ann))

            if arg_type.to_s == param_type.to_s
              # Exact match: highest score
              score += 2
            elsif is_subtype?(arg_type, param_type)
              # Subtype match: medium score
              score += 1
            end
            # Union/generic: 0 points
          end
          score
        end

        # ============================================================
        # Helper Methods
        # ============================================================

        # Creates a union type from constituent types
        #
        # Normalizes the union (flattens, removes duplicates, sorts).
        # If only one type remains after normalization, returns that type directly.
        private def union_of(types : Array(Type)) : Type
          guard_watchdog!

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
            when "size", "bytesize"
              # String#size : Int32, String#bytesize : Int32
              methods << MethodSymbol.new(
                method_name,
                dummy_node_id,
                params: [] of Frontend::Parameter,
                return_annotation: "Int32",
                scope: dummy_scope
              )
            when "empty?", "blank?", "ascii_only?"
              # String#empty? : Bool
              methods << MethodSymbol.new(
                method_name,
                dummy_node_id,
                params: [] of Frontend::Parameter,
                return_annotation: "Bool",
                scope: dummy_scope
              )
            when "upcase", "downcase", "capitalize", "strip", "chomp", "reverse", "to_s"
              # String#upcase : String
              methods << MethodSymbol.new(
                method_name,
                dummy_node_id,
                params: [] of Frontend::Parameter,
                return_annotation: "String",
                scope: dummy_scope
              )
            when "chars"
              # String#chars : Array(Char)
              methods << MethodSymbol.new(
                method_name,
                dummy_node_id,
                params: [] of Frontend::Parameter,
                return_annotation: "Array(Char)",
                scope: dummy_scope
              )
            when "bytes"
              # String#bytes : Array(UInt8)
              methods << MethodSymbol.new(
                method_name,
                dummy_node_id,
                params: [] of Frontend::Parameter,
                return_annotation: "Array(UInt8)",
                scope: dummy_scope
              )
            when "split"
              # String#split(String) : Array(String)
              param = Frontend::Parameter.new(name: "separator".to_slice, type_annotation: "String".to_slice)
              methods << MethodSymbol.new(
                method_name,
                dummy_node_id,
                params: [param],
                return_annotation: "Array(String)",
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
            when "*"
              # String#*(Int32) : String
              param = Frontend::Parameter.new(name: "times".to_slice, type_annotation: "Int32".to_slice)
              methods << MethodSymbol.new(
                method_name,
                dummy_node_id,
                params: [param],
                return_annotation: "String",
                scope: dummy_scope
              )
            when "includes?", "starts_with?", "ends_with?"
              # String#includes?(String) : Bool
              param = Frontend::Parameter.new(name: "str".to_slice, type_annotation: "String".to_slice)
              methods << MethodSymbol.new(
                method_name,
                dummy_node_id,
                params: [param],
                return_annotation: "Bool",
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
            when "to_i", "to_i32"
              methods << MethodSymbol.new(method_name, dummy_node_id, params: [] of Frontend::Parameter, return_annotation: "Int32", scope: dummy_scope)
            when "to_i64"
              methods << MethodSymbol.new(method_name, dummy_node_id, params: [] of Frontend::Parameter, return_annotation: "Int64", scope: dummy_scope)
            when "to_f", "to_f64"
              methods << MethodSymbol.new(method_name, dummy_node_id, params: [] of Frontend::Parameter, return_annotation: "Float64", scope: dummy_scope)
            end
          when "Nil"
            case method_name
            when "nil?"
              # Nil#nil? : Bool (always true)
              methods << MethodSymbol.new(
                method_name,
                dummy_node_id,
                params: [] of Frontend::Parameter,
                return_annotation: "Bool",
                scope: dummy_scope
              )
            when "to_s"
              methods << MethodSymbol.new(method_name, dummy_node_id, params: [] of Frontend::Parameter, return_annotation: "String", scope: dummy_scope)
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
          when "+"
            # Array(T)#+(Array(T)) : Array(T)
            param = Frontend::Parameter.new(name: "other".to_slice, type_annotation: "Array(#{element_type_name})".to_slice)
            methods << MethodSymbol.new(
              method_name,
              dummy_node_id,
              params: [param],
              return_annotation: "Array(#{element_type_name})",
              scope: dummy_scope
            )
          when "includes?"
            # Array(T)#includes?(T) : Bool
            param = Frontend::Parameter.new(name: "value".to_slice, type_annotation: element_type_name.to_slice)
            methods << MethodSymbol.new(
              method_name,
              dummy_node_id,
              params: [param],
              return_annotation: "Bool",
              scope: dummy_scope
            )
          when "compact"
            # Array(T | Nil)#compact : Array(T)
            # Strip Nil from element type
            compacted_type = case array_type.element_type
                             when UnionType
                               non_nil = array_type.element_type.as(UnionType).types.reject { |t|
                                 t.is_a?(PrimitiveType) && t.name == "Nil"
                               }
                               if non_nil.size == 1
                                 non_nil.first.to_s
                               elsif non_nil.empty?
                                 "Nil"
                               else
                                 non_nil.map(&.to_s).join(" | ")
                               end
                             else
                               element_type_name
                             end
            methods << MethodSymbol.new(
              method_name,
              dummy_node_id,
              params: [] of Frontend::Parameter,
              return_annotation: "Array(#{compacted_type})",
              scope: dummy_scope
            )
          when "flatten"
            # Array(Array(T))#flatten : Array(T)
            # Extract inner element type
            flattened_type = case array_type.element_type
                             when ArrayType
                               array_type.element_type.as(ArrayType).element_type.to_s
                             else
                               element_type_name  # Already flat
                             end
            methods << MethodSymbol.new(
              method_name,
              dummy_node_id,
              params: [] of Frontend::Parameter,
              return_annotation: "Array(#{flattened_type})",
              scope: dummy_scope
            )
          when "map"
            # Array(T)#map(&block) : Array(U) - returns Array based on block
            # Simplified: we need block type inference, handled elsewhere
            methods << MethodSymbol.new(
              method_name,
              dummy_node_id,
              params: [] of Frontend::Parameter,
              return_annotation: "Array(#{element_type_name})",
              scope: dummy_scope
            )
          when "select", "reject"
            # Array(T)#select(&block) : Array(T)
            methods << MethodSymbol.new(
              method_name,
              dummy_node_id,
              params: [] of Frontend::Parameter,
              return_annotation: "Array(#{element_type_name})",
              scope: dummy_scope
            )
          when "each"
            # Array(T)#each(&block) : Array(T)
            methods << MethodSymbol.new(
              method_name,
              dummy_node_id,
              params: [] of Frontend::Parameter,
              return_annotation: "Array(#{element_type_name})",
              scope: dummy_scope
            )
          when "join"
            # Array(T)#join(String) : String
            param = Frontend::Parameter.new(name: "separator".to_slice, type_annotation: "String".to_slice)
            methods << MethodSymbol.new(
              method_name,
              dummy_node_id,
              params: [param],
              return_annotation: "String",
              scope: dummy_scope
            )
          when "reverse", "sort", "uniq", "shuffle"
            # Array(T)#reverse : Array(T)
            methods << MethodSymbol.new(
              method_name,
              dummy_node_id,
              params: [] of Frontend::Parameter,
              return_annotation: "Array(#{element_type_name})",
              scope: dummy_scope
            )
          when "pop", "shift"
            # Array(T)#pop : T
            methods << MethodSymbol.new(
              method_name,
              dummy_node_id,
              params: [] of Frontend::Parameter,
              return_annotation: element_type_name,
              scope: dummy_scope
            )
          when "any?", "all?", "none?"
            # Array(T)#any? : Bool
            methods << MethodSymbol.new(
              method_name,
              dummy_node_id,
              params: [] of Frontend::Parameter,
              return_annotation: "Bool",
              scope: dummy_scope
            )
          when "index"
            # Array(T)#index(T) : Int32 | Nil
            param = Frontend::Parameter.new(name: "value".to_slice, type_annotation: element_type_name.to_slice)
            methods << MethodSymbol.new(
              method_name,
              dummy_node_id,
              params: [param],
              return_annotation: "Int32 | Nil",
              scope: dummy_scope
            )
          end

          methods
        end

        # Phase 103C: Built-in methods for Hash(K, V)
        private def get_hash_builtin_methods(hash_type : HashType, method_name : String) : Array(MethodSymbol)
          methods = [] of MethodSymbol
          dummy_node_id = ExprId.new(0)
          dummy_scope = SymbolTable.new(nil)

          key_type_name = hash_type.key_type.to_s
          value_type_name = hash_type.value_type.to_s

          case method_name
          when "size"
            methods << MethodSymbol.new(method_name, dummy_node_id, params: [] of Frontend::Parameter, return_annotation: "Int32", scope: dummy_scope)
          when "empty?"
            methods << MethodSymbol.new(method_name, dummy_node_id, params: [] of Frontend::Parameter, return_annotation: "Bool", scope: dummy_scope)
          when "keys"
            methods << MethodSymbol.new(method_name, dummy_node_id, params: [] of Frontend::Parameter, return_annotation: "Array(#{key_type_name})", scope: dummy_scope)
          when "values"
            methods << MethodSymbol.new(method_name, dummy_node_id, params: [] of Frontend::Parameter, return_annotation: "Array(#{value_type_name})", scope: dummy_scope)
          when "has_key?"
            param = Frontend::Parameter.new(name: "key".to_slice, type_annotation: key_type_name.to_slice)
            methods << MethodSymbol.new(method_name, dummy_node_id, params: [param], return_annotation: "Bool", scope: dummy_scope)
          when "has_value?"
            param = Frontend::Parameter.new(name: "value".to_slice, type_annotation: value_type_name.to_slice)
            methods << MethodSymbol.new(method_name, dummy_node_id, params: [param], return_annotation: "Bool", scope: dummy_scope)
          when "[]?"
            param = Frontend::Parameter.new(name: "key".to_slice, type_annotation: key_type_name.to_slice)
            methods << MethodSymbol.new(method_name, dummy_node_id, params: [param], return_annotation: "#{value_type_name} | Nil", scope: dummy_scope)
          when "fetch"
            param = Frontend::Parameter.new(name: "key".to_slice, type_annotation: key_type_name.to_slice)
            methods << MethodSymbol.new(method_name, dummy_node_id, params: [param], return_annotation: value_type_name, scope: dummy_scope)
          when "delete"
            param = Frontend::Parameter.new(name: "key".to_slice, type_annotation: key_type_name.to_slice)
            methods << MethodSymbol.new(method_name, dummy_node_id, params: [param], return_annotation: "#{value_type_name} | Nil", scope: dummy_scope)
          when "clear"
            methods << MethodSymbol.new(method_name, dummy_node_id, params: [] of Frontend::Parameter, return_annotation: "Hash(#{key_type_name}, #{value_type_name})", scope: dummy_scope)
          when "each"
            methods << MethodSymbol.new(method_name, dummy_node_id, params: [] of Frontend::Parameter, return_annotation: "Hash(#{key_type_name}, #{value_type_name})", scope: dummy_scope)
          when "each_key"
            methods << MethodSymbol.new(method_name, dummy_node_id, params: [] of Frontend::Parameter, return_annotation: "Hash(#{key_type_name}, #{value_type_name})", scope: dummy_scope)
          when "each_value"
            methods << MethodSymbol.new(method_name, dummy_node_id, params: [] of Frontend::Parameter, return_annotation: "Hash(#{key_type_name}, #{value_type_name})", scope: dummy_scope)
          when "merge"
            param = Frontend::Parameter.new(name: "other".to_slice, type_annotation: "Hash(#{key_type_name}, #{value_type_name})".to_slice)
            methods << MethodSymbol.new(method_name, dummy_node_id, params: [param], return_annotation: "Hash(#{key_type_name}, #{value_type_name})", scope: dummy_scope)
          when "to_a"
            methods << MethodSymbol.new(method_name, dummy_node_id, params: [] of Frontend::Parameter, return_annotation: "Array(Tuple(#{key_type_name}, #{value_type_name}))", scope: dummy_scope)
          end

          methods
        end

        # Phase 101: Built-in methods for Proc types
        private def get_proc_builtin_methods(proc_type : ProcType, method_name : String) : Array(MethodSymbol)
          methods = [] of MethodSymbol

          dummy_node_id = ExprId.new(0)
          dummy_scope = SymbolTable.new(nil)

          case method_name
          when "call"
            # Proc(T1, T2, ..., R)#call(T1, T2, ...) : R
            # Build params from proc parameter types
            params = proc_type.param_types.map_with_index do |ptype, i|
              Frontend::Parameter.new(name: "arg#{i}".to_slice, type_annotation: ptype.to_s.to_slice)
            end
            return_type_name = proc_type.return_type.to_s

            methods << MethodSymbol.new(
              method_name,
              dummy_node_id,
              params: params,
              return_annotation: return_type_name,
              scope: dummy_scope
            )
          when "arity"
            # Proc#arity : Int32
            methods << MethodSymbol.new(
              method_name,
              dummy_node_id,
              params: [] of Frontend::Parameter,
              return_annotation: "Int32",
              scope: dummy_scope
            )
          end

          methods
        end

        # ============================================================
        # PHASE 10: Blocks and Yield
        # ============================================================

        private def infer_block(node : Frontend::BlockNode, expr_id : ExprId) : Type
          guard_watchdog!

          # Check if block has untyped parameters that need caller context
          # (e.g., short block form &.method with synthesized __arg0 param)
          if (params = node.params)
            params.each do |param|
              if (name_slice = param.name)
                param_name = String.new(name_slice)
                # If param is synthesized (__arg0) and not in assignments, defer to caller
                if param_name.starts_with?("__arg") && !@assignments.has_key?(param_name)
                  return @context.nil_type
                end
              end
            end
          end

          # Regular block - infer body expressions
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
        # Returns ProcType(param_types..., return_type)
        private def infer_proc_literal(node : Frontend::ProcLiteralNode, expr_id : ExprId) : Type
          guard_watchdog!

          # Collect parameter types
          param_types = [] of Type

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
                param_types << param_type
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

          # Determine return type:
          # 1. Use explicit return type annotation if present
          # 2. Otherwise use inferred body type
          # 3. Default to Nil for empty body
          return_type = if rt = node.return_type
                          parse_type_name(String.new(rt))
                        elsif body_type
                          body_type
                        else
                          @context.nil_type
                        end

          # Return proper ProcType with parameter and return types
          ProcType.new(param_types, return_type)
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
          guard_watchdog!

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
        # Count required parameters (those without default values, splat, or double splat)
        private def count_required_params(params : Array(Frontend::Parameter)) : Int32
          params.count { |p| p.default_value.nil? && !p.is_splat && !p.is_double_splat && !p.is_block }
        end

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
          guard_watchdog!

          # If we've already inferred this method body, reuse it to prevent cycles
          if cached = @method_body_cache[method]?
            return cached
          end

          # Break recursive inference cycles gracefully
          if @method_body_in_progress.includes?(method)
            return @context.nil_type
          end

          @method_body_in_progress << method

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

          # Cache the result to avoid re-inferring on subsequent calls
          @method_body_cache[method] = result_type

          if ENV["DEBUG"]?
            puts "  result_type: #{result_type.class} = #{result_type.inspect}"
          end

          # Restore context
          @receiver_type_context = previous_receiver_context
          @current_class = previous_class

          result_type
        ensure
          @method_body_in_progress.delete(method)
        end

        # ============================================================
        # PHASE 11: Case/When
        # ============================================================

        private def infer_case(node : Frontend::CaseNode, expr_id : ExprId) : Type
          guard_watchdog!

          # Phase 97: Case/when type narrowing
          # Get the case value and its type for narrowing
          case_value_id = node.value
          case_value_type : Type? = nil
          case_var_name : String? = nil

          if case_value_id
            case_value_type = infer_expression(case_value_id)
            # Check if case value is a simple identifier (for narrowing)
            case_value_node = @program.arena[case_value_id]
            if case_value_node.is_a?(Frontend::IdentifierNode)
              case_var_name = String.new(case_value_node.name)
            end
          end

          branch_types = [] of Type
          matched_types = [] of Type  # Track types matched by when branches

          node.when_branches.each do |branch|
            # Extract type from when condition for narrowing
            narrowed_type = extract_when_narrowing(branch.conditions, case_value_type)

            # Apply narrowing for this branch
            if case_var_name && narrowed_type
              @flow_narrowings[case_var_name] = narrowed_type
              matched_types << narrowed_type
            end

            branch.conditions.each { |cond_id| infer_expression(cond_id) }
            branch_types << infer_block_result(branch.body)

            # Remove narrowing after branch
            if case_var_name && narrowed_type
              @flow_narrowings.delete(case_var_name)
            end
          end

          # Else branch gets remaining types (if any)
          if else_body = node.else_branch
            if case_var_name && case_value_type && !matched_types.empty?
              remaining = compute_remaining_types(case_value_type, matched_types)
              if remaining
                @flow_narrowings[case_var_name] = remaining
              end
            end

            branch_types << infer_block_result(else_body)

            if case_var_name
              @flow_narrowings.delete(case_var_name)
            end
          else
            branch_types << @context.nil_type
          end

          case_type = branch_types.size == 1 ? branch_types[0] : @context.union_of(branch_types)
          case_type
        end

        # Phase 97: Extract narrowed type from when condition
        # Returns the type to narrow to, or nil if not applicable
        private def extract_when_narrowing(conditions : Array(ExprId), case_type : Type?) : Type?
          return nil unless case_type

          # Check each condition - any type literal narrows the case value
          conditions.each do |cond_id|
            cond_node = @program.arena[cond_id]

            case cond_node
            when Frontend::IdentifierNode
              # when String, when Int32, etc. (type as identifier)
              type_name = String.new(cond_node.name)
              if is_type_name?(type_name)
                return parse_type_name(type_name)
              end
            when Frontend::PathNode
              # when Foo::Bar (namespaced type)
              if type = resolve_path_as_type(cond_node)
                return type
              end
            when Frontend::IsANode
              # when .is_a?(Type) - rarely used in case but possible
              type_name = String.new(cond_node.target_type)
              return parse_type_name(type_name)
            when Frontend::MemberAccessNode
              # when .class (check if it's a .class call on type)
              if member_name = extract_member_name(cond_node)
                if member_name == "class"
                  # This is x.class - the object is the type
                  object_type = infer_expression(cond_node.object)
                  if object_type.is_a?(ClassType)
                    return InstanceType.new(object_type.symbol)
                  end
                end
              end
            end
          end

          nil
        end

        # Phase 97: Check if a name is a type name (capitalized)
        private def is_type_name?(name : String) : Bool
          return false if name.empty?
          first_char = name[0]
          first_char.uppercase? || name == "Nil"
        end

        # Phase 97: Resolve a path node as a type
        private def resolve_path_as_type(node : Frontend::PathNode) : Type?
          segments = [] of String
          collect_path_segments_for_type(node, segments)
          return nil if segments.empty?

          # Try to find the type in global table
          full_name = segments.join("::")
          if symbol = @global_table.try(&.lookup(full_name))
            case symbol
            when ClassSymbol
              return InstanceType.new(symbol)
            end
          end

          # Try segment by segment lookup
          if @global_table
            current : Symbol? = nil
            segments.each_with_index do |seg, idx|
              if idx == 0
                current = @global_table.not_nil!.lookup(seg)
              else
                case current
                when ClassSymbol
                  current = current.scope.lookup(seg)
                when ModuleSymbol
                  current = current.scope.lookup(seg)
                else
                  return nil
                end
              end
              return nil unless current
            end

            if current.is_a?(ClassSymbol)
              return InstanceType.new(current)
            end
          end

          nil
        end

        # Phase 97: Collect path segments from PathNode
        private def collect_path_segments_for_type(node : Frontend::PathNode, segments : Array(String))
          if left_id = node.left
            left_node = @program.arena[left_id]
            case left_node
            when Frontend::PathNode
              collect_path_segments_for_type(left_node, segments)
            when Frontend::IdentifierNode
              segments << String.new(left_node.name)
            end
          end

          right_node = @program.arena[node.right]
          case right_node
          when Frontend::IdentifierNode
            segments << String.new(right_node.name)
          when Frontend::PathNode
            collect_path_segments_for_type(right_node, segments)
          end
        end

        # Phase 97: Compute remaining types after matching some types
        private def compute_remaining_types(original : Type, matched : Array(Type)) : Type?
          case original
          when UnionType
            remaining = original.types.reject do |t|
              matched.any? { |m| t == m || t.to_s == m.to_s }
            end
            return nil if remaining.empty?
            return remaining.first if remaining.size == 1
            UnionType.new(remaining)
          else
            # Non-union type - if matched, nothing remains
            matched.any? { |m| original == m || original.to_s == m.to_s } ? nil : original
          end
        end

        # Phase 97: Extract member name from MemberAccessNode
        private def extract_member_name(node : Frontend::MemberAccessNode) : String?
          if member_slice = node.member
            String.new(member_slice)
          else
            nil
          end
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
              key_type = lookup_type_by_name(String.new(key_type_slice)) || @context.nil_type
              value_type = lookup_type_by_name(String.new(value_type_slice)) || @context.nil_type
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

          # Empty named tuple is valid in Crystal
          if entries.empty?
            return NamedTupleType.new([] of {String, Type})
          end

          # Infer type of each value and build NamedTupleType
          type_entries = [] of {String, Type}
          entries.each do |entry|
            key = String.new(entry.key)
            value_type = infer_expression(entry.value)
            type_entries << {key, value_type}
          end

          NamedTupleType.new(type_entries)
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

        private def lookup_type_by_name(name : String) : Type?
          # Handle nilable shorthand: T? → T | Nil
          if name.ends_with?("?")
            base_name = name[0..-2]
            if base_type = lookup_type_by_name(base_name)
              return @context.union_of([base_type, @context.nil_type])
            end
            return nil
          end

          case name
          when "Int8"
            @context.int8_type
          when "Int16"
            @context.int16_type
          when "Int32"
            @context.int32_type
          when "Int64"
            @context.int64_type
          when "Int128"
            @context.int128_type
          when "UInt8"
            @context.uint8_type
          when "UInt16"
            @context.uint16_type
          when "UInt32"
            @context.uint32_type
          when "UInt64"
            @context.uint64_type
          when "UInt128"
            @context.uint128_type
          when "Float32"
            @context.float32_type
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
          when "Symbol"
            @context.symbol_type
          when "Regex"
            @context.regex_type
          else
            # Unknown type - return nil to let caller decide
            nil
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
