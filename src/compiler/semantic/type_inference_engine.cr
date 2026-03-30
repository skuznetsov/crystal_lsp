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
require "../hir/debug_hooks"
require "./analyzer"
require "../frontend/ast"
require "../../runtime"

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
        @current_method_scope : SymbolTable?   # Track current method scope for generic/module-local annotations
        @receiver_type_context : Type? # Week 1: Track generic receiver context for method body inference
        @depth : Int32
        MAX_DEPTH = 512

        @debug_enabled : Bool

        # Cache method body return types to avoid repeated/recursive inference loops
        @method_body_cache : Hash(MethodSymbol, Type) = {} of MethodSymbol => Type
        @method_body_in_progress : Set(MethodSymbol) = Set(MethodSymbol).new
        @method_return_stack : Array(Array(Type))
        @current_method_is_class_method_stack : Array(Bool)
        @yield_return_stack : Array(Type?)
        @yield_call_stack : Array(Frontend::CallNode?)

        # Instance-level cycle guard for expression inference (prevents infinite recursion)
        @expr_in_progress : Set(Int32) = Set(Int32).new
        @identifier_name_cache : Array(String?)
        @member_name_cache : Array(String?)
        @node_kind_cache : Array(Frontend::NodeKind?)
        @method_candidates_cache : Hash(MethodCandidatesKey, Array(MethodSymbol))
        @parse_type_cache : Hash(String, Type)
        @class_type_cache : Hash(ClassSymbol, ClassType)
        @module_type_cache : Hash(ModuleSymbol, ModuleType)
        @instance_type_cache : Hash(ClassSymbol, InstanceType)
        @method_lookup_cache : Hash(MethodLookupKey, MethodSymbol?)
        @unknown_type : PrimitiveType
        @flags : Set(String)

        private struct MethodCandidatesKey
          getter receiver_id : UInt64
          getter name : String

          def initialize(receiver : Type, @name : String)
            @receiver_id = receiver.object_id
          end

          def hash : UInt64
            receiver_id.hash ^ name.hash
          end

          def ==(other : MethodCandidatesKey) : Bool
            receiver_id == other.receiver_id && name == other.name
          end
        end

        private struct MethodLookupKey
          getter receiver_id : UInt64
          getter name : String
          getter arg_sig : UInt64
          getter has_block : Bool

          def initialize(receiver : Type, @name : String, arg_types : Array(Type), @has_block : Bool)
            @receiver_id = receiver.object_id
            sig = arg_types.size.hash
            arg_types.each do |arg|
              sig ^= arg.object_id.hash
            end
            @arg_sig = sig
          end

          def hash : UInt64
            receiver_id.hash ^ name.hash ^ arg_sig.hash ^ has_block.hash
          end

          def ==(other : MethodLookupKey) : Bool
            receiver_id == other.receiver_id && name == other.name && arg_sig == other.arg_sig && has_block == other.has_block
          end
        end

        def initialize(
          @program : Frontend::Program,
          @identifier_symbols : Hash(ExprId, Symbol),
          @global_table : SymbolTable? = nil,
          @context : TypeContext = TypeContext.new,
          @extra_roots : Array(ExprId) = [] of ExprId,
          @flags : Set(String) = Runtime.target_flags,
        )
          # Self-hosted binaries have shown unstable reads through Program#arena.
          # Type inference only operates on parser-built AstArena programs here.
          @arena = @program.ast_arena
          @diagnostics = [] of Diagnostic
          @assignments = {} of String => Type        # Track variable assignments: name → type
          @instance_var_types = {} of String => Type # Phase 5A: Track instance variable types
          @class_var_types = {} of String => Type    # Track class variable assignments: scoped name → type
          @global_var_types = {} of String => Type   # Track global variable assignments: name → type
          @flow_narrowings = {} of String => Type    # Phase 95: Flow typing - narrowed types in conditionals
          @children_cache = Array(Array(ExprId)?).new(@arena.size)
          @identifier_name_cache = Array(String?).new(@arena.size)
          @member_name_cache = Array(String?).new(@arena.size)
          @node_kind_cache = Array(Frontend::NodeKind?).new(@arena.size)
          @method_candidates_cache = {} of MethodCandidatesKey => Array(MethodSymbol)
          @parse_type_cache = {} of String => Type
          @class_type_cache = {} of ClassSymbol => ClassType
          @module_type_cache = {} of ModuleSymbol => ModuleType
          @instance_type_cache = {} of ClassSymbol => InstanceType
          @method_lookup_cache = {} of MethodLookupKey => MethodSymbol?
          @current_class = nil
          @current_module = nil
          @current_method_scope = nil
          @receiver_type_context = nil
          @method_return_stack = [] of Array(Type)
          @current_method_is_class_method_stack = [] of Bool
          @yield_return_stack = [] of Type?
          @yield_call_stack = [] of Frontend::CallNode?
          @depth = 0
          @debug_enabled = ENV["TYPE_INFERENCE_DEBUG"]? == "1"
          @unknown_type = PrimitiveType.new("Unknown")
        end

        # Debug helper
        private def debug(msg : String)
          STDERR.puts "[TYPE_INFERENCE_DEBUG] #{msg}" if @debug_enabled
          debug_hook("infer.debug", msg)
        end

        private def debug_type_trace_name?(name : String) : Bool
          filter = ENV["DEBUG_TYPE_TRACE_NAMES"]?
          return false unless filter

          names = filter.split(',').map(&.strip).reject(&.empty?)
          names.includes?(name)
        end

        private def debug_type_trace(name : String, message : String)
          return unless debug_type_trace_name?(name)
          STDERR.puts "[TYPE_TRACE] #{message}"
        end

        # Centralized watchdog guard to ensure we don't miss deadlines inside
        # long-running helper methods.
        private def guard_watchdog!
          Frontend::Watchdog.check!
        end

        # Main entry point: Infer types for all root expressions
        def infer_types
          debug_hook("infer.start", "roots=#{analysis_root_count}")
          each_analysis_root do |root_id|
            debug_hook("infer.root", "expr_id=#{root_id}")
            type = begin
                     infer_expression(root_id)
                   rescue OverflowError
                     debug("overflow while inferring root expr #{root_id}") if @debug_enabled
                     @context.nil_type
                   end
            @context.set_type(root_id, type)
          end
          debug_hook("infer.finish", "roots=#{analysis_root_count} diagnostics=#{@diagnostics.size}")
        end

        private def each_analysis_root(& : ExprId ->)
          @program.roots.each { |root_id| yield root_id }
          @extra_roots.each { |root_id| yield root_id }
        end

        private def analysis_root_count : Int32
          @program.roots.size + @extra_roots.size
        end

        # Recursive type inference for a single expression
        private def infer_expression(expr_id : ExprId) : Type
          # Guard against runaway inference when watchdog is enabled
          Frontend::Watchdog.check!

          # Iterative fast path to avoid deep recursion/stack overflows
          if t0 = @context.get_type(expr_id)
            return t0
          end
          # Keep traversal state in scalar stacks; tuple pop? on wrapper structs
          # is still unstable on self-hosted stage2.
          stack_ids = [] of Int32
          stack_visited = [] of Bool
          state = {} of Int32 => Int32
          stack_ids << expr_id.index
          stack_visited << false
          state[expr_id.index] = 1
          until stack_ids.empty?
            # Check watchdog to prevent infinite loops in type inference
            Frontend::Watchdog.check!

            id = ExprId.new(stack_ids.pop)
            visited = stack_visited.pop
            next if @context.get_type(id)
            node = @arena[id]
            if !visited
              stack_ids << id.index
              stack_visited << true
              children_of(id, node).each do |child|
                next if @context.get_type(child)
                case state[child.index]?
                when 2
                  next
                when 1
                  # Cycle detected; assign nil_type to break it
                  # EXCEPTION: Don't set Nil for IdentifierNode - it needs to check @assignments
                  child_node = @arena[child]
                  debug_hook("infer.cycle", "expr_id=#{child} node=#{child_node.class.name}")
                  unless child_node.is_a?(Frontend::IdentifierNode)
                    @context.set_type(child, @context.nil_type)
                  end
                  next
                else
                  state[child.index] = 1
                  stack_ids << child.index
                  stack_visited << false
                end
              end
            else
              # Try to compute type for simple nodes
              if t = compute_node_type_no_recurse(node, id)
                # Successfully computed - mark as done
                if @debug_enabled
                  debug("ITERATIVE: #{last_path_segment(node.class.name)} computed type #{t}")
                end
                @context.set_type(id, t)
                state[id.index] = 2
              else
                # Complex node - skip in iterative path, leave for recursive fallback
                if @debug_enabled
                  debug("ITERATIVE: #{last_path_segment(node.class.name)} returned nil, will use recursive")
                end
                # Clear any type seeded by cycle detection so recursive fallback
                # can recompute the node from scratch.
                @context.expression_types.delete(id)
                state[id.index] = 0 # Reset to allow recursive processing
              end
            end
          end
          if t1 = @context.get_type(expr_id)
            if @debug_enabled
              node = @arena[expr_id]
              debug("ITERATIVE SUCCESS: #{last_path_segment(node.class.name)} got type #{t1}")
            end
            return t1
          end
          # Fallback to recursive implementation below
          if @debug_enabled
            node = @arena[expr_id]
            debug("FALLBACK TO RECURSIVE: #{last_path_segment(node.class.name)}")
          end
          debug_hook("infer.fallback", "expr_id=#{expr_id}")
          if @depth > MAX_DEPTH
            debug("max recursion depth reached at expr #{expr_id}") if @debug_enabled
            debug_hook("infer.max_depth", "expr_id=#{expr_id}")
            return @context.nil_type
          end

          # Instance-level cycle guard: prevent infinite recursion across recursive calls
          expr_idx = expr_id.index
          if @expr_in_progress.includes?(expr_idx)
            debug("cycle detected at expr #{expr_id}, breaking with Nil") if @debug_enabled
            debug_hook("infer.cycle_guard", "expr_id=#{expr_id}")
            return @context.nil_type
          end
          @expr_in_progress.add(expr_idx)

          @depth += 1
          node = @arena[expr_id]

          # Re-check inside the recursive path to catch long-running branches
          Frontend::Watchdog.check!

          result_type = if node.is_a?(Frontend::SplatNode)
                          @context.nil_type
                        else
                          case node_kind_for(expr_id, node)
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
                        when Frontend::NodeKind::Nil
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
                          infer_class_var(node.as(Frontend::ClassVarNode), expr_id)
                        when .class_var_decl?
                          # Phase 77: Class variable declaration (@@var : Type)
                          infer_class_var_decl(node, expr_id)
                        when .global?
                          # Phase 75: Global variables
                          infer_global(node.as(Frontend::GlobalNode), expr_id)
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
        ensure
          @depth -= 1
          @expr_in_progress.delete(expr_id.index)
        end

        private def infer_splat(node : Frontend::SplatNode, expr_id : ExprId) : Type
          # Placeholder until splat typing is implemented; keep inference total.
          @context.nil_type
        end

        # Return direct child ExprIds of a node (used by iterative inference)
        private def children_of(expr_id : ExprId, node) : Array(ExprId)
          idx = expr_id.index
          return [] of ExprId if idx < 0
          if idx < @children_cache.size
            if cached = @children_cache[idx]?
              return cached
            end
          else
            # Arena can grow during macro expansion; keep cache aligned.
            new_size = @arena.size
            if new_size > @children_cache.size
              (new_size - @children_cache.size).times { @children_cache << nil }
            end
          end
          children = [] of ExprId
          case node
          when Frontend::GroupingNode
            children << node.expression
          when Frontend::UnaryNode
            op = Frontend.node_operator_string(node) || ""
            children << node.operand unless op == "->"
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
            # OPTIMIZATION: For large hashes, only add sample entries to stack
            entries = node.entries
            if entries.size > 10
              # Large hash - only sample first 3 key-value pairs
              3.times do |i|
                break if i >= entries.size
                children << entries[i].key
                children << entries[i].value
              end
            else
              entries.each { |entry| children << entry.key; children << entry.value }
            end
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
          @children_cache[idx] = children if idx >= 0 && idx < @children_cache.size
          children
        end

        private def clear_cached_type_tree(expr_id : ExprId, seen : Set(Int32)? = nil) : Nil
          return if expr_id.invalid?

          visited = seen || Set(Int32).new
          return if visited.includes?(expr_id.index)
          visited << expr_id.index

          @context.expression_types.delete(expr_id)
          node = @arena[expr_id]
          children_of(expr_id, node).each do |child_id|
            clear_cached_type_tree(child_id, visited)
          end
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
            elsif op == "->"
              @context.proc_type
            else
              # +x, -x, ~x: return operand's type
              infer_expression(node.operand)
            end
          when Frontend::BinaryNode
            op = Frontend.node_operator_string(node) || ""
            case op
            when "==", "!=", "<", ">", "<=", ">="
              @context.bool_type
            when "&&"
              if control_flow_terminator?(node.right)
                left_type = infer_expression(node.left)
                if falsy = extract_falsy_component(left_type)
                  falsy
                else
                  infer_expression(node.right)
                end
              else
                @context.bool_type
              end
            when "||"
              if control_flow_terminator?(node.right)
                left_type = infer_expression(node.left)
                if truthy = extract_truthy_component(left_type)
                  truthy
                else
                  infer_expression(node.right)
                end
              else
                @context.bool_type
              end
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
            infer_hash_literal(node, expr_id)
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
          # ============================================================
          # FAST PATH: Statements that always return Nil
          # These are definitions/declarations - they don't produce values.
          # Return Nil immediately without recursive fallback.
          # ============================================================
          when Frontend::MacroDefNode,
               Frontend::EnumNode,
               Frontend::LibNode,
               Frontend::AliasNode,
               Frontend::RequireNode,
               Frontend::IncludeNode,
               Frontend::ExtendNode,
               Frontend::AnnotationNode,
               Frontend::AnnotationDefNode,
               Frontend::UnionNode,
               Frontend::MacroLiteralNode,
               Frontend::GlobalVarDeclNode,
               Frontend::MacroIfNode,
               Frontend::MacroForNode,
               Frontend::MacroExpressionNode
            # Pure statements that don't need body processing
            @context.nil_type
          # Note: DefNode, ClassNode, ModuleNode need recursive path
          # because they have special body processing (infer_def, infer_class, etc.)
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
          identifier_name = identifier_name_for(expr_id, node)
          debug_type_trace(
            identifier_name,
            "infer_identifier name=#{identifier_name} expr_id=#{expr_id.index} current_class=#{@current_class.try(&.name)} current_module=#{@current_module.try(&.name)} receiver=#{@receiver_type_context.try(&.to_s)}"
          )

          debug("infer_identifier: name = '#{identifier_name}' (expr_id=#{expr_id} object_id=#{identifier_name.object_id})")
          debug("  @assignments has #{@assignments.size} entries: #{@assignments.keys.inspect}")

          # Phase 95: Check flow narrowings first (type narrowing in conditionals)
          if narrowed_type = @flow_narrowings[identifier_name]?
            debug("  Found in @flow_narrowings: #{narrowed_type}")
            return narrowed_type
          end

          # First, check if this identifier has a tracked assignment
          if assigned_type = @assignments[identifier_name]?
            debug("  Found in @assignments: #{assigned_type}")
            debug_type_trace(identifier_name, "assignment_hit name=#{identifier_name} type=#{assigned_type}")
            return assigned_type
          else
            debug("  NOT found in @assignments")
          end

          # Try name resolution first
          symbol = @identifier_symbols[expr_id]?

          # Fallback to lexical scope lookup if name resolution didn't resolve this identifier
          if symbol.nil?
            symbol = @current_method_scope.try(&.lookup(identifier_name))
          end
          if symbol.nil?
            if current_class = @current_class
              symbol = current_class.scope.lookup(identifier_name) || current_class.class_scope.lookup(identifier_name)
            end
          end
          if symbol.nil?
            if current_module = @current_module
              symbol = current_module.scope.lookup(identifier_name)
            end
          end
          symbol ||= resolve_enclosing_namespace_self(identifier_name)

          # Final fallback to global symbol table lookup if lexical scopes missed
          if symbol.nil?
            symbol = @global_table.try(&.lookup(identifier_name))
          end

          if symbol.nil? && is_type_name?(identifier_name)
            if resolved_symbol = lookup_type_symbol(identifier_name)
              if type = type_from_symbol(resolved_symbol)
                return type
              elsif resolved_symbol.is_a?(EnumSymbol)
                return EnumType.new(resolved_symbol)
              end
            end

            if class_symbol = find_class_symbol_by_suffix(identifier_name)
              return class_type_for(class_symbol)
            end

            if primitive = primitive_type_for(identifier_name)
              return primitive
            end
          end

          debug("  Symbol lookup: #{symbol ? symbol.class.name : "nil"}")
          debug_type_trace(identifier_name, "symbol_lookup name=#{identifier_name} symbol=#{symbol ? symbol.class.name : "nil"}")

          unless symbol
            if result = infer_receiverless_current_context_reference(identifier_name)
              return result
            end
            return @context.nil_type
          end

          case symbol
          when VariableSymbol
            # Explicit type annotation: var : Int32
            if declared_type_name = symbol.declared_type
              resolve_method_annotation_type(declared_type_name, @receiver_type_context, @current_method_scope)
            elsif is_type_name?(identifier_name)
              resolved = resolve_method_annotation_type(identifier_name, @receiver_type_context, @current_method_scope)
              unknownish_type?(resolved) ? unknown_type : resolved
            else
              unknown_type
            end
          when ClassSymbol
            # Reference to class → ClassType
            # Class itself is a value (for calling class methods like Dog.new)
            class_type_for(symbol)
          when ModuleSymbol
            # Reference to module → ModuleType
            # Module itself is a value (for calling module methods like Utils.helper)
            module_type_for(symbol)
          when EnumSymbol
            # Reference to enum → EnumType
            EnumType.new(symbol)
          when ConstantSymbol
            infer_expression(symbol.value)
          when AliasSymbol
            aliased_type = parse_type_name(symbol.target)
            aliased_type.is_a?(EnumType) ? aliased_type : class_type_reference_for(aliased_type)
          when MethodSymbol
            infer_receiverless_method_reference(symbol)
          when OverloadSetSymbol
            if method = resolve_zero_arg_overload(symbol)
              infer_receiverless_method_reference(method)
            else
              @context.nil_type
            end
          else
            @context.nil_type
          end
        end

        private def infer_receiverless_method_reference(method : MethodSymbol) : Type
          if unresolved_generic_module_receiver?(method)
            return @context.nil_type
          end

          return @context.nil_type unless zero_arg_method?(method)

          if ann = method.return_annotation
            receiver_type = implicit_receiver_type_for(method)
            return resolve_method_annotation_type(ann, receiver_type, method.scope)
          end

          if receiver_type = implicit_receiver_type_for(method)
            return infer_method_body_type(method, receiver_type)
          end

          @context.nil_type
        end

        private def infer_receiverless_method_call(
          method : MethodSymbol,
          arg_types : Array(Type),
          has_block : Bool,
          call_node : Frontend::CallNode? = nil
        ) : Type?
          if unresolved_generic_module_receiver?(method)
            return @context.nil_type
          end

          receiver_type = implicit_receiver_type_for(method)
          return nil unless receiver_type

          infer_method_call_result(method, receiver_type, arg_types, call_node)
        end

        private def infer_receiverless_current_context_call(
          method_name : String,
          arg_types : Array(Type),
          has_block : Bool,
          call_node : Frontend::CallNode? = nil
        ) : Type?
          receiver_type =
            if @current_method_is_class_method_stack.last?
              if current_class = @current_class
                class_type_for(current_class)
              elsif current_module = @current_module
                module_type_for(current_module)
              else
                nil
              end
            else
              @receiver_type_context || @current_class.try { |klass| instance_type_for(klass) } || @current_module.try { |mod| module_type_for(mod) }
            end

          return nil unless receiver_type
          method = lookup_method(receiver_type, method_name, arg_types, has_block)
          return nil unless method

          infer_method_call_result(method, receiver_type, arg_types, call_node)
        end

        private def infer_receiverless_current_context_reference(method_name : String) : Type?
          receiver_type =
            if @current_method_is_class_method_stack.last?
              if current_class = @current_class
                class_type_for(current_class)
              elsif current_module = @current_module
                module_type_for(current_module)
              else
                nil
              end
            else
              @receiver_type_context || @current_class.try { |klass| instance_type_for(klass) } || @current_module.try { |mod| module_type_for(mod) }
            end

          return nil unless receiver_type
          method = lookup_method(receiver_type, method_name, [] of Type, false)
          return nil unless method

          infer_method_call_result(method, receiver_type, [] of Type, nil)
        end

        private def infer_receiverless_overload_call(
          symbol : OverloadSetSymbol,
          arg_types : Array(Type),
          has_block : Bool,
          call_node : Frontend::CallNode? = nil
        ) : Type?
          if symbol.overloads.any? { |method| unresolved_generic_module_receiver?(method) }
            return @context.nil_type
          end

          matches = symbol.overloads.select do |method|
            receiver_type = implicit_receiver_type_for(method)
            next false unless receiver_type

            required = count_required_params(method.params)
            has_splat = method.params.any? { |param| param.is_splat || param.is_double_splat }
            max = has_splat ? Int32::MAX : method.params.count { |param| !param.is_block }
            next false unless arg_types.size >= required && arg_types.size <= max

            method_has_block = method.params.any?(&.is_block)
            next false unless method_has_block == has_block

            parameters_match?(method, arg_types, receiver_type)
          end

          return nil if matches.empty?

          selected = if matches.size == 1
                       matches.first
                     else
                       matches.max_by { |method| specificity_score(method, arg_types) }
                     end
          receiver_type = implicit_receiver_type_for(selected)
          return nil unless receiver_type

          infer_method_call_result(selected, receiver_type, arg_types, call_node)
        end

        private def unresolved_generic_module_receiver?(method : MethodSymbol) : Bool
          return false if method.is_class_method?
          return false if @receiver_type_context

          owner_module = method.scope.owner_module
          return false unless owner_module
          return false unless current_module = @current_module
          return false unless owner_module == current_module

          type_params = owner_module.type_parameters
          !!(type_params && !type_params.empty?)
        end

        private def defer_generic_module_method_body?(method : MethodSymbol, receiver_type : Type) : Bool
          return false if method.is_class_method?

          owner_module = method.scope.owner_module
          return false unless owner_module

          type_params = owner_module.type_parameters
          return false unless type_params && !type_params.empty?

          case receiver_type
          when ModuleType
            has_no_type_args = if type_args = receiver_type.type_args
                                 type_args.empty?
                               else
                                 true
                               end
            receiver_type.symbol == owner_module && has_no_type_args
          else
            false
          end
        end

        private def infer_method_call_result(
          method : MethodSymbol,
          receiver_type : Type,
          arg_types : Array(Type),
          call_node : Frontend::CallNode? = nil
        ) : Type
          if type_params = method.type_parameters
            type_args = infer_method_type_arguments(method, receiver_type, arg_types, call_node)
            if ret_ann = method.return_annotation
              return substitute_type_parameters(ret_ann, type_args, type_params)
            end
          elsif ann = method.return_annotation
            return resolve_method_annotation_type(ann, receiver_type, method.scope)
          end

          infer_method_body_type(method, receiver_type, arg_types, call_node)
        end

        private def resolve_zero_arg_overload(symbol : OverloadSetSymbol) : MethodSymbol?
          symbol.overloads.find { |method| zero_arg_method?(method) }
        end

        private def zero_arg_method?(method : MethodSymbol) : Bool
          count_required_params(method.params) == 0
        end

        private def implicit_receiver_type_for(method : MethodSymbol) : Type?
          if method.is_class_method?
            if current_class = @current_class
              return class_type_for(current_class)
            end

            if current_module = @current_module
              return module_type_for(current_module)
            end
          else
            if receiver_type = @receiver_type_context
              return receiver_type
            end

            if current_class = @current_class
              return instance_type_for(current_class)
            end

            if current_module = @current_module
              return module_type_for(current_module)
            end
          end

          nil
        end

        private def identifier_name_for(expr_id : ExprId, node : Frontend::IdentifierNode) : String
          idx = expr_id.index
          if idx >= 0
            if idx >= @identifier_name_cache.size
              new_size = @arena.size
              if new_size > @identifier_name_cache.size
                (new_size - @identifier_name_cache.size).times { @identifier_name_cache << nil }
              end
            elsif cached = @identifier_name_cache[idx]?
              return cached
            end
            name = intern_name(node.name)
            @identifier_name_cache[idx] = name
            return name
          end
          intern_name(node.name)
        end

        private def member_name_for(expr_id : ExprId, node : Frontend::MemberAccessNode | Frontend::SafeNavigationNode) : String
          idx = expr_id.index
          if idx >= 0
            if idx >= @member_name_cache.size
              new_size = @arena.size
              if new_size > @member_name_cache.size
                (new_size - @member_name_cache.size).times { @member_name_cache << nil }
              end
            elsif cached = @member_name_cache[idx]?
              return cached
            end
            name = intern_name(node.member)
            @member_name_cache[idx] = name
            return name
          end
          intern_name(node.member)
        end

        private def node_kind_for(expr_id : ExprId, node : Frontend::Node) : Frontend::NodeKind
          idx = expr_id.index
          if idx >= 0
            if idx >= @node_kind_cache.size
              new_size = @arena.size
              if new_size > @node_kind_cache.size
                (new_size - @node_kind_cache.size).times { @node_kind_cache << nil }
              end
            elsif cached = @node_kind_cache[idx]?
              return cached
            end
            kind = Frontend.node_kind(node)
            @node_kind_cache[idx] = kind
            return kind
          end
          Frontend.node_kind(node)
        end

        private def class_type_for(symbol : ClassSymbol) : ClassType
          @class_type_cache[symbol] ||= ClassType.new(symbol)
        end

        private def module_type_for(symbol : ModuleSymbol, type_args : Array(Type)? = nil) : ModuleType
          return ModuleType.new(symbol, type_args) if type_args && !type_args.empty?
          @module_type_cache[symbol] ||= ModuleType.new(symbol)
        end

        private def instance_type_for(symbol : ClassSymbol, type_args : Array(Type)? = nil) : InstanceType
          # Avoid caching generic instantiations until we have a stable key for type_args.
          return InstanceType.new(symbol, type_args) if type_args && !type_args.empty?
          @instance_type_cache[symbol] ||= InstanceType.new(symbol)
        end

        private def instantiate_class_receiver(receiver_type : ClassType) : Type
          base_name = normalize_class_receiver_name(receiver_type.symbol.name)

          if primitive = primitive_type_for(base_name)
            return primitive
          end

          if type_args = receiver_type.type_args
            case base_name
            when "Array", "Slice", "StaticArray"
              return ArrayType.new(type_args.first? || @context.nil_type)
            when "Hash"
              return HashType.new(type_args[0]? || @context.nil_type, type_args[1]? || @context.nil_type)
            when "Tuple"
              return TupleType.new(type_args)
            end
          end

          instance_type_for(receiver_type.symbol, receiver_type.type_args)
        end

        private def array_like_literal_receiver_type(type : Type) : Type?
          case type
          when ArrayType
            type
          when ClassType
            base_name = normalize_class_receiver_name(type.symbol.name)
            return instantiate_class_receiver(type) if {"Array", "Slice", "StaticArray"}.includes?(base_name)
            nil
          when PrimitiveType
            return nil unless primitive_metaclass?(type)
            value_type = primitive_metaclass_value_type(type)
            value_type.is_a?(ArrayType) ? value_type : nil
          else
            nil
          end
        end

        private def normalize_class_receiver_name(name : String) : String
          name.ends_with?(".class") ? name[0...-6] : name
        end

        private def primitive_instance_class_symbol(type : PrimitiveType) : ClassSymbol?
          return nil if primitive_metaclass?(type)

          symbol = lookup_type_symbol(type.name) || find_class_symbol_by_suffix(type.name)
          symbol.as?(ClassSymbol)
        end

        private def primitive_metaclass?(type : Type) : Bool
          type.is_a?(PrimitiveType) && type.name.ends_with?(".class")
        end

        private def primitive_metaclass_value_type(type : PrimitiveType) : Type?
          base_name = normalize_class_receiver_name(type.name)
          return primitive_type_for(base_name) if primitive = primitive_type_for(base_name)

          resolved = resolve_method_annotation_type(base_name, @receiver_type_context, @current_method_scope)
          unknownish_type?(resolved) ? nil : resolved
        end

        private def class_receiver_type_for_expression(type : Type) : Type
          return type if primitive_metaclass?(type)

          case type
          when PrimitiveType, ArrayType, HashType, TupleType, PointerType, InstanceType
            class_type_reference_for(type)
          else
            type
          end
        end

        private def resolve_enclosing_namespace_self(name : String) : Symbol?
          current_module = @current_module
          while current_module
            return current_module if current_module.name == name
            current_module = enclosing_module_for(current_module.scope)
          end

          nil
        end

        private def type_receiver_expression?(expr_id : ExprId) : Bool
          node = @arena[expr_id]

          case node
          when Frontend::IdentifierNode
            identifier_name = identifier_name_for(expr_id, node)
            symbol = @identifier_symbols[expr_id]?
            symbol ||= @current_method_scope.try(&.lookup(identifier_name))
            if symbol.nil?
              if current_class = @current_class
                symbol = current_class.scope.lookup(identifier_name) || current_class.class_scope.lookup(identifier_name)
              end
            end
            if symbol.nil?
              if current_module = @current_module
                symbol = current_module.scope.lookup(identifier_name)
              end
            end
            symbol ||= resolve_enclosing_namespace_self(identifier_name)
            symbol ||= @global_table.try(&.lookup(identifier_name))

            case symbol
            when ClassSymbol, ModuleSymbol, AliasSymbol
              true
            else
              type_symbol = lookup_type_symbol(identifier_name)
              generic_type_parameter_receiver?(identifier_name) ||
                type_symbol.is_a?(ClassSymbol) ||
                type_symbol.is_a?(ModuleSymbol) ||
                type_symbol.is_a?(AliasSymbol) ||
                type_symbol.is_a?(EnumSymbol) ||
                primitive_type_for(identifier_name) != nil
            end
          when Frontend::PathNode
            if symbol = resolve_path_symbol(node)
              symbol.is_a?(ClassSymbol) || symbol.is_a?(ModuleSymbol) || symbol.is_a?(AliasSymbol)
            else
              false
            end
          when Frontend::GenericNode
            type_from_type_expr(expr_id) != nil
          else
            false
          end
        end

        private def generic_type_parameter_receiver?(identifier_name : String) : Bool
          return false unless is_type_name?(identifier_name)
          return true if @assignments.has_key?(identifier_name)

          if current_class = @current_class
            if type_params = current_class.type_parameters
              return true if type_params.includes?(identifier_name)
            end
          end

          if current_module = @current_module
            if type_params = current_module.type_parameters
              return true if type_params.includes?(identifier_name)
            end
          end

          false
        end

        private def class_type_reference_for(type : Type) : Type
          case type
          when PrimitiveType
            if symbol = lookup_runtime_class_symbol(type.name)
              return class_type_for(symbol)
            end
            if symbol = @global_table.try(&.lookup("#{type.name}.class")).as?(ClassSymbol)
              return class_type_for(symbol)
            end
          when InstanceType
            return ClassType.new(type.class_symbol, type.type_args)
          when ArrayType
            if symbol = lookup_runtime_class_symbol("Array")
              return ClassType.new(symbol, [type.element_type])
            end
          when HashType
            if symbol = lookup_runtime_class_symbol("Hash")
              return ClassType.new(symbol, [type.key_type, type.value_type])
            end
          when TupleType
            if symbol = lookup_runtime_class_symbol("Tuple")
              return ClassType.new(symbol, type.element_types)
            end
          when PointerType
            if symbol = lookup_runtime_class_symbol("Pointer")
              return ClassType.new(symbol, [type.element_type])
            end
          when ClassType
            return type
          end

          PrimitiveType.new("#{type}.class")
        end

        private def intern_name(slice : Slice(UInt8)) : String
          @program.string_pool.intern_string(slice)
        end

        # ============================================================
        # PHASE 5: Classes, Methods, and Instance Variables
        # ============================================================

        # Phase 6: Process method definitions and their bodies
        private def infer_def(node : Frontend::DefNode, expr_id : ExprId) : Type
          guard_watchdog!
          previous_method_scope = @current_method_scope
          method_symbol = current_method_symbol_for(node, expr_id)
          @current_method_scope = current_method_scope_for(node, expr_id)
          @current_method_is_class_method_stack << !!method_symbol.try(&.is_class_method?)

          generic_owner = false
          if current_class = @current_class
            type_params = current_class.type_parameters
            generic_owner ||= !!(type_params && !type_params.empty?)
          end
          if current_module = @current_module
            type_params = current_module.type_parameters
            generic_owner ||= !!(type_params && !type_params.empty?)
          end

          method_is_generic = false
          if method_symbol
            type_params = method_symbol.type_parameters
            method_is_generic = type_params && !type_params.empty?
          end

          has_untyped_params = false
          if params = node.params
            has_untyped_params = params.any? { |param| !param.is_block && param.type_annotation.nil? }
          end

          defer_body_inference = generic_owner || method_is_generic || has_untyped_params

          # Phase 71: Process default parameter values
          if !defer_body_inference && (params = node.params)
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
            # Only infer body eagerly when no generic substitution context is
            # required. Generic class/module owners and generic methods must be
            # deferred until a concrete call site supplies bindings. Methods with
            # untyped parameters also need call-site bindings to avoid seeding
            # stale Nil/Unknown diagnostics from placeholder eager inference.
            unless defer_body_inference
              body.each do |stmt|
                infer_expression(stmt)
              end
            end
          end

          # Method definitions don't have value types (they're statements)
          @context.nil_type
        ensure
          @current_method_is_class_method_stack.pop unless @current_method_is_class_method_stack.empty?
          @current_method_scope = previous_method_scope
        end

        # Phase 5C: Process class bodies and track current class context
        private def infer_class(node : Frontend::ClassNode, expr_id : ExprId) : Type
          guard_watchdog!

          # Look up the ClassSymbol from the symbol table
          class_name = intern_name(node.name)

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

        private def infer_union(node : Frontend::UnionNode, expr_id : ExprId) : Type
          (node.body || [] of ExprId).each do |body_expr_id|
            infer_expression(body_expr_id)
          end

          @context.nil_type
        end

        # Phase 31: Type inference for module definition
        private def infer_module(node : Frontend::ModuleNode, expr_id : ExprId) : Type
          module_name = intern_name(node.name)
          previous_module = @current_module
          previous_class = @current_class

          resolved =
            if previous_module
              previous_module.scope.lookup(module_name)
            elsif current_class = @current_class
              current_class.scope.lookup(module_name)
            else
              @global_table.try(&.lookup(module_name))
            end

          case resolved
          when ModuleSymbol
            @current_module = resolved
          when ClassSymbol
            @current_class = resolved
          end

          (node.body || [] of ExprId).each do |body_expr_id|
            infer_expression(body_expr_id)
          end

          # Restore previous module context
          @current_module = previous_module
          @current_class = previous_class

          # Module definitions don't have value types
          @context.nil_type
        end

        private def current_method_scope_for(node : Frontend::DefNode, expr_id : ExprId) : SymbolTable?
          current_method_symbol_for(node, expr_id).try(&.scope)
        end

        private def current_method_symbol_for(node : Frontend::DefNode, expr_id : ExprId) : MethodSymbol?
          name = intern_name(node.name)

          target_scope =
            if receiver = node.receiver
              if intern_name(receiver) == "self"
                @current_class.try(&.class_scope) || @current_module.try(&.scope)
              else
                nil
              end
            else
              @current_class.try(&.scope) || @current_module.try(&.scope) || @global_table
            end

          return nil unless scope = target_scope

          case symbol = scope.lookup(name)
          when MethodSymbol
            symbol
          when OverloadSetSymbol
            symbol.overloads.find { |entry| entry.node_id == expr_id }
          else
            nil
          end
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

          if body = node.body
            body.each { |expr_id| infer_expression(expr_id) }
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
            name = intern_name(node.name)
            if symbol = resolve_scoped_symbol(name)
              case symbol
              when ConstantSymbol
                infer_expression(symbol.value)
              when EnumSymbol
                EnumType.new(symbol)
              else
                if type = type_from_symbol(symbol)
                  type
                else
                  @context.nil_type
                end
              end
            else
              @context.nil_type
            end
          end
        end

        private def infer_instance_var(node : Frontend::InstanceVarNode, expr_id : ExprId) : Type
          var_name = intern_name(node.name)

          # Remove @ prefix
          clean_name = var_name.starts_with?("@") ? var_name[1..-1] : var_name
          debug_hook("infer.instance_var", "name=#{clean_name} current_class=#{@current_class.try(&.name)} receiver=#{@receiver_type_context.try(&.to_s)}")

          if ENV["DEBUG"]?
            puts "DEBUG infer_instance_var:"
            puts "  var_name: #{clean_name}"
            puts "  @current_class: #{@current_class.try(&.name)}"
            puts "  @receiver_type_context: #{@receiver_type_context.inspect}"
          end

          # Phase 5C: Check explicit type annotation from ClassSymbol first
          if current_class = @current_class
            if type_annotation = current_class.get_instance_var_type(clean_name)
              debug_hook("infer.instance_var.annotated", "name=#{clean_name} type=#{type_annotation}")
              if ENV["DEBUG"]?
                puts "  type_annotation: #{type_annotation}"
              end
              # Week 1: If we have receiver type context (generic instance), substitute type parameters
              if receiver = @receiver_type_context
                if ENV["DEBUG"]?
                  puts "  receiver: #{receiver.inspect}"
                end
                if receiver.is_a?(InstanceType)
                  if ENV["DEBUG"]?
                    puts "  receiver.type_args: #{receiver.type_args.inspect}"
                    puts "  receiver.class_symbol.type_parameters: #{receiver.class_symbol.type_parameters.inspect}"
                  end
                  if (type_args = receiver.type_args) && (type_params = receiver.class_symbol.type_parameters)
                    result = substitute_type_parameters(type_annotation, type_args, type_params)
                    debug_hook("infer.instance_var.substitute", "name=#{clean_name} result=#{result}")
                    if ENV["DEBUG"]?
                      puts "  substituted result: #{result.class} = #{result.inspect}"
                    end
                    return result
                  end
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
            debug_hook("infer.instance_var.inferred", "name=#{clean_name} type=#{inferred_type}")
            return inferred_type
          end

          # Not found - return Nil
          if ENV["DEBUG"]?
            puts "  returning Nil (not found)"
          end
          debug_hook("infer.instance_var.nil", "name=#{clean_name}")
          @context.nil_type
        end

        # Phase 76: Infer type of class variable
        private def infer_class_var(node : Frontend::ClassVarNode, expr_id : ExprId) : Type
          var_name = intern_name(node.name)

          if type = @class_var_types[class_var_key(var_name)]?
            return type
          end

          if symbol = lookup_class_var_symbol(var_name)
            if declared_type = symbol.declared_type
              return parse_type_name(declared_type)
            end
          end

          @context.nil_type
        end

        # Phase 75: Infer type of global variable
        private def infer_global(node : Frontend::GlobalNode, expr_id : ExprId) : Type
          var_name = intern_name(node.name)

          if type = @global_var_types[global_var_key(var_name)]?
            return type
          end

          if symbol = lookup_global_var_symbol(var_name)
            if declared_type = symbol.declared_type
              return parse_type_name(declared_type)
            end
          end

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
          if cached = @parse_type_cache[name]?
            return cached
          end

          if proc_type = parse_proc_type_name(name)
            return @parse_type_cache[name] = proc_type
          end

          # Handle nilable syntax: T? = T | Nil
          if name.ends_with?("?") && name.size > 1
            base_name = name[0...-1]  # Remove trailing ?
            base_type = parse_type_name(base_name)
            return @parse_type_cache[name] = union_of([base_type, @context.nil_type])
          end

          # Handle pointer suffix syntax: T* = Pointer(T)
          if name.ends_with?("*") && name.size > 1
            element_name = name[0...-1]
            element_type = parse_type_name(element_name)
            return @parse_type_cache[name] = PointerType.new(element_type)
          end

          if name.ends_with?(".class") && name.size > 6
            base_name = name[0...-6]
            base_type = parse_type_name(base_name)
            return @parse_type_cache[name] = class_type_reference_for(base_type)
          end

          # Handle union syntax: T | U | V (zero-copy scan, no split)
          if union = parse_union_type_name(name)
            return @parse_type_cache[name] = union
          end

          if tuple_like = parse_brace_collection_type_name(name) { |type_part| parse_type_name(type_part) }
            return @parse_type_cache[name] = tuple_like
          end

          # Check for generic type syntax: Array(T), StaticArray(T, N)
          if name.includes?('(') && name.includes?(')')
            # Extract base type and type argument
            paren_start = name.index('(').not_nil!
            paren_end = name.rindex(')').not_nil!

            base_type = name[0...paren_start]
            type_arg_list = name[(paren_start + 1)...paren_end]
            type_args = split_top_level_generic_args(type_arg_list)
            resolved_args = Array(Type).new(type_args.size)
            type_args.each do |arg_name|
              resolved_args << parse_type_name(arg_name)
            end

            if resolved = resolve_generic_type_application(base_type, resolved_args)
              return @parse_type_cache[name] = resolved
            end

            emit_error("Unknown generic type '#{base_type}'")
            return @parse_type_cache[name] = @context.nil_type
          end

          # Handle primitive types
          case name
          when "Int"     then return @parse_type_cache[name] = PrimitiveType.new("Int")
          when "UInt"    then return @parse_type_cache[name] = PrimitiveType.new("UInt")
          when "Int32"   then return @parse_type_cache[name] = @context.int32_type
          when "Int64"   then return @parse_type_cache[name] = @context.int64_type
          when "Float64" then return @parse_type_cache[name] = @context.float64_type
          when "String"  then return @parse_type_cache[name] = @context.string_type
          when "Bool"    then return @parse_type_cache[name] = @context.bool_type
          when "Nil"     then return @parse_type_cache[name] = @context.nil_type
          when "Char"    then return @parse_type_cache[name] = @context.char_type
          else
            # Try resolving scoped names (Time::Span) by full path
            if symbol = lookup_type_symbol(name)
              case symbol
              when ClassSymbol
                return @parse_type_cache[name] = instance_type_for(symbol)
              when ModuleSymbol
                return @parse_type_cache[name] = PrimitiveType.new(name)
              when EnumSymbol
                return @parse_type_cache[name] = EnumType.new(symbol)
              when AliasSymbol
                return @parse_type_cache[name] = parse_type_name(symbol.target)
              end
            end
            # Try finding class by last segment anywhere in global table
            if symbol = find_class_symbol_by_suffix(name)
              return @parse_type_cache[name] = instance_type_for(symbol)
            end

            base_name = last_path_segment(name)

            if prim = primitive_type_for(base_name)
              return @parse_type_cache[name] = prim
            end

            if symbol = lookup_type_symbol(base_name)
              if type = type_from_symbol(symbol)
                return @parse_type_cache[name] = normalize_literal_type(type)
              end
            end

            # As a fallback, create a nominal primitive type to avoid Nil/Unknown
            @parse_type_cache[name] = PrimitiveType.new(name)
          end
        end

        private def lookup_type_symbol(name : String) : Symbol?
          if name.includes?("::")
            return resolve_scoped_symbol(name)
          end

          if current_class = @current_class
            if symbol = current_class.scope.lookup(name)
              return symbol
            end

            if symbol = current_class.class_scope.lookup(name)
              return symbol
            end
          end

          if current_module = @current_module
            if symbol = current_module.scope.lookup(name)
              return symbol
            end
          end

          @global_table.try(&.lookup(name))
        end

        @[AlwaysInline]
        private def parse_union_type_name(name : String) : Type?
          size = name.bytesize
          return nil if size < 5 # "A | B" minimum
          types = [] of Type
          start = 0
          i = 0
          found = false
          while i + 2 < size
            if name.byte_at(i) == 32_u8 && name.byte_at(i + 1) == 124_u8 && name.byte_at(i + 2) == 32_u8
              found = true
              types << parse_type_name(trim_slice(name, start, i))
              i += 3
              start = i
              next
            end
            i += 1
          end
          return nil unless found
          types << parse_type_name(trim_slice(name, start, size))
          union_of(types)
        end

        @[AlwaysInline]
        private def trim_slice(name : String, start_idx : Int32, end_idx : Int32) : String
          s = start_idx
          e = end_idx
          while s < e && whitespace_byte?(name.byte_at(s))
            s += 1
          end
          while e > s && whitespace_byte?(name.byte_at(e - 1))
            e -= 1
          end
          name[s, e - s]
        end

        @[AlwaysInline]
        private def last_path_segment(name : String) : String
          if idx = name.rindex("::")
            start = idx + 2
            name.byte_slice(start, name.bytesize - start)
          else
            name
          end
        end

        @[AlwaysInline]
        private def intern_path_segment(name : String, start_idx : Int32, end_idx : Int32) : String
          intern_name(name.to_slice[start_idx, end_idx - start_idx])
        end

        @[AlwaysInline]
        private def whitespace_byte?(byte : UInt8) : Bool
          byte == 32_u8 || byte == 9_u8 || byte == 10_u8 || byte == 13_u8
        end

        # Resolve a scoped name like Folding::Core::Protein against the global symbol table.
        private def resolve_scoped_symbol(name : String) : Symbol?
          guard_watchdog!

          return nil unless table = @global_table
          size = name.bytesize
          return nil if size == 0
          current_table = table
          current_symbol : Symbol? = nil

          start = 0
          i = 0
          while i <= size
            delim = i + 1 < size && name.byte_at(i) == 58_u8 && name.byte_at(i + 1) == 58_u8
            at_end = i == size
            unless delim || at_end
              i += 1
              next
            end

            return nil if i <= start

            seg = intern_path_segment(name, start, i)
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

            break if at_end
            i += 2
            start = i
          end

          current_symbol
        end

        # Find a class symbol by matching the rightmost segment across global table (fallback).
        private def find_class_symbol_by_suffix(name : String) : ClassSymbol?
          guard_watchdog!

          suffix = last_path_segment(name)
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

          if unknownish_type?(left_type) || unknownish_type?(right_type)
            return case op
                   when "==", "!=", "<", ">", "<=", ">=", "===", "=~", "!~", "in"
                     @context.bool_type
                   when "<=>"
                     @context.int32_type
                   else
                     unknown_type
                   end
          end

          result_type = case op
                        when "+", "-", "*", "/", "//", "%", "**", "<<", ">>", "&", "|", "^", "&+", "&-", "&*", "&**"
                          if pointer_members = pointer_union_members(left_type)
                            if {"+", "&+"}.includes?(op) && numeric_type?(right_type)
                              union_of(pointer_members)
                            elsif {"-", "&-"}.includes?(op)
                              if right_type.is_a?(PointerType) || pointer_union_members(right_type)
                                @context.int64_type
                              elsif numeric_type?(right_type)
                                union_of(pointer_members)
                              else
                                debug("  NO method found, emitting error")
                                emit_error("Operator '#{op}' not defined for #{left_type} and #{right_type}", expr_id)
                                @context.nil_type
                              end
                            else
                              debug("  NO method found, emitting error")
                              emit_error("Operator '#{op}' not defined for #{left_type} and #{right_type}", expr_id)
                              @context.nil_type
                            end
                          elsif left_type.is_a?(PointerType)
                            if {"+", "&+"}.includes?(op) && numeric_type?(right_type)
                              left_type
                            elsif {"-", "&-"}.includes?(op)
                              if right_type.is_a?(PointerType)
                                @context.int64_type
                              elsif numeric_type?(right_type)
                                left_type
                              else
                                debug("  NO method found, emitting error")
                                emit_error("Operator '#{op}' not defined for #{left_type} and #{right_type}", expr_id)
                                @context.nil_type
                              end
                            else
                              debug("  NO method found, emitting error")
                              emit_error("Operator '#{op}' not defined for #{left_type} and #{right_type}", expr_id)
                              @context.nil_type
                            end
                          # Phase 4B.3/4B.5/18/19/21/22/78/89: Try method lookup first for built-in methods
                          # Phase 89: Wrapping arithmetic operators (&+, &-, &*, &**)
                          elsif method = lookup_method(left_type, op, [right_type], false)
                            debug("  lookup_method found: #{method.name}, return_annotation=#{method.return_annotation.inspect}")
                            if ann = method.return_annotation
                              result = resolve_method_annotation_type(ann, left_type, method.scope)
                              debug("  parse_type_name(#{ann}) => #{result}")
                              result
                            else
                              debug("  NO return_annotation, inferring operator method body")
                              infer_method_call_result(method, left_type, [right_type], nil)
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
                          if method = lookup_method(left_type, op, [right_type], false)
                            if ann = method.return_annotation
                              resolve_method_annotation_type(ann, left_type, method.scope)
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
                          if method = lookup_method(left_type, op, [right_type], false)
                            if ann = method.return_annotation
                              resolve_method_annotation_type(ann, left_type, method.scope)
                            else
                              @context.int32_type
                            end
                          else
                            # Fallback: spaceship operator → Int32
                            @context.int32_type
                          end
                        when "&&"
                          if control_flow_terminator?(right_id)
                            if falsy = extract_falsy_component(left_type)
                              falsy
                            else
                              right_type
                            end
                          else
                            infer_logical_and_type(left_type, right_type)
                          end
                        when "||"
                          if control_flow_terminator?(right_id)
                            if truthy = extract_truthy_component(left_type)
                              truthy
                            else
                              right_type
                            end
                          else
                            infer_logical_or_type(left_type, right_type)
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
          op = String.new(node.operator)

          result_type = case op
                        when "->"
                          # Proc pointer targets carry type-position syntax in
                          # their operand (for example Void*). Treat the whole
                          # expression as a proc value during semantic prepass
                          # without recursing into the target signature.
                          @context.proc_type
                        when "*"
                          PointerType.new(infer_expression(operand_id))
                        when "!"
                          operand_type = infer_expression(operand_id)
                          # Logical not: always returns Bool
                          # In Crystal: nil and false are falsy, everything else is truthy
                          @context.bool_type
                        when "+"
                          operand_type = infer_expression(operand_id)
                          # Unary plus: identity for numeric types
                          if numeric_type?(operand_type)
                            operand_type
                          else
                            emit_error("Unary '+' not defined for #{operand_type}", expr_id)
                            @context.nil_type
                          end
                        when "-"
                          operand_type = infer_expression(operand_id)
                          # Unary minus: negation for numeric types
                          if numeric_type?(operand_type)
                            operand_type
                          else
                            emit_error("Unary '-' not defined for #{operand_type}", expr_id)
                            @context.nil_type
                          end
                        when "&+"
                          operand_type = infer_expression(operand_id)
                          # Phase 89: Unary wrapping plus (identity with wrapping semantics)
                          if numeric_type?(operand_type)
                            operand_type
                          else
                            emit_error("Unary '&+' not defined for #{operand_type}", expr_id)
                            @context.nil_type
                          end
                        when "&-"
                          operand_type = infer_expression(operand_id)
                          # Phase 89: Unary wrapping minus (negation with wrapping semantics)
                          if numeric_type?(operand_type)
                            operand_type
                          else
                            emit_error("Unary '&-' not defined for #{operand_type}", expr_id)
                            @context.nil_type
                          end
                        when "~"
                          operand_type = infer_expression(operand_id)
                          # Phase 21: Bitwise NOT for integer types
                          if numeric_type?(operand_type)
                            operand_type
                          elsif operand_type.is_a?(EnumType)
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
            (integer_primitive_name?(type.name) || type.name == "Float32" || type.name == "Float64")
        end

        private def integer_primitive_name?(name : String) : Bool
          case name
          when "Int8", "Int16", "Int32", "Int64", "Int128",
               "UInt8", "UInt16", "UInt32", "UInt64", "UInt128"
            true
          else
            false
          end
        end

        private def bool_type?(type : Type) : Bool
          type.is_a?(PrimitiveType) && type.name == "Bool"
        end

        private def infer_logical_and_type(left_type : Type, right_type : Type) : Type
          if falsy = extract_falsy_component(left_type)
            union_of([falsy, right_type])
          else
            right_type
          end
        end

        private def infer_logical_or_type(left_type : Type, right_type : Type) : Type
          if truthy = extract_truthy_component(left_type)
            union_of([truthy, right_type])
          else
            right_type
          end
        end

        private def control_flow_terminator?(expr_id : ExprId) : Bool
          node = @arena[expr_id]
          case node
          when Frontend::GroupingNode
            control_flow_terminator?(node.expression)
          when Frontend::ReturnNode, Frontend::RaiseNode, Frontend::BreakNode, Frontend::NextNode
            true
          else
            false
          end
        end

        private def extract_falsy_component(type : Type) : Type?
          case type
          when UnionType
            falsy_types = type.types.select do |entry|
              entry.is_a?(PrimitiveType) && (entry.name == "Nil" || entry.name == "Bool")
            end
            return nil if falsy_types.empty?
            return falsy_types.first if falsy_types.size == 1
            UnionType.new(falsy_types)
          when PrimitiveType
            return type if type.name == "Nil" || type.name == "Bool"
          end

          nil
        end

        private def extract_truthy_component(type : Type) : Type?
          case type
          when UnionType
            truthy_types = type.types.reject { |entry| entry.is_a?(PrimitiveType) && entry.name == "Nil" }
            return nil if truthy_types.empty?
            return truthy_types.first if truthy_types.size == 1
            UnionType.new(truthy_types)
          when PrimitiveType
            return nil if type.name == "Nil"
          end

          type
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

          if known_branch = known_if_branch_body(node)
            return infer_block_result(known_branch)
          end

          # Crystal allows any type as condition (truthy check: nil and false are falsy)
          # No need to require Bool type

          # Phase 95: Extract flow narrowing from is_a? condition
          narrowing = extract_is_a_narrowing(condition_id)

          # Phase 96: Extract truthy narrowings from nilable conditions.
          # This includes simple identifiers/assignments and the right-hand side
          # of logical && chains like: if cond && (x = maybe)
          truthy_narrowings = narrowing.nil? ? extract_truthy_narrowings(condition_id) : [] of {String, Type}
          simple_nil_narrowing = narrowing.nil? ? extract_nil_narrowing(condition_id, condition_type) : nil

          # Apply narrowing for then-branch
          if narrowing
            var_name, narrowed_type = narrowing
            @flow_narrowings[var_name] = narrowed_type
          else
            truthy_narrowings.each do |var_name, narrowed_type|
              @flow_narrowings[var_name] = narrowed_type
            end
          end

          then_type = infer_block_result(node.then_body)

          # Remove narrowing after then-branch
          if narrowing
            @flow_narrowings.delete(narrowing[0])
          else
            truthy_narrowings.each do |var_name, _|
              @flow_narrowings.delete(var_name)
            end
          end

          elsif_types = [] of Type
          if elsifs = node.elsifs
            elsifs.each do |elsif_branch|
              branch_condition = elsif_branch.condition
              infer_expression(branch_condition)

              # Phase 95: Extract flow narrowing for elsif branch
              elsif_narrowing = extract_is_a_narrowing(branch_condition)
              elsif_truthy_narrowings = [] of {String, Type}
              if elsif_narrowing
                @flow_narrowings[elsif_narrowing[0]] = elsif_narrowing[1]
              else
                elsif_truthy_narrowings = extract_truthy_narrowings(branch_condition)
                elsif_truthy_narrowings.each do |var_name, narrowed_type|
                  @flow_narrowings[var_name] = narrowed_type
                end
              end

              elsif_types << infer_block_result(elsif_branch.body)

              if elsif_narrowing
                @flow_narrowings.delete(elsif_narrowing[0])
              else
                elsif_truthy_narrowings.each do |var_name, _|
                  @flow_narrowings.delete(var_name)
                end
              end
            end
          end

          # Phase 96: Apply negative narrowing for else branch
          else_narrowing = compute_else_narrowing(narrowing, simple_nil_narrowing, condition_type)
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
          condition_node = @arena[condition_id]

          # Check if condition is an is_a? expression
          return nil unless condition_node.is_a?(Frontend::IsANode)

          # Get the expression being checked
          expr_id = condition_node.expression
          expr_node = @arena[expr_id]

          # Only narrow if checking a simple variable (identifier)
          return nil unless expr_node.is_a?(Frontend::IdentifierNode)

          var_name = intern_name(expr_node.name)
          type_name = intern_name(condition_node.target_type)

          # Resolve target type
          narrowed_type = preserve_structural_narrowing(lookup_variable_type(var_name), type_name, parse_type_name(type_name))

          {var_name, narrowed_type}
        end

        private def preserve_structural_narrowing(current_type : Type?, target_type_name : String, fallback_type : Type) : Type
          return fallback_type unless current_type

          matched_types = collect_structural_narrowing_matches(current_type, target_type_name)
          return fallback_type if matched_types.empty?
          return matched_types.first if matched_types.size == 1

          union_of(matched_types)
        end

        private def collect_structural_narrowing_matches(type : Type, target_type_name : String) : Array(Type)
          case type
          when UnionType
            matches = [] of Type
            type.types.each do |member|
              matches.concat(collect_structural_narrowing_matches(member, target_type_name))
            end
            matches
          else
            structural_match?(type, target_type_name) ? [type] : [] of Type
          end
        end

        private def structural_match?(type : Type, target_type_name : String) : Bool
          case type
          when ArrayType
            {"Array", "Slice", "StaticArray"}.includes?(target_type_name)
          when HashType
            target_type_name == "Hash"
          when TupleType
            target_type_name == "Tuple"
          when PointerType
            target_type_name == "Pointer"
          when InstanceType
            type.class_symbol.name == target_type_name
          else
            false
          end
        end

        # Phase 96: Extract nil narrowing from truthy check
        # If condition is a simple identifier (or assignment to identifier) with nilable type,
        # narrow to non-nil in then branch.
        # Returns {variable_name, narrowed_type} or nil if not applicable
        private def extract_nil_narrowing(condition_id : ExprId, condition_type : Type) : {String, Type}?
          condition_node = @arena[condition_id]

          # Only narrow if condition is a simple variable (identifier)
          var_name = case condition_node
                     when Frontend::IdentifierNode
                       intern_name(condition_node.name)
                     when Frontend::AssignNode
                       target_node = @arena[condition_node.target]
                       return nil unless target_node.is_a?(Frontend::IdentifierNode)
                       intern_name(target_node.name)
                     else
                       return nil
                     end

          effective_condition_type = condition_type_for_truthy_narrowing(condition_id, condition_type)
          return nil unless effective_condition_type

          # Check if type includes Nil
          narrowed = remove_nil_from_type(effective_condition_type)
          return nil if narrowed == effective_condition_type  # No nil to remove

          {var_name, narrowed}
        end

        private def extract_truthy_narrowings(condition_id : ExprId) : Array({String, Type})
          result = [] of {String, Type}
          seen = Set(String).new
          collect_truthy_narrowings(condition_id, result, seen)
          result
        end

        private def collect_truthy_narrowings(
          condition_id : ExprId,
          result : Array({String, Type}),
          seen : Set(String)
        ) : Nil
          return if condition_id.invalid?

          condition_node = @arena[condition_id]

          case condition_node
          when Frontend::GroupingNode
            collect_truthy_narrowings(condition_node.expression, result, seen)
          when Frontend::BinaryNode
            if String.new(condition_node.operator) == "&&"
              collect_truthy_narrowings(condition_node.left, result, seen)
              collect_truthy_narrowings(condition_node.right, result, seen)
            end
          when Frontend::IdentifierNode
            add_truthy_narrowing(
              intern_name(condition_node.name),
              condition_type_for_truthy_narrowing(condition_id),
              result,
              seen
            )
          when Frontend::AssignNode
            target_node = @arena[condition_node.target]
            return unless target_node.is_a?(Frontend::IdentifierNode)
            add_truthy_narrowing(
              intern_name(target_node.name),
              condition_type_for_truthy_narrowing(condition_id),
              result,
              seen
            )
          end
        end

        private def condition_type_for_truthy_narrowing(condition_id : ExprId, fallback : Type? = nil) : Type?
          if type = @context.get_type(condition_id)
            return type
          end

          condition_node = @arena[condition_id]
          case condition_node
          when Frontend::AssignNode
            if type = @context.get_type(condition_node.value)
              return type
            end
          when Frontend::GroupingNode
            if type = condition_type_for_truthy_narrowing(condition_node.expression)
              return type
            end
          end

          fallback || infer_expression(condition_id)
        end

        private def add_truthy_narrowing(
          var_name : String,
          original_type : Type?,
          result : Array({String, Type}),
          seen : Set(String)
        ) : Nil
          return if original_type.nil?
          narrowed = remove_nil_from_type(original_type)
          return if narrowed == original_type
          return if seen.includes?(var_name)

          seen << var_name
          result << {var_name, narrowed}
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
                if (param_name = param.name) && intern_name(param_name) == var_name && (param_type = param.type_annotation)
                  return parse_type_name(intern_name(param_type))
                end
              end
            end
          end

          nil
        end

        private def extract_nil_check_then_narrowing(condition_id : ExprId) : {String, Type}?
          condition_node = @arena[condition_id]
          return nil unless condition_node.is_a?(Frontend::MemberAccessNode)
          return nil unless member_name_for(condition_id, condition_node) == "nil?"

          target_node = @arena[condition_node.object]
          return nil unless target_node.is_a?(Frontend::IdentifierNode)

          var_name = intern_name(target_node.name)
          original_type = @assignments[var_name]? || lookup_variable_type(var_name)
          return nil unless original_type

          narrowed = remove_nil_from_type(original_type)
          return nil if narrowed == original_type

          {var_name, narrowed}
        end

        # Phase 24: Type inference for unless (similar to if but without elsif)
        private def infer_unless(node : Frontend::UnlessNode) : Type
          guard_watchdog!

          condition_id = node.condition
          condition_type = infer_expression(condition_id)

          if known_condition = known_macro_condition_value(condition_id)
            selected_body = known_condition ? (node.else_branch || [] of ExprId) : node.then_branch
            return infer_block_result(selected_body)
          end

          # Crystal allows any type as condition (truthy check)
          # No need to require Bool type

          nil_check_narrowing = extract_nil_check_then_narrowing(condition_id)
          if nil_check_narrowing
            @flow_narrowings[nil_check_narrowing[0]] = nil_check_narrowing[1]
          end

          then_type = infer_block_result(node.then_branch)

          if nil_check_narrowing
            @flow_narrowings.delete(nil_check_narrowing[0])
            @flow_narrowings[nil_check_narrowing[0]] = @context.nil_type
          end

          else_type = node.else_branch ? infer_block_result(node.else_branch.not_nil!) : @context.nil_type

          if nil_check_narrowing
            @flow_narrowings.delete(nil_check_narrowing[0])
          end

          union_of([then_type, else_type])
        end

        private def known_if_branch_body(node : Frontend::IfNode) : Array(ExprId)?
          known_condition = known_macro_condition_value(node.condition)
          return nil if known_condition.nil?
          return node.then_body if known_condition

          if elsifs = node.elsifs
            elsifs.each do |elsif_branch|
              elsif_condition = known_macro_condition_value(elsif_branch.condition)
              return nil if elsif_condition.nil?
              return elsif_branch.body if elsif_condition
            end
          end

          node.else_body || [] of ExprId
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
          var_name = intern_name(decl.name)
          type_name = intern_name(decl.declared_type)

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
          target_node = @arena[target_id]

          # Phase 5A: Check if target is instance variable
          case target_node
          when Frontend::InstanceVarNode
            target_name = intern_name(target_node.name)
            clean_name = target_name.starts_with?("@") ? target_name[1..-1] : target_name
            @instance_var_types[clean_name] = value_type
            @context.set_type(target_id, value_type)
          when Frontend::ClassVarNode
            @class_var_types[class_var_key(intern_name(target_node.name))] = value_type
            @context.set_type(target_id, value_type)
          when Frontend::GlobalNode
            @global_var_types[global_var_key(intern_name(target_node.name))] = value_type
            @context.set_type(target_id, value_type)
          when Frontend::IdentifierNode
            @assignments[intern_name(target_node.name)] = value_type
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
            target_node = @arena[target_id]
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
              @assignments[intern_name(target_node.name)] = element_type
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
          value_type = if value_id = node.value
                         infer_expression(value_id)
                       else
                         # Return without value returns nil
                         @context.nil_type
                       end

          if explicit_returns = @method_return_stack.last?
            explicit_returns << value_type
          end

          value_type
        end

        # ============================================================
        # PHASE 7: Self Keyword
        # ============================================================

        private def infer_self(node, expr_id : ExprId) : Type
          if receiver_type = @receiver_type_context
            case receiver_type
            when ClassType, ModuleType
              return receiver_type
            end
          end

          if @current_method_is_class_method_stack.last?
            if current_class = @current_class
              return class_type_for(current_class)
            end

            if current_module = @current_module
              return module_type_for(current_module)
            end
          end

          # self returns InstanceType of the current class
          if current_class = @current_class
            instance_type = instance_type_for(current_class)
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
          inferred = Array(Type).new(node.args.size)
          node.args.each do |arg_expr_id|
            inferred << infer_expression(arg_expr_id)
          end

          return @context.nil_type if inferred.empty?
          return inferred.first if inferred.size == 1
          union_of(inferred)
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

          if static_array_type = infer_static_array_type_expression(node.type)
            return static_array_type
          end

          # Infer the type expression and convert type references (Foo, Box(Int32))
          # to their runtime value types.
          normalize_runtime_type_reference(infer_expression(node.type))
        end

        private def infer_static_array_type_expression(type_expr_id : ExprId) : Type?
          type_node = @arena[type_expr_id]
          return nil unless type_node.is_a?(Frontend::IndexNode)
          return nil unless type_node.indexes.size == 1

          index_node = @arena[type_node.indexes.first]
          return nil unless index_node.is_a?(Frontend::NumberNode)

          element_type = case object_node = @arena[type_node.object]
                         when Frontend::IdentifierNode
                           parse_type_name(intern_name(object_node.name))
                         else
                           normalize_runtime_type_reference(infer_expression(type_node.object))
                         end
          ArrayType.new(element_type)
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
          target_type_name = intern_name(node.target_type)
          parse_type_name(target_type_name)
        end

        # Phase 45: as? keyword (safe cast - nilable)
        private def infer_as_question(node : Frontend::AsQuestionNode, expr_id : ExprId) : Type
          # Safe cast: value.as?(Type)
          # Returns Type | Nil (nilable)

          # Phase 103C: Resolve target type and make nilable
          infer_expression(node.expression)
          target_type_name = intern_name(node.target_type)
          target_type = parse_type_name(target_type_name)

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
          base_type = type_from_type_expr(node.base_type) || infer_expression(node.base_type)
          debug_hook("infer.generic.start", "base=#{base_type} args=#{node.type_args.size}")

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
              arg_node = @arena[arg]
              puts "  arg #{i}: node=#{arg_node.class}"
            end
            inferred = type_from_type_expr(arg) || infer_expression(arg)
            if ENV["DEBUG"]?
              puts "    inferred: #{inferred.class} = #{inferred.inspect}"
            end
            type_args << normalize_type_argument(inferred)
          end

          # If base is a class/module (Box / Enumerable), preserve type args.
          if base_type.is_a?(ClassType)
            result = ClassType.new(base_type.symbol, type_args)
            debug_hook("infer.generic.result", "base=#{base_type.symbol.name} result=#{result}")
            return result
          end
          if base_type.is_a?(PrimitiveType)
            if instantiated = resolve_generic_type_application(base_type.name, type_args)
              class_ref = class_type_reference_for(instantiated)
              result = class_ref.is_a?(ClassType) ? class_ref : instantiated
              debug_hook("infer.generic.result", "base=#{base_type.name} result=#{result}")
              return result
            end
          end
          if base_type.is_a?(InstanceType)
            class_ref = class_type_reference_for(base_type)
            if class_ref.is_a?(ClassType)
              result = ClassType.new(class_ref.symbol, type_args)
              debug_hook("infer.generic.result", "base=#{class_ref.symbol.name} result=#{result}")
              return result
            end
          end
          if base_type.is_a?(ModuleType)
            result = ModuleType.new(base_type.symbol, type_args)
            debug_hook("infer.generic.result", "base=#{base_type.symbol.name} result=#{result}")
            return result
          end

          # Otherwise return base type (for built-in types like Array, Hash)
          # Future: handle Array(Int32), Hash(String, Int32)
          debug_hook("infer.generic.result", "base=#{base_type} result=#{base_type}")
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
            debug_hook("infer.normalize_type_arg", "input=#{type} result=#{result}")
            result
          else
            debug_hook("infer.normalize_type_arg", "input=#{type} result=#{type}")
            type
          end
        end

        # Normalize literal/annotation types to primitives when possible
        private def normalize_literal_type(type : Type) : Type
          normalize_type_argument(type)
        end

        private def normalize_runtime_type_reference(type : Type) : Type
          case type
          when ClassType
            instance_type_for(type.symbol, type.type_args)
          else
            type
          end
        end

        private def unknown_type : PrimitiveType
          @unknown_type
        end

        private def unknownish_type?(type : Type) : Bool
          return true if type.is_a?(PrimitiveType) && {"Unknown", "Object"}.includes?(type.name)
          type.is_a?(InstanceType) && type.class_symbol.name == "Object"
        end

        private def union_contains_unknownish?(type : Type) : Bool
          type.is_a?(UnionType) && type.types.any? { |member| unknownish_type?(member) }
        end

        private def class_var_key(name : String) : String
          if current_class = @current_class
            return "#{current_class.name}:#{name}"
          end

          if current_module = @current_module
            return "#{current_module.name}:#{name}"
          end

          name
        end

        private def global_var_key(name : String) : String
          name
        end

        private def lookup_class_var_symbol(name : String) : ClassVarSymbol?
          if current_class = @current_class
            if symbol = current_class.class_scope.lookup(name)
              return symbol.as?(ClassVarSymbol)
            end

            if symbol = current_class.scope.lookup(name)
              return symbol.as?(ClassVarSymbol)
            end
          end

          if current_module = @current_module
            if symbol = current_module.scope.lookup(name)
              return symbol.as?(ClassVarSymbol)
            end
          end

          @global_table.try(&.lookup(name)).as?(ClassVarSymbol)
        end

        private def lookup_global_var_symbol(name : String) : GlobalVarSymbol?
          @global_table.try(&.lookup(name)).as?(GlobalVarSymbol)
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
            if symbol.is_a?(ConstantSymbol)
              return infer_expression(symbol.value)
            end
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
          method_name = member_name_for(expr_id, node)

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

          if symbol = resolve_enclosing_namespace_path(segments)
            return symbol
          end

          path_lookup_tables.each do |table|
            if symbol = resolve_path_symbol_in_table(table, segments)
              return symbol
            end
          end

          nil
        end

        private def resolve_enclosing_namespace_path(segments : Array(String)) : Symbol?
          return nil if segments.empty?

          current_module = @current_module
          while current_module
            if current_module.name == segments.first
              return resolve_remaining_path_from_symbol(current_module, segments, 1)
            end

            current_module = enclosing_module_for(current_module.scope)
          end

          nil
        end

        private def enclosing_module_for(scope : SymbolTable) : ModuleSymbol?
          table = scope.parent
          while table
            if owner_module = table.owner_module
              return owner_module
            end
            table = table.parent
          end
          nil
        end

        private def resolve_remaining_path_from_symbol(symbol : Symbol, segments : Array(String), start_index : Int32) : Symbol?
          current_symbol = symbol
          index = start_index

          while index < segments.size
            current_table = case current_symbol
                            when ModuleSymbol
                              current_symbol.scope
                            when ClassSymbol
                              current_symbol.scope
                            when EnumSymbol
                              current_symbol.scope
                            else
                              return nil
                            end

            next_symbol = current_table.lookup(segments[index])
            return nil unless next_symbol
            current_symbol = next_symbol
            index += 1
          end

          current_symbol
        end

        private def collect_path_segments(node : Frontend::PathNode, segments : Array(String))
          if left_id = node.left
            append_path_segments(left_id, segments)
          end
          append_path_segments(node.right, segments)
        end

        private def append_path_segments(expr_id : ExprId, segments : Array(String))
          expr = @arena[expr_id]
          case expr
          when Frontend::PathNode
            collect_path_segments(expr, segments)
          when Frontend::IdentifierNode
            segments << identifier_name_for(expr_id, expr)
          when Frontend::ConstantNode
            segments << intern_name(expr.name)
          end
        end

        private def path_lookup_tables : Array(SymbolTable)
          tables = [] of SymbolTable

          if current_class = @current_class
            tables << current_class.scope
            tables << current_class.class_scope unless current_class.class_scope.same?(current_class.scope)
          end

          if current_module = @current_module
            tables << current_module.scope unless tables.any? { |table| table.same?(current_module.scope) }
          end

          if global_table = @global_table
            tables << global_table unless tables.any? { |table| table.same?(global_table) }
          end

          tables
        end

        private def resolve_path_symbol_in_table(table : SymbolTable, segments : Array(String)) : Symbol?
          current_table = table
          symbol : Symbol? = nil

          segments.each_with_index do |segment, index|
            debug("  resolving segment '#{segment}' (index #{index}) from table=#{current_table.object_id}") if @debug_enabled
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
                                symbol.scope
                              else
                                return nil
                              end
            end
          end

          symbol
        end

        private def type_from_symbol(symbol : Symbol) : Type?
          case symbol
          when ClassSymbol
            class_type_for(symbol)
          when ModuleSymbol
            module_type_for(symbol)
          when EnumSymbol
            EnumType.new(symbol)
          when AliasSymbol
            class_type_reference_for(parse_type_name(symbol.target))
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
          node = @arena[expr_id]

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
            name = identifier_name_for(expr_id, node)
            if special = resolve_special_type_expr_string(name)
              return normalize_literal_type(special)
            end
            resolved = resolve_method_annotation_type(name, @receiver_type_context, @current_method_scope)
            unless unknownish_type?(resolved)
              return normalize_literal_type(resolved)
            end

            table = @global_table
            return primitive_type_for(name) unless table
            if symbol = table.lookup(name)
              if type = type_from_symbol(symbol)
                return normalize_literal_type(type)
              end
            end
            if prim = primitive_type_for(name)
              return prim
            end
          when Frontend::GenericNode
            return normalize_literal_type(infer_generic(node, expr_id))
          when Frontend::TypeofNode
            return normalize_type_argument(infer_typeof(node, expr_id))
          end

          nil
        end

        TYPE_EXPR_ELEMENT_PREFIXES = ["Enumerable.element_type", "::Enumerable.element_type", "Indexable.element_type",
                                      "::Indexable.element_type", "Iterator.element_type", "::Iterator.element_type",
                                      "Iterable.element_type", "::Iterable.element_type"] of String

        private def resolve_special_type_expr_string(name : String) : Type?
          expr = name.strip

          if expr.starts_with?("typeof(") && expr.ends_with?(')')
            inner = expr[7...-1]
            return resolve_type_expr_string(inner)
          end

          resolve_element_type_expression_type(expr)
        end

        private def resolve_type_expr_string(expr : String) : Type?
          expr = expr.strip
          return nil if expr.empty?

          if expr.starts_with?("flat_map_type(") && expr.ends_with?(')')
            inner = expr[13...-1].strip
            inner = inner[6..].strip if inner.starts_with?("yield ")
            if inner_type = resolve_type_expr_string(inner)
              return element_type_from_type(inner_type) || inner_type
            end
          end

          if expr.ends_with?(".not_nil!")
            if base_type = resolve_type_expr_string(expr[0...-9])
              return remove_nil_from_type(base_type)
            end
          end

          if expr.starts_with?("yield ")
            yielded = expr[6..].strip
            if yielded_type = resolve_type_expr_string(yielded)
              return yielded_type
            end
            return @yield_return_stack.last?
          end

          if indexed = resolve_indexed_type_expr_string(expr)
            return indexed
          end

          if element_type = resolve_element_type_expression_type(expr)
            return element_type
          end

          if expr == "self"
            if receiver = @receiver_type_context
              return normalize_runtime_type_reference(receiver)
            end
          end

          resolved = resolve_method_annotation_type(expr, @receiver_type_context, @current_method_scope)
          return resolved unless unknownish_type?(resolved)

          nil
        end

        private def resolve_indexed_type_expr_string(expr : String) : Type?
          return nil unless expr.ends_with?(']')
          bracket = expr.rindex('[')
          return nil unless bracket

          index_literal = expr[(bracket + 1)...-1].strip
          return nil unless index = index_literal.to_i32?

          base_expr = expr[0...bracket].strip
          return nil if base_expr.empty?
          return nil unless base_type = resolve_type_expr_string(base_expr)

          case base_type
          when TupleType
            return base_type.element_types[index]? if index >= 0
          when ArrayType
            return base_type.element_type
          when HashType
            return index == 0 ? base_type.key_type : (index == 1 ? base_type.value_type : nil)
          end

          nil
        end

        private def resolve_element_type_expression_type(expr : String) : Type?
          prefix = TYPE_EXPR_ELEMENT_PREFIXES.find { |entry| expr.starts_with?(entry) }
          return nil unless prefix

          rest = expr[prefix.size, expr.bytesize - prefix.size].strip
          arg_expr =
            if rest.starts_with?('(') && rest.ends_with?(')')
              rest[1...-1].strip
            else
              rest
            end

          return nil if arg_expr.empty?

          if arg_expr == "self"
            if receiver = @receiver_type_context
              if element_type = element_type_from_type(receiver)
                return element_type
              end
            end

            return current_module_generic_element_type
          end

          source_type = resolve_type_expr_string(arg_expr)

          return nil unless source_type
          element_type_from_type(source_type)
        end

        private def element_type_from_type(type : Type) : Type?
          case type
          when ArrayType
            type.element_type
          when HashType
            TupleType.new([type.key_type, type.value_type])
          when TupleType
            type.element_types.first?
          when InstanceType
            type.type_args.try(&.first?)
          when PointerType
            type.element_type
          else
            nil
          end
        end

        private def rightmost_segment(node : Frontend::PathNode) : String?
          segments = [] of String
          collect_path_segments(node, segments)
          segments.last?
        end

        private def primitive_type_for(name : String) : Type?
          case name
          when "Int"     then PrimitiveType.new("Int")
          when "UInt"    then PrimitiveType.new("UInt")
          when "Int8"    then @context.int8_type
          when "Int16"   then @context.int16_type
          when "Int32"   then @context.int32_type
          when "Int64"   then @context.int64_type
          when "Int128"  then @context.int128_type
          when "UInt8"   then @context.uint8_type
          when "UInt16"  then @context.uint16_type
          when "UInt32"  then @context.uint32_type
          when "UInt64"  then @context.uint64_type
          when "UInt128" then @context.uint128_type
          when "Float32" then @context.float32_type
          when "Float64" then @context.float64_type
          when "String"  then @context.string_type
          when "Bool"    then @context.bool_type
          when "Nil"     then @context.nil_type
          when "Char"    then @context.char_type
          when "Symbol"  then @context.symbol_type
          when "Proc"    then @context.proc_type
          when "Void"    then PrimitiveType.new("Void")
          when "NoReturn" then PrimitiveType.new("NoReturn")
          else
            nil
          end
        end

        private def infer_index(node : Frontend::IndexNode, expr_id : ExprId) : Type
          target_type = infer_expression(node.object)
          index_ids = node.indexes
          index_id = index_ids.first?
          return @context.nil_type unless index_id
          index_types = Array(Type).new(index_ids.size)
          index_ids.each do |idx|
            index_types << infer_expression(idx)
          end
          _index_type = index_types.first

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
            index_node = @arena[index_id]
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
            index_node = @arena[index_id]
            key_name = case index_node
            when Frontend::SymbolNode
              # Symbol name may include leading colon, strip it
              sym_name = intern_name(index_node.name)
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
          elsif target_type.is_a?(PointerType)
            target_type.element_type
          elsif target_type.is_a?(EnumType)
            target_type
          elsif target_type.is_a?(ClassType) || target_type.is_a?(ModuleType)
            if macro_result = infer_static_index_macro(target_type, index_types)
              macro_result
            elsif method = lookup_method(target_type, "[]", index_types, false)
              if ann = method.return_annotation
                resolve_method_annotation_type(ann, target_type, method.scope)
              else
                infer_method_body_type(method, target_type)
              end
            else
              emit_error("Cannot index type #{target_type}", expr_id)
              @context.nil_type
            end
          else
            # Not an array, hash, tuple, named tuple, or string - emit error
            emit_error("Cannot index type #{target_type}", expr_id)
            @context.nil_type
          end
        end

        private def infer_static_index_macro(target_type : Type, index_types : Array(Type)) : Type?
          return nil unless target_type.is_a?(ClassType)

          owner_symbol = target_type.symbol
          macro_symbol = owner_symbol.scope.lookup_macro("[]")
          return nil unless macro_symbol.is_a?(MacroSymbol)

          case owner_symbol.name
          when "Slice"
            element_type = target_type.type_args.try(&.first?) || union_of(index_types)
            instance_type_for(owner_symbol, [element_type])
          else
            nil
          end
        end

        # ============================================================
        # PHASE 4: Method Calls
        # ============================================================

        private def infer_member_access(node : Frontend::MemberAccessNode, expr_id : ExprId) : Type
          # MemberAccess without parens is a zero-argument method call in Crystal
          # obj.method → obj.method()
          receiver_type = infer_expression(node.object)
          receiver_type = class_receiver_type_for_expression(receiver_type) if type_receiver_expression?(node.object)
          # If receiver is a nominal primitive (unknown class), try resolving to class type
          if receiver_type.is_a?(PrimitiveType) && receiver_type.name.includes?("::")
            if sym = resolve_scoped_symbol(receiver_type.name)
              if sym.is_a?(ClassSymbol)
                receiver_type = instance_type_for(sym)
              end
            end
          end
          method_name = member_name_for(expr_id, node)
          debug_type_trace(method_name, "member_access method=#{method_name} receiver=#{receiver_type} receiver_class=#{receiver_type.class.name}")

          debug("infer_member_access: receiver_type = #{receiver_type.class.name}: #{receiver_type}, method = #{method_name}")

          if unknownish_type?(receiver_type)
            return unknown_type
          end

          if union_contains_unknownish?(receiver_type)
            return unknown_type
          end

          if pointer_members = pointer_union_members(receiver_type)
            case method_name
            when "value"
              return union_of(pointer_members.map(&.element_type))
            when "null?"
              return @context.bool_type
            end
          end

          if receiver_type.is_a?(PointerType) && method_name == "value"
            return receiver_type.element_type
          end

          if receiver_type.is_a?(PointerType) && method_name == "null?"
            return @context.bool_type
          end

          # Phase 4B.5 + Week 1: Special case for constructor - ClassName.new → InstanceType
          # Week 1: Zero-argument constructor
          # Box(Int32).new or Box.new (no args)
          if {"new", "new!"}.includes?(method_name)
            if receiver_type.is_a?(ClassType)
              # If ClassType has type_args (e.g., Box(Int32)), copy them to InstanceType
              debug("  Constructor call - returning InstanceType")
              return instantiate_class_receiver(receiver_type)
            elsif primitive_metaclass?(receiver_type)
              if value_type = primitive_metaclass_value_type(receiver_type.as(PrimitiveType))
                return value_type
              end
            end
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
          method = lookup_method(receiver_type, method_name, arg_types, false)
          debug("  lookup_method returned: #{method ? "MethodSymbol(#{method.name})" : "nil"}")

          result_type = if method
                            if ann = method.return_annotation
                              debug("  Method has return annotation: #{ann}")
                              resolve_method_annotation_type(ann, receiver_type, method.scope)
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

          if macro_symbol = @identifier_symbols[node.callee]?
            return @context.nil_type if macro_symbol.is_a?(MacroSymbol)
          end

          callee_node = @arena[node.callee]
          has_block = call_has_block?(node)

          debug("infer_call: callee_node type = #{callee_node.class.name}")
          debug("  has_block = #{has_block}")
          debug_hook("infer.call.start", "expr_id=#{expr_id} callee_node=#{callee_node.class.name}")

          receiver_type : Type?
          method_name : String?

          case callee_node
          when Frontend::MemberAccessNode
            receiver_type = infer_expression(callee_node.object)
            receiver_type = class_receiver_type_for_expression(receiver_type) if type_receiver_expression?(callee_node.object)
            method_name = member_name_for(node.callee, callee_node)
            debug("  receiver_type = #{receiver_type.class.name}: #{receiver_type}")
            debug("  method_name = #{method_name}")
          when Frontend::PathNode
            if type_application = infer_path_type_application_call(callee_node, node)
              return type_application
            end
            debug("  PathNode callee unsupported - returning Nil!")
            return @context.nil_type
          when Frontend::IdentifierNode
            if type_application = infer_identifier_type_application_call(callee_node, node)
              return type_application
            end
            # Week 1 Day 2: Top-level function call (e.g., identity(42))
            method_name = identifier_name_for(node.callee, callee_node)
            debug("  IdentifierNode call: method_name = #{method_name}")
            # Infer argument types
            arg_ids = positional_call_arg_ids(node)
            arg_types = Array(Type).new(arg_ids.size)
            arg_ids.each { |arg_id| arg_types << infer_expression(arg_id) }

            # Helper to ensure block is inferred before returning
            infer_block_if_present = ->(result : Type) {
              if block_id = node.block
                infer_expression(block_id)
              end
              result
            }

            if result = infer_macro_builtin_receiverless_call(method_name, node)
              return infer_block_if_present.call(result)
            end

            # If resolver already bound this identifier to a method (incl. self/class methods), use it
            if symbol = @identifier_symbols[node.callee]?
              debug("  identifier_symbols hit: #{symbol.class.name}") if @debug_enabled
              if @debug_enabled && method_name == "total_energy_breakdown"
                debug("  identifier_symbols[callee]=#{symbol.class.name}")
              end
              case symbol
              when MethodSymbol
                if result = infer_receiverless_method_call(symbol, arg_types, has_block, node)
                  if has_block
                    if receiver_type = implicit_receiver_type_for(symbol)
                      if infer_receiverless_iteration_block(node, method_name, receiver_type, symbol)
                        return result
                      end
                    end
                  end
                  return infer_block_if_present.call(result)
                end
              when OverloadSetSymbol
                if result = infer_receiverless_overload_call(symbol, arg_types, has_block, node)
                  return infer_block_if_present.call(result)
                end
              when ClassSymbol
                # Calling a class name without .new is likely a missing resolution; fall through
              end
            end
            # Try current class/module scope for class methods (implicit self)
            if current_class = @current_class
              if sym = current_class.scope.lookup(method_name)
                case sym
                when MethodSymbol
                  if result = infer_receiverless_method_call(sym, arg_types, has_block, node)
                    debug("  current_class scope hit for #{method_name}") if @debug_enabled
                    return infer_block_if_present.call(result)
                  end
                when OverloadSetSymbol
                  if result = infer_receiverless_overload_call(sym, arg_types, has_block, node)
                    debug("  current_class overload hit for #{method_name}") if @debug_enabled
                    return infer_block_if_present.call(result)
                  end
                end
              end
            end
            # Try current module scope (def self.* inside a module)
            if current_module = @current_module
              if sym = current_module.scope.lookup(method_name)
                case sym
                when MethodSymbol
                  if result = infer_receiverless_method_call(sym, arg_types, has_block, node)
                    debug("  current_module scope hit for #{method_name}") if @debug_enabled
                    return infer_block_if_present.call(result)
                  end
                when OverloadSetSymbol
                  if result = infer_receiverless_overload_call(sym, arg_types, has_block, node)
                    debug("  current_module overload hit for #{method_name}") if @debug_enabled
                    return infer_block_if_present.call(result)
                  end
                end
              end
            end
            if result = infer_receiverless_current_context_call(method_name, arg_types, has_block, node)
              return infer_block_if_present.call(result)
            end
            # Fallback: search global table for a module/class method with this name
            if method = find_method_in_scope(@global_table, method_name)
              debug("  find_method_in_scope hit for #{method_name}: #{method.class.name}") if @debug_enabled
              if result = infer_receiverless_method_call(method, arg_types, has_block, node)
                return infer_block_if_present.call(result)
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
          debug_hook("infer.call.receiver", "method=#{method_name} receiver=#{receiver_type}")

          if unknownish_type?(receiver_type)
            if block_id = node.block
              infer_expression(block_id)
            end
            return unknown_type
          end

          if union_contains_unknownish?(receiver_type)
            if block_id = node.block
              infer_expression(block_id)
            end
            return unknown_type
          end

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
          if {"new", "new!"}.includes?(method_name)
            if receiver_type.is_a?(ClassType)
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
                return instantiate_class_receiver(receiver_type)
                # Otherwise try to infer type arguments from constructor arguments
              elsif type_args = infer_type_arguments(receiver_type.symbol, arg_types)
                return instantiate_class_receiver(ClassType.new(receiver_type.symbol, type_args))
              else
                return instantiate_class_receiver(receiver_type)
              end
            elsif primitive_metaclass?(receiver_type)
              if value_type = primitive_metaclass_value_type(receiver_type.as(PrimitiveType))
                return value_type
              end
            elsif receiver_type.is_a?(ArrayType) || receiver_type.is_a?(HashType) || receiver_type.is_a?(TupleType) || receiver_type.is_a?(PointerType)
              return receiver_type
            end
          end

          if method_name == "literal"
            if literal_type = array_like_literal_receiver_type(receiver_type)
              arg_ids = positional_call_arg_ids(node)
              arg_ids.each { |arg_id| infer_expression(arg_id) }
              return literal_type
            end
          end

          arg_ids = positional_call_arg_ids(node)
          arg_types = Array(Type).new(arg_ids.size)
          arg_ids.each_with_index do |arg_id, index|
            if method_name == "unsafe_as" && index == 0
              arg_types << (type_from_type_expr(arg_id) || infer_expression(arg_id))
            else
              arg_types << infer_expression(arg_id)
            end
          end

          if named_args = node.named_args
            named_args.each { |named_arg| infer_expression(named_arg.value) }
          end

          infer_pointer_linked_list_block(node, receiver_type, method_name)

          if method_name == "unsafe_as"
            if target_type = unsafe_as_target_type(arg_types.first?)
              return target_type
            end
          end

          # Enumerable heuristics for Array element types
          if receiver_type.is_a?(ArrayType)
            elem_type = receiver_type.element_type
            case method_name
            when "<<", "push"
              pushed_type = arg_types.first? || elem_type
              if collection_builder_element_type?(elem_type)
                return ArrayType.new(pushed_type)
              end
              return receiver_type
            when "concat"
              if other_array = arg_types.first?.as?(ArrayType)
                if collection_builder_element_type?(elem_type)
                  return ArrayType.new(other_array.element_type)
                end
                return receiver_type
              end
            when "map", "collect"
              mapped = elem_type
              # Find block - can be in node.block or node.args
              map_block_node : Frontend::BlockNode? = nil
              if blk_id = node.block
                map_block_node = @arena[blk_id].as(Frontend::BlockNode)
              elsif node.args.first?
                arg_node = @arena[node.args.first]
                if arg_node.is_a?(Frontend::BlockNode)
                  map_block_node = arg_node
                end
              end

              if block = map_block_node
                # Set up block parameter with element type
                if (params = block.params) && (first_param = params.first?) && (name_slice = first_param.name)
                  param_name = intern_name(name_slice)
                  old_assignment = @assignments[param_name]?
                  @assignments[param_name] = elem_type
                  # Clear cached types
                  block.body.each do |body_id|
                    @context.expression_types.delete(body_id)
                    body_node = @arena[body_id]
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
            when "find"
              if_none_type = arg_types.first? || @context.nil_type
              return @context.union_of([elem_type, if_none_type])
            when "find_index"
              return @context.union_of([@context.int32_type, @context.nil_type])
            when "find!"
              return elem_type
            when "first", "last"
              return elem_type
            when "each", "each_with_index"
              return receiver_type
            when "to_a", "dup", "clear", "shuffle!", "reverse!", "swap"
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
                block_node = @arena[blk_id].as(Frontend::BlockNode)
              elsif node.args.first?
                arg_node = @arena[node.args.first]
                if arg_node.is_a?(Frontend::BlockNode)
                  block_node = arg_node
                end
              end

              if block = block_node
                # Block receives the non-nil type as implicit parameter
                if (params = block.params) && (first_param = params.first?) && (name_slice = first_param.name)
                  param_name = intern_name(name_slice)
                  old_assignment = @assignments[param_name]?
                  @assignments[param_name] = non_nil_type
                  # Clear any cached types for the param identifier
                  block.body.each do |body_id|
                    @context.expression_types.delete(body_id)
                    # Also clear nested expressions that might reference the param
                    inner_node = @arena[body_id]
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
                    body_node = @arena[body_id]
                    if body_node.is_a?(Frontend::MemberAccessNode)
                      member_name = intern_name(body_node.member)
                      if method = lookup_method(non_nil_type, member_name, [] of Type, false)
                        if ann = method.return_annotation
                          block_result = resolve_method_annotation_type(ann, non_nil_type, method.scope)
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

          if method = lookup_method(receiver_type, method_name, arg_types, has_block)
            if ann = method.return_annotation
              resolve_method_annotation_type(ann, receiver_type, method.scope)
            else
              debug("  No return annotation - inferring from method body")
              infer_method_body_type(method, receiver_type, arg_types, node)
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

        private def unsafe_as_target_type(type : Type?) : Type?
          case type
          when ClassType
            instantiate_class_receiver(type)
          when PrimitiveType
            primitive_metaclass?(type) ? (primitive_metaclass_value_type(type) || type) : type
          when ArrayType, HashType, TupleType, PointerType, InstanceType
            type
          else
            nil
          end
        end

        # Helper: Ensure block is inferred for call with block
        private def ensure_block_inferred(node : Frontend::CallNode)
          if block_id = node.block
            infer_expression(block_id)
          end
        end

        # Helper: detect block presence (explicit or passed as BlockNode argument).
        private def call_has_block?(node : Frontend::CallNode) : Bool
          return true if node.block
          if args = node.args
            args.any? { |arg_id| block_pass_arg?(arg_id) }
          else
            false
          end
        end

        private def positional_call_arg_ids(node : Frontend::CallNode) : Array(ExprId)
          args = [] of ExprId
          node.args.each do |arg_id|
            next if block_pass_arg?(arg_id)
            args << arg_id
          end
          args
        end

        private def block_pass_arg?(arg_id : ExprId) : Bool
          node = @arena[arg_id]
          return true if node.is_a?(Frontend::BlockNode)
          return false unless node.is_a?(Frontend::UnaryNode)

          (Frontend.node_operator_string(node) || "") == "&"
        end

        private def infer_pointer_linked_list_block(node : Frontend::CallNode, receiver_type : Type, method_name : String) : Nil
          return unless receiver_type.is_a?(InstanceType)
          return unless {"each", "consume_each"}.includes?(method_name)

          class_name = receiver_type.class_symbol.name
          return unless class_name == "PointerLinkedList" || class_name.ends_with?("::PointerLinkedList")

          element_type = receiver_type.type_args.try(&.first?) || @context.nil_type
          infer_call_block_with_first_param_type(node, PointerType.new(normalize_runtime_type_reference(element_type)))
        end

        private def infer_path_type_application_call(node : Frontend::PathNode, call : Frontend::CallNode) : Type?
          return nil if call.block
          return nil if call.named_args
          return nil if call.args.empty?

          symbol = resolve_path_symbol(node)
          return nil unless symbol.is_a?(ClassSymbol)

          type_args = Array(Type).new(call.args.size)
          call.args.each do |arg_id|
            arg_type = type_from_type_expr(arg_id) || infer_expression(arg_id)
            type_args << normalize_type_argument(arg_type)
          end

          ClassType.new(symbol, type_args)
        end

        private def infer_identifier_type_application_call(node : Frontend::IdentifierNode, call : Frontend::CallNode) : Type?
          return nil if call.block
          return nil if call.named_args
          return nil if call.args.empty?

          name = identifier_name_for(call.callee, node)
          symbol = @identifier_symbols[call.callee]?
          symbol = lookup_type_symbol(name) unless symbol.is_a?(ClassSymbol)
          return nil unless symbol.is_a?(ClassSymbol)

          type_args = Array(Type).new(call.args.size)
          call.args.each do |arg_id|
            arg_type = type_from_type_expr(arg_id) || infer_expression(arg_id)
            type_args << normalize_type_argument(arg_type)
          end

          ClassType.new(symbol, type_args)
        end

        private def infer_call_block_with_first_param_type(node : Frontend::CallNode, param_type : Type) : Type?
          infer_call_block_with_param_types(node, [param_type])
        end

        private def infer_call_block_with_param_types(node : Frontend::CallNode, param_types : Array(Type)) : Type?
          block_node = explicit_call_block_node(node)
          return nil unless block_node

          assigned_names = [] of String
          old_assignments = {} of String => Type?

          if params = block_node.params
            params.each_with_index do |param, index|
              break if index >= param_types.size
              next unless name_slice = param.name

              param_name = intern_name(name_slice)
              old_assignments[param_name] = @assignments[param_name]?
              @assignments[param_name] = param_types[index]
              assigned_names << param_name
            end

            unless assigned_names.empty?
              block_node.body.each do |body_id|
                clear_cached_type_tree(body_id)
              end
              result = infer_block_result(block_node.body)
              assigned_names.each do |param_name|
                if old_assignment = old_assignments[param_name]?
                  if old_assignment
                    @assignments[param_name] = old_assignment
                  else
                    @assignments.delete(param_name)
                  end
                else
                  @assignments.delete(param_name)
                end
              end
              return result
            end
          end

          block_node.body.each do |body_id|
            clear_cached_type_tree(body_id)
          end
          infer_block_result(block_node.body)
        end

        private def infer_receiverless_iteration_block(node : Frontend::CallNode, method_name : String, receiver_type : Type, method : MethodSymbol) : Bool
          return false unless {"each", "each_with_index"}.includes?(method_name)
          return false unless explicit_call_block_node(node)

          element_type = block_element_type_from_signature(method) || implicit_receiver_iteration_element_type(receiver_type)
          return false unless element_type

          if method_name == "each_with_index"
            infer_call_block_with_param_types(node, [element_type, @context.int32_type])
          else
            infer_call_block_with_first_param_type(node, element_type)
          end
          true
        end

        private def block_element_type_from_signature(method : MethodSymbol) : Type?
          block_param = method.params.find(&.is_block)
          return nil unless type_annotation = block_param.try(&.type_annotation)

          type_name = intern_name(type_annotation)
          scope = @current_method_scope || method.scope

          if proc_type = resolve_proc_type_name_in_scope(type_name, scope)
            return proc_type.param_types.first?
          end

          nil
        end

        private def implicit_receiver_iteration_element_type(receiver_type : Type) : Type?
          case receiver_type
          when ArrayType
            receiver_type.element_type
          when InstanceType
            receiver_type.type_args.try(&.first?) || @receiver_type_context.as?(InstanceType).try(&.type_args).try(&.first?)
          when ModuleType
            current_module_generic_element_type
          else
            nil
          end
        end

        private def current_module_generic_element_type : Type?
          current_module = @current_module
          current_scope = @current_method_scope
          return nil unless current_module && current_scope

          node = @arena[current_module.node_id]
          return nil unless node.is_a?(Frontend::ModuleNode)

          type_param = node.type_params.try(&.first?)
          return nil unless type_param

          resolve_annotation_type_in_scope(intern_name(type_param), current_scope)
        end

        private def collection_builder_element_type?(type : Type) : Bool
          unknownish_type?(type) || (type.is_a?(PrimitiveType) && type.name == "Nil")
        end

        private def explicit_call_block_node(node : Frontend::CallNode) : Frontend::BlockNode?
          if block_id = node.block
            return @arena[block_id].as?(Frontend::BlockNode)
          end

          node.args.each do |arg_id|
            arg_node = @arena[arg_id]
            return arg_node.as?(Frontend::BlockNode) if arg_node.is_a?(Frontend::BlockNode)
          end

          nil
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
            type_args = infer_method_type_arguments(method, @context.nil_type, arg_types)

            # Substitute type parameters in return type
            if ret_ann = method.return_annotation
              return substitute_type_parameters(ret_ann, type_args, type_params)
            else
              return infer_method_body_type(method, @context.nil_type, arg_types)
            end
          else
            # Non-generic method - just return the annotated type
            if ret_ann = method.return_annotation
              return parse_type_name(ret_ann)
            else
              return infer_method_body_type(method, @context.nil_type, arg_types)
            end
          end
        end

        private def infer_macro_builtin_receiverless_call(method_name : String, node : Frontend::CallNode) : Type?
          case method_name
          when "flag?"
            return @context.bool_type unless macro_flag_condition_value(node).nil?
          end

          nil
        end

        private def known_macro_condition_value(expr_id : ExprId) : Bool?
          node = @arena[expr_id]

          case node
          when Frontend::BoolNode
            node.value
          when Frontend::NilNode
            false
          when Frontend::GroupingNode
            known_macro_condition_value(node.expression)
          when Frontend::MacroExpressionNode
            known_macro_condition_value(node.expression)
          when Frontend::UnaryNode
            op = Frontend.node_operator_string(node) || ""
            return nil unless op == "!"
            value = known_macro_condition_value(node.operand)
            value.nil? ? nil : !value
          when Frontend::BinaryNode
            left = known_macro_condition_value(node.left)
            right = known_macro_condition_value(node.right)

            case Frontend.node_operator_string(node) || ""
            when "&&"
              return false if left == false || right == false
              return true if left == true && right == true
              nil
            when "||"
              return true if left == true || right == true
              return false if left == false && right == false
              nil
            else
              nil
            end
          when Frontend::CallNode
            macro_flag_condition_value(node)
          else
            nil
          end
        end

        private def macro_flag_condition_value(node : Frontend::CallNode) : Bool?
          callee = @arena[node.callee]
          return nil unless callee.is_a?(Frontend::IdentifierNode)
          return nil unless identifier_name_for(node.callee, callee) == "flag?"
          return nil unless node.args.size == 1

          arg = @arena[node.args[0]]
          flag_name = case arg
                      when Frontend::SymbolNode
                        intern_name(arg.name).lchop(':')
                      when Frontend::StringNode
                        intern_name(arg.value)
                      else
                        nil
                      end
          return nil unless flag_name

          @flags.includes?(flag_name)
        end

        # Week 1 Day 2: Infer type arguments for generic method from call arguments
        # Similar to infer_type_arguments but for methods instead of classes
        private def infer_method_type_arguments(
          method : MethodSymbol,
          receiver_type : Type,
          arg_types : Array(Type),
          call_node : Frontend::CallNode? = nil
        ) : Array(Type)
          type_params = method.type_parameters
          return [] of Type unless type_params && !type_params.empty?

          binding = infer_method_type_argument_bindings(method, receiver_type, arg_types, call_node)
          type_params.map { |param_name| binding[param_name]? || @context.nil_type }
        end

        private def infer_method_type_argument_bindings(
          method : MethodSymbol,
          receiver_type : Type,
          arg_types : Array(Type),
          call_node : Frontend::CallNode? = nil
        ) : Hash(String, Type)
          type_params = method.type_parameters
          return {} of String => Type unless type_params && !type_params.empty?

          params = method.params
          required_count = count_required_params(params)
          # Allow fewer args than params if defaults exist
          return {} of String => Type if arg_types.size < required_count || arg_types.size > params.size

          # Build binding map: type parameter name → inferred type
          binding = {} of String => Type
          arg_types.each_with_index do |arg_type, i|
            param = params[i]
            if type_ann = param.type_annotation
              infer_type_parameter_bindings_from_annotation(intern_name(type_ann), arg_type, type_params, binding)
            end
          end

          if call_node
            previous_method_bindings = {} of String => Type?
            binding.each do |type_param_name, type_arg|
              previous_method_bindings[type_param_name] = @assignments[type_param_name]?
              @assignments[type_param_name] = type_arg
            end

            begin
              if block_result_info = infer_method_block_result_type(method, receiver_type, call_node)
                block_result, return_type_name = block_result_info
                debug("method type bindings #{method.name}: block_return=#{block_result} return_name=#{return_type_name}") if @debug_enabled
                if return_type_name && type_params.includes?(return_type_name)
                  binding[return_type_name] = block_result
                end
              end
            ensure
              binding.each_key do |type_param_name|
                if previous_method_bindings.has_key?(type_param_name)
                  if previous_type = previous_method_bindings[type_param_name]
                    @assignments[type_param_name] = previous_type
                  else
                    @assignments.delete(type_param_name)
                  end
                else
                  @assignments.delete(type_param_name)
                end
              end
            end
          end

          debug("method type bindings #{method.name}: #{binding.inspect}") if @debug_enabled
          binding
        end

        private def infer_type_parameter_bindings_from_annotation(
          annotation_name : String,
          arg_type : Type,
          type_params : Array(String),
          binding : Hash(String, Type)
        ) : Nil
          return if annotation_name.empty?

          if type_params.includes?(annotation_name)
            binding[annotation_name] = arg_type
            return
          end

          if annotation_name.ends_with?("*") && annotation_name.size > 1
            if arg_type.is_a?(PointerType)
              infer_type_parameter_bindings_from_annotation(annotation_name[0...-1], arg_type.element_type, type_params, binding)
            end
            return
          end

          if annotation_name.ends_with?("?") && annotation_name.size > 1
            if union = arg_type.as?(UnionType)
              non_nil_members = union.types.reject { |member| member.is_a?(PrimitiveType) && member.name == "Nil" }
              inner_arg_type =
                if non_nil_members.size == 1
                  non_nil_members.first
                elsif non_nil_members.empty?
                  @context.nil_type
                else
                  UnionType.new(non_nil_members)
                end
              infer_type_parameter_bindings_from_annotation(annotation_name[0...-1], inner_arg_type, type_params, binding)
            end
            return
          end

          if annotation_name.ends_with?(".class") && annotation_name.size > 6
            case arg_type
            when ClassType
              infer_type_parameter_bindings_from_annotation(annotation_name[0...-6], instantiate_class_receiver(arg_type), type_params, binding)
            when PrimitiveType
              if primitive_metaclass?(arg_type)
                if value_type = primitive_metaclass_value_type(arg_type)
                  infer_type_parameter_bindings_from_annotation(annotation_name[0...-6], value_type, type_params, binding)
                end
              end
            end
            return
          end

          return unless annotation_name.includes?('(') && annotation_name.includes?(')')

          paren_start = annotation_name.index('(').not_nil!
          paren_end = annotation_name.rindex(')').not_nil!
          base_type = annotation_name[0...paren_start]
          inner_types = split_top_level_generic_args(annotation_name[(paren_start + 1)...paren_end])

          actual_type_args =
            case arg_type
            when InstanceType
              arg_type.class_symbol.name == base_type ? arg_type.type_args : nil
            when ClassType
              arg_type.symbol.name == base_type ? arg_type.type_args : nil
            when ModuleType
              arg_type.symbol.name == base_type ? arg_type.type_args : nil
            when ArrayType
              {"Array", "Slice", "StaticArray"}.includes?(base_type) ? [arg_type.element_type] : nil
            when PointerType
              base_type == "Pointer" ? [arg_type.element_type] : nil
            when HashType
              base_type == "Hash" ? [arg_type.key_type, arg_type.value_type] : nil
            when TupleType
              base_type == "Tuple" ? arg_type.element_types : nil
            else
              nil
            end

          return unless actual_type_args

          inner_types.each_with_index do |inner_name, index|
            if actual_type = actual_type_args[index]?
              infer_type_parameter_bindings_from_annotation(inner_name, actual_type, type_params, binding)
            end
          end
        end

        private def method_block_signature(
          type_name : Slice(UInt8),
          receiver_type : Type,
          scope : SymbolTable
        ) : {Array(Type), String?}?
          type_string = intern_name(type_name)
          arrow_index = find_top_level_arrow(type_string)
          return nil unless arrow_index

          params_part = trim_slice(type_string, 0, arrow_index)
          return_part = trim_slice(type_string, arrow_index + 2, type_string.bytesize)

          param_types = [] of Type
          unless params_part.empty?
            split_top_level_generic_args(params_part).each do |param_name|
              param_types << resolve_method_annotation_type(param_name, receiver_type, scope)
            end
          end

          return_name = return_part.empty? || return_part == "_" ? nil : return_part
          {param_types, return_name}
        end

        private def infer_method_block_result_type(
          method : MethodSymbol,
          receiver_type : Type,
          call_node : Frontend::CallNode
        ) : {Type, String?}?
          block_param = method.params.find(&.is_block)
          return nil unless type_ann = block_param.try(&.type_annotation)

          signature = method_block_signature(type_ann, receiver_type, method.scope)
          return nil unless signature

          block_param_types, return_type_name = signature
          block_result = infer_call_block_with_param_types(call_node, block_param_types)
          return nil unless block_result

          {block_result, return_type_name}
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
        private def lookup_method(receiver_type : Type, method_name : String, arg_types : Array(Type), has_block : Bool) : MethodSymbol?
          debug_type_trace(method_name, "lookup_method start method=#{method_name} receiver=#{receiver_type} receiver_class=#{receiver_type.class.name} args=#{arg_types.map(&.to_s).join(",")}")
          debug_hook("infer.lookup.start", "method=#{method_name} receiver=#{receiver_type} args=#{arg_types.size}")
          lookup_key = MethodLookupKey.new(receiver_type, method_name, arg_types, has_block)
          if @method_lookup_cache.has_key?(lookup_key)
            return @method_lookup_cache[lookup_key]
          end
          candidates = method_candidates_for(receiver_type, method_name)
          debug_type_trace(method_name, "lookup_method candidates method=#{method_name} count=#{candidates.size} receiver=#{receiver_type}")
          if candidates.empty?
            debug_hook("infer.lookup.miss", "method=#{method_name} receiver=#{receiver_type} stage=candidates")
            @method_lookup_cache[lookup_key] = nil
            return nil
          end

          # Filter by parameter count (accounting for default values, splat, double_splat)
          actual_count = arg_types.size
          matching_count = candidates.select do |m|
            required = count_required_params(m.params)
            has_splat = m.params.any? { |p| p.is_splat || p.is_double_splat }
            max = has_splat ? Int32::MAX : m.params.count { |p| !p.is_block }
            actual_count >= required && actual_count <= max
          end
          if matching_count.empty?
            debug_hook("infer.lookup.miss", "method=#{method_name} receiver=#{receiver_type} stage=arity")
            @method_lookup_cache[lookup_key] = nil
            return nil
          end

          # Prefer overloads that match block presence.
          block_filtered = matching_count.select do |method|
            method_has_block = method.params.any?(&.is_block)
            method_has_block == has_block
          end
          matching_count = block_filtered unless block_filtered.empty?

          # Filter by parameter types (for typed parameters)
          matches = matching_count.select do |method|
            parameters_match?(method, arg_types, receiver_type)
          end

          if matches.empty?
            debug_hook("infer.lookup.miss", "method=#{method_name} receiver=#{receiver_type} stage=types")
            @method_lookup_cache[lookup_key] = nil
            return nil
          end
          if matches.size == 1
            debug_hook("infer.lookup.hit", "method=#{method_name} receiver=#{receiver_type} selected=#{matches.first.name}")
            @method_lookup_cache[lookup_key] = matches.first
            return matches.first
          end

          # Phase 98: Specificity ranking - prefer more specific overload
          #
          # Sort by specificity score (highest first) and return best match
          # Ties are resolved by order of definition (first defined wins)
          selected = matches.max_by { |m| specificity_score(m, arg_types) }
          debug_hook("infer.lookup.hit", "method=#{method_name} receiver=#{receiver_type} selected=#{selected.name} overloads=#{matches.size}")
          @method_lookup_cache[lookup_key] = selected
          selected
        end

        # Find all methods with given name on receiver type
        private def find_all_methods(receiver_type : Type, method_name : String) : Array(MethodSymbol)
          methods = [] of MethodSymbol

          case receiver_type
          when ClassType
            class_name = normalize_class_receiver_name(receiver_type.symbol.name)
            get_builtin_class_methods(class_name, method_name).each { |entry| methods << entry }
            get_specialized_builtin_class_methods(receiver_type, method_name).each { |entry| methods << entry }

            # Look in class_scope for class methods (def self.*)
            if symbol = receiver_type.symbol.class_scope.lookup(method_name)
              case symbol
              when MethodSymbol
                # Single method
                methods << symbol
              when OverloadSetSymbol
                # Phase 4B.2: Multiple overloads
                symbol.overloads.each { |entry| methods << entry }
              end
            end

            # Phase 4B.2: Inheritance search - look in superclass class_scope
            if methods.empty?
              find_in_superclass(receiver_type.symbol, method_name, class_methods: true).each { |entry| methods << entry }
            end
          when ModuleType
            # Phase 102: Look for module methods (def self.* inside module)
            # Module methods are stored in the module's scope
            if symbol = receiver_type.symbol.scope.lookup(method_name)
              case symbol
              when MethodSymbol
                methods << symbol
              when OverloadSetSymbol
                symbol.overloads.each { |entry| methods << entry }
              end
            end
          when InstanceType
            get_builtin_methods(receiver_type.class_symbol.name, method_name).each { |entry| methods << entry }

            # Phase 4B.2: Look for instance methods in class scope
            if symbol = receiver_type.class_symbol.scope.lookup(method_name)
              case symbol
              when MethodSymbol
                methods << symbol
              when OverloadSetSymbol
                symbol.overloads.each { |entry| methods << entry }
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
                      sym.overloads.each { |entry| methods << entry }
                    end
                  end
                end
              end
            end

            # Phase 4B.2: Inheritance search - look in superclass
            if methods.empty?
              find_in_superclass(receiver_type.class_symbol, method_name).each { |entry| methods << entry }
            end
          when PrimitiveType
            if primitive_metaclass?(receiver_type)
              class_name = normalize_class_receiver_name(receiver_type.name)
              get_builtin_class_methods(class_name, method_name).each { |entry| methods << entry }
              get_specialized_builtin_class_methods(receiver_type, method_name).each { |entry| methods << entry }

              if methods.empty?
                if primitive_class = lookup_runtime_class_symbol(class_name)
                  if symbol = primitive_class.class_scope.lookup(method_name)
                    case symbol
                    when MethodSymbol
                      methods << symbol
                    when OverloadSetSymbol
                      symbol.overloads.each { |entry| methods << entry }
                    end
                  end

                  if methods.empty?
                    find_in_superclass(primitive_class, method_name, class_methods: true).each { |entry| methods << entry }
                  end
                end
              end
            else
              # Phase 4B.3: Built-in methods for primitive types
              get_builtin_methods(receiver_type.name, method_name).each { |entry| methods << entry }
              if primitive_class = lookup_runtime_class_symbol(receiver_type.name)
                if symbol = primitive_class.scope.lookup(method_name)
                  case symbol
                  when MethodSymbol
                    methods << symbol
                  when OverloadSetSymbol
                    symbol.overloads.each { |entry| methods << entry }
                  end
                end

                if methods.empty?
                  find_in_superclass(primitive_class, method_name).each { |entry| methods << entry }
                end
              end
            end
          when ArrayType
            # Phase 9: Built-in methods for arrays
            get_array_builtin_methods(receiver_type, method_name).each { |entry| methods << entry }
          when PointerType
            get_pointer_builtin_methods(receiver_type, method_name).each { |entry| methods << entry }
          when HashType
            # Phase 103C: Built-in methods for hashes
            get_hash_builtin_methods(receiver_type, method_name).each { |entry| methods << entry }
          when UnionType
            # Phase 4B.4: Find common method in all union members
            # Method can only be called on union if it exists in ALL constituent types
            find_methods_in_union(receiver_type, method_name).each { |entry| methods << entry }
          when VirtualType
            # Phase 99: Virtual type method dispatch
            # Find method in base class and all known subclasses
            find_methods_in_virtual(receiver_type, method_name).each { |entry| methods << entry }
          when ProcType
            # Phase 101: Built-in methods for procs
            get_proc_builtin_methods(receiver_type, method_name).each { |entry| methods << entry }
          when EnumType
            if symbol = receiver_type.symbol.scope.lookup(method_name)
              case symbol
              when MethodSymbol
                methods << symbol
              when OverloadSetSymbol
                symbol.overloads.each { |entry| methods << entry }
              end
            end
            get_enum_builtin_methods(receiver_type, method_name).each { |entry| methods << entry }
          end

          # Phase 103C: Universal methods available on all types
          if methods.empty?
            get_universal_methods(receiver_type, method_name).each { |entry| methods << entry }
          end

          methods
        end

        private def lookup_runtime_class_symbol(name : String) : ClassSymbol?
          return nil unless table = @global_table

          symbol = if name.includes?("::")
                     resolve_scoped_symbol(name)
                   else
                     table.lookup(name)
                   end

          symbol.as?(ClassSymbol)
        end

        private def method_candidates_for(receiver_type : Type, method_name : String) : Array(MethodSymbol)
          key = MethodCandidatesKey.new(receiver_type, method_name)
          if cached = @method_candidates_cache[key]?
            return cached
          end
          methods = find_all_methods(receiver_type, method_name)
          @method_candidates_cache[key] = methods
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
          when "itself"
            methods << MethodSymbol.new(method_name, dummy_node_id, params: [] of Frontend::Parameter, return_annotation: receiver_type.to_s, scope: dummy_scope)
          when "tap"
            block_param = Frontend::Parameter.new(name: "block".to_slice, is_block: true)
            methods << MethodSymbol.new(method_name, dummy_node_id, params: [block_param], return_annotation: receiver_type.to_s, scope: dummy_scope)
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

        private def get_builtin_class_methods(type_name : String, method_name : String) : Array(MethodSymbol)
          methods = [] of MethodSymbol
          dummy_node_id = ExprId.new(0)
          dummy_scope = SymbolTable.new(nil)

          if primitive_type_for(type_name) || is_type_name?(type_name)
            case method_name
            when "zero", "additive_identity", "multiplicative_identity"
              methods << MethodSymbol.new(
                method_name,
                dummy_node_id,
                params: [] of Frontend::Parameter,
                return_annotation: type_name,
                scope: dummy_scope,
                is_class_method: true
              )
            end
          end

          methods
        end

        private def builtin_class_receiver_signature(receiver_type : Type) : {String, Array(Type)}?
          case receiver_type
          when ClassType
            {normalize_class_receiver_name(receiver_type.symbol.name), receiver_type.type_args || [] of Type}
          when PrimitiveType
            return nil unless primitive_metaclass?(receiver_type)

            receiver_name = normalize_class_receiver_name(receiver_type.name)
            if receiver_name.includes?('(') && receiver_name.includes?(')')
              paren_start = receiver_name.index('(').not_nil!
              paren_end = receiver_name.rindex(')').not_nil!
              base_name = receiver_name[0...paren_start]
              arg_names = split_top_level_generic_args(receiver_name[(paren_start + 1)...paren_end])
              type_args = Array(Type).new(arg_names.size)
              arg_names.each do |arg_name|
                type_args << parse_type_name(arg_name)
              end
              {base_name, type_args}
            else
              {receiver_name, [] of Type}
            end
          else
            nil
          end
        end

        private def get_specialized_builtin_class_methods(receiver_type : Type, method_name : String) : Array(MethodSymbol)
          methods = [] of MethodSymbol
          signature = builtin_class_receiver_signature(receiver_type)
          return methods unless signature

          base_name, type_args = signature
          dummy_node_id = ExprId.new(0)
          dummy_scope = SymbolTable.new(nil)

          case base_name
          when "Array", "Slice", "StaticArray"
            element_type = type_args.first?
            if method_name == "empty" && element_type
              methods << MethodSymbol.new(
                method_name,
                dummy_node_id,
                params: [] of Frontend::Parameter,
                return_annotation: "Array(#{element_type})",
                scope: dummy_scope,
                is_class_method: true
              )
            end
          when "Pointer"
            element_type = type_args.first?
            if method_name == "null" && element_type
              methods << MethodSymbol.new(
                method_name,
                dummy_node_id,
                params: [] of Frontend::Parameter,
                return_annotation: "Pointer(#{element_type})",
                scope: dummy_scope,
                is_class_method: true
              )
            end
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
                symbol.overloads.each { |entry| methods << entry }
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
              symbol.overloads.each { |entry| methods << entry }
            end
          end

          # Recursively search in superclass's superclass (and its included modules)
          if methods.empty?
            find_in_superclass(superclass_symbol, method_name, class_methods: class_methods, visited: visited).each do |entry|
              methods << entry
            end
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
            if method = lookup_method(member_type, method_name, arg_types, false)
              if ann = method.return_annotation
                return_types << resolve_method_annotation_type(ann, member_type, method.scope)
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

        private def pointer_union_members(type : Type) : Array(PointerType)?
          return nil unless type.is_a?(UnionType)

          members = [] of PointerType
          type.types.each do |member|
            pointer_member = member.as?(PointerType)
            return nil unless pointer_member
            members << pointer_member
          end

          members
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
              symbol.overloads.each { |entry| methods << entry }
            end
          end

          # If not found in base, search superclass chain
          if methods.empty?
            find_in_superclass(base_class, method_name).each { |entry| methods << entry }
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
                  method_sym.overloads.each { |entry| overrides << entry }
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
        private def parameters_match?(method : MethodSymbol, arg_types : Array(Type), receiver_type : Type? = nil) : Bool
          params = method.params.reject(&.is_block)
          required_count = count_required_params(params)

          previous_method_bindings = {} of String => Type?
          if type_params = method.type_parameters
            bindings = infer_method_type_argument_bindings(method, receiver_type || @context.nil_type, arg_types)
            type_params.each do |type_param_name|
              previous_method_bindings[type_param_name] = @assignments[type_param_name]?
              if bound_type = bindings[type_param_name]?
                @assignments[type_param_name] = bound_type
              end
            end
          end

          begin

            # Check argument count (allow fewer args if defaults exist)
            return false if arg_types.size < required_count
            splat_index = params.index { |param| param.is_splat }
            return false if splat_index.nil? && arg_types.size > params.size

            if splat_at = splat_index
              suffix_count = params.size - splat_at - 1
              return false if arg_types.size < splat_at + suffix_count

              arg_types.each_with_index do |arg_type, i|
                param =
                  if i < splat_at
                    params[i]
                  elsif i >= arg_types.size - suffix_count
                    suffix_index = params.size - (arg_types.size - i)
                    params[suffix_index]
                  else
                    params[splat_at]
                  end

                type_ann = param.type_annotation
                next unless type_ann # No annotation means any type matches

                type_name = intern_name(type_ann)
                next if type_name == "_"

                param_type = resolve_method_annotation_type(type_name, receiver_type, method.scope)
                return false unless type_matches?(arg_type, param_type)
              end

              return true
            end

            # Check each provided argument matches its parameter
            arg_types.each_with_index do |arg_type, i|
              param = params[i]
              type_ann = param.type_annotation
              next unless type_ann # No annotation means any type matches

              type_name = intern_name(type_ann)
              next if type_name == "_"

              param_type = resolve_method_annotation_type(type_name, receiver_type, method.scope)
              return false unless type_matches?(arg_type, param_type)
            end

            true
          ensure
            previous_method_bindings.each do |type_param_name, previous_type|
              if previous_type
                @assignments[type_param_name] = previous_type
              else
                @assignments.delete(type_param_name)
              end
            end
          end
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

          if actual.is_a?(PrimitiveType) && expected.is_a?(PrimitiveType)
            if expected.name == "Int"
              return signed_integer_type_name?(actual.name)
            elsif expected.name == "UInt"
              return unsigned_integer_type_name?(actual.name)
            end
          end

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
          if child.is_a?(PrimitiveType) && parent.is_a?(PrimitiveType)
            if parent.name == "Int"
              return signed_integer_type_name?(child.name)
            elsif parent.name == "UInt"
              return unsigned_integer_type_name?(child.name)
            end
          end

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

        private def signed_integer_type_name?(name : String) : Bool
          case name
          when "Int8", "Int16", "Int32", "Int64", "Int128"
            true
          else
            false
          end
        end

        private def unsigned_integer_type_name?(name : String) : Bool
          case name
          when "UInt8", "UInt16", "UInt32", "UInt64", "UInt128"
            true
          else
            false
          end
        end

        private def integer_bang_cast_target(method_name : String) : String?
          case method_name
          when "to_i8!"    then "Int8"
          when "to_i16!"   then "Int16"
          when "to_i32!"   then "Int32"
          when "to_i64!"   then "Int64"
          when "to_i128!"  then "Int128"
          when "to_u8!"    then "UInt8"
          when "to_u16!"   then "UInt16"
          when "to_u32!"   then "UInt32"
          when "to_u64!"   then "UInt64"
          when "to_u128!"  then "UInt128"
          else
            nil
          end
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
          arg_types.each_with_index do |arg_type, i|
            param = method.params[i]?
            next unless param
            type_ann = param.type_annotation
            next unless type_ann

            type_name = intern_name(type_ann)
            next if type_name == "_"

            param_type = resolve_method_annotation_type(type_name, nil, method.scope)

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

          if integer_primitive_name?(type_name)
            # Integer arithmetic and bitwise operators
            case method_name
            when "+", "-", "*", "/", "//", "%", "&", "|", "^", "<<", ">>"
              param = Frontend::Parameter.new(name: "other".to_slice, type_annotation: type_name.to_slice)
              methods << MethodSymbol.new(
                method_name,
                dummy_node_id,
                params: [param],
                return_annotation: type_name,
                scope: dummy_scope
              )
            when "unsafe_shl", "unsafe_shr", "unsafe_div", "unsafe_mod"
              param = Frontend::Parameter.new(name: "other".to_slice, type_annotation: "_".to_slice)
              methods << MethodSymbol.new(
                method_name,
                dummy_node_id,
                params: [param],
                return_annotation: type_name,
                scope: dummy_scope
              )
            when "<", ">", "<=", ">=", "==", "!="
              param = Frontend::Parameter.new(name: "other".to_slice, type_annotation: type_name.to_slice)
              methods << MethodSymbol.new(
                method_name,
                dummy_node_id,
                params: [param],
                return_annotation: "Bool",
                scope: dummy_scope
              )
            when "bits_set?"
              param = Frontend::Parameter.new(name: "mask".to_slice, type_annotation: "_".to_slice)
              methods << MethodSymbol.new(
                method_name,
                dummy_node_id,
                params: [param],
                return_annotation: "Bool",
                scope: dummy_scope
              )
            when "byte_swap", "leading_zeros_count", "trailing_zeros_count"
              methods << MethodSymbol.new(
                method_name,
                dummy_node_id,
                params: [] of Frontend::Parameter,
                return_annotation: type_name,
                scope: dummy_scope
              )
            when "sign"
              methods << MethodSymbol.new(
                method_name,
                dummy_node_id,
                params: [] of Frontend::Parameter,
                return_annotation: "Int32",
                scope: dummy_scope
              )
            else
              if cast_target = integer_bang_cast_target(method_name)
                methods << MethodSymbol.new(
                  method_name,
                  dummy_node_id,
                  params: [] of Frontend::Parameter,
                  return_annotation: cast_target,
                  scope: dummy_scope
                )
              end
            end
          elsif type_name == "Float32" || type_name == "Float64"
            # Floating-point arithmetic operators
            case method_name
            when "+", "-", "*", "/", "//", "%"
              param = Frontend::Parameter.new(name: "other".to_slice, type_annotation: type_name.to_slice)
              methods << MethodSymbol.new(
                method_name,
                dummy_node_id,
                params: [param],
                return_annotation: type_name,
                scope: dummy_scope
              )
            when "<", ">", "<=", ">=", "==", "!="
              param = Frontend::Parameter.new(name: "other".to_slice, type_annotation: type_name.to_slice)
              methods << MethodSymbol.new(
                method_name,
                dummy_node_id,
                params: [param],
                return_annotation: "Bool",
                scope: dummy_scope
              )
            when "sign"
              methods << MethodSymbol.new(
                method_name,
                dummy_node_id,
                params: [] of Frontend::Parameter,
                return_annotation: "Int32",
                scope: dummy_scope
              )
            end
          else
            case type_name
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
            when "to_unsafe"
              methods << MethodSymbol.new(
                method_name,
                dummy_node_id,
                params: [] of Frontend::Parameter,
                return_annotation: "Pointer(UInt8)",
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
            when "includes?"
              string_param = Frontend::Parameter.new(name: "str".to_slice, type_annotation: "String".to_slice)
              methods << MethodSymbol.new(
                method_name,
                dummy_node_id,
                params: [string_param],
                return_annotation: "Bool",
                scope: dummy_scope
              )

              char_param = Frontend::Parameter.new(name: "char".to_slice, type_annotation: "Char".to_slice)
              methods << MethodSymbol.new(
                method_name,
                dummy_node_id,
                params: [char_param],
                return_annotation: "Bool",
                scope: dummy_scope
              )
            when "starts_with?", "ends_with?"
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
          when "to_unsafe"
            methods << MethodSymbol.new(
              method_name,
              dummy_node_id,
              params: [] of Frontend::Parameter,
              return_annotation: "Pointer(#{element_type_name})",
              scope: dummy_scope
            )
          when "to_slice"
            methods << MethodSymbol.new(
              method_name,
              dummy_node_id,
              params: [] of Frontend::Parameter,
              return_annotation: "Slice(#{element_type_name})",
              scope: dummy_scope
            )
          when "copy_to"
            same_target_param = Frontend::Parameter.new(name: "target".to_slice, type_annotation: "_".to_slice)
            methods << MethodSymbol.new(
              method_name,
              dummy_node_id,
              params: [same_target_param],
              return_annotation: "Nil",
              scope: dummy_scope
            )

            pointer_target_param = Frontend::Parameter.new(name: "target".to_slice, type_annotation: "Pointer(#{element_type_name})".to_slice)
            count_param = Frontend::Parameter.new(name: "count".to_slice, type_annotation: "_".to_slice)
            methods << MethodSymbol.new(
              method_name,
              dummy_node_id,
              params: [pointer_target_param, count_param],
              return_annotation: "Nil",
              scope: dummy_scope
            )
          when "copy_from"
            source_param = Frontend::Parameter.new(name: "source".to_slice, type_annotation: "_".to_slice)
            methods << MethodSymbol.new(
              method_name,
              dummy_node_id,
              params: [source_param],
              return_annotation: "Nil",
              scope: dummy_scope
            )

            pointer_source_param = Frontend::Parameter.new(name: "source".to_slice, type_annotation: "Pointer(#{element_type_name})".to_slice)
            count_param = Frontend::Parameter.new(name: "count".to_slice, type_annotation: "_".to_slice)
            methods << MethodSymbol.new(
              method_name,
              dummy_node_id,
              params: [pointer_source_param, count_param],
              return_annotation: "Nil",
              scope: dummy_scope
            )
          when "fill"
            value_param = Frontend::Parameter.new(name: "value".to_slice, type_annotation: "_".to_slice)
            methods << MethodSymbol.new(
              method_name,
              dummy_node_id,
              params: [value_param],
              return_annotation: "Array(#{element_type_name})",
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
          when "clear"
            methods << MethodSymbol.new(
              method_name,
              dummy_node_id,
              params: [] of Frontend::Parameter,
              return_annotation: "Array(#{element_type_name})",
              scope: dummy_scope
            )
          when "unsafe_fetch"
            param = Frontend::Parameter.new(name: "index".to_slice, type_annotation: "Int32".to_slice)
            methods << MethodSymbol.new(
              method_name,
              dummy_node_id,
              params: [param],
              return_annotation: element_type_name,
              scope: dummy_scope
            )
          when "unsafe_put"
            index_param = Frontend::Parameter.new(name: "index".to_slice, type_annotation: "Int32".to_slice)
            value_param = Frontend::Parameter.new(name: "value".to_slice, type_annotation: element_type_name.to_slice)
            methods << MethodSymbol.new(
              method_name,
              dummy_node_id,
              params: [index_param, value_param],
              return_annotation: element_type_name,
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
          when "push"
            param = Frontend::Parameter.new(name: "value".to_slice, type_annotation: element_type_name.to_slice)
            methods << MethodSymbol.new(
              method_name,
              dummy_node_id,
              params: [param],
              return_annotation: "Array(#{element_type_name})",
              scope: dummy_scope
            )
          when "concat"
            param = Frontend::Parameter.new(name: "other".to_slice, type_annotation: "Array(#{element_type_name})".to_slice)
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
          when "dup", "to_a", "shuffle!", "reverse!"
            methods << MethodSymbol.new(
              method_name,
              dummy_node_id,
              params: [] of Frontend::Parameter,
              return_annotation: "Array(#{element_type_name})",
              scope: dummy_scope
            )
          when "swap"
            first = Frontend::Parameter.new(name: "a".to_slice, type_annotation: "Int32".to_slice)
            second = Frontend::Parameter.new(name: "b".to_slice, type_annotation: "Int32".to_slice)
            methods << MethodSymbol.new(
              method_name,
              dummy_node_id,
              params: [first, second],
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

        private def get_pointer_builtin_methods(pointer_type : PointerType, method_name : String) : Array(MethodSymbol)
          methods = [] of MethodSymbol
          dummy_node_id = ExprId.new(0)
          dummy_scope = SymbolTable.new(nil)
          element_type_name = pointer_type.element_type.to_s

          case method_name
          when "copy_to", "copy_from"
            target_param = Frontend::Parameter.new(name: "target".to_slice, type_annotation: "Pointer(#{element_type_name})".to_slice)
            count_param = Frontend::Parameter.new(name: "count".to_slice, type_annotation: "_".to_slice)
            methods << MethodSymbol.new(
              method_name,
              dummy_node_id,
              params: [target_param, count_param],
              return_annotation: "Nil",
              scope: dummy_scope
            )
          when "memcmp"
            other_param = Frontend::Parameter.new(name: "other".to_slice, type_annotation: "Pointer(#{element_type_name})".to_slice)
            count_param = Frontend::Parameter.new(name: "count".to_slice, type_annotation: "_".to_slice)
            methods << MethodSymbol.new(
              method_name,
              dummy_node_id,
              params: [other_param, count_param],
              return_annotation: "Int32",
              scope: dummy_scope
            )
          when "fill"
            count_param = Frontend::Parameter.new(name: "count".to_slice, type_annotation: "_".to_slice)
            value_param = Frontend::Parameter.new(name: "value".to_slice, type_annotation: element_type_name.to_slice)
            methods << MethodSymbol.new(
              method_name,
              dummy_node_id,
              params: [count_param, value_param],
              return_annotation: "Pointer(#{element_type_name})",
              scope: dummy_scope
            )
          when "to_slice"
            size_param = Frontend::Parameter.new(name: "size".to_slice, type_annotation: "_".to_slice)
            methods << MethodSymbol.new(
              method_name,
              dummy_node_id,
              params: [size_param],
              return_annotation: "Slice(#{element_type_name})",
              scope: dummy_scope
            )
          when "address"
            methods << MethodSymbol.new(
              method_name,
              dummy_node_id,
              params: [] of Frontend::Parameter,
              return_annotation: "UInt64",
              scope: dummy_scope
            )
          when "null?"
            methods << MethodSymbol.new(
              method_name,
              dummy_node_id,
              params: [] of Frontend::Parameter,
              return_annotation: "Bool",
              scope: dummy_scope
            )
          when "value"
            methods << MethodSymbol.new(
              method_name,
              dummy_node_id,
              params: [] of Frontend::Parameter,
              return_annotation: element_type_name,
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
          when "put_if_absent"
            key_param = Frontend::Parameter.new(name: "key".to_slice, type_annotation: key_type_name.to_slice)
            block_param = Frontend::Parameter.new(name: "block".to_slice, is_block: true)
            methods << MethodSymbol.new(method_name, dummy_node_id, params: [key_param, block_param], return_annotation: value_type_name, scope: dummy_scope)
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

        private def get_enum_builtin_methods(enum_type : EnumType, method_name : String) : Array(MethodSymbol)
          methods = [] of MethodSymbol

          dummy_node_id = ExprId.new(0)
          dummy_scope = SymbolTable.new(nil)
          enum_name = enum_type.symbol.name

          case method_name
          when "&", "|", "^"
            param = Frontend::Parameter.new(name: "other".to_slice, type_annotation: enum_name.to_slice)
            methods << MethodSymbol.new(
              method_name,
              dummy_node_id,
              params: [param],
              return_annotation: enum_name,
              scope: dummy_scope
            )
          when "includes?"
            param = Frontend::Parameter.new(name: "other".to_slice, type_annotation: enum_name.to_slice)
            methods << MethodSymbol.new(
              method_name,
              dummy_node_id,
              params: [param],
              return_annotation: "Bool",
              scope: dummy_scope
            )
          else
            if method_name.ends_with?("?")
              predicate_name = method_name[0...-1]
              if enum_type.symbol.members.keys.any? { |member_name| member_name.underscore == predicate_name }
                methods << MethodSymbol.new(
                  method_name,
                  dummy_node_id,
                  params: [] of Frontend::Parameter,
                  return_annotation: "Bool",
                  scope: dummy_scope
                )
              end
            end
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
                param_name = intern_name(name_slice)
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
          arg_types = [] of Type
          if args = node.args
            args.each { |arg_id| arg_types << infer_expression(arg_id) }
          end

          if current_call = @yield_call_stack.last?
            saved_call = @yield_call_stack.pop
            saved_return = @yield_return_stack.pop

            begin
              return infer_call_block_with_param_types(current_call, arg_types) || saved_return || @context.nil_type
            ensure
              @yield_return_stack << saved_return
              @yield_call_stack << saved_call
            end
          end

          @yield_return_stack.last? || @context.nil_type
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
                               parse_type_name(intern_name(type_ann))
                             else
                               @context.nil_type
                             end
                @assignments[intern_name(param_name)] = param_type
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
                @assignments.delete(intern_name(param_name))
              end
            end
          end

          # Determine return type:
          # 1. Use explicit return type annotation if present
          # 2. Otherwise use inferred body type
          # 3. Default to Nil for empty body
          return_type = if rt = node.return_type
                          parse_type_name(intern_name(rt))
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
          params = init_symbol.params.reject(&.is_block)
          required = count_required_params(params)
          has_splat = params.any? { |param| param.is_splat || param.is_double_splat }
          max = has_splat ? Int32::MAX : params.size
          return nil unless arg_types.size >= required && arg_types.size <= max

          # Build type parameter binding map
          binding = {} of String => Type

          arg_types.each_with_index do |arg_type, index|
            next unless param = params[index]?
            if type_ann = param.type_annotation
              bind_constructor_type_arguments(intern_name(type_ann), arg_type, binding, type_params)
            end
          end

          # Return type arguments in the same order as type_parameters
          type_params.map { |param_name| binding[param_name]? || @context.nil_type }
        end

        private def bind_constructor_type_arguments(
          annotation_name : String,
          actual_type : Type,
          binding : Hash(String, Type),
          type_params : Array(String)
        ) : Nil
          if type_params.includes?(annotation_name)
            binding[annotation_name] = actual_type
            return
          end

          if annotation_name.ends_with?("?") && annotation_name.bytesize > 1
            base_name = annotation_name[0...-1]
            if actual_type.is_a?(UnionType)
              non_nil = actual_type.types.reject { |type| type == @context.nil_type }
              if non_nil.size == 1
                bind_constructor_type_arguments(base_name, non_nil.first, binding, type_params)
                return
              end
            end

            bind_constructor_type_arguments(base_name, actual_type, binding, type_params)
            return
          end

          return unless annotation_name.includes?('(') && annotation_name.ends_with?(')')

          paren_start = annotation_name.index('(')
          return unless paren_start

          base_name = annotation_name[0...paren_start]
          arg_names = split_top_level_generic_args(annotation_name[(paren_start + 1)...-1])
          return if arg_names.empty?

          case actual_type
          when PointerType
            if base_name == "Pointer"
              bind_constructor_type_arguments(arg_names.first, actual_type.element_type, binding, type_params)
            end
          when ArrayType
            if {"Array", "Slice", "StaticArray"}.includes?(base_name)
              bind_constructor_type_arguments(arg_names.first, actual_type.element_type, binding, type_params)
            end
          when HashType
            if base_name == "Hash" && arg_names.size >= 2
              bind_constructor_type_arguments(arg_names[0], actual_type.key_type, binding, type_params)
              bind_constructor_type_arguments(arg_names[1], actual_type.value_type, binding, type_params)
            end
          when TupleType
            if base_name == "Tuple"
              arg_names.zip(actual_type.element_types) do |arg_name, element_type|
                bind_constructor_type_arguments(arg_name, element_type, binding, type_params)
              end
            end
          when InstanceType
            if actual_type.class_symbol.name == base_name && (type_args = actual_type.type_args)
              arg_names.zip(type_args) do |arg_name, type_arg|
                bind_constructor_type_arguments(arg_name, type_arg, binding, type_params)
              end
            end
          when ClassType
            if actual_type.symbol.name == base_name && (type_args = actual_type.type_args)
              arg_names.zip(type_args) do |arg_name, type_arg|
                bind_constructor_type_arguments(arg_name, type_arg, binding, type_params)
              end
            end
          when ModuleType
            if actual_type.symbol.name == base_name && (type_args = actual_type.type_args)
              arg_names.zip(type_args) do |arg_name, type_arg|
                bind_constructor_type_arguments(arg_name, type_arg, binding, type_params)
              end
            end
          end
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

        private def resolve_method_annotation_type(type_name : String, receiver_type : Type?, scope : SymbolTable? = nil) : Type
          if type_name == "self"
            if receiver_type
              return receiver_type
            end

            if @current_method_is_class_method_stack.last?
              if current_class = @current_class
                return class_type_for(current_class)
              end

              if current_module = @current_module
                return module_type_for(current_module)
              end
            end

            if current_class = @current_class
              return instance_type_for(current_class)
            end

            if current_module = @current_module
              return module_type_for(current_module)
            end
          end

          if bound_type = @assignments[type_name]?
            return bound_type
          end

          if receiver_context = receiver_type_parameter_context(receiver_type)
            type_args, type_params = receiver_context
            return substitute_type_parameters(type_name, type_args, type_params)
          end

          if included_context = included_module_type_parameter_context(scope, receiver_type)
            type_args, type_params = included_context
            return substitute_type_parameters(type_name, type_args, type_params)
          end

          resolve_annotation_type_in_scope(type_name, scope)
        end

        private def resolve_annotation_type_in_scope(type_name : String, scope : SymbolTable?) : Type
          return parse_type_name(type_name) unless scope

          if proc_type = resolve_proc_type_name_in_scope(type_name, scope)
            return proc_type
          end

          if scoped_symbol = scope.lookup(type_name)
            if scoped_symbol.is_a?(VariableSymbol)
              if bound_type = @assignments[type_name]?
                return bound_type
              end
            end

            case scoped_symbol
            when AliasSymbol
              return resolve_annotation_type_in_scope(scoped_symbol.target, scope)
            when ClassSymbol
              if builtin_type = lookup_type_by_name(type_name)
                return builtin_type
              end
              return instance_type_for(scoped_symbol)
            when EnumSymbol
              return EnumType.new(scoped_symbol)
            when ModuleSymbol
              return module_type_for(scoped_symbol)
            end
          elsif builtin_type = lookup_type_by_name(type_name)
            return builtin_type
          end

          if type_name.ends_with?("?") && type_name.size > 1
            base_type = resolve_annotation_type_in_scope(type_name[0...-1], scope)
            return union_of([base_type, @context.nil_type])
          end

          if type_name.ends_with?("*") && type_name.size > 1
            element_type = resolve_annotation_type_in_scope(type_name[0...-1], scope)
            return PointerType.new(element_type)
          end

          if type_name.ends_with?(".class") && type_name.size > 6
            base_type = resolve_annotation_type_in_scope(type_name[0...-6], scope)
            return class_type_reference_for(base_type)
          end

          if union = resolve_annotation_union_type_in_scope(type_name, scope)
            return union
          end

          if tuple_like = parse_brace_collection_type_name(type_name) { |type_part| resolve_annotation_type_in_scope(type_part, scope) }
            return tuple_like
          end

          if type_name.includes?('(') && type_name.includes?(')')
            paren_start = type_name.index('(').not_nil!
            paren_end = type_name.rindex(')').not_nil!
            base_type = type_name[0...paren_start]
            type_args = split_top_level_generic_args(type_name[(paren_start + 1)...paren_end])
            resolved_args = Array(Type).new(type_args.size)
            type_args.each do |arg_name|
              resolved_args << resolve_annotation_type_in_scope(arg_name, scope)
            end

            case base_type
            when "Array", "Slice", "StaticArray"
              return ArrayType.new(resolved_args.first? || @context.nil_type)
            when "Pointer"
              return PointerType.new(resolved_args.first? || @context.nil_type)
            when "Tuple"
              return TupleType.new(resolved_args)
            end

            if symbol = scope.lookup(base_type)
              case symbol
              when AliasSymbol
                return resolve_annotation_type_in_scope(symbol.target, scope)
              when ClassSymbol
                return instance_type_for(symbol, resolved_args.empty? ? nil : resolved_args)
              when EnumSymbol
                return EnumType.new(symbol)
              when ModuleSymbol
                return module_type_for(symbol, resolved_args.empty? ? nil : resolved_args)
              end
            end
          end

          parse_type_name(type_name)
        end

        private def resolve_annotation_union_type_in_scope(type_name : String, scope : SymbolTable?) : Type?
          size = type_name.bytesize
          return nil if size < 5

          types = [] of Type
          start = 0
          i = 0
          found = false
          while i + 2 < size
            if type_name.byte_at(i) == 32_u8 && type_name.byte_at(i + 1) == 124_u8 && type_name.byte_at(i + 2) == 32_u8
              found = true
              types << resolve_annotation_type_in_scope(trim_slice(type_name, start, i), scope)
              i += 3
              start = i
              next
            end
            i += 1
          end

          return nil unless found

          types << resolve_annotation_type_in_scope(trim_slice(type_name, start, size), scope)
          union_of(types)
        end

        private def parse_proc_type_name(type_name : String) : Type?
          arrow_index = find_top_level_arrow(type_name)
          return nil unless arrow_index

          params_part = trim_slice(type_name, 0, arrow_index)
          return_part = trim_slice(type_name, arrow_index + 2, type_name.bytesize)

          param_types = [] of Type
          unless params_part.empty?
            split_top_level_generic_args(params_part).each do |param_name|
              param_types << parse_type_name(param_name)
            end
          end

          return_type = return_part.empty? || return_part == "_" ? unknown_type : parse_type_name(return_part)
          ProcType.new(param_types, return_type)
        end

        private def resolve_proc_type_name_in_scope(type_name : String, scope : SymbolTable) : Type?
          arrow_index = find_top_level_arrow(type_name)
          return nil unless arrow_index

          params_part = trim_slice(type_name, 0, arrow_index)
          return_part = trim_slice(type_name, arrow_index + 2, type_name.bytesize)

          param_types = [] of Type
          unless params_part.empty?
            split_top_level_generic_args(params_part).each do |param_name|
              param_types << resolve_annotation_type_in_scope(param_name, scope)
            end
          end

          return_type = return_part.empty? || return_part == "_" ? unknown_type : resolve_annotation_type_in_scope(return_part, scope)
          ProcType.new(param_types, return_type)
        end

        private def find_top_level_arrow(type_name : String) : Int32?
          depth = 0
          i = 0
          limit = type_name.bytesize - 1
          while i < limit
            case type_name.byte_at(i)
            when 40_u8
              depth += 1
            when 41_u8
              depth -= 1 if depth > 0
            when 45_u8
              if depth == 0 && type_name.byte_at(i + 1) == 62_u8
                return i
              end
            end
            i += 1
          end
          nil
        end

        private def receiver_type_parameter_context(receiver_type : Type?) : {Array(Type), Array(String)}?
          case receiver_type
          when InstanceType
            type_args = receiver_type.type_args
            type_params = receiver_type.class_symbol.type_parameters
            if type_args && type_params && !type_args.empty? && !type_params.empty?
              return {type_args, type_params}
            end
          when ClassType
            type_args = receiver_type.type_args
            type_params = receiver_type.symbol.type_parameters
            if type_args && type_params && !type_args.empty? && !type_params.empty?
              return {type_args, type_params}
            end
          when ModuleType
            type_args = receiver_type.type_args
            type_params = receiver_type.symbol.type_parameters
            if type_args && type_params && !type_args.empty? && !type_params.empty?
              return {type_args, type_params}
            end
          end

          nil
        end

        private def included_module_type_parameter_context(scope : SymbolTable?, receiver_type : Type?) : {Array(Type), Array(String)}?
          return nil unless scope && receiver_type
          owner_module = scope.owner_module
          return nil unless owner_module

          type_params = owner_module.type_parameters
          return nil unless type_params && !type_params.empty?

          receiver_context = receiver_type_parameter_context(receiver_type)
          receiver_include_search_scopes(receiver_type).each do |search_scope|
            if resolved_args = find_included_module_type_args(search_scope, owner_module, receiver_context)
              return {resolved_args, type_params}
            end
          end

          nil
        end

        private def receiver_include_search_scopes(receiver_type : Type) : Array(SymbolTable)
          case receiver_type
          when InstanceType
            [receiver_type.class_symbol.scope]
          when ClassType
            [receiver_type.symbol.class_scope, receiver_type.symbol.scope]
          when ModuleType
            [receiver_type.symbol.scope]
          else
            [] of SymbolTable
          end
        end

        private def find_included_module_type_args(
          scope : SymbolTable,
          owner_module : ModuleSymbol,
          current_context : {Array(Type), Array(String)}?,
          visited : Set(SymbolTable) = Set(SymbolTable).new
        ) : Array(Type)?
          return nil unless visited.add?(scope)

          scope.included_modules.each do |ref|
            resolved_args = resolve_included_module_type_args(ref, current_context)
            return resolved_args if ref.symbol == owner_module

            child_context = if child_type_params = ref.symbol.type_parameters
                              if !child_type_params.empty? && resolved_args.size == child_type_params.size
                                {resolved_args, child_type_params}
                              else
                                nil
                              end
                            end

            if nested = find_included_module_type_args(ref.symbol.scope, owner_module, child_context, visited)
              return nested
            end
          end

          nil
        end

        private def resolve_included_module_type_args(
          ref : IncludedModuleRef,
          current_context : {Array(Type), Array(String)}?
        ) : Array(Type)
          type_arg_names = ref.type_arg_names
          return [] of Type unless type_arg_names && !type_arg_names.empty?

          resolved_args = Array(Type).new(type_arg_names.size)
          type_arg_names.each do |type_arg_name|
            if current_context
              type_args, type_params = current_context
              resolved = substitute_type_parameters(type_arg_name, type_args, type_params)
              resolved_args << (unknownish_type?(resolved) ? parse_type_name(type_arg_name) : resolved)
            else
              resolved_args << parse_type_name(type_arg_name)
            end
          end

          resolved_args
        end

        # Week 1: Recursive substitution for receiver/class type parameters.
        private def substitute_type_parameters(type_name : String, type_args : Array(Type), type_params : Array(String)) : Type
          if idx = type_params.index(type_name)
            return type_args[idx] if idx < type_args.size
          end

          if type_name.ends_with?("?") && type_name.size > 1
            base_name = type_name[0...-1]
            return union_of([substitute_type_parameters(base_name, type_args, type_params), @context.nil_type])
          end

          if type_name.ends_with?("*") && type_name.size > 1
            element_name = type_name[0...-1]
            return PointerType.new(substitute_type_parameters(element_name, type_args, type_params))
          end

          if type_name.ends_with?(".class") && type_name.size > 6
            base_name = type_name[0...-6]
            return class_type_reference_for(substitute_type_parameters(base_name, type_args, type_params))
          end

          if union = substitute_union_type_parameters(type_name, type_args, type_params)
            return union
          end

          if type_name.includes?('(') && type_name.includes?(')')
            paren_start = type_name.index('(').not_nil!
            paren_end = type_name.rindex(')').not_nil!

            base_type = type_name[0...paren_start]
            arg_list = type_name[(paren_start + 1)...paren_end]
            resolved_args = split_top_level_generic_args(arg_list).map do |arg_name|
              substitute_type_parameters(arg_name, type_args, type_params)
            end

            if resolved = resolve_generic_type_application(base_type, resolved_args)
              return resolved
            end

            if symbol = lookup_type_symbol(base_type) || find_class_symbol_by_suffix(base_type)
              case symbol
              when AliasSymbol
                return substitute_type_parameters(symbol.target, type_args, type_params)
              end
            end
          end

          parse_type_name(type_name)
        end

        private def resolve_generic_type_application(base_type : String, resolved_args : Array(Type)) : Type?
          case base_type
          when "Array", "Slice"
            ArrayType.new(resolved_args.first? || @context.nil_type)
          when "Pointer"
            PointerType.new(resolved_args.first? || @context.nil_type)
          when "StaticArray"
            ArrayType.new(resolved_args.first? || @context.nil_type)
          when "Tuple"
            TupleType.new(resolved_args)
          when "Hash"
            HashType.new(resolved_args[0]? || @context.nil_type, resolved_args[1]? || @context.nil_type)
          else
            if symbol = lookup_type_symbol(base_type) || find_class_symbol_by_suffix(base_type)
              case symbol
              when ClassSymbol
                instance_type_for(symbol, resolved_args)
              when ModuleSymbol
                module_type_for(symbol, resolved_args)
              when EnumSymbol
                EnumType.new(symbol)
              end
            end
          end
        end

        private def substitute_union_type_parameters(type_name : String, type_args : Array(Type), type_params : Array(String)) : Type?
          size = type_name.bytesize
          return nil if size < 5

          types = [] of Type
          start = 0
          i = 0
          found = false
          while i + 2 < size
            if type_name.byte_at(i) == 32_u8 && type_name.byte_at(i + 1) == 124_u8 && type_name.byte_at(i + 2) == 32_u8
              found = true
              types << substitute_type_parameters(trim_slice(type_name, start, i), type_args, type_params)
              i += 3
              start = i
              next
            end
            i += 1
          end

          return nil unless found

          types << substitute_type_parameters(trim_slice(type_name, start, size), type_args, type_params)
          union_of(types)
        end

        private def split_top_level_generic_args(arg_list : String) : Array(String)
          args = [] of String
          start = 0
          paren_depth = 0
          brace_depth = 0
          bracket_depth = 0
          i = 0

          while i < arg_list.bytesize
            case arg_list.byte_at(i)
            when 40_u8
              paren_depth += 1
            when 41_u8
              paren_depth -= 1 if paren_depth > 0
            when 123_u8
              brace_depth += 1
            when 125_u8
              brace_depth -= 1 if brace_depth > 0
            when 91_u8
              bracket_depth += 1
            when 93_u8
              bracket_depth -= 1 if bracket_depth > 0
            when 44_u8
              if paren_depth == 0 && brace_depth == 0 && bracket_depth == 0
                args << trim_slice(arg_list, start, i)
                start = i + 1
              end
            end
            i += 1
          end

          args << trim_slice(arg_list, start, arg_list.bytesize)
          args
        end

        private def parse_brace_collection_type_name(name : String, &resolver : String -> Type) : Type?
          return nil unless name.starts_with?('{') && name.ends_with?('}')

          inner = name[1...-1]
          stripped = inner.strip
          return TupleType.new([] of Type) if stripped.empty?

          parts = split_top_level_generic_args(stripped)
          if parts.all? { |part| top_level_named_tuple_separator(part) }
            entries = [] of {String, Type}
            parts.each do |part|
              colon_index = top_level_named_tuple_separator(part).not_nil!
              key = trim_slice(part, 0, colon_index)
              value_name = trim_slice(part, colon_index + 1, part.bytesize)
              entries << {key, yield value_name}
            end
            return NamedTupleType.new(entries)
          end

          TupleType.new(parts.map { |part| yield part })
        end

        private def top_level_named_tuple_separator(part : String) : Int32?
          paren_depth = 0
          brace_depth = 0
          bracket_depth = 0
          i = 0

          while i < part.bytesize
            case part.byte_at(i)
            when 40_u8
              paren_depth += 1
            when 41_u8
              paren_depth -= 1 if paren_depth > 0
            when 123_u8
              brace_depth += 1
            when 125_u8
              brace_depth -= 1 if brace_depth > 0
            when 91_u8
              bracket_depth += 1
            when 93_u8
              bracket_depth -= 1 if bracket_depth > 0
            when 58_u8
              return i if paren_depth == 0 && brace_depth == 0 && bracket_depth == 0
            end
            i += 1
          end

          nil
        end

        # Week 1: Infer method body type when there's no return annotation
        # Used for generic methods like: def direct_value; @value; end
        private def infer_method_body_type(
          method : MethodSymbol,
          receiver_type : Type,
          arg_types : Array(Type)? = nil,
          call_node : Frontend::CallNode? = nil
        ) : Type
          guard_watchdog!
          if defer_generic_module_method_body?(method, receiver_type)
            debug_hook("infer.method_body.defer_generic_module", "method=#{method.name} receiver=#{receiver_type}")
            return @context.nil_type
          end

          pushed_return_frame = false
          pushed_yield_return_frame = false
          pushed_yield_call_frame = false
          bound_param_names = [] of String
          previous_param_assignments = {} of String => Type?
          cacheable = arg_types.nil? || arg_types.empty?

          # If we've already inferred this method body, reuse it to prevent cycles
          if cacheable && (cached = @method_body_cache[method]?)
            debug_hook("infer.method_body.cache", "method=#{method.name} receiver=#{receiver_type}")
            return cached
          end

          # Break recursive inference cycles gracefully
          if @method_body_in_progress.includes?(method)
            debug_hook("infer.method_body.cycle", "method=#{method.name} receiver=#{receiver_type}")
            return @context.nil_type
          end

          @method_body_in_progress << method
          debug_hook("infer.method_body.start", "method=#{method.name} receiver=#{receiver_type}")

          if ENV["DEBUG"]?
            puts "DEBUG infer_method_body_type:"
            puts "  method: #{method.name}"
            puts "  receiver_type: #{receiver_type.class} = #{receiver_type.inspect}"
          end

          # Get the method's DefNode to access its body
          def_node = @arena[method.node_id]
          unless def_node.is_a?(Frontend::DefNode)
            puts "  ERROR: def_node is not DefNode!" if ENV["DEBUG"]?
            debug_hook("infer.method_body.error", "method=#{method.name} error=not_def_node")
            return unknown_type
          end

          # Get the method body
          body = def_node.body
          unless body && !body.empty?
            puts "  ERROR: body is empty!" if ENV["DEBUG"]?
            debug_hook("infer.method_body.error", "method=#{method.name} error=empty_body")
            return unknown_type
          end

          # Save current receiver context
          previous_receiver_context = @receiver_type_context
          previous_class = @current_class
          previous_module = @current_module
          previous_method_scope = @current_method_scope
          @current_method_is_class_method_stack << method.is_class_method?
          @method_return_stack << [] of Type
          pushed_return_frame = true

          # Set receiver context for type parameter substitution
          if receiver_type.is_a?(InstanceType)
            @receiver_type_context = receiver_type
            @current_class = receiver_type.class_symbol
            if ENV["DEBUG"]?
              puts "  Set receiver_type_context: Box(#{receiver_type.type_args.inspect})"
            end
          elsif receiver_type.is_a?(ClassType)
            @receiver_type_context = receiver_type
            @current_class = receiver_type.symbol
          elsif receiver_type.is_a?(ModuleType)
            @receiver_type_context = receiver_type
            @current_module = receiver_type.symbol
          elsif receiver_type.is_a?(PrimitiveType)
            @receiver_type_context = receiver_type
            @current_class = primitive_instance_class_symbol(receiver_type)
          end
          if owner_module = method.scope.owner_module
            @current_module = owner_module
          end
          @current_method_scope = method.scope

          if arg_types
            method.params.each_with_index do |param, index|
              next if param.is_block
              next unless param_name_slice = param.name

              arg_type = arg_types[index]?
              if arg_type.nil?
                if default_value = param.default_value
                  arg_type = infer_expression(default_value)
                else
                  next
                end
              end

              param_name = intern_name(param_name_slice)
              previous_param_assignments[param_name] = @assignments[param_name]?
              @assignments[param_name] = arg_type
              bound_param_names << param_name
            end
          end

          infer_method_type_argument_bindings(method, receiver_type, arg_types || [] of Type, call_node).each do |type_param_name, type_arg|
            previous_param_assignments[type_param_name] = @assignments[type_param_name]?
            @assignments[type_param_name] = type_arg
            bound_param_names << type_param_name unless bound_param_names.includes?(type_param_name)
          end

          if included_context = included_module_type_parameter_context(method.scope, receiver_type)
            type_args, type_params = included_context
            type_params.zip(type_args).each do |type_param_name, type_arg|
              previous_param_assignments[type_param_name] = @assignments[type_param_name]?
              @assignments[type_param_name] = type_arg
              bound_param_names << type_param_name unless bound_param_names.includes?(type_param_name)
            end
          end

          yielded_block_return_type = call_node.try do |current_call|
            infer_method_block_result_type(method, receiver_type, current_call).try(&.[0])
          end
          @yield_return_stack << yielded_block_return_type
          pushed_yield_return_frame = true
          untyped_yield_call = if yielded_block_return_type.nil? &&
                                  call_node &&
                                  method.params.any?(&.is_block) &&
                                  call_has_block?(call_node)
                                 call_node
                               end
          @yield_call_stack << untyped_yield_call
          pushed_yield_call_frame = true

          # Re-infer the body under the current receiver/return context instead
          # of reusing eager pass results, because those earlier caches were
          # computed without explicit-return collection and may lack the right
          # receiver context.
          body.each do |expr_id|
            clear_cached_type_tree(expr_id)
          end

          implicit_return_type = infer_block_result(body)
          explicit_return_types = @method_return_stack.last
          result_type = combine_method_return_types(implicit_return_type, explicit_return_types)

          # Cache the result to avoid re-inferring on subsequent calls
          @method_body_cache[method] = result_type if cacheable

          if ENV["DEBUG"]?
            puts "  result_type: #{result_type.class} = #{result_type.inspect}"
          end
          debug_hook("infer.method_body.done", "method=#{method.name} result=#{result_type}")

          # Restore context
          @receiver_type_context = previous_receiver_context
          @current_class = previous_class
          @current_module = previous_module
          @current_method_scope = previous_method_scope
          bound_param_names.each do |param_name|
            if previous_param_assignments.has_key?(param_name)
              if previous_type = previous_param_assignments[param_name]
                @assignments[param_name] = previous_type
              else
                @assignments.delete(param_name)
              end
            else
              @assignments.delete(param_name)
            end
          end

          result_type
        ensure
          @current_method_is_class_method_stack.pop unless @current_method_is_class_method_stack.empty?
          @yield_call_stack.pop if pushed_yield_call_frame && !@yield_call_stack.empty?
          @yield_return_stack.pop if pushed_yield_return_frame && !@yield_return_stack.empty?
          @method_return_stack.pop if pushed_return_frame && !@method_return_stack.empty?
          @method_body_in_progress.delete(method)
        end

        private def combine_method_return_types(implicit_return_type : Type, explicit_return_types : Array(Type)) : Type
          return implicit_return_type if explicit_return_types.empty?

          types = explicit_return_types.dup
          types << implicit_return_type
          union_of(types)
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
            case_value_node = @arena[case_value_id]
            if case_value_node.is_a?(Frontend::IdentifierNode)
              case_var_name = intern_name(case_value_node.name)
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
            cond_node = @arena[cond_id]

            case cond_node
            when Frontend::IdentifierNode
              # when String, when Int32, etc. (type as identifier)
              type_name = intern_name(cond_node.name)
              if is_type_name?(type_name)
                return preserve_structural_narrowing(case_type, type_name, parse_type_name(type_name))
              end
            when Frontend::PathNode
              # when Foo::Bar (namespaced type)
              if type = resolve_path_as_type(cond_node)
                return type
              end
            when Frontend::IsANode
              # when .is_a?(Type) - rarely used in case but possible
              type_name = intern_name(cond_node.target_type)
              return parse_type_name(type_name)
            when Frontend::MemberAccessNode
              # when .class (check if it's a .class call on type)
              if member_name = extract_member_name(cond_node)
                if member_name == "class"
                  # This is x.class - the object is the type
                  object_type = infer_expression(cond_node.object)
                  if object_type.is_a?(ClassType)
                    return instance_type_for(object_type.symbol)
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
              return instance_type_for(symbol)
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
              return instance_type_for(current)
            end
          end

          nil
        end

        # Phase 97: Collect path segments from PathNode
        private def collect_path_segments_for_type(node : Frontend::PathNode, segments : Array(String))
          if left_id = node.left
            left_node = @arena[left_id]
            case left_node
            when Frontend::PathNode
              collect_path_segments_for_type(left_node, segments)
            when Frontend::IdentifierNode
              segments << intern_name(left_node.name)
            when Frontend::ConstantNode
              segments << intern_name(left_node.name)
            end
          end

          right_node = @arena[node.right]
          case right_node
          when Frontend::IdentifierNode
            segments << intern_name(right_node.name)
          when Frontend::ConstantNode
            segments << intern_name(right_node.name)
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
            intern_name(member_slice)
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
            # TODO: honor explicit annotations in Phase 91B; current spec expects Nil/Nil
            return HashType.new(@context.nil_type, @context.nil_type)
          end

          # OPTIMIZATION: For large hashes of uniform types, sample first few entries
          if entries.size > 10
            sample_key_types = Array(Type).new(3)
            sample_value_types = Array(Type).new(3)
            3.times do |i|
              break if i >= entries.size
              sample_key_types << infer_expression(entries[i].key)
              sample_value_types << infer_expression(entries[i].value)
            end

            # Check if all samples are same types
            first_key = sample_key_types.first?
            first_value = sample_value_types.first?
            if first_key && first_value &&
               sample_key_types.all? { |t| t == first_key } && first_key.is_a?(PrimitiveType) &&
               sample_value_types.all? { |t| t == first_value } && first_value.is_a?(PrimitiveType)
              # Uniform types - use sampled types
              return HashType.new(first_key, first_value)
            end
            # Mixed types - fall back to full inference
          end

          key_types = Array(Type).new(entries.size)
          value_types = Array(Type).new(entries.size)

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
            key = intern_name(entry.key)
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
                   node = @arena[node_id]
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
            primary_span: span,
            primary_node_id: node_id
          )
          @diagnostics << diagnostic
        end
      end
    end
  end
end
