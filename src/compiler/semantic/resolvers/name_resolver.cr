require "../../frontend/ast"
require "../../frontend/parser/diagnostic"
require "../symbol_table"
require "../symbol"

module CrystalV2
  module Compiler
    module Semantic
      class NameResolver
        BLOCK_SYMBOL_NODE_ID = Frontend::ExprId.new(-1)
        alias Program = Frontend::Program
        alias ExprId = Frontend::ExprId
        alias Diagnostic = Frontend::Diagnostic

        @root_table : SymbolTable
        @current_table : SymbolTable

        struct Result
          getter identifier_symbols : Hash(ExprId, Symbol)
          getter diagnostics : Array(Diagnostic)

          def initialize(@identifier_symbols : Hash(ExprId, Symbol), @diagnostics : Array(Diagnostic))
          end
        end

        def initialize(@program : Program, root_table : SymbolTable)
          @arena = @program.arena
          @root_table = root_table
          @identifier_symbols = {} of ExprId => Symbol
          @diagnostics = [] of Diagnostic
          @current_table = @root_table
        end

        def resolve : Result
          @current_table = @root_table
          @program.roots.each do |root_id|
            visit(root_id)
          end
          Result.new(@identifier_symbols, @diagnostics)
        end

        private def visit(node_id : ExprId)
          return if node_id.invalid?
          node = @arena[node_id]

          case node
          when Frontend::IdentifierNode
            resolve_identifier(node_id, node)
          when Frontend::AssignNode
            # Visit the value first (it may reference existing variables)
            visit(node.value)
            # Then handle the target (which declares a new variable if it's an identifier)
            handle_assign_target(node.target)
          when Frontend::MemberAccessNode
            visit(node.object)
          when Frontend::CallNode
            if callee_id = node.callee
              visit(callee_id)
            end
            node.args.each { |arg| visit(arg) }
            if block_id = node.block
              visit(block_id)
            end
            node.named_args.try &.each { |named| visit(named.value) }
          when Frontend::UnaryNode
            visit(node.operand)
          when Frontend::BinaryNode
            visit(node.left)
            visit(node.right)
          when Frontend::GroupingNode
            visit(node.expression)
          when Frontend::MacroExpressionNode
            visit(node.expression)
          when Frontend::MacroLiteralNode
            visit_macro_literal(node)
          when Frontend::MacroDefNode
            # Body handled via MacroLiteral; skip definition node
          when Frontend::DefNode
            visit_def(node_id, node)
          when Frontend::ClassNode
            visit_class(node_id, node)
          when Frontend::ConstantNode
            visit(node.value)
          when Frontend::IfNode
            visit_if(node)
          when Frontend::UnlessNode
            visit_unless(node)
          when Frontend::WhileNode
            visit_while(node)
          when Frontend::UntilNode
            visit_until(node)
          when Frontend::LoopNode
            visit_loop(node)
          when Frontend::BlockNode
            visit_block(node)
          when Frontend::ProcLiteralNode
            visit_proc_literal(node)
          when Frontend::ModuleNode
            visit_module(node_id, node)
          else
            # Other kinds currently unsupported; ignore
          end
        end

        private def resolve_identifier(node_id : ExprId, node : Frontend::IdentifierNode)
          slice = node.name
          return unless slice
          name = String.new(slice)

          debug("[NameResolver] resolve #{name} in table=#{@current_table.object_id}")
          if symbol = @current_table.lookup(name)
            debug("[NameResolver] matched #{name} -> #{symbol.class}")
            @identifier_symbols[node_id] = symbol
          else
            debug("[NameResolver] unresolved #{name}")
            @diagnostics << Diagnostic.new("undefined local variable or method '#{name}'", node.span)
          end
        end

        # Handle assignment target - creates a new local variable if it's an identifier
        private def handle_assign_target(target_id : ExprId)
          return if target_id.invalid?
          target_node = @arena[target_id]

          # Only handle simple identifier targets (not instance vars, etc.)
          if target_node.is_a?(Frontend::IdentifierNode)
            slice = target_node.name
            return unless slice
            name = String.new(slice)

            # Check if variable already exists (reassignment vs first assignment)
            if existing = @current_table.lookup_local(name)
              # Reassignment: reuse existing symbol, update identifier mapping
              @identifier_symbols[target_id] = existing
            else
              # First assignment: create new symbol
              symbol = VariableSymbol.new(name, target_id)
              @current_table.define(name, symbol)
              @identifier_symbols[target_id] = symbol
            end
          else
            # For other assignment targets (instance vars, indexed access, etc.), just visit them
            visit(target_id)
          end
        end

        private def visit_macro_literal(node : Frontend::MacroLiteralNode)
          pieces = node.pieces
          return unless pieces

          pieces.each do |piece|
            case piece.kind
            when Frontend::MacroPiece::Kind::Expression
              visit(piece.expr.not_nil!) if piece.expr
            when Frontend::MacroPiece::Kind::ControlStart,
                 Frontend::MacroPiece::Kind::ControlElseIf,
                 Frontend::MacroPiece::Kind::ControlElse,
                 Frontend::MacroPiece::Kind::ControlEnd
              # TODO: Handle control flow bodies once semantic stages support them
            when Frontend::MacroPiece::Kind::Text
              # No identifiers to resolve
            end
          end
        end

        private def visit_def(node_id : ExprId, node : Frontend::DefNode)
          name_slice = node.name
          return unless name_slice

          name = String.new(name_slice)
          symbol = @current_table.lookup(name)

          method_symbol = case symbol
          when MethodSymbol
            symbol
          when OverloadSetSymbol
            symbol.overloads.find { |overload| overload.node_id == node_id } || symbol.overloads.last?
          else
            nil
          end

          return unless method_symbol

          method_scope = method_symbol.scope
          @identifier_symbols[node_id] = method_symbol
          prev_table = @current_table
          @current_table = method_scope

          (node.body || [] of ExprId).each do |expr_id|
            visit(expr_id)
          end

          @current_table = prev_table
        end

        private def visit_class(node_id : ExprId, node : Frontend::ClassNode)
          name_slice = node.name
          return unless name_slice

          name = String.new(name_slice)
          symbol = @current_table.lookup(name)
          unless symbol.is_a?(ClassSymbol)
            return
          end

          class_scope = symbol.scope
          prev_table = @current_table
          @current_table = class_scope

          (node.body || [] of ExprId).each do |expr_id|
            visit(expr_id)
          end

          @current_table = prev_table
        end

        private def visit_module(node_id : ExprId, node : Frontend::ModuleNode)
          name_slice = node.name
          return unless name_slice

          name = String.new(name_slice)
          symbol = @current_table.lookup(name)
          unless symbol.is_a?(ModuleSymbol)
            return
          end

          @identifier_symbols[node_id] = symbol

          prev_table = @current_table
          @current_table = symbol.scope
          (node.body || [] of ExprId).each { |expr_id| visit(expr_id) }
          @current_table = prev_table
        end

        # Control flow node visitors

        private def visit_if(node : Frontend::IfNode)
          # Visit condition
          visit(node.condition)
          # Visit then body
          node.then_body.each { |expr_id| visit(expr_id) }
          # Visit elsif branches
          node.elsifs.try &.each do |elsif_branch|
            visit(elsif_branch.condition)
            elsif_branch.body.each { |expr_id| visit(expr_id) }
          end
          # Visit else body
          node.else_body.try &.each { |expr_id| visit(expr_id) }
        end

        private def visit_unless(node : Frontend::UnlessNode)
          # Visit condition
          visit(node.condition)
          # Visit then branch
          node.then_branch.each { |expr_id| visit(expr_id) }
          # Visit else branch
          node.else_branch.try &.each { |expr_id| visit(expr_id) }
        end

        private def visit_while(node : Frontend::WhileNode)
          # Visit condition
          visit(node.condition)
          # Visit body
          node.body.each { |expr_id| visit(expr_id) }
        end

        private def visit_until(node : Frontend::UntilNode)
          # Visit condition
          visit(node.condition)
          # Visit body
          node.body.each { |expr_id| visit(expr_id) }
        end

        private def visit_loop(node : Frontend::LoopNode)
          node.body.each { |expr_id| visit(expr_id) }
        end

        private def visit_block(node : Frontend::BlockNode)
          prev_table = @current_table
          block_scope = SymbolTable.new(prev_table)
          @current_table = block_scope

          node.params.try do |params|
            params.each do |param|
              if default = param.default_value
                visit(default)
              end

              next unless param_name = param.name
              name = String.new(param_name)
              declared_type = param.type_annotation.try { |ann| String.new(ann) }

              debug("[NameResolver] define block param #{name}")
              unless block_scope.lookup_local(name)
                block_scope.define(name, VariableSymbol.new(name, BLOCK_SYMBOL_NODE_ID, declared_type: declared_type))
              end
            end
          end

          node.body.each { |expr_id| visit(expr_id) }

          @current_table = prev_table
        end

        private def visit_proc_literal(node : Frontend::ProcLiteralNode)
          prev_table = @current_table
          proc_scope = SymbolTable.new(prev_table)
          @current_table = proc_scope

          node.params.try do |params|
            params.each do |param|
              if default = param.default_value
                visit(default)
              end

              next unless param_name = param.name
              name = String.new(param_name)
              declared_type = param.type_annotation.try { |ann| String.new(ann) }

              debug("[NameResolver] define proc param #{name}")
              unless proc_scope.lookup_local(name)
                proc_scope.define(name, VariableSymbol.new(name, BLOCK_SYMBOL_NODE_ID, declared_type: declared_type))
              end
            end
          end

          node.body.each { |expr_id| visit(expr_id) }

          @current_table = prev_table
        end

        private def debug(message : String)
          return unless ENV.has_key?("LSP_DEBUG_BLOCK")
          STDOUT.puts(message)
        end
      end
    end
  end
end
