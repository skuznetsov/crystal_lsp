require "../../frontend/ast"
require "../../frontend/parser/diagnostic"
require "../symbol_table"
require "../symbol"

module CrystalV2
  module Compiler
    module Semantic
      class NameResolver
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
            visit_def(node)
          when Frontend::ClassNode
            visit_class(node)
          else
            # Other kinds currently unsupported; ignore
          end
        end

        private def resolve_identifier(node_id : ExprId, node : Frontend::IdentifierNode)
          slice = node.name
          return unless slice
          name = String.new(slice)

          if symbol = @current_table.lookup(name)
            @identifier_symbols[node_id] = symbol
          else
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

            # Create a new local variable symbol
            symbol = VariableSymbol.new(name, target_id)
            @current_table.define(name, symbol)
            @identifier_symbols[target_id] = symbol
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

        private def visit_def(node : Frontend::DefNode)
          name_slice = node.name
          return unless name_slice

          name = String.new(name_slice)
          symbol = @current_table.lookup(name)
          unless symbol.is_a?(MethodSymbol)
            return
          end

          method_scope = symbol.scope
          prev_table = @current_table
          @current_table = method_scope

          (node.body || [] of ExprId).each do |expr_id|
            visit(expr_id)
          end

          @current_table = prev_table
        end

        private def visit_class(node : Frontend::ClassNode)
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
      end
    end
  end
end
