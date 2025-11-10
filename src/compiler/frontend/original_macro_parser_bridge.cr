# Hybrid Parser Bridge: Delegates macro body parsing to original Crystal parser
# This allows us to use the proven macro tokenization logic while keeping our optimized parser for everything else

require "../../../src/compiler/crystal/syntax/parser"
require "../../../src/compiler/crystal/syntax/lexer"

module CrystalV2
  module Compiler
    module Frontend
      # Bridge to delegate macro body parsing to original Crystal parser
      class OriginalMacroParserBridge
        def self.parse_macro_body(source : String) : Array(Crystal::ASTNode)
          # Create original lexer and parser
          lexer = Crystal::Lexer.new(source)
          parser = Crystal::Parser.new(source)
          parser.filename = "macro_body"

          # Parse as macro body
          # Note: We wrap in a dummy macro definition to get proper context
          wrapped_source = "macro __dummy__\n#{source}\nend"
          lexer = Crystal::Lexer.new(wrapped_source)
          parser = Crystal::Parser.new(wrapped_source)
          parser.filename = "macro_body"

          # Parse the macro
          ast = parser.parse

          # Extract the body from the dummy macro
          if ast.is_a?(Crystal::Expressions)
            first_node = ast.expressions.first?
            if first_node.is_a?(Crystal::Macro)
              return first_node.body.is_a?(Crystal::Expressions) ? first_node.body.expressions : [first_node.body]
            end
          end

          # Fallback
          [] of Crystal::ASTNode
        end
      end
    end
  end
end
