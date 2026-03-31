require "spec"

require "../../src/compiler/frontend/ast"
require "../../src/compiler/frontend/lexer"
require "../../src/compiler/frontend/parser"
require "../../src/compiler/semantic/analyzer"
require "../../src/compiler/semantic/type_inference_engine"

alias Frontend = CrystalV2::Compiler::Frontend
alias Semantic = CrystalV2::Compiler::Semantic

private def infer_ternary_narrowing_types(source : String)
  parser = Frontend::Parser.new(Frontend::Lexer.new(source))
  program = parser.parse_program

  analyzer = Semantic::Analyzer.new(program)
  analyzer.collect_symbols
  name_result = analyzer.resolve_names
  engine = analyzer.infer_types(name_result.identifier_symbols)

  {program, analyzer, engine}
end

describe Semantic::TypeInferenceEngine do
  describe "ternary narrowing" do
    it "preserves truthy narrowing for nilable tuples returned from helper methods" do
      source = <<-CRYSTAL
        def read_char_with_bytesize(flag)
          return nil unless flag
          {'a', 1}
        end

        def read_char
          info = read_char_with_bytesize(true)
          info ? info[0] : nil
        end

        read_char()
      CRYSTAL

      program, analyzer, engine = infer_ternary_narrowing_types(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.should be_empty
      {"Char | Nil", "Nil | Char"}.should contain(engine.context.get_type(program.roots.last).to_s)
    end
  end
end
