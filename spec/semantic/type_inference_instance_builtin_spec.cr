require "spec"

require "../../src/compiler/frontend/ast"
require "../../src/compiler/frontend/lexer"
require "../../src/compiler/frontend/parser"
require "../../src/compiler/semantic/symbol_table"
require "../../src/compiler/semantic/symbol"
require "../../src/compiler/semantic/collectors/symbol_collector"
require "../../src/compiler/semantic/resolvers/name_resolver"
require "../../src/compiler/semantic/analyzer"
require "../../src/compiler/semantic/types/type"
require "../../src/compiler/semantic/type_inference_engine"

module TypeInferenceInstanceBuiltinSpecAliases
  alias Frontend = CrystalV2::Compiler::Frontend
  alias Semantic = CrystalV2::Compiler::Semantic
end

include TypeInferenceInstanceBuiltinSpecAliases

private def infer_types_for_instance_builtin(source : String)
  lexer = Frontend::Lexer.new(source)
  parser = Frontend::Parser.new(lexer)
  program = parser.parse_program

  analyzer = Semantic::Analyzer.new(program)
  analyzer.collect_symbols
  name_result = analyzer.resolve_names

  engine = Semantic::TypeInferenceEngine.new(program, name_result.identifier_symbols, analyzer.global_context.symbol_table)
  engine.infer_types

  {analyzer, engine}
end

describe CrystalV2::Compiler::Semantic::TypeInferenceEngine do
  it "resolves builtin String methods on implicit self inside instance methods" do
    source = <<-CRYSTAL
      class String
        def semantic_probe(other : self)
          size = bytesize
          includes?(other) ? size : other.bytesize
        end
      end

      "a".semantic_probe("b")
    CRYSTAL

    analyzer, engine = infer_types_for_instance_builtin(source)

    analyzer.semantic_diagnostics.should be_empty
    analyzer.name_resolver_diagnostics.should be_empty
    engine.diagnostics.should be_empty
  end
end
