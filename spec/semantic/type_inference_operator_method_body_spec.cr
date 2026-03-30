require "spec"

require "../../src/compiler/frontend/ast"
require "../../src/compiler/frontend/lexer"
require "../../src/compiler/frontend/parser"
require "../../src/compiler/semantic/analyzer"
require "../../src/compiler/semantic/type_inference_engine"

alias Frontend = CrystalV2::Compiler::Frontend
alias Semantic = CrystalV2::Compiler::Semantic

private def infer_operator_method_body_types(source : String)
  parser = Frontend::Parser.new(Frontend::Lexer.new(source))
  program = parser.parse_program

  analyzer = Semantic::Analyzer.new(program)
  analyzer.collect_symbols
  name_result = analyzer.resolve_names
  engine = analyzer.infer_types(name_result.identifier_symbols)

  {program, analyzer, engine}
end

describe Semantic::TypeInferenceEngine do
  describe "binary operator methods without return annotations" do
    it "infers method bodies instead of collapsing to Nil" do
      source = <<-CRYSTAL
        struct UInt32
          def //(other : Int32)
            self
          end
        end

        module Probe
          def self.wrap(value : UInt32)
            value //= 10000
            value
          end
        end

        Probe.wrap(1_u32)
      CRYSTAL

      program, analyzer, engine = infer_operator_method_body_types(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.select(&.level.error?).should be_empty
      engine.context.get_type(program.roots.last).to_s.should eq("UInt32")
    end
  end
end
