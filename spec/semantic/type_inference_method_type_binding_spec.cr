require "spec"

require "../../src/compiler/frontend/ast"
require "../../src/compiler/frontend/lexer"
require "../../src/compiler/frontend/parser"
require "../../src/compiler/semantic/analyzer"
require "../../src/compiler/semantic/type_inference_engine"

alias Frontend = CrystalV2::Compiler::Frontend
alias Semantic = CrystalV2::Compiler::Semantic

private def infer_method_type_binding_types(source : String)
  parser = Frontend::Parser.new(Frontend::Lexer.new(source))
  program = parser.parse_program

  analyzer = Semantic::Analyzer.new(program)
  analyzer.collect_symbols
  name_result = analyzer.resolve_names
  engine = Semantic::TypeInferenceEngine.new(program, name_result.identifier_symbols, analyzer.global_context.symbol_table)
  engine.infer_types

  {program, analyzer, engine}
end

describe Semantic::TypeInferenceEngine do
  describe "method type-parameter binding" do
    it "refines placeholder pointer bindings from later concrete arguments" do
      source = <<-CRYSTAL
        struct Float
        end

        module Float::FastFloat
          def self.fastfloat_strncasecmp(input1 : UC*, input2 : UC*, length : Int) : Bool forall UC
            true
          end
        end

        module Float::FastFloat
          module Detail
            def self.parse(first : UC*) : Bool forall UC
              FastFloat.fastfloat_strncasecmp(first, "nan".to_unsafe, 3)
            end
          end
        end

        byte = 0_u8
        Float::FastFloat::Detail.parse(pointerof(byte))
      CRYSTAL

      program, analyzer, engine = infer_method_type_binding_types(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.should be_empty
      engine.context.get_type(program.roots.last).to_s.should eq("Bool")
    end
  end
end
