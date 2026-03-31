require "spec"

require "../../src/compiler/frontend/ast"
require "../../src/compiler/frontend/lexer"
require "../../src/compiler/frontend/parser"
require "../../src/compiler/semantic/analyzer"
require "../../src/compiler/semantic/type_inference_engine"

alias Frontend = CrystalV2::Compiler::Frontend
alias Semantic = CrystalV2::Compiler::Semantic

private def infer_proc_type_annotation_types(source : String)
  parser = Frontend::Parser.new(Frontend::Lexer.new(source))
  program = parser.parse_program

  analyzer = Semantic::Analyzer.new(program)
  analyzer.collect_symbols
  name_result = analyzer.resolve_names
  engine = analyzer.infer_types(name_result.identifier_symbols)

  {program, analyzer, engine}
end

describe Semantic::TypeInferenceEngine do
  describe "proc type annotations with wrapped parameter lists" do
    it "resolves alias targets of the form (A, B, C) ->" do
      source = <<-CRYSTAL
        lib LibC
          struct SiginfoT
          end

          alias SigactionHandlerT = (Int, SiginfoT*, Void*) ->
        end

        handler = uninitialized LibC::SigactionHandlerT
        handler
      CRYSTAL

      program, analyzer, engine = infer_proc_type_annotation_types(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.should be_empty
      engine.context.get_type(program.roots.last).should be_a(Semantic::ProcType)
    end

    it "resolves block annotations of the form (A, B, C) ->" do
      source = <<-CRYSTAL
        lib LibC
          struct SiginfoT
          end
        end

        def install(& : (Int, LibC::SiginfoT*, Void*) ->)
          nil
        end

        install do |signum, info, data|
          nil
        end
      CRYSTAL

      program, analyzer, engine = infer_proc_type_annotation_types(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.should be_empty
      engine.context.get_type(program.roots.last).to_s.should eq("Nil")
    end
  end
end
