require "spec"

require "../../src/compiler/frontend/ast"
require "../../src/compiler/frontend/lexer"
require "../../src/compiler/frontend/parser"
require "../../src/compiler/semantic/analyzer"
require "../../src/compiler/semantic/type_inference_engine"

alias Frontend = CrystalV2::Compiler::Frontend
alias Semantic = CrystalV2::Compiler::Semantic

private def infer_module_annotation_types(source : String)
  parser = Frontend::Parser.new(Frontend::Lexer.new(source))
  program = parser.parse_program

  analyzer = Semantic::Analyzer.new(program)
  analyzer.collect_symbols
  name_result = analyzer.resolve_names
  engine = analyzer.infer_types(name_result.identifier_symbols)

  {program, analyzer, engine}
end

describe Semantic::TypeInferenceEngine do
  describe "qualified module annotations" do
    it "resolves nested module return annotations as module receivers" do
      source = <<-CRYSTAL
        abstract class Crystal::EventLoop
          module FileDescriptor
            abstract def reopened(file_descriptor : Crystal::System::FileDescriptor) : Nil
            abstract def close(file_descriptor : Crystal::System::FileDescriptor) : Nil
          end
        end

        module Crystal::System
          module FileDescriptor
            private def event_loop : Crystal::EventLoop::FileDescriptor
              uninitialized Crystal::EventLoop::FileDescriptor
            end

            def probe
              event_loop.reopened(self)
              event_loop.close(self)
            end
          end
        end
      CRYSTAL

      _, analyzer, engine = infer_module_annotation_types(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.should be_empty
    end
  end
end
