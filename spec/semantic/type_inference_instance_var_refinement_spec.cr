require "spec"

require "../../src/compiler/frontend/ast"
require "../../src/compiler/frontend/lexer"
require "../../src/compiler/frontend/parser"
require "../../src/compiler/semantic/analyzer"
require "../../src/compiler/semantic/type_inference_engine"

alias Frontend = CrystalV2::Compiler::Frontend
alias Semantic = CrystalV2::Compiler::Semantic

private def infer_instance_var_refinement_types(source : String)
  parser = Frontend::Parser.new(Frontend::Lexer.new(source))
  program = parser.parse_program

  analyzer = Semantic::Analyzer.new(program)
  analyzer.collect_symbols
  name_result = analyzer.resolve_names
  engine = analyzer.infer_types(name_result.identifier_symbols)

  {program, analyzer, engine}
end

describe Semantic::TypeInferenceEngine do
  describe "annotated ivar refinement from concrete assignments" do
    it "keeps a concrete includer surface behind a module-typed ivar" do
      source = <<-CRYSTAL
        module Event
          abstract def free : Nil
        end

        class FiberEvent
          include Event

          def free : Nil
            nil
          end

          def delete : Nil
            nil
          end
        end

        class Fiber
          @timeout_event : Event?

          def initialize
            @timeout_event = nil
          end

          def seed : Event
            @timeout_event ||= FiberEvent.new
          end

          def cancel_timeout : Nil
            if ev = @timeout_event
              ev.delete
            end
          end
        end

        fiber = Fiber.new
        fiber.seed
        fiber.cancel_timeout
      CRYSTAL

      program, analyzer, engine = infer_instance_var_refinement_types(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.should be_empty
      engine.context.get_type(program.roots.last).to_s.should eq("Nil")
    end

    it "seeds untyped nested-class ivars from constructor bodies reached via .new" do
      source = <<-CRYSTAL
        class Outer
          def push : Int32
            inner = Breakable.new
            inner.take
          end

          private class Breakable
            def initialize
              @cached = Group.new
            end

            def take : Int32
              @cached.depth
            end
          end

          private class Group
            def depth : Int32
              1
            end
          end
        end

        Outer.new.push
      CRYSTAL

      program, analyzer, engine = infer_instance_var_refinement_types(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.should be_empty
      engine.context.get_type(program.roots.last).to_s.should eq("Int32")
    end
  end
end
