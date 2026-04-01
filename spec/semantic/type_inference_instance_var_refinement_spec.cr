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

    it "keeps initialize-seeded collection ivars concrete across queue and stack helpers" do
      source = <<-CRYSTAL
        class Printer
          def initialize
            root = Group.new(0)

            @group_stack = [] of Group
            @group_stack << root

            @group_queue = GroupQueue.new
            @group_queue.enq root
          end

          def current_depth : Int32
            @group_stack.last.depth
          end

          def consume : Int32
            return 0 unless group = @group_queue.deq
            group.depth
          end

          def push_and_pop : Int32
            group = Group.new(@group_stack.last.depth + 1)
            @group_stack.push group
            @group_queue.enq group
            begin
              group.depth
            ensure
              @group_stack.pop
              @group_queue.delete(group) if group.items.empty?
            end
          end

          private class Group
            getter depth
            getter items

            def initialize(@depth : Int32)
              @items = [] of Int32
            end
          end

          private class GroupQueue
            def initialize
              @groups = [] of Group
            end

            def enq(group : Group) : Nil
              @groups << group
            end

            def deq : Group?
              @groups.shift?
            end

            def delete(group : Group) : Nil
              @groups.delete(group)
            end
          end
        end

        printer = Printer.new
        printer.current_depth
        printer.consume
        printer.push_and_pop
      CRYSTAL

      program, analyzer, engine = infer_instance_var_refinement_types(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.should be_empty
      engine.context.get_type(program.roots.last).to_s.should eq("Int32")
    end
  end
end
