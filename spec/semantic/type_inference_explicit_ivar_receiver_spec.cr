require "spec"

require "../../src/compiler/frontend/ast"
require "../../src/compiler/frontend/lexer"
require "../../src/compiler/frontend/parser"
require "../../src/compiler/semantic/analyzer"
require "../../src/compiler/semantic/type_inference_engine"

alias Frontend = CrystalV2::Compiler::Frontend
alias Semantic = CrystalV2::Compiler::Semantic

private def infer_explicit_ivar_receiver_types(source : String)
  parser = Frontend::Parser.new(Frontend::Lexer.new(source))
  program = parser.parse_program

  analyzer = Semantic::Analyzer.new(program)
  analyzer.collect_symbols
  name_result = analyzer.resolve_names
  engine = analyzer.infer_types(name_result.identifier_symbols)

  {program, analyzer, engine}
end

describe Semantic::TypeInferenceEngine do
  describe "explicit ivar receiver access" do
    it "resolves pointerof(action.@field) through field metadata" do
      source = <<-CRYSTAL
        struct Sigaction
          @sa_mask : UInt32

          def initialize(@sa_mask : UInt32)
          end
        end

        action = Sigaction.new(0_u32)
        pointerof(action.@sa_mask)
      CRYSTAL

      program, analyzer, engine = infer_explicit_ivar_receiver_types(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.should be_empty
      engine.context.get_type(program.roots.last).to_s.should eq("Pointer(UInt32)")
    end

    it "supports chained calls through explicit receiver ivars" do
      source = <<-CRYSTAL
        class Mutex
          def lock
            nil
          end
        end

        class LinkedList(T)
          @mutex : Mutex

          def initialize(@mutex : Mutex)
          end
        end

        threads = LinkedList(Int32).new(Mutex.new)
        threads.@mutex.lock
      CRYSTAL

      program, analyzer, engine = infer_explicit_ivar_receiver_types(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.should be_empty
      engine.context.get_type(program.roots.last).to_s.should eq("Nil")
    end

    it "infers explicit receiver ivars from default values when no annotation exists" do
      source = <<-CRYSTAL
        class Thread
          class Mutex
            def lock : Nil
            end
          end

          class LinkedList(T)
            @mutex = Thread::Mutex.new
          end

          @@threads = uninitialized Thread::LinkedList(Thread)

          protected def self.threads : Thread::LinkedList(Thread)
            @@threads
          end

          def self.init : Nil
            @@threads = Thread::LinkedList(Thread).new
          end

          def self.lock : Nil
            threads.@mutex.lock
          end
        end

        Thread.init
        Thread.lock
      CRYSTAL

      program, analyzer, engine = infer_explicit_ivar_receiver_types(source)
      thread_symbol = analyzer.global_context.symbol_table.lookup("Thread").as(Semantic::ClassSymbol)
      linked_list_symbol = thread_symbol.scope.lookup("LinkedList").as(Semantic::ClassSymbol)
      mutex_info = linked_list_symbol.get_instance_var_info("mutex")

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      mutex_info.should_not be_nil
      mutex_info.not_nil!.has_default?.should be_true
      mutex_info.not_nil!.default_value.should_not be_nil
      engine.diagnostics.should be_empty
      engine.context.get_type(program.roots.last).to_s.should eq("Nil")
    end
  end
end
