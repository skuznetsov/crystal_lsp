require "spec"

require "../../src/compiler/frontend/ast"
require "../../src/compiler/frontend/lexer"
require "../../src/compiler/frontend/parser"
require "../../src/compiler/semantic/analyzer"
require "../../src/compiler/semantic/type_inference_engine"

alias Frontend = CrystalV2::Compiler::Frontend
alias Semantic = CrystalV2::Compiler::Semantic

private def infer_block_lexical_self_types(source : String)
  parser = Frontend::Parser.new(Frontend::Lexer.new(source))
  program = parser.parse_program

  analyzer = Semantic::Analyzer.new(program)
  analyzer.collect_symbols
  name_result = analyzer.resolve_names
  engine = analyzer.infer_types(name_result.identifier_symbols)

  {program, analyzer, engine}
end

describe Semantic::TypeInferenceEngine do
  describe "lexical self inside receiver blocks" do
    it "keeps outer ivar lookup when inferring zero-arg blocks on another receiver" do
      source = <<-CRYSTAL
        class Item
        end

        module Crystal
          class SpinLock
            def sync(&)
              yield
            end
          end

          class PointerLinkedList(T)
            def initialize
            end

            def shift? : T?
              nil
            end
          end
        end

        class Mutex
          @queue = Crystal::PointerLinkedList(Item).new
          @lock = Crystal::SpinLock.new

          def unlock
            @lock.sync do
              @queue.shift?
            end
          end
        end

        Mutex.new.unlock
      CRYSTAL

      program, analyzer, engine = infer_block_lexical_self_types(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.select(&.level.error?).should be_empty
      {"Item | Nil", "Nil | Item"}.should contain(engine.context.get_type(program.roots.last).to_s)
    end
  end
end
