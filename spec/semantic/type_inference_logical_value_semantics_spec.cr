require "spec"

require "../../src/compiler/frontend/ast"
require "../../src/compiler/frontend/lexer"
require "../../src/compiler/frontend/parser"
require "../../src/compiler/semantic/symbol_table"
require "../../src/compiler/semantic/symbol"
require "../../src/compiler/semantic/collectors/symbol_collector"
require "../../src/compiler/semantic/resolvers/name_resolver"
require "../../src/compiler/semantic/analyzer"
require "../../src/compiler/semantic/type_inference_engine"

alias Frontend = CrystalV2::Compiler::Frontend
alias Semantic = CrystalV2::Compiler::Semantic

private def infer_logical_value_types(source : String)
  lexer = Frontend::Lexer.new(source)
  parser = Frontend::Parser.new(lexer)
  program = parser.parse_program

  analyzer = Semantic::Analyzer.new(program)
  analyzer.collect_symbols
  name_result = analyzer.resolve_names

  engine = Semantic::TypeInferenceEngine.new(program, name_result.identifier_symbols, analyzer.global_context.symbol_table)
  engine.infer_types

  {program, analyzer, engine}
end

describe Semantic::TypeInferenceEngine do
  describe "logical value semantics" do
    it "preserves tuple values through || return nil in multiple assignment" do
      source = <<-CRYSTAL
        def parse_transition(day : Int32)
          return unless day >= 1
          {day, 1}
        end

        def probe(day : Int32)
          x, y = parse_transition(day) || return nil
          x + y
        end

        probe(1)
      CRYSTAL

      program, analyzer, engine = infer_logical_value_types(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.select(&.level.error?).should be_empty

      type = engine.context.get_type(program.roots.last)
      type.to_s.should contain("Int32")
    end

    it "preserves non-bool values through nil-coalescing || assignments" do
      source = <<-CRYSTAL
        lib LibC
          alias TcflagT = UInt64

          struct Termios
            c_lflag : TcflagT
          end
        end

        class Terminal
          def system_tcgetattr
            termios = uninitialized LibC::Termios
            termios
          end

          def probe(mode = nil)
            new_mode = mode || system_tcgetattr
            new_mode.c_lflag
          end
        end

        Terminal.new.probe()
      CRYSTAL

      program, analyzer, engine = infer_logical_value_types(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.select(&.level.error?).should be_empty

      engine.context.get_type(program.roots.last).to_s.should eq("UInt64")
    end

    it "returns the truthy value type for nilable || expressions" do
      source = <<-CRYSTAL
        lib LibC
          struct Termios
            c_lflag : UInt64
          end
        end

        class Terminal
          def system_tcgetattr
            termios = uninitialized LibC::Termios
            termios
          end

          def probe(mode = nil)
            mode || system_tcgetattr
          end
        end

        Terminal.new.probe()
      CRYSTAL

      program, analyzer, engine = infer_logical_value_types(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.select(&.level.error?).should be_empty

      engine.context.get_type(program.roots.last).to_s.should eq("Termios")
    end

    it "infers uninitialized lib structs through helper returns" do
      source = <<-CRYSTAL
        lib LibC
          struct Termios
            c_lflag : UInt64
          end
        end

        class Terminal
          def system_tcgetattr
            termios = uninitialized LibC::Termios
            termios
          end
        end

        Terminal.new.system_tcgetattr()
      CRYSTAL

      program, analyzer, engine = infer_logical_value_types(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.select(&.level.error?).should be_empty

      engine.context.get_type(program.roots.last).to_s.should eq("Termios")
    end

    it "preserves receiverless helper return types inside instance methods" do
      source = <<-CRYSTAL
        lib LibC
          struct Termios
            c_lflag : UInt64
          end
        end

        class Terminal
          def system_tcgetattr
            termios = uninitialized LibC::Termios
            termios
          end

          def probe
            system_tcgetattr
          end
        end

        Terminal.new.probe()
      CRYSTAL

      program, analyzer, engine = infer_logical_value_types(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.select(&.level.error?).should be_empty

      engine.context.get_type(program.roots.last).to_s.should eq("Termios")
    end

    it "binds nil defaults for untyped instance-method params" do
      source = <<-CRYSTAL
        class Terminal
          def probe(mode = nil)
            mode
          end
        end

        Terminal.new.probe()
      CRYSTAL

      program, analyzer, engine = infer_logical_value_types(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.select(&.level.error?).should be_empty

      engine.context.get_type(program.roots.last).to_s.should eq("Nil")
    end
  end
end
