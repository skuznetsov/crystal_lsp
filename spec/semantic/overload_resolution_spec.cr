require "spec"

require "../../src/compiler/frontend/ast"
require "../../src/compiler/frontend/lexer"
require "../../src/compiler/frontend/parser"
require "../../src/compiler/semantic/context"
require "../../src/compiler/semantic/symbol_table"
require "../../src/compiler/semantic/collectors/symbol_collector"
require "../../src/compiler/semantic/type_inference_engine"

include CrystalV2::Compiler

describe "Phase 98: Method Overload Resolution" do
  # ==================================================================
  # Category 1: Basic overload selection by argument types
  # ==================================================================

  describe "basic overload selection" do
    it "selects correct overload based on argument type" do
      source = <<-CRYSTAL
        class Foo
          def process(x : Int32)
            1
          end

          def process(x : String)
            "string"
          end
        end

        def test
          foo = Foo.new
          foo.process(42)      # Should select Int32 overload
          foo.process("hello") # Should select String overload
        end
        CRYSTAL

      lexer = Frontend::Lexer.new(source)
      parser = Frontend::Parser.new(lexer)
      program = parser.parse_program

      analyzer = Semantic::Analyzer.new(program)
      analyzer.collect_symbols
      name_result = analyzer.resolve_names

      engine = Semantic::TypeInferenceEngine.new(program, name_result.identifier_symbols, analyzer.global_context.symbol_table)
      engine.infer_types

      # No errors means overloads were resolved correctly
      engine.diagnostics.select(&.level.error?).should be_empty
    end

    it "selects overload by parameter count" do
      source = <<-CRYSTAL
        class Calculator
          def add(x : Int32)
            x
          end

          def add(x : Int32, y : Int32)
            x + y
          end
        end

        def test
          calc = Calculator.new
          calc.add(5)      # 1-arg overload
          calc.add(5, 10)  # 2-arg overload
        end
        CRYSTAL

      lexer = Frontend::Lexer.new(source)
      parser = Frontend::Parser.new(lexer)
      program = parser.parse_program

      analyzer = Semantic::Analyzer.new(program)
      analyzer.collect_symbols
      name_result = analyzer.resolve_names

      engine = Semantic::TypeInferenceEngine.new(program, name_result.identifier_symbols, analyzer.global_context.symbol_table)
      engine.infer_types

      engine.diagnostics.select(&.level.error?).should be_empty
    end
  end

  # ==================================================================
  # Category 2: Specificity ranking (prefer more specific types)
  # ==================================================================

  describe "specificity ranking" do
    it "prefers exact type match over superclass match" do
      source = <<-CRYSTAL
        class Animal
        end

        class Dog < Animal
        end

        class Handler
          def handle(x : Animal)
            "animal"
          end

          def handle(x : Dog)
            "dog"
          end
        end

        def test
          handler = Handler.new
          dog = Dog.new
          handler.handle(dog)  # Should prefer Dog overload (exact match)
        end
        CRYSTAL

      lexer = Frontend::Lexer.new(source)
      parser = Frontend::Parser.new(lexer)
      program = parser.parse_program

      analyzer = Semantic::Analyzer.new(program)
      analyzer.collect_symbols
      name_result = analyzer.resolve_names

      engine = Semantic::TypeInferenceEngine.new(program, name_result.identifier_symbols, analyzer.global_context.symbol_table)
      engine.infer_types

      engine.diagnostics.select(&.level.error?).should be_empty
    end

    it "matches subtype to superclass parameter" do
      source = <<-CRYSTAL
        class Animal
        end

        class Cat < Animal
        end

        class Vet
          def treat(animal : Animal)
            "treated"
          end
        end

        def test
          vet = Vet.new
          cat = Cat.new
          vet.treat(cat)  # Cat matches Animal parameter (subtyping)
        end
        CRYSTAL

      lexer = Frontend::Lexer.new(source)
      parser = Frontend::Parser.new(lexer)
      program = parser.parse_program

      analyzer = Semantic::Analyzer.new(program)
      analyzer.collect_symbols
      name_result = analyzer.resolve_names

      engine = Semantic::TypeInferenceEngine.new(program, name_result.identifier_symbols, analyzer.global_context.symbol_table)
      engine.infer_types

      engine.diagnostics.select(&.level.error?).should be_empty
    end
  end

  # ==================================================================
  # Category 3: Union type matching in overloads
  # ==================================================================

  describe "union type matching" do
    it "matches concrete type to union parameter" do
      source = <<-CRYSTAL
        class Processor
          def process(x : Int32 | String)
            x
          end
        end

        def test
          p = Processor.new
          p.process(42)      # Int32 matches Int32 | String
          p.process("hello") # String matches Int32 | String
        end
        CRYSTAL

      lexer = Frontend::Lexer.new(source)
      parser = Frontend::Parser.new(lexer)
      program = parser.parse_program

      analyzer = Semantic::Analyzer.new(program)
      analyzer.collect_symbols
      name_result = analyzer.resolve_names

      engine = Semantic::TypeInferenceEngine.new(program, name_result.identifier_symbols, analyzer.global_context.symbol_table)
      engine.infer_types

      engine.diagnostics.select(&.level.error?).should be_empty
    end

    it "prefers specific type over union type" do
      source = <<-CRYSTAL
        class Handler
          def handle(x : Int32)
            "specific"
          end

          def handle(x : Int32 | String)
            "union"
          end
        end

        def test
          h = Handler.new
          h.handle(42)  # Should prefer Int32 overload (more specific)
        end
        CRYSTAL

      lexer = Frontend::Lexer.new(source)
      parser = Frontend::Parser.new(lexer)
      program = parser.parse_program

      analyzer = Semantic::Analyzer.new(program)
      analyzer.collect_symbols
      name_result = analyzer.resolve_names

      engine = Semantic::TypeInferenceEngine.new(program, name_result.identifier_symbols, analyzer.global_context.symbol_table)
      engine.infer_types

      engine.diagnostics.select(&.level.error?).should be_empty
    end
  end

  # ==================================================================
  # Category 4: Nilable type matching
  # ==================================================================

  describe "nilable type matching" do
    it "matches non-nil to nilable parameter" do
      source = <<-CRYSTAL
        class Optional
          def maybe(x : String?)
            x
          end
        end

        def test
          opt = Optional.new
          opt.maybe("hello")  # String matches String?
          opt.maybe(nil)      # Nil matches String?
        end
        CRYSTAL

      lexer = Frontend::Lexer.new(source)
      parser = Frontend::Parser.new(lexer)
      program = parser.parse_program

      analyzer = Semantic::Analyzer.new(program)
      analyzer.collect_symbols
      name_result = analyzer.resolve_names

      engine = Semantic::TypeInferenceEngine.new(program, name_result.identifier_symbols, analyzer.global_context.symbol_table)
      engine.infer_types

      engine.diagnostics.select(&.level.error?).should be_empty
    end
  end

  # ==================================================================
  # Category 5: Multiple parameters overload resolution
  # ==================================================================

  describe "multiple parameters" do
    it "selects overload based on multiple parameter types" do
      source = <<-CRYSTAL
        class Math
          def op(a : Int32, b : Int32)
            a + b
          end

          def op(a : String, b : String)
            a + b
          end
        end

        def test
          m = Math.new
          m.op(1, 2)          # Int32, Int32 overload
          m.op("a", "b")      # String, String overload
        end
        CRYSTAL

      lexer = Frontend::Lexer.new(source)
      parser = Frontend::Parser.new(lexer)
      program = parser.parse_program

      analyzer = Semantic::Analyzer.new(program)
      analyzer.collect_symbols
      name_result = analyzer.resolve_names

      engine = Semantic::TypeInferenceEngine.new(program, name_result.identifier_symbols, analyzer.global_context.symbol_table)
      engine.infer_types

      engine.diagnostics.select(&.level.error?).should be_empty
    end
  end
end
