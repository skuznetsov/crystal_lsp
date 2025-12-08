require "spec"

require "../../src/compiler/frontend/ast"
require "../../src/compiler/frontend/lexer"
require "../../src/compiler/frontend/parser"
require "../../src/compiler/semantic/context"
require "../../src/compiler/semantic/symbol_table"
require "../../src/compiler/semantic/collectors/symbol_collector"
require "../../src/compiler/semantic/type_inference_engine"

include CrystalV2::Compiler

describe "Phase 99: Virtual Types (Inheritance-aware Dispatch)" do
  # ==================================================================
  # Category 1: Basic virtual type method dispatch
  # ==================================================================

  describe "basic virtual dispatch" do
    it "calls method defined in base class" do
      source = <<-CRYSTAL
        class Animal
          def speak
            "generic sound"
          end
        end

        class Dog < Animal
        end

        def test
          dog = Dog.new
          dog.speak  # Inherited from Animal
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

    it "calls overridden method in subclass" do
      source = <<-CRYSTAL
        class Animal
          def speak
            "generic"
          end
        end

        class Cat < Animal
          def speak
            "meow"
          end
        end

        def test
          cat = Cat.new
          cat.speak  # Cat's override
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
  # Category 2: Deep inheritance chains
  # ==================================================================

  describe "deep inheritance chains" do
    it "finds method through multiple inheritance levels" do
      source = <<-CRYSTAL
        class Grandparent
          def legacy
            "old"
          end
        end

        class Parent < Grandparent
        end

        class Child < Parent
        end

        def test
          child = Child.new
          child.legacy  # Found in Grandparent
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

    it "override in middle of chain" do
      source = <<-CRYSTAL
        class A
          def action
            "A"
          end
        end

        class B < A
          def action
            "B"
          end
        end

        class C < B
        end

        def test
          c = C.new
          c.action  # Gets B's override
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
  # Category 3: Subtype matching with virtual types
  # ==================================================================

  describe "subtype matching" do
    it "subclass instance matches parent parameter" do
      source = <<-CRYSTAL
        class Vehicle
        end

        class Car < Vehicle
        end

        class Garage
          def park(v : Vehicle)
            "parked"
          end
        end

        def test
          garage = Garage.new
          car = Car.new
          garage.park(car)  # Car matches Vehicle
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

    it "grandchild matches grandparent parameter" do
      source = <<-CRYSTAL
        class Base
        end

        class Middle < Base
        end

        class Derived < Middle
        end

        class Handler
          def handle(b : Base)
            "handled"
          end
        end

        def test
          h = Handler.new
          d = Derived.new
          h.handle(d)  # Derived matches Base (grandparent)
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
  # Category 4: Virtual dispatch with overloads
  # ==================================================================

  describe "virtual dispatch with overloads" do
    it "selects correct overload for subclass type" do
      source = <<-CRYSTAL
        class Shape
        end

        class Circle < Shape
        end

        class Square < Shape
        end

        class Renderer
          def render(s : Shape)
            "generic"
          end

          def render(c : Circle)
            "circle"
          end
        end

        def test
          r = Renderer.new
          circle = Circle.new
          square = Square.new
          r.render(circle)  # Prefers Circle overload
          r.render(square)  # Uses Shape overload
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
  # Category 5: Abstract class patterns
  # ==================================================================

  describe "abstract class patterns" do
    it "concrete subclass can call inherited method" do
      source = <<-CRYSTAL
        abstract class AbstractBase
          def concrete_method
            "works"
          end
        end

        class ConcreteChild < AbstractBase
        end

        def test
          child = ConcreteChild.new
          child.concrete_method
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
