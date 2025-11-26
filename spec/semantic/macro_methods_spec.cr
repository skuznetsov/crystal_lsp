require "spec"

require "../../src/compiler/frontend/ast"
require "../../src/compiler/frontend/lexer"
require "../../src/compiler/frontend/parser"
require "../../src/compiler/semantic/context"
require "../../src/compiler/semantic/symbol_table"
require "../../src/compiler/semantic/collectors/symbol_collector"

include CrystalV2::Compiler

describe "Phase 87B-6: Macro Methods (.stringify, .id, .class_name)" do
  # ==================================================================
  # Category 1: .stringify method (5 tests)
  # ==================================================================

  describe ".stringify" do
    it "converts identifier to string literal" do
      source = <<-CRYSTAL
        macro test(name)
          {{ name.stringify }}
        end

        test(foo)
        CRYSTAL

      lexer = Frontend::Lexer.new(source)
      parser = Frontend::Parser.new(lexer)
      program = parser.parse_program

      context = Semantic::Context.new(Semantic::SymbolTable.new)
      collector = Semantic::SymbolCollector.new(program, context)
      collector.collect

      # Should expand without errors
      collector.diagnostics.select(&.level.error?).should be_empty
    end

    it "converts number to string literal" do
      source = <<-CRYSTAL
        macro test(num)
          {{ num.stringify }}
        end

        test(42)
        CRYSTAL

      lexer = Frontend::Lexer.new(source)
      parser = Frontend::Parser.new(lexer)
      program = parser.parse_program

      context = Semantic::Context.new(Semantic::SymbolTable.new)
      collector = Semantic::SymbolCollector.new(program, context)
      collector.collect

      collector.diagnostics.select(&.level.error?).should be_empty
    end

    it "handles @type.name.stringify" do
      source = <<-CRYSTAL
        class Foo
          macro type_name_str
            {{ @type.name.stringify }}
          end
        end
        CRYSTAL

      lexer = Frontend::Lexer.new(source)
      parser = Frontend::Parser.new(lexer)
      program = parser.parse_program

      context = Semantic::Context.new(Semantic::SymbolTable.new)
      collector = Semantic::SymbolCollector.new(program, context)
      collector.collect

      collector.diagnostics.select(&.level.error?).should be_empty
    end
  end

  # ==================================================================
  # Category 2: .id method (4 tests)
  # ==================================================================

  describe ".id" do
    it "converts string to identifier" do
      source = <<-CRYSTAL
        macro define_method(name)
          def {{ name.id }}
            42
          end
        end

        define_method("my_method")
        CRYSTAL

      lexer = Frontend::Lexer.new(source)
      parser = Frontend::Parser.new(lexer)
      program = parser.parse_program

      context = Semantic::Context.new(Semantic::SymbolTable.new)
      collector = Semantic::SymbolCollector.new(program, context)
      collector.collect

      collector.diagnostics.select(&.level.error?).should be_empty
    end

    it "converts symbol to identifier" do
      source = <<-CRYSTAL
        macro define_method(name)
          def {{ name.id }}
            42
          end
        end

        define_method(:my_method)
        CRYSTAL

      lexer = Frontend::Lexer.new(source)
      parser = Frontend::Parser.new(lexer)
      program = parser.parse_program

      context = Semantic::Context.new(Semantic::SymbolTable.new)
      collector = Semantic::SymbolCollector.new(program, context)
      collector.collect

      collector.diagnostics.select(&.level.error?).should be_empty
    end

    it "handles chained .id.stringify" do
      source = <<-CRYSTAL
        macro test(name)
          {{ name.id.stringify }}
        end

        test(:foo)
        CRYSTAL

      lexer = Frontend::Lexer.new(source)
      parser = Frontend::Parser.new(lexer)
      program = parser.parse_program

      context = Semantic::Context.new(Semantic::SymbolTable.new)
      collector = Semantic::SymbolCollector.new(program, context)
      collector.collect

      collector.diagnostics.select(&.level.error?).should be_empty
    end
  end

  # ==================================================================
  # Category 3: .class_name method (3 tests)
  # ==================================================================

  describe ".class_name" do
    it "returns NumberLiteral for number" do
      source = <<-CRYSTAL
        macro test(x)
          {{ x.class_name }}
        end

        test(42)
        CRYSTAL

      lexer = Frontend::Lexer.new(source)
      parser = Frontend::Parser.new(lexer)
      program = parser.parse_program

      context = Semantic::Context.new(Semantic::SymbolTable.new)
      collector = Semantic::SymbolCollector.new(program, context)
      collector.collect

      collector.diagnostics.select(&.level.error?).should be_empty
    end

    it "returns StringLiteral for string" do
      source = <<-CRYSTAL
        macro test(x)
          {{ x.class_name }}
        end

        test("hello")
        CRYSTAL

      lexer = Frontend::Lexer.new(source)
      parser = Frontend::Parser.new(lexer)
      program = parser.parse_program

      context = Semantic::Context.new(Semantic::SymbolTable.new)
      collector = Semantic::SymbolCollector.new(program, context)
      collector.collect

      collector.diagnostics.select(&.level.error?).should be_empty
    end

    it "returns SymbolLiteral for symbol" do
      source = <<-CRYSTAL
        macro test(x)
          {{ x.class_name }}
        end

        test(:foo)
        CRYSTAL

      lexer = Frontend::Lexer.new(source)
      parser = Frontend::Parser.new(lexer)
      program = parser.parse_program

      context = Semantic::Context.new(Semantic::SymbolTable.new)
      collector = Semantic::SymbolCollector.new(program, context)
      collector.collect

      collector.diagnostics.select(&.level.error?).should be_empty
    end
  end
end
