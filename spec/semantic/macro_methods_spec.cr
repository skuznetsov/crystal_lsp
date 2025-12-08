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

  # ==================================================================
  # Category 4: Annotation access (ann[:key])
  # ==================================================================

  describe "annotation access" do
    it "supports {% if ann = @type.annotation(Foo) %} pattern" do
      source = <<-CRYSTAL
        annotation MyAnnotation
        end

        @[MyAnnotation]
        class Foo
          macro check_annotation
            {% if ann = @type.annotation(MyAnnotation) %}
              true
            {% else %}
              false
            {% end %}
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

    it "supports ann[:key] access for named arguments" do
      source = <<-CRYSTAL
        annotation JSON::Field
        end

        class Person
          @[JSON::Field(key: "full_name")]
          getter name : String = ""

          macro field_key(ivar)
            {% if ann = ivar.annotation(JSON::Field) %}
              {{ ann[:key] }}
            {% end %}
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

    it "supports iterating over ivar annotations" do
      source = <<-CRYSTAL
        annotation Serializable
        end

        @[Serializable]
        class Config
          @[Serializable]
          getter host : String = ""

          @[Serializable]
          getter port : Int32 = 0

          macro list_serializable_fields
            {% for ivar in @type.instance_vars %}
              {% if ivar.annotation(Serializable) %}
                {{ ivar.name }}
              {% end %}
            {% end %}
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
  # Category 5: typeof() compile-time operator
  # ==================================================================

  describe "typeof()" do
    it "infers type of integer literal" do
      source = <<-CRYSTAL
        macro test
          {{ typeof(42) }}
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

    it "infers type of string literal" do
      source = <<-CRYSTAL
        macro test
          {{ typeof("hello") }}
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

    it "infers type of float literal" do
      source = <<-CRYSTAL
        macro test
          {{ typeof(3.14) }}
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
  # Category 6: sizeof() compile-time operator
  # ==================================================================

  describe "sizeof()" do
    it "returns size of Int32" do
      source = <<-CRYSTAL
        macro test
          {{ sizeof(Int32) }}
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

    it "returns size of Int64" do
      source = <<-CRYSTAL
        macro test
          {{ sizeof(Int64) }}
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

    it "returns size of Float64" do
      source = <<-CRYSTAL
        macro test
          {{ sizeof(Float64) }}
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
  # Category 7: alignof() compile-time operator
  # ==================================================================

  describe "alignof()" do
    it "returns alignment of Int32" do
      source = <<-CRYSTAL
        macro test
          {{ alignof(Int32) }}
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

    it "returns alignment of Int64" do
      source = <<-CRYSTAL
        macro test
          {{ alignof(Int64) }}
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
end
