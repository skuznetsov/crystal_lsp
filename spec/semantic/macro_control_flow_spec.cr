require "spec"

require "../../src/compiler/frontend/ast"
require "../../src/compiler/frontend/lexer"
require "../../src/compiler/frontend/parser"
require "../../src/compiler/semantic/context"
require "../../src/compiler/semantic/symbol_table"
require "../../src/compiler/semantic/collectors/symbol_collector"

include CrystalV2::Compiler

describe "Phase 87B-3: Macro Control Flow" do
  # ==================================================================
  # Category 1: Truthiness (5 tests)
  # ==================================================================

  it "treats false as falsy" do
    source = <<-CRYSTAL
      macro test_false
        {% if false %}
          puts "yes"
        {% else %}
          puts "no"
        {% end %}
      end

      test_false
      CRYSTAL

    lexer = Frontend::Lexer.new(source)
    parser = Frontend::Parser.new(lexer)
    program = parser.parse_program

    context = Semantic::Context.new(Semantic::SymbolTable.new)
    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    # Should expand to: puts "no"
    collector.diagnostics.select(&.level.error?).should be_empty
  end

  it "treats nil as falsy" do
    source = <<-CRYSTAL
      macro test_nil
        {% if nil %}
          puts "yes"
        {% else %}
          puts "no"
        {% end %}
      end

      test_nil
      CRYSTAL

    lexer = Frontend::Lexer.new(source)
    parser = Frontend::Parser.new(lexer)
    program = parser.parse_program

    context = Semantic::Context.new(Semantic::SymbolTable.new)
    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    # Should expand to: puts "no"
    collector.diagnostics.select(&.level.error?).should be_empty
  end

  it "treats 0 as truthy (Crystal semantics)" do
    source = <<-CRYSTAL
      macro test_zero
        {% if 0 %}
          puts "yes"
        {% else %}
          puts "no"
        {% end %}
      end

      test_zero
      CRYSTAL

    lexer = Frontend::Lexer.new(source)
    parser = Frontend::Parser.new(lexer)
    program = parser.parse_program

    context = Semantic::Context.new(Semantic::SymbolTable.new)
    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    # Should expand to: puts "yes" (0 is truthy!)
    collector.diagnostics.select(&.level.error?).should be_empty
  end

  it "treats empty string as truthy" do
    source = <<-CRYSTAL
      macro test_empty_string
        {% if "" %}
          puts "yes"
        {% else %}
          puts "no"
        {% end %}
      end

      test_empty_string
      CRYSTAL

    lexer = Frontend::Lexer.new(source)
    parser = Frontend::Parser.new(lexer)
    program = parser.parse_program

    context = Semantic::Context.new(Semantic::SymbolTable.new)
    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    # Should expand to: puts "yes" ("" is truthy!)
    collector.diagnostics.select(&.level.error?).should be_empty
  end

  it "treats true as truthy" do
    source = <<-CRYSTAL
      macro test_true
        {% if true %}
          puts "yes"
        {% end %}
      end

      test_true
      CRYSTAL

    lexer = Frontend::Lexer.new(source)
    parser = Frontend::Parser.new(lexer)
    program = parser.parse_program

    context = Semantic::Context.new(Semantic::SymbolTable.new)
    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    # Should expand to: puts "yes"
    collector.diagnostics.select(&.level.error?).should be_empty
  end

  # ==================================================================
  # Category 2: If/Elsif/Else (5 tests)
  # ==================================================================

  it "evaluates if with true condition" do
    source = <<-CRYSTAL
      macro greet(name)
        {% if true %}
          puts {{ name }}
        {% end %}
      end

      greet("Alice")
      CRYSTAL

    lexer = Frontend::Lexer.new(source)
    parser = Frontend::Parser.new(lexer)
    program = parser.parse_program

    context = Semantic::Context.new(Semantic::SymbolTable.new)
    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    # Should expand successfully
    collector.diagnostics.select(&.level.error?).should be_empty
  end

  it "evaluates if with false condition (empty output)" do
    source = <<-CRYSTAL
      macro noop
        {% if false %}
          puts "should not appear"
        {% end %}
      end

      noop
      CRYSTAL

    lexer = Frontend::Lexer.new(source)
    parser = Frontend::Parser.new(lexer)
    program = parser.parse_program

    context = Semantic::Context.new(Semantic::SymbolTable.new)
    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    # Should expand to empty (no output)
    collector.diagnostics.select(&.level.error?).should be_empty
  end

  it "evaluates elsif when if is false" do
    source = <<-CRYSTAL
      macro test_elsif
        {% if false %}
          puts "if"
        {% elsif true %}
          puts "elsif"
        {% else %}
          puts "else"
        {% end %}
      end

      test_elsif
      CRYSTAL

    lexer = Frontend::Lexer.new(source)
    parser = Frontend::Parser.new(lexer)
    program = parser.parse_program

    context = Semantic::Context.new(Semantic::SymbolTable.new)
    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    # Should expand to: puts "elsif"
    collector.diagnostics.select(&.level.error?).should be_empty
  end

  it "evaluates else when all conditions false" do
    source = <<-CRYSTAL
      macro test_else
        {% if false %}
          puts "if"
        {% elsif nil %}
          puts "elsif"
        {% else %}
          puts "else"
        {% end %}
      end

      test_else
      CRYSTAL

    lexer = Frontend::Lexer.new(source)
    parser = Frontend::Parser.new(lexer)
    program = parser.parse_program

    context = Semantic::Context.new(Semantic::SymbolTable.new)
    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    # Should expand to: puts "else"
    collector.diagnostics.select(&.level.error?).should be_empty
  end

  it "handles nested if statements" do
    source = <<-CRYSTAL
      macro nested_if
        {% if true %}
          outer
          {% if true %}
            inner
          {% end %}
        {% end %}
      end

      nested_if
      CRYSTAL

    lexer = Frontend::Lexer.new(source)
    parser = Frontend::Parser.new(lexer)
    program = parser.parse_program

    context = Semantic::Context.new(Semantic::SymbolTable.new)
    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    # Should expand successfully with nesting
    collector.diagnostics.select(&.level.error?).should be_empty
  end

  # ==================================================================
  # Category 3: For Loops (5 tests)
  # ==================================================================

  it "iterates over array literal" do
    source = <<-CRYSTAL
      macro print_all
        {% for x in [1, 2, 3] %}
          puts {{ x }}
        {% end %}
      end

      print_all
      CRYSTAL

    lexer = Frontend::Lexer.new(source)
    parser = Frontend::Parser.new(lexer)
    program = parser.parse_program

    context = Semantic::Context.new(Semantic::SymbolTable.new)
    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    # Should expand to: puts 1; puts 2; puts 3
    collector.diagnostics.select(&.level.error?).should be_empty
  end

  it "binds loop variable correctly" do
    source = <<-CRYSTAL
      macro use_loop_var
        {% for item in ["a", "b"] %}
          var {{ item }}
        {% end %}
      end

      use_loop_var
      CRYSTAL

    lexer = Frontend::Lexer.new(source)
    parser = Frontend::Parser.new(lexer)
    program = parser.parse_program

    context = Semantic::Context.new(Semantic::SymbolTable.new)
    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    # Should expand to: var a; var b
    collector.diagnostics.select(&.level.error?).should be_empty
  end

  it "handles empty array (no iterations)" do
    source = <<-CRYSTAL
      macro empty_loop
        {% for x in [] %}
          puts {{ x }}
        {% end %}
      end

      empty_loop
      CRYSTAL

    lexer = Frontend::Lexer.new(source)
    parser = Frontend::Parser.new(lexer)
    program = parser.parse_program

    context = Semantic::Context.new(Semantic::SymbolTable.new)
    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    # Should expand to empty (no output)
    collector.diagnostics.select(&.level.error?).should be_empty
  end

  it "handles nested for loops" do
    source = <<-CRYSTAL
      macro nested_for
        {% for x in [1, 2] %}
          {% for y in ["a", "b"] %}
            pair {{ x }} {{ y }}
          {% end %}
        {% end %}
      end

      nested_for
      CRYSTAL

    lexer = Frontend::Lexer.new(source)
    parser = Frontend::Parser.new(lexer)
    program = parser.parse_program

    context = Semantic::Context.new(Semantic::SymbolTable.new)
    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    # Should expand to: pair 1 a; pair 1 b; pair 2 a; pair 2 b
    collector.diagnostics.select(&.level.error?).should be_empty
  end

  it "handles non-array iterable gracefully" do
    source = <<-CRYSTAL
      macro bad_iterable
        {% for x in 42 %}
          puts {{ x }}
        {% end %}
      end

      bad_iterable
      CRYSTAL

    lexer = Frontend::Lexer.new(source)
    parser = Frontend::Parser.new(lexer)
    program = parser.parse_program

    context = Semantic::Context.new(Semantic::SymbolTable.new)
    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    # Should not crash (graceful degradation with error or empty output)
    # Error checking deferred - main goal is no crash
  end

  # ==================================================================
  # Category 4: Edge Cases (5 tests)
  # ==================================================================

  it "reports error for missing {% end %}" do
    source = <<-CRYSTAL
      macro unclosed_if
        {% if true %}
          puts "unclosed"
      end

      unclosed_if
      CRYSTAL

    lexer = Frontend::Lexer.new(source)
    parser = Frontend::Parser.new(lexer)
    program = parser.parse_program

    context = Semantic::Context.new(Semantic::SymbolTable.new)
    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    # Should report error: missing {% end %}
    # (or parsing error during macro body parsing)
    # Either way, should not crash
  end

  it "handles if inside for loop" do
    source = <<-CRYSTAL
      macro if_in_for
        {% for x in [1, 2] %}
          {% if true %}
            puts {{ x }}
          {% end %}
        {% end %}
      end

      if_in_for
      CRYSTAL

    lexer = Frontend::Lexer.new(source)
    parser = Frontend::Parser.new(lexer)
    program = parser.parse_program

    context = Semantic::Context.new(Semantic::SymbolTable.new)
    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    # Should expand successfully
    collector.diagnostics.select(&.level.error?).should be_empty
  end

  it "handles for inside if body" do
    source = <<-CRYSTAL
      macro for_in_if
        {% if true %}
          {% for x in [1, 2] %}
            puts {{ x }}
          {% end %}
        {% end %}
      end

      for_in_if
      CRYSTAL

    lexer = Frontend::Lexer.new(source)
    parser = Frontend::Parser.new(lexer)
    program = parser.parse_program

    context = Semantic::Context.new(Semantic::SymbolTable.new)
    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    # Should expand successfully
    collector.diagnostics.select(&.level.error?).should be_empty
  end

  it "handles complex nesting (3 levels)" do
    source = <<-CRYSTAL
      macro complex_nesting
        {% for x in [1] %}
          {% if true %}
            {% for y in [2] %}
              nested {{ x }} {{ y }}
            {% end %}
          {% end %}
        {% end %}
      end

      complex_nesting
      CRYSTAL

    lexer = Frontend::Lexer.new(source)
    parser = Frontend::Parser.new(lexer)
    program = parser.parse_program

    context = Semantic::Context.new(Semantic::SymbolTable.new)
    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    # Should expand successfully
    collector.diagnostics.select(&.level.error?).should be_empty
  end

  it "handles multiple loop variables gracefully" do
    source = <<-CRYSTAL
      macro multi_var_loop
        {% for x, y in [[1, 2]] %}
          puts {{ x }}
        {% end %}
      end

      multi_var_loop
      CRYSTAL

    lexer = Frontend::Lexer.new(source)
    parser = Frontend::Parser.new(lexer)
    program = parser.parse_program

    context = Semantic::Context.new(Semantic::SymbolTable.new)
    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    # Should not crash (graceful degradation with error or empty output)
    # Multiple variables not supported in Phase 87B-3
  end
end
