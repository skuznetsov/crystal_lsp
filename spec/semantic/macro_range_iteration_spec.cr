require "spec"

require "../../src/compiler/frontend/ast"
require "../../src/compiler/frontend/lexer"
require "../../src/compiler/frontend/parser"
require "../../src/compiler/semantic/context"
require "../../src/compiler/semantic/symbol_table"
require "../../src/compiler/semantic/collectors/symbol_collector"

include CrystalV2::Compiler

describe "Phase 87B-4A: Range Iteration in Macros" do
  # ==================================================================
  # Category 1: Basic Range Iteration (3 tests)
  # ==================================================================

  it "iterates over inclusive range (1..3)" do
    source = <<-CRYSTAL
      macro generate_methods
        {% for x in 1..3 %}
          method_{{ x }}
        {% end %}
      end

      generate_methods
      CRYSTAL

    lexer = Frontend::Lexer.new(source)
    parser = Frontend::Parser.new(lexer)
    program = parser.parse_program

    context = Semantic::Context.new(Semantic::SymbolTable.new)
    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    # Should expand to: method_1, method_2, method_3 (3 iterations)
    collector.diagnostics.select(&.level.error?).should be_empty
  end

  it "iterates over exclusive range (1...3)" do
    source = <<-CRYSTAL
      macro generate_vars
        {% for x in 1...3 %}
          var_{{ x }}
        {% end %}
      end

      generate_vars
      CRYSTAL

    lexer = Frontend::Lexer.new(source)
    parser = Frontend::Parser.new(lexer)
    program = parser.parse_program

    context = Semantic::Context.new(Semantic::SymbolTable.new)
    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    # Should expand to: var_1, var_2 (2 iterations, excludes 3)
    collector.diagnostics.select(&.level.error?).should be_empty
  end

  it "handles single element range (5..5)" do
    source = <<-CRYSTAL
      macro single_value
        {% for x in 5..5 %}
          value_{{ x }}
        {% end %}
      end

      single_value
      CRYSTAL

    lexer = Frontend::Lexer.new(source)
    parser = Frontend::Parser.new(lexer)
    program = parser.parse_program

    context = Semantic::Context.new(Semantic::SymbolTable.new)
    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    # Should expand to: value_5 (1 iteration)
    collector.diagnostics.select(&.level.error?).should be_empty
  end

  # ==================================================================
  # Category 2: Edge Cases (4 tests)
  # ==================================================================

  it "handles empty exclusive range (5...5)" do
    source = <<-CRYSTAL
      macro empty_range
        {% for x in 5...5 %}
          never_{{ x }}
        {% end %}
      end

      empty_range
      CRYSTAL

    lexer = Frontend::Lexer.new(source)
    parser = Frontend::Parser.new(lexer)
    program = parser.parse_program

    context = Semantic::Context.new(Semantic::SymbolTable.new)
    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    # Should expand to empty (0 iterations)
    collector.diagnostics.select(&.level.error?).should be_empty
  end

  it "handles reverse range as empty (10..1)" do
    source = <<-CRYSTAL
      macro reverse_range
        {% for x in 10..1 %}
          backward_{{ x }}
        {% end %}
      end

      reverse_range
      CRYSTAL

    lexer = Frontend::Lexer.new(source)
    parser = Frontend::Parser.new(lexer)
    program = parser.parse_program

    context = Semantic::Context.new(Semantic::SymbolTable.new)
    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    # Should expand to empty (0 iterations, Crystal behavior)
    collector.diagnostics.select(&.level.error?).should be_empty
  end

  it "handles negative range (-2..2)" do
    source = <<-CRYSTAL
      macro negative_range
        {% for x in -2..2 %}
          num_{{ x }}
        {% end %}
      end

      negative_range
      CRYSTAL

    lexer = Frontend::Lexer.new(source)
    parser = Frontend::Parser.new(lexer)
    program = parser.parse_program

    context = Semantic::Context.new(Semantic::SymbolTable.new)
    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    # Should expand with negative numbers: -2, -1, 0, 1, 2 (5 iterations)
    collector.diagnostics.select(&.level.error?).should be_empty
  end

  it "handles large range near limit (1..1000)" do
    source = <<-CRYSTAL
      macro large_range
        {% for x in 1..1000 %}
          item_{{ x }}
        {% end %}
      end

      large_range
      CRYSTAL

    lexer = Frontend::Lexer.new(source)
    parser = Frontend::Parser.new(lexer)
    program = parser.parse_program

    context = Semantic::Context.new(Semantic::SymbolTable.new)
    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    # Should work (1000 elements < 10000 limit)
    collector.diagnostics.select(&.level.error?).should be_empty
  end

  # ==================================================================
  # Category 3: Error Cases (2 tests)
  # ==================================================================

  it "handles range too large gracefully (1..100000)" do
    source = <<-CRYSTAL
      macro huge_range
        {% for x in 1..100000 %}
          huge_{{ x }}
        {% end %}
      end

      huge_range
      CRYSTAL

    lexer = Frontend::Lexer.new(source)
    parser = Frontend::Parser.new(lexer)
    program = parser.parse_program

    context = Semantic::Context.new(Semantic::SymbolTable.new)
    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    # Should not crash (graceful degradation with error or empty output)
    # Size limit prevents compilation DOS
  end

  it "handles non-integer bounds gracefully (1.5..3.5)" do
    source = <<-CRYSTAL
      macro float_range
        {% for x in 1.5..3.5 %}
          float_{{ x }}
        {% end %}
      end

      float_range
      CRYSTAL

    lexer = Frontend::Lexer.new(source)
    parser = Frontend::Parser.new(lexer)
    program = parser.parse_program

    context = Semantic::Context.new(Semantic::SymbolTable.new)
    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    # Should not crash (graceful degradation with error or empty output)
    # Phase 87B-4A supports integer bounds only
  end

  # ==================================================================
  # Category 4: Integration (1 test)
  # ==================================================================

  it "handles nested range loops" do
    source = <<-CRYSTAL
      macro matrix
        {% for x in 1..2 %}
          {% for y in 1..2 %}
            cell_{{ x }}_{{ y }}
          {% end %}
        {% end %}
      end

      matrix
      CRYSTAL

    lexer = Frontend::Lexer.new(source)
    parser = Frontend::Parser.new(lexer)
    program = parser.parse_program

    context = Semantic::Context.new(Semantic::SymbolTable.new)
    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    # Should expand nested loops: cell_1_1, cell_1_2, cell_2_1, cell_2_2
    collector.diagnostics.select(&.level.error?).should be_empty
  end
end
