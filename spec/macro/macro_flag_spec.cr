require "spec"

require "../../src/compiler/semantic/analyzer"
require "../../src/runtime"

private def expand_flag_macro(flag_name : String) : String
  source = <<-CR
  macro __flag_test
    {% if flag?(:#{flag_name}) %}
      "true"
    {% else %}
      "false"
    {% end %}
  end
  CR

  lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
  parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
  program = parser.parse_program
  context = CrystalV2::Compiler::Semantic::Context.new(CrystalV2::Compiler::Semantic::SymbolTable.new)
  context.flags.should_not be_empty

  analyzer = CrystalV2::Compiler::Semantic::Analyzer.new(program, context)
  analyzer.collect_symbols
  table = context.symbol_table
  symbol = table.lookup("__flag_test").as?(CrystalV2::Compiler::Semantic::MacroSymbol)
  symbol.should_not be_nil

  expander = CrystalV2::Compiler::Semantic::MacroExpander.new(program, program.arena, context.flags)
  expr_id = expander.expand(symbol.not_nil!, [] of CrystalV2::Compiler::Frontend::ExprId)
  node = program.arena[expr_id]
  CrystalV2::Compiler::Frontend.node_literal_string(node) || ""
end

describe "Macro flag? evaluation" do
  it "returns true for existing flags" do
    flag = CrystalV2::Runtime.target_flags.first?
    flag.should_not be_nil
    expand_flag_macro(flag.not_nil!).should eq("true")
  end

  it "returns false for missing flags" do
    expand_flag_macro("nonexistent_flag").should eq("false")
  end

  it "respects custom flags passed to context" do
    source = <<-CR
    macro __custom_flag_test
      {% if flag?(:my_custom_feature) %}
        "enabled"
      {% else %}
        "disabled"
      {% end %}
    end
    CR

    lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
    parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
    program = parser.parse_program

    # Create context with custom flag
    custom_flags = CrystalV2::Runtime.target_flags.dup
    custom_flags << "my_custom_feature"
    context = CrystalV2::Compiler::Semantic::Context.new(CrystalV2::Compiler::Semantic::SymbolTable.new, custom_flags)

    analyzer = CrystalV2::Compiler::Semantic::Analyzer.new(program, context)
    analyzer.collect_symbols
    table = context.symbol_table
    symbol = table.lookup("__custom_flag_test").as?(CrystalV2::Compiler::Semantic::MacroSymbol)
    symbol.should_not be_nil

    expander = CrystalV2::Compiler::Semantic::MacroExpander.new(program, program.arena, context.flags)
    expr_id = expander.expand(symbol.not_nil!, [] of CrystalV2::Compiler::Frontend::ExprId)
    node = program.arena[expr_id]
    result = CrystalV2::Compiler::Frontend.node_literal_string(node) || ""
    result.should eq("enabled")
  end
end
