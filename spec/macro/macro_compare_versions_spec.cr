require "spec"

require "../../src/compiler/semantic/analyzer"
require "../../src/runtime"

private def expand_macro_with_context(
  source : String,
  macro_name : String,
  flags : Set(String) = CrystalV2::Runtime.target_flags
) : {String, Array(CrystalV2::Compiler::Semantic::Diagnostic)}
  lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
  parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
  program = parser.parse_program

  context = CrystalV2::Compiler::Semantic::Context.new(
    CrystalV2::Compiler::Semantic::SymbolTable.new,
    flags
  )

  analyzer = CrystalV2::Compiler::Semantic::Analyzer.new(program, context)
  analyzer.collect_symbols

  symbol = context.symbol_table.lookup_macro(macro_name)
  symbol.should_not be_nil

  expander = CrystalV2::Compiler::Semantic::MacroExpander.new(
    program,
    program.arena,
    context.flags,
    symbol_table: context.symbol_table
  )

  expr_id = expander.expand(symbol.not_nil!, [] of CrystalV2::Compiler::Frontend::ExprId)
  node = program.arena[expr_id]
  {
    CrystalV2::Compiler::Frontend.node_literal_string(node) || expander.last_output.to_s.strip,
    expander.diagnostics.dup,
  }
end

describe "Macro compare_versions evaluation" do
  it "compares semantic version literals" do
    source = <<-CR
    macro __compare_versions_literal
      {{ compare_versions("1.11.0-dev", "1.11.0") }}
    end
    CR

    result, diagnostics = expand_macro_with_context(source, "__compare_versions_literal", Set(String).new)
    diagnostics.should be_empty
    result.should eq("-1")
  end

  it "selects elsif branches that compare Crystal::LLVM_VERSION" do
    source = <<-CR
    module Crystal
      LLVM_VERSION = "12.0.0"
    end

    macro __powi_branch
      {% if flag?(:win32) %}
        "win32"
      {% elsif flag?(:interpreted) %}
        "interpreted"
      {% elsif compare_versions(Crystal::LLVM_VERSION, "13.0.0") < 0 %}
        "legacy"
      {% else %}
        "modern"
      {% end %}
    end
    CR

    result, diagnostics = expand_macro_with_context(source, "__powi_branch", Set(String).new)
    diagnostics.should be_empty
    result.should eq("legacy")
  end

  it "supports host Crystal constants in compare_versions conditions" do
    source = <<-CR
    macro __host_version_branch
      {% if compare_versions(Crystal::VERSION, "0.0.0") > 0 %}
        "newer"
      {% else %}
        "older"
      {% end %}
    end
    CR

    result, diagnostics = expand_macro_with_context(source, "__host_version_branch", Set(String).new)
    diagnostics.should be_empty
    result.should eq("newer")
  end
end
