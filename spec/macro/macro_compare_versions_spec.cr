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

private def expand_first_top_level_macro_text(
  source : String,
  *,
  flags : Set(String) = CrystalV2::Runtime.target_flags,
  scope_name : String? = nil
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

  expander = CrystalV2::Compiler::Semantic::MacroExpander.new(
    program,
    program.arena,
    context.flags,
    symbol_table: context.symbol_table
  )

  scope = if scope_name
            context.symbol_table.lookup(scope_name).as(CrystalV2::Compiler::Semantic::ModuleSymbol).scope
          else
            context.symbol_table
          end

  target_id = if scope_name
                lib_node = program.roots
                  .map { |id| program.arena[id].as?(CrystalV2::Compiler::Frontend::LibNode) }
                  .compact
                  .find { |node| String.new(node.name) == scope_name }
                lib_node.should_not be_nil
                body = lib_node.not_nil!.body || [] of CrystalV2::Compiler::Frontend::ExprId
                body.find do |id|
                  node = program.arena[id]
                  node.is_a?(CrystalV2::Compiler::Frontend::MacroLiteralNode) ||
                    node.is_a?(CrystalV2::Compiler::Frontend::MacroIfNode) ||
                    node.is_a?(CrystalV2::Compiler::Frontend::MacroForNode)
                end
              else
                program.roots.find do |id|
                  node = program.arena[id]
                  node.is_a?(CrystalV2::Compiler::Frontend::MacroLiteralNode) ||
                    node.is_a?(CrystalV2::Compiler::Frontend::MacroIfNode) ||
                    node.is_a?(CrystalV2::Compiler::Frontend::MacroForNode)
                end
              end

  target_id.should_not be_nil
  output = expander.expand_top_level_text(target_id.not_nil!, scope: scope)
  {output.strip, expander.diagnostics.dup}
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

  it "expands top-level elsif branches that compare Crystal::LLVM_VERSION" do
    source = <<-CR
    module Crystal
      LLVM_VERSION = "12.0.0"
    end

    {% if flag?(:win32) %}
      "win32"
    {% elsif flag?(:interpreted) %}
      "interpreted"
    {% elsif compare_versions(Crystal::LLVM_VERSION, "13.0.0") < 0 %}
      "legacy"
    {% else %}
      "modern"
    {% end %}
    CR

    result, diagnostics = expand_first_top_level_macro_text(source, flags: Set(String).new)
    diagnostics.should be_empty
    result.should eq(%("legacy"))
  end

  it "expands scoped compare_versions conditions from local VERSION constants" do
    source = <<-CR
    lib LibGC
      VERSION = {{ `printf "8.2.1"`.chomp.stringify }}

      {% if compare_versions(VERSION, "8.2.0") >= 0 %}
        fun ok
      {% else %}
        fun bad
      {% end %}
    end
    CR

    result, diagnostics = expand_first_top_level_macro_text(source, scope_name: "LibGC", flags: Set(String).new)
    diagnostics.should be_empty
    result.should contain("fun ok")
  end

  it "uses macro source providers for compare_versions constants outside the current macro body" do
    source = <<-CR
    lib LibGC
      VERSION = {{ `printf "8.2.1"`.chomp.stringify }}

      {% if compare_versions(VERSION, "8.2.0") >= 0 %}
        fun ok
      {% else %}
        fun bad
      {% end %}
    end
    CR

    lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
    parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
    program = parser.parse_program

    context = CrystalV2::Compiler::Semantic::Context.new(
      CrystalV2::Compiler::Semantic::SymbolTable.new,
      Set(String).new
    )

    analyzer = CrystalV2::Compiler::Semantic::Analyzer.new(program, context)
    analyzer.collect_symbols

    lib_symbol = context.symbol_table.lookup("LibGC").as(CrystalV2::Compiler::Semantic::ModuleSymbol)
    lib_node = program.roots
      .map { |id| program.arena[id].as?(CrystalV2::Compiler::Frontend::LibNode) }
      .compact
      .find { |node| String.new(node.name) == "LibGC" }
    lib_node.should_not be_nil

    body = lib_node.not_nil!.body || [] of CrystalV2::Compiler::Frontend::ExprId
    target_id = body.find do |id|
      program.arena[id].is_a?(CrystalV2::Compiler::Frontend::MacroIfNode)
    end
    target_id.should_not be_nil

    current_macro_body = <<-CR
    {% if compare_versions(VERSION, "8.2.0") >= 0 %}
      fun ok
    {% else %}
      fun bad
    {% end %}
    CR

    expander = CrystalV2::Compiler::Semantic::MacroExpander.new(
      program,
      program.arena,
      Set(String).new,
      symbol_table: context.symbol_table,
      macro_source: current_macro_body
    )
    expander.macro_source_provider = ->(node_id : CrystalV2::Compiler::Frontend::ExprId) { source.as(String?) }

    result = expander.expand_top_level_text(target_id.not_nil!, scope: lib_symbol.scope)
    expander.diagnostics.should be_empty
    result.should contain("fun ok")
  end

  it "expands scoped elsif branches that compare Crystal::LLVM_VERSION" do
    source = <<-CR
    lib LibM
      {% if flag?(:win32) %}
      {% elsif flag?(:interpreted) %}
        fun interp
      {% elsif compare_versions(Crystal::LLVM_VERSION, "13.0.0") < 0 %}
        fun legacy
      {% else %}
        fun modern
      {% end %}
    end
    CR

    result, diagnostics = expand_first_top_level_macro_text(source, scope_name: "LibM", flags: Set(String).new)
    diagnostics.should be_empty
    result.should contain("fun modern")
  end

  it "expands raw-text lib macro literals that compare Crystal::LLVM_VERSION" do
    source = File.read("src/stdlib/math/libm.cr")
    lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
    parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
    program = parser.parse_program

    context = CrystalV2::Compiler::Semantic::Context.new(
      CrystalV2::Compiler::Semantic::SymbolTable.new,
      Set(String).new
    )

    analyzer = CrystalV2::Compiler::Semantic::Analyzer.new(program, context)
    analyzer.collect_symbols

    lib_symbol = context.symbol_table.lookup("LibM").as(CrystalV2::Compiler::Semantic::ModuleSymbol)
    lib_node = program.roots
      .map { |id| program.arena[id].as?(CrystalV2::Compiler::Frontend::LibNode) }
      .compact
      .find { |node| String.new(node.name) == "LibM" }
    lib_node.should_not be_nil

    body = lib_node.not_nil!.body || [] of CrystalV2::Compiler::Frontend::ExprId
    target_id = body.find do |id|
      node = program.arena[id]
      node.span.start_line == 92
    end
    target_id.should_not be_nil

    target = program.arena[target_id.not_nil!]
    target.should be_a(CrystalV2::Compiler::Frontend::MacroLiteralNode)
    target.as(CrystalV2::Compiler::Frontend::MacroLiteralNode).pieces.size.should eq(1)

    expander = CrystalV2::Compiler::Semantic::MacroExpander.new(
      program,
      program.arena,
      Set(String).new,
      symbol_table: context.symbol_table
    )

    result = expander.expand_top_level_text(target_id.not_nil!, scope: lib_symbol.scope)
    expander.diagnostics.should be_empty
    result.should contain("llvm.powi.f32.i32")
    result.should contain("llvm.powi.f64.i32")
    result.should_not contain("{%")
  end
end
