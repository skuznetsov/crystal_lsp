require "spec"
require "./ast_fixtures"

require "../../src/compiler/frontend/ast"
require "../../src/compiler/semantic/analyzer"

module NameResolverSpecAliases
  alias Frontend = CrystalV2::Compiler::Frontend
  alias Semantic = CrystalV2::Compiler::Semantic
end

include NameResolverSpecAliases

private def test_span
  Frontend::Span.new(0, 0, 1, 1, 1, 1)
end

describe Semantic::NameResolver do
  it "resolves identifiers to macro symbols" do
    arena = Frontend::AstArena.new

    body_id = arena.add(Frontend::MacroLiteralNode.new(
      test_span,
      [] of Frontend::MacroPiece,
      false,
      false
    ))

    macro_id = arena.add(Frontend::MacroDefNode.new(
      test_span,
      "greet".to_slice,
      body_id
    ))

    callee_id = arena.add(Frontend::IdentifierNode.new(
      test_span,
      "greet".to_slice
    ))

    call_id = arena.add(Frontend::CallNode.new(
      test_span,
      callee_id,
      [] of Frontend::ExprId,
      nil,
      nil
    ))

    program = Frontend::Program.new(arena, [macro_id, call_id])
    analyzer = Semantic::Analyzer.new(program)
    analyzer.collect_symbols
    result = analyzer.resolve_names

    result.diagnostics.should be_empty
    result.identifier_symbols[callee_id].should be_a(Semantic::MacroSymbol)
  end

  it "emits diagnostics for undefined identifiers" do
    arena = Frontend::AstArena.new

    callee_id = arena.add(Frontend::IdentifierNode.new(
      test_span,
      "missing".to_slice
    ))

    call_id = arena.add(Frontend::CallNode.new(
      test_span,
      callee_id,
      [] of Frontend::ExprId,
      nil,
      nil
    ))

    program = Frontend::Program.new(arena, [call_id])
    analyzer = Semantic::Analyzer.new(program)
    analyzer.collect_symbols
    result = analyzer.resolve_names

    result.identifier_symbols.should be_empty
    result.diagnostics.size.should eq(1)
    result.diagnostics.first.message.should eq("undefined local variable or method 'missing'")
  end

  it "resolves method parameters within method scope" do
    arena = Frontend::AstArena.new

    param_ref = AstFixtures.make_identifier(arena, "name")
    method_id = AstFixtures.make_def(arena, "greet", params: ["name"], body: [param_ref])

    program = Frontend::Program.new(arena, [method_id])
    analyzer = Semantic::Analyzer.new(program)
    analyzer.collect_symbols
    result = analyzer.resolve_names

    result.diagnostics.should be_empty
    result.identifier_symbols[param_ref].should be_a(Semantic::VariableSymbol)
  end

  it "resolves method calls within class scope" do
    arena = Frontend::AstArena.new

    greet_method = AstFixtures.make_def(arena, "greet")
    call_id = AstFixtures.make_call(arena, "greet")
    call_node = arena[call_id]
    call_node.should be_a(Frontend::CallNode)
    callee_id = call_node.as(Frontend::CallNode).callee
    say_hello = AstFixtures.make_def(arena, "say_hello", body: [call_id])
    class_id = AstFixtures.make_class(arena, "Greeter", body: [greet_method, say_hello])

    program = Frontend::Program.new(arena, [class_id])
    analyzer = Semantic::Analyzer.new(program)
    analyzer.collect_symbols
    result = analyzer.resolve_names

    result.diagnostics.should be_empty
    result.identifier_symbols[callee_id].should be_a(Semantic::MethodSymbol)
  end
end
