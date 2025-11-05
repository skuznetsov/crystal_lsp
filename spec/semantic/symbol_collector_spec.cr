require "spec"
require "./ast_fixtures"

require "../../src/compiler/frontend/ast"
require "../../src/compiler/frontend/lexer"
require "../../src/compiler/frontend/parser"
require "../../src/compiler/semantic/context"
require "../../src/compiler/semantic/collectors/symbol_collector"
require "../../src/compiler/semantic/symbol"

module SymbolCollectorSpecAliases
  alias Frontend = CrystalV2::Compiler::Frontend
  alias Semantic = CrystalV2::Compiler::Semantic
end

include SymbolCollectorSpecAliases

describe Semantic::SymbolCollector do
  it "collects macro definitions into the global symbol table" do
    arena = Frontend::AstArena.new
    macro_id = AstFixtures.make_macro(arena, "greet")

    program = Frontend::Program.new(arena, [macro_id])
    context = Semantic::Context.new(Semantic::SymbolTable.new)

    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    symbol = context.symbol_table.lookup("greet").should_not be_nil
    symbol.should be_a(Semantic::MacroSymbol)

    macro_def = arena[macro_id].as(Frontend::MacroDefNode)
    symbol.as(Semantic::MacroSymbol).body.should eq(macro_def.body)
  end

  it "redefines macros when names repeat" do
    arena = Frontend::AstArena.new
    first_macro = AstFixtures.make_macro(arena, "repeat")
    second_macro = AstFixtures.make_macro(arena, "repeat")

    program = Frontend::Program.new(arena, [first_macro, second_macro])
    context = Semantic::Context.new(Semantic::SymbolTable.new)

    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    symbol = context.symbol_table.lookup("repeat").should_not be_nil
    symbol.as(Semantic::MacroSymbol).node_id.should eq(second_macro)
  end

  it "collects method definitions with parameters" do
    arena = Frontend::AstArena.new
    method_id = AstFixtures.make_def(arena, "greet", params: ["name"])

    program = Frontend::Program.new(arena, [method_id])
    context = Semantic::Context.new(Semantic::SymbolTable.new)

    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    symbol = context.symbol_table.lookup("greet").should_not be_nil
    method_symbol = symbol.as(Semantic::MethodSymbol)
    method_symbol.params.map { |p| String.new(p.name.not_nil!) }.should eq(["name"])
    method_symbol.scope.lookup("name").should be_a(Semantic::VariableSymbol)
  end

  it "collects method return type annotation" do
    source = <<-CR
      def get_number : Int32
        42
      end
    CR

    lexer = Frontend::Lexer.new(source)
    parser = Frontend::Parser.new(lexer)
    program = parser.parse_program

    context = Semantic::Context.new(Semantic::SymbolTable.new)
    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    symbol = context.symbol_table.lookup("get_number").should_not be_nil
    method_symbol = symbol.as(Semantic::MethodSymbol)
    method_symbol.return_annotation.should eq("Int32")
  end

  it "collects class definitions and nested methods" do
    arena = Frontend::AstArena.new
    method_id = AstFixtures.make_def(arena, "greet")
    class_id = AstFixtures.make_class(arena, "Greeter", body: [method_id])

    program = Frontend::Program.new(arena, [class_id])
    context = Semantic::Context.new(Semantic::SymbolTable.new)

    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    class_symbol = context.symbol_table.lookup("Greeter").should_not be_nil
    class_symbol.should be_a(Semantic::ClassSymbol)
    class_scope = class_symbol.as(Semantic::ClassSymbol).scope
    class_scope.lookup("greet").should be_a(Semantic::MethodSymbol)
  end

  it "emits diagnostic for incompatible redefinition" do
    arena = Frontend::AstArena.new
    class_id = AstFixtures.make_class(arena, "Thing")
    def_id = AstFixtures.make_def(arena, "Thing")

    program = Frontend::Program.new(arena, [class_id, def_id])
    context = Semantic::Context.new(Semantic::SymbolTable.new)

    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    diags = collector.diagnostics
    diags.size.should eq(1)
    diag = diags.first
    diag.code.should eq("E2001")
    diag.message.should contain("cannot redefine class 'Thing' as method")
  end

  it "warns when method parameter shadows outer variable" do
    arena = Frontend::AstArena.new
    inner_def = AstFixtures.make_def(arena, "inner", params: ["name"])
    outer_def = AstFixtures.make_def(arena, "outer", params: ["name"], body: [inner_def])

    program = Frontend::Program.new(arena, [outer_def])
    context = Semantic::Context.new(Semantic::SymbolTable.new)

    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    collector.diagnostics.any? { |diag| diag.code == "W2001" }.should be_true
  end

  it "emits error when parameters duplicate within a scope" do
    arena = Frontend::AstArena.new
    def_id = AstFixtures.make_def(arena, "echo", params: ["value", "value"])

    program = Frontend::Program.new(arena, [def_id])
    context = Semantic::Context.new(Semantic::SymbolTable.new)

    collector = Semantic::SymbolCollector.new(program, context)
    collector.collect

    collector.diagnostics.any? { |diag| diag.code == "E2002" }.should be_true
  end
end
