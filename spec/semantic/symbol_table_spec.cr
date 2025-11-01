require "spec"

require "../../src/compiler/frontend/ast"
require "../../src/compiler/semantic/symbol_table"
require "../../src/compiler/semantic/symbol"

alias SymbolTable = CrystalV2::Compiler::Semantic::SymbolTable
alias DummySymbol = CrystalV2::Compiler::Semantic::DummySymbol
alias SymbolRedefinitionError = CrystalV2::Compiler::Semantic::SymbolRedefinitionError
alias ExprId = CrystalV2::Compiler::Frontend::ExprId

private def dummy_symbol(name, metadata)
  DummySymbol.new(name, ExprId.new(-1), metadata)
end

describe SymbolTable do
  it "defines and looks up local symbols" do
    table = SymbolTable.new
    symbol = dummy_symbol("foo", "test")

    table.define("foo", symbol)
    table.lookup("foo").should be(symbol)
  end

  it "finds symbols in parent scopes" do
    parent = SymbolTable.new
    parent_symbol = dummy_symbol("bar", "parent")
    parent.define("bar", parent_symbol)

    child = SymbolTable.new(parent)
    child.lookup("bar").should be(parent_symbol)
  end

  it "distinguishes local and inherited symbols" do
    parent = SymbolTable.new
    parent.define("foo", dummy_symbol("foo", "parent"))

    child = SymbolTable.new(parent)
    child.local?("foo").should be_false
    child.define("bar", dummy_symbol("bar", "local"))
    child.local?("bar").should be_true
  end

  it "raises on duplicate definitions" do
    table = SymbolTable.new
    table.define("dup", dummy_symbol("dup", "first"))

    expect_raises(SymbolRedefinitionError) do
      table.define("dup", dummy_symbol("dup", "second"))
    end
  end
end
