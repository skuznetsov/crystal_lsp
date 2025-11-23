require "spec"

require "../../src/compiler/frontend/parser"

describe "CrystalV2::Compiler::Frontend::Parser" do
  describe "Calls with blocks inside index operator" do
    it "parses foo[bar { 1 }]" do
      source = "foo[bar { 1 }]"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      index = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(index).should eq(CrystalV2::Compiler::Frontend::NodeKind::Index)
    end
  end
end
