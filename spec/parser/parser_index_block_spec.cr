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

    it "parses nilable indexer postfix chains inside ternary true branch" do
      source = "value = resolved ? table[key]?.try { |t| convert(t) } : nil"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      parser.diagnostics.should be_empty
      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      ternary = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(ternary).should eq(CrystalV2::Compiler::Frontend::NodeKind::Ternary)

      true_branch = arena[CrystalV2::Compiler::Frontend.node_ternary_true_branch(ternary).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(true_branch).should eq(CrystalV2::Compiler::Frontend::NodeKind::Call)

      try_callee = arena[CrystalV2::Compiler::Frontend.node_callee(true_branch).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(try_callee).should eq(CrystalV2::Compiler::Frontend::NodeKind::MemberAccess)
      String.new(CrystalV2::Compiler::Frontend.node_member(try_callee).not_nil!).should eq("try")

      nilable_index_call = arena[CrystalV2::Compiler::Frontend.node_left(try_callee).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(nilable_index_call).should eq(CrystalV2::Compiler::Frontend::NodeKind::Call)
      nilable_index_callee = arena[CrystalV2::Compiler::Frontend.node_callee(nilable_index_call).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_member(nilable_index_callee).not_nil!).should eq("[]?")
    end

    it "keeps indexed ternary conditions as ternary expressions" do
      source = "value = table[key] ? yes : no"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      parser.diagnostics.should be_empty
      arena = program.arena
      assign = arena[program.roots[0]]
      ternary = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]

      CrystalV2::Compiler::Frontend.node_kind(ternary).should eq(CrystalV2::Compiler::Frontend::NodeKind::Ternary)
      condition = arena[CrystalV2::Compiler::Frontend.node_ternary_condition(ternary).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(condition).should eq(CrystalV2::Compiler::Frontend::NodeKind::Index)
    end
  end
end
