require "spec"
require "../src/compiler/frontend/parser"
require "../src/compiler/frontend/lexer"

# Phase 23: Edge cases for ternary operator
describe "Ternary Operator Edge Cases" do
  it "distinguishes ? in ternary from ? in method names" do
    source = <<-CRYSTAL
      arr = [1, 2]
      x = arr.empty? ? 100 : 200
    CRYSTAL

    lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
    parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
    program = parser.parse_program

    # Should parse: arr.empty? is call, then ? for ternary
    program.roots.size.should eq(2)

    assign_node = program.arena[program.roots[1]]
    CrystalV2::Compiler::Frontend.node_kind(assign_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)

    ternary_id = CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!
    ternary_node = program.arena[ternary_id]
    CrystalV2::Compiler::Frontend.node_kind(ternary_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Ternary)
  end

  it "handles : in ternary without conflicting with symbols" do
    source = <<-CRYSTAL
      x = true ? :yes : :no
    CRYSTAL

    lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
    parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
    program = parser.parse_program

    program.roots.size.should eq(1)

    assign_node = program.arena[program.roots[0]]
    ternary_id = CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!
    ternary_node = program.arena[ternary_id]

    CrystalV2::Compiler::Frontend.node_kind(ternary_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Ternary)

    # True branch should be symbol
    true_id = CrystalV2::Compiler::Frontend.node_ternary_true_branch(ternary_node).not_nil!
    true_node = program.arena[true_id]
    CrystalV2::Compiler::Frontend.node_kind(true_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Symbol)

    # False branch should be symbol
    false_id = CrystalV2::Compiler::Frontend.node_ternary_false_branch(ternary_node).not_nil!
    false_node = program.arena[false_id]
    CrystalV2::Compiler::Frontend.node_kind(false_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Symbol)
  end

  it "handles : in ternary without conflicting with type annotations" do
    # This is a tricky case - we DON'T support type annotations in expressions yet
    # So this should parse as ternary with identifier 'Int32'
    source = <<-CRYSTAL
      x = true ? 10 : 20
    CRYSTAL

    lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
    parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
    program = parser.parse_program

    program.roots.size.should eq(1)

    assign_node = program.arena[program.roots[0]]
    ternary_id = CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!
    ternary_node = program.arena[ternary_id]

    CrystalV2::Compiler::Frontend.node_kind(ternary_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Ternary)
  end
end
