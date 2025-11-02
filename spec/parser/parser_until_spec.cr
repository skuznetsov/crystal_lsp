require "spec"

require "../../src/compiler/frontend/parser"

describe "CrystalV2::Compiler::Frontend::Parser" do
  describe "Phase 25: Until" do
    it "parses basic until loop" do
      source = <<-CRYSTAL
        until false
          x = 10
        end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      until_node = arena[program.roots.first]
      CrystalV2::Compiler::Frontend.node_kind(until_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Until)

      # Check condition
      condition = arena[CrystalV2::Compiler::Frontend.node_condition(until_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(condition).should eq(CrystalV2::Compiler::Frontend::NodeKind::Bool)
      CrystalV2::Compiler::Frontend.node_literal_string(condition).should eq("false")

      # Check body
      body = CrystalV2::Compiler::Frontend.node_while_body(until_node).not_nil!
      body.size.should eq(1)

      assign = arena[body[0]]
      CrystalV2::Compiler::Frontend.node_kind(assign).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)
    end

    it "parses until with empty body" do
      source = <<-CRYSTAL
        until true
        end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      until_node = arena[program.roots.first]
      CrystalV2::Compiler::Frontend.node_kind(until_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Until)

      # Check condition is Bool
      condition = arena[CrystalV2::Compiler::Frontend.node_condition(until_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(condition).should eq(CrystalV2::Compiler::Frontend::NodeKind::Bool)
      CrystalV2::Compiler::Frontend.node_literal_string(condition).should eq("true")

      body = CrystalV2::Compiler::Frontend.node_while_body(until_node).not_nil!
      body.size.should eq(0)
    end

    it "parses until with multiple statements" do
      source = <<-CRYSTAL
        until x == 10
          x = x + 1
          y = y + 2
          z = z + 3
        end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      until_node = arena[program.roots.first]
      CrystalV2::Compiler::Frontend.node_kind(until_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Until)

      body = CrystalV2::Compiler::Frontend.node_while_body(until_node).not_nil!
      body.size.should eq(3)
    end

    it "parses until with complex condition" do
      source = <<-CRYSTAL
        until x > 10 && y < 20
          process()
        end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      until_node = arena[program.roots.first]
      CrystalV2::Compiler::Frontend.node_kind(until_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Until)

      # Check condition is binary AND
      condition = arena[CrystalV2::Compiler::Frontend.node_condition(until_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(condition).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      String.new(CrystalV2::Compiler::Frontend.node_operator(condition).not_nil!).should eq("&&")
    end

    it "parses until with break inside" do
      source = <<-CRYSTAL
        until false
          break
        end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      until_node = arena[program.roots.first]
      CrystalV2::Compiler::Frontend.node_kind(until_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Until)

      body = CrystalV2::Compiler::Frontend.node_while_body(until_node).not_nil!
      body.size.should eq(1)

      # Body contains break statement
      break_node = arena[body[0]]
      CrystalV2::Compiler::Frontend.node_kind(break_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Break)
    end
  end
end
