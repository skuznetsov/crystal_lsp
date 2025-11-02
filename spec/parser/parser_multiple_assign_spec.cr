require "spec"

require "../../src/compiler/frontend/parser"

describe "CrystalV2::Compiler::Frontend::Parser" do
  describe "Phase 73: Multiple assignment (a, b = 1, 2) (PRODUCTION-READY)" do
    it "parses simple two-target assignment" do
      source = "a, b = 1, 2"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      multi_assign = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(multi_assign).should eq(CrystalV2::Compiler::Frontend::NodeKind::MultipleAssign)

      targets = multi_assign.as(CrystalV2::Compiler::Frontend::MultipleAssignNode).targets
      targets.size.should eq(2)

      target1 = arena[targets[0]]
      CrystalV2::Compiler::Frontend.node_kind(target1).should eq(CrystalV2::Compiler::Frontend::NodeKind::Identifier)

      target2 = arena[targets[1]]
      CrystalV2::Compiler::Frontend.node_kind(target2).should eq(CrystalV2::Compiler::Frontend::NodeKind::Identifier)
    end

    it "parses three-target assignment" do
      source = "a, b, c = 1, 2, 3"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      multi_assign = arena[program.roots[0]]
      targets = multi_assign.as(CrystalV2::Compiler::Frontend::MultipleAssignNode).targets
      targets.size.should eq(3)
    end

    it "parses assignment with tuple literal on right side" do
      source = "a, b = {1, 2}"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      multi_assign = arena[program.roots[0]]
      targets = multi_assign.as(CrystalV2::Compiler::Frontend::MultipleAssignNode).targets
      targets.size.should eq(2)

      value = arena[multi_assign.as(CrystalV2::Compiler::Frontend::MultipleAssignNode).value]
      CrystalV2::Compiler::Frontend.node_kind(value).should eq(CrystalV2::Compiler::Frontend::NodeKind::TupleLiteral)
    end

    it "parses assignment with expressions on right side" do
      source = "x, y = 1 + 1, 2 * 2"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      multi_assign = arena[program.roots[0]]
      targets = multi_assign.as(CrystalV2::Compiler::Frontend::MultipleAssignNode).targets
      targets.size.should eq(2)
    end

    it "parses assignment with array literal on right side" do
      source = "a, b = [1, 2]"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      multi_assign = arena[program.roots[0]]
      value = arena[multi_assign.as(CrystalV2::Compiler::Frontend::MultipleAssignNode).value]
      CrystalV2::Compiler::Frontend.node_kind(value).should eq(CrystalV2::Compiler::Frontend::NodeKind::ArrayLiteral)
    end

    it "parses assignment with method call on right side" do
      source = "a, b = foo.bar()"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      multi_assign = arena[program.roots[0]]
      value = arena[multi_assign.as(CrystalV2::Compiler::Frontend::MultipleAssignNode).value]
      CrystalV2::Compiler::Frontend.node_kind(value).should eq(CrystalV2::Compiler::Frontend::NodeKind::Call)
    end

    it "parses assignment with identifier on right side" do
      source = "a, b = c"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      multi_assign = arena[program.roots[0]]
      value = arena[multi_assign.as(CrystalV2::Compiler::Frontend::MultipleAssignNode).value]
      CrystalV2::Compiler::Frontend.node_kind(value).should eq(CrystalV2::Compiler::Frontend::NodeKind::Identifier)
    end

    it "parses assignment inside method body" do
      source = <<-CRYSTAL
      def foo
        a, b = 1, 2
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      method_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(method_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Def)

      body = CrystalV2::Compiler::Frontend.node_def_body(method_node).not_nil!
      body.size.should eq(1)

      multi_assign = arena[body[0]]
      CrystalV2::Compiler::Frontend.node_kind(multi_assign).should eq(CrystalV2::Compiler::Frontend::NodeKind::MultipleAssign)
    end

    it "parses multiple assignments in sequence" do
      source = <<-CRYSTAL
      a, b = 1, 2
      c, d = 3, 4
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(2)
      arena = program.arena

      multi_assign1 = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(multi_assign1).should eq(CrystalV2::Compiler::Frontend::NodeKind::MultipleAssign)

      multi_assign2 = arena[program.roots[1]]
      CrystalV2::Compiler::Frontend.node_kind(multi_assign2).should eq(CrystalV2::Compiler::Frontend::NodeKind::MultipleAssign)
    end

    it "parses assignment with string literals" do
      source = "a, b = \"hello\", \"world\""

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      multi_assign = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(multi_assign).should eq(CrystalV2::Compiler::Frontend::NodeKind::MultipleAssign)
    end

    it "parses assignment with nil and boolean values" do
      source = "a, b, c = nil, true, false"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      multi_assign = arena[program.roots[0]]
      targets = multi_assign.as(CrystalV2::Compiler::Frontend::MultipleAssignNode).targets
      targets.size.should eq(3)
    end

    it "parses assignment with mixed expression types" do
      source = "a, b, c = 1, \"two\", [3]"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      multi_assign = arena[program.roots[0]]
      targets = multi_assign.as(CrystalV2::Compiler::Frontend::MultipleAssignNode).targets
      targets.size.should eq(3)
    end

    it "distinguishes from single assignment" do
      source = "a = 1"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      # Should be regular Assign, not MultipleAssign
      CrystalV2::Compiler::Frontend.node_kind(assign).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)
    end

    it "parses four or more targets" do
      source = "a, b, c, d, e = 1, 2, 3, 4, 5"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      multi_assign = arena[program.roots[0]]
      targets = multi_assign.as(CrystalV2::Compiler::Frontend::MultipleAssignNode).targets
      targets.size.should eq(5)
    end
  end
end
