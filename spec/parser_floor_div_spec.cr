require "spec"

require "../src/compiler/frontend/parser"

describe "CrystalV2::Compiler::Frontend::Parser" do
  describe "Phase 78: Floor division operator (//)" do
    it "parses simple floor division" do
      source = "7 // 2"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      binary = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(binary).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)

      CrystalV2::Compiler::Frontend.node_operator(binary).not_nil!.should eq("//".to_slice)
    end

    it "parses floor division with negative numbers" do
      source = "-7 // 2"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      binary = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(binary).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)

      CrystalV2::Compiler::Frontend.node_operator(binary).not_nil!.should eq("//".to_slice)
    end

    it "parses floor division in expression" do
      source = "x = 10 // 3"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(assign).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)

      value = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(value).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      CrystalV2::Compiler::Frontend.node_operator(value).not_nil!.should eq("//".to_slice)
    end

    it "parses floor division compound assignment" do
      source = "x //= 3"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(assign).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)

      # Should desugar to: x = x // 3
      value = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(value).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      CrystalV2::Compiler::Frontend.node_operator(value).not_nil!.should eq("//".to_slice)
    end

    it "distinguishes floor division from regular division" do
      source = "a / b // c"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      # Left-associative: (a / b) // c
      binary1 = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(binary1).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      CrystalV2::Compiler::Frontend.node_operator(binary1).not_nil!.should eq("//".to_slice)

      left = arena[CrystalV2::Compiler::Frontend.node_left(binary1).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(left).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      CrystalV2::Compiler::Frontend.node_operator(left).not_nil!.should eq("/".to_slice)
    end

    it "parses floor division with precedence" do
      source = "2 + 10 // 3"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      # Should parse as: 2 + (10 // 3) due to precedence
      binary = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(binary).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      CrystalV2::Compiler::Frontend.node_operator(binary).not_nil!.should eq("+".to_slice)

      right = arena[CrystalV2::Compiler::Frontend.node_right(binary).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(right).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      CrystalV2::Compiler::Frontend.node_operator(right).not_nil!.should eq("//".to_slice)
    end

    it "parses floor division as method argument" do
      source = "foo(10 // 3)"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      call = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(call).should eq(CrystalV2::Compiler::Frontend::NodeKind::Call)

      args = CrystalV2::Compiler::Frontend.node_args(call).not_nil!
      args.size.should eq(1)

      arg = arena[args[0]]
      CrystalV2::Compiler::Frontend.node_kind(arg).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      CrystalV2::Compiler::Frontend.node_operator(arg).not_nil!.should eq("//".to_slice)
    end

    it "parses floor division in array" do
      source = "[10 // 3, 20 // 4]"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      array = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(array).should eq(CrystalV2::Compiler::Frontend::NodeKind::ArrayLiteral)

      elements = CrystalV2::Compiler::Frontend.node_array_elements(array).not_nil!
      elements.size.should eq(2)

      elem1 = arena[elements[0]]
      CrystalV2::Compiler::Frontend.node_kind(elem1).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      CrystalV2::Compiler::Frontend.node_operator(elem1).not_nil!.should eq("//".to_slice)
    end

    it "parses chained floor division" do
      source = "100 // 10 // 2"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      # Left-associative: (100 // 10) // 2
      binary1 = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(binary1).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      CrystalV2::Compiler::Frontend.node_operator(binary1).not_nil!.should eq("//".to_slice)

      left = arena[CrystalV2::Compiler::Frontend.node_left(binary1).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(left).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      CrystalV2::Compiler::Frontend.node_operator(left).not_nil!.should eq("//".to_slice)
    end

    it "parses floor division with parentheses" do
      source = "(20 + 5) // 3"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      binary = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(binary).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      CrystalV2::Compiler::Frontend.node_operator(binary).not_nil!.should eq("//".to_slice)

      left = arena[CrystalV2::Compiler::Frontend.node_left(binary).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(left).should eq(CrystalV2::Compiler::Frontend::NodeKind::Grouping)
    end
  end
end
