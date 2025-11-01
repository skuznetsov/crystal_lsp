require "spec"

require "../src/compiler/frontend/parser"

describe "CrystalV2::Compiler::Frontend::Parser" do
  describe "Phase 56: Character literals (PRODUCTION-READY)" do
    it "parses simple character 'a'" do
      source = "c = 'a'"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(assign).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)

      # Value is character literal
      char = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(char).should eq(CrystalV2::Compiler::Frontend::NodeKind::Char)
      String.new(CrystalV2::Compiler::Frontend.node_literal(char).not_nil!).should eq("a")
    end

    it "parses character 'b'" do
      source = "c = 'b'"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      char = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(char).not_nil!).should eq("b")
    end

    it "parses character with \\n newline escape" do
      source = "c = '\\n'"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      char = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(char).should eq(CrystalV2::Compiler::Frontend::NodeKind::Char)
      String.new(CrystalV2::Compiler::Frontend.node_literal(char).not_nil!).should eq("\n")
    end

    it "parses character with \\t tab escape" do
      source = "c = '\\t'"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      char = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(char).not_nil!).should eq("\t")
    end

    it "parses character with \\r carriage return escape" do
      source = "c = '\\r'"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      char = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(char).not_nil!).should eq("\r")
    end

    it "parses character with \\\\ backslash escape" do
      source = "c = '\\\\'"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      char = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(char).not_nil!).should eq("\\")
    end

    it "parses character with \\' quote escape" do
      source = "c = '\\''"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      char = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(char).not_nil!).should eq("'")
    end

    it "parses character with \\0 null escape" do
      source = "c = '\\0'"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      char = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(char).not_nil!).should eq("\0")
    end

    it "parses uppercase character 'A'" do
      source = "c = 'A'"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      char = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(char).not_nil!).should eq("A")
    end

    it "parses digit character '5'" do
      source = "c = '5'"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      char = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(char).not_nil!).should eq("5")
    end

    it "parses special character '@'" do
      source = "c = '@'"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      char = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(char).not_nil!).should eq("@")
    end

    it "parses space character ' '" do
      source = "c = ' '"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      char = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(char).not_nil!).should eq(" ")
    end

    it "parses characters in array" do
      source = "['a', 'b', 'c']"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      array = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(array).should eq(CrystalV2::Compiler::Frontend::NodeKind::ArrayLiteral)

      elements = CrystalV2::Compiler::Frontend.node_array_elements(array).not_nil!
      elements.size.should eq(3)

      # Check all three are characters
      char1 = arena[elements[0]]
      CrystalV2::Compiler::Frontend.node_kind(char1).should eq(CrystalV2::Compiler::Frontend::NodeKind::Char)
      String.new(CrystalV2::Compiler::Frontend.node_literal(char1).not_nil!).should eq("a")

      char2 = arena[elements[1]]
      String.new(CrystalV2::Compiler::Frontend.node_literal(char2).not_nil!).should eq("b")

      char3 = arena[elements[2]]
      String.new(CrystalV2::Compiler::Frontend.node_literal(char3).not_nil!).should eq("c")
    end

    it "parses character in method call" do
      source = "puts('x')"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      call = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(call).should eq(CrystalV2::Compiler::Frontend::NodeKind::Call)

      args = CrystalV2::Compiler::Frontend.node_args(call).not_nil!
      args.size.should eq(1)

      char = arena[args[0]]
      CrystalV2::Compiler::Frontend.node_kind(char).should eq(CrystalV2::Compiler::Frontend::NodeKind::Char)
      String.new(CrystalV2::Compiler::Frontend.node_literal(char).not_nil!).should eq("x")
    end

    it "parses multiple characters with different escapes" do
      source = <<-CRYSTAL
      a = 'a'
      b = '\\n'
      c = '\\\\'
      d = '\\t'
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(4)
      arena = program.arena

      # Check all four assignments
      literals = ["a", "\n", "\\", "\t"]
      (0..3).each do |i|
        assign = arena[program.roots[i]]
        char = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
        CrystalV2::Compiler::Frontend.node_kind(char).should eq(CrystalV2::Compiler::Frontend::NodeKind::Char)
        String.new(CrystalV2::Compiler::Frontend.node_literal(char).not_nil!).should eq(literals[i])
      end
    end

    it "parses character in binary expression" do
      source = "result = 'a' == 'b'"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      binary = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(binary).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)

      left = arena[CrystalV2::Compiler::Frontend.node_left(binary).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(left).should eq(CrystalV2::Compiler::Frontend::NodeKind::Char)
      String.new(CrystalV2::Compiler::Frontend.node_literal(left).not_nil!).should eq("a")

      right = arena[CrystalV2::Compiler::Frontend.node_right(binary).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(right).should eq(CrystalV2::Compiler::Frontend::NodeKind::Char)
      String.new(CrystalV2::Compiler::Frontend.node_literal(right).not_nil!).should eq("b")
    end

    it "parses all supported escape sequences" do
      source = <<-CRYSTAL
      a = '\\n'
      b = '\\t'
      c = '\\r'
      d = '\\\\'
      e = '\\''
      f = '\\0'
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(6)
      arena = program.arena

      # Check all six escape sequences
      literals = ["\n", "\t", "\r", "\\", "'", "\0"]
      (0..5).each do |i|
        assign = arena[program.roots[i]]
        char = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
        String.new(CrystalV2::Compiler::Frontend.node_literal(char).not_nil!).should eq(literals[i])
      end
    end
  end
end
