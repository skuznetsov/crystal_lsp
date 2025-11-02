require "spec"

require "../../src/compiler/frontend/parser"

describe "CrystalV2::Compiler::Frontend::Parser" do
  describe "Phase 62: Octal escape sequences (PRODUCTION-READY)" do
    # String literal tests

    it "parses \\NNN octal in string (single digit)" do
      source = "s = \"\\7\""  # bell (7)

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      string_node = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(string_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::String)
      bytes = CrystalV2::Compiler::Frontend.node_literal(string_node).not_nil!
      bytes.size.should eq(1)
      bytes[0].should eq(7_u8)
    end

    it "parses \\NNN octal in string (two digits)" do
      source = "s = \"\\101\""  # 'A' (65)

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      string_node = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(string_node).not_nil!).should eq("A")
    end

    it "parses \\NNN octal in string (three digits)" do
      source = "s = \"\\102\""  # 'B' (66)

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      string_node = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(string_node).not_nil!).should eq("B")
    end

    it "parses \\0 as octal null byte in string" do
      source = "s = \"\\0\""  # null (0)

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      string_node = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(string_node).not_nil!).should eq("\0")
    end

    it "parses \\000 as octal null byte in string" do
      source = "s = \"\\000\""  # null (0)

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      string_node = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(string_node).not_nil!).should eq("\0")
    end

    it "parses \\012 as octal newline in string" do
      source = "s = \"\\012\""  # newline (10)

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      string_node = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(string_node).not_nil!).should eq("\n")
    end

    it "parses \\177 as octal DEL character in string" do
      source = "s = \"\\177\""  # DEL (127)

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      string_node = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      bytes = CrystalV2::Compiler::Frontend.node_literal(string_node).not_nil!
      bytes.size.should eq(1)
      bytes[0].should eq(127_u8)
    end

    it "parses multiple \\NNN octal escapes in string" do
      source = "s = \"\\101\\102\\103\""  # "ABC" (65, 66, 67)

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      string_node = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(string_node).not_nil!).should eq("ABC")
    end

    it "parses mixed octal and regular text in string" do
      source = "s = \"Hello\\040World\""  # "Hello World" (040 = space = 32)

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      string_node = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(string_node).not_nil!).should eq("Hello World")
    end

    it "parses mixed octal and other escapes in string" do
      source = "s = \"\\101\\nB\""  # "A\\nB" (101 = 'A' = 65)

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      string_node = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(string_node).not_nil!).should eq("A\nB")
    end

    it "parses octal with 1, 2, and 3 digits in same string" do
      source = "s = \"\\7\\40\\101\""  # bell, space, 'A' (7, 32, 65)

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      string_node = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      bytes = CrystalV2::Compiler::Frontend.node_literal(string_node).not_nil!
      bytes.size.should eq(3)
      bytes[0].should eq(7_u8)
      bytes[1].should eq(32_u8)
      bytes[2].should eq(65_u8)
    end

    it "stops parsing at non-octal digit" do
      source = "s = \"\\78\""  # \7 followed by '8' (7 and 56)

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      string_node = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      bytes = CrystalV2::Compiler::Frontend.node_literal(string_node).not_nil!
      bytes.size.should eq(2)
      bytes[0].should eq(7_u8)   # \7
      bytes[1].should eq(56_u8)  # '8'
    end

    # Character literal tests

    it "parses \\NNN octal in character (single digit)" do
      source = "c = '\\7'"  # bell (7)

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      char_node = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(char_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Char)
      bytes = CrystalV2::Compiler::Frontend.node_literal(char_node).not_nil!
      bytes.size.should eq(1)
      bytes[0].should eq(7_u8)
    end

    it "parses \\NNN octal in character (three digits)" do
      source = "c = '\\101'"  # 'A' (65)

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      char_node = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(char_node).not_nil!).should eq("A")
    end

    it "parses \\0 as octal null byte in character" do
      source = "c = '\\0'"  # null (0)

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      char_node = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(char_node).not_nil!).should eq("\0")
    end

    it "parses \\012 as octal newline in character" do
      source = "c = '\\012'"  # newline (10)

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      char_node = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(char_node).not_nil!).should eq("\n")
    end

    it "parses \\177 as octal DEL character in character literal" do
      source = "c = '\\177'"  # DEL (127)

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      char_node = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      bytes = CrystalV2::Compiler::Frontend.node_literal(char_node).not_nil!
      bytes.size.should eq(1)
      bytes[0].should eq(127_u8)
    end

    # Integration tests

    it "parses octal escapes in array" do
      source = "[\"\\101\", \"\\102\"]"  # ["A", "B"]

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      array = arena[program.roots[0]]
      elements = CrystalV2::Compiler::Frontend.node_array_elements(array).not_nil!
      elements.size.should eq(2)

      str1 = arena[elements[0]]
      String.new(CrystalV2::Compiler::Frontend.node_literal(str1).not_nil!).should eq("A")

      str2 = arena[elements[1]]
      String.new(CrystalV2::Compiler::Frontend.node_literal(str2).not_nil!).should eq("B")
    end

    it "parses octal escapes in method call" do
      source = "puts(\"\\110ello\")"  # puts("Hello") (110 = 'H' = 72)

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      call = arena[program.roots[0]]
      args = CrystalV2::Compiler::Frontend.node_args(call).not_nil!
      args.size.should eq(1)

      string_node = arena[args[0]]
      String.new(CrystalV2::Compiler::Frontend.node_literal(string_node).not_nil!).should eq("Hello")
    end

    it "parses multiple statements with octal escapes" do
      source = <<-CRYSTAL
      a = "\\110ello"
      b = '\\101'
      c = "\\061\\062\\063"
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(3)
      arena = program.arena

      # First: "Hello" (110 = 'H' = 72)
      assign1 = arena[program.roots[0]]
      str1 = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign1).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(str1).not_nil!).should eq("Hello")

      # Second: 'A' (101 = 65)
      assign2 = arena[program.roots[1]]
      char2 = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign2).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(char2).not_nil!).should eq("A")

      # Third: "123" (061='1'=49, 062='2'=50, 063='3'=51)
      assign3 = arena[program.roots[2]]
      str3 = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign3).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(str3).not_nil!).should eq("123")
    end

    it "parses mixed hex and octal escapes" do
      source = "s = \"\\x41\\101\""  # "AA" (hex 41 = 65, octal 101 = 65)

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      string_node = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(string_node).not_nil!).should eq("AA")
    end
  end
end
