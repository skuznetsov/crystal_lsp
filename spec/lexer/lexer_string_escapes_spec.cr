require "spec"

require "../../src/compiler/frontend/parser"

describe "CrystalV2::Compiler::Frontend::Parser" do
  describe "Phase 54: String escape sequences (PRODUCTION-READY)" do
    it "parses string with \\n newline escape" do
      source = <<-CRYSTAL
      x = "hello\\nworld"
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(assign).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)

      # Value is string with processed escape
      string = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(string).should eq(CrystalV2::Compiler::Frontend::NodeKind::String)
      String.new(CrystalV2::Compiler::Frontend.node_literal(string).not_nil!).should eq("hello\nworld")
    end

    it "parses string with \\t tab escape" do
      source = <<-CRYSTAL
      x = "name\\tvalue"
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      string = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(string).not_nil!).should eq("name\tvalue")
    end

    it "parses string with \\r carriage return escape" do
      source = <<-CRYSTAL
      x = "line1\\rline2"
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      string = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(string).not_nil!).should eq("line1\rline2")
    end

    it "parses string with \\\\ backslash escape" do
      source = <<-CRYSTAL
      x = "path\\\\to\\\\file"
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      string = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(string).not_nil!).should eq("path\\to\\file")
    end

    it "parses string with \\\" quote escape" do
      source = <<-CRYSTAL
      x = "He said \\"Hello\\""
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      string = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(string).not_nil!).should eq("He said \"Hello\"")
    end

    it "parses string with \\0 null escape" do
      source = <<-CRYSTAL
      x = "before\\0after"
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      string = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(string).not_nil!).should eq("before\0after")
    end

    it "parses string with multiple escape sequences" do
      source = <<-CRYSTAL
      x = "line1\\nline2\\tindented\\r\\n"
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      string = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(string).not_nil!).should eq("line1\nline2\tindented\r\n")
    end

    it "parses string without escapes (fast path)" do
      source = <<-CRYSTAL
      x = "simple string"
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      string = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(string).not_nil!).should eq("simple string")
    end

    it "parses string with unknown escape (keeps as is)" do
      source = <<-CRYSTAL
      x = "test\\xunknown"
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      string = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      # Unknown escape \x should be kept as \x
      String.new(CrystalV2::Compiler::Frontend.node_literal(string).not_nil!).should eq("test\\xunknown")
    end

    it "parses escaped strings in array" do
      source = <<-CRYSTAL
      arr = ["line1\\nline2", "tab\\there"]
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      array = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(array).should eq(CrystalV2::Compiler::Frontend::NodeKind::ArrayLiteral)

      elements = CrystalV2::Compiler::Frontend.node_array_elements(array).not_nil!
      elements.size.should eq(2)

      str1 = arena[elements[0]]
      String.new(CrystalV2::Compiler::Frontend.node_literal(str1).not_nil!).should eq("line1\nline2")

      str2 = arena[elements[1]]
      String.new(CrystalV2::Compiler::Frontend.node_literal(str2).not_nil!).should eq("tab\there")
    end

    it "parses escaped string in method call" do
      source = <<-CRYSTAL
      puts("hello\\nworld")
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      call = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(call).should eq(CrystalV2::Compiler::Frontend::NodeKind::Call)

      args = CrystalV2::Compiler::Frontend.node_args(call).not_nil!
      args.size.should eq(1)

      string = arena[args[0]]
      CrystalV2::Compiler::Frontend.node_kind(string).should eq(CrystalV2::Compiler::Frontend::NodeKind::String)
      String.new(CrystalV2::Compiler::Frontend.node_literal(string).not_nil!).should eq("hello\nworld")
    end

    it "parses all supported escapes together" do
      source = <<-CRYSTAL
      x = "\\n\\t\\r\\\\\\\"\\0"
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      string = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(string).not_nil!).should eq("\n\t\r\\\"\0")
    end

    it "parses empty string" do
      source = <<-CRYSTAL
      x = ""
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      string = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(string).not_nil!).should eq("")
    end

    it "parses string with escape at start" do
      source = <<-CRYSTAL
      x = "\\nstart"
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      string = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(string).not_nil!).should eq("\nstart")
    end

    it "parses string with escape at end" do
      source = <<-CRYSTAL
      x = "end\\n"
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      string = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(string).not_nil!).should eq("end\n")
    end

    it "parses multiple strings with different escapes" do
      source = <<-CRYSTAL
      a = "first\\n"
      b = "second\\t"
      c = "third\\\\"
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(3)
      arena = program.arena

      # Check all three assignments
      assign1 = arena[program.roots[0]]
      str1 = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign1).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(str1).not_nil!).should eq("first\n")

      assign2 = arena[program.roots[1]]
      str2 = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign2).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(str2).not_nil!).should eq("second\t")

      assign3 = arena[program.roots[2]]
      str3 = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign3).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(str3).not_nil!).should eq("third\\")
    end
  end
end
