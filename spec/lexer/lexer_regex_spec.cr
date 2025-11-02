require "spec"

require "../../src/compiler/frontend/parser"

describe "CrystalV2::Compiler::Frontend::Parser" do
  describe "Phase 57: Regex literals (PRODUCTION-READY)" do
    it "parses simple regex /abc/" do
      source = "r = /abc/"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(assign).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)

      # Value is regex literal
      regex = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(regex).should eq(CrystalV2::Compiler::Frontend::NodeKind::Regex)
      CrystalV2::Compiler::Frontend.node_literal(regex).not_nil!.should eq("abc".to_slice)
    end

    it "parses regex with digits /\\d+/" do
      source = "r = /\\d+/"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      regex = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(regex).should eq(CrystalV2::Compiler::Frontend::NodeKind::Regex)
      CrystalV2::Compiler::Frontend.node_literal(regex).not_nil!.should eq("\\d+".to_slice)
    end

    it "parses regex with word boundary /\\btest\\b/" do
      source = "r = /\\btest\\b/"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      regex = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      CrystalV2::Compiler::Frontend.node_literal(regex).not_nil!.should eq("\\btest\\b".to_slice)
    end

    it "parses regex with escaped slash /a\\/b/" do
      source = "r = /a\\/b/"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      regex = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      CrystalV2::Compiler::Frontend.node_literal(regex).not_nil!.should eq("a\\/b".to_slice)
    end

    it "parses regex with i flag /test/i" do
      source = "r = /test/i"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      regex = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      CrystalV2::Compiler::Frontend.node_literal(regex).not_nil!.should eq("test/i".to_slice)
    end

    it "parses regex with multiple flags /abc/im" do
      source = "r = /abc/im"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      regex = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      CrystalV2::Compiler::Frontend.node_literal(regex).not_nil!.should eq("abc/im".to_slice)
    end

    it "parses regex with m and x flags /pattern/mx" do
      source = "r = /pattern/mx"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      regex = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      CrystalV2::Compiler::Frontend.node_literal(regex).not_nil!.should eq("pattern/mx".to_slice)
    end

    it "parses regex in array" do
      source = "[/abc/, /def/]"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      array = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(array).should eq(CrystalV2::Compiler::Frontend::NodeKind::ArrayLiteral)

      elements = CrystalV2::Compiler::Frontend.node_array_elements(array).not_nil!
      elements.size.should eq(2)

      regex1 = arena[elements[0]]
      CrystalV2::Compiler::Frontend.node_kind(regex1).should eq(CrystalV2::Compiler::Frontend::NodeKind::Regex)
      CrystalV2::Compiler::Frontend.node_literal(regex1).not_nil!.should eq("abc".to_slice)

      regex2 = arena[elements[1]]
      CrystalV2::Compiler::Frontend.node_kind(regex2).should eq(CrystalV2::Compiler::Frontend::NodeKind::Regex)
      CrystalV2::Compiler::Frontend.node_literal(regex2).not_nil!.should eq("def".to_slice)
    end

    it "parses regex in method call" do
      source = "match(/pattern/)"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      call = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(call).should eq(CrystalV2::Compiler::Frontend::NodeKind::Call)

      args = CrystalV2::Compiler::Frontend.node_args(call).not_nil!
      args.size.should eq(1)

      regex = arena[args[0]]
      CrystalV2::Compiler::Frontend.node_kind(regex).should eq(CrystalV2::Compiler::Frontend::NodeKind::Regex)
      CrystalV2::Compiler::Frontend.node_literal(regex).not_nil!.should eq("pattern".to_slice)
    end

    it "parses regex with character classes /[a-z]+/" do
      source = "r = /[a-z]+/"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      regex = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      CrystalV2::Compiler::Frontend.node_literal(regex).not_nil!.should eq("[a-z]+".to_slice)
    end

    it "parses regex with groups /(foo|bar)/" do
      source = "r = /(foo|bar)/"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      regex = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      CrystalV2::Compiler::Frontend.node_literal(regex).not_nil!.should eq("(foo|bar)".to_slice)
    end

    it "parses multiple regex with different patterns" do
      source = <<-CRYSTAL
      a = /abc/
      b = /\\d+/
      c = /test/i
      d = /[a-z]/m
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(4)
      arena = program.arena

      # Check all four regex literals
      literals = ["abc", "\\d+", "test/i", "[a-z]/m"]
      (0..3).each do |i|
        assign = arena[program.roots[i]]
        regex = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
        CrystalV2::Compiler::Frontend.node_kind(regex).should eq(CrystalV2::Compiler::Frontend::NodeKind::Regex)
        CrystalV2::Compiler::Frontend.node_literal(regex).not_nil!.should eq(literals[i].to_slice)
      end
    end

    it "distinguishes division from regex after number" do
      source = "result = 10 / 2"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      binary = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(binary).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      CrystalV2::Compiler::Frontend.node_operator(binary).not_nil!.should eq("/".to_slice)
    end

    it "distinguishes division from regex after identifier" do
      source = "result = x / y"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      binary = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(binary).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      CrystalV2::Compiler::Frontend.node_operator(binary).not_nil!.should eq("/".to_slice)
    end

    it "parses regex after comma in array" do
      source = "[1, /test/]"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      array = arena[program.roots[0]]
      elements = CrystalV2::Compiler::Frontend.node_array_elements(array).not_nil!
      elements.size.should eq(2)

      # First element is number
      num = arena[elements[0]]
      CrystalV2::Compiler::Frontend.node_kind(num).should eq(CrystalV2::Compiler::Frontend::NodeKind::Number)

      # Second element is regex
      regex = arena[elements[1]]
      CrystalV2::Compiler::Frontend.node_kind(regex).should eq(CrystalV2::Compiler::Frontend::NodeKind::Regex)
    end

    it "parses regex with backslash escapes /\\n\\t/" do
      source = "r = /\\n\\t/"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      regex = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      # Escapes are preserved for regex engine
      CrystalV2::Compiler::Frontend.node_literal(regex).not_nil!.should eq("\\n\\t".to_slice)
    end

    it "parses empty regex //" do
      source = "r = //"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      regex = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(regex).should eq(CrystalV2::Compiler::Frontend::NodeKind::Regex)
      CrystalV2::Compiler::Frontend.node_literal(regex).not_nil!.should eq("".to_slice)
    end
  end
end
