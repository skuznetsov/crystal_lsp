require "spec"

require "../src/compiler/frontend/parser"

describe "CrystalV2::Compiler::Frontend::Parser" do
  describe "Phase 55: Numeric separators (underscores in numbers) (PRODUCTION-READY)" do
    it "parses decimal with thousand separator" do
      source = "x = 1_000"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(assign).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)

      # Value is number with separator
      number = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(number).should eq(CrystalV2::Compiler::Frontend::NodeKind::Number)
      String.new(CrystalV2::Compiler::Frontend.node_literal(number).not_nil!).should eq("1_000")
      CrystalV2::Compiler::Frontend.node_number_kind(number).should eq(CrystalV2::Compiler::Frontend::NumberKind::I32)
    end

    it "parses decimal with million separator" do
      source = "x = 1_000_000"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      number = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(number).not_nil!).should eq("1_000_000")
    end

    it "parses hex with separators" do
      source = "x = 0xFF_FF"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      number = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(number).not_nil!).should eq("0xFF_FF")
    end

    it "parses hex with multiple separators" do
      source = "x = 0x1A_2B_3C_4D"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      number = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(number).not_nil!).should eq("0x1A_2B_3C_4D")
    end

    it "parses binary with separators" do
      source = "x = 0b1111_0000"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      number = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(number).not_nil!).should eq("0b1111_0000")
    end

    it "parses binary with byte separators" do
      source = "x = 0b1010_1010_0101_0101"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      number = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(number).not_nil!).should eq("0b1010_1010_0101_0101")
    end

    it "parses octal with separators" do
      source = "x = 0o777_666"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      number = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(number).not_nil!).should eq("0o777_666")
    end

    it "parses float with separators in integer part" do
      source = "x = 1_234.56"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      number = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(number).not_nil!).should eq("1_234.56")
      CrystalV2::Compiler::Frontend.node_number_kind(number).should eq(CrystalV2::Compiler::Frontend::NumberKind::F64)
    end

    it "parses float with separators in fractional part" do
      source = "x = 3.14_159_265"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      number = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(number).not_nil!).should eq("3.14_159_265")
      CrystalV2::Compiler::Frontend.node_number_kind(number).should eq(CrystalV2::Compiler::Frontend::NumberKind::F64)
    end

    it "parses float with separators in both parts" do
      source = "x = 1_234.567_890"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      number = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(number).not_nil!).should eq("1_234.567_890")
    end

    it "parses number with suffix and separators" do
      source = "x = 1_000_000_i64"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      number = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(number).not_nil!).should eq("1_000_000_i64")
      CrystalV2::Compiler::Frontend.node_number_kind(number).should eq(CrystalV2::Compiler::Frontend::NumberKind::I64)
    end

    it "parses hex with suffix and separators" do
      source = "x = 0xDEAD_BEEF_i64"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      number = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(number).not_nil!).should eq("0xDEAD_BEEF_i64")
      CrystalV2::Compiler::Frontend.node_number_kind(number).should eq(CrystalV2::Compiler::Frontend::NumberKind::I64)
    end

    it "parses numbers with separators in array" do
      source = "[1_000, 2_000, 3_000]"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      array = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(array).should eq(CrystalV2::Compiler::Frontend::NodeKind::ArrayLiteral)

      elements = CrystalV2::Compiler::Frontend.node_array_elements(array).not_nil!
      elements.size.should eq(3)

      # Check all three have separators
      num1 = arena[elements[0]]
      String.new(CrystalV2::Compiler::Frontend.node_literal(num1).not_nil!).should eq("1_000")

      num2 = arena[elements[1]]
      String.new(CrystalV2::Compiler::Frontend.node_literal(num2).not_nil!).should eq("2_000")

      num3 = arena[elements[2]]
      String.new(CrystalV2::Compiler::Frontend.node_literal(num3).not_nil!).should eq("3_000")
    end

    it "parses number with separators in method call" do
      source = "sleep(1_000)"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      call = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(call).should eq(CrystalV2::Compiler::Frontend::NodeKind::Call)

      args = CrystalV2::Compiler::Frontend.node_args(call).not_nil!
      args.size.should eq(1)

      number = arena[args[0]]
      String.new(CrystalV2::Compiler::Frontend.node_literal(number).not_nil!).should eq("1_000")
    end

    it "parses multiple numbers with different separators" do
      source = <<-CRYSTAL
      a = 1_000
      b = 0xFF_00
      c = 0b1010_0101
      d = 3.14_159
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(4)
      arena = program.arena

      # Check all four have separators
      literals = ["1_000", "0xFF_00", "0b1010_0101", "3.14_159"]
      (0..3).each do |i|
        assign = arena[program.roots[i]]
        number = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
        String.new(CrystalV2::Compiler::Frontend.node_literal(number).not_nil!).should eq(literals[i])
      end
    end

    it "parses number without separator (backward compatibility)" do
      source = "x = 1000"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      number = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(number).not_nil!).should eq("1000")
    end

    it "parses binary expression with separators" do
      source = "x = 1_000 + 2_000"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      binary = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(binary).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)

      left = arena[CrystalV2::Compiler::Frontend.node_left(binary).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(left).not_nil!).should eq("1_000")

      right = arena[CrystalV2::Compiler::Frontend.node_right(binary).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(right).not_nil!).should eq("2_000")
    end
  end
end
