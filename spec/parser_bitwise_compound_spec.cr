require "spec"

require "../src/compiler/frontend/parser"

describe "CrystalV2::Compiler::Frontend::Parser" do
  describe "Phase 52: &=, |=, ^=, <<=, >>= bitwise compound assignment (PRODUCTION-READY)" do
    it "parses simple &= assignment" do
      source = <<-CRYSTAL
      flags &= mask
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      # Should desugar to: flags = flags & mask
      assign_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(assign_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)

      # Value is binary expression: flags & mask
      binary_node = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(binary_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      String.new(CrystalV2::Compiler::Frontend.node_operator(binary_node).not_nil!).should eq("&")

      # Left side is 'flags'
      left = arena[CrystalV2::Compiler::Frontend.node_left(binary_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(left).should eq(CrystalV2::Compiler::Frontend::NodeKind::Identifier)
      String.new(CrystalV2::Compiler::Frontend.node_literal(left).not_nil!).should eq("flags")

      # Right side is 'mask'
      right = arena[CrystalV2::Compiler::Frontend.node_right(binary_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(right).should eq(CrystalV2::Compiler::Frontend::NodeKind::Identifier)
      String.new(CrystalV2::Compiler::Frontend.node_literal(right).not_nil!).should eq("mask")
    end

    it "parses simple |= assignment" do
      source = <<-CRYSTAL
      bits |= value
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign_node = arena[program.roots[0]]
      binary_node = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_operator(binary_node).not_nil!).should eq("|")
    end

    it "parses simple ^= assignment" do
      source = <<-CRYSTAL
      state ^= toggle
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign_node = arena[program.roots[0]]
      binary_node = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_operator(binary_node).not_nil!).should eq("^")
    end

    it "parses simple <<= assignment" do
      source = <<-CRYSTAL
      num <<= shift
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign_node = arena[program.roots[0]]
      binary_node = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_operator(binary_node).not_nil!).should eq("<<")
    end

    it "parses simple >>= assignment" do
      source = <<-CRYSTAL
      value >>= count
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign_node = arena[program.roots[0]]
      binary_node = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_operator(binary_node).not_nil!).should eq(">>")
    end

    it "parses &= with number literal" do
      source = <<-CRYSTAL
      flags &= 255
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign_node = arena[program.roots[0]]
      binary_node = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_operator(binary_node).not_nil!).should eq("&")

      # Right side is number
      right = arena[CrystalV2::Compiler::Frontend.node_right(binary_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(right).should eq(CrystalV2::Compiler::Frontend::NodeKind::Number)
    end

    it "parses <<= with constant" do
      source = <<-CRYSTAL
      result <<= SHIFT_AMOUNT
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign_node = arena[program.roots[0]]
      binary_node = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_operator(binary_node).not_nil!).should eq("<<")

      # Right side is identifier (constant)
      right = arena[CrystalV2::Compiler::Frontend.node_right(binary_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(right).should eq(CrystalV2::Compiler::Frontend::NodeKind::Identifier)
    end

    it "parses multiple bitwise compound assignments" do
      source = <<-CRYSTAL
      a &= 1
      b |= 2
      c ^= 3
      d <<= 4
      e >>= 5
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(5)
      arena = program.arena

      # Check operators in order
      operators = ["&", "|", "^", "<<", ">>"]
      (0..4).each do |i|
        assign = arena[program.roots[i]]
        CrystalV2::Compiler::Frontend.node_kind(assign).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)

        binary = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
        String.new(CrystalV2::Compiler::Frontend.node_operator(binary).not_nil!).should eq(operators[i])
      end
    end

    it "parses bitwise compound in method definition" do
      source = <<-CRYSTAL
      def apply_mask
        @flags &= 15
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      method_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(method_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Def)

      def_body = CrystalV2::Compiler::Frontend.node_def_body(method_node).not_nil!
      def_body.size.should eq(1)
      assign = arena[def_body[0]]
      CrystalV2::Compiler::Frontend.node_kind(assign).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)

      binary = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_operator(binary).not_nil!).should eq("&")
    end

    it "parses bitwise compound in class" do
      source = <<-CRYSTAL
      class Flags
        def shift_left
          @value <<= 1
        end
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      class_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(class_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Class)

      class_body = CrystalV2::Compiler::Frontend.node_class_body(class_node).not_nil!
      class_body.size.should eq(1)
      method = arena[class_body[0]]
      CrystalV2::Compiler::Frontend.node_kind(method).should eq(CrystalV2::Compiler::Frontend::NodeKind::Def)

      method_def_body = CrystalV2::Compiler::Frontend.node_def_body(method).not_nil!
      method_def_body.size.should eq(1)
      assign = arena[method_def_body[0]]

      binary = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_operator(binary).not_nil!).should eq("<<")
    end

    it "correctly distinguishes &= from &&= and &" do
      source = <<-CRYSTAL
      a = x && y
      b &&= z
      c &= mask
      d = e & f
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(4)
      arena = program.arena

      # First: x && y (logical and)
      assign1 = arena[program.roots[0]]
      binary1 = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign1).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_operator(binary1).not_nil!).should eq("&&")

      # Second: b &&= z (logical and assign)
      assign2 = arena[program.roots[1]]
      binary2 = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign2).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_operator(binary2).not_nil!).should eq("&&")

      # Third: c &= mask (bitwise and assign)
      assign3 = arena[program.roots[2]]
      binary3 = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign3).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_operator(binary3).not_nil!).should eq("&")

      # Fourth: e & f (bitwise and)
      assign4 = arena[program.roots[3]]
      binary4 = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign4).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_operator(binary4).not_nil!).should eq("&")
    end

    it "correctly distinguishes |= from ||= and |" do
      source = <<-CRYSTAL
      a = x || y
      b ||= z
      c |= bits
      d = e | f
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(4)
      arena = program.arena

      # Operators in order: ||, ||, |, |
      operators = ["||", "||", "|", "|"]
      (0..3).each do |i|
        assign = arena[program.roots[i]]
        binary = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
        String.new(CrystalV2::Compiler::Frontend.node_operator(binary).not_nil!).should eq(operators[i])
      end
    end

    it "correctly distinguishes <<= from << and <" do
      source = <<-CRYSTAL
      a = x << y
      b <<= shift
      c = d < e
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(3)
      arena = program.arena

      # First: x << y (left shift)
      assign1 = arena[program.roots[0]]
      binary1 = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign1).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_operator(binary1).not_nil!).should eq("<<")

      # Second: b <<= shift (left shift assign)
      assign2 = arena[program.roots[1]]
      binary2 = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign2).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_operator(binary2).not_nil!).should eq("<<")

      # Third: d < e (less than)
      assign3 = arena[program.roots[2]]
      binary3 = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign3).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_operator(binary3).not_nil!).should eq("<")
    end

    it "correctly distinguishes >>= from >> and >" do
      source = <<-CRYSTAL
      a = x >> y
      b >>= count
      c = d > e
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(3)
      arena = program.arena

      # First: x >> y (right shift)
      assign1 = arena[program.roots[0]]
      binary1 = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign1).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_operator(binary1).not_nil!).should eq(">>")

      # Second: b >>= count (right shift assign)
      assign2 = arena[program.roots[1]]
      binary2 = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign2).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_operator(binary2).not_nil!).should eq(">>")

      # Third: d > e (greater than)
      assign3 = arena[program.roots[2]]
      binary3 = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign3).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_operator(binary3).not_nil!).should eq(">")
    end

    it "parses all compound operators together" do
      source = <<-CRYSTAL
      a += 1
      b &= mask
      c |= flag
      d ^= toggle
      e <<= 2
      f >>= 1
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(6)
      arena = program.arena

      # All should be assignments with correct operators
      operators = ["+", "&", "|", "^", "<<", ">>"]
      (0..5).each do |i|
        assign = arena[program.roots[i]]
        CrystalV2::Compiler::Frontend.node_kind(assign).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)

        binary = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
        String.new(CrystalV2::Compiler::Frontend.node_operator(binary).not_nil!).should eq(operators[i])
      end
    end
  end
end
