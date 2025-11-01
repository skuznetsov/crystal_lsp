require "spec"

require "../src/compiler/frontend/parser"

describe "CrystalV2::Compiler::Frontend::Parser" do
  describe "Phase 89: Wrapping Arithmetic Operators" do
    # ============================================================
    # Binary Wrapping Operators
    # ============================================================

    it "parses wrapping addition (&+)" do
      source = "a &+ b"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      binary = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(binary).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      String.new(CrystalV2::Compiler::Frontend.node_operator(binary).not_nil!).should eq("&+")
    end

    it "parses wrapping subtraction (&-)" do
      source = "x &- y"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      binary = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(binary).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      String.new(CrystalV2::Compiler::Frontend.node_operator(binary).not_nil!).should eq("&-")
    end

    it "parses wrapping multiplication (&*)" do
      source = "m &* n"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      binary = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(binary).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      String.new(CrystalV2::Compiler::Frontend.node_operator(binary).not_nil!).should eq("&*")
    end

    it "parses wrapping exponentiation (&**)" do
      source = "base &** exp"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      binary = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(binary).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      String.new(CrystalV2::Compiler::Frontend.node_operator(binary).not_nil!).should eq("&**")
    end

    # ============================================================
    # Precedence Verification
    # ============================================================

    it "respects precedence: &+ same as + (lower than &*)" do
      source = "a &+ b &* c"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      # Should parse as: a &+ (b &* c)
      binary = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(binary).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      String.new(CrystalV2::Compiler::Frontend.node_operator(binary).not_nil!).should eq("&+")

      # Right side should be &*
      right = arena[CrystalV2::Compiler::Frontend.node_right(binary).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(right).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      String.new(CrystalV2::Compiler::Frontend.node_operator(right).not_nil!).should eq("&*")
    end

    it "respects precedence: &** highest (higher than &*)" do
      source = "a &* b &** c"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      # Should parse as: a &* (b &** c)
      binary = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(binary).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      String.new(CrystalV2::Compiler::Frontend.node_operator(binary).not_nil!).should eq("&*")

      # Right side should be &**
      right = arena[CrystalV2::Compiler::Frontend.node_right(binary).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(right).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      String.new(CrystalV2::Compiler::Frontend.node_operator(right).not_nil!).should eq("&**")
    end

    # ============================================================
    # Compound Assignment (Desugared)
    # ============================================================

    it "desugars wrapping addition assignment (&+=)" do
      source = "a &+= 5"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      # Should desugar to: a = a &+ 5
      assign = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(assign).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)

      # Value should be: a &+ 5
      value = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(value).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      String.new(CrystalV2::Compiler::Frontend.node_operator(value).not_nil!).should eq("&+")
    end

    it "desugars wrapping subtraction assignment (&-=)" do
      source = "x &-= 10"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(assign).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)

      value = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(value).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      String.new(CrystalV2::Compiler::Frontend.node_operator(value).not_nil!).should eq("&-")
    end

    it "desugars wrapping multiplication assignment (&*=)" do
      source = "m &*= 3"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(assign).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)

      value = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(value).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      String.new(CrystalV2::Compiler::Frontend.node_operator(value).not_nil!).should eq("&*")
    end

    it "desugars wrapping exponentiation assignment (&**=)" do
      source = "base &**= 2"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(assign).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)

      value = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(value).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      String.new(CrystalV2::Compiler::Frontend.node_operator(value).not_nil!).should eq("&**")
    end

    # ============================================================
    # Unary Forms
    # ============================================================

    it "parses unary wrapping plus (&+x)" do
      source = "&+value"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      unary = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(unary).should eq(CrystalV2::Compiler::Frontend::NodeKind::Unary)
      String.new(CrystalV2::Compiler::Frontend.node_operator(unary).not_nil!).should eq("&+")

      # Operand should be 'value'
      operand = arena[CrystalV2::Compiler::Frontend.node_right(unary).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(operand).should eq(CrystalV2::Compiler::Frontend::NodeKind::Identifier)
    end

    it "parses unary wrapping minus (&-x)" do
      source = "&-number"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      unary = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(unary).should eq(CrystalV2::Compiler::Frontend::NodeKind::Unary)
      String.new(CrystalV2::Compiler::Frontend.node_operator(unary).not_nil!).should eq("&-")

      operand = arena[CrystalV2::Compiler::Frontend.node_right(unary).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(operand).should eq(CrystalV2::Compiler::Frontend::NodeKind::Identifier)
    end

    # ============================================================
    # Disambiguation Tests (Critical)
    # ============================================================

    it "disambiguates &. (safe navigation) from &+ (wrapping add)" do
      source = "obj&.method"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      # Should parse as safe navigation, NOT &+ followed by .method
      node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(node).should eq(CrystalV2::Compiler::Frontend::NodeKind::SafeNavigation)
    end

    it "disambiguates && (logical and) from &+ (wrapping add)" do
      source = "a && b"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      # Should parse as logical &&, NOT & followed by &b
      binary = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(binary).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      String.new(CrystalV2::Compiler::Frontend.node_operator(binary).not_nil!).should eq("&&")
    end

    it "disambiguates &= (bitwise and assign) from &+ (wrapping add)" do
      source = "x &= mask"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      # Should parse as assignment with &= desugared to x = x & mask
      assign = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(assign).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)

      value = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(value).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      String.new(CrystalV2::Compiler::Frontend.node_operator(value).not_nil!).should eq("&")
    end

    # ============================================================
    # Complex Expression Tests
    # ============================================================

    it "handles mixed wrapping and non-wrapping operators" do
      source = "a + b &* c - d &+ e"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      # Should parse correctly with proper precedence
      root = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(root).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      # Complex structure test - just verify it parses without error
    end

    it "handles wrapping operators in method call arguments" do
      source = "foo(a &+ b, x &* y)"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      call = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(call).should eq(CrystalV2::Compiler::Frontend::NodeKind::Call)

      # Verify arguments contain wrapping operators
      args = CrystalV2::Compiler::Frontend.node_args(call).not_nil!
      args.size.should eq(2)

      arg1 = arena[args[0]]
      CrystalV2::Compiler::Frontend.node_kind(arg1).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      String.new(CrystalV2::Compiler::Frontend.node_operator(arg1).not_nil!).should eq("&+")

      arg2 = arena[args[1]]
      CrystalV2::Compiler::Frontend.node_kind(arg2).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      String.new(CrystalV2::Compiler::Frontend.node_operator(arg2).not_nil!).should eq("&*")
    end

    it "handles wrapping operators in parenthesized expressions" do
      source = "(a &+ b) &* (c &- d)"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      binary = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(binary).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      String.new(CrystalV2::Compiler::Frontend.node_operator(binary).not_nil!).should eq("&*")

      # Left should be grouping with &+
      left = arena[CrystalV2::Compiler::Frontend.node_left(binary).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(left).should eq(CrystalV2::Compiler::Frontend::NodeKind::Grouping)

      # Right should be grouping with &-
      right = arena[CrystalV2::Compiler::Frontend.node_right(binary).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(right).should eq(CrystalV2::Compiler::Frontend::NodeKind::Grouping)
    end
  end
end
