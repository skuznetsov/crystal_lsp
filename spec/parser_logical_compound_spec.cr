require "spec"

require "../src/compiler/frontend/parser"

describe "CrystalV2::Compiler::Frontend::Parser" do
  describe "Phase 51: ||= and &&= logical compound assignment (PRODUCTION-READY)" do
    it "parses simple ||= assignment" do
      source = <<-CRYSTAL
      a ||= b
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      # Should desugar to: a = a || b
      assign_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(assign_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)

      # Target is identifier 'a'
      target_node = arena[CrystalV2::Compiler::Frontend.node_assign_target(assign_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(target_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Identifier)
      String.new(CrystalV2::Compiler::Frontend.node_literal(target_node).not_nil!).should eq("a")

      # Value is binary expression: a || b
      binary_node = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(binary_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      String.new(CrystalV2::Compiler::Frontend.node_operator(binary_node).not_nil!).should eq("||")

      # Left side of || is 'a'
      left = arena[CrystalV2::Compiler::Frontend.node_left(binary_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(left).should eq(CrystalV2::Compiler::Frontend::NodeKind::Identifier)
      String.new(CrystalV2::Compiler::Frontend.node_literal(left).not_nil!).should eq("a")

      # Right side of || is 'b'
      right = arena[CrystalV2::Compiler::Frontend.node_right(binary_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(right).should eq(CrystalV2::Compiler::Frontend::NodeKind::Identifier)
      String.new(CrystalV2::Compiler::Frontend.node_literal(right).not_nil!).should eq("b")
    end

    it "parses simple &&= assignment" do
      source = <<-CRYSTAL
      x &&= y
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      # Should desugar to: x = x && y
      assign_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(assign_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)

      # Target is identifier 'x'
      target_node = arena[CrystalV2::Compiler::Frontend.node_assign_target(assign_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(target_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Identifier)
      String.new(CrystalV2::Compiler::Frontend.node_literal(target_node).not_nil!).should eq("x")

      # Value is binary expression: x && y
      binary_node = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(binary_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      String.new(CrystalV2::Compiler::Frontend.node_operator(binary_node).not_nil!).should eq("&&")

      # Left side of && is 'x'
      left = arena[CrystalV2::Compiler::Frontend.node_left(binary_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(left).should eq(CrystalV2::Compiler::Frontend::NodeKind::Identifier)
      String.new(CrystalV2::Compiler::Frontend.node_literal(left).not_nil!).should eq("x")

      # Right side of && is 'y'
      right = arena[CrystalV2::Compiler::Frontend.node_right(binary_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(right).should eq(CrystalV2::Compiler::Frontend::NodeKind::Identifier)
      String.new(CrystalV2::Compiler::Frontend.node_literal(right).not_nil!).should eq("y")
    end

    it "parses ||= with number literal" do
      source = <<-CRYSTAL
      value ||= 42
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign_node = arena[program.roots[0]]
      binary_node = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_operator(binary_node).not_nil!).should eq("||")

      # Right side is number
      right = arena[CrystalV2::Compiler::Frontend.node_right(binary_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(right).should eq(CrystalV2::Compiler::Frontend::NodeKind::Number)
    end

    it "parses &&= with string literal" do
      source = <<-CRYSTAL
      name &&= "default"
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign_node = arena[program.roots[0]]
      binary_node = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_operator(binary_node).not_nil!).should eq("&&")

      # Right side is string
      right = arena[CrystalV2::Compiler::Frontend.node_right(binary_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(right).should eq(CrystalV2::Compiler::Frontend::NodeKind::String)
    end

    it "parses ||= with complex expression" do
      source = <<-CRYSTAL
      result ||= compute_value()
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign_node = arena[program.roots[0]]
      binary_node = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_operator(binary_node).not_nil!).should eq("||")

      # Right side is method call
      right = arena[CrystalV2::Compiler::Frontend.node_right(binary_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(right).should eq(CrystalV2::Compiler::Frontend::NodeKind::Call)
    end

    it "parses &&= with complex expression" do
      source = <<-CRYSTAL
      flag &&= check_condition()
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign_node = arena[program.roots[0]]
      binary_node = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_operator(binary_node).not_nil!).should eq("&&")

      # Right side is method call
      right = arena[CrystalV2::Compiler::Frontend.node_right(binary_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(right).should eq(CrystalV2::Compiler::Frontend::NodeKind::Call)
    end

    it "parses ||= in method definition" do
      source = <<-CRYSTAL
      def foo
        @cache ||= expensive_operation()
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

      # Check it's instance variable assignment
      target_node = arena[CrystalV2::Compiler::Frontend.node_assign_target(assign).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(target_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::InstanceVar)
      String.new(CrystalV2::Compiler::Frontend.node_literal(target_node).not_nil!).should eq("@cache")

      # Check desugaring: @cache = @cache || expensive_operation()
      binary = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_operator(binary).not_nil!).should eq("||")
    end

    it "parses &&= in class method" do
      source = <<-CRYSTAL
      class Foo
        def bar
          @enabled &&= true
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
      String.new(CrystalV2::Compiler::Frontend.node_operator(binary).not_nil!).should eq("&&")
    end

    it "parses multiple ||= assignments" do
      source = <<-CRYSTAL
      a ||= 1
      b ||= 2
      c ||= 3
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(3)
      arena = program.arena

      # All three should be assignments with || binary
      (0..2).each do |i|
        assign = arena[program.roots[i]]
        CrystalV2::Compiler::Frontend.node_kind(assign).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)

        binary = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
        String.new(CrystalV2::Compiler::Frontend.node_operator(binary).not_nil!).should eq("||")
      end
    end

    it "parses multiple &&= assignments" do
      source = <<-CRYSTAL
      x &&= true
      y &&= false
      z &&= nil
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(3)
      arena = program.arena

      # All three should be assignments with && binary
      (0..2).each do |i|
        assign = arena[program.roots[i]]
        CrystalV2::Compiler::Frontend.node_kind(assign).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)

        binary = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
        String.new(CrystalV2::Compiler::Frontend.node_operator(binary).not_nil!).should eq("&&")
      end
    end

    it "correctly distinguishes ||= from || and |" do
      source = <<-CRYSTAL
      a = x || y
      b ||= z
      c = d | e
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(3)
      arena = program.arena

      # First: x || y (binary expression)
      assign1 = arena[program.roots[0]]
      binary1 = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign1).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_operator(binary1).not_nil!).should eq("||")

      # Second: b ||= z (desugared to b = b || z)
      assign2 = arena[program.roots[1]]
      binary2 = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign2).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_operator(binary2).not_nil!).should eq("||")
      # Left of || should be 'b' (desugared)
      left2 = arena[CrystalV2::Compiler::Frontend.node_left(binary2).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(left2).not_nil!).should eq("b")

      # Third: d | e (bitwise or)
      assign3 = arena[program.roots[2]]
      binary3 = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign3).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_operator(binary3).not_nil!).should eq("|")
    end

    it "correctly distinguishes &&= from && and &" do
      source = <<-CRYSTAL
      a = x && y
      b &&= z
      c = d & e
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(3)
      arena = program.arena

      # First: x && y (binary expression)
      assign1 = arena[program.roots[0]]
      binary1 = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign1).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_operator(binary1).not_nil!).should eq("&&")

      # Second: b &&= z (desugared to b = b && z)
      assign2 = arena[program.roots[1]]
      binary2 = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign2).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_operator(binary2).not_nil!).should eq("&&")
      # Left of && should be 'b' (desugared)
      left2 = arena[CrystalV2::Compiler::Frontend.node_left(binary2).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(left2).not_nil!).should eq("b")

      # Third: d & e (bitwise and)
      assign3 = arena[program.roots[2]]
      binary3 = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign3).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_operator(binary3).not_nil!).should eq("&")
    end
  end
end
