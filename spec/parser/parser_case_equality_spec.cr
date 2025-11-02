require "spec"

require "../../src/compiler/frontend/parser"

describe "CrystalV2::Compiler::Frontend::Parser" do
  describe "Phase 50: === case equality operator (PRODUCTION-READY)" do
    it "parses simple case equality comparison" do
      source = <<-CRYSTAL
      result = a === b
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(assign_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)

      # Right side is Binary node with === operator
      binary_node = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(binary_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)

      # Check operator
      String.new(CrystalV2::Compiler::Frontend.node_operator(binary_node).not_nil!).should eq("===")

      # Check left operand
      left = arena[CrystalV2::Compiler::Frontend.node_left(binary_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(left).should eq(CrystalV2::Compiler::Frontend::NodeKind::Identifier)
      String.new(CrystalV2::Compiler::Frontend.node_literal(left).not_nil!).should eq("a")

      # Check right operand
      right = arena[CrystalV2::Compiler::Frontend.node_right(binary_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(right).should eq(CrystalV2::Compiler::Frontend::NodeKind::Identifier)
      String.new(CrystalV2::Compiler::Frontend.node_literal(right).not_nil!).should eq("b")
    end

    it "parses case equality with type on left" do
      source = <<-CRYSTAL
      x = String === value
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign_node = arena[program.roots[0]]
      binary_node = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(binary_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)

      String.new(CrystalV2::Compiler::Frontend.node_operator(binary_node).not_nil!).should eq("===")

      # Left is type identifier
      left = arena[CrystalV2::Compiler::Frontend.node_left(binary_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(left).should eq(CrystalV2::Compiler::Frontend::NodeKind::Identifier)
      String.new(CrystalV2::Compiler::Frontend.node_literal(left).not_nil!).should eq("String")

      # Right is value
      right = arena[CrystalV2::Compiler::Frontend.node_right(binary_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(right).should eq(CrystalV2::Compiler::Frontend::NodeKind::Identifier)
      String.new(CrystalV2::Compiler::Frontend.node_literal(right).not_nil!).should eq("value")
    end

    it "parses case equality with range" do
      source = <<-CRYSTAL
      result = (1..10) === x
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign_node = arena[program.roots[0]]
      binary_node = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(binary_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)

      String.new(CrystalV2::Compiler::Frontend.node_operator(binary_node).not_nil!).should eq("===")

      # Left is grouping (range)
      left = arena[CrystalV2::Compiler::Frontend.node_left(binary_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(left).should eq(CrystalV2::Compiler::Frontend::NodeKind::Grouping)
    end

    it "parses case equality in conditional" do
      source = <<-CRYSTAL
      if String === obj
        x
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      if_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(if_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::If)

      # Condition is case equality
      condition = arena[CrystalV2::Compiler::Frontend.node_condition(if_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(condition).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      String.new(CrystalV2::Compiler::Frontend.node_operator(condition).not_nil!).should eq("===")
    end

    it "parses case equality in method call arguments" do
      source = <<-CRYSTAL
      puts(String === value)
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      call_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(call_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Call)

      # Argument is case equality
      args = CrystalV2::Compiler::Frontend.node_args(call_node).not_nil!
      args.size.should eq(1)

      arg = arena[args[0]]
      CrystalV2::Compiler::Frontend.node_kind(arg).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      String.new(CrystalV2::Compiler::Frontend.node_operator(arg).not_nil!).should eq("===")
    end

    it "parses case equality in array literal" do
      source = <<-CRYSTAL
      [String === a, Integer === b]
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      array_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(array_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::ArrayLiteral)

      elements = CrystalV2::Compiler::Frontend.node_array_elements(array_node).not_nil!
      elements.size.should eq(2)

      # Both elements are case equality
      first = arena[elements[0]]
      CrystalV2::Compiler::Frontend.node_kind(first).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      String.new(CrystalV2::Compiler::Frontend.node_operator(first).not_nil!).should eq("===")

      second = arena[elements[1]]
      CrystalV2::Compiler::Frontend.node_kind(second).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      String.new(CrystalV2::Compiler::Frontend.node_operator(second).not_nil!).should eq("===")
    end

    it "parses case equality in binary expression" do
      source = <<-CRYSTAL
      String === a && Integer === b
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      binary_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(binary_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      String.new(CrystalV2::Compiler::Frontend.node_operator(binary_node).not_nil!).should eq("&&")

      # Left side is case equality
      left = arena[CrystalV2::Compiler::Frontend.node_left(binary_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(left).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      String.new(CrystalV2::Compiler::Frontend.node_operator(left).not_nil!).should eq("===")

      # Right side is case equality
      right = arena[CrystalV2::Compiler::Frontend.node_right(binary_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(right).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      String.new(CrystalV2::Compiler::Frontend.node_operator(right).not_nil!).should eq("===")
    end

    it "parses case equality in return statement" do
      source = <<-CRYSTAL
      def check
        return String === obj
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
      body = arena[def_body[0]]
      CrystalV2::Compiler::Frontend.node_kind(body).should eq(CrystalV2::Compiler::Frontend::NodeKind::Return)

      return_value = arena[CrystalV2::Compiler::Frontend.node_return_value(body).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(return_value).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      String.new(CrystalV2::Compiler::Frontend.node_operator(return_value).not_nil!).should eq("===")
    end

    it "parses chained case equality" do
      source = <<-CRYSTAL
      result = (String === a) === (Integer === b)
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign_node = arena[program.roots[0]]
      outer_binary = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(outer_binary).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      String.new(CrystalV2::Compiler::Frontend.node_operator(outer_binary).not_nil!).should eq("===")

      # Left is grouping containing ===
      left_grouping = arena[CrystalV2::Compiler::Frontend.node_left(outer_binary).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(left_grouping).should eq(CrystalV2::Compiler::Frontend::NodeKind::Grouping)

      # Right is grouping containing ===
      right_grouping = arena[CrystalV2::Compiler::Frontend.node_right(outer_binary).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(right_grouping).should eq(CrystalV2::Compiler::Frontend::NodeKind::Grouping)
    end

    it "parses case equality in ternary expression" do
      source = <<-CRYSTAL
      String === obj ? "yes" : "no"
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      ternary_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(ternary_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Ternary)

      # Condition is case equality
      condition = arena[CrystalV2::Compiler::Frontend.node_ternary_condition(ternary_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(condition).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      String.new(CrystalV2::Compiler::Frontend.node_operator(condition).not_nil!).should eq("===")
    end

    it "parses case equality in class method" do
      source = <<-CRYSTAL
      class Foo
        def match?
          String === @value
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
      def_body = arena[method_def_body[0]]
      CrystalV2::Compiler::Frontend.node_kind(def_body).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      String.new(CrystalV2::Compiler::Frontend.node_operator(def_body).not_nil!).should eq("===")
    end

    it "correctly distinguishes === from ==" do
      source = <<-CRYSTAL
      a = x == y
      b = x === y
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(2)
      arena = program.arena

      # First: x == y
      assign1 = arena[program.roots[0]]
      binary1 = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign1).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_operator(binary1).not_nil!).should eq("==")

      # Second: x === y
      assign2 = arena[program.roots[1]]
      binary2 = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign2).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_operator(binary2).not_nil!).should eq("===")
    end
  end
end
