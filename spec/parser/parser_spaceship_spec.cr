require "spec"

require "../../src/compiler/frontend/parser"

describe "CrystalV2::Compiler::Frontend::Parser" do
  describe "Phase 48: <=> spaceship operator (three-way comparison) (PRODUCTION-READY)" do
    it "parses simple spaceship comparison" do
      source = <<-CRYSTAL
      result = a <=> b
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(assign_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)

      # Right side is Binary node with <=> operator
      binary_node = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(binary_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)

      # Check operator
      String.new(CrystalV2::Compiler::Frontend.node_operator(binary_node).not_nil!).should eq("<=>")

      # Check left operand
      left = arena[CrystalV2::Compiler::Frontend.node_left(binary_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(left).should eq(CrystalV2::Compiler::Frontend::NodeKind::Identifier)
      String.new(CrystalV2::Compiler::Frontend.node_literal(left).not_nil!).should eq("a")

      # Check right operand
      right = arena[CrystalV2::Compiler::Frontend.node_right(binary_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(right).should eq(CrystalV2::Compiler::Frontend::NodeKind::Identifier)
      String.new(CrystalV2::Compiler::Frontend.node_literal(right).not_nil!).should eq("b")
    end

    it "parses spaceship with number literals" do
      source = <<-CRYSTAL
      x = 42 <=> 100
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign_node = arena[program.roots[0]]
      binary_node = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(binary_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)

      String.new(CrystalV2::Compiler::Frontend.node_operator(binary_node).not_nil!).should eq("<=>")

      # Check left is number
      left = arena[CrystalV2::Compiler::Frontend.node_left(binary_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(left).should eq(CrystalV2::Compiler::Frontend::NodeKind::Number)

      # Check right is number
      right = arena[CrystalV2::Compiler::Frontend.node_right(binary_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(right).should eq(CrystalV2::Compiler::Frontend::NodeKind::Number)
    end

    it "parses chained spaceship comparisons" do
      source = <<-CRYSTAL
      result = (a <=> b) <=> (c <=> d)
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign_node = arena[program.roots[0]]
      outer_binary = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(outer_binary).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      String.new(CrystalV2::Compiler::Frontend.node_operator(outer_binary).not_nil!).should eq("<=>")

      # Left is (a <=> b)
      left_binary = arena[CrystalV2::Compiler::Frontend.node_left(outer_binary).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(left_binary).should eq(CrystalV2::Compiler::Frontend::NodeKind::Grouping)

      # Right is (c <=> d)
      right_binary = arena[CrystalV2::Compiler::Frontend.node_right(outer_binary).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(right_binary).should eq(CrystalV2::Compiler::Frontend::NodeKind::Grouping)
    end

    it "parses spaceship in method call arguments" do
      source = <<-CRYSTAL
      puts(a <=> b)
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      call_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(call_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Call)

      # Check argument is spaceship comparison
      args = CrystalV2::Compiler::Frontend.node_args(call_node).not_nil!
      args.size.should eq(1)

      arg = arena[args[0]]
      CrystalV2::Compiler::Frontend.node_kind(arg).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      String.new(CrystalV2::Compiler::Frontend.node_operator(arg).not_nil!).should eq("<=>")
    end

    it "parses spaceship in array literal" do
      source = <<-CRYSTAL
      arr = [a <=> b, c <=> d]
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign_node = arena[program.roots[0]]
      array_node = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(array_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::ArrayLiteral)

      elements = CrystalV2::Compiler::Frontend.node_array_elements(array_node).not_nil!
      elements.size.should eq(2)

      first = arena[elements[0]]
      CrystalV2::Compiler::Frontend.node_kind(first).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      String.new(CrystalV2::Compiler::Frontend.node_operator(first).not_nil!).should eq("<=>")

      second = arena[elements[1]]
      CrystalV2::Compiler::Frontend.node_kind(second).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      String.new(CrystalV2::Compiler::Frontend.node_operator(second).not_nil!).should eq("<=>")
    end

    it "parses spaceship in conditional" do
      source = <<-CRYSTAL
      if (a <=> b) == 0
        x
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      if_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(if_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::If)

      # Condition is (a <=> b) == 0
      condition = arena[CrystalV2::Compiler::Frontend.node_condition(if_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(condition).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      String.new(CrystalV2::Compiler::Frontend.node_operator(condition).not_nil!).should eq("==")

      # Left of == is spaceship
      left = arena[CrystalV2::Compiler::Frontend.node_left(condition).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(left).should eq(CrystalV2::Compiler::Frontend::NodeKind::Grouping)
    end

    it "parses spaceship with complex expressions" do
      source = <<-CRYSTAL
      result = (x + 1) <=> (y * 2)
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign_node = arena[program.roots[0]]
      binary_node = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(binary_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)

      String.new(CrystalV2::Compiler::Frontend.node_operator(binary_node).not_nil!).should eq("<=>")

      # Left is (x + 1)
      left = arena[CrystalV2::Compiler::Frontend.node_left(binary_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(left).should eq(CrystalV2::Compiler::Frontend::NodeKind::Grouping)

      # Right is (y * 2)
      right = arena[CrystalV2::Compiler::Frontend.node_right(binary_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(right).should eq(CrystalV2::Compiler::Frontend::NodeKind::Grouping)
    end

    it "parses spaceship in method definition" do
      source = <<-CRYSTAL
      def compare
        a <=> b
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
      CrystalV2::Compiler::Frontend.node_kind(body).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      String.new(CrystalV2::Compiler::Frontend.node_operator(body).not_nil!).should eq("<=>")
    end

    it "parses spaceship in class method" do
      source = <<-CRYSTAL
      class Foo
        def compare
          @x <=> @y
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
      String.new(CrystalV2::Compiler::Frontend.node_operator(def_body).not_nil!).should eq("<=>")
    end

    it "parses spaceship with strings" do
      source = <<-CRYSTAL
      "apple" <=> "banana"
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      binary_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(binary_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      String.new(CrystalV2::Compiler::Frontend.node_operator(binary_node).not_nil!).should eq("<=>")

      # Check left is string
      left = arena[CrystalV2::Compiler::Frontend.node_left(binary_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(left).should eq(CrystalV2::Compiler::Frontend::NodeKind::String)

      # Check right is string
      right = arena[CrystalV2::Compiler::Frontend.node_right(binary_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(right).should eq(CrystalV2::Compiler::Frontend::NodeKind::String)
    end

    it "parses spaceship in return statement" do
      source = <<-CRYSTAL
      def compare
        return a <=> b
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
      String.new(CrystalV2::Compiler::Frontend.node_operator(return_value).not_nil!).should eq("<=>")
    end

    it "correctly distinguishes <=> from <= and >=" do
      source = <<-CRYSTAL
      a = x <= y
      b = x >= y
      c = x <=> y
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(3)
      arena = program.arena

      # First: x <= y
      assign1 = arena[program.roots[0]]
      binary1 = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign1).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_operator(binary1).not_nil!).should eq("<=")

      # Second: x >= y
      assign2 = arena[program.roots[1]]
      binary2 = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign2).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_operator(binary2).not_nil!).should eq(">=")

      # Third: x <=> y
      assign3 = arena[program.roots[2]]
      binary3 = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign3).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_operator(binary3).not_nil!).should eq("<=>")
    end
  end
end
