require "spec"

require "../src/compiler/frontend/parser"

describe "CrystalV2::Compiler::Frontend::Parser" do
  describe "Phase 93: is_a? keyword (type check pseudo-method - returns Bool)" do
    it "parses simple type check" do
      source = <<-CRYSTAL
      x = value.is_a?(Int32)
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(assign_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)

      # Right side is IsA node
      is_a_node = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(is_a_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::IsA)

      # Check target type
      String.new(CrystalV2::Compiler::Frontend.node_is_a_target_type(is_a_node).not_nil!).should eq("Int32")

      # Check value being checked
      value_node = arena[CrystalV2::Compiler::Frontend.node_is_a_value(is_a_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(value_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Identifier)
      String.new(CrystalV2::Compiler::Frontend.node_literal(value_node).not_nil!).should eq("value")
    end

    it "parses type check with complex expression" do
      source = <<-CRYSTAL
      y = (x + 1).is_a?(String)
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign_node = arena[program.roots[0]]
      is_a_node = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(is_a_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::IsA)

      String.new(CrystalV2::Compiler::Frontend.node_is_a_target_type(is_a_node).not_nil!).should eq("String")

      # Check value is grouping (parenthesized expression)
      value_node = arena[CrystalV2::Compiler::Frontend.node_is_a_value(is_a_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(value_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Grouping)
    end

    it "parses chained type checks" do
      source = <<-CRYSTAL
      result = value.is_a?(Int32).is_a?(String)
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign_node = arena[program.roots[0]]
      outer_check = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(outer_check).should eq(CrystalV2::Compiler::Frontend::NodeKind::IsA)
      String.new(CrystalV2::Compiler::Frontend.node_is_a_target_type(outer_check).not_nil!).should eq("String")

      # Inner check
      inner_check = arena[CrystalV2::Compiler::Frontend.node_is_a_value(outer_check).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(inner_check).should eq(CrystalV2::Compiler::Frontend::NodeKind::IsA)
      String.new(CrystalV2::Compiler::Frontend.node_is_a_target_type(inner_check).not_nil!).should eq("Int32")
    end

    it "parses type check in method call arguments" do
      source = <<-CRYSTAL
      puts(value.is_a?(Int32))
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      call_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(call_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Call)

      # Check argument is type check
      args = CrystalV2::Compiler::Frontend.node_args(call_node).not_nil!
      args.size.should eq(1)

      arg = arena[args[0]]
      CrystalV2::Compiler::Frontend.node_kind(arg).should eq(CrystalV2::Compiler::Frontend::NodeKind::IsA)
      String.new(CrystalV2::Compiler::Frontend.node_is_a_target_type(arg).not_nil!).should eq("Int32")
    end

    it "parses type check in array literal" do
      source = <<-CRYSTAL
      arr = [value.is_a?(Int32), other.is_a?(String)]
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
      CrystalV2::Compiler::Frontend.node_kind(first).should eq(CrystalV2::Compiler::Frontend::NodeKind::IsA)
      String.new(CrystalV2::Compiler::Frontend.node_is_a_target_type(first).not_nil!).should eq("Int32")

      second = arena[elements[1]]
      CrystalV2::Compiler::Frontend.node_kind(second).should eq(CrystalV2::Compiler::Frontend::NodeKind::IsA)
      String.new(CrystalV2::Compiler::Frontend.node_is_a_target_type(second).not_nil!).should eq("String")
    end

    it "parses type check in conditional" do
      source = <<-CRYSTAL
      if value.is_a?(Int32)
        x
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      if_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(if_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::If)

      condition = arena[CrystalV2::Compiler::Frontend.node_condition(if_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(condition).should eq(CrystalV2::Compiler::Frontend::NodeKind::IsA)
      String.new(CrystalV2::Compiler::Frontend.node_is_a_target_type(condition).not_nil!).should eq("Int32")
    end

    it "parses type check with custom type" do
      source = <<-CRYSTAL
      obj.is_a?(MyClass)
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      is_a_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(is_a_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::IsA)
      String.new(CrystalV2::Compiler::Frontend.node_is_a_target_type(is_a_node).not_nil!).should eq("MyClass")
    end

    it "parses type check in method definition" do
      source = <<-CRYSTAL
      def foo
        value.is_a?(Int32)
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
      CrystalV2::Compiler::Frontend.node_kind(body).should eq(CrystalV2::Compiler::Frontend::NodeKind::IsA)
      String.new(CrystalV2::Compiler::Frontend.node_is_a_target_type(body).not_nil!).should eq("Int32")
    end

    it "parses type check in class method" do
      source = <<-CRYSTAL
      class Foo
        def bar
          @x.is_a?(String)
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
      CrystalV2::Compiler::Frontend.node_kind(def_body).should eq(CrystalV2::Compiler::Frontend::NodeKind::IsA)
      String.new(CrystalV2::Compiler::Frontend.node_is_a_target_type(def_body).not_nil!).should eq("String")
    end

    it "parses type check after method call" do
      source = <<-CRYSTAL
      obj.get_value.is_a?(Int32)
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      is_a_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(is_a_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::IsA)
      String.new(CrystalV2::Compiler::Frontend.node_is_a_target_type(is_a_node).not_nil!).should eq("Int32")

      # Check receiver is member access
      receiver = arena[CrystalV2::Compiler::Frontend.node_is_a_value(is_a_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(receiver).should eq(CrystalV2::Compiler::Frontend::NodeKind::MemberAccess)
    end

    it "parses type check with literal" do
      source = <<-CRYSTAL
      42.is_a?(Int32)
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      is_a_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(is_a_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::IsA)
      String.new(CrystalV2::Compiler::Frontend.node_is_a_target_type(is_a_node).not_nil!).should eq("Int32")

      # Check receiver is number literal
      receiver = arena[CrystalV2::Compiler::Frontend.node_is_a_value(is_a_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(receiver).should eq(CrystalV2::Compiler::Frontend::NodeKind::Number)
    end

    it "parses type check in return statement" do
      source = <<-CRYSTAL
      def foo
        return value.is_a?(String)
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
      CrystalV2::Compiler::Frontend.node_kind(return_value).should eq(CrystalV2::Compiler::Frontend::NodeKind::IsA)
      String.new(CrystalV2::Compiler::Frontend.node_is_a_target_type(return_value).not_nil!).should eq("String")
    end
  end
end
