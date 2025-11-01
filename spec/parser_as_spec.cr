require "spec"

require "../src/compiler/frontend/parser"

describe "CrystalV2::Compiler::Frontend::Parser" do
  describe "Phase 44: as keyword (type casting) (PRODUCTION-READY)" do
    it "parses simple type cast" do
      source = <<-CRYSTAL
      x = value.as(Int32)
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(assign_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)

      # Right side is As node
      as_node = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(as_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::As)

      # Check target type
      String.new(CrystalV2::Compiler::Frontend.node_as_target_type(as_node).not_nil!).should eq("Int32")

      # Check value being cast
      value_node = arena[CrystalV2::Compiler::Frontend.node_as_value(as_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(value_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Identifier)
      String.new(CrystalV2::Compiler::Frontend.node_literal(value_node).not_nil!).should eq("value")
    end

    it "parses type cast with complex expression" do
      source = <<-CRYSTAL
      y = (x + 1).as(String)
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign_node = arena[program.roots[0]]
      as_node = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(as_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::As)

      String.new(CrystalV2::Compiler::Frontend.node_as_target_type(as_node).not_nil!).should eq("String")

      # Value is a grouping with binary expression inside
      grouping_node = arena[CrystalV2::Compiler::Frontend.node_as_value(as_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(grouping_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Grouping)
    end

    it "parses chained type casts" do
      source = <<-CRYSTAL
      z = x.as(Int32).as(Int64)
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign_node = arena[program.roots[0]]

      # Outer cast to Int64
      outer_as = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(outer_as).should eq(CrystalV2::Compiler::Frontend::NodeKind::As)
      String.new(CrystalV2::Compiler::Frontend.node_as_target_type(outer_as).not_nil!).should eq("Int64")

      # Inner cast to Int32
      inner_as = arena[CrystalV2::Compiler::Frontend.node_as_value(outer_as).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(inner_as).should eq(CrystalV2::Compiler::Frontend::NodeKind::As)
      String.new(CrystalV2::Compiler::Frontend.node_as_target_type(inner_as).not_nil!).should eq("Int32")

      # Original value
      value_node = arena[CrystalV2::Compiler::Frontend.node_as_value(inner_as).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(value_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Identifier)
      String.new(CrystalV2::Compiler::Frontend.node_literal(value_node).not_nil!).should eq("x")
    end

    it "parses type cast in method call" do
      source = <<-CRYSTAL
      foo(x.as(String))
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      call_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(call_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Call)

      # Check argument is type cast
      args = CrystalV2::Compiler::Frontend.node_args(call_node).not_nil!
      args.size.should eq(1)

      as_node = arena[args[0]]
      CrystalV2::Compiler::Frontend.node_kind(as_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::As)
      String.new(CrystalV2::Compiler::Frontend.node_as_target_type(as_node).not_nil!).should eq("String")
    end

    it "parses type cast in array literal" do
      source = <<-CRYSTAL
      arr = [x.as(Int32), y.as(Int32)]
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

      # First element
      first_as = arena[elements[0]]
      CrystalV2::Compiler::Frontend.node_kind(first_as).should eq(CrystalV2::Compiler::Frontend::NodeKind::As)
      String.new(CrystalV2::Compiler::Frontend.node_as_target_type(first_as).not_nil!).should eq("Int32")

      # Second element
      second_as = arena[elements[1]]
      CrystalV2::Compiler::Frontend.node_kind(second_as).should eq(CrystalV2::Compiler::Frontend::NodeKind::As)
      String.new(CrystalV2::Compiler::Frontend.node_as_target_type(second_as).not_nil!).should eq("Int32")
    end

    it "parses type cast in conditional" do
      source = <<-CRYSTAL
      if value.as(Bool)
        1
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      if_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(if_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::If)

      # Condition is type cast
      condition_node = arena[CrystalV2::Compiler::Frontend.node_condition(if_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(condition_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::As)
      String.new(CrystalV2::Compiler::Frontend.node_as_target_type(condition_node).not_nil!).should eq("Bool")
    end

    it "parses type cast with custom type" do
      source = <<-CRYSTAL
      obj = value.as(MyClass)
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign_node = arena[program.roots[0]]
      as_node = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(as_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::As)

      String.new(CrystalV2::Compiler::Frontend.node_as_target_type(as_node).not_nil!).should eq("MyClass")
    end

    it "parses type cast in method definition" do
      source = <<-CRYSTAL
      def foo
        x.as(Int32)
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      method_node = arena[program.roots[0]]
      method_body = CrystalV2::Compiler::Frontend.node_def_body(method_node).not_nil!
      method_body.size.should eq(1)

      as_node = arena[method_body[0]]
      CrystalV2::Compiler::Frontend.node_kind(as_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::As)
      String.new(CrystalV2::Compiler::Frontend.node_as_target_type(as_node).not_nil!).should eq("Int32")
    end

    it "parses type cast in class" do
      source = <<-CRYSTAL
      class Foo
        def bar
          @value.as(String)
        end
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      class_node = arena[program.roots[0]]
      class_body = CrystalV2::Compiler::Frontend.node_class_body(class_node).not_nil!
      method_node = arena[class_body[0]]
      method_body = CrystalV2::Compiler::Frontend.node_def_body(method_node).not_nil!

      as_node = arena[method_body[0]]
      CrystalV2::Compiler::Frontend.node_kind(as_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::As)
      String.new(CrystalV2::Compiler::Frontend.node_as_target_type(as_node).not_nil!).should eq("String")

      # Value is instance variable
      ivar_node = arena[CrystalV2::Compiler::Frontend.node_as_value(as_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(ivar_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::InstanceVar)
    end

    it "parses type cast after method call" do
      source = <<-CRYSTAL
      result = obj.method.as(Int32)
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign_node = arena[program.roots[0]]
      as_node = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(as_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::As)
      String.new(CrystalV2::Compiler::Frontend.node_as_target_type(as_node).not_nil!).should eq("Int32")

      # Value is method call (actually MemberAccess without parens)
      member_node = arena[CrystalV2::Compiler::Frontend.node_as_value(as_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(member_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::MemberAccess)
      String.new(CrystalV2::Compiler::Frontend.node_member(member_node).not_nil!).should eq("method")
    end

    it "parses type cast with number literal" do
      source = <<-CRYSTAL
      x = 42.as(Int64)
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign_node = arena[program.roots[0]]
      as_node = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(as_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::As)
      String.new(CrystalV2::Compiler::Frontend.node_as_target_type(as_node).not_nil!).should eq("Int64")

      # Value is number literal
      number_node = arena[CrystalV2::Compiler::Frontend.node_as_value(as_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(number_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Number)
    end

    it "parses type cast in return statement" do
      source = <<-CRYSTAL
      def foo
        return x.as(String)
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      method_node = arena[program.roots[0]]
      method_body = CrystalV2::Compiler::Frontend.node_def_body(method_node).not_nil!

      return_node = arena[method_body[0]]
      CrystalV2::Compiler::Frontend.node_kind(return_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Return)

      # Return value is type cast
      as_node = arena[CrystalV2::Compiler::Frontend.node_return_value(return_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(as_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::As)
      String.new(CrystalV2::Compiler::Frontend.node_as_target_type(as_node).not_nil!).should eq("String")
    end
  end
end
