require "spec"

require "../src/compiler/frontend/parser"

describe "CrystalV2::Compiler::Frontend::Parser" do
  describe "Phase 40: typeof (type introspection) (PRODUCTION-READY)" do
    it "parses typeof with single argument" do
      source = <<-CRYSTAL
      x = 1
      y = typeof(x)
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(2)
      arena = program.arena

      # Second statement is assignment with typeof
      assign_node = arena[program.roots[1]]
      CrystalV2::Compiler::Frontend.node_kind(assign_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)

      # Value side is typeof
      typeof_expr = CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!
      typeof_node = arena[typeof_expr]
      CrystalV2::Compiler::Frontend.node_kind(typeof_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Typeof)

      # Check arguments
      args = CrystalV2::Compiler::Frontend.node_typeof_args(typeof_node).not_nil!
      args.size.should eq(1)

      # Argument is identifier x
      arg_node = arena[args[0]]
      CrystalV2::Compiler::Frontend.node_kind(arg_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Identifier)
      String.new(CrystalV2::Compiler::Frontend.node_literal(arg_node).not_nil!).should eq("x")
    end

    it "parses typeof with multiple arguments (union type)" do
      source = <<-CRYSTAL
      x = typeof(1, "hello", true)
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign_node = arena[program.roots[0]]
      typeof_expr = CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!
      typeof_node = arena[typeof_expr]

      CrystalV2::Compiler::Frontend.node_kind(typeof_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Typeof)

      # Check we have 3 arguments
      args = CrystalV2::Compiler::Frontend.node_typeof_args(typeof_node).not_nil!
      args.size.should eq(3)

      # First arg: number
      arg1 = arena[args[0]]
      CrystalV2::Compiler::Frontend.node_kind(arg1).should eq(CrystalV2::Compiler::Frontend::NodeKind::Number)

      # Second arg: string
      arg2 = arena[args[1]]
      CrystalV2::Compiler::Frontend.node_kind(arg2).should eq(CrystalV2::Compiler::Frontend::NodeKind::String)

      # Third arg: boolean
      arg3 = arena[args[2]]
      CrystalV2::Compiler::Frontend.node_kind(arg3).should eq(CrystalV2::Compiler::Frontend::NodeKind::Bool)
    end

    it "parses typeof with expression argument" do
      source = <<-CRYSTAL
      x = typeof(1 + 2)
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign_node = arena[program.roots[0]]
      typeof_expr = CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!
      typeof_node = arena[typeof_expr]

      CrystalV2::Compiler::Frontend.node_kind(typeof_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Typeof)

      args = CrystalV2::Compiler::Frontend.node_typeof_args(typeof_node).not_nil!
      args.size.should eq(1)

      # Argument is binary expression (1 + 2)
      arg_node = arena[args[0]]
      CrystalV2::Compiler::Frontend.node_kind(arg_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
    end

    it "parses typeof with method call argument" do
      source = <<-CRYSTAL
      x = typeof(foo.bar)
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign_node = arena[program.roots[0]]
      typeof_expr = CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!
      typeof_node = arena[typeof_expr]

      CrystalV2::Compiler::Frontend.node_kind(typeof_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Typeof)

      args = CrystalV2::Compiler::Frontend.node_typeof_args(typeof_node).not_nil!
      args.size.should eq(1)

      # Argument is member access (foo.bar)
      arg_node = arena[args[0]]
      CrystalV2::Compiler::Frontend.node_kind(arg_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::MemberAccess)
    end

    it "parses typeof in method definition" do
      source = <<-CRYSTAL
      def foo
        x = 1
        typeof(x)
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      method_node = arena[program.roots[0]]
      method_body = CrystalV2::Compiler::Frontend.node_def_body(method_node).not_nil!
      method_body.size.should eq(2)

      # Last statement is typeof
      typeof_node = arena[method_body[1]]
      CrystalV2::Compiler::Frontend.node_kind(typeof_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Typeof)
    end

    it "parses nested typeof" do
      source = <<-CRYSTAL
      x = typeof(typeof(1))
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign_node = arena[program.roots[0]]
      outer_typeof_expr = CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!
      outer_typeof = arena[outer_typeof_expr]

      CrystalV2::Compiler::Frontend.node_kind(outer_typeof).should eq(CrystalV2::Compiler::Frontend::NodeKind::Typeof)

      # Outer typeof has one argument
      outer_args = CrystalV2::Compiler::Frontend.node_typeof_args(outer_typeof).not_nil!
      outer_args.size.should eq(1)

      # That argument is also a typeof
      inner_typeof = arena[outer_args[0]]
      CrystalV2::Compiler::Frontend.node_kind(inner_typeof).should eq(CrystalV2::Compiler::Frontend::NodeKind::Typeof)

      # Inner typeof has one argument (number 1)
      inner_args = CrystalV2::Compiler::Frontend.node_typeof_args(inner_typeof).not_nil!
      inner_args.size.should eq(1)

      number_node = arena[inner_args[0]]
      CrystalV2::Compiler::Frontend.node_kind(number_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Number)
    end

    it "parses typeof with array literal" do
      source = <<-CRYSTAL
      x = typeof([1, 2, 3])
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign_node = arena[program.roots[0]]
      typeof_expr = CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!
      typeof_node = arena[typeof_expr]

      CrystalV2::Compiler::Frontend.node_kind(typeof_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Typeof)

      args = CrystalV2::Compiler::Frontend.node_typeof_args(typeof_node).not_nil!
      args.size.should eq(1)

      # Argument is array literal
      arg_node = arena[args[0]]
      CrystalV2::Compiler::Frontend.node_kind(arg_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::ArrayLiteral)
    end

    it "parses typeof in class" do
      source = <<-CRYSTAL
      class Foo
        def bar
          x = 1
          typeof(x)
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

      # Last statement is typeof
      typeof_node = arena[method_body[1]]
      CrystalV2::Compiler::Frontend.node_kind(typeof_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Typeof)
    end

    it "parses typeof with complex union type" do
      source = <<-CRYSTAL
      x = typeof(1, "str", 3.14, true, nil)
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign_node = arena[program.roots[0]]
      typeof_expr = CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!
      typeof_node = arena[typeof_expr]

      CrystalV2::Compiler::Frontend.node_kind(typeof_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Typeof)

      args = CrystalV2::Compiler::Frontend.node_typeof_args(typeof_node).not_nil!
      args.size.should eq(5)

      # Verify each argument type
      CrystalV2::Compiler::Frontend.node_kind(arena[args[0]]).should eq(CrystalV2::Compiler::Frontend::NodeKind::Number)
      CrystalV2::Compiler::Frontend.node_kind(arena[args[1]]).should eq(CrystalV2::Compiler::Frontend::NodeKind::String)
      CrystalV2::Compiler::Frontend.node_kind(arena[args[2]]).should eq(CrystalV2::Compiler::Frontend::NodeKind::Number)
      CrystalV2::Compiler::Frontend.node_kind(arena[args[3]]).should eq(CrystalV2::Compiler::Frontend::NodeKind::Bool)
      CrystalV2::Compiler::Frontend.node_kind(arena[args[4]]).should eq(CrystalV2::Compiler::Frontend::NodeKind::Nil)
    end
  end
end
