require "spec"

require "../src/compiler/frontend/parser"

describe "CrystalV2::Compiler::Frontend::Parser" do
  describe "Phase 41: sizeof (size in bytes) (PRODUCTION-READY)" do
    it "parses sizeof with type identifier" do
      source = <<-CRYSTAL
      x = sizeof(Int32)
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(assign_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)

      # Value side is sizeof
      sizeof_expr = CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!
      sizeof_node = arena[sizeof_expr]
      CrystalV2::Compiler::Frontend.node_kind(sizeof_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Sizeof)

      # Check arguments
      args = CrystalV2::Compiler::Frontend.node_sizeof_args(sizeof_node).not_nil!
      args.size.should eq(1)

      # Argument is identifier Int32
      arg_node = arena[args[0]]
      CrystalV2::Compiler::Frontend.node_kind(arg_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Identifier)
      CrystalV2::Compiler::Frontend.node_literal(arg_node).not_nil!.should eq("Int32".to_slice)
    end

    it "parses sizeof with variable" do
      source = <<-CRYSTAL
      x = 1
      y = sizeof(x)
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(2)
      arena = program.arena

      # Second statement is assignment with sizeof
      assign_node = arena[program.roots[1]]
      sizeof_expr = CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!
      sizeof_node = arena[sizeof_expr]

      CrystalV2::Compiler::Frontend.node_kind(sizeof_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Sizeof)

      args = CrystalV2::Compiler::Frontend.node_sizeof_args(sizeof_node).not_nil!
      args.size.should eq(1)

      # Argument is identifier x
      arg_node = arena[args[0]]
      CrystalV2::Compiler::Frontend.node_kind(arg_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Identifier)
      CrystalV2::Compiler::Frontend.node_literal(arg_node).not_nil!.should eq("x".to_slice)
    end

    it "parses sizeof with expression" do
      source = <<-CRYSTAL
      x = sizeof(1 + 2)
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign_node = arena[program.roots[0]]
      sizeof_expr = CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!
      sizeof_node = arena[sizeof_expr]

      CrystalV2::Compiler::Frontend.node_kind(sizeof_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Sizeof)

      args = CrystalV2::Compiler::Frontend.node_sizeof_args(sizeof_node).not_nil!
      args.size.should eq(1)

      # Argument is binary expression
      arg_node = arena[args[0]]
      CrystalV2::Compiler::Frontend.node_kind(arg_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
    end

    it "parses sizeof with array literal" do
      source = <<-CRYSTAL
      x = sizeof([1, 2, 3])
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign_node = arena[program.roots[0]]
      sizeof_expr = CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!
      sizeof_node = arena[sizeof_expr]

      CrystalV2::Compiler::Frontend.node_kind(sizeof_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Sizeof)

      args = CrystalV2::Compiler::Frontend.node_sizeof_args(sizeof_node).not_nil!
      args.size.should eq(1)

      # Argument is array literal
      arg_node = arena[args[0]]
      CrystalV2::Compiler::Frontend.node_kind(arg_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::ArrayLiteral)
    end

    it "parses sizeof in method definition" do
      source = <<-CRYSTAL
      def foo
        x = 1
        sizeof(x)
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      method_node = arena[program.roots[0]]
      method_body = CrystalV2::Compiler::Frontend.node_def_body(method_node).not_nil!
      method_body.size.should eq(2)

      # Last statement is sizeof
      sizeof_node = arena[method_body[1]]
      CrystalV2::Compiler::Frontend.node_kind(sizeof_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Sizeof)
    end

    it "parses sizeof in class" do
      source = <<-CRYSTAL
      class Foo
        def bar
          x = 1
          sizeof(x)
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

      # Last statement is sizeof
      sizeof_node = arena[method_body[1]]
      CrystalV2::Compiler::Frontend.node_kind(sizeof_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Sizeof)
    end

    it "parses nested sizeof" do
      source = <<-CRYSTAL
      x = sizeof(sizeof(Int32))
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign_node = arena[program.roots[0]]
      outer_sizeof_expr = CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!
      outer_sizeof = arena[outer_sizeof_expr]

      CrystalV2::Compiler::Frontend.node_kind(outer_sizeof).should eq(CrystalV2::Compiler::Frontend::NodeKind::Sizeof)

      # Outer sizeof has one argument
      outer_args = CrystalV2::Compiler::Frontend.node_sizeof_args(outer_sizeof).not_nil!
      outer_args.size.should eq(1)

      # That argument is also a sizeof
      inner_sizeof = arena[outer_args[0]]
      CrystalV2::Compiler::Frontend.node_kind(inner_sizeof).should eq(CrystalV2::Compiler::Frontend::NodeKind::Sizeof)

      # Inner sizeof has one argument (Int32)
      inner_args = CrystalV2::Compiler::Frontend.node_sizeof_args(inner_sizeof).not_nil!
      inner_args.size.should eq(1)

      identifier_node = arena[inner_args[0]]
      CrystalV2::Compiler::Frontend.node_kind(identifier_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Identifier)
    end

    it "parses sizeof with method call" do
      source = <<-CRYSTAL
      x = sizeof(foo.bar)
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign_node = arena[program.roots[0]]
      sizeof_expr = CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!
      sizeof_node = arena[sizeof_expr]

      CrystalV2::Compiler::Frontend.node_kind(sizeof_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Sizeof)

      args = CrystalV2::Compiler::Frontend.node_sizeof_args(sizeof_node).not_nil!
      args.size.should eq(1)

      # Argument is member access (foo.bar)
      arg_node = arena[args[0]]
      CrystalV2::Compiler::Frontend.node_kind(arg_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::MemberAccess)
    end

    it "parses sizeof with self" do
      source = <<-CRYSTAL
      class Foo
        def size
          sizeof(self)
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

      # Method body has sizeof
      sizeof_node = arena[method_body[0]]
      CrystalV2::Compiler::Frontend.node_kind(sizeof_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Sizeof)

      args = CrystalV2::Compiler::Frontend.node_sizeof_args(sizeof_node).not_nil!
      args.size.should eq(1)

      # Argument is self
      arg_node = arena[args[0]]
      CrystalV2::Compiler::Frontend.node_kind(arg_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Self)
    end
  end
end
