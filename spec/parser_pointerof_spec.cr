require "spec"

require "../src/compiler/frontend/parser"

describe "CrystalV2::Compiler::Frontend::Parser" do
  describe "Phase 42: pointerof (pointer to variable/expression) (PRODUCTION-READY)" do
    it "parses pointerof with type identifier" do
      source = <<-CRYSTAL
      x = pointerof(Int32)
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(assign_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)

      # Value side is pointerof
      pointerof_expr = CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!
      pointerof_node = arena[pointerof_expr]
      CrystalV2::Compiler::Frontend.node_kind(pointerof_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Pointerof)

      # Check arguments
      args = CrystalV2::Compiler::Frontend.node_pointerof_args(pointerof_node).not_nil!
      args.size.should eq(1)

      # Argument is identifier Int32
      arg_node = arena[args[0]]
      CrystalV2::Compiler::Frontend.node_kind(arg_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Identifier)
      String.new(CrystalV2::Compiler::Frontend.node_literal(arg_node).not_nil!).should eq("Int32")
    end

    it "parses pointerof with variable" do
      source = <<-CRYSTAL
      x = 1
      y = pointerof(x)
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(2)
      arena = program.arena

      # Second statement is assignment with pointerof
      assign_node = arena[program.roots[1]]
      pointerof_expr = CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!
      pointerof_node = arena[pointerof_expr]

      CrystalV2::Compiler::Frontend.node_kind(pointerof_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Pointerof)

      args = CrystalV2::Compiler::Frontend.node_pointerof_args(pointerof_node).not_nil!
      args.size.should eq(1)

      # Argument is identifier x
      arg_node = arena[args[0]]
      CrystalV2::Compiler::Frontend.node_kind(arg_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Identifier)
      String.new(CrystalV2::Compiler::Frontend.node_literal(arg_node).not_nil!).should eq("x")
    end

    it "parses pointerof with expression" do
      source = <<-CRYSTAL
      x = pointerof(1 + 2)
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign_node = arena[program.roots[0]]
      pointerof_expr = CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!
      pointerof_node = arena[pointerof_expr]

      CrystalV2::Compiler::Frontend.node_kind(pointerof_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Pointerof)

      args = CrystalV2::Compiler::Frontend.node_pointerof_args(pointerof_node).not_nil!
      args.size.should eq(1)

      # Argument is binary expression
      arg_node = arena[args[0]]
      CrystalV2::Compiler::Frontend.node_kind(arg_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
    end

    it "parses pointerof with array literal" do
      source = <<-CRYSTAL
      x = pointerof([1, 2, 3])
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign_node = arena[program.roots[0]]
      pointerof_expr = CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!
      pointerof_node = arena[pointerof_expr]

      CrystalV2::Compiler::Frontend.node_kind(pointerof_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Pointerof)

      args = CrystalV2::Compiler::Frontend.node_pointerof_args(pointerof_node).not_nil!
      args.size.should eq(1)

      # Argument is array literal
      arg_node = arena[args[0]]
      CrystalV2::Compiler::Frontend.node_kind(arg_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::ArrayLiteral)
    end

    it "parses pointerof in method definition" do
      source = <<-CRYSTAL
      def foo
        x = 1
        pointerof(x)
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      method_node = arena[program.roots[0]]
      method_body = CrystalV2::Compiler::Frontend.node_def_body(method_node).not_nil!
      method_body.size.should eq(2)

      # Last statement is pointerof
      pointerof_node = arena[method_body[1]]
      CrystalV2::Compiler::Frontend.node_kind(pointerof_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Pointerof)
    end

    it "parses pointerof in class" do
      source = <<-CRYSTAL
      class Foo
        def bar
          x = 1
          pointerof(x)
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

      # Last statement is pointerof
      pointerof_node = arena[method_body[1]]
      CrystalV2::Compiler::Frontend.node_kind(pointerof_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Pointerof)
    end

    it "parses nested pointerof" do
      source = <<-CRYSTAL
      x = pointerof(pointerof(Int32))
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign_node = arena[program.roots[0]]
      outer_pointerof_expr = CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!
      outer_pointerof = arena[outer_pointerof_expr]

      CrystalV2::Compiler::Frontend.node_kind(outer_pointerof).should eq(CrystalV2::Compiler::Frontend::NodeKind::Pointerof)

      # Outer pointerof has one argument
      outer_args = CrystalV2::Compiler::Frontend.node_pointerof_args(outer_pointerof).not_nil!
      outer_args.size.should eq(1)

      # That argument is also a pointerof
      inner_pointerof = arena[outer_args[0]]
      CrystalV2::Compiler::Frontend.node_kind(inner_pointerof).should eq(CrystalV2::Compiler::Frontend::NodeKind::Pointerof)

      # Inner pointerof has one argument (Int32)
      inner_args = CrystalV2::Compiler::Frontend.node_pointerof_args(inner_pointerof).not_nil!
      inner_args.size.should eq(1)

      identifier_node = arena[inner_args[0]]
      CrystalV2::Compiler::Frontend.node_kind(identifier_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Identifier)
    end

    it "parses pointerof with method call" do
      source = <<-CRYSTAL
      x = pointerof(foo.bar)
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign_node = arena[program.roots[0]]
      pointerof_expr = CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!
      pointerof_node = arena[pointerof_expr]

      CrystalV2::Compiler::Frontend.node_kind(pointerof_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Pointerof)

      args = CrystalV2::Compiler::Frontend.node_pointerof_args(pointerof_node).not_nil!
      args.size.should eq(1)

      # Argument is member access (foo.bar)
      arg_node = arena[args[0]]
      CrystalV2::Compiler::Frontend.node_kind(arg_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::MemberAccess)
    end

    it "parses pointerof with self" do
      source = <<-CRYSTAL
      class Foo
        def address
          pointerof(self)
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

      # Method body has pointerof
      pointerof_node = arena[method_body[0]]
      CrystalV2::Compiler::Frontend.node_kind(pointerof_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Pointerof)

      args = CrystalV2::Compiler::Frontend.node_pointerof_args(pointerof_node).not_nil!
      args.size.should eq(1)

      # Argument is self
      arg_node = arena[args[0]]
      CrystalV2::Compiler::Frontend.node_kind(arg_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Self)
    end
  end
end
