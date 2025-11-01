require "spec"

require "../src/compiler/frontend/parser"

describe "CrystalV2::Compiler::Frontend::Parser" do
  describe "Phase 88: alignof (ABI alignment in bytes)" do
    it "parses alignof with type identifier" do
      source = <<-CRYSTAL
      x = alignof(Int32)
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(assign_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)

      # Value side is alignof
      alignof_expr = CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!
      alignof_node = arena[alignof_expr]
      CrystalV2::Compiler::Frontend.node_kind(alignof_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Alignof)

      # Check arguments
      args = CrystalV2::Compiler::Frontend.node_alignof_args(alignof_node).not_nil!
      args.size.should eq(1)

      # Argument is identifier Int32
      arg_node = arena[args[0]]
      CrystalV2::Compiler::Frontend.node_kind(arg_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Identifier)
      String.new(CrystalV2::Compiler::Frontend.node_literal(arg_node).not_nil!).should eq("Int32")
    end

    it "parses alignof with Int64" do
      source = <<-CRYSTAL
      x = alignof(Int64)
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign_node = arena[program.roots[0]]
      alignof_expr = CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!
      alignof_node = arena[alignof_expr]

      CrystalV2::Compiler::Frontend.node_kind(alignof_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Alignof)

      args = CrystalV2::Compiler::Frontend.node_alignof_args(alignof_node).not_nil!
      args.size.should eq(1)

      arg_node = arena[args[0]]
      String.new(CrystalV2::Compiler::Frontend.node_literal(arg_node).not_nil!).should eq("Int64")
    end

    it "parses alignof with String" do
      source = <<-CRYSTAL
      x = alignof(String)
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign_node = arena[program.roots[0]]
      alignof_expr = CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!
      alignof_node = arena[alignof_expr]

      CrystalV2::Compiler::Frontend.node_kind(alignof_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Alignof)
      args = CrystalV2::Compiler::Frontend.node_alignof_args(alignof_node).not_nil!
      args.size.should eq(1)
    end

    it "parses alignof in method definition" do
      source = <<-CRYSTAL
      def foo
        alignof(Int32)
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      method_node = arena[program.roots[0]]
      method_body = CrystalV2::Compiler::Frontend.node_def_body(method_node).not_nil!
      method_body.size.should eq(1)

      # Method body is alignof
      alignof_node = arena[method_body[0]]
      CrystalV2::Compiler::Frontend.node_kind(alignof_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Alignof)
    end

    it "parses alignof in class" do
      source = <<-CRYSTAL
      class Foo
        def bar
          alignof(Int64)
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

      # Method body is alignof
      alignof_node = arena[method_body[0]]
      CrystalV2::Compiler::Frontend.node_kind(alignof_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Alignof)
    end

    it "parses nested alignof" do
      source = <<-CRYSTAL
      x = alignof(alignof(Int32))
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign_node = arena[program.roots[0]]
      outer_alignof_expr = CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!
      outer_alignof = arena[outer_alignof_expr]

      CrystalV2::Compiler::Frontend.node_kind(outer_alignof).should eq(CrystalV2::Compiler::Frontend::NodeKind::Alignof)

      # Outer alignof has one argument
      outer_args = CrystalV2::Compiler::Frontend.node_alignof_args(outer_alignof).not_nil!
      outer_args.size.should eq(1)

      # That argument is also an alignof
      inner_alignof = arena[outer_args[0]]
      CrystalV2::Compiler::Frontend.node_kind(inner_alignof).should eq(CrystalV2::Compiler::Frontend::NodeKind::Alignof)
    end

    it "parses alignof in assignment" do
      source = <<-CRYSTAL
      a = 1
      b = alignof(Int32)
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(2)
      arena = program.arena

      # Second statement is alignof
      assign_node = arena[program.roots[1]]
      alignof_expr = CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!
      alignof_node = arena[alignof_expr]

      CrystalV2::Compiler::Frontend.node_kind(alignof_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Alignof)
    end

    it "parses alignof as expression" do
      source = <<-CRYSTAL
      x = 1 + alignof(Int32)
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign_node = arena[program.roots[0]]
      binary_expr = CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!
      binary_node = arena[binary_expr]

      # Right side of + is alignof
      right_expr = CrystalV2::Compiler::Frontend.node_right(binary_node).not_nil!
      alignof_node = arena[right_expr]
      CrystalV2::Compiler::Frontend.node_kind(alignof_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Alignof)
    end

    it "parses alignof with custom type" do
      source = <<-CRYSTAL
      class MyClass
      end

      x = alignof(MyClass)
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(2)
      arena = program.arena

      # Second statement is assignment with alignof
      assign_node = arena[program.roots[1]]
      alignof_expr = CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!
      alignof_node = arena[alignof_expr]

      CrystalV2::Compiler::Frontend.node_kind(alignof_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Alignof)

      args = CrystalV2::Compiler::Frontend.node_alignof_args(alignof_node).not_nil!
      args.size.should eq(1)

      arg_node = arena[args[0]]
      String.new(CrystalV2::Compiler::Frontend.node_literal(arg_node).not_nil!).should eq("MyClass")
    end
  end

  describe "Phase 88: instance_alignof (instance alignment)" do
    it "parses instance_alignof with type identifier" do
      source = <<-CRYSTAL
      x = instance_alignof(Int32)
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(assign_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)

      # Value side is instance_alignof
      instance_alignof_expr = CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!
      instance_alignof_node = arena[instance_alignof_expr]
      CrystalV2::Compiler::Frontend.node_kind(instance_alignof_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::InstanceAlignof)

      # Check arguments
      args = CrystalV2::Compiler::Frontend.node_alignof_args(instance_alignof_node).not_nil!
      args.size.should eq(1)

      # Argument is identifier Int32
      arg_node = arena[args[0]]
      CrystalV2::Compiler::Frontend.node_kind(arg_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Identifier)
      String.new(CrystalV2::Compiler::Frontend.node_literal(arg_node).not_nil!).should eq("Int32")
    end

    it "parses instance_alignof with String" do
      source = <<-CRYSTAL
      x = instance_alignof(String)
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign_node = arena[program.roots[0]]
      instance_alignof_expr = CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!
      instance_alignof_node = arena[instance_alignof_expr]

      CrystalV2::Compiler::Frontend.node_kind(instance_alignof_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::InstanceAlignof)

      args = CrystalV2::Compiler::Frontend.node_alignof_args(instance_alignof_node).not_nil!
      args.size.should eq(1)

      arg_node = arena[args[0]]
      String.new(CrystalV2::Compiler::Frontend.node_literal(arg_node).not_nil!).should eq("String")
    end

    it "parses instance_alignof with custom class" do
      source = <<-CRYSTAL
      class Foo
      end

      x = instance_alignof(Foo)
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(2)
      arena = program.arena

      # Second statement is assignment with instance_alignof
      assign_node = arena[program.roots[1]]
      instance_alignof_expr = CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!
      instance_alignof_node = arena[instance_alignof_expr]

      CrystalV2::Compiler::Frontend.node_kind(instance_alignof_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::InstanceAlignof)

      args = CrystalV2::Compiler::Frontend.node_alignof_args(instance_alignof_node).not_nil!
      args.size.should eq(1)

      arg_node = arena[args[0]]
      String.new(CrystalV2::Compiler::Frontend.node_literal(arg_node).not_nil!).should eq("Foo")
    end

    it "parses instance_alignof in method definition" do
      source = <<-CRYSTAL
      def foo
        instance_alignof(Int64)
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      method_node = arena[program.roots[0]]
      method_body = CrystalV2::Compiler::Frontend.node_def_body(method_node).not_nil!
      method_body.size.should eq(1)

      # Method body is instance_alignof
      instance_alignof_node = arena[method_body[0]]
      CrystalV2::Compiler::Frontend.node_kind(instance_alignof_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::InstanceAlignof)
    end

    it "parses instance_alignof in class" do
      source = <<-CRYSTAL
      class Foo
        def bar
          instance_alignof(String)
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

      # Method body is instance_alignof
      instance_alignof_node = arena[method_body[0]]
      CrystalV2::Compiler::Frontend.node_kind(instance_alignof_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::InstanceAlignof)
    end

    it "parses nested instance_alignof" do
      source = <<-CRYSTAL
      x = instance_alignof(instance_alignof(Int32))
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign_node = arena[program.roots[0]]
      outer_instance_alignof_expr = CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!
      outer_instance_alignof = arena[outer_instance_alignof_expr]

      CrystalV2::Compiler::Frontend.node_kind(outer_instance_alignof).should eq(CrystalV2::Compiler::Frontend::NodeKind::InstanceAlignof)

      # Outer instance_alignof has one argument
      outer_args = CrystalV2::Compiler::Frontend.node_alignof_args(outer_instance_alignof).not_nil!
      outer_args.size.should eq(1)

      # That argument is also an instance_alignof
      inner_instance_alignof = arena[outer_args[0]]
      CrystalV2::Compiler::Frontend.node_kind(inner_instance_alignof).should eq(CrystalV2::Compiler::Frontend::NodeKind::InstanceAlignof)
    end

    it "parses instance_alignof in assignment" do
      source = <<-CRYSTAL
      a = 1
      b = instance_alignof(Int32)
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(2)
      arena = program.arena

      # Second statement is instance_alignof
      assign_node = arena[program.roots[1]]
      instance_alignof_expr = CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!
      instance_alignof_node = arena[instance_alignof_expr]

      CrystalV2::Compiler::Frontend.node_kind(instance_alignof_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::InstanceAlignof)
    end

    it "parses instance_alignof as expression" do
      source = <<-CRYSTAL
      x = 1 + instance_alignof(Int32)
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign_node = arena[program.roots[0]]
      binary_expr = CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!
      binary_node = arena[binary_expr]

      # Right side of + is instance_alignof
      right_expr = CrystalV2::Compiler::Frontend.node_right(binary_node).not_nil!
      instance_alignof_node = arena[right_expr]
      CrystalV2::Compiler::Frontend.node_kind(instance_alignof_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::InstanceAlignof)
    end

    it "parses mixed alignof and instance_alignof" do
      source = <<-CRYSTAL
      x = alignof(Int32) + instance_alignof(String)
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign_node = arena[program.roots[0]]
      binary_expr = CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!
      binary_node = arena[binary_expr]

      # Left side is alignof
      left_expr = CrystalV2::Compiler::Frontend.node_left(binary_node).not_nil!
      left_node = arena[left_expr]
      CrystalV2::Compiler::Frontend.node_kind(left_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Alignof)

      # Right side is instance_alignof
      right_expr = CrystalV2::Compiler::Frontend.node_right(binary_node).not_nil!
      right_node = arena[right_expr]
      CrystalV2::Compiler::Frontend.node_kind(right_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::InstanceAlignof)
    end
  end
end
