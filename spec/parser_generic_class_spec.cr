require "spec"

require "../src/compiler/frontend/parser"

describe "CrystalV2::Compiler::Frontend::Parser" do
  describe "Phase 61: Generic class/struct/module definitions (PRODUCTION-READY)" do
    # Class tests

    it "parses generic class with single type parameter" do
      source = <<-CRYSTAL
      class Box(T)
        def initialize(@value : T)
        end
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      klass = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(klass).should eq(CrystalV2::Compiler::Frontend::NodeKind::Class)

      # Check class name
      String.new(CrystalV2::Compiler::Frontend.node_class_name(klass).not_nil!).should eq("Box")

      # Check type parameters
      type_params = klass.as(CrystalV2::Compiler::Frontend::ClassNode).type_params.not_nil!
      type_params.size.should eq(1)
      String.new(type_params[0]).should eq("T")
    end

    it "parses generic class with multiple type parameters" do
      source = <<-CRYSTAL
      class Hash(K, V)
        def initialize
        end
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      klass = arena[program.roots[0]]

      # Check type parameters
      type_params = klass.as(CrystalV2::Compiler::Frontend::ClassNode).type_params.not_nil!
      type_params.size.should eq(2)
      String.new(type_params[0]).should eq("K")
      String.new(type_params[1]).should eq("V")
    end

    it "parses generic class with three type parameters" do
      source = <<-CRYSTAL
      class Triple(A, B, C)
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      klass = arena[program.roots[0]]

      type_params = klass.as(CrystalV2::Compiler::Frontend::ClassNode).type_params.not_nil!
      type_params.size.should eq(3)
      String.new(type_params[0]).should eq("A")
      String.new(type_params[1]).should eq("B")
      String.new(type_params[2]).should eq("C")
    end

    it "parses non-generic class without parentheses" do
      source = <<-CRYSTAL
      class Foo
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      klass = arena[program.roots[0]]
      String.new(CrystalV2::Compiler::Frontend.node_class_name(klass).not_nil!).should eq("Foo")

      # No type parameters
      klass.as(CrystalV2::Compiler::Frontend::ClassNode).type_params.should be_nil
    end

    # Struct tests

    it "parses generic struct with single type parameter" do
      source = <<-CRYSTAL
      struct Point(T)
        def initialize(@x : T, @y : T)
        end
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      struct_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(struct_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Struct)

      # Check struct name
      String.new(CrystalV2::Compiler::Frontend.node_class_name(struct_node).not_nil!).should eq("Point")

      # Check type parameters
      type_params = struct_node.as(CrystalV2::Compiler::Frontend::ClassNode).type_params.not_nil!
      type_params.size.should eq(1)
      String.new(type_params[0]).should eq("T")
    end

    it "parses generic struct with multiple type parameters" do
      source = <<-CRYSTAL
      struct Pair(K, V)
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      struct_node = arena[program.roots[0]]

      type_params = struct_node.as(CrystalV2::Compiler::Frontend::ClassNode).type_params.not_nil!
      type_params.size.should eq(2)
      String.new(type_params[0]).should eq("K")
      String.new(type_params[1]).should eq("V")
    end

    # Module tests

    it "parses generic module with single type parameter" do
      source = <<-CRYSTAL
      module Enumerable(T)
        def each
        end
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      mod = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(mod).should eq(CrystalV2::Compiler::Frontend::NodeKind::Module)

      # Check module name
      String.new(CrystalV2::Compiler::Frontend.node_module_name(mod).not_nil!).should eq("Enumerable")

      # Check type parameters
      type_params = mod.as(CrystalV2::Compiler::Frontend::ModuleNode).type_params.not_nil!
      type_params.size.should eq(1)
      String.new(type_params[0]).should eq("T")
    end

    it "parses generic module with multiple type parameters" do
      source = <<-CRYSTAL
      module Comparable(T, U)
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      mod = arena[program.roots[0]]

      type_params = mod.as(CrystalV2::Compiler::Frontend::ModuleNode).type_params.not_nil!
      type_params.size.should eq(2)
      String.new(type_params[0]).should eq("T")
      String.new(type_params[1]).should eq("U")
    end

    # Mixed tests

    it "parses generic class with superclass" do
      source = <<-CRYSTAL
      class Array(T) < Parent
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      klass = arena[program.roots[0]]

      # Check type parameters
      type_params = klass.as(CrystalV2::Compiler::Frontend::ClassNode).type_params.not_nil!
      type_params.size.should eq(1)
      String.new(type_params[0]).should eq("T")

      # Check superclass
      String.new(CrystalV2::Compiler::Frontend.node_class_super_name(klass).not_nil!).should eq("Parent")
    end

    it "parses abstract generic class" do
      source = <<-CRYSTAL
      abstract class Base(T)
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      klass = arena[program.roots[0]]

      # Check abstract flag
      CrystalV2::Compiler::Frontend.node_class_is_abstract(klass).should be_true

      # Check type parameters
      type_params = klass.as(CrystalV2::Compiler::Frontend::ClassNode).type_params.not_nil!
      type_params.size.should eq(1)
      String.new(type_params[0]).should eq("T")
    end

    it "parses multiple generic definitions" do
      source = <<-CRYSTAL
      class Box(T)
      end

      struct Point(T)
      end

      module Enumerable(T)
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(3)
      arena = program.arena

      # Check all have type parameters
      klass = arena[program.roots[0]]
      klass.as(CrystalV2::Compiler::Frontend::ClassNode).type_params.not_nil!.size.should eq(1)

      struct_node = arena[program.roots[1]]
      struct_node.as(CrystalV2::Compiler::Frontend::ClassNode).type_params.not_nil!.size.should eq(1)

      mod = arena[program.roots[2]]
      mod.as(CrystalV2::Compiler::Frontend::ModuleNode).type_params.not_nil!.size.should eq(1)
    end

    it "parses nested generic classes" do
      source = <<-CRYSTAL
      class Outer(T)
        class Inner(U)
        end
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      outer = arena[program.roots[0]]
      String.new(CrystalV2::Compiler::Frontend.node_class_name(outer).not_nil!).should eq("Outer")
      type_params = outer.as(CrystalV2::Compiler::Frontend::ClassNode).type_params.not_nil!
      type_params.size.should eq(1)
      String.new(type_params[0]).should eq("T")

      # Check inner class
      inner_id = CrystalV2::Compiler::Frontend.node_class_body(outer).not_nil![0]
      inner = arena[inner_id].as(CrystalV2::Compiler::Frontend::ClassNode)
      String.new(CrystalV2::Compiler::Frontend.node_class_name(inner).not_nil!).should eq("Inner")
      inner_params = inner.type_params.not_nil!
      inner_params.size.should eq(1)
      String.new(inner_params[0]).should eq("U")
    end
  end
end
