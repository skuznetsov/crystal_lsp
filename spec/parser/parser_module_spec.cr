require "spec"

require "../../src/compiler/frontend/parser"

describe "CrystalV2::Compiler::Frontend::Parser" do
  describe "Phase 31: Module/Include/Extend (PRODUCTION-READY)" do
    it "parses empty module" do
      source = <<-CRYSTAL
        module Math
        end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      module_node = arena[program.roots.first]
      CrystalV2::Compiler::Frontend.node_kind(module_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Module)

      module_name = String.new(CrystalV2::Compiler::Frontend.node_module_name(module_node).not_nil!)
      module_name.should eq("Math")

      module_body = CrystalV2::Compiler::Frontend.node_module_body(module_node).not_nil!
      module_body.size.should eq(0)
    end

    it "parses module with method" do
      source = <<-CRYSTAL
        module Math
          def add(a, b)
            a + b
          end
        end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      arena = program.arena
      module_node = arena[program.roots.first]
      CrystalV2::Compiler::Frontend.node_kind(module_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Module)

      module_body = CrystalV2::Compiler::Frontend.node_module_body(module_node).not_nil!
      module_body.size.should eq(1)

      method_node = arena[module_body[0]]
      CrystalV2::Compiler::Frontend.node_kind(method_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Def)
    end

    it "parses module with nested class" do
      source = <<-CRYSTAL
        module Collections
          class List
            def initialize
            end
          end
        end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      arena = program.arena
      module_node = arena[program.roots.first]
      CrystalV2::Compiler::Frontend.node_kind(module_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Module)

      module_body = CrystalV2::Compiler::Frontend.node_module_body(module_node).not_nil!
      module_body.size.should eq(1)

      class_node = arena[module_body[0]]
      CrystalV2::Compiler::Frontend.node_kind(class_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Class)
    end

    it "parses module with nested module" do
      source = <<-CRYSTAL
        module Outer
          module Inner
            def helper
            end
          end
        end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      arena = program.arena
      module_node = arena[program.roots.first]
      CrystalV2::Compiler::Frontend.node_kind(module_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Module)

      module_body = CrystalV2::Compiler::Frontend.node_module_body(module_node).not_nil!
      module_body.size.should eq(1)

      inner_module = arena[module_body[0]]
      CrystalV2::Compiler::Frontend.node_kind(inner_module).should eq(CrystalV2::Compiler::Frontend::NodeKind::Module)
    end

    it "parses include in class" do
      source = <<-CRYSTAL
        class Person
          include Comparable
        end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      arena = program.arena
      class_node = arena[program.roots.first]
      CrystalV2::Compiler::Frontend.node_kind(class_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Class)

      class_body = CrystalV2::Compiler::Frontend.node_class_body(class_node).not_nil!
      class_body.size.should eq(1)

      include_node = arena[class_body[0]]
      CrystalV2::Compiler::Frontend.node_kind(include_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Include)

      include_name = String.new(CrystalV2::Compiler::Frontend.node_include_name(include_node).not_nil!)
      include_name.should eq("Comparable")
    end

    it "parses extend in class" do
      source = <<-CRYSTAL
        class Person
          extend Enumerable
        end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      arena = program.arena
      class_node = arena[program.roots.first]
      CrystalV2::Compiler::Frontend.node_kind(class_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Class)

      class_body = CrystalV2::Compiler::Frontend.node_class_body(class_node).not_nil!
      class_body.size.should eq(1)

      extend_node = arena[class_body[0]]
      CrystalV2::Compiler::Frontend.node_kind(extend_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Extend)

      extend_name = String.new(CrystalV2::Compiler::Frontend.node_extend_name(extend_node).not_nil!)
      extend_name.should eq("Enumerable")
    end

    it "parses multiple includes and extends" do
      source = <<-CRYSTAL
        class Person
          include Comparable
          include Serializable
          extend ClassMethods
        end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      arena = program.arena
      class_node = arena[program.roots.first]
      class_body = CrystalV2::Compiler::Frontend.node_class_body(class_node).not_nil!
      class_body.size.should eq(3)

      # First include
      include1 = arena[class_body[0]]
      CrystalV2::Compiler::Frontend.node_kind(include1).should eq(CrystalV2::Compiler::Frontend::NodeKind::Include)
      String.new(CrystalV2::Compiler::Frontend.node_include_name(include1).not_nil!).should eq("Comparable")

      # Second include
      include2 = arena[class_body[1]]
      CrystalV2::Compiler::Frontend.node_kind(include2).should eq(CrystalV2::Compiler::Frontend::NodeKind::Include)
      String.new(CrystalV2::Compiler::Frontend.node_include_name(include2).not_nil!).should eq("Serializable")

      # Extend
      extend_node = arena[class_body[2]]
      CrystalV2::Compiler::Frontend.node_kind(extend_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Extend)
      String.new(CrystalV2::Compiler::Frontend.node_extend_name(extend_node).not_nil!).should eq("ClassMethods")
    end

    it "parses include in module" do
      source = <<-CRYSTAL
        module MyModule
          include BaseModule

          def helper
          end
        end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      arena = program.arena
      module_node = arena[program.roots.first]
      CrystalV2::Compiler::Frontend.node_kind(module_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Module)

      module_body = CrystalV2::Compiler::Frontend.node_module_body(module_node).not_nil!
      module_body.size.should eq(2)

      # Include
      include_node = arena[module_body[0]]
      CrystalV2::Compiler::Frontend.node_kind(include_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Include)
      String.new(CrystalV2::Compiler::Frontend.node_include_name(include_node).not_nil!).should eq("BaseModule")

      # Method
      method_node = arena[module_body[1]]
      CrystalV2::Compiler::Frontend.node_kind(method_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Def)
    end

    it "parses include with namespace" do
      source = <<-CRYSTAL
        class Example
          include JSON::Serializable
        end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program
      arena = program.arena

      class_node = arena[program.roots.first]
      body = CrystalV2::Compiler::Frontend.node_class_body(class_node).not_nil!
      body.size.should eq(1)

      include_node = arena[body[0]]
      CrystalV2::Compiler::Frontend.node_kind(include_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Include)
      target_id = CrystalV2::Compiler::Frontend.node_include_target(include_node)
      target_node = arena[target_id]
      CrystalV2::Compiler::Frontend.node_kind(target_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Path)

      path_node = target_node.as(CrystalV2::Compiler::Frontend::PathNode)
      left_id = path_node.left.not_nil!
      right_id = path_node.right

      left_node = arena[left_id].as(CrystalV2::Compiler::Frontend::IdentifierNode)
      String.new(left_node.name).should eq("JSON")

      right_node = arena[right_id].as(CrystalV2::Compiler::Frontend::IdentifierNode)
      String.new(right_node.name).should eq("Serializable")

      String.new(CrystalV2::Compiler::Frontend.node_include_name(include_node).not_nil!).should eq("Serializable")
    end

    it "parses class with methods and includes" do
      source = <<-CRYSTAL
        class Calculator
          include Math

          def initialize
          end

          def add(a, b)
            a + b
          end

          extend Helpers
        end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      arena = program.arena
      class_node = arena[program.roots.first]
      class_body = CrystalV2::Compiler::Frontend.node_class_body(class_node).not_nil!
      class_body.size.should eq(4)

      # Include
      CrystalV2::Compiler::Frontend.node_kind(arena[class_body[0]]).should eq(CrystalV2::Compiler::Frontend::NodeKind::Include)

      # initialize method
      CrystalV2::Compiler::Frontend.node_kind(arena[class_body[1]]).should eq(CrystalV2::Compiler::Frontend::NodeKind::Def)

      # add method
      CrystalV2::Compiler::Frontend.node_kind(arena[class_body[2]]).should eq(CrystalV2::Compiler::Frontend::NodeKind::Def)

      # Extend
      CrystalV2::Compiler::Frontend.node_kind(arena[class_body[3]]).should eq(CrystalV2::Compiler::Frontend::NodeKind::Extend)
    end
  end
end
