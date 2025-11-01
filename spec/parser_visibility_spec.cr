require "spec"

require "../src/compiler/frontend/parser"

describe "CrystalV2::Compiler::Frontend::Parser" do
  describe "Phase 37: Visibility modifiers (PRODUCTION-READY)" do
    it "parses private method" do
      source = <<-CRYSTAL
      class MyClass
        private def secret_method
          puts "secret"
        end
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena
      class_node = arena[program.roots.first]

      class_body = CrystalV2::Compiler::Frontend.node_class_body(class_node).not_nil!
      class_body.size.should eq(1)

      method_node = arena[class_body[0]]
      CrystalV2::Compiler::Frontend.node_kind(method_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Def)
      String.new(CrystalV2::Compiler::Frontend.node_def_name(method_node).not_nil!).should eq("secret_method")
      CrystalV2::Compiler::Frontend.node_def_visibility(method_node).should eq(CrystalV2::Compiler::Frontend::Visibility::Private)
    end

    it "parses protected method" do
      source = <<-CRYSTAL
      class MyClass
        protected def helper_method
          puts "helper"
        end
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena
      class_node = arena[program.roots.first]

      class_body = CrystalV2::Compiler::Frontend.node_class_body(class_node).not_nil!
      method_node = arena[class_body[0]]

      String.new(CrystalV2::Compiler::Frontend.node_def_name(method_node).not_nil!).should eq("helper_method")
      CrystalV2::Compiler::Frontend.node_def_visibility(method_node).should eq(CrystalV2::Compiler::Frontend::Visibility::Protected)
    end

    it "parses public method (default)" do
      source = <<-CRYSTAL
      class MyClass
        def public_method
          puts "public"
        end
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena
      class_node = arena[program.roots.first]

      class_body = CrystalV2::Compiler::Frontend.node_class_body(class_node).not_nil!
      method_node = arena[class_body[0]]

      String.new(CrystalV2::Compiler::Frontend.node_def_name(method_node).not_nil!).should eq("public_method")
      CrystalV2::Compiler::Frontend.node_def_visibility(method_node).should be_nil  # nil = public (default)
    end

    it "parses mixed visibility methods" do
      source = <<-CRYSTAL
      class MyClass
        def public_method
        end

        private def private_method
        end

        protected def protected_method
        end
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena
      class_node = arena[program.roots.first]

      class_body = CrystalV2::Compiler::Frontend.node_class_body(class_node).not_nil!
      class_body.size.should eq(3)

      # Public method
      public_method = arena[class_body[0]]
      CrystalV2::Compiler::Frontend.node_def_visibility(public_method).should be_nil

      # Private method
      private_method = arena[class_body[1]]
      CrystalV2::Compiler::Frontend.node_def_visibility(private_method).should eq(CrystalV2::Compiler::Frontend::Visibility::Private)

      # Protected method
      protected_method = arena[class_body[2]]
      CrystalV2::Compiler::Frontend.node_def_visibility(protected_method).should eq(CrystalV2::Compiler::Frontend::Visibility::Protected)
    end

    it "parses private method with parameters" do
      source = <<-CRYSTAL
      class MyClass
        private def calculate(x : Int32, y : Int32) : Int32
          x + y
        end
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena
      class_node = arena[program.roots.first]

      class_body = CrystalV2::Compiler::Frontend.node_class_body(class_node).not_nil!
      method_node = arena[class_body[0]]

      CrystalV2::Compiler::Frontend.node_def_visibility(method_node).should eq(CrystalV2::Compiler::Frontend::Visibility::Private)
      params = CrystalV2::Compiler::Frontend.node_def_params(method_node).not_nil!
      params.size.should eq(2)
    end

    it "parses private method in module" do
      source = <<-CRYSTAL
      module MyModule
        private def helper
          puts "private helper"
        end
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena
      module_node = arena[program.roots.first]

      module_body = CrystalV2::Compiler::Frontend.node_module_body(module_node).not_nil!
      method_node = arena[module_body[0]]

      CrystalV2::Compiler::Frontend.node_def_visibility(method_node).should eq(CrystalV2::Compiler::Frontend::Visibility::Private)
    end

    it "parses nested class with visibility" do
      source = <<-CRYSTAL
      class Outer
        class Inner
          private def secret
          end
        end
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena
      outer_class = arena[program.roots.first]

      outer_body = CrystalV2::Compiler::Frontend.node_class_body(outer_class).not_nil!
      inner_class = arena[outer_body[0]]

      inner_body = CrystalV2::Compiler::Frontend.node_class_body(inner_class).not_nil!
      method_node = arena[inner_body[0]]

      CrystalV2::Compiler::Frontend.node_def_visibility(method_node).should eq(CrystalV2::Compiler::Frontend::Visibility::Private)
    end

    it "distinguishes visibility from method body" do
      source = <<-CRYSTAL
      class MyClass
        private def secret_method
          puts "I have a body"
        end

        def public_method
          puts "I also have a body"
        end
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena
      class_node = arena[program.roots.first]

      class_body = CrystalV2::Compiler::Frontend.node_class_body(class_node).not_nil!

      # Private method still has body
      private_method = arena[class_body[0]]
      CrystalV2::Compiler::Frontend.node_def_visibility(private_method).should eq(CrystalV2::Compiler::Frontend::Visibility::Private)
      CrystalV2::Compiler::Frontend.node_def_body(private_method).should_not be_nil
      CrystalV2::Compiler::Frontend.node_def_body(private_method).not_nil!.size.should be > 0

      # Public method has body
      public_method = arena[class_body[1]]
      CrystalV2::Compiler::Frontend.node_def_visibility(public_method).should be_nil
      CrystalV2::Compiler::Frontend.node_def_body(public_method).should_not be_nil
    end

    it "parses private method at top level" do
      source = <<-CRYSTAL
      private def top_level_private
        puts "private at top level"
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena
      method_node = arena[program.roots.first]

      CrystalV2::Compiler::Frontend.node_kind(method_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Def)
      CrystalV2::Compiler::Frontend.node_def_visibility(method_node).should eq(CrystalV2::Compiler::Frontend::Visibility::Private)
    end
  end
end
