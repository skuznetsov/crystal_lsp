require "spec"

require "../src/compiler/frontend/parser"

describe "CrystalV2::Compiler::Frontend::Parser" do
  describe "Phase 71: Default parameter values (PRODUCTION-READY)" do
    it "parses method with single default parameter" do
      source = <<-CRYSTAL
      def foo(x = 5)
        x
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      method_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(method_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Def)

      params = CrystalV2::Compiler::Frontend.node_def_params(method_node).not_nil!
      params.size.should eq(1)
      String.new(params[0].name).should eq("x")
      params[0].default_value.should_not be_nil
    end

    it "parses method with type annotation and default value" do
      source = <<-CRYSTAL
      def foo(x : Int32 = 10)
        x
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      method_node = arena[program.roots[0]]
      params = CrystalV2::Compiler::Frontend.node_def_params(method_node).not_nil!
      params.size.should eq(1)
      String.new(params[0].name).should eq("x")
      String.new(params[0].type_annotation.not_nil!).should eq("Int32")
      params[0].default_value.should_not be_nil
    end

    it "parses method with multiple default parameters" do
      source = <<-CRYSTAL
      def foo(x = 1, y = 2, z = 3)
        x + y + z
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      method_node = arena[program.roots[0]]
      params = CrystalV2::Compiler::Frontend.node_def_params(method_node).not_nil!
      params.size.should eq(3)
      String.new(params[0].name).should eq("x")
      params[0].default_value.should_not be_nil
      String.new(params[1].name).should eq("y")
      params[1].default_value.should_not be_nil
      String.new(params[2].name).should eq("z")
      params[2].default_value.should_not be_nil
    end

    it "parses method with mixed required and default parameters" do
      source = <<-CRYSTAL
      def foo(a, b = 2, c = 3)
        a + b + c
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      method_node = arena[program.roots[0]]
      params = CrystalV2::Compiler::Frontend.node_def_params(method_node).not_nil!
      params.size.should eq(3)
      String.new(params[0].name).should eq("a")
      params[0].default_value.should be_nil
      String.new(params[1].name).should eq("b")
      params[1].default_value.should_not be_nil
      String.new(params[2].name).should eq("c")
      params[2].default_value.should_not be_nil
    end

    it "parses method with expression as default value" do
      source = <<-CRYSTAL
      def foo(x = 1 + 1)
        x
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      method_node = arena[program.roots[0]]
      params = CrystalV2::Compiler::Frontend.node_def_params(method_node).not_nil!
      params.size.should eq(1)

      default_value = params[0].default_value.not_nil!
      default_node = arena[default_value]
      CrystalV2::Compiler::Frontend.node_kind(default_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
    end

    it "parses method with identifier as default value" do
      source = <<-CRYSTAL
      def foo(x = y)
        x
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      method_node = arena[program.roots[0]]
      params = CrystalV2::Compiler::Frontend.node_def_params(method_node).not_nil!
      params.size.should eq(1)

      default_value = params[0].default_value.not_nil!
      default_node = arena[default_value]
      CrystalV2::Compiler::Frontend.node_kind(default_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Identifier)
    end

    it "parses method with string literal as default value" do
      source = <<-CRYSTAL
      def foo(name = "default")
        name
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      method_node = arena[program.roots[0]]
      params = CrystalV2::Compiler::Frontend.node_def_params(method_node).not_nil!
      params.size.should eq(1)

      default_value = params[0].default_value.not_nil!
      default_node = arena[default_value]
      CrystalV2::Compiler::Frontend.node_kind(default_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::String)
    end

    it "parses method with array literal as default value" do
      source = <<-CRYSTAL
      def foo(arr = [1, 2, 3])
        arr
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      method_node = arena[program.roots[0]]
      params = CrystalV2::Compiler::Frontend.node_def_params(method_node).not_nil!
      params.size.should eq(1)

      default_value = params[0].default_value.not_nil!
      default_node = arena[default_value]
      CrystalV2::Compiler::Frontend.node_kind(default_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::ArrayLiteral)
    end

    it "parses method with default after splat" do
      source = <<-CRYSTAL
      def foo(x, *args, y = 5)
        y
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      method_node = arena[program.roots[0]]
      params = CrystalV2::Compiler::Frontend.node_def_params(method_node).not_nil!
      params.size.should eq(3)
      String.new(params[0].name).should eq("x")
      params[0].default_value.should be_nil
      String.new(params[1].name).should eq("args")
      params[1].is_splat.should be_true
      String.new(params[2].name).should eq("y")
      params[2].default_value.should_not be_nil
    end

    it "parses method with default and type annotation in mixed order" do
      source = <<-CRYSTAL
      def foo(a : Int32, b = 2, c : String = "hello")
        c
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      method_node = arena[program.roots[0]]
      params = CrystalV2::Compiler::Frontend.node_def_params(method_node).not_nil!
      params.size.should eq(3)

      String.new(params[0].name).should eq("a")
      String.new(params[0].type_annotation.not_nil!).should eq("Int32")
      params[0].default_value.should be_nil

      String.new(params[1].name).should eq("b")
      params[1].type_annotation.should be_nil
      params[1].default_value.should_not be_nil

      String.new(params[2].name).should eq("c")
      String.new(params[2].type_annotation.not_nil!).should eq("String")
      params[2].default_value.should_not be_nil
    end

    it "parses method inside class with default parameters" do
      source = <<-CRYSTAL
      class Foo
        def bar(x = 10)
          x
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

      method_node = arena[class_body[0]]
      CrystalV2::Compiler::Frontend.node_kind(method_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Def)

      params = CrystalV2::Compiler::Frontend.node_def_params(method_node).not_nil!
      params.size.should eq(1)
      params[0].default_value.should_not be_nil
    end

    it "parses method with nil as default value" do
      source = <<-CRYSTAL
      def foo(x = nil)
        x
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      method_node = arena[program.roots[0]]
      params = CrystalV2::Compiler::Frontend.node_def_params(method_node).not_nil!
      params.size.should eq(1)

      default_value = params[0].default_value.not_nil!
      default_node = arena[default_value]
      CrystalV2::Compiler::Frontend.node_kind(default_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Nil)
    end

    it "parses method with boolean as default value" do
      source = <<-CRYSTAL
      def foo(flag = true)
        flag
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      method_node = arena[program.roots[0]]
      params = CrystalV2::Compiler::Frontend.node_def_params(method_node).not_nil!
      params.size.should eq(1)

      default_value = params[0].default_value.not_nil!
      default_node = arena[default_value]
      CrystalV2::Compiler::Frontend.node_kind(default_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Bool)
    end

    it "parses method with complex expression as default value" do
      source = <<-CRYSTAL
      def foo(x = 1 + 2 * 3)
        x
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      method_node = arena[program.roots[0]]
      params = CrystalV2::Compiler::Frontend.node_def_params(method_node).not_nil!
      params.size.should eq(1)

      default_value = params[0].default_value.not_nil!
      default_node = arena[default_value]
      # Complex expression: 1 + (2 * 3)
      CrystalV2::Compiler::Frontend.node_kind(default_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
    end
  end
end
