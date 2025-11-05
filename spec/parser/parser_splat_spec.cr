require "spec"

require "../../src/compiler/frontend/parser"

describe "CrystalV2::Compiler::Frontend::Parser" do
  describe "Phase 68: Splat operators (*args, **kwargs) (PRODUCTION-READY)" do
    it "parses method with single splat parameter" do
      source = <<-CRYSTAL
      def foo(*args)
        args
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
      String.new(params[0].name.not_nil!).should eq("args")
      params[0].is_splat.should be_true
      params[0].is_double_splat.should be_false
    end

    it "parses method with double splat parameter" do
      source = <<-CRYSTAL
      def foo(**options)
        options
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
      String.new(params[0].name.not_nil!).should eq("options")
      params[0].is_splat.should be_false
      params[0].is_double_splat.should be_true
    end

    it "parses method with regular and splat parameters" do
      source = <<-CRYSTAL
      def foo(x, y, *rest)
        rest
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      method_node = arena[program.roots[0]]
      params = CrystalV2::Compiler::Frontend.node_def_params(method_node).not_nil!
      params.size.should eq(3)

      String.new(params[0].name.not_nil!).should eq("x")
      params[0].is_splat.should be_false
      params[0].is_double_splat.should be_false

      String.new(params[1].name.not_nil!).should eq("y")
      params[1].is_splat.should be_false
      params[1].is_double_splat.should be_false

      String.new(params[2].name.not_nil!).should eq("rest")
      params[2].is_splat.should be_true
      params[2].is_double_splat.should be_false
    end

    it "parses method with splat and double splat" do
      source = <<-CRYSTAL
      def foo(x, *args, **options)
        args
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      method_node = arena[program.roots[0]]
      params = CrystalV2::Compiler::Frontend.node_def_params(method_node).not_nil!
      params.size.should eq(3)

      String.new(params[0].name.not_nil!).should eq("x")
      params[0].is_splat.should be_false

      String.new(params[1].name.not_nil!).should eq("args")
      params[1].is_splat.should be_true
      params[1].is_double_splat.should be_false

      String.new(params[2].name.not_nil!).should eq("options")
      params[2].is_splat.should be_false
      params[2].is_double_splat.should be_true
    end

    it "parses splat parameter with type annotation" do
      source = <<-CRYSTAL
      def foo(*args : Int32)
        args
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      method_node = arena[program.roots[0]]
      params = CrystalV2::Compiler::Frontend.node_def_params(method_node).not_nil!
      params.size.should eq(1)

      String.new(params[0].name.not_nil!).should eq("args")
      params[0].is_splat.should be_true
      String.new(params[0].type_annotation.not_nil!).should eq("Int32")
    end

    it "parses double splat parameter with type annotation" do
      source = <<-CRYSTAL
      def foo(**options : String)
        options
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      method_node = arena[program.roots[0]]
      params = CrystalV2::Compiler::Frontend.node_def_params(method_node).not_nil!
      params.size.should eq(1)

      String.new(params[0].name.not_nil!).should eq("options")
      params[0].is_double_splat.should be_true
      String.new(params[0].type_annotation.not_nil!).should eq("String")
    end

    it "parses method with only splat (no regular params)" do
      source = <<-CRYSTAL
      def foo(*args)
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      method_node = arena[program.roots[0]]
      params = CrystalV2::Compiler::Frontend.node_def_params(method_node).not_nil!
      params.size.should eq(1)
      params[0].is_splat.should be_true
    end

    it "parses method with only double splat (no regular params)" do
      source = <<-CRYSTAL
      def foo(**kwargs)
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      method_node = arena[program.roots[0]]
      params = CrystalV2::Compiler::Frontend.node_def_params(method_node).not_nil!
      params.size.should eq(1)
      params[0].is_double_splat.should be_true
    end

    it "parses complex signature with all parameter types" do
      source = <<-CRYSTAL
      def foo(a, b : Int32, *rest, **options)
        rest
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      method_node = arena[program.roots[0]]
      params = CrystalV2::Compiler::Frontend.node_def_params(method_node).not_nil!
      params.size.should eq(4)

      String.new(params[0].name.not_nil!).should eq("a")
      params[0].is_splat.should be_false
      params[0].type_annotation.should be_nil

      String.new(params[1].name.not_nil!).should eq("b")
      params[1].is_splat.should be_false
      String.new(params[1].type_annotation.not_nil!).should eq("Int32")

      String.new(params[2].name.not_nil!).should eq("rest")
      params[2].is_splat.should be_true

      String.new(params[3].name.not_nil!).should eq("options")
      params[3].is_double_splat.should be_true
    end

    it "parses splat in method inside class" do
      source = <<-CRYSTAL
      class Foo
        def bar(*args)
          args
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
      params[0].is_splat.should be_true
    end
  end
end
