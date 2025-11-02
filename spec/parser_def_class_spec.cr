require "spec"

require "../src/compiler/frontend/parser"

describe CrystalV2::Compiler::Frontend::Parser do
  it "parses simple def with params and body" do
    source = <<-CR
      def greet(name)
        name
      end
    CR

    parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
    program = parser.parse_program

    program.roots.size.should eq(1)
    def_node = program.arena[program.roots.first].as(CrystalV2::Compiler::Frontend::DefNode)
    String.new(def_node.name).should eq("greet")
    def_node.params.not_nil!.map(&.name).should eq(["name"])
    def_node.body.not_nil!.size.should eq(1)
  end

  it "parses simple class with body" do
    source = <<-CR
      class Greeter
        greet(name)
      end
    CR

    parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
    program = parser.parse_program

    program.roots.size.should eq(1)
    class_node = program.arena[program.roots.first].as(CrystalV2::Compiler::Frontend::ClassNode)
    String.new(class_node.name).should eq("Greeter")
    class_node.body.not_nil!.size.should eq(1)
  end

  # Phase 4A: Parameter type annotations
  it "parses def with single typed parameter" do
    source = <<-CR
      def add_one(x : Int32)
        x
      end
    CR

    parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
    program = parser.parse_program

    program.roots.size.should eq(1)
    def_node = program.arena[program.roots.first].as(CrystalV2::Compiler::Frontend::DefNode)

    params = def_node.params.not_nil!
    params.size.should eq(1)
    String.new(params[0].name).should eq("x")
    String.new(params[0].type_annotation.not_nil!).should eq("Int32")
  end

  it "parses def with multiple typed parameters" do
    source = <<-CR
      def concat(x : String, y : String)
        x
      end
    CR

    parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
    program = parser.parse_program

    program.roots.size.should eq(1)
    def_node = program.arena[program.roots.first].as(CrystalV2::Compiler::Frontend::DefNode)

    params = def_node.params.not_nil!
    params.size.should eq(2)
    String.new(params[0].name).should eq("x")
    String.new(params[0].type_annotation.not_nil!).should eq("String")
    String.new(params[1].name).should eq("y")
    String.new(params[1].type_annotation.not_nil!).should eq("String")
  end

  it "parses def with mixed typed and untyped parameters" do
    source = <<-CR
      def mixed(x, y : Int32, z)
        x
      end
    CR

    parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
    program = parser.parse_program

    program.roots.size.should eq(1)
    def_node = program.arena[program.roots.first].as(CrystalV2::Compiler::Frontend::DefNode)

    params = def_node.params.not_nil!
    params.size.should eq(3)
    String.new(params[0].name).should eq("x")
    params[0].type_annotation.should be_nil
    String.new(params[1].name).should eq("y")
    String.new(params[1].type_annotation.not_nil!).should eq("Int32")
    String.new(params[2].name).should eq("z")
    params[2].type_annotation.should be_nil
  end

  it "parses def with no parameters" do
    source = <<-CR
      def get_answer
        42
      end
    CR

    parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
    program = parser.parse_program

    program.roots.size.should eq(1)
    def_node = program.arena[program.roots.first].as(CrystalV2::Compiler::Frontend::DefNode)

    params = def_node.params.not_nil!
    params.size.should eq(0)
  end

  # Phase 4A: Return type annotations
  it "parses def with return type annotation" do
    source = <<-CR
      def get_int : Int32
        42
      end
    CR

    parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
    program = parser.parse_program

    program.roots.size.should eq(1)
    def_node = program.arena[program.roots.first].as(CrystalV2::Compiler::Frontend::DefNode)

    return_type = def_node.return_type
    return_type.should_not be_nil
    String.new(return_type.not_nil!).should eq("Int32")
  end

  it "parses def with params and return type" do
    source = <<-CR
      def add(x : Int32, y : Int32) : Int32
        x
      end
    CR

    parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
    program = parser.parse_program

    program.roots.size.should eq(1)
    def_node = program.arena[program.roots.first].as(CrystalV2::Compiler::Frontend::DefNode)

    # Check params
    params = def_node.params.not_nil!
    params.size.should eq(2)
    String.new(params[0].name).should eq("x")
    String.new(params[0].type_annotation.not_nil!).should eq("Int32")

    # Check return type
    return_type = def_node.return_type
    return_type.should_not be_nil
    String.new(return_type.not_nil!).should eq("Int32")
  end
end
