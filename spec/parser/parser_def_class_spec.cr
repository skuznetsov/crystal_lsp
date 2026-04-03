require "spec"

require "../../src/compiler/frontend/parser"

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
    def_node.params.not_nil!.map { |p| String.new(p.name.not_nil!) }.should eq(["name"])
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
    String.new(params[0].name.not_nil!).should eq("x")
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
    String.new(params[0].name.not_nil!).should eq("x")
    String.new(params[0].type_annotation.not_nil!).should eq("String")
    String.new(params[1].name.not_nil!).should eq("y")
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
    String.new(params[0].name.not_nil!).should eq("x")
    params[0].type_annotation.should be_nil
    String.new(params[1].name.not_nil!).should eq("y")
    String.new(params[1].type_annotation.not_nil!).should eq("Int32")
    String.new(params[2].name.not_nil!).should eq("z")
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
    String.new(params[0].name.not_nil!).should eq("x")
    String.new(params[0].type_annotation.not_nil!).should eq("Int32")

    # Check return type
    return_type = def_node.return_type
    return_type.should_not be_nil
    String.new(return_type.not_nil!).should eq("Int32")
  end

  it "parses tuple-union return types without leaking the rhs into the body" do
    source = <<-CR
      def minmax_by?(& : T -> U) : {T, T} | {Nil, Nil} forall U
        found ? value : {nil, nil}
      end
    CR

    parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
    program = parser.parse_program

    program.roots.size.should eq(1)
    def_node = program.arena[program.roots.first].as(CrystalV2::Compiler::Frontend::DefNode)

    return_type = def_node.return_type
    return_type.should_not be_nil
    String.new(return_type.not_nil!).should eq("{T, T}|{Nil, Nil}")

    body = def_node.body
    body.should_not be_nil
    body.not_nil!.size.should eq(1)

    body_node = program.arena[body.not_nil!.first]
    CrystalV2::Compiler::Frontend.node_kind(body_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Ternary)
  end

  it "keeps parenthesized do blocks inside def bodies after simple return types" do
    source = <<-CR
      class Dir
        def self.each_child(dirname : Path | String, & : String ->)
        end

        def self.empty?(path : Path | String) : Bool
          each_child(path) do |f|
            false
          end
          true
        end
      end
    CR

    parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
    program = parser.parse_program

    program.roots.size.should eq(1)
    class_node = program.arena[program.roots.first].as(CrystalV2::Compiler::Frontend::ClassNode)
    body = class_node.body.not_nil!
    body.size.should eq(2)

    empty_def = program.arena[body[1]].as(CrystalV2::Compiler::Frontend::DefNode)
    return_type = empty_def.return_type
    return_type.should_not be_nil
    String.new(return_type.not_nil!).should eq("Bool")

    empty_body = empty_def.body
    empty_body.should_not be_nil
    empty_body.not_nil!.size.should eq(2)

    first_stmt = program.arena[empty_body.not_nil!.first]
    second_stmt = program.arena[empty_body.not_nil!.last]
    CrystalV2::Compiler::Frontend.node_kind(first_stmt).should eq(CrystalV2::Compiler::Frontend::NodeKind::Call)
    CrystalV2::Compiler::Frontend.node_kind(second_stmt).should eq(CrystalV2::Compiler::Frontend::NodeKind::Bool)
  end
end
