require "spec"

require "../../src/compiler/frontend/parser"

describe "CrystalV2::Compiler::Frontend::Parser" do
  describe "Phase 74: Proc literal (->) (PRODUCTION-READY)" do
    it "parses parameterless proc with brace form" do
      source = "-> { 42 }"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      proc_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(proc_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::ProcLiteral)

      params = proc_node.as(CrystalV2::Compiler::Frontend::ProcLiteralNode).params
      params.should_not be_nil
      params.not_nil!.size.should eq(0)

      body = proc_node.as(CrystalV2::Compiler::Frontend::ProcLiteralNode).body.not_nil!
      body.size.should eq(1)

      body_expr = arena[body[0]]
      CrystalV2::Compiler::Frontend.node_kind(body_expr).should eq(CrystalV2::Compiler::Frontend::NodeKind::Number)
    end

    it "parses single parameter without type annotation" do
      source = "->(x) { x + 1 }"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      proc_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(proc_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::ProcLiteral)

      params = proc_node.as(CrystalV2::Compiler::Frontend::ProcLiteralNode).params.not_nil!
      params.size.should eq(1)
      String.new(params[0].name.not_nil!).should eq("x")
      params[0].type_annotation.should be_nil
    end

    it "parses single parameter with type annotation" do
      source = "->(x : Int32) { x + 1 }"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      proc_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(proc_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::ProcLiteral)

      params = proc_node.as(CrystalV2::Compiler::Frontend::ProcLiteralNode).params.not_nil!
      params.size.should eq(1)
      String.new(params[0].name.not_nil!).should eq("x")

      type_annotation = params[0].type_annotation.not_nil!
      String.new(type_annotation).should eq("Int32")
    end

    it "parses two parameters with type annotations" do
      source = "->(x : Int32, y : Int32) { x + y }"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      proc_node = arena[program.roots[0]]
      params = proc_node.as(CrystalV2::Compiler::Frontend::ProcLiteralNode).params.not_nil!
      params.size.should eq(2)

      String.new(params[0].name.not_nil!).should eq("x")
      String.new(params[0].type_annotation.not_nil!).should eq("Int32")

      String.new(params[1].name.not_nil!).should eq("y")
      String.new(params[1].type_annotation.not_nil!).should eq("Int32")
    end

    it "parses two parameters without type annotations" do
      source = "->(x, y) { x + y }"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      proc_node = arena[program.roots[0]]
      params = proc_node.as(CrystalV2::Compiler::Frontend::ProcLiteralNode).params.not_nil!
      params.size.should eq(2)

      String.new(params[0].name.not_nil!).should eq("x")
      params[0].type_annotation.should be_nil

      String.new(params[1].name.not_nil!).should eq("y")
      params[1].type_annotation.should be_nil
    end

    it "parses proc with return type annotation" do
      source = "->(x : Int32) : Int32 { x * 2 }"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      proc_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(proc_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::ProcLiteral)

      return_type = proc_node.as(CrystalV2::Compiler::Frontend::ProcLiteralNode).return_type.not_nil!
      String.new(return_type).should eq("Int32")
    end

    it "parses proc with do...end form" do
      source = <<-CRYSTAL
      ->(x : Int32) do
        x + 1
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      proc_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(proc_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::ProcLiteral)

      params = proc_node.as(CrystalV2::Compiler::Frontend::ProcLiteralNode).params.not_nil!
      params.size.should eq(1)
      String.new(params[0].name.not_nil!).should eq("x")

      body = proc_node.as(CrystalV2::Compiler::Frontend::ProcLiteralNode).body.not_nil!
      body.size.should eq(1)
    end

    it "parses proc with multi-statement body" do
      source = <<-CRYSTAL
      ->(x : Int32) {
        y = x + 1
        y * 2
      }
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      proc_node = arena[program.roots[0]]
      body = proc_node.as(CrystalV2::Compiler::Frontend::ProcLiteralNode).body.not_nil!
      body.size.should eq(2)
    end

    it "parses nested proc literals" do
      source = "-> { ->(x) { x } }"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      outer_proc = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(outer_proc).should eq(CrystalV2::Compiler::Frontend::NodeKind::ProcLiteral)

      outer_body = outer_proc.as(CrystalV2::Compiler::Frontend::ProcLiteralNode).body.not_nil!
      outer_body.size.should eq(1)

      inner_proc = arena[outer_body[0]]
      CrystalV2::Compiler::Frontend.node_kind(inner_proc).should eq(CrystalV2::Compiler::Frontend::NodeKind::ProcLiteral)
    end

    it "parses proc as method call argument" do
      source = "foo(->(x) { x })"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      call = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(call).should eq(CrystalV2::Compiler::Frontend::NodeKind::Call)

      args = CrystalV2::Compiler::Frontend.node_args(call).not_nil!
      args.size.should eq(1)

      proc_arg = arena[args[0]]
      CrystalV2::Compiler::Frontend.node_kind(proc_arg).should eq(CrystalV2::Compiler::Frontend::NodeKind::ProcLiteral)
    end

    it "parses proc assigned to variable" do
      source = "p = ->(x) { x }"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(assign).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)

      value = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(value).should eq(CrystalV2::Compiler::Frontend::NodeKind::ProcLiteral)
    end

    it "parses proc with empty body" do
      source = "-> { }"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      proc_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(proc_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::ProcLiteral)

      body = proc_node.as(CrystalV2::Compiler::Frontend::ProcLiteralNode).body.not_nil!
      body.size.should eq(0)
    end
  end
end
