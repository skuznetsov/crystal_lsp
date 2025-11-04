require "spec"

require "../../src/compiler/frontend/parser"

describe "CrystalV2::Compiler::Frontend::Parser" do
  describe "Phase 72: Named arguments at call site (PRODUCTION-READY)" do
    it "parses method call with single named argument" do
      source = "foo(x: 10)"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      call = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(call).should eq(CrystalV2::Compiler::Frontend::NodeKind::Call)

      named_args = call.as(CrystalV2::Compiler::Frontend::CallNode).named_args.not_nil!
      named_args.size.should eq(1)
      String.new(named_args[0].name).should eq("x")
    end

    it "parses method call with multiple named arguments" do
      source = "foo(x: 10, y: 20)"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      call = arena[program.roots[0]]
      named_args = call.as(CrystalV2::Compiler::Frontend::CallNode).named_args.not_nil!
      named_args.size.should eq(2)
      String.new(named_args[0].name).should eq("x")
      String.new(named_args[1].name).should eq("y")
    end

    it "parses method call with named arguments and expressions" do
      source = "foo(x: 1 + 1, y: 2 * 2)"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      call = arena[program.roots[0]]
      named_args = call.as(CrystalV2::Compiler::Frontend::CallNode).named_args.not_nil!
      named_args.size.should eq(2)

      # Values are expressions
      value1 = arena[named_args[0].value]
      CrystalV2::Compiler::Frontend.node_kind(value1).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
    end

    it "parses method call with only positional arguments" do
      source = "foo(10, 20)"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      call = arena[program.roots[0]]
      args = call.as(CrystalV2::Compiler::Frontend::CallNode).args.not_nil!
      args.size.should eq(2)
      call.as(CrystalV2::Compiler::Frontend::CallNode).named_args.should be_nil
    end

    it "parses method call with mixed positional and named arguments" do
      source = "foo(10, y: 20)"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      call = arena[program.roots[0]]
      args = call.as(CrystalV2::Compiler::Frontend::CallNode).args.not_nil!
      args.size.should eq(1)

      named_args = call.as(CrystalV2::Compiler::Frontend::CallNode).named_args.not_nil!
      named_args.size.should eq(1)
      String.new(named_args[0].name).should eq("y")
    end

    it "parses method call with multiple positional then named" do
      source = "foo(1, 2, x: 3, y: 4)"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      call = arena[program.roots[0]]
      args = call.as(CrystalV2::Compiler::Frontend::CallNode).args.not_nil!
      args.size.should eq(2)

      named_args = call.as(CrystalV2::Compiler::Frontend::CallNode).named_args.not_nil!
      named_args.size.should eq(2)
      String.new(named_args[0].name).should eq("x")
      String.new(named_args[1].name).should eq("y")
    end

    it "parses empty method call" do
      source = "foo()"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      call = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(call).should eq(CrystalV2::Compiler::Frontend::NodeKind::Call)
      call.as(CrystalV2::Compiler::Frontend::CallNode).args.empty?.should be_true
      call.as(CrystalV2::Compiler::Frontend::CallNode).named_args.should be_nil
    end

    it "parses named arguments with string values" do
      source = "foo(name: \"Alice\", greeting: \"Hello\")"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      call = arena[program.roots[0]]
      named_args = call.as(CrystalV2::Compiler::Frontend::CallNode).named_args.not_nil!
      named_args.size.should eq(2)

      value1 = arena[named_args[0].value]
      CrystalV2::Compiler::Frontend.node_kind(value1).should eq(CrystalV2::Compiler::Frontend::NodeKind::String)
    end

    it "parses named arguments with array values" do
      source = "foo(items: [1, 2, 3])"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      call = arena[program.roots[0]]
      named_args = call.as(CrystalV2::Compiler::Frontend::CallNode).named_args.not_nil!
      named_args.size.should eq(1)

      value = arena[named_args[0].value]
      CrystalV2::Compiler::Frontend.node_kind(value).should eq(CrystalV2::Compiler::Frontend::NodeKind::ArrayLiteral)
    end

    it "parses named arguments with identifier values" do
      source = "foo(x: y, a: b)"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      call = arena[program.roots[0]]
      named_args = call.as(CrystalV2::Compiler::Frontend::CallNode).named_args.not_nil!
      named_args.size.should eq(2)

      value1 = arena[named_args[0].value]
      CrystalV2::Compiler::Frontend.node_kind(value1).should eq(CrystalV2::Compiler::Frontend::NodeKind::Identifier)
    end

    it "parses member access with named arguments" do
      source = "obj.method(x: 10)"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      call = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(call).should eq(CrystalV2::Compiler::Frontend::NodeKind::Call)

      callee = arena[call.as(CrystalV2::Compiler::Frontend::CallNode).callee.not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(callee).should eq(CrystalV2::Compiler::Frontend::NodeKind::MemberAccess)

      named_args = call.as(CrystalV2::Compiler::Frontend::CallNode).named_args.not_nil!
      named_args.size.should eq(1)
      String.new(named_args[0].name).should eq("x")
    end

    it "parses named arguments with nil and boolean values" do
      source = "foo(flag: true, value: nil)"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      call = arena[program.roots[0]]
      named_args = call.as(CrystalV2::Compiler::Frontend::CallNode).named_args.not_nil!
      named_args.size.should eq(2)

      value1 = arena[named_args[0].value]
      CrystalV2::Compiler::Frontend.node_kind(value1).should eq(CrystalV2::Compiler::Frontend::NodeKind::Bool)

      value2 = arena[named_args[1].value]
      CrystalV2::Compiler::Frontend.node_kind(value2).should eq(CrystalV2::Compiler::Frontend::NodeKind::Nil)
    end

    it "parses nested calls with named arguments" do
      source = "outer(inner(x: 1), y: 2)"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      outer_call = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(outer_call).should eq(CrystalV2::Compiler::Frontend::NodeKind::Call)

      # outer has positional arg (inner call) and named arg
      outer_args = outer_call.as(CrystalV2::Compiler::Frontend::CallNode).args.not_nil!
      outer_args.size.should eq(1)

      inner_call = arena[outer_args[0]]
      CrystalV2::Compiler::Frontend.node_kind(inner_call).should eq(CrystalV2::Compiler::Frontend::NodeKind::Call)

      inner_named_args = inner_call.as(CrystalV2::Compiler::Frontend::CallNode).named_args.not_nil!
      inner_named_args.size.should eq(1)
      String.new(inner_named_args[0].name).should eq("x")

      outer_named_args = outer_call.as(CrystalV2::Compiler::Frontend::CallNode).named_args.not_nil!
      outer_named_args.size.should eq(1)
      String.new(outer_named_args[0].name).should eq("y")
    end

    it "parses named arguments with trailing comma" do
      source = "foo(x: 10, y: 20,)"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      call = arena[program.roots[0]]
      named_args = call.as(CrystalV2::Compiler::Frontend::CallNode).named_args.not_nil!
      named_args.size.should eq(2)
    end
  end
end
