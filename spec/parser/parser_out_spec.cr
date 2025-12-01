require "spec"

require "../../src/compiler/frontend/parser"

describe "CrystalV2::Compiler::Frontend::Parser" do
  describe "Phase 98: out keyword (C bindings output parameter)" do
    it "parses out with identifier" do
      source = "out result"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena
      out_node = arena[program.roots.first]

      CrystalV2::Compiler::Frontend.node_kind(out_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Out)
      CrystalV2::Compiler::Frontend.node_out_identifier(out_node).should_not be_nil
      String.new(CrystalV2::Compiler::Frontend.node_out_identifier(out_node).not_nil!).should eq("result")
    end

    it "parses out in function call" do
      source = "C.get_value(out new_var)"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena
      call_node = arena[program.roots.first]

      # Should be a call expression
      CrystalV2::Compiler::Frontend.node_kind(call_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Call)

      # Check callee (C.get_value)
      callee_id = CrystalV2::Compiler::Frontend.node_callee(call_node).not_nil!
      callee = arena[callee_id]
      CrystalV2::Compiler::Frontend.node_kind(callee).should eq(CrystalV2::Compiler::Frontend::NodeKind::MemberAccess)

      # Check argument is out expression
      args = CrystalV2::Compiler::Frontend.node_args(call_node).not_nil!
      args.size.should eq(1)

      out_arg = arena[args[0]]
      CrystalV2::Compiler::Frontend.node_kind(out_arg).should eq(CrystalV2::Compiler::Frontend::NodeKind::Out)
      CrystalV2::Compiler::Frontend.node_out_identifier(out_arg).should_not be_nil
      String.new(CrystalV2::Compiler::Frontend.node_out_identifier(out_arg).not_nil!).should eq("new_var")
    end

    it "parses multiple out parameters" do
      source = "C.get_values(out x, out y, out z)"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena
      call_node = arena[program.roots.first]

      # Check arguments
      args = CrystalV2::Compiler::Frontend.node_args(call_node).not_nil!
      args.size.should eq(3)

      # Verify all three out expressions
      ["x", "y", "z"].each_with_index do |name, idx|
        out_arg = arena[args[idx]]
        CrystalV2::Compiler::Frontend.node_kind(out_arg).should eq(CrystalV2::Compiler::Frontend::NodeKind::Out)
        String.new(CrystalV2::Compiler::Frontend.node_out_identifier(out_arg).not_nil!).should eq(name)
      end
    end

    it "parses out with different identifier names" do
      source = "out foo_bar_123"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena
      out_node = arena[program.roots.first]

      CrystalV2::Compiler::Frontend.node_kind(out_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Out)
      String.new(CrystalV2::Compiler::Frontend.node_out_identifier(out_node).not_nil!).should eq("foo_bar_123")
    end

    it "parses out in lib context" do
      source = <<-CR
        lib C
          fun get_value(out_ptr : Int32*)
        end

        C.get_value(out result)
      CR

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      # Should have 2 root expressions: lib and function call
      program.roots.size.should eq(2)
      arena = program.arena

      # Second root is the function call
      call_node = arena[program.roots[1]]
      CrystalV2::Compiler::Frontend.node_kind(call_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Call)

      # Check argument
      args = CrystalV2::Compiler::Frontend.node_args(call_node).not_nil!
      args.size.should eq(1)

      out_arg = arena[args[0]]
      CrystalV2::Compiler::Frontend.node_kind(out_arg).should eq(CrystalV2::Compiler::Frontend::NodeKind::Out)
      String.new(CrystalV2::Compiler::Frontend.node_out_identifier(out_arg).not_nil!).should eq("result")
    end

    it "parses out mixed with regular arguments" do
      source = "C.process(100, out status, \"hello\", out error_code)"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena
      call_node = arena[program.roots.first]

      # Check arguments
      args = CrystalV2::Compiler::Frontend.node_args(call_node).not_nil!
      args.size.should eq(4)

      # First arg: 100 (number)
      arg0 = arena[args[0]]
      CrystalV2::Compiler::Frontend.node_kind(arg0).should eq(CrystalV2::Compiler::Frontend::NodeKind::Number)

      # Second arg: out status
      arg1 = arena[args[1]]
      CrystalV2::Compiler::Frontend.node_kind(arg1).should eq(CrystalV2::Compiler::Frontend::NodeKind::Out)
      String.new(CrystalV2::Compiler::Frontend.node_out_identifier(arg1).not_nil!).should eq("status")

      # Third arg: "hello" (string)
      arg2 = arena[args[2]]
      CrystalV2::Compiler::Frontend.node_kind(arg2).should eq(CrystalV2::Compiler::Frontend::NodeKind::String)

      # Fourth arg: out error_code
      arg3 = arena[args[3]]
      CrystalV2::Compiler::Frontend.node_kind(arg3).should eq(CrystalV2::Compiler::Frontend::NodeKind::Out)
      String.new(CrystalV2::Compiler::Frontend.node_out_identifier(arg3).not_nil!).should eq("error_code")
    end

    it "parses out without identifier as an identifier (semantic error later)" do
      # Crystal allows `out` as an identifier - error is semantic, not parser
      # "undefined local variable or method 'out'"
      source = "out"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      # No parser error - Crystal treats `out` as identifier
      parser.diagnostics.should be_empty
      program.roots.size.should eq(1)

      arena = program.arena
      node = arena[program.roots.first]
      CrystalV2::Compiler::Frontend.node_kind(node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Identifier)
    end

    it "parses out followed by non-identifier as two expressions (semantic error later)" do
      # Crystal allows `out 123` - parses as two expressions (identifier + number)
      # Error is semantic: "undefined local variable or method 'out'"
      source = "out 123"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      # No parser error - Crystal parses this successfully
      parser.diagnostics.should be_empty
      # Parses as two separate expressions: `out` and `123`
      program.roots.size.should eq(2)
    end
  end
end
