require "spec"

require "../../src/compiler/frontend/parser"

describe "CrystalV2::Compiler::Frontend::Parser" do
  it "allows using 'out' as a regular identifier in expressions" do
    source = <<-CRYSTAL
    def foo(out : Int32)
      out = out + 1
      out
    end
    CRYSTAL

    parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
    program = parser.parse_program

    parser.diagnostics.should be_empty

    # method body assigns to `out` without treating it as the out-parameter syntax
    arena = program.arena
    def_node = arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::DefNode)
    body = def_node.body.not_nil!
    # last expression is identifier "out"
    last_expr = arena[body.last]
    CrystalV2::Compiler::Frontend.node_kind(last_expr).should eq(CrystalV2::Compiler::Frontend::NodeKind::Identifier)
  end
end
