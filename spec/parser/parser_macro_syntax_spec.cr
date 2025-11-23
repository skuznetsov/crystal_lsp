require "spec"

require "../../src/compiler/frontend/parser"

describe "CrystalV2::Compiler::Frontend::Parser" do
  describe "Macro syntax (parser-level)" do
    it "parses macro expression interpolation" do
      source = "value = {{FOO}}"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(assign).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)

      value = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(value).should eq(CrystalV2::Compiler::Frontend::NodeKind::MacroExpression)
    end

    it "parses macro control block with nested expression as a macro literal" do
      source = <<-CRYSTAL
      {% if flag %}
      value = 1
      {% else %}
      value = 2
      {% end %}
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      macro_if = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(macro_if).should eq(CrystalV2::Compiler::Frontend::NodeKind::MacroLiteral)
    end
  end
end
