require "spec"

require "../../src/compiler/frontend/ast"
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

    it "parses macro control block with nested expression as a macro if" do
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
      CrystalV2::Compiler::Frontend.node_kind(macro_if).should eq(CrystalV2::Compiler::Frontend::NodeKind::MacroIf)

      macro_if_node = macro_if.as(CrystalV2::Compiler::Frontend::MacroIfNode)
      then_body = arena[macro_if_node.then_body]
      CrystalV2::Compiler::Frontend.node_kind(then_body).should eq(CrystalV2::Compiler::Frontend::NodeKind::MacroLiteral)
    end

    it "captures require statements inside macro control blocks" do
      source = <<-CRYSTAL
      {% if flag?(:unix) %}
        require "./unix/file_descriptor"
      {% end %}
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      macro_if = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(macro_if).should eq(CrystalV2::Compiler::Frontend::NodeKind::MacroIf)

      macro_if_node = macro_if.as(CrystalV2::Compiler::Frontend::MacroIfNode)
      macro_literal = arena[macro_if_node.then_body]
      CrystalV2::Compiler::Frontend.node_kind(macro_literal).should eq(CrystalV2::Compiler::Frontend::NodeKind::MacroLiteral)

      pieces = CrystalV2::Compiler::Frontend.node_macro_pieces(macro_literal).not_nil!
      text = pieces.compact_map(&.text).join
      text.should contain("require")
      text.should contain("./unix/file_descriptor")
    end

    it "keeps class body members after a macro begin block with nested header controls" do
      source = <<-CRYSTAL
      class File
        {% begin %}
          def self.new(filename : String, mode = "r", {% if compare_versions(Crystal::VERSION, "1.5.0") >= 0 %} @[Deprecated] {% end %} blocking = nil)
            filename
          end
        {% end %}

        protected def self.new_internal(filename, mode = "r", blocking = nil)
          filename
        end

        getter path : String
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      parser.diagnostics.should be_empty
      program.roots.size.should eq(1)

      arena = program.arena
      class_node = arena[program.roots.first].as(CrystalV2::Compiler::Frontend::ClassNode)
      class_body = class_node.body.not_nil!

      class_body.size.should eq(3)

      macro_if = arena[class_body[0]]
      CrystalV2::Compiler::Frontend.node_kind(macro_if).should eq(CrystalV2::Compiler::Frontend::NodeKind::MacroIf)

      protected_def = arena[class_body[1]]
      CrystalV2::Compiler::Frontend.node_kind(protected_def).should eq(CrystalV2::Compiler::Frontend::NodeKind::Def)
      String.new(CrystalV2::Compiler::Frontend.node_def_name(protected_def).not_nil!).should eq("new_internal")
      CrystalV2::Compiler::Frontend.node_def_visibility(protected_def).should eq(CrystalV2::Compiler::Frontend::Visibility::Protected)
    end
  end
end
