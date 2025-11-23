require "spec"

require "../../src/compiler/frontend/parser"

describe "CrystalV2::Compiler::Frontend::Parser" do
  describe "case with constant paths" do
    it "parses chained const paths in when branches" do
      source = <<-CRYSTAL
        severity = case diag.level
        when Semantic::DiagnosticLevel::Error
          DiagnosticSeverity::Error.value
        when Semantic::DiagnosticLevel::Warning
          DiagnosticSeverity::Warning.value
        else
          DiagnosticSeverity::Information.value
        end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(
        CrystalV2::Compiler::Frontend::Lexer.new(source)
      )
      program = parser.parse_program

      parser.diagnostics.should be_empty
      program.roots.size.should eq(1)

      arena = program.arena
      assign_node = arena[program.roots.first].as(CrystalV2::Compiler::Frontend::AssignNode)
      CrystalV2::Compiler::Frontend.node_kind(assign_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)

      case_id = CrystalV2::Compiler::Frontend.node_assign_value(assign_node)
      case_node = arena[case_id].as(CrystalV2::Compiler::Frontend::CaseNode)
      CrystalV2::Compiler::Frontend.node_kind(case_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Case)

      whens = CrystalV2::Compiler::Frontend.node_when_branches(case_node)
      whens.size.should eq(2)

      else_branch = CrystalV2::Compiler::Frontend.node_case_else(case_node)
      else_branch.should_not be_nil
      else_branch.not_nil!.size.should eq(1)
    end
  end
end
