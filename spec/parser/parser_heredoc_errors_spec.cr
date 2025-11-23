require "spec"

require "../../src/compiler/frontend/parser"

describe "CrystalV2::Compiler::Frontend::Parser" do
  describe "heredoc error handling" do
    it "reports indent error" do
      source = "<<-HERE\n One\n  #{1}\n  HERE"
      parser = CrystalV2::Compiler::Frontend::Parser.new(
        CrystalV2::Compiler::Frontend::Lexer.new(source, diagnostics: [] of CrystalV2::Compiler::Frontend::Diagnostic)
      )
      parser.parse_program

      parser.diagnostics.should_not be_empty
    end

    pending "heredoc interpolation diagnostic not fully wired yet" do
      it "reports heredoc inside interpolation" do
        source = %(\#{<<-HERE}\nHERE)
        parser = CrystalV2::Compiler::Frontend::Parser.new(
          CrystalV2::Compiler::Frontend::Lexer.new(source, diagnostics: [] of CrystalV2::Compiler::Frontend::Diagnostic)
        )
        parser.parse_program

        parser.diagnostics.should_not be_empty
      end
    end

    it "reports missing terminator" do
      source = "<<-FOO\n1\nFOO.bar"
      parser = CrystalV2::Compiler::Frontend::Parser.new(
        CrystalV2::Compiler::Frontend::Lexer.new(source)
      )
      parser.parse_program

      parser.diagnostics.should_not be_empty
    end

    it "reports unexpected EOF" do
      source = "<<-HEREDOC"
      parser = CrystalV2::Compiler::Frontend::Parser.new(
        CrystalV2::Compiler::Frontend::Lexer.new(source)
      )
      parser.parse_program

      parser.diagnostics.should_not be_empty
    end
  end
end
