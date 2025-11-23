require "spec"

require "../../src/compiler/frontend/parser"

describe "CrystalV2::Compiler::Frontend::Parser" do
  describe "macro syntax errors" do
    it "rejects empty external name" do
      source = %(macro foo("" y); end)
      parser = CrystalV2::Compiler::Frontend::Parser.new(
        CrystalV2::Compiler::Frontend::Lexer.new(source)
      )
      parser.parse_program

      parser.diagnostics.should_not be_empty
    end

    it "rejects named args before bare splat" do
      source = %(macro foo(x, *); 1; end)
      parser = CrystalV2::Compiler::Frontend::Parser.new(
        CrystalV2::Compiler::Frontend::Lexer.new(source)
      )
      parser.parse_program

      parser.diagnostics.should_not be_empty
    end

    it "rejects double splat followed by positional" do
      source = %(macro foo(**x, y); end)
      parser = CrystalV2::Compiler::Frontend::Parser.new(
        CrystalV2::Compiler::Frontend::Lexer.new(source)
      )
      parser.parse_program

      parser.diagnostics.should_not be_empty
    end

    it "rejects macro with receiver" do
      source = %(macro Foo.bar; end)
      parser = CrystalV2::Compiler::Frontend::Parser.new(
        CrystalV2::Compiler::Frontend::Lexer.new(source)
      )
      parser.parse_program

      parser.diagnostics.should_not be_empty
    end
  end
end
