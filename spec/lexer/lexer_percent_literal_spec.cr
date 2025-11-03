require "spec"

require "../../src/compiler/frontend/lexer"

describe CrystalV2::Compiler::Frontend::Lexer do
  describe "Percent literals" do
    it "tokenizes %() string literal" do
      source = "%(hello world)"
      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      kinds = [] of CrystalV2::Compiler::Frontend::Token::Kind
      lexer.each_token { |token| kinds << token.kind }

      kinds.should contain(CrystalV2::Compiler::Frontend::Token::Kind::String)
    end

    it "tokenizes %{} string literal with braces" do
      source = "%{hello world}"
      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      kinds = [] of CrystalV2::Compiler::Frontend::Token::Kind
      lexer.each_token { |token| kinds << token.kind }

      kinds.should contain(CrystalV2::Compiler::Frontend::Token::Kind::String)
    end

    it "tokenizes %w() array of words" do
      source = "%w(one two three)"
      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      kinds = [] of CrystalV2::Compiler::Frontend::Token::Kind
      lexer.each_token { |token| kinds << token.kind }

      # %w creates an array literal
      kinds.should contain(CrystalV2::Compiler::Frontend::Token::Kind::LBracket)
    end

    it "tokenizes %i() array of symbols" do
      source = "%i(one two three)"
      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      kinds = [] of CrystalV2::Compiler::Frontend::Token::Kind
      lexer.each_token { |token| kinds << token.kind }

      # %i creates an array literal with symbols
      kinds.should contain(CrystalV2::Compiler::Frontend::Token::Kind::LBracket)
    end

    it "handles nested delimiters in %()" do
      source = "%(hello (nested) world)"
      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      kinds = [] of CrystalV2::Compiler::Frontend::Token::Kind
      lexer.each_token { |token| kinds << token.kind }

      kinds.should contain(CrystalV2::Compiler::Frontend::Token::Kind::String)
    end

    it "handles JSON in %{}" do
      source = "%{{\"key\":\"value\"}}"
      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      kinds = [] of CrystalV2::Compiler::Frontend::Token::Kind
      lexer.each_token { |token| kinds << token.kind }

      kinds.should contain(CrystalV2::Compiler::Frontend::Token::Kind::String)
    end
  end
end
