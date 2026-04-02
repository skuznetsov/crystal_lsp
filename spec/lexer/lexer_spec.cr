require "spec"

require "../../src/main"
require "../../src/compiler/frontend/lexer"

describe CrystalV2::Compiler::Frontend::Lexer do
  it "tokenizes identifiers and numbers" do
    source = "foo 123\nbar"
    lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
    kinds = [] of CrystalV2::Compiler::Frontend::Token::Kind
    lexer.each_token { |token| kinds << token.kind }

    kinds.should eq([
      CrystalV2::Compiler::Frontend::Token::Kind::Identifier,
      CrystalV2::Compiler::Frontend::Token::Kind::Whitespace,
      CrystalV2::Compiler::Frontend::Token::Kind::Number,
      CrystalV2::Compiler::Frontend::Token::Kind::Newline,
      CrystalV2::Compiler::Frontend::Token::Kind::Identifier,
      CrystalV2::Compiler::Frontend::Token::Kind::EOF,
    ])
  end

  it "keeps plain double braces inside regular strings" do
    source = "value = text.includes?(\"{{\")\nafter_call"
    lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
    tokens = [] of CrystalV2::Compiler::Frontend::Token
    lexer.each_token(skip_trivia: true) { |token| tokens << token }

    string_tokens = tokens.select { |token| token.kind == CrystalV2::Compiler::Frontend::Token::Kind::String }
    string_tokens.size.should eq(1)
    String.new(string_tokens.first.slice).should eq("{{")
    tokens.none? { |token| token.kind == CrystalV2::Compiler::Frontend::Token::Kind::StringInterpolation }.should be_true
    tokens.any? { |token| token.kind == CrystalV2::Compiler::Frontend::Token::Kind::Identifier && String.new(token.slice) == "after_call" }.should be_true
  end
end
