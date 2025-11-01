require "spec"

require "../src/main"
require "../src/compiler/frontend/lexer"

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
end
