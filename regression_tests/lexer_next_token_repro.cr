require "../src/compiler/frontend/parser/diagnostic"
require "../src/compiler/frontend/lexer"
alias FE = CrystalV2::Compiler::Frontend
lexer = FE::Lexer.new("puts 1\n")
16.times do
  tok = lexer.next_token
  puts tok.kind
  break if tok.kind == FE::Token::Kind::EOF
end
