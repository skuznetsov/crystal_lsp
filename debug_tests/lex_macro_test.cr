require "../src/compiler/frontend/lexer"

source = <<-CRYSTAL
{%
  x = {
    key: "value",
  }
%}
CRYSTAL

puts "Source:"
puts source
puts "\n" + "=" * 60

lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
tokens = [] of CrystalV2::Compiler::Frontend::Token

loop do
  token = lexer.next_token
  tokens << token
  break if token.kind == CrystalV2::Compiler::Frontend::Token::Kind::EOF
end

puts "Tokens (#{tokens.size}):"
tokens.each_with_index do |tok, i|
  text = lexer.token_text(tok)
  text_display = text.gsub("\n", "\\n").gsub("\r", "\\r")
  puts "#{i}. #{tok.kind} | '#{text_display}'"
end
