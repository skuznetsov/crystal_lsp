require "../src/compiler/frontend/lexer"

source = "{% if true %}
x = 1
{% elsif false %}
x = 2
{% end %}"

lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
tokens = [] of CrystalV2::Compiler::Frontend::Token

loop do
  tok = lexer.next_token
  tokens << tok
  break if tok.kind == CrystalV2::Compiler::Frontend::Token::Kind::EOF
end

puts "Token sequence:"
tokens.each_with_index do |tok, i|
  kind = tok.kind.to_s.gsub("CrystalV2::Compiler::Frontend::Token::Kind::", "")
  text = String.new(tok.slice)[0...30]
  puts "#{i.to_s.rjust(3)}: #{kind.ljust(20)} #{text.inspect}"
end
