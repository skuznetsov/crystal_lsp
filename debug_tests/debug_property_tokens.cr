require "../src/compiler/frontend/lexer"

source = "property end : Int32"

puts "Tokenizing: #{source.inspect}"
puts

lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
tokens = [] of CrystalV2::Compiler::Frontend::Token

loop do
  tok = lexer.next_token
  tokens << tok
  break if tok.kind == CrystalV2::Compiler::Frontend::Token::Kind::EOF
end

tokens.each_with_index do |tok, i|
  kind = tok.kind.to_s.gsub("CrystalV2::Compiler::Frontend::Token::Kind::", "")
  text = String.new(tok.slice).inspect
  puts "#{i.to_s.rjust(2)}: #{kind.ljust(15)} #{text}"
end
