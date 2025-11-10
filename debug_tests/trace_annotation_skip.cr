require "../src/compiler/frontend/lexer"
require "../src/compiler/frontend/parser"

# Simple test case
source = "@[Link]\nclass Foo\nend"

puts "Source (with escaped newlines shown):"
puts source.inspect
puts

# Show tokens
puts "Tokens:"
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
  line = tok.span.start_line + 1
  col = tok.span.start_column + 1
  puts "#{i.to_s.rjust(2)}: Line #{line}:#{col.to_s.rjust(2)} #{kind.ljust(15)} #{text}"
end

puts "\n" + "="*70
puts "Parsing..."
lexer2 = CrystalV2::Compiler::Frontend::Lexer.new(source)
parser = CrystalV2::Compiler::Frontend::Parser.new(lexer2)
program = parser.parse_program

if parser.diagnostics.size == 0
  puts "✓ Success"
else
  puts "✗ Errors: #{parser.diagnostics.size}"
  parser.diagnostics.each do |d|
    puts "  Line #{d.span.start_line + 1}:#{d.span.start_column + 1}: #{d.message}"
  end
end
