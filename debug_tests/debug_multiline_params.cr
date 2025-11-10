require "../src/compiler/frontend/lexer"
require "../src/compiler/frontend/parser"

# Minimal failing case
source = "def initialize(
  @x : Int32
)
end"

puts "Source:"
puts source
puts "\n" + "=" * 70

# Show tokens
lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
puts "\nTokens:"
tokens = [] of CrystalV2::Compiler::Frontend::Token
loop do
  tok = lexer.next_token
  tokens << tok
  break if tok.kind == CrystalV2::Compiler::Frontend::Token::Kind::EOF
end

tokens.each_with_index do |tok, i|
  kind = tok.kind.to_s.gsub("CrystalV2::Compiler::Frontend::Token::Kind::", "")
  text = String.new(tok.slice).inspect
  puts "#{i.to_s.rjust(3)}: #{kind.ljust(15)} #{text}"
end

# Now parse
puts "\n" + "=" * 70
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
