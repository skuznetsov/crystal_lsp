require "../src/compiler/frontend/parser"
require "../src/compiler/frontend/lexer"

ENV["PARSER_DEBUG"] = "1"

source = File.read("/tmp/test_simple_multiline.cr")
puts "Source:"
puts source
puts "\n" + "=" * 50 + "\n"

lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)

puts "Before parsing:"
puts "Current token: #{parser.current_token.kind} at #{parser.current_token.span.start_line}:#{parser.current_token.span.start_column}"

program = parser.parse_program

puts "\nAfter parsing:"
puts "Current token: #{parser.current_token.kind} at #{parser.current_token.span.start_line}:#{parser.current_token.span.start_column}"
puts "\nRoots: #{program.roots.size}"
puts "Diagnostics: #{parser.diagnostics.size}"

if parser.diagnostics.size > 0
  puts "\nDiagnostics:"
  parser.diagnostics.each do |d|
    puts "  - #{d.message} at #{d.span.start_line}:#{d.span.start_column}"
  end
else
  puts "\n✅ No diagnostics!"
end
