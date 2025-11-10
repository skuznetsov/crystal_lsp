require "../src/compiler/frontend/parser"
require "../src/compiler/frontend/lexer"

ENV["PARSER_DEBUG"] = "1"

source = "{% x = 123 %}"
puts "Source: #{source.inspect}"
puts "=" * 80

lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
program = parser.parse_program

puts "\n" + "=" * 80
puts "Roots: #{program.roots.size}"
puts "Diagnostics: #{parser.diagnostics.size}"

if parser.diagnostics.size > 0
  puts "\nDiagnostics:"
  parser.diagnostics.each do |d|
    puts "  - #{d.message} at #{d.span.start_line}:#{d.span.start_column}"
  end
else
  puts "\n✅ No diagnostics!"
end
