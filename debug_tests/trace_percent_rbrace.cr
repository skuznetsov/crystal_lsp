require "../src/compiler/frontend/parser"
require "../src/compiler/frontend/lexer"

ENV["PARSER_UNEXPECTED_TRACE"] = "1"

source = File.read("/tmp/test_macro_multiline_expr.cr")
puts "Source:"
puts source
puts "\n" + "=" * 50 + "\n"

lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
program = parser.parse_program

puts "\nDiagnostics: #{parser.diagnostics.size}"
parser.diagnostics.each do |d|
  puts "  - #{d.message}"
end
