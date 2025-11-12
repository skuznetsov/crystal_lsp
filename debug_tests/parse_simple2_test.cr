require "../src/compiler/frontend/parser"
require "../src/compiler/frontend/lexer"

source = File.read("crystal_v2/debug_tests/test_simple2.cr")
puts source
puts "=" * 60

lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
program = parser.parse_program

puts "Diagnostics: #{parser.diagnostics.size}"
parser.diagnostics.each { |d| puts "  - #{d}" }
puts parser.diagnostics.size == 0 ? "✓ SUCCESS" : "✗ FAILED"
