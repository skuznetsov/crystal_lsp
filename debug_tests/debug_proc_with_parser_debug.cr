ENV["PARSER_DEBUG"] = "1"

require "../src/compiler/frontend/parser"

source = "->(x : Int32) { x + 1 }"

parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
program = parser.parse_program

puts "\n=== RESULTS ==="
puts "Roots: #{program.roots.size}"
puts "Diagnostics: #{parser.diagnostics.size}"
parser.diagnostics.each { |d| puts "  #{d.message}" }
