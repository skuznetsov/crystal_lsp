require "../src/compiler/frontend/lexer"
require "../src/compiler/frontend/parser"

# Test what property parses as (simple case that works)
source1 = "property x : Int32 = 42"
lexer1 = CrystalV2::Compiler::Frontend::Lexer.new(source1)
parser1 = CrystalV2::Compiler::Frontend::Parser.new(lexer1)
program1 = parser1.parse_program

puts "Simple property: '#{source1}'"
puts "Diagnostics: #{parser1.diagnostics.size}"
puts

# Test complex case that fails
source2 = "property x : Hash(Int32, String) = nil"
lexer2 = CrystalV2::Compiler::Frontend::Lexer.new(source2)
parser2 = CrystalV2::Compiler::Frontend::Parser.new(lexer2)
program2 = parser2.parse_program

puts "Complex property: '#{source2}'"
puts "Diagnostics: #{parser2.diagnostics.size}"
parser2.diagnostics.each { |d| puts "  #{d.message}" }
