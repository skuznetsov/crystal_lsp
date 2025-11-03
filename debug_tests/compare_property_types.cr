require "../src/compiler/frontend/lexer"
require "../src/compiler/frontend/parser"

# Works
source1 = "property x : Int32 = 42"
lexer1 = CrystalV2::Compiler::Frontend::Lexer.new(source1)
parser1 = CrystalV2::Compiler::Frontend::Parser.new(lexer1)
program1 = parser1.parse_program
puts "Simple: #{source1}"
puts "  Errors: #{parser1.diagnostics.size}"
puts

# Fails
source2 = "property x : Hash(Int32, String) = nil"
lexer2 = CrystalV2::Compiler::Frontend::Lexer.new(source2)
parser2 = CrystalV2::Compiler::Frontend::Parser.new(lexer2)
program2 = parser2.parse_program
puts "Complex: #{source2}"
puts "  Errors: #{parser2.diagnostics.size}"
parser2.diagnostics.each { |d| puts "    #{d.message}" }
puts

# Also fails
source3 = "property x : Hash(Int32, String)? = nil"
lexer3 = CrystalV2::Compiler::Frontend::Lexer.new(source3)
parser3 = CrystalV2::Compiler::Frontend::Parser.new(lexer3)
program3 = parser3.parse_program
puts "Complex?: #{source3}"
puts "  Errors: #{parser3.diagnostics.size}"
parser3.diagnostics.each { |d| puts "    #{d.message}" }
