require "../src/compiler/frontend/parser"
require "../src/compiler/frontend/lexer"

file = "../src/json/serialization.cr"
source = File.read(file)
lines = source.lines

lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
program = parser.parse_program

puts "=== SERIALIZATION.CR DIAGNOSTICS ===\n"
puts "Total: #{parser.diagnostics.size}\n\n"

# Group by message type
by_type = parser.diagnostics.group_by(&.message)

by_type.each do |msg, diags|
  puts "#{msg} (#{diags.size} occurrences)"
  puts ""
end
