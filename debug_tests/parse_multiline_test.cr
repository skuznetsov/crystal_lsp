require "../src/compiler/frontend/parser"
require "../src/compiler/frontend/lexer"

source = File.read("crystal_v2/debug_tests/test_macro_multiline.cr")

puts "Source:"
puts source
puts "\n" + "=" * 80

lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
program = parser.parse_program

puts "Diagnostics: #{parser.diagnostics.size}"
parser.diagnostics.each_with_index do |diag, i|
  puts "#{i + 1}. #{diag}"
end

if parser.diagnostics.size == 0
  puts "\n✓ No errors! Multiline macro expression parsed successfully."
else
  puts "\n✗ Found #{parser.diagnostics.size} errors."
end
