require "../src/compiler/frontend/parser"
require "../src/compiler/frontend/lexer"

source = <<-CRYSTAL
class Test
  macro test
    {%
      x = {
        a: 1,
        b: 2,
      }
    %}
  end
end
CRYSTAL

puts "=== SOURCE ==="
puts source
puts "\n=== PARSING ==="

lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
program = parser.parse_program

puts "\nDiagnostics: #{parser.diagnostics.size}"
parser.diagnostics.each_with_index do |diag, i|
  puts "#{i + 1}. #{diag}"
end

if parser.diagnostics.size == 0
  puts "\n✓ SUCCESS: No errors!"
else
  puts "\n✗ FAILED: #{parser.diagnostics.size} errors"
end
