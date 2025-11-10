require "../src/compiler/frontend/lexer"
require "../src/compiler/frontend/parser"

source = "@[Link]
class Foo
end"

puts "Source:"
puts source
puts "\n" + "="*70

lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
program = parser.parse_program

puts "\nDiagnostics: #{parser.diagnostics.size}"
parser.diagnostics.each do |d|
  line_num = d.span.start_line + 1
  col_num = d.span.start_column + 1

  # Show context
  lines = source.lines
  if line_num <= lines.size
    puts "\nLine #{line_num}:#{col_num}: #{d.message}"
    puts "  #{lines[line_num - 1]}"
    puts "  #{\" \" * (col_num - 1)}^"
  end
end
