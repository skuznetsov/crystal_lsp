require "../src/compiler/frontend/lexer"
require "../src/compiler/frontend/parser"

source = File.read("/Users/sergey/Projects/Crystal/crystal/src/compiler/crystal/syntax/lexer.cr")

lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
program = parser.parse_program

puts "=== PlusEq ERRORS IN LEXER.CR ==="
puts "Total: #{parser.diagnostics.count { |d| d.message.includes?("PlusEq") }}\n"

parser.diagnostics.each_with_index do |diag, idx|
  next unless diag.message.includes?("PlusEq")

  line_num = diag.span.start_line

  # Extract context: 2 lines before, error line, 2 lines after
  lines = source.lines
  start_idx = [line_num - 3, 0].max
  end_idx = [line_num + 2, lines.size - 1].min

  puts "\n--- Error ##{idx + 1}: #{diag.message} at line #{line_num} ---"
  (start_idx..end_idx).each do |i|
    prefix = i == line_num - 1 ? ">>> " : "    "
    puts "#{prefix}#{i + 1}: #{lines[i]}"
  end
end
