require "../src/compiler/frontend/lexer"
require "../src/compiler/frontend/parser"

# Show context for "unexpected Eq" errors in lexer.cr
lexer_file = "/Users/sergey/Projects/Crystal/crystal/src/compiler/crystal/syntax/lexer.cr"
content = File.read(lexer_file)
lines = content.lines

lexer = CrystalV2::Compiler::Frontend::Lexer.new(content)
parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
program = parser.parse_program

eq_errors = parser.diagnostics.select { |d| d.message == "unexpected Eq" }

puts "=== EQ ERRORS IN LEXER.CR ==="
puts "Total: #{eq_errors.size}\n"

eq_errors.each_with_index do |diag, idx|
  line_num = diag.span.start_line - 1

  start_line = [0, line_num - 2].max
  end_line = [lines.size - 1, line_num + 2].min

  puts "\n--- Error ##{idx + 1}: #{diag.message} at line #{line_num + 1} ---"
  (start_line..end_line).each do |i|
    prefix = i == line_num ? ">>> " : "    "
    puts "#{prefix}#{i + 1}: #{lines[i]}"
  end
end
