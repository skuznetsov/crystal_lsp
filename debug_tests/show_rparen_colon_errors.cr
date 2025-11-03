require "../src/compiler/frontend/lexer"
require "../src/compiler/frontend/parser"

# Show context for RParen and Colon errors
ast_file = "/Users/sergey/Projects/Crystal/crystal/src/compiler/crystal/syntax/ast.cr"
content = File.read(ast_file)
lines = content.lines

lexer = CrystalV2::Compiler::Frontend::Lexer.new(content)
parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
program = parser.parse_program

rparen_errors = parser.diagnostics.select { |d| d.message == "unexpected RParen" }
colon_errors = parser.diagnostics.select { |d| d.message == "unexpected Colon" }

puts "=== RPAREN ERRORS (#{rparen_errors.size}) ==="
rparen_errors.first(5).each_with_index do |diag, idx|
  line_num = diag.span.start_line - 1
  start_line = [0, line_num - 1].max
  end_line = [lines.size - 1, line_num + 1].min

  puts "\n#{idx + 1}. Line #{line_num + 1}:"
  (start_line..end_line).each do |i|
    prefix = i == line_num ? ">>> " : "    "
    puts "#{prefix}#{lines[i]}"
  end
end

puts "\n\n=== COLON ERRORS (#{colon_errors.size}) ==="
colon_errors.first(5).each_with_index do |diag, idx|
  line_num = diag.span.start_line - 1
  start_line = [0, line_num - 1].max
  end_line = [lines.size - 1, line_num + 1].min

  puts "\n#{idx + 1}. Line #{line_num + 1}:"
  (start_line..end_line).each do |i|
    prefix = i == line_num ? ">>> " : "    "
    puts "#{prefix}#{lines[i]}"
  end
end
