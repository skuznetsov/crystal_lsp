require "../src/compiler/frontend/lexer"
require "../src/compiler/frontend/parser"

# Show context for "unexpected Eq" errors
ast_file = "/Users/sergey/Projects/Crystal/crystal/src/compiler/crystal/syntax/ast.cr"
content = File.read(ast_file)
lines = content.lines

lexer = CrystalV2::Compiler::Frontend::Lexer.new(content)
parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
program = parser.parse_program

eq_errors = parser.diagnostics.select { |d| d.message == "unexpected Eq" }

puts "Found #{eq_errors.size} 'unexpected Eq' errors\n"
puts "=" * 80

eq_errors.each_with_index do |diag, idx|
  line_num = diag.span.start_line - 1  # Span uses 1-based line numbers

  # Show context: 2 lines before and after
  start_line = [0, line_num - 2].max
  end_line = [lines.size - 1, line_num + 2].min

  puts "\n#{idx + 1}. Line #{line_num + 1}:"
  (start_line..end_line).each do |i|
    prefix = i == line_num ? ">>> " : "    "
    puts "#{prefix}#{lines[i]}"
  end
end
