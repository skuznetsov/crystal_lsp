require "../src/compiler/frontend/lexer"
require "../src/compiler/frontend/parser"

source = File.read("/Users/sergey/Projects/Crystal/crystal/src/compiler/crystal/syntax/lexer.cr")
lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
program = parser.parse_program

eq_errors = parser.diagnostics.select { |d| d.message.includes?("unexpected Eq") }

puts "Found #{eq_errors.size} 'unexpected Eq' errors:\n"

eq_errors.each_with_index do |diag, i|
  puts "\n#{i+1}. Line #{diag.span.start_line}, Column #{diag.span.start_column}"
  puts "   #{diag.message}"

  # Show context (3 lines before and after)
  lines = source.lines
  line_num = diag.span.start_line

  start_line = [1, line_num - 2].max
  end_line = [lines.size, line_num + 2].min

  (start_line..end_line).each do |ln|
    marker = ln == line_num ? ">>>" : "   "
    puts "#{marker} #{ln}: #{lines[ln - 1]}"
  end
end
