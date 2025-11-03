require "../src/compiler/frontend/lexer"
require "../src/compiler/frontend/parser"

ast_file = "/Users/sergey/Projects/Crystal/crystal/src/compiler/crystal/syntax/ast.cr"
content = File.read(ast_file)

lexer = CrystalV2::Compiler::Frontend::Lexer.new(content)
parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
program = parser.parse_program

# Find all "unexpected Else" errors
else_errors = parser.diagnostics.select { |d| d.message == "unexpected Else" }

puts "Found #{else_errors.size} 'unexpected Else' errors"
puts "=" * 80

else_errors.each_with_index do |diag, idx|
  puts "\n#{idx + 1}. Error at line #{diag.span.start_line + 1}:"

  # Extract line from content
  lines = content.lines
  line_num = diag.span.start_line

  if line_num >= 0 && line_num < lines.size
    # Show context: 3 lines before, error line, 2 lines after
    start = [0, line_num - 3].max
    finish = [lines.size - 1, line_num + 2].min

    (start..finish).each do |i|
      prefix = i == line_num ? ">>> " : "    "
      puts "#{prefix}#{i + 1}: #{lines[i].chomp}"
    end
  end

  puts "-" * 80
end
