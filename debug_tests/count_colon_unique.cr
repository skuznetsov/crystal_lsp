require "../src/compiler/frontend/lexer"
require "../src/compiler/frontend/parser"

ast_file = "/Users/sergey/Projects/Crystal/crystal/src/compiler/crystal/syntax/ast.cr"
content = File.read(ast_file)

lexer = CrystalV2::Compiler::Frontend::Lexer.new(content)
parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
program = parser.parse_program

# Find all "unexpected Colon" errors
colon_errors = parser.diagnostics.select { |d| d.message == "unexpected Colon" }

# Group by line number
by_line = Hash(Int32, Int32).new(0)
colon_errors.each do |diag|
  by_line[diag.span.start_line] += 1
end

puts "Total 'unexpected Colon' errors: #{colon_errors.size}"
puts "\nGrouped by line:"
by_line.to_a.sort_by { |line, count| -count }.each do |(line, count)|
  puts "  Line #{line + 1}: #{count}x"
end

puts "\nUnique lines with errors: #{by_line.size}"
