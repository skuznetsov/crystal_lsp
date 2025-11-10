require "./src/compiler/frontend/lexer"
require "./src/compiler/frontend/parser"

# Analyze error patterns in ast.cr
ast_file = "/Users/sergey/Projects/Crystal/crystal/src/compiler/crystal/syntax/ast.cr"
content = File.read(ast_file)

lexer = CrystalV2::Compiler::Frontend::Lexer.new(content)
parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
program = parser.parse_program

puts "Total diagnostics: #{parser.diagnostics.size}\n"

# Group by error message
grouped = Hash(String, Int32).new(0)
parser.diagnostics.each do |diag|
  grouped[diag.message] += 1
end

puts "Error breakdown:"
grouped.to_a.sort_by { |k, v| -v }.each do |msg, count|
  puts "  #{count}x: #{msg}"
end
