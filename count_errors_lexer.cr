require "./src/compiler/frontend/lexer"
require "./src/compiler/frontend/parser"

lexer_file = "/Users/sergey/Projects/Crystal/crystal/src/compiler/crystal/syntax/lexer.cr"
content = File.read(lexer_file)

lexer = CrystalV2::Compiler::Frontend::Lexer.new(content)
parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
program = parser.parse_program

puts "Total diagnostics: #{parser.diagnostics.size}"
puts

# Count by message
counts = Hash(String, Int32).new(0)
parser.diagnostics.each do |diag|
  counts[diag.message] += 1
end

# Sort and display
counts.to_a.sort_by { |(msg, count)| -count }.each do |(msg, count)|
  puts "#{count}x #{msg}"
end
