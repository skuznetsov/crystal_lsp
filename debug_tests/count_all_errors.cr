require "../src/compiler/frontend/lexer"
require "../src/compiler/frontend/parser"

# Count ALL diagnostics across entire codebase
total = 0
counts = Hash(String, Int32).new(0)

Dir.glob("/Users/sergey/Projects/Crystal/crystal/src/**/*.cr") do |file|
  source = File.read(file)
  
  lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
  parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
  program = parser.parse_program
  
  total += parser.diagnostics.size
  parser.diagnostics.each do |diag|
    counts[diag.message] += 1
  end
end

puts "Total diagnostics across all files: #{total}"

puts "\nBreakdown:"
counts.to_a.sort_by { |k, v| -v }.each do |(msg, count)|
  puts "  #{count}x #{msg}"
end
