require "./src/compiler/frontend/lexer"
require "./src/compiler/frontend/parser"

total_files = 0
total_errors = 0
error_counts = Hash(String, Int32).new(0)

Dir.glob("/Users/sergey/Projects/Crystal/crystal/src/**/*.cr") do |file|
  total_files += 1

  source = File.read(file)

  lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
  parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
  program = parser.parse_program

  parser.diagnostics.each do |diagnostic|
    total_errors += 1
    error_counts[diagnostic.message] += 1
  end
end

puts "Total files: #{total_files}"
puts "Total diagnostics: #{total_errors}"
puts

# Sort by count descending
error_counts.to_a.sort_by { |k, v| -v }.each do |message, count|
  puts "#{count}x #{message}"
end
