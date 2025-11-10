require "../src/compiler/frontend/lexer"
require "../src/compiler/frontend/parser"

# Show examples of each error type
error_examples = Hash(String, Array(String)).new { |h, k| h[k] = [] of String }

Dir.glob("/Users/sergey/Projects/Crystal/crystal/src/**/*.cr") do |file|
  next if error_examples.values.all? { |v| v.size >= 2 }  # Stop when we have 2 examples of each

  source = File.read(file)

  lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
  parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
  program = parser.parse_program

  parser.diagnostics.each do |diag|
    error_examples[diag.message] << file if error_examples[diag.message].size < 2
  end
end

# Show top error types with file examples
puts "Error breakdown with examples:\n\n"

error_counts = Hash(String, Int32).new(0)
Dir.glob("/Users/sergey/Projects/Crystal/crystal/src/**/*.cr") do |file|
  source = File.read(file)
  lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
  parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
  program = parser.parse_program
  parser.diagnostics.each { |d| error_counts[d.message] += 1 }
end

error_counts.to_a.sort_by { |k, v| -v }.first(10).each do |message, count|
  puts "#{count}x #{message}"
  if error_examples[message]?
    error_examples[message].first(2).each { |file| puts "  - #{file}" }
  end
  puts
end
