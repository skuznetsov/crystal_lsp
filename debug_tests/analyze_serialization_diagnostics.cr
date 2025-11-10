require "../src/compiler/frontend/parser"
require "../src/compiler/frontend/lexer"

file = "../src/json/serialization.cr"
puts "Analyzing diagnostics in #{file}...\n"

source = File.read(file)
lines = source.lines

lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
program = parser.parse_program

puts "Total diagnostics: #{parser.diagnostics.size}\n\n"

parser.diagnostics.each_with_index do |d, idx|
  puts "=== Diagnostic #{idx + 1}/#{parser.diagnostics.size} ==="
  puts "Message: #{d.message}"

  # Find approximate location by searching for the error context
  # Since we don't have exact positions, let's show the diagnostic info we have
  puts "Diagnostic details: #{d.inspect}"
  puts ""
end

# Let's also try to get some context by re-parsing with debug
puts "\n=== Attempting to find error locations ==="
puts "Re-parsing to identify problematic patterns...\n"

# Search for common patterns that might cause these errors
patterns = {
  "PercentRBrace" => /%\}/,
  "ColonColon" => /::/,
  "Star" => /\*/,
  "RBracket" => /\]/,
}

patterns.each do |name, pattern|
  matches = source.scan(pattern)
  if matches.size > 0
    puts "#{name}: Found #{matches.size} occurrences"
  end
end
