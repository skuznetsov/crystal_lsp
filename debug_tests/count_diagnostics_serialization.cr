require "../src/compiler/frontend/parser"
require "../src/compiler/frontend/lexer"

source_path = "~/Projects/Crystal/crystal/src/json/serialization.cr"
expanded_path = source_path.gsub(/^~/, ENV["HOME"])
source = File.read(expanded_path)

puts "Parsing: #{expanded_path}"
puts "Size: #{source.size} bytes"
puts "=" * 80

lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
program = parser.parse_program

puts "\nResults:"
puts "  Roots: #{program.roots.size}"
puts "  Diagnostics: #{parser.diagnostics.size}"

if parser.diagnostics.size > 0
  puts "\nFirst 10 diagnostics:"
  parser.diagnostics.first(10).each_with_index do |d, i|
    puts "  #{i+1}. #{d.message} at #{d.span.start_line}:#{d.span.start_column}"
  end

  if parser.diagnostics.size > 10
    puts "  ... and #{parser.diagnostics.size - 10} more"
  end
else
  puts "\n✅ No diagnostics!"
end
