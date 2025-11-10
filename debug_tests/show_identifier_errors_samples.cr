require "../src/compiler/frontend/lexer"
require "../src/compiler/frontend/parser"

# Show actual code examples with "unexpected Identifier" errors
found = 0
max_examples = 15

puts "Searching for 'unexpected Identifier' errors...\n"

Dir.glob("../src/**/*.cr").each do |file|
  next if found >= max_examples

  source = File.read(file)
  lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
  parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
  program = parser.parse_program

  identifier_errors = parser.diagnostics.select { |d| d.message == "unexpected Identifier" }

  if identifier_errors.size > 0
    identifier_errors.first(2).each do |err|
      break if found >= max_examples

      lines = source.lines
      line_num = err.span.start_line
      next if line_num >= lines.size

      puts "="*70
      puts "Example #{found + 1}: #{file}:#{line_num + 1}"
      puts "-"*70

      # Show context: 2 lines before, error line, 2 lines after
      puts "  #{lines[line_num - 2].rstrip}" if line_num >= 2
      puts "  #{lines[line_num - 1].rstrip}" if line_num >= 1
      puts "→ #{lines[line_num].rstrip}"
      puts "  #{" " * err.span.start_column}^--- error here"
      puts "  #{lines[line_num + 1].rstrip}" if line_num + 1 < lines.size
      puts "  #{lines[line_num + 2].rstrip}" if line_num + 2 < lines.size
      puts

      found += 1
    end
  end
end

puts "="*70
puts "Showed #{found} examples of 'unexpected Identifier' errors"
