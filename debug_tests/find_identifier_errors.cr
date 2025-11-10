require "../src/compiler/frontend/lexer"
require "../src/compiler/frontend/parser"

# Find examples of "unexpected Identifier" errors
found = 0
max_examples = 5

Dir.glob("../src/**/*.cr").each do |file|
  next if found >= max_examples

  source = File.read(file)
  lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
  parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
  program = parser.parse_program

  id_errors = parser.diagnostics.select { |d| d.message.includes?("Identifier") }

  if id_errors.size > 0
    puts "\n" + "="*70
    puts "File: #{file}"
    puts "Total Identifier-related errors: #{id_errors.size}"
    puts "Messages: #{id_errors.map(&.message).uniq.join(", ")}"
    puts "-"*70

    id_errors.first(2).each do |err|
      lines = source.lines
      line_num = err.span.start_line
      if line_num >= 0 && line_num < lines.size
        puts "\nLine #{line_num + 1}: #{err.message}"
        puts "  #{lines[line_num - 1].rstrip}" if line_num > 0
        puts "→ #{lines[line_num].rstrip}"
        puts "  #{lines[line_num + 1].rstrip}" if line_num + 1 < lines.size
      end
    end

    found += 1
  end
end

puts "\n" + "="*70
puts "Showed #{found} files"
