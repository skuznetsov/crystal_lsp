require "../src/compiler/frontend/lexer"
require "../src/compiler/frontend/parser"

found = 0
max_examples = 3

Dir.glob("/Users/sergey/Projects/Crystal/crystal/src/**/*.cr") do |file|
  next if found >= max_examples

  source = File.read(file)
  lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
  parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
  program = parser.parse_program

  amp_errors = parser.diagnostics.select { |d| d.message == "unexpected Amp" }

  if amp_errors.size > 0
    puts "\n" + "="*70
    puts "File: #{file}"
    puts "Amp errors: #{amp_errors.size}"
    puts "-"*70

    err = amp_errors.first
    lines = source.lines
    line_num = err.span.start_line
    col_num = err.span.start_column

    if line_num >= 0 && line_num < lines.size
      puts "\nLine #{line_num + 1}, col #{col_num + 1}:"
      puts "  #{line_num}: #{lines[line_num - 1].rstrip}" if line_num > 0
      line_text = lines[line_num].rstrip
      puts "→ #{line_num + 1}: #{line_text}"
      puts " " * (col_num + "→ #{line_num + 1}: ".size) + "^"
      puts "  #{line_num + 2}: #{lines[line_num + 1].rstrip}" if line_num + 1 < lines.size
    end

    found += 1
  end
end

puts "\n" + "="*70
puts "Showed #{found} files"
