require "../src/compiler/frontend/lexer"
require "../src/compiler/frontend/parser"

found = 0
max_examples = 5

Dir.glob("/Users/sergey/Projects/Crystal/crystal/src/**/*.cr") do |file|
  next if found >= max_examples

  source = File.read(file)
  lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
  parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
  program = parser.parse_program

  id_errors = parser.diagnostics.select { |d| d.message == "unexpected Identifier" }

  if id_errors.size > 0
    puts "\n" + "="*70
    puts "File: #{file}"
    puts "Errors: #{id_errors.size}"
    puts "-"*70

    id_errors.first(2).each do |err|
      lines = source.lines
      line_num = err.span.start_line
      col_num = err.span.start_column

      if line_num >= 0 && line_num < lines.size
        puts "\nLine #{line_num + 1}, column #{col_num + 1}:"

        # Show context
        if line_num > 0
          puts "  #{line_num}: #{lines[line_num - 1].rstrip}"
        end

        line_text = lines[line_num].rstrip
        puts "→ #{line_num + 1}: #{line_text}"

        # Show column pointer
        pointer = " " * (col_num + "→ #{line_num + 1}: ".size) + "^"
        puts pointer

        if line_num + 1 < lines.size
          puts "  #{line_num + 2}: #{lines[line_num + 1].rstrip}"
        end
      end
    end

    found += 1
  end
end

puts "\n" + "="*70
puts "Showed #{found} files with 'unexpected Identifier' errors"
