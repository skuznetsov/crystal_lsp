require "../src/compiler/frontend/lexer"
require "../src/compiler/frontend/parser"

# Find "unexpected EOF" errors and show context
count = 0

Dir.glob("/Users/sergey/Projects/Crystal/crystal/src/**/*.cr") do |file|
  break if count >= 5

  source = File.read(file)

  lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
  parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
  program = parser.parse_program

  eof_errors = parser.diagnostics.select { |d| d.message.includes?("unexpected EOF") }

  if eof_errors.size > 0
    puts "=" * 80
    puts file
    puts "=" * 80

    eof_errors.first(1).each do |diag|
      count += 1
      break if count > 5

      lines = source.lines
      line_num = diag.span.start_line

      # Extract context - show last 5 lines before EOF
      start_line = [0, line_num - 5].max
      end_line = [lines.size - 1, line_num].min

      puts "\nLines #{start_line + 1}-#{end_line + 1} (before EOF):"
      (start_line..end_line).each do |i|
        marker = i == line_num - 1 ? ">>> " : "    "
        puts "#{marker}#{i + 1}: #{lines[i]}"
      end
      puts "Error: #{diag.message}"
      puts "Total lines in file: #{lines.size}"
    end
  end
end

puts "\nShowed #{count} examples"
