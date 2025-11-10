require "../src/compiler/frontend/lexer"
require "../src/compiler/frontend/parser"

# Find "unexpected RParen" errors in non-macro files
count = 0

Dir.glob("/Users/sergey/Projects/Crystal/crystal/src/**/*.cr") do |file|
  break if count >= 3

  source = File.read(file)

  lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
  parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
  program = parser.parse_program

  # Skip files with many errors (likely macro-heavy)
  next if parser.diagnostics.size > 10

  rparen_errors = parser.diagnostics.select { |d| d.message == "unexpected RParen" }

  if rparen_errors.size > 0
    puts "=" * 80
    puts file
    puts "Total errors: #{parser.diagnostics.size}"
    puts "=" * 80

    rparen_errors.first(1).each do |diag|
      count += 1
      break if count > 3

      lines = source.lines
      line_num = diag.span.start_line

      # Extract context
      start_line = [0, line_num - 3].max
      end_line = [lines.size - 1, line_num + 2].min

      puts "\nLine #{line_num}:"
      (start_line..end_line).each do |i|
        marker = i == line_num - 1 ? ">>> " : "    "
        puts "#{marker}#{i + 1}: #{lines[i]}"
      end
      puts "Error: #{diag.message}"
    end
  end
end

puts "\nShowed #{count} examples"
