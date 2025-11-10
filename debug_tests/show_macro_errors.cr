require "../src/compiler/frontend/lexer"
require "../src/compiler/frontend/parser"

# Find files with macro errors (low count to see patterns)
count = 0

Dir.glob("/Users/sergey/Projects/Crystal/crystal/src/**/*.cr") do |file|
  break if count >= 3

  source = File.read(file)

  lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
  parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
  program = parser.parse_program

  macro_errors = parser.diagnostics.select { |d|
    d.message.includes?("macro") || d.message.includes?("Percent") || d.message.includes?("%}")
  }

  # Focus on files with moderate error count (not too many)
  if macro_errors.size > 0 && macro_errors.size < 20 && parser.diagnostics.size < 30
    puts "=" * 80
    puts file
    puts "Total errors: #{parser.diagnostics.size}, Macro errors: #{macro_errors.size}"
    puts "=" * 80

    macro_errors.first(5).each do |diag|
      lines = source.lines
      line_num = diag.span.start_line

      start_line = [0, line_num - 2].max
      end_line = [lines.size - 1, line_num + 1].min

      puts "\nLine #{line_num}:"
      (start_line..end_line).each do |i|
        marker = i == line_num - 1 ? ">>> " : "    "
        puts "#{marker}#{i + 1}: #{lines[i]}"
      end
      puts "Error: #{diag.message}"
    end

    count += 1
  end
end

puts "\nShowed #{count} files"
