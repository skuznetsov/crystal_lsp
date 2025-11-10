require "../src/compiler/frontend/lexer"
require "../src/compiler/frontend/parser"

# Find files with "Unexpected macro control keyword 'end'" errors
Dir.glob("../src/**/*.cr").each do |file|
  source = File.read(file)
  next unless source.includes?("{% end %}")

  lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
  parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
  program = parser.parse_program

  end_errors = parser.diagnostics.select { |d| d.message.includes?("Unexpected macro control keyword 'end'") }

  if end_errors.size > 0
    puts "\n" + "="*60
    puts "File: #{file}"
    puts "Errors: #{end_errors.size}"
    puts "-"*60

    end_errors.first(3).each do |err|
      # Find the line in source
      lines = source.lines
      line_num = err.span.start_line
      if line_num >= 0 && line_num < lines.size
        puts "Line #{line_num + 1}: #{lines[line_num].strip[0...80]}"
        puts "  Error: #{err.message}"
      end
    end

    break  # Just show first file
  end
end
