require "../src/compiler/frontend/lexer"
require "../src/compiler/frontend/parser"

# Find code with potential named arguments pattern: foo(name: value)
found = 0
max_examples = 15

puts "Searching for named argument patterns...\n"

Dir.glob("../src/**/*.cr").each do |file|
  next if found >= max_examples

  source = File.read(file)

  # Look for pattern: identifier(stuff, name: value)
  # Regex to find: word ( ... word : ...
  if source =~ /\w+\([^)]*\w+\s*:\s*[^)]/
    lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
    parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
    program = parser.parse_program

    # Check if this file has Colon or RParen errors
    relevant_errors = parser.diagnostics.select { |d|
      d.message == "unexpected Colon" || d.message == "unexpected RParen"
    }

    if relevant_errors.size > 0
      lines = source.lines

      # Find line with pattern
      lines.each_with_index do |line, idx|
        if line =~ /\w+\([^)]*\w+\s*:\s*/ && found < max_examples
          puts "="*70
          puts "Example #{found + 1}: #{file}:#{idx + 1}"
          puts "-"*70
          puts "  #{lines[idx - 1].rstrip}" if idx >= 1
          puts "→ #{line.rstrip}"
          puts "  #{lines[idx + 1].rstrip}" if idx + 1 < lines.size

          # Show relevant errors near this line
          nearby_errors = relevant_errors.select { |e| (e.span.start_line - idx).abs <= 2 }
          if nearby_errors.size > 0
            puts "\n  Errors:"
            nearby_errors.first(3).each { |e| puts "    - Line #{e.span.start_line + 1}: #{e.message}" }
          end
          puts

          found += 1
          break
        end
      end
    end
  end
end

puts "="*70
puts "Found #{found} examples with named argument pattern and errors"
