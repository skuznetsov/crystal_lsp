require "../src/compiler/frontend/lexer"
require "../src/compiler/frontend/parser"

# Show token sequence around Percent errors
found = 0
max_examples = 10

puts "Analyzing token context around 'unexpected Percent' errors...\n"

Dir.glob("../src/**/*.cr").each do |file|
  next if found >= max_examples

  source = File.read(file)

  # First pass: parse to find errors
  lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
  parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
  program = parser.parse_program

  percent_errors = parser.diagnostics.select { |d| d.message == "unexpected Percent" }

  if percent_errors.size > 0
    # Second pass: tokenize to show context
    lexer2 = CrystalV2::Compiler::Frontend::Lexer.new(source)
    tokens = [] of CrystalV2::Compiler::Frontend::Token
    loop do
      tok = lexer2.next_token
      tokens << tok
      break if tok.kind == CrystalV2::Compiler::Frontend::Token::Kind::EOF
    end

    percent_errors.first(1).each do |err|
      break if found >= max_examples

      # Find token at error position
      error_token_idx = tokens.index { |t|
        t.span.start_line == err.span.start_line &&
        t.span.start_column >= err.span.start_column
      }

      if error_token_idx
        puts "="*70
        puts "Example #{found + 1}: #{file}"
        puts "-"*70

        # Show 5 tokens before and after
        start_idx = [0, error_token_idx - 5].max
        end_idx = [tokens.size - 1, error_token_idx + 5].min

        (start_idx..end_idx).each do |i|
          tok = tokens[i]
          prefix = i == error_token_idx ? "→ " : "  "
          text = String.new(tok.slice)[0...50]
          kind = tok.kind.to_s.gsub("CrystalV2::Compiler::Frontend::Token::Kind::", "")
          puts "#{prefix}#{kind}: #{text.inspect}"
        end
        puts

        found += 1
      end
    end
  end
end

puts "="*70
puts "Analyzed #{found} Percent error contexts"
