require "../src/compiler/frontend/lexer"
require "../src/compiler/frontend/parser"

# Find examples of "unexpected RParen" errors
found = 0
max_examples = 10

Dir.glob("../src/**/*.cr").each do |file|
  next if found >= max_examples

  source = File.read(file)
  lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
  parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
  program = parser.parse_program

  rparen_errors = parser.diagnostics.select { |d| d.message == "unexpected RParen" }

  if rparen_errors.size > 0
    puts "\n" + "="*70
    puts "File: #{file}"
    puts "Total 'unexpected RParen' errors: #{rparen_errors.size}"
    puts "-"*70

    rparen_errors.first(3).each do |err|
      lines = source.lines
      line_num = err.span.start_line
      if line_num >= 0 && line_num < lines.size
        # Show context: 2 lines before, error line, 2 lines after
        puts "\nLine #{line_num + 1}:"
        puts "  #{lines[line_num - 2].rstrip}" if line_num >= 2
        puts "  #{lines[line_num - 1].rstrip}" if line_num >= 1
        puts "→ #{lines[line_num].rstrip}"
        puts "  #{lines[line_num + 1].rstrip}" if line_num + 1 < lines.size
        puts "  #{lines[line_num + 2].rstrip}" if line_num + 2 < lines.size

        # Show tokens around error position
        lexer2 = CrystalV2::Compiler::Frontend::Lexer.new(source)
        tokens = [] of CrystalV2::Compiler::Frontend::Token
        loop do
          tok = lexer2.next_token
          tokens << tok
          break if tok.kind == CrystalV2::Compiler::Frontend::Token::Kind::EOF
        end

        # Find token at error position
        error_token_idx = tokens.index { |t| t.span.start_line == line_num && t.span.start_column >= err.span.start_column }
        if error_token_idx
          start_idx = [0, error_token_idx - 4].max
          end_idx = [tokens.size - 1, error_token_idx + 4].min
          puts "\nTokens around error:"
          (start_idx..end_idx).each do |i|
            tok = tokens[i]
            next if tok.kind == CrystalV2::Compiler::Frontend::Token::Kind::Whitespace
            next if tok.kind == CrystalV2::Compiler::Frontend::Token::Kind::Newline
            prefix = i == error_token_idx ? "→ " : "  "
            text = String.new(tok.slice)[0...40]
            puts "#{prefix}#{tok.kind}: #{text.inspect}"
          end
        end
      end
    end

    found += 1
  end
end

puts "\n" + "="*70
puts "Showed #{found} files with 'unexpected RParen' errors"
