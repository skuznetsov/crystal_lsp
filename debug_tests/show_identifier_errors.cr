require "../src/compiler/frontend/lexer"
require "../src/compiler/frontend/parser"

# Find examples of "unexpected Identifier" errors
found = 0
max_examples = 10

Dir.glob("../src/**/*.cr").each do |file|
  next if found >= max_examples

  source = File.read(file)
  lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
  parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
  program = parser.parse_program

  id_errors = parser.diagnostics.select { |d| d.message == "unexpected Identifier" }

  if id_errors.size > 0
    puts "\n" + "="*70
    puts "File: #{file}"
    puts "Total 'unexpected Identifier' errors: #{id_errors.size}"
    puts "-"*70

    id_errors.first(3).each do |err|
      lines = source.lines
      line_num = err.span.start_line
      if line_num >= 0 && line_num < lines.size
        # Show context: line before, error line, line after
        puts "\nLine #{line_num + 1}:"
        puts "  #{lines[line_num - 1].rstrip}" if line_num > 0
        puts "→ #{lines[line_num].rstrip}"
        puts "  #{lines[line_num + 1].rstrip}" if line_num + 1 < lines.size

        # Show a few tokens around the error
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
          start_idx = [0, error_token_idx - 3].max
          end_idx = [tokens.size - 1, error_token_idx + 3].min
          puts "\nTokens around error:"
          (start_idx..end_idx).each do |i|
            tok = tokens[i]
            next if tok.kind == CrystalV2::Compiler::Frontend::Token::Kind::Whitespace
            next if tok.kind == CrystalV2::Compiler::Frontend::Token::Kind::Newline
            prefix = i == error_token_idx ? "→ " : "  "
            text = String.new(tok.slice)[0...30]
            puts "#{prefix}#{tok.kind}: #{text.inspect}"
          end
        end
      end
    end

    found += 1
  end
end

puts "\n" + "="*70
puts "Showed #{found} files with 'unexpected Identifier' errors"
