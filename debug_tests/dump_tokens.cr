require "../src/compiler/frontend/lexer"

source = <<-'CR2'
  macro newline_macro
    {{ value }}\
    line2
  end
CR2
lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
lexer.each_token(skip_trivia: false) { |t| puts "#{t.kind} '#{String.new(t.slice)}' @ #{t.span.start_line}:#{t.span.start_column}" }
