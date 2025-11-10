require "../src/compiler/frontend/lexer"

tests = [
  "x = 0x80000000_u32",
  "y = (0x80000000_u32)",
]

tests.each_with_index do |source, idx|
  puts "\n" + "="*60
  puts "Test #{idx + 1}: #{source}"
  puts "-"*60
  puts "Tokens:"
  
  lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
  loop do
    tok = lexer.next_token
    break if tok.kind == CrystalV2::Compiler::Frontend::Token::Kind::EOF
    if tok.slice.size > 0
      puts "  #{tok.kind}: '#{String.new(tok.slice)}'"
    else
      puts "  #{tok.kind}"
    end
  end
end
