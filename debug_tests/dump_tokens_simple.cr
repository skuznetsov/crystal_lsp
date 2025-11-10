require "../src/compiler/frontend/lexer"

source = File.read("/tmp/test_simple_multiline.cr")
puts "Source:"
puts source
puts "\n" + "=" * 50 + "\n"

lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
puts "Tokens:"
lexer.each_token do |token|
  puts "#{token.kind} at #{token.span.start_line}:#{token.span.start_column}-#{token.span.end_line}:#{token.span.end_column}"
end
