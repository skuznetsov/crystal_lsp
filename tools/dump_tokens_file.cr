require "../src/compiler/frontend/lexer"
abort "Usage: dump_tokens_file <file>" unless ARGV.size == 1
path = ARGV[0]
source = File.read(path)
lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
lexer.each_token(skip_trivia: false) do |t|
  text = String.new(t.slice)
  puts "%4d:%-3d %-18s %s" % [t.span.start_line + 1, t.span.start_column + 1, t.kind, text.inspect]
end
