require "../src/compiler/frontend/lexer"
require "../src/compiler/frontend/parser"
source = File.read("src/compiler/lsp/server.cr")
lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
parser.parse_program
parser.diagnostics.each do |diag|
  puts "#{diag.message} @ #{diag.span.start_line}:#{diag.span.start_column}"
end
