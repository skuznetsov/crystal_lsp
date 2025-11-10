require "../src/compiler/frontend/lexer"
require "../src/compiler/frontend/parser"
require "../src/compiler/semantic/analyzer"

source = File.read("../debug_tests/check_lexer.cr")
lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
program = parser.parse_program
analyzer = CrystalV2::Compiler::Semantic::Analyzer.new(program)
analyzer.collect_symbols
result = analyzer.resolve_names
result.identifier_symbols.each do |expr_id, symbol|
  node = program.arena[expr_id]
  span = node.span
  text = source.lines[span.start_line - 1][span.start_column - 1...span.end_column - 1] rescue "?"
  puts "expr #{expr_id.index}: #{node.class} => #{symbol.class} text=#{text.inspect}"
end
