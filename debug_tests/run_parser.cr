require "../src/compiler/frontend/parser"

source = File.read(ARGV[0])
lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
program = parser.parse_program

STDOUT.puts program.roots.size
program.roots.each_with_index do |id, i|
  node = program.arena[id]
  kind = CrystalV2::Compiler::Frontend.node_kind(node)
  STDOUT.puts "root[#{i}] kind=#{kind} span=#{node.span.start_line}:#{node.span.start_column}-#{node.span.end_line}:#{node.span.end_column}"
end
