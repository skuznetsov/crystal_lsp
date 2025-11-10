require "../src/compiler/frontend/lexer"
require "../src/compiler/frontend/parser"
require "../src/compiler/lsp/server"

# Dump LSP semantic tokens and parser diagnostics for a given Crystal source file.
# Usage:
#   CRYSTAL_CACHE_DIR=./.crystal-cache crystal run crystal_v2/debug_tests/dump_lsp_semantic_tokens.cr -- path/to/file.cr

def main
  path = ARGV[0]? || (abort "usage: dump_lsp_semantic_tokens <file.cr>")
  source = File.read(path)

  lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
  parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
  program = parser.parse_program

  puts "-- Parser diagnostics (#{parser.diagnostics.size}) --"
  parser.diagnostics.each do |d|
    # Frontend::Diagnostic only exposes message and span
    puts "parser: #{d.message} @ #{d.span.start_line}:#{d.span.start_column}"
  end

  server = CrystalV2::Compiler::LSP::Server.new
  tokens = server.collect_semantic_tokens(program, source)

  puts "-- Semantic tokens (delta-encoded length=#{tokens.data.size}) --"
  data = tokens.data
  line = 0
  start = 0
  i = 0
  lines = source.lines
  while i < data.size
    dl = data[i]; ds = data[i + 1]; length = data[i + 2]; kind = data[i + 3]; mods = data[i + 4]
    line += dl
    start = (dl == 0) ? start + ds : ds
    text = lines[line]? || ""
    snippet = length > 0 ? text.byte_slice(start, length) : ""
    printf "%5d:%-4d len=%-3d kind=%-2d text=\"%s\"\n", line + 1, start + 1, length, kind, snippet
    i += 5
  end
end

main
