require "../src/compiler/lsp/server"

# Dump LSP diagnostics (post-semantics) and semantic tokens for a given file
# Usage:
#   CRYSTAL_CACHE_DIR=./.crystal-cache crystal run crystal_v2/debug_tests/dump_lsp_analysis.cr -- path/to/file.cr

def main
  path = ARGV[0]? || (abort "usage: dump_lsp_analysis <file.cr>")
  source = File.read(path)

  server = CrystalV2::Compiler::LSP::Server.new
  diagnostics, tokens, using_stub, prelude_path = server.debug_analyze(source)

  puts "-- Prelude --"
  puts "path: #{prelude_path}"
  puts "using_stub: #{using_stub}"

  puts "-- LSP diagnostics (#{diagnostics.size}) --"
  diagnostics.each do |d|
    sev = d.severity || 1
    puts "sev=#{sev} msg=#{d.message.inspect} range=#{d.range.start.line + 1}:#{d.range.start.character + 1}-#{d.range.end.line + 1}:#{d.range.end.character + 1}"
  end

  puts "-- Semantic tokens (#{tokens.data.size / 5} tokens) --"
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

