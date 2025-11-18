require "../src/compiler/lsp/server"

module CrystalV2::Compiler::LSP
  class Server
    def spec_analyze(path)
      source = File.read(path)
      analyze_document(source, File.dirname(path), path)
    end
  end
end

server = CrystalV2::Compiler::LSP::Server.new(IO::Memory.new, IO::Memory.new)
path = "crystal_v2/benchmarks/bench_parser_single.cr"
_, program, type_context, identifier_symbols, symbol_table, requires = server.spec_analyze(path)
source = File.read(path)
tokens = server.collect_semantic_tokens(program, source, identifier_symbols, type_context, symbol_table, path)
line = 0
col = 0
encoded = tokens.data
encoded.each_slice(5).with_index do |(dl, dc, len, type_id, mod), idx|
  line += dl
  col = dl.zero? ? col + dc : dc
  puts "#{line}:#{col} len=#{len} type=#{type_id}"
end
