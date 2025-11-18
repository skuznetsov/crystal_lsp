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
puts "token ints=#{tokens.data.size / 5}"
# decode sample
line = 0
col = 0
encoded = tokens.data
encoded.each_slice(5).with_index do |(delta_line, delta_col, length, type_id, modifier), idx|
  line += delta_line
  col = delta_line.zero? ? col + delta_col : delta_col
  puts "#{idx}: line=#{line} col=#{col} len=#{length} type=#{type_id}"
  break if idx > 20
end
