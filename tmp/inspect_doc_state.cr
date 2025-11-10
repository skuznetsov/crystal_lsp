require "../src/compiler/lsp/server"

class Inspector < CrystalV2::Compiler::LSP::Server
  def analyze_public(source : String, base : String)
    analyze_document(source, base)
  end
end

server = Inspector.new(IO::Memory.new, IO::Memory.new)
source = File.read("../benchmarks/bench_parser_single.cr")
base = File.dirname(File.expand_path("../benchmarks/bench_parser_single.cr", __DIR__))
info = server.analyze_public(source, base)
puts({diagnostics: info[0].size, type_context_nil: info[2].nil?, identifier_symbols: info[3]?.try(&.size) || 0, requires: info[5].size}.to_s)
