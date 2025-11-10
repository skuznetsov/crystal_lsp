require "../src/compiler/lsp/server"

class Inspector < CrystalV2::Compiler::LSP::Server
  def analyze_public(source : String, base : String)
    analyze_document(source, base)
  end
end

server = Inspector.new(IO::Memory.new, IO::Memory.new)
source = File.read("../src/main.cr")
base = File.dirname(File.expand_path("../src/main.cr", __DIR__))
info = server.analyze_public(source, base)
pp info[5]
