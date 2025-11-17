require "../../../src/compiler/lsp/server"

module CrystalV2::Compiler::LSP
  class Server
    def spec_analyze_document(source : String, base_dir : String?, path : String?)
      analyze_document(source, base_dir, path)
    end
  end
end
