require "spec"
require "file_utils"
require "random/secure"

require "./support/server_helper"

describe CrystalV2::Compiler::LSP::Server do
  it "soft-fails hover when indexing is in progress" do
    dir = File.join(Dir.tempdir, "lsp_hover_idx_#{Random::Secure.hex(6)}")
    FileUtils.mkdir_p(dir)
    path = File.join(dir, "main.cr")
    source = "value = 1\n"
    File.write(path, source)

    server = CrystalV2::Compiler::LSP::Server.new(IO::Memory.new, IO::Memory.new, CrystalV2::Compiler::LSP::ServerConfig.new(background_indexing: false, project_cache: false))
    diagnostics, program, _tc, _ids, _symtab, requires = server.spec_analyze_document(source, dir, path)
    diagnostics.should be_empty

    text_doc = CrystalV2::Compiler::LSP::TextDocumentItem.new(uri: server.spec_file_uri(path), language_id: "crystal", version: 1, text: source)
    # Simulate indexing in progress by clearing symbol table and identifier symbols.
    doc_state = CrystalV2::Compiler::LSP::DocumentState.new(text_doc, program, nil, nil, nil, requires, path)
    server.spec_set_document(doc_state)
    uri = text_doc.uri

    hover = server.spec_hover(uri, 0, 0)
    hover["result"].should_not be_nil
    hover["result"]["contents"]["value"].as_s.includes?("Indexing").should be_true
  ensure
    FileUtils.rm_rf(dir) if dir
  end
end
