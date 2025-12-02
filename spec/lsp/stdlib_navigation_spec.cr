require "spec"

require "./support/server_helper"

describe CrystalV2::Compiler::LSP::Server do
  it "provides hover/definition for Time.now (stdlib)" do
    source = "Time.now\n"
    server = CrystalV2::Compiler::LSP::Server.new(IO::Memory.new, IO::Memory.new, CrystalV2::Compiler::LSP::ServerConfig.new(background_indexing: false, project_cache: false))
    uri = server.spec_store_document(source, nil, "/tmp/stdnav.cr")

    col = source.index("Time").not_nil!
    hover = server.spec_hover(uri, 0, col)
    hover["result"].should_not be_nil

    definition = server.spec_definition(uri, 0, col)
    definition["result"].should_not be_nil
    definition["result"].as_a.size.should be >= 1
  end

  it "provides hover/definition for File.basename (stdlib)" do
    source = "File.basename(\"/tmp/foo\")\n"
    server = CrystalV2::Compiler::LSP::Server.new(IO::Memory.new, IO::Memory.new, CrystalV2::Compiler::LSP::ServerConfig.new(background_indexing: false, project_cache: false))
    uri = server.spec_store_document(source, nil, "/tmp/stdnav2.cr")

    col = source.index("File").not_nil!
    hover = server.spec_hover(uri, 0, col)
    hover["result"].should_not be_nil

    definition = server.spec_definition(uri, 0, col)
    definition["result"].should_not be_nil
    definition["result"].as_a.size.should be >= 1
  end
end
