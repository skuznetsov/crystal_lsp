require "spec"

require "./support/server_helper"

describe CrystalV2::Compiler::LSP::Server do
  around_each do |example|
    prev = ENV["CRYSTALV2_LSP_FORCE_STUB"]?
    ENV["CRYSTALV2_LSP_FORCE_STUB"] = "1"
    begin
      example.run
    ensure
      if prev
        ENV["CRYSTALV2_LSP_FORCE_STUB"] = prev
      else
        ENV.delete("CRYSTALV2_LSP_FORCE_STUB")
      end
    end
  end

  it "returns parameter and type inlay hints for a simple call" do
    source = <<-CR
    def add(x : Int32, y : Int32)
      x + y
    end

    total = add(1, 2)
    CR

    server = CrystalV2::Compiler::LSP::Server.new(IO::Memory.new, IO::Memory.new, CrystalV2::Compiler::LSP::ServerConfig.new(background_indexing: false, project_cache: false))
    uri = server.spec_store_document(source, nil, "/tmp/inlay.cr")

    hints = server.spec_inlay_hints(uri, 0, 0, 5, 0)
    hints["result"].should_not be_nil
    hints["result"].as_a.size.should be > 0
  end
end
