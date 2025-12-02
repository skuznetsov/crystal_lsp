require "spec"
require "file_utils"
require "random/secure"

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

  it "provides hover and definition for a prelude type" do
    dir = File.join(Dir.tempdir, "lsp_hover_pre_#{Random::Secure.hex(6)}")
    FileUtils.mkdir_p(dir)
    path = File.join(dir, "main.cr")
    source = "Time.now\n"
    File.write(path, source)

    server = CrystalV2::Compiler::LSP::Server.new(IO::Memory.new, IO::Memory.new, CrystalV2::Compiler::LSP::ServerConfig.new(background_indexing: false, project_cache: false))
    uri = server.spec_store_document(source, dir, path)

    col = source.index("Time").not_nil!
    hover = server.spec_hover(uri, 0, col)
    hover["result"].should_not be_nil

    definition = server.spec_definition(uri, 0, col)
    definition["result"].should_not be_nil
    definition["result"].as_a.size.should be >= 1
  ensure
    FileUtils.rm_rf(dir) if dir
  end
end
