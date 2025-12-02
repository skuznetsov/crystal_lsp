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

  it "returns hover then definition for a path in the same file" do
    dir = File.join(Dir.tempdir, "lsp_hover_def_#{Random::Secure.hex(6)}")
    FileUtils.mkdir_p(dir)
    path = File.join(dir, "main.cr")
    source = <<-CR
    module Foo
      class Bar
        def run
          1
        end
      end
    end

    result = Foo::Bar.new.run
    CR
    File.write(path, source)

    server = CrystalV2::Compiler::LSP::Server.new(IO::Memory.new, IO::Memory.new, CrystalV2::Compiler::LSP::ServerConfig.new(background_indexing: false, project_cache: false))
    uri = server.spec_store_document(source, dir, path)

    bar_offset = source.index("Bar").not_nil!
    line_idx = source[0, bar_offset].count('\n')
    last_nl = source.rindex('\n', bar_offset) || -1
    char_idx = bar_offset - last_nl - 1

    hover = server.spec_hover(uri, line_idx, char_idx) # position on Foo::Bar
    hover["result"].should_not be_nil

    definition = server.spec_definition(uri, line_idx, char_idx)
    definition["result"].should_not be_nil
    locations = definition["result"].as_a
    locations.size.should eq(1)
    loc = locations.first.as_h
    loc["uri"].as_s.should eq(uri)
  ensure
    FileUtils.rm_rf(dir) if dir
  end
end
