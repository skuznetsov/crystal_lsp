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

  it "finds references across a required dependency" do
    dir = File.join(Dir.tempdir, "lsp_refs_#{Random::Secure.hex(6)}")
    FileUtils.mkdir_p(dir)
    dep_path = File.join(dir, "dep.cr")
    File.write(dep_path, <<-CR)
    module Dep
      class Helper
      end
    end
    CR

    main_path = File.join(dir, "main.cr")
    File.write(main_path, <<-CR)
    require "./dep"

    module Entry
      def self.run
        Dep::Helper.new
      end
    end
    CR

    server = CrystalV2::Compiler::LSP::Server.new(IO::Memory.new, IO::Memory.new, CrystalV2::Compiler::LSP::ServerConfig.new(background_indexing: false, project_cache: false))
    source = File.read(main_path)
    uri = server.spec_store_document(source, dir, main_path)

    line_idx = source.lines.index { |l| l.includes?("Helper") }.not_nil!
    char_idx = source.lines[line_idx].index("Helper").not_nil!

    # Position on the constant "Helper" in the main file
    refs = server.spec_references(uri, line_idx, char_idx, include_declaration: true)
    refs["result"].should_not be_nil
    locations = refs["result"].as_a
    # Expect two references: declaration in dep.cr and usage in main.cr
    locations.size.should eq(2)
    uris = locations.map { |loc| loc.as_h["uri"].as_s }.to_set
    uris.should eq(Set{server.spec_file_uri(dep_path), uri})
  ensure
    FileUtils.rm_rf(dir) if dir
  end
end
