require "spec"
require "file_utils"
require "random/secure"

require "./support/server_helper"

describe CrystalV2::Compiler::LSP::Server do
  it "renames a constant across files" do
    dir = File.join(Dir.tempdir, "lsp_rename_#{Random::Secure.hex(6)}")
    FileUtils.mkdir_p(dir)

    dep_path = File.join(dir, "dep.cr")
    File.write(dep_path, <<-CR)
    module Dep
      class Helper
      end
    end
    CR

    main_path = File.join(dir, "main.cr")
    source = <<-CR
    require "./dep"

    module Entry
      def self.run
        Dep::Helper.new
      end
    end
    CR
    File.write(main_path, source)

    server = CrystalV2::Compiler::LSP::Server.new(IO::Memory.new, IO::Memory.new, CrystalV2::Compiler::LSP::ServerConfig.new(background_indexing: false, project_cache: false))
    uri = server.spec_store_document(source, dir, main_path)

    line_idx = source.lines.index { |l| l.includes?("Helper") }.not_nil!
    char_idx = source.lines[line_idx].index("Helper").not_nil!

    prep = server.spec_prepare_rename(uri, line_idx, char_idx)
    prep["result"].should_not be_nil

    rename = server.spec_rename(uri, line_idx, char_idx, "Worker")
    rename["result"].should_not be_nil
    changes = rename["result"]["changes"].as_h
    changes.keys.to_set.should eq(Set{server.spec_file_uri(dep_path), uri})
  ensure
    FileUtils.rm_rf(dir) if dir
  end
end
