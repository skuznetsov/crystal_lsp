require "spec"
require "file_utils"
require "random/secure"

require "./support/server_helper"

describe CrystalV2::Compiler::LSP::Server do
  around_each do |example|
    previous = ENV["CRYSTALV2_LSP_FORCE_STUB"]?
    ENV["CRYSTALV2_LSP_FORCE_STUB"] = "1"
    begin
      example.run
    ensure
      if previous
        ENV["CRYSTALV2_LSP_FORCE_STUB"] = previous
      else
        ENV.delete("CRYSTALV2_LSP_FORCE_STUB")
      end
    end
  end

  it "resolves types from included modules" do
    base_dir = File.join(Dir.tempdir, "lsp_include_spec_#{Random::Secure.hex(6)}")
    Dir.mkdir(base_dir)
    begin
      dep_path = File.join(base_dir, "dep.cr")
      File.write(dep_path, <<-CR)
      module Foo
        module Bar
          class Baz
          end
        end
      end
      CR

      main_path = File.join(base_dir, "main.cr")
      File.write(main_path, <<-CR)
      require "./dep"

      include Foo::Bar

      Baz.new
      CR

      server = CrystalV2::Compiler::LSP::Server.new(IO::Memory.new, IO::Memory.new)
      diagnostics, _program, _type_context, _identifier_symbols, _symbol_table, _requires =
        server.spec_analyze_document(File.read(main_path), base_dir, main_path)

      diagnostics.should be_empty
    ensure
      FileUtils.rm_rf(base_dir)
    end
  end
end
