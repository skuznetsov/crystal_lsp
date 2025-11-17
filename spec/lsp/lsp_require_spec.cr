require "spec"
require "file_utils"
require "random/secure"

require "../../src/compiler/lsp/server"

module CrystalV2::Compiler::LSP
  class Server
    def spec_analyze_document(source : String, base_dir : String?, path : String?)
      analyze_document(source, base_dir, path)
    end
  end
end

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

  it "merges require dependencies into a VirtualArena" do
    base_dir = File.join(Dir.tempdir, "lsp_require_spec_#{Random::Secure.hex(6)}")
    Dir.mkdir(base_dir)
    begin
      helper_path = File.join(base_dir, "helper.cr")
      File.write(helper_path, <<-CR)
      module Dep
        class Helper
          def value
            42
          end
        end
      end
      CR

      main_path = File.join(base_dir, "main.cr")
      File.write(main_path, <<-CR)
      require "./helper"

      module Entry
        def self.run
          Dep::Helper.new.value
        end
      end
      CR

      server = CrystalV2::Compiler::LSP::Server.new(IO::Memory.new, IO::Memory.new)
      diagnostics, program, _type_context, _identifier_symbols, symbol_table, requires =
        server.spec_analyze_document(File.read(main_path), base_dir, main_path)

      requires.should eq([helper_path])
      program.arena.should be_a(CrystalV2::Compiler::Frontend::VirtualArena)

      dep_symbol = symbol_table.try(&.lookup("Dep"))
      dep_symbol.should_not be_nil
      dep_symbol.should be_a(CrystalV2::Compiler::Semantic::ModuleSymbol)

    ensure
      FileUtils.rm_rf(base_dir)
    end
  end
end
