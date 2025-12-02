require "spec"

require "./support/server_helper"

describe CrystalV2::Compiler::LSP::Server do

  it "reports parser diagnostics on invalid code" do
    prev_sem = ENV["CRYSTALV2_LSP_ENABLE_SEMANTIC_DIAGNOSTICS"]?
    ENV["CRYSTALV2_LSP_ENABLE_SEMANTIC_DIAGNOSTICS"] = "1"
    ENV.delete("CRYSTALV2_LSP_FORCE_STUB")
    source = "Foo.bar\n"
    server = CrystalV2::Compiler::LSP::Server.new(IO::Memory.new, IO::Memory.new, CrystalV2::Compiler::LSP::ServerConfig.new(background_indexing: false, project_cache: false, parser_recovery_mode: false))

    diagnostics, _program, _type_context, _identifier_symbols, _symbol_table, _requires =
      server.spec_analyze_document(source, nil, nil)

    diagnostics.should_not be_empty
  ensure
    if prev_sem
      ENV["CRYSTALV2_LSP_ENABLE_SEMANTIC_DIAGNOSTICS"] = prev_sem
    else
      ENV.delete("CRYSTALV2_LSP_ENABLE_SEMANTIC_DIAGNOSTICS")
    end
  end
end
