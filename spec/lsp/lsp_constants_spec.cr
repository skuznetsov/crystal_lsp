require "spec"

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

  it "registers constant symbols in the global table" do
    source = <<-CR
    PARSER_FILE = "/tmp/foo"
    PARSER_FILE
    CR

    server = CrystalV2::Compiler::LSP::Server.new(IO::Memory.new, IO::Memory.new)
    diagnostics, _program, _type_context, identifier_symbols, symbol_table, _requires =
      server.spec_analyze_document(source, nil, nil)

    diagnostics.should be_empty
    symbol_table.should_not be_nil
    const_symbol = symbol_table.not_nil!.lookup("PARSER_FILE")
    const_symbol.should be_a(CrystalV2::Compiler::Semantic::ConstantSymbol)
    identifier_symbols.should_not be_nil
    identifier_symbols.not_nil!.values.should contain(const_symbol.not_nil!)
  end
end
