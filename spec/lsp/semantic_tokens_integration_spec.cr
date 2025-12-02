require "spec"

require "../../src/compiler/frontend/parser"
require "../../src/compiler/lsp/server"

module SemanticTokensIntegrationHelper
  def self.collect_tokens(source : String)
    lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
    parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
    program = parser.parse_program
    server = CrystalV2::Compiler::LSP::Server.new
    server.collect_semantic_tokens(program, source)
  end

  def self.decode(tokens : CrystalV2::Compiler::LSP::SemanticTokens)
    data = tokens.data
    line = 0
    start = 0
    out = [] of {Int32, Int32, Int32, Int32}
    i = 0
    while i < data.size
      dl = data[i]; ds = data[i + 1]; length = data[i + 2]; kind = data[i + 3]; _mods = data[i + 4]
      line += dl
      start = (dl == 0) ? start + ds : ds
      out << {line, start, length, kind}
      i += 5
    end
    out
  end
end

describe "LSP semantic tokens integration" do
  it "emits a single full-span token for symbol literals" do
    source = "options[:accel_healthcheck] = true\n"
    tokens = SemanticTokensIntegrationHelper.collect_tokens(source)
    decoded = SemanticTokensIntegrationHelper.decode(tokens)

    enum_kind = 10 # enumMember per legend
    symbol_text = ":accel_healthcheck"
    expected_len = symbol_text.bytesize

    matches = decoded.select { |(line, col, len, kind)| line == 0 && kind == enum_kind && col == "options[".bytesize }
    matches.size.should eq(1)
    _line, col, len, _kind = matches.first
    len.should be >= expected_len
    source.byte_slice(col, len).starts_with?(symbol_text).should be_true
  end

  it "does not classify require strings as enum members" do
    source = "require \"./poisson/event_sweep\"\n"
    tokens = SemanticTokensIntegrationHelper.collect_tokens(source)
    decoded = SemanticTokensIntegrationHelper.decode(tokens)

    enum_kind = 10 # enumMember per legend
    decoded.any? { |(_, _, _, kind)| kind == enum_kind }.should be_false
  end
end
