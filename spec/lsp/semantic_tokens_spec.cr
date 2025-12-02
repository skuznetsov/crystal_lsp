require "spec"

require "../../src/compiler/frontend/parser"
require "../../src/compiler/lsp/server"

module SemanticTokensSpecHelper
  # Legend as defined in LSP::Server::SemanticTokenType
  def self.legend_index(name : String)
    case name
    when "namespace" then 0
    when "type" then 1
    when "class" then 2
    when "enum" then 3
    when "interface" then 4
    when "struct" then 5
    when "typeParameter" then 6
    when "parameter" then 7
    when "variable" then 8
    when "property" then 9
    when "enumMember" then 10
    when "event" then 11
    when "function" then 12
    when "method" then 13
    when "macro" then 14
    when "keyword" then 15
    when "modifier" then 16
    when "comment" then 17
    when "string" then 18
    when "number" then 19
    when "regexp" then 20
    when "operator" then 21
    else -1
    end
  end
  

  def self.collect(program, source)
    server = CrystalV2::Compiler::LSP::Server.new
    server.collect_semantic_tokens(program, source)
  end

  def self.decode(tokens : CrystalV2::Compiler::LSP::SemanticTokens, source : String)
    data = tokens.data
    line = 0
    start = 0
    out = [] of Tuple(Int32, Int32, Int32, Int32, String)
    i = 0
    lines = source.lines
    while i < data.size
      dl = data[i]; ds = data[i + 1]; length = data[i + 2]; kind = data[i + 3]; mods = data[i + 4]
      line += dl
      start = (dl == 0) ? start + ds : ds
      text = lines[line]? || ""
      snippet = length > 0 ? text.byte_slice(start, length) : ""
      out << {line + 1, start + 1, length, kind, snippet}
      i += 5
    end
    out
  end

  it "highlights members and nested identifiers" do
    source = "a = foo.bar(b[0]) { x }\n"
    parser = CrystalV2::Compiler::Frontend::Parser.new(
      CrystalV2::Compiler::Frontend::Lexer.new(source)
    )
    program = parser.parse_program
    tokens = SemanticTokensSpecHelper.collect(program, source)
    decoded = SemanticTokensSpecHelper.decode(tokens, source)

    # Expect variable 'a'
    decoded.any? { |(_, _, _, kind, text)| kind == SemanticTokensSpecHelper.legend_index("variable") && text == "a" }.should be_true
    # Expect receiver 'foo'
    decoded.any? { |(_, _, _, kind, text)| kind == SemanticTokensSpecHelper.legend_index("variable") && text == "foo" }.should be_true
    # Expect index target 'b' and number '0'
    decoded.any? { |(_, _, _, kind, text)| kind == SemanticTokensSpecHelper.legend_index("variable") && text == "b" }.should be_true
    decoded.any? { |(_, _, _, kind, text)| kind == SemanticTokensSpecHelper.legend_index("number") && text == "0" }.should be_true
    # Expect method member 'bar'
    decoded.any? { |(_, _, _, kind, text)| kind == SemanticTokensSpecHelper.legend_index("method") && text == "bar" }.should be_true
    # Expect block var 'x'
    decoded.any? { |(_, _, _, kind, text)| kind == SemanticTokensSpecHelper.legend_index("variable") && text == "x" }.should be_true
  end

  it "highlights control flow keywords" do
    source = "if cond\n  begin\n    do_something\n  end\nend\n"
    parser = CrystalV2::Compiler::Frontend::Parser.new(
      CrystalV2::Compiler::Frontend::Lexer.new(source)
    )
    program = parser.parse_program
    tokens = SemanticTokensSpecHelper.collect(program, source)
    decoded = SemanticTokensSpecHelper.decode(tokens, source)

    decoded.any? { |(_, _, _, kind, text)| kind == SemanticTokensSpecHelper.legend_index("keyword") && text == "if" }.should be_true
    decoded.any? { |(_, _, _, kind, text)| kind == SemanticTokensSpecHelper.legend_index("keyword") && text == "begin" }.should be_true
    decoded.count { |(_, _, _, kind, text)| kind == SemanticTokensSpecHelper.legend_index("keyword") && text == "end" }.should be >= 2
  end

  it "highlights symbol literals in hash access" do
    source = "options[:accel_usage_log] = true\n"
    parser = CrystalV2::Compiler::Frontend::Parser.new(
      CrystalV2::Compiler::Frontend::Lexer.new(source)
    )
    program = parser.parse_program
    tokens = SemanticTokensSpecHelper.collect(program, source)
    decoded = SemanticTokensSpecHelper.decode(tokens, source)

    property_kind = SemanticTokensSpecHelper.legend_index("property")
    decoded.any? { |(_, _, _, kind, text)| kind == property_kind && text == ":accel_usage_log" }.should be_true
  end

  it "lexically marks symbol literals inside string interpolation" do
    source = %("#{ :foo }")
    lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
    parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
    program = parser.parse_program
    tokens = SemanticTokensSpecHelper.collect(program, source)
    decoded = SemanticTokensSpecHelper.decode(tokens, source)

    # Interpolation path currently classifies symbol text as string content (lexical pass)
    string_kind = SemanticTokensSpecHelper.legend_index("string")
    decoded.any? { |(_, _, _, kind, text)| kind == string_kind && text.includes?("foo") }.should be_true
  end
end
