require "spec"

require "../src/compiler/bootstrap_shims"
require "../src/compiler/frontend/parser/diagnostic"
require "../src/compiler/formatter"

alias Formatter = CrystalV2::Compiler::Formatter

describe Formatter do
  it "formats compact assignments without rewriting already spaced code" do
    Formatter.format("x=1\n").should eq("x = 1\n")
    Formatter.format("x = 1\n").should eq("x = 1\n")
  end

  it "preserves namespace paths without inserting spaces around ::" do
    source = "alias Frontend = CrystalV2::Compiler::Frontend\ngetter span : Frontend::Span\n"
    Formatter.format(source).should eq(source)
  end

  it "does not drift indentation after modifier conditionals" do
    source = "def value\n  return nil if missing?\n  value = 1\nend\n"
    Formatter.format(source).should eq(source)
  end

  it "preserves named arguments, bare splats, indexers, and block pipes" do
    source = <<-CR
def call(
  *,
  path : String,
)
  build(path: path)
  values = Hash(String, Array(Int32)).new { |h, k| h[k] = [] of Int32 }
end
CR

    Formatter.format(source).should eq(source)
  end

  it "preserves aligned comments and line-continuation spacing" do
    source = <<-CR
CONFIG_A = 1,                      # aligned
CONFIG_LONG_NAME = 2,              # aligned

STDERR.puts "a " \\
            "b"
CR

    Formatter.format(source).should eq(source)
  end

  it "does not emit destructive edits for the LSP server source" do
    source = File.read("src/compiler/lsp/server.cr")
    Formatter.format(source).should eq(source)
  end
end
