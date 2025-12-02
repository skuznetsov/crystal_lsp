require "spec"

require "./support/server_helper"

describe CrystalV2::Compiler::LSP::Server do
  it "navigates from macro expansion placeholder to macro definition" do
    source = <<-CR
    macro hello(name)
      def hello_{{name.id}}
        42
      end
    end

    hello world
    CR

    server = CrystalV2::Compiler::LSP::Server.new(IO::Memory.new, IO::Memory.new, CrystalV2::Compiler::LSP::ServerConfig.new(background_indexing: false, project_cache: false))
    uri = server.spec_store_document(source, nil, "/tmp/macro_nav.cr")

    line_idx = source.lines.index { |l| l.includes?("hello world") }.not_nil!
    char_idx = source.lines[line_idx].index("hello").not_nil!

    hover = server.spec_hover(uri, line_idx, char_idx)
    hover["result"].should_not be_nil

    definition = server.spec_definition(uri, line_idx, char_idx)
    definition["result"].should_not be_nil
    definition["result"].as_a.size.should be >= 1
  end
end
