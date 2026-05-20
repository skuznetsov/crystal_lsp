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

  it "returns hover then definition for a path in the same file" do
    dir = File.join(Dir.tempdir, "lsp_hover_def_#{Random::Secure.hex(6)}")
    FileUtils.mkdir_p(dir)
    path = File.join(dir, "main.cr")
    source = <<-CR
    module Foo
      class Bar
        def run
          1
        end
      end
    end

    result = Foo::Bar.new.run
    CR
    File.write(path, source)

    server = CrystalV2::Compiler::LSP::Server.new(IO::Memory.new, IO::Memory.new, CrystalV2::Compiler::LSP::ServerConfig.new(background_indexing: false, project_cache: false))
    uri = server.spec_store_document(source, dir, path)

    bar_offset = source.index("Bar").not_nil!
    line_idx = source[0, bar_offset].count('\n')
    last_nl = source.rindex('\n', bar_offset) || -1
    char_idx = bar_offset - last_nl - 1

    hover = server.spec_hover(uri, line_idx, char_idx) # position on Foo::Bar
    hover["result"].should_not be_nil

    definition = server.spec_definition(uri, line_idx, char_idx)
    definition["result"].should_not be_nil
    locations = definition["result"].as_a
    locations.size.should eq(1)
    loc = locations.first.as_h
    loc["uri"].as_s.should eq(uri)
  ensure
    FileUtils.rm_rf(dir) if dir
  end

  it "keeps hover reference counting off the foreground path by default" do
    dir = File.join(Dir.tempdir, "lsp_hover_refs_#{Random::Secure.hex(6)}")
    FileUtils.mkdir_p(dir)
    path = File.join(dir, "main.cr")
    source = <<-CR
    value = 1
    value
    CR
    File.write(path, source)

    default_server = CrystalV2::Compiler::LSP::Server.new(
      IO::Memory.new,
      IO::Memory.new,
      CrystalV2::Compiler::LSP::ServerConfig.new(background_indexing: false, project_cache: false)
    )
    default_uri = default_server.spec_store_document(source, dir, path)

    value_offset = source.rindex("value").not_nil!
    line_idx = source[0, value_offset].count('\n')
    last_nl = source.rindex('\n', value_offset) || -1
    char_idx = value_offset - last_nl - 1

    hover = default_server.spec_hover(default_uri, line_idx, char_idx)
    contents = hover["result"]["contents"]["value"].as_s
    contents.includes?("reference").should be_false

    opt_in_server = CrystalV2::Compiler::LSP::Server.new(
      IO::Memory.new,
      IO::Memory.new,
      CrystalV2::Compiler::LSP::ServerConfig.new(
        background_indexing: false,
        project_cache: false,
        hover_reference_count: true
      )
    )
    opt_in_uri = opt_in_server.spec_store_document(source, dir, path)
    opt_in_hover = opt_in_server.spec_hover(opt_in_uri, line_idx, char_idx)
    opt_in_hover["result"]["contents"]["value"].as_s.includes?("reference").should be_true
  ensure
    FileUtils.rm_rf(dir) if dir
  end

  it "returns AST document symbols without depending on semantic symbol tables" do
    dir = File.join(Dir.tempdir, "lsp_doc_symbols_#{Random::Secure.hex(6)}")
    FileUtils.mkdir_p(dir)
    path = File.join(dir, "main.cr")
    source = <<-CR
    module Outer
      COUNT = 1

      class Thing
        property value : Int32

        def run(value : Int32) : Int32
          value
        end
      end
    end
    CR
    File.write(path, source)

    server = CrystalV2::Compiler::LSP::Server.new(
      IO::Memory.new,
      IO::Memory.new,
      CrystalV2::Compiler::LSP::ServerConfig.new(background_indexing: false, project_cache: false)
    )
    uri = server.spec_store_document(source, dir, path)
    server.spec_document_symbol_cache_size(uri).should eq(5)

    response = server.spec_document_symbols(uri)
    symbols = response["result"].as_a
    symbols.size.should eq(1)
    outer = symbols.first
    outer["name"].as_s.should eq("Outer")
    outer_children = outer["children"].as_a
    outer_children.map { |child| child["name"].as_s }.should eq(["COUNT", "Thing"])
    thing_children = outer_children[1]["children"].as_a
    thing_children.map { |child| child["name"].as_s }.should eq(["value", "run"])
  ensure
    FileUtils.rm_rf(dir) if dir
  end
end
