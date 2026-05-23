require "spec"
require "file_utils"
require "random/secure"

require "./support/server_helper"

private def lsp_line_char(source : String, needle : String, occurrence : Int32 = 0, delta : Int32 = 0) : {Int32, Int32}
  offset = nil
  search_from = 0
  (occurrence + 1).times do
    found = source.index(needle, search_from)
    raise "Missing needle #{needle}" unless found
    offset = found
    search_from = found + needle.bytesize
  end

  target = offset.not_nil! + delta
  line = source[0, target].count('\n')
  line_start = source.rindex('\n', target) || -1
  {line, target - line_start - 1}
end

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

  it "keeps qualified path hover off the dependency-loading path" do
    dir = File.join(Dir.tempdir, "lsp_hover_no_dep_load_#{Random::Secure.hex(6)}")
    FileUtils.mkdir_p(dir)
    path = File.join(dir, "main.cr")
    helper_path = File.join(dir, "helper.cr")
    source = <<-CR
    require "./helper"

    value = Missing::Thing.new
    CR
    File.write(path, source)
    File.write(helper_path, "class Helper; end\n")

    server = CrystalV2::Compiler::LSP::Server.new(
      IO::Memory.new,
      IO::Memory.new,
      CrystalV2::Compiler::LSP::ServerConfig.new(background_indexing: false, project_cache: false)
    )
    uri = server.spec_store_document(source, dir, path)
    server.spec_clear_dependency_documents

    missing_offset = source.index("Missing").not_nil!
    line_idx = source[0, missing_offset].count('\n')
    last_nl = source.rindex('\n', missing_offset) || -1
    char_idx = missing_offset - last_nl - 1

    before_count = server.spec_dependency_document_count
    hover = server.spec_hover(uri, line_idx, char_idx)
    hover["error"]?.should be_nil
    server.spec_dependency_document_count.should eq(before_count)
  ensure
    FileUtils.rm_rf(dir) if dir
  end

  it "selects hover method signature by call arity and keeps default parameters" do
    dir = File.join(Dir.tempdir, "lsp_hover_overload_arity_#{Random::Secure.hex(6)}")
    FileUtils.mkdir_p(dir)
    path = File.join(dir, "main.cr")
    source = <<-CR
    class Generator
      def new_seed : UInt32
        new_seed(1_u64, 2_u64)
      end

      def new_seed(initstate : UInt64, initseq = 0_u64) : UInt32
        1_u32
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

    call_line, call_char = lsp_line_char(source, "new_seed(1_u64", delta: 5)
    call_hover = server.spec_hover(uri, call_line, call_char)
    call_hover["result"]["contents"]["value"].as_s.should contain("def new_seed(initstate : UInt64, initseq = 0_u64) : UInt32")

    def_line, def_char = lsp_line_char(source, "def new_seed(initstate", delta: 5)
    call_definition = server.spec_definition(uri, call_line, call_char)
    call_definition["result"].as_a.first["range"]["start"]["line"].as_i.should eq(def_line)

    def_hover = server.spec_hover(uri, def_line, def_char)
    def_hover["result"]["contents"]["value"].as_s.should contain("def new_seed(initstate : UInt64, initseq = 0_u64) : UInt32")
  ensure
    FileUtils.rm_rf(dir) if dir
  end

  it "hovers bang member calls without parentheses" do
    dir = File.join(Dir.tempdir, "lsp_hover_bang_member_#{Random::Secure.hex(6)}")
    FileUtils.mkdir_p(dir)
    path = File.join(dir, "main.cr")
    source = <<-CR
    class Converter
      def self.convert(value)
        value.to_i8!
      end

      def to_i8 : Int8
        0_i8
      end

      def to_i8! : Int8
        1_i8
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

    call_line, call_char = lsp_line_char(source, "to_i8!", delta: 3)
    call_hover = server.spec_hover(uri, call_line, call_char)
    call_hover["result"]["contents"]["value"].as_s.should contain("def to_i8! : Int8")

    def_line, _def_char = lsp_line_char(source, "def to_i8! : Int8", delta: 5)
    call_definition = server.spec_definition(uri, call_line, call_char)
    call_definition["result"].as_a.first["range"]["start"]["line"].as_i.should eq(def_line)
  ensure
    FileUtils.rm_rf(dir) if dir
  end

  it "synthesizes hover for generated numeric bang conversions" do
    dir = File.join(Dir.tempdir, "lsp_hover_generated_numeric_#{Random::Secure.hex(6)}")
    FileUtils.mkdir_p(dir)
    path = File.join(dir, "main.cr")
    source = <<-CR
    class Converter
      def self.convert(value)
        value.to_i8!
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

    call_line, call_char = lsp_line_char(source, "to_i8!", delta: 3)
    call_hover = server.spec_hover(uri, call_line, call_char)
    call_hover["result"]["contents"]["value"].as_s.should contain("def to_i8! : Int8")

    call_definition = server.spec_definition(uri, call_line, call_char)
    location = call_definition["result"].as_a.first
    location["uri"].as_s.should end_with("/primitives.cr")
  ensure
    FileUtils.rm_rf(dir) if dir
  end

  it "hovers and defines stdlib type constants in macro argument lists" do
    dir = File.join(Dir.tempdir, "lsp_hover_macro_constant_#{Random::Secure.hex(6)}")
    FileUtils.mkdir_p(dir)
    path = File.join(dir, "main.cr")
    source = <<-CR
    struct Number
      Number.expand_div [Float64], Float64
    end
    CR
    File.write(path, source)

    server = CrystalV2::Compiler::LSP::Server.new(
      IO::Memory.new,
      IO::Memory.new,
      CrystalV2::Compiler::LSP::ServerConfig.new(background_indexing: false, project_cache: false)
    )
    uri = server.spec_store_document(source, dir, path)

    const_line, const_char = lsp_line_char(source, "], Float64", delta: 4)
    hover = server.spec_hover(uri, const_line, const_char)
    hover["result"]["contents"]["value"].as_s.should contain("struct Float64")

    definition = server.spec_definition(uri, const_line, const_char)
    location = definition["result"].as_a.first
    location["uri"].as_s.should end_with("/float.cr")
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

  it "collects AST document symbols lazily after didOpen" do
    dir = File.join(Dir.tempdir, "lsp_doc_symbols_lazy_#{Random::Secure.hex(6)}")
    FileUtils.mkdir_p(dir)
    path = File.join(dir, "main.cr")
    source = <<-CR
    module Outer
      class Thing
        def run : Int32
          1
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
    uri = server.spec_did_open_document(source, path)
    server.spec_document_symbol_cache_size(uri).should eq(0)

    response = server.spec_document_symbols(uri)
    symbols = response["result"].as_a
    symbols.size.should eq(1)
    symbols.first["name"].as_s.should eq("Outer")
    server.spec_document_symbol_cache_size(uri).should eq(3)
  ensure
    FileUtils.rm_rf(dir) if dir
  end

  it "keeps foreground expression indexes lazy while preserving navigation and tokens" do
    dir = File.join(Dir.tempdir, "lsp_lazy_expr_index_#{Random::Secure.hex(6)}")
    FileUtils.mkdir_p(dir)
    path = File.join(dir, "main.cr")
    source = <<-CR
    module Outer
      VALUE = 1
      FAKE_DEF = "not a def run(value) header"

      class Thing
        def run(value : Int32) : Int32
          value + VALUE
        end
      end
    end

    result = Outer::Thing.new.run(41)
    CR
    File.write(path, source)

    server = CrystalV2::Compiler::LSP::Server.new(
      IO::Memory.new,
      IO::Memory.new,
      CrystalV2::Compiler::LSP::ServerConfig.new(background_indexing: false, project_cache: false)
    )
    uri = server.spec_did_open_document(source, path)
    server.spec_document_expr_index_built?(uri).should be_false

    method_offset = source.index("run(value : Int32)").not_nil!
    method_line = source[0, method_offset].count('\n')
    method_char = method_offset - (source.rindex('\n', method_offset) || -1) - 1

    method_hover = server.spec_hover(uri, method_line, method_char)
    method_hover["result"].should_not be_nil
    method_hover["result"]["contents"]["value"].as_s.should contain("def run(value : Int32) : Int32")
    server.spec_document_expr_index_built?(uri).should be_false

    def_offset = source.index("def run(value : Int32)").not_nil! + 1
    def_line = source[0, def_offset].count('\n')
    def_char = def_offset - (source.rindex('\n', def_offset) || -1) - 1

    def_hover = server.spec_hover(uri, def_line, def_char)
    def_hover["result"].should_not be_nil
    def_hover["result"]["contents"]["value"].as_s.should contain("def run(value : Int32) : Int32")
    server.spec_document_expr_index_built?(uri).should be_false

    fake_offset = source.index("def run(value) header").not_nil!
    fake_line = source[0, fake_offset].count('\n')
    fake_char = fake_offset - (source.rindex('\n', fake_offset) || -1) - 1

    fake_hover = server.spec_hover(uri, fake_line, fake_char)
    unless fake_hover["result"].raw.nil?
      fake_hover["result"]["contents"]["value"].as_s.should_not contain("def run(value : Int32) : Int32")
    end
    server.spec_document_expr_index_built?(uri).should be_false

    value_offset = source.index("value + VALUE").not_nil!
    value_line = source[0, value_offset].count('\n')
    value_char = value_offset - (source.rindex('\n', value_offset) || -1) - 1

    hover = server.spec_hover(uri, value_line, value_char)
    hover["result"].should_not be_nil

    definition = server.spec_definition(uri, value_line, value_char)
    locations = definition["result"].as_a
    locations.size.should eq(1)
    locations.first["uri"].as_s.should eq(uri)

    tokens_before = server.spec_semantic_tokens(uri)["result"]["data"].as_a
    tokens_after = server.spec_semantic_tokens(uri)["result"]["data"].as_a
    tokens_before.should_not be_empty
    tokens_after.should eq(tokens_before)
    server.spec_document_expr_index_built?(uri).should be_false

    symbols = server.spec_document_symbols(uri)["result"].as_a
    symbols.size.should eq(1)
    symbols.first["name"].as_s.should eq("Outer")
  ensure
    FileUtils.rm_rf(dir) if dir
  end
end
