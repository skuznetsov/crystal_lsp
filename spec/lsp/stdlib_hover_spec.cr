require "spec"
require "file_utils"
require "random/secure"

require "./support/server_helper"

describe "Stdlib hover and definition" do
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

  it "provides hover for stdlib types inferred from literals" do
    source = <<-CR
    arr = [1, 2, 3]
    str = "hello"
    hash = {"key" => "value"}
    int = 42
    float = 3.14
    CR

    server = CrystalV2::Compiler::LSP::Server.new(
      IO::Memory.new, IO::Memory.new,
      CrystalV2::Compiler::LSP::ServerConfig.new(background_indexing: false, project_cache: false)
    )
    uri = server.spec_store_document(source, nil, "/tmp/stdlib_types_test.cr")

    # Test arr hover - should be Array(Int32)
    hover = server.spec_hover(uri, 0, 0)
    hover["result"].should_not be_nil
    contents = hover["result"]["contents"]["value"].as_s
    contents.should contain("Array")

    # Test str hover - should be String
    hover = server.spec_hover(uri, 1, 0)
    hover["result"].should_not be_nil
    contents = hover["result"]["contents"]["value"].as_s
    contents.should contain("String")

    # Test hash hover - should be Hash
    hover = server.spec_hover(uri, 2, 0)
    hover["result"].should_not be_nil
    contents = hover["result"]["contents"]["value"].as_s
    contents.should contain("Hash")

    # Test int hover - should be Int32
    hover = server.spec_hover(uri, 3, 0)
    hover["result"].should_not be_nil
    contents = hover["result"]["contents"]["value"].as_s
    contents.should contain("Int32")

    # Test float hover - should be Float64
    hover = server.spec_hover(uri, 4, 0)
    hover["result"].should_not be_nil
    contents = hover["result"]["contents"]["value"].as_s
    contents.should contain("Float64")
  end

  it "provides hover and definition for array element access" do
    source = <<-CR
    class Atom
      property sigma : Float64 = 1.0
    end

    arr = [Atom.new, Atom.new]
    first = arr[0]
    val = first.sigma
    CR

    server = CrystalV2::Compiler::LSP::Server.new(
      IO::Memory.new, IO::Memory.new,
      CrystalV2::Compiler::LSP::ServerConfig.new(background_indexing: false, project_cache: false)
    )
    uri = server.spec_store_document(source, nil, "/tmp/array_element_test.cr")

    # Test arr hover - should be Array(Atom)
    hover = server.spec_hover(uri, 4, 0)
    hover["result"].should_not be_nil
    contents = hover["result"]["contents"]["value"].as_s
    contents.should contain("Array")

    # Test first hover - should be Atom
    hover = server.spec_hover(uri, 5, 0)
    hover["result"].should_not be_nil
    contents = hover["result"]["contents"]["value"].as_s
    contents.should contain("Atom")
  end
end
