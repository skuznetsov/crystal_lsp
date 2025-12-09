require "spec"
require "file_utils"
require "random/secure"

require "./support/server_helper"

# Tests that hover/definition work for types from required files
# when those files are cached (not re-parsed).
describe "Cached types across required files" do
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

  it "hover shows type from required file" do
    base_dir = File.join(Dir.tempdir, "lsp_cached_types_#{Random::Secure.hex(6)}")
    Dir.mkdir(base_dir)
    begin
      # Create a helper file with a class that returns a typed value
      helper_path = File.join(base_dir, "helper.cr")
      File.write(helper_path, <<-CR)
      class Helper
        def value : Int32
          42
        end

        def self.create : Helper
          new
        end
      end
      CR

      # Create main file that uses the helper
      main_path = File.join(base_dir, "main.cr")
      main_source = <<-CR
      require "./helper"

      h = Helper.create
      x = h.value
      CR
      File.write(main_path, main_source)

      server = CrystalV2::Compiler::LSP::Server.new(
        IO::Memory.new, IO::Memory.new,
        CrystalV2::Compiler::LSP::ServerConfig.new(background_indexing: false, project_cache: false)
      )
      uri = server.spec_store_document(main_source, base_dir, main_path)

      # Hover on 'h' should show Helper type
      h_offset = main_source.index("h = Helper").not_nil!
      h_line = main_source[0, h_offset].count('\n')
      h_char = h_offset - (main_source.rindex('\n', h_offset) || -1) - 1

      hover_result = server.spec_hover(uri, h_line, h_char)
      hover_result["result"].should_not be_nil
      contents = hover_result["result"]["contents"]?
      contents.should_not be_nil
      # Should mention Helper type
      value = contents.not_nil!["value"]?.try(&.as_s) || contents.to_s
      value.should contain("Helper")

      # Hover on 'x' should show Int32
      x_offset = main_source.index("x = h.value").not_nil!
      x_line = main_source[0, x_offset].count('\n')
      x_char = x_offset - (main_source.rindex('\n', x_offset) || -1) - 1

      hover_x = server.spec_hover(uri, x_line, x_char)
      hover_x["result"].should_not be_nil
      contents_x = hover_x["result"]["contents"]?
      contents_x.should_not be_nil
      value_x = contents_x.not_nil!["value"]?.try(&.as_s) || contents_x.to_s
      value_x.should contain("Int32")
    ensure
      FileUtils.rm_rf(base_dir)
    end
  end

  it "definition navigates to class in required file" do
    base_dir = File.join(Dir.tempdir, "lsp_cached_def_#{Random::Secure.hex(6)}")
    Dir.mkdir(base_dir)
    begin
      helper_path = File.join(base_dir, "helper.cr")
      File.write(helper_path, <<-CR)
      class Helper
        def run
          :ok
        end
      end
      CR

      main_path = File.join(base_dir, "main.cr")
      main_source = <<-CR
      require "./helper"

      obj = Helper.new
      obj.run
      CR
      File.write(main_path, main_source)

      server = CrystalV2::Compiler::LSP::Server.new(
        IO::Memory.new, IO::Memory.new,
        CrystalV2::Compiler::LSP::ServerConfig.new(background_indexing: false, project_cache: false)
      )
      uri = server.spec_store_document(main_source, base_dir, main_path)

      # Find Helper in "Helper.new"
      helper_offset = main_source.index("Helper.new").not_nil!
      helper_line = main_source[0, helper_offset].count('\n')
      helper_char = helper_offset - (main_source.rindex('\n', helper_offset) || -1) - 1

      definition = server.spec_definition(uri, helper_line, helper_char)
      definition["result"].should_not be_nil
      locations = definition["result"].as_a
      locations.size.should be >= 1

      # Should point to helper.cr
      loc = locations.first.as_h
      loc["uri"].as_s.should contain("helper.cr")
      # Line 0 is where "class Helper" is defined
      loc["range"]["start"]["line"].as_i.should eq(0)
    ensure
      FileUtils.rm_rf(base_dir)
    end
  end

  it "definition navigates to method in required file" do
    base_dir = File.join(Dir.tempdir, "lsp_cached_method_#{Random::Secure.hex(6)}")
    Dir.mkdir(base_dir)
    begin
      helper_path = File.join(base_dir, "helper.cr")
      File.write(helper_path, <<-CR)
      class Helper
        def compute(x : Int32) : Int32
          x * 2
        end
      end
      CR

      main_path = File.join(base_dir, "main.cr")
      main_source = <<-CR
      require "./helper"

      h = Helper.new
      result = h.compute(10)
      CR
      File.write(main_path, main_source)

      server = CrystalV2::Compiler::LSP::Server.new(
        IO::Memory.new, IO::Memory.new,
        CrystalV2::Compiler::LSP::ServerConfig.new(background_indexing: false, project_cache: false)
      )
      uri = server.spec_store_document(main_source, base_dir, main_path)

      # Find 'compute' in "h.compute(10)"
      compute_offset = main_source.index("compute").not_nil!
      compute_line = main_source[0, compute_offset].count('\n')
      compute_char = compute_offset - (main_source.rindex('\n', compute_offset) || -1) - 1

      definition = server.spec_definition(uri, compute_line, compute_char)
      definition["result"].should_not be_nil
      locations = definition["result"].as_a
      locations.size.should be >= 1

      loc = locations.first.as_h
      loc["uri"].as_s.should contain("helper.cr")
      # Line 1 is where "def compute" is (0-indexed)
      loc["range"]["start"]["line"].as_i.should eq(1)
    ensure
      FileUtils.rm_rf(base_dir)
    end
  end

  it "hover works for nested module from required file" do
    base_dir = File.join(Dir.tempdir, "lsp_cached_nested_#{Random::Secure.hex(6)}")
    Dir.mkdir(base_dir)
    begin
      helper_path = File.join(base_dir, "helper.cr")
      File.write(helper_path, <<-CR)
      module Outer
        module Inner
          class Widget
            def id : String
              "widget-1"
            end
          end
        end
      end
      CR

      main_path = File.join(base_dir, "main.cr")
      main_source = <<-CR
      require "./helper"

      w = Outer::Inner::Widget.new
      name = w.id
      CR
      File.write(main_path, main_source)

      server = CrystalV2::Compiler::LSP::Server.new(
        IO::Memory.new, IO::Memory.new,
        CrystalV2::Compiler::LSP::ServerConfig.new(background_indexing: false, project_cache: false)
      )
      uri = server.spec_store_document(main_source, base_dir, main_path)

      # Hover on 'w' should show Outer::Inner::Widget
      w_offset = main_source.index("w = Outer").not_nil!
      w_line = main_source[0, w_offset].count('\n')
      w_char = w_offset - (main_source.rindex('\n', w_offset) || -1) - 1

      hover = server.spec_hover(uri, w_line, w_char)
      hover["result"].should_not be_nil
      contents = hover["result"]["contents"]?
      contents.should_not be_nil
      value = contents.not_nil!["value"]?.try(&.as_s) || contents.to_s
      value.should contain("Widget")

      # Hover on 'name' should show String
      name_offset = main_source.index("name = w").not_nil!
      name_line = main_source[0, name_offset].count('\n')
      name_char = name_offset - (main_source.rindex('\n', name_offset) || -1) - 1

      hover_name = server.spec_hover(uri, name_line, name_char)
      hover_name["result"].should_not be_nil
      contents_name = hover_name["result"]["contents"]?
      contents_name.should_not be_nil
      value_name = contents_name.not_nil!["value"]?.try(&.as_s) || contents_name.to_s
      value_name.should contain("String")
    ensure
      FileUtils.rm_rf(base_dir)
    end
  end
end
