require "spec"
require "file_utils"
require "random/secure"

require "./support/server_helper"

private def line_and_character_for_stripped_line(
  source : String,
  stripped_line : String,
  *,
  last : Bool = false,
  token : String? = nil
) : {Int32, Int32}
  lines = source.lines
  matches = [] of Int32
  target = stripped_line.strip

  lines.each_with_index do |line, index|
    matches << index if line.strip == target
  end

  line_index = last ? matches.last? : matches.first?
  raise "Missing stripped line: #{stripped_line}" unless line_index

  line_text = lines[line_index].rstrip
  search_token = token || target
  column = line_text.index(search_token)
  raise "Missing token #{search_token} on line #{stripped_line}" unless column

  {line_index.to_i, column.to_i}
end

describe "LSP local inference and navigation" do
  it "shows local receiver and call signature in the same file" do
    dir = File.join(Dir.tempdir, "lsp_local_nav_#{Random::Secure.hex(6)}")
    Dir.mkdir(dir)
    begin
      path = File.join(dir, "main.cr")
      source = <<-CR
      class Program
      end

      class ParserLike
        def parse_program : Program
          Program.new
        end
      end

      def run
        parser = ParserLike.new
        program = parser.parse_program
        program
      end
      CR
      File.write(path, source)

      server = CrystalV2::Compiler::LSP::Server.new(
        IO::Memory.new,
        IO::Memory.new,
        CrystalV2::Compiler::LSP::ServerConfig.new(background_indexing: false, project_cache: false)
      )
      uri = server.spec_store_document(source, dir, path)

      parser_offset = source.index("parser.parse_program").not_nil!
      parser_line = source[0, parser_offset].count('\n')
      parser_char = parser_offset - (source.rindex('\n', parser_offset) || -1) - 1
      parser_hover = server.spec_hover(uri, parser_line, parser_char)
      parser_hover["result"].should_not be_nil
      parser_hover["result"]["contents"]["value"].as_s.should contain("ParserLike")

      parse_offset = source.index("parse_program").not_nil!
      parse_line = source[0, parse_offset].count('\n')
      parse_char = parse_offset - (source.rindex('\n', parse_offset) || -1) - 1
      parse_hover = server.spec_hover(uri, parse_line, parse_char)
      parse_hover["result"].should_not be_nil
      parse_hover["result"]["contents"]["value"].as_s.should contain("def parse_program() : Program")

      program_line, program_char = line_and_character_for_stripped_line(source, "        program", last: true)
      program_hover = server.spec_hover(uri, program_line, program_char)
      program_hover["result"].should_not be_nil
      program_hover["result"]["contents"]["value"].as_s.should contain("Program")

      parser_definition = server.spec_definition(uri, parser_line, parser_char)
      parser_definition["result"].should_not be_nil
      parser_definition["result"].as_a.first["uri"].as_s.should contain("main.cr")
    ensure
      FileUtils.rm_rf(dir)
    end
  end

  it "narrows a local variable after as? assignment and nil guard" do
    dir = File.join(Dir.tempdir, "lsp_local_cast_#{Random::Secure.hex(6)}")
    Dir.mkdir(dir)
    begin
      path = File.join(dir, "main.cr")
      source = <<-CR
      module Frontend
        class ArenaLike
        end

        class AstArena < ArenaLike
        end
      end

      def persist(arena_like : Frontend::ArenaLike)
        arena = arena_like.as?(Frontend::AstArena)
        return unless arena
        arena
      end
      CR
      File.write(path, source)

      server = CrystalV2::Compiler::LSP::Server.new(
        IO::Memory.new,
        IO::Memory.new,
        CrystalV2::Compiler::LSP::ServerConfig.new(background_indexing: false, project_cache: false)
      )
      uri = server.spec_store_document(source, dir, path)

      arena_line, arena_char = line_and_character_for_stripped_line(source, "        arena", last: true)
      hover = server.spec_hover(uri, arena_line, arena_char)
      hover["result"].should_not be_nil
      hover["result"]["contents"]["value"].as_s.should contain("Frontend::AstArena")
    ensure
      FileUtils.rm_rf(dir)
    end
  end

  it "navigates class receiver and class method to a required file" do
    dir = File.join(Dir.tempdir, "lsp_class_method_nav_#{Random::Secure.hex(6)}")
    Dir.mkdir(dir)
    begin
      helper_path = File.join(dir, "helper.cr")
      File.write(helper_path, <<-CR)
      class AstCache
        def self.load(path : String) : AstCache?
          nil
        end
      end
      CR

      main_path = File.join(dir, "main.cr")
      main_source = <<-CR
      require "./helper"

      if cached = AstCache.load("cache.bin")
        cached
      end
      CR
      File.write(main_path, main_source)

      server = CrystalV2::Compiler::LSP::Server.new(
        IO::Memory.new,
        IO::Memory.new,
        CrystalV2::Compiler::LSP::ServerConfig.new(background_indexing: false, project_cache: false)
      )
      uri = server.spec_store_document(main_source, dir, main_path)

      ast_cache_offset = main_source.index("AstCache").not_nil!
      ast_cache_line = main_source[0, ast_cache_offset].count('\n')
      ast_cache_char = ast_cache_offset - (main_source.rindex('\n', ast_cache_offset) || -1) - 1
      ast_cache_definition = server.spec_definition(uri, ast_cache_line, ast_cache_char)
      ast_cache_definition["result"].should_not be_nil
      ast_cache_definition["result"].as_a.first["uri"].as_s.should contain("helper.cr")

      load_offset = main_source.index("load").not_nil!
      load_line = main_source[0, load_offset].count('\n')
      load_char = load_offset - (main_source.rindex('\n', load_offset) || -1) - 1
      load_hover = server.spec_hover(uri, load_line, load_char)
      load_hover["result"].should_not be_nil
      load_hover["result"]["contents"]["value"].as_s.should contain("def self.load(path : String) : AstCache?")

      load_definition = server.spec_definition(uri, load_line, load_char)
      load_definition["result"].should_not be_nil
      load_definition["result"].as_a.first["uri"].as_s.should contain("helper.cr")
    ensure
      FileUtils.rm_rf(dir)
    end
  end
end
