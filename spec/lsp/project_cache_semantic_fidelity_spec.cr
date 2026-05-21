require "spec"
require "file_utils"
require "random/secure"

require "./support/server_helper"
require "../../src/compiler/lsp/project_cache"

private def lsp_line_char(source : String, needle : String, occurrence : Int32 = 0, at_end : Bool = false) : {Int32, Int32}
  offset = nil
  search_from = 0
  (occurrence + 1).times do
    found = source.index(needle, search_from)
    raise "Missing needle #{needle}" unless found
    offset = found
    search_from = found + needle.bytesize
  end

  target = offset.not_nil! + (at_end ? needle.bytesize : 0)
  line = source[0, target].count('\n')
  line_start = source.rindex('\n', target) || -1
  {line, target - line_start - 1}
end

private def with_lsp_project_cache_env(&)
  prev_stub = ENV["CRYSTALV2_LSP_FORCE_STUB"]?
  prev_xdg = ENV["XDG_CACHE_HOME"]?
  cache_dir = File.join(Dir.tempdir, "lsp_project_cache_env_#{Random::Secure.hex(6)}")
  FileUtils.mkdir_p(cache_dir)
  ENV["CRYSTALV2_LSP_FORCE_STUB"] = "1"
  ENV["XDG_CACHE_HOME"] = cache_dir
  yield
ensure
  if prev_stub
    ENV["CRYSTALV2_LSP_FORCE_STUB"] = prev_stub
  else
    ENV.delete("CRYSTALV2_LSP_FORCE_STUB")
  end

  if prev_xdg
    ENV["XDG_CACHE_HOME"] = prev_xdg
  else
    ENV.delete("XDG_CACHE_HOME")
  end

  FileUtils.rm_rf(cache_dir) if cache_dir
end

describe "LSP project cache semantic fidelity" do
  it "preserves cached method parameter metadata for signature help" do
    with_lsp_project_cache_env do
      root = File.join(Dir.tempdir, "lsp_project_cache_sig_#{Random::Secure.hex(6)}")
      src_dir = File.join(root, "src")
      FileUtils.mkdir_p(src_dir)
      File.write(File.join(root, "shard.yml"), "name: lsp_project_cache_sig\n")

      helper_path = File.join(src_dir, "helper.cr")
      helper_source = <<-CR
      class Helper
        def value(scale : Int32) : Int32
          scale
        end
      end
      CR
      File.write(helper_path, helper_source)

      main_path = File.join(src_dir, "main.cr")
      main_source = <<-CR
      require "./helper"

      helper = Helper.new
      helper.value(2)
      CR
      File.write(main_path, main_source)

      baseline = CrystalV2::Compiler::LSP::Server.new(
        IO::Memory.new,
        IO::Memory.new,
        CrystalV2::Compiler::LSP::ServerConfig.new(background_indexing: false, project_cache: false)
      )
      baseline_uri = baseline.spec_store_document(main_source, src_dir, main_path)
      sig_line, sig_char = lsp_line_char(main_source, "value(", at_end: true)
      baseline_sig = baseline.spec_signature_help(baseline_uri, sig_line, sig_char)
      baseline_label = baseline_sig["result"]["signatures"].as_a.first["label"].as_s
      baseline_label.should contain("scale : Int32")
      completion_line, completion_char = lsp_line_char(main_source, "helper.", at_end: true)
      baseline_completion = baseline.spec_completion(baseline_uri, completion_line, completion_char)
      baseline_labels = baseline_completion["result"].as_a.map { |item| item["label"].as_s }
      baseline_labels.should contain("value")
      definition_line, definition_char = lsp_line_char(main_source, "value", occurrence: 0)
      baseline_definition = baseline.spec_definition(baseline_uri, definition_line, definition_char)
      baseline_definition["result"].as_a.first["uri"].as_s.should contain("helper.cr")

      project = CrystalV2::Compiler::LSP::UnifiedProjectState.new
      project.update_file(helper_path, helper_source)
      project.update_file(main_path, main_source)
      CrystalV2::Compiler::LSP::ProjectCacheLoader.save_to_cache(project, root)

      cached = CrystalV2::Compiler::LSP::Server.new(
        IO::Memory.new,
        IO::Memory.new,
        CrystalV2::Compiler::LSP::ServerConfig.new(background_indexing: false, project_cache: true)
      )
      cached_uri = cached.spec_did_open_document(main_source, main_path)
      cached_sig = cached.spec_signature_help(cached_uri, sig_line, sig_char)
      cached_label = cached_sig["result"]["signatures"].as_a.first["label"].as_s

      cached_label.should eq(baseline_label)
      cached_completion = cached.spec_completion(cached_uri, completion_line, completion_char)
      cached_completion["result"].as_a.map { |item| item["label"].as_s }.should contain("value")
      cached_definition = cached.spec_definition(cached_uri, definition_line, definition_char)
      cached_definition["result"].as_a.first["uri"].as_s.should eq(baseline_definition["result"].as_a.first["uri"].as_s)
    ensure
      FileUtils.rm_rf(root) if root
    end
  end

  it "keeps same-file method definitions visible after warming project cache" do
    with_lsp_project_cache_env do
      root = File.join(Dir.tempdir, "lsp_project_cache_def_#{Random::Secure.hex(6)}")
      src_dir = File.join(root, "src")
      FileUtils.mkdir_p(src_dir)
      File.write(File.join(root, "shard.yml"), "name: lsp_project_cache_def\n")

      path = File.join(src_dir, "main.cr")
      source = <<-CR
      class Service
        def handle_completion(id : Int32, params : String?) : Nil
        end

        def dispatch
          handle_completion(1, nil)
        end
      end
      CR
      File.write(path, source)

      baseline = CrystalV2::Compiler::LSP::Server.new(
        IO::Memory.new,
        IO::Memory.new,
        CrystalV2::Compiler::LSP::ServerConfig.new(background_indexing: false, project_cache: false)
      )
      baseline_uri = baseline.spec_store_document(source, src_dir, path)
      definition_line, definition_char = lsp_line_char(source, "handle_completion(1")
      baseline_definition = baseline.spec_definition(baseline_uri, definition_line, definition_char)
      baseline_definition["result"].as_a.size.should eq(1)

      project = CrystalV2::Compiler::LSP::UnifiedProjectState.new
      project.update_file(path, source)
      CrystalV2::Compiler::LSP::ProjectCacheLoader.save_to_cache(project, root)

      cached = CrystalV2::Compiler::LSP::Server.new(
        IO::Memory.new,
        IO::Memory.new,
        CrystalV2::Compiler::LSP::ServerConfig.new(background_indexing: false, project_cache: true)
      )
      cached_uri = cached.spec_did_open_document(source, path)
      cached.spec_set_prelude_loading(true)
      cached_definition = cached.spec_definition(cached_uri, definition_line, definition_char)

      cached_definition["result"].as_a.size.should eq(1)
      cached_definition["result"].as_a.first["uri"].as_s.should eq(baseline_definition["result"].as_a.first["uri"].as_s)
    ensure
      FileUtils.rm_rf(root) if root
    end
  end

  it "preserves require fallback paths for cached foreground documents" do
    with_lsp_project_cache_env do
      root = File.join(Dir.tempdir, "lsp_project_cache_requires_#{Random::Secure.hex(6)}")
      src_dir = File.join(root, "src")
      FileUtils.mkdir_p(src_dir)
      File.write(File.join(root, "shard.yml"), "name: lsp_project_cache_requires\n")

      helper_path = File.join(src_dir, "parser_like.cr")
      helper_source = <<-CR
      class LexerLike
      end

      class ParserLike
        def initialize(lexer : LexerLike)
        end

        def parse_program : String
          "ok"
        end

        private def reset_state : Nil
        end
      end
      CR
      File.write(helper_path, helper_source)

      main_path = File.join(src_dir, "main.cr")
      main_source = <<-CR
      require "./parser_like"

      lexer = LexerLike.new
      parser = ParserLike.new(lexer)
      parser.parse_program
      CR
      File.write(main_path, main_source)

      project = CrystalV2::Compiler::LSP::UnifiedProjectState.new
      project.update_file(main_path, main_source)
      CrystalV2::Compiler::LSP::ProjectCacheLoader.save_to_cache(project, root)

      cached = CrystalV2::Compiler::LSP::Server.new(
        IO::Memory.new,
        IO::Memory.new,
        CrystalV2::Compiler::LSP::ServerConfig.new(background_indexing: false, project_cache: true)
      )
      cached_uri = cached.spec_did_open_document(main_source, main_path)

      lexer_line, lexer_char = lsp_line_char(main_source, "LexerLike.new")
      lexer_definition = cached.spec_definition(cached_uri, lexer_line, lexer_char)
      lexer_definition["result"].as_a.first["uri"].as_s.should contain("parser_like.cr")

      signature_line, signature_char = lsp_line_char(main_source, "ParserLike.new(", at_end: true)
      signature = cached.spec_signature_help(cached_uri, signature_line, signature_char)
      signature["result"]["signatures"].as_a.size.should be >= 1

      completion_line, completion_char = lsp_line_char(main_source, "parser.", at_end: true)
      completion = cached.spec_completion(cached_uri, completion_line, completion_char)
      completion_labels = completion["result"].as_a.map { |item| item["label"].as_s }
      completion_labels.should contain("parse_program")
      completion_labels.should contain("reset_state")
    ensure
      FileUtils.rm_rf(root) if root
    end
  end

  it "serves semantic tokens before cached foreground identifier materialization" do
    with_lsp_project_cache_env do
      root = File.join(Dir.tempdir, "lsp_project_cache_tokens_#{Random::Secure.hex(6)}")
      src_dir = File.join(root, "src")
      FileUtils.mkdir_p(src_dir)
      File.write(File.join(root, "shard.yml"), "name: lsp_project_cache_tokens\n")

      path = File.join(src_dir, "main.cr")
      source = <<-CR
      class Service
        def handle(value : Int32) : Int32
          value
        end

        def dispatch
          handle(1)
        end
      end
      CR
      File.write(path, source)

      project = CrystalV2::Compiler::LSP::UnifiedProjectState.new
      project.update_file(path, source)
      CrystalV2::Compiler::LSP::ProjectCacheLoader.save_to_cache(project, root)

      cached = CrystalV2::Compiler::LSP::Server.new(
        IO::Memory.new,
        IO::Memory.new,
        CrystalV2::Compiler::LSP::ServerConfig.new(background_indexing: false, project_cache: true)
      )
      cached_uri = cached.spec_did_open_document(source, path)
      cached.spec_identifier_symbols_built?(cached_uri).should be_false

      tokens = cached.spec_semantic_tokens(cached_uri)
      tokens["result"]["data"].as_a.should_not be_empty
      cached.spec_identifier_symbols_built?(cached_uri).should be_false

      definition_line, definition_char = lsp_line_char(source, "handle(1")
      definition = cached.spec_definition(cached_uri, definition_line, definition_char)
      definition["result"].as_a.size.should eq(1)
      cached.spec_identifier_symbols_built?(cached_uri).should be_true
    ensure
      FileUtils.rm_rf(root) if root
    end
  end
end
