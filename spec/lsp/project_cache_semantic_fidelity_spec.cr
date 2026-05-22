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

      service = Service.new
      service.handle(1)
      CR
      File.write(path, source)

      project = CrystalV2::Compiler::LSP::UnifiedProjectState.new
      project.update_file(path, source)
      CrystalV2::Compiler::LSP::ProjectCacheLoader.save_to_cache(project, root)
      prewarm = CrystalV2::Compiler::LSP::Server.new(
        IO::Memory.new,
        IO::Memory.new,
        CrystalV2::Compiler::LSP::ServerConfig.new(background_indexing: false, project_cache: false, ast_cache: true)
      )
      prewarm.spec_did_open_document(source, path)

      cached = CrystalV2::Compiler::LSP::Server.new(
        IO::Memory.new,
        IO::Memory.new,
        CrystalV2::Compiler::LSP::ServerConfig.new(background_indexing: false, project_cache: true)
      )
      cached_uri = cached.spec_did_open_document(source, path)
      cached.spec_identifier_symbols_built?(cached_uri).should be_false
      cached.spec_document_ast_loaded?(cached_uri).should be_false
      cached.spec_project_update_pending?(cached_uri).should be_false

      tokens = cached.spec_semantic_tokens(cached_uri)
      tokens["result"]["data"].as_a.should_not be_empty
      cached.spec_document_ast_loaded?(cached_uri).should be_true
      cached.spec_identifier_symbols_built?(cached_uri).should be_false

      definition_line, definition_char = lsp_line_char(source, "handle(1")
      definition = cached.spec_definition(cached_uri, definition_line, definition_char)
      definition["result"].as_a.size.should eq(1)
      cached.spec_identifier_symbols_built?(cached_uri).should be_false
    ensure
      FileUtils.rm_rf(root) if root
    end
  end

  it "materializes cached lightweight opens for first precision requests" do
    with_lsp_project_cache_env do
      root = File.join(Dir.tempdir, "lsp_project_cache_precision_#{Random::Secure.hex(6)}")
      src_dir = File.join(root, "src")
      FileUtils.mkdir_p(src_dir)
      File.write(File.join(root, "shard.yml"), "name: lsp_project_cache_precision\n")

      helper_path = File.join(src_dir, "helper.cr")
      helper_source = <<-CR
      class Helper
        def value(scale : Int32) : Int32
          scale
        end
      end
      CR
      File.write(helper_path, helper_source)

      path = File.join(src_dir, "main.cr")
      source = <<-CR
      require "./helper"

      class Entry
        def self.target(value : Int32) : Int32
          value
        end

        def self.run
          helper = Helper.new
          helper.value(2)
          target(1)
        end
      end
      CR
      File.write(path, source)

      project = CrystalV2::Compiler::LSP::UnifiedProjectState.new
      project.update_file(helper_path, helper_source)
      project.update_file(path, source)
      CrystalV2::Compiler::LSP::ProjectCacheLoader.save_to_cache(project, root)
      prewarm = CrystalV2::Compiler::LSP::Server.new(
        IO::Memory.new,
        IO::Memory.new,
        CrystalV2::Compiler::LSP::ServerConfig.new(background_indexing: false, project_cache: false, ast_cache: true)
      )
      prewarm.spec_did_open_document(source, path)

      rename_server = CrystalV2::Compiler::LSP::Server.new(
        IO::Memory.new,
        IO::Memory.new,
        CrystalV2::Compiler::LSP::ServerConfig.new(background_indexing: false, project_cache: true)
      )
      rename_uri = rename_server.spec_did_open_document(source, path)
      rename_server.spec_document_ast_loaded?(rename_uri).should be_false

      rename_line, rename_char = lsp_line_char(source, "Entry")
      prepare_rename = rename_server.spec_prepare_rename(rename_uri, rename_line, rename_char)
      prepare_rename["result"].should_not be_nil
      rename_server.spec_document_ast_loaded?(rename_uri).should be_true
      rename_server.spec_identifier_symbols_built?(rename_uri).should be_true

      definition_server = CrystalV2::Compiler::LSP::Server.new(
        IO::Memory.new,
        IO::Memory.new,
        CrystalV2::Compiler::LSP::ServerConfig.new(background_indexing: false, project_cache: true)
      )
      definition_uri = definition_server.spec_did_open_document(source, path)
      definition_server.spec_document_ast_loaded?(definition_uri).should be_false

      target_line, target_char = lsp_line_char(source, "target(1")
      definition = definition_server.spec_definition(definition_uri, target_line, target_char)
      definition["result"].as_a.size.should eq(1)
      definition_server.spec_document_ast_loaded?(definition_uri).should be_false

      hover_server = CrystalV2::Compiler::LSP::Server.new(
        IO::Memory.new,
        IO::Memory.new,
        CrystalV2::Compiler::LSP::ServerConfig.new(background_indexing: false, project_cache: true)
      )
      hover_uri = hover_server.spec_did_open_document(source, path)
      hover_server.spec_document_ast_loaded?(hover_uri).should be_false

      hover = hover_server.spec_hover(hover_uri, target_line, target_char)
      hover["result"]["contents"]["value"].as_s.should contain("def self.target")
      hover_server.spec_document_ast_loaded?(hover_uri).should be_false

      member_definition_server = CrystalV2::Compiler::LSP::Server.new(
        IO::Memory.new,
        IO::Memory.new,
        CrystalV2::Compiler::LSP::ServerConfig.new(background_indexing: false, project_cache: true)
      )
      member_definition_uri = member_definition_server.spec_did_open_document(source, path)
      member_definition_server.spec_document_ast_loaded?(member_definition_uri).should be_false

      value_line, value_char = lsp_line_char(source, "value(2")
      member_definition = member_definition_server.spec_definition(member_definition_uri, value_line, value_char)
      member_definition["result"].as_a.size.should eq(1)
      member_definition["result"].as_a.first["uri"].as_s.should contain("helper.cr")
      member_definition_server.spec_document_ast_loaded?(member_definition_uri).should be_false

      member_hover_server = CrystalV2::Compiler::LSP::Server.new(
        IO::Memory.new,
        IO::Memory.new,
        CrystalV2::Compiler::LSP::ServerConfig.new(background_indexing: false, project_cache: true)
      )
      member_hover_uri = member_hover_server.spec_did_open_document(source, path)
      member_hover_server.spec_document_ast_loaded?(member_hover_uri).should be_false

      member_hover = member_hover_server.spec_hover(member_hover_uri, value_line, value_char)
      member_hover["result"]["contents"]["value"].as_s.should contain("def value")
      member_hover_server.spec_document_ast_loaded?(member_hover_uri).should be_false

      signature_server = CrystalV2::Compiler::LSP::Server.new(
        IO::Memory.new,
        IO::Memory.new,
        CrystalV2::Compiler::LSP::ServerConfig.new(background_indexing: false, project_cache: true)
      )
      signature_uri = signature_server.spec_did_open_document(source, path)
      signature_server.spec_document_ast_loaded?(signature_uri).should be_false

      signature_line, signature_char = lsp_line_char(source, "value(", at_end: true)
      signature = signature_server.spec_signature_help(signature_uri, signature_line, signature_char)
      signature["result"]["signatures"].as_a.size.should be >= 1
      signature_server.spec_document_ast_loaded?(signature_uri).should be_true

      completion_server = CrystalV2::Compiler::LSP::Server.new(
        IO::Memory.new,
        IO::Memory.new,
        CrystalV2::Compiler::LSP::ServerConfig.new(background_indexing: false, project_cache: true)
      )
      completion_uri = completion_server.spec_did_open_document(source, path)
      completion_server.spec_document_ast_loaded?(completion_uri).should be_false

      completion_line, completion_char = lsp_line_char(source, "helper.", at_end: true)
      completion = completion_server.spec_completion(completion_uri, completion_line, completion_char)
      completion["result"].as_a.map { |item| item["label"].as_s }.should contain("value")
      completion_server.spec_document_ast_loaded?(completion_uri).should be_false

      symbols_server = CrystalV2::Compiler::LSP::Server.new(
        IO::Memory.new,
        IO::Memory.new,
        CrystalV2::Compiler::LSP::ServerConfig.new(background_indexing: false, project_cache: true)
      )
      symbols_uri = symbols_server.spec_did_open_document(source, path)
      symbols_server.spec_document_ast_loaded?(symbols_uri).should be_false

      symbols = symbols_server.spec_document_symbols(symbols_uri)
      symbols["result"].as_a.map { |entry| entry["name"].as_s }.should contain("Entry")
      symbols_server.spec_document_ast_loaded?(symbols_uri).should be_true

      folding_server = CrystalV2::Compiler::LSP::Server.new(
        IO::Memory.new,
        IO::Memory.new,
        CrystalV2::Compiler::LSP::ServerConfig.new(background_indexing: false, project_cache: true)
      )
      folding_uri = folding_server.spec_did_open_document(source, path)
      folding_server.spec_document_ast_loaded?(folding_uri).should be_false

      folding = folding_server.spec_folding_ranges(folding_uri)
      folding["result"].as_a.should_not be_empty
      folding_server.spec_document_ast_loaded?(folding_uri).should be_true
    ensure
      FileUtils.rm_rf(root) if root
    end
  end
end
