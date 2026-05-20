require "spec"
require "file_utils"
require "random/secure"

require "./support/server_helper"

describe "LSP AST cache foreground document loading" do
  it "reuses AST cache when reopening an unchanged foreground document" do
    dir = File.join(Dir.tempdir, "lsp_ast_foreground_#{Random::Secure.hex(6)}")
    cache_home = File.join(dir, "cache")
    src_dir = File.join(dir, "src")
    FileUtils.mkdir_p(src_dir)

    prev_cache_home = ENV["XDG_CACHE_HOME"]?
    ENV["XDG_CACHE_HOME"] = cache_home

    path = File.join(src_dir, "main.cr")
    log1 = File.join(dir, "server1.log")
    log2 = File.join(dir, "server2.log")
    source = <<-CR
    class ForegroundCached
      def answer
        42
      end
    end
    CR
    File.write(path, source)

    server1 = CrystalV2::Compiler::LSP::Server.new(
      IO::Memory.new,
      IO::Memory.new,
      CrystalV2::Compiler::LSP::ServerConfig.new(
        background_indexing: false,
        project_cache: false,
        ast_cache: true,
        debug_log_path: log1
      )
    )
    server1.spec_did_open_document(source, path)
    File.exists?(CrystalV2::Compiler::LSP::AstCache.cache_path(path)).should be_true

    server2 = CrystalV2::Compiler::LSP::Server.new(
      IO::Memory.new,
      IO::Memory.new,
      CrystalV2::Compiler::LSP::ServerConfig.new(
        background_indexing: false,
        project_cache: false,
        ast_cache: true,
        debug_log_path: log2
      )
    )
    server2.spec_did_open_document(source, path)

    File.read(log2).should contain("Loading foreground document #{path} from AST cache")
  ensure
    if prev_cache_home
      ENV["XDG_CACHE_HOME"] = prev_cache_home
    else
      ENV.delete("XDG_CACHE_HOME")
    end
    FileUtils.rm_rf(dir) if dir
  end

  it "does not reuse disk AST cache for unsaved foreground edits" do
    dir = File.join(Dir.tempdir, "lsp_ast_foreground_edit_#{Random::Secure.hex(6)}")
    cache_home = File.join(dir, "cache")
    src_dir = File.join(dir, "src")
    FileUtils.mkdir_p(src_dir)

    prev_cache_home = ENV["XDG_CACHE_HOME"]?
    ENV["XDG_CACHE_HOME"] = cache_home

    path = File.join(src_dir, "main.cr")
    log1 = File.join(dir, "server1.log")
    log2 = File.join(dir, "server2.log")
    source = <<-CR
    class ForegroundEdited
      def answer
        42
      end
    end
    CR
    edited_source = source.sub("42", "43")
    File.write(path, source)

    server1 = CrystalV2::Compiler::LSP::Server.new(
      IO::Memory.new,
      IO::Memory.new,
      CrystalV2::Compiler::LSP::ServerConfig.new(
        background_indexing: false,
        project_cache: false,
        ast_cache: true,
        debug_log_path: log1
      )
    )
    server1.spec_did_open_document(source, path)
    File.exists?(CrystalV2::Compiler::LSP::AstCache.cache_path(path)).should be_true

    server2 = CrystalV2::Compiler::LSP::Server.new(
      IO::Memory.new,
      IO::Memory.new,
      CrystalV2::Compiler::LSP::ServerConfig.new(
        background_indexing: false,
        project_cache: false,
        ast_cache: true,
        debug_log_path: log2,
        debounce_ms: 10_000
      )
    )
    uri = server2.spec_did_open_document(source, path)
    server2.spec_did_change(uri, 2, %([{"text":#{edited_source.to_json}}]))

    File.read(log2).lines.count(&.includes?("Loading foreground document #{path} from AST cache")).should eq(1)
    server2.spec_document_text(uri).should eq(edited_source)
  ensure
    if prev_cache_home
      ENV["XDG_CACHE_HOME"] = prev_cache_home
    else
      ENV.delete("XDG_CACHE_HOME")
    end
    FileUtils.rm_rf(dir) if dir
  end
end
