require "spec"
require "file_utils"
require "random/secure"

require "./support/server_helper"

describe "LSP AST cache prelude loading" do
  around_each do |example|
    prev_ast_cache = ENV["LSP_AST_CACHE"]?
    ENV["LSP_AST_CACHE"] = "1"
    begin
      example.run
    ensure
      if prev_ast_cache
        ENV["LSP_AST_CACHE"] = prev_ast_cache
      else
        ENV.delete("LSP_AST_CACHE")
      end
    end
  end

  it "keeps the prelude summary cache enabled with AST cache enabled" do
    dir = File.join(Dir.tempdir, "lsp_ast_prelude_summary_#{Random::Secure.hex(6)}")
    cache_home = File.join(dir, "cache")
    log = File.join(dir, "server.log")
    FileUtils.mkdir_p(cache_home)

    prev_cache_home = ENV["XDG_CACHE_HOME"]?
    ENV["XDG_CACHE_HOME"] = cache_home

    stdlib_path = File.dirname(CrystalV2::Compiler::LSP::Server::PRELUDE_PATH)
    cache = CrystalV2::Compiler::LSP::PreludeCache.new(
      [] of CrystalV2::Compiler::LSP::CachedSymbolInfo,
      CrystalV2::Compiler::LSP::PreludeCache.compute_stdlib_hash(stdlib_path)
    )
    cache.save

    CrystalV2::Compiler::LSP::Server.new(
      IO::Memory.new,
      IO::Memory.new,
      CrystalV2::Compiler::LSP::ServerConfig.new(
        background_indexing: false,
        project_cache: false,
        ast_cache: true,
        debug_log_path: log
      )
    )

    log_text = File.read(log)
    log_text.should contain("Prelude loaded from cache")
    log_text.should_not contain("Analyzing prelude file")
  ensure
    if prev_cache_home
      ENV["XDG_CACHE_HOME"] = prev_cache_home
    else
      ENV.delete("XDG_CACHE_HOME")
    end
    FileUtils.rm_rf(dir) if dir
  end

  it "reuses AST cache when reloading a prelude dependency" do
    dir = File.join(Dir.tempdir, "lsp_ast_prelude_#{Random::Secure.hex(6)}")
    FileUtils.mkdir_p(dir)
    path = File.join(dir, "prelude_dep.cr")
    log1 = File.join(dir, "prelude1.log")
    log2 = File.join(dir, "prelude2.log")
    cache_path = CrystalV2::Compiler::LSP::AstCache.cache_path(path)

    File.write(path, <<-CR)
    module PreludeDep
      VALUE = 42

      def self.answer
        VALUE
      end
    end
    CR

    program_cache1 = {} of String => CrystalV2::Compiler::Frontend::Program
    source_cache1 = {} of String => String
    diagnostics1 = [] of CrystalV2::Compiler::LSP::Diagnostic
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
    server1.spec_load_prelude_program(path, program_cache1, source_cache1, diagnostics1).should be_true
    File.exists?(cache_path).should be_true

    program_cache2 = {} of String => CrystalV2::Compiler::Frontend::Program
    source_cache2 = {} of String => String
    diagnostics2 = [] of CrystalV2::Compiler::LSP::Diagnostic
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
    server2.spec_load_prelude_program(path, program_cache2, source_cache2, diagnostics2).should be_true

    File.read(log2).should contain("Loading prelude dependency #{path} from AST cache")
  ensure
    FileUtils.rm_rf(dir) if dir
    File.delete(cache_path) if cache_path && File.exists?(cache_path)
  end
end
