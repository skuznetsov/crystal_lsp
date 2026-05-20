require "spec"
require "file_utils"
require "random/secure"

require "./support/server_helper"

describe "LSP background prelude loading" do
  it "does not start duplicate background prelude loads while one is already in flight" do
    dir = File.join(Dir.tempdir, "lsp_bg_prelude_#{Random::Secure.hex(6)}")
    FileUtils.mkdir_p(dir)
    log_path = File.join(dir, "server.log")

    server = CrystalV2::Compiler::LSP::Server.new(
      IO::Memory.new,
      IO::Memory.new,
      CrystalV2::Compiler::LSP::ServerConfig.new(
        background_indexing: true,
        project_cache: false,
        debug_log_path: log_path
      )
    )

    5.times { server.spec_ensure_prelude_loaded }

    log = File.read(log_path)
    start_count = log.lines.count(&.includes?("Background prelude loading started"))
    start_count.should eq(1)
  ensure
    FileUtils.rm_rf(dir) if dir
  end
end
