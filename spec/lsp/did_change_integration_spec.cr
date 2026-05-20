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

  it "applies incremental range edits from didChange" do
    dir = File.join(Dir.tempdir, "lsp_did_change_#{Random::Secure.hex(6)}")
    FileUtils.mkdir_p(dir)
    path = File.join(dir, "main.cr")
    source = "x = 1\ny = x + 1\n"
    File.write(path, source)

    server = CrystalV2::Compiler::LSP::Server.new(
      IO::Memory.new,
      IO::Memory.new,
      CrystalV2::Compiler::LSP::ServerConfig.new(background_indexing: false, project_cache: false)
    )
    uri = server.spec_store_document(source, dir, path)

    # Replace "1" in "x = 1" with "2" using an incremental range edit.
    changes = %([{"range":{"start":{"line":0,"character":4},"end":{"line":0,"character":5}},"text":"2"}])
    server.spec_did_change(uri, 2, changes)

    server.spec_document_text(uri).should eq("x = 2\ny = x + 1\n")
  ensure
    FileUtils.rm_rf(dir) if dir
  end

  it "keeps full-sync didChange behavior when range is absent" do
    dir = File.join(Dir.tempdir, "lsp_did_change_full_#{Random::Secure.hex(6)}")
    FileUtils.mkdir_p(dir)
    path = File.join(dir, "main.cr")
    source = "a = 10\n"
    File.write(path, source)

    server = CrystalV2::Compiler::LSP::Server.new(
      IO::Memory.new,
      IO::Memory.new,
      CrystalV2::Compiler::LSP::ServerConfig.new(background_indexing: false, project_cache: false)
    )
    uri = server.spec_store_document(source, dir, path)

    updated = "a = 20\nb = a + 1\n"
    changes = %([{"text":#{updated.to_json}}])
    server.spec_did_change(uri, 2, changes)

    server.spec_document_text(uri).should eq(updated)
  ensure
    FileUtils.rm_rf(dir) if dir
  end

  it "defers UnifiedProject updates behind the immediate document path" do
    dir = File.join(Dir.tempdir, "lsp_deferred_project_update_#{Random::Secure.hex(6)}")
    FileUtils.mkdir_p(dir)
    path = File.join(dir, "main.cr")
    source = "value = 1\n"
    File.write(path, source)

    server = CrystalV2::Compiler::LSP::Server.new(
      IO::Memory.new,
      IO::Memory.new,
      CrystalV2::Compiler::LSP::ServerConfig.new(background_indexing: false, project_cache: false, debounce_ms: 10_000)
    )

    uri = server.spec_did_open_document(source, path)

    server.spec_document_text(uri).should eq(source)
    server.spec_project_update_pending?(uri).should be_true
    server.spec_project_pending_version(uri).should eq(1)
    server.spec_project_has_file?(path).should be_false

    server.spec_flush_project_updates
    server.spec_project_has_file?(path).should be_true
  ensure
    FileUtils.rm_rf(dir) if dir
  end

  it "requeues UnifiedProject updates while foreground activity is recent" do
    dir = File.join(Dir.tempdir, "lsp_project_update_idle_#{Random::Secure.hex(6)}")
    FileUtils.mkdir_p(dir)
    path = File.join(dir, "main.cr")
    source = "value = 1\n"
    File.write(path, source)

    server = CrystalV2::Compiler::LSP::Server.new(
      IO::Memory.new,
      IO::Memory.new,
      CrystalV2::Compiler::LSP::ServerConfig.new(background_indexing: false, project_cache: false, debounce_ms: 10_000)
    )

    uri = server.spec_file_uri(path)
    server.spec_process_queued_project_update(uri, source, 1).should be_false

    server.spec_project_update_pending?(uri).should be_true
    server.spec_project_pending_version(uri).should eq(1)
    server.spec_project_has_file?(path).should be_false
  ensure
    FileUtils.rm_rf(dir) if dir
  end

  it "defers invalid project-cache reparse until foreground is idle" do
    dir = File.join(Dir.tempdir, "lsp_invalid_reparse_idle_#{Random::Secure.hex(6)}")
    FileUtils.mkdir_p(dir)
    path = File.join(dir, "stale.cr")
    source = "value = 1\n"
    File.write(path, source)

    server = CrystalV2::Compiler::LSP::Server.new(
      IO::Memory.new,
      IO::Memory.new,
      CrystalV2::Compiler::LSP::ServerConfig.new(background_indexing: false, project_cache: false, debounce_ms: 40)
    )

    server.spec_schedule_reparse_invalid_files([path])
    sleep 80.milliseconds
    server.spec_project_has_file?(path).should be_false

    server.spec_did_open_document(source, path)

    deadline = Time.instant + 1.second
    until server.spec_project_has_file?(path) || Time.instant >= deadline
      sleep 10.milliseconds
    end
    server.spec_project_has_file?(path).should be_true
  ensure
    FileUtils.rm_rf(dir) if dir
  end
end
