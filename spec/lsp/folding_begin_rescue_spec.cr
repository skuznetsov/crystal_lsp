require "spec"

require "./support/server_helper"

describe CrystalV2::Compiler::LSP::Server do
  it "folds begin block stopping before rescue" do
    source = <<-CR
    begin
      work
    rescue ex
      handle(ex)
    end
    CR

    server = CrystalV2::Compiler::LSP::Server.new(IO::Memory.new, IO::Memory.new, CrystalV2::Compiler::LSP::ServerConfig.new(background_indexing: false, project_cache: false))
    _diags, program, _tc, _ids, _symtab, _req = server.spec_analyze_document(source, nil, "/tmp/fold_rescue.cr")

    ranges = server.spec_collect_folding_ranges(program)
    # Expect a single range from begin line to the line before rescue
    ranges.should_not be_empty
    range = ranges.first
    range.start_line.should eq(0)
    range.end_line.should eq(1) # line before rescue
  end

  it "folds rescue clause body" do
    source = <<-CR
    begin
      work
    rescue ex
      handle(ex)
      log(ex)
    end
    CR

    server = CrystalV2::Compiler::LSP::Server.new(IO::Memory.new, IO::Memory.new, CrystalV2::Compiler::LSP::ServerConfig.new(background_indexing: false, project_cache: false))
    _diags, program, _tc, _ids, _symtab, _req = server.spec_analyze_document(source, nil, "/tmp/fold_rescue.cr")
    ranges = server.spec_collect_folding_ranges(program)

    rescue_range = ranges.find { |r| r.start_line == 2 }
    rescue_range.should_not be_nil
    rescue_range.not_nil!.end_line.should eq(4)
  end

  it "folds ensure clause body" do
    source = <<-CR
    begin
      work
    rescue ex
      handle(ex)
    ensure
      cleanup
      more_cleanup
    end
    CR

    server = CrystalV2::Compiler::LSP::Server.new(IO::Memory.new, IO::Memory.new, CrystalV2::Compiler::LSP::ServerConfig.new(background_indexing: false, project_cache: false))
    _diags, program, _tc, _ids, _symtab, _req = server.spec_analyze_document(source, nil, "/tmp/fold_rescue.cr")
    ranges = server.spec_collect_folding_ranges(program)

    ensure_range = ranges.find { |r| r.start_line == 5 }
    ensure_range.should_not be_nil
    ensure_range.not_nil!.end_line.should eq(6)
  end
end
