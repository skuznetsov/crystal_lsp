require "../src/compiler/frontend/lexer"
require "../src/compiler/frontend/parser"
require "../src/compiler/frontend/watchdog"

struct FileReport
  getter path : String
  getter errors : Int32
  def initialize(@path : String, @errors : Int32); end
end

begin
  # Use streaming mode for the v2 parser during full-tree scans to avoid
  # pre-tokenizing entire files up front. This significantly reduces memory
  # pressure and can improve throughput on large stdlib files like array.cr.
  ENV["CRYSTAL_V2_PARSER_STREAM"] = "1" if ENV["CRYSTAL_V2_PARSER_STREAM"]?.nil?
rescue KeyError
  # Some stdlib Env implementations may raise on writes; ignore and fall back
  # to non-streaming mode in that case.
end

files = Dir.glob("#{__DIR__}/../src/**/*.cr").sort
reports = [] of FileReport
total_errors = 0
scanned = 0

files.each_with_index do |file, idx|
  source = File.read(file)
  lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
  parser = CrystalV2::Compiler::Frontend::Parser.new(lexer, recovery_mode: ENV["CRYSTAL_V2_LSP_RECOVERY"]? == "1")
  begin
    # Optional watchdog to prevent hangs on individual files during a global
    # scan. Enable with environment:
    #   ENABLE_WATCHDOG=1 SCAN_TIMEOUT=0.1 ./bin/count_parser_diagnostics
    # By default SCAN_TIMEOUT falls back to 0.1 seconds if not provided.
    if ENV["ENABLE_WATCHDOG"]? || ENV["SCAN_TIMEOUT"]?
      timeout_s = (ENV["SCAN_TIMEOUT"]? || "0.1").to_f
      CrystalV2::Compiler::Frontend::Watchdog.enable!(
        "count_parser_diagnostics timeout for #{file}",
        timeout_s.seconds
      )
    end

    parser.parse_program
  rescue ex : CrystalV2::Compiler::Frontend::Watchdog::TimeoutError
    STDERR.puts "Watchdog timeout while parsing #{file} (##{idx}): #{ex.message}"
    STDERR.puts ex.backtrace?.try(&.join('\n'))
    exit 1
  ensure
    CrystalV2::Compiler::Frontend::Watchdog.disable!
  end

  errs = parser.diagnostics.size
  total_errors += errs
  scanned += 1
  reports << FileReport.new(file, errs) if errs > 0
end

reports.sort_by!(&.errors).reverse!

puts "Scanned files: #{scanned}"
puts "Files with diagnostics: #{reports.size}"
puts "Total diagnostics: #{total_errors}"
puts
puts "Top 20 files by diagnostics:"
reports.first(20).each do |r|
  puts "#{r.errors}	#{r.path}"
end
