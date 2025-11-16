require "../src/compiler/frontend/lexer"
require "../src/compiler/frontend/parser"
require "../src/compiler/frontend/watchdog"

module ScanParser
  alias Watchdog = CrystalV2::Compiler::Frontend::Watchdog

  TIMEOUT = (ENV["SCAN_WATCHDOG_TIMEOUT"]? || "30").to_f.seconds

  def self.parse_file(path : String)
    source = File.read(path)
    lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
    parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
    parser.parse_program
  end

  def self.with_watchdog(label : String, &block : ->)
    Watchdog.enable!(label, TIMEOUT)
    begin
      yield
    ensure
      Watchdog.disable!
    end
  end
end

files = Dir.glob("#{__DIR__}/../src/**/*.cr").sort
max_files = ENV["SCAN_MAX_FILES"]?.try(&.to_i)
max_duration = ENV["SCAN_MAX_DURATION"]?.try(&.to_f)
start_time = Time.monotonic
processed = 0

files.each_with_index do |file, idx|
  break if max_files && processed >= max_files
  break if max_duration && (Time.monotonic - start_time) > max_duration.seconds
  puts "[#{idx}] start #{file}"
  STDOUT.flush
  begin
    ScanParser.with_watchdog("parsing #{file}") do
      ScanParser.parse_file(file)
    end
  rescue e : CrystalV2::Compiler::Frontend::Watchdog::TimeoutError
    STDERR.puts "Watchdog triggered: #{e.message}"
    raise e
  end
  processed += 1
end

puts "scan complete"
