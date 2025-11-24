require "../src/compiler/frontend/lexer"
require "../src/compiler/frontend/parser"
require "../src/compiler/frontend/watchdog"

watchdog_timeout = ENV["SCAN_TIMEOUT"]? || (ENV["ENABLE_WATCHDOG"]? ? "0.5" : nil)
timeout_span = watchdog_timeout ? watchdog_timeout.to_f.seconds : nil

paths = (ENV["CRYSTAL_PATH"]? || "").split(":")
files = paths.flat_map { |p| Dir.glob(File.join(p, "**", "*.cr")) }.sort
total = 0
reports = [] of {String, Int32}
timeouts = [] of String

files.each do |file|
  src = File.read(file)
  lex = CrystalV2::Compiler::Frontend::Lexer.new(src)
  parser = CrystalV2::Compiler::Frontend::Parser.new(lex, recovery_mode: true)

  if timeout_span
    CrystalV2::Compiler::Frontend::Watchdog.enable!("Timeout while scanning #{file}", timeout_span)
  end

  begin
    parser.parse_program
    errs = parser.diagnostics.size
    total += errs
    reports << {file, errs} if errs > 0
  rescue CrystalV2::Compiler::Frontend::Watchdog::TimeoutError
    timeouts << file
  ensure
    CrystalV2::Compiler::Frontend::Watchdog.disable! if timeout_span
  end
end

reports.sort_by! { |_, e| -e }
puts "Files with diagnostics: #{reports.size}, total: #{total}"
reports.first(30).each do |path, errs|
  puts "#{errs}\t#{path}"
end

unless timeouts.empty?
  puts "Timed out while scanning #{timeouts.size} file(s):"
  timeouts.each { |file| puts "TIMEOUT\t#{file}" }
end
