require "../src/compiler/frontend/lexer"
require "../src/compiler/frontend/parser"
require "../src/compiler/frontend/watchdog"

abort "Usage: show_diagnostics <file> [limit]" unless ARGV.size >= 1
path = ARGV[0]
limit = (ARGV[1]? || "50").to_i
source = File.read(path)
lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)

# Optional streaming mode for large files to avoid pre-tokenization stalls
begin
  # Use streaming to avoid pre-tokenization stalls on large files
  ENV["CRYSTAL_V2_PARSER_STREAM"] = "1" if ENV["CRYSTAL_V2_PARSER_STREAM"].nil?
rescue KeyError
  # Some stdlib Env impls require rescue for writes; ignore
end

parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)

begin
  # Optional watchdog to prevent hangs during large-file diagnostics.
  # Enable with environment: ENABLE_WATCHDOG=1 SHOW_TIMEOUT=seconds
  # Default timeout is 0.1 seconds when ENABLE_WATCHDOG is set without SHOW_TIMEOUT.
  if ENV["ENABLE_WATCHDOG"]? || ENV["SHOW_TIMEOUT"]?
    timeout_s = (ENV["SHOW_TIMEOUT"]? || "0.1").to_f
    CrystalV2::Compiler::Frontend::Watchdog.enable!("show_diagnostics timeout", timeout_s.seconds)
  end

  parser.parse_program
rescue ex : CrystalV2::Compiler::Frontend::Watchdog::TimeoutError
  STDERR.puts "Watchdog timeout while parsing #{path}: #{ex.message}"
  # Best-effort hint: current implementation does not expose full parser
  # internals here, but the stack trace together with this message should
  # help identify the hotspot in parser methods.
  STDERR.puts ex.backtrace?.try(&.join('\n'))
  exit 1
ensure
  CrystalV2::Compiler::Frontend::Watchdog.disable!
end

puts "Diagnostics: #{parser.diagnostics.size}"
parser.diagnostics.first(limit).each_with_index do |d, i|
  puts "%3d %s @ %d:%d" % [i+1, d.message, d.span.start_line+1, d.span.start_column+1]
end
