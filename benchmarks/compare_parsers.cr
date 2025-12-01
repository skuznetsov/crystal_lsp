# Benchmark: Compare v2 parser vs original Crystal compiler
#
# Tests parsing/semantic time on:
# 1. Prelude (stdlib)
# 2. Original Crystal compiler
# 3. Our lsp_main.cr

require "../src/compiler/frontend/lexer"
require "../src/compiler/frontend/parser"
require "../src/compiler/file_loader"

PRELUDE_PATH     = "/opt/homebrew/Cellar/crystal/1.18.2/share/crystal/src/prelude.cr"
COMPILER_PATH    = "/opt/homebrew/Cellar/crystal/1.18.2/share/crystal/src/compiler/crystal.cr"
LSP_MAIN_PATH    = "#{__DIR__}/../src/lsp_main.cr"

def format_duration(duration : Time::Span) : String
  ms = duration.total_milliseconds
  if ms < 1000
    "#{ms.round(1)}ms"
  else
    "#{(ms / 1000).round(2)}s"
  end
end

def benchmark_v2_single_file(path : String) : {Time::Span, Int32, Int32}
  source = File.read(path)

  start = Time.monotonic
  lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
  parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
  program = parser.parse_program
  duration = Time.monotonic - start

  {duration, program.roots.size, parser.diagnostics.size}
end

def benchmark_v2_multi_file(entry_path : String) : {Time::Span, Int32, Int32}
  start = Time.monotonic
  loader = CrystalV2::Compiler::FileLoader.new
  program = loader.load_with_requires(entry_path)
  duration = Time.monotonic - start

  stats = loader.stats
  {duration, stats[:files_loaded], stats[:total_nodes]}
end

def benchmark_original_crystal(path : String) : {Time::Span, String}
  start = Time.monotonic
  output = `crystal build --no-codegen "#{path}" 2>&1`
  duration = Time.monotonic - start

  {duration, output}
end

puts "=" * 70
puts "Parser Benchmark: V2 vs Original Crystal Compiler"
puts "=" * 70
puts

# Test 1: Single file parsing (lsp_main.cr)
puts "## Test 1: Single File - lsp_main.cr"
puts "-" * 50

if File.exists?(LSP_MAIN_PATH)
  v2_duration, roots, errors = benchmark_v2_single_file(LSP_MAIN_PATH)
  puts "V2 Parser:      #{format_duration(v2_duration)} (#{roots} roots, #{errors} errors)"

  orig_duration, output = benchmark_original_crystal(LSP_MAIN_PATH)
  status = output.includes?("Error") ? "ERRORS" : "OK"
  puts "Original:       #{format_duration(orig_duration)} (#{status})"

  if orig_duration.total_milliseconds > 0
    speedup = orig_duration.total_milliseconds / v2_duration.total_milliseconds
    puts "V2 Speedup:     #{speedup.round(1)}x faster (parsing only vs full semantic)"
  end
else
  puts "File not found: #{LSP_MAIN_PATH}"
end
puts

# Test 2: Multi-file project (prelude)
puts "## Test 2: Multi-File Project - Prelude"
puts "-" * 50

if File.exists?(PRELUDE_PATH)
  v2_duration, files, nodes = benchmark_v2_multi_file(PRELUDE_PATH)
  puts "V2 Parser:      #{format_duration(v2_duration)} (#{files} files, ~#{nodes} nodes)"

  orig_duration, output = benchmark_original_crystal(PRELUDE_PATH)
  status = output.includes?("Error") ? "ERRORS" : "OK"
  puts "Original:       #{format_duration(orig_duration)} (#{status})"

  if orig_duration.total_milliseconds > 0
    speedup = orig_duration.total_milliseconds / v2_duration.total_milliseconds
    puts "V2 Speedup:     #{speedup.round(1)}x faster"
  end
else
  puts "File not found: #{PRELUDE_PATH}"
end
puts

# Test 3: Multi-file project (compiler.cr)
puts "## Test 3: Multi-File Project - Crystal Compiler"
puts "-" * 50

if File.exists?(COMPILER_PATH)
  v2_duration, files, nodes = benchmark_v2_multi_file(COMPILER_PATH)
  puts "V2 Parser:      #{format_duration(v2_duration)} (#{files} files, ~#{nodes} nodes)"

  orig_duration, output = benchmark_original_crystal(COMPILER_PATH)
  status = output.includes?("Error") ? "ERRORS" : "OK"
  puts "Original:       #{format_duration(orig_duration)} (#{status})"

  if orig_duration.total_milliseconds > 0
    speedup = orig_duration.total_milliseconds / v2_duration.total_milliseconds
    puts "V2 Speedup:     #{speedup.round(1)}x faster"
  end
else
  puts "File not found: #{COMPILER_PATH}"
end
puts

# Test 4: Multi-file project (our lsp_main.cr with all deps)
puts "## Test 4: Multi-File Project - V2 LSP (lsp_main.cr)"
puts "-" * 50

if File.exists?(LSP_MAIN_PATH)
  v2_duration, files, nodes = benchmark_v2_multi_file(LSP_MAIN_PATH)
  puts "V2 Parser:      #{format_duration(v2_duration)} (#{files} files, ~#{nodes} nodes)"

  orig_duration, output = benchmark_original_crystal(LSP_MAIN_PATH)
  status = output.includes?("Error") ? "ERRORS" : "OK"
  puts "Original:       #{format_duration(orig_duration)} (#{status})"

  if orig_duration.total_milliseconds > 0
    speedup = orig_duration.total_milliseconds / v2_duration.total_milliseconds
    puts "V2 Speedup:     #{speedup.round(1)}x faster"
  end
else
  puts "File not found: #{LSP_MAIN_PATH}"
end
puts

puts "=" * 70
puts "Notes:"
puts "- V2 Parser: parsing only (no semantic analysis)"
puts "- Original: parsing + full semantic analysis (--no-codegen)"
puts "- Fair comparison requires V2 semantic to be complete"
puts "=" * 70
