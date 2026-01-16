require "../src/compiler/file_loader"

include CrystalV2::Compiler

PRELUDE_ENTRY = "/Users/sergey/Projects/Crystal/crystal/src/prelude.cr"

search_paths = [
  "/Users/sergey/Projects/Crystal/crystal/src",
  "/Users/sergey/Projects/Crystal/crystal/lib",
]

puts "=== Prelude.cr Benchmark (Release) ==="
puts ""

# Sequential
puts "Sequential loading..."
loader_seq = FileLoader.new(search_paths, parallel: false)
start = Time.instant
program_seq = loader_seq.load_with_requires(PRELUDE_ENTRY)
time_seq = (Time.instant - start).total_milliseconds
stats_seq = loader_seq.stats

puts "  Time:  #{time_seq.round(2)} ms"
puts "  Files: #{stats_seq[:files_loaded]}"
puts "  Nodes: #{stats_seq[:total_nodes]}"
puts ""

# Parallel
puts "Parallel loading..."
loader_par = FileLoader.new(search_paths, parallel: true)
start = Time.instant
program_par = loader_par.load_with_requires(PRELUDE_ENTRY)
time_par = (Time.instant - start).total_milliseconds
stats_par = loader_par.stats

puts "  Time:  #{time_par.round(2)} ms"
puts "  Files: #{stats_par[:files_loaded]}"
puts "  Nodes: #{stats_par[:total_nodes]}"
puts ""

puts "=== Summary ==="
puts "Sequential: #{time_seq.round(2)} ms"
puts "Parallel:   #{time_par.round(2)} ms"
puts "Speedup:    #{(time_seq / time_par).round(2)}x"
