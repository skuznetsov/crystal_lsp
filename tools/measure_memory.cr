require "../src/compiler/frontend/lexer"
require "../src/compiler/frontend/parser"
require "../src/compiler/file_loader"

def memory_mb : Float64
  GC.stats.heap_size.to_f / (1024 * 1024)
end

# Measure prelude parsing with v2
PRELUDE = "/opt/homebrew/Cellar/crystal/1.18.2/share/crystal/src/prelude.cr"

puts "=== Memory Usage: V2 Arena Parser ==="
puts

GC.collect
before = memory_mb
puts "Before parsing: #{before.round(2)} MB"

loader = CrystalV2::Compiler::FileLoader.new
program = loader.load_with_requires(PRELUDE)

GC.collect
after = memory_mb
puts "After parsing:  #{after.round(2)} MB"
puts "Delta:          #{(after - before).round(2)} MB"
puts
puts "Files loaded:   #{loader.stats[:files_loaded]}"
puts "Total nodes:    #{loader.stats[:total_nodes]}"
puts
puts "Bytes per node: #{((after - before) * 1024 * 1024 / loader.stats[:total_nodes]).round(1)}"
