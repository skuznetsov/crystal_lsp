require "../src/main"

# Read file
parser_path = "/Users/sergey/Projects/Crystal/crystal/src/compiler/crystal/syntax/parser.cr"
source = File.read(parser_path)

puts "File: parser.cr"
puts "Size: #{source.bytesize} bytes (#{source.lines.size} lines)"
puts ""

# Benchmark CrystalV2 formatter (10 runs)
puts "=== CrystalV2 Token-based Formatter ==="
times = [] of Float64
10.times do |i|
  start = Time.monotonic
  formatted = CrystalV2::Compiler::Formatter.format(source)
  elapsed = Time.monotonic - start
  times << elapsed.total_milliseconds
  print "." if i % 2 == 0
end
puts ""

avg = times.sum / times.size
min = times.min
max = times.max
puts "Average: #{avg.round(2)} ms"
puts "Min: #{min.round(2)} ms"
puts "Max: #{max.round(2)} ms"
