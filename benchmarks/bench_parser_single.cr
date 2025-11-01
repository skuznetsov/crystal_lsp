require "../src/compiler/frontend/lexer"
require "../src/compiler/frontend/parser"

include CrystalV2::Compiler

PARSER_FILE = "/Users/sergey/Projects/Crystal/crystal/crystal_v2/src/compiler/frontend/parser.cr"

puts "=== Single File Parser Benchmark (Release) ==="
puts "File: parser.cr"
puts ""

content = File.read(PARSER_FILE)

# Warm-up
3.times do
  lexer = Lexer.new(content)
  parser = Parser.new(lexer)
  parser.parse_program
end

# Actual benchmark
times = [] of Float64
program = nil
10.times do
  start = Time.monotonic
  lexer = Lexer.new(content)
  parser = Parser.new(lexer)
  program = parser.parse_program
  time = (Time.monotonic - start).total_milliseconds
  times << time
end

avg = times.sum / times.size
min = times.min
max = times.max

puts "Results (10 runs):"
puts "  Average: #{avg.round(2)} ms"
puts "  Min:     #{min.round(2)} ms"
puts "  Max:     #{max.round(2)} ms"
puts "  Nodes:   #{program.not_nil!.arena.size}" if program
