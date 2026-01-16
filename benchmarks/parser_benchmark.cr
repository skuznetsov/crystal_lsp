require "./src/compiler/frontend/lexer"
require "./src/compiler/frontend/parser"
require "./src/compiler/frontend/ast"

include CrystalV2::Compiler::Frontend

# Benchmark parsing multiple files
files = ARGV

if files.empty?
  puts "Usage: crystal run parser_benchmark.cr file1.cr file2.cr ..."
  exit 1
end

total_files = 0
total_lines = 0
total_nodes = 0
failed_files = 0
start_time = Time.instant

files.each do |file_path|
  next unless File.exists?(file_path)

  source = File.read(file_path)
  lines = source.lines.size

  # Parse
  begin
    lexer = Lexer.new(source)
    parser = Parser.new(lexer)
    program = parser.parse_program

    total_files += 1
    total_lines += lines
    total_nodes += program.roots.size

    puts "✓ #{file_path} (#{lines} lines, #{program.roots.size} root nodes)"
  rescue ex
    failed_files += 1
    puts "✗ #{file_path}: #{ex.class.name}: #{ex.message}"
  end
end

end_time = Time.instant
elapsed = (end_time - start_time).total_seconds

puts
puts "=" * 70
puts "CRYSTAL V2 PARSER BENCHMARK"
puts "=" * 70
puts "Files attempted:  #{files.size}"
puts "Files parsed:     #{total_files}"
puts "Files failed:     #{failed_files}"
puts "Total lines:      #{total_lines}"
puts "Total root nodes: #{total_nodes}"
puts "Time:             #{elapsed.round(3)}s"

if elapsed > 0
  puts "Speed:            #{(total_lines / elapsed).round(0)} lines/sec"
  puts "Files/sec:        #{(total_files / elapsed).round(2)}"
end

puts "=" * 70
