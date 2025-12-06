require "../src/compiler/frontend/lexer"
require "../src/compiler/frontend/parser"

STDLIB_PATH = "/opt/homebrew/Cellar/crystal/1.18.2/share/crystal/src"

# Find all .cr files in stdlib
files = Dir.glob("#{STDLIB_PATH}/**/*.cr")
puts "Found #{files.size} .cr files in stdlib"

total_errors = 0
files_with_errors = [] of String

files.each_with_index do |path, idx|
  begin
    source = File.read(path)
    lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
    parser = CrystalV2::Compiler::Frontend::Parser.new(lexer, recovery_mode: true)
    parser.parse_program

    if parser.diagnostics.size > 0
      total_errors += parser.diagnostics.size
      files_with_errors << "#{path.sub(STDLIB_PATH + "/", "")}: #{parser.diagnostics.size}"
      parser.diagnostics.first(3).each do |d|
        files_with_errors << "  - #{d.message} @ #{d.span.start_line+1}:#{d.span.start_column+1}"
      end
    end
  rescue ex
    files_with_errors << "#{path.sub(STDLIB_PATH + "/", "")}: EXCEPTION: #{ex.message}"
  end

  # Progress indicator
  print "\r#{idx + 1}/#{files.size}" if (idx + 1) % 50 == 0 || idx == files.size - 1
end

puts "\n\n=== Results ==="
puts "Total files: #{files.size}"
puts "Files with errors: #{files_with_errors.select { |l| !l.starts_with?("  ") }.size}"
puts "Total diagnostics: #{total_errors}"
puts ""
files_with_errors.each { |l| puts l }
