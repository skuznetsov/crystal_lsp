require "../src/compiler/frontend/lexer"
require "../src/compiler/frontend/parser"
require "../src/compiler/frontend/watchdog"

paths = (ENV["CRYSTAL_PATH"]? || "").split(":")
files = paths.flat_map { |p| Dir.glob(File.join(p, "**", "*.cr")) }.sort
total = 0
reports = [] of {String, Int32}

files.each do |file|
  src = File.read(file)
  lex = CrystalV2::Compiler::Frontend::Lexer.new(src)
  parser = CrystalV2::Compiler::Frontend::Parser.new(lex, recovery_mode: true)
  parser.parse_program
  errs = parser.diagnostics.size
  total += errs
  reports << {file, errs} if errs > 0
end

reports.sort_by! { |_, e| -e }
puts "Files with diagnostics: #{reports.size}, total: #{total}"
reports.first(30).each do |path, errs|
  puts "#{errs}\t#{path}"
end
