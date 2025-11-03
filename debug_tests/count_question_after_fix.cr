require "../src/compiler/frontend/lexer"
require "../src/compiler/frontend/parser"

# Count "unexpected Question" errors after fix
count = 0

Dir.glob("/Users/sergey/Projects/Crystal/crystal/src/**/*.cr") do |file|
  source = File.read(file)
  
  lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
  parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
  program = parser.parse_program
  
  question_errors = parser.diagnostics.select { |d| d.message.includes?("unexpected Question") }
  count += question_errors.size
end

puts "Total 'unexpected Question' errors: #{count}"
