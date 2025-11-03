require "../src/compiler/frontend/lexer"
require "../src/compiler/frontend/parser"

# Check original problematic files (from first analysis)
files = [
  "/Users/sergey/Projects/Crystal/crystal/src/oauth/access_token.cr",
  "/Users/sergey/Projects/Crystal/crystal/src/oauth/signature.cr",
  "/Users/sergey/Projects/Crystal/crystal/src/oauth/oauth.cr",
  "/Users/sergey/Projects/Crystal/crystal/src/oauth/consumer.cr"
]

total_question = 0

files.each do |file|
  source = File.read(file)
  
  lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
  parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
  program = parser.parse_program
  
  question_errors = parser.diagnostics.select { |d| d.message.includes?("unexpected Question") }
  
  if question_errors.size > 0
    puts "#{file}: #{question_errors.size} errors"
    total_question += question_errors.size
  else
    puts "#{file}: ✓ OK"
  end
end

puts "\nTotal 'unexpected Question' in original problematic files: #{total_question}"
