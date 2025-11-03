require "../src/compiler/frontend/lexer"
require "../src/compiler/frontend/parser"

# Test various property declarations with complex types
test_cases = [
  "property x : Int32 = 42",
  "property x : String?",
  "property x : Hash(Int32, String)?",
  "property x : Hash(Int32, String)? = nil",
  "property items : Array(String) = [] of String",
  "property ptr : Int32*",
  "property data : UInt8**",
]

test_cases.each do |source|
  puts "\nTesting: #{source.inspect}"

  lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
  parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
  program = parser.parse_program

  if parser.diagnostics.empty?
    puts "  ✓ Success (0 diagnostics)"
  else
    puts "  ✗ Failed (#{parser.diagnostics.size} diagnostics):"
    parser.diagnostics.each { |d| puts "    - #{d.message}" }
  end
end
