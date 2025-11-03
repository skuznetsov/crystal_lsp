require "../src/compiler/frontend/lexer"
require "../src/compiler/frontend/parser"

# Smoke test for recent fixes
tests = [
  # Keywords as property names
  "property else : String",
  "property if : Bool", 
  "property when : Int32",
  
  # Type suffixes
  "def foo(x : String?) : Int32?; 1; end",
  "def bar(ptr : Int32*); end",
  "def baz(arr : Int32[10]); end",
  
  # def as identifier  
  "getter def : String",
  "property def : String",
  "foo(def: 42)",
  
  # Real-world patterns
  "def self.encode(d, len : Int32) : String; \"\"; end",
  "initialize(@token : String, @secret : String, @extra : Hash(String, String)? = nil)",
]

passed = 0
failed = 0

tests.each_with_index do |source, i|
  lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
  parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
  program = parser.parse_program
  
  if parser.diagnostics.size == 0
    passed += 1
    puts "✓ Test #{i + 1}: #{source[0...50]}"
  else
    failed += 1
    puts "✗ Test #{i + 1}: #{source[0...50]}"
    puts "  Errors: #{parser.diagnostics.map(&.message).join(", ")}"
  end
end

puts "\n" + "="*80
puts "Smoke test: #{passed}/#{tests.size} passed (#{(passed * 100 / tests.size)}%)"
puts "="*80
