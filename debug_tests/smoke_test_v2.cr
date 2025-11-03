require "../src/compiler/frontend/lexer"
require "../src/compiler/frontend/parser"

# Smoke test for recent fixes (no semicolons)
tests = [
  # Keywords as property names
  "property else : String",
  "property if : Bool", 
  "property when : Int32",
  
  # Type suffixes - return type
  "def foo(x : String?) : Int32?
    1
  end",
  
  # Type suffixes - pointer
  "def bar(ptr : Int32*)
    ptr
  end",
  
  # Type suffixes - static array
  "def baz(arr : Int32[10])
    arr
  end",
  
  # def as identifier  
  "getter def : String",
  "property def : String",
  "foo(def: 42)",
  
  # Real-world patterns
  "def self.encode(d, len : Int32) : String
    \"\"
  end",
  
  # From oauth (original issue)
  "class Foo
    def initialize(@token : String, @secret : String, @extra : Hash(String, String)? = nil)
    end
  end",
]

passed = 0
failed = 0

tests.each_with_index do |source, i|
  lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
  parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
  program = parser.parse_program
  
  if parser.diagnostics.size == 0
    passed += 1
    puts "✓ Test #{i + 1}"
  else
    failed += 1
    puts "✗ Test #{i + 1}"
    puts "  Errors: #{parser.diagnostics.map(&.message).join(", ")}"
  end
end

puts "\n" + "="*80
puts "Smoke test: #{passed}/#{tests.size} passed (#{(passed * 100 / tests.size)}%)"
exit(failed == 0 ? 0 : 1)
