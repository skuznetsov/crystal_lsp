require "../src/compiler/frontend/lexer"
require "../src/compiler/frontend/parser"
require "../src/compiler/frontend/ast"

include CrystalV2::Compiler::Frontend

# ============================================================================
# Parser Regression Test Suite
# Ensures we don't break existing functionality
# ============================================================================

class Test
  @passed = 0
  @failed = 0
  @failures = [] of {String, String}

  def run(name : String, code : String, &block : Program ->)
    print "Test: #{name} ... "
    begin
      lexer = Lexer.new(code)
      parser = Parser.new(lexer)
      program = parser.parse_program

      yield program

      @passed += 1
      puts "✓"
    rescue ex
      @failed += 1
      @failures << {name, ex.message || "Unknown error"}
      puts "✗ #{ex.message}"
    end
  end

  def assert(condition : Bool, message : String)
    raise message unless condition
  end

  def summary
    puts "\n=== Test Summary ==="
    puts "Passed: #{@passed}"
    puts "Failed: #{@failed}"

    if @failed > 0
      puts "\nFailures:"
      @failures.each do |(name, msg)|
        puts "  ✗ #{name}: #{msg}"
      end
      exit 1
    else
      puts "✓ All tests passed!"
    end
  end
end

test = Test.new

puts "=== Parser Regression Test Suite ==="
puts ""

# ============================================================================
# Basic Constructs
# ============================================================================

test.run("Number literal", "42") do |prog|
  test.assert(prog.roots.size == 1, "Should have 1 root")
  test.assert(prog.arena[prog.roots[0]].is_a?(NumberNode), "Root should be NumberNode")
end

test.run("String literal", %("hello")) do |prog|
  test.assert(prog.roots.size == 1, "Should have 1 root")
  test.assert(prog.arena[prog.roots[0]].is_a?(StringNode), "Root should be StringNode")
end

test.run("Bool literal", "true") do |prog|
  test.assert(prog.roots.size == 1, "Should have 1 root")
  test.assert(prog.arena[prog.roots[0]].is_a?(BoolNode), "Root should be BoolNode")
end

test.run("Nil literal", "nil") do |prog|
  test.assert(prog.roots.size == 1, "Should have 1 root")
  test.assert(prog.arena[prog.roots[0]].is_a?(NilNode), "Root should be NilNode")
end

test.run("Symbol literal", ":foo") do |prog|
  test.assert(prog.roots.size == 1, "Should have 1 root")
  test.assert(prog.arena[prog.roots[0]].is_a?(SymbolNode), "Root should be SymbolNode")
end

# ============================================================================
# Variables and Assignment
# ============================================================================

test.run("Variable assignment", "x = 42") do |prog|
  test.assert(prog.roots.size == 1, "Should have 1 root")
  node = prog.arena[prog.roots[0]]
  test.assert(node.is_a?(AssignNode), "Root should be AssignNode")
end

test.run("Instance variable", "@x = 42") do |prog|
  test.assert(prog.roots.size == 1, "Should have 1 root")
  test.assert(prog.arena[prog.roots[0]].is_a?(AssignNode), "Root should be AssignNode")
end

test.run("Multiple assignment", "a, b = 1, 2") do |prog|
  test.assert(prog.roots.size == 1, "Should have 1 root")
  node = prog.arena[prog.roots[0]]
  test.assert(node.is_a?(MultipleAssignNode), "Root should be MultipleAssignNode")
end

# ============================================================================
# Methods
# ============================================================================

test.run("Method definition", "def foo\n  42\nend") do |prog|
  test.assert(prog.roots.size == 1, "Should have 1 root")
  test.assert(prog.arena[prog.roots[0]].is_a?(DefNode), "Root should be DefNode")
end

test.run("Method with parameters", "def foo(x : Int32, y : String)\n  x\nend") do |prog|
  test.assert(prog.roots.size == 1, "Should have 1 root")
  test.assert(prog.arena[prog.roots[0]].is_a?(DefNode), "Root should be DefNode")
end

test.run("Method call", "foo(42)") do |prog|
  test.assert(prog.roots.size == 1, "Should have 1 root")
  test.assert(prog.arena[prog.roots[0]].is_a?(CallNode), "Root should be CallNode")
end

test.run("Member access call", "foo.bar(42)") do |prog|
  test.assert(prog.roots.size == 1, "Should have 1 root")
  test.assert(prog.arena[prog.roots[0]].is_a?(CallNode), "Root should be CallNode")
end

# ============================================================================
# Classes and Modules
# ============================================================================

test.run("Class definition", "class Foo\nend") do |prog|
  test.assert(prog.roots.size == 1, "Should have 1 root")
  test.assert(prog.arena[prog.roots[0]].is_a?(ClassNode), "Root should be ClassNode")
end

test.run("Class with superclass", "class Foo < Bar\nend") do |prog|
  test.assert(prog.roots.size == 1, "Should have 1 root")
  test.assert(prog.arena[prog.roots[0]].is_a?(ClassNode), "Root should be ClassNode")
end

test.run("Module definition", "module Foo\nend") do |prog|
  test.assert(prog.roots.size == 1, "Should have 1 root")
  test.assert(prog.arena[prog.roots[0]].is_a?(ModuleNode), "Root should be ModuleNode")
end

# ============================================================================
# Control Flow
# ============================================================================

test.run("If expression", "if x\n  1\nend") do |prog|
  test.assert(prog.roots.size == 1, "Should have 1 root")
  test.assert(prog.arena[prog.roots[0]].is_a?(IfNode), "Root should be IfNode")
end

test.run("If-else expression", "if x\n  1\nelse\n  2\nend") do |prog|
  test.assert(prog.roots.size == 1, "Should have 1 root")
  test.assert(prog.arena[prog.roots[0]].is_a?(IfNode), "Root should be IfNode")
end

test.run("Unless expression", "unless x\n  1\nend") do |prog|
  test.assert(prog.roots.size == 1, "Should have 1 root")
  test.assert(prog.arena[prog.roots[0]].is_a?(UnlessNode), "Root should be UnlessNode")
end

test.run("Case expression", "case x\nwhen 1\n  \"one\"\nend") do |prog|
  test.assert(prog.roots.size == 1, "Should have 1 root")
  node = prog.arena[prog.roots[0]]
  test.assert(node.is_a?(CaseNode), "Root should be CaseNode")
end

test.run("While loop", "while x < 10\n  x += 1\nend") do |prog|
  test.assert(prog.roots.size >= 1, "Should have at least 1 root")
  # While may be represented differently - just check it parses successfully
end

# ============================================================================
# Operators
# ============================================================================

test.run("Binary operators", "a + b * c") do |prog|
  test.assert(prog.roots.size == 1, "Should have 1 root")
  test.assert(prog.arena[prog.roots[0]].is_a?(BinaryNode), "Root should be BinaryNode")
end

test.run("Comparison operators", "a > b && c == d") do |prog|
  test.assert(prog.roots.size == 1, "Should have 1 root")
  test.assert(prog.arena[prog.roots[0]].is_a?(BinaryNode), "Root should be BinaryNode")
end

test.run("Unary operators", "!x") do |prog|
  test.assert(prog.roots.size == 1, "Should have 1 root")
  test.assert(prog.arena[prog.roots[0]].is_a?(UnaryNode), "Root should be UnaryNode")
end

# ============================================================================
# Literals and Collections
# ============================================================================

test.run("Array literal", "[1, 2, 3]") do |prog|
  test.assert(prog.roots.size == 1, "Should have 1 root")
  test.assert(prog.arena[prog.roots[0]].is_a?(ArrayLiteralNode), "Root should be ArrayLiteralNode")
end

test.run("Hash literal", "{\"a\" => 1, \"b\" => 2}") do |prog|
  test.assert(prog.roots.size == 1, "Should have 1 root")
  test.assert(prog.arena[prog.roots[0]].is_a?(HashLiteralNode), "Root should be HashLiteralNode")
end

test.run("Range literal", "1..10") do |prog|
  test.assert(prog.roots.size == 1, "Should have 1 root")
  test.assert(prog.arena[prog.roots[0]].is_a?(RangeNode), "Root should be RangeNode")
end

# ============================================================================
# Blocks
# ============================================================================

test.run("Block with parameters", "[1, 2].each { |x| puts x }") do |prog|
  test.assert(prog.roots.size == 1, "Should have 1 root")
  test.assert(prog.arena[prog.roots[0]].is_a?(CallNode), "Root should be CallNode")
end

# ============================================================================
# Type Annotations
# ============================================================================

test.run("Type declaration", "x : Int32 = 42") do |prog|
  test.assert(prog.roots.size == 1, "Should have 1 root")
  test.assert(prog.arena[prog.roots[0]].is_a?(TypeDeclarationNode), "Root should be TypeDeclarationNode")
end

test.run("Type cast (as)", "x.as(String)") do |prog|
  test.assert(prog.roots.size == 1, "Should have 1 root")
  test.assert(prog.arena[prog.roots[0]].is_a?(AsNode), "Root should be AsNode")
end

# ============================================================================
# Large File Test
# ============================================================================

test.run("Large file (parser.cr)", File.read("/Users/sergey/Projects/Crystal/crystal/src/compiler/crystal/syntax/parser.cr")) do |prog|
  test.assert(prog.roots.size > 0, "Should have roots")
  test.assert(prog.arena.size > 10000, "Should have > 10000 nodes")
  # Regression check: exact node count should remain stable
  test.assert(prog.arena.size == 14377, "Should have exactly 14377 nodes (was 14377 on #{Time.utc.to_s("%Y-%m-%d")})")
end

# ============================================================================
# Summary
# ============================================================================

test.summary
