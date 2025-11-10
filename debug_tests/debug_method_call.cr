require "../src/compiler/frontend/parser"
require "../src/compiler/frontend/lexer"
require "../src/compiler/semantic/semantic_analyzer"

source = <<-CRYSTAL
class Calculator
  def add(x : Int32, y : Int32) : Int32
    x + y
  end
end

calc = Calculator.new
result = calc.add(1, 2)
CRYSTAL

lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
program = parser.parse_program

puts "=== PARSING ==="
puts "Roots: #{program.roots.size}"
program.roots.each_with_index do |root_id, i|
  node = program.arena[root_id]
  puts "Root #{i}: #{node.class.name.split("::").last}"
end

puts "\n=== SEMANTIC ANALYSIS ==="
analyzer = CrystalV2::Compiler::Semantic::SemanticAnalyzer.new(program)
engine = analyzer.run

puts "Diagnostics: #{engine.diagnostics.size}"

# Check types
puts "\n=== TYPES ==="
program.roots.each_with_index do |root_id, i|
  type = engine.context.get_type(root_id)
  node = program.arena[root_id]
  puts "Root #{i} (#{node.class.name.split("::").last}): #{type ? type.to_s : "nil"}"
end
