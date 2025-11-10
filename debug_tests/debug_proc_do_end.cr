require "../src/compiler/frontend/parser"

source = <<-CRYSTAL
->(x : Int32) do
  x + 1
end
CRYSTAL

parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
program = parser.parse_program

puts "Roots: #{program.roots.size}"
puts "Diagnostics: #{parser.diagnostics.size}"
parser.diagnostics.each { |d| puts "  #{d.message}" }

if program.roots.size > 0
  arena = program.arena
  root = arena[program.roots[0]]
  puts "Root node kind: #{CrystalV2::Compiler::Frontend.node_kind(root)}"
end
