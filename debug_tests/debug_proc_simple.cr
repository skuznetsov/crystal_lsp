require "../src/compiler/frontend/parser"

# Simple single-line test
source = "->(x : Int32) { x + 1 }"

parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
program = parser.parse_program

puts "Roots: #{program.roots.size}"
puts "Diagnostics: #{parser.diagnostics.size}"
parser.diagnostics.each { |d| puts "  #{d.message}" }

if program.roots.size > 0
  arena = program.arena
  root = arena[program.roots[0]]
  puts "Root node kind: #{CrystalV2::Compiler::Frontend.node_kind(root)}"

  if CrystalV2::Compiler::Frontend.node_kind(root) == CrystalV2::Compiler::Frontend::NodeKind::ProcLiteral
    proc_node = root.as(CrystalV2::Compiler::Frontend::ProcLiteralNode)
    puts "Body size: #{proc_node.body ? proc_node.body.not_nil!.size : 0}"
  end
end
