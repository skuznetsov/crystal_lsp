require "../src/compiler/frontend/parser"

source = <<-CRYSTAL
->(x : Int32) {
  y = x + 1
  y * 2
}
CRYSTAL

parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
program = parser.parse_program

puts "Roots: #{program.roots.size}"
puts "Diagnostics: #{parser.diagnostics.size}"
parser.diagnostics.each { |d| puts "  #{d.message}" }

if program.roots.size > 0
  arena = program.arena
  program.roots.each_with_index do |root_id, idx|
    root = arena[root_id]
    puts "Root #{idx} kind: #{CrystalV2::Compiler::Frontend.node_kind(root)}"
    if CrystalV2::Compiler::Frontend.node_kind(root) == CrystalV2::Compiler::Frontend::NodeKind::ProcLiteral
      body = root.as(CrystalV2::Compiler::Frontend::ProcLiteralNode).body
      puts "  Body size: #{body ? body.size : 0}"
    end
  end
end
