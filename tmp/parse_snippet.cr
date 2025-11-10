require "../src/compiler/frontend/lexer"
require "../src/compiler/frontend/parser"

snippets = {
  "if def_node.is_a?(Frontend::DefNode)\nend\n",
  "node.is_a?(Frontend::InstanceVarDeclNode)",
  "if node.is_a?(Frontend::InstanceVarDeclNode)\nend\n"
}

snippets.each do |src|
  lexer = CrystalV2::Compiler::Frontend::Lexer.new(src)
  parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
  program = parser.parse_program
  puts "Src: #{src.inspect}\n  diagnostics: #{parser.diagnostics.size}"
  parser.diagnostics.each do |diag|
    puts "    #{diag.message} at #{diag.span.start_line}:#{diag.span.start_column}"
  end
end
