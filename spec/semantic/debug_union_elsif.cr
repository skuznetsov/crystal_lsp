require "./spec_helper"

source = <<-CRYSTAL
  if true
    42
  elsif false
    "hello"
  end
CRYSTAL

lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
tokens = lexer.tokenize

parser = CrystalV2::Compiler::Frontend::Parser.new(tokens)
program = parser.parse_program

analyzer = CrystalV2::Compiler::Semantic::SemanticAnalyzer.new(program)
analyzer.analyze

engine = analyzer.type_inference_engine

root_id = program.roots[0]
type = engine.context.get_type(root_id)

puts "Type: #{type.class}"
puts "Type: #{type.inspect}"

if type.is_a?(CrystalV2::Compiler::Semantic::UnionType)
  union = type.as(CrystalV2::Compiler::Semantic::UnionType)
  puts "Union size: #{union.types.size}"
  union.types.each_with_index do |t, i|
    puts "  [#{i}]: #{t}"
  end
else
  puts "NOT a union type!"
end
