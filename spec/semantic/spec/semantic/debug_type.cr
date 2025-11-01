require "../spec_helper"

# Minimal reproduction of type inference test

source = "1 + 2"

lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
tokens = lexer.tokenize

parser = CrystalV2::Compiler::Frontend::Parser.new(tokens)
program = parser.parse_program

analyzer = CrystalV2::Compiler::Semantic::SemanticAnalyzer.new(program)
analyzer.analyze

engine = analyzer.type_inference_engine

# Get the root expression (the binary + operation)
root_id = program.roots[0]
type = engine.context.get_type(root_id)

puts "Root ID: #{root_id}"
puts "Type: #{type.inspect}"
if type.is_a?(CrystalV2::Compiler::Semantic::PrimitiveType)
  puts "PrimitiveType name: #{type.name}"
else
  puts "Type class: #{type.class}"
end

# Let's also check what the binary node looks like
binary_node = program.arena[root_id]
puts "\nBinary node operator: #{CrystalV2::Compiler::Frontend.node_operator_string(binary_node)}"

left_id = CrystalV2::Compiler::Frontend.node_left(binary_node)
right_id = CrystalV2::Compiler::Frontend.node_right(binary_node)

puts "Left ID: #{left_id}"
puts "Right ID: #{right_id}"

if left_id
  left_type = engine.context.get_type(left_id)
  puts "Left type: #{left_type.inspect}"
  if left_type.is_a?(CrystalV2::Compiler::Semantic::PrimitiveType)
    puts "Left PrimitiveType name: #{left_type.name}"
  end
end

if right_id
  right_type = engine.context.get_type(right_id)
  puts "Right type: #{right_type.inspect}"
  if right_type.is_a?(CrystalV2::Compiler::Semantic::PrimitiveType)
    puts "Right PrimitiveType name: #{right_type.name}"
  end
end
