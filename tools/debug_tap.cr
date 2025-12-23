require "../src/compiler/frontend/parser"
require "../src/compiler/frontend/ast"

# Parse object.cr and check for yield in tap
parser = CrystalV2::Compiler::Frontend::Parser.new

# Parse object.cr  
arena, ast = parser.parse_file("src/stdlib/object.cr")

def contains_yield_rec?(arena, expr_ids : Array(CrystalV2::Compiler::Frontend::ExprId)) : Bool
  expr_ids.each do |id|
    node = arena[id]
    if node.is_a?(CrystalV2::Compiler::Frontend::YieldNode)
      return true
    end
    # Check children
    children = node.children_of(arena)
    return true if contains_yield_rec?(arena, children)
  end
  false
end

# Find tap method
ast.each do |node_id|
  node = arena[node_id]
  if node.is_a?(CrystalV2::Compiler::Frontend::ClassNode)
    class_name = String.new(node.name)
    next unless class_name == "Object"
    puts "Found class Object"
    if body = node.body
      body.each do |member_id|
        member = arena[member_id]
        if member.is_a?(CrystalV2::Compiler::Frontend::DefNode)
          method_name = String.new(member.name)
          next unless method_name == "tap"
          puts "  Found def tap"
          if body = member.body
            has_yield = contains_yield_rec?(arena, body)
            puts "  Has yield: #{has_yield}"
          end
        end
      end
    end
  end
end
