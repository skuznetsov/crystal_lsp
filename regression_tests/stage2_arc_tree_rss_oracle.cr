@[Acyclic]
class TreeNode
  property value : Int32
  property left : TreeNode?
  property right : TreeNode?

  def initialize(@value : Int32, @left : TreeNode? = nil, @right : TreeNode? = nil)
  end
end

def make_tree(depth : Int32) : TreeNode
  if depth <= 0
    TreeNode.new(1)
  else
    TreeNode.new(depth, make_tree(depth - 1), make_tree(depth - 1))
  end
end

def check_tree(node : TreeNode?) : Int32
  return 0 if node.nil?
  node.value + check_tree(node.left) - check_tree(node.right)
end

result = 0_i32
i = 0
while i < 10
  tree = make_tree(18)
  result += check_tree(tree)
  i += 1
end

puts result
