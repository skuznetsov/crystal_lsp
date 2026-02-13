# Test abstract classes, virtual dispatch, and visitor pattern
# EXPECT: done
abstract class Node
  abstract def to_s : String
end

class NumberNode < Node
  getter value : Int32

  def initialize(@value : Int32)
  end

  def to_s : String
    @value.to_s
  end
end

class BinOpNode < Node
  getter op : String
  getter left : Node
  getter right : Node

  def initialize(@op : String, @left : Node, @right : Node)
  end

  def to_s : String
    "(#{@left.to_s} #{@op} #{@right.to_s})"
  end
end

class UnaryNode < Node
  getter op : String
  getter operand : Node

  def initialize(@op : String, @operand : Node)
  end

  def to_s : String
    "(#{@op}#{@operand.to_s})"
  end
end

# Build: -(1 + 2)
one = NumberNode.new(1)
two = NumberNode.new(2)
add = BinOpNode.new("+", one, two)
neg = UnaryNode.new("-", add)

puts one.to_s
puts add.to_s
puts neg.to_s

# Test is_a?
nodes = [one, add, neg] of Node
nodes.each do |n|
  if n.is_a?(NumberNode)
    puts "num:#{n.value}"
  elsif n.is_a?(BinOpNode)
    puts "binop:#{n.op}"
  elsif n.is_a?(UnaryNode)
    puts "unary:#{n.op}"
  end
end

puts "done"
