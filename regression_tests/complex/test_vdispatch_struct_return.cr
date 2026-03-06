# EXPECT: vdispatch_struct_ok
# Tests virtual dispatch through abstract class hierarchy where
# the abstract method returns a STRUCT (not a primitive or String).
# This is the exact pattern that crashes stage1: abstract Node#span returns Span struct.

struct Span
  @start : Int32
  @stop : Int32

  def initialize(@start, @stop)
  end

  def start
    @start
  end

  def stop
    @stop
  end

  def cover(other : Span) : Span
    s = start <= other.start ? self : other
    e = stop >= other.stop ? self : other
    Span.new(s.start, e.stop)
  end
end

abstract class Node
  abstract def span : Span
end

class LiteralNode < Node
  @span : Span
  @value : Int32

  def initialize(@span, @value)
  end

  def span
    @span
  end
end

class BinaryNode < Node
  @span : Span
  @left : Int32
  @right : Int32
  @op : String

  def initialize(@span, @left, @right, @op)
  end

  def span
    @span
  end
end

class CallNode < Node
  @span : Span
  @name : String
  @args : Array(Int32)

  def initialize(@span, @name, @args)
  end

  def span
    @span
  end
end

class IfNode < Node
  @span : Span
  @cond : Int32
  @then_body : Int32
  @else_body : Int32

  def initialize(@span, @cond, @then_body, @else_body)
  end

  def span
    @span
  end
end

class StringNode < Node
  @span : Span
  @value : String

  def initialize(@span, @value)
  end

  def span
    @span
  end
end

# Store in array (like AstArena)
nodes = Array(Node).new
nodes << LiteralNode.new(Span.new(0, 5), 42)
nodes << BinaryNode.new(Span.new(6, 15), 0, 1, "+")
nodes << CallNode.new(Span.new(16, 30), "foo", [0, 1])
nodes << IfNode.new(Span.new(31, 50), 2, 0, 1)
nodes << StringNode.new(Span.new(51, 60), "hello")

# Access struct field through virtual dispatch
ok = true
expected_starts = [0, 6, 16, 31, 51]
expected_stops = [5, 15, 30, 50, 60]

nodes.each_with_index do |node, i|
  s = node.span
  if s.start != expected_starts[i]
    puts "FAIL: node #{i} start=#{s.start} expected=#{expected_starts[i]}"
    ok = false
  end
  if s.stop != expected_stops[i]
    puts "FAIL: node #{i} stop=#{s.stop} expected=#{expected_stops[i]}"
    ok = false
  end
end

# Cover pattern through virtual dispatch (like parse_index does)
acc = nodes[0].span
nodes.each do |node|
  acc = acc.cover(node.span)
end

if acc.start != 0
  puts "FAIL: cover start=#{acc.start} expected=0"
  ok = false
end
if acc.stop != 60
  puts "FAIL: cover stop=#{acc.stop} expected=60"
  ok = false
end

# Access by index with guard (like node_span)
def get_span(nodes : Array(Node), id : Int32) : Span
  return Span.new(0, 0) if id < 0
  nodes[id].span
end

s = get_span(nodes, 0).cover(get_span(nodes, 4))
if s.start != 0 || s.stop != 60
  puts "FAIL: indexed cover start=#{s.start} stop=#{s.stop}"
  ok = false
end

s2 = get_span(nodes, -1).cover(get_span(nodes, 2))
if s2.start != 0 || s2.stop != 30
  puts "FAIL: invalid cover start=#{s2.start} stop=#{s2.stop}"
  ok = false
end

puts ok ? "vdispatch_struct_ok" : "vdispatch_struct_FAIL"
