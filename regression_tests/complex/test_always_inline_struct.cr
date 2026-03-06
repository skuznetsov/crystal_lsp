# EXPECT: inline_struct_ok
# Tests @[AlwaysInline] on methods that return structs with conditional branches.
# This is the exact pattern of AstArena#node_span: inline, guard check, virtual dispatch return.

struct Data
  @a : Int32
  @b : Int32
  @c : Int32

  def initialize(@a, @b, @c)
  end

  def a; @a; end
  def b; @b; end
  def c; @c; end
end

class Container
  @items : Array(Data)

  def initialize
    @items = Array(Data).new
  end

  def add(d : Data) : Int32
    @items << d
    @items.size - 1
  end

  @[AlwaysInline]
  def get(id : Int32) : Data
    return Data.new(0, 0, 0) if id < 0
    @items[id]
  end

  @[AlwaysInline]
  def get_a(id : Int32) : Int32
    return -1 if id < 0
    @items[id].a
  end
end

c = Container.new
i0 = c.add(Data.new(10, 20, 30))
i1 = c.add(Data.new(40, 50, 60))
i2 = c.add(Data.new(70, 80, 90))

ok = true

# Normal access (inlined)
d = c.get(i0)
if d.a != 10 || d.b != 20 || d.c != 30
  puts "FAIL get(0): #{d.a} #{d.b} #{d.c}"
  ok = false
end

d1 = c.get(i2)
if d1.a != 70
  puts "FAIL get(2): #{d1.a}"
  ok = false
end

# Invalid id guard (inlined)
d2 = c.get(-1)
if d2.a != 0 || d2.b != 0 || d2.c != 0
  puts "FAIL get(-1): #{d2.a} #{d2.b} #{d2.c}"
  ok = false
end

# Chained: get then use result
r = c.get(i0).a + c.get(i1).b + c.get(i2).c
if r != 10 + 50 + 90
  puts "FAIL chain: #{r}"
  ok = false
end

# Inlined method returning Int32
if c.get_a(i1) != 40
  puts "FAIL get_a(1): #{c.get_a(i1)}"
  ok = false
end
if c.get_a(-5) != -1
  puts "FAIL get_a(-5): #{c.get_a(-5)}"
  ok = false
end

# Loop with inlined access
sum = 0
[i0, i1, i2].each do |idx|
  sum += c.get(idx).a
end
if sum != 120
  puts "FAIL loop sum: #{sum}"
  ok = false
end

puts ok ? "inline_struct_ok" : "inline_struct_FAIL"
