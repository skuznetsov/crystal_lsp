# EXPECT: ternary_struct_ok
# Tests ternary operator selecting between struct values.
# This is the exact pattern in Span#cover: `start <= other.start ? self : other`
# The ternary must correctly select and copy the full struct, not just a pointer.

struct Vec2
  @x : Int32
  @y : Int32

  def initialize(@x, @y)
  end

  def x; @x; end
  def y; @y; end

  def min_x(other : Vec2) : Vec2
    x <= other.x ? self : other
  end

  def max_y(other : Vec2) : Vec2
    y >= other.y ? self : other
  end

  def cover(other : Vec2) : Vec2
    low = min_x(other)
    high = max_y(other)
    Vec2.new(low.x, high.y)
  end
end

ok = true

a = Vec2.new(10, 20)
b = Vec2.new(5, 30)

# a.min_x(b) should be b (5 < 10)
r = a.min_x(b)
if r.x != 5
  puts "FAIL min_x: #{r.x}"
  ok = false
end

# a.max_y(b) should be b (30 > 20)
r2 = a.max_y(b)
if r2.y != 30
  puts "FAIL max_y: #{r2.y}"
  ok = false
end

# cover
c = a.cover(b)
if c.x != 5 || c.y != 30
  puts "FAIL cover: #{c.x}, #{c.y}"
  ok = false
end

# Chained covers
v1 = Vec2.new(0, 10)
v2 = Vec2.new(5, 30)
v3 = Vec2.new(-3, 20)
v4 = Vec2.new(8, 50)

acc = v1
acc = acc.cover(v2)
acc = acc.cover(v3)
acc = acc.cover(v4)
if acc.x != -3 || acc.y != 50
  puts "FAIL chain: #{acc.x}, #{acc.y}"
  ok = false
end

# Loop cover
items = [Vec2.new(10, 20), Vec2.new(5, 15), Vec2.new(0, 25), Vec2.new(3, 18)]
acc2 = items[0]
items.each do |v|
  acc2 = acc2.cover(v)
end
if acc2.x != 0 || acc2.y != 25
  puts "FAIL loop: #{acc2.x}, #{acc2.y}"
  ok = false
end

puts ok ? "ternary_struct_ok" : "ternary_struct_FAIL"
