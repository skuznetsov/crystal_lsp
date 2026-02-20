# EXPECT: struct_value_ok
# Tests struct value semantics (copy on assign, independent mutation).
# Exercises struct field access and value-type layout.

struct Point
  getter x : Int32
  getter y : Int32

  def initialize(@x : Int32, @y : Int32)
  end

  def distance_squared(other : Point) : Int32
    dx = @x - other.x
    dy = @y - other.y
    dx * dx + dy * dy
  end

  def translate(dx : Int32, dy : Int32) : Point
    Point.new(@x + dx, @y + dy)
  end

  def to_s : String
    "(#{@x},#{@y})"
  end
end

p1 = Point.new(0, 0)
p2 = Point.new(3, 4)
p3 = p1.translate(1, 1)

d = p1.distance_squared(p2)
# d = 9 + 16 = 25

ok = true
ok = false unless d == 25
ok = false unless p1.x == 0 && p1.y == 0   # original unchanged
ok = false unless p3.x == 1 && p3.y == 1
ok = false unless p2.to_s == "(3,4)"

if ok
  puts "struct_value_ok"
else
  puts "struct_value_bad: d=#{d} p1=#{p1.to_s} p3=#{p3.to_s}"
end
