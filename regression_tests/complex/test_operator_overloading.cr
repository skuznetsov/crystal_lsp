# EXPECT: operator_ok
# Tests custom operator overloading — gap in current test coverage.
# Important for struct arithmetic (like Span comparisons).

struct Vec
  @x : Int32
  @y : Int32

  def initialize(@x, @y)
  end

  def x; @x; end
  def y; @y; end

  def +(other : Vec) : Vec
    Vec.new(@x + other.x, @y + other.y)
  end

  def -(other : Vec) : Vec
    Vec.new(@x - other.x, @y - other.y)
  end

  def *(scalar : Int32) : Vec
    Vec.new(@x * scalar, @y * scalar)
  end

  def ==(other : Vec) : Bool
    @x == other.x && @y == other.y
  end

  def !=(other : Vec) : Bool
    !(@x == other.x && @y == other.y)
  end

  def <(other : Vec) : Bool
    @x < other.x && @y < other.y
  end
end

ok = true

a = Vec.new(1, 2)
b = Vec.new(3, 4)

# Addition
c = a + b
if c.x != 4 || c.y != 6
  puts "FAIL +: #{c.x},#{c.y}"
  ok = false
end

# Subtraction
d = b - a
if d.x != 2 || d.y != 2
  puts "FAIL -: #{d.x},#{d.y}"
  ok = false
end

# Scalar multiply
e = a * 3
if e.x != 3 || e.y != 6
  puts "FAIL *: #{e.x},#{e.y}"
  ok = false
end

# Equality
if !(a == Vec.new(1, 2))
  puts "FAIL =="
  ok = false
end
if a == b
  puts "FAIL == (should be false)"
  ok = false
end

# Inequality
if a != Vec.new(1, 2)
  puts "FAIL != (should be false)"
  ok = false
end
if !(a != b)
  puts "FAIL !="
  ok = false
end

# Less than
if !(a < b)
  puts "FAIL <"
  ok = false
end
if b < a
  puts "FAIL < (should be false)"
  ok = false
end

# Chain operations
r = (a + b) * 2
if r.x != 8 || r.y != 12
  puts "FAIL chain: #{r.x},#{r.y}"
  ok = false
end

# Operators in loop
sum = Vec.new(0, 0)
vecs = [Vec.new(1, 1), Vec.new(2, 2), Vec.new(3, 3)]
vecs.each do |v|
  sum = sum + v
end
if sum.x != 6 || sum.y != 6
  puts "FAIL loop: #{sum.x},#{sum.y}"
  ok = false
end

# Index operator on class
class Grid
  @data : Array(Int32)
  @width : Int32

  def initialize(@width, @data)
  end

  def [](x : Int32, y : Int32) : Int32
    @data[y * @width + x]
  end

  def []=(x : Int32, y : Int32, val : Int32)
    @data[y * @width + x] = val
  end
end

g = Grid.new(3, [1, 2, 3, 4, 5, 6, 7, 8, 9])
if g[0, 0] != 1 || g[1, 1] != 5 || g[2, 2] != 9
  puts "FAIL grid[]"
  ok = false
end

g[1, 1] = 99
if g[1, 1] != 99
  puts "FAIL grid[]="
  ok = false
end

puts ok ? "operator_ok" : "operator_FAIL"
