# EXPECT: nilable_struct_ok
# Tests nilable struct fields (Struct | Nil union).
# V2 has a known bug where nil.as(StructType?) generates type_id=-1 instead of 0,
# which breaks nil checks. This test verifies the pattern.

struct Point
  @x : Int32
  @y : Int32

  def initialize(@x, @y)
  end

  def x; @x; end
  def y; @y; end
end

class Container
  @point : Point?

  def initialize
    @point = nil
  end

  def set(p : Point)
    @point = p
  end

  def get : Point?
    @point
  end

  def has_point? : Bool
    !@point.nil?
  end
end

ok = true

c = Container.new

# Initially nil
if c.has_point?
  puts "FAIL: should be nil"
  ok = false
end

p = c.get
if !p.nil?
  puts "FAIL: get should be nil"
  ok = false
end

# Set value
c.set(Point.new(10, 20))

if !c.has_point?
  puts "FAIL: should have point"
  ok = false
end

p2 = c.get
if p2.nil?
  puts "FAIL: get should not be nil"
  ok = false
else
  if p2.x != 10 || p2.y != 20
    puts "FAIL: point values #{p2.x}, #{p2.y}"
    ok = false
  end
end

# Test nilable struct in if-let pattern
def process(c : Container) : Int32
  if p = c.get
    p.x + p.y
  else
    -1
  end
end

if process(c) != 30
  puts "FAIL: process with point: #{process(c)}"
  ok = false
end

c2 = Container.new
if process(c2) != -1
  puts "FAIL: process without point: #{process(c2)}"
  ok = false
end

# Test nilable struct in array
class Registry
  @items : Array(Point?)

  def initialize
    @items = Array(Point?).new
  end

  def add(p : Point?)
    @items << p
  end

  def get(i : Int32) : Point?
    @items[i]
  end

  def count_non_nil : Int32
    count = 0
    @items.each do |item|
      count += 1 unless item.nil?
    end
    count
  end
end

r = Registry.new
r.add(Point.new(1, 2))
r.add(nil)
r.add(Point.new(3, 4))
r.add(nil)
r.add(Point.new(5, 6))

if r.count_non_nil != 3
  puts "FAIL: count_non_nil: #{r.count_non_nil}"
  ok = false
end

p3 = r.get(0)
if p3.nil? || p3.x != 1
  puts "FAIL: get(0)"
  ok = false
end

p4 = r.get(1)
if !p4.nil?
  puts "FAIL: get(1) should be nil"
  ok = false
end

puts ok ? "nilable_struct_ok" : "nilable_struct_FAIL"
