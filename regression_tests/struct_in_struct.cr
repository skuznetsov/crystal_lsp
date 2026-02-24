# Test struct containing another struct field — V2 heap-allocates structs
# so struct-typed fields must reserve pointer-sized slots to avoid overlap
# EXPECT: 42
# EXPECT: 99
struct Inner
  getter val : Int32
  def initialize(@val : Int32)
  end
end

struct Outer
  getter inner : Inner
  getter tag : Int32
  def initialize(@inner : Inner, @tag : Int32)
  end
end

inner = Inner.new(42)
outer = Outer.new(inner, 99)
puts outer.inner.val
puts outer.tag
