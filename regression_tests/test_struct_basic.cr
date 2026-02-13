# Minimal struct test — field storage and retrieval
# EXPECT: struct_basic_done
struct Point
  getter x : Int32
  getter y : Int32

  def initialize(@x : Int32, @y : Int32)
  end
end

p = Point.new(10, 20)
puts p.x   # Should print 10
puts p.y   # Should print 20

puts "sizeof Point: #{sizeof(Point)}"

# Now test Pointer(Point)
ptr = Pointer(Point).malloc(2)
ptr[0] = Point.new(100, 200)
ptr[1] = Point.new(300, 400)

puts ptr[0].x  # 100
puts ptr[0].y  # 200
puts ptr[1].x  # 300
puts ptr[1].y  # 400

puts "struct_basic_done"
