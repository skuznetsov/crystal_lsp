class Point
# EXPECT: class_done
  getter x : Int32
  getter y : Int32
  
  def initialize(@x : Int32, @y : Int32)
  end
  
  def distance_squared
    @x * @x + @y * @y
  end
end

p = Point.new(3, 4)
puts p.x
puts p.y
puts p.distance_squared
puts "class_done"
