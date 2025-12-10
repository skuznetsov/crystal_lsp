# Test puts support

class Point
  @x : Int32
  @y : Int32

  def initialize(x : Int32, y : Int32)
    @x = x
    @y = y
  end

  def get_x : Int32
    @x
  end

  def get_y : Int32
    @y
  end

  def distance_squared : Int32
    @x * @x + @y * @y
  end
end

def main : Int32
  p = Point.new(3, 4)

  puts(p.get_x())       # prints 3
  puts(p.get_y())       # prints 4
  puts(p.distance_squared())  # prints 25

  0
end
