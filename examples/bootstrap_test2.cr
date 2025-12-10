# Bootstrap test 2: Multiple objects of same class

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

  def move(dx : Int32, dy : Int32) : Int32
    @x = @x + dx
    @y = @y + dy
    @x + @y
  end

  def distance_squared : Int32
    @x * @x + @y * @y
  end
end

def main : Int32
  p1 = Point.new(3, 4)
  p2 = Point.new(10, 20)

  # p1: (3, 4)
  # p2: (10, 20)

  d1 = p1.distance_squared()  # 9 + 16 = 25
  sum = p2.get_x() + p2.get_y()  # 10 + 20 = 30

  p1.move(1, 1)  # p1 becomes (4, 5), returns 9

  d2 = p1.distance_squared()  # 16 + 25 = 41

  # Result: 25 + 30 + 41 = 96
  d1 + sum + d2
end
