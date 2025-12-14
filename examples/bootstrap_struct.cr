struct Point
  @x : Int32
  @y : Int32

  def initialize(@x : Int32, @y : Int32)
  end

  def x : Int32
    @x
  end

  def y : Int32
    @y
  end

  def sum : Int32
    @x + @y
  end
end

def main : Int32
  p = Point.new(10, 20)
  p.sum  # Expected: 30
end
