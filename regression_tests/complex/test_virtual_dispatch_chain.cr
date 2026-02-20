# EXPECT: vdispatch_ok
# Tests virtual dispatch through abstract class hierarchy with method chaining.
# Exercises type_id switch generation and correct vtable resolution.

abstract class Shape
  abstract def area : Float64
  abstract def name : String

  def describe : String
    "#{name}: #{area}"
  end
end

class Circle < Shape
  def initialize(@radius : Float64)
  end

  def area : Float64
    3.14159 * @radius * @radius
  end

  def name : String
    "Circle"
  end
end

class Rectangle < Shape
  def initialize(@width : Float64, @height : Float64)
  end

  def area : Float64
    @width * @height
  end

  def name : String
    "Rectangle"
  end
end

class Triangle < Shape
  def initialize(@base : Float64, @height : Float64)
  end

  def area : Float64
    @base * @height / 2.0
  end

  def name : String
    "Triangle"
  end
end

shapes = [] of Shape
shapes << Circle.new(1.0)
shapes << Rectangle.new(2.0, 3.0)
shapes << Triangle.new(4.0, 5.0)

total = 0.0
names = [] of String
shapes.each do |s|
  total += s.area
  names << s.name
end

if names.size == 3 && total > 19.0 && total < 20.0
  puts "vdispatch_ok"
else
  puts "vdispatch_bad: names=#{names.size} total=#{total}"
end
