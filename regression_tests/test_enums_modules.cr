enum Color
# EXPECT: done
  Red
  Green
  Blue
end

module Printable
  def description
    "#{self}"
  end
end

class Shape
  include Printable
  getter color : Color

  def initialize(@color : Color)
  end
end

class Circle < Shape
  getter radius : Float64

  def initialize(color : Color, @radius : Float64)
    super(color)
  end

  def area
    3.14159 * @radius * @radius
  end
end

class Rectangle < Shape
  getter width : Float64
  getter height : Float64

  def initialize(color : Color, @width : Float64, @height : Float64)
    super(color)
  end

  def area
    @width * @height
  end
end

c = Circle.new(Color::Red, 5.0)
r = Rectangle.new(Color::Blue, 3.0, 4.0)

puts c.area
puts r.area

# Case on enum
color = Color::Green
case color
when Color::Red
  puts "red"
when Color::Green
  puts "green"
when Color::Blue
  puts "blue"
end

# Test enum value
puts Color::Red.value
puts Color::Green.value
puts Color::Blue.value

puts "done"
