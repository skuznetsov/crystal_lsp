# EXPECT: module_include_ok
# Tests module inclusion with instance methods and self-type resolution.
# Exercises module mixin expansion and method resolution through includes.

module Printable
  def to_display : String
    "#{type_label}: #{display_value}"
  end

  def type_label : String
    "Unknown"
  end
end

module Measurable
  def measure : Int32
    0
  end
end

class Temperature
  include Printable
  include Measurable

  def initialize(@celsius : Float64)
  end

  def type_label : String
    "Temp"
  end

  def display_value : String
    "#{@celsius}C"
  end

  def measure : Int32
    @celsius.to_i
  end
end

class Weight
  include Printable
  include Measurable

  def initialize(@kg : Float64)
  end

  def type_label : String
    "Weight"
  end

  def display_value : String
    "#{@kg}kg"
  end

  def measure : Int32
    @kg.to_i
  end
end

t = Temperature.new(36.6)
w = Weight.new(75.5)

ok = true
ok = false unless t.to_display == "Temp: 36.6C"
ok = false unless w.to_display == "Weight: 75.5kg"
ok = false unless t.measure == 36
ok = false unless w.measure == 75

if ok
  puts "module_include_ok"
else
  puts "module_include_bad"
end
