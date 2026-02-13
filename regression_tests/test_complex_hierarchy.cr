# Test complex class hierarchy and module inclusion
# EXPECT: done
module Printable
  def print_info
    puts to_s
  end
end

module Comparable(T)
  abstract def <=>(other : T) : Int32

  def <(other : T) : Bool
    (self <=> other) < 0
  end

  def >(other : T) : Bool
    (self <=> other) > 0
  end
end

class Animal
  include Printable
  getter name : String
  getter age : Int32

  def initialize(@name : String, @age : Int32)
  end

  def to_s : String
    "#{@name} (age #{@age})"
  end
end

class Dog < Animal
  getter breed : String

  def initialize(name : String, age : Int32, @breed : String)
    super(name, age)
  end

  def to_s : String
    "#{@name} the #{@breed} (age #{@age})"
  end

  def bark : String
    "Woof!"
  end
end

class Cat < Animal
  getter indoor : Bool

  def initialize(name : String, age : Int32, @indoor : Bool)
    super(name, age)
  end

  def to_s : String
    loc = @indoor ? "indoor" : "outdoor"
    "#{@name} the #{loc} cat (age #{@age})"
  end
end

# Test polymorphism
animals = [] of Animal
animals << Dog.new("Rex", 5, "German Shepherd")
animals << Cat.new("Whiskers", 3, true)
animals << Dog.new("Buddy", 2, "Labrador")

animals.each do |a|
  a.print_info
end

# Test is_a? and casting
animals.each do |a|
  if a.is_a?(Dog)
    puts a.bark
  end
end

puts animals.size
puts "done"
