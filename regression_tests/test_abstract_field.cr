abstract class Animal
# EXPECT: done
  abstract def speak : String
end

class Dog < Animal
  def speak : String; "woof"; end
end

class Container
  getter animal : Animal
  def initialize(@animal : Animal); end
  def show : String
    "contains: #{@animal.speak}"
  end
end

d = Dog.new
c = Container.new(d)
puts c.show
puts c.animal.speak
puts "done"
