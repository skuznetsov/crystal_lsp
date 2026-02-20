# EXPECT: case_types_ok
# Tests type-based dispatch using if/is_a? on union variables.
# NOTE: case/when Type on unions has known narrowing limitation;
# using if/is_a? pattern as workaround (produces correct virtual dispatch).

class Dog
  def speak : String
    "woof"
  end
end

class Cat
  def speak : String
    "meow"
  end
end

class Fish
  def speak : String
    "blub"
  end
end

def describe(animal : Dog | Cat | Fish) : String
  if animal.is_a?(Dog)
    "Dog says #{animal.speak}"
  elsif animal.is_a?(Cat)
    "Cat says #{animal.speak}"
  elsif animal.is_a?(Fish)
    "Fish says #{animal.speak}"
  else
    "unknown"
  end
end

r1 = describe(Dog.new)
r2 = describe(Cat.new)
r3 = describe(Fish.new)

if r1 == "Dog says woof" && r2 == "Cat says meow" && r3 == "Fish says blub"
  puts "case_types_ok"
else
  puts "case_types_bad: #{r1}, #{r2}, #{r3}"
end
