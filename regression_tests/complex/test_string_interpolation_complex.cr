# EXPECT: interpolation_ok
# Tests complex string interpolation with method calls, arithmetic, conditionals.
# Exercises string builder codegen and temporary value handling.

class Person
  getter name : String
  getter age : Int32

  def initialize(@name : String, @age : Int32)
  end

  def greeting : String
    status = @age >= 18 ? "adult" : "minor"
    "#{@name} (age #{@age}, #{status})"
  end
end

people = [Person.new("Alice", 30), Person.new("Bob", 15)]
results = [] of String
people.each do |p|
  results << p.greeting
end

count = people.size
summary = "#{count} people: #{results[0]} and #{results[1]}"

if summary == "2 people: Alice (age 30, adult) and Bob (age 15, minor)"
  puts "interpolation_ok"
else
  puts "interpolation_bad: #{summary}"
end
