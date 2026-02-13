module Greetable
# EXPECT: module_done
  def greet
    "Hello, I am #{name}"
  end
end

class Person
  include Greetable
  
  getter name : String
  
  def initialize(@name : String)
  end
end

p = Person.new("Alice")
puts p.greet
puts "module_done"
