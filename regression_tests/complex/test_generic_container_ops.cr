# EXPECT: generic_container_ok
# Tests generic class with a single type param and container operations.
# Exercises monomorphization, generic arg substitution.
# NOTE: multi-type-param generics (class Pair(A, B)) crash - known bug.

class Box(T)
  getter value : T

  def initialize(@value : T)
  end

  def map(&block : T -> T) : Box(T)
    Box.new(yield @value)
  end

  def to_s : String
    "Box(#{@value})"
  end
end

b1 = Box.new("hello")
b2 = Box.new(42)
b3 = b2.map { |v| v * 2 }

ok = true
ok = false unless b1.value == "hello"
ok = false unless b2.value == 42
ok = false unless b3.value == 84
ok = false unless b1.to_s == "Box(hello)"

if ok
  puts "generic_container_ok"
else
  puts "generic_container_bad"
end
