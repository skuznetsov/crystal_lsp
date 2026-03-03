# EXPECT: tuple_single_letter_class_tuple_array_ok

class A
  getter x : Int32

  def initialize(@x : Int32)
  end
end

arr = [] of Tuple(A, String, String, String)
a = A.new(42)
arr << {a, "b", "c", "d"}

val = arr[0]
puts val[0].x
puts val[1]
puts "tuple_single_letter_class_tuple_array_ok"
