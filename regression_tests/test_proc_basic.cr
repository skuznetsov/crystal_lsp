# Proc and closure test
# EXPECT: proc_test_done
adder = ->(a : Int32, b : Int32) { a + b }
puts adder.call(3, 4)

x = 10
multiplier = ->(n : Int32) { n * x }
puts multiplier.call(5)

puts "proc_test_done"
