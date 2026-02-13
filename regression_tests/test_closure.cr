# Test closures that capture variables
# EXPECT: closure_done
x = 10
add_x = ->(n : Int32) { n + x }
puts add_x.call(5)
puts "closure_done"
