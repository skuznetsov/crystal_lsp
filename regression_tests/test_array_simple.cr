arr = [10, 20, 30]
# EXPECT: array_simple_done
puts arr.size
puts arr[0]
puts arr[1]
puts arr[2]
arr << 40
puts arr.size
puts arr.includes?(20)
puts arr.includes?(99)
puts "array_simple_done"
