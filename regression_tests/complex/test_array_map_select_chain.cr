# EXPECT: chain_ok
# Tests method chaining with blocks: map + select + each.
# Common pattern in compiler code.

arr = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

evens = arr.select { |x| x % 2 == 0 }
doubled = evens.map { |x| x * 2 }

sum = 0
doubled.each { |x| sum += x }

# evens = [2,4,6,8,10], doubled = [4,8,12,16,20], sum = 60
if sum == 60
  puts "chain_ok"
else
  puts "chain_bad: #{sum}"
end
