# Test basic each
# EXPECT: blocks_done
arr = [1, 2, 3, 4, 5]
sum = 0
arr.each do |x|
  sum += x
end
puts sum

# Test map
doubled = arr.map { |x| x * 2 }
puts doubled.size
doubled.each { |x| puts x }

# Test select
evens = arr.select { |x| x % 2 == 0 }
puts evens.size

puts "blocks_done"
