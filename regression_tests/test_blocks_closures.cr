# Test blocks, closures, and iterators
# EXPECT: done
arr = [10, 20, 30, 40, 50]

# map
doubled = arr.map { |x| x * 2 }
puts doubled.size
puts doubled[0]
puts doubled[4]

# select
big = arr.select { |x| x > 25 }
puts big.size
puts big[0]

# each_with_object / reduce pattern via manual accumulation
sum = 0
arr.each { |x| sum += x }
puts sum

# any? / all?
puts arr.any? { |x| x > 40 }
puts arr.all? { |x| x > 5 }

# index
idx = arr.index(30)
puts idx.nil? ? "nil" : idx.to_s

# compact_map (skip nil via if)
result = [] of Int32
arr.each do |x|
  result << x * 10 if x > 20
end
puts result.size
puts result[0]

# Nested blocks
matrix = [[1, 2], [3, 4], [5, 6]]
flat = [] of Int32
matrix.each do |row|
  row.each do |val|
    flat << val
  end
end
puts flat.size
puts flat[5]

puts "done"
