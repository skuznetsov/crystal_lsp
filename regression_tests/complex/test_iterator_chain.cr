# EXPECT: iterator_chain_ok
# Tests chained array operations (map, select, each) with closures.
# Exercises block inline yield and type inference through chains.

numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

# Select evens, multiply by 3, collect
evens_x3 = [] of Int32
numbers.each do |n|
  if n % 2 == 0
    evens_x3 << n * 3
  end
end

# Sum the results: 2*3 + 4*3 + 6*3 + 8*3 + 10*3 = 6+12+18+24+30 = 90
sum = 0
evens_x3.each { |v| sum += v }

# Map strings
labels = [] of String
evens_x3.each { |v| labels << "v#{v}" }

if sum == 90 && evens_x3.size == 5 && labels[0] == "v6" && labels[4] == "v30"
  puts "iterator_chain_ok"
else
  puts "iterator_chain_bad: sum=#{sum} size=#{evens_x3.size}"
end
