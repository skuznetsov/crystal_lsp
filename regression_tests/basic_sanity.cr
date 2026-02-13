# Basic compilation sanity check
# EXPECT: basic_sanity_ok
# Tests: puts, arithmetic, array, hash literal, each

puts "hello world"
puts 42
puts 3 + 7

a = [1, 2, 3]
sum = 0
a.each { |x| sum += x }
puts "sum=#{sum}"  # expect 6

h = {"x" => 10, "y" => 20}
puts "x=#{h["x"]}"  # expect 10
puts "y=#{h["y"]}"  # expect 20

puts "basic_sanity_ok"
