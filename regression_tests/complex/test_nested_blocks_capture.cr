# EXPECT: nested_blocks_ok
# Tests nested blocks with outer variable capture and mutation.
# Exercises closure cell generation for nested block scopes.

def transform(arr : Array(Int32), &block : Int32 -> Int32) : Array(Int32)
  result = [] of Int32
  arr.each do |x|
    result << yield(x)
  end
  result
end

base = 100
multiplier = 3

result = transform([1, 2, 3, 4, 5]) do |x|
  x * multiplier + base
end

sum = 0
result.each { |v| sum += v }

# Expected: (1*3+100) + (2*3+100) + (3*3+100) + (4*3+100) + (5*3+100)
# = 103 + 106 + 109 + 112 + 115 = 545
if sum == 545
  puts "nested_blocks_ok"
else
  puts "nested_blocks_bad: sum=#{sum}"
end
