# Regression: nested yield through Enumerable#each_with_index loses caller locals in inline chain
# EXPECT: yield_nested_each_with_index_ok

def test
  [10, 11, 12].each_with_index do |x, i|
    yield x, i
  end
end

count = 0
sum = 0

test do |x, i|
  count += 1
  sum += i
end

puts "count=#{count} sum=#{sum}" # expect count=3 sum=3
puts "yield_nested_each_with_index_ok" if count == 3 && sum == 3
