# EXPECT: nested_yield_ok
# Tests nested yield/block patterns that exercise inline yield paths.

def with_logging(label : String, &)
  yield
end

def transform(arr : Array(Int32), &block : Int32 -> Int32) : Array(Int32)
  result = [] of Int32
  arr.each do |item|
    with_logging("transform") do
      result << block.call(item)
    end
  end
  result
end

input = [1, 2, 3, 4, 5]
doubled = transform(input) { |x| x * 2 }
sum = 0
doubled.each { |x| sum += x }

if sum == 30
  puts "nested_yield_ok"
else
  puts "nested_yield_bad: sum=#{sum}"
end
