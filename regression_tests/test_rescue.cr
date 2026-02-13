# Test: exception handling
# EXPECT: after rescue
begin
  raise "test error"
rescue ex
  puts "caught: #{ex.message}"
end

begin
  arr = [1, 2, 3]
  puts arr[5]
rescue ex
  puts "index error caught"
end

puts "after rescue"
