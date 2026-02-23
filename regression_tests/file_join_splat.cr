# Regression test: File.join with multiple arguments
# Bug: splat argument handling only used first element, losing subsequent path components
# EXPECT: join_ok

a = "/tmp"
b = "subdir"
c = "file.txt"

result2 = File.join(a, b)
result3 = File.join(a, b, c)

if result2.includes?(b) && result3.includes?(c)
  puts "join_ok"
else
  puts "FAIL: join2=#{result2}, join3=#{result3}"
end
