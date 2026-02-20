# EXPECT: builder_ok
# Tests String.build with block pattern.
# Common compiler pattern: String.build { |io| io << ... }

result = String.build do |io|
  io << "hello"
  io << " "
  io << "world"
  3.times do |i|
    io << " "
    io << (i + 1)
  end
end

if result == "hello world 1 2 3"
  puts "builder_ok"
else
  puts "builder_bad: #{result}"
end
