def describe(x)
# EXPECT: done_case
  case x
  when 1 then "one"
  when 2 then "two"
  when 3 then "three"
  else "other"
  end
end

puts describe(1)
puts describe(2)
puts describe(5)

# Case with string
str = "hello"
case str
when "hello"
  puts "greeting"
when "bye"
  puts "farewell"
else
  puts "unknown"
end

puts "done_case"
