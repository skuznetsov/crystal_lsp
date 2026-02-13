# Test: enums and case/when
enum Color
  Red
  Green  
  Blue
end

c = Color::Green
case c
when Color::Red
  puts "red"
when Color::Green
  puts "green"
when Color::Blue
  puts "blue"
end

puts c.value
