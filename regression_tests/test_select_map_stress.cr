# EXPECT: select_map_stress_ok

arr = [] of Int32
100.times { |i| arr << i }

selected = arr.select { |x| x % 2 == 0 }
mapped = selected.map { |x| x + 1 }

if selected.size == 50 && mapped.size == 50 && mapped.first == 1 && mapped.last == 99
  puts "select_map_stress_ok"
else
  puts "select_map_stress_bad: #{selected.size},#{mapped.size},#{mapped.first?},#{mapped.last?}"
end
