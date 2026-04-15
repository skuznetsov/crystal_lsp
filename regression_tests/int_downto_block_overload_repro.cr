count = 0
1.downto(0) do |i|
  puts i
  count += 1
end

raise "downto block did not execute twice" unless count == 2

edge_up = 0
127_i8.upto(127_i8) do |i|
  edge_up += 1
end
raise "upto single-value edge should not overflow or repeat" unless edge_up == 1

edge_down = 0
(-128_i8).downto(-128_i8) do |i|
  edge_down += 1
end
raise "downto single-value edge should not overflow or repeat" unless edge_down == 1

puts "int_downto_block_overload_ok"
