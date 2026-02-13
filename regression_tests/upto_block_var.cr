# Regression: upto with block that modifies outer variable
# EXPECT: upto_block_var_ok
# Bug: while loop exit didn't propagate inline-modified variable
# Fixed: ast_to_hir.cr (while loop exit backedge value propagation)

total = 0
1.upto(5) do |i|
  total += i
end
puts "total=#{total}"  # expect 15

puts "upto_block_var_ok" if total == 15
