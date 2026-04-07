# Semantic regression: String#starts_with? / #ends_with? (compare stdout to host if predicates look wrong).
# EXPECT: edge_sb_prefix_ok

puts "hello".starts_with?("hel")
puts "hello".ends_with?("llo")
puts "hello".starts_with?("world")

puts "edge_sb_prefix_ok"
