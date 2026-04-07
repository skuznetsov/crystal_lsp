# Semantic regression: String#* and String#+ (isolate from Builder).
# EXPECT: edge_sb_mul_concat_ok

s = "abc" * 3
puts s
puts s.size

s2 = "hello" + " " + "world"
puts s2

puts "edge_sb_mul_concat_ok"
