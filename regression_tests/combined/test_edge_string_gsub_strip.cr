# Semantic regression: String#gsub, String#strip.
# EXPECT: edge_sb_gsub_strip_ok

puts "hello world".gsub("world", "crystal")
puts "  hello  ".strip

puts "edge_sb_gsub_strip_ok"
