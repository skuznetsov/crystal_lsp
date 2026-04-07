# No-prelude semantic regression: String#size, #empty? on empty and non-empty strings.
# Strict stdout: sibling .out (reference Crystal). Compiled with --no-prelude.
# EXPECT: edge_sb_empty_ok

puts "".size
puts "".empty?
puts "hello".empty?

puts "edge_sb_empty_ok"
