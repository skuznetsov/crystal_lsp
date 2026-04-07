# No-prelude semantic regression: String#size, String#[](index).
# Strict stdout: sibling .out (reference Crystal). Compiled with --no-prelude.
# EXPECT: edge_sb_index_ok

puts "hello".size
puts "hello"[0]
puts "hello"[-1]

puts "edge_sb_index_ok"
