# No-prelude semantic regression: String#each_char.
# Strict stdout: sibling .out (reference Crystal). Compiled with --no-prelude (see run_combined.sh).
# EXPECT: edge_sb_each_char_ok

count = 0
"hello world".each_char { |c| count += 1 if c == 'l' }
puts count

puts "edge_sb_each_char_ok"
