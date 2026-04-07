# Integration smoke: full string-builder + string-ops path in one binary.
# For bisecting semantic gaps vs host Crystal, use the focused oracles:
#   test_edge_string_builder_manual.cr, test_edge_string_mul_concat.cr, test_no_prelude_edge_string_index.cr,
#   test_no_prelude_edge_string_each_char.cr, test_edge_string_prefix_suffix.cr, test_edge_string_gsub_strip.cr,
#   test_no_prelude_edge_string_empty.cr (strict .out baselines under --no-prelude)
#
# EXPECT: string_builder_all_ok

# String::Builder basic
io = String::Builder.new
io << "hello"
io << " "
io << "world"
puts io.to_s

# Building with numbers
io2 = String::Builder.new
io2 << "count="
io2 << 42
puts io2.to_s

# Joining with String::Builder manually
parts = ["a", "b", "c"]
io3 = String::Builder.new
parts.each_with_index do |part, i|
  io3 << ", " if i > 0
  io3 << part
end
puts io3.to_s

# String multiplication and concatenation
s = "abc" * 3
puts s
puts s.size

# String concatenation via +
s2 = "hello" + " " + "world"
puts s2

# String chars
puts "hello".size
puts "hello"[0]
puts "hello"[-1]

# String each_char
count = 0
"hello world".each_char { |c| count += 1 if c == 'l' }
puts count

# String starts_with? / ends_with?
puts "hello".starts_with?("hel")
puts "hello".ends_with?("llo")
puts "hello".starts_with?("world")

# String gsub
puts "hello world".gsub("world", "crystal")

# String strip
puts "  hello  ".strip

# Empty string
puts "".size
puts "".empty?
puts "hello".empty?

puts "string_builder_all_ok"
