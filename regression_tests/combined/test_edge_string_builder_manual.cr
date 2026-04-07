# Semantic regression: manual String::Builder (stdlib-surface; isolate from other string ops).
# EXPECT: edge_sb_manual_ok

io = String::Builder.new
io << "hello"
io << " "
io << "world"
puts io.to_s

io2 = String::Builder.new
io2 << "count="
io2 << 42
puts io2.to_s

parts = ["a", "b", "c"]
io3 = String::Builder.new
parts.each_with_index do |part, i|
  io3 << ", " if i > 0
  io3 << part
end
puts io3.to_s

puts "edge_sb_manual_ok"
