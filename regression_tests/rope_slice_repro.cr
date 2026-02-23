require "../src/compiler/frontend/rope"

source = "abc"
rope = CrystalV2::Compiler::Frontend::Rope.new(source)
bytes = rope.bytes
s1 = bytes[0...1]
s2 = bytes[1...3]
puts "#{s1.size}:#{s2.size}:#{s1[0].chr}:#{s2[0].chr}"
