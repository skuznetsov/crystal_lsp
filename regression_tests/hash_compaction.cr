# Regression: Hash entries lost after delete + insert (compaction)
# EXPECT: hash_compaction_ok
# Bug: yield suffix `unless` parsed as argument modifier, AND
#      loop exit didn't propagate inline-modified new_entry_index
# Fixed: parser.cr (yield postfix modifier) + ast_to_hir.cr (while loop exit)

h = {} of String => Int32

# Fill 16 entries
16.times do |i|
  h["k#{i}"] = i
end

# Delete 8 even keys
8.times do |i|
  h.delete("k#{i * 2}")
end

# Insert one new key (triggers compaction)
h["new0"] = 100

# Verify size
puts "size=#{h.size}"  # expect 9

# Verify each iterates all entries
each_count = 0
h.each { |k, v| each_count += 1 }
puts "each_count=#{each_count}"  # expect 9

# Verify all odd keys present via lookup
missing = 0
8.times do |i|
  k = "k#{i * 2 + 1}"
  v = h[k]?
  unless v
    puts "MISSING: #{k}"
    missing += 1
  end
end
puts "missing=#{missing}"  # expect 0

# Expected output:
# size=9
# each_count=9
# missing=0
puts "hash_compaction_ok" if missing == 0 && each_count == 9
