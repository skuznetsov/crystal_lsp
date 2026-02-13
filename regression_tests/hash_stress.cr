# Regression: Multiple rounds of hash insert/delete/compaction
# Tests that compaction works correctly across multiple cycles

h = {} of String => Int32

# Round 1: fill 32, delete 16
32.times { |i| h["r1_#{i}"] = i }
16.times { |i| h.delete("r1_#{i * 2}") }
h["trigger1"] = 1000

c1 = 0
h.each { |k, v| c1 += 1 }
puts "r1: size=#{h.size} each=#{c1}"  # expect 17

# Round 2: add more, delete some
16.times { |i| h["r2_#{i}"] = i + 100 }
8.times { |i| h.delete("r2_#{i * 2}") }
h["trigger2"] = 2000

c2 = 0
h.each { |k, v| c2 += 1 }
puts "r2: size=#{h.size} each=#{c2}"  # expect 26

# Verify no keys missing
missing = 0
16.times do |i|
  k = "r1_#{i * 2 + 1}"
  missing += 1 unless h[k]?
end
8.times do |i|
  k = "r2_#{i * 2 + 1}"
  missing += 1 unless h[k]?
end
puts "missing=#{missing}"  # expect 0
puts "hash_stress_ok" if missing == 0
