# Regression: yield with suffix unless/if parsed incorrectly
# EXPECT: yield_suffix_unless_ok
# Bug: `yield entry, i unless entry.deleted?` parsed as
#      `yield(entry, (i unless entry.deleted?))` instead of
#      `(yield entry, i) unless entry.deleted?`
# Fixed: parser.cr (without_postfix_modifiers for yield args + postfix modifier)

# This test verifies that hash iteration correctly skips deleted entries
# (which exercises the yield...unless pattern in Hash#each_entry_with_index)

h = {} of String => Int32
8.times { |i| h["k#{i}"] = i }
4.times { |i| h.delete("k#{i * 2}") }  # delete even keys

# Trigger compaction
h["new"] = 99

each_keys = [] of String
h.each { |k, v| each_keys << k }

# All 4 odd keys + "new" should be present
puts "count=#{each_keys.size}"  # expect 5
puts "yield_suffix_unless_ok" if each_keys.size == 5
