# crystal: no-prelude
# Minimal Hash[key] lookup repro — no compaction, no delete.
# Surfaces after cluster A fix (0526cd6b) since earlier STUB crashes
# preempted hash code paths.
#
# EXPECT: hash_basic_lookup_ok
require "primitives"
require "comparable"
require "string"
require "hash"
require "io"

h = {"x" => 10, "y" => 20}
v = h["x"]
if v == 10
  puts "hash_basic_lookup_ok"
else
  puts "FAIL v=#{v}"
end
