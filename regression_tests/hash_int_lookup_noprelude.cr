# crystal: no-prelude
# EXPECT: hash_int_lookup_ok
require "primitives"
require "comparable"
require "string"
require "hash"
require "io"

h = {1 => 10, 2 => 20}
v = h[1]
if v == 10
  puts "hash_int_lookup_ok"
else
  puts "FAIL v=#{v}"
end
