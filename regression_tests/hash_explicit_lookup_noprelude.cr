# crystal: no-prelude
# EXPECT: hash_explicit_lookup_ok
require "primitives"
require "comparable"
require "string"
require "hash"
require "io"

h = Hash(Int32, Int32).new
h[1] = 10
h[2] = 20
puts "size=#{h.size}"
puts "1=#{h[1]?}"
puts "2=#{h[2]?}"
v = h[1]?
if v == 10
  puts "hash_explicit_lookup_ok"
else
  puts "FAIL v=#{v}"
end
