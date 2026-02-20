# EXPECT: hash_each_ok
# Tests Hash#each with block that captures outer variable.
# Common pattern in compiler internals.

hash = {"a" => 1, "b" => 2, "c" => 3}
total = 0
keys = ""

hash.each do |key, value|
  total += value
  keys = keys + key
end

if total == 6
  puts "hash_each_ok"
else
  puts "hash_each_bad: total=#{total}"
end
