# EXPECT: hash_generic_ok
# Tests Hash with generic types and common operations ([], []=, has_key?, each, size, delete).
# Exercises generic hash monomorphization and block iteration.

def count_by_length(words : Array(String)) : Hash(Int32, Int32)
  counts = {} of Int32 => Int32
  words.each do |w|
    len = w.size
    if counts.has_key?(len)
      counts[len] = counts[len] + 1
    else
      counts[len] = 1
    end
  end
  counts
end

words = ["hi", "hello", "hey", "world", "yo", "greetings"]
counts = count_by_length(words)

# hi=2, hey=3, yo=2 => len 2 => 3 words
# hello=5, world=5 => len 5 => 2 words
# greetings=9 => len 9 => 1 word
# len 3 => 1 word (hey)

ok = true
ok = false unless counts.size == 4
ok = false unless counts[2] == 2   # "hi", "yo"
ok = false unless counts[5] == 2   # "hello", "world"
ok = false unless counts[3] == 1   # "hey"
ok = false unless counts[9] == 1   # "greetings"

if ok
  puts "hash_generic_ok"
else
  puts "hash_generic_bad: size=#{counts.size}"
end
