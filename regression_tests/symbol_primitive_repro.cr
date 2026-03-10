# Symbol primitive regression test
# Tests: Symbol#to_s, Symbol#==, Symbol in tuples

sym = :hello
sym2 = :world
sym3 = :hello

# Symbol#to_s
str = sym.to_s
unless str == "hello"
  STDERR.puts "FAIL: Symbol#to_s expected 'hello', got '#{str}'"
  exit 1
end

# Symbol#== (same)
unless sym == sym3
  STDERR.puts "FAIL: :hello == :hello should be true"
  exit 1
end

# Symbol#== (different)
if sym == sym2
  STDERR.puts "FAIL: :hello == :world should be false"
  exit 1
end

# Symbol in tuple
tup = {sym, 42, true}
unless tup[0] == :hello
  STDERR.puts "FAIL: tuple[0] should be :hello"
  exit 1
end

puts "OK"
