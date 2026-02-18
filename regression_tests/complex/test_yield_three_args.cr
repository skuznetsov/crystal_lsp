# EXPECT: yield_three_args_ok

def with_triplet(&)
  ptr = Pointer(UInt8).null
  count = 1
  negative = false
  yield ptr, count, negative
end

ok = false
with_triplet do |ptr, count, negative|
  ok = ptr.null? && count == 1 && !negative
end

puts(ok ? "yield_three_args_ok" : "yield_three_args_bad")
