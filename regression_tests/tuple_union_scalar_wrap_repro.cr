# Reduces the descriptor-backed union variant resolution bug independent
# of Channel. A scalar value wrapped into a (T | Sentinel) union that is
# returned as part of a tuple must keep its variant type_id so that a
# subsequent is_a?(Sentinel) check observes the scalar, not the sentinel.
# Matches the same code path that Channel(Int64)#receive_internal uses.
# EXPECT: tuple_union_scalar_wrap_ok
record Sentinel

def mk : {Int64 | Sentinel}
  u = 42_i64.as(Int64 | Sentinel)
  {u}
end

t = mk
v = t[0]
if v.is_a?(Sentinel)
  puts "tuple_union_scalar_wrap_fail_sentinel"
elsif v.is_a?(Int64) && v == 42_i64
  puts "tuple_union_scalar_wrap_ok"
else
  puts "tuple_union_scalar_wrap_fail_other"
end
