# Int32 sibling of tuple_union_scalar_wrap_repro: proves the descriptor
# issue is NOT Int64-specific nor Channel-specific. A scalar Int32 value
# wrapped into (Int32 | Sentinel) and returned as part of a tuple must
# keep its variant type_id across the unwrap.
# EXPECT: tuple_union_int32_wrap_ok
record Sentinel

def mk : {Int32 | Sentinel}
  u = 7.as(Int32 | Sentinel)
  {u}
end

t = mk
v = t[0]
if v.is_a?(Sentinel)
  puts "tuple_union_int32_wrap_fail_sentinel"
elsif v.is_a?(Int32) && v == 7
  puts "tuple_union_int32_wrap_ok"
else
  puts "tuple_union_int32_wrap_fail_other"
end
