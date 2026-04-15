# Reduces tuple-branch union unification: when `if`/`else` branches each
# return a tuple literal of the same arity but with different element
# types in some position, the unified result must be a single tuple whose
# divergent slot is a union, so callers can reliably `.is_a?` each
# variant. Non-Channel, non-stdlib reducer of the same ABI that
# channel_ping_pong_repro exercises via Channel#receive_internal.
# EXPECT: tuple_branch_unify_ok
record Sentinel

def choose(flag : Bool)
  if flag
    {1, 42}
  else
    {1, Sentinel.new}
  end
end

t = choose(true)
v = t[1]
if v.is_a?(Sentinel)
  puts "tuple_branch_unify_fail_sentinel"
elsif v.is_a?(Int32) && v == 42
  puts "tuple_branch_unify_ok"
else
  puts "tuple_branch_unify_fail_other"
end
