# EXPECT: closure_escape_ok
# Tests closure that escapes its creating function and is called later.
# Pattern from original Crystal codegen/closure_spec.cr.
# Known bug: closures use global variables for captures instead of
# heap-allocated environments, so all closures share the same capture.

def make_adder(n : Int32) : Proc(Int32, Int32)
  ->(x : Int32) { x + n }
end

add5 = make_adder(5)
add10 = make_adder(10)

r1 = add5.call(3)    # 8
r2 = add10.call(3)   # 13
r3 = add5.call(100)  # 105

if r1 == 8 && r2 == 13 && r3 == 105
  puts "closure_escape_ok"
else
  puts "closure_escape_bad: r1=#{r1} r2=#{r2} r3=#{r3}"
end
