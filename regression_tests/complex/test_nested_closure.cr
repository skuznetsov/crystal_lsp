# EXPECT: nested_closure_ok
# Tests nested closure that captures variable from multiple levels.
# Pattern from original Crystal codegen/closure_spec.cr.

def outer(x : Int32) : Proc(Int32, Int32)
  y = x * 2
  ->(z : Int32) { x + y + z }
end

f = outer(5)
# x=5, y=10, z=3 => 5+10+3=18
result = f.call(3)

if result == 18
  puts "nested_closure_ok"
else
  puts "nested_closure_bad: result=#{result}"
end
