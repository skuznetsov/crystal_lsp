# EXPECT: block_capture_ok
# Tests capturing a block as a Proc and calling it.
# Pattern from original Crystal codegen/proc_spec.cr: "codegens captured block".

def capture(&block : -> Int32) : Proc(Int32)
  block
end

a = 10
captured = capture { a + 32 }
result = captured.call

if result == 42
  puts "block_capture_ok"
else
  puts "block_capture_bad: result=#{result}"
end
