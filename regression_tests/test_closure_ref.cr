# Test closure by-reference capture: mutations inside block propagate back
# EXPECT: 42
def call_block(&block : ->)
  block.call
end

x = 10
call_block do
  x = 42
end
puts x
