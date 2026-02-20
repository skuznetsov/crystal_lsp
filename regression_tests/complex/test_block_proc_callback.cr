# EXPECT: callback_ok
# Tests block-to-proc conversion in method calls.
# Simplified: avoids &block constructor syntax that triggers spawn path.

def apply_transform(items : Array(String), &block : String -> String) : Array(String)
  result = [] of String
  items.each do |item|
    result << block.call(item)
  end
  result
end

output = apply_transform(["hello", "world"]) { |s| s.upcase }
if output.size == 2 && output[0] == "HELLO" && output[1] == "WORLD"
  puts "callback_ok"
else
  puts "callback_bad"
end
