# EXPECT: multi_ret_ok
# Tests blocks that return different types across branches (union return).
# Exercises union type inference for block proc return types.

def find_item(arr : Array(String), target : String) : String?
  result : String? = nil
  arr.each do |item|
    if item == target
      result = item
    end
  end
  result
end

found = find_item(["a", "b", "c"], "b")
not_found = find_item(["a", "b", "c"], "z")

if found == "b" && not_found.nil?
  puts "multi_ret_ok"
else
  puts "multi_ret_bad: found=#{found.inspect} not_found=#{not_found.inspect}"
end
