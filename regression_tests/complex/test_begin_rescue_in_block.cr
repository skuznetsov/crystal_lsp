# EXPECT: rescue_block_ok
# Tests begin/rescue inside a block (common in compiler error handling).

def safe_each(arr : Array(String), &block : String -> String) : Array(String)
  results = [] of String
  arr.each do |item|
    begin
      results << block.call(item)
    rescue ex
      results << "error:#{ex.message}"
    end
  end
  results
end

items = ["hello", "world"]
output = safe_each(items) { |s| s.upcase }

if output.size == 2 && output[0] == "HELLO" && output[1] == "WORLD"
  puts "rescue_block_ok"
else
  puts "rescue_block_bad: #{output.size}"
end
