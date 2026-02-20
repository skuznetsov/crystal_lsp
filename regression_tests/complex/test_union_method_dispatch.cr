# EXPECT: union_dispatch_ok
# Tests method dispatch on union types (String | Int32 | Nil).
# Exercises union type_id checks and payload extraction.

def classify(value : String | Int32 | Nil) : String
  if value.is_a?(String)
    "string:#{value.size}"
  elsif value.is_a?(Int32)
    "int:#{value}"
  else
    "nil"
  end
end

results = [] of String
results << classify("hello")
results << classify(42)
results << classify(nil)
results << classify("x")

if results[0] == "string:5" && results[1] == "int:42" && results[2] == "nil" && results[3] == "string:1"
  puts "union_dispatch_ok"
else
  puts "union_dispatch_bad: #{results[0]}, #{results[1]}, #{results[2]}, #{results[3]}"
end
