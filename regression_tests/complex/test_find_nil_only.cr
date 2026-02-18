# EXPECT: find_nil_only_ok

array = ["a", "b", "c"]
value = array.find { |item| item == "zzz" }

puts(value.nil? ? "find_nil_only_ok" : "find_nil_only_bad")
