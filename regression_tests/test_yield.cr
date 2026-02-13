def with_value(x : Int32, &)
# EXPECT: yield_done
  yield x
end

result = with_value(42) do |v|
  v * 2
end
puts result
puts "yield_done"
