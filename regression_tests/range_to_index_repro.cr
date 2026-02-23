range = 0...1
res = Indexable.range_to_index_and_count(range, 3)
if res
  puts "#{res[0]}:#{res[1]}"
else
  puts "nil"
end
