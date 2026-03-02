# EXPECT: done
flag = false
1_000_000.times do
  STDERR.puts "TRACE" if flag
end
puts "done"
