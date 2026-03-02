# Hash resize regression test - entries lost during linear→indexed transition
h = {} of Int32 => Int32
20.times do |i|
  h[i] = i * 10
end
passed = true
20.times do |i|
  val = h[i]?
  if val.nil?
    passed = false
  elsif val != i * 10
    passed = false
  end
end
if passed
  puts "hash_resize_ok"
else
  puts "hash_resize_FAILED"
end
