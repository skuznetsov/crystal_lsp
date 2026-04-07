# Classification: stdlib-surface oracle (String::Builder.new(Int32) + append + to_s).
#
# EXPECT: builder_cap_ok
b = String::Builder.new(8)
b << "hi"
puts b.to_s
puts "builder_cap_ok"
