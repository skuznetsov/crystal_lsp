# Classification: stdlib-surface oracle (String::Builder#to_s single-use / abort path in stub).
#
# Second String::Builder#to_s must abort (matches stdlib single-use rule).
# EXPECT: first_ok
b = String::Builder.new
b << "x"
puts b.to_s
puts "first_ok"
b.to_s
