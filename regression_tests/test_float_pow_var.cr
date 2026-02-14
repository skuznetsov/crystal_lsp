# EXPECT: float_pow_var_ok

x = 3.0
value = x ** 2
if value == 9.0
  puts "float_pow_var_ok"
else
  puts "float_pow_var_bad: #{value}"
end
