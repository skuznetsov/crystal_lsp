# EXPECT: ternary_ok
# Tests ternary operator with various types and nesting.
# Gap: no existing test for condition ? true_val : false_val.

ok = true

# Basic ternary
r = 5 > 3 ? "yes" : "no"
if r != "yes"
  puts "FAIL basic: #{r}"
  ok = false
end

# Ternary with variables
a = 10
b = 20
max = a > b ? a : b
if max != 20
  puts "FAIL max: #{max}"
  ok = false
end

# Nested ternary
x = 15
category = x < 10 ? "small" : (x < 20 ? "medium" : "large")
if category != "medium"
  puts "FAIL nested: #{category}"
  ok = false
end

# Ternary in method
def clamp(val : Int32, lo : Int32, hi : Int32) : Int32
  val < lo ? lo : (val > hi ? hi : val)
end

if clamp(5, 0, 10) != 5
  puts "FAIL clamp mid"
  ok = false
end
if clamp(-5, 0, 10) != 0
  puts "FAIL clamp lo"
  ok = false
end
if clamp(15, 0, 10) != 10
  puts "FAIL clamp hi"
  ok = false
end

# Ternary with method calls
def sign(n : Int32) : String
  n > 0 ? "positive" : (n < 0 ? "negative" : "zero")
end

if sign(5) != "positive" || sign(-3) != "negative" || sign(0) != "zero"
  puts "FAIL sign"
  ok = false
end

# Ternary in loop
arr = [3, 1, 4, 1, 5, 9, 2, 6]
max_val = arr[0]
arr.each do |v|
  max_val = v > max_val ? v : max_val
end
if max_val != 9
  puts "FAIL loop max: #{max_val}"
  ok = false
end

# Ternary with assignment
y = 42
z = y % 2 == 0 ? y / 2 : y * 3 + 1
if z != 21
  puts "FAIL assign: #{z}"
  ok = false
end

puts ok ? "ternary_ok" : "ternary_FAIL"
