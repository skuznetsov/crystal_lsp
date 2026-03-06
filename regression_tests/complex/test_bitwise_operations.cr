# EXPECT: bitwise_ok
# Tests bitwise operations — gap in current test coverage.
# Important for type_id masking, flag manipulation, etc.

ok = true

# Basic bitwise AND
a = 0xFF
b = 0x0F
if (a & b) != 0x0F
  puts "FAIL AND: #{a & b}"
  ok = false
end

# Bitwise OR
if (0xF0 | 0x0F) != 0xFF
  puts "FAIL OR: #{0xF0 | 0x0F}"
  ok = false
end

# Bitwise XOR
if (0xFF ^ 0x0F) != 0xF0
  puts "FAIL XOR: #{0xFF ^ 0x0F}"
  ok = false
end

# Left shift
if (1 << 8) != 256
  puts "FAIL LSHIFT: #{1 << 8}"
  ok = false
end

# Right shift
if (256 >> 4) != 16
  puts "FAIL RSHIFT: #{256 >> 4}"
  ok = false
end

# Complement
if (~0 & 0xFF) != 0xFF
  puts "FAIL NOT: #{~0 & 0xFF}"
  ok = false
end

# Bit flag pattern
flags = 0
flags |= (1 << 0)  # flag 0
flags |= (1 << 3)  # flag 3
flags |= (1 << 7)  # flag 7

if (flags & (1 << 0)) == 0
  puts "FAIL flag 0 not set"
  ok = false
end
if (flags & (1 << 3)) == 0
  puts "FAIL flag 3 not set"
  ok = false
end
if (flags & (1 << 1)) != 0
  puts "FAIL flag 1 should not be set"
  ok = false
end

# Clear flag
flags &= ~(1 << 3)
if (flags & (1 << 3)) != 0
  puts "FAIL flag 3 should be cleared"
  ok = false
end

# Bit counting in loop
def popcount(n : Int32) : Int32
  count = 0
  v = n
  while v != 0
    count += v & 1
    v >>= 1
  end
  count
end

if popcount(0) != 0
  puts "FAIL popcount(0)"
  ok = false
end
if popcount(0xFF) != 8
  puts "FAIL popcount(0xFF): #{popcount(0xFF)}"
  ok = false
end
if popcount(0x55) != 4
  puts "FAIL popcount(0x55): #{popcount(0x55)}"
  ok = false
end

puts ok ? "bitwise_ok" : "bitwise_FAIL"
