# Integer types - arithmetic is handled by compiler intrinsics
# No explicit method definitions needed - compiler recognizes
# Int32#+, Int32#-, etc. directly

struct Int32
  MIN = -2147483648
  MAX = 2147483647
end

struct Int64
  MIN = -9223372036854775808_i64
  MAX = 9223372036854775807_i64
end

struct UInt8
end

struct UInt32
end

struct UInt64
end
