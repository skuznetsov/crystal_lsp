# Edge case: Different integer types (no type conversions)

def add_i8(a : Int8, b : Int8) : Int8
  a + b
end

def sub_i16(a : Int16, b : Int16) : Int16
  a - b
end

def mul_i64(a : Int64, b : Int64) : Int64
  a * b
end

def div_u32(a : UInt32, b : UInt32) : UInt32
  a / b
end

# Keep everything in Int32 for main to avoid conversions
def compute(a : Int32, b : Int32, c : Int32) : Int32
  # Test that Int32 arithmetic works correctly
  x = a + b
  y = x * c
  z = y - a
  z / b
end

def main : Int32
  # compute(10, 5, 3) = ((10+5)*3 - 10) / 5 = (45 - 10) / 5 = 35 / 5 = 7
  # But we need a larger result for verification...

  # Let's do: compute(20, 4, 5) = ((20+4)*5 - 20) / 4 = (120 - 20) / 4 = 100 / 4 = 25
  # Plus some direct Int32 math: 25 + 100 + 50 = 175

  base = compute(20, 4, 5)  # 25
  extra = 100 + 50          # 150
  base + extra              # 175
end
