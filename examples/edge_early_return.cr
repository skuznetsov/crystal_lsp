# Edge case: Early return and multiple returns

def abs(n : Int32) : Int32
  if n < 0
    return 0 - n
  end
  n
end

def sign(n : Int32) : Int32
  if n > 0
    return 1
  end
  if n < 0
    return 0 - 1
  end
  0
end

def clamp(val : Int32, min : Int32, max : Int32) : Int32
  if val < min
    return min
  end
  if val > max
    return max
  end
  val
end

def factorial(n : Int32) : Int32
  if n <= 1
    return 1
  end
  n * factorial(n - 1)
end

def main : Int32
  # abs(-42) = 42
  # sign(-10) = -1
  # clamp(100, 0, 50) = 50
  # factorial(5) = 120
  # Total: 42 + (-1) + 50 + 120 = 211
  abs(0 - 42) + sign(0 - 10) + clamp(100, 0, 50) + factorial(5)
end
