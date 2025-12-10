# Edge case: Short-circuit evaluation (&&, ||)

# Test that && short-circuits: if left is false, right is not evaluated
def test_and(a : Int32, b : Int32) : Int32
  # If a > 0 && b > 0, return 1, else 0
  if a > 0 && b > 0
    return 1
  end
  0
end

# Test that || short-circuits: if left is true, right is not evaluated
def test_or(a : Int32, b : Int32) : Int32
  # If a > 0 || b > 0, return 1, else 0
  if a > 0 || b > 0
    return 1
  end
  0
end

# Combined logical expressions
def test_complex(a : Int32, b : Int32, c : Int32) : Int32
  # (a > 0 && b > 0) || c > 0
  if (a > 0 && b > 0) || c > 0
    return 1
  end
  0
end

# Negation with short-circuit
def test_not(a : Int32) : Int32
  if !(a > 0)
    return 1
  end
  0
end

def main : Int32
  # test_and(5, 3) = 1 (both positive)
  # test_and(5, 0-1) = 0 (second negative)
  # test_and(0-1, 5) = 0 (first negative, short-circuits)

  # test_or(5, 3) = 1 (first positive, short-circuits)
  # test_or(0-1, 5) = 1 (second positive)
  # test_or(0-1, 0-2) = 0 (both negative)

  # test_complex(5, 3, 0-1) = 1 (first two positive)
  # test_complex(0-1, 5, 10) = 1 (third positive)
  # test_complex(0-1, 5, 0-1) = 0 (all conditions false)

  # test_not(5) = 0 (5 > 0, so !(true) = false)
  # test_not(0-5) = 1 (-5 not > 0, so !(false) = true)

  r1 = test_and(5, 3)       # 1
  r2 = test_and(5, 0 - 1)   # 0
  r3 = test_and(0 - 1, 5)   # 0

  r4 = test_or(5, 3)        # 1
  r5 = test_or(0 - 1, 5)    # 1
  r6 = test_or(0 - 1, 0 - 2) # 0

  r7 = test_complex(5, 3, 0 - 1)   # 1
  r8 = test_complex(0 - 1, 5, 10)  # 1
  r9 = test_complex(0 - 1, 5, 0 - 1) # 0

  r10 = test_not(5)         # 0
  r11 = test_not(0 - 5)     # 1

  # Sum: 1 + 0 + 0 + 1 + 1 + 0 + 1 + 1 + 0 + 0 + 1 = 6
  # Multiply by 20 for distinguishable exit code
  (r1 + r2 + r3 + r4 + r5 + r6 + r7 + r8 + r9 + r10 + r11) * 20
  # = 6 * 20 = 120
end
