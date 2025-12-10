# Benchmark: Recursive sum of squares
# Tests: recursion, conditionals, arithmetic

def square(x : Int32) : Int32
  x * x
end

def sum_squares(n : Int32) : Int32
  if n <= 0
    0
  else
    square(n) + sum_squares(n - 1)
  end
end

def main : Int32
  sum_squares(10)
end
