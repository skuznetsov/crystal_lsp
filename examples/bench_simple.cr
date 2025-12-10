# Benchmark 1: Simple arithmetic
# Tests: function calls, basic math

def square(x : Int32) : Int32
  x * x
end

def sum_of_squares(n : Int32) : Int32
  result = 0
  i = 1
  while i <= n
    result = result + square(i)
    i = i + 1
  end
  result
end

def main : Int32
  sum_of_squares(100)
end
