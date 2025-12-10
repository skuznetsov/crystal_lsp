# Edge case: Multiple parameters and complex expressions

def weighted_sum(a : Int32, b : Int32, c : Int32, wa : Int32, wb : Int32, wc : Int32) : Int32
  a * wa + b * wb + c * wc
end

def complex_expr(x : Int32, y : Int32) : Int32
  # Tests operator precedence and nested operations
  (x + y) * (x - y) + x * x - y * y
end

def chained_calls(n : Int32) : Int32
  # a + b should equal c due to algebraic identity
  a = complex_expr(n, n + 1)
  b = complex_expr(n + 1, n)
  c = weighted_sum(a, b, n, 1, 1, 0)
  c
end

def main : Int32
  # weighted_sum(1, 2, 3, 10, 20, 30) = 10 + 40 + 90 = 140
  # complex_expr(5, 3) = 8*2 + 25 - 9 = 16 + 16 = 32
  # chained_calls(5) = complex_expr(5,6) + complex_expr(6,5) = (-11 + 25 - 36) + (11 + 36 - 25) = -22 + 22 = 0
  weighted_sum(1, 2, 3, 10, 20, 30) + complex_expr(5, 3)
end
