# Benchmark: Fibonacci(42)
# ~500 million recursive calls
# Tests: deep recursion, conditionals, arithmetic

def fib(n : Int32) : Int32
  if n <= 1
    n
  else
    fib(n - 1) + fib(n - 2)
  end
end

def main : Int32
  fib(42)
end
