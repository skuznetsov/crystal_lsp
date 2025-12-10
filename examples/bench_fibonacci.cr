# Benchmark 2: Recursive Fibonacci
# Tests: recursion, conditionals

def fib(n : Int32) : Int32
  if n <= 1
    n
  else
    fib(n - 1) + fib(n - 2)
  end
end

def main : Int32
  fib(20)
end
