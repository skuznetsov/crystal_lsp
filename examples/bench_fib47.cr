# Benchmark: Fibonacci(42) for original Crystal
# ~500 million recursive calls

def fib(n : UInt64) : UInt64
  if n <= 1
    n
  else
    fib(n - 1) + fib(n - 2)
  end
end

result = fib(47)
puts result
