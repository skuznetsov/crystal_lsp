# Benchmark: Fibonacci(35) for original Crystal
# ~9 million recursive calls

def fib(n : Int32) : Int32
  if n <= 1
    n
  else
    fib(n - 1) + fib(n - 2)
  end
end

result = fib(35)
puts result
