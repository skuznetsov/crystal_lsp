# Benchmark 3: Nested loops
# Tests: loop performance, variable updates

def matrix_sum(n : Int32) : Int32
  sum = 0
  i = 0
  while i < n
    j = 0
    while j < n
      sum = sum + i * j
      j = j + 1
    end
    i = i + 1
  end
  sum
end

def main : Int32
  matrix_sum(100)
end
