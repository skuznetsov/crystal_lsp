# Test .times with sum accumulator
# Expected: returns 3 (0 + 1 + 2)

def main() : Int32
  sum = 0
  3.times { |i|
    sum = sum + i
  }
  sum
end
