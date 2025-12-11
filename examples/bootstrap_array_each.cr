# Test array literal with .each iteration
# [1, 2, 3].each { |x| sum += x }
# Expected: returns 6 (1 + 2 + 3)

def main() : Int32
  sum = 0
  [1, 2, 3].each { |x|
    sum = sum + x
  }
  sum
end
