# Test arr.each where arr is a variable (not literal)
# Expected: returns 6 (1 + 2 + 3)

def main() : Int32
  arr = [1, 2, 3]
  sum = 0
  arr.each { |x|
    sum = sum + x
  }
  sum
end
