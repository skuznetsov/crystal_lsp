# Test case/when with multiple conditions
# when 1, 2, 3 should match any of them
# Expected: returns 10 for input 2

def categorize(x : Int32) : Int32
  case x
  when 1, 2, 3
    10  # small
  when 4, 5, 6
    20  # medium
  when 7, 8, 9
    30  # large
  else
    0   # other
  end
end

def main() : Int32
  categorize(2)  # should return 10
end
