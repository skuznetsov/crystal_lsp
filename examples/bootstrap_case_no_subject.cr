# Test case without subject (condition chain)
# Works like if/elsif/else but cleaner syntax
# Expected: returns 2 for input 15

def range_check(x : Int32) : Int32
  case
  when x < 10
    1
  when x < 20
    2
  when x < 30
    3
  else
    4
  end
end

def main() : Int32
  range_check(15)  # 15 < 20, should return 2
end
