# Test case/when with values
# Expected: returns 2 for input 10

def check(x : Int32) : Int32
  case x
  when 5
    1
  when 10
    2
  when 15
    3
  else
    0
  end
end

def main() : Int32
  check(10)
end
