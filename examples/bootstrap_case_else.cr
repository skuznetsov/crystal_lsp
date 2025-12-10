# Test case/when else branch
# When no match, should return else value
# Expected: returns 99 for input 100

def classify(x : Int32) : Int32
  case x
  when 1
    10
  when 2
    20
  when 3
    30
  else
    99  # default
  end
end

def main() : Int32
  classify(100)  # no match, should return 99
end
