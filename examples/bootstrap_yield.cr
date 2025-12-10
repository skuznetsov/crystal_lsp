# Test generic yield with inline expansion
# Expected: returns 11 (5 * 2 + 1)

def with_double(x : Int32) : Int32
  yield x * 2
end

def main() : Int32
  result = with_double(5) { |doubled|
    doubled + 1
  }
  result
end
