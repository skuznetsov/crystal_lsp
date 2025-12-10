# Test multiple calls to yield function
# Expected: returns 35 (10 + 11 + 14)

def with_double(x : Int32) : Int32
  yield x * 2
end

def main() : Int32
  a = with_double(5) { |v| v }       # 10
  b = with_double(5) { |v| v + 1 }   # 11
  c = with_double(7) { |v| v }       # 14
  a + b + c
end
