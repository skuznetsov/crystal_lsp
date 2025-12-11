# Test array literal [1, 2, 3] with indexing
# Expected: returns 6 (1 + 2 + 3)

def main() : Int32
  arr = [1, 2, 3]
  arr[0] + arr[1] + arr[2]
end
