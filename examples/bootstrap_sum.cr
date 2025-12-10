# Test manual summation with while loop
# Expected: returns 3 (0 + 1 + 2)

def main() : Int32
  # Using while loop with phi-compatible structure
  i = 0
  result = 0
  while i < 3
    # This modifies result, needs phi support
    result = result + i
    i = i + 1
  end
  result
end
