# Test basic Array(Int32) with each
# For now, use simulated array with while loop
# Expected: prints 1, 2, 3 and returns 6

def main() : Int32
  # Simulating: arr = [1, 2, 3]
  # arr.each { |x| sum += x }

  # Manual simulation until Array is implemented
  sum = 0
  3.times { |i|
    # arr[i] would be i + 1
    val = i + 1
    sum = sum + val
  }
  sum
end
