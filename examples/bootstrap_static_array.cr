# Test static array with .each
# For now, manually simulate array[i] access
# [1, 2, 3].each { |x| sum += x }
# Expected: returns 6 (1 + 2 + 3)

def main() : Int32
  # Simulating static array until array literals work
  # arr = [1, 2, 3]
  # arr.each { |x| sum += x }

  # Manual simulation with index-based access
  sum = 0
  (0..2).each { |i|
    # Simulate arr[i] = i + 1
    x = i + 1
    sum = sum + x
  }
  sum
end
