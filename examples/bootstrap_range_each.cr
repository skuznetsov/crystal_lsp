# Test Range#each intrinsic
# (1..3).each { |i| sum += i }
# sum = 1 + 2 + 3 = 6
# Expected: returns 6

def main() : Int32
  sum = 0
  (1..3).each { |i|
    sum = sum + i
  }
  sum
end
