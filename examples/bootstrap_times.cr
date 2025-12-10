# Test .times { |i| body } intrinsic - simple version
# Just prints, no accumulator
# Expected: prints 0, 1, 2 and returns 0

def main() : Int32
  3.times { |i|
    puts i
  }
  0
end
