# Test case/when with Range
# when 1..10 should match if subject is in range
# Expected: returns 1 for input 5 (in 1..10)

def grade(score : Int32) : Int32
  case score
  when 90..100
    4  # A
  when 80..89
    3  # B
  when 70..79
    2  # C
  when 60..69
    1  # D
  else
    0  # F
  end
end

def main() : Int32
  grade(85)  # 85 is in 80..89, should return 3
end
