# Edge case: Nested control flow
# if/else inside while loop with conditionals affecting loop variables

def collatz_steps(n : Int32) : Int32
  steps = 0
  current = n
  while current > 1
    if current % 2 == 0
      current = current / 2
    else
      current = current * 3 + 1
    end
    steps = steps + 1
  end
  steps
end

def main : Int32
  # collatz_steps(27) = 111 steps
  collatz_steps(27)
end
