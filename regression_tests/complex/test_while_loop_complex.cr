# EXPECT: while_complex_ok
# Tests while loops with complex conditions, break, next.
# Exercises loop phi types and control flow codegen.

# Collatz sequence length
def collatz_len(n : Int32) : Int32
  steps = 0
  current = n
  while current != 1
    if current % 2 == 0
      current = current // 2
    else
      current = current * 3 + 1
    end
    steps += 1
  end
  steps
end

# Find first element matching predicate
def find_first(arr : Array(Int32), min : Int32) : Int32
  i = 0
  result = -1
  while i < arr.size
    if arr[i] >= min
      result = arr[i]
      break
    end
    i += 1
  end
  result
end

# Sum skipping multiples of 3
def sum_skip_3(n : Int32) : Int32
  i = 1
  total = 0
  while i <= n
    if i % 3 == 0
      i += 1
      next
    end
    total += i
    i += 1
  end
  total
end

c6 = collatz_len(6)      # 6->3->10->5->16->8->4->2->1 = 8 steps
f = find_first([1, 5, 3, 8, 2], 4)  # 5
s = sum_skip_3(10)        # 1+2+4+5+7+8+10 = 37

if c6 == 8 && f == 5 && s == 37
  puts "while_complex_ok"
else
  puts "while_complex_bad: c6=#{c6} f=#{f} s=#{s}"
end
