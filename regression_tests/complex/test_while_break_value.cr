# EXPECT: while_break_ok
# Tests while loop with break returning values and variable mutation.
# Known pattern from Hash#do_compaction where break value was lost.

class Searcher
  @data : Array(Int32)

  def initialize(@data)
  end

  def find_first_over(threshold : Int32) : Int32
    i = 0
    result = -1
    while i < @data.size
      if @data[i] > threshold
        result = @data[i]
        break
      end
      i += 1
    end
    result
  end

  def find_index(value : Int32) : Int32
    i = 0
    while i < @data.size
      return i if @data[i] == value
      i += 1
    end
    -1
  end

  def count_until(limit : Int32) : Int32
    count = 0
    i = 0
    while i < @data.size
      break if @data[i] >= limit
      count += 1
      i += 1
    end
    count
  end

  # Nested while loops
  def find_pair_sum(target : Int32) : Bool
    i = 0
    while i < @data.size
      j = i + 1
      while j < @data.size
        return true if @data[i] + @data[j] == target
        j += 1
      end
      i += 1
    end
    false
  end
end

ok = true

s = Searcher.new([3, 7, 1, 9, 4, 6, 2, 8, 5])

r = s.find_first_over(5)
if r != 7
  puts "FAIL find_first_over: #{r}"
  ok = false
end

r2 = s.find_first_over(100)
if r2 != -1
  puts "FAIL find_first_over(100): #{r2}"
  ok = false
end

idx = s.find_index(9)
if idx != 3
  puts "FAIL find_index(9): #{idx}"
  ok = false
end

idx2 = s.find_index(99)
if idx2 != -1
  puts "FAIL find_index(99): #{idx2}"
  ok = false
end

c = s.count_until(9)
if c != 3
  puts "FAIL count_until(9): #{c}"
  ok = false
end

if !s.find_pair_sum(10)
  puts "FAIL find_pair_sum(10)"
  ok = false
end

if s.find_pair_sum(100)
  puts "FAIL find_pair_sum(100) should be false"
  ok = false
end

# While with mutation of outer variable
def compact(arr : Array(Int32)) : Array(Int32)
  result = Array(Int32).new
  i = 0
  prev = -1
  while i < arr.size
    val = arr[i]
    if val != prev
      result << val
      prev = val
    end
    i += 1
  end
  result
end

c2 = compact([1, 1, 2, 2, 2, 3, 1, 1, 4])
if c2.size != 5
  puts "FAIL compact size: #{c2.size}"
  ok = false
end
if c2[0] != 1 || c2[1] != 2 || c2[2] != 3 || c2[3] != 1 || c2[4] != 4
  puts "FAIL compact: wrong values"
  ok = false
end

puts ok ? "while_break_ok" : "while_break_FAIL"
