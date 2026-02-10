# Comprehensive benchmark: sorting, classes, recursion, string output
# Compare execution between original Crystal and Crystal V2

# --- Part 1: Quicksort on Array(Int32) ---

def quicksort(arr : Array(Int32), lo : Int32, hi : Int32) : Nil
  if lo < hi
    pivot = arr[hi]
    i = lo - 1
    j = lo
    while j < hi
      if arr[j] <= pivot
        i += 1
        tmp = arr[i]
        arr[i] = arr[j]
        arr[j] = tmp
      end
      j += 1
    end
    i += 1
    tmp = arr[i]
    arr[i] = arr[hi]
    arr[hi] = tmp

    quicksort(arr, lo, i - 1)
    quicksort(arr, i + 1, hi)
  end
end

# Simple LCG pseudo-random number generator
def lcg(seed : Int32) : Int32
  # Use 32-bit arithmetic to stay in Int32
  ((seed &* 1103515245) &+ 12345) & 0x7fffffff
end

# --- Part 2: Binary tree (class allocation + recursion) ---

class TreeNode
  @value : Int32
  @left_val : Int32
  @right_val : Int32
  @has_children : Bool

  def initialize(@value : Int32)
    @left_val = 0
    @right_val = 0
    @has_children = false
  end

  def value : Int32
    @value
  end

  def left_val : Int32
    @left_val
  end

  def right_val : Int32
    @right_val
  end

  def has_children : Bool
    @has_children
  end

  def set_children(l : Int32, r : Int32) : Nil
    @left_val = l
    @right_val = r
    @has_children = true
  end
end

# Build a complete binary tree, return checksum (node count)
def build_tree_check(depth : Int32) : Int32
  if depth <= 0
    1
  else
    1 + build_tree_check(depth - 1) + build_tree_check(depth - 1)
  end
end

# --- Part 3: Fibonacci with memoization (array-based) ---

def fib_memo(n : Int32) : Int32
  if n <= 1
    return n
  end

  a = 0
  b = 1
  i = 2
  while i <= n
    c = a + b
    a = b
    b = c
    i += 1
  end
  b
end

# --- Part 4: Sieve of Eratosthenes ---

def count_primes(limit : Int32) : Int32
  # Use array of Int32 (0=prime, 1=not prime) since Bool arrays may not work
  sieve = Array(Int32).new(limit, 0)
  sieve[0] = 1
  sieve[1] = 1

  i = 2
  while i * i < limit
    if sieve[i] == 0
      j = i * i
      while j < limit
        sieve[j] = 1
        j += i
      end
    end
    i += 1
  end

  count = 0
  i = 2
  while i < limit
    if sieve[i] == 0
      count += 1
    end
    i += 1
  end
  count
end

# --- Main ---

# Part 1: Quicksort 200,000 elements
n = 2000000
arr = Array(Int32).new(n, 0)
seed = 42
i = 0
while i < n
  seed = lcg(seed)
  arr[i] = seed
  i += 1
end

quicksort(arr, 0, n - 1)

sorted = true
i = 1
while i < n
  if arr[i] < arr[i - 1]
    sorted = false
  end
  i += 1
end

if sorted
  puts "Sort: OK (#{n} elements, first=#{arr[0]}, last=#{arr[n - 1]})"
else
  puts "Sort: FAILED"
end

# Part 2: Binary tree checks (allocation-heavy)
tree_sum = 0
depth = 24
j = 0
while j < 3
  tree_sum += build_tree_check(depth)
  j += 1
end
puts "Tree: depth=#{depth}, check=#{tree_sum}"

# Part 3: Fibonacci
fib_result = fib_memo(46)
puts "Fib(46): #{fib_result}"

# Part 4: Sieve of Eratosthenes
prime_count = count_primes(5000000)
puts "Primes below 5000000: #{prime_count}"

# --- Part 5: Fiber ping-pong (context switching benchmark) ---

ping_ch = Channel(Int32).new
pong_ch = Channel(Int32).new
rounds = 500000

spawn do
  pp_i = 0
  while pp_i < rounds
    val = ping_ch.receive
    pong_ch.send(val + 1)
    pp_i += 1
  end
end

ping_pong_result = 0
pp_j = 0
while pp_j < rounds
  ping_ch.send(ping_pong_result)
  ping_pong_result = pong_ch.receive
  pp_j += 1
end

puts "Ping-pong: #{rounds} rounds, final=#{ping_pong_result}"

# --- Part 6: Fiber fan-out ---

result_ch = Channel(Int64).new

4.times do |producer_id|
  spawn do
    psum = 0_i64
    pk = 0
    while pk < 10000
      psum += (producer_id * 10000 + pk).to_i64
      pk += 1
    end
    result_ch.send(psum)
  end
end

fiber_total = 0_i64
fi = 0
while fi < 4
  fiber_total += result_ch.receive
  fi += 1
end

puts "Fibers: 4 producers, total=#{fiber_total}"

puts "Done."
