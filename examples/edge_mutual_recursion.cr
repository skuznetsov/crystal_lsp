# Edge case: Mutual recursion (functions calling each other)

# Classic even/odd mutual recursion
def is_even(n : Int32) : Int32
  if n == 0
    return 1  # true
  end
  is_odd(n - 1)
end

def is_odd(n : Int32) : Int32
  if n == 0
    return 0  # false
  end
  is_even(n - 1)
end

# Another mutual recursion: ping-pong counter
def ping(n : Int32, acc : Int32) : Int32
  if n <= 0
    return acc
  end
  pong(n - 1, acc + 1)
end

def pong(n : Int32, acc : Int32) : Int32
  if n <= 0
    return acc
  end
  ping(n - 1, acc + 2)
end

def main : Int32
  # is_even(10) = 1 (true, 10 is even)
  # is_even(7) = 0 (false, 7 is odd)
  # is_odd(5) = 1 (true, 5 is odd)
  # is_odd(4) = 0 (false, 4 is even)

  # ping(10, 0):
  # ping(10,0) -> pong(9,1) -> ping(8,3) -> pong(7,4) -> ping(6,6)
  # -> pong(5,7) -> ping(4,9) -> pong(3,10) -> ping(2,12) -> pong(1,13)
  # -> ping(0,15) -> return 15

  e1 = is_even(10)  # 1
  e2 = is_even(7)   # 0
  o1 = is_odd(5)    # 1
  o2 = is_odd(4)    # 0
  pp = ping(10, 0)  # 15

  # Total: 1 + 0 + 1 + 0 + 15 = 17
  # Let's add more to make it distinguishable
  e1 * 100 + e2 * 50 + o1 * 30 + o2 * 10 + pp
  # 100 + 0 + 30 + 0 + 15 = 145
end
