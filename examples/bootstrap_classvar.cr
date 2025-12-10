# Test class variables and constants

class Counter
  @@total : Int32 = 0
  @value : Int32

  def initialize()
    @value = 0
  end

  def increment() : Int32
    @value = @value + 1
    @@total = @@total + 1
    @value
  end

  def get() : Int32
    @value
  end

  def get_total() : Int32
    @@total
  end
end

def main : Int32
  c1 = Counter.new()
  c2 = Counter.new()

  c1.increment()  # c1.value=1, total=1
  c1.increment()  # c1.value=2, total=2
  c2.increment()  # c2.value=1, total=3

  # c1.get() = 2
  # c2.get() = 1
  # c1.get_total() = 3 (shared across instances)

  result = c1.get() + c2.get() + c1.get_total()
  puts(result)  # Should print 6
  result        # Return 6
end
