# Bootstrap test 1: Minimal class
# This tests what we need for basic OOP

class Counter
  @value : Int32

  def initialize
    @value = 0
  end

  def increment : Int32
    @value = @value + 1
    @value
  end

  def get : Int32
    @value
  end
end

def main : Int32
  c = Counter.new()
  c.increment()
  c.increment()
  c.increment()
  c.get()  # Should return 3
end
