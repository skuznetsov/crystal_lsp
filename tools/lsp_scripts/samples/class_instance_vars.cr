class Counter
  @@total : Int32 = 0
  @value : Int32

  def initialize(@value = 0)
    @@total += 1
  end

  def increment
    @value += 1
    @@total += 1
  end

  def value : Int32
    @value
  end

  def self.total : Int32
    @@total
  end
end

c1 = Counter.new(10)
c2 = Counter.new(20)

c1.increment
val = c1.value
total = Counter.total
