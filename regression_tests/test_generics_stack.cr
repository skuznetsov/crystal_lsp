# Test generic class with push/pop
# EXPECT: done
class Stack(T)
  def initialize
    @data = [] of T
  end

  def push(value : T)
    @data << value
  end

  def pop : T
    @data.pop
  end

  def peek : T
    @data.last
  end

  def size : Int32
    @data.size
  end

  def empty? : Bool
    @data.empty?
  end

  def each(&block : T ->)
    @data.each { |item| block.call(item) }
  end
end

# Int stack
s = Stack(Int32).new
s.push(10)
s.push(20)
s.push(30)
puts s.size
puts s.peek
puts s.pop
puts s.size

# String stack
ss = Stack(String).new
ss.push("hello")
ss.push("world")
puts ss.peek
puts ss.pop
puts ss.empty?
puts ss.pop
puts ss.empty?

puts "done"
