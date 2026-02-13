abstract class Base
# EXPECT: done
  getter id : Int32
  def initialize(@id : Int32); end
end

class Child1 < Base
  def value : Int32; 42; end
end

class Child2 < Base
  getter extra : String
  def initialize(id : Int32, @extra : String)
    super(id)
  end
  def value : Int32; 99; end
end

c1 = Child1.new(1)
puts "c1 id: #{c1.id}"
puts "c1 val: #{c1.value}"

c2 = Child2.new(2, "hello")
puts "c2 id: #{c2.id}"
puts "c2 val: #{c2.value}"
puts "c2 extra: #{c2.extra}"
puts "done"
