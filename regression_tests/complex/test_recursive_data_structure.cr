# EXPECT: recursive_ds_ok
# Tests recursive data structure (linked list) with generic types.
# Exercises nilable union return types in recursive contexts.

class Node(T)
  getter value : T
  property next_node : Node(T)?

  def initialize(@value : T)
    @next_node = nil
  end

  def append(val : T) : Node(T)
    if n = @next_node
      n.append(val)
    else
      new_node = Node(T).new(val)
      @next_node = new_node
    end
    self
  end

  def to_array : Array(T)
    result = [value]
    if n = @next_node
      n.to_array.each { |v| result << v }
    end
    result
  end

  def size : Int32
    if n = @next_node
      1 + n.size
    else
      1
    end
  end
end

list = Node(Int32).new(1)
list.append(2)
list.append(3)
list.append(4)

arr = list.to_array
if list.size == 4 && arr.size == 4 && arr[0] == 1 && arr[3] == 4
  puts "recursive_ds_ok"
else
  puts "recursive_ds_bad: size=#{list.size} arr=#{arr.size}"
end
