# EXPECT: generic_block_ok
# Tests generic class with method that yields to block.
# Exercises generic monomorphization + block lowering interaction.

class Container(T)
  def initialize(@items : Array(T))
  end

  def each(&block : T ->)
    @items.each do |item|
      yield item
    end
  end

  def map_to_string(&block : T -> String) : Array(String)
    result = [] of String
    each do |item|
      result << block.call(item)
    end
    result
  end
end

ints = Container(Int32).new([1, 2, 3])
strs = ints.map_to_string { |i| "n:#{i}" }

if strs.size == 3 && strs[0] == "n:1" && strs[2] == "n:3"
  puts "generic_block_ok"
else
  puts "generic_block_bad"
end
