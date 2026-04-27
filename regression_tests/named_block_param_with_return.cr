# Reducer: named &block param + non-local return through inline-yield path.
# Pattern mirrors Hash#fetch / ENV.fetch (named &block on yielding callee).
# EXPECT: named_block_return_ok
class Box
  def initialize(@n : Int32)
  end

  # Yielding method with a NAMED &block parameter.
  # The inline-yield path lowers `yield` to the block body inline,
  # but apply_inline previously also materialized `&block` as a Proc
  # unconditionally — which dropped non-local return semantics when
  # the block contained a `return`.
  def fetch(idx : Int32, &block : Int32 -> Int32) : Int32
    if idx >= 0 && idx < @n
      idx * 10
    else
      yield idx
    end
  end
end

def use_box : Int32
  result = Box.new(5).fetch(99) do |i|
    return 7777 # non-local return from use_box, NOT from the block
  end
  result
end

x = use_box
unless x == 7777
  puts "FAIL: expected 7777, got #{x}"
  exit 1
end
puts "named_block_return_ok"
