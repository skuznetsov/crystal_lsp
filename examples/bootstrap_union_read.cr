# Test reading union field
# Expected: returns 10

class Box
  @value : Int32 | Nil

  def initialize(val : Int32)
    @value = val  # Wraps val into union with type_id=0 (Int32)
  end

  # For now just verify we can read and assign the union value
  def copy_value() : Int32
    x = @value   # Read union field into local variable
    # Return fixed value for now - full unwrap needs case/is_a?
    10
  end
end

def main() : Int32
  box = Box.new(42)
  box.copy_value()
end
