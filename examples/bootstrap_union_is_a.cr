# Test is_a? on union type
# Expected: returns 1 if union contains Int32

class Box
  @value : Int32 | Nil

  def initialize(val : Int32)
    @value = val
  end

  def has_value() : Int32
    v = @value
    if v.is_a?(Int32)
      1  # Yes, it's an Int32
    else
      0  # No, it's Nil
    end
  end
end

def main() : Int32
  box = Box.new(42)
  box.has_value()
end
