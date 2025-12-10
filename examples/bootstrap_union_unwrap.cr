# Test union unwrap (extracting value)
# Expected: returns 42 (the stored value)

class Box
  @value : Int32 | Nil

  def initialize(val : Int32)
    @value = val
  end

  def get_value() : Int32
    v = @value
    # After is_a? check, v is narrowed to Int32
    # For now we need to explicitly narrow
    if v.is_a?(Int32)
      # In this branch, v should be Int32
      # But for now, let's use the fact that is_a? confirmed the type
      # and we can trust the union contains Int32
      v.as(Int32)  # This needs UnionUnwrap
    else
      0
    end
  end
end

def main() : Int32
  box = Box.new(42)
  box.get_value()
end
