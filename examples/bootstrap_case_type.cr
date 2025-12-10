# Test case/when with type matching on union
# Expected: returns 1 when union contains Int32

class Container
  @value : Int32 | Nil

  def initialize(val : Int32)
    @value = val
  end

  def check() : Int32
    v = @value
    case v
    when Int32
      1  # It's an Int32
    when Nil
      2  # It's Nil
    else
      0  # Unknown
    end
  end
end

def main() : Int32
  c = Container.new(42)
  c.check()  # should return 1
end
