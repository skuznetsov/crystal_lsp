# Test union type syntax: Int32 | Nil
# This tests basic parsing and type handling of union types

class Container
  # Union type field: can hold Int32 or nothing (Nil)
  @value : Int32 | Nil

  def initialize(val : Int32)
    @value = val
  end

  def get() : Int32
    # For now just return a fixed value since union unwrap isn't implemented
    42
  end
end

def main() : Int32
  c = Container.new(10)
  c.get()
end
