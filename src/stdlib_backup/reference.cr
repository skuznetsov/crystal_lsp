# Reference types - passed by reference, allocated on heap

class Reference
  # Returns a unique identifier for this object (memory address)
  def object_id() : UInt64
    0_u64
  end

  # Returns true if this is the same object (same memory address)
  def same?(other : Reference) : Bool
    object_id() == other.object_id()
  end

  def same?(other : Nil) : Bool
    false
  end

  def ==(other : Reference) : Bool
    same?(other)
  end

  def ==(other : Nil) : Bool
    false
  end
end
