# Base class for all Crystal objects

class Object
  # Returns true if this object is equal to other
  def ==(other) : Bool
    self.same?(other)
  end

  # Returns true if this object is not equal to other
  def !=(other) : Bool
    !(self == other)
  end

  # Returns true if this object is the same as other (identity)
  def same?(other : Reference) : Bool
    false
  end

  def same?(other : Nil) : Bool
    false
  end

  # Returns the hash code for this object
  def hash() : Int32
    0
  end

  # Returns a string representation
  def to_s() : String
    ""
  end
end
