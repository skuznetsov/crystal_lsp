# String type - Crystal v2 bootstrap implementation
# String operations use runtime functions

class String
  @buffer : Pointer(UInt8)
  @size : Int32

  def size : Int32
    @size
  end

  def bytesize : Int32
    @size
  end

  def empty? : Bool
    @size == 0
  end
end
