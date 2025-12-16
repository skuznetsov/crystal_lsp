# Pointer type - raw memory access
# For bootstrap, pointer operations are handled by compiler intrinsics

struct Pointer(T)
  # Allocate count elements of type T
  @[Primitive(:pointer_malloc)]
  def self.malloc(count : Int32) : Pointer(T)
  end

  # Get value at pointer (equivalent to ptr[0])
  @[Primitive(:pointer_get)]
  def value : T
  end

  # Set value at pointer (equivalent to ptr[0] = value)
  @[Primitive(:pointer_set)]
  def value=(val : T)
  end

  # Get value at index
  @[Primitive(:pointer_get)]
  def [](index : Int32) : T
  end

  # Set value at index
  @[Primitive(:pointer_set)]
  def []=(index : Int32, val : T)
  end

  # Pointer arithmetic - advance by offset elements
  @[Primitive(:pointer_add)]
  def +(offset : Int32) : Pointer(T)
  end

  # Pointer arithmetic - go back by offset elements
  @[Primitive(:pointer_add)]
  def -(offset : Int32) : Pointer(T)
  end

  # Reallocate to new size
  @[Primitive(:pointer_realloc)]
  def realloc(new_count : Int32) : Pointer(T)
  end
end
