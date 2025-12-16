# Set type - unordered collection of unique elements
# Implemented using hash table with linear probing (same as Hash)

class Set(T)
  @elements : Pointer(T)
  @used : Pointer(Bool)
  @size : Int32
  @capacity : Int32

  def initialize
    @capacity = 16
    @size = 0
    @elements = Pointer(T).malloc(@capacity)
    @used = Pointer(Bool).malloc(@capacity)
    # Initialize used flags to false
    i = 0
    while i < @capacity
      @used[i] = false
      i = i + 1
    end
  end

  def size : Int32
    @size
  end

  def empty? : Bool
    @size == 0
  end

  # Add element to set (returns self for chaining)
  def add(value : T) : self
    # Check if we need to resize (load factor > 0.7)
    if @size * 10 > @capacity * 7
      resize
    end

    hash = hash_element(value)
    idx = hash % @capacity

    # Linear probing - find existing or empty slot
    while @used[idx]
      if elements_equal(@elements[idx], value)
        # Already exists, no need to add
        return self
      end
      idx = (idx + 1) % @capacity
    end

    # Insert new element
    @elements[idx] = value
    @used[idx] = true
    @size = @size + 1
    self
  end

  # Alias for add
  def <<(value : T) : self
    add(value)
  end

  # Check if element exists in set
  def includes?(value : T) : Bool
    idx = find_index(value)
    idx >= 0
  end

  # Remove element from set
  def delete(value : T) : Bool
    idx = find_index(value)
    if idx >= 0
      @used[idx] = false
      @size = @size - 1
      # Rehash subsequent entries to maintain probe sequence
      rehash_after_delete(idx)
      true
    else
      false
    end
  end

  # Clear all elements
  def clear
    i = 0
    while i < @capacity
      @used[i] = false
      i = i + 1
    end
    @size = 0
  end

  # Iterate over elements
  def each
    i = 0
    while i < @capacity
      if @used[i]
        yield @elements[i]
      end
      i = i + 1
    end
  end

  # Private: find index of element, returns -1 if not found
  private def find_index(value : T) : Int32
    hash = hash_element(value)
    idx = hash % @capacity
    start_idx = idx

    while @used[idx]
      if elements_equal(@elements[idx], value)
        return idx
      end
      idx = (idx + 1) % @capacity
      if idx == start_idx
        # Full circle, not found
        result = -1
        return result
      end
    end

    # Note: using intermediate variable to avoid parser issue with '-1' after 'end'
    result = -1
    result
  end

  # Private: compute hash for element
  private def hash_element(value : T) : Int32
    value.hash
  end

  # Private: compare elements for equality
  private def elements_equal(a : T, b : T) : Bool
    a == b
  end

  # Private: resize hash table (double capacity)
  private def resize
    old_elements = @elements
    old_used = @used
    old_capacity = @capacity

    @capacity = @capacity * 2
    @size = 0
    @elements = Pointer(T).malloc(@capacity)
    @used = Pointer(Bool).malloc(@capacity)

    # Initialize new used flags
    i = 0
    while i < @capacity
      @used[i] = false
      i = i + 1
    end

    # Rehash all existing elements
    i = 0
    while i < old_capacity
      if old_used[i]
        add(old_elements[i])
      end
      i = i + 1
    end

    # TODO: free old memory when we have GC
  end

  # Private: rehash entries after deletion to maintain probe sequence
  private def rehash_after_delete(deleted_idx : Int32)
    idx = (deleted_idx + 1) % @capacity

    while @used[idx]
      value = @elements[idx]
      @used[idx] = false
      @size = @size - 1

      # Re-insert this element
      add(value)

      idx = (idx + 1) % @capacity
      if idx == deleted_idx
        break
      end
    end
  end
end
