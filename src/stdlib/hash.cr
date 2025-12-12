# Hash type - key-value storage with O(1) average lookup
# Uses open addressing with linear probing

class Hash(K, V)
  @keys : Pointer(K)
  @values : Pointer(V)
  @used : Pointer(Bool)
  @size : Int32
  @capacity : Int32

  def initialize
    @capacity = 16
    @size = 0
    @keys = Pointer(K).malloc(@capacity)
    @values = Pointer(V).malloc(@capacity)
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

  # Get value by key, raises if not found
  def [](key : K) : V
    idx = find_index(key)
    if idx >= 0
      @values[idx]
    else
      raise "Key not found"
    end
  end

  # Get value by key, returns nil if not found
  def []?(key : K) : V?
    idx = find_index(key)
    if idx >= 0
      @values[idx]
    else
      nil
    end
  end

  # Set value for key
  def []=(key : K, value : V)
    # Check if we need to resize (load factor > 0.7)
    if @size * 10 > @capacity * 7
      resize
    end

    hash = hash_key(key)
    idx = hash % @capacity

    # Linear probing
    while @used[idx]
      if keys_equal(@keys[idx], key)
        # Key exists, update value
        @values[idx] = value
        return value
      end
      idx = (idx + 1) % @capacity
    end

    # Insert new key-value pair
    @keys[idx] = key
    @values[idx] = value
    @used[idx] = true
    @size = @size + 1
    value
  end

  # Check if key exists
  def has_key?(key : K) : Bool
    find_index(key) >= 0
  end

  # Delete key, returns value if existed
  def delete(key : K) : V?
    idx = find_index(key)
    if idx >= 0
      value = @values[idx]
      @used[idx] = false
      @size = @size - 1
      # Rehash subsequent entries to maintain probe sequence
      rehash_after_delete(idx)
      value
    else
      nil
    end
  end

  # Iterate over each key-value pair
  def each
    i = 0
    while i < @capacity
      if @used[i]
        yield @keys[i], @values[i]
      end
      i = i + 1
    end
  end

  # Iterate over keys
  def each_key
    i = 0
    while i < @capacity
      if @used[i]
        yield @keys[i]
      end
      i = i + 1
    end
  end

  # Iterate over values
  def each_value
    i = 0
    while i < @capacity
      if @used[i]
        yield @values[i]
      end
      i = i + 1
    end
  end

  # Get all keys as array (simplified - returns first key for now)
  def keys : Array(K)
    # TODO: implement when Array is fully working
    raise "Hash#keys not yet implemented"
  end

  # Get all values as array
  def values : Array(V)
    # TODO: implement when Array is fully working
    raise "Hash#values not yet implemented"
  end

  # Clear all entries
  def clear
    i = 0
    while i < @capacity
      @used[i] = false
      i = i + 1
    end
    @size = 0
  end

  # Private: find index of key, returns -1 if not found
  private def find_index(key : K) : Int32
    hash = hash_key(key)
    idx = hash % @capacity
    start_idx = idx

    while @used[idx]
      if keys_equal(@keys[idx], key)
        return idx
      end
      idx = (idx + 1) % @capacity
      if idx == start_idx
        # Full circle, not found
        return -1
      end
    end

    # Note: using intermediate variable to avoid parser issue with '-1' after 'end'
    result = -1
    result
  end

  # Private: compute hash for key
  private def hash_key(key : K) : Int32
    # Simple hash - use object_id or value for primitives
    # This will be optimized later
    key.hash
  end

  # Private: compare keys for equality
  private def keys_equal(a : K, b : K) : Bool
    a == b
  end

  # Private: resize hash table (double capacity)
  private def resize
    old_keys = @keys
    old_values = @values
    old_used = @used
    old_capacity = @capacity

    @capacity = @capacity * 2
    @size = 0
    @keys = Pointer(K).malloc(@capacity)
    @values = Pointer(V).malloc(@capacity)
    @used = Pointer(Bool).malloc(@capacity)

    # Initialize new used flags
    i = 0
    while i < @capacity
      @used[i] = false
      i = i + 1
    end

    # Rehash all existing entries
    i = 0
    while i < old_capacity
      if old_used[i]
        self[old_keys[i]] = old_values[i]
      end
      i = i + 1
    end

    # TODO: free old memory when we have GC
  end

  # Private: rehash entries after deletion to maintain probe sequence
  private def rehash_after_delete(deleted_idx : Int32)
    idx = (deleted_idx + 1) % @capacity

    while @used[idx]
      key = @keys[idx]
      value = @values[idx]
      @used[idx] = false
      @size = @size - 1

      # Re-insert this entry
      self[key] = value

      idx = (idx + 1) % @capacity
      if idx == deleted_idx
        break
      end
    end
  end
end

# Add hash method to basic types
struct Int32
  def hash : Int32
    self
  end
end

struct Int64
  def hash : Int32
    # Simple stub - proper implementation needs to_i32
    # For now, this is rarely used as most hashes are Int32
    0
  end
end

struct Bool
  def hash : Int32
    if self
      1
    else
      0
    end
  end
end

class String
  def hash : Int32
    # Simple string hash (djb2 algorithm)
    h = 5381
    # TODO: iterate over chars when String iteration works
    h
  end
end
