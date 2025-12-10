# Crystal v2 Runtime - Memory Allocation
#
# Low-level memory allocation wrappers used by MIR codegen.
# These functions are called directly from generated LLVM IR.

module Crystal::Runtime
  # Memory allocation statistics (debug builds only)
  {% if flag?(:runtime_stats) %}
    class_property total_allocations : UInt64 = 0
    class_property total_bytes_allocated : UInt64 = 0
    class_property current_bytes : Int64 = 0
    class_property peak_bytes : Int64 = 0
  {% end %}

  # Allocate memory of given size.
  # Returns pointer to allocated memory or raises on failure.
  #
  # This is the primary allocation function called by MIR codegen.
  # For ARC objects, the caller adds 8 bytes for the RC field.
  @[AlwaysInline]
  def self.malloc(size : UInt64) : Pointer(Void)
    {% if flag?(:runtime_stats) %}
      @@total_allocations += 1
      @@total_bytes_allocated += size
      @@current_bytes += size.to_i64
      if @@current_bytes > @@peak_bytes
        @@peak_bytes = @@current_bytes
      end
    {% end %}

    ptr = LibC.malloc(size)
    if ptr.null?
      raise "Out of memory: failed to allocate #{size} bytes"
    end
    ptr
  end

  # Reallocate memory to new size.
  @[AlwaysInline]
  def self.realloc(ptr : Pointer(Void), new_size : UInt64) : Pointer(Void)
    {% if flag?(:runtime_stats) %}
      @@total_allocations += 1
      @@total_bytes_allocated += new_size
    {% end %}

    new_ptr = LibC.realloc(ptr, new_size)
    if new_ptr.null? && new_size > 0
      raise "Out of memory: failed to reallocate to #{new_size} bytes"
    end
    new_ptr
  end

  # Free allocated memory.
  @[AlwaysInline]
  def self.free(ptr : Pointer(Void)) : Nil
    return if ptr.null?

    {% if flag?(:runtime_stats) %}
      # Note: we don't track size on free, so current_bytes may drift
    {% end %}

    LibC.free(ptr)
  end

  # Zero-initialize allocated memory.
  @[AlwaysInline]
  def self.calloc(count : UInt64, size : UInt64) : Pointer(Void)
    {% if flag?(:runtime_stats) %}
      total = count * size
      @@total_allocations += 1
      @@total_bytes_allocated += total
      @@current_bytes += total.to_i64
      if @@current_bytes > @@peak_bytes
        @@peak_bytes = @@current_bytes
      end
    {% end %}

    ptr = LibC.calloc(count, size)
    if ptr.null?
      raise "Out of memory: failed to allocate #{count * size} bytes"
    end
    ptr
  end

  # Memory copy (non-overlapping regions).
  @[AlwaysInline]
  def self.memcpy(dest : Pointer(Void), src : Pointer(Void), size : UInt64) : Pointer(Void)
    LibC.memcpy(dest, src, size)
  end

  # Memory move (handles overlapping regions).
  @[AlwaysInline]
  def self.memmove(dest : Pointer(Void), src : Pointer(Void), size : UInt64) : Pointer(Void)
    LibC.memmove(dest, src, size)
  end

  # Memory set.
  @[AlwaysInline]
  def self.memset(ptr : Pointer(Void), value : Int32, size : UInt64) : Pointer(Void)
    LibC.memset(ptr, value, size)
  end

  # Print memory statistics (debug builds only).
  def self.print_stats
    {% if flag?(:runtime_stats) %}
      puts "=== Crystal Runtime Memory Stats ==="
      puts "Total allocations: #{@@total_allocations}"
      puts "Total bytes allocated: #{@@total_bytes_allocated}"
      puts "Current bytes: #{@@current_bytes}"
      puts "Peak bytes: #{@@peak_bytes}"
      puts "==================================="
    {% else %}
      puts "(Runtime stats disabled - compile with -Druntime_stats)"
    {% end %}
  end
end

# C library bindings for memory functions
lib LibC
  fun malloc(size : UInt64) : Void*
  fun realloc(ptr : Void*, size : UInt64) : Void*
  fun free(ptr : Void*) : Void
  fun calloc(count : UInt64, size : UInt64) : Void*
  fun memcpy(dest : Void*, src : Void*, size : UInt64) : Void*
  fun memmove(dest : Void*, src : Void*, size : UInt64) : Void*
  fun memset(ptr : Void*, value : Int32, size : UInt64) : Void*
end

# Exported functions for LLVM IR calls
# These use C calling convention and are the symbols referenced in llvm_backend.cr
#
# Note: We use __crystal_v2_* prefix to avoid conflicts with the standard
# Crystal runtime (__crystal_malloc, etc.). The LLVM backend should reference
# these v2 versions.

# Primary allocation function - allocates `size` bytes
fun __crystal_v2_malloc64(size : UInt64) : Void*
  Crystal::Runtime.malloc(size)
end

# Reallocate to new size
fun __crystal_v2_realloc(ptr : Void*, size : UInt64) : Void*
  Crystal::Runtime.realloc(ptr, size)
end

# Free memory
fun __crystal_v2_free(ptr : Void*) : Void
  Crystal::Runtime.free(ptr)
end

# Zero-initialized allocation
fun __crystal_v2_calloc(count : UInt64, size : UInt64) : Void*
  Crystal::Runtime.calloc(count, size)
end
