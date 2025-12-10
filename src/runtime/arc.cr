# Crystal v2 Runtime - Automatic Reference Counting (ARC)
#
# Implements reference counting for heap-allocated objects.
# The MIR codegen emits rc_inc/rc_dec calls at appropriate points.
#
# Object Layout:
# ┌─────────────────────────────────────────────────────────┐
# │                  ARC Header (16 bytes)                  │
# ├─────────────────┬─────────────────┬─────────────────────┤
# │ ref_count (8b)  │ type_id (4b)    │ flags (4b)          │
# └─────────────────┴─────────────────┴─────────────────────┘
# │                  Object Data...                         │
# └─────────────────────────────────────────────────────────┘
#
# The user pointer points to the start of Object Data.
# RC operations use negative offset to access the header.

module Crystal::Runtime
  # ARC header size in bytes
  ARC_HEADER_SIZE = 16_u64

  # Offset from user pointer to ref_count field
  RC_OFFSET = -16_i64

  # Offset from user pointer to type_id field
  TYPE_ID_OFFSET = -8_i64

  # Offset from user pointer to flags field
  FLAGS_OFFSET = -4_i64

  # Flag bits
  FLAG_WEAK_REFS   = 1_u32 << 0  # Object has weak references
  FLAG_DESTRUCTOR  = 1_u32 << 1  # Object has custom destructor
  FLAG_CYCLE_ROOT  = 1_u32 << 2  # Object is potential cycle root
  FLAG_FREED       = 1_u32 << 31 # Object has been freed (debug)

  # Reference count type (64-bit for atomic operations)
  alias RefCount = UInt64

  # Special ref count values
  RC_IMMORTAL = 0xFFFFFFFF_FFFF0000_u64  # Never freed (static objects)
  RC_STATIC   = 0xFFFFFFFF_FFFE0000_u64  # Static allocation

  # ARC statistics (debug builds only)
  {% if flag?(:arc_stats) %}
    class_property arc_increments : UInt64 = 0
    class_property arc_decrements : UInt64 = 0
    class_property arc_frees : UInt64 = 0
    class_property arc_resurrections : UInt64 = 0
  {% end %}

  # Get pointer to ref_count field from user pointer.
  @[AlwaysInline]
  def self.rc_ptr(ptr : Pointer(Void)) : Pointer(RefCount)
    (ptr.as(Pointer(UInt8)) + RC_OFFSET).as(Pointer(RefCount))
  end

  # Get pointer to type_id field from user pointer.
  @[AlwaysInline]
  def self.type_id_ptr(ptr : Pointer(Void)) : Pointer(UInt32)
    (ptr.as(Pointer(UInt8)) + TYPE_ID_OFFSET).as(Pointer(UInt32))
  end

  # Get pointer to flags field from user pointer.
  @[AlwaysInline]
  def self.flags_ptr(ptr : Pointer(Void)) : Pointer(UInt32)
    (ptr.as(Pointer(UInt8)) + FLAGS_OFFSET).as(Pointer(UInt32))
  end

  # Read current reference count.
  @[AlwaysInline]
  def self.rc_get(ptr : Pointer(Void)) : RefCount
    rc_ptr(ptr).value
  end

  # Check if object is immortal (never freed).
  @[AlwaysInline]
  def self.rc_is_immortal?(ptr : Pointer(Void)) : Bool
    rc_get(ptr) >= RC_STATIC
  end

  # Increment reference count.
  # Returns the new count.
  @[AlwaysInline]
  def self.rc_inc(ptr : Pointer(Void)) : RefCount
    return RC_IMMORTAL if ptr.null?

    {% if flag?(:arc_stats) %}
      @@arc_increments += 1
    {% end %}

    rc = rc_ptr(ptr)
    current = rc.value

    # Skip increment for immortal objects
    return current if current >= RC_STATIC

    # Increment
    new_count = current + 1

    {% if flag?(:arc_debug) %}
      if current == 0
        @@arc_resurrections += 1
        STDERR.puts "ARC WARNING: Resurrecting freed object at #{ptr}"
      end
    {% end %}

    rc.value = new_count
    new_count
  end

  # Decrement reference count.
  # If count reaches zero, calls destructor (if provided) and frees memory.
  # Returns true if object was freed.
  @[AlwaysInline]
  def self.rc_dec(ptr : Pointer(Void), destructor : Pointer(Void)) : Bool
    return false if ptr.null?

    {% if flag?(:arc_stats) %}
      @@arc_decrements += 1
    {% end %}

    rc = rc_ptr(ptr)
    current = rc.value

    # Skip decrement for immortal objects
    return false if current >= RC_STATIC

    {% if flag?(:arc_debug) %}
      if current == 0
        STDERR.puts "ARC ERROR: Double free detected at #{ptr}"
        return false
      end
    {% end %}

    new_count = current - 1
    rc.value = new_count

    if new_count == 0
      {% if flag?(:arc_stats) %}
        @@arc_frees += 1
      {% end %}

      # Call destructor if provided
      unless destructor.null?
        # Destructor signature: void (*)(void* obj)
        destructor.as(Proc(Pointer(Void), Nil)*).value.call(ptr)
      end

      {% if flag?(:arc_debug) %}
        # Mark as freed in debug mode
        flags_ptr(ptr).value |= FLAG_FREED
      {% end %}

      # Free the allocation (including header)
      actual_ptr = ptr.as(Pointer(UInt8)) + RC_OFFSET
      LibC.free(actual_ptr.as(Pointer(Void)))
      return true
    end

    false
  end

  # Allocate an ARC-managed object.
  # Returns pointer to object data (after header).
  def self.arc_alloc(size : UInt64, type_id : UInt32) : Pointer(Void)
    # Allocate header + object
    total_size = ARC_HEADER_SIZE + size
    ptr = malloc(total_size)

    # Zero the header
    memset(ptr, 0, ARC_HEADER_SIZE)

    # User pointer is after header
    user_ptr = (ptr.as(Pointer(UInt8)) + ARC_HEADER_SIZE.to_i64).as(Pointer(Void))

    # Initialize header fields
    rc_ptr(user_ptr).value = 1_u64  # Initial ref count
    type_id_ptr(user_ptr).value = type_id
    flags_ptr(user_ptr).value = 0_u32

    user_ptr
  end

  # Print ARC statistics.
  def self.print_arc_stats
    {% if flag?(:arc_stats) %}
      puts "=== Crystal ARC Stats ==="
      puts "Increments: #{@@arc_increments}"
      puts "Decrements: #{@@arc_decrements}"
      puts "Frees: #{@@arc_frees}"
      puts "Resurrections: #{@@arc_resurrections}"
      balance = @@arc_increments.to_i64 - @@arc_decrements.to_i64
      puts "Balance: #{balance} (should be >= 0)"
      puts "========================="
    {% else %}
      puts "(ARC stats disabled - compile with -Darc_stats)"
    {% end %}
  end
end

# Exported functions for LLVM IR calls
# These use C calling convention and are the symbols referenced in llvm_backend.cr
#
# Note: We use __crystal_v2_* prefix to avoid conflicts with standard runtime.

# Increment reference count
fun __crystal_v2_rc_inc(ptr : Void*) : Void
  Crystal::Runtime.rc_inc(ptr)
end

# Decrement reference count, optionally call destructor
fun __crystal_v2_rc_dec(ptr : Void*, destructor : Void*) : Void
  Crystal::Runtime.rc_dec(ptr, destructor)
end

# Allocate ARC-managed object
fun __crystal_v2_arc_alloc(size : UInt64, type_id : UInt32) : Void*
  Crystal::Runtime.arc_alloc(size, type_id)
end

# Get current reference count (for debugging)
fun __crystal_v2_rc_get(ptr : Void*) : UInt64
  Crystal::Runtime.rc_get(ptr)
end

# Check if object is valid (not freed)
fun __crystal_v2_rc_is_valid(ptr : Void*) : Bool
  return false if ptr.null?
  rc = Crystal::Runtime.rc_get(ptr)
  rc > 0 && rc < Crystal::Runtime::RC_STATIC
end
