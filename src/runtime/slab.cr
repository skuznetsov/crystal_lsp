# Crystal v2 Runtime - Slab Allocator
#
# Efficient allocator for small, fixed-size objects.
# This is a simplified version that falls back to malloc for now.
# Full slab allocation will be implemented in a future milestone.
#
# Size Classes:
#   Class 0: <= 16 bytes
#   Class 1: <= 32 bytes
#   Class 2: <= 64 bytes
#   Class 3: <= 128 bytes
#   Class 4: <= 256 bytes
#   Class 5: <= 512 bytes
#   Class 6: <= 1024 bytes
#   Class 7: <= 2048 bytes
#
# Larger allocations fall through to regular malloc.

module Crystal::Runtime
  # Number of size classes
  SLAB_SIZE_CLASSES = 8

  # Size class boundaries (upper bound for each class)
  SLAB_SIZES = StaticArray[16_u32, 32_u32, 64_u32, 128_u32, 256_u32, 512_u32, 1024_u32, 2048_u32]

  # Slab allocator statistics
  class SlabStats
    property alloc_count : UInt64 = 0
    property free_count : UInt64 = 0
    property bytes_allocated : UInt64 = 0
  end

  module Slab
    @@initialized = false
    @@stats = SlabStats.new

    # Initialize the slab allocator
    def self.init
      return if @@initialized
      @@stats = SlabStats.new
      @@initialized = true
    end

    # Shutdown slab allocator
    def self.shutdown
      @@initialized = false
    end

    # Get size class for allocation size
    @[AlwaysInline]
    def self.size_class(size : UInt32) : Int32
      SLAB_SIZE_CLASSES.times do |i|
        return i if size <= SLAB_SIZES[i]
      end
      -1  # Too large for slab allocation
    end

    # Allocate from slab (currently falls back to malloc)
    def self.alloc(size_class : Int32) : Pointer(Void)
      return Pointer(Void).null if size_class < 0 || size_class >= SLAB_SIZE_CLASSES

      init unless @@initialized

      size = SLAB_SIZES[size_class].to_u64
      ptr = LibC.malloc(size)

      if !ptr.null?
        @@stats.alloc_count += 1
        @@stats.bytes_allocated += size
      end

      ptr
    end

    # Free slab allocation (currently falls back to free)
    def self.free(ptr : Pointer(Void), size_class : Int32)
      return if ptr.null? || size_class < 0 || size_class >= SLAB_SIZE_CLASSES

      @@stats.free_count += 1
      LibC.free(ptr)
    end

    # Print slab allocator statistics
    def self.print_stats
      puts "=== Crystal Slab Allocator Stats ==="
      puts "Allocations: #{@@stats.alloc_count}"
      puts "Frees: #{@@stats.free_count}"
      puts "Outstanding: #{@@stats.alloc_count - @@stats.free_count}"
      puts "Bytes allocated: #{@@stats.bytes_allocated}"
      puts "Note: Currently using malloc fallback"
      puts "====================================="
    end
  end
end

# Exported functions for LLVM IR calls
# Note: We use __crystal_v2_* prefix to avoid conflicts with standard runtime.

# Allocate from slab (by size class)
fun __crystal_v2_slab_alloc(size_class : Int32) : Void*
  Crystal::Runtime::Slab.alloc(size_class)
end

# Free to slab
fun __crystal_v2_slab_free(ptr : Void*, size_class : Int32) : Void
  Crystal::Runtime::Slab.free(ptr, size_class)
end

# Get size class for given size
fun __crystal_v2_slab_size_class(size : UInt32) : Int32
  Crystal::Runtime::Slab.size_class(size)
end

# Initialize slab allocator
fun __crystal_v2_slab_init : Void
  Crystal::Runtime::Slab.init
end

# Shutdown slab allocator
fun __crystal_v2_slab_shutdown : Void
  Crystal::Runtime::Slab.shutdown
end
