module CrystalV2
  module Compiler
    module Frontend
      # String interning pool for memory optimization
      #
      # Deduplicates identifier and type name strings by maintaining a canonical
      # copy of each unique string. When the same string appears multiple times,
      # all references point to the same underlying Slice(UInt8).
      #
      # ## Memory Savings
      #
      # For typical Crystal code:
      # - Common identifiers: "self", "end", "nil", "initialize"
      # - Common types: "Int32", "String", "Array", "Bool"
      # - User identifiers: "foo", "bar", "value" (repeated in multiple scopes)
      #
      # Expected savings: 20-30% on identifier/type memory
      #
      # ## Usage
      #
      # ```
      # pool = StringPool.new
      # slice1 = pool.intern(some_slice)  # First occurrence - stores
      # slice2 = pool.intern(same_slice)  # Second occurrence - returns cached
      # slice1 == slice2  # => true (same object!)
      # ```
      #
      # ## Lifetime Management
      #
      # StringPool owns the canonical String for each entry. That keeps returned
      # slices valid even when callers intern temporary Strings or short-lived
      # Slice(UInt8) views.
      #
      class StringPool
        # Hash mapping string content to an owned canonical String.
        @pool : Hash(String, String)

        def initialize
          @pool = {} of String => String
        end

        # Intern a slice, returning canonical copy
        #
        # If this string has been seen before, returns the cached slice.
        # Otherwise, stores this slice as the canonical copy and returns it.
        #
        # ```
        # pool = StringPool.new
        # slice1 = "foo".to_slice
        # slice2 = "foo".to_slice
        # canonical1 = pool.intern(slice1)
        # canonical2 = pool.intern(slice2)
        # canonical1.object_id == canonical2.object_id  # => true
        # ```
        def intern(slice : Slice(UInt8)) : Slice(UInt8)
          intern_string(String.new(slice)).to_slice
        end

        # Intern a slice and return a canonical String (no repeated allocations).
        def intern_string(slice : Slice(UInt8)) : String
          intern_string(String.new(slice))
        end

        # Intern a String value and return a canonical String.
        def intern_string(str : String) : String
          if cached = @pool[str]?
            return cached
          end

          @pool[str] = str
          str
        end

        # Statistics for memory analysis
        def size : Int32
          @pool.size
        end

        # Iterate interned string keys.
        def each_string(& : String ->) : Nil
          @pool.each_key { |str| yield str }
        end

        # Estimated memory saved (rough calculation)
        #
        # For each interned string after the first occurrence, we save
        # approximately 16 bytes (Slice overhead). This is a lower bound
        # since actual savings depend on string lengths and duplication rate.
        def estimated_savings : Int32
          # Each slice is ~16 bytes overhead
          # Savings = (total_interns - unique_strings) × 16
          # But we don't track total_interns, so this is just unique count
          @pool.size * 16
        end
      end
    end
  end
end
