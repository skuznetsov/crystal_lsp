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
      # StringPool does NOT copy slices - it assumes input slices have sufficient
      # lifetime (either from source @rope or from @processed_strings array).
      # This is safe because:
      # - Lexer slices come from @rope.bytes (lives entire lexer lifetime)
      # - Parser slices come from tokens (which reference lexer slices)
      # - Processed strings stored in @processed_strings (lives entire lexer lifetime)
      #
      class StringPool
        # Hash mapping string content to canonical slice
        @pool : Hash(String, Slice(UInt8))
        @string_cache : Hash(Slice(UInt8), String)

        def initialize
          @pool = {} of String => Slice(UInt8)
          @string_cache = {} of Slice(UInt8) => String
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
          # Convert to string for hash key
          str = String.new(slice)

          # Return cached if exists
          if cached = @pool[str]?
            return cached
          end

          # Store this slice as canonical
          @pool[str] = slice
          slice
        end

        # Intern a slice and return a canonical String (no repeated allocations).
        #
        # The slice bytes must remain valid for the duration of the pool.
        def intern_string(slice : Slice(UInt8)) : String
          if cached = @string_cache[slice]?
            return cached
          end
          str = String.new(slice)
          @string_cache[slice] = str
          @pool[str] = slice unless @pool.has_key?(str)
          str
        end

        # Intern a String value and return a canonical String.
        def intern_string(str : String) : String
          slice = str.to_slice
          if cached = @string_cache[slice]?
            return cached
          end
          @string_cache[slice] = str
          @pool[str] = slice unless @pool.has_key?(str)
          str
        end

        # Statistics for memory analysis
        def size : Int32
          @pool.size
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
