require "./type"

module CrystalV2
  module Compiler
    module Semantic
      # Represents a union type (T | U | V)
      #
      # Union types are normalized on creation:
      # - Nested unions are flattened: (Int32 | String) | Bool → Int32 | String | Bool
      # - Duplicate types are removed: Int32 | Int32 → Int32
      # - Types are stored in canonical order for equality checking
      #
      # A union with only one constituent type is NOT a union - caller should
      # detect this and use the single type directly.
      class UnionType < Type
        getter types : Array(Type)

        # Creates a union type from constituent types
        #
        # Automatically flattens nested unions and removes duplicates.
        # Does NOT validate that types.size > 1 - caller must ensure this.
        def initialize(types : Array(Type))
          @types = UnionType.normalize(types)
        end

        # Normalizes a list of types into canonical union form
        #
        # 1. Flattens nested UnionTypes
        # 2. Removes duplicates
        # 3. Sorts by string representation for canonical order
        #
        # Returns array of normalized types (may have size 1!)
        def self.normalize(types : Array(Type)) : Array(Type)
          # Step 1: Flatten nested unions
          flattened = [] of Type
          types.each do |type|
            if type.is_a?(UnionType)
              type.types.each { |entry| flattened << entry }
            else
              flattened << type
            end
          end

          # Step 2: Remove duplicates
          unique = [] of Type
          flattened.each do |type|
            unless unique.any? { |t| t == type }
              unique << type
            end
          end

          # Step 3: Sort for canonical order
          unique.sort_by! { |t| t.to_s }

          unique
        end

        def to_s(io : IO)
          @types.join(io, " | ") { |type, io| type.to_s(io) }
        end

        def ==(other : Type) : Bool
          return false unless other.is_a?(UnionType)
          return false unless other.types.size == @types.size

          # Types are already sorted, so direct comparison works
          @types.zip(other.types).all? { |a, b| a == b }
        end

        def hash : UInt64
          # Combine hashes of constituent types
          @types.reduce(0_u64) { |acc, t| acc ^ t.hash }
        end
      end
    end
  end
end
