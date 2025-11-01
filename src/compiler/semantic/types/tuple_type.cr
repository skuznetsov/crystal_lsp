require "./type"

module CrystalV2
  module Compiler
    module Semantic
      # Represents a Tuple generic type: Tuple(T1, T2, ..., Tn)
      #
      # Phase 15: Tuples
      # - Tuple(Int32, String, Bool) for {1, "hello", true}
      # - Tuple(Int32) for {42} or {42,}
      # - Tuple(Int32, Tuple(String, Bool)) for {1, {"a", true}}
      #
      # Key properties:
      # - Fixed size known at compile time
      # - Each position has specific type (heterogeneous)
      # - Immutable (no element assignment)
      # - Structural equality - two tuple types are equal if they have
      #   the same number of elements and each element type is equal
      class TupleType < Type
        getter element_types : Array(Type)

        def initialize(@element_types : Array(Type))
        end

        # Number of elements in this tuple type
        def size : Int32
          @element_types.size
        end

        # Get type of element at specific index
        # Returns nil if index out of bounds
        def type_at(index : Int32) : Type?
          @element_types[index]?
        end

        def to_s(io : IO)
          io << "Tuple("
          @element_types.join(io, ", ") { |t, io_inner| t.to_s(io_inner) }
          io << ")"
        end

        def ==(other : Type) : Bool
          return false unless other.is_a?(TupleType)
          return false unless @element_types.size == other.element_types.size

          # Check each element type for equality
          @element_types.zip(other.element_types).all? { |a, b| a == b }
        end

        def hash : UInt64
          # Combine class identity with all element type hashes
          # Start with prime number, fold in each element type hash
          @element_types.reduce(17_u64) { |acc, t| acc * 31_u64 + t.hash }
        end
      end
    end
  end
end
