require "./type"

module CrystalV2
  module Compiler
    module Semantic
      # Represents an Array generic type: Array(T)
      #
      # Phase 9: Arrays
      # - Array(Int32) for [1, 2, 3]
      # - Array(String) for ["a", "b"]
      # - Array(Int32 | String) for [1, "hello"]
      # - Array(Array(Int32)) for [[1, 2], [3, 4]]
      #
      # Uses structural equality - two array types are equal if their
      # element types are equal.
      class ArrayType < Type
        getter element_type : Type

        def initialize(@element_type : Type)
        end

        def to_s(io : IO)
          io << "Array("
          @element_type.to_s(io)
          io << ")"
        end

        def ==(other : Type) : Bool
          return false unless other.is_a?(ArrayType)
          @element_type == other.element_type
        end

        def hash : UInt64
          # Combine class identity with element type hash
          31_u64 * 17_u64 + @element_type.hash
        end
      end
    end
  end
end
