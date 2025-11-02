require "./type"

module CrystalV2
  module Compiler
    module Semantic
      # Represents an unbound type parameter (e.g., T, U, K, V)
      #
      # Week 1: Generic Type Instantiation
      #
      # TypeParameter is a temporary type used during type inference to
      # represent generic type parameters before they are bound to concrete types.
      #
      # Example:
      #   class Box(T)  # T is a TypeParameter during class definition
      #     def get : T # T appears in return type
      #       @value
      #     end
      #   end
      #
      #   Box.new(42)   # T gets bound to Int32 → InstanceType(Box, [Int32])
      #
      # Important: TypeParameter should NEVER appear in final inferred types.
      # All type parameters must be substituted with concrete types during
      # generic instantiation.
      #
      # Uses structural equality - two type parameters are equal if they
      # have the same name.
      class TypeParameter < Type
        getter name : String

        def initialize(@name : String)
        end

        def to_s(io : IO)
          io << @name
        end

        def ==(other : Type) : Bool
          return false unless other.is_a?(TypeParameter)
          @name == other.name
        end

        def hash : UInt64
          @name.hash
        end
      end
    end
  end
end
