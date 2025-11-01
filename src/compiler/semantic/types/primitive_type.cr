require "./type"

module CrystalV2
  module Compiler
    module Semantic
      # Represents a primitive (built-in) type
      #
      # Examples: Int32, String, Bool, Nil, Float64
      #
      # Primitive types use structural equality - two Int32 are the same type
      # regardless of where they were created.
      class PrimitiveType < Type
        getter name : String

        def initialize(@name : String)
        end

        def to_s(io : IO)
          io << @name
        end

        def ==(other : Type) : Bool
          other.is_a?(PrimitiveType) && other.name == @name
        end

        def hash : UInt64
          @name.hash
        end
      end
    end
  end
end
