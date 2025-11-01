module CrystalV2
  module Compiler
    module Semantic
      # Base class for all types in the type system
      #
      # Stage 3: Type Inference Foundation
      # This hierarchy represents types that will be inferred for expressions.
      #
      # Type inference (algorithms, unification, constraint solving) will be
      # added by GPT-5. This file provides only the data structures.
      abstract class Type
        # Returns a human-readable representation of this type
        abstract def to_s(io : IO)

        # Two types are equal if they represent the same type
        # Subclasses must implement structural or nominal equality
        abstract def ==(other : Type) : Bool

        # Convenience wrapper for to_s
        def to_s : String
          String.build { |io| to_s(io) }
        end

        # Hash for use in Hash/Set collections
        # Subclasses should override if needed for performance
        def hash : UInt64
          to_s.hash
        end
      end
    end
  end
end
