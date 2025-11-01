require "./type"

module CrystalV2
  module Compiler
    module Semantic
      # Represents a Range generic type: Range(B, E)
      #
      # Phase 13: Ranges
      # - Range(Int32, Int32) for 1..10 or 1...10
      # - Range(Char, Char) for 'a'..'z'
      # - Stores whether the range is exclusive (...) or inclusive (..)
      #
      # Crystal's Range is generic over begin and end types, but in practice
      # they are usually the same type. We store both for full type accuracy.
      class RangeType < Type
        getter begin_type : Type
        getter end_type : Type

        def initialize(@begin_type : Type, @end_type : Type)
        end

        def to_s(io : IO)
          io << "Range("
          @begin_type.to_s(io)
          io << ", "
          @end_type.to_s(io)
          io << ")"
        end

        def ==(other : Type) : Bool
          return false unless other.is_a?(RangeType)
          @begin_type == other.begin_type && @end_type == other.end_type
        end

        def hash : UInt64
          # Combine class identity with both type hashes
          31_u64 * 17_u64 + @begin_type.hash + @end_type.hash
        end
      end
    end
  end
end
