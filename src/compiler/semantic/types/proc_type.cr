require "./type"

module CrystalV2
  module Compiler
    module Semantic
      # Represents a Proc type: Proc(T1, T2, ..., Tn, R)
      #
      # Crystal proc types represent callable objects with typed parameters
      # and a return type. Examples:
      #   - Proc(Int32, String) - takes Int32, returns String
      #   - Proc(Int32, Int32, Int32) - takes 2 Int32s, returns Int32
      #   - Proc(Nil) - takes nothing, returns Nil (void)
      #   - Proc(Int32, Nil) - takes Int32, returns Nil
      #
      # In Crystal syntax:
      #   - `->` for proc literals
      #   - `Proc(T1, T2, R)` for type annotations
      #   - `(T1, T2) -> R` shorthand in some contexts
      #
      # Key properties:
      #   - Parameter types are ordered
      #   - Return type is always last in type arguments
      #   - Supports splat parameters (future)
      #   - Supports block forwarding (future)
      class ProcType < Type
        getter param_types : Array(Type)
        getter return_type : Type

        def initialize(@param_types : Array(Type), @return_type : Type)
        end

        # Number of parameters (excluding return type)
        def arity : Int32
          @param_types.size
        end

        # Get parameter type at specific index
        def param_type_at(index : Int32) : Type?
          @param_types[index]?
        end

        # Check if this proc takes no parameters
        def niladic? : Bool
          @param_types.empty?
        end

        def to_s(io : IO)
          io << "Proc("
          unless @param_types.empty?
            @param_types.join(io, ", ") { |t, io_inner| t.to_s(io_inner) }
            io << ", "
          end
          @return_type.to_s(io)
          io << ")"
        end

        # Alternative representation: (T1, T2) -> R
        def to_arrow_s : String
          String.build do |io|
            io << "("
            @param_types.join(io, ", ") { |t, io_inner| t.to_s(io_inner) }
            io << ") -> "
            @return_type.to_s(io)
          end
        end

        def ==(other : Type) : Bool
          return false unless other.is_a?(ProcType)
          return false unless @param_types.size == other.param_types.size
          return false unless @return_type == other.return_type

          # Check each parameter type for equality
          @param_types.zip(other.param_types).all? { |a, b| a == b }
        end

        def hash : UInt64
          # Combine param types and return type hashes
          hash = @param_types.reduce(17_u64) { |acc, t| acc * 31_u64 + t.hash }
          hash * 31_u64 + @return_type.hash
        end
      end
    end
  end
end
