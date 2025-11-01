require "./type"
require "../symbol"

module CrystalV2
  module Compiler
    module Semantic
      # Represents a user-defined class type
      #
      # Uses nominal equality - two class types are equal if they reference
      # the same ClassSymbol (same class definition).
      #
      # Future: type_args will support generics (e.g., Array(Int32))
      class ClassType < Type
        getter symbol : ClassSymbol
        getter type_args : Array(Type)?

        def initialize(@symbol : ClassSymbol, @type_args : Array(Type)? = nil)
        end

        def to_s(io : IO)
          io << @symbol.name
          if args = @type_args
            io << "("
            args.join(io, ", ") { |arg, io| arg.to_s(io) }
            io << ")"
          end
        end

        def ==(other : Type) : Bool
          return false unless other.is_a?(ClassType)
          return false unless other.symbol == @symbol

          # Compare type arguments if present
          my_args = @type_args
          other_args = other.type_args

          return my_args.nil? && other_args.nil? if my_args.nil? || other_args.nil?

          return false unless my_args.size == other_args.size
          my_args.zip(other_args).all? { |a, b| a == b }
        end

        def hash : UInt64
          # Use symbol's object_id for nominal identity
          @symbol.object_id.hash
        end
      end
    end
  end
end
