require "./type"
require "../symbol"

module CrystalV2
  module Compiler
    module Semantic
      # Represents an instance of a user-defined class
      #
      # Production-ready Crystal semantics:
      # - `Dog` → ClassType (reference to class itself)
      # - `Dog.new` → InstanceType(Dog) (instance of class)
      #
      # InstanceType has instance methods, ClassType has class methods.
      #
      # Uses nominal equality - two instance types are equal if they
      # reference the same ClassSymbol.
      class InstanceType < Type
        getter class_symbol : ClassSymbol
        getter type_args : Array(Type)?

        def initialize(@class_symbol : ClassSymbol, @type_args : Array(Type)? = nil)
        end

        def to_s(io : IO)
          io << @class_symbol.name
          if args = @type_args
            io << "("
            args.join(io, ", ") { |arg, io| arg.to_s(io) }
            io << ")"
          end
        end

        def ==(other : Type) : Bool
          return false unless other.is_a?(InstanceType)
          return false unless other.class_symbol == @class_symbol

          # Compare type arguments if present
          my_args = @type_args
          other_args = other.type_args

          return my_args.nil? && other_args.nil? if my_args.nil? || other_args.nil?

          return false unless my_args.size == other_args.size
          my_args.zip(other_args).all? { |a, b| a == b }
        end

        def hash : UInt64
          # Use class_symbol's object_id for nominal identity
          @class_symbol.object_id.hash
        end
      end
    end
  end
end
