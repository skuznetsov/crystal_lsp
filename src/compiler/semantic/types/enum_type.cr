require "./type"
require "../symbol"

module CrystalV2
  module Compiler
    module Semantic
      # Represents an enum type (the enum itself, not an instance)
      class EnumType < Type
        getter symbol : EnumSymbol

        def initialize(@symbol : EnumSymbol)
        end

        def to_s(io : IO)
          io << @symbol.name
        end

        def ==(other : Type) : Bool
          other.is_a?(EnumType) && other.symbol == @symbol
        end

        def hash : UInt64
          @symbol.object_id.hash
        end

        # Check if enum has a member with given name
        def has_member?(name : String) : Bool
          @symbol.members.has_key?(name)
        end

        # Get the value of a member
        def member_value(name : String) : Int64?
          @symbol.members[name]?
        end
      end
    end
  end
end
