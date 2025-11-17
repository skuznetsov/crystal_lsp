require "./type"
require "../symbol"

module CrystalV2
  module Compiler
    module Semantic
      # Represents a module constant (namespace)
      class ModuleType < Type
        getter symbol : ModuleSymbol

        def initialize(@symbol : ModuleSymbol)
        end

        def to_s(io : IO)
          io << @symbol.name
        end

        def ==(other : Type) : Bool
          other.is_a?(ModuleType) && other.symbol == @symbol
        end

        def hash : UInt64
          @symbol.object_id.hash
        end
      end
    end
  end
end
