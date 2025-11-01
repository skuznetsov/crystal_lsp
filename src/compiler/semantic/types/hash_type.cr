require "./type"

module CrystalV2
  module Compiler
    module Semantic
      # Represents a Hash generic type: Hash(K, V)
      #
      # Phase 14: Hashes
      # - Hash(String, Int32) for {"name" => 42}
      # - Hash(String, String | Int32) for {"name" => "Alice", "age" => 30}
      # - Hash(Int32, String) for {1 => "one", 2 => "two"}
      # - Hash(String, Hash(String, Int32)) for nested hashes
      #
      # Crystal's Hash is generic over key and value types. Values can be
      # union types for heterogeneous hashes.
      class HashType < Type
        getter key_type : Type
        getter value_type : Type

        def initialize(@key_type : Type, @value_type : Type)
        end

        def to_s(io : IO)
          io << "Hash("
          @key_type.to_s(io)
          io << ", "
          @value_type.to_s(io)
          io << ")"
        end

        def ==(other : Type) : Bool
          return false unless other.is_a?(HashType)
          @key_type == other.key_type && @value_type == other.value_type
        end

        def hash : UInt64
          # Combine class identity with both type hashes
          31_u64 * 17_u64 + @key_type.hash + @value_type.hash
        end
      end
    end
  end
end
