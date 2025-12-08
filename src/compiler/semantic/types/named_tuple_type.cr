require "./type"

module CrystalV2
  module Compiler
    module Semantic
      # Represents a NamedTuple type: NamedTuple(key1: T1, key2: T2, ...)
      #
      # Crystal named tuples are like tuples but with string keys:
      #   - {name: "John", age: 30} → NamedTuple(name: String, age: Int32)
      #   - {x: 1, y: 2} → NamedTuple(x: Int32, y: Int32)
      #
      # Key properties:
      #   - Keys are compile-time string constants (symbols in syntax)
      #   - Key order matters for equality (structural)
      #   - Each key has a specific type
      #   - Immutable (no assignment to keys)
      #   - Access via [] with symbol key: nt[:name]
      class NamedTupleType < Type
        # Entries as ordered array of (key, type) pairs
        # Order is significant for equality and iteration
        getter entries : Array({String, Type})

        def initialize(@entries : Array({String, Type}))
        end

        # Convenience constructor from Hash
        def self.from_hash(hash : Hash(String, Type)) : NamedTupleType
          entries = hash.map { |k, v| {k, v} }
          new(entries)
        end

        # Number of entries
        def size : Int32
          @entries.size
        end

        # Get type for a specific key
        def type_for(key : String) : Type?
          @entries.find { |k, _| k == key }.try &.[1]
        end

        # Check if key exists
        def has_key?(key : String) : Bool
          @entries.any? { |k, _| k == key }
        end

        # Get all keys in order
        def keys : Array(String)
          @entries.map &.[0]
        end

        # Get all types in order
        def types : Array(Type)
          @entries.map &.[1]
        end

        def to_s(io : IO)
          io << "NamedTuple("
          @entries.join(io, ", ") do |(key, type), io_inner|
            io_inner << key << ": "
            type.to_s(io_inner)
          end
          io << ")"
        end

        def ==(other : Type) : Bool
          return false unless other.is_a?(NamedTupleType)
          return false unless @entries.size == other.entries.size

          # Check each entry (key and type) for equality, order matters
          @entries.zip(other.entries).all? do |(k1, t1), (k2, t2)|
            k1 == k2 && t1 == t2
          end
        end

        def hash : UInt64
          @entries.reduce(17_u64) do |acc, (key, type)|
            acc * 31_u64 + key.hash + type.hash
          end
        end
      end
    end
  end
end
