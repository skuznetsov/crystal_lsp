require "./type"

module CrystalV2
  module Compiler
    module Semantic
      # Represents a Pointer type: Pointer(T)
      #
      # Crystal pointer types for low-level memory access and C interop:
      #   - Pointer(Int32) - pointer to Int32
      #   - Pointer(UInt8) - pointer to byte (like char*)
      #   - Pointer(Void) - void pointer (like void*)
      #
      # In Crystal syntax:
      #   - `Pointer(T)` for type annotations
      #   - `pointerof(x)` to get pointer to variable
      #   - `ptr.value` to dereference
      #   - `ptr[i]` for pointer arithmetic
      #
      # Key properties:
      #   - Unsafe: no bounds checking
      #   - Used primarily for C bindings
      #   - Supports null pointer (Pointer(T).null)
      class PointerType < Type
        getter element_type : Type

        def initialize(@element_type : Type)
        end

        # Check if this is a void pointer
        def void_pointer? : Bool
          @element_type.to_s == "Void"
        end

        def to_s(io : IO)
          io << "Pointer("
          @element_type.to_s(io)
          io << ")"
        end

        def ==(other : Type) : Bool
          return false unless other.is_a?(PointerType)
          @element_type == other.element_type
        end

        def hash : UInt64
          17_u64 * 31_u64 + @element_type.hash
        end
      end
    end
  end
end
