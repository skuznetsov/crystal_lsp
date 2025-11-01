module CrystalV2
  module Compiler
    module Frontend
      # Minimal rope-like view over a string.
      class Rope
        getter bytes : Bytes

        def initialize(source : String)
          @bytes = Bytes.new(source.bytesize)
          @bytes.copy_from(source.to_slice)
        end

        def size : Int32
          bytes.size
        end

        def slice(range : Range(Int32, Int32)) : Slice(UInt8)
          bytes[range]
        end

        def to_s
          String.new(bytes)
        end
      end
    end
  end
end
