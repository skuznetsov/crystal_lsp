module CrystalV2
  module Compiler
    module Frontend
      # SmallVec: V2-compatible version that always uses Array.
      # The inline StaticArray + heap spill path is broken under V2's
      # struct-as-pointer ABI (corrupted Array buffer on spill).
      # Using Array directly is safe and correct for stage2 bootstrap.
      struct SmallVec(T, N)
        @arr : Array(T)

        def initialize
          @arr = Array(T).new(N)
        end

        def size : Int32
          @arr.size
        end

        def empty? : Bool
          @arr.empty?
        end

        def push(value : T)
          @arr << value
        end

        def <<(value : T)
          push(value)
          self
        end

        def last : T
          @arr.last
        end

        def pop : T
          @arr.pop
        end

        def each(&block : T ->)
          @arr.each { |e| yield e }
        end

        def to_a : Array(T)
          @arr
        end
      end
    end
  end
end
