module CrystalV2
  module Compiler
    module Frontend
      # SmallVec stores up to N elements inline (StaticArray) and spills to a heap Array when exceeded.
      # Generic, minimal API for parser builders: push, size, empty?, last, each, to_a
      struct SmallVec(T, N)
        @len : Int32
        @inline : StaticArray(T, N)
        @heap : Array(T)?

        def initialize
          @len = 0
          @inline = uninitialized StaticArray(T, N)
          @heap = nil
        end

        def size : Int32
          if arr = @heap
            arr.size
          else
            @len
          end
        end

        def empty? : Bool
          size == 0
        end

        def push(value : T)
          if arr = @heap
            arr << value
            return
          end

          if @len < N
            @inline[@len] = value
            @len += 1
          else
            # Spill to heap: move inline elements
            arr2 = Array(T).new(N * 2)
            buf = @inline
            i = 0
            while i < @len
              arr2 << buf[i]
              i += 1
            end
            arr2 << value
            @heap = arr2
            # Free inline state
            @len = arr2.size
          end
        end

        def <<(value : T)
          push(value)
          self
        end

        def last : T
          if arr = @heap
            arr.last
          else
            @inline.not_nil![@len - 1]
          end
        end

        def pop : T
          if arr = @heap
            arr.pop
          else
            raise IndexError.new("SmallVec is empty") if @len == 0
            @len -= 1
            @inline.not_nil![@len]
          end
        end

        def each(&block : T ->)
          if arr = @heap
            arr.each { |e| yield e }
          else
            i = 0
            while i < @len
              yield @inline.not_nil![i]
              i += 1
            end
          end
        end

        def to_a : Array(T)
          if arr = @heap
            return arr
          end
          out = Array(T).new(@len)
          i = 0
          buf = @inline
          while i < @len
            out << buf.not_nil![i]
            i += 1
          end
          out
        end
      end
    end
  end
end
