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

        def unsafe_fetch(index : Int32) : T
          @arr.unsafe_fetch(index)
        end

        def to_a : Array(T)
          @arr
        end
      end

      # Self-hosted release builds still corrupt growable parser buffers that
      # store ExprId wrappers directly. Keep the transient storage scalar and
      # materialize ExprId objects only once after the final size is known.
      struct ExprIdBuffer
        @indexes : Array(Int32)

        def initialize(capacity : Int32 = 0)
          @indexes = Array(Int32).new(capacity)
        end

        def size : Int32
          @indexes.size
        end

        def empty? : Bool
          @indexes.empty?
        end

        def last : ExprId
          ExprId.new(@indexes.last)
        end

        def pop : ExprId
          ExprId.new(@indexes.pop)
        end

        def push(value : ExprId)
          @indexes << value.index
        end

        def <<(value : ExprId)
          push(value)
          self
        end

        def to_a : Array(ExprId)
          Array(ExprId).new(@indexes.size) do |i|
            ExprId.new(@indexes.unsafe_fetch(i))
          end
        end
      end

      # Parameter wrappers are also fragile in growable parser buffers under
      # self-hosted release builds. Keep the growable storage reference-typed
      # and materialize the final Array(Parameter) only once.
      private class ParameterBox
        getter value : Parameter

        def initialize(@value : Parameter)
        end
      end

      struct ParameterBuffer
        @items : Array(ParameterBox)

        def initialize(capacity : Int32 = 0)
          @items = Array(ParameterBox).new(capacity)
        end

        def push(value : Parameter)
          @items << ParameterBox.new(value)
        end

        def <<(value : Parameter)
          push(value)
          self
        end

        def to_a : Array(Parameter)
          Array(Parameter).new(@items.size) do |i|
            @items.unsafe_fetch(i).value
          end
        end
      end

      # NamedArgument carries source slices and spans by value, which remains
      # fragile in self-hosted growable parser buffers. Box during collection
      # and materialize the final Array(NamedArgument) once at the end.
      private class NamedArgumentBox
        getter value : NamedArgument

        def initialize(@value : NamedArgument)
        end
      end

      struct NamedArgumentBuffer
        @items : Array(NamedArgumentBox)

        def initialize(capacity : Int32 = 0)
          @items = Array(NamedArgumentBox).new(capacity)
        end

        def size : Int32
          @items.size
        end

        def empty? : Bool
          @items.empty?
        end

        def push(value : NamedArgument)
          @items << NamedArgumentBox.new(value)
        end

        def <<(value : NamedArgument)
          push(value)
          self
        end

        def to_a : Array(NamedArgument)
          Array(NamedArgument).new(@items.size) do |i|
            @items.unsafe_fetch(i).value
          end
        end
      end

      # MacroPiece carries several nested optional/value fields and remains
      # fragile in growable self-hosted parser buffers. Box during collection
      # and materialize the final Array(MacroPiece) only once.
      private class MacroPieceBox
        getter value : MacroPiece

        def initialize(@value : MacroPiece)
        end
      end

      struct MacroPieceBuffer
        @items : Array(MacroPieceBox)

        def initialize(capacity : Int32 = 0)
          @items = Array(MacroPieceBox).new(capacity)
        end

        def size : Int32
          @items.size
        end

        def empty? : Bool
          @items.empty?
        end

        def push(value : MacroPiece)
          @items << MacroPieceBox.new(value)
        end

        def <<(value : MacroPiece)
          push(value)
          self
        end

        def each(& : MacroPiece ->)
          @items.each do |item|
            yield item.value
          end
        end

        def to_a : Array(MacroPiece)
          Array(MacroPiece).new(@items.size) do |i|
            @items.unsafe_fetch(i).value
          end
        end
      end
    end
  end
end
