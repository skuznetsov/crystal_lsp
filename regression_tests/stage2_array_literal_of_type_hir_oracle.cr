module CrystalV2
  module Compiler
    module Frontend
      struct Span
      end

      abstract class Node
      end

      struct ExprId
        getter index : Int32

        def initialize(@index : Int32)
        end
      end

      struct NamedArgument
      end

      class CallNode < Node
        getter span : Span
        getter callee : ExprId
        getter args : Array(ExprId)
        @block_index : Int32
        @has_block : Bool
        @named_args_storage : Array(NamedArgument)
        @has_named_args : Bool

        def initialize(@span : Span, @callee : ExprId, @args : Array(ExprId))
          @block_index = -1
          @has_block = false
          @named_args_storage = [] of NamedArgument
          @has_named_args = false
        end
      end
    end
  end
end
