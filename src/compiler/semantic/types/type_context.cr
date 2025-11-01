require "./type"
require "./primitive_type"
require "./class_type"
require "./union_type"

module CrystalV2
  module Compiler
    module Semantic
      # TypeContext stores all type information for a program
      #
      # This is the central registry for:
      # - Built-in primitive types (Int32, String, Bool, Nil, etc.)
      # - Expression type mappings (ExprId → Type) - populated by type inference
      #
      # Stage 3 Foundation: Infrastructure only, no type inference yet.
      # Type inference algorithms will be added by GPT-5.
      class TypeContext
        # Maps ExprId → inferred Type
        # Populated by type inference engine (future work)
        getter expression_types : Hash(ExprId, Type)

        # Built-in primitive types
        getter int32_type : PrimitiveType
        getter int64_type : PrimitiveType
        getter float64_type : PrimitiveType
        getter string_type : PrimitiveType
        getter bool_type : PrimitiveType
        getter nil_type : PrimitiveType
        getter char_type : PrimitiveType
        getter symbol_type : PrimitiveType  # Phase 16
        getter proc_type : PrimitiveType  # Phase 74

        def initialize
          @expression_types = {} of ExprId => Type

          # Initialize built-in primitive types
          @int32_type = PrimitiveType.new("Int32")
          @int64_type = PrimitiveType.new("Int64")
          @float64_type = PrimitiveType.new("Float64")
          @string_type = PrimitiveType.new("String")
          @bool_type = PrimitiveType.new("Bool")
          @nil_type = PrimitiveType.new("Nil")
          @char_type = PrimitiveType.new("Char")
          @symbol_type = PrimitiveType.new("Symbol")  # Phase 16
          @proc_type = PrimitiveType.new("Proc")  # Phase 74
        end

        # Records the inferred type for an expression
        # Called by type inference engine (future work)
        def set_type(expr_id : ExprId, type : Type)
          @expression_types[expr_id] = type
        end

        # Retrieves the inferred type for an expression
        # Returns nil if type not yet inferred
        def get_type(expr_id : ExprId) : Type?
          @expression_types[expr_id]?
        end

        # Helper: Create a union type from constituent types
        #
        # Automatically normalizes and handles edge cases:
        # - Empty array → returns nil_type
        # - Single type → returns that type directly
        # - Multiple types → creates UnionType
        def union_of(types : Array(Type)) : Type
          normalized = UnionType.normalize(types)

          case normalized.size
          when 0
            @nil_type
          when 1
            normalized[0]
          else
            UnionType.new(normalized)
          end
        end

        # Helper: Create a nilable type (T | Nil)
        # Sugar for T?
        def nilable(type : Type) : Type
          union_of([type, @nil_type])
        end
      end
    end
  end
end
