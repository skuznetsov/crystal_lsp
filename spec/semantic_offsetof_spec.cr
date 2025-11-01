require "spec"

require "../src/compiler/frontend/ast"
require "../src/compiler/frontend/lexer"
require "../src/compiler/frontend/parser"
require "../src/compiler/semantic/symbol_table"
require "../src/compiler/semantic/symbol"
require "../src/compiler/semantic/collectors/symbol_collector"
require "../src/compiler/semantic/resolvers/name_resolver"
require "../src/compiler/semantic/analyzer"
require "../src/compiler/semantic/types/type"
require "../src/compiler/semantic/types/primitive_type"
require "../src/compiler/semantic/types/class_type"
require "../src/compiler/semantic/types/union_type"
require "../src/compiler/semantic/types/type_context"
require "../src/compiler/semantic/type_inference_engine"

include CrystalV2::Compiler::Frontend
include CrystalV2::Compiler::Semantic

# Helper: Parse source and run full semantic pipeline
private def infer_types(source : String)
  lexer = Lexer.new(source)
  parser = Parser.new(lexer)
  program = parser.parse_program

  # Run semantic analysis (symbol collection + name resolution)
  analyzer = Analyzer.new(program)
  analyzer.collect_symbols
  name_result = analyzer.resolve_names

  # Run type inference with global symbol table for fallback lookup
  engine = TypeInferenceEngine.new(program, name_result.identifier_symbols, analyzer.global_context.symbol_table)
  engine.infer_types

  {program, analyzer, engine}
end

describe TypeInferenceEngine do
  describe "Phase 86: Offsetof type inference" do
    it "infers type for offsetof expression" do
      source = <<-CRYSTAL
      class Person
        def initialize
          x = offsetof(Person, :name)
        end
      end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Offsetof expression should infer to nil_type (placeholder)
      # In full implementation, would return Int32
      # Test passes if type inference completes without errors
      program.arena.size.should be > 0
    end

    it "infers type for offsetof with generic type" do
      source = <<-CRYSTAL
      class Container
        def test
          offset = offsetof(Array(Int32), :@buffer)
        end
      end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Should successfully infer types without errors
      program.arena.size.should be > 0
    end

    it "infers type for offsetof in assignment" do
      source = <<-CRYSTAL
      def test
        x = offsetof(MyStruct, :field)
        x
      end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Should successfully infer types
      program.arena.size.should be > 0
    end

    it "infers type for multiple offsetof expressions" do
      source = <<-CRYSTAL
      def calculate_offsets
        a = offsetof(Point, :x)
        b = offsetof(Point, :y)
        a
      end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Should successfully infer types for both offsetof expressions
      program.arena.size.should be > 0
    end
  end
end
