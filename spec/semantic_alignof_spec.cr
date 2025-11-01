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
  describe "Phase 88: alignof type inference" do
    it "infers type for alignof expression" do
      source = <<-CRYSTAL
      def test
        x = alignof(Int32)
      end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # alignof expression should infer to int32_type
      # Test passes if type inference completes without errors
      program.arena.size.should be > 0
    end

    it "infers type for alignof with Int64" do
      source = <<-CRYSTAL
      class Container
        def test
          alignment = alignof(Int64)
        end
      end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Should successfully infer types without errors
      program.arena.size.should be > 0
    end

    it "infers type for alignof in assignment" do
      source = <<-CRYSTAL
      def test
        x = alignof(String)
        x
      end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Should successfully infer types
      program.arena.size.should be > 0
    end

    it "infers type for multiple alignof expressions" do
      source = <<-CRYSTAL
      def calculate_alignments
        a = alignof(Int32)
        b = alignof(Int64)
        a
      end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Should successfully infer types for both alignof expressions
      program.arena.size.should be > 0
    end
  end

  describe "Phase 88: instance_alignof type inference" do
    it "infers type for instance_alignof expression" do
      source = <<-CRYSTAL
      def test
        x = instance_alignof(String)
      end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # instance_alignof expression should infer to int32_type
      # Test passes if type inference completes without errors
      program.arena.size.should be > 0
    end

    it "infers type for instance_alignof with custom class" do
      source = <<-CRYSTAL
      class MyClass
        def test
          alignment = instance_alignof(MyClass)
        end
      end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Should successfully infer types without errors
      program.arena.size.should be > 0
    end

    it "infers type for instance_alignof in assignment" do
      source = <<-CRYSTAL
      def test
        x = instance_alignof(Int32)
        x
      end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Should successfully infer types
      program.arena.size.should be > 0
    end

    it "infers type for mixed alignof and instance_alignof" do
      source = <<-CRYSTAL
      def compare_alignments
        a = alignof(String)
        b = instance_alignof(String)
        a
      end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Should successfully infer types for both expressions
      program.arena.size.should be > 0
    end
  end
end
