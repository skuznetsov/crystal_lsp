require "spec"

require "../../src/compiler/frontend/ast"
require "../../src/compiler/frontend/lexer"
require "../../src/compiler/frontend/parser"
require "../../src/compiler/semantic/symbol_table"
require "../../src/compiler/semantic/symbol"
require "../../src/compiler/semantic/collectors/symbol_collector"
require "../../src/compiler/semantic/resolvers/name_resolver"
require "../../src/compiler/semantic/analyzer"
require "../../src/compiler/semantic/types/type"
require "../../src/compiler/semantic/types/primitive_type"
require "../../src/compiler/semantic/types/class_type"
require "../../src/compiler/semantic/types/union_type"
require "../../src/compiler/semantic/types/type_context"
require "../../src/compiler/semantic/type_inference_engine"

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
  describe "Phase 29: Exception handling" do
    it "handles begin with rescue (union type)" do
      source = <<-CRYSTAL
        x = begin
          10
        rescue
          20
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      root_id = program.roots.first
      assign_type = engine.context.get_type(root_id)

      # Result is union of begin body (Int32) and rescue body (Int32) = Int32
      assign_type.to_s.should eq("Int32")
    end

    it "handles begin with rescue different types" do
      source = <<-CRYSTAL
        x = begin
          10
        rescue
          "error"
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      root_id = program.roots.first
      assign_type = engine.context.get_type(root_id)

      # Result is union of Int32 | String
      assign_type.to_s.should eq("Int32 | String")
    end

    it "handles begin with multiple rescue clauses" do
      source = <<-CRYSTAL
        x = begin
          10
        rescue RuntimeError
          20
        rescue ArgumentError
          "error"
        rescue
          true
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      root_id = program.roots.first
      assign_type = engine.context.get_type(root_id)

      # Result is union of all: Int32 | String | Bool
      assign_type.to_s.should eq("Bool | Int32 | String")
    end

    it "handles begin with ensure (ensure doesn't affect type)" do
      source = <<-CRYSTAL
        x = begin
          10
        ensure
          cleanup()
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      root_id = program.roots.first
      assign_type = engine.context.get_type(root_id)

      # Ensure doesn't affect type, result is Int32
      assign_type.to_s.should eq("Int32")
    end

    it "handles begin with rescue and ensure" do
      source = <<-CRYSTAL
        x = begin
          10
        rescue
          "error"
        ensure
          cleanup()
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      root_id = program.roots.first
      assign_type = engine.context.get_type(root_id)

      # Ensure doesn't affect type, result is union of begin and rescue
      assign_type.to_s.should eq("Int32 | String")
    end

    it "handles raise statement" do
      source = <<-CRYSTAL
        raise "error"
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      root_id = program.roots.first
      raise_type = engine.context.get_type(root_id)

      # Raise returns Nil (in real Crystal it would be NoReturn)
      raise_type.to_s.should eq("Nil")
    end

    it "handles bare raise (re-raise)" do
      source = <<-CRYSTAL
        begin
          x = 10
        rescue
          raise
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      root_id = program.roots.first
      begin_type = engine.context.get_type(root_id)

      # Begin + rescue with raise = Nil | Int32
      begin_type.to_s.should eq("Nil | Int32")
    end

    it "handles empty rescue clause" do
      source = <<-CRYSTAL
        x = begin
          10
        rescue
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      root_id = program.roots.first
      assign_type = engine.context.get_type(root_id)

      # Empty rescue returns Nil, so Nil | Int32
      assign_type.to_s.should eq("Nil | Int32")
    end

    it "handles nested begin/rescue blocks" do
      source = <<-CRYSTAL
        x = begin
          begin
            10
          rescue
            20
          end
        rescue
          "outer error"
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      root_id = program.roots.first
      assign_type = engine.context.get_type(root_id)

      # Inner: Int32 | Int32 = Int32
      # Outer: Int32 | String
      assign_type.to_s.should eq("Int32 | String")
    end
  end
end
