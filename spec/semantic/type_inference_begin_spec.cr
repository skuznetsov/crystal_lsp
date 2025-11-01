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
  describe "Phase 28: Begin/end blocks" do
    it "handles begin with integer result" do
      source = <<-CRYSTAL
        x = begin
          10
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      root_id = program.roots.first
      assign_type = engine.context.get_type(root_id)

      # Begin returns type of last expression (Int32)
      assign_type.to_s.should eq("Int32")
    end

    it "handles begin with string result" do
      source = <<-CRYSTAL
        x = begin
          "hello"
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      root_id = program.roots.first
      assign_type = engine.context.get_type(root_id)

      # Begin returns type of last expression (String)
      assign_type.to_s.should eq("String")
    end

    it "handles begin with empty body (returns Nil)" do
      source = <<-CRYSTAL
        x = begin
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      root_id = program.roots.first
      assign_type = engine.context.get_type(root_id)

      # Empty begin returns Nil
      assign_type.to_s.should eq("Nil")
    end

    it "handles begin with multiple statements (returns last type)" do
      source = <<-CRYSTAL
        x = begin
          a = 1
          b = "string"
          true
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      root_id = program.roots.first
      assign_type = engine.context.get_type(root_id)

      # Begin returns type of last expression (Bool)
      assign_type.to_s.should eq("Bool")
    end

    it "handles nested begin blocks" do
      source = <<-CRYSTAL
        x = begin
          a = 10
          begin
            b = 20
            "nested"
          end
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      root_id = program.roots.first
      assign_type = engine.context.get_type(root_id)

      # Outer begin returns type of inner begin (String)
      assign_type.to_s.should eq("String")
    end

    it "handles begin with if expression" do
      source = <<-CRYSTAL
        x = begin
          if true
            10
          else
            20
          end
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      root_id = program.roots.first
      assign_type = engine.context.get_type(root_id)

      # Begin returns type of if expression (Int32)
      assign_type.to_s.should eq("Int32")
    end

    it "handles begin as scope boundary" do
      source = <<-CRYSTAL
        outer = 1
        result = begin
          inner = 2
          outer + inner
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Get second statement (result assignment)
      assign_id = program.roots[1]
      assign_type = engine.context.get_type(assign_id)

      # Begin returns type of last expression (Int32)
      assign_type.to_s.should eq("Int32")
    end
  end
end
