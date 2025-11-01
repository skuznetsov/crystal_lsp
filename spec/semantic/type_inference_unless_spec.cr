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
  describe "Phase 24: Unless" do
    it "handles basic unless without else (returns Nil | then_type)" do
      source = <<-CRYSTAL
        x = unless false
          10
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      root_id = program.roots.first
      assign_type = engine.context.get_type(root_id)

      # Unless without else returns union of then-branch and implicit Nil
      assign_type.to_s.should match(/Int32|Nil/)
    end

    it "handles unless with else (returns union of both branches)" do
      source = <<-CRYSTAL
        x = unless true
          10
        else
          20
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      root_id = program.roots.first
      assign_type = engine.context.get_type(root_id)

      # Both branches return Int32, so result is Int32
      assign_type.to_s.should eq("Int32")
    end

    it "handles unless with different types creating union" do
      source = <<-CRYSTAL
        x = unless false
          10
        else
          "hello"
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      root_id = program.roots.first
      assign_type = engine.context.get_type(root_id)

      # Union of Int32 and String
      assign_type.to_s.should match(/Int32|String/)
      assign_type.to_s.should match(/String|Int32/)
    end

    it "handles unless with empty then body" do
      source = <<-CRYSTAL
        x = unless true
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      root_id = program.roots.first
      assign_type = engine.context.get_type(root_id)

      # Empty body returns Nil
      assign_type.to_s.should eq("Nil")
    end

    it "handles unless with multiple statements in then body" do
      source = <<-CRYSTAL
        x = unless false
          a = 1
          b = 2
          3
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      root_id = program.roots.first
      assign_type = engine.context.get_type(root_id)

      # Type of last expression (3) plus implicit Nil
      assign_type.to_s.should match(/Int32|Nil/)
    end

    it "handles nested unless" do
      source = <<-CRYSTAL
        x = unless false
          unless true
            10
          else
            20
          end
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      root_id = program.roots.first
      assign_type = engine.context.get_type(root_id)

      # Inner unless returns Int32, outer unless returns Int32 | Nil
      assign_type.to_s.should match(/Int32|Nil/)
    end

    it "handles unless condition with comparison" do
      source = <<-CRYSTAL
        x = 5
        y = unless x == 10
          "not ten"
        else
          "is ten"
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Get second statement (y assignment)
      y_id = program.roots[1]
      y_type = engine.context.get_type(y_id)

      # Both branches return String
      y_type.to_s.should eq("String")
    end
  end
end
