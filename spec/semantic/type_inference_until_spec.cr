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
  describe "Phase 25: Until" do
    it "handles basic until loop (always returns Nil)" do
      source = <<-CRYSTAL
        x = until false
          10
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      root_id = program.roots.first
      assign_type = engine.context.get_type(root_id)

      # Until loops always return Nil (like while)
      assign_type.to_s.should eq("Nil")
    end

    it "handles until with complex body (still returns Nil)" do
      source = <<-CRYSTAL
        x = until true
          a = 1
          b = 2
          "result"
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      root_id = program.roots.first
      assign_type = engine.context.get_type(root_id)

      # Regardless of body type, until returns Nil
      assign_type.to_s.should eq("Nil")
    end

    it "handles until with empty body" do
      source = <<-CRYSTAL
        until condition
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      root_id = program.roots.first
      until_type = engine.context.get_type(root_id)

      # Empty body still returns Nil
      until_type.to_s.should eq("Nil")
    end

    it "handles until with condition expression" do
      source = <<-CRYSTAL
        x = 0
        until x == 10
          x = x + 1
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Get second statement (until loop)
      until_id = program.roots[1]
      until_type = engine.context.get_type(until_id)

      # Until always returns Nil
      until_type.to_s.should eq("Nil")
    end

    it "handles nested until loops" do
      source = <<-CRYSTAL
        until false
          until true
            x = 1
          end
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      root_id = program.roots.first
      outer_until_type = engine.context.get_type(root_id)

      # Outer until returns Nil
      outer_until_type.to_s.should eq("Nil")
    end

    it "handles until with break statement" do
      source = <<-CRYSTAL
        until false
          break if x > 10
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      root_id = program.roots.first
      until_type = engine.context.get_type(root_id)

      # Even with break, until returns Nil
      until_type.to_s.should eq("Nil")
    end

    it "handles until mixed with other control flow" do
      source = <<-CRYSTAL
        x = 0
        until x == 10
          if x == 5
            break
          end
          x = x + 1
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Get second statement (until loop)
      until_id = program.roots[1]
      until_type = engine.context.get_type(until_id)

      # Until returns Nil
      until_type.to_s.should eq("Nil")
    end
  end
end
