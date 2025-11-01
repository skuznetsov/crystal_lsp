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
  describe "Phase 30: Accessor macros" do
    it "handles getter (returns Nil as declaration)" do
      source = <<-CRYSTAL
        class Person
          getter name
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Accessor macros are declarations, return Nil
      class_id = program.roots.first
      class_type = engine.context.get_type(class_id)
      class_type.to_s.should eq("Nil")
    end

    it "handles setter (returns Nil as declaration)" do
      source = <<-CRYSTAL
        class Person
          setter age
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      class_id = program.roots.first
      class_type = engine.context.get_type(class_id)
      class_type.to_s.should eq("Nil")
    end

    it "handles property (returns Nil as declaration)" do
      source = <<-CRYSTAL
        class Person
          property email
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      class_id = program.roots.first
      class_type = engine.context.get_type(class_id)
      class_type.to_s.should eq("Nil")
    end

    it "handles multiple accessors in class" do
      source = <<-CRYSTAL
        class Person
          getter name
          setter age
          property email
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      class_id = program.roots.first
      class_type = engine.context.get_type(class_id)
      class_type.to_s.should eq("Nil")
    end
  end
end
