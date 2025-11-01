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
  describe "Phase 27: Modifier while/until" do
    it "handles modifier while (always returns Nil)" do
      source = <<-CRYSTAL
        x = 0
        x = x + 1 while x < 10
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Get second statement (modifier while)
      while_id = program.roots[1]
      while_type = engine.context.get_type(while_id)

      # Modifier while always returns Nil (like regular while)
      while_type.to_s.should eq("Nil")
    end

    it "handles modifier until (always returns Nil)" do
      source = <<-CRYSTAL
        x = 10
        x = x - 1 until x == 0
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Get second statement (modifier until)
      until_id = program.roots[1]
      until_type = engine.context.get_type(until_id)

      # Modifier until always returns Nil (like regular until)
      until_type.to_s.should eq("Nil")
    end

    it "handles modifier while with method call" do
      source = <<-CRYSTAL
        puts("hello") while true
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      root_id = program.roots.first
      while_type = engine.context.get_type(root_id)

      # Even though puts returns Nil, the while loop itself returns Nil
      while_type.to_s.should eq("Nil")
    end

    it "handles modifier until with method call" do
      source = <<-CRYSTAL
        puts("hello") until false
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      root_id = program.roots.first
      until_type = engine.context.get_type(root_id)

      # Even though puts returns Nil, the until loop itself returns Nil
      until_type.to_s.should eq("Nil")
    end

    it "handles modifier while with assignment returning Int32" do
      source = <<-CRYSTAL
        x = 0
        x = 10 while false
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Get second statement (modifier while)
      while_id = program.roots[1]
      while_type = engine.context.get_type(while_id)

      # Even though assignment returns Int32, while loop returns Nil
      while_type.to_s.should eq("Nil")
    end

    it "handles modifier until with assignment returning Int32" do
      source = <<-CRYSTAL
        x = 0
        x = 20 until true
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Get second statement (modifier until)
      until_id = program.roots[1]
      until_type = engine.context.get_type(until_id)

      # Even though assignment returns Int32, until loop returns Nil
      until_type.to_s.should eq("Nil")
    end

    it "handles modifier while with complex condition" do
      source = <<-CRYSTAL
        x = 0
        y = 0
        process() while x > 0 && y < 100
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Get third statement (modifier while)
      while_id = program.roots[2]
      while_type = engine.context.get_type(while_id)

      # While loop always returns Nil
      while_type.to_s.should eq("Nil")
    end
  end
end
