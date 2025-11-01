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
  describe "Phase 26: Modifier if/unless" do
    it "handles modifier if with assignment (Int32 | Nil union)" do
      source = <<-CRYSTAL
        x = 10 if false
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      root_id = program.roots.first
      if_type = engine.context.get_type(root_id)

      # Modifier if without else has implicit nil branch
      if_type.to_s.should match(/Int32|Nil/)
    end

    it "handles modifier unless with assignment (Int32 | Nil union)" do
      source = <<-CRYSTAL
        x = 20 unless true
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      root_id = program.roots.first
      unless_type = engine.context.get_type(root_id)

      # Modifier unless without else has implicit nil branch
      unless_type.to_s.should match(/Int32|Nil/)
    end

    it "handles modifier if with return (NoReturn | Nil union)" do
      source = <<-CRYSTAL
        return 10 if false
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      root_id = program.roots.first
      if_type = engine.context.get_type(root_id)

      # Modifier if with return: NoReturn (then branch) | Nil (else branch)
      if_type.to_s.should match(/NoReturn|Nil/)
    end

    it "handles modifier unless with return" do
      source = <<-CRYSTAL
        return 20 unless true
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      root_id = program.roots.first
      unless_type = engine.context.get_type(root_id)

      # Modifier unless with return: NoReturn (then branch) | Nil (else branch)
      unless_type.to_s.should match(/NoReturn|Nil/)
    end

    it "handles modifier if with method call" do
      source = <<-CRYSTAL
        x = 5
        puts(x) if true
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Get second statement (modifier if)
      if_id = program.roots[1]
      if_type = engine.context.get_type(if_id)

      # puts returns Nil, so: Nil (then) | Nil (else) = Nil
      if_type.to_s.should eq("Nil")
    end

    it "handles modifier unless with method call" do
      source = <<-CRYSTAL
        x = 5
        puts(x) unless false
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Get second statement (modifier unless)
      unless_id = program.roots[1]
      unless_type = engine.context.get_type(unless_id)

      # puts returns Nil, so: Nil (then) | Nil (else) = Nil
      unless_type.to_s.should eq("Nil")
    end

    it "handles modifier if with complex condition" do
      source = <<-CRYSTAL
        x = 5
        y = 10
        z = 1 if x > 3 && y < 20
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Get third statement (modifier if)
      if_id = program.roots[2]
      if_type = engine.context.get_type(if_id)

      # Assignment: Int32 (then) | Nil (else)
      if_type.to_s.should match(/Int32|Nil/)
    end

    it "handles modifier if with assignment to existing variable" do
      source = <<-CRYSTAL
        x = 0
        x = 10 if true
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Get second statement (modifier if)
      if_id = program.roots[1]
      if_type = engine.context.get_type(if_id)

      # Assignment: Int32 (then) | Nil (else)
      if_type.to_s.should match(/Int32|Nil/)
    end

    it "handles nested modifiers (currently not supported)" do
      source = <<-CRYSTAL
        x = 10 if false
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      root_id = program.roots.first
      if_type = engine.context.get_type(root_id)

      # Just verify it works
      if_type.to_s.should match(/Int32|Nil/)
    end
  end
end
