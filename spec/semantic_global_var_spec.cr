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
  describe "Phase 75: Global variable type inference (PRODUCTION-READY)" do
    it "infers Nil type for global variable" do
      source = "$global_var"

      program, analyzer, engine = infer_types(source)

      global_expr = program.roots[0]
      global_type = engine.context.get_type(global_expr)
      global_type.should_not be_nil
      global_type.not_nil!.to_s.should eq("Nil")
    end

    it "infers types for global variable assignment" do
      source = "$count = 42"

      program, analyzer, engine = infer_types(source)

      arena = program.arena
      assign_node = arena[program.roots[0]]
      value = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!]

      value_type = engine.context.get_type(CrystalV2::Compiler::Frontend.node_assign_value(assign_node).not_nil!)
      value_type.should_not be_nil
    end

    it "infers types for global variable in expression" do
      source = "$count + 1"

      program, analyzer, engine = infer_types(source)

      binary_expr = program.roots[0]
      binary_type = engine.context.get_type(binary_expr)
      binary_type.should_not be_nil
    end

    it "infers types for multiple global variables" do
      source = <<-CRYSTAL
      $first = 1
      $second = 2
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      arena = program.arena
      assign1 = arena[program.roots[0]]
      assign2 = arena[program.roots[1]]

      value1_type = engine.context.get_type(CrystalV2::Compiler::Frontend.node_assign_value(assign1).not_nil!)
      value1_type.should_not be_nil

      value2_type = engine.context.get_type(CrystalV2::Compiler::Frontend.node_assign_value(assign2).not_nil!)
      value2_type.should_not be_nil
    end
  end
end
