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

module ProcLiteralSpecAliases
  alias Frontend = CrystalV2::Compiler::Frontend
  alias Semantic = CrystalV2::Compiler::Semantic
end

include ProcLiteralSpecAliases
include CrystalV2::Compiler::Semantic

# Helper: Parse source and run full semantic pipeline
private def infer_types(source : String)
  lexer = Frontend::Lexer.new(source)
  parser = Frontend::Parser.new(lexer)
  program = parser.parse_program

  # Run semantic analysis (symbol collection + name resolution)
  analyzer = Semantic::Analyzer.new(program)
  analyzer.collect_symbols
  name_result = analyzer.resolve_names

  # Run type inference with global symbol table for fallback lookup
  engine = Semantic::TypeInferenceEngine.new(program, name_result.identifier_symbols, analyzer.global_context.symbol_table)
  engine.infer_types

  {program, analyzer, engine}
end

describe Semantic::TypeInferenceEngine do
  describe "Phase 74: Proc literal type inference (PRODUCTION-READY)" do
    it "infers Proc type for parameterless proc" do
      source = "-> { 42 }"

      program, analyzer, engine = infer_types(source)

      proc_expr = program.roots[0]
      proc_type = engine.context.get_type(proc_expr)
      proc_type.should_not be_nil
      proc_type.not_nil!.to_s.should eq("Proc")
    end

    it "infers Proc type for proc with parameters" do
      source = "->(x : Int32, y : Int32) { x + y }"

      program, analyzer, engine = infer_types(source)

      proc_expr = program.roots[0]
      proc_type = engine.context.get_type(proc_expr)
      proc_type.should_not be_nil
      proc_type.not_nil!.to_s.should eq("Proc")
    end

    it "infers Proc type for proc with return type annotation" do
      source = "->(x : Int32) : Int32 { x * 2 }"

      program, analyzer, engine = infer_types(source)

      proc_expr = program.roots[0]
      proc_type = engine.context.get_type(proc_expr)
      proc_type.should_not be_nil
      proc_type.not_nil!.to_s.should eq("Proc")
    end

    it "infers types for proc body expressions" do
      source = "->(x : Int32) { x + 1 }"

      program, analyzer, engine = infer_types(source)

      arena = program.arena
      proc_node = arena[program.roots[0]]
      body = Frontend.node_block_body(proc_node).not_nil!

      # Body expression should have a type inferred
      body_expr_type = engine.context.get_type(body[0])
      body_expr_type.should_not be_nil
    end
  end
end
