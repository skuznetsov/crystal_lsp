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
  describe "Phase 77: Variable declaration type inference" do
    describe "Class variable declarations" do
      it "infers Nil type for class variable declaration" do
        source = <<-CRYSTAL
        class Foo
          @@count : Int32
        end
        CRYSTAL

        program, analyzer, engine = infer_types(source)

        arena = program.arena
        class_node = arena[program.roots[0]]
        body = CrystalV2::Compiler::Frontend.node_class_body(class_node).not_nil!
        decl_id = body[0]

        decl_type = engine.context.get_type(decl_id)
        decl_type.should_not be_nil
        decl_type.not_nil!.to_s.should eq("Nil")
      end

      it "handles multiple class variable declarations" do
        source = <<-CRYSTAL
        class Foo
          @@count : Int32
          @@name : String
        end
        CRYSTAL

        program, analyzer, engine = infer_types(source)

        arena = program.arena
        class_node = arena[program.roots[0]]
        body = CrystalV2::Compiler::Frontend.node_class_body(class_node).not_nil!

        decl1_type = engine.context.get_type(body[0])
        decl1_type.should_not be_nil
        decl1_type.not_nil!.to_s.should eq("Nil")

        decl2_type = engine.context.get_type(body[1])
        decl2_type.should_not be_nil
        decl2_type.not_nil!.to_s.should eq("Nil")
      end
    end

    describe "Global variable declarations" do
      it "infers Nil type for global variable declaration" do
        source = "$count : Int32"

        program, analyzer, engine = infer_types(source)

        decl_id = program.roots[0]
        decl_type = engine.context.get_type(decl_id)
        decl_type.should_not be_nil
        decl_type.not_nil!.to_s.should eq("Nil")
      end

      it "handles multiple global variable declarations" do
        source = <<-CRYSTAL
        $count : Int32
        $name : String
        $flag : Bool
        CRYSTAL

        program, analyzer, engine = infer_types(source)

        decl1_type = engine.context.get_type(program.roots[0])
        decl1_type.should_not be_nil
        decl1_type.not_nil!.to_s.should eq("Nil")

        decl2_type = engine.context.get_type(program.roots[1])
        decl2_type.should_not be_nil

        decl3_type = engine.context.get_type(program.roots[2])
        decl3_type.should_not be_nil
      end
    end
  end
end
