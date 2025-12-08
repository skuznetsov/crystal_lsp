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
require "../../src/compiler/semantic/types/instance_type"
require "../../src/compiler/semantic/types/type_context"
require "../../src/compiler/semantic/type_inference_engine"

module TypeInferenceGenericsSpecAliases
  alias Frontend = CrystalV2::Compiler::Frontend
  alias Semantic = CrystalV2::Compiler::Semantic
end

include TypeInferenceGenericsSpecAliases
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
  describe "Week 1: Generic Type Instantiation" do

    # ========================================
    # MILESTONE 1: Basic Generic Class
    # ========================================

    describe "Generic class instantiation" do
      it "infers concrete type for generic class with explicit type args" do
        source = <<-CRYSTAL
          class Box(T)
            def initialize(value : T)
              @value = value
            end
          end

          Box(Int32).new(42)
        CRYSTAL

        program, analyzer, engine = infer_types(source)

        # Find the Box(Int32).new(42) call expression
        # It should be the last root
        call_id = program.roots.last
        type = engine.context.get_type(call_id)

        type.should be_a(InstanceType)
        instance_type = type.as(InstanceType)

        instance_type.class_symbol.name.should eq("Box")
        instance_type.type_args.should_not be_nil
        instance_type.type_args.not_nil!.size.should eq(1)
        instance_type.type_args.not_nil![0].should be_a(PrimitiveType)
        instance_type.type_args.not_nil![0].as(PrimitiveType).name.should eq("Int32")
      end

      it "infers type parameter from constructor argument (type inference)" do
        source = <<-CRYSTAL
          class Box(T)
            def initialize(value : T)
              @value = value
            end
          end

          Box.new(42)
        CRYSTAL

        program, analyzer, engine = infer_types(source)

        call_id = program.roots.last
        type = engine.context.get_type(call_id)

        type.should be_a(InstanceType)
        instance_type = type.as(InstanceType)

        # Should infer Box(Int32) from argument type
        instance_type.class_symbol.name.should eq("Box")
        instance_type.type_args.should_not be_nil
        instance_type.type_args.not_nil!.size.should eq(1)
        instance_type.type_args.not_nil![0].should be_a(PrimitiveType)
        instance_type.type_args.not_nil![0].as(PrimitiveType).name.should eq("Int32")
      end

      it "infers different type parameters for different instantiations" do
        source = <<-CRYSTAL
          class Box(T)
            def initialize(value : T)
              @value = value
            end
          end

          Box.new(42)
          Box.new("hello")
        CRYSTAL

        program, analyzer, engine = infer_types(source)

        # First call: Box.new(42) → Box(Int32)
        first_call_id = program.roots[-2]
        first_type = engine.context.get_type(first_call_id)
        first_type.should be_a(InstanceType)
        first_type.as(InstanceType).type_args.not_nil![0].as(PrimitiveType).name.should eq("Int32")

        # Second call: Box.new("hello") → Box(String)
        second_call_id = program.roots[-1]
        second_type = engine.context.get_type(second_call_id)
        second_type.should be_a(InstanceType)
        second_type.as(InstanceType).type_args.not_nil![0].as(PrimitiveType).name.should eq("String")
      end
    end

    # ========================================
    # MILESTONE 2: Generic Methods
    # ========================================

    describe "Generic class methods" do
      it "returns correct type from generic method" do
        source = <<-CRYSTAL
          class Box(T)
            def initialize(value : T)
              @value = value
            end

            def get : T
              @value
            end
          end

          box = Box.new(42)
          box.get
        CRYSTAL

        program, analyzer, engine = infer_types(source)

        # Find box.get call (should be last root)
        get_call_id = program.roots.last
        type = engine.context.get_type(get_call_id)

        # Should return Int32 (substituted from T)
        type.should be_a(PrimitiveType)
        type.as(PrimitiveType).name.should eq("Int32")
      end

      it "handles multiple type parameters" do
        source = <<-CRYSTAL
          class Pair(K, V)
            def initialize(key : K, value : V)
              @key = key
              @value = value
            end

            def key : K
              @key
            end

            def value : V
              @value
            end
          end

          pair = Pair.new("name", 42)
          pair.key
          pair.value
        CRYSTAL

        program, analyzer, engine = infer_types(source)

        # pair.key should be String
        key_call_id = program.roots[-2]
        key_type = engine.context.get_type(key_call_id)
        key_type.should be_a(PrimitiveType)
        key_type.as(PrimitiveType).name.should eq("String")

        # pair.value should be Int32
        value_call_id = program.roots[-1]
        value_type = engine.context.get_type(value_call_id)
        value_type.should be_a(PrimitiveType)
        value_type.as(PrimitiveType).name.should eq("Int32")
      end
    end

    # ========================================
    # MILESTONE 3: Generic Instance Variables
    # ========================================

    describe "Generic instance variables" do
      it "infers correct type for generic instance variable access" do
        source = <<-CRYSTAL
          class Box(T)
            def initialize(value : T)
              @value = value
            end

            def direct_value
              @value
            end
          end

          box = Box.new(42)
          box.direct_value
        CRYSTAL

        program, analyzer, engine = infer_types(source)

        # box.direct_value should return Int32
        call_id = program.roots.last
        type = engine.context.get_type(call_id)

        type.should be_a(PrimitiveType)
        type.as(PrimitiveType).name.should eq("Int32")
      end
    end

    # ========================================
    # MILESTONE 4: Nested Generics
    # ========================================

    describe "Nested generic types" do
      it "handles generic type as type argument" do
        source = <<-CRYSTAL
          class Box(T)
            def initialize(value : T)
              @value = value
            end

            def get : T
              @value
            end
          end

          class Container(T)
            def initialize(item : T)
              @item = item
            end

            def item : T
              @item
            end
          end

          inner = Box.new(42)
          outer = Container.new(inner)
          outer.item
        CRYSTAL

        program, analyzer, engine = infer_types(source)

        # outer.item should return Box(Int32)
        item_call_id = program.roots.last
        type = engine.context.get_type(item_call_id)

        type.should be_a(InstanceType)
        instance_type = type.as(InstanceType)
        instance_type.class_symbol.name.should eq("Box")
        instance_type.type_args.not_nil![0].as(PrimitiveType).name.should eq("Int32")
      end
    end

    # ========================================
    # EXECUTION_PLAN.md Week 1 Success Criteria
    # ========================================

    describe "Week 1 Success Criteria from EXECUTION_PLAN.md" do
      it "passes Box(T) example from execution plan" do
        source = <<-CRYSTAL
          class Box(T)
            def initialize(value : T)
              @value = value
            end

            def get : T
              @value
            end
          end

          box = Box.new(42)
          box.get
        CRYSTAL

        program, analyzer, engine = infer_types(source)

        # box should be Box(Int32)
        box_assignment_id = program.roots[-2]
        box_type = engine.context.get_type(box_assignment_id)
        box_type.should be_a(InstanceType)
        box_type.as(InstanceType).type_args.not_nil![0].as(PrimitiveType).name.should eq("Int32")

        # box.get should return Int32
        get_call_id = program.roots.last
        get_type = engine.context.get_type(get_call_id)
        get_type.should be_a(PrimitiveType)
        get_type.as(PrimitiveType).name.should eq("Int32")
      end
    end
  end

  describe "Week 1 Day 2: Generic Methods" do

    # ========================================
    # MILESTONE 1: Basic Generic Method
    # ========================================

    describe "Generic method with single type parameter" do
      pending "infers type parameter from argument" do
        source = <<-CRYSTAL
          def identity(x : T) : T
            x
          end

          identity(42)
        CRYSTAL

        program, analyzer, engine = infer_types(source)

        # identity(42) should return Int32
        call_id = program.roots.last
        type = engine.context.get_type(call_id)

        type.should be_a(PrimitiveType)
        type.as(PrimitiveType).name.should eq("Int32")
      end

      pending "works with different argument types" do
        source = <<-CRYSTAL
          def identity(x : T) : T
            x
          end

          identity(42)
          identity("hello")
        CRYSTAL

        program, analyzer, engine = infer_types(source)

        # First call: identity(42) → Int32
        first_call_id = program.roots[-2]
        first_type = engine.context.get_type(first_call_id)
        first_type.should be_a(PrimitiveType)
        first_type.as(PrimitiveType).name.should eq("Int32")

        # Second call: identity("hello") → String
        second_call_id = program.roots[-1]
        second_type = engine.context.get_type(second_call_id)
        second_type.should be_a(PrimitiveType)
        second_type.as(PrimitiveType).name.should eq("String")
      end
    end

    # ========================================
    # MILESTONE 2: Multiple Type Parameters
    # ========================================

    describe "Generic method with multiple type parameters" do
      pending "infers multiple type parameters from arguments" do
        source = <<-CRYSTAL
          def pair(first : T, second : U) : T
            first
          end

          pair(42, "hello")
        CRYSTAL

        program, analyzer, engine = infer_types(source)

        # pair(42, "hello") should return Int32 (type of first)
        call_id = program.roots.last
        type = engine.context.get_type(call_id)

        type.should be_a(PrimitiveType)
        type.as(PrimitiveType).name.should eq("Int32")
      end
    end

    # ========================================
    # MILESTONE 3: Generic Method with Generic Types
    # ========================================

    describe "Generic method with generic class arguments" do
      pending "infers type parameters from generic class instances" do
        source = <<-CRYSTAL
          class Box(T)
            def initialize(value : T)
              @value = value
            end

            def get : T
              @value
            end
          end

          def unwrap(box : Box(T)) : T
            box.get
          end

          box = Box.new(42)
          unwrap(box)
        CRYSTAL

        program, analyzer, engine = infer_types(source)

        # unwrap(box) should return Int32
        call_id = program.roots.last
        type = engine.context.get_type(call_id)

        type.should be_a(PrimitiveType)
        type.as(PrimitiveType).name.should eq("Int32")
      end
    end

    # ========================================
    # MILESTONE 4: Chained Generic Method Calls
    # ========================================

    describe "Chained generic method calls" do
      pending "propagates type parameters through chain" do
        source = <<-CRYSTAL
          def identity(x : T) : T
            x
          end

          def wrap(x : T) : T
            identity(x)
          end

          wrap(42)
        CRYSTAL

        program, analyzer, engine = infer_types(source)

        # wrap(42) should return Int32
        call_id = program.roots.last
        type = engine.context.get_type(call_id)

        type.should be_a(PrimitiveType)
        type.as(PrimitiveType).name.should eq("Int32")
      end
    end
  end
end
