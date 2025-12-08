require "spec"

require "../../src/compiler/frontend/ast"
require "../../src/compiler/frontend/lexer"
require "../../src/compiler/frontend/parser"
require "../../src/compiler/semantic/context"
require "../../src/compiler/semantic/symbol_table"
require "../../src/compiler/semantic/collectors/symbol_collector"
require "../../src/compiler/semantic/type_inference_engine"
require "../../src/compiler/semantic/types/proc_type"
require "../../src/compiler/semantic/types/named_tuple_type"
require "../../src/compiler/semantic/types/pointer_type"

include CrystalV2::Compiler

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

  {program, engine}
end

describe "Type System" do
  # ==================================================================
  # ProcType
  # ==================================================================

  describe Semantic::ProcType do
    it "creates ProcType with params and return type" do
      int_type = Semantic::PrimitiveType.new("Int32").as(Semantic::Type)
      string_type = Semantic::PrimitiveType.new("String").as(Semantic::Type)

      proc_type = Semantic::ProcType.new([int_type, int_type], string_type)

      proc_type.arity.should eq(2)
      proc_type.param_type_at(0).should eq(int_type)
      proc_type.param_type_at(1).should eq(int_type)
      proc_type.return_type.should eq(string_type)
      proc_type.to_s.should eq("Proc(Int32, Int32, String)")
    end

    it "creates niladic proc (no parameters)" do
      nil_type = Semantic::PrimitiveType.new("Nil").as(Semantic::Type)
      proc_type = Semantic::ProcType.new([] of Semantic::Type, nil_type)

      proc_type.arity.should eq(0)
      proc_type.niladic?.should be_true
      proc_type.to_s.should eq("Proc(Nil)")
    end

    it "provides arrow string representation" do
      int_type = Semantic::PrimitiveType.new("Int32").as(Semantic::Type)
      proc_type = Semantic::ProcType.new([int_type], int_type)

      proc_type.to_arrow_s.should eq("(Int32) -> Int32")
    end

    it "compares proc types for equality" do
      int_type = Semantic::PrimitiveType.new("Int32").as(Semantic::Type)
      proc1 = Semantic::ProcType.new([int_type], int_type)
      proc2 = Semantic::ProcType.new([int_type], int_type)
      proc3 = Semantic::ProcType.new([int_type, int_type], int_type)

      (proc1 == proc2).should be_true
      (proc1 == proc3).should be_false
    end
  end

  # ==================================================================
  # NamedTupleType
  # ==================================================================

  describe Semantic::NamedTupleType do
    it "creates NamedTupleType with entries" do
      string_type = Semantic::PrimitiveType.new("String").as(Semantic::Type)
      int_type = Semantic::PrimitiveType.new("Int32").as(Semantic::Type)

      entries = [{"name", string_type}, {"age", int_type}]
      nt_type = Semantic::NamedTupleType.new(entries)

      nt_type.size.should eq(2)
      nt_type.has_key?("name").should be_true
      nt_type.has_key?("age").should be_true
      nt_type.has_key?("unknown").should be_false
      nt_type.type_for("name").should eq(string_type)
      nt_type.type_for("age").should eq(int_type)
      nt_type.to_s.should eq("NamedTuple(name: String, age: Int32)")
    end

    it "preserves key order" do
      int_type = Semantic::PrimitiveType.new("Int32").as(Semantic::Type)
      entries = [{"z", int_type}, {"a", int_type}, {"m", int_type}]
      nt_type = Semantic::NamedTupleType.new(entries)

      nt_type.keys.should eq(["z", "a", "m"])
    end

    it "compares named tuple types for equality (order matters)" do
      int_type = Semantic::PrimitiveType.new("Int32").as(Semantic::Type)

      nt1 = Semantic::NamedTupleType.new([{"a", int_type}, {"b", int_type}])
      nt2 = Semantic::NamedTupleType.new([{"a", int_type}, {"b", int_type}])
      nt3 = Semantic::NamedTupleType.new([{"b", int_type}, {"a", int_type}])

      (nt1 == nt2).should be_true
      (nt1 == nt3).should be_false  # Order matters
    end
  end

  # ==================================================================
  # PointerType
  # ==================================================================

  describe Semantic::PointerType do
    it "creates PointerType with element type" do
      int_type = Semantic::PrimitiveType.new("Int32").as(Semantic::Type)
      ptr_type = Semantic::PointerType.new(int_type)

      ptr_type.element_type.should eq(int_type)
      ptr_type.void_pointer?.should be_false
      ptr_type.to_s.should eq("Pointer(Int32)")
    end

    it "identifies void pointer" do
      void_type = Semantic::PrimitiveType.new("Void").as(Semantic::Type)
      ptr_type = Semantic::PointerType.new(void_type)

      ptr_type.void_pointer?.should be_true
      ptr_type.to_s.should eq("Pointer(Void)")
    end

    it "compares pointer types for equality" do
      int_type = Semantic::PrimitiveType.new("Int32").as(Semantic::Type)
      str_type = Semantic::PrimitiveType.new("String").as(Semantic::Type)

      ptr1 = Semantic::PointerType.new(int_type)
      ptr2 = Semantic::PointerType.new(int_type)
      ptr3 = Semantic::PointerType.new(str_type)

      (ptr1 == ptr2).should be_true
      (ptr1 == ptr3).should be_false
    end
  end

  # ==================================================================
  # Type Inference Integration
  # ==================================================================

  describe "Type inference for new types" do
    it "infers NamedTuple type from literal" do
      source = %({name: "Alice", age: 30})

      program, engine = infer_types(source)

      expr_id = program.roots[0]
      type = engine.context.get_type(expr_id)
      type.should_not be_nil
      type.not_nil!.to_s.should eq("NamedTuple(name: String, age: Int32)")
    end

    it "infers ProcType from proc literal with typed params" do
      source = %(->(x : Int32) { x * 2 })

      program, engine = infer_types(source)

      expr_id = program.roots[0]
      type = engine.context.get_type(expr_id)
      type.should_not be_nil
      type.not_nil!.to_s.should eq("Proc(Int32, Int32)")
    end

    it "infers NamedTuple with multiple entries" do
      source = %({x: 1, y: 2, z: 3})

      program, engine = infer_types(source)

      expr_id = program.roots[0]
      type = engine.context.get_type(expr_id)
      type.should_not be_nil
      type.not_nil!.to_s.should eq("NamedTuple(x: Int32, y: Int32, z: Int32)")
    end
  end

  # ==================================================================
  # Module Include/Extend
  # ==================================================================

  describe "include/extend module support" do
    it "finds method from included module" do
      source = <<-CRYSTAL
        module Greetable
          def greet : String
            "hello"
          end
        end

        class Person
          include Greetable
        end

        Person.new.greet
        CRYSTAL

      program, engine = infer_types(source)
      # Should not have errors about missing method
      engine.diagnostics.select(&.level.error?).each do |diag|
        diag.message.should_not contain("not found")
      end
    end

    it "handles module inheritance chain" do
      source = <<-CRYSTAL
        module A
          def a_method : Int32
            1
          end
        end

        module B
          include A
        end

        class C
          include B
        end

        C.new.a_method
        CRYSTAL

      program, engine = infer_types(source)
      engine.diagnostics.select { |d| d.level.error? && d.message.includes?("not found") }.should be_empty
    end

    it "extend adds class methods" do
      source = <<-CRYSTAL
        module ClassMethods
          def create : String
            "created"
          end
        end

        class Factory
          extend ClassMethods
        end

        Factory.create
        CRYSTAL

      program, engine = infer_types(source)
      engine.diagnostics.select { |d| d.level.error? && d.message.includes?("not found") }.should be_empty
    end
  end
end
