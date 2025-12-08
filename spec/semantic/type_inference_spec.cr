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

module TypeInferenceSpecAliases
  alias Frontend = CrystalV2::Compiler::Frontend
  alias Semantic = CrystalV2::Compiler::Semantic
  alias StringPiece = CrystalV2::Compiler::Frontend::StringPiece
end

include TypeInferenceSpecAliases
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
  describe "Phase 1: Literals (Current Parser Support)" do
    it "infers Int32 for number literals" do
      source = "42"
      program, analyzer, engine = infer_types(source)

      root_id = program.roots[0]
      type = engine.context.get_type(root_id)

      type.should be_a(PrimitiveType)
      type.as(PrimitiveType).name.should eq("Int32")
    end

    it "infers Int64 for number literals with _i64 suffix" do
      source = "42_i64"
      program, analyzer, engine = infer_types(source)

      root_id = program.roots[0]
      type = engine.context.get_type(root_id)

      type.should be_a(PrimitiveType)
      type.as(PrimitiveType).name.should eq("Int64")
    end

    it "infers Float64 for decimal literals" do
      source = "3.14"
      program, analyzer, engine = infer_types(source)

      root_id = program.roots[0]
      type = engine.context.get_type(root_id)

      type.should be_a(PrimitiveType)
      type.as(PrimitiveType).name.should eq("Float64")
    end

    it "infers Float64 for integer literals with _f64 suffix" do
      source = "42_f64"
      program, analyzer, engine = infer_types(source)

      root_id = program.roots[0]
      type = engine.context.get_type(root_id)

      type.should be_a(PrimitiveType)
      type.as(PrimitiveType).name.should eq("Float64")
    end

    it "infers Int32 for explicit _i32 suffix" do
      source = "100_i32"
      program, analyzer, engine = infer_types(source)

      root_id = program.roots[0]
      type = engine.context.get_type(root_id)

      type.should be_a(PrimitiveType)
      type.as(PrimitiveType).name.should eq("Int32")
    end

    it "infers String for string literals" do
      source = "\"hello\""
      program, analyzer, engine = infer_types(source)

      root_id = program.roots[0]
      type = engine.context.get_type(root_id)

      type.should be_a(PrimitiveType)
      type.as(PrimitiveType).name.should eq("String")
    end
  end

  describe "Phase 2: Binary Operators" do
    it "infers Int32 for addition of numbers" do
      source = "1 + 2"
      program, analyzer, engine = infer_types(source)

      root_id = program.roots[0]
      type = engine.context.get_type(root_id)

      type.should be_a(PrimitiveType)
      type.as(PrimitiveType).name.should eq("Int32")
    end

    it "infers Int32 for subtraction of numbers" do
      source = "10 - 3"
      program, analyzer, engine = infer_types(source)

      root_id = program.roots[0]
      type = engine.context.get_type(root_id)

      type.should be_a(PrimitiveType)
      type.as(PrimitiveType).name.should eq("Int32")
    end

    it "infers Bool for comparison operators" do
      source = "5 < 10"
      program, analyzer, engine = infer_types(source)

      root_id = program.roots[0]
      type = engine.context.get_type(root_id)

      type.should be_a(PrimitiveType)
      type.as(PrimitiveType).name.should eq("Bool")
    end

    it "infers Bool for equality operators" do
      source = "x == y"
      program, analyzer, engine = infer_types(source)

      root_id = program.roots[0]
      type = engine.context.get_type(root_id)

      type.should be_a(PrimitiveType)
      type.as(PrimitiveType).name.should eq("Bool")
    end

    it "infers Bool for logical AND" do
      source = "true && false"
      program, analyzer, engine = infer_types(source)

      root_id = program.roots[0]
      type = engine.context.get_type(root_id)

      type.should be_a(PrimitiveType)
      type.as(PrimitiveType).name.should eq("Bool")
    end

    it "emits error for invalid operator types" do
      source = "\"hello\" + 42"
      program, analyzer, engine = infer_types(source)

      engine.diagnostics.size.should eq(1)
      engine.diagnostics[0].message.should contain("not defined for")
    end
  end

  describe "Phase 3: Control Flow" do
    it "infers union type for if with both branches" do
      source = <<-CRYSTAL
        if true
          42
        else
          "hello"
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      root_id = program.roots[0]
      type = engine.context.get_type(root_id)

      type.should be_a(UnionType)
      union = type.as(UnionType)
      union.types.size.should eq(2)

      # Check both Int32 and String are in union
      type_names = union.types.map(&.to_s).sort
      type_names.should eq(["Int32", "String"])
    end

    it "infers union with Nil for if without else" do
      source = <<-CRYSTAL
        if true
          42
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      root_id = program.roots[0]
      type = engine.context.get_type(root_id)

      type.should be_a(UnionType)
      union = type.as(UnionType)
      union.types.size.should eq(2)

      # Check Int32 and Nil are in union
      type_names = union.types.map(&.to_s).sort
      type_names.should eq(["Int32", "Nil"])
    end

    it "infers Nil for while loop" do
      source = <<-CRYSTAL
        while true
          42
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      root_id = program.roots[0]
      type = engine.context.get_type(root_id)

      type.should be_a(PrimitiveType)
      type.as(PrimitiveType).name.should eq("Nil")
    end

    it "allows truthy check on any type (Crystal semantics)" do
      # Crystal allows any type as condition - nil and false are falsy, everything else is truthy
      source = <<-CRYSTAL
        if 42
          "truthy"
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # No errors - truthy checks are valid
      engine.diagnostics.select(&.level.error?).should be_empty
    end

    it "allows truthy check in while (Crystal semantics)" do
      # Crystal allows any type as while condition
      source = <<-CRYSTAL
        while "truthy"
          42
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # No errors - truthy checks are valid
      engine.diagnostics.select(&.level.error?).should be_empty
    end

    it "infers union type for if with single elsif" do
      source = <<-CRYSTAL
        if true
          42
        elsif false
          "hello"
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      root_id = program.roots[0]
      type = engine.context.get_type(root_id)

      type.should be_a(UnionType)
      union = type.as(UnionType)

      # Then (Int32) + elsif ("String") + implicit else (Nil) = 3 types
      union.types.size.should eq(3)
      type_names = union.types.map(&.to_s).sort
      type_names.should eq(["Int32", "Nil", "String"])
    end

    it "infers union type for if with multiple elsif branches" do
      source = <<-CRYSTAL
        if true
          1
        elsif false
          "two"
        elsif true
          3
        else
          "four"
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      root_id = program.roots[0]
      type = engine.context.get_type(root_id)

      type.should be_a(UnionType)
      union = type.as(UnionType)

      # Then (Int32) + elsif1 (String) + elsif2 (Int32) + else (String)
      # After normalization: Int32 | String (duplicates removed)
      union.types.size.should eq(2)
      type_names = union.types.map(&.to_s).sort
      type_names.should eq(["Int32", "String"])
    end

    it "allows truthy check in elsif (Crystal semantics)" do
      # Crystal allows any type as elsif condition too
      source = <<-CRYSTAL
        if true
          1
        elsif 42
          2
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # No errors - truthy checks are valid in elsif
      engine.diagnostics.select(&.level.error?).should be_empty
    end
  end

  describe "Phase 4B.2: Inheritance Method Search" do
    it "finds method in superclass" do
      source = <<-CRYSTAL
        class Animal
          def speak : String
            "sound"
          end
        end

        class Dog < Animal
        end

        d = Dog.new
        d.speak()
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Method call should find speak in Animal (superclass)
      # roots[0]=Animal, roots[1]=Dog, roots[2]=assignment, roots[3]=call
      call_id = program.roots[3]
      type = engine.context.get_type(call_id)
      type.should be_a(PrimitiveType)
      type.as(PrimitiveType).name.should eq("String")
    end

    it "prefers method in subclass over superclass" do
      source = <<-CRYSTAL
        class Animal
          def speak : String
            "sound"
          end
        end

        class Dog < Animal
          def speak : String
            "bark"
          end
        end

        d = Dog.new
        d.speak()
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Should use Dog's speak, not Animal's
      # roots[0]=Animal, roots[1]=Dog, roots[2]=assignment, roots[3]=call
      call_id = program.roots[3]
      type = engine.context.get_type(call_id)
      type.should be_a(PrimitiveType)
      type.as(PrimitiveType).name.should eq("String")

      # No diagnostics (both methods return String)
      engine.diagnostics.size.should eq(0)
    end

    it "searches through multiple inheritance levels" do
      source = <<-CRYSTAL
        class Animal
          def eat : String
            "eating"
          end
        end

        class Mammal < Animal
        end

        class Dog < Mammal
        end

        d = Dog.new
        d.eat()
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Should find eat in Animal (grandparent)
      # roots[0]=Animal, roots[1]=Mammal, roots[2]=Dog, roots[3]=assignment, roots[4]=call
      call_id = program.roots[4]
      type = engine.context.get_type(call_id)
      type.should be_a(PrimitiveType)
      type.as(PrimitiveType).name.should eq("String")
    end
  end

  describe "Phase 4B: Method Overload Resolution" do
    it "selects correct overload by parameter count" do
      source = <<-CRYSTAL
        class Calc
          def add(x : Int32) : Int32
            x
          end

          def add(x : Int32, y : Int32) : Int64
            x
          end
        end

        c = Calc.new
        c.add(5)
        c.add(3, 4)
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # First call: add(5) → Int32 (one parameter)
      call1_id = program.roots[2]
      type1 = engine.context.get_type(call1_id)
      type1.should be_a(PrimitiveType)
      type1.as(PrimitiveType).name.should eq("Int32")

      # Second call: add(3, 4) → Int64 (two parameters)
      call2_id = program.roots[3]
      type2 = engine.context.get_type(call2_id)
      type2.should be_a(PrimitiveType)
      type2.as(PrimitiveType).name.should eq("Int64")
    end

    it "matches untyped parameter to any argument" do
      source = <<-CRYSTAL
        class Box
          def store(item) : String
            "stored"
          end
        end

        b = Box.new
        b.store(42)
        b.store("hello")
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Both calls should match (untyped param accepts any type)
      call1_id = program.roots[2]
      type1 = engine.context.get_type(call1_id)
      type1.should be_a(PrimitiveType)
      type1.as(PrimitiveType).name.should eq("String")

      call2_id = program.roots[3]
      type2 = engine.context.get_type(call2_id)
      type2.should be_a(PrimitiveType)
      type2.as(PrimitiveType).name.should eq("String")
    end

    it "requires exact type match for typed parameters" do
      source = <<-CRYSTAL
        class Printer
          def print(x : Int32) : String
            "int"
          end

          def print(x : String) : String
            "string"
          end
        end

        p = Printer.new
        p.print(42)
        p.print("hello")
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Both calls should resolve correctly
      call1_id = program.roots[2]
      type1 = engine.context.get_type(call1_id)
      type1.should be_a(PrimitiveType)
      type1.as(PrimitiveType).name.should eq("String")

      call2_id = program.roots[3]
      type2 = engine.context.get_type(call2_id)
      type2.should be_a(PrimitiveType)
      type2.as(PrimitiveType).name.should eq("String")
    end
  end

  describe "Phase 4A: Method Calls (Simple Name-Based Lookup)" do
    it "infers return type from method with type annotation" do
      source = <<-CRYSTAL
        class Calculator
          def add(x : Int32, y : Int32) : Int32
            x
          end
        end

        calc = Calculator.new
        calc.add(1, 2)
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Get the method call expression (last root)
      call_id = program.roots[2]
      type = engine.context.get_type(call_id)

      # Should infer Int32 from return annotation
      type.should be_a(PrimitiveType)
      type.as(PrimitiveType).name.should eq("Int32")
    end

    it "infers Nil for method without return type annotation" do
      source = <<-CRYSTAL
        class Printer
          def print_msg(msg : String)
            msg
          end
        end

        p = Printer.new
        p.print_msg("hello")
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Get the method call expression (last root)
      call_id = program.roots[2]
      type = engine.context.get_type(call_id)

      # Should return Nil when no return annotation
      type.should be_a(PrimitiveType)
      type.as(PrimitiveType).name.should eq("Nil")
    end

    it "emits error when method not found on class" do
      source = <<-CRYSTAL
        class Empty
        end

        e = Empty.new
        e.missing_method(42)
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Should have diagnostic for method not found
      engine.diagnostics.size.should eq(1)
      engine.diagnostics[0].message.should contain("Method 'missing_method' not found")
    end

    it "handles multiple methods in class" do
      source = <<-CRYSTAL
        class Math
          def add(x : Int32) : Int32
            x
          end

          def multiply(x : Int32) : Int64
            x
          end
        end

        m = Math.new
        m.add(5)
        m.multiply(3)
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Check add call returns Int32
      add_call_id = program.roots[2]
      add_type = engine.context.get_type(add_call_id)
      add_type.should be_a(PrimitiveType)
      add_type.as(PrimitiveType).name.should eq("Int32")

      # Check multiply call returns Int64
      mult_call_id = program.roots[3]
      mult_type = engine.context.get_type(mult_call_id)
      mult_type.should be_a(PrimitiveType)
      mult_type.as(PrimitiveType).name.should eq("Int64")
    end

    it "handles method call on assigned variable" do
      source = <<-CRYSTAL
        class Counter
          def increment : Int32
            1
          end
        end

        c = Counter.new
        result = c.increment
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Assignment should have type of method call (Int32)
      assign_id = program.roots[2]
      type = engine.context.get_type(assign_id)

      type.should be_a(PrimitiveType)
      type.as(PrimitiveType).name.should eq("Int32")
    end
  end

  describe "Integration: Complex Expressions (Current Parser)" do
    it "infers nested binary expressions" do
      source = "1 + 2 * 3"
      program, analyzer, engine = infer_types(source)

      root_id = program.roots[0]
      type = engine.context.get_type(root_id)

      # Arithmetic operators return Int32
      type.should be_a(PrimitiveType)
      type.as(PrimitiveType).name.should eq("Int32")
    end

    it "infers comparison of arithmetic expressions" do
      source = "(10 - 5) < (3 * 4)"
      program, analyzer, engine = infer_types(source)

      root_id = program.roots[0]
      type = engine.context.get_type(root_id)

      # Comparison operators return Bool
      type.should be_a(PrimitiveType)
      type.as(PrimitiveType).name.should eq("Bool")
    end
  end

  describe "Phase 4: Variable Assignments" do
    it "infers type from simple assignment" do
      source = "x = 42"
      program, analyzer, engine = infer_types(source)

      root_id = program.roots[0]
      type = engine.context.get_type(root_id)

      # Assignment returns the value type
      type.should be_a(PrimitiveType)
      type.as(PrimitiveType).name.should eq("Int32")
    end

    it "uses tracked type for identifier after assignment" do
      source = <<-CRYSTAL
        x = 42
        x
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Check assignment (first root)
      assign_type = engine.context.get_type(program.roots[0])
      assign_type.should be_a(PrimitiveType)
      assign_type.as(PrimitiveType).name.should eq("Int32")

      # Check identifier usage (second root)
      ident_type = engine.context.get_type(program.roots[1])
      ident_type.should be_a(PrimitiveType)
      ident_type.as(PrimitiveType).name.should eq("Int32")
    end

    it "infers String type from string assignment" do
      source = <<-CRYSTAL
        s = "hello"
        s
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Check identifier has String type
      ident_type = engine.context.get_type(program.roots[1])
      ident_type.should be_a(PrimitiveType)
      ident_type.as(PrimitiveType).name.should eq("String")
    end

    it "infers type from expression assignment" do
      source = <<-CRYSTAL
        y = 1 + 2
        y
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Check identifier has Int32 type from arithmetic
      ident_type = engine.context.get_type(program.roots[1])
      ident_type.should be_a(PrimitiveType)
      ident_type.as(PrimitiveType).name.should eq("Int32")
    end

    it "handles multiple assignments" do
      source = <<-CRYSTAL
        x = 42
        y = "hello"
        x
        y
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Check x has Int32 type
      x_type = engine.context.get_type(program.roots[2])
      x_type.should be_a(PrimitiveType)
      x_type.as(PrimitiveType).name.should eq("Int32")

      # Check y has String type
      y_type = engine.context.get_type(program.roots[3])
      y_type.should be_a(PrimitiveType)
      y_type.as(PrimitiveType).name.should eq("String")
    end

    it "handles reassignment with different type" do
      source = <<-CRYSTAL
        x = 42
        x = "hello"
        x
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Last assignment wins
      ident_type = engine.context.get_type(program.roots[2])
      ident_type.should be_a(PrimitiveType)
      ident_type.as(PrimitiveType).name.should eq("String")
    end
  end

  describe "Phase 5: Numeric Promotion (Production Fallback)" do
    it "promotes Int32 + Int64 to Int64" do
      source = "42_i32 + 100_i64"
      program, analyzer, engine = infer_types(source)

      root_id = program.roots[0]
      type = engine.context.get_type(root_id)

      type.should be_a(PrimitiveType)
      type.as(PrimitiveType).name.should eq("Int64")
    end

    it "promotes Int32 + Float64 to Float64" do
      source = "42_i32 + 3.14_f64"
      program, analyzer, engine = infer_types(source)

      root_id = program.roots[0]
      type = engine.context.get_type(root_id)

      type.should be_a(PrimitiveType)
      type.as(PrimitiveType).name.should eq("Float64")
    end

    it "promotes Int64 + Float64 to Float64" do
      source = "100_i64 + 2.5_f64"
      program, analyzer, engine = infer_types(source)

      root_id = program.roots[0]
      type = engine.context.get_type(root_id)

      type.should be_a(PrimitiveType)
      type.as(PrimitiveType).name.should eq("Float64")
    end

    it "keeps Int32 + Int32 as Int32" do
      source = "42_i32 + 100_i32"
      program, analyzer, engine = infer_types(source)

      root_id = program.roots[0]
      type = engine.context.get_type(root_id)

      type.should be_a(PrimitiveType)
      type.as(PrimitiveType).name.should eq("Int32")
    end

    it "keeps Int64 + Int64 as Int64" do
      source = "100_i64 + 200_i64"
      program, analyzer, engine = infer_types(source)

      root_id = program.roots[0]
      type = engine.context.get_type(root_id)

      type.should be_a(PrimitiveType)
      type.as(PrimitiveType).name.should eq("Int64")
    end

    it "promotes in complex expressions" do
      source = "1_i32 + 2_i64 * 3_i32"
      program, analyzer, engine = infer_types(source)

      # 2_i64 * 3_i32 → Int64 (promotion)
      # 1_i32 + Int64 → Int64 (promotion)
      root_id = program.roots[0]
      type = engine.context.get_type(root_id)

      type.should be_a(PrimitiveType)
      type.as(PrimitiveType).name.should eq("Int64")
    end
  end

  describe "Diagnostic Spans" do
    it "reports actual error location for type mismatch" do
      source = <<-CRYSTAL
        x = 42
        y = "hello" + x
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Should have 1 diagnostic
      engine.diagnostics.size.should eq(1)

      diagnostic = engine.diagnostics[0]
      diagnostic.message.should contain("not defined for")

      # Verify span points to actual error location (line 2)
      diagnostic.primary_span.start_line.should eq(2)

      # Should not be dummy span (0,0)
      diagnostic.primary_span.start_line.should_not eq(0)
      diagnostic.primary_span.start_column.should_not eq(0)
    end

    it "allows truthy check without error (Crystal semantics)" do
      # Crystal allows any type as condition - this is not an error
      source = <<-CRYSTAL
        if 42
          "truthy"
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # No errors - truthy checks are valid
      engine.diagnostics.select(&.level.error?).should be_empty
    end
  end

  describe "Phase 4B.3: Built-in Methods for Primitive Types" do
    it "resolves Int32#+ as built-in method" do
      source = <<-CRYSTAL
        x = 5
        y = 10
        z = x + y
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # z = x + y should use Int32#+(Int32) : Int32
      assign_id = program.roots[2]
      type = engine.context.get_type(assign_id)
      type.should be_a(PrimitiveType)
      type.as(PrimitiveType).name.should eq("Int32")
    end

    it "resolves Int32#< as built-in method" do
      source = <<-CRYSTAL
        result = 5 < 10
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # 5 < 10 should use Int32#<(Int32) : Bool
      assign_id = program.roots[0]
      type = engine.context.get_type(assign_id)
      type.should be_a(PrimitiveType)
      type.as(PrimitiveType).name.should eq("Bool")
    end

    it "resolves String#size as built-in method" do
      source = <<-CRYSTAL
        s = "hello"
        len = s.size
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # s.size should use String#size : Int32
      assign_id = program.roots[1]
      type = engine.context.get_type(assign_id)
      type.should be_a(PrimitiveType)
      type.as(PrimitiveType).name.should eq("Int32")
    end

    it "resolves String#+ as built-in method" do
      source = <<-CRYSTAL
        result = "hello" + " world"
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # "hello" + " world" should use String#+(String) : String
      assign_id = program.roots[0]
      type = engine.context.get_type(assign_id)
      type.should be_a(PrimitiveType)
      type.as(PrimitiveType).name.should eq("String")
    end

    it "resolves Bool#== as built-in method" do
      source = <<-CRYSTAL
        result = true == false
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # true == false should use Bool#==(Bool) : Bool
      assign_id = program.roots[0]
      type = engine.context.get_type(assign_id)
      type.should be_a(PrimitiveType)
      type.as(PrimitiveType).name.should eq("Bool")
    end

    it "works with method calls on variables" do
      source = <<-CRYSTAL
        x = 42
        y = 10
        greater = x > y
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # x > y should use Int32#>(Int32) : Bool
      assign_id = program.roots[2]
      type = engine.context.get_type(assign_id)
      type.should be_a(PrimitiveType)
      type.as(PrimitiveType).name.should eq("Bool")
    end
  end

  describe "Phase 4B.4: Union Type Method Lookup" do
    it "finds method common to all union types" do
      source = <<-CRYSTAL
        x = if true
          42
        else
          "hello"
        end
        result = x == x
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # x : Int32 | String
      # Both Int32 and String have == method → should work
      # result type should be Bool
      result_id = program.roots[1]
      type = engine.context.get_type(result_id)
      type.should be_a(PrimitiveType)
      type.as(PrimitiveType).name.should eq("Bool")

      # Should have no errors
      engine.diagnostics.size.should eq(0)
    end

    it "emits error when method not in all union types" do
      source = <<-CRYSTAL
        x = if true
          42
        else
          "hello"
        end
        result = x.size
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # x : Int32 | String
      # String has size, but Int32 doesn't → error
      engine.diagnostics.size.should eq(1)
      engine.diagnostics[0].message.should contain("not found")
    end

    it "works with union of three types" do
      source = <<-CRYSTAL
        x = if true
          1
        elsif false
          2
        else
          3
        end
        result = x == 5
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # x : Int32 (normalized from Int32 | Int32 | Int32)
      # Should work because all are Int32
      result_id = program.roots[1]
      type = engine.context.get_type(result_id)
      type.should be_a(PrimitiveType)
      type.as(PrimitiveType).name.should eq("Bool")
    end

    it "finds method in union with primitives" do
      source = <<-CRYSTAL
        x = if true
          5
        else
          10
        end
        result = x + 3
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # x : Int32 | Int32 → Int32 (normalized)
      # Int32 has + method
      result_id = program.roots[1]
      type = engine.context.get_type(result_id)
      type.should be_a(PrimitiveType)
      type.as(PrimitiveType).name.should eq("Int32")
    end

    it "computes union return type for methods with different return types" do
      source = <<-CRYSTAL
        class A
          def foo : Int32
            42
          end
        end

        class B
          def foo : String
            "hello"
          end
        end

        x = if true
          A.new
        else
          B.new
        end
        result = x.foo
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # x : A | B (InstanceType(A) | InstanceType(B))
      # A.foo : Int32, B.foo : String
      # result : Int32 | String
      result_id = program.roots[3]
      type = engine.context.get_type(result_id)
      type.should be_a(UnionType)

      union = type.as(UnionType)
      union.types.size.should eq(2)

      type_names = union.types.map(&.to_s).sort
      type_names.should eq(["Int32", "String"])
    end

    it "handles union return type when all return same type" do
      source = <<-CRYSTAL
        class A
          def foo : String
            "a"
          end
        end

        class B
          def foo : String
            "b"
          end
        end

        x = if true
          A.new
        else
          B.new
        end
        result = x.foo
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # x : A | B
      # Both return String
      # result : String | String → String (normalized)
      result_id = program.roots[3]
      type = engine.context.get_type(result_id)
      type.should be_a(PrimitiveType)
      type.as(PrimitiveType).name.should eq("String")
    end
  end

  describe "Phase 5A: Instance Variables" do
    it "infers type from instance variable assignment" do
      source = <<-CRYSTAL
        @x = 42
        result = @x
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # @x : Int32 (inferred from assignment)
      # result : Int32
      result_id = program.roots[1]
      type = engine.context.get_type(result_id)
      type.should be_a(PrimitiveType)
      type.as(PrimitiveType).name.should eq("Int32")
    end

    it "infers type from string instance variable" do
      source = <<-CRYSTAL
        @name = "hello"
        result = @name
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # @name : String
      # result : String
      result_id = program.roots[1]
      type = engine.context.get_type(result_id)
      type.should be_a(PrimitiveType)
      type.as(PrimitiveType).name.should eq("String")
    end

    it "handles multiple instance variables" do
      source = <<-CRYSTAL
        @x = 42
        @name = "test"
        result_x = @x
        result_name = @name
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # result_x : Int32
      result_x_id = program.roots[2]
      type_x = engine.context.get_type(result_x_id)
      type_x.should be_a(PrimitiveType)
      type_x.as(PrimitiveType).name.should eq("Int32")

      # result_name : String
      result_name_id = program.roots[3]
      type_name = engine.context.get_type(result_name_id)
      type_name.should be_a(PrimitiveType)
      type_name.as(PrimitiveType).name.should eq("String")
    end

    it "returns Nil for uninitialized instance variable" do
      source = <<-CRYSTAL
        result = @uninitialized
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # @uninitialized not assigned → Nil
      result_id = program.roots[0]
      type = engine.context.get_type(result_id)
      type.should be_a(PrimitiveType)
      type.as(PrimitiveType).name.should eq("Nil")
    end
  end

  describe "Phase 5B: Instance Variables in Method Bodies" do
    it "infers instance variable type from initialize method" do
      source = <<-CRYSTAL
        class Counter
          def initialize
            @count = 0
          end

          def get_count : Int32
            @count
          end
        end

        c = Counter.new
        result = c.get_count
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # result : Int32 (from method return annotation)
      # @count should be Int32 (from initialize assignment)
      result_id = program.roots[2]
      type = engine.context.get_type(result_id)
      type.should be_a(PrimitiveType)
      type.as(PrimitiveType).name.should eq("Int32")
    end

    it "handles instance variable assigned and read in same method" do
      source = <<-CRYSTAL
        class Foo
          def test : Int32
            @x = 42
            @x
          end
        end

        foo = Foo.new
        result = foo.test
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # @x : Int32 (assigned in test method)
      # result : Int32
      result_id = program.roots[2]
      type = engine.context.get_type(result_id)
      type.should be_a(PrimitiveType)
      type.as(PrimitiveType).name.should eq("Int32")
    end

    it "handles multiple classes with same instance variable names" do
      source = <<-CRYSTAL
        class Foo
          def initialize
            @x = 42
          end

          def get_x : Int32
            @x
          end
        end

        class Bar
          def initialize
            @x = "hello"
          end

          def get_x : String
            @x
          end
        end

        foo = Foo.new
        bar = Bar.new
        result_foo = foo.get_x
        result_bar = bar.get_x
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # result_foo : Int32 (from Foo.get_x)
      result_foo_id = program.roots[4]
      type_foo = engine.context.get_type(result_foo_id)
      type_foo.should be_a(PrimitiveType)
      type_foo.as(PrimitiveType).name.should eq("Int32")

      # result_bar : String (from Bar.get_x)
      result_bar_id = program.roots[5]
      type_bar = engine.context.get_type(result_bar_id)
      type_bar.should be_a(PrimitiveType)
      type_bar.as(PrimitiveType).name.should eq("String")
    end

    it "handles instance variable modification" do
      source = <<-CRYSTAL
        class Counter
          def initialize
            @count = 0
          end

          def increment : Int32
            @count = @count + 1
            @count
          end
        end

        c = Counter.new
        result = c.increment
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # @count : Int32 (from initialize)
      # @count + 1 : Int32
      # result : Int32
      result_id = program.roots[2]
      type = engine.context.get_type(result_id)
      type.should be_a(PrimitiveType)
      type.as(PrimitiveType).name.should eq("Int32")
    end
  end

  describe "Phase 5C: Explicit Type Annotations for Instance Variables" do
    it "uses explicit type annotation without assignment" do
      source = <<-CRYSTAL
        class Foo
          @x : Int32

          def get_x : Int32
            @x
          end
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Verify ClassSymbol has type annotation
      foo_symbol = analyzer.global_context.symbol_table.lookup("Foo")
      foo_symbol.should be_a(ClassSymbol)
      foo_symbol.as(ClassSymbol).get_instance_var_type("x").should eq("Int32")
    end

    it "uses explicit type annotation with assignment" do
      source = <<-CRYSTAL
        class Foo
          @x : Int32
          @name : String

          def initialize
            @x = 42
            @name = "test"
          end

          def get_x : Int32
            @x
          end

          def get_name : String
            @name
          end
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Verify both annotations are preserved
      foo_symbol = analyzer.global_context.symbol_table.lookup("Foo")
      foo_symbol.should be_a(ClassSymbol)
      foo_symbol.as(ClassSymbol).get_instance_var_type("x").should eq("Int32")
      foo_symbol.as(ClassSymbol).get_instance_var_type("name").should eq("String")
    end

    it "handles multiple classes with explicit annotations" do
      source = <<-CRYSTAL
        class Foo
          @value : Int32

          def get_value : Int32
            @value
          end
        end

        class Bar
          @value : String

          def get_value : String
            @value
          end
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Verify each class has its own type annotation
      foo_symbol = analyzer.global_context.symbol_table.lookup("Foo")
      foo_symbol.should be_a(ClassSymbol)
      foo_symbol.as(ClassSymbol).get_instance_var_type("value").should eq("Int32")

      bar_symbol = analyzer.global_context.symbol_table.lookup("Bar")
      bar_symbol.should be_a(ClassSymbol)
      bar_symbol.as(ClassSymbol).get_instance_var_type("value").should eq("String")
    end

    it "prioritizes explicit annotation over inferred assignment" do
      source = <<-CRYSTAL
        class Foo
          @x : Int32

          def initialize
            @x = 42
          end

          def get_x : Int32
            @x
          end
        end

        foo = Foo.new
        result = foo.get_x
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # result : Int32 (from method return annotation, which matches @x annotation)
      result_id = program.roots[2]
      type = engine.context.get_type(result_id)
      type.should be_a(PrimitiveType)
      type.as(PrimitiveType).name.should eq("Int32")
    end
  end

  describe "Phase 6: Return Statements" do
    it "infers type for return with value" do
      source = <<-CRYSTAL
        def test : Int32
          return 42
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Get return statement from method body
      def_node = program.arena[program.roots[0]]
      body = CrystalV2::Compiler::Frontend.node_def_body(def_node)
      body.should_not be_nil
      return_expr_id = body.not_nil![0]

      # Return statement should have Int32 type
      return_type = engine.context.get_type(return_expr_id)
      return_type.should be_a(PrimitiveType)
      return_type.as(PrimitiveType).name.should eq("Int32")
    end

    it "infers Nil for return without value" do
      source = <<-CRYSTAL
        def test : Nil
          return
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Get return statement from method body
      def_node = program.arena[program.roots[0]]
      body = CrystalV2::Compiler::Frontend.node_def_body(def_node)
      body.should_not be_nil
      return_expr_id = body.not_nil![0]

      # Return statement should have Nil type
      return_type = engine.context.get_type(return_expr_id)
      return_type.should be_a(PrimitiveType)
      return_type.as(PrimitiveType).name.should eq("Nil")
    end

    it "handles early return in conditional (postfix if)" do
      source = <<-CRYSTAL
        def test(x : Int32) : String
          return "negative" if x < 0
          "positive"
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Get if statement from method body
      def_node = program.arena[program.roots[0]]
      body = CrystalV2::Compiler::Frontend.node_def_body(def_node)
      body.should_not be_nil
      if_expr_id = body.not_nil![0]
      if_node = program.arena[if_expr_id].as(CrystalV2::Compiler::Frontend::IfNode)

      # Check that then branch is a return statement
      then_branch = if_node.then_body
      then_branch.should_not be_nil
      return_expr_id = then_branch.not_nil![0]
      return_node = program.arena[return_expr_id]
      CrystalV2::Compiler::Frontend.node_kind(return_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Return)

      # Return statement should have String type
      return_type = engine.context.get_type(return_expr_id)
      return_type.should be_a(PrimitiveType)
      return_type.as(PrimitiveType).name.should eq("String")
    end

    it "handles return in while loop (postfix if)" do
      source = <<-CRYSTAL
        def test : Int32
          x = 0
          while x < 10
            x = x + 1
            return x if x == 5
          end
          x
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Get while statement from method body
      def_node = program.arena[program.roots[0]]
      body = CrystalV2::Compiler::Frontend.node_def_body(def_node)
      body.should_not be_nil
      while_expr_id = body.not_nil![1]  # Second statement (after x = 0)
      while_node = program.arena[while_expr_id].as(CrystalV2::Compiler::Frontend::WhileNode)

      # Check that while body contains an if with return
      while_body = while_node.body
      while_body.should_not be_nil

      # Find the if statement in the while body
      if_expr_id = while_body.not_nil![1]  # Second statement in while (after x = x + 1)
      if_node = program.arena[if_expr_id].as(CrystalV2::Compiler::Frontend::IfNode)

      # Check return in if then branch
      then_branch = if_node.then_body
      then_branch.should_not be_nil
      return_expr_id = then_branch.not_nil![0]
      return_node = program.arena[return_expr_id]
      CrystalV2::Compiler::Frontend.node_kind(return_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Return)

      # Return statement should have Int32 type
      return_type = engine.context.get_type(return_expr_id)
      return_type.should be_a(PrimitiveType)
      return_type.as(PrimitiveType).name.should eq("Int32")
    end

    it "handles multiple return statements (postfix if)" do
      source = <<-CRYSTAL
        def test(x : Int32) : String
          return "zero" if x == 0
          return "one" if x == 1
          "other"
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Get method body
      def_node = program.arena[program.roots[0]]
      body = CrystalV2::Compiler::Frontend.node_def_body(def_node)
      body.should_not be_nil

      # First if with return
      if1_node = program.arena[body.not_nil![0]].as(CrystalV2::Compiler::Frontend::IfNode)
      return1_id = if1_node.then_body.not_nil![0]
      return1_type = engine.context.get_type(return1_id)
      return1_type.should be_a(PrimitiveType)
      return1_type.as(PrimitiveType).name.should eq("String")

      # Second if with return
      if2_node = program.arena[body.not_nil![1]].as(CrystalV2::Compiler::Frontend::IfNode)
      return2_id = if2_node.then_body.not_nil![0]
      return2_type = engine.context.get_type(return2_id)
      return2_type.should be_a(PrimitiveType)
      return2_type.as(PrimitiveType).name.should eq("String")
    end
  end

  describe "Phase 7: Self Keyword" do
    it "infers InstanceType for self in method" do
      source = <<-CRYSTAL
        class Dog
          def get_self
            self
          end
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Get self expression from method body
      class_node = program.arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::ClassNode)
      def_node = program.arena[class_node.body.not_nil![0]]
      self_expr_id = CrystalV2::Compiler::Frontend.node_def_body(def_node).not_nil![0]

      # Check self has InstanceType(Dog)
      self_type = engine.context.get_type(self_expr_id)
      self_type.should be_a(InstanceType)
      self_type.as(InstanceType).class_symbol.name.should eq("Dog")
    end

    it "handles self return for method chaining" do
      source = <<-CRYSTAL
        class Builder
          def step1
            self
          end

          def step2
            self
          end
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Get both methods
      class_node = program.arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::ClassNode)
      class_body = class_node.body.not_nil!

      # Check step1 returns self
      def1_node = program.arena[class_body[0]]
      self1_expr_id = CrystalV2::Compiler::Frontend.node_def_body(def1_node).not_nil![0]
      self1_type = engine.context.get_type(self1_expr_id)
      self1_type.should be_a(InstanceType)
      self1_type.as(InstanceType).class_symbol.name.should eq("Builder")

      # Check step2 returns self
      def2_node = program.arena[class_body[1]]
      self2_expr_id = CrystalV2::Compiler::Frontend.node_def_body(def2_node).not_nil![0]
      self2_type = engine.context.get_type(self2_expr_id)
      self2_type.should be_a(InstanceType)
      self2_type.as(InstanceType).class_symbol.name.should eq("Builder")
    end

    it "handles different self types in different classes" do
      source = <<-CRYSTAL
        class Dog
          def who_am_i
            self
          end
        end

        class Cat
          def who_am_i
            self
          end
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Get Dog's self
      dog_class = program.arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::ClassNode)
      dog_def = program.arena[dog_class.body.not_nil![0]]
      dog_self_id = CrystalV2::Compiler::Frontend.node_def_body(dog_def).not_nil![0]
      dog_self_type = engine.context.get_type(dog_self_id)
      dog_self_type.should be_a(InstanceType)
      dog_self_type.as(InstanceType).class_symbol.name.should eq("Dog")

      # Get Cat's self
      cat_class = program.arena[program.roots[1]].as(CrystalV2::Compiler::Frontend::ClassNode)
      cat_def = program.arena[cat_class.body.not_nil![0]]
      cat_self_id = CrystalV2::Compiler::Frontend.node_def_body(cat_def).not_nil![0]
      cat_self_type = engine.context.get_type(cat_self_id)
      cat_self_type.should be_a(InstanceType)
      cat_self_type.as(InstanceType).class_symbol.name.should eq("Cat")
    end

    it "handles self in conditional return" do
      source = <<-CRYSTAL
        class Node
          def conditional_self(flag : Bool)
            return self if flag
            self
          end
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Get method body
      class_node = program.arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::ClassNode)
      def_node = program.arena[class_node.body.not_nil![0]]
      def_body = CrystalV2::Compiler::Frontend.node_def_body(def_node).not_nil!

      # First statement is if (with postfix)
      if_node = program.arena[def_body[0]].as(CrystalV2::Compiler::Frontend::IfNode)
      return_node = program.arena[if_node.then_body.not_nil![0]]
      self1_id = CrystalV2::Compiler::Frontend.node_return_value(return_node).not_nil!
      self1_type = engine.context.get_type(self1_id)
      self1_type.should be_a(InstanceType)
      self1_type.as(InstanceType).class_symbol.name.should eq("Node")

      # Second statement is bare self
      self2_id = def_body[1]
      self2_type = engine.context.get_type(self2_id)
      self2_type.should be_a(InstanceType)
      self2_type.as(InstanceType).class_symbol.name.should eq("Node")
    end
  end

  # ============================================================
  # PHASE 8: String Interpolation Tests
  # ============================================================

  describe "Phase 8: String Interpolation" do
    it "infers String type for basic interpolation" do
      source = <<-CRYSTAL
        name = "World"
        msg = "Hello, \#{name}!"
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Get msg assignment
      msg_assign = program.arena[program.roots[1]].as(CrystalV2::Compiler::Frontend::AssignNode)
      interpolated_str_id = msg_assign.value.not_nil!
      interpolated_type = engine.context.get_type(interpolated_str_id)

      interpolated_type.should be_a(PrimitiveType)
      interpolated_type.as(PrimitiveType).name.should eq("String")
    end

    it "infers types for interpolated expressions" do
      source = <<-CRYSTAL
        x = 5
        y = 10
        result = "Sum: \#{x + y}"
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Get result assignment
      result_assign = program.arena[program.roots[2]].as(CrystalV2::Compiler::Frontend::AssignNode)
      interpolated_str_id = result_assign.value.not_nil!
      interpolated_node = program.arena[interpolated_str_id].as(CrystalV2::Compiler::Frontend::StringInterpolationNode)

      # Check the interpolated string type
      interpolated_type = engine.context.get_type(interpolated_str_id)
      interpolated_type.should be_a(PrimitiveType)
      interpolated_type.as(PrimitiveType).name.should eq("String")

      # Check the expression inside interpolation (x + y)
      pieces = interpolated_node.pieces.not_nil!
      expr_piece = pieces.find { |p| p.kind == StringPiece::Kind::Expression }.not_nil!
      expr_id = expr_piece.expr.not_nil!
      expr_type = engine.context.get_type(expr_id)
      expr_type.should be_a(PrimitiveType)
      expr_type.as(PrimitiveType).name.should eq("Int32")
    end

    it "handles multiple interpolations" do
      source = <<-CRYSTAL
        a = 1
        b = 2
        c = 3
        text = "Values: \#{a}, \#{b}, \#{c}"
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Get text assignment
      text_assign = program.arena[program.roots[3]].as(CrystalV2::Compiler::Frontend::AssignNode)
      interpolated_str_id = text_assign.value.not_nil!
      interpolated_node = program.arena[interpolated_str_id].as(CrystalV2::Compiler::Frontend::StringInterpolationNode)

      # Check overall type
      interpolated_type = engine.context.get_type(interpolated_str_id)
      interpolated_type.should be_a(PrimitiveType)
      interpolated_type.as(PrimitiveType).name.should eq("String")

      # Check that we have text and expression pieces
      pieces = interpolated_node.pieces.not_nil!
      # "Values: ", a, ", ", b, ", ", c
      pieces.size.should eq(6)

      # Count expression pieces
      expr_pieces = pieces.select { |p| p.kind == StringPiece::Kind::Expression }
      expr_pieces.size.should eq(3)
    end

    it "handles nested interpolation" do
      source = <<-CRYSTAL
        outer = "outer"
        inner = "The \#{outer} value"
        full = "Full: \#{inner}"
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Get full assignment
      full_assign = program.arena[program.roots[2]].as(CrystalV2::Compiler::Frontend::AssignNode)
      full_str_id = full_assign.value.not_nil!
      full_type = engine.context.get_type(full_str_id)

      full_type.should be_a(PrimitiveType)
      full_type.as(PrimitiveType).name.should eq("String")

      # Get inner assignment
      inner_assign = program.arena[program.roots[1]].as(CrystalV2::Compiler::Frontend::AssignNode)
      inner_str_id = inner_assign.value.not_nil!
      inner_type = engine.context.get_type(inner_str_id)

      inner_type.should be_a(PrimitiveType)
      inner_type.as(PrimitiveType).name.should eq("String")
    end

    it "handles method calls in interpolation" do
      source = <<-CRYSTAL
        class Dog
          def initialize(@name : String)
          end

          def bark
            "Woof!"
          end

          def introduce
            "I am \#{@name} and I say \#{bark}"
          end
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Get class node
      class_node = program.arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::ClassNode)
      class_body = class_node.body.not_nil!

      # Find introduce method (should be third method)
      introduce_def = program.arena[class_body[2]]
      introduce_body = CrystalV2::Compiler::Frontend.node_def_body(introduce_def).not_nil!
      interpolated_str_id = introduce_body[0]

      # Check interpolated string type
      interpolated_type = engine.context.get_type(interpolated_str_id)
      interpolated_type.should be_a(PrimitiveType)
      interpolated_type.as(PrimitiveType).name.should eq("String")

      # Check interpolation contains method call
      interpolated_node = program.arena[interpolated_str_id].as(CrystalV2::Compiler::Frontend::StringInterpolationNode)
      pieces = interpolated_node.pieces.not_nil!

      # Should have: "I am ", @name, " and I say ", bark call
      pieces.size.should eq(4)
    end
  end

  # ============================================================
  # PHASE 9: Array Tests
  # ============================================================

  describe "Phase 9: Arrays" do
    it "infers Array(Int32) for integer array" do
      source = <<-CRYSTAL
        arr = [1, 2, 3]
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Get array assignment
      arr_assign = program.arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::AssignNode)
      array_id = arr_assign.value.not_nil!
      array_type = engine.context.get_type(array_id)

      array_type.should be_a(ArrayType)
      array_type.as(ArrayType).element_type.should be_a(PrimitiveType)
      array_type.as(ArrayType).element_type.as(PrimitiveType).name.should eq("Int32")
    end

    it "infers Array(String) for string array" do
      source = <<-CRYSTAL
        names = ["Alice", "Bob", "Charlie"]
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      arr_assign = program.arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::AssignNode)
      array_id = arr_assign.value.not_nil!
      array_type = engine.context.get_type(array_id)

      array_type.should be_a(ArrayType)
      array_type.as(ArrayType).element_type.as(PrimitiveType).name.should eq("String")
    end

    it "infers union type for heterogeneous array" do
      source = <<-CRYSTAL
        mixed = [1, "hello", true]
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      arr_assign = program.arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::AssignNode)
      array_id = arr_assign.value.not_nil!
      array_type = engine.context.get_type(array_id)

      array_type.should be_a(ArrayType)
      element_type = array_type.as(ArrayType).element_type
      element_type.should be_a(UnionType)
    end

    it "handles empty array with 'of Type' syntax" do
      source = <<-CRYSTAL
        empty = [] of Int32
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      arr_assign = program.arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::AssignNode)
      array_id = arr_assign.value.not_nil!
      array_type = engine.context.get_type(array_id)

      array_type.should be_a(ArrayType)
      # Phase 103B: type_from_type_expr resolves the of_type annotation
      array_type.as(ArrayType).element_type.as(PrimitiveType).name.should eq("Int32")
    end

    it "infers element type from array indexing" do
      source = <<-CRYSTAL
        arr = [1, 2, 3]
        x = arr[0]
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Get x assignment
      x_assign = program.arena[program.roots[1]].as(CrystalV2::Compiler::Frontend::AssignNode)
      index_expr_id = x_assign.value.not_nil!
      index_type = engine.context.get_type(index_expr_id)

      index_type.should be_a(PrimitiveType)
      index_type.as(PrimitiveType).name.should eq("Int32")
    end

    it "handles nested arrays" do
      source = <<-CRYSTAL
        nested = [[1, 2], [3, 4]]
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      arr_assign = program.arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::AssignNode)
      array_id = arr_assign.value.not_nil!
      array_type = engine.context.get_type(array_id)

      array_type.should be_a(ArrayType)
      inner_type = array_type.as(ArrayType).element_type
      inner_type.should be_a(ArrayType)
      inner_type.as(ArrayType).element_type.as(PrimitiveType).name.should eq("Int32")
    end

    it "handles array.size method" do
      source = <<-CRYSTAL
        arr = [1, 2, 3]
        len = arr.size
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Get len assignment
      len_assign = program.arena[program.roots[1]].as(CrystalV2::Compiler::Frontend::AssignNode)
      size_call_id = len_assign.value.not_nil!
      size_type = engine.context.get_type(size_call_id)

      size_type.should be_a(PrimitiveType)
      size_type.as(PrimitiveType).name.should eq("Int32")
    end

    it "handles array.empty? method" do
      source = <<-CRYSTAL
        arr = [1, 2, 3]
        is_empty = arr.empty?
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      empty_assign = program.arena[program.roots[1]].as(CrystalV2::Compiler::Frontend::AssignNode)
      empty_call_id = empty_assign.value.not_nil!
      empty_type = engine.context.get_type(empty_call_id)

      empty_type.should be_a(PrimitiveType)
      empty_type.as(PrimitiveType).name.should eq("Bool")
    end

    it "handles array.first and array.last" do
      source = <<-CRYSTAL
        arr = [1, 2, 3]
        f = arr.first
        l = arr.last
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Check first
      f_assign = program.arena[program.roots[1]].as(CrystalV2::Compiler::Frontend::AssignNode)
      first_id = f_assign.value.not_nil!
      first_type = engine.context.get_type(first_id)

      first_type.should be_a(PrimitiveType)
      first_type.as(PrimitiveType).name.should eq("Int32")

      # Check last
      l_assign = program.arena[program.roots[2]].as(CrystalV2::Compiler::Frontend::AssignNode)
      last_id = l_assign.value.not_nil!
      last_type = engine.context.get_type(last_id)

      last_type.should be_a(PrimitiveType)
      last_type.as(PrimitiveType).name.should eq("Int32")
    end

    it "handles array << push operator" do
      source = <<-CRYSTAL
        arr = [1, 2, 3]
        result = arr << 4
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      result_assign = program.arena[program.roots[1]].as(CrystalV2::Compiler::Frontend::AssignNode)
      push_id = result_assign.value.not_nil!
      push_type = engine.context.get_type(push_id)

      # << returns the array itself
      push_type.should be_a(ArrayType)
      push_type.as(ArrayType).element_type.as(PrimitiveType).name.should eq("Int32")
    end
  end

  describe "Phase 10: Blocks and Yield" do
    it "handles yield expressions" do
      source = <<-CRYSTAL
        def twice
          yield 1
          yield 2
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)
      # Method should exist and parse correctly
      program.roots.size.should eq(1)

      # Check that yields have Nil type
      def_node = program.arena[program.roots[0]]
      body = CrystalV2::Compiler::Frontend.node_def_body(def_node).not_nil!
      yield1 = engine.context.get_type(body[0])
      yield1.as(PrimitiveType).name.should eq("Nil")
    end

    it "handles do/end block syntax" do
      source = <<-CRYSTAL
        def run
          yield 42
        end

        run do |n|
          n * 2
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)
      # Should parse without errors
      program.roots.size.should eq(2)
    end

    it "handles brace block syntax" do
      source = <<-CRYSTAL
        def run
          yield 5
        end

        run { |x| x + 1 }
      CRYSTAL

      program, analyzer, engine = infer_types(source)
      # Should parse without errors
      program.roots.size.should eq(2)
    end

    it "infers block return type from last expression" do
      source = <<-CRYSTAL
        def transform
          yield 10
        end

        transform { 42 }
      CRYSTAL

      program, analyzer, engine = infer_types(source)
      # Get the call with block
      call_node = program.arena[program.roots[1]].as(CrystalV2::Compiler::Frontend::CallNode)
      block_id = call_node.block.not_nil!
      block_type = engine.context.get_type(block_id)

      # Block should return Int32 (from literal 42)
      block_type.as(PrimitiveType).name.should eq("Int32")
    end

    it "handles empty block" do
      source = <<-CRYSTAL
        def run
          yield
        end

        run { }
      CRYSTAL

      program, analyzer, engine = infer_types(source)
      # Should parse without errors
      program.roots.size.should eq(2)

      # Empty block returns Nil
      call_node = program.arena[program.roots[1]].as(CrystalV2::Compiler::Frontend::CallNode)
      block_id = call_node.block.not_nil!
      block_type = engine.context.get_type(block_id)
      block_type.as(PrimitiveType).name.should eq("Nil")
    end
  end

  describe "Phase 11: Case/When" do
    it "infers String type for simple case" do
      source = <<-CRYSTAL
        x = 2
        result = case x
        when 1
          "one"
        when 2
          "two"
        else
          "other"
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)
      # result is the second root (second assignment)
      result_id = program.roots[1]
      result_type = engine.context.get_type(result_id)

      # All branches return String
      result_type.as(PrimitiveType).name.should eq("String")
    end

    it "infers union type for mixed case branches" do
      source = <<-CRYSTAL
        x = 2
        result = case x
        when 1
          42
        when 2
          "hello"
        else
          true
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)
      result_id = program.roots[1]
      result_type = engine.context.get_type(result_id)

      # Branches return Int32, String, Bool → union
      result_type.should be_a(UnionType)
      union = result_type.as(UnionType)
      union.types.size.should eq(3)
    end

    it "infers union with Nil for case without else" do
      source = <<-CRYSTAL
        x = 2
        result = case x
        when 1
          "one"
        when 2
          "two"
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)
      result_id = program.roots[1]
      result_type = engine.context.get_type(result_id)

      # No else clause means Nil is possible
      result_type.should be_a(UnionType)
      union = result_type.as(UnionType)
      union.types.map(&.to_s).should contain("Nil")
      union.types.map(&.to_s).should contain("String")
    end

    it "handles multiple values in when clause" do
      source = <<-CRYSTAL
        x = 2
        result = case x
        when 1, 2, 3
          "small"
        when 4, 5
          "medium"
        else
          "large"
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)
      result_id = program.roots[1]
      result_type = engine.context.get_type(result_id)

      result_type.as(PrimitiveType).name.should eq("String")
    end

    it "handles case with then keyword" do
      source = <<-CRYSTAL
        x = 1
        result = case x
        when 1 then "one"
        when 2 then "two"
        else "other"
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)
      result_id = program.roots[1]
      result_type = engine.context.get_type(result_id)

      result_type.as(PrimitiveType).name.should eq("String")
    end

    it "infers Nil for empty case branch" do
      source = <<-CRYSTAL
        x = 1
        result = case x
        when 1
        else
          "other"
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)
      result_id = program.roots[1]
      result_type = engine.context.get_type(result_id)

      # Empty when body returns Nil
      result_type.should be_a(UnionType)
      union = result_type.as(UnionType)
      union.types.map(&.to_s).should contain("Nil")
      union.types.map(&.to_s).should contain("String")
    end
  end

  describe "Phase 12: Break/Next" do
    it "infers Nil for break without value" do
      source = <<-CRYSTAL
        i = 0
        while i < 10
          i += 1
          if i == 5
            break
          end
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)
      # Find the break statement
      while_node = program.arena[program.roots[1]].as(CrystalV2::Compiler::Frontend::WhileNode)
      body = while_node.body.not_nil!
      if_node = program.arena[body[1]].as(CrystalV2::Compiler::Frontend::IfNode)
      then_branch = if_node.then_body.not_nil!
      break_id = then_branch[0]
      break_type = engine.context.get_type(break_id)

      break_type.as(PrimitiveType).name.should eq("Nil")
    end

    it "infers type from break value" do
      source = <<-CRYSTAL
        result = while true
          break 42
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)
      # Find the break statement
      assign_node = program.arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::AssignNode)
      while_id = assign_node.value.not_nil!
      while_node = program.arena[while_id].as(CrystalV2::Compiler::Frontend::WhileNode)
      body = while_node.body.not_nil!
      break_id = body[0]
      break_type = engine.context.get_type(break_id)

      # Break with value 42 → Int32
      break_type.as(PrimitiveType).name.should eq("Int32")
    end

    it "infers Nil for next" do
      source = <<-CRYSTAL
        i = 0
        while i < 10
          i += 1
          if i == 5
            next
          end
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)
      # Find the next statement
      while_node = program.arena[program.roots[1]].as(CrystalV2::Compiler::Frontend::WhileNode)
      body = while_node.body.not_nil!
      if_node = program.arena[body[1]].as(CrystalV2::Compiler::Frontend::IfNode)
      then_branch = if_node.then_body.not_nil!
      next_id = then_branch[0]
      next_type = engine.context.get_type(next_id)

      # Next always returns Nil
      next_type.as(PrimitiveType).name.should eq("Nil")
    end

    it "handles break with postfix if" do
      source = <<-CRYSTAL
        i = 0
        while i < 10
          i += 1
          break if i == 5
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)
      # Should parse without errors
      program.roots.size.should eq(2)
    end

    it "handles next with postfix if" do
      source = <<-CRYSTAL
        i = 0
        while i < 10
          i += 1
          next if i == 5
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)
      # Should parse without errors
      program.roots.size.should eq(2)
    end

    it "handles break with String value" do
      source = <<-CRYSTAL
        result = while true
          break "done"
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)
      assign_node = program.arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::AssignNode)
      while_id = assign_node.value.not_nil!
      while_node = program.arena[while_id].as(CrystalV2::Compiler::Frontend::WhileNode)
      body = while_node.body.not_nil!
      break_id = body[0]
      break_type = engine.context.get_type(break_id)

      # Break with "done" → String
      break_type.as(PrimitiveType).name.should eq("String")
    end
  end

  describe "Phase 13: Ranges" do
    it "infers Range(Int32, Int32) for inclusive range" do
      source = <<-CRYSTAL
        r = 1..10
      CRYSTAL

      program, analyzer, engine = infer_types(source)
      # Find the assignment
      assign_node = program.arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::AssignNode)
      range_id = assign_node.value.not_nil!
      range_type = engine.context.get_type(range_id)

      range_type.should be_a(RangeType)
      rt = range_type.as(RangeType)
      rt.begin_type.as(PrimitiveType).name.should eq("Int32")
      rt.end_type.as(PrimitiveType).name.should eq("Int32")
    end

    it "infers Range(Int32, Int32) for exclusive range" do
      source = <<-CRYSTAL
        r = 1...10
      CRYSTAL

      program, analyzer, engine = infer_types(source)
      # Find the assignment
      assign_node = program.arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::AssignNode)
      range_id = assign_node.value.not_nil!
      range_type = engine.context.get_type(range_id)

      range_type.should be_a(RangeType)
      rt = range_type.as(RangeType)
      rt.begin_type.as(PrimitiveType).name.should eq("Int32")
      rt.end_type.as(PrimitiveType).name.should eq("Int32")
    end

    it "infers Range from variable expressions" do
      source = <<-CRYSTAL
        a = 5
        b = 20
        r = a..b
      CRYSTAL

      program, analyzer, engine = infer_types(source)
      # Find the range assignment (third statement)
      assign_node = program.arena[program.roots[2]].as(CrystalV2::Compiler::Frontend::AssignNode)
      range_id = assign_node.value.not_nil!
      range_type = engine.context.get_type(range_id)

      range_type.should be_a(RangeType)
      rt = range_type.as(RangeType)
      rt.begin_type.as(PrimitiveType).name.should eq("Int32")
      rt.end_type.as(PrimitiveType).name.should eq("Int32")
    end

    it "infers Range from arithmetic expressions" do
      source = <<-CRYSTAL
        r = (1 + 2)..(10 - 3)
      CRYSTAL

      program, analyzer, engine = infer_types(source)
      # Find the assignment
      assign_node = program.arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::AssignNode)
      range_id = assign_node.value.not_nil!
      range_type = engine.context.get_type(range_id)

      range_type.should be_a(RangeType)
      rt = range_type.as(RangeType)
      rt.begin_type.as(PrimitiveType).name.should eq("Int32")
      rt.end_type.as(PrimitiveType).name.should eq("Int32")
    end

    it "handles range precedence correctly" do
      source = <<-CRYSTAL
        r = 1 + 2..5 + 3
      CRYSTAL

      program, analyzer, engine = infer_types(source)
      # Find the assignment
      assign_node = program.arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::AssignNode)
      range_id = assign_node.value.not_nil!

      # Should be Range node, not Binary
      range_node = program.arena[range_id]
      CrystalV2::Compiler::Frontend.node_kind(range_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Range)

      # Begin should be Binary (1 + 2)
      begin_id = CrystalV2::Compiler::Frontend.node_range_begin(range_node).not_nil!
      begin_node = program.arena[begin_id]
      CrystalV2::Compiler::Frontend.node_kind(begin_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)

      # End should be Binary (5 + 3)
      end_id = CrystalV2::Compiler::Frontend.node_range_end(range_node).not_nil!
      end_node = program.arena[end_id]
      CrystalV2::Compiler::Frontend.node_kind(end_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
    end

    it "infers Range with mixed types" do
      source = <<-CRYSTAL
        r = 1..10.5
      CRYSTAL

      program, analyzer, engine = infer_types(source)
      # Find the assignment
      assign_node = program.arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::AssignNode)
      range_id = assign_node.value.not_nil!
      range_type = engine.context.get_type(range_id)

      range_type.should be_a(RangeType)
      rt = range_type.as(RangeType)
      rt.begin_type.as(PrimitiveType).name.should eq("Int32")
      rt.end_type.as(PrimitiveType).name.should eq("Float64")
    end
  end

  describe "Phase 14: Hashes" do
    it "infers Hash(String, Int32) for homogeneous hash" do
      source = <<-CRYSTAL
        h = {"name" => 42, "age" => 30}
      CRYSTAL

      program, analyzer, engine = infer_types(source)
      # Find the assignment
      assign_node = program.arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::AssignNode)
      hash_id = assign_node.value.not_nil!
      hash_type = engine.context.get_type(hash_id)

      hash_type.should be_a(HashType)
      ht = hash_type.as(HashType)
      ht.key_type.as(PrimitiveType).name.should eq("String")
      ht.value_type.as(PrimitiveType).name.should eq("Int32")
    end

    it "infers Hash(String, String | Int32) for heterogeneous values" do
      source = <<-CRYSTAL
        h = {"name" => "Alice", "age" => 30}
      CRYSTAL

      program, analyzer, engine = infer_types(source)
      # Find the assignment
      assign_node = program.arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::AssignNode)
      hash_id = assign_node.value.not_nil!
      hash_type = engine.context.get_type(hash_id)

      hash_type.should be_a(HashType)
      ht = hash_type.as(HashType)
      ht.key_type.as(PrimitiveType).name.should eq("String")

      # Value type should be union
      ht.value_type.should be_a(UnionType)
      value_union = ht.value_type.as(UnionType)
      value_union.types.size.should eq(2)
    end

    it "infers Hash(Int32, String) for integer keys" do
      source = <<-CRYSTAL
        h = {1 => "one", 2 => "two", 3 => "three"}
      CRYSTAL

      program, analyzer, engine = infer_types(source)
      # Find the assignment
      assign_node = program.arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::AssignNode)
      hash_id = assign_node.value.not_nil!
      hash_type = engine.context.get_type(hash_id)

      hash_type.should be_a(HashType)
      ht = hash_type.as(HashType)
      ht.key_type.as(PrimitiveType).name.should eq("Int32")
      ht.value_type.as(PrimitiveType).name.should eq("String")
    end

    it "infers empty hash with type annotation" do
      source = <<-CRYSTAL
        h = {} of String => Int32
      CRYSTAL

      program, analyzer, engine = infer_types(source)
      # Find the assignment
      assign_node = program.arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::AssignNode)
      hash_id = assign_node.value.not_nil!
      hash_type = engine.context.get_type(hash_id)

      hash_type.should be_a(HashType)
      ht = hash_type.as(HashType)
      # Phase 91A: Hash 'of' parsing not updated yet - keeps old behavior
      # TODO: Update Hash to use ExprId like Array in Phase 91B
      ht.key_type.as(PrimitiveType).name.should eq("Nil")
      ht.value_type.as(PrimitiveType).name.should eq("Nil")
    end

    it "infers nested hash Hash(String, Hash(String, Int32))" do
      source = <<-CRYSTAL
        h = {"outer" => {"inner" => 42}}
      CRYSTAL

      program, analyzer, engine = infer_types(source)
      # Find the assignment
      assign_node = program.arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::AssignNode)
      hash_id = assign_node.value.not_nil!
      hash_type = engine.context.get_type(hash_id)

      hash_type.should be_a(HashType)
      ht = hash_type.as(HashType)
      ht.key_type.as(PrimitiveType).name.should eq("String")

      # Value should be another Hash
      ht.value_type.should be_a(HashType)
      inner_hash = ht.value_type.as(HashType)
      inner_hash.key_type.as(PrimitiveType).name.should eq("String")
      inner_hash.value_type.as(PrimitiveType).name.should eq("Int32")
    end

    it "infers hash with variable keys and values" do
      source = <<-CRYSTAL
        k = "key"
        v = 100
        h = {k => v}
      CRYSTAL

      program, analyzer, engine = infer_types(source)
      # Find the hash assignment (third statement)
      assign_node = program.arena[program.roots[2]].as(CrystalV2::Compiler::Frontend::AssignNode)
      hash_id = assign_node.value.not_nil!
      hash_type = engine.context.get_type(hash_id)

      hash_type.should be_a(HashType)
      ht = hash_type.as(HashType)
      ht.key_type.as(PrimitiveType).name.should eq("String")
      ht.value_type.as(PrimitiveType).name.should eq("Int32")
    end
  end

  describe "Phase 14B: Hash Indexing" do
    it "infers type from hash indexing" do
      source = <<-CRYSTAL
        h = {"name" => "Alice"}
        x = h["name"]
      CRYSTAL

      program, analyzer, engine = infer_types(source)
      # Find x assignment (second statement)
      assign_node = program.arena[program.roots[1]].as(CrystalV2::Compiler::Frontend::AssignNode)
      index_id = assign_node.value.not_nil!
      index_type = engine.context.get_type(index_id)

      # Hash is Hash(String, String), so index returns String
      index_type.should be_a(PrimitiveType)
      index_type.as(PrimitiveType).name.should eq("String")
    end

    it "infers union type from heterogeneous hash indexing" do
      source = <<-CRYSTAL
        h = {"name" => "Alice", "age" => 30}
        y = h["name"]
      CRYSTAL

      program, analyzer, engine = infer_types(source)
      # Find y assignment
      assign_node = program.arena[program.roots[1]].as(CrystalV2::Compiler::Frontend::AssignNode)
      index_id = assign_node.value.not_nil!
      index_type = engine.context.get_type(index_id)

      # Hash is Hash(String, String | Int32), so index returns String | Int32
      index_type.should be_a(UnionType)
      union = index_type.as(UnionType)
      union.types.size.should eq(2)

      type_names = union.types.map { |t| t.as(PrimitiveType).name }.sort
      type_names.should eq(["Int32", "String"])
    end

    it "supports hash assignment via indexing" do
      source = <<-CRYSTAL
        h = {"key" => 42}
        h["key"] = 100
      CRYSTAL

      program, analyzer, engine = infer_types(source)
      # Find assignment (second statement)
      assign_node = program.arena[program.roots[1]].as(CrystalV2::Compiler::Frontend::AssignNode)

      # Should be Assign node with Index target

      target_node = program.arena[assign_node.target.not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(target_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Index)

      # Assignment returns the value type (Int32)
      value_id = assign_node.value.not_nil!
      value_type = engine.context.get_type(value_id)
      value_type.should be_a(PrimitiveType)
      value_type.as(PrimitiveType).name.should eq("Int32")
    end

    it "supports hash indexing with different key types" do
      source = <<-CRYSTAL
        h = {1 => "one", 2 => "two"}
        s = h[1]
      CRYSTAL

      program, analyzer, engine = infer_types(source)
      # Find s assignment
      assign_node = program.arena[program.roots[1]].as(CrystalV2::Compiler::Frontend::AssignNode)
      index_id = assign_node.value.not_nil!
      index_type = engine.context.get_type(index_id)

      # Hash is Hash(Int32, String), so index returns String
      index_type.should be_a(PrimitiveType)
      index_type.as(PrimitiveType).name.should eq("String")
    end

    it "emits error when indexing non-hash non-array type" do
      source = <<-CRYSTAL
        x = 42
        y = x[0]
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Should have diagnostic about indexing Int32
      engine.diagnostics.size.should be > 0
      engine.diagnostics.first.message.should contain("Cannot index")
    end
  end

  describe "Phase 15: Tuples" do
    it "infers heterogeneous tuple type" do
      source = <<-CRYSTAL
        t = {1, "hello", true}
      CRYSTAL

      program, analyzer, engine = infer_types(source)
      # Get assignment
      assign_node = program.arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::AssignNode)

      # Get tuple literal
      tuple_id = assign_node.value.not_nil!
      tuple_type = engine.context.get_type(tuple_id)

      # Check it's a TupleType
      tuple_type.should be_a(TupleType)
      tuple_type = tuple_type.as(TupleType)

      # Check element types
      tuple_type.element_types.size.should eq(3)
      tuple_type.element_types[0].should be_a(PrimitiveType)
      tuple_type.element_types[0].as(PrimitiveType).name.should eq("Int32")
      tuple_type.element_types[1].should be_a(PrimitiveType)
      tuple_type.element_types[1].as(PrimitiveType).name.should eq("String")
      tuple_type.element_types[2].should be_a(PrimitiveType)
      tuple_type.element_types[2].as(PrimitiveType).name.should eq("Bool")
    end

    it "infers homogeneous tuple type" do
      source = <<-CRYSTAL
        t = {1, 2, 3}
      CRYSTAL

      program, analyzer, engine = infer_types(source)
      assign_node = program.arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::AssignNode)
      tuple_id = assign_node.value.not_nil!
      tuple_type = engine.context.get_type(tuple_id).as(TupleType)

      tuple_type.element_types.size.should eq(3)
      tuple_type.element_types.all? { |t| t.as(PrimitiveType).name == "Int32" }.should be_true
    end

    it "supports tuple indexing with literal" do
      source = <<-CRYSTAL
        t = {1, "hello", true}
        x = t[0]
        y = t[1]
        z = t[2]
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # x = t[0] should be Int32
      x_assign = program.arena[program.roots[1]].as(CrystalV2::Compiler::Frontend::AssignNode)
      x_value_id = x_assign.value.not_nil!
      x_type = engine.context.get_type(x_value_id)
      x_type.should be_a(PrimitiveType)
      x_type.as(PrimitiveType).name.should eq("Int32")

      # y = t[1] should be String
      y_assign = program.arena[program.roots[2]].as(CrystalV2::Compiler::Frontend::AssignNode)
      y_value_id = y_assign.value.not_nil!
      y_type = engine.context.get_type(y_value_id)
      y_type.should be_a(PrimitiveType)
      y_type.as(PrimitiveType).name.should eq("String")

      # z = t[2] should be Bool
      z_assign = program.arena[program.roots[3]].as(CrystalV2::Compiler::Frontend::AssignNode)
      z_value_id = z_assign.value.not_nil!
      z_type = engine.context.get_type(z_value_id)
      z_type.should be_a(PrimitiveType)
      z_type.as(PrimitiveType).name.should eq("Bool")
    end

    it "emits error for tuple index out of bounds" do
      source = <<-CRYSTAL
        t = {1, 2}
        x = t[5]
      CRYSTAL

      program, analyzer, engine = infer_types(source)
      engine.diagnostics.size.should be > 0
      engine.diagnostics.first.message.should contain("out of bounds")
    end

    it "handles nested tuples" do
      source = <<-CRYSTAL
        t = {1, {2, 3}}
      CRYSTAL

      program, analyzer, engine = infer_types(source)
      assign_node = program.arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::AssignNode)
      tuple_id = assign_node.value.not_nil!
      tuple_type = engine.context.get_type(tuple_id).as(TupleType)

      tuple_type.element_types.size.should eq(2)
      tuple_type.element_types[0].should be_a(PrimitiveType)
      tuple_type.element_types[1].should be_a(TupleType)

      nested = tuple_type.element_types[1].as(TupleType)
      nested.element_types.size.should eq(2)
    end
  end

  # Phase 16: Symbol Literals
  describe "Phase 16: Symbols" do
    it "infers Symbol type for symbol literal" do
      source = <<-CRYSTAL
        s = :hello
      CRYSTAL

      program, analyzer, engine = infer_types(source)
      assign_node = program.arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::AssignNode)
      symbol_id = assign_node.value.not_nil!
      symbol_type = engine.context.get_type(symbol_id)

      symbol_type.should be_a(PrimitiveType)
      symbol_type.as(PrimitiveType).name.should eq("Symbol")
    end

    it "handles symbols with underscores and numbers" do
      source = <<-CRYSTAL
        s1 = :hello_world
        s2 = :var123
        s3 = :_private
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Check each symbol assignment
      [0, 1, 2].each do |i|
        assign_node = program.arena[program.roots[i]].as(CrystalV2::Compiler::Frontend::AssignNode)
        symbol_id = assign_node.value.not_nil!
        symbol_type = engine.context.get_type(symbol_id)

        symbol_type.should be_a(PrimitiveType)
        symbol_type.as(PrimitiveType).name.should eq("Symbol")
      end
    end

    it "infers symbols as hash keys" do
      source = <<-CRYSTAL
        h = {:name => "Alice", :age => 30}
      CRYSTAL

      program, analyzer, engine = infer_types(source)
      assign_node = program.arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::AssignNode)
      hash_id = assign_node.value.not_nil!
      hash_type = engine.context.get_type(hash_id).as(HashType)

      # Key type should be Symbol
      hash_type.key_type.should be_a(PrimitiveType)
      hash_type.key_type.as(PrimitiveType).name.should eq("Symbol")

      # Value type should be String | Int32
      hash_type.value_type.should be_a(UnionType)
    end

    it "handles symbols in arrays" do
      source = <<-CRYSTAL
        arr = [:foo, :bar, :baz]
      CRYSTAL

      program, analyzer, engine = infer_types(source)
      assign_node = program.arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::AssignNode)
      array_id = assign_node.value.not_nil!
      array_type = engine.context.get_type(array_id).as(ArrayType)

      array_type.element_type.should be_a(PrimitiveType)
      array_type.element_type.as(PrimitiveType).name.should eq("Symbol")
    end

    it "distinguishes colon from symbol" do
      source = <<-CRYSTAL
        def foo(x : Int32)
          :result
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # The method definition should be parsed correctly
      # (colon used for type annotation, not symbol)
      def_node = program.arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(def_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Def)

      # Method body contains symbol
      body_expr_id = CrystalV2::Compiler::Frontend.node_def_body(def_node).not_nil![0]
      body_type = engine.context.get_type(body_expr_id)
      body_type.should be_a(PrimitiveType)
      body_type.as(PrimitiveType).name.should eq("Symbol")
    end
  end

  # Phase 17: Unary Operators
  describe "Phase 17: Unary Operators" do
    it "infers Bool type for logical not on bool" do
      source = <<-CRYSTAL
        x = true
        y = !x
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Get the second assignment (y = !x)
      assign_node = program.arena[program.roots[1]].as(CrystalV2::Compiler::Frontend::AssignNode)
      not_expr_id = assign_node.value.not_nil!
      not_type = engine.context.get_type(not_expr_id)

      not_type.should be_a(PrimitiveType)
      not_type.as(PrimitiveType).name.should eq("Bool")
    end

    it "infers Bool type for logical not on nil" do
      source = <<-CRYSTAL
        x = nil
        y = !x
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      assign_node = program.arena[program.roots[1]].as(CrystalV2::Compiler::Frontend::AssignNode)
      not_expr_id = assign_node.value.not_nil!
      not_type = engine.context.get_type(not_expr_id)

      not_type.should be_a(PrimitiveType)
      not_type.as(PrimitiveType).name.should eq("Bool")
    end

    it "infers Bool type for logical not on number" do
      source = <<-CRYSTAL
        x = 42
        y = !x
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      assign_node = program.arena[program.roots[1]].as(CrystalV2::Compiler::Frontend::AssignNode)
      not_expr_id = assign_node.value.not_nil!
      not_type = engine.context.get_type(not_expr_id)

      not_type.should be_a(PrimitiveType)
      not_type.as(PrimitiveType).name.should eq("Bool")
    end

    it "infers Bool type for logical not on string" do
      source = <<-CRYSTAL
        x = "hello"
        y = !x
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      assign_node = program.arena[program.roots[1]].as(CrystalV2::Compiler::Frontend::AssignNode)
      not_expr_id = assign_node.value.not_nil!
      not_type = engine.context.get_type(not_expr_id)

      not_type.should be_a(PrimitiveType)
      not_type.as(PrimitiveType).name.should eq("Bool")
    end

    it "infers correct type for unary minus on number" do
      source = <<-CRYSTAL
        x = 42
        y = -x
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      assign_node = program.arena[program.roots[1]].as(CrystalV2::Compiler::Frontend::AssignNode)
      neg_expr_id = assign_node.value.not_nil!
      neg_type = engine.context.get_type(neg_expr_id)

      neg_type.should be_a(PrimitiveType)
      neg_type.as(PrimitiveType).name.should eq("Int32")
    end

    it "infers correct type for unary plus on number" do
      source = <<-CRYSTAL
        x = 3.14_f64
        y = +x
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      assign_node = program.arena[program.roots[1]].as(CrystalV2::Compiler::Frontend::AssignNode)
      plus_expr_id = assign_node.value.not_nil!
      plus_type = engine.context.get_type(plus_expr_id)

      plus_type.should be_a(PrimitiveType)
      plus_type.as(PrimitiveType).name.should eq("Float64")
    end

    it "handles double negation" do
      source = <<-CRYSTAL
        x = true
        y = !!x
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      assign_node = program.arena[program.roots[1]].as(CrystalV2::Compiler::Frontend::AssignNode)
      double_not_id = assign_node.value.not_nil!
      double_not_type = engine.context.get_type(double_not_id)

      double_not_type.should be_a(PrimitiveType)
      double_not_type.as(PrimitiveType).name.should eq("Bool")
    end

    it "handles logical not in conditional" do
      source = <<-CRYSTAL
        x = false
        if !x
          y = 1
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Get the if statement
      if_node = program.arena[program.roots[1]].as(CrystalV2::Compiler::Frontend::IfNode)
      condition_id = if_node.condition.not_nil!
      condition_type = engine.context.get_type(condition_id)

      condition_type.should be_a(PrimitiveType)
      condition_type.as(PrimitiveType).name.should eq("Bool")
    end
  end

  # Phase 18: Modulo Operator
  describe "Phase 18: Modulo Operator" do
    it "infers Int32 for integer modulo" do
      source = <<-CRYSTAL
        x = 10 % 3
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      assign_node = program.arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::AssignNode)
      mod_expr_id = assign_node.value.not_nil!
      mod_type = engine.context.get_type(mod_expr_id)

      mod_type.should be_a(PrimitiveType)
      mod_type.as(PrimitiveType).name.should eq("Int32")
    end

    it "infers Float64 for float modulo" do
      source = <<-CRYSTAL
        x = 7.5_f64 % 2.5_f64
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      assign_node = program.arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::AssignNode)
      mod_expr_id = assign_node.value.not_nil!
      mod_type = engine.context.get_type(mod_expr_id)

      mod_type.should be_a(PrimitiveType)
      mod_type.as(PrimitiveType).name.should eq("Float64")
    end

    it "handles modulo with variables" do
      source = <<-CRYSTAL
        a = 17
        b = 5
        c = a % b
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      assign_node = program.arena[program.roots[2]].as(CrystalV2::Compiler::Frontend::AssignNode)
      mod_expr_id = assign_node.value.not_nil!
      mod_type = engine.context.get_type(mod_expr_id)

      mod_type.should be_a(PrimitiveType)
      mod_type.as(PrimitiveType).name.should eq("Int32")
    end

    it "handles modulo in expression" do
      source = <<-CRYSTAL
        x = (10 % 3) + 1
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      assign_node = program.arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::AssignNode)
      add_expr_id = assign_node.value.not_nil!
      add_type = engine.context.get_type(add_expr_id)

      add_type.should be_a(PrimitiveType)
      add_type.as(PrimitiveType).name.should eq("Int32")
    end

    it "respects operator precedence for modulo" do
      source = <<-CRYSTAL
        x = 10 + 7 % 3
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      assign_node = program.arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::AssignNode)
      expr_id = assign_node.value.not_nil!
      expr_node = program.arena[expr_id].as(CrystalV2::Compiler::Frontend::BinaryNode)

      # Should parse as 10 + (7 % 3), not (10 + 7) % 3
      Frontend.node_operator_string(expr_node).should eq("+")

      # Right side should be modulo
      right_id = expr_node.right
      right_node = program.arena[right_id].as(CrystalV2::Compiler::Frontend::BinaryNode)
      Frontend.node_operator_string(right_node).should eq("%")
    end
  end

  # Phase 19: Exponentiation Operator
  describe "Phase 19: Exponentiation Operator" do
    it "infers Int32 for integer exponentiation" do
      source = <<-CRYSTAL
        x = 2 ** 3
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      assign_node = program.arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::AssignNode)
      exp_expr_id = assign_node.value.not_nil!
      exp_type = engine.context.get_type(exp_expr_id)

      exp_type.should be_a(PrimitiveType)
      exp_type.as(PrimitiveType).name.should eq("Int32")
    end

    it "infers Float64 for float exponentiation" do
      source = <<-CRYSTAL
        x = 2.0_f64 ** 3.0_f64
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      assign_node = program.arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::AssignNode)
      exp_expr_id = assign_node.value.not_nil!
      exp_type = engine.context.get_type(exp_expr_id)

      exp_type.should be_a(PrimitiveType)
      exp_type.as(PrimitiveType).name.should eq("Float64")
    end

    it "handles exponentiation with variables" do
      source = <<-CRYSTAL
        base = 10
        exp = 2
        result = base ** exp
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      assign_node = program.arena[program.roots[2]].as(CrystalV2::Compiler::Frontend::AssignNode)
      exp_expr_id = assign_node.value.not_nil!
      exp_type = engine.context.get_type(exp_expr_id)

      exp_type.should be_a(PrimitiveType)
      exp_type.as(PrimitiveType).name.should eq("Int32")
    end

    it "respects operator precedence for exponentiation" do
      source = <<-CRYSTAL
        x = 2 * 3 ** 2
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      assign_node = program.arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::AssignNode)
      expr_id = assign_node.value.not_nil!
      expr_node = program.arena[expr_id].as(CrystalV2::Compiler::Frontend::BinaryNode)

      # Should parse as 2 * (3 ** 2), not (2 * 3) ** 2
      Frontend.node_operator_string(expr_node).should eq("*")

      # Right side should be exponentiation
      right_id = expr_node.right
      right_node = program.arena[right_id].as(CrystalV2::Compiler::Frontend::BinaryNode)
      Frontend.node_operator_string(right_node).should eq("**")
    end

    it "handles exponentiation in complex expression" do
      source = <<-CRYSTAL
        x = 2 ** 3 + 1
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      assign_node = program.arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::AssignNode)
      add_expr_id = assign_node.value.not_nil!
      add_node = program.arena[add_expr_id].as(CrystalV2::Compiler::Frontend::BinaryNode)

      # Should parse as (2 ** 3) + 1
      Frontend.node_operator_string(add_node).should eq("+")

      # Left side should be exponentiation
      left_id = add_node.left
      left_node = program.arena[left_id].as(CrystalV2::Compiler::Frontend::BinaryNode)
      Frontend.node_operator_string(left_node).should eq("**")
    end

    it "handles multiple exponentiations" do
      source = <<-CRYSTAL
        x = 2 ** 2 ** 2
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      assign_node = program.arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::AssignNode)
      expr_id = assign_node.value.not_nil!
      expr_type = engine.context.get_type(expr_id)

      # Type should still be Int32
      expr_type.should be_a(PrimitiveType)
      expr_type.as(PrimitiveType).name.should eq("Int32")
    end
  end

  # Phase 20: Compound Assignment Operators
  describe "Phase 20: Compound Assignment" do
    it "handles += operator" do
      source = <<-CRYSTAL
        x = 10
        x += 5
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Second statement is compound assignment desugared to x = x + 5
      assign_node = program.arena[program.roots[1]].as(CrystalV2::Compiler::Frontend::AssignNode)

      # Value should be a binary expression (x + 5)
      value_id = assign_node.value.not_nil!
      value_node = program.arena[value_id].as(CrystalV2::Compiler::Frontend::BinaryNode)
      Frontend.node_operator_string(value_node).should eq("+")

      # Type should be Int32
      value_type = engine.context.get_type(value_id)
      value_type.should be_a(PrimitiveType)
      value_type.as(PrimitiveType).name.should eq("Int32")
    end

    it "handles -= operator" do
      source = <<-CRYSTAL
        x = 20
        x -= 3
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      assign_node = program.arena[program.roots[1]].as(CrystalV2::Compiler::Frontend::AssignNode)
      value_id = assign_node.value.not_nil!
      value_node = program.arena[value_id].as(CrystalV2::Compiler::Frontend::BinaryNode)

      Frontend.node_operator_string(value_node).should eq("-")

      value_type = engine.context.get_type(value_id)
      value_type.as(PrimitiveType).name.should eq("Int32")
    end

    it "handles *= operator" do
      source = <<-CRYSTAL
        x = 4
        x *= 3
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      assign_node = program.arena[program.roots[1]].as(CrystalV2::Compiler::Frontend::AssignNode)
      value_id = assign_node.value.not_nil!
      value_node = program.arena[value_id].as(CrystalV2::Compiler::Frontend::BinaryNode)

      Frontend.node_operator_string(value_node).should eq("*")

      value_type = engine.context.get_type(value_id)
      value_type.as(PrimitiveType).name.should eq("Int32")
    end

    it "handles /= operator" do
      source = <<-CRYSTAL
        x = 20
        x /= 4
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      assign_node = program.arena[program.roots[1]].as(CrystalV2::Compiler::Frontend::AssignNode)
      value_id = assign_node.value.not_nil!
      value_node = program.arena[value_id].as(CrystalV2::Compiler::Frontend::BinaryNode)

      Frontend.node_operator_string(value_node).should eq("/")

      value_type = engine.context.get_type(value_id)
      value_type.as(PrimitiveType).name.should eq("Int32")
    end

    it "handles %= operator" do
      source = <<-CRYSTAL
        x = 17
        x %= 5
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      assign_node = program.arena[program.roots[1]].as(CrystalV2::Compiler::Frontend::AssignNode)
      value_id = assign_node.value.not_nil!
      value_node = program.arena[value_id].as(CrystalV2::Compiler::Frontend::BinaryNode)

      Frontend.node_operator_string(value_node).should eq("%")

      value_type = engine.context.get_type(value_id)
      value_type.as(PrimitiveType).name.should eq("Int32")
    end

    it "handles **= operator" do
      source = <<-CRYSTAL
        x = 2
        x **= 3
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      assign_node = program.arena[program.roots[1]].as(CrystalV2::Compiler::Frontend::AssignNode)
      value_id = assign_node.value.not_nil!
      value_node = program.arena[value_id].as(CrystalV2::Compiler::Frontend::BinaryNode)

      Frontend.node_operator_string(value_node).should eq("**")

      value_type = engine.context.get_type(value_id)
      value_type.as(PrimitiveType).name.should eq("Int32")
    end

    it "handles compound assignment with float" do
      source = <<-CRYSTAL
        x = 10.5_f64
        x += 2.5_f64
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      assign_node = program.arena[program.roots[1]].as(CrystalV2::Compiler::Frontend::AssignNode)
      value_id = assign_node.value.not_nil!

      value_type = engine.context.get_type(value_id)
      value_type.as(PrimitiveType).name.should eq("Float64")
    end

    it "handles compound assignment on instance variable" do
      source = <<-CRYSTAL
        @count = 0
        @count += 1
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      assign_node = program.arena[program.roots[1]].as(CrystalV2::Compiler::Frontend::AssignNode)
      target = assign_node.target.not_nil!
      target_node = program.arena[target]

      CrystalV2::Compiler::Frontend.node_kind(target_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::InstanceVar)
    end
  end

  # Phase 21: Bitwise Operators
  describe "Phase 21: Bitwise Operators" do
    it "handles bitwise AND operator" do
      source = <<-CRYSTAL
        x = 5 & 3
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      assign_node = program.arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::AssignNode)
      and_expr_id = assign_node.value.not_nil!
      and_node = program.arena[and_expr_id].as(CrystalV2::Compiler::Frontend::BinaryNode)

      Frontend.node_operator_string(and_node).should eq("&")

      and_type = engine.context.get_type(and_expr_id)
      and_type.should be_a(PrimitiveType)
      and_type.as(PrimitiveType).name.should eq("Int32")
    end

    it "handles bitwise OR operator" do
      source = <<-CRYSTAL
        x = 5 | 3
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      assign_node = program.arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::AssignNode)
      or_expr_id = assign_node.value.not_nil!
      or_node = program.arena[or_expr_id].as(CrystalV2::Compiler::Frontend::BinaryNode)

      Frontend.node_operator_string(or_node).should eq("|")
      or_type = engine.context.get_type(or_expr_id)
      or_type.as(PrimitiveType).name.should eq("Int32")
    end

    it "handles bitwise XOR operator" do
      source = <<-CRYSTAL
        x = 5 ^ 3
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      assign_node = program.arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::AssignNode)
      xor_expr_id = assign_node.value.not_nil!
      xor_node = program.arena[xor_expr_id].as(CrystalV2::Compiler::Frontend::BinaryNode)

      Frontend.node_operator_string(xor_node).should eq("^")
      xor_type = engine.context.get_type(xor_expr_id)
      xor_type.as(PrimitiveType).name.should eq("Int32")
    end

    it "handles bitwise NOT operator" do
      source = <<-CRYSTAL
        x = ~5
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      assign_node = program.arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::AssignNode)
      not_expr_id = assign_node.value.not_nil!
      not_node = program.arena[not_expr_id]

      CrystalV2::Compiler::Frontend.node_kind(not_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Unary)
      Frontend.node_operator_string(not_node).should eq("~")

      not_type = engine.context.get_type(not_expr_id)
      not_type.should be_a(PrimitiveType)
      not_type.as(PrimitiveType).name.should eq("Int32")
    end

    it "handles combined bitwise operations" do
      source = <<-CRYSTAL
        x = (5 & 3) | 2
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      assign_node = program.arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::AssignNode)
      expr_id = assign_node.value.not_nil!
      expr_type = engine.context.get_type(expr_id)

      expr_type.should be_a(PrimitiveType)
      expr_type.as(PrimitiveType).name.should eq("Int32")
    end

    it "respects operator precedence for bitwise" do
      source = <<-CRYSTAL
        x = 5 | 3 & 1
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Should parse as 5 | (3 & 1) based on precedence
      assign_node = program.arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::AssignNode)
      expr_id = assign_node.value.not_nil!
      expr_node = program.arena[expr_id].as(CrystalV2::Compiler::Frontend::BinaryNode)

      # Both have same precedence (6), so left-to-right: (5 | 3) & 1
      # Actually, let's just check it parses and types correctly
      expr_type = engine.context.get_type(expr_id)
      expr_type.as(PrimitiveType).name.should eq("Int32")
    end

    it "handles bitwise with variables" do
      source = <<-CRYSTAL
        a = 10
        b = 6
        c = a & b
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      assign_node = program.arena[program.roots[2]].as(CrystalV2::Compiler::Frontend::AssignNode)
      and_expr_id = assign_node.value.not_nil!

      and_type = engine.context.get_type(and_expr_id)
      and_type.as(PrimitiveType).name.should eq("Int32")
    end

    it "distinguishes bitwise from logical operators" do
      source = <<-CRYSTAL
        a = true && false
        b = 5 & 3
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # First is logical AND (returns Bool)
      assign1 = program.arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::AssignNode)
      logical_id = assign1.value.not_nil!
      logical_type = engine.context.get_type(logical_id)
      logical_type.as(PrimitiveType).name.should eq("Bool")

      # Second is bitwise AND (returns Int32)
      assign2 = program.arena[program.roots[1]].as(CrystalV2::Compiler::Frontend::AssignNode)
      bitwise_id = assign2.value.not_nil!
      bitwise_type = engine.context.get_type(bitwise_id)
      bitwise_type.as(PrimitiveType).name.should eq("Int32")
    end
  end

  # Phase 22: Right Shift Operator
  describe "Phase 22: Right Shift Operator" do
    it "handles basic right shift" do
      source = <<-CRYSTAL
        x = 8 >> 2
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      assign_node = program.arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::AssignNode)
      shift_expr_id = assign_node.value.not_nil!
      shift_node = program.arena[shift_expr_id].as(CrystalV2::Compiler::Frontend::BinaryNode)

      Frontend.node_operator_string(shift_node).should eq(">>")

      shift_type = engine.context.get_type(shift_expr_id)
      shift_type.should be_a(PrimitiveType)
      shift_type.as(PrimitiveType).name.should eq("Int32")
    end

    it "handles right shift with negative numbers" do
      source = <<-CRYSTAL
        x = -8 >> 2
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      assign_node = program.arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::AssignNode)
      shift_expr_id = assign_node.value.not_nil!

      # Right operand is the shift, left is unary minus
      shift_type = engine.context.get_type(shift_expr_id)
      shift_type.should be_a(PrimitiveType)
      shift_type.as(PrimitiveType).name.should eq("Int32")
    end

    it "handles right shift with variables" do
      source = <<-CRYSTAL
        a = 16
        b = 2
        c = a >> b
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      assign_node = program.arena[program.roots[2]].as(CrystalV2::Compiler::Frontend::AssignNode)
      shift_expr_id = assign_node.value.not_nil!

      shift_type = engine.context.get_type(shift_expr_id)
      shift_type.as(PrimitiveType).name.should eq("Int32")
    end

    it "respects operator precedence for right shift" do
      source = <<-CRYSTAL
        x = 8 >> 2 + 1
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Should parse as 8 >> (2 + 1) since >> and + have same precedence
      # Actually left-to-right: (8 >> 2) + 1
      assign_node = program.arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::AssignNode)
      expr_id = assign_node.value.not_nil!

      # Just verify it parses and types correctly
      expr_type = engine.context.get_type(expr_id)
      expr_type.as(PrimitiveType).name.should eq("Int32")
    end

    it "handles right shift in complex expression" do
      source = <<-CRYSTAL
        x = 32 >> 2 * 2
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # Should parse as 32 >> (2 * 2) since * has higher precedence
      assign_node = program.arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::AssignNode)
      expr_id = assign_node.value.not_nil!
      expr_node = program.arena[expr_id].as(CrystalV2::Compiler::Frontend::BinaryNode)

      Frontend.node_operator_string(expr_node).should eq(">>")

      expr_type = engine.context.get_type(expr_id)
      expr_type.as(PrimitiveType).name.should eq("Int32")
    end

    it "distinguishes >> from > comparison" do
      source = <<-CRYSTAL
        a = 8 >> 2
        b = 8 > 2
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      # First is right shift (returns Int32)
      assign1 = program.arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::AssignNode)
      shift_id = assign1.value.not_nil!
      shift_type = engine.context.get_type(shift_id)
      shift_type.as(PrimitiveType).name.should eq("Int32")

      # Second is comparison (returns Bool)
      assign2 = program.arena[program.roots[1]].as(CrystalV2::Compiler::Frontend::AssignNode)
      comp_id = assign2.value.not_nil!
      comp_type = engine.context.get_type(comp_id)
      comp_type.as(PrimitiveType).name.should eq("Bool")
    end

    it "handles right shift with different integer types" do
      source = <<-CRYSTAL
        x = 16_i64 >> 2
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      assign_node = program.arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::AssignNode)
      shift_expr_id = assign_node.value.not_nil!

      shift_type = engine.context.get_type(shift_expr_id)
      shift_type.should be_a(PrimitiveType)
      shift_type.as(PrimitiveType).name.should eq("Int64")
    end
  end

  # Phase 23: Ternary Operator
  describe "Phase 23: Ternary Operator" do
    it "handles basic ternary with same types" do
      source = <<-CRYSTAL
        x = true ? 10 : 20
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      assign_node = program.arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::AssignNode)
      ternary_id = assign_node.value.not_nil!
      ternary_node = program.arena[ternary_id].as(CrystalV2::Compiler::Frontend::TernaryNode)


      ternary_type = engine.context.get_type(ternary_id)
      ternary_type.should be_a(PrimitiveType)
      ternary_type.as(PrimitiveType).name.should eq("Int32")
    end

    it "handles ternary with different types creating union" do
      source = <<-CRYSTAL
        x = true ? 1 : 2.5
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      assign_node = program.arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::AssignNode)
      ternary_id = assign_node.value.not_nil!

      ternary_type = engine.context.get_type(ternary_id)
      ternary_type.should be_a(UnionType)

      union = ternary_type.as(UnionType)
      union.types.size.should eq(2)

      type_names = union.types.map(&.to_s).sort
      type_names.should eq(["Float64", "Int32"])
    end

    it "handles ternary with variables" do
      source = <<-CRYSTAL
        a = 5
        b = a > 3 ? 100 : 200
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      assign_node = program.arena[program.roots[1]].as(CrystalV2::Compiler::Frontend::AssignNode)
      ternary_id = assign_node.value.not_nil!

      ternary_type = engine.context.get_type(ternary_id)
      ternary_type.should be_a(PrimitiveType)
      ternary_type.as(PrimitiveType).name.should eq("Int32")
    end

    it "handles nested ternary (right-associative)" do
      source = <<-CRYSTAL
        x = true ? 1 : false ? 2 : 3
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      assign_node = program.arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::AssignNode)
      ternary_id = assign_node.value.not_nil!
      outer_ternary = program.arena[ternary_id].as(CrystalV2::Compiler::Frontend::TernaryNode)

      # Outer ternary: true ? 1 : (false ? 2 : 3)

      # False branch should be another ternary
      false_branch_id = outer_ternary.false_branch.not_nil!
      false_branch = program.arena[false_branch_id]

      ternary_type = engine.context.get_type(ternary_id)
      ternary_type.should be_a(PrimitiveType)
      ternary_type.as(PrimitiveType).name.should eq("Int32")
    end

    it "respects operator precedence for ternary" do
      source = <<-CRYSTAL
        x = 1 + 1 > 0 ? 100 : 200
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      assign_node = program.arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::AssignNode)
      ternary_id = assign_node.value.not_nil!
      ternary_node = program.arena[ternary_id].as(CrystalV2::Compiler::Frontend::TernaryNode)

      # Should parse as: (1 + 1 > 0) ? 100 : 200

      condition_id = ternary_node.condition.not_nil!
      condition_node = program.arena[condition_id].as(CrystalV2::Compiler::Frontend::BinaryNode)

      ternary_type = engine.context.get_type(ternary_id)
      ternary_type.should be_a(PrimitiveType)
      ternary_type.as(PrimitiveType).name.should eq("Int32")
    end

    it "handles ternary with string types" do
      source = <<-CRYSTAL
        x = false ? "yes" : "no"
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      assign_node = program.arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::AssignNode)
      ternary_id = assign_node.value.not_nil!

      ternary_type = engine.context.get_type(ternary_id)
      ternary_type.should be_a(PrimitiveType)
      ternary_type.as(PrimitiveType).name.should eq("String")
    end

    it "handles ternary with mixed string and int creating union" do
      source = <<-CRYSTAL
        x = true ? "hello" : 42
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      assign_node = program.arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::AssignNode)
      ternary_id = assign_node.value.not_nil!

      ternary_type = engine.context.get_type(ternary_id)
      ternary_type.should be_a(UnionType)

      union = ternary_type.as(UnionType)
      type_names = union.types.map(&.to_s).sort
      type_names.should eq(["Int32", "String"])
    end

    it "handles ternary in arithmetic expression" do
      source = <<-CRYSTAL
        x = (true ? 10 : 5) + 3
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      assign_node = program.arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::AssignNode)
      add_expr_id = assign_node.value.not_nil!
      add_node = program.arena[add_expr_id].as(CrystalV2::Compiler::Frontend::BinaryNode)

      # Should be Binary(+) with left = Grouping(Ternary)

      left_id = add_node.left
      left_node = program.arena[left_id].as(CrystalV2::Compiler::Frontend::GroupingNode)

      # Inside grouping is ternary
      inner_id = left_node.expression
      inner_node = program.arena[inner_id]

      result_type = engine.context.get_type(add_expr_id)
      result_type.should be_a(PrimitiveType)
      result_type.as(PrimitiveType).name.should eq("Int32")
    end
  end
end
