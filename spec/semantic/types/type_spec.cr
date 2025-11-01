require "spec"

# Load in dependency order to avoid circular dependencies
require "../../../src/compiler/frontend/ast"
require "../../../src/compiler/semantic/symbol_table"
require "../../../src/compiler/semantic/symbol"
require "../../../src/compiler/semantic/types/type"
require "../../../src/compiler/semantic/types/primitive_type"
require "../../../src/compiler/semantic/types/class_type"
require "../../../src/compiler/semantic/types/union_type"
require "../../../src/compiler/semantic/types/type_context"

include CrystalV2::Compiler::Semantic

# Note: ExprId alias is already available from Semantic module (defined in symbol.cr)

describe "Type System" do
  describe PrimitiveType do
    it "has structural equality - two Int32 are the same type" do
      type1 = PrimitiveType.new("Int32")
      type2 = PrimitiveType.new("Int32")
      type3 = PrimitiveType.new("String")

      (type1 == type2).should be_true
      (type1 == type3).should be_false
    end

    it "has consistent hash for equal types" do
      type1 = PrimitiveType.new("Int32")
      type2 = PrimitiveType.new("Int32")

      type1.hash.should eq(type2.hash)
    end

    it "converts to string representation" do
      PrimitiveType.new("Int32").to_s.should eq("Int32")
      PrimitiveType.new("String").to_s.should eq("String")
    end
  end

  describe ClassType do
    it "has nominal equality - compares by ClassSymbol identity" do
      # Create two different ClassSymbols with same name
      expr_id1 = ExprId.new(1)
      expr_id2 = ExprId.new(2)
      scope1 = SymbolTable.new(nil)
      scope2 = SymbolTable.new(nil)

      symbol1 = ClassSymbol.new("Foo", expr_id1, scope: scope1)
      symbol2 = ClassSymbol.new("Foo", expr_id2, scope: scope2)

      type1 = ClassType.new(symbol1)
      type2 = ClassType.new(symbol1)  # Same symbol
      type3 = ClassType.new(symbol2)  # Different symbol, same name

      (type1 == type2).should be_true   # Same symbol
      (type1 == type3).should be_false  # Different symbol (even if same name)
    end

    it "converts to string with class name" do
      expr_id = ExprId.new(1)
      scope = SymbolTable.new(nil)
      symbol = ClassSymbol.new("MyClass", expr_id, scope: scope)
      type = ClassType.new(symbol)

      type.to_s.should eq("MyClass")
    end

    it "supports type arguments for generics (future use)" do
      expr_id = ExprId.new(1)
      scope = SymbolTable.new(nil)
      symbol = ClassSymbol.new("Array", expr_id, scope: scope)
      elem_type = PrimitiveType.new("Int32")
      type = ClassType.new(symbol, [elem_type] of Type)

      type.to_s.should eq("Array(Int32)")
    end
  end

  describe UnionType do
    it "flattens nested unions" do
      int32 = PrimitiveType.new("Int32")
      string = PrimitiveType.new("String")
      bool = PrimitiveType.new("Bool")

      # Create (Int32 | String) first
      union1 = UnionType.new([int32, string])

      # Then create (Int32 | String) | Bool
      union2 = UnionType.new([union1, bool])

      # Should be flattened to Bool | Int32 | String (sorted)
      union2.types.size.should eq(3)
      union2.types[0].should eq(bool)    # "Bool" comes first alphabetically
      union2.types[1].should eq(int32)   # "Int32"
      union2.types[2].should eq(string)  # "String"
    end

    it "removes duplicate types" do
      int32 = PrimitiveType.new("Int32")
      string = PrimitiveType.new("String")

      # Int32 | String | Int32 → Int32 | String
      union = UnionType.new([int32, string, int32])

      union.types.size.should eq(2)
      union.types.should contain(int32)
      union.types.should contain(string)
    end

    it "has order-independent equality (normalized)" do
      int32 = PrimitiveType.new("Int32")
      string = PrimitiveType.new("String")

      union1 = UnionType.new([int32, string])
      union2 = UnionType.new([string, int32])  # Different order

      (union1 == union2).should be_true  # Normalized to same order
    end

    it "converts to string with | separator" do
      int32 = PrimitiveType.new("Int32")
      string = PrimitiveType.new("String")

      union = UnionType.new([int32, string])

      # Should be sorted: Int32 | String
      union.to_s.should eq("Int32 | String")
    end
  end

  describe TypeContext do
    it "initializes with built-in primitive types" do
      ctx = TypeContext.new

      ctx.int32_type.should be_a(PrimitiveType)
      ctx.string_type.should be_a(PrimitiveType)
      ctx.bool_type.should be_a(PrimitiveType)
      ctx.nil_type.should be_a(PrimitiveType)

      ctx.int32_type.name.should eq("Int32")
      ctx.string_type.name.should eq("String")
    end

    it "stores and retrieves expression types" do
      ctx = TypeContext.new
      expr_id = ExprId.new(42)

      ctx.get_type(expr_id).should be_nil

      ctx.set_type(expr_id, ctx.int32_type)
      ctx.get_type(expr_id).should eq(ctx.int32_type)
    end

    it "creates union types with normalization" do
      ctx = TypeContext.new

      # Create Int32 | String
      union = ctx.union_of([ctx.int32_type, ctx.string_type])

      union.should be_a(UnionType)
      union.to_s.should eq("Int32 | String")
    end

    it "handles edge case: union of single type returns that type" do
      ctx = TypeContext.new

      union = ctx.union_of([ctx.int32_type])

      union.should eq(ctx.int32_type)  # Not a UnionType, just Int32
    end

    it "handles edge case: union of empty array returns Nil" do
      ctx = TypeContext.new

      union = ctx.union_of([] of Type)

      union.should eq(ctx.nil_type)
    end

    it "creates nilable types (T | Nil)" do
      ctx = TypeContext.new

      nilable_int = ctx.nilable(ctx.int32_type)

      nilable_int.should be_a(UnionType)
      nilable_int.to_s.should eq("Int32 | Nil")
    end
  end
end
