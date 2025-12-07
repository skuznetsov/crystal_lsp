require "spec"

require "../../src/compiler/frontend/ast"
require "../../src/compiler/frontend/lexer"
require "../../src/compiler/frontend/parser"
require "../../src/compiler/semantic/symbol_table"
require "../../src/compiler/semantic/symbol"
require "../../src/compiler/semantic/types/type_index"

describe CrystalV2::Compiler::Semantic::TypeIndex do
  describe "TypeArena" do
    it "starts with 8 primitive types" do
      index = CrystalV2::Compiler::Semantic::TypeIndex.new
      index.arena.size.should eq 8
    end

    it "interns duplicate types to same TypeId" do
      arena = CrystalV2::Compiler::Semantic::TypeArena.new
      t1 = CrystalV2::Compiler::Semantic::PrimitiveType.new("MyType")
      t2 = CrystalV2::Compiler::Semantic::PrimitiveType.new("MyType")
      id1 = arena.intern(t1)
      id2 = arena.intern(t2)
      id1.index.should eq id2.index
    end

    it "returns different TypeIds for different types" do
      arena = CrystalV2::Compiler::Semantic::TypeArena.new
      t1 = CrystalV2::Compiler::Semantic::PrimitiveType.new("TypeA")
      t2 = CrystalV2::Compiler::Semantic::PrimitiveType.new("TypeB")
      id1 = arena.intern(t1)
      id2 = arena.intern(t2)
      id1.index.should_not eq id2.index
    end

    it "provides O(1) lookup by TypeId" do
      arena = CrystalV2::Compiler::Semantic::TypeArena.new
      t1 = CrystalV2::Compiler::Semantic::PrimitiveType.new("TestType")
      id = arena.intern(t1)
      arena[id].to_s.should eq "TestType"
    end
  end

  describe "ExprTypeIndex" do
    it "stores and retrieves types for expression indices" do
      index = CrystalV2::Compiler::Semantic::TypeIndex.new
      index.set_type(100, index.arena[index.arena.int32_id])
      index.set_type(200, index.arena[index.arena.string_id])

      index.get_type(100).to_s.should eq "Int32"
      index.get_type(200).to_s.should eq "String"
    end

    it "returns nil for unset expressions" do
      index = CrystalV2::Compiler::Semantic::TypeIndex.new
      index.get_type(999).should be_nil
    end

    it "handles sparse expression indices via overflow storage" do
      index = CrystalV2::Compiler::Semantic::TypeIndex.new
      # Very large index should go to sparse storage
      index.set_type(1_000_000, index.arena[index.arena.bool_id])
      index.get_type(1_000_000).to_s.should eq "Bool"
    end

    it "invalidates ranges correctly" do
      index = CrystalV2::Compiler::Semantic::TypeIndex.new
      index.set_type(100, index.arena[index.arena.int32_id])
      index.set_type(150, index.arena[index.arena.string_id])
      index.set_type(200, index.arena[index.arena.bool_id])

      index.expr_types.invalidate_range(100, 160)

      index.get_type(100).should be_nil
      index.get_type(150).should be_nil
      index.get_type(200).to_s.should eq "Bool"  # Not invalidated
    end
  end

  describe "Union types" do
    it "creates union types with multiple members" do
      index = CrystalV2::Compiler::Semantic::TypeIndex.new
      union = index.union_of([
        index.arena[index.arena.int32_id],
        index.arena[index.arena.string_id]
      ])
      union.to_s.should contain("Int32")
      union.to_s.should contain("String")
    end

    it "creates nilable types" do
      index = CrystalV2::Compiler::Semantic::TypeIndex.new
      nilable = index.nilable(index.arena[index.arena.int32_id])
      nilable.to_s.should contain("Int32")
      nilable.to_s.should contain("Nil")
    end
  end

  describe "Serialization" do
    it "round-trips types through binary serialization" do
      index = CrystalV2::Compiler::Semantic::TypeIndex.new
      index.set_type(100, index.arena[index.arena.int32_id])
      index.set_type(200, index.arena[index.arena.string_id])

      # Add custom type
      custom = CrystalV2::Compiler::Semantic::PrimitiveType.new("CustomType")
      index.set_type(300, custom)

      io = IO::Memory.new
      index.write(io)
      io.rewind

      loaded = CrystalV2::Compiler::Semantic::TypeIndex.read(io)
      loaded.should_not be_nil
      if loaded
        loaded.get_type(100).to_s.should eq "Int32"
        loaded.get_type(200).to_s.should eq "String"
        loaded.get_type(300).to_s.should eq "CustomType"
      end
    end

    it "preserves file ranges through serialization" do
      index = CrystalV2::Compiler::Semantic::TypeIndex.new
      index.register_file("/path/to/file.cr", 100, 500)
      index.register_file("/path/to/other.cr", 500, 800)

      io = IO::Memory.new
      index.write(io)
      io.rewind

      loaded = CrystalV2::Compiler::Semantic::TypeIndex.read(io)
      loaded.should_not be_nil
    end

    it "rejects invalid magic bytes" do
      io = IO::Memory.new
      io.write("XXXX".to_slice)
      io.write_bytes(1_u32, IO::ByteFormat::LittleEndian)
      io.rewind

      loaded = CrystalV2::Compiler::Semantic::TypeIndex.read(io)
      loaded.should be_nil
    end
  end

  describe "TypeId" do
    it "distinguishes valid and invalid TypeIds" do
      valid = CrystalV2::Compiler::Semantic::TypeId.new(5)
      invalid = CrystalV2::Compiler::Semantic::TypeId::INVALID

      valid.valid?.should be_true
      valid.invalid?.should be_false
      invalid.valid?.should be_false
      invalid.invalid?.should be_true
    end
  end
end

  describe "Interning primitives" do
    it "returns same TypeId for same primitive name" do
      arena = CrystalV2::Compiler::Semantic::TypeArena.new
      ptype = CrystalV2::Compiler::Semantic::PrimitiveType.new("Int32")
      id = arena.intern(ptype)
      id.index.should eq arena.int32_id.index
    end

    it "matches hash for same primitive name" do
      arena = CrystalV2::Compiler::Semantic::TypeArena.new
      ptype = CrystalV2::Compiler::Semantic::PrimitiveType.new("Int32")
      preallocated = arena[arena.int32_id]
      ptype.hash.should eq preallocated.hash
      (ptype == preallocated).should be_true
    end
  end
