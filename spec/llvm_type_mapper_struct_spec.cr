require "../spec_helper"

describe Crystal::MIR::LLVMTypeMapper do
  it "maps struct with fields to named type" do
    registry = Crystal::MIR::TypeRegistry.new
    t = registry.create_type(Crystal::MIR::TypeKind::Struct, "Foo", 16, 8)
    t.add_field("a", Crystal::MIR::TypeRef::INT32, 0)
    t.add_field("b", Crystal::MIR::TypeRef::INT64, 8)

    mapper = Crystal::MIR::LLVMTypeMapper.new(registry)
    mapper.llvm_type(t).should eq("%Foo")
    mapper.llvm_alloca_type(Crystal::MIR::TypeRef.new(t.id)).should eq("%Foo")
  end
end
