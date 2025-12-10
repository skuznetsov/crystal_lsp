require "../spec_helper"
require "../../src/compiler/mir/mir"
require "../../src/compiler/mir/llvm_backend"

describe Crystal::MIR::LLVMTypeMapper do
  describe "#llvm_type" do
    it "maps primitive types correctly" do
      registry = Crystal::MIR::TypeRegistry.new
      mapper = Crystal::MIR::LLVMTypeMapper.new(registry)

      mapper.llvm_type(Crystal::MIR::TypeRef::VOID).should eq("void")
      mapper.llvm_type(Crystal::MIR::TypeRef::BOOL).should eq("i1")
      mapper.llvm_type(Crystal::MIR::TypeRef::INT8).should eq("i8")
      mapper.llvm_type(Crystal::MIR::TypeRef::INT16).should eq("i16")
      mapper.llvm_type(Crystal::MIR::TypeRef::INT32).should eq("i32")
      mapper.llvm_type(Crystal::MIR::TypeRef::INT64).should eq("i64")
      mapper.llvm_type(Crystal::MIR::TypeRef::INT128).should eq("i128")
      mapper.llvm_type(Crystal::MIR::TypeRef::UINT8).should eq("i8")
      mapper.llvm_type(Crystal::MIR::TypeRef::UINT32).should eq("i32")
      mapper.llvm_type(Crystal::MIR::TypeRef::FLOAT32).should eq("float")
      mapper.llvm_type(Crystal::MIR::TypeRef::FLOAT64).should eq("double")
      mapper.llvm_type(Crystal::MIR::TypeRef::POINTER).should eq("ptr")
    end

    it "maps struct types to named types" do
      registry = Crystal::MIR::TypeRegistry.new
      struct_type = registry.create_type(Crystal::MIR::TypeKind::Struct, "MyStruct", 16, 8)
      mapper = Crystal::MIR::LLVMTypeMapper.new(registry)

      mapper.llvm_type(struct_type).should eq("%MyStruct")
    end

    it "maps reference types to ptr" do
      registry = Crystal::MIR::TypeRegistry.new
      ref_type = registry.create_type(Crystal::MIR::TypeKind::Reference, "MyClass", 8, 8)
      mapper = Crystal::MIR::LLVMTypeMapper.new(registry)

      mapper.llvm_type(ref_type).should eq("ptr")
    end

    it "maps union types with .union suffix" do
      registry = Crystal::MIR::TypeRegistry.new
      union_type = registry.create_type(Crystal::MIR::TypeKind::Union, "IntOrString", 16, 8)
      mapper = Crystal::MIR::LLVMTypeMapper.new(registry)

      mapper.llvm_type(union_type).should eq("%IntOrString.union")
    end

    it "mangles special characters in type names" do
      registry = Crystal::MIR::TypeRegistry.new
      mapper = Crystal::MIR::LLVMTypeMapper.new(registry)

      mapper.mangle_name("Array(Int32)").should eq("Array_Int32_")
      mapper.mangle_name("Hash(String, Int32)").should eq("Hash_String__Int32_")
    end
  end
end

describe Crystal::MIR::LLVMIRGenerator do
  describe "#generate" do
    it "generates module header" do
      mod = Crystal::MIR::Module.new("test_module")
      mod.source_file = "test.cr"

      gen = Crystal::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      output.should contain("; ModuleID = 'test_module'")
      output.should contain("source_filename = \"test.cr\"")
      output.should contain("target triple")
    end

    it "generates runtime declarations" do
      mod = Crystal::MIR::Module.new("test")
      gen = Crystal::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      output.should contain("declare ptr @__crystal_malloc64(i64)")
      output.should contain("declare void @__crystal_rc_inc(ptr)")
      output.should contain("declare void @__crystal_rc_dec(ptr, ptr)")
      output.should contain("declare ptr @__crystal_slab_alloc(i32)")
    end

    it "generates simple function" do
      mod = Crystal::MIR::Module.new("test")
      func = mod.create_function("add", Crystal::MIR::TypeRef::INT32)
      func.add_param("a", Crystal::MIR::TypeRef::INT32)
      func.add_param("b", Crystal::MIR::TypeRef::INT32)

      builder = Crystal::MIR::Builder.new(func)
      # a + b
      sum = builder.add(0_u32, 1_u32, Crystal::MIR::TypeRef::INT32)
      builder.ret(sum)

      gen = Crystal::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      output.should contain("define i32 @add(i32 %a, i32 %b)")
      output.should contain("add i32")
      output.should contain("ret")
    end

    it "generates stack allocation" do
      mod = Crystal::MIR::Module.new("test")
      func = mod.create_function("alloc_test", Crystal::MIR::TypeRef::VOID)

      builder = Crystal::MIR::Builder.new(func)
      ptr = builder.alloc(Crystal::MIR::MemoryStrategy::Stack, Crystal::MIR::TypeRef::INT32, 4_u64, 4_u32)
      builder.ret

      gen = Crystal::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      output.should contain("alloca i32, align 4")
    end

    it "generates ARC allocation with RC initialization" do
      mod = Crystal::MIR::Module.new("test")
      func = mod.create_function("arc_test", Crystal::MIR::TypeRef::POINTER)

      builder = Crystal::MIR::Builder.new(func)
      ptr = builder.alloc(Crystal::MIR::MemoryStrategy::ARC, Crystal::MIR::TypeRef::STRING, 32_u64, 8_u32)
      builder.ret(ptr)

      gen = Crystal::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      output.should contain("call ptr @__crystal_malloc64(i64 40)")  # 32 + 8 for RC
      output.should contain("store i64 1")  # Initialize RC to 1
      output.should contain("getelementptr i8")  # Skip RC to get object pointer
    end

    it "generates slab allocation" do
      mod = Crystal::MIR::Module.new("test")
      func = mod.create_function("slab_test", Crystal::MIR::TypeRef::POINTER)

      builder = Crystal::MIR::Builder.new(func)
      ptr = builder.alloc(Crystal::MIR::MemoryStrategy::Slab, Crystal::MIR::TypeRef::INT32, 16_u64, 4_u32)
      builder.ret(ptr)

      gen = Crystal::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      output.should contain("call ptr @__crystal_slab_alloc(i32 0)")  # Size class 0 for <=16 bytes
    end

    it "generates RC increment and decrement" do
      mod = Crystal::MIR::Module.new("test")
      func = mod.create_function("rc_test", Crystal::MIR::TypeRef::VOID)
      func.add_param("ptr", Crystal::MIR::TypeRef::POINTER)

      builder = Crystal::MIR::Builder.new(func)
      builder.rc_inc(0_u32)
      builder.rc_dec(0_u32)
      builder.ret

      gen = Crystal::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      # Note: value_ref uses value ID, so %r0 instead of %ptr
      output.should contain("call void @__crystal_rc_inc(ptr %r0)")
      output.should contain("call void @__crystal_rc_dec(ptr %r0, ptr null)")
    end

    it "generates binary operations" do
      mod = Crystal::MIR::Module.new("test")
      func = mod.create_function("binop_test", Crystal::MIR::TypeRef::INT32)
      func.add_param("a", Crystal::MIR::TypeRef::INT32)
      func.add_param("b", Crystal::MIR::TypeRef::INT32)

      builder = Crystal::MIR::Builder.new(func)
      sum = builder.add(0_u32, 1_u32, Crystal::MIR::TypeRef::INT32)
      diff = builder.sub(0_u32, 1_u32, Crystal::MIR::TypeRef::INT32)
      prod = builder.mul(0_u32, 1_u32, Crystal::MIR::TypeRef::INT32)
      builder.ret(prod)

      gen = Crystal::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      output.should contain("add i32")
      output.should contain("sub i32")
      output.should contain("mul i32")
    end

    it "generates conditional branch" do
      mod = Crystal::MIR::Module.new("test")
      func = mod.create_function("branch_test", Crystal::MIR::TypeRef::INT32)
      func.add_param("cond", Crystal::MIR::TypeRef::BOOL)
      func.add_param("a", Crystal::MIR::TypeRef::INT32)
      func.add_param("b", Crystal::MIR::TypeRef::INT32)

      then_block = func.create_block
      else_block = func.create_block
      exit_block = func.create_block

      builder = Crystal::MIR::Builder.new(func)
      builder.branch(0_u32, then_block, else_block)

      builder.current_block = then_block
      builder.jump(exit_block)

      builder.current_block = else_block
      builder.jump(exit_block)

      builder.current_block = exit_block
      builder.ret(1_u32)

      gen = Crystal::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      output.should contain("br i1")
      output.should contain("br label")
    end
  end

  describe "type metadata generation" do
    it "generates __crystal_type_count global" do
      mod = Crystal::MIR::Module.new("test")

      gen = Crystal::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = true
      output = gen.generate

      output.should contain("@__crystal_type_count = constant i32")
    end

    it "generates __crystal_type_info array" do
      mod = Crystal::MIR::Module.new("test")

      gen = Crystal::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = true
      output = gen.generate

      output.should contain("@__crystal_type_info = constant")
      output.should contain("%__crystal_type_info_entry = type { i32, i32, i32, i32, i32, i32, i32, i32 }")
    end

    it "generates __crystal_type_strings table" do
      mod = Crystal::MIR::Module.new("test")

      gen = Crystal::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = true
      output = gen.generate

      output.should contain("@__crystal_type_strings = constant")
    end

    it "includes primitive types in metadata" do
      mod = Crystal::MIR::Module.new("test")

      gen = Crystal::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = true
      output = gen.generate

      # Should have entries for all primitive types
      # TypeRegistry registers 19 primitive types (including Pointer)
      output.should contain("@__crystal_type_count = constant i32 19")
    end

    it "generates field info for struct types" do
      mod = Crystal::MIR::Module.new("test")

      # Create a struct type with fields
      struct_type = mod.type_registry.create_type(
        Crystal::MIR::TypeKind::Struct,
        "Point",
        8_u64,
        4_u32
      )
      struct_type.add_field("x", Crystal::MIR::TypeRef::INT32, 0_u32)
      struct_type.add_field("y", Crystal::MIR::TypeRef::INT32, 4_u32)

      gen = Crystal::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = true
      output = gen.generate

      output.should contain("@__crystal_field_info = constant")
      output.should contain("%__crystal_field_info_entry = type { i32, i32, i32, i32 }")
    end
  end
end

describe Crystal::MIR::TypeRegistry do
  it "pre-registers primitive types" do
    registry = Crystal::MIR::TypeRegistry.new

    int32 = registry.get(Crystal::MIR::TypeRef::INT32)
    int32.should_not be_nil
    int32.not_nil!.name.should eq("Int32")
    int32.not_nil!.kind.should eq(Crystal::MIR::TypeKind::Int32)
    int32.not_nil!.size.should eq(4)
  end

  it "creates custom types" do
    registry = Crystal::MIR::TypeRegistry.new

    point = registry.create_type(Crystal::MIR::TypeKind::Struct, "Point", 8_u64, 4_u32)
    point.id.should be >= 100  # Custom types start at ID 100
    point.name.should eq("Point")
    point.kind.should eq(Crystal::MIR::TypeKind::Struct)
  end

  it "looks up types by name" do
    registry = Crystal::MIR::TypeRegistry.new

    int32 = registry.get_by_name("Int32")
    int32.should_not be_nil
    int32.not_nil!.id.should eq(Crystal::MIR::TypeRef::INT32.id)
  end
end

describe Crystal::MIR::Type do
  it "supports adding fields" do
    type = Crystal::MIR::Type.new(100_u32, Crystal::MIR::TypeKind::Struct, "Point", 8_u64, 4_u32)

    type.add_field("x", Crystal::MIR::TypeRef::INT32, 0_u32)
    type.add_field("y", Crystal::MIR::TypeRef::INT32, 4_u32)

    type.fields.should_not be_nil
    type.fields.not_nil!.size.should eq(2)
    type.fields.not_nil![0].name.should eq("x")
    type.fields.not_nil![1].name.should eq("y")
  end

  it "detects value types" do
    struct_type = Crystal::MIR::Type.new(100_u32, Crystal::MIR::TypeKind::Struct, "Point", 8_u64, 4_u32)
    class_type = Crystal::MIR::Type.new(101_u32, Crystal::MIR::TypeKind::Reference, "Object", 8_u64, 8_u32)

    struct_type.is_value_type?.should be_true
    class_type.is_value_type?.should be_false
  end
end
