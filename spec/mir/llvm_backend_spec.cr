require "../spec_helper"
require "../../src/compiler/mir/mir"
require "../../src/compiler/mir/hir_to_mir"
require "../../src/compiler/mir/optimizations"
require "../../src/compiler/mir/llvm_backend"

class Adamas::MIR::DwarfDebugContext
  def __test_stable_metadata_id(key : String, base : Int32, span : Int32) : Int32
    stable_metadata_id(key, base, span)
  end

  def __test_unique_stable_metadata_id(
    assigned_ids : Hash(Int32, String),
    key : String,
    base : Int32,
    span : Int32,
  ) : Int32
    unique_stable_metadata_id(assigned_ids, key, base, span)
  end
end

describe Adamas::MIR::LLVMTypeMapper do
  describe "#llvm_type" do
    it "maps primitive types correctly" do
      registry = Adamas::MIR::TypeRegistry.new
      mapper = Adamas::MIR::LLVMTypeMapper.new(registry)

      mapper.llvm_type(Adamas::MIR::TypeRef::VOID).should eq("void")
      mapper.llvm_type(Adamas::MIR::TypeRef::BOOL).should eq("i1")
      mapper.llvm_type(Adamas::MIR::TypeRef::INT8).should eq("i8")
      mapper.llvm_type(Adamas::MIR::TypeRef::INT16).should eq("i16")
      mapper.llvm_type(Adamas::MIR::TypeRef::INT32).should eq("i32")
      mapper.llvm_type(Adamas::MIR::TypeRef::INT64).should eq("i64")
      mapper.llvm_type(Adamas::MIR::TypeRef::INT128).should eq("i128")
      mapper.llvm_type(Adamas::MIR::TypeRef::UINT8).should eq("i8")
      mapper.llvm_type(Adamas::MIR::TypeRef::UINT32).should eq("i32")
      mapper.llvm_type(Adamas::MIR::TypeRef::FLOAT32).should eq("float")
      mapper.llvm_type(Adamas::MIR::TypeRef::FLOAT64).should eq("double")
      mapper.llvm_type(Adamas::MIR::TypeRef::POINTER).should eq("ptr")
    end

    it "maps struct types to ptr (structs passed by pointer in ABI)" do
      registry = Adamas::MIR::TypeRegistry.new
      struct_type = registry.create_type(Adamas::MIR::TypeKind::Struct, "MyStruct", 16, 8)
      mapper = Adamas::MIR::LLVMTypeMapper.new(registry)

      # Structs are passed by pointer in our ABI for consistency
      mapper.llvm_type(struct_type).should eq("ptr")
    end

    it "maps struct types to named types for alloca" do
      registry = Adamas::MIR::TypeRegistry.new
      struct_type = registry.create_type(Adamas::MIR::TypeKind::Struct, "MyStruct", 16, 8)
      mapper = Adamas::MIR::LLVMTypeMapper.new(registry)

      # For alloca, we need the actual struct type (via TypeRef)
      struct_type_ref = Adamas::MIR::TypeRef.new(struct_type.id)
      mapper.llvm_alloca_type(struct_type_ref).should eq("%MyStruct")
    end

    it "maps reference types to ptr" do
      registry = Adamas::MIR::TypeRegistry.new
      ref_type = registry.create_type(Adamas::MIR::TypeKind::Reference, "MyClass", 8, 8)
      mapper = Adamas::MIR::LLVMTypeMapper.new(registry)

      mapper.llvm_type(ref_type).should eq("ptr")
    end

    it "maps union types with .union suffix" do
      registry = Adamas::MIR::TypeRegistry.new
      union_type = registry.create_type(Adamas::MIR::TypeKind::Union, "IntOrString", 16, 8)
      mapper = Adamas::MIR::LLVMTypeMapper.new(registry)

      mapper.llvm_type(union_type).should eq("%IntOrString.union")
    end

    it "mangles special characters in type names" do
      registry = Adamas::MIR::TypeRegistry.new
      mapper = Adamas::MIR::LLVMTypeMapper.new(registry)

      mapper.mangle_name("Array(Int32)").should eq("Array$LInt32$R")
      mapper.mangle_name("Hash(String, Int32)").should eq("Hash$LString$C$_Int32$R")
    end
  end
end

describe Adamas::MIR::LLVMIRGenerator do
  describe "#generate" do
    it "generates module header" do
      mod = Adamas::MIR::Module.new("test_module")
      mod.source_file = "test.cr"

      gen = Adamas::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      output.should contain("; ModuleID = 'test_module'")
      output.should contain("source_filename = \"test.cr\"")
      output.should contain("target triple")
    end

    it "generates runtime definitions" do
      mod = Adamas::MIR::Module.new("test")
      gen = Adamas::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      # Runtime functions are now defined with implementations
      output.should contain("define ptr @__adamas_malloc64(i64 %size)")
      output.should contain("define void @__adamas_rc_inc(ptr %ptr)")
      output.should contain("define void @__adamas_rc_dec(ptr %ptr, ptr %destructor)")
      output.should contain("define ptr @__adamas_slab_alloc(i32 %size_class)")
      output.should contain("shl i64 16, %size")
      output.should contain("define void @__adamas_slab_frame_push()")
      output.should contain("define void @__adamas_slab_frame_pop()")
    end

    it "emits entrypoint when __adamas_main is present" do
      mod = Adamas::MIR::Module.new("test")
      func = mod.create_function("__adamas_main", Adamas::MIR::TypeRef::VOID)
      builder = Adamas::MIR::Builder.new(func)
      builder.ret

      gen = Adamas::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      gen.reachability = true
      output = gen.generate

      output.should contain("define i32 @main")
      output.should contain("call void @__adamas_main")
    end

    it "emits slab frame prolog/epilog when enabled" do
      mod = Adamas::MIR::Module.new("test")
      func = mod.create_function("foo", Adamas::MIR::TypeRef::VOID)
      func.slab_frame = true
      builder = Adamas::MIR::Builder.new(func)
      builder.ret

      gen = Adamas::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      output.should contain("call void @__adamas_slab_frame_push()")
      output.should contain("call void @__adamas_slab_frame_pop()")
    end

    it "keeps entrypoint when __adamas_main contains typeof_ extern calls" do
      mod = Adamas::MIR::Module.new("test")
      func = mod.create_function("__adamas_main", Adamas::MIR::TypeRef::VOID)
      builder = Adamas::MIR::Builder.new(func)
      args = [] of Adamas::MIR::ValueId
      builder.extern_call("typeof_foo", args, Adamas::MIR::TypeRef::VOID)
      builder.ret

      gen = Adamas::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      gen.reachability = true
      output = gen.generate

      output.should contain("define i32 @main")
      output.should contain("call void @__adamas_main")
    end

    it "generates simple function" do
      mod = Adamas::MIR::Module.new("test")
      func = mod.create_function("add", Adamas::MIR::TypeRef::INT32)
      func.add_param("a", Adamas::MIR::TypeRef::INT32)
      func.add_param("b", Adamas::MIR::TypeRef::INT32)

      builder = Adamas::MIR::Builder.new(func)
      # a + b
      sum = builder.add(0_u32, 1_u32, Adamas::MIR::TypeRef::INT32)
      builder.ret(sum)

      gen = Adamas::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      output.should contain("define i32 @add(i32 %a, i32 %b)")
      output.should contain("add i32")
      output.should contain("ret")
    end

    it "generates stack allocation" do
      mod = Adamas::MIR::Module.new("test")
      func = mod.create_function("alloc_test", Adamas::MIR::TypeRef::VOID)

      builder = Adamas::MIR::Builder.new(func)
      ptr = builder.alloc(Adamas::MIR::MemoryStrategy::Stack, Adamas::MIR::TypeRef::INT32, 4_u64, 4_u32)
      builder.ret

      gen = Adamas::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      output.should contain("alloca i32, align 4")
    end

    it "generates ARC allocation with RC initialization" do
      mod = Adamas::MIR::Module.new("test")
      func = mod.create_function("arc_test", Adamas::MIR::TypeRef::POINTER)

      builder = Adamas::MIR::Builder.new(func)
      ptr = builder.alloc(Adamas::MIR::MemoryStrategy::ARC, Adamas::MIR::TypeRef::STRING, 32_u64, 8_u32)
      builder.ret(ptr)

      gen = Adamas::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      output.should contain("call ptr @__adamas_malloc64(i64 40)")  # 32 + 8 for RC
      output.should contain("store i64 1")  # Initialize RC to 1
      output.should contain("getelementptr i8")  # Skip RC to get object pointer
    end

    it "generates slab allocation" do
      mod = Adamas::MIR::Module.new("test")
      func = mod.create_function("slab_test", Adamas::MIR::TypeRef::POINTER)

      builder = Adamas::MIR::Builder.new(func)
      ptr = builder.alloc(Adamas::MIR::MemoryStrategy::Slab, Adamas::MIR::TypeRef::INT32, 16_u64, 4_u32)
      builder.ret(ptr)

      gen = Adamas::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      output.should contain("call ptr @__adamas_slab_alloc(i32 0)")  # Size class 0 for <=16 bytes
    end

    it "generates slab free" do
      mod = Adamas::MIR::Module.new("test")
      func = mod.create_function("slab_free_test", Adamas::MIR::TypeRef::VOID)

      builder = Adamas::MIR::Builder.new(func)
      ptr = builder.alloc(Adamas::MIR::MemoryStrategy::Slab, Adamas::MIR::TypeRef::INT32, 16_u64, 4_u32)
      builder.free(ptr, Adamas::MIR::MemoryStrategy::Slab)
      builder.ret

      gen = Adamas::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      output.should contain("call void @__adamas_slab_free(ptr %")
    end

    it "normalizes union stores to zeroinitializer" do
      mod = Adamas::MIR::Module.new("test")
      union_type = mod.type_registry.create_type(Adamas::MIR::TypeKind::Union, "IntOrNil", 16, 8)
      union_ref = Adamas::MIR::TypeRef.new(union_type.id)
      mod.register_union(
        union_ref,
        Adamas::MIR::UnionDescriptor.new(
          "IntOrNil",
          [
            Adamas::MIR::UnionVariantDescriptor.new(
              type_id: 0,
              type_ref: Adamas::MIR::TypeRef::INT32,
              full_name: "Int32",
              size: 4,
              alignment: 4,
              field_offsets: nil
            ),
            Adamas::MIR::UnionVariantDescriptor.new(
              type_id: 1,
              type_ref: Adamas::MIR::TypeRef::NIL,
              full_name: "Nil",
              size: 0,
              alignment: 1,
              field_offsets: nil
            ),
          ],
          16,
          8
        )
      )

      func = mod.create_function("union_store", Adamas::MIR::TypeRef::VOID)
      builder = Adamas::MIR::Builder.new(func)
      ptr = builder.alloc(Adamas::MIR::MemoryStrategy::Stack, union_ref, 16_u64, 8_u32)

      block = func.get_block(func.entry_block)
      union_nil = Adamas::MIR::Constant.new(func.next_value_id, union_ref, nil)
      block.add(union_nil)
      builder.store(ptr, union_nil.id)
      builder.ret

      gen = Adamas::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      output.should contain("store %IntOrNil.union zeroinitializer")
    end

    it "normalizes union nil array element stores to zeroinitializer" do
      mod = Adamas::MIR::Module.new("test")
      union_type = mod.type_registry.create_type(Adamas::MIR::TypeKind::Union, "IntOrNil", 16, 8)
      union_ref = Adamas::MIR::TypeRef.new(union_type.id)
      mod.register_union(
        union_ref,
        Adamas::MIR::UnionDescriptor.new(
          "IntOrNil",
          [
            Adamas::MIR::UnionVariantDescriptor.new(
              type_id: 0,
              type_ref: Adamas::MIR::TypeRef::INT32,
              full_name: "Int32",
              size: 4,
              alignment: 4,
              field_offsets: nil
            ),
            Adamas::MIR::UnionVariantDescriptor.new(
              type_id: 1,
              type_ref: Adamas::MIR::TypeRef::NIL,
              full_name: "Nil",
              size: 0,
              alignment: 1,
              field_offsets: nil
            ),
          ],
          16,
          8
        )
      )

      func = mod.create_function("union_array_set", Adamas::MIR::TypeRef::VOID)
      array_param = func.add_param("arr", Adamas::MIR::TypeRef::POINTER)
      builder = Adamas::MIR::Builder.new(func)
      index = builder.const_int(0_i64, Adamas::MIR::TypeRef::INT32)

      block = func.get_block(func.entry_block)
      union_nil = Adamas::MIR::Constant.new(func.next_value_id, union_ref, nil)
      block.add(union_nil)
      block.add(Adamas::MIR::ArraySet.new(func.next_value_id, union_ref, array_param, index, union_nil.id))
      builder.ret

      gen = Adamas::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      output.should contain("store %IntOrNil.union zeroinitializer")
      output.should_not contain("store %IntOrNil.union 0")
    end

    it "skips emitting SSA values for void casts" do
      mod = Adamas::MIR::Module.new("test")
      func = mod.create_function("void_cast", Adamas::MIR::TypeRef::VOID)
      builder = Adamas::MIR::Builder.new(func)
      val = builder.const_int(1_i64, Adamas::MIR::TypeRef::INT32)
      builder.cast(Adamas::MIR::CastKind::Bitcast, val, Adamas::MIR::TypeRef::VOID)
      builder.ret

      gen = Adamas::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      output.should_not contain("alloca void")
    end

    it "casts ptr to float64 via ptrtoint + uitofp (without dereference)" do
      mod = Adamas::MIR::Module.new("test")
      func = mod.create_function("ptr_to_float_cast", Adamas::MIR::TypeRef::FLOAT64)
      func.add_param("p", Adamas::MIR::TypeRef::POINTER)
      builder = Adamas::MIR::Builder.new(func)

      casted = builder.cast(Adamas::MIR::CastKind::Bitcast, 0_u32, Adamas::MIR::TypeRef::FLOAT64)
      builder.ret(casted)

      gen = Adamas::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      func_ir = output[/define double @ptr_to_float_cast\(ptr %p\)\s*\{.*?\n\}/m]
      func_ir.should_not be_nil
      body = func_ir.not_nil!

      body.should contain("ptrtoint ptr %p to i64")
      body.should contain("uitofp i64")
      body.should_not contain("load double, ptr %p")
    end

    it "uses zext for unsigned fixed vararg widening in extern call" do
      mod = Adamas::MIR::Module.new("test")
      func = mod.create_function("extern_unsigned_widen", Adamas::MIR::TypeRef::INT32)
      func.add_param("fmt", Adamas::MIR::TypeRef::POINTER)
      builder = Adamas::MIR::Builder.new(func)

      dst = builder.const_nil_typed(Adamas::MIR::TypeRef::POINTER)
      len = builder.const_uint(255_u64, Adamas::MIR::TypeRef::UINT32)
      call_args = [dst, len, 0_u32] of Adamas::MIR::ValueId
      res = builder.extern_call("snprintf", call_args, Adamas::MIR::TypeRef::INT32)
      builder.ret(res)

      gen = Adamas::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      func_ir = output[/define i32 @extern_unsigned_widen\(ptr %fmt\)\s*\{.*?\n\}/m]
      func_ir.should_not be_nil
      body = func_ir.not_nil!
      cast_lines = body.lines.select(&.includes?("varargs_cast."))
      cast_lines.should_not be_empty
      cast_blob = cast_lines.join("\n")
      cast_blob.should contain("zext i32")
      cast_blob.should_not contain("sext i32")
      body.should contain("call i32 (ptr, i64, ptr, ...) @snprintf(")
    end

    it "uses fptoui for float64 to uint32 cast" do
      mod = Adamas::MIR::Module.new("test")
      func = mod.create_function("float_to_uint_cast", Adamas::MIR::TypeRef::UINT32)
      func.add_param("x", Adamas::MIR::TypeRef::FLOAT64)
      builder = Adamas::MIR::Builder.new(func)

      casted = builder.cast(Adamas::MIR::CastKind::Bitcast, 0_u32, Adamas::MIR::TypeRef::UINT32)
      builder.ret(casted)

      gen = Adamas::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      func_ir = output[/define i32 @float_to_uint_cast\(double %x\)\s*\{.*?\n\}/m]
      func_ir.should_not be_nil
      body = func_ir.not_nil!
      body.should contain("fptoui double %x to i32")
      body.should_not contain("fptosi double %x to i32")
    end

    it "uses uitofp for uint32 to float64 cast" do
      mod = Adamas::MIR::Module.new("test")
      func = mod.create_function("uint_to_float_cast", Adamas::MIR::TypeRef::FLOAT64)
      func.add_param("x", Adamas::MIR::TypeRef::UINT32)
      builder = Adamas::MIR::Builder.new(func)

      casted = builder.cast(Adamas::MIR::CastKind::Bitcast, 0_u32, Adamas::MIR::TypeRef::FLOAT64)
      builder.ret(casted)

      gen = Adamas::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      func_ir = output[/define double @uint_to_float_cast\(i32 %x\)\s*\{.*?\n\}/m]
      func_ir.should_not be_nil
      body = func_ir.not_nil!
      body.should contain("uitofp i32 %x to double")
      body.should_not contain("sitofp i32 %x to double")
    end

    it "uses uitofp for uint64 to float64 cast" do
      mod = Adamas::MIR::Module.new("test")
      func = mod.create_function("uint64_to_float_cast", Adamas::MIR::TypeRef::FLOAT64)
      func.add_param("x", Adamas::MIR::TypeRef::UINT64)
      builder = Adamas::MIR::Builder.new(func)

      casted = builder.cast(Adamas::MIR::CastKind::UIToFP, 0_u32, Adamas::MIR::TypeRef::FLOAT64)
      builder.ret(casted)

      gen = Adamas::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      func_ir = output[/define double @uint64_to_float_cast\(i64 %x\)\s*\{.*?\n\}/m]
      func_ir.should_not be_nil
      body = func_ir.not_nil!
      body.should contain("uitofp i64 %x to double")
      body.should_not contain("sitofp i64 %x to double")
    end

    it "uses uitofp for uint128 to float64 cast" do
      mod = Adamas::MIR::Module.new("test")
      func = mod.create_function("uint128_to_float_cast", Adamas::MIR::TypeRef::FLOAT64)
      func.add_param("x", Adamas::MIR::TypeRef::UINT128)
      builder = Adamas::MIR::Builder.new(func)

      casted = builder.cast(Adamas::MIR::CastKind::UIToFP, 0_u32, Adamas::MIR::TypeRef::FLOAT64)
      builder.ret(casted)

      gen = Adamas::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      func_ir = output[/define double @uint128_to_float_cast\(i128 %x\)\s*\{.*?\n\}/m]
      func_ir.should_not be_nil
      body = func_ir.not_nil!
      body.should contain("uitofp i128 %x to double")
      body.should_not contain("sitofp i128 %x to double")
    end

    it "uses uitofp for uint32 argument when calling float64 callee" do
      mod = Adamas::MIR::Module.new("test")

      callee = mod.create_function("takes_f64_arg", Adamas::MIR::TypeRef::FLOAT64)
      callee.add_param("x", Adamas::MIR::TypeRef::FLOAT64)
      callee_builder = Adamas::MIR::Builder.new(callee)
      callee_builder.ret(0_u32)

      caller = mod.create_function("call_uint_to_f64_arg", Adamas::MIR::TypeRef::FLOAT64)
      caller.add_param("x", Adamas::MIR::TypeRef::UINT32)
      caller_builder = Adamas::MIR::Builder.new(caller)
      call = caller_builder.call(callee.id, ([0_u32] of Adamas::MIR::ValueId), Adamas::MIR::TypeRef::FLOAT64)
      caller_builder.ret(call)

      gen = Adamas::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      func_ir = output[/define double @call_uint_to_f64_arg\([^)]*\)\s*\{.*?\n\}/m]
      func_ir.should_not be_nil
      body = func_ir.not_nil!

      body.should contain("uitofp i32 %x to double")
      body.should_not contain("sitofp i32 %x to double")
    end

    it "uses fptoui for float64 argument when calling uint32 callee" do
      mod = Adamas::MIR::Module.new("test")

      callee = mod.create_function("takes_u32_arg", Adamas::MIR::TypeRef::UINT32)
      callee.add_param("x", Adamas::MIR::TypeRef::UINT32)
      callee_builder = Adamas::MIR::Builder.new(callee)
      callee_builder.ret(0_u32)

      caller = mod.create_function("call_f64_to_u32_arg", Adamas::MIR::TypeRef::UINT32)
      caller.add_param("x", Adamas::MIR::TypeRef::FLOAT64)
      caller_builder = Adamas::MIR::Builder.new(caller)
      call = caller_builder.extern_call("takes_u32_arg", ([0_u32] of Adamas::MIR::ValueId), Adamas::MIR::TypeRef::UINT32)
      caller_builder.ret(call)

      gen = Adamas::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      func_ir = output[/define i32 @call_f64_to_u32_arg\([^)]*\)\s*\{.*?\n\}/m]
      func_ir.should_not be_nil
      body = func_ir.not_nil!

      body.should contain("fptoui double %x to i32")
      body.should_not contain("fptosi double %x to i32")
    end

    it "uses ptrtoint + uitofp for pointer argument when calling float64 callee" do
      mod = Adamas::MIR::Module.new("test")

      callee = mod.create_function("takes_f64_ptr_arg", Adamas::MIR::TypeRef::FLOAT64)
      callee.add_param("x", Adamas::MIR::TypeRef::FLOAT64)
      callee_builder = Adamas::MIR::Builder.new(callee)
      callee_builder.ret(0_u32)

      caller = mod.create_function("call_ptr_to_f64_arg", Adamas::MIR::TypeRef::FLOAT64)
      caller.add_param("p", Adamas::MIR::TypeRef::POINTER)
      caller_builder = Adamas::MIR::Builder.new(caller)
      call = caller_builder.call(callee.id, ([0_u32] of Adamas::MIR::ValueId), Adamas::MIR::TypeRef::FLOAT64)
      caller_builder.ret(call)

      gen = Adamas::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      func_ir = output[/define double @call_ptr_to_f64_arg\([^)]*\)\s*\{.*?\n\}/m]
      func_ir.should_not be_nil
      body = func_ir.not_nil!

      body.should contain("ptrtoint ptr %p to i64")
      body.should match(/uitofp i64 %ptrtofp\.\d+\.int to double/)
      body.should_not contain("load double, ptr %p")
    end

    it "evaluates abstract Int#remainder(Int32) directly for scalar receivers" do
      mod = Adamas::MIR::Module.new("test")

      check = mod.create_function("Int#check_div_argument$Int32", Adamas::MIR::TypeRef::INT32)
      check.add_param("self", Adamas::MIR::TypeRef::POINTER)
      check.add_param("other", Adamas::MIR::TypeRef::INT32)
      check_builder = Adamas::MIR::Builder.new(check)
      zero = check_builder.const_int(0, Adamas::MIR::TypeRef::INT32)
      check_builder.ret(zero)

      callee = mod.create_function("Int#remainder$Int32", Adamas::MIR::TypeRef::INT32)
      callee.add_param("self", Adamas::MIR::TypeRef::POINTER)
      callee.add_param("other", Adamas::MIR::TypeRef::INT32)
      callee_builder = Adamas::MIR::Builder.new(callee)
      callee_builder.ret(1_u32)

      caller = mod.create_function("call_int_remainder", Adamas::MIR::TypeRef::INT32)
      caller.add_param("self", Adamas::MIR::TypeRef::INT32)
      caller.add_param("other", Adamas::MIR::TypeRef::INT32)
      caller_builder = Adamas::MIR::Builder.new(caller)
      call = caller_builder.call(callee.id, [0_u32, 1_u32], Adamas::MIR::TypeRef::INT32)
      caller_builder.ret(call)

      gen = Adamas::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      func_ir = output[/define i32 @call_int_remainder\([^)]*\)\s*\{.*?\n\}/m]
      func_ir.should_not be_nil
      body = func_ir.not_nil!

      body.should contain("alloca i32")
      body.should contain("store i32 %self")
      body.should contain("call i32 @Int$Hcheck_div_argument$$Int32")
      body.should contain("srem i32 %self, %other")
      body.should_not contain("call i32 @Int$Hremainder$$Int32")
      body.should_not contain("inttoptr i64 %self")
    end

    it "evaluates bare Tuple#size directly for concrete tuple receivers" do
      mod = Adamas::MIR::Module.new("test")

      tuple_type = mod.type_registry.create_type(
        Adamas::MIR::TypeKind::Tuple,
        "Tuple(Float64)",
        8_u64,
        8_u32
      )
      float64_type = mod.type_registry.get(Adamas::MIR::TypeRef::FLOAT64)
      float64_type.should_not be_nil
      tuple_type.add_element_type(float64_type.not_nil!)
      tuple_ref = Adamas::MIR::TypeRef.new(tuple_type.id)

      callee = mod.create_function("Tuple#size", Adamas::MIR::TypeRef::INT32)
      callee.add_param("self", tuple_ref)
      callee_builder = Adamas::MIR::Builder.new(callee)
      zero = callee_builder.const_int(0, Adamas::MIR::TypeRef::INT32)
      callee_builder.ret(zero)

      caller = mod.create_function("call_tuple_size", Adamas::MIR::TypeRef::INT32)
      caller.add_param("self", tuple_ref)
      caller_builder = Adamas::MIR::Builder.new(caller)
      call = caller_builder.call(callee.id, [0_u32], Adamas::MIR::TypeRef::INT32)
      caller_builder.ret(call)

      gen = Adamas::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      func_ir = output[/define i32 @call_tuple_size\([^)]*\)\s*\{.*?\n\}/m]
      func_ir.should_not be_nil
      body = func_ir.not_nil!

      body.should contain("add i32 0, 1")
      body.should_not contain("call i32 @Tuple$Hsize")
    end

    it "evaluates abstract Int#tdiv(Int32) directly for scalar receivers" do
      mod = Adamas::MIR::Module.new("test")

      check = mod.create_function("Int#check_div_argument$Int32", Adamas::MIR::TypeRef::INT32)
      check.add_param("self", Adamas::MIR::TypeRef::POINTER)
      check.add_param("other", Adamas::MIR::TypeRef::INT32)
      check_builder = Adamas::MIR::Builder.new(check)
      zero = check_builder.const_int(0, Adamas::MIR::TypeRef::INT32)
      check_builder.ret(zero)

      callee = mod.create_function("Int#tdiv$Int32", Adamas::MIR::TypeRef::POINTER)
      callee.add_param("self", Adamas::MIR::TypeRef::POINTER)
      callee.add_param("other", Adamas::MIR::TypeRef::INT32)
      callee_builder = Adamas::MIR::Builder.new(callee)
      callee_builder.ret(0_u32)

      caller = mod.create_function("call_int_tdiv", Adamas::MIR::TypeRef::POINTER)
      caller.add_param("self", Adamas::MIR::TypeRef::INT32)
      caller.add_param("other", Adamas::MIR::TypeRef::INT32)
      caller_builder = Adamas::MIR::Builder.new(caller)
      call = caller_builder.call(callee.id, [0_u32, 1_u32], Adamas::MIR::TypeRef::POINTER)
      caller_builder.ret(call)

      gen = Adamas::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      func_ir = output[/define ptr @call_int_tdiv\([^)]*\)\s*\{.*?\n\}/m]
      func_ir.should_not be_nil
      body = func_ir.not_nil!

      body.should contain("alloca i32")
      body.should contain("store i32 %self")
      body.should contain("call i32 @Int$Hcheck_div_argument$$Int32")
      body.should contain("sdiv i32 %self, %other")
      body.should contain("%int_div_ret.")
      body.should contain("store i32 %int_div_res.")
      body.should_not contain("call ptr @Int$Htdiv$$Int32")
      body.should_not contain("inttoptr i64 %self")
    end

    it "dispatches abstract Int#to_s on scalar receivers to concrete integer implementations" do
      mod = Adamas::MIR::Module.new("test")

      concrete = mod.create_function("Int32#to_s", Adamas::MIR::TypeRef::POINTER)
      concrete.add_param("self", Adamas::MIR::TypeRef::INT32)
      concrete.add_param("base", Adamas::MIR::TypeRef::INT32)
      concrete.add_param("precision", Adamas::MIR::TypeRef::INT32)
      concrete.add_param("upcase", Adamas::MIR::TypeRef::BOOL)
      concrete_builder = Adamas::MIR::Builder.new(concrete)
      concrete_builder.ret(0_u32)

      callee = mod.create_function("Int#to_s$Int32_Int32_Bool", Adamas::MIR::TypeRef::POINTER)
      callee.add_param("self", Adamas::MIR::TypeRef::POINTER)
      callee.add_param("base", Adamas::MIR::TypeRef::INT32)
      callee.add_param("precision", Adamas::MIR::TypeRef::INT32)
      callee.add_param("upcase", Adamas::MIR::TypeRef::BOOL)
      callee_builder = Adamas::MIR::Builder.new(callee)
      callee_builder.ret(0_u32)

      caller = mod.create_function("call_abstract_int_to_s", Adamas::MIR::TypeRef::POINTER)
      caller.add_param("self", Adamas::MIR::TypeRef::INT32)
      caller.add_param("base", Adamas::MIR::TypeRef::INT32)
      caller.add_param("precision", Adamas::MIR::TypeRef::INT32)
      caller.add_param("upcase", Adamas::MIR::TypeRef::BOOL)
      caller_builder = Adamas::MIR::Builder.new(caller)
      call = caller_builder.call(callee.id, [0_u32, 1_u32, 2_u32, 3_u32], Adamas::MIR::TypeRef::POINTER)
      caller_builder.ret(call)

      gen = Adamas::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      func_ir = output[/define ptr @call_abstract_int_to_s\([^)]*\)\s*\{.*?\n\}/m]
      func_ir.should_not be_nil
      body = func_ir.not_nil!

      body.should contain("call ptr @Int32$Hto_s(i32 %self, i32 %base, i32 %precision, i1 %upcase)")
      body.should_not contain("call ptr @Int$Hto_s$$Int32_Int32_Bool")
      body.should_not contain("inttoptr i64 %self")
      body.should_not contain("inttoptr i32 %self")
    end

    it "lowers abstract Int#to_s(IO, ...) on scalar receivers via concrete to_s plus IO << String" do
      mod = Adamas::MIR::Module.new("test")

      concrete = mod.create_function("Int32#to_s", Adamas::MIR::TypeRef::POINTER)
      concrete.add_param("self", Adamas::MIR::TypeRef::INT32)
      concrete.add_param("base", Adamas::MIR::TypeRef::INT32)
      concrete.add_param("precision", Adamas::MIR::TypeRef::INT32)
      concrete.add_param("upcase", Adamas::MIR::TypeRef::BOOL)
      concrete_builder = Adamas::MIR::Builder.new(concrete)
      concrete_builder.ret(0_u32)

      io_append = mod.create_function("IO#<<$String", Adamas::MIR::TypeRef::POINTER)
      io_append.add_param("io", Adamas::MIR::TypeRef::POINTER)
      io_append.add_param("str", Adamas::MIR::TypeRef::POINTER)
      io_builder = Adamas::MIR::Builder.new(io_append)
      io_builder.ret(0_u32)

      callee = mod.create_function("Int#to_s$IO_Int32_Int32_Bool", Adamas::MIR::TypeRef::VOID)
      callee.add_param("self", Adamas::MIR::TypeRef::POINTER)
      callee.add_param("io", Adamas::MIR::TypeRef::POINTER)
      callee.add_param("base", Adamas::MIR::TypeRef::INT32)
      callee.add_param("precision", Adamas::MIR::TypeRef::INT32)
      callee.add_param("upcase", Adamas::MIR::TypeRef::BOOL)
      callee_builder = Adamas::MIR::Builder.new(callee)
      callee_builder.ret

      caller = mod.create_function("call_abstract_int_to_s_io", Adamas::MIR::TypeRef::VOID)
      caller.add_param("self", Adamas::MIR::TypeRef::INT32)
      caller.add_param("io", Adamas::MIR::TypeRef::POINTER)
      caller.add_param("base", Adamas::MIR::TypeRef::INT32)
      caller.add_param("precision", Adamas::MIR::TypeRef::INT32)
      caller.add_param("upcase", Adamas::MIR::TypeRef::BOOL)
      caller_builder = Adamas::MIR::Builder.new(caller)
      caller_builder.call(callee.id, [0_u32, 1_u32, 2_u32, 3_u32, 4_u32], Adamas::MIR::TypeRef::VOID)
      caller_builder.ret

      gen = Adamas::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      func_ir = output[/define void @call_abstract_int_to_s_io\([^)]*\)\s*\{.*?\n\}/m]
      func_ir.should_not be_nil
      body = func_ir.not_nil!

      body.should contain("call ptr @Int32$Hto_s(i32 %self, i32 %base, i32 %precision, i1 %upcase)")
      body.should contain("call ptr @IO$H$SHL$$String(ptr %io, ptr %int_to_s_io.")
      body.should_not contain("call void @Int$Hto_s$$IO_Int32_Int32_Bool")
      body.should_not contain("inttoptr i64 %self")
      body.should_not contain("inttoptr i32 %self")
    end

    it "uses uitofp for uint128 argument when calling float64 callee" do
      mod = Adamas::MIR::Module.new("test")

      callee = mod.create_function("takes_f64_arg_u128", Adamas::MIR::TypeRef::FLOAT64)
      callee.add_param("x", Adamas::MIR::TypeRef::FLOAT64)
      callee_builder = Adamas::MIR::Builder.new(callee)
      callee_builder.ret(0_u32)

      caller = mod.create_function("call_uint128_to_f64_arg", Adamas::MIR::TypeRef::FLOAT64)
      caller.add_param("x", Adamas::MIR::TypeRef::UINT128)
      caller_builder = Adamas::MIR::Builder.new(caller)
      call = caller_builder.call(callee.id, ([0_u32] of Adamas::MIR::ValueId), Adamas::MIR::TypeRef::FLOAT64)
      caller_builder.ret(call)

      gen = Adamas::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      func_ir = output[/define double @call_uint128_to_f64_arg\([^)]*\)\s*\{.*?\n\}/m]
      func_ir.should_not be_nil
      body = func_ir.not_nil!

      body.should contain("uitofp i128 %x to double")
      body.should_not contain("sitofp i128 %x to double")
    end

    it "uses align 4 for union payload load in union-to-float call coercion" do
      mod = Adamas::MIR::Module.new("test")

      union_type = mod.type_registry.create_type(Adamas::MIR::TypeKind::Union, "FloatOrNilArg", 16, 8)
      union_ref = Adamas::MIR::TypeRef.new(union_type.id)
      mod.register_union(
        union_ref,
        Adamas::MIR::UnionDescriptor.new(
          "FloatOrNilArg",
          [
            Adamas::MIR::UnionVariantDescriptor.new(
              type_id: 0,
              type_ref: Adamas::MIR::TypeRef::FLOAT64,
              full_name: "Float64",
              size: 8,
              alignment: 8,
              field_offsets: nil
            ),
            Adamas::MIR::UnionVariantDescriptor.new(
              type_id: 1,
              type_ref: Adamas::MIR::TypeRef::NIL,
              full_name: "Nil",
              size: 0,
              alignment: 1,
              field_offsets: nil
            ),
          ],
          16,
          8
        )
      )

      callee = mod.create_function("takes_union_coerced_f64", Adamas::MIR::TypeRef::FLOAT64)
      callee.add_param("x", Adamas::MIR::TypeRef::FLOAT64)
      callee_builder = Adamas::MIR::Builder.new(callee)
      callee_builder.ret(0_u32)

      caller = mod.create_function("call_union_to_f64_arg", Adamas::MIR::TypeRef::FLOAT64)
      caller.add_param("u", union_ref)
      caller_builder = Adamas::MIR::Builder.new(caller)
      call = caller_builder.call(callee.id, ([0_u32] of Adamas::MIR::ValueId), Adamas::MIR::TypeRef::FLOAT64)
      caller_builder.ret(call)

      gen = Adamas::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      func_ir = output[/define double @call_union_to_f64_arg\([^)]*\)\s*\{.*?\n\}/m]
      func_ir.should_not be_nil
      body = func_ir.not_nil!

      body.should match(/%union_to_fp\.\d+\.val = load double, ptr %union_to_fp\.\d+\.payload_ptr, align 4/)
    end

    it "emits ptr phi when a union phi has ptr incoming" do
      mod = Adamas::MIR::Module.new("phi_union_ptr")

      union_type = mod.type_registry.create_type(Adamas::MIR::TypeKind::Union, "PtrOrNil", 16, 8)
      union_ref = Adamas::MIR::TypeRef.new(union_type.id)
      mod.register_union(
        union_ref,
        Adamas::MIR::UnionDescriptor.new(
          "PtrOrNil",
          [
            Adamas::MIR::UnionVariantDescriptor.new(
              type_id: 0,
              type_ref: Adamas::MIR::TypeRef::POINTER,
              full_name: "Pointer",
              size: 8,
              alignment: 8,
              field_offsets: nil
            ),
            Adamas::MIR::UnionVariantDescriptor.new(
              type_id: 1,
              type_ref: Adamas::MIR::TypeRef::NIL,
              full_name: "Nil",
              size: 0,
              alignment: 1,
              field_offsets: nil
            ),
          ],
          16,
          8
        )
      )

      func = mod.create_function("phi_union_ptr", Adamas::MIR::TypeRef::VOID)
      builder = Adamas::MIR::Builder.new(func)

      entry = func.entry_block
      then_block = func.create_block
      else_block = func.create_block
      merge_block = func.create_block

      cond = builder.const_bool(true)
      builder.branch(cond, then_block, else_block)

      builder.current_block = then_block
      ptr_val = builder.alloc(Adamas::MIR::MemoryStrategy::Stack, Adamas::MIR::TypeRef::INT32, 4_u64, 4_u32)
      wrapped_ptr = builder.union_wrap(ptr_val, 0, union_ref)
      builder.jump(merge_block)

      builder.current_block = else_block
      nil_val = builder.const_nil
      wrapped_nil = builder.union_wrap(nil_val, 1, union_ref)
      builder.jump(merge_block)

      builder.current_block = merge_block
      phi = builder.phi(union_ref)
      phi.add_incoming(from: then_block, value: wrapped_ptr)
      phi.add_incoming(from: else_block, value: wrapped_nil)
      builder.ret

      gen = Adamas::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      phi_line = output.lines.find { |line| line.includes?("phi %PtrOrNil.union") }
      phi_line.should_not be_nil
      phi_line.not_nil!.should_not contain("zeroinitializer")
    end

    it "emits align 4 for union payload store/load" do
      mod = Adamas::MIR::Module.new("union_align")

      union_type = mod.type_registry.create_type(Adamas::MIR::TypeKind::Union, "PtrOrNilAlign", 16, 8)
      union_ref = Adamas::MIR::TypeRef.new(union_type.id)
      mod.register_union(
        union_ref,
        Adamas::MIR::UnionDescriptor.new(
          "PtrOrNilAlign",
          [
            Adamas::MIR::UnionVariantDescriptor.new(
              type_id: 0,
              type_ref: Adamas::MIR::TypeRef::POINTER,
              full_name: "Pointer",
              size: 8,
              alignment: 8,
              field_offsets: nil
            ),
            Adamas::MIR::UnionVariantDescriptor.new(
              type_id: 1,
              type_ref: Adamas::MIR::TypeRef::NIL,
              full_name: "Nil",
              size: 0,
              alignment: 1,
              field_offsets: nil
            ),
          ],
          16,
          8
        )
      )

      func = mod.create_function("union_align_payload", Adamas::MIR::TypeRef::POINTER)
      builder = Adamas::MIR::Builder.new(func)
      ptr_val = builder.alloc(Adamas::MIR::MemoryStrategy::Stack, Adamas::MIR::TypeRef::INT32, 4_u64, 4_u32)
      wrapped = builder.union_wrap(ptr_val, 0, union_ref)
      unwrapped = builder.emit(Adamas::MIR::UnionUnwrap.new(func.next_value_id, Adamas::MIR::TypeRef::POINTER, wrapped, 0))
      builder.ret(unwrapped)

      gen = Adamas::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      func_ir = output[/define ptr @union_align_payload\(\)\s*\{.*?\n\}/m]
      func_ir.should_not be_nil
      body = func_ir.not_nil!
      body.should match(/store ptr .*payload_ptr, align 4/)
      body.should match(/load ptr, ptr %.*payload_ptr, align 4/)
    end

    it "emits align 4 for UInt64 union payload store/load" do
      mod = Adamas::MIR::Module.new("union_align_u64")

      union_type = mod.type_registry.create_type(Adamas::MIR::TypeKind::Union, "U64OrNilAlign", 16, 8)
      union_ref = Adamas::MIR::TypeRef.new(union_type.id)
      mod.register_union(
        union_ref,
        Adamas::MIR::UnionDescriptor.new(
          "U64OrNilAlign",
          [
            Adamas::MIR::UnionVariantDescriptor.new(
              type_id: 0,
              type_ref: Adamas::MIR::TypeRef::UINT64,
              full_name: "UInt64",
              size: 8,
              alignment: 8,
              field_offsets: nil
            ),
            Adamas::MIR::UnionVariantDescriptor.new(
              type_id: 1,
              type_ref: Adamas::MIR::TypeRef::NIL,
              full_name: "Nil",
              size: 0,
              alignment: 1,
              field_offsets: nil
            ),
          ],
          16,
          8
        )
      )

      func = mod.create_function("union_align_payload_u64", Adamas::MIR::TypeRef::UINT64)
      func.add_param("x", Adamas::MIR::TypeRef::UINT64)
      builder = Adamas::MIR::Builder.new(func)
      wrapped = builder.union_wrap(0_u32, 0, union_ref)
      unwrapped = builder.emit(Adamas::MIR::UnionUnwrap.new(func.next_value_id, Adamas::MIR::TypeRef::UINT64, wrapped, 0))
      builder.ret(unwrapped)

      gen = Adamas::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      func_ir = output[/define i64 @union_align_payload_u64\(i64 %x\)\s*\{.*?\n\}/m]
      func_ir.should_not be_nil
      body = func_ir.not_nil!
      body.should match(/store i64 %x, ptr %.*payload_ptr, align 4/)
      body.should match(/load i64, ptr %.*payload_ptr, align 4/)
    end

    it "generates RC increment and decrement" do
      mod = Adamas::MIR::Module.new("test")
      func = mod.create_function("rc_test", Adamas::MIR::TypeRef::VOID)
      func.add_param("ptr", Adamas::MIR::TypeRef::POINTER)

      builder = Adamas::MIR::Builder.new(func)
      builder.rc_inc(0_u32)
      builder.rc_dec(0_u32)
      builder.ret

      gen = Adamas::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      # Parameters use their name directly
      output.should contain("call void @__adamas_rc_inc(ptr %ptr)")
      output.should contain("call void @__adamas_rc_dec(ptr %ptr, ptr null)")
    end

    it "generates atomic RC increment with atomicrmw" do
      mod = Adamas::MIR::Module.new("test")
      func = mod.create_function("atomic_rc_inc", Adamas::MIR::TypeRef::VOID)
      func.add_param("ptr", Adamas::MIR::TypeRef::POINTER)

      builder = Adamas::MIR::Builder.new(func)
      builder.rc_inc(0_u32, atomic: true)
      builder.ret

      gen = Adamas::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      # Should use inline atomicrmw instead of function call
      output.should contain("getelementptr i8, ptr %ptr, i64 -8")
      output.should contain("atomicrmw add")
      output.should contain("seq_cst")
      # Should NOT call external function
      output.should_not contain("call void @__adamas_rc_inc_atomic")
    end

    it "generates atomic RC decrement with conditional free" do
      mod = Adamas::MIR::Module.new("test")
      func = mod.create_function("atomic_rc_dec", Adamas::MIR::TypeRef::VOID)
      func.add_param("ptr", Adamas::MIR::TypeRef::POINTER)

      builder = Adamas::MIR::Builder.new(func)
      builder.rc_dec(0_u32, atomic: true)
      builder.ret

      gen = Adamas::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      # Should use inline atomicrmw sub
      output.should contain("atomicrmw sub")
      output.should contain("acq_rel")
      # Should check if RC reached zero
      output.should contain("icmp eq i64")
      # Should have conditional branch to free block
      output.should contain("do_free")
      output.should contain("skip_free")
      output.should contain("call void @free")
    end

    it "generates TSan instrumentation for load/store when enabled" do
      mod = Adamas::MIR::Module.new("test")
      func = mod.create_function("tsan_test", Adamas::MIR::TypeRef::VOID)
      func.add_param("ptr", Adamas::MIR::TypeRef::POINTER)

      builder = Adamas::MIR::Builder.new(func)
      loaded = builder.load(0_u32, Adamas::MIR::TypeRef::INT32)
      builder.store(0_u32, loaded)
      builder.ret

      gen = Adamas::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      gen.emit_tsan = true
      output = gen.generate

      # Should declare TSan functions
      output.should contain("declare void @__tsan_read4(ptr)")
      output.should contain("declare void @__tsan_write4(ptr)")
      output.should contain("declare void @__tsan_func_entry(ptr)")
      output.should contain("declare void @__tsan_func_exit()")

      # Should instrument load and store
      output.should contain("call void @__tsan_read4(ptr %ptr)")
      output.should contain("call void @__tsan_write4(ptr %ptr)")

      # Should have function entry/exit
      output.should contain("call void @__tsan_func_entry")
      output.should contain("call void @__tsan_func_exit()")
    end

    it "does not generate TSan instrumentation when disabled" do
      mod = Adamas::MIR::Module.new("test")
      func = mod.create_function("no_tsan", Adamas::MIR::TypeRef::VOID)
      func.add_param("ptr", Adamas::MIR::TypeRef::POINTER)

      builder = Adamas::MIR::Builder.new(func)
      loaded = builder.load(0_u32, Adamas::MIR::TypeRef::INT32)
      builder.store(0_u32, loaded)
      builder.ret

      gen = Adamas::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      gen.emit_tsan = false
      output = gen.generate

      # Should NOT have TSan instrumentation
      output.should_not contain("@__tsan_read")
      output.should_not contain("@__tsan_write")
      output.should_not contain("@__tsan_func_entry")
    end

    it "generates TSan acquire/release for atomic RC" do
      mod = Adamas::MIR::Module.new("test")
      func = mod.create_function("tsan_atomic_rc", Adamas::MIR::TypeRef::VOID)
      func.add_param("ptr", Adamas::MIR::TypeRef::POINTER)

      builder = Adamas::MIR::Builder.new(func)
      builder.rc_inc(0_u32, atomic: true)
      builder.rc_dec(0_u32, atomic: true)
      builder.ret

      gen = Adamas::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      gen.emit_tsan = true
      output = gen.generate

      # Should have acquire/release annotations for TSan
      output.should contain("call void @__tsan_release(ptr %ptr)")
      output.should contain("call void @__tsan_acquire(ptr %ptr)")
    end

    it "generates binary operations" do
      mod = Adamas::MIR::Module.new("test")
      func = mod.create_function("binop_test", Adamas::MIR::TypeRef::INT32)
      func.add_param("a", Adamas::MIR::TypeRef::INT32)
      func.add_param("b", Adamas::MIR::TypeRef::INT32)

      builder = Adamas::MIR::Builder.new(func)
      sum = builder.add(0_u32, 1_u32, Adamas::MIR::TypeRef::INT32)
      diff = builder.sub(0_u32, 1_u32, Adamas::MIR::TypeRef::INT32)
      prod = builder.mul(0_u32, 1_u32, Adamas::MIR::TypeRef::INT32)
      builder.ret(prod)

      gen = Adamas::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      output.should contain("add i32")
      output.should contain("sub i32")
      output.should contain("mul i32")
    end

    it "generates conditional branch" do
      mod = Adamas::MIR::Module.new("test")
      func = mod.create_function("branch_test", Adamas::MIR::TypeRef::INT32)
      func.add_param("cond", Adamas::MIR::TypeRef::BOOL)
      func.add_param("a", Adamas::MIR::TypeRef::INT32)
      func.add_param("b", Adamas::MIR::TypeRef::INT32)

      then_block = func.create_block
      else_block = func.create_block
      exit_block = func.create_block

      builder = Adamas::MIR::Builder.new(func)
      builder.branch(0_u32, then_block, else_block)

      builder.current_block = then_block
      builder.jump(exit_block)

      builder.current_block = else_block
      builder.jump(exit_block)

      builder.current_block = exit_block
      builder.ret(1_u32)

      gen = Adamas::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      output.should contain("br i1")
      output.should contain("br label")
    end
  end

  describe "type metadata generation" do
    it "generates __crystal_type_count global" do
      mod = Adamas::MIR::Module.new("test")

      gen = Adamas::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = true
      output = gen.generate

      output.should contain("@__crystal_type_count = constant i32")
    end

    it "generates __crystal_type_info array" do
      mod = Adamas::MIR::Module.new("test")

      gen = Adamas::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = true
      output = gen.generate

      output.should contain("@__crystal_type_info = constant")
      output.should contain("%__crystal_type_info_entry = type { i32, i32, i32, i32, i32, i32, i32, i32 }")
    end

    it "generates __crystal_type_strings table" do
      mod = Adamas::MIR::Module.new("test")

      gen = Adamas::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = true
      output = gen.generate

      output.should contain("@__crystal_type_strings = constant")
    end

    it "includes primitive types in metadata" do
      mod = Adamas::MIR::Module.new("test")

      gen = Adamas::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = true
      output = gen.generate

      # Should have entries for all primitive types
      # TypeRegistry registers 19 primitive types (including Pointer)
      output.should contain("@__crystal_type_count = constant i32 19")
    end

    it "generates field info for struct types" do
      mod = Adamas::MIR::Module.new("test")

      # Create a struct type with fields
      struct_type = mod.type_registry.create_type(
        Adamas::MIR::TypeKind::Struct,
        "Point",
        8_u64,
        4_u32
      )
      struct_type.add_field("x", Adamas::MIR::TypeRef::INT32, 0_u32)
      struct_type.add_field("y", Adamas::MIR::TypeRef::INT32, 4_u32)

      gen = Adamas::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = true
      output = gen.generate

      output.should contain("@__crystal_field_info = constant")
      output.should contain("%__crystal_field_info_entry = type { i32, i32, i32, i32 }")
    end

    it "emits large homogeneous tuples as DW_TAG_array_type with a DISubrange" do
      mod = Adamas::MIR::Module.new("test")
      tuple_type = mod.type_registry.create_type(
        Adamas::MIR::TypeKind::Tuple,
        "HugeUInt64Tuple",
        1301_u64 * 8_u64,
        8_u32
      )
      uint64_type = mod.type_registry.get(Adamas::MIR::TypeRef::UINT64)
      uint64_type.should_not be_nil
      1301.times do
        tuple_type.add_element_type(uint64_type.not_nil!)
      end

      gen = Adamas::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      gen.emit_debug_info = true
      output = gen.generate

      output.should contain("!DICompositeType(tag: DW_TAG_array_type, baseType: !1300000011")
      output.should contain("!DISubrange(count: 1301)")
      output.should_not contain("name: \"[1300]\"")
    end

    it "deduplicates colliding lexical block metadata ids" do
      mod = Adamas::MIR::Module.new("test")
      ctx = Adamas::MIR::DwarfDebugContext.new(mod, Adamas::MIR::LLVMTypeMapper.new(mod.type_registry))
      assigned = {} of Int32 => String
      first_key = "lexblock:300001524:3"
      second_key = "lexblock:300002527:9"

      raw_first = ctx.__test_stable_metadata_id(
        first_key,
        Adamas::MIR::DwarfDebugContext::LEXICAL_BLOCK_ID_BASE,
        Adamas::MIR::DwarfDebugContext::LEXICAL_BLOCK_ID_SPAN
      )
      raw_second = ctx.__test_stable_metadata_id(
        second_key,
        Adamas::MIR::DwarfDebugContext::LEXICAL_BLOCK_ID_BASE,
        Adamas::MIR::DwarfDebugContext::LEXICAL_BLOCK_ID_SPAN
      )
      raw_first.should eq(raw_second)

      unique_first = ctx.__test_unique_stable_metadata_id(
        assigned,
        first_key,
        Adamas::MIR::DwarfDebugContext::LEXICAL_BLOCK_ID_BASE,
        Adamas::MIR::DwarfDebugContext::LEXICAL_BLOCK_ID_SPAN
      )
      unique_second = ctx.__test_unique_stable_metadata_id(
        assigned,
        second_key,
        Adamas::MIR::DwarfDebugContext::LEXICAL_BLOCK_ID_BASE,
        Adamas::MIR::DwarfDebugContext::LEXICAL_BLOCK_ID_SPAN
      )

      unique_first.should_not eq(unique_second)
      assigned.size.should eq(2)
    end
  end

  describe "synchronization primitives" do
    it "generates atomic load with memory ordering" do
      mod = Adamas::MIR::Module.new("test")
      func = mod.create_function("atomic_load_test", Adamas::MIR::TypeRef::INT64)
      func.add_param("ptr", Adamas::MIR::TypeRef::POINTER)

      builder = Adamas::MIR::Builder.new(func)
      result = builder.atomic_load(0_u32, Adamas::MIR::TypeRef::INT64, Adamas::MIR::MemoryOrdering::Acquire)
      builder.ret(result)

      gen = Adamas::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      output.should contain("load atomic i64")
      output.should contain("acquire")
    end

    it "generates atomic store with memory ordering" do
      mod = Adamas::MIR::Module.new("test")
      func = mod.create_function("atomic_store_test", Adamas::MIR::TypeRef::VOID)
      func.add_param("ptr", Adamas::MIR::TypeRef::POINTER)
      func.add_param("val", Adamas::MIR::TypeRef::INT64)

      builder = Adamas::MIR::Builder.new(func)
      builder.atomic_store(0_u32, 1_u32, Adamas::MIR::MemoryOrdering::Release)
      builder.ret

      gen = Adamas::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      output.should contain("store atomic")
      output.should contain("release")
    end

    it "generates atomic compare-and-swap" do
      mod = Adamas::MIR::Module.new("test")
      func = mod.create_function("atomic_cas_test", Adamas::MIR::TypeRef::INT64)
      func.add_param("ptr", Adamas::MIR::TypeRef::POINTER)
      func.add_param("expected", Adamas::MIR::TypeRef::INT64)
      func.add_param("desired", Adamas::MIR::TypeRef::INT64)

      builder = Adamas::MIR::Builder.new(func)
      result = builder.atomic_cas(0_u32, 1_u32, 2_u32, Adamas::MIR::TypeRef::INT64)
      builder.ret(result)

      gen = Adamas::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      output.should contain("cmpxchg")
      output.should contain("seq_cst")
      output.should contain("extractvalue")
    end

    it "generates atomic read-modify-write operations" do
      mod = Adamas::MIR::Module.new("test")
      func = mod.create_function("atomic_rmw_test", Adamas::MIR::TypeRef::INT64)
      func.add_param("ptr", Adamas::MIR::TypeRef::POINTER)
      func.add_param("val", Adamas::MIR::TypeRef::INT64)

      builder = Adamas::MIR::Builder.new(func)
      result = builder.atomic_rmw(Adamas::MIR::AtomicRMWOp::Add, 0_u32, 1_u32, Adamas::MIR::TypeRef::INT64)
      builder.ret(result)

      gen = Adamas::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      output.should contain("atomicrmw add")
      output.should contain("seq_cst")
    end

    it "generates memory fence" do
      mod = Adamas::MIR::Module.new("test")
      func = mod.create_function("fence_test", Adamas::MIR::TypeRef::VOID)

      builder = Adamas::MIR::Builder.new(func)
      builder.fence(Adamas::MIR::MemoryOrdering::AcqRel)
      builder.ret

      gen = Adamas::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      output.should contain("fence acq_rel")
    end

    it "generates mutex lock/unlock calls" do
      mod = Adamas::MIR::Module.new("test")
      func = mod.create_function("mutex_test", Adamas::MIR::TypeRef::VOID)
      func.add_param("mutex", Adamas::MIR::TypeRef::POINTER)

      builder = Adamas::MIR::Builder.new(func)
      builder.mutex_lock(0_u32)
      builder.mutex_unlock(0_u32)
      builder.ret

      gen = Adamas::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      output.should contain("call void @__adamas_mutex_lock")
      output.should contain("call void @__adamas_mutex_unlock")
    end

    it "generates mutex trylock call" do
      mod = Adamas::MIR::Module.new("test")
      func = mod.create_function("trylock_test", Adamas::MIR::TypeRef::BOOL)
      func.add_param("mutex", Adamas::MIR::TypeRef::POINTER)

      builder = Adamas::MIR::Builder.new(func)
      result = builder.mutex_trylock(0_u32)
      builder.ret(result)

      gen = Adamas::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      output.should contain("call i1 @__adamas_mutex_trylock")
    end

    it "generates channel send/receive/close calls" do
      mod = Adamas::MIR::Module.new("test")
      func = mod.create_function("channel_test", Adamas::MIR::TypeRef::POINTER)
      func.add_param("channel", Adamas::MIR::TypeRef::POINTER)
      func.add_param("data", Adamas::MIR::TypeRef::POINTER)

      builder = Adamas::MIR::Builder.new(func)
      builder.channel_send(0_u32, 1_u32)
      result = builder.channel_receive(0_u32, Adamas::MIR::TypeRef::POINTER)
      builder.channel_close(0_u32)
      builder.ret(result)

      gen = Adamas::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      output.should contain("call void @__adamas_channel_send")
      output.should contain("call ptr @__adamas_channel_receive")
      output.should contain("call void @__adamas_channel_close")
    end

    it "generates TSan annotations for mutex operations" do
      mod = Adamas::MIR::Module.new("test")
      func = mod.create_function("mutex_tsan_test", Adamas::MIR::TypeRef::VOID)
      func.add_param("mutex", Adamas::MIR::TypeRef::POINTER)

      builder = Adamas::MIR::Builder.new(func)
      builder.mutex_lock(0_u32)
      builder.mutex_unlock(0_u32)
      builder.ret

      gen = Adamas::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      gen.emit_tsan = true
      output = gen.generate

      output.should contain("@__tsan_acquire")
      output.should contain("@__tsan_release")
    end

    it "generates TSan annotations for channel operations" do
      mod = Adamas::MIR::Module.new("test")
      func = mod.create_function("channel_tsan_test", Adamas::MIR::TypeRef::POINTER)
      func.add_param("channel", Adamas::MIR::TypeRef::POINTER)
      func.add_param("data", Adamas::MIR::TypeRef::POINTER)

      builder = Adamas::MIR::Builder.new(func)
      builder.channel_send(0_u32, 1_u32)
      result = builder.channel_receive(0_u32, Adamas::MIR::TypeRef::POINTER)
      builder.ret(result)

      gen = Adamas::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      gen.emit_tsan = true
      output = gen.generate

      # Send releases, receive acquires
      output.should contain("@__tsan_release")
      output.should contain("@__tsan_acquire")
    end

    it "keeps widened primitive override results consistent with the declared function return type" do
      mod = Adamas::MIR::Module.new("test")
      func = mod.create_function("UInt32#+$UInt64", Adamas::MIR::TypeRef::UINT32)
      func.add_param("self", Adamas::MIR::TypeRef::UINT32)
      func.add_param("other", Adamas::MIR::TypeRef::UINT64)

      gen = Adamas::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      output.should contain("define i32 @UInt32$H$ADD$$UInt64(i32 %self, i64 %other)")
      output.should contain("; UInt32#ADD primitive override")
      output.should contain("%result = add i64")
      output.should contain("%result_ret = trunc i64 %result to i32")
      output.should contain("ret i32 %result_ret")
      output.should_not contain("ret i64 %result")
    end

    it "inlines zeroed Crystal::Hasher allocation in key_hash overrides" do
      mod = Adamas::MIR::Module.new("test")
      mod.type_registry.create_type(
        Adamas::MIR::TypeKind::Struct,
        "Crystal::Hasher",
        16_u64,
        8_u32
      )

      func = mod.create_function("Hash(Int32, Int32)#key_hash$Int32", Adamas::MIR::TypeRef::INT32)
      func.add_param("self", Adamas::MIR::TypeRef::POINTER)
      func.add_param("key", Adamas::MIR::TypeRef::INT32)

      builder = Adamas::MIR::Builder.new(func)
      zero = builder.const_int(0_i64, Adamas::MIR::TypeRef::INT32)
      builder.ret(zero)

      gen = Adamas::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      output.should contain("define i32 @Hash$LInt32$C$_Int32$R$Hkey_hash$$Int32")
      output.should contain("call ptr @__adamas_malloc64(i64 24)")
      output.should contain("call void @llvm.memset.p0.i64(ptr %hasher, i8 0, i64 16, i1 false)")
      output.should_not contain("call ptr @Crystal$CCHasher$Dnew(i64 0, i64 0)")
      output.should_not contain("call i64 @Crystal$CCHasher$Hresult(ptr %hasher2)")
    end

    it "delegates tuple key_hash overrides to generic Tuple#hash with a live hasher" do
      mod = Adamas::MIR::Module.new("test")
      mod.type_registry.create_type(
        Adamas::MIR::TypeKind::Struct,
        "Crystal::Hasher",
        16_u64,
        8_u32
      )

      tuple_type = mod.type_registry.create_type(
        Adamas::MIR::TypeKind::Tuple,
        "Tuple(Float64)",
        8_u64,
        8_u32
      )
      float64_type = mod.type_registry.get(Adamas::MIR::TypeRef::FLOAT64)
      float64_type.should_not be_nil
      tuple_type.add_element_type(float64_type.not_nil!)
      tuple_ref = Adamas::MIR::TypeRef.new(tuple_type.id)

      tuple_hash = mod.create_function("Tuple#hash", Adamas::MIR::TypeRef::POINTER)
      tuple_hash.add_param("self", tuple_ref)
      tuple_hash.add_param("hasher", Adamas::MIR::TypeRef::POINTER)
      tuple_hash_builder = Adamas::MIR::Builder.new(tuple_hash)
      tuple_hash_builder.ret(1_u32)

      func = mod.create_function("Hash(Tuple(Float64), Nil)#key_hash$Tuple(Float64)", Adamas::MIR::TypeRef::INT32)
      func.add_param("self", Adamas::MIR::TypeRef::POINTER)
      func.add_param("key", tuple_ref)

      builder = Adamas::MIR::Builder.new(func)
      zero = builder.const_int(0_i64, Adamas::MIR::TypeRef::INT32)
      builder.ret(zero)

      gen = Adamas::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      output.should contain("define i32 @Hash$LTuple$LFloat64$R$C$_Nil$R$Hkey_hash$$Tuple$LFloat64$R")
      output.should contain("call ptr @Tuple$Hhash(ptr %key, ptr %hasher)")
      output.should_not contain("call ptr @Tuple$Hhash(ptr %key, ptr null)")
    end
  end
end

describe Adamas::MIR::TypeRegistry do
  it "pre-registers primitive types" do
    registry = Adamas::MIR::TypeRegistry.new

    int32 = registry.get(Adamas::MIR::TypeRef::INT32)
    int32.should_not be_nil
    int32.not_nil!.name.should eq("Int32")
    int32.not_nil!.kind.should eq(Adamas::MIR::TypeKind::Int32)
    int32.not_nil!.size.should eq(4)
  end

  it "creates custom types" do
    registry = Adamas::MIR::TypeRegistry.new

    point = registry.create_type(Adamas::MIR::TypeKind::Struct, "Point", 8_u64, 4_u32)
    point.id.should be >= 100  # Custom types start at ID 100
    point.name.should eq("Point")
    point.kind.should eq(Adamas::MIR::TypeKind::Struct)
  end

  it "looks up types by name" do
    registry = Adamas::MIR::TypeRegistry.new

    int32 = registry.get_by_name("Int32")
    int32.should_not be_nil
    int32.not_nil!.id.should eq(Adamas::MIR::TypeRef::INT32.id)
  end
end

describe Adamas::MIR::Type do
  it "supports adding fields" do
    type = Adamas::MIR::Type.new(100_u32, Adamas::MIR::TypeKind::Struct, "Point", 8_u64, 4_u32)

    type.add_field("x", Adamas::MIR::TypeRef::INT32, 0_u32)
    type.add_field("y", Adamas::MIR::TypeRef::INT32, 4_u32)

    type.fields.should_not be_nil
    type.fields.not_nil!.size.should eq(2)
    type.fields.not_nil![0].name.should eq("x")
    type.fields.not_nil![1].name.should eq("y")
  end

  it "detects value types" do
    struct_type = Adamas::MIR::Type.new(100_u32, Adamas::MIR::TypeKind::Struct, "Point", 8_u64, 4_u32)
    class_type = Adamas::MIR::Type.new(101_u32, Adamas::MIR::TypeKind::Reference, "Object", 8_u64, 8_u32)

    struct_type.is_value_type?.should be_true
    class_type.is_value_type?.should be_false
  end
end
