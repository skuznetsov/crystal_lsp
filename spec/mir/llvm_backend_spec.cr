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

    it "maps struct types to ptr (structs passed by pointer in ABI)" do
      registry = Crystal::MIR::TypeRegistry.new
      struct_type = registry.create_type(Crystal::MIR::TypeKind::Struct, "MyStruct", 16, 8)
      mapper = Crystal::MIR::LLVMTypeMapper.new(registry)

      # Structs are passed by pointer in our ABI for consistency
      mapper.llvm_type(struct_type).should eq("ptr")
    end

    it "maps struct types to named types for alloca" do
      registry = Crystal::MIR::TypeRegistry.new
      struct_type = registry.create_type(Crystal::MIR::TypeKind::Struct, "MyStruct", 16, 8)
      mapper = Crystal::MIR::LLVMTypeMapper.new(registry)

      # For alloca, we need the actual struct type (via TypeRef)
      struct_type_ref = Crystal::MIR::TypeRef.new(struct_type.id)
      mapper.llvm_alloca_type(struct_type_ref).should eq("%MyStruct")
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

      mapper.mangle_name("Array(Int32)").should eq("Array$LInt32$R")
      mapper.mangle_name("Hash(String, Int32)").should eq("Hash$LString$C$_Int32$R")
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

    it "generates runtime definitions" do
      mod = Crystal::MIR::Module.new("test")
      gen = Crystal::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      # Runtime functions are now defined with implementations
      output.should contain("define ptr @__crystal_v2_malloc64(i64 %size)")
      output.should contain("define void @__crystal_v2_rc_inc(ptr %ptr)")
      output.should contain("define void @__crystal_v2_rc_dec(ptr %ptr, ptr %destructor)")
      output.should contain("define ptr @__crystal_v2_slab_alloc(i32 %size_class)")
      output.should contain("shl i64 16, %size")
      output.should contain("define void @__crystal_v2_slab_frame_push()")
      output.should contain("define void @__crystal_v2_slab_frame_pop()")
    end

    it "emits entrypoint when __crystal_main is present" do
      mod = Crystal::MIR::Module.new("test")
      func = mod.create_function("__crystal_main", Crystal::MIR::TypeRef::VOID)
      builder = Crystal::MIR::Builder.new(func)
      builder.ret

      gen = Crystal::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      gen.reachability = true
      output = gen.generate

      output.should contain("define i32 @main")
      output.should contain("call void @__crystal_main")
    end

    it "emits slab frame prolog/epilog when enabled" do
      mod = Crystal::MIR::Module.new("test")
      func = mod.create_function("foo", Crystal::MIR::TypeRef::VOID)
      func.slab_frame = true
      builder = Crystal::MIR::Builder.new(func)
      builder.ret

      gen = Crystal::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      output.should contain("call void @__crystal_v2_slab_frame_push()")
      output.should contain("call void @__crystal_v2_slab_frame_pop()")
    end

    it "skips entrypoint when __crystal_main is filtered" do
      mod = Crystal::MIR::Module.new("test")
      func = mod.create_function("__crystal_main", Crystal::MIR::TypeRef::VOID)
      builder = Crystal::MIR::Builder.new(func)
      args = [] of Crystal::MIR::ValueId
      builder.extern_call("typeof_foo", args, Crystal::MIR::TypeRef::VOID)
      builder.ret

      gen = Crystal::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      gen.reachability = true
      output = gen.generate

      output.should_not contain("define i32 @main")
      output.should_not contain("call void @__crystal_main")
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

      output.should contain("call ptr @__crystal_v2_malloc64(i64 40)")  # 32 + 8 for RC
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

      output.should contain("call ptr @__crystal_v2_slab_alloc(i32 0)")  # Size class 0 for <=16 bytes
    end

    it "generates slab free" do
      mod = Crystal::MIR::Module.new("test")
      func = mod.create_function("slab_free_test", Crystal::MIR::TypeRef::VOID)

      builder = Crystal::MIR::Builder.new(func)
      ptr = builder.alloc(Crystal::MIR::MemoryStrategy::Slab, Crystal::MIR::TypeRef::INT32, 16_u64, 4_u32)
      builder.free(ptr, Crystal::MIR::MemoryStrategy::Slab)
      builder.ret

      gen = Crystal::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      output.should contain("call void @__crystal_v2_slab_free(ptr %")
    end

    it "normalizes union stores to zeroinitializer" do
      mod = Crystal::MIR::Module.new("test")
      union_type = mod.type_registry.create_type(Crystal::MIR::TypeKind::Union, "IntOrNil", 16, 8)
      union_ref = Crystal::MIR::TypeRef.new(union_type.id)
      mod.register_union(
        union_ref,
        Crystal::MIR::UnionDescriptor.new(
          "IntOrNil",
          [
            Crystal::MIR::UnionVariantDescriptor.new(
              type_id: 0,
              type_ref: Crystal::MIR::TypeRef::INT32,
              full_name: "Int32",
              size: 4,
              alignment: 4,
              field_offsets: nil
            ),
            Crystal::MIR::UnionVariantDescriptor.new(
              type_id: 1,
              type_ref: Crystal::MIR::TypeRef::NIL,
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

      func = mod.create_function("union_store", Crystal::MIR::TypeRef::VOID)
      builder = Crystal::MIR::Builder.new(func)
      ptr = builder.alloc(Crystal::MIR::MemoryStrategy::Stack, union_ref, 16_u64, 8_u32)

      block = func.get_block(func.entry_block)
      union_nil = Crystal::MIR::Constant.new(func.next_value_id, union_ref, nil)
      block.add(union_nil)
      builder.store(ptr, union_nil.id)
      builder.ret

      gen = Crystal::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      output.should contain("store %IntOrNil.union zeroinitializer")
    end

    it "skips emitting SSA values for void casts" do
      mod = Crystal::MIR::Module.new("test")
      func = mod.create_function("void_cast", Crystal::MIR::TypeRef::VOID)
      builder = Crystal::MIR::Builder.new(func)
      val = builder.const_int(1_i64, Crystal::MIR::TypeRef::INT32)
      builder.cast(Crystal::MIR::CastKind::Bitcast, val, Crystal::MIR::TypeRef::VOID)
      builder.ret

      gen = Crystal::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      output.should_not contain("alloca void")
    end

    it "casts ptr to float64 via ptrtoint + uitofp (without dereference)" do
      mod = Crystal::MIR::Module.new("test")
      func = mod.create_function("ptr_to_float_cast", Crystal::MIR::TypeRef::FLOAT64)
      func.add_param("p", Crystal::MIR::TypeRef::POINTER)
      builder = Crystal::MIR::Builder.new(func)

      casted = builder.cast(Crystal::MIR::CastKind::Bitcast, 0_u32, Crystal::MIR::TypeRef::FLOAT64)
      builder.ret(casted)

      gen = Crystal::MIR::LLVMIRGenerator.new(mod)
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
      mod = Crystal::MIR::Module.new("test")
      func = mod.create_function("extern_unsigned_widen", Crystal::MIR::TypeRef::INT32)
      func.add_param("fmt", Crystal::MIR::TypeRef::POINTER)
      builder = Crystal::MIR::Builder.new(func)

      dst = builder.const_nil_typed(Crystal::MIR::TypeRef::POINTER)
      len = builder.const_uint(255_u64, Crystal::MIR::TypeRef::UINT32)
      call_args = [dst, len, 0_u32] of Crystal::MIR::ValueId
      res = builder.extern_call("snprintf", call_args, Crystal::MIR::TypeRef::INT32)
      builder.ret(res)

      gen = Crystal::MIR::LLVMIRGenerator.new(mod)
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
      mod = Crystal::MIR::Module.new("test")
      func = mod.create_function("float_to_uint_cast", Crystal::MIR::TypeRef::UINT32)
      func.add_param("x", Crystal::MIR::TypeRef::FLOAT64)
      builder = Crystal::MIR::Builder.new(func)

      casted = builder.cast(Crystal::MIR::CastKind::Bitcast, 0_u32, Crystal::MIR::TypeRef::UINT32)
      builder.ret(casted)

      gen = Crystal::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      func_ir = output[/define i32 @float_to_uint_cast\(double %x\)\s*\{.*?\n\}/m]
      func_ir.should_not be_nil
      body = func_ir.not_nil!
      body.should contain("fptoui double %x to i32")
      body.should_not contain("fptosi double %x to i32")
    end

    it "uses uitofp for uint32 to float64 cast" do
      mod = Crystal::MIR::Module.new("test")
      func = mod.create_function("uint_to_float_cast", Crystal::MIR::TypeRef::FLOAT64)
      func.add_param("x", Crystal::MIR::TypeRef::UINT32)
      builder = Crystal::MIR::Builder.new(func)

      casted = builder.cast(Crystal::MIR::CastKind::Bitcast, 0_u32, Crystal::MIR::TypeRef::FLOAT64)
      builder.ret(casted)

      gen = Crystal::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      func_ir = output[/define double @uint_to_float_cast\(i32 %x\)\s*\{.*?\n\}/m]
      func_ir.should_not be_nil
      body = func_ir.not_nil!
      body.should contain("uitofp i32 %x to double")
      body.should_not contain("sitofp i32 %x to double")
    end

    it "uses uitofp for uint32 argument when calling float64 callee" do
      mod = Crystal::MIR::Module.new("test")

      callee = mod.create_function("takes_f64_arg", Crystal::MIR::TypeRef::FLOAT64)
      callee.add_param("x", Crystal::MIR::TypeRef::FLOAT64)
      callee_builder = Crystal::MIR::Builder.new(callee)
      callee_builder.ret(0_u32)

      caller = mod.create_function("call_uint_to_f64_arg", Crystal::MIR::TypeRef::FLOAT64)
      caller.add_param("x", Crystal::MIR::TypeRef::UINT32)
      caller_builder = Crystal::MIR::Builder.new(caller)
      call = caller_builder.call(callee.id, ([0_u32] of Crystal::MIR::ValueId), Crystal::MIR::TypeRef::FLOAT64)
      caller_builder.ret(call)

      gen = Crystal::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      func_ir = output[/define double @call_uint_to_f64_arg\([^)]*\)\s*\{.*?\n\}/m]
      func_ir.should_not be_nil
      body = func_ir.not_nil!

      body.should contain("uitofp i32 %x to double")
      body.should_not contain("sitofp i32 %x to double")
    end

    it "uses ptrtoint + uitofp for pointer argument when calling float64 callee" do
      mod = Crystal::MIR::Module.new("test")

      callee = mod.create_function("takes_f64_ptr_arg", Crystal::MIR::TypeRef::FLOAT64)
      callee.add_param("x", Crystal::MIR::TypeRef::FLOAT64)
      callee_builder = Crystal::MIR::Builder.new(callee)
      callee_builder.ret(0_u32)

      caller = mod.create_function("call_ptr_to_f64_arg", Crystal::MIR::TypeRef::FLOAT64)
      caller.add_param("p", Crystal::MIR::TypeRef::POINTER)
      caller_builder = Crystal::MIR::Builder.new(caller)
      call = caller_builder.call(callee.id, ([0_u32] of Crystal::MIR::ValueId), Crystal::MIR::TypeRef::FLOAT64)
      caller_builder.ret(call)

      gen = Crystal::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      func_ir = output[/define double @call_ptr_to_f64_arg\([^)]*\)\s*\{.*?\n\}/m]
      func_ir.should_not be_nil
      body = func_ir.not_nil!

      body.should contain("ptrtoint ptr %p to i64")
      body.should match(/uitofp i64 %ptrtofp\.\d+\.int to double/)
      body.should_not contain("load double, ptr %p")
    end

    it "uses align 4 for union payload load in union-to-float call coercion" do
      mod = Crystal::MIR::Module.new("test")

      union_type = mod.type_registry.create_type(Crystal::MIR::TypeKind::Union, "FloatOrNilArg", 16, 8)
      union_ref = Crystal::MIR::TypeRef.new(union_type.id)
      mod.register_union(
        union_ref,
        Crystal::MIR::UnionDescriptor.new(
          "FloatOrNilArg",
          [
            Crystal::MIR::UnionVariantDescriptor.new(
              type_id: 0,
              type_ref: Crystal::MIR::TypeRef::FLOAT64,
              full_name: "Float64",
              size: 8,
              alignment: 8,
              field_offsets: nil
            ),
            Crystal::MIR::UnionVariantDescriptor.new(
              type_id: 1,
              type_ref: Crystal::MIR::TypeRef::NIL,
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

      callee = mod.create_function("takes_union_coerced_f64", Crystal::MIR::TypeRef::FLOAT64)
      callee.add_param("x", Crystal::MIR::TypeRef::FLOAT64)
      callee_builder = Crystal::MIR::Builder.new(callee)
      callee_builder.ret(0_u32)

      caller = mod.create_function("call_union_to_f64_arg", Crystal::MIR::TypeRef::FLOAT64)
      caller.add_param("u", union_ref)
      caller_builder = Crystal::MIR::Builder.new(caller)
      call = caller_builder.call(callee.id, ([0_u32] of Crystal::MIR::ValueId), Crystal::MIR::TypeRef::FLOAT64)
      caller_builder.ret(call)

      gen = Crystal::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      func_ir = output[/define double @call_union_to_f64_arg\([^)]*\)\s*\{.*?\n\}/m]
      func_ir.should_not be_nil
      body = func_ir.not_nil!

      body.should match(/%union_to_fp\.\d+\.val = load double, ptr %union_to_fp\.\d+\.payload_ptr, align 4/)
    end

    it "emits ptr phi when a union phi has ptr incoming" do
      mod = Crystal::MIR::Module.new("phi_union_ptr")

      union_type = mod.type_registry.create_type(Crystal::MIR::TypeKind::Union, "PtrOrNil", 16, 8)
      union_ref = Crystal::MIR::TypeRef.new(union_type.id)
      mod.register_union(
        union_ref,
        Crystal::MIR::UnionDescriptor.new(
          "PtrOrNil",
          [
            Crystal::MIR::UnionVariantDescriptor.new(
              type_id: 0,
              type_ref: Crystal::MIR::TypeRef::POINTER,
              full_name: "Pointer",
              size: 8,
              alignment: 8,
              field_offsets: nil
            ),
            Crystal::MIR::UnionVariantDescriptor.new(
              type_id: 1,
              type_ref: Crystal::MIR::TypeRef::NIL,
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

      func = mod.create_function("phi_union_ptr", Crystal::MIR::TypeRef::VOID)
      builder = Crystal::MIR::Builder.new(func)

      entry = func.entry_block
      then_block = func.create_block
      else_block = func.create_block
      merge_block = func.create_block

      cond = builder.const_bool(true)
      builder.branch(cond, then_block, else_block)

      builder.current_block = then_block
      ptr_val = builder.alloc(Crystal::MIR::MemoryStrategy::Stack, Crystal::MIR::TypeRef::INT32, 4_u64, 4_u32)
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

      gen = Crystal::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      phi_line = output.lines.find { |line| line.includes?("phi %PtrOrNil.union") }
      phi_line.should_not be_nil
      phi_line.not_nil!.should_not contain("zeroinitializer")
    end

    it "emits align 4 for union payload store/load" do
      mod = Crystal::MIR::Module.new("union_align")

      union_type = mod.type_registry.create_type(Crystal::MIR::TypeKind::Union, "PtrOrNilAlign", 16, 8)
      union_ref = Crystal::MIR::TypeRef.new(union_type.id)
      mod.register_union(
        union_ref,
        Crystal::MIR::UnionDescriptor.new(
          "PtrOrNilAlign",
          [
            Crystal::MIR::UnionVariantDescriptor.new(
              type_id: 0,
              type_ref: Crystal::MIR::TypeRef::POINTER,
              full_name: "Pointer",
              size: 8,
              alignment: 8,
              field_offsets: nil
            ),
            Crystal::MIR::UnionVariantDescriptor.new(
              type_id: 1,
              type_ref: Crystal::MIR::TypeRef::NIL,
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

      func = mod.create_function("union_align_payload", Crystal::MIR::TypeRef::POINTER)
      builder = Crystal::MIR::Builder.new(func)
      ptr_val = builder.alloc(Crystal::MIR::MemoryStrategy::Stack, Crystal::MIR::TypeRef::INT32, 4_u64, 4_u32)
      wrapped = builder.union_wrap(ptr_val, 0, union_ref)
      unwrapped = builder.emit(Crystal::MIR::UnionUnwrap.new(func.next_value_id, Crystal::MIR::TypeRef::POINTER, wrapped, 0))
      builder.ret(unwrapped)

      gen = Crystal::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      func_ir = output[/define ptr @union_align_payload\(\)\s*\{.*?\n\}/m]
      func_ir.should_not be_nil
      body = func_ir.not_nil!
      body.should match(/store ptr .*payload_ptr, align 4/)
      body.should match(/load ptr, ptr %.*payload_ptr, align 4/)
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

      # Parameters use their name directly
      output.should contain("call void @__crystal_v2_rc_inc(ptr %ptr)")
      output.should contain("call void @__crystal_v2_rc_dec(ptr %ptr, ptr null)")
    end

    it "generates atomic RC increment with atomicrmw" do
      mod = Crystal::MIR::Module.new("test")
      func = mod.create_function("atomic_rc_inc", Crystal::MIR::TypeRef::VOID)
      func.add_param("ptr", Crystal::MIR::TypeRef::POINTER)

      builder = Crystal::MIR::Builder.new(func)
      builder.rc_inc(0_u32, atomic: true)
      builder.ret

      gen = Crystal::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      # Should use inline atomicrmw instead of function call
      output.should contain("getelementptr i8, ptr %ptr, i64 -8")
      output.should contain("atomicrmw add")
      output.should contain("seq_cst")
      # Should NOT call external function
      output.should_not contain("call void @__crystal_v2_rc_inc_atomic")
    end

    it "generates atomic RC decrement with conditional free" do
      mod = Crystal::MIR::Module.new("test")
      func = mod.create_function("atomic_rc_dec", Crystal::MIR::TypeRef::VOID)
      func.add_param("ptr", Crystal::MIR::TypeRef::POINTER)

      builder = Crystal::MIR::Builder.new(func)
      builder.rc_dec(0_u32, atomic: true)
      builder.ret

      gen = Crystal::MIR::LLVMIRGenerator.new(mod)
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
      mod = Crystal::MIR::Module.new("test")
      func = mod.create_function("tsan_test", Crystal::MIR::TypeRef::VOID)
      func.add_param("ptr", Crystal::MIR::TypeRef::POINTER)

      builder = Crystal::MIR::Builder.new(func)
      loaded = builder.load(0_u32, Crystal::MIR::TypeRef::INT32)
      builder.store(0_u32, loaded)
      builder.ret

      gen = Crystal::MIR::LLVMIRGenerator.new(mod)
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
      mod = Crystal::MIR::Module.new("test")
      func = mod.create_function("no_tsan", Crystal::MIR::TypeRef::VOID)
      func.add_param("ptr", Crystal::MIR::TypeRef::POINTER)

      builder = Crystal::MIR::Builder.new(func)
      loaded = builder.load(0_u32, Crystal::MIR::TypeRef::INT32)
      builder.store(0_u32, loaded)
      builder.ret

      gen = Crystal::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      gen.emit_tsan = false
      output = gen.generate

      # Should NOT have TSan instrumentation
      output.should_not contain("@__tsan_read")
      output.should_not contain("@__tsan_write")
      output.should_not contain("@__tsan_func_entry")
    end

    it "generates TSan acquire/release for atomic RC" do
      mod = Crystal::MIR::Module.new("test")
      func = mod.create_function("tsan_atomic_rc", Crystal::MIR::TypeRef::VOID)
      func.add_param("ptr", Crystal::MIR::TypeRef::POINTER)

      builder = Crystal::MIR::Builder.new(func)
      builder.rc_inc(0_u32, atomic: true)
      builder.rc_dec(0_u32, atomic: true)
      builder.ret

      gen = Crystal::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      gen.emit_tsan = true
      output = gen.generate

      # Should have acquire/release annotations for TSan
      output.should contain("call void @__tsan_release(ptr %ptr)")
      output.should contain("call void @__tsan_acquire(ptr %ptr)")
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

  describe "synchronization primitives" do
    it "generates atomic load with memory ordering" do
      mod = Crystal::MIR::Module.new("test")
      func = mod.create_function("atomic_load_test", Crystal::MIR::TypeRef::INT64)
      func.add_param("ptr", Crystal::MIR::TypeRef::POINTER)

      builder = Crystal::MIR::Builder.new(func)
      result = builder.atomic_load(0_u32, Crystal::MIR::TypeRef::INT64, Crystal::MIR::MemoryOrdering::Acquire)
      builder.ret(result)

      gen = Crystal::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      output.should contain("load atomic i64")
      output.should contain("acquire")
    end

    it "generates atomic store with memory ordering" do
      mod = Crystal::MIR::Module.new("test")
      func = mod.create_function("atomic_store_test", Crystal::MIR::TypeRef::VOID)
      func.add_param("ptr", Crystal::MIR::TypeRef::POINTER)
      func.add_param("val", Crystal::MIR::TypeRef::INT64)

      builder = Crystal::MIR::Builder.new(func)
      builder.atomic_store(0_u32, 1_u32, Crystal::MIR::MemoryOrdering::Release)
      builder.ret

      gen = Crystal::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      output.should contain("store atomic")
      output.should contain("release")
    end

    it "generates atomic compare-and-swap" do
      mod = Crystal::MIR::Module.new("test")
      func = mod.create_function("atomic_cas_test", Crystal::MIR::TypeRef::INT64)
      func.add_param("ptr", Crystal::MIR::TypeRef::POINTER)
      func.add_param("expected", Crystal::MIR::TypeRef::INT64)
      func.add_param("desired", Crystal::MIR::TypeRef::INT64)

      builder = Crystal::MIR::Builder.new(func)
      result = builder.atomic_cas(0_u32, 1_u32, 2_u32, Crystal::MIR::TypeRef::INT64)
      builder.ret(result)

      gen = Crystal::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      output.should contain("cmpxchg")
      output.should contain("seq_cst")
      output.should contain("extractvalue")
    end

    it "generates atomic read-modify-write operations" do
      mod = Crystal::MIR::Module.new("test")
      func = mod.create_function("atomic_rmw_test", Crystal::MIR::TypeRef::INT64)
      func.add_param("ptr", Crystal::MIR::TypeRef::POINTER)
      func.add_param("val", Crystal::MIR::TypeRef::INT64)

      builder = Crystal::MIR::Builder.new(func)
      result = builder.atomic_rmw(Crystal::MIR::AtomicRMWOp::Add, 0_u32, 1_u32, Crystal::MIR::TypeRef::INT64)
      builder.ret(result)

      gen = Crystal::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      output.should contain("atomicrmw add")
      output.should contain("seq_cst")
    end

    it "generates memory fence" do
      mod = Crystal::MIR::Module.new("test")
      func = mod.create_function("fence_test", Crystal::MIR::TypeRef::VOID)

      builder = Crystal::MIR::Builder.new(func)
      builder.fence(Crystal::MIR::MemoryOrdering::AcqRel)
      builder.ret

      gen = Crystal::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      output.should contain("fence acq_rel")
    end

    it "generates mutex lock/unlock calls" do
      mod = Crystal::MIR::Module.new("test")
      func = mod.create_function("mutex_test", Crystal::MIR::TypeRef::VOID)
      func.add_param("mutex", Crystal::MIR::TypeRef::POINTER)

      builder = Crystal::MIR::Builder.new(func)
      builder.mutex_lock(0_u32)
      builder.mutex_unlock(0_u32)
      builder.ret

      gen = Crystal::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      output.should contain("call void @__crystal_v2_mutex_lock")
      output.should contain("call void @__crystal_v2_mutex_unlock")
    end

    it "generates mutex trylock call" do
      mod = Crystal::MIR::Module.new("test")
      func = mod.create_function("trylock_test", Crystal::MIR::TypeRef::BOOL)
      func.add_param("mutex", Crystal::MIR::TypeRef::POINTER)

      builder = Crystal::MIR::Builder.new(func)
      result = builder.mutex_trylock(0_u32)
      builder.ret(result)

      gen = Crystal::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      output.should contain("call i1 @__crystal_v2_mutex_trylock")
    end

    it "generates channel send/receive/close calls" do
      mod = Crystal::MIR::Module.new("test")
      func = mod.create_function("channel_test", Crystal::MIR::TypeRef::POINTER)
      func.add_param("channel", Crystal::MIR::TypeRef::POINTER)
      func.add_param("data", Crystal::MIR::TypeRef::POINTER)

      builder = Crystal::MIR::Builder.new(func)
      builder.channel_send(0_u32, 1_u32)
      result = builder.channel_receive(0_u32, Crystal::MIR::TypeRef::POINTER)
      builder.channel_close(0_u32)
      builder.ret(result)

      gen = Crystal::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      output = gen.generate

      output.should contain("call void @__crystal_v2_channel_send")
      output.should contain("call ptr @__crystal_v2_channel_receive")
      output.should contain("call void @__crystal_v2_channel_close")
    end

    it "generates TSan annotations for mutex operations" do
      mod = Crystal::MIR::Module.new("test")
      func = mod.create_function("mutex_tsan_test", Crystal::MIR::TypeRef::VOID)
      func.add_param("mutex", Crystal::MIR::TypeRef::POINTER)

      builder = Crystal::MIR::Builder.new(func)
      builder.mutex_lock(0_u32)
      builder.mutex_unlock(0_u32)
      builder.ret

      gen = Crystal::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      gen.emit_tsan = true
      output = gen.generate

      output.should contain("@__tsan_acquire")
      output.should contain("@__tsan_release")
    end

    it "generates TSan annotations for channel operations" do
      mod = Crystal::MIR::Module.new("test")
      func = mod.create_function("channel_tsan_test", Crystal::MIR::TypeRef::POINTER)
      func.add_param("channel", Crystal::MIR::TypeRef::POINTER)
      func.add_param("data", Crystal::MIR::TypeRef::POINTER)

      builder = Crystal::MIR::Builder.new(func)
      builder.channel_send(0_u32, 1_u32)
      result = builder.channel_receive(0_u32, Crystal::MIR::TypeRef::POINTER)
      builder.ret(result)

      gen = Crystal::MIR::LLVMIRGenerator.new(mod)
      gen.emit_type_metadata = false
      gen.emit_tsan = true
      output = gen.generate

      # Send releases, receive acquires
      output.should contain("@__tsan_release")
      output.should contain("@__tsan_acquire")
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
