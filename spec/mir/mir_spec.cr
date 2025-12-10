require "../spec_helper"
require "../../src/compiler/mir/mir"

describe Crystal::MIR do
  # ═══════════════════════════════════════════════════════════════════════════
  # MODULE AND FUNCTION
  # ═══════════════════════════════════════════════════════════════════════════

  describe "Module" do
    it "creates functions" do
      mod = Crystal::MIR::Module.new("test")

      func = mod.create_function("add", Crystal::MIR::TypeRef::INT32)
      func.add_param("a", Crystal::MIR::TypeRef::INT32)
      func.add_param("b", Crystal::MIR::TypeRef::INT32)

      func.name.should eq("add")
      func.params.size.should eq(2)
      func.return_type.should eq(Crystal::MIR::TypeRef::INT32)
    end

    it "looks up functions by name" do
      mod = Crystal::MIR::Module.new("test")
      mod.create_function("foo", Crystal::MIR::TypeRef::VOID)

      mod.get_function("foo").should_not be_nil
      mod.get_function("bar").should be_nil
    end
  end

  describe "Function" do
    it "creates basic blocks" do
      mod = Crystal::MIR::Module.new
      func = mod.create_function("test", Crystal::MIR::TypeRef::VOID)

      # Entry block created automatically
      func.blocks.size.should eq(1)
      func.entry_block.should eq(0_u32)

      # Create additional blocks
      block1 = func.create_block
      block2 = func.create_block

      func.blocks.size.should eq(3)
      block1.should eq(1_u32)
      block2.should eq(2_u32)
    end

    it "computes predecessors" do
      mod = Crystal::MIR::Module.new
      func = mod.create_function("test", Crystal::MIR::TypeRef::VOID)

      then_block = func.create_block
      else_block = func.create_block
      merge_block = func.create_block

      # entry → then, else
      func.get_block(func.entry_block).terminator = Crystal::MIR::Branch.new(0_u32, then_block, else_block)

      # then → merge
      func.get_block(then_block).terminator = Crystal::MIR::Jump.new(merge_block)

      # else → merge
      func.get_block(else_block).terminator = Crystal::MIR::Jump.new(merge_block)

      # merge → return
      func.get_block(merge_block).terminator = Crystal::MIR::Return.new(nil)

      func.compute_predecessors

      func.get_block(func.entry_block).predecessors.should be_empty
      func.get_block(then_block).predecessors.should eq([func.entry_block])
      func.get_block(else_block).predecessors.should eq([func.entry_block])
      func.get_block(merge_block).predecessors.sort.should eq([then_block, else_block].sort)
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # BUILDER
  # ═══════════════════════════════════════════════════════════════════════════

  describe "Builder" do
    it "builds constants" do
      mod = Crystal::MIR::Module.new
      func = mod.create_function("test", Crystal::MIR::TypeRef::INT32)
      builder = Crystal::MIR::Builder.new(func)

      a = builder.const_int(42)
      b = builder.const_bool(true)
      c = builder.const_nil

      func.get_block(func.entry_block).instructions.size.should eq(3)
    end

    it "builds arithmetic operations" do
      mod = Crystal::MIR::Module.new
      func = mod.create_function("test", Crystal::MIR::TypeRef::INT32)
      builder = Crystal::MIR::Builder.new(func)

      a = builder.const_int(10, Crystal::MIR::TypeRef::INT32)
      b = builder.const_int(20, Crystal::MIR::TypeRef::INT32)

      sum = builder.add(a, b, Crystal::MIR::TypeRef::INT32)
      diff = builder.sub(a, b, Crystal::MIR::TypeRef::INT32)
      prod = builder.mul(a, b, Crystal::MIR::TypeRef::INT32)

      builder.ret(sum)

      block = func.get_block(func.entry_block)
      block.instructions.size.should eq(5)

      # Verify types
      block.instructions[2].should be_a(Crystal::MIR::BinaryOp)
      (block.instructions[2].as(Crystal::MIR::BinaryOp)).op.should eq(Crystal::MIR::BinOp::Add)
    end

    it "builds comparisons" do
      mod = Crystal::MIR::Module.new
      func = mod.create_function("test", Crystal::MIR::TypeRef::BOOL)
      builder = Crystal::MIR::Builder.new(func)

      a = builder.const_int(5, Crystal::MIR::TypeRef::INT32)
      b = builder.const_int(10, Crystal::MIR::TypeRef::INT32)

      lt_result = builder.lt(a, b)
      eq_result = builder.eq(a, b)

      lt_result.should_not eq(eq_result)

      block = func.get_block(func.entry_block)
      block.instructions[2].type.should eq(Crystal::MIR::TypeRef::BOOL)
      block.instructions[3].type.should eq(Crystal::MIR::TypeRef::BOOL)
    end

    it "builds memory operations" do
      mod = Crystal::MIR::Module.new
      func = mod.create_function("test", Crystal::MIR::TypeRef::VOID)
      builder = Crystal::MIR::Builder.new(func)

      # Stack allocation
      stack_ptr = builder.alloc(Crystal::MIR::MemoryStrategy::Stack, Crystal::MIR::TypeRef::INT32)

      # Store
      val = builder.const_int(42, Crystal::MIR::TypeRef::INT32)
      builder.store(stack_ptr, val)

      # Load
      loaded = builder.load(stack_ptr, Crystal::MIR::TypeRef::INT32)

      builder.ret(nil)

      block = func.get_block(func.entry_block)
      block.instructions.size.should eq(4)

      alloc_inst = block.instructions[0].as(Crystal::MIR::Alloc)
      alloc_inst.strategy.should eq(Crystal::MIR::MemoryStrategy::Stack)
    end

    it "builds ARC operations" do
      mod = Crystal::MIR::Module.new
      func = mod.create_function("test", Crystal::MIR::TypeRef::VOID)
      builder = Crystal::MIR::Builder.new(func)

      # ARC allocation
      ptr = builder.alloc(Crystal::MIR::MemoryStrategy::ARC, Crystal::MIR::TypeRef.new(100_u32))

      # Increment refcount
      builder.rc_inc(ptr)

      # Decrement refcount
      builder.rc_dec(ptr)

      builder.ret(nil)

      block = func.get_block(func.entry_block)
      block.instructions[1].should be_a(Crystal::MIR::RCIncrement)
      block.instructions[2].should be_a(Crystal::MIR::RCDecrement)
    end

    it "builds atomic ARC operations" do
      mod = Crystal::MIR::Module.new
      func = mod.create_function("test", Crystal::MIR::TypeRef::VOID)
      builder = Crystal::MIR::Builder.new(func)

      ptr = builder.alloc(Crystal::MIR::MemoryStrategy::AtomicARC, Crystal::MIR::TypeRef.new(100_u32))

      builder.rc_inc(ptr, atomic: true)
      builder.rc_dec(ptr, atomic: true)

      builder.ret(nil)

      block = func.get_block(func.entry_block)
      inc = block.instructions[1].as(Crystal::MIR::RCIncrement)
      dec = block.instructions[2].as(Crystal::MIR::RCDecrement)

      inc.atomic.should be_true
      dec.atomic.should be_true
    end

    it "builds GEP for field access" do
      mod = Crystal::MIR::Module.new
      func = mod.create_function("test", Crystal::MIR::TypeRef::VOID)
      builder = Crystal::MIR::Builder.new(func)

      ptr = builder.alloc(Crystal::MIR::MemoryStrategy::Stack, Crystal::MIR::TypeRef.new(100_u32))

      # Access field at index 1
      field_ptr = builder.gep(ptr, [0_u32, 1_u32], Crystal::MIR::TypeRef::POINTER)

      field_ptr.should_not eq(ptr)

      gep = func.get_block(func.entry_block).instructions[1].as(Crystal::MIR::GetElementPtr)
      gep.indices.should eq([0_u32, 1_u32])
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # PHI NODES
  # ═══════════════════════════════════════════════════════════════════════════

  describe "Phi nodes" do
    it "builds phi with multiple incoming values" do
      mod = Crystal::MIR::Module.new
      func = mod.create_function("test", Crystal::MIR::TypeRef::INT32)
      builder = Crystal::MIR::Builder.new(func)

      then_block = func.create_block
      else_block = func.create_block
      merge_block = func.create_block

      # Entry: branch on condition
      cond = builder.const_bool(true)
      builder.branch(cond, then_block, else_block)

      # Then block: value = 1
      builder.current_block = then_block
      val1 = builder.const_int(1, Crystal::MIR::TypeRef::INT32)
      builder.jump(merge_block)

      # Else block: value = 2
      builder.current_block = else_block
      val2 = builder.const_int(2, Crystal::MIR::TypeRef::INT32)
      builder.jump(merge_block)

      # Merge block: phi
      builder.current_block = merge_block
      phi = builder.phi(Crystal::MIR::TypeRef::INT32)
      phi.add_incoming(then_block, val1)
      phi.add_incoming(else_block, val2)
      builder.ret(phi.id)

      func.compute_predecessors

      # Verify phi node
      phi.incoming.size.should eq(2)
      phi.type.should eq(Crystal::MIR::TypeRef::INT32)

      # Phi should be first instruction in merge block
      merge = func.get_block(merge_block)
      merge.instructions[0].should be_a(Crystal::MIR::Phi)
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # FUNCTION CALLS
  # ═══════════════════════════════════════════════════════════════════════════

  describe "Function calls" do
    it "builds direct calls" do
      mod = Crystal::MIR::Module.new

      # Create callee
      callee = mod.create_function("add", Crystal::MIR::TypeRef::INT32)
      callee.add_param("a", Crystal::MIR::TypeRef::INT32)
      callee.add_param("b", Crystal::MIR::TypeRef::INT32)

      # Create caller
      caller_func = mod.create_function("test", Crystal::MIR::TypeRef::INT32)
      builder = Crystal::MIR::Builder.new(caller_func)

      a = builder.const_int(10, Crystal::MIR::TypeRef::INT32)
      b = builder.const_int(20, Crystal::MIR::TypeRef::INT32)

      result = builder.call(callee.id, [a, b], Crystal::MIR::TypeRef::INT32)
      builder.ret(result)

      call_inst = caller_func.get_block(caller_func.entry_block).instructions[2].as(Crystal::MIR::Call)
      call_inst.callee.should eq(callee.id)
      call_inst.args.size.should eq(2)
    end

    it "builds indirect calls" do
      mod = Crystal::MIR::Module.new
      func = mod.create_function("test", Crystal::MIR::TypeRef::VOID)
      builder = Crystal::MIR::Builder.new(func)

      # Assume we have a function pointer in some value
      func_ptr = builder.const_int(0, Crystal::MIR::TypeRef::POINTER)  # placeholder

      arg = builder.const_int(42, Crystal::MIR::TypeRef::INT32)
      result = builder.call_indirect(func_ptr, [arg], Crystal::MIR::TypeRef::VOID)

      builder.ret(nil)

      block = func.get_block(func.entry_block)
      block.instructions[2].should be_a(Crystal::MIR::IndirectCall)
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # SWITCH
  # ═══════════════════════════════════════════════════════════════════════════

  describe "Switch terminator" do
    it "builds switch with cases" do
      mod = Crystal::MIR::Module.new
      func = mod.create_function("test", Crystal::MIR::TypeRef::INT32)
      builder = Crystal::MIR::Builder.new(func)

      case1_block = func.create_block
      case2_block = func.create_block
      default_block = func.create_block

      val = builder.const_int(1, Crystal::MIR::TypeRef::INT32)

      cases = [
        {1_i64, case1_block},
        {2_i64, case2_block},
      ]
      builder.switch(val, cases, default_block)

      term = func.get_block(func.entry_block).terminator.as(Crystal::MIR::Switch)
      term.cases.size.should eq(2)
      term.default_block.should eq(default_block)
      term.successors.size.should eq(3)  # 2 cases + default
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # TEXT OUTPUT
  # ═══════════════════════════════════════════════════════════════════════════

  describe "text output" do
    it "prints function" do
      mod = Crystal::MIR::Module.new("test")
      func = mod.create_function("add", Crystal::MIR::TypeRef::INT32)
      func.add_param("a", Crystal::MIR::TypeRef::INT32)
      func.add_param("b", Crystal::MIR::TypeRef::INT32)

      builder = Crystal::MIR::Builder.new(func)
      # a + b where a=param0, b=param1
      # In MIR, params are accessed via Load from param slots
      a = builder.const_int(0, Crystal::MIR::TypeRef::INT32)  # placeholder
      b = builder.const_int(0, Crystal::MIR::TypeRef::INT32)  # placeholder
      sum = builder.add(a, b, Crystal::MIR::TypeRef::INT32)
      builder.ret(sum)

      output = func.to_s
      output.should contain("func @add")
      output.should contain("block.0:")
      output.should contain("add")
      output.should contain("ret")
    end

    it "prints memory operations" do
      mod = Crystal::MIR::Module.new
      func = mod.create_function("test", Crystal::MIR::TypeRef::VOID)
      builder = Crystal::MIR::Builder.new(func)

      ptr = builder.alloc(Crystal::MIR::MemoryStrategy::ARC, Crystal::MIR::TypeRef.new(100_u32))
      builder.rc_inc(ptr)
      builder.rc_dec(ptr)
      builder.ret(nil)

      output = func.to_s
      output.should contain("alloc arc")
      output.should contain("rc_inc")
      output.should contain("rc_dec")
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # CASTS
  # ═══════════════════════════════════════════════════════════════════════════

  describe "Cast operations" do
    it "builds various casts" do
      mod = Crystal::MIR::Module.new
      func = mod.create_function("test", Crystal::MIR::TypeRef::VOID)
      builder = Crystal::MIR::Builder.new(func)

      i32_val = builder.const_int(42, Crystal::MIR::TypeRef::INT32)

      # Sign extend to i64
      i64_val = builder.cast(Crystal::MIR::CastKind::SExt, i32_val, Crystal::MIR::TypeRef::INT64)

      # Convert to float
      f64_val = builder.cast(Crystal::MIR::CastKind::SIToFP, i32_val, Crystal::MIR::TypeRef::FLOAT64)

      builder.ret(nil)

      block = func.get_block(func.entry_block)
      block.instructions[1].as(Crystal::MIR::Cast).kind.should eq(Crystal::MIR::CastKind::SExt)
      block.instructions[2].as(Crystal::MIR::Cast).kind.should eq(Crystal::MIR::CastKind::SIToFP)
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # SELECT (TERNARY)
  # ═══════════════════════════════════════════════════════════════════════════

  describe "Select instruction" do
    it "builds select (ternary)" do
      mod = Crystal::MIR::Module.new
      func = mod.create_function("max", Crystal::MIR::TypeRef::INT32)
      builder = Crystal::MIR::Builder.new(func)

      a = builder.const_int(10, Crystal::MIR::TypeRef::INT32)
      b = builder.const_int(20, Crystal::MIR::TypeRef::INT32)
      cond = builder.gt(a, b)

      result = builder.select(cond, a, b, Crystal::MIR::TypeRef::INT32)
      builder.ret(result)

      select_inst = func.get_block(func.entry_block).instructions[3].as(Crystal::MIR::Select)
      select_inst.condition.should eq(cond)
      select_inst.then_value.should eq(a)
      select_inst.else_value.should eq(b)
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # OPERANDS
  # ═══════════════════════════════════════════════════════════════════════════

  describe "operands" do
    it "returns correct operands for each instruction type" do
      mod = Crystal::MIR::Module.new
      func = mod.create_function("test", Crystal::MIR::TypeRef::VOID)
      builder = Crystal::MIR::Builder.new(func)

      a = builder.const_int(1, Crystal::MIR::TypeRef::INT32)
      b = builder.const_int(2, Crystal::MIR::TypeRef::INT32)

      sum = builder.add(a, b, Crystal::MIR::TypeRef::INT32)
      neg = builder.neg(a, Crystal::MIR::TypeRef::INT32)

      block = func.get_block(func.entry_block)

      # Constants have no operands
      block.instructions[0].operands.should be_empty
      block.instructions[1].operands.should be_empty

      # BinaryOp has two operands
      block.instructions[2].operands.should eq([a, b])

      # UnaryOp has one operand
      block.instructions[3].operands.should eq([a])
    end
  end
end
