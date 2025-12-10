require "../spec_helper"
require "../../src/compiler/hir/hir"
require "../../src/compiler/hir/escape_analysis"
require "../../src/compiler/hir/taint_analysis"
require "../../src/compiler/hir/memory_strategy"
require "../../src/compiler/mir/mir"
require "../../src/compiler/mir/hir_to_mir"

describe Crystal::MIR::HIRToMIRLowering do
  # ═══════════════════════════════════════════════════════════════════════════
  # BASIC LOWERING
  # ═══════════════════════════════════════════════════════════════════════════

  describe "basic lowering" do
    it "lowers empty function" do
      hir_mod = Crystal::HIR::Module.new("test")
      hir_func = hir_mod.create_function("empty", Crystal::HIR::TypeRef::VOID)
      hir_func.get_block(hir_func.entry_block).terminator = Crystal::HIR::Return.new

      lowering = Crystal::MIR::HIRToMIRLowering.new(hir_mod)
      mir_mod = lowering.lower

      mir_mod.functions.size.should eq(1)
      mir_mod.functions[0].name.should eq("empty")
      mir_mod.functions[0].blocks.size.should be >= 1
      lowering.stats.functions_lowered.should eq(1)
    end

    it "lowers function with parameters" do
      hir_mod = Crystal::HIR::Module.new("test")
      hir_func = hir_mod.create_function("add", Crystal::HIR::TypeRef::INT32)
      hir_func.add_param("a", Crystal::HIR::TypeRef::INT32)
      hir_func.add_param("b", Crystal::HIR::TypeRef::INT32)
      hir_func.get_block(hir_func.entry_block).terminator = Crystal::HIR::Return.new

      mir_mod = hir_mod.lower_to_mir

      mir_func = mir_mod.functions[0]
      mir_func.params.size.should eq(2)
      mir_func.params[0].name.should eq("a")
      mir_func.params[1].name.should eq("b")
    end

    it "lowers integer literals" do
      hir_mod = Crystal::HIR::Module.new("test")
      hir_func = hir_mod.create_function("const_int", Crystal::HIR::TypeRef::INT64)
      block = hir_func.get_block(hir_func.entry_block)

      lit = Crystal::HIR::Literal.new(hir_func.next_value_id, Crystal::HIR::TypeRef::INT64, 42_i64)
      block.add(lit)
      block.terminator = Crystal::HIR::Return.new(lit.id)

      mir_mod = hir_mod.lower_to_mir

      mir_block = mir_mod.functions[0].get_block(mir_mod.functions[0].entry_block)
      mir_block.instructions.size.should be >= 1

      const = mir_block.instructions.find { |i| i.is_a?(Crystal::MIR::Constant) }
      const.should_not be_nil
    end

    it "lowers boolean literals" do
      hir_mod = Crystal::HIR::Module.new("test")
      hir_func = hir_mod.create_function("const_bool", Crystal::HIR::TypeRef::BOOL)
      block = hir_func.get_block(hir_func.entry_block)

      lit = Crystal::HIR::Literal.new(hir_func.next_value_id, Crystal::HIR::TypeRef::BOOL, true)
      block.add(lit)
      block.terminator = Crystal::HIR::Return.new(lit.id)

      mir_mod = hir_mod.lower_to_mir

      mir_block = mir_mod.functions[0].get_block(mir_mod.functions[0].entry_block)
      const = mir_block.instructions.find { |i| i.is_a?(Crystal::MIR::Constant) }
      const.should_not be_nil
      const.as(Crystal::MIR::Constant).value.should eq(true)
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # ARITHMETIC LOWERING
  # ═══════════════════════════════════════════════════════════════════════════

  describe "arithmetic operations" do
    it "lowers add operation" do
      hir_mod = Crystal::HIR::Module.new("test")
      hir_func = hir_mod.create_function("add", Crystal::HIR::TypeRef::INT32)
      block = hir_func.get_block(hir_func.entry_block)

      a = Crystal::HIR::Literal.new(hir_func.next_value_id, Crystal::HIR::TypeRef::INT32, 10_i64)
      b = Crystal::HIR::Literal.new(hir_func.next_value_id, Crystal::HIR::TypeRef::INT32, 20_i64)
      add_op = Crystal::HIR::BinaryOperation.new(
        hir_func.next_value_id,
        Crystal::HIR::TypeRef::INT32,
        Crystal::HIR::BinaryOp::Add,
        a.id,
        b.id
      )

      block.add(a)
      block.add(b)
      block.add(add_op)
      block.terminator = Crystal::HIR::Return.new(add_op.id)

      mir_mod = hir_mod.lower_to_mir

      mir_block = mir_mod.functions[0].get_block(mir_mod.functions[0].entry_block)
      binop = mir_block.instructions.find { |i| i.is_a?(Crystal::MIR::BinaryOp) }
      binop.should_not be_nil
      binop.as(Crystal::MIR::BinaryOp).op.should eq(Crystal::MIR::BinOp::Add)
    end

    it "lowers comparison operations" do
      hir_mod = Crystal::HIR::Module.new("test")
      hir_func = hir_mod.create_function("compare", Crystal::HIR::TypeRef::BOOL)
      block = hir_func.get_block(hir_func.entry_block)

      a = Crystal::HIR::Literal.new(hir_func.next_value_id, Crystal::HIR::TypeRef::INT32, 5_i64)
      b = Crystal::HIR::Literal.new(hir_func.next_value_id, Crystal::HIR::TypeRef::INT32, 10_i64)
      cmp = Crystal::HIR::BinaryOperation.new(
        hir_func.next_value_id,
        Crystal::HIR::TypeRef::BOOL,
        Crystal::HIR::BinaryOp::Lt,
        a.id,
        b.id
      )

      block.add(a)
      block.add(b)
      block.add(cmp)
      block.terminator = Crystal::HIR::Return.new(cmp.id)

      mir_mod = hir_mod.lower_to_mir

      mir_block = mir_mod.functions[0].get_block(mir_mod.functions[0].entry_block)
      binop = mir_block.instructions.find { |i| i.is_a?(Crystal::MIR::BinaryOp) }
      binop.should_not be_nil
      binop.as(Crystal::MIR::BinaryOp).op.should eq(Crystal::MIR::BinOp::Lt)
    end

    it "lowers unary negation" do
      hir_mod = Crystal::HIR::Module.new("test")
      hir_func = hir_mod.create_function("negate", Crystal::HIR::TypeRef::INT32)
      block = hir_func.get_block(hir_func.entry_block)

      a = Crystal::HIR::Literal.new(hir_func.next_value_id, Crystal::HIR::TypeRef::INT32, 42_i64)
      neg = Crystal::HIR::UnaryOperation.new(
        hir_func.next_value_id,
        Crystal::HIR::TypeRef::INT32,
        Crystal::HIR::UnaryOp::Neg,
        a.id
      )

      block.add(a)
      block.add(neg)
      block.terminator = Crystal::HIR::Return.new(neg.id)

      mir_mod = hir_mod.lower_to_mir

      mir_block = mir_mod.functions[0].get_block(mir_mod.functions[0].entry_block)
      unop = mir_block.instructions.find { |i| i.is_a?(Crystal::MIR::UnaryOp) }
      unop.should_not be_nil
      unop.as(Crystal::MIR::UnaryOp).op.should eq(Crystal::MIR::UnOp::Neg)
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # CONTROL FLOW LOWERING
  # ═══════════════════════════════════════════════════════════════════════════

  describe "control flow" do
    it "lowers conditional branch" do
      hir_mod = Crystal::HIR::Module.new("test")
      hir_func = hir_mod.create_function("branch", Crystal::HIR::TypeRef::INT32)
      func_scope = hir_func.entry_block

      # Create blocks
      entry_block = hir_func.get_block(hir_func.entry_block)
      then_block_id = hir_func.create_block(0_u32)
      else_block_id = hir_func.create_block(0_u32)

      # Entry: branch on condition
      cond = Crystal::HIR::Literal.new(hir_func.next_value_id, Crystal::HIR::TypeRef::BOOL, true)
      entry_block.add(cond)
      entry_block.terminator = Crystal::HIR::Branch.new(cond.id, then_block_id, else_block_id)

      # Then block: return 1
      then_block = hir_func.get_block(then_block_id)
      one = Crystal::HIR::Literal.new(hir_func.next_value_id, Crystal::HIR::TypeRef::INT32, 1_i64)
      then_block.add(one)
      then_block.terminator = Crystal::HIR::Return.new(one.id)

      # Else block: return 0
      else_block = hir_func.get_block(else_block_id)
      zero = Crystal::HIR::Literal.new(hir_func.next_value_id, Crystal::HIR::TypeRef::INT32, 0_i64)
      else_block.add(zero)
      else_block.terminator = Crystal::HIR::Return.new(zero.id)

      mir_mod = hir_mod.lower_to_mir

      mir_func = mir_mod.functions[0]
      mir_func.blocks.size.should be >= 3

      # Check branch terminator
      entry_mir = mir_func.get_block(mir_func.entry_block)
      entry_mir.terminator.should be_a(Crystal::MIR::Branch)
    end

    it "lowers unconditional jump" do
      hir_mod = Crystal::HIR::Module.new("test")
      hir_func = hir_mod.create_function("jump", Crystal::HIR::TypeRef::VOID)

      entry_block = hir_func.get_block(hir_func.entry_block)
      target_block_id = hir_func.create_block(0_u32)

      entry_block.terminator = Crystal::HIR::Jump.new(target_block_id)

      target_block = hir_func.get_block(target_block_id)
      target_block.terminator = Crystal::HIR::Return.new

      mir_mod = hir_mod.lower_to_mir

      mir_func = mir_mod.functions[0]
      entry_mir = mir_func.get_block(mir_func.entry_block)
      entry_mir.terminator.should be_a(Crystal::MIR::Jump)
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # MEMORY ALLOCATION LOWERING
  # ═══════════════════════════════════════════════════════════════════════════

  describe "memory allocation" do
    it "lowers allocation with stack strategy for non-escaping values" do
      hir_mod = Crystal::HIR::Module.new("test")
      hir_func = hir_mod.create_function("alloc", Crystal::HIR::TypeRef::VOID)
      block = hir_func.get_block(hir_func.entry_block)

      # Non-escaping allocation
      alloc = Crystal::HIR::Allocate.new(hir_func.next_value_id, Crystal::HIR::TypeRef.new(100_u32))
      alloc.lifetime = Crystal::HIR::LifetimeTag::StackLocal
      block.add(alloc)
      block.terminator = Crystal::HIR::Return.new

      lowering = Crystal::MIR::HIRToMIRLowering.new(hir_mod)
      mir_mod = lowering.lower

      lowering.stats.stack_allocations.should be >= 1
    end

    it "lowers allocation with ARC strategy for escaping values" do
      hir_mod = Crystal::HIR::Module.new("test")
      hir_func = hir_mod.create_function("alloc", Crystal::HIR::TypeRef.new(100_u32))
      block = hir_func.get_block(hir_func.entry_block)

      # Escaping allocation
      alloc = Crystal::HIR::Allocate.new(hir_func.next_value_id, Crystal::HIR::TypeRef.new(100_u32))
      alloc.lifetime = Crystal::HIR::LifetimeTag::HeapEscape
      block.add(alloc)
      block.terminator = Crystal::HIR::Return.new(alloc.id)

      lowering = Crystal::MIR::HIRToMIRLowering.new(hir_mod)
      mir_mod = lowering.lower

      lowering.stats.arc_allocations.should be >= 1
    end

    it "lowers allocation with GC strategy for cyclic types" do
      hir_mod = Crystal::HIR::Module.new("test")
      hir_func = hir_mod.create_function("alloc", Crystal::HIR::TypeRef::VOID)
      block = hir_func.get_block(hir_func.entry_block)

      # Cyclic allocation
      alloc = Crystal::HIR::Allocate.new(hir_func.next_value_id, Crystal::HIR::TypeRef.new(100_u32))
      alloc.lifetime = Crystal::HIR::LifetimeTag::StackLocal
      alloc.taints = Crystal::HIR::Taint::Cyclic
      block.add(alloc)
      block.terminator = Crystal::HIR::Return.new

      lowering = Crystal::MIR::HIRToMIRLowering.new(hir_mod)
      mir_mod = lowering.lower

      lowering.stats.gc_allocations.should be >= 1
    end

    it "inserts RC operations for ARC allocations" do
      hir_mod = Crystal::HIR::Module.new("test")
      hir_func = hir_mod.create_function("arc_alloc", Crystal::HIR::TypeRef.new(100_u32))
      block = hir_func.get_block(hir_func.entry_block)

      alloc = Crystal::HIR::Allocate.new(hir_func.next_value_id, Crystal::HIR::TypeRef.new(100_u32))
      alloc.lifetime = Crystal::HIR::LifetimeTag::HeapEscape
      block.add(alloc)
      block.terminator = Crystal::HIR::Return.new(alloc.id)

      mir_mod = hir_mod.lower_to_mir

      mir_block = mir_mod.functions[0].get_block(mir_mod.functions[0].entry_block)
      rc_inc = mir_block.instructions.find { |i| i.is_a?(Crystal::MIR::RCIncrement) }
      rc_inc.should_not be_nil
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # CLOSURE LOWERING
  # ═══════════════════════════════════════════════════════════════════════════

  describe "closure lowering" do
    it "lowers closure with captures" do
      hir_mod = Crystal::HIR::Module.new("test")
      hir_func = hir_mod.create_function("make_closure", Crystal::HIR::TypeRef.new(200_u32))
      block = hir_func.get_block(hir_func.entry_block)

      # Create captured variable
      cap_var = Crystal::HIR::Literal.new(hir_func.next_value_id, Crystal::HIR::TypeRef::INT32, 0_i64)
      block.add(cap_var)

      # Create closure
      closure_block = hir_func.create_block(0_u32)
      captures = [Crystal::HIR::CapturedVar.new(cap_var.id, "x", by_reference: true)]
      closure = Crystal::HIR::MakeClosure.new(
        hir_func.next_value_id,
        Crystal::HIR::TypeRef.new(200_u32),
        closure_block,
        captures
      )
      block.add(closure)
      block.terminator = Crystal::HIR::Return.new(closure.id)

      # Empty closure body
      hir_func.get_block(closure_block).terminator = Crystal::HIR::Return.new

      lowering = Crystal::MIR::HIRToMIRLowering.new(hir_mod)
      mir_mod = lowering.lower

      lowering.stats.closures_lowered.should eq(1)
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # PHI NODE LOWERING
  # ═══════════════════════════════════════════════════════════════════════════

  describe "phi node lowering" do
    it "lowers phi nodes with incoming edges" do
      hir_mod = Crystal::HIR::Module.new("test")
      hir_func = hir_mod.create_function("phi_test", Crystal::HIR::TypeRef::INT32)

      entry_block = hir_func.get_block(hir_func.entry_block)
      then_block_id = hir_func.create_block(0_u32)
      else_block_id = hir_func.create_block(0_u32)
      merge_block_id = hir_func.create_block(0_u32)

      # Entry: branch
      cond = Crystal::HIR::Literal.new(hir_func.next_value_id, Crystal::HIR::TypeRef::BOOL, true)
      entry_block.add(cond)
      entry_block.terminator = Crystal::HIR::Branch.new(cond.id, then_block_id, else_block_id)

      # Then: value 1
      then_block = hir_func.get_block(then_block_id)
      val1 = Crystal::HIR::Literal.new(hir_func.next_value_id, Crystal::HIR::TypeRef::INT32, 1_i64)
      then_block.add(val1)
      then_block.terminator = Crystal::HIR::Jump.new(merge_block_id)

      # Else: value 2
      else_block = hir_func.get_block(else_block_id)
      val2 = Crystal::HIR::Literal.new(hir_func.next_value_id, Crystal::HIR::TypeRef::INT32, 2_i64)
      else_block.add(val2)
      else_block.terminator = Crystal::HIR::Jump.new(merge_block_id)

      # Merge: phi node
      merge_block = hir_func.get_block(merge_block_id)
      phi = Crystal::HIR::Phi.new(hir_func.next_value_id, Crystal::HIR::TypeRef::INT32)
      phi.add_incoming(then_block_id, val1.id)
      phi.add_incoming(else_block_id, val2.id)
      merge_block.add(phi)
      merge_block.terminator = Crystal::HIR::Return.new(phi.id)

      mir_mod = hir_mod.lower_to_mir

      mir_func = mir_mod.functions[0]
      mir_func.blocks.size.should be >= 4

      # Check predecessors are computed
      mir_func.compute_predecessors
      merge_mir = mir_func.blocks.find { |b| b.instructions.any? { |i| i.is_a?(Crystal::MIR::Phi) } }
      merge_mir.should_not be_nil
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # LOWERING STATISTICS
  # ═══════════════════════════════════════════════════════════════════════════

  describe "lowering statistics" do
    it "tracks allocation counts by strategy" do
      hir_mod = Crystal::HIR::Module.new("test")
      hir_func = hir_mod.create_function("multi_alloc", Crystal::HIR::TypeRef::VOID)
      block = hir_func.get_block(hir_func.entry_block)

      # Stack allocation
      alloc1 = Crystal::HIR::Allocate.new(hir_func.next_value_id, Crystal::HIR::TypeRef.new(100_u32))
      alloc1.lifetime = Crystal::HIR::LifetimeTag::StackLocal
      block.add(alloc1)

      # Heap escape -> ARC
      alloc2 = Crystal::HIR::Allocate.new(hir_func.next_value_id, Crystal::HIR::TypeRef.new(101_u32))
      alloc2.lifetime = Crystal::HIR::LifetimeTag::HeapEscape
      block.add(alloc2)

      # Cyclic -> GC
      alloc3 = Crystal::HIR::Allocate.new(hir_func.next_value_id, Crystal::HIR::TypeRef.new(102_u32))
      alloc3.taints = Crystal::HIR::Taint::Cyclic
      block.add(alloc3)

      block.terminator = Crystal::HIR::Return.new

      lowering = Crystal::MIR::HIRToMIRLowering.new(hir_mod)
      mir_mod = lowering.lower

      lowering.stats.total_allocations.should eq(3)
      lowering.stats.stack_allocations.should eq(1)
      lowering.stats.arc_allocations.should eq(1)
      lowering.stats.gc_allocations.should eq(1)
    end

    it "reports statistics as string" do
      lowering = Crystal::MIR::HIRToMIRLowering.new(Crystal::HIR::Module.new)
      lowering.stats.functions_lowered = 5
      lowering.stats.blocks_lowered = 20
      lowering.stats.values_lowered = 100

      str = lowering.stats.to_s
      str.should contain("Functions: 5")
      str.should contain("Blocks: 20")
      str.should contain("Values: 100")
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # TYPE CONVERSION
  # ═══════════════════════════════════════════════════════════════════════════

  describe "type conversion" do
    it "converts primitive types correctly" do
      hir_mod = Crystal::HIR::Module.new("test")
      hir_func = hir_mod.create_function("types", Crystal::HIR::TypeRef::INT32)
      block = hir_func.get_block(hir_func.entry_block)

      # Different primitive types
      int_lit = Crystal::HIR::Literal.new(hir_func.next_value_id, Crystal::HIR::TypeRef::INT32, 1_i64)
      float_lit = Crystal::HIR::Literal.new(hir_func.next_value_id, Crystal::HIR::TypeRef::FLOAT64, 3.14_f64)
      bool_lit = Crystal::HIR::Literal.new(hir_func.next_value_id, Crystal::HIR::TypeRef::BOOL, true)

      block.add(int_lit)
      block.add(float_lit)
      block.add(bool_lit)
      block.terminator = Crystal::HIR::Return.new(int_lit.id)

      mir_mod = hir_mod.lower_to_mir

      mir_block = mir_mod.functions[0].get_block(mir_mod.functions[0].entry_block)
      mir_block.instructions.size.should eq(3)
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # CONVENIENCE EXTENSION
  # ═══════════════════════════════════════════════════════════════════════════

  describe "convenience extension" do
    it "allows calling lower_to_mir on HIR module" do
      hir_mod = Crystal::HIR::Module.new("test")
      hir_func = hir_mod.create_function("test", Crystal::HIR::TypeRef::VOID)
      hir_func.get_block(hir_func.entry_block).terminator = Crystal::HIR::Return.new

      mir_mod = hir_mod.lower_to_mir

      mir_mod.should be_a(Crystal::MIR::Module)
      mir_mod.functions.size.should eq(1)
    end
  end
end
