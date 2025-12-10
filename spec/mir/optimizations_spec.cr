require "../spec_helper"
require "../../src/compiler/mir/mir"
require "../../src/compiler/mir/optimizations"

describe Crystal::MIR do
  # ═══════════════════════════════════════════════════════════════════════════
  # RC ELISION
  # ═══════════════════════════════════════════════════════════════════════════

  describe "RCElisionPass" do
    it "eliminates adjacent rc_inc/rc_dec pairs" do
      mod = Crystal::MIR::Module.new
      func = mod.create_function("test", Crystal::MIR::TypeRef::VOID)
      builder = Crystal::MIR::Builder.new(func)

      ptr = builder.alloc(Crystal::MIR::MemoryStrategy::ARC, Crystal::MIR::TypeRef.new(100_u32))
      builder.rc_inc(ptr)
      builder.rc_dec(ptr)
      builder.ret(nil)

      # Before: alloc, rc_inc, rc_dec, ret (4 instructions + terminator)
      func.get_block(func.entry_block).instructions.size.should eq(3)

      pass = Crystal::MIR::RCElisionPass.new(func)
      eliminated = pass.run

      eliminated.should eq(2)
      # After: only alloc
      func.get_block(func.entry_block).instructions.size.should eq(1)
    end

    it "eliminates multiple pairs" do
      mod = Crystal::MIR::Module.new
      func = mod.create_function("test", Crystal::MIR::TypeRef::VOID)
      builder = Crystal::MIR::Builder.new(func)

      ptr1 = builder.alloc(Crystal::MIR::MemoryStrategy::ARC, Crystal::MIR::TypeRef.new(100_u32))
      ptr2 = builder.alloc(Crystal::MIR::MemoryStrategy::ARC, Crystal::MIR::TypeRef.new(101_u32))

      builder.rc_inc(ptr1)
      builder.rc_inc(ptr2)
      builder.rc_dec(ptr2)
      builder.rc_dec(ptr1)

      builder.ret(nil)

      func.get_block(func.entry_block).instructions.size.should eq(6)

      pass = Crystal::MIR::RCElisionPass.new(func)
      eliminated = pass.run

      eliminated.should eq(4)
      func.get_block(func.entry_block).instructions.size.should eq(2)  # just allocs
    end

    it "doesn't elide across function calls" do
      mod = Crystal::MIR::Module.new
      func = mod.create_function("test", Crystal::MIR::TypeRef::VOID)
      builder = Crystal::MIR::Builder.new(func)

      ptr = builder.alloc(Crystal::MIR::MemoryStrategy::ARC, Crystal::MIR::TypeRef.new(100_u32))
      builder.rc_inc(ptr)

      # Call that uses ptr - don't elide
      builder.call(0_u32, [ptr], Crystal::MIR::TypeRef::VOID)

      builder.rc_dec(ptr)
      builder.ret(nil)

      pass = Crystal::MIR::RCElisionPass.new(func)
      eliminated = pass.run

      # Should not elide because call might consume the reference
      eliminated.should eq(0)
    end

    it "doesn't elide across stores" do
      mod = Crystal::MIR::Module.new
      func = mod.create_function("test", Crystal::MIR::TypeRef::VOID)
      builder = Crystal::MIR::Builder.new(func)

      ptr = builder.alloc(Crystal::MIR::MemoryStrategy::ARC, Crystal::MIR::TypeRef.new(100_u32))
      dest = builder.alloc(Crystal::MIR::MemoryStrategy::Stack, Crystal::MIR::TypeRef::POINTER)

      builder.rc_inc(ptr)
      builder.store(dest, ptr)  # Transfers ownership
      builder.rc_dec(ptr)

      builder.ret(nil)

      pass = Crystal::MIR::RCElisionPass.new(func)
      eliminated = pass.run

      # Should not elide because store transfers the reference
      eliminated.should eq(0)
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # DEAD CODE ELIMINATION
  # ═══════════════════════════════════════════════════════════════════════════

  describe "DeadCodeEliminationPass" do
    it "removes unused constants" do
      mod = Crystal::MIR::Module.new
      func = mod.create_function("test", Crystal::MIR::TypeRef::INT32)
      builder = Crystal::MIR::Builder.new(func)

      used = builder.const_int(42, Crystal::MIR::TypeRef::INT32)
      _unused = builder.const_int(99, Crystal::MIR::TypeRef::INT32)  # Never used
      builder.ret(used)

      func.get_block(func.entry_block).instructions.size.should eq(2)

      pass = Crystal::MIR::DeadCodeEliminationPass.new(func)
      eliminated = pass.run

      eliminated.should eq(1)
      func.get_block(func.entry_block).instructions.size.should eq(1)
    end

    it "removes unused arithmetic" do
      mod = Crystal::MIR::Module.new
      func = mod.create_function("test", Crystal::MIR::TypeRef::INT32)
      builder = Crystal::MIR::Builder.new(func)

      a = builder.const_int(10, Crystal::MIR::TypeRef::INT32)
      b = builder.const_int(20, Crystal::MIR::TypeRef::INT32)

      _unused_sum = builder.add(a, b, Crystal::MIR::TypeRef::INT32)  # Not used
      result = builder.mul(a, b, Crystal::MIR::TypeRef::INT32)

      builder.ret(result)

      func.get_block(func.entry_block).instructions.size.should eq(4)

      pass = Crystal::MIR::DeadCodeEliminationPass.new(func)
      eliminated = pass.run

      eliminated.should eq(1)
      func.get_block(func.entry_block).instructions.size.should eq(3)
    end

    it "cascades removal" do
      mod = Crystal::MIR::Module.new
      func = mod.create_function("test", Crystal::MIR::TypeRef::VOID)
      builder = Crystal::MIR::Builder.new(func)

      a = builder.const_int(10, Crystal::MIR::TypeRef::INT32)
      b = builder.const_int(20, Crystal::MIR::TypeRef::INT32)
      sum = builder.add(a, b, Crystal::MIR::TypeRef::INT32)
      _prod = builder.mul(sum, a, Crystal::MIR::TypeRef::INT32)  # Not used

      builder.ret(nil)

      func.get_block(func.entry_block).instructions.size.should eq(4)

      pass = Crystal::MIR::DeadCodeEliminationPass.new(func)
      eliminated = pass.run

      # Should remove prod, then sum, then a and b
      eliminated.should eq(4)
      func.get_block(func.entry_block).instructions.size.should eq(0)
    end

    it "preserves side effects" do
      mod = Crystal::MIR::Module.new
      func = mod.create_function("test", Crystal::MIR::TypeRef::VOID)
      builder = Crystal::MIR::Builder.new(func)

      ptr = builder.alloc(Crystal::MIR::MemoryStrategy::ARC, Crystal::MIR::TypeRef.new(100_u32))
      builder.rc_dec(ptr)  # Has side effect (may free)
      builder.ret(nil)

      pass = Crystal::MIR::DeadCodeEliminationPass.new(func)
      eliminated = pass.run

      # rc_dec preserved because it has side effects
      # alloc preserved because rc_dec uses it
      eliminated.should eq(0)
    end

    it "preserves stores" do
      mod = Crystal::MIR::Module.new
      func = mod.create_function("test", Crystal::MIR::TypeRef::VOID)
      builder = Crystal::MIR::Builder.new(func)

      ptr = builder.alloc(Crystal::MIR::MemoryStrategy::Stack, Crystal::MIR::TypeRef::INT32)
      val = builder.const_int(42, Crystal::MIR::TypeRef::INT32)
      builder.store(ptr, val)
      builder.ret(nil)

      pass = Crystal::MIR::DeadCodeEliminationPass.new(func)
      eliminated = pass.run

      # Store has side effect, keeps alloc and val alive
      eliminated.should eq(0)
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # CONSTANT FOLDING
  # ═══════════════════════════════════════════════════════════════════════════

  describe "ConstantFoldingPass" do
    it "folds integer addition" do
      mod = Crystal::MIR::Module.new
      func = mod.create_function("test", Crystal::MIR::TypeRef::INT64)
      builder = Crystal::MIR::Builder.new(func)

      a = builder.const_int(10_i64, Crystal::MIR::TypeRef::INT64)
      b = builder.const_int(20_i64, Crystal::MIR::TypeRef::INT64)
      sum = builder.add(a, b, Crystal::MIR::TypeRef::INT64)
      builder.ret(sum)

      pass = Crystal::MIR::ConstantFoldingPass.new(func)
      folded = pass.run

      folded.should eq(1)

      # The add should now be a constant
      block = func.get_block(func.entry_block)
      result = block.instructions[2].as(Crystal::MIR::Constant)
      result.value.should eq(30_i64)
    end

    it "folds integer comparisons" do
      mod = Crystal::MIR::Module.new
      func = mod.create_function("test", Crystal::MIR::TypeRef::BOOL)
      builder = Crystal::MIR::Builder.new(func)

      a = builder.const_int(10_i64, Crystal::MIR::TypeRef::INT64)
      b = builder.const_int(20_i64, Crystal::MIR::TypeRef::INT64)
      cmp = builder.lt(a, b)
      builder.ret(cmp)

      pass = Crystal::MIR::ConstantFoldingPass.new(func)
      folded = pass.run

      folded.should eq(1)

      block = func.get_block(func.entry_block)
      result = block.instructions[2].as(Crystal::MIR::Constant)
      result.value.should eq(true)
    end

    it "folds multiple operations" do
      mod = Crystal::MIR::Module.new
      func = mod.create_function("test", Crystal::MIR::TypeRef::INT64)
      builder = Crystal::MIR::Builder.new(func)

      a = builder.const_int(2_i64, Crystal::MIR::TypeRef::INT64)
      b = builder.const_int(3_i64, Crystal::MIR::TypeRef::INT64)
      c = builder.const_int(4_i64, Crystal::MIR::TypeRef::INT64)

      # (2 + 3) * 4 = 20
      sum = builder.add(a, b, Crystal::MIR::TypeRef::INT64)
      prod = builder.mul(sum, c, Crystal::MIR::TypeRef::INT64)
      builder.ret(prod)

      pass = Crystal::MIR::ConstantFoldingPass.new(func)
      folded = pass.run

      # Should fold both add and mul in single pass
      # (add is folded first, result added to constants map, then mul sees it)
      folded.should eq(2)

      block = func.get_block(func.entry_block)
      # Last arithmetic instruction should now be the folded mul
      result = block.instructions[4].as(Crystal::MIR::Constant)
      result.value.should eq(20_i64)
    end

    it "doesn't fold division by zero" do
      mod = Crystal::MIR::Module.new
      func = mod.create_function("test", Crystal::MIR::TypeRef::INT64)
      builder = Crystal::MIR::Builder.new(func)

      a = builder.const_int(10_i64, Crystal::MIR::TypeRef::INT64)
      b = builder.const_int(0_i64, Crystal::MIR::TypeRef::INT64)
      div = builder.div(a, b, Crystal::MIR::TypeRef::INT64)
      builder.ret(div)

      pass = Crystal::MIR::ConstantFoldingPass.new(func)
      folded = pass.run

      # Should not fold division by zero
      folded.should eq(0)
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # OPTIMIZATION PIPELINE
  # ═══════════════════════════════════════════════════════════════════════════

  describe "OptimizationPipeline" do
    it "runs all passes" do
      mod = Crystal::MIR::Module.new
      func = mod.create_function("test", Crystal::MIR::TypeRef::VOID)
      builder = Crystal::MIR::Builder.new(func)

      # Constant that can be folded
      a = builder.const_int(5_i64, Crystal::MIR::TypeRef::INT64)
      b = builder.const_int(10_i64, Crystal::MIR::TypeRef::INT64)
      _sum = builder.add(a, b, Crystal::MIR::TypeRef::INT64)  # unused

      # RC operations that can be elided
      ptr = builder.alloc(Crystal::MIR::MemoryStrategy::ARC, Crystal::MIR::TypeRef.new(100_u32))
      builder.rc_inc(ptr)
      builder.rc_dec(ptr)

      builder.ret(nil)

      original_count = func.get_block(func.entry_block).instructions.size

      stats = func.optimize

      # Should fold constant, elide RC, and remove dead code
      stats.total.should be > 0

      # Final instruction count should be lower
      func.get_block(func.entry_block).instructions.size.should be < original_count
    end

    it "provides stats" do
      mod = Crystal::MIR::Module.new
      func = mod.create_function("test", Crystal::MIR::TypeRef::VOID)
      builder = Crystal::MIR::Builder.new(func)

      ptr = builder.alloc(Crystal::MIR::MemoryStrategy::ARC, Crystal::MIR::TypeRef.new(100_u32))
      builder.rc_inc(ptr)
      builder.rc_dec(ptr)
      builder.ret(nil)

      stats = func.optimize

      stats.rc_eliminated.should eq(2)
      stats.dead_eliminated.should be >= 0
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # EDGE CASES
  # ═══════════════════════════════════════════════════════════════════════════

  describe "edge cases" do
    it "handles empty blocks" do
      mod = Crystal::MIR::Module.new
      func = mod.create_function("test", Crystal::MIR::TypeRef::VOID)
      builder = Crystal::MIR::Builder.new(func)
      builder.ret(nil)

      stats = func.optimize
      stats.total.should eq(0)
    end

    it "handles phi nodes" do
      mod = Crystal::MIR::Module.new
      func = mod.create_function("test", Crystal::MIR::TypeRef::INT32)
      builder = Crystal::MIR::Builder.new(func)

      then_block = func.create_block
      else_block = func.create_block
      merge_block = func.create_block

      cond = builder.const_bool(true)
      builder.branch(cond, then_block, else_block)

      builder.current_block = then_block
      val1 = builder.const_int(1, Crystal::MIR::TypeRef::INT32)
      builder.jump(merge_block)

      builder.current_block = else_block
      val2 = builder.const_int(2, Crystal::MIR::TypeRef::INT32)
      builder.jump(merge_block)

      builder.current_block = merge_block
      phi = builder.phi(Crystal::MIR::TypeRef::INT32)
      phi.add_incoming(then_block, val1)
      phi.add_incoming(else_block, val2)
      builder.ret(phi.id)

      # Phi operands should keep values alive
      pass = Crystal::MIR::DeadCodeEliminationPass.new(func)
      eliminated = pass.run

      # val1 and val2 are used by phi, should not be eliminated
      eliminated.should eq(0)
    end
  end
end
