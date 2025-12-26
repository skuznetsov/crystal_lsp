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

    it "folds UInt64 addition" do
      mod = Crystal::MIR::Module.new
      func = mod.create_function("test", Crystal::MIR::TypeRef::UINT64)
      builder = Crystal::MIR::Builder.new(func)

      a = builder.const_uint(10_u64, Crystal::MIR::TypeRef::UINT64)
      b = builder.const_uint(20_u64, Crystal::MIR::TypeRef::UINT64)
      sum = builder.add(a, b, Crystal::MIR::TypeRef::UINT64)
      builder.ret(sum)

      pass = Crystal::MIR::ConstantFoldingPass.new(func)
      folded = pass.run

      folded.should eq(1)

      block = func.get_block(func.entry_block)
      result = block.instructions[2].as(Crystal::MIR::Constant)
      result.value.should eq(30_u64)
    end

    it "folds Bool ops" do
      mod = Crystal::MIR::Module.new
      func = mod.create_function("test", Crystal::MIR::TypeRef::BOOL)
      builder = Crystal::MIR::Builder.new(func)

      a = builder.const_bool(true)
      b = builder.const_bool(false)
      and_val = builder.bit_and(a, b, Crystal::MIR::TypeRef::BOOL)
      builder.ret(and_val)

      pass = Crystal::MIR::ConstantFoldingPass.new(func)
      folded = pass.run

      folded.should eq(1)

      block = func.get_block(func.entry_block)
      result = block.instructions[2].as(Crystal::MIR::Constant)
      result.value.should eq(false)
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
  # COPY PROPAGATION
  # ═══════════════════════════════════════════════════════════════════════════

  describe "CopyPropagationPass" do
    it "propagates trivial phi nodes" do
      mod = Crystal::MIR::Module.new
      func = mod.create_function("test", Crystal::MIR::TypeRef::INT32)
      builder = Crystal::MIR::Builder.new(func)

      value = builder.const_int(1_i64, Crystal::MIR::TypeRef::INT32)
      cond = builder.const_bool(true)

      then_block = func.create_block
      else_block = func.create_block
      join_block = func.create_block

      builder.branch(cond, then_block, else_block)

      builder.current_block = then_block
      builder.jump(join_block)

      builder.current_block = else_block
      builder.jump(join_block)

      builder.current_block = join_block
      phi = builder.phi(Crystal::MIR::TypeRef::INT32)
      phi.add_incoming(then_block, value)
      phi.add_incoming(else_block, value)

      sum = builder.add(phi.id, value, Crystal::MIR::TypeRef::INT32)
      builder.ret(sum)

      pass = Crystal::MIR::CopyPropagationPass.new(func)
      propagated = pass.run

      propagated.should be > 0

      join = func.get_block(join_block)
      add = join.instructions.find { |inst| inst.is_a?(Crystal::MIR::BinaryOp) }.as(Crystal::MIR::BinaryOp)
      add.left.should eq(value)
    end

    it "propagates select with constant condition" do
      mod = Crystal::MIR::Module.new
      func = mod.create_function("test", Crystal::MIR::TypeRef::INT32)
      builder = Crystal::MIR::Builder.new(func)

      cond = builder.const_bool(true)
      a = builder.const_int(10_i64, Crystal::MIR::TypeRef::INT32)
      b = builder.const_int(99_i64, Crystal::MIR::TypeRef::INT32)
      sel = builder.select(cond, a, b, Crystal::MIR::TypeRef::INT32)
      sum = builder.add(sel, a, Crystal::MIR::TypeRef::INT32)
      builder.ret(sum)

      pass = Crystal::MIR::CopyPropagationPass.new(func)
      propagated = pass.run

      propagated.should be > 0

      block = func.get_block(func.entry_block)
      add = block.instructions.find { |inst| inst.is_a?(Crystal::MIR::BinaryOp) }.as(Crystal::MIR::BinaryOp)
      add.left.should eq(a)
    end

    it "propagates no-op bitcasts" do
      mod = Crystal::MIR::Module.new
      func = mod.create_function("test", Crystal::MIR::TypeRef::VOID)
      builder = Crystal::MIR::Builder.new(func)

      ptr = builder.alloc(Crystal::MIR::MemoryStrategy::ARC, Crystal::MIR::TypeRef.new(100_u32))
      cast = builder.bitcast(ptr, Crystal::MIR::TypeRef::POINTER)
      builder.rc_inc(cast)
      builder.ret(nil)

      pass = Crystal::MIR::CopyPropagationPass.new(func)
      propagated = pass.run

      propagated.should be > 0

      block = func.get_block(func.entry_block)
      rc_inc = block.instructions.find { |inst| inst.is_a?(Crystal::MIR::RCIncrement) }.as(Crystal::MIR::RCIncrement)
      rc_inc.ptr.should eq(ptr)
    end

    it "propagates algebraic identities" do
      mod = Crystal::MIR::Module.new
      func = mod.create_function("test", Crystal::MIR::TypeRef::INT32)
      builder = Crystal::MIR::Builder.new(func)

      x = builder.const_int(7_i64, Crystal::MIR::TypeRef::INT32)
      zero = builder.const_int(0_i64, Crystal::MIR::TypeRef::INT32)
      one = builder.const_int(1_i64, Crystal::MIR::TypeRef::INT32)

      sum = builder.add(x, zero, Crystal::MIR::TypeRef::INT32)   # x + 0 -> x
      prod = builder.mul(sum, one, Crystal::MIR::TypeRef::INT32) # x * 1 -> x
      builder.ret(prod)

      pass = Crystal::MIR::CopyPropagationPass.new(func)
      propagated = pass.run

      propagated.should be > 0

      block = func.get_block(func.entry_block)
      ret = block.terminator.as(Crystal::MIR::Return)
      ret.value.should eq(x)
    end

    it "forwards load after store in the same block" do
      mod = Crystal::MIR::Module.new
      func = mod.create_function("test", Crystal::MIR::TypeRef::INT32)
      builder = Crystal::MIR::Builder.new(func)

      ptr = builder.alloc(Crystal::MIR::MemoryStrategy::Stack, Crystal::MIR::TypeRef::INT32)
      val = builder.const_int(42_i64, Crystal::MIR::TypeRef::INT32)
      builder.store(ptr, val)
      load = builder.load(ptr, Crystal::MIR::TypeRef::INT32)
      builder.ret(load)

      pass = Crystal::MIR::CopyPropagationPass.new(func)
      propagated = pass.run

      propagated.should be > 0

      block = func.get_block(func.entry_block)
      ret = block.terminator.as(Crystal::MIR::Return)
      ret.value.should eq(val)
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
  # STRUCTURAL NOALIAS (Enhanced Alias Analysis)
  # ═══════════════════════════════════════════════════════════════════════════

  describe "Structural NoAlias" do
    it "elides RC across unrelated stores (different allocation sites)" do
      # This tests the key improvement: stores to one allocation
      # should not invalidate pending incs for different allocation
      mod = Crystal::MIR::Module.new
      func = mod.create_function("test", Crystal::MIR::TypeRef::VOID)
      builder = Crystal::MIR::Builder.new(func)

      # Two separate allocations - different allocation sites
      ptr1 = builder.alloc(Crystal::MIR::MemoryStrategy::ARC, Crystal::MIR::TypeRef.new(100_u32))
      ptr2 = builder.alloc(Crystal::MIR::MemoryStrategy::ARC, Crystal::MIR::TypeRef.new(101_u32))

      # RC inc on ptr1
      builder.rc_inc(ptr1)

      # Store to ptr2 - should NOT affect ptr1's pending inc
      val = builder.const_int(42, Crystal::MIR::TypeRef::INT32)
      builder.store(ptr2, val)

      # RC dec on ptr1 - should elide with above inc since ptr1 ≠ ptr2
      builder.rc_dec(ptr1)

      builder.ret(nil)

      pass = Crystal::MIR::RCElisionPass.new(func)
      eliminated = pass.run

      # Should elide the rc_inc/rc_dec pair for ptr1
      # because store to ptr2 doesn't alias ptr1
      eliminated.should eq(2)
    end

    it "blocks RC elision when value escapes to field" do
      # When we store a value to a field, it escapes and we can't elide
      mod = Crystal::MIR::Module.new
      func = mod.create_function("test", Crystal::MIR::TypeRef::VOID)
      builder = Crystal::MIR::Builder.new(func)

      # Object with a field
      obj = builder.alloc(Crystal::MIR::MemoryStrategy::ARC, Crystal::MIR::TypeRef.new(100_u32))
      # Value we're storing
      val = builder.alloc(Crystal::MIR::MemoryStrategy::ARC, Crystal::MIR::TypeRef.new(101_u32))

      # RC inc on val
      builder.rc_inc(val)

      # Store val to obj's field - val escapes!
      builder.store(obj, val)

      # RC dec on val - should NOT elide because val escaped
      builder.rc_dec(val)

      builder.ret(nil)

      pass = Crystal::MIR::RCElisionPass.new(func)
      eliminated = pass.run

      # Should NOT elide because val was stored (escaped)
      eliminated.should eq(0)
    end

    it "tracks allocation sites through loads" do
      # Test that allocation site tracking works through load chains
      mod = Crystal::MIR::Module.new
      func = mod.create_function("test", Crystal::MIR::TypeRef::VOID)
      builder = Crystal::MIR::Builder.new(func)

      # Two separate allocations
      ptr1 = builder.alloc(Crystal::MIR::MemoryStrategy::ARC, Crystal::MIR::TypeRef.new(100_u32))
      ptr2 = builder.alloc(Crystal::MIR::MemoryStrategy::ARC, Crystal::MIR::TypeRef.new(101_u32))

      builder.rc_inc(ptr1)

      # Load from ptr2 - creates a derived pointer
      loaded = builder.load(ptr2, Crystal::MIR::TypeRef::INT32)

      # Use the loaded value (doesn't affect aliasing)
      _unused = builder.add(loaded, loaded, Crystal::MIR::TypeRef::INT32)

      builder.rc_dec(ptr1)

      builder.ret(nil)

      pass = Crystal::MIR::RCElisionPass.new(func)
      eliminated = pass.run

      # Should elide - load from ptr2 doesn't affect ptr1
      eliminated.should eq(2)
    end

    it "handles self-referential store conservatively" do
      # Storing to the same allocation we're tracking RC for
      mod = Crystal::MIR::Module.new
      func = mod.create_function("test", Crystal::MIR::TypeRef::VOID)
      builder = Crystal::MIR::Builder.new(func)

      ptr = builder.alloc(Crystal::MIR::MemoryStrategy::ARC, Crystal::MIR::TypeRef.new(100_u32))

      builder.rc_inc(ptr)

      # Store value TO the tracked pointer itself
      val = builder.const_int(42, Crystal::MIR::TypeRef::INT32)
      builder.store(ptr, val)

      builder.rc_dec(ptr)

      builder.ret(nil)

      pass = Crystal::MIR::RCElisionPass.new(func)
      eliminated = pass.run

      # The store IS to ptr, but the stored VALUE (val) is not ptr
      # So ptr itself hasn't escaped - elision should work
      eliminated.should eq(2)
    end

    it "blocks elision when storing tracked value" do
      # Storing the tracked pointer as a value escapes it
      mod = Crystal::MIR::Module.new
      func = mod.create_function("test", Crystal::MIR::TypeRef::VOID)
      builder = Crystal::MIR::Builder.new(func)

      dest = builder.alloc(Crystal::MIR::MemoryStrategy::Stack, Crystal::MIR::TypeRef::POINTER)
      ptr = builder.alloc(Crystal::MIR::MemoryStrategy::ARC, Crystal::MIR::TypeRef.new(100_u32))

      builder.rc_inc(ptr)

      # Store ptr to dest - ptr escapes as a VALUE
      builder.store(dest, ptr)

      builder.rc_dec(ptr)

      builder.ret(nil)

      pass = Crystal::MIR::RCElisionPass.new(func)
      eliminated = pass.run

      # Should NOT elide - ptr was stored somewhere
      eliminated.should eq(0)
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # TBAA (Type-Based Alias Analysis)
  # ═══════════════════════════════════════════════════════════════════════════

  describe "TBAA" do
    it "TypeRef.primitive? identifies primitive types" do
      Crystal::MIR::TypeRef::INT32.primitive?.should be_true
      Crystal::MIR::TypeRef::FLOAT64.primitive?.should be_true
      Crystal::MIR::TypeRef::BOOL.primitive?.should be_true
      Crystal::MIR::TypeRef::CHAR.primitive?.should be_true
      Crystal::MIR::TypeRef::SYMBOL.primitive?.should be_true

      # STRING is a reference type
      Crystal::MIR::TypeRef::STRING.primitive?.should be_false

      # User-defined types (id >= 100) are not primitive
      Crystal::MIR::TypeRef.new(100_u32).primitive?.should be_false
      Crystal::MIR::TypeRef.new(200_u32).primitive?.should be_false
    end

    it "TypeRef.reference? identifies reference types" do
      Crystal::MIR::TypeRef::STRING.reference?.should be_true
      Crystal::MIR::TypeRef.new(100_u32).reference?.should be_true
      Crystal::MIR::TypeRef.new(200_u32).reference?.should be_true

      # Primitives are not references
      Crystal::MIR::TypeRef::INT32.reference?.should be_false
      Crystal::MIR::TypeRef::FLOAT64.reference?.should be_false
    end

    it "TypeRef.may_alias_type? returns false for primitive vs reference" do
      int_type = Crystal::MIR::TypeRef::INT32
      class_type = Crystal::MIR::TypeRef.new(100_u32)  # MyClass

      # Primitive cannot alias reference
      int_type.may_alias_type?(class_type).should be_false
      class_type.may_alias_type?(int_type).should be_false

      # Same type may alias
      int_type.may_alias_type?(int_type).should be_true
      class_type.may_alias_type?(class_type).should be_true
    end

    it "TypeRef.may_alias_type? is conservative for POINTER" do
      ptr_type = Crystal::MIR::TypeRef::POINTER
      int_type = Crystal::MIR::TypeRef::INT32
      class_type = Crystal::MIR::TypeRef.new(100_u32)

      # POINTER (void*) can alias anything
      ptr_type.may_alias_type?(int_type).should be_true
      ptr_type.may_alias_type?(class_type).should be_true
      int_type.may_alias_type?(ptr_type).should be_true
    end

    it "elides RC when store is to incompatible type (TBAA)" do
      # This is the key TBAA optimization: store to Int32* cannot affect MyClass*
      mod = Crystal::MIR::Module.new
      func = mod.create_function("test", Crystal::MIR::TypeRef::VOID)
      builder = Crystal::MIR::Builder.new(func)

      # Allocate a MyClass (reference type, id=100)
      my_class_type = Crystal::MIR::TypeRef.new(100_u32)
      obj = builder.alloc(Crystal::MIR::MemoryStrategy::ARC, my_class_type)

      # Allocate an Int32 (primitive type)
      int_ptr = builder.alloc(Crystal::MIR::MemoryStrategy::Stack, Crystal::MIR::TypeRef::INT32)

      builder.rc_inc(obj)

      # Store to int_ptr - this is Int32* which cannot alias MyClass*
      val = builder.const_int(42, Crystal::MIR::TypeRef::INT32)
      builder.store(int_ptr, val)

      builder.rc_dec(obj)

      builder.ret(nil)

      pass = Crystal::MIR::RCElisionPass.new(func)
      eliminated = pass.run

      # Should elide because Int32* cannot alias MyClass*
      eliminated.should eq(2)
    end

    it "blocks RC elision when store type could alias (same type family)" do
      # Store to MyClass* could affect another MyClass*
      mod = Crystal::MIR::Module.new
      func = mod.create_function("test", Crystal::MIR::TypeRef::VOID)
      builder = Crystal::MIR::Builder.new(func)

      # Two allocations of same type
      my_class_type = Crystal::MIR::TypeRef.new(100_u32)
      obj1 = builder.alloc(Crystal::MIR::MemoryStrategy::ARC, my_class_type)
      obj2 = builder.alloc(Crystal::MIR::MemoryStrategy::ARC, my_class_type)

      builder.rc_inc(obj1)

      # Store obj1 to obj2's field - obj1 escapes
      builder.store(obj2, obj1)

      builder.rc_dec(obj1)

      builder.ret(nil)

      pass = Crystal::MIR::RCElisionPass.new(func)
      eliminated = pass.run

      # Should NOT elide - obj1 was stored (escaped)
      eliminated.should eq(0)
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

  # ═══════════════════════════════════════════════════════════════════════════
  # LOCK ELISION
  # ═══════════════════════════════════════════════════════════════════════════

  describe "LockElisionPass" do
    it "elides locks on thread-local mutex" do
      mod = Crystal::MIR::Module.new
      func = mod.create_function("test", Crystal::MIR::TypeRef::VOID)
      builder = Crystal::MIR::Builder.new(func)

      # Allocate a mutex with AtomicARC (treated as mutex candidate)
      mutex = builder.alloc(Crystal::MIR::MemoryStrategy::AtomicARC, Crystal::MIR::TypeRef.new(100_u32))
      builder.mutex_lock(mutex)
      builder.mutex_unlock(mutex)
      builder.ret

      # Before: alloc, lock, unlock (3 instructions)
      func.get_block(func.entry_block).instructions.size.should eq(3)

      pass = Crystal::MIR::LockElisionPass.new(func)
      elided = pass.run

      # Both lock and unlock should be elided (thread-local, no escape)
      elided.should eq(2)
      func.get_block(func.entry_block).instructions.size.should eq(1)  # Only alloc remains
    end

    it "keeps locks on escaped mutex" do
      mod = Crystal::MIR::Module.new
      func = mod.create_function("test", Crystal::MIR::TypeRef::VOID)
      builder = Crystal::MIR::Builder.new(func)

      # Allocate a mutex
      mutex = builder.alloc(Crystal::MIR::MemoryStrategy::AtomicARC, Crystal::MIR::TypeRef.new(100_u32))
      # Escape via store
      field = builder.alloc(Crystal::MIR::MemoryStrategy::Stack, Crystal::MIR::TypeRef::POINTER)
      builder.store(field, mutex)

      builder.mutex_lock(mutex)
      builder.mutex_unlock(mutex)
      builder.ret

      # Before: 2 allocs, store, lock, unlock = 5 instructions
      func.get_block(func.entry_block).instructions.size.should eq(5)

      pass = Crystal::MIR::LockElisionPass.new(func)
      elided = pass.run

      # Locks should NOT be elided (mutex escaped)
      elided.should eq(0)
      func.get_block(func.entry_block).instructions.size.should eq(5)
    end

    it "elides redundant nested locks" do
      mod = Crystal::MIR::Module.new
      func = mod.create_function("test", Crystal::MIR::TypeRef::VOID)
      func.add_param("mutex", Crystal::MIR::TypeRef::POINTER)
      builder = Crystal::MIR::Builder.new(func)

      # param 0 is the mutex pointer (externally provided, can't elide entirely)
      mutex_ptr = 0_u32

      # Nested locks on same mutex: lock; lock; unlock; unlock
      builder.mutex_lock(mutex_ptr)
      builder.mutex_lock(mutex_ptr)  # Redundant
      builder.mutex_unlock(mutex_ptr)  # Corresponds to redundant lock
      builder.mutex_unlock(mutex_ptr)
      builder.ret

      func.get_block(func.entry_block).instructions.size.should eq(4)

      pass = Crystal::MIR::LockElisionPass.new(func)
      elided = pass.run

      # Inner lock/unlock pair should be elided
      elided.should eq(2)
      func.get_block(func.entry_block).instructions.size.should eq(2)
    end

    it "coarsens adjacent critical sections" do
      mod = Crystal::MIR::Module.new
      func = mod.create_function("test", Crystal::MIR::TypeRef::VOID)
      func.add_param("mutex", Crystal::MIR::TypeRef::POINTER)
      func.add_param("x", Crystal::MIR::TypeRef::INT32)
      builder = Crystal::MIR::Builder.new(func)

      mutex_ptr = 0_u32

      # Pattern: lock; unlock; lock; unlock → should coarsen to: lock; unlock
      builder.mutex_lock(mutex_ptr)
      builder.mutex_unlock(mutex_ptr)
      # Only safe operations between (add doesn't prevent coarsening)
      result = builder.add(1_u32, 1_u32, Crystal::MIR::TypeRef::INT32)
      builder.mutex_lock(mutex_ptr)
      builder.mutex_unlock(mutex_ptr)
      builder.ret

      # Before: lock, unlock, add, lock, unlock = 5 instructions
      func.get_block(func.entry_block).instructions.size.should eq(5)

      pass = Crystal::MIR::LockElisionPass.new(func)
      elided = pass.run

      # Should coarsen: remove unlock-lock pair in the middle
      elided.should eq(2)
      func.get_block(func.entry_block).instructions.size.should eq(3)  # lock, add, unlock
    end

    it "does not coarsen when call is between unlock/lock" do
      mod = Crystal::MIR::Module.new
      func = mod.create_function("test", Crystal::MIR::TypeRef::VOID)
      func.add_param("mutex", Crystal::MIR::TypeRef::POINTER)
      builder = Crystal::MIR::Builder.new(func)

      mutex_ptr = 0_u32

      builder.mutex_lock(mutex_ptr)
      builder.mutex_unlock(mutex_ptr)
      # External call between - prevents coarsening
      other_func = mod.create_function("other", Crystal::MIR::TypeRef::VOID)
      empty_args = [] of Crystal::MIR::ValueId
      builder.call(other_func.id, empty_args, Crystal::MIR::TypeRef::VOID)
      builder.mutex_lock(mutex_ptr)
      builder.mutex_unlock(mutex_ptr)
      builder.ret

      # Before: lock, unlock, call, lock, unlock = 5 instructions
      func.get_block(func.entry_block).instructions.size.should eq(5)

      pass = Crystal::MIR::LockElisionPass.new(func)
      elided = pass.run

      # No coarsening due to call
      elided.should eq(0)
      func.get_block(func.entry_block).instructions.size.should eq(5)
    end

    it "tracks locks_elided in OptimizationStats" do
      mod = Crystal::MIR::Module.new
      func = mod.create_function("test", Crystal::MIR::TypeRef::VOID)
      builder = Crystal::MIR::Builder.new(func)

      mutex = builder.alloc(Crystal::MIR::MemoryStrategy::AtomicARC, Crystal::MIR::TypeRef.new(100_u32))
      builder.mutex_lock(mutex)
      builder.mutex_unlock(mutex)
      builder.ret

      pipeline = Crystal::MIR::OptimizationPipeline.new(func)
      stats = pipeline.run

      stats.locks_elided.should eq(2)
    end
  end
end
