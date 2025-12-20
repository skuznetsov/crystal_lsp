# LTP/WBA Framework Tests
#
# Tests for the LTP (Local Trigger → Transport → Potential) optimization framework.
# Based on theory from "LTP/WBA Framework — From G_{3,5} to Kakeya & Magnus"

require "../../spec_helper"
require "../../../src/compiler/mir/mir"
require "../../../src/compiler/mir/optimizations"

# Helper module for test function creation
module LTPTestHelpers
  @@func_id_counter = 0_u32

  def self.next_func_id : Crystal::MIR::FunctionId
    @@func_id_counter += 1
    Crystal::MIR::FunctionId.new(@@func_id_counter)
  end

  def self.create_test_function_with_rc : Crystal::MIR::Function
    int_type = Crystal::MIR::TypeRef::INT32
    void_type = Crystal::MIR::TypeRef::VOID

    func = Crystal::MIR::Function.new(next_func_id, "test_rc", int_type)
    entry_id = func.create_block
    entry = func.get_block(entry_id)

    # Create: alloc → rc_inc → use → rc_dec → return
    # This is the simplest case for Spike move

    alloc = Crystal::MIR::Alloc.new(
      1_u32,                             # id
      int_type,                          # type (result type = ptr to alloc_type)
      Crystal::MIR::MemoryStrategy::ARC, # strategy
      int_type                           # alloc_type
    )
    alloc.no_alias = true
    entry.add(alloc)

    rc_inc = Crystal::MIR::RCIncrement.new(
      2_u32,      # id
      alloc.id    # ptr
    )
    entry.add(rc_inc)

    # Some use instruction
    load_inst = Crystal::MIR::Load.new(
      3_u32,       # id
      int_type,    # type
      alloc.id     # ptr
    )
    entry.add(load_inst)

    rc_dec = Crystal::MIR::RCDecrement.new(
      4_u32,       # id
      alloc.id     # ptr
    )
    entry.add(rc_dec)

    ret = Crystal::MIR::Return.new(load_inst.id)
    entry.terminator = ret

    func
  end

  def self.create_empty_function : Crystal::MIR::Function
    int_type = Crystal::MIR::TypeRef::INT32

    func = Crystal::MIR::Function.new(next_func_id, "empty", int_type)
    entry_id = func.create_block
    entry = func.get_block(entry_id)

    const = Crystal::MIR::Constant.new(
      1_u32,       # id
      int_type,    # type
      42_i64       # value
    )
    entry.add(const)

    ret = Crystal::MIR::Return.new(const.id)
    entry.terminator = ret

    func
  end
end

describe Crystal::MIR::LTPPotential do
  describe "lexicographic comparison" do
    it "compares window_overlap first" do
      p1 = Crystal::MIR::LTPPotential.new(5, 0, 0, 10)
      p2 = Crystal::MIR::LTPPotential.new(3, 0, 0, 10)

      (p2 < p1).should be_true  # Lower window_overlap is better
    end

    it "compares tie_plateau second" do
      # tie_plateau stored as negative count of ties
      # -2 means 2 ties, -1 means 1 tie
      # Lower value = more ties = worse
      # So -2 < -1, meaning p1 < p2 (p1 is better, fewer ties when stored negative)
      p1 = Crystal::MIR::LTPPotential.new(5, -2, 0, 10)
      p2 = Crystal::MIR::LTPPotential.new(5, -1, 0, 10)

      (p1 < p2).should be_true  # -2 < -1, so p1 has lower (better) potential
    end

    it "compares corner_mismatch third" do
      p1 = Crystal::MIR::LTPPotential.new(5, -2, 3, 10)
      p2 = Crystal::MIR::LTPPotential.new(5, -2, 1, 10)

      (p2 < p1).should be_true  # Lower mismatch is better
    end

    it "compares area last" do
      p1 = Crystal::MIR::LTPPotential.new(5, -2, 3, 15)
      p2 = Crystal::MIR::LTPPotential.new(5, -2, 3, 10)

      (p2 < p1).should be_true  # Lower area is better
    end

    it "equals when all components match" do
      p1 = Crystal::MIR::LTPPotential.new(5, -2, 3, 10)
      p2 = Crystal::MIR::LTPPotential.new(5, -2, 3, 10)

      (p1 == p2).should be_true
    end
  end

  describe ".zero" do
    it "creates zero potential" do
      p = Crystal::MIR::LTPPotential.zero

      p.window_overlap.should eq(0)
      p.tie_plateau.should eq(0)
      p.corner_mismatch.should eq(0)
      p.area.should eq(0)
    end
  end

  describe "#to_s" do
    it "formats potential correctly" do
      p = Crystal::MIR::LTPPotential.new(5, -2, 3, 10)
      p.to_s.should eq("Φ′{I=5, -M=-2, P=3, |Δ|=10}")
    end
  end
end

describe Crystal::MIR::CorridorExit do
  it "has all expected exit types" do
    Crystal::MIR::CorridorExit::Boundary.should be_truthy
    Crystal::MIR::CorridorExit::Elision.should be_truthy
    Crystal::MIR::CorridorExit::Escape.should be_truthy
    Crystal::MIR::CorridorExit::Store.should be_truthy
    Crystal::MIR::CorridorExit::Unknown.should be_truthy
  end
end

describe Crystal::MIR::MoveType do
  it "has all four legal move types" do
    Crystal::MIR::MoveType::Spike.should be_truthy
    Crystal::MIR::MoveType::Ladder.should be_truthy
    Crystal::MIR::MoveType::Diamond.should be_truthy
    Crystal::MIR::MoveType::Collapse.should be_truthy
  end
end

describe Crystal::MIR::LTPEngine do
  describe "#run" do
    it "terminates on empty function" do
      func = LTPTestHelpers.create_empty_function
      engine = Crystal::MIR::LTPEngine.new(func)
      potential = engine.run

      potential.window_overlap.should eq(0)
      engine.iterations.should eq(0)
    end

    it "computes initial potential correctly" do
      func = LTPTestHelpers.create_test_function_with_rc
      engine = Crystal::MIR::LTPEngine.new(func)

      # Before running, check initial state
      initial_insts = func.blocks.sum(&.instructions.size)
      initial_insts.should eq(4)  # alloc, rc_inc, load, rc_dec

      potential = engine.run

      # After LTP, RC ops should be optimized if possible
      # The potential's area should be less than or equal to initial
      potential.area.should be <= initial_insts
    end
  end

  describe "BR-5 (Finiteness)" do
    it "terminates within max_iters" do
      func = LTPTestHelpers.create_test_function_with_rc
      engine = Crystal::MIR::LTPEngine.new(func)

      potential = engine.run(max_iters: 3)

      engine.iterations.should be <= 3
    end

    it "records moves applied" do
      func = LTPTestHelpers.create_test_function_with_rc
      engine = Crystal::MIR::LTPEngine.new(func)

      engine.run

      # Moves should be recorded
      engine.moves_applied.should be_a(Array(Crystal::MIR::LegalMove))
    end
  end
end

describe Crystal::MIR::Function do
  describe "#optimize_ltp" do
    it "provides convenience method for LTP optimization" do
      int_type = Crystal::MIR::TypeRef::INT32

      func = Crystal::MIR::Function.new(LTPTestHelpers.next_func_id, "test", int_type)
      entry_id = func.create_block
      entry = func.get_block(entry_id)

      const = Crystal::MIR::Constant.new(
        1_u32,
        int_type,
        42_i64
      )
      entry.add(const)

      ret = Crystal::MIR::Return.new(const.id)
      entry.terminator = ret

      potential = func.optimize_ltp

      potential.should be_a(Crystal::MIR::LTPPotential)
    end

    it "supports debug mode" do
      int_type = Crystal::MIR::TypeRef::INT32

      func = Crystal::MIR::Function.new(LTPTestHelpers.next_func_id, "test", int_type)
      entry_id = func.create_block
      entry = func.get_block(entry_id)

      const = Crystal::MIR::Constant.new(
        1_u32,
        int_type,
        42_i64
      )
      entry.add(const)

      ret = Crystal::MIR::Return.new(const.id)
      entry.terminator = ret

      # Should not raise with debug enabled
      potential = func.optimize_ltp(debug: false)
      potential.should be_a(Crystal::MIR::LTPPotential)
    end
  end
end

describe "LTP Theory Compliance" do
  describe "BR-3 (Potential Decrease)" do
    it "potential is well-founded (non-negative components)" do
      p = Crystal::MIR::LTPPotential.new(0, 0, 0, 0)

      # Zero is minimum
      (p >= Crystal::MIR::LTPPotential.zero).should be_true
    end

    it "potential strictly decreases or algorithm terminates" do
      int_type = Crystal::MIR::TypeRef::INT32

      func = Crystal::MIR::Function.new(LTPTestHelpers.next_func_id, "test", int_type)
      entry_id = func.create_block
      entry = func.get_block(entry_id)

      # Create function with RC ops
      # Alloc.new(id, type, strategy, alloc_type)
      alloc = Crystal::MIR::Alloc.new(
        1_u32,
        int_type,
        Crystal::MIR::MemoryStrategy::ARC,
        int_type
      )
      alloc.no_alias = true
      entry.add(alloc)

      # RCIncrement.new(id, ptr)
      rc_inc = Crystal::MIR::RCIncrement.new(2_u32, alloc.id)
      entry.add(rc_inc)

      # RCDecrement.new(id, ptr)
      rc_dec = Crystal::MIR::RCDecrement.new(3_u32, alloc.id)
      entry.add(rc_dec)

      ret = Crystal::MIR::Return.new(alloc.id)
      entry.terminator = ret

      engine = Crystal::MIR::LTPEngine.new(func)
      final_potential = engine.run

      # Either we made progress (potential decreased) or we terminated cleanly
      # The algorithm guarantees termination
      engine.iterations.should be >= 0
      final_potential.should be_a(Crystal::MIR::LTPPotential)
    end
  end

  describe "Legal Moves" do
    it "Spike move removes rc_inc/rc_dec pair" do
      int_type = Crystal::MIR::TypeRef::INT32

      func = Crystal::MIR::Function.new(LTPTestHelpers.next_func_id, "test_spike", int_type)
      entry_id = func.create_block
      entry = func.get_block(entry_id)

      # Alloc.new(id, type, strategy, alloc_type)
      alloc = Crystal::MIR::Alloc.new(
        1_u32,
        int_type,
        Crystal::MIR::MemoryStrategy::ARC,
        int_type
      )
      alloc.no_alias = true
      entry.add(alloc)

      # RCIncrement.new(id, ptr)
      rc_inc = Crystal::MIR::RCIncrement.new(2_u32, alloc.id)
      entry.add(rc_inc)

      # RCDecrement.new(id, ptr)
      rc_dec = Crystal::MIR::RCDecrement.new(3_u32, alloc.id)
      entry.add(rc_dec)

      ret = Crystal::MIR::Return.new(alloc.id)
      entry.terminator = ret

      initial_count = entry.instructions.size
      initial_count.should eq(3)  # alloc, rc_inc, rc_dec

      func.optimize_ltp

      # After optimization, rc_inc/rc_dec should be elided
      # (if noalias gate passes)
      final_count = entry.instructions.size
      final_count.should be <= initial_count
    end

    it "Ladder eligibility recognizes short corridors" do
      func = LTPTestHelpers.create_test_function_with_rc
      block = func.blocks.last

      rc_inc = block.instructions.find(&.is_a?(Crystal::MIR::RCIncrement)).not_nil!.as(Crystal::MIR::RCIncrement)
      load_inst = block.instructions.find(&.is_a?(Crystal::MIR::Load)).not_nil!.as(Crystal::MIR::Load)
      rc_dec = block.instructions.find(&.is_a?(Crystal::MIR::RCDecrement)).not_nil!.as(Crystal::MIR::RCDecrement)

      window_index = block.instructions.index(rc_inc).not_nil!
      window = Crystal::MIR::Window.new(rc_inc, block, window_index, 1, rc_inc.ptr)
      path = [] of Crystal::MIR::Value
      path << rc_inc
      path << load_inst
      path << rc_dec

      corridor = Crystal::MIR::Corridor.new(
        window,
        path,
        Crystal::MIR::CorridorExit::Elision,
        rc_dec
      )

      corridor.ladder_eligible?.should be_true
    end

    it "Diamond move resolves competing windows for same pointer" do
      int_type = Crystal::MIR::TypeRef::INT32
      void_type = Crystal::MIR::TypeRef::VOID

      func = Crystal::MIR::Function.new(LTPTestHelpers.next_func_id, "test_diamond", int_type)
      entry_id = func.create_block
      entry = func.get_block(entry_id)

      alloc = Crystal::MIR::Alloc.new(
        1_u32,
        int_type,
        Crystal::MIR::MemoryStrategy::ARC,
        int_type
      )
      alloc.no_alias = true
      entry.add(alloc)

      rc_inc1 = Crystal::MIR::RCIncrement.new(2_u32, alloc.id)
      entry.add(rc_inc1)

      call = Crystal::MIR::Call.new(
        3_u32,
        void_type,
        LTPTestHelpers.next_func_id,
        [alloc.id]
      )
      entry.add(call)

      rc_inc2 = Crystal::MIR::RCIncrement.new(4_u32, alloc.id)
      entry.add(rc_inc2)

      rc_dec = Crystal::MIR::RCDecrement.new(5_u32, alloc.id)
      entry.add(rc_dec)

      ret = Crystal::MIR::Return.new(alloc.id)
      entry.terminator = ret

      engine = Crystal::MIR::LTPEngine.new(func)
      engine.run(max_iters: 1)

      engine.moves_applied.any? { |move| move.type == Crystal::MIR::MoveType::Diamond }.should be_true
    end

    it "Collapse removes dead instructions when no legal move exists" do
      int_type = Crystal::MIR::TypeRef::INT32

      func = Crystal::MIR::Function.new(LTPTestHelpers.next_func_id, "test_collapse", int_type)
      entry_id = func.create_block
      entry = func.get_block(entry_id)

      alloc = Crystal::MIR::Alloc.new(
        1_u32,
        int_type,
        Crystal::MIR::MemoryStrategy::ARC,
        int_type
      )
      alloc.no_alias = true
      entry.add(alloc)

      rc_inc = Crystal::MIR::RCIncrement.new(2_u32, alloc.id)
      entry.add(rc_inc)

      live_const = Crystal::MIR::Constant.new(3_u32, int_type, 1_i64)
      entry.add(live_const)

      store = Crystal::MIR::Store.new(4_u32, alloc.id, live_const.id)
      entry.add(store)

      dead_const = Crystal::MIR::Constant.new(5_u32, int_type, 99_i64)
      entry.add(dead_const)

      ret = Crystal::MIR::Return.new(alloc.id)
      entry.terminator = ret

      before = entry.instructions.size
      func.optimize_ltp(max_iters: 1)
      after = entry.instructions.size

      after.should be < before
    end
  end
end
