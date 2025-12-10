require "../spec_helper"
require "../../src/compiler/mir/mir"
require "../../src/compiler/mir/profile"

describe Crystal::MIR do
  # ═══════════════════════════════════════════════════════════════════════════
  # ALLOCATION SITE STATS
  # ═══════════════════════════════════════════════════════════════════════════

  describe "AllocationSiteStats" do
    it "calculates average lifetime" do
      stats = Crystal::MIR::AllocationSiteStats.new
      stats.alloc_count = 100
      stats.total_lifetime = 5000

      stats.avg_lifetime.should eq(50.0)
    end

    it "calculates escape rate" do
      stats = Crystal::MIR::AllocationSiteStats.new
      stats.alloc_count = 100
      stats.escape_count = 25

      stats.escape_rate.should eq(0.25)
    end

    it "handles zero allocations" do
      stats = Crystal::MIR::AllocationSiteStats.new
      stats.avg_lifetime.should eq(0.0)
      stats.escape_rate.should eq(0.0)
      stats.thread_share_rate.should eq(0.0)
    end

    it "recommends Stack for non-escaping short-lived allocations" do
      stats = Crystal::MIR::AllocationSiteStats.new
      stats.alloc_count = 1000
      stats.escape_count = 5        # 0.5% escape rate
      stats.total_lifetime = 500000  # avg 500 ticks

      stats.recommended_strategy.should eq(Crystal::MIR::MemoryStrategy::Stack)
    end

    it "recommends AtomicARC for thread-shared allocations" do
      stats = Crystal::MIR::AllocationSiteStats.new
      stats.alloc_count = 1000
      stats.thread_share_count = 200  # 20% thread sharing

      stats.recommended_strategy.should eq(Crystal::MIR::MemoryStrategy::AtomicARC)
    end

    it "recommends Slab for low-escape moderate-lifetime allocations" do
      stats = Crystal::MIR::AllocationSiteStats.new
      stats.alloc_count = 1000
      stats.escape_count = 20       # 2% escape rate
      stats.total_lifetime = 5000000  # avg 5000 ticks

      stats.recommended_strategy.should eq(Crystal::MIR::MemoryStrategy::Slab)
    end

    it "calculates confidence based on sample count" do
      stats = Crystal::MIR::AllocationSiteStats.new

      stats.alloc_count = 100
      stats.confidence.should eq(0.1)

      stats.alloc_count = 500
      stats.confidence.should eq(0.5)

      stats.alloc_count = 1000
      stats.confidence.should eq(1.0)

      stats.alloc_count = 5000
      stats.confidence.should eq(1.0)  # Capped at 1.0
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # BRANCH STATS
  # ═══════════════════════════════════════════════════════════════════════════

  describe "BranchStats" do
    it "calculates taken probability" do
      b = Crystal::MIR::BranchStats.new
      b.taken_count = 80
      b.not_taken_count = 20

      b.taken_probability.should be_close(0.8, 0.001)
      b.not_taken_probability.should be_close(0.2, 0.001)
    end

    it "detects biased branches" do
      b = Crystal::MIR::BranchStats.new
      b.taken_count = 900
      b.not_taken_count = 100

      b.biased?.should be_true
      b.hot_path_is_taken?.should be_true
    end

    it "handles zero executions" do
      b = Crystal::MIR::BranchStats.new
      b.taken_probability.should eq(0.5)
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # LOOP STATS
  # ═══════════════════════════════════════════════════════════════════════════

  describe "LoopStats" do
    it "calculates average trip count" do
      l = Crystal::MIR::LoopStats.new
      l.entry_count = 100
      l.iteration_count = 800

      l.avg_trip_count.should eq(8.0)
    end

    it "recommends unrolling for small consistent loops" do
      l = Crystal::MIR::LoopStats.new
      l.entry_count = 1000
      l.iteration_count = 4000  # avg 4
      l.max_trip_count = 8

      l.should_unroll?.should be_true
      l.suggested_unroll_factor.should eq(2)
    end

    it "does not recommend unrolling for large loops" do
      l = Crystal::MIR::LoopStats.new
      l.entry_count = 1000
      l.iteration_count = 100000  # avg 100
      l.max_trip_count = 500

      l.should_unroll?.should be_false
      l.suggested_unroll_factor.should eq(0)
    end

    it "identifies hot loops" do
      l = Crystal::MIR::LoopStats.new
      l.entry_count = 5000
      l.iteration_count = 50000

      l.hot?.should be_true
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # CALL SITE STATS
  # ═══════════════════════════════════════════════════════════════════════════

  describe "CallSiteStats" do
    it "identifies hot call sites" do
      c = Crystal::MIR::CallSiteStats.new
      c.call_count = 5000

      c.hot?.should be_true
    end

    it "finds dominant target for virtual calls" do
      c = Crystal::MIR::CallSiteStats.new
      c.is_virtual = true
      c.target_distribution["TypeA"] = 950
      c.target_distribution["TypeB"] = 50

      dominant = c.dominant_target
      dominant.should_not be_nil
      dominant.not_nil![0].should eq("TypeA")
      dominant.not_nil![1].should be_close(0.95, 0.01)
    end

    it "returns nil when no dominant target" do
      c = Crystal::MIR::CallSiteStats.new
      c.is_virtual = true
      c.target_distribution["TypeA"] = 400
      c.target_distribution["TypeB"] = 350
      c.target_distribution["TypeC"] = 250

      c.dominant_target.should be_nil
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # PROFILE DATA
  # ═══════════════════════════════════════════════════════════════════════════

  describe "ProfileData" do
    it "creates and tracks allocation sites" do
      profile = Crystal::MIR::ProfileData.new
      site = profile.add_site(1_u64, "test_function", "test.cr", 42_u32)

      site.site_id.should eq(1_u64)
      site.function_name.should eq("test_function")
      site.source_file.should eq("test.cr")
      site.source_line.should eq(42_u32)
    end

    it "records allocations" do
      profile = Crystal::MIR::ProfileData.new
      profile.add_site(1_u64, "test")

      profile.record_allocation(1_u64)
      profile.record_allocation(1_u64)
      profile.record_allocation(1_u64)

      site = profile.get_site(1_u64).not_nil!
      site.alloc_count.should eq(3)
      site.current_live.should eq(3)
      site.max_live.should eq(3)
      profile.total_allocations.should eq(3)
    end

    it "records deallocations with lifetime" do
      profile = Crystal::MIR::ProfileData.new
      profile.add_site(1_u64, "test")

      profile.record_allocation(1_u64)
      profile.record_allocation(1_u64)
      profile.record_deallocation(1_u64, 100_u64)
      profile.record_deallocation(1_u64, 200_u64)

      site = profile.get_site(1_u64).not_nil!
      site.current_live.should eq(0)
      site.total_lifetime.should eq(300)
      site.max_lifetime.should eq(200)
      site.min_lifetime.should eq(100)
    end

    it "records escape events" do
      profile = Crystal::MIR::ProfileData.new
      profile.add_site(1_u64, "test")

      profile.record_escape(1_u64, Crystal::MIR::EscapeKind::ReturnEscape)
      profile.record_escape(1_u64, Crystal::MIR::EscapeKind::ClosureEscape)
      profile.record_escape(1_u64, Crystal::MIR::EscapeKind::ContainerEscape)

      site = profile.get_site(1_u64).not_nil!
      site.escape_count.should eq(3)
      site.return_escape_count.should eq(1)
      site.closure_escape_count.should eq(1)
      site.container_escape_count.should eq(1)
    end

    it "records thread sharing" do
      profile = Crystal::MIR::ProfileData.new
      profile.add_site(1_u64, "test")

      profile.record_thread_share(1_u64)
      profile.record_thread_share(1_u64)

      site = profile.get_site(1_u64).not_nil!
      site.thread_share_count.should eq(2)
    end

    it "records RC operations" do
      profile = Crystal::MIR::ProfileData.new
      profile.add_site(1_u64, "test")

      profile.record_rc_inc(1_u64)
      profile.record_rc_inc(1_u64)
      profile.record_rc_dec(1_u64)

      site = profile.get_site(1_u64).not_nil!
      site.total_rc_inc.should eq(2)
      site.total_rc_dec.should eq(1)
    end

    it "merges profile data" do
      profile1 = Crystal::MIR::ProfileData.new
      profile1.add_site(1_u64, "func1")
      profile1.record_allocation(1_u64)
      profile1.record_allocation(1_u64)

      profile2 = Crystal::MIR::ProfileData.new
      profile2.add_site(1_u64, "func1")
      profile2.record_allocation(1_u64)
      profile2.add_site(2_u64, "func2")
      profile2.record_allocation(2_u64)

      profile1.merge(profile2)

      profile1.get_site(1_u64).not_nil!.alloc_count.should eq(3)
      profile1.get_site(2_u64).should_not be_nil
      profile1.total_allocations.should eq(4)
    end

    it "generates summary report" do
      profile = Crystal::MIR::ProfileData.new
      profile.add_site(1_u64, "hot_function", "hot.cr", 10_u32)
      1000.times { profile.record_allocation(1_u64) }

      summary = profile.summary
      summary.should contain("Profile Summary")
      summary.should contain("hot_function")
      summary.should contain("1000")
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # SERIALIZATION
  # ═══════════════════════════════════════════════════════════════════════════

  describe "ProfileData serialization" do
    it "round-trips through binary format" do
      profile = Crystal::MIR::ProfileData.new
      profile.add_site(1_u64, "test_func", "test.cr", 42_u32)
      profile.record_allocation(1_u64)
      profile.record_allocation(1_u64)
      profile.record_escape(1_u64, Crystal::MIR::EscapeKind::ReturnEscape)
      profile.finalize_run

      # Serialize
      io = IO::Memory.new
      profile.to_binary(io)

      # Deserialize
      io.rewind
      loaded = Crystal::MIR::ProfileData.from_binary(io)

      loaded.should_not be_nil
      loaded = loaded.not_nil!

      loaded.total_allocations.should eq(2)
      loaded.profile_runs.should eq(1)

      site = loaded.get_site(1_u64)
      site.should_not be_nil
      site = site.not_nil!
      site.function_name.should eq("test_func")
      site.source_file.should eq("test.cr")
      site.source_line.should eq(42_u32)
      site.alloc_count.should eq(2)
      site.escape_count.should eq(1)
    end

    it "handles empty profile" do
      profile = Crystal::MIR::ProfileData.new

      io = IO::Memory.new
      profile.to_binary(io)

      io.rewind
      loaded = Crystal::MIR::ProfileData.from_binary(io)

      loaded.should_not be_nil
      loaded.not_nil!.sites.should be_empty
    end

    it "returns nil for invalid magic" do
      io = IO::Memory.new
      io.write("XXXX".to_slice)
      io.rewind

      Crystal::MIR::ProfileData.from_binary(io).should be_nil
    end

    it "round-trips branch statistics" do
      profile = Crystal::MIR::ProfileData.new
      profile.add_branch(100_u64, "test_func", 5_u32)
      profile.record_branch_taken(100_u64)
      profile.record_branch_taken(100_u64)
      profile.record_branch_not_taken(100_u64)

      io = IO::Memory.new
      profile.to_binary(io)
      io.rewind

      loaded = Crystal::MIR::ProfileData.from_binary(io).not_nil!
      loaded.branches.size.should eq(1)

      b = loaded.get_branch(100_u64).not_nil!
      b.function_name.should eq("test_func")
      b.block_id.should eq(5_u32)
      b.taken_count.should eq(2)
      b.not_taken_count.should eq(1)
    end

    it "round-trips loop statistics" do
      profile = Crystal::MIR::ProfileData.new
      profile.add_loop(200_u64, "loop_func", 10_u32)
      profile.record_loop_entry(200_u64)
      5.times { profile.record_loop_iteration(200_u64) }
      profile.record_loop_exit(200_u64, 5_u64)

      io = IO::Memory.new
      profile.to_binary(io)
      io.rewind

      loaded = Crystal::MIR::ProfileData.from_binary(io).not_nil!
      loaded.loops.size.should eq(1)

      l = loaded.get_loop(200_u64).not_nil!
      l.function_name.should eq("loop_func")
      l.header_block_id.should eq(10_u32)
      l.entry_count.should eq(1)
      l.iteration_count.should eq(5)
      l.exit_count.should eq(1)
      l.trip_counts[5_u64].should eq(1)
    end

    it "round-trips block statistics" do
      profile = Crystal::MIR::ProfileData.new
      profile.add_block(300_u64, "block_func")
      100.times { profile.record_block_execution(300_u64) }

      io = IO::Memory.new
      profile.to_binary(io)
      io.rewind

      loaded = Crystal::MIR::ProfileData.from_binary(io).not_nil!
      loaded.blocks.size.should eq(1)

      b = loaded.blocks[300_u64]
      b.function_name.should eq("block_func")
      b.execution_count.should eq(100)
    end

    it "round-trips call site statistics" do
      profile = Crystal::MIR::ProfileData.new
      profile.add_call_site(400_u64, "caller", "callee", is_virtual: true)
      profile.record_call(400_u64, "ConcreteType1", 50_u64)
      profile.record_call(400_u64, "ConcreteType1", 60_u64)
      profile.record_call(400_u64, "ConcreteType2", 70_u64)

      io = IO::Memory.new
      profile.to_binary(io)
      io.rewind

      loaded = Crystal::MIR::ProfileData.from_binary(io).not_nil!
      loaded.call_sites.size.should eq(1)

      c = loaded.get_call_site(400_u64).not_nil!
      c.caller_function.should eq("caller")
      c.callee_function.should eq("callee")
      c.is_virtual.should be_true
      c.call_count.should eq(3)
      c.total_cycles.should eq(180)
      c.target_distribution["ConcreteType1"].should eq(2)
      c.target_distribution["ConcreteType2"].should eq(1)
    end

    it "round-trips complete profile with all stat types" do
      profile = Crystal::MIR::ProfileData.new

      # Add sites
      profile.add_site(1_u64, "alloc_func")
      profile.record_allocation(1_u64)

      # Add branches
      profile.add_branch(2_u64, "branch_func", 1_u32)
      profile.record_branch_taken(2_u64)

      # Add loops
      profile.add_loop(3_u64, "loop_func", 2_u32)
      profile.record_loop_entry(3_u64)

      # Add blocks
      profile.add_block(4_u64, "block_func")
      profile.record_block_execution(4_u64)

      # Add call sites
      profile.add_call_site(5_u64, "caller", "callee")
      profile.record_call(5_u64)

      profile.finalize_run

      io = IO::Memory.new
      profile.to_binary(io)
      io.rewind

      loaded = Crystal::MIR::ProfileData.from_binary(io).not_nil!

      loaded.sites.size.should eq(1)
      loaded.branches.size.should eq(1)
      loaded.loops.size.should eq(1)
      loaded.blocks.size.should eq(1)
      loaded.call_sites.size.should eq(1)
      loaded.profile_runs.should eq(1)
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # COMPILER FLAGS
  # ═══════════════════════════════════════════════════════════════════════════

  describe "CompilerFlags" do
    it "parses --mm=conservative" do
      flags = Crystal::MIR::CompilerFlags.from_args(["--mm=conservative"])
      flags.mm_mode.should eq("conservative")
    end

    it "parses --mm=profile-gen" do
      flags = Crystal::MIR::CompilerFlags.from_args(["--mm=profile-gen"])
      flags.profile_mode.should eq(Crystal::MIR::ProfileMode::Generate)
      flags.should_instrument?.should be_true
      flags.should_use_profile?.should be_false
    end

    it "parses --mm=profile-use" do
      flags = Crystal::MIR::CompilerFlags.from_args(["--mm=profile-use"])
      flags.profile_mode.should eq(Crystal::MIR::ProfileMode::Use)
      flags.should_instrument?.should be_false
      flags.should_use_profile?.should be_true
    end

    it "parses --mm=profile (both)" do
      flags = Crystal::MIR::CompilerFlags.from_args(["--mm=profile"])
      flags.profile_mode.should eq(Crystal::MIR::ProfileMode::Both)
      flags.should_instrument?.should be_true
      flags.should_use_profile?.should be_true
    end

    it "parses --profile-path" do
      flags = Crystal::MIR::CompilerFlags.from_args(["--profile-path=/tmp/my.profile"])
      flags.profile_path.should eq("/tmp/my.profile")
    end

    it "handles multiple flags" do
      flags = Crystal::MIR::CompilerFlags.from_args([
        "--mm=aggressive",
        "--mm=profile-gen",
        "--profile-path=custom.profile",
      ])
      flags.mm_mode.should eq("aggressive")
      flags.profile_mode.should eq(Crystal::MIR::ProfileMode::Generate)
      flags.profile_path.should eq("custom.profile")
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # PROFILE-GUIDED OPTIMIZER
  # ═══════════════════════════════════════════════════════════════════════════

  describe "ProfileGuidedOptimizer" do
    it "uses profile data for strategy recommendations" do
      profile = Crystal::MIR::ProfileData.new

      # Site with high escape rate
      profile.add_site(1_u64, "escaping_func")
      site1 = profile.get_site(1_u64).not_nil!
      site1.alloc_count = 1000
      site1.escape_count = 900  # 90% escape

      # Site with no escapes
      profile.add_site(2_u64, "local_func")
      site2 = profile.get_site(2_u64).not_nil!
      site2.alloc_count = 1000
      site2.escape_count = 5
      site2.total_lifetime = 500000  # avg 500

      optimizer = Crystal::MIR::ProfileGuidedOptimizer.new(profile)

      # High escape → GC (default fallback for complex cases)
      recommended1 = optimizer.recommended_strategy(1_u64, Crystal::MIR::MemoryStrategy::GC)
      recommended1.should eq(Crystal::MIR::MemoryStrategy::GC)

      # Low escape, short lifetime → Stack
      recommended2 = optimizer.recommended_strategy(2_u64, Crystal::MIR::MemoryStrategy::GC)
      recommended2.should eq(Crystal::MIR::MemoryStrategy::Stack)
    end

    it "uses default for insufficient samples" do
      profile = Crystal::MIR::ProfileData.new
      profile.add_site(1_u64, "test")
      site = profile.get_site(1_u64).not_nil!
      site.alloc_count = 10  # Too few samples

      optimizer = Crystal::MIR::ProfileGuidedOptimizer.new(profile)
      optimizer.recommended_strategy(1_u64, Crystal::MIR::MemoryStrategy::ARC).should eq(Crystal::MIR::MemoryStrategy::ARC)
    end

    it "generates optimization summary" do
      profile = Crystal::MIR::ProfileData.new
      profile.add_site(1_u64, "func1")
      profile.get_site(1_u64).not_nil!.alloc_count = 1000

      optimizer = Crystal::MIR::ProfileGuidedOptimizer.new(profile)
      summary = optimizer.optimization_summary

      summary.should contain("Profile-Guided Optimization Summary")
      summary.should contain("High confidence sites:")
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # INSTRUMENTATION
  # ═══════════════════════════════════════════════════════════════════════════

  describe "ProfileInstrumentationPass" do
    it "instruments allocations" do
      mod = Crystal::MIR::Module.new
      func = mod.create_function("test", Crystal::MIR::TypeRef::VOID)
      builder = Crystal::MIR::Builder.new(func)

      builder.alloc(Crystal::MIR::MemoryStrategy::ARC, Crystal::MIR::TypeRef.new(100_u32))
      builder.alloc(Crystal::MIR::MemoryStrategy::GC, Crystal::MIR::TypeRef.new(101_u32))
      builder.ret(nil)

      profile = Crystal::MIR::ProfileData.new
      instrumented = func.instrument_for_profile(profile)

      instrumented.should eq(2)

      # Should have profile instructions before each alloc
      block = func.get_block(func.entry_block)
      block.instructions[0].should be_a(Crystal::MIR::ProfileInstrument)
      block.instructions[1].should be_a(Crystal::MIR::Alloc)
      block.instructions[2].should be_a(Crystal::MIR::ProfileInstrument)
      block.instructions[3].should be_a(Crystal::MIR::Alloc)
    end

    it "registers sites in profile data" do
      mod = Crystal::MIR::Module.new
      func = mod.create_function("my_function", Crystal::MIR::TypeRef::VOID)
      builder = Crystal::MIR::Builder.new(func)

      builder.alloc(Crystal::MIR::MemoryStrategy::Stack, Crystal::MIR::TypeRef::INT32)
      builder.ret(nil)

      profile = Crystal::MIR::ProfileData.new
      func.instrument_for_profile(profile)

      # Site should be registered
      profile.sites.size.should eq(1)
      site = profile.sites.values.first
      site.function_name.should eq("my_function")
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # PROFILE INSTRUCTION
  # ═══════════════════════════════════════════════════════════════════════════

  describe "ProfileInstrument" do
    it "represents profile instrumentation call" do
      inst = Crystal::MIR::ProfileInstrument.new(
        0_u32,
        Crystal::MIR::ProfileInstrument::Kind::Alloc,
        12345_u64
      )

      inst.kind.should eq(Crystal::MIR::ProfileInstrument::Kind::Alloc)
      inst.site_id.should eq(12345_u64)
      inst.type.should eq(Crystal::MIR::TypeRef::VOID)
    end

    it "prints correctly" do
      inst = Crystal::MIR::ProfileInstrument.new(
        5_u32,
        Crystal::MIR::ProfileInstrument::Kind::Escape,
        999_u64
      )

      inst.to_s.should contain("profile.escape")
      inst.to_s.should contain("site=999")
    end
  end
end
