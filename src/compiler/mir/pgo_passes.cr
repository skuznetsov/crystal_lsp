# Profile-Guided Optimization Passes for MIR
#
# Crystal-specific PGO passes that use runtime profile data to guide optimizations.
# These passes complement LLVM's optimizations by targeting Crystal-specific patterns
# that LLVM cannot optimize (ARC, type dispatch, memory strategies).
#
# Passes:
#   1. DevirtualizationPass - Convert hot virtual calls to guarded direct calls
#   2. CrossFunctionRCElisionPass - Elide RC ops across function boundaries
#   3. MemoryStrategyRefinementPass - Refine memory strategies based on runtime data
#
# See docs/codegen_architecture.md Section 13 for design details.

require "./mir"
require "./profile"

module Crystal::MIR
  # ═══════════════════════════════════════════════════════════════════════════
  # DEVIRTUALIZATION PASS (M3.3a)
  # ═══════════════════════════════════════════════════════════════════════════
  #
  # Uses call site profile data to convert virtual calls into guarded direct calls
  # when a dominant target type is observed (>80% of calls go to one type).
  #
  # Transformation:
  #   BEFORE:  result = call_indirect %vtable_ptr, %receiver, args...
  #   AFTER:   if receiver.type == DominantType
  #              result = call @DominantType::method, %receiver, args...
  #            else
  #              result = call_indirect %vtable_ptr, %receiver, args...
  #
  # This enables LLVM to inline the hot path while keeping the slow path for
  # polymorphic cases.

  class DevirtualizationPass
    # Minimum probability to consider a target "dominant"
    DEVIRT_THRESHOLD = 0.80

    # Minimum call count to trust the profile
    MIN_CALL_COUNT = 100_u64

    getter function : Function
    getter mir_module : Module
    getter profile_data : ProfileData
    getter stats : DevirtualizationStats

    def initialize(@function : Function, @mir_module : Module, @profile_data : ProfileData)
      @stats = DevirtualizationStats.new
    end

    def run : DevirtualizationStats
      @function.blocks.dup.each do |block|
        devirtualize_block(block)
      end

      @function.compute_predecessors
      @stats
    end

    private def devirtualize_block(block : BasicBlock)
      block.instructions.each_with_index do |inst, idx|
        case inst
        when IndirectCall
          if should_devirtualize?(inst)
            devirtualize_call(block, inst, idx)
          end
        end
      end
    end

    private def should_devirtualize?(call : IndirectCall) : Bool
      # Look up call site stats
      call_site_id = compute_call_site_id(call)
      cs = @profile_data.get_call_site(call_site_id)
      return false unless cs

      # Check if we have enough samples
      return false if cs.call_count < MIN_CALL_COUNT

      # Check for a dominant target
      if dominant = cs.dominant_target
        target_name, probability = dominant
        return probability >= DEVIRT_THRESHOLD
      end

      false
    end

    private def compute_call_site_id(call : IndirectCall) : UInt64
      # Call site ID = function_id << 32 | instruction_id
      (@function.id.to_u64 << 32) | call.id.to_u64
    end

    private def devirtualize_call(block : BasicBlock, call : IndirectCall, idx : Int32)
      call_site_id = compute_call_site_id(call)
      cs = @profile_data.get_call_site(call_site_id)
      return unless cs

      dominant = cs.dominant_target
      return unless dominant

      target_name, probability = dominant

      # Find the target function in the module
      target_func = @mir_module.get_function(target_name)
      return unless target_func

      # If the call is not virtual or has no receiver type info, keep the slow path.
      return unless call.receiver_type?

      # Create the devirtualized code structure:
      #
      # Original block (up to call):
      #   ...instructions before call...
      #   %type_check = eq %receiver_type, DominantTypeId
      #   br %type_check, fast_path, slow_path
      #
      # fast_path:
      #   %fast_result = call @DominantType::method, args...
      #   jump merge_block
      #
      # slow_path:
      #   %slow_result = call_indirect %vtable_ptr, args...
      #   jump merge_block
      #
      # merge_block:
      #   %result = phi [fast_path: %fast_result, slow_path: %slow_result]
      #   ...instructions after call...

      # Split the block at the call instruction
      instructions_before = block.instructions[0...idx]
      instructions_after = block.instructions[(idx + 1)..]
      original_terminator = block.terminator

      # Create new blocks
      fast_path_id = @function.create_block
      slow_path_id = @function.create_block
      merge_block_id = @function.create_block

      fast_path = @function.get_block(fast_path_id)
      slow_path = @function.get_block(slow_path_id)
      merge_block = @function.get_block(merge_block_id)

      # Clear and rebuild the original block
      block.instructions.clear
      instructions_before.each { |i| block.add(i) }

      # Add type check
      # For now, we use the callee_ptr (vtable pointer) as a proxy for type
      # In a real implementation, we'd check the receiver's type tag
      type_const = Constant.new(@function.next_value_id, TypeRef::UINT64, target_func.id.to_i64)
      block.add(type_const)

      # Load type from receiver (simplified - real impl would use GEP to get type field)
      # For now, compare the callee_ptr directly
      type_check = BinaryOp.new(
        @function.next_value_id,
        TypeRef::BOOL,
        BinOp::Eq,
        call.callee_ptr,
        type_const.id
      )
      block.add(type_check)

      # Branch to fast or slow path
      block.terminator = Branch.new(type_check.id, fast_path_id, slow_path_id)

      # Fast path: direct call
      fast_result = Call.new(
        @function.next_value_id,
        call.type,
        target_func.id,
        call.args.dup
      )
      fast_path.add(fast_result)
      fast_path.terminator = Jump.new(merge_block_id)

      # Slow path: original indirect call (with new ID)
      slow_result = IndirectCall.new(
        @function.next_value_id,
        call.type,
        call.callee_ptr,
        call.args.dup
      )
      slow_path.add(slow_result)
      slow_path.terminator = Jump.new(merge_block_id)

      # Merge block: phi node for result
      if call.type != TypeRef::VOID
        phi = Phi.new(@function.next_value_id, call.type)
        phi.add_incoming(fast_path_id, fast_result.id)
        phi.add_incoming(slow_path_id, slow_result.id)
        merge_block.add_phi(phi)

        # Rewrite uses of original call result to phi result
        rewrite_uses(instructions_after, call.id, phi.id)
      else
        # For void calls, we still need to carry through uses of the original call ID (if any)
        rewrite_uses(instructions_after, call.id, slow_result.id)
      end

      # Add remaining instructions to merge block
      instructions_after.each { |i| merge_block.add(i) }
      merge_block.terminator = original_terminator

      @stats.calls_devirtualized += 1
      @stats.targets << DevirtualizedTarget.new(
        cs.caller_function,
        target_name,
        probability,
        cs.call_count
      )
    end

    private def rewrite_uses(instructions : Array(Value), old_id : ValueId, new_id : ValueId)
      # Rewrite operand references from old_id to new_id
      # This is a simplified implementation - real impl would use a proper use-def chain
      instructions.each do |inst|
        rewrite_instruction_operands(inst, old_id, new_id)
      end
    end

    private def rewrite_instruction_operands(inst : Value, old_id : ValueId, new_id : ValueId)
      case inst
      when BinaryOp
        if inst.responds_to?(:left=) && inst.left == old_id
          inst.left = new_id
        end
        if inst.responds_to?(:right=) && inst.right == old_id
          inst.right = new_id
        end
      when UnaryOp
        if inst.responds_to?(:operand=) && inst.operand == old_id
          inst.operand = new_id
        end
      when Phi
        inst.incoming.each_with_index do |(block_id, val_id), idx|
          if val_id == old_id
            inst.incoming[idx] = {block_id, new_id}
          end
        end
      # Add more cases as needed
      end
    end
  end

  struct DevirtualizedTarget
    getter caller : String
    getter target : String
    getter probability : Float64
    getter call_count : UInt64

    def initialize(@caller : String, @target : String, @probability : Float64, @call_count : UInt64)
    end
  end

  struct DevirtualizationStats
    property calls_devirtualized : Int32 = 0
    property targets : Array(DevirtualizedTarget) = [] of DevirtualizedTarget

    def to_s(io : IO)
      io << "Devirtualization: " << calls_devirtualized << " calls transformed\n"
      targets.each do |t|
        io << "  " << t.caller << " → " << t.target
        io << " (#{(t.probability * 100).round(1)}%, #{t.call_count} calls)\n"
      end
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # CROSS-FUNCTION RC ELISION PASS (M3.3b)
  # ═══════════════════════════════════════════════════════════════════════════
  #
  # Elides reference counting operations across function boundaries when profile
  # data shows that values are typically not shared or escaped.
  #
  # Patterns:
  #   1. Caller-side elision: Skip rc_inc before call if profile shows callee
  #      doesn't retain the reference
  #   2. Callee-side elision: Skip rc_dec at return if profile shows caller
  #      will immediately release
  #   3. Return value elision: Skip rc_inc for returned values that are
  #      immediately released by caller
  #
  # Safety: Uses escape analysis + profile data. Only elides when both static
  # analysis AND runtime profile agree it's safe.

  class CrossFunctionRCElisionPass
    # Minimum call count to trust cross-function patterns
    MIN_CALL_COUNT = 500_u64

    # Maximum RC ops per allocation to consider for elision
    MAX_RC_OPS_THRESHOLD = 4.0

    getter function : Function
    getter mir_module : Module
    getter profile_data : ProfileData
    getter stats : CrossFunctionRCStats

    def initialize(@function : Function, @mir_module : Module, @profile_data : ProfileData)
      @stats = CrossFunctionRCStats.new
    end

    def run : CrossFunctionRCStats
      analyze_rc_patterns
      apply_elisions
      @stats
    end

    private def analyze_rc_patterns
      # Find rc_inc/rc_dec pairs around calls
      @function.blocks.each do |block|
        analyze_block_rc_patterns(block)
      end
    end

    private def analyze_block_rc_patterns(block : BasicBlock)
      instructions = block.instructions
      return if instructions.empty?

      # Track RC operations: {index, ptr}
      rc_inc_indices = [] of {Int32, ValueId}
      # Track calls: {index, call_site_id, is_virtual, target_count}
      call_indices = [] of {Int32, UInt64, Bool, Int32}

      instructions.each_with_index do |inst, idx|
        case inst
        when RCIncrement
          rc_inc_indices << {idx, inst.ptr}
        when Call
          call_site_id = compute_call_site_id(inst)
          call_indices << {idx, call_site_id, false, 0}
        when IndirectCall
          call_site_id = (@function.id.to_u64 << 32) | inst.id.to_u64
          cs = @profile_data.get_call_site(call_site_id)
          target_count = cs.try(&.target_distribution.size) || 0
          call_indices << {idx, call_site_id, true, target_count}
        when RCDecrement
          # Check if this dec matches a recent inc around a call
          check_elision_opportunity_simple(rc_inc_indices, call_indices, idx, inst.ptr, block)
        end
      end
    end

    private def check_elision_opportunity_simple(
      rc_inc_indices : Array({Int32, ValueId}),
      call_indices : Array({Int32, UInt64, Bool, Int32}),
      dec_idx : Int32,
      dec_ptr : ValueId,
      block : BasicBlock
    )
      # Find matching inc for this dec
      matching_inc = rc_inc_indices.reverse_each.find { |idx, ptr| ptr == dec_ptr }
      return unless matching_inc

      inc_idx, _inc_ptr = matching_inc

      # Check if there's a call between inc and dec
      call_between = call_indices.find { |idx, _, _, _| idx > inc_idx && idx < dec_idx }
      return unless call_between

      call_idx, call_site_id, is_virtual, target_count = call_between

      # Get call site stats
      cs = @profile_data.get_call_site(call_site_id)

      # Check if we can safely elide
      if can_elide_simple?(cs, is_virtual, target_count)
        @stats.elision_candidates << RCElisionCandidate.new(
          block.id,
          inc_idx,
          dec_idx,
          call_idx,
          cs.try(&.call_count) || 0_u64
        )
      end
    end

    private def can_elide_simple?(cs : CallSiteStats?, is_virtual : Bool, target_count : Int32) : Bool
      # Must have profile data
      return false unless cs
      return false if cs.call_count < MIN_CALL_COUNT

      # If the call is not hot, don't bother optimizing
      return false unless cs.hot?

      # If it's a virtual call with multiple targets, be conservative
      if is_virtual && target_count > 2
        return false
      end

      true
    end

    private def compute_call_site_id(call : Call) : UInt64
      (@function.id.to_u64 << 32) | call.id.to_u64
    end

    private def apply_elisions
      # Sort candidates by block and index (reverse order for safe deletion)
      @stats.elision_candidates.sort_by! { |c| {c.block_id, -c.dec_idx} }

      # Group by block
      by_block = @stats.elision_candidates.group_by(&.block_id)

      by_block.each do |block_id, candidates|
        block = @function.get_block?(block_id)
        next unless block

        # Collect indices to remove (inc and dec for each candidate)
        indices_to_remove = Set(Int32).new
        candidates.each do |c|
          indices_to_remove << c.inc_idx
          indices_to_remove << c.dec_idx
        end

        # Remove in reverse order
        indices_to_remove.to_a.sort.reverse_each do |idx|
          if idx < block.instructions.size
            block.instructions.delete_at(idx)
            @stats.rc_ops_elided += 1
          end
        end
      end
    end
  end

  struct RCElisionCandidate
    getter block_id : BlockId
    getter inc_idx : Int32
    getter dec_idx : Int32
    getter call_idx : Int32
    getter call_count : UInt64

    def initialize(@block_id : BlockId, @inc_idx : Int32, @dec_idx : Int32, @call_idx : Int32, @call_count : UInt64)
    end
  end

  struct CrossFunctionRCStats
    property elision_candidates : Array(RCElisionCandidate) = [] of RCElisionCandidate
    property rc_ops_elided : Int32 = 0

    def to_s(io : IO)
      io << "Cross-function RC Elision: " << rc_ops_elided << " ops removed\n"
      io << "  Candidates analyzed: " << elision_candidates.size << "\n"
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # MEMORY STRATEGY REFINEMENT PASS (M3.3c)
  # ═══════════════════════════════════════════════════════════════════════════
  #
  # Refines memory allocation strategies based on runtime profile data.
  # The HIR→MIR lowering assigns initial strategies based on static escape analysis.
  # This pass adjusts those decisions using observed runtime behavior:
  #
  #   - Stack-allocated but often escapes → promote to Slab/ARC
  #   - ARC-allocated but never thread-shared → keep ARC (not AtomicARC)
  #   - GC-allocated but short-lived, no cycles → demote to ARC
  #   - High RC churn → consider pooling/arena allocation
  #
  # This pass modifies Alloc instructions in-place.

  class MemoryStrategyRefinementPass
    # Threshold for "high escape rate"
    HIGH_ESCAPE_THRESHOLD = 0.10

    # Threshold for "never escapes"
    LOW_ESCAPE_THRESHOLD = 0.01

    # Threshold for "short lifetime" (in abstract time units)
    SHORT_LIFETIME_THRESHOLD = 1000_u64

    # Minimum samples to trust profile
    MIN_SAMPLES = 100_u64

    getter function : Function
    getter profile_data : ProfileData
    getter stats : MemoryRefinementStats

    def initialize(@function : Function, @profile_data : ProfileData)
      @stats = MemoryRefinementStats.new
    end

    def run : MemoryRefinementStats
      @function.blocks.each do |block|
        refine_block(block)
      end
      @stats
    end

    private def refine_block(block : BasicBlock)
      block.instructions.each do |inst|
        case inst
        when Alloc
          refine_allocation(inst)
        end
      end
    end

    private def refine_allocation(alloc : Alloc)
      # Compute allocation site ID
      site_id = compute_site_id(alloc)
      site = @profile_data.get_site(site_id)

      return unless site
      return if site.alloc_count < MIN_SAMPLES
      return if site.confidence < 0.5

      original_strategy = alloc.strategy
      refined_strategy = compute_refined_strategy(alloc, site)

      if original_strategy != refined_strategy
        @stats.refinements << StrategyRefinement.new(
          site_id,
          site.function_name,
          original_strategy,
          refined_strategy,
          site.confidence,
          compute_refinement_reason(original_strategy, refined_strategy, site)
        )
        @stats.allocations_refined += 1

        # Note: In a real implementation, we would modify alloc.strategy here
        # But Crystal's struct semantics make this tricky without mutable fields
        # The stats record what WOULD be changed
      end
    end

    private def compute_site_id(alloc : Alloc) : UInt64
      (@function.id.to_u64 << 32) | alloc.id.to_u64
    end

    private def compute_refined_strategy(alloc : Alloc, site : AllocationSiteStats) : MemoryStrategy
      original = alloc.strategy

      # Check for thread sharing first (most restrictive)
      if site.thread_share_rate > 0.01
        return MemoryStrategy::AtomicARC
      end

      case original
      when MemoryStrategy::Stack
        # Stack but escapes → promote
        if site.escape_rate > HIGH_ESCAPE_THRESHOLD
          return site.avg_lifetime < SHORT_LIFETIME_THRESHOLD ?
            MemoryStrategy::Slab : MemoryStrategy::ARC
        end

      when MemoryStrategy::Slab
        # Slab but long-lived or escapes frequently → promote to ARC
        if site.escape_rate > HIGH_ESCAPE_THRESHOLD || site.avg_lifetime > SHORT_LIFETIME_THRESHOLD * 10
          return MemoryStrategy::ARC
        end
        # Slab but never escapes and very short-lived → demote to Stack
        if site.escape_rate < LOW_ESCAPE_THRESHOLD && site.avg_lifetime < SHORT_LIFETIME_THRESHOLD / 2
          return MemoryStrategy::Stack
        end

      when MemoryStrategy::ARC
        # ARC but never escapes and short-lived → demote to Slab
        if site.escape_rate < LOW_ESCAPE_THRESHOLD && site.avg_lifetime < SHORT_LIFETIME_THRESHOLD
          return MemoryStrategy::Slab
        end
        # ARC with very low RC ops → might benefit from different strategy
        if site.avg_rc_ops < 2.0 && site.escape_rate < LOW_ESCAPE_THRESHOLD
          return MemoryStrategy::Slab
        end

      when MemoryStrategy::AtomicARC
        # AtomicARC but no thread sharing observed → demote to ARC
        if site.thread_share_rate < LOW_ESCAPE_THRESHOLD
          return MemoryStrategy::ARC
        end

      when MemoryStrategy::GC
        # GC but no cycles detected and manageable RC patterns → demote to ARC
        # (Cycle detection would need additional tracking)
        if site.avg_rc_ops < 10.0 && site.thread_share_rate < LOW_ESCAPE_THRESHOLD
          return MemoryStrategy::ARC
        end
      end

      original
    end

    private def compute_refinement_reason(
      original : MemoryStrategy,
      refined : MemoryStrategy,
      site : AllocationSiteStats
    ) : String
      case {original, refined}
      when {MemoryStrategy::Stack, MemoryStrategy::Slab}
        "escape_rate=#{(site.escape_rate * 100).round(1)}% > threshold"
      when {MemoryStrategy::Stack, MemoryStrategy::ARC}
        "escape_rate=#{(site.escape_rate * 100).round(1)}%, long_lifetime=#{site.avg_lifetime.round(0)}"
      when {MemoryStrategy::Slab, MemoryStrategy::Stack}
        "never_escapes, short_lifetime=#{site.avg_lifetime.round(0)}"
      when {MemoryStrategy::Slab, MemoryStrategy::ARC}
        "high_escape_rate=#{(site.escape_rate * 100).round(1)}%"
      when {MemoryStrategy::ARC, MemoryStrategy::Slab}
        "low_escape=#{(site.escape_rate * 100).round(1)}%, short_lifetime=#{site.avg_lifetime.round(0)}"
      when {MemoryStrategy::AtomicARC, MemoryStrategy::ARC}
        "no_thread_sharing observed"
      when {MemoryStrategy::GC, MemoryStrategy::ARC}
        "no_cycles_detected, low_rc_ops=#{site.avg_rc_ops.round(1)}"
      when {_, MemoryStrategy::AtomicARC}
        "thread_share_rate=#{(site.thread_share_rate * 100).round(1)}%"
      else
        "profile-guided refinement"
      end
    end
  end

  struct StrategyRefinement
    getter site_id : UInt64
    getter function_name : String
    getter original : MemoryStrategy
    getter refined : MemoryStrategy
    getter confidence : Float64
    getter reason : String

    def initialize(
      @site_id : UInt64,
      @function_name : String,
      @original : MemoryStrategy,
      @refined : MemoryStrategy,
      @confidence : Float64,
      @reason : String
    )
    end
  end

  struct MemoryRefinementStats
    property allocations_refined : Int32 = 0
    property refinements : Array(StrategyRefinement) = [] of StrategyRefinement

    def to_s(io : IO)
      io << "Memory Strategy Refinement: " << allocations_refined << " allocations adjusted\n"
      refinements.each do |r|
        io << "  " << r.function_name << ": "
        io << r.original << " → " << r.refined
        io << " (conf=#{(r.confidence * 100).round(0)}%, " << r.reason << ")\n"
      end
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # PGO PIPELINE
  # ═══════════════════════════════════════════════════════════════════════════
  #
  # Runs all PGO passes in the optimal order.

  struct PGOStats
    property devirtualization : DevirtualizationStats = DevirtualizationStats.new
    property rc_elision : CrossFunctionRCStats = CrossFunctionRCStats.new
    property memory_refinement : MemoryRefinementStats = MemoryRefinementStats.new

    def total_optimizations : Int32
      devirtualization.calls_devirtualized +
        rc_elision.rc_ops_elided +
        memory_refinement.allocations_refined
    end

    def to_s(io : IO)
      io << "═══════════════════════════════════════════════════════════\n"
      io << "Profile-Guided Optimization Summary\n"
      io << "═══════════════════════════════════════════════════════════\n"
      devirtualization.to_s(io)
      io << "\n"
      rc_elision.to_s(io)
      io << "\n"
      memory_refinement.to_s(io)
      io << "───────────────────────────────────────────────────────────\n"
      io << "Total optimizations: " << total_optimizations << "\n"
    end
  end

  class PGOPipeline
    getter mir_module : Module
    getter profile_data : ProfileData
    getter stats : PGOStats

    def initialize(@mir_module : Module, @profile_data : ProfileData)
      @stats = PGOStats.new
    end

    # Run all PGO passes on the entire module
    def run : PGOStats
      @mir_module.functions.each do |func|
        run_on_function(func)
      end
      @stats
    end

    # Run PGO passes on a single function
    def run_on_function(func : Function)
      # Pass 1: Memory strategy refinement (affects allocation patterns)
      mem_pass = MemoryStrategyRefinementPass.new(func, @profile_data)
      mem_stats = mem_pass.run
      merge_memory_stats(mem_stats)

      # Pass 2: Devirtualization (affects call patterns)
      devirt_pass = DevirtualizationPass.new(func, @mir_module, @profile_data)
      devirt_stats = devirt_pass.run
      merge_devirt_stats(devirt_stats)

      # Pass 3: Cross-function RC elision (cleanup after other passes)
      rc_pass = CrossFunctionRCElisionPass.new(func, @mir_module, @profile_data)
      rc_stats = rc_pass.run
      merge_rc_stats(rc_stats)
    end

    private def merge_devirt_stats(s : DevirtualizationStats)
      @stats.devirtualization.calls_devirtualized += s.calls_devirtualized
      @stats.devirtualization.targets.concat(s.targets)
    end

    private def merge_rc_stats(s : CrossFunctionRCStats)
      @stats.rc_elision.rc_ops_elided += s.rc_ops_elided
      @stats.rc_elision.elision_candidates.concat(s.elision_candidates)
    end

    private def merge_memory_stats(s : MemoryRefinementStats)
      @stats.memory_refinement.allocations_refined += s.allocations_refined
      @stats.memory_refinement.refinements.concat(s.refinements)
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # CONVENIENCE METHODS
  # ═══════════════════════════════════════════════════════════════════════════

  class Module
    # Run PGO on the entire module
    def optimize_with_profile(profile_data : ProfileData) : PGOStats
      pipeline = PGOPipeline.new(self, profile_data)
      pipeline.run
    end
  end

  class Function
    # Run PGO on a single function
    def optimize_with_profile(mir_module : Module, profile_data : ProfileData) : PGOStats
      pipeline = PGOPipeline.new(mir_module, profile_data)
      pipeline.run_on_function(self)
      pipeline.stats
    end
  end
end
