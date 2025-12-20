# MIR Optimizations
#
# Crystal-specific and standard SSA optimizations that run before LLVM lowering.
# These optimizations target patterns that LLVM doesn't understand (like ARC)
# and simplify the IR to reduce LLVM's workload.
#
# Passes:
#   1. RC Elision - Remove redundant rc_inc/rc_dec pairs
#   2. Dead Code Elimination - Remove unused instructions
#   3. Constant Folding - Evaluate constant expressions at compile time
#   4. Copy Propagation - Replace copies with their sources
#
# LTP/WBA Framework (Local Trigger → Transport → Potential):
#   A unifying descent framework based on the theory from:
#   "LTP/WBA Framework — From G_{3,5} to Kakeya & Magnus"
#
#   Bedrock Axioms:
#     BR-1 (Trigger): Every non-optimal config admits a detectable local window W
#     BR-2 (Transport): From W starts a corridor that exits boundary or triggers alt frame
#     BR-3 (Potential): Lexicographic Φ strictly decreases under every legal move
#     BR-4 (Dual frame): If progress stalls, switch to alternative analysis
#     BR-5 (Finiteness): No infinite descending chains; process terminates
#
#   Legal Moves: Spike / Ladder / Diamond / Collapse
#   Priority: S ≻ L ≻ D ≻ C
#
# Structural NoAlias Analysis:
#   Instead of computing "may alias", we track "provably no-alias" conditions:
#
#   NoAlias(a, b) :=
#     different_allocation_site(a, b) ∧
#     ¬escaped_to_field(a) ∧ ¬escaped_to_field(b) ∧
#     ¬escaped_to_container(a) ∧ ¬escaped_to_container(b)
#
#   Key insight: In Crystal ARC patterns, most allocations are used locally,
#   have single owners, and are distinct types. Track these properties
#   POSITIVELY (no_alias = true) and INVALIDATE on escape.

require "./mir"

module Crystal::MIR
  # ═══════════════════════════════════════════════════════════════════════════
  # RC ELISION
  # ═══════════════════════════════════════════════════════════════════════════
  #
  # Removes redundant reference counting operations:
  #   - rc_inc(x); rc_dec(x) → remove both (cancel out)
  #   - rc_inc(x); use(x); rc_dec(x) → remove if x doesn't escape in between
  #   - Multiple rc_inc for same use → combine

  class RCElisionPass
    getter function : Function
    getter eliminated : Int32 = 0
    # Track must-alias relationships (ptr1, ptr2) within a block
    getter must_alias : Set(Tuple(ValueId, ValueId))

    def initialize(@function : Function)
      @must_alias = Set(Tuple(ValueId, ValueId)).new
    end

    def run : Int32
      @eliminated = 0
      @function.blocks.each do |block|
        optimize_block(block)
      end

      @eliminated
    end

    private def optimize_block(block : BasicBlock)
      # Track rc_inc operations by pointer
      pending_incs = {} of ValueId => Array(Int32)  # ptr → instruction indices
      alias_map = {} of ValueId => ValueId          # simple copy-based alias map
      no_alias_ids = Set(ValueId).new
      # Track which allocation sites have "escaped" (stored to field/container)
      escaped_allocs = Set(ValueId).new
      # Track allocation site for each pointer (for structural noalias)
      alloc_site = {} of ValueId => ValueId  # ptr → original alloc id
      # TBAA: Track alloc_type for each allocation site
      alloc_type = {} of ValueId => TypeRef  # alloc_site → type of allocated object

      instructions = block.instructions
      to_remove = Set(Int32).new

      # Pre-scan for noalias-producing instructions and build alloc_site map
      instructions.each do |inst|
        case inst
        when Alloc
          if inst.no_alias
            no_alias_ids << inst.id
            alloc_site[inst.id] = inst.id  # Alloc is its own allocation site
            alloc_type[inst.id] = inst.alloc_type  # TBAA: track allocated type
          end
        when Load
          if inst.responds_to?(:no_alias) && inst.no_alias
            no_alias_ids << inst.id
          end
        end
      end

      instructions.each_with_index do |inst, idx|
        case inst
        when RCIncrement
          # Track this inc
          ptr = canonical_ptr(inst.ptr, alias_map)
          next unless no_alias_ids.includes?(ptr)
          # Skip if this allocation has escaped (stored to field/container)
          next if escaped_allocs.includes?(ptr)
          (pending_incs[ptr] ||= [] of Int32) << idx
          # Add a conservative MustAlias marker for identical ptrs within block
          @must_alias << {ptr, ptr}

        when RCDecrement
          # Check if we can elide with a pending inc
          ptr = canonical_ptr(inst.ptr, alias_map)
          next unless no_alias_ids.includes?(ptr)
          if incs = pending_incs[ptr]?
            if !incs.empty?
              # Found a matching inc - elide both
              inc_idx = incs.pop
              to_remove << inc_idx
              to_remove << idx
              @eliminated += 2
            end
          end

        when Call, IndirectCall
          # Calls may consume the reference - don't elide across them
          # Conservative: clear all pending incs for args
          case inst
          when Call
            inst.args.each { |arg| pending_incs.delete(canonical_ptr(arg, alias_map)) }
          when IndirectCall
            inst.args.each { |arg| pending_incs.delete(canonical_ptr(arg, alias_map)) }
            pending_incs.delete(canonical_ptr(inst.callee_ptr, alias_map))
          end

        when Store
          # Structural NoAlias: Handle stores carefully
          #
          # A store affects RC elision safety in two ways:
          # 1. If we store a noalias value → it escapes, others may hold reference
          # 2. If we store THROUGH a pointer that could modify tracked object's memory
          #
          # Key insight: For different allocation sites, storing TO one allocation
          # cannot affect the reference count of another allocation. The RC header
          # is separate from the object content.
          #
          # The escape check (storing a value) handles case 1.
          # For case 2, we only need to worry about stores through unknown pointers
          # that might alias our tracked allocation's RC header.
          store_value = canonical_ptr(inst.value, alias_map)
          store_ptr = canonical_ptr(inst.ptr, alias_map)

          # If we're storing a noalias value to a field, mark it as escaped
          # This is the KEY safety check: the stored value now lives elsewhere
          if no_alias_ids.includes?(store_value)
            escaped_allocs << store_value
            pending_incs.delete(store_value)
          end

          # For stores through unknown pointers (not from a known noalias alloc),
          # we must be conservative - they might modify any memory
          store_site = alloc_site[store_ptr]?
          store_type = store_site ? alloc_type[store_site]? : nil

          if store_site.nil? || !no_alias_ids.includes?(store_site)
            # Unknown store target - could alias anything, be conservative
            # But use TBAA: keep pending incs for types that CANNOT alias
            pending_incs.reject! do |pending_ptr, _|
              pending_site = alloc_site[pending_ptr]?
              next true unless pending_site  # No site info → conservative reject

              # Keep if from known noalias alloc that hasn't escaped
              next false if no_alias_ids.includes?(pending_site) && !escaped_allocs.includes?(pending_ptr)

              # TBAA: If we know the store type, check type compatibility
              if store_type
                pending_type = alloc_type[pending_site]?
                if pending_type && !store_type.may_alias_type?(pending_type)
                  # Types cannot alias (e.g., Int32* vs MyClass*) → keep pending inc
                  next false
                end
              end

              # Conservative: reject (may alias)
              true
            end
          end
          # If store is to a known noalias allocation, it only affects that allocation's
          # memory, not any other allocation's RC state - no need to clear anything

        when Load
          # Loaded value aliases its source pointer
          src = canonical_ptr(inst.ptr, alias_map)
          alias_map[inst.id] = src
          # Propagate allocation site through loads
          if site = alloc_site[src]?
            alloc_site[inst.id] = site
          end

        when GetElementPtr
          # GEP derives from its base - propagate allocation site
          base = canonical_ptr(inst.base, alias_map)
          if site = alloc_site[base]?
            alloc_site[inst.id] = site
          end
        end
      end

      # Remove elided instructions (in reverse order to preserve indices)
      to_remove.to_a.sort.reverse_each do |idx|
        instructions.delete_at(idx)
      end
    end

    # Simple canonicalization using copy-based aliases
    private def canonical_ptr(ptr : ValueId, alias_map : Hash(ValueId, ValueId)) : ValueId
      current = ptr
      while (aliased = alias_map[current]?)
        current = aliased
      end
      current
    end

    # Structural NoAlias query: Returns true if ptr_a MAY alias with ptr_b
    # Returns false (no alias) only when we can PROVE they don't alias
    private def may_alias?(
      ptr_a : ValueId,
      ptr_b : ValueId,
      alias_map : Hash(ValueId, ValueId),
      no_alias_ids : Set(ValueId),
      escaped_allocs : Set(ValueId),
      alloc_site : Hash(ValueId, ValueId)
    ) : Bool
      # Canonicalize both pointers
      canon_a = canonical_ptr(ptr_a, alias_map)
      canon_b = canonical_ptr(ptr_b, alias_map)

      # Same pointer = must alias
      return true if canon_a == canon_b

      # If either has escaped, be conservative
      return true if escaped_allocs.includes?(canon_a) || escaped_allocs.includes?(canon_b)

      # If both are from different allocation sites AND both are noalias → no alias
      site_a = alloc_site[canon_a]?
      site_b = alloc_site[canon_b]?

      if site_a && site_b && site_a != site_b
        # Different allocation sites
        if no_alias_ids.includes?(site_a) && no_alias_ids.includes?(site_b)
          # Both allocations are noalias and from different sites → cannot alias
          return false
        end
      end

      # If one is a known noalias alloc and the other is from unknown source
      # We can't prove they don't alias, be conservative
      true
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # DEAD CODE ELIMINATION
  # ═══════════════════════════════════════════════════════════════════════════
  #
  # Removes instructions whose results are never used.
  # Does NOT remove:
  #   - Instructions with side effects (calls, stores, rc_dec)
  #   - Terminators

  class DeadCodeEliminationPass
    getter function : Function
    getter eliminated : Int32 = 0

    def initialize(@function : Function)
    end

    def run : Int32
      @eliminated = 0

      # Build use counts for all values
      use_counts = Hash(ValueId, Int32).new(0)

      @function.blocks.each do |block|
        block.instructions.each do |inst|
          inst.operands.each { |op| use_counts[op] += 1 }
        end

        # Count uses in terminator
        case term = block.terminator
        when Branch
          use_counts[term.condition] += 1
        when Switch
          use_counts[term.value] += 1
        when Return
          if v = term.value
            use_counts[v] += 1
          end
        end
      end

      # Remove dead instructions (iterate until fixpoint)
      changed = true
      while changed
        changed = false

        @function.blocks.each do |block|
          block.instructions.reject! do |inst|
            if !has_side_effects?(inst) && use_counts[inst.id] == 0
              # This instruction is dead - remove it
              # Also decrement use counts for its operands
              inst.operands.each { |op| use_counts[op] -= 1 }
              @eliminated += 1
              changed = true
              true  # remove
            else
              false  # keep
            end
          end
        end
      end

      @eliminated
    end

    private def has_side_effects?(inst : Value) : Bool
      case inst
      when Store, Free, RCDecrement, ArraySet
        true
      when Call, IndirectCall, ExternCall
        # Calls always have potential side effects
        true
      when MutexLock, MutexUnlock, MutexTryLock
        true
      when ChannelSend, ChannelReceive, ChannelClose
        true
      when AtomicStore, AtomicRMW, AtomicCAS, Fence
        true
      else
        false
      end
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # CONSTANT FOLDING
  # ═══════════════════════════════════════════════════════════════════════════
  #
  # Evaluates constant expressions at compile time:
  #   - add const1, const2 → const (const1 + const2)
  #   - cmp const1, const2 → true/false
  #   - select true, a, b → a
  #   - select false, a, b → b

  class ConstantFoldingPass
    getter function : Function
    getter folded : Int32 = 0

    def initialize(@function : Function)
    end

    def run : Int32
      @folded = 0

      # Build value → constant map (only numeric/bool constants for folding)
      constants = {} of ValueId => (Int64 | UInt64 | Float64 | Bool | Nil)

      @function.blocks.each do |block|
        block.instructions.each do |inst|
          case inst
          when Constant
            case v = inst.value
            when Int64, UInt64, Float64, Bool, Nil
              constants[inst.id] = v
            end
          end
        end
      end

      # Fold operations
      @function.blocks.each do |block|
        new_instructions = [] of Value

        block.instructions.each do |inst|
          folded_inst = try_fold(inst, constants)
          if folded_inst
            new_instructions << folded_inst
            # Update constants map if we created a new constant
            if folded_inst.is_a?(Constant)
              case v = folded_inst.value
              when Int64, UInt64, Float64, Bool, Nil
                constants[folded_inst.id] = v
              end
            end
            @folded += 1
          else
            new_instructions << inst
          end
        end

        block.instructions.clear
        new_instructions.each { |i| block.add(i) }
      end

      @folded
    end

    private def try_fold(inst : Value, constants : Hash(ValueId, Int64 | UInt64 | Float64 | Bool | Nil)) : Value?
      case inst
      when BinaryOp
        left_const = constants[inst.left]?
        right_const = constants[inst.right]?

        if left_const && right_const
          result = fold_binary(inst.op, left_const, right_const)
          if result
            return Constant.new(inst.id, inst.type, result)
          end
        end

      when Select
        cond_const = constants[inst.condition]?
        if cond_const.is_a?(Bool)
          # select true, a, b → a
          # select false, a, b → b
          # We can't actually fold this to just the value, but we can
          # mark it for copy propagation
          # For now, skip this - would need more infrastructure
        end
      end

      nil
    end

    private def fold_binary(op : BinOp, left, right) : (Int64 | UInt64 | Float64 | Bool)?
      case {left, right}
      when {Int64, Int64}
        fold_int_op(op, left, right)
      when {Float64, Float64}
        fold_float_op(op, left, right)
      else
        nil
      end
    end

    private def fold_int_op(op : BinOp, left : Int64, right : Int64) : (Int64 | Bool)?
      case op
      when .add? then left + right
      when .sub? then left - right
      when .mul? then left * right
      when .div? then right != 0 ? left // right : nil
      when .rem? then right != 0 ? left % right : nil
      when .shl? then left << right
      when .shr? then left >> right
      when .and? then left & right
      when .or?  then left | right
      when .xor? then left ^ right
      when .eq?  then left == right
      when .ne?  then left != right
      when .lt?  then left < right
      when .le?  then left <= right
      when .gt?  then left > right
      when .ge?  then left >= right
      else nil
      end
    end

    private def fold_float_op(op : BinOp, left : Float64, right : Float64) : (Float64 | Bool)?
      case op
      when .add? then left + right
      when .sub? then left - right
      when .mul? then left * right
      when .div? then right != 0 ? left / right : nil
      when .eq?  then left == right
      when .ne?  then left != right
      when .lt?  then left < right
      when .le?  then left <= right
      when .gt?  then left > right
      when .ge?  then left >= right
      else nil
      end
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # LOCK ELISION
  # ═══════════════════════════════════════════════════════════════════════════
  #
  # Removes unnecessary synchronization operations:
  #   1. Thread-local elision: Remove locks on objects that don't escape to other threads
  #   2. Redundant lock elision: Remove nested locks on the same mutex
  #   3. Lock coarsening: Merge adjacent lock/unlock pairs
  #
  # This pass relies on escape analysis and ThreadShared taint tracking.
  # A mutex can be elided if:
  #   - The protected data doesn't have ThreadShared taint
  #   - The mutex allocation doesn't escape to spawn/channel/shared-memory
  #
  # Safety: Lock elision is conservative. We only elide when we can PROVE
  # that no other thread can observe the protected data.

  class LockElisionPass
    getter function : Function
    getter eliminated : Int32 = 0
    getter coarsened : Int32 = 0

    def initialize(@function : Function)
    end

    def run : Int32
      @eliminated = 0
      @coarsened = 0

      # Track allocation sites and their escape status
      escaped_allocs = Set(ValueId).new
      thread_shared = Set(ValueId).new  # Allocations passed to spawn/channel
      mutex_allocs = Set(ValueId).new   # Known mutex allocations

      # First pass: identify escapes and thread-shared allocations
      @function.blocks.each do |block|
        block.instructions.each do |inst|
          case inst
          when Alloc
            # Could be a mutex if strategy suggests synchronization
            if inst.strategy == MemoryStrategy::AtomicARC
              mutex_allocs << inst.id
            end

          when Store
            # Storing to a field = escape
            escaped_allocs << inst.value

          when Call
            # Internal calls - mark args as escaped but not necessarily thread-shared
            # (we'd need function analysis to determine if it spawns threads)
            inst.args.each do |arg|
              escaped_allocs << arg
            end

          when ExternCall
            # Check extern names for thread-related functions
            inst.args.each do |arg|
              escaped_allocs << arg
              # If function name suggests threading, mark as thread-shared
              if inst.extern_name.includes?("spawn") ||
                 inst.extern_name.includes?("fiber") ||
                 inst.extern_name.includes?("thread") ||
                 inst.extern_name.includes?("channel") ||
                 inst.extern_name.includes?("pthread")
                thread_shared << arg
              end
            end

          when ChannelSend
            # Data sent to channel is thread-shared
            thread_shared << inst.value

          when IndirectCall
            # Indirect calls = conservative escape for all args
            inst.args.each do |arg|
              escaped_allocs << arg
              thread_shared << arg  # Conservative: assume potential threading
            end
          end
        end
      end

      # Second pass: elide locks on thread-local objects
      @function.blocks.each do |block|
        elide_thread_local_locks(block, escaped_allocs, thread_shared, mutex_allocs)
      end

      # Third pass: redundant lock elision (nested locks on same mutex)
      @function.blocks.each do |block|
        elide_redundant_locks(block)
      end

      # Fourth pass: lock coarsening (merge adjacent critical sections)
      @function.blocks.each do |block|
        coarsen_locks(block)
      end

      @eliminated + @coarsened
    end

    # Remove locks on objects that are provably thread-local
    private def elide_thread_local_locks(
      block : BasicBlock,
      escaped_allocs : Set(ValueId),
      thread_shared : Set(ValueId),
      mutex_allocs : Set(ValueId)
    )
      to_remove = [] of Int32

      block.instructions.each_with_index do |inst, idx|
        case inst
        when MutexLock, MutexUnlock, MutexTryLock
          mutex_ptr = case inst
                      when MutexLock   then inst.mutex_ptr
                      when MutexUnlock then inst.mutex_ptr
                      when MutexTryLock then inst.mutex_ptr
                      else 0_u32
                      end

          # Can elide if mutex is:
          # 1. Known allocation that hasn't escaped to thread-shared context
          # 2. Not protecting thread-shared data
          if mutex_allocs.includes?(mutex_ptr) && !thread_shared.includes?(mutex_ptr)
            # Additional check: the mutex shouldn't have escaped at all
            # for thread-local elision (more conservative)
            if !escaped_allocs.includes?(mutex_ptr)
              to_remove << idx
              @eliminated += 1
            end
          end
        end
      end

      # Remove in reverse order
      to_remove.reverse_each do |idx|
        block.instructions.delete_at(idx)
      end
    end

    # Remove nested locks on the same mutex within a single block
    # Pattern: lock(m); lock(m); unlock(m); unlock(m) → lock(m); unlock(m)
    # Only the inner lock/unlock pair is redundant
    private def elide_redundant_locks(block : BasicBlock)
      # Track lock depth per mutex: depth > 1 means nested
      lock_depth = Hash(ValueId, Int32).new(0)
      to_remove = [] of Int32

      block.instructions.each_with_index do |inst, idx|
        case inst
        when MutexLock
          lock_depth[inst.mutex_ptr] += 1
          if lock_depth[inst.mutex_ptr] > 1
            # Nested lock - mark for removal
            to_remove << idx
            @eliminated += 1
          end

        when MutexUnlock
          if lock_depth[inst.mutex_ptr] > 1
            # Unlock of a nested lock - mark for removal
            to_remove << idx
            @eliminated += 1
          end
          lock_depth[inst.mutex_ptr] = {lock_depth[inst.mutex_ptr] - 1, 0}.max
        end
      end

      # Remove in reverse order
      to_remove.reverse_each do |idx|
        block.instructions.delete_at(idx)
      end
    end

    # Merge adjacent critical sections on same mutex
    # Pattern: lock(m); ...; unlock(m); lock(m); ...; unlock(m)
    #       → lock(m); ...; ...; unlock(m)
    private def coarsen_locks(block : BasicBlock)
      # Find unlock/lock pairs on same mutex with only safe operations between
      i = 0
      while i < block.instructions.size - 1
        inst = block.instructions[i]

        if inst.is_a?(MutexUnlock)
          # Look for immediately following or nearby MutexLock on same mutex
          next_idx = i + 1
          can_coarsen = true
          lock_idx : Int32? = nil

          while next_idx < block.instructions.size && can_coarsen
            next_inst = block.instructions[next_idx]

            case next_inst
            when MutexLock
              if next_inst.mutex_ptr == inst.mutex_ptr
                lock_idx = next_idx
                break
              else
                # Different mutex - can't coarsen through this
                can_coarsen = false
              end

            when Call, IndirectCall, ExternCall, ChannelSend, ChannelReceive
              # Calls between unlock/lock prevent coarsening (side effects)
              can_coarsen = false

            when Store
              # Stores to potentially shared memory prevent coarsening
              can_coarsen = false

            when MutexUnlock, MutexTryLock
              # Other mutex ops prevent coarsening
              can_coarsen = false

            else
              # Safe instruction - can look further
              next_idx += 1
            end
          end

          if lock_idx && can_coarsen
            # Remove the unlock at i and lock at lock_idx
            # Must remove from higher index first
            block.instructions.delete_at(lock_idx)
            block.instructions.delete_at(i)
            @coarsened += 2
            # Don't increment i - we shifted indices
            next
          end
        end

        i += 1
      end
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # COPY PROPAGATION (light stub)
  # ═══════════════════════════════════════════════════════════════════════════
  #
  # Currently a stub to keep stats/pipeline consistent; real implementation
  # would replace copies with sources using def-use chains.
  class CopyPropagationPass
    getter function : Function
    getter propagated : Int32 = 0

    def initialize(@function : Function)
    end

    def run : Int32
      @propagated = 0
      # TODO: implement use-def based copy propagation when Copy instruction exists
      0
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # OPTIMIZATION PIPELINE
  # ═══════════════════════════════════════════════════════════════════════════

  struct OptimizationStats
    property rc_eliminated : Int32 = 0
    property dead_eliminated : Int32 = 0
    property constants_folded : Int32 = 0
    property copies_propagated : Int32 = 0
    property locks_elided : Int32 = 0

    def total : Int32
      rc_eliminated + dead_eliminated + constants_folded + copies_propagated + locks_elided
    end

    def to_s(io : IO)
      io << "Optimizations: "
      io << rc_eliminated << " RC ops, "
      io << dead_eliminated << " dead insts, "
      io << constants_folded << " constants folded, "
      io << locks_elided << " locks elided"
    end
  end

  class OptimizationPipeline
    getter function : Function
    getter stats : OptimizationStats

    def initialize(@function : Function)
      @stats = OptimizationStats.new
    end

    # Run all optimization passes
    def run : OptimizationStats
      # Pass 1: Constant folding (enables more DCE)
      cf = ConstantFoldingPass.new(@function)
      @stats.constants_folded = cf.run

      # Pass 2: RC elision (Crystal-specific)
      rc = RCElisionPass.new(@function)
      @stats.rc_eliminated = rc.run

      # Pass 2.5: Copy propagation (light)
      cp = CopyPropagationPass.new(@function)
      @stats.copies_propagated = cp.run

      # Pass 3: Lock elision (thread-safety optimization)
      le = LockElisionPass.new(@function)
      @stats.locks_elided = le.run

      # Pass 4: Dead code elimination
      dce = DeadCodeEliminationPass.new(@function)
      @stats.dead_eliminated = dce.run

      # Pass 5: DCE again (RC/lock elision may have created more dead code)
      dce2 = DeadCodeEliminationPass.new(@function)
      @stats.dead_eliminated += dce2.run

      @stats
    end

    # Run specific passes
    def run_rc_elision : Int32
      RCElisionPass.new(@function).run
    end

    def run_dce : Int32
      DeadCodeEliminationPass.new(@function).run
    end

    def run_constant_folding : Int32
      ConstantFoldingPass.new(@function).run
    end

    def run_lock_elision : Int32
      LockElisionPass.new(@function).run
    end
  end

  # Lexicographic potential for LTP-style local optimization.
  # Lower is better. Order: RC ops → instruction count → unsafe sites.
  record PotentialMetrics,
    rc_ops : Int32,
    instructions : Int32,
    unsafe_sites : Int32 do
    include Comparable(PotentialMetrics)

    def <=>(other : PotentialMetrics)
      return rc_ops <=> other.rc_ops if rc_ops != other.rc_ops
      return instructions <=> other.instructions if instructions != other.instructions
      unsafe_sites <=> other.unsafe_sites
    end

    def to_s(io : IO)
      io << "{rc_ops=" << rc_ops << ", insts=" << instructions << ", unsafe=" << unsafe_sites << "}"
    end
  end

  # Convenience method on Function
  class Function
    def optimize : OptimizationStats
      pipeline = OptimizationPipeline.new(self)
      pipeline.run
    end

    # LTP/WBA-inspired local optimization loop with monotone potential.
    # Runs a small pipeline repeatedly while the legacy potential decreases,
    # then finishes with LTP optimization for window/corridor moves.
    def optimize_with_potential(max_iters : Int32 = 4) : Tuple(OptimizationStats, LTPPotential)
      aggregate = OptimizationStats.new
      last_potential = compute_potential
      iter = 0

      while iter < max_iters
        stats = optimize
        aggregate.rc_eliminated += stats.rc_eliminated
        aggregate.dead_eliminated += stats.dead_eliminated
        aggregate.constants_folded += stats.constants_folded

        current = compute_potential
        break unless current < last_potential

        last_potential = current
        iter += 1
      end

      ltp_potential = optimize_ltp(max_iters: max_iters)
      {aggregate, ltp_potential}
    end

    private def compute_potential : PotentialMetrics
      rc_ops = 0
      insts = 0
      unsafe = 0

      blocks.each do |block|
        insts += block.instructions.size
        block.instructions.each do |inst|
          case inst
          when RCIncrement, RCDecrement
            rc_ops += 1
          when IndirectCall
            unsafe += 1
          end
        end
        # Terminators do not contribute to rc_ops, but can add unsafe if indirect jumps were used; none yet.
      end

      PotentialMetrics.new(rc_ops, insts, unsafe)
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # LTP/WBA OPTIMIZATION FRAMEWORK
  # ═══════════════════════════════════════════════════════════════════════════
  #
  # Full implementation of the LTP (Local Trigger → Transport → Potential)
  # framework with Window-Band-Area semantics adapted for MIR optimization.
  #
  # Theory reference: "LTP/WBA Framework — From G_{3,5} to Kakeya & Magnus"

  # ─────────────────────────────────────────────────────────────────────────────
  # Enhanced 4-Component Potential (Φ′)
  # ─────────────────────────────────────────────────────────────────────────────
  #
  # Φ′(Δ) = (I, -M, P, |Δ|) where:
  #   I  = window_overlap: max exposure of trigger window (boundary contact)
  #   M  = tie_plateau: number of windows attaining max I (negative for lex order)
  #   P  = corner_mismatch: bad corners / conflicts at window endpoints
  #   |Δ|= area: total instruction count

  record LTPPotential,
    window_overlap : Int32,    # I: max |∂Π ∩ ∂Δ| - higher means more exposed
    tie_plateau : Int32,       # -M: negative count of tied windows
    corner_mismatch : Int32,   # P: bad corners at endpoints
    area : Int32 do            # |Δ|: instruction count

    include Comparable(LTPPotential)

    # Lexicographic comparison: minimize (I desc, -M asc, P asc, area asc)
    # But we want LOWER potential = BETTER, so:
    # - Lower window_overlap is better (fewer exposed RC ops)
    # - Higher tie_plateau is better (fewer tied windows, stored as negative)
    # - Lower corner_mismatch is better
    # - Lower area is better
    def <=>(other : LTPPotential)
      # Compare in order: window_overlap, tie_plateau, corner_mismatch, area
      return window_overlap <=> other.window_overlap if window_overlap != other.window_overlap
      return tie_plateau <=> other.tie_plateau if tie_plateau != other.tie_plateau
      return corner_mismatch <=> other.corner_mismatch if corner_mismatch != other.corner_mismatch
      area <=> other.area
    end

    def to_s(io : IO)
      io << "Φ′{I=" << window_overlap << ", -M=" << tie_plateau
      io << ", P=" << corner_mismatch << ", |Δ|=" << area << "}"
    end

    def self.zero : LTPPotential
      new(0, 0, 0, 0)
    end
  end

  # ─────────────────────────────────────────────────────────────────────────────
  # Window: Local trigger point (BR-1)
  # ─────────────────────────────────────────────────────────────────────────────
  #
  # A Window is a "boundary cell" - an instruction with maximum exposure
  # to optimization opportunities. In our case, RC operations that can
  # potentially be elided.

  class Window
    getter instruction : Value           # The trigger instruction
    getter block : BasicBlock            # Containing block
    getter index : Int32                 # Position in block
    getter exposure : Int32              # How "exposed" this is (use count, etc)
    getter ptr : ValueId                 # The pointer being operated on

    def initialize(@instruction, @block, @index, @exposure, @ptr)
    end

    def to_s(io : IO)
      io << "Window{" << @instruction.class.name << " @" << @index
      io << ", exp=" << @exposure << ", ptr=r" << @ptr << "}"
    end
  end

  # ─────────────────────────────────────────────────────────────────────────────
  # Corridor: Transport path from window (BR-2)
  # ─────────────────────────────────────────────────────────────────────────────
  #
  # A Corridor traces the def-use chain from a Window's pointer to its
  # terminal use (rc_dec, escape via call, or function return).

  enum CorridorExit
    Boundary      # Exits at function return
    Elision       # Found matching rc_dec - can elide
    Escape        # Escapes via call argument
    Store         # Stored to memory (may alias)
    Unknown       # Could not trace
  end

  class Corridor
    getter window : Window
    getter path : Array(Value)           # Instructions along the corridor
    getter exit_type : CorridorExit
    getter exit_instruction : Value?     # Terminal instruction (if any)

    def initialize(@window, @path, @exit_type, @exit_instruction = nil)
    end

    # Can this corridor support a Spike move (rc_inc/rc_dec elision)?
    def spike_eligible? : Bool
      @exit_type == CorridorExit::Elision
    end

    # Can this corridor support a Ladder move (short corridor elimination)?
    def ladder_eligible? : Bool
      # Ladder: single intermediate use between inc and dec
      @exit_type == CorridorExit::Elision && @path.size <= 3
    end

    def to_s(io : IO)
      io << "Corridor{" << @path.size << " steps, exit=" << @exit_type << "}"
    end
  end

  # ─────────────────────────────────────────────────────────────────────────────
  # Legal Moves (S/L/D/C)
  # ─────────────────────────────────────────────────────────────────────────────

  enum MoveType
    Spike     # Length-2 cancellation (rc_inc + rc_dec)
    Ladder    # Short corridor elimination
    Diamond   # Confluent resolution of critical pair
    Collapse  # Dead code removal
  end

  struct LegalMove
    property type : MoveType
    property window : Window?
    property corridor : Corridor?
    property instructions_to_remove : Array(Int32)  # Indices to remove
    property potential_decrease : LTPPotential      # Expected Φ decrease

    def initialize(@type, @window = nil, @corridor = nil,
                   @instructions_to_remove = [] of Int32,
                   @potential_decrease = LTPPotential.zero)
    end

    def to_s(io : IO)
      io << "Move{" << @type << ", remove=" << @instructions_to_remove.size
      io << ", ΔΦ=" << @potential_decrease << "}"
    end
  end

  # ─────────────────────────────────────────────────────────────────────────────
  # LTP Engine: Main optimization driver
  # ─────────────────────────────────────────────────────────────────────────────

  class LTPEngine
    getter function : Function
    getter moves_applied : Array(LegalMove)
    getter iterations : Int32
    getter final_potential : LTPPotential
    property debug : Bool

    # Def-use chains for corridor tracing
    @use_map : Hash(ValueId, Array(Tuple(BasicBlock, Int32, Value)))
    # Alias map for pointer canonicalization
    @alias_map : Hash(ValueId, ValueId)
    # NoAlias set (values that don't alias anything)
    @no_alias_ids : Set(ValueId)

    def initialize(@function : Function)
      @moves_applied = [] of LegalMove
      @iterations = 0
      @final_potential = LTPPotential.zero
      @debug = false
      @use_map = Hash(ValueId, Array(Tuple(BasicBlock, Int32, Value))).new
      @alias_map = Hash(ValueId, ValueId).new
      @no_alias_ids = Set(ValueId).new
    end

    # ═══════════════════════════════════════════════════════════════════════════
    # Main Entry Point
    # ═══════════════════════════════════════════════════════════════════════════

    def run(max_iters : Int32 = 10) : LTPPotential
      build_analysis_maps
      @final_potential = compute_ltp_potential

      log "LTP Engine start: #{@final_potential}"

      while @iterations < max_iters
        # BR-1: Find trigger window
        window = find_window
        unless window
          log "  Iter #{@iterations}: No window found. Trying dual frame..."
          if try_dual_frame
            build_analysis_maps
            @final_potential = compute_ltp_potential
            @iterations += 1
            next
          end
          break
        end

        log "  Iter #{@iterations}: Window = #{window}"

        # BR-2: Trace corridor from window
        corridor = trace_corridor(window)
        log "    Corridor: #{corridor}"

        # Find best legal move (priority: S > L > D > C)
        move = find_best_move(window, corridor)

        if move
          log "    Applying: #{move}"

          # Apply the move
          apply_move(move)
          @moves_applied << move

          # Recompute potential
          build_analysis_maps  # Rebuild after modification
          new_potential = compute_ltp_potential

          # BR-3: Verify strict decrease
          if new_potential >= @final_potential
            log "    WARNING: Potential did not decrease! Trying dual frame..."
            # BR-4: Try dual frame (escape-based analysis)
            if !try_dual_frame
              log "    Dual frame exhausted. Stopping."
              break
            end
          end

          @final_potential = new_potential
          log "    New potential: #{@final_potential}"
        else
          log "    No legal move found. Trying Collapse..."
          # Try Collapse (DCE) as fallback
          collapsed = try_collapse_move
          if collapsed > 0
            log "    Collapsed #{collapsed} dead instructions"
            build_analysis_maps
            @final_potential = compute_ltp_potential
          else
            log "    Nothing to collapse. Trying dual frame..."
            if try_dual_frame
              build_analysis_maps
              @final_potential = compute_ltp_potential
            else
              log "    Dual frame exhausted. Stopping."
              break
            end
          end
        end

        @iterations += 1
      end

      log "LTP Engine done: #{@iterations} iterations, #{@moves_applied.size} moves"
      log "Final potential: #{@final_potential}"

      @final_potential
    end

    # ═══════════════════════════════════════════════════════════════════════════
    # BR-1: Window Discovery
    # ═══════════════════════════════════════════════════════════════════════════

    private def find_window : Window?
      best_window : Window? = nil
      best_exposure = -1

      @function.blocks.each do |block|
        block.instructions.each_with_index do |inst, idx|
          case inst
          when RCIncrement
            # Calculate exposure: how many uses does the ptr have?
            ptr = canonical_ptr(inst.ptr)
            next unless @no_alias_ids.includes?(ptr)

            exposure = (@use_map[ptr]?.try(&.size) || 0)

            if exposure > best_exposure
              best_exposure = exposure
              best_window = Window.new(inst, block, idx, exposure, ptr)
            end
          end
        end
      end

      best_window
    end

    # ═══════════════════════════════════════════════════════════════════════════
    # BR-2: Corridor Tracing
    # ═══════════════════════════════════════════════════════════════════════════

    private def trace_corridor(window : Window) : Corridor
      path = [window.instruction] of Value
      ptr = window.ptr
      exit_type = CorridorExit::Unknown
      exit_inst : Value? = nil

      # Walk forward from window looking for rc_dec or escape
      current_block = window.block
      current_idx = window.index + 1

      # Simple forward scan in same block first
      while current_idx < current_block.instructions.size
        inst = current_block.instructions[current_idx]
        path << inst

        case inst
        when RCDecrement
          if canonical_ptr(inst.ptr) == ptr
            exit_type = CorridorExit::Elision
            exit_inst = inst
            break
          end

        when Call, IndirectCall
          # Check if ptr is an argument
          args = case inst
                 when Call       then inst.args
                 when IndirectCall then inst.args
                 else [] of ValueId
                 end

          if args.includes?(ptr)
            exit_type = CorridorExit::Escape
            exit_inst = inst
            break
          end

        when Store
          # Store may clobber - conservative exit
          exit_type = CorridorExit::Store
          exit_inst = inst
          break
        end

        current_idx += 1
      end

      # If we reached end of block without finding exit, check terminator
      if exit_type == CorridorExit::Unknown
        case term = current_block.terminator
        when Return
          if term.value == ptr
            exit_type = CorridorExit::Boundary
          end
        end
      end

      Corridor.new(window, path, exit_type, exit_inst)
    end

    # ═══════════════════════════════════════════════════════════════════════════
    # Move Selection (Priority: S > L > D > C)
    # ═══════════════════════════════════════════════════════════════════════════

    private def find_best_move(window : Window, corridor : Corridor) : LegalMove?
      # Try Spike first
      if corridor.spike_eligible?
        return create_spike_move(window, corridor)
      end

      # Try Ladder
      if corridor.ladder_eligible?
        return create_ladder_move(window, corridor)
      end

      # Try Diamond (if we have conflicting moves)
      diamond = find_diamond_move(window)
      return diamond if diamond

      # Collapse handled separately in main loop
      nil
    end

    # ─────────────────────────────────────────────────────────────────────────────
    # Spike Move: rc_inc/rc_dec pair cancellation
    # ─────────────────────────────────────────────────────────────────────────────

    private def create_spike_move(window : Window, corridor : Corridor) : LegalMove
      indices_to_remove = [] of Int32

      # Find the rc_inc index
      indices_to_remove << window.index

      # Find the rc_dec index
      if exit_inst = corridor.exit_instruction
        corridor.path.each_with_index do |inst, path_idx|
          if inst.id == exit_inst.id
            # Calculate actual index in block
            actual_idx = window.index + path_idx
            indices_to_remove << actual_idx
            break
          end
        end
      end

      # Potential decrease: -2 RC ops, -2 instructions
      decrease = LTPPotential.new(-2, 0, 0, -2)

      LegalMove.new(MoveType::Spike, window, corridor, indices_to_remove, decrease)
    end

    # ─────────────────────────────────────────────────────────────────────────────
    # Ladder Move: Short corridor elimination
    # ─────────────────────────────────────────────────────────────────────────────

    private def create_ladder_move(window : Window, corridor : Corridor) : LegalMove
      # Ladder removes the entire short corridor
      indices_to_remove = (0...corridor.path.size).map { |i| window.index + i }

      # Decrease depends on corridor length
      rc_decrease = -2  # inc + dec
      inst_decrease = -corridor.path.size

      decrease = LTPPotential.new(rc_decrease, 0, -1, inst_decrease)

      LegalMove.new(MoveType::Ladder, window, corridor, indices_to_remove, decrease)
    end

    # ─────────────────────────────────────────────────────────────────────────────
    # Diamond Move: Confluent resolution
    # ─────────────────────────────────────────────────────────────────────────────

    private def find_diamond_move(window : Window) : LegalMove?
      # Look for conflicting optimizations on same value
      ptr = window.ptr

      # Find all windows targeting same ptr
      competing_windows = [] of Window

      @function.blocks.each do |block|
        block.instructions.each_with_index do |inst, idx|
          next if inst.id == window.instruction.id

          case inst
          when RCIncrement
            if canonical_ptr(inst.ptr) == ptr
              exp = (@use_map[ptr]?.try(&.size) || 0)
              competing_windows << Window.new(inst, block, idx, exp, ptr)
            end
          end
        end
      end

      return nil if competing_windows.empty?

      # Evaluate each competing window and choose the one with best Φ decrease
      best_move : LegalMove? = nil
      best_decrease = LTPPotential.zero

      competing_windows.each do |comp_window|
        comp_corridor = trace_corridor(comp_window)

        if comp_corridor.spike_eligible?
          move = create_spike_move(comp_window, comp_corridor)
          if best_move.nil? || move.potential_decrease < best_decrease
            best_move = move
            best_decrease = move.potential_decrease
          end
        end
      end

      if best_move
        # Wrap as Diamond move
        LegalMove.new(MoveType::Diamond, best_move.window, best_move.corridor,
          best_move.instructions_to_remove, best_move.potential_decrease)
      else
        nil
      end
    end

    # ─────────────────────────────────────────────────────────────────────────────
    # Collapse Move: Dead code elimination
    # ─────────────────────────────────────────────────────────────────────────────

    private def try_collapse_move : Int32
      dce = DeadCodeEliminationPass.new(@function)
      dce.run
    end

    # ═══════════════════════════════════════════════════════════════════════════
    # Move Application
    # ═══════════════════════════════════════════════════════════════════════════

    private def apply_move(move : LegalMove)
      return if move.instructions_to_remove.empty?

      window = move.window
      return unless window

      block = window.block

      # Remove instructions in reverse order to preserve indices
      move.instructions_to_remove.sort.reverse_each do |idx|
        if idx >= 0 && idx < block.instructions.size
          block.instructions.delete_at(idx)
        end
      end
    end

    # ═══════════════════════════════════════════════════════════════════════════
    # BR-4: Dual Frame Fallback
    # ═══════════════════════════════════════════════════════════════════════════

    private def try_dual_frame : Bool
      # Dual frame: switch to escape-based analysis
      # For now, just try constant folding as alternative
      cf = ConstantFoldingPass.new(@function)
      folded = cf.run

      if folded > 0
        log "    Dual frame (CF): folded #{folded} constants"
        return true
      end

      # Could add more frames here:
      # - Lifetime analysis frame
      # - Curvature/region analysis frame

      false
    end

    # ═══════════════════════════════════════════════════════════════════════════
    # Potential Computation
    # ═══════════════════════════════════════════════════════════════════════════

    private def compute_ltp_potential : LTPPotential
      window_overlap = 0      # I: count of RC ops (exposure)
      tie_plateau = 0         # -M: count of "tied" windows
      corner_mismatch = 0     # P: conflicts/bad patterns
      area = 0                # |Δ|: total instructions

      windows_by_ptr = Hash(ValueId, Int32).new(0)

      @function.blocks.each do |block|
        area += block.instructions.size

        block.instructions.each do |inst|
          case inst
          when RCIncrement
            ptr = canonical_ptr(inst.ptr)
            window_overlap += 1
            windows_by_ptr[ptr] += 1

          when RCDecrement
            ptr = canonical_ptr(inst.ptr)
            window_overlap += 1
            windows_by_ptr[ptr] += 1

          when IndirectCall
            # Indirect calls are "corner mismatches" - uncertainty
            corner_mismatch += 1

          when Store
            # Stores can create aliasing conflicts
            corner_mismatch += 1
          end
        end
      end

      # Count tied windows (multiple RC ops on same ptr)
      windows_by_ptr.each_value do |count|
        if count > 1
          tie_plateau += count - 1
        end
      end

      LTPPotential.new(window_overlap, -tie_plateau, corner_mismatch, area)
    end

    # ═══════════════════════════════════════════════════════════════════════════
    # Analysis Maps
    # ═══════════════════════════════════════════════════════════════════════════

    private def build_analysis_maps
      @use_map.clear
      @alias_map.clear
      @no_alias_ids.clear

      @function.blocks.each do |block|
        block.instructions.each_with_index do |inst, idx|
          # Build use map
          inst.operands.each do |op|
            (@use_map[op] ||= [] of Tuple(BasicBlock, Int32, Value)) << {block, idx, inst}
          end

          # Build alias map and no_alias set
          case inst
          when Load
            @alias_map[inst.id] = canonical_ptr(inst.ptr)
            if inst.responds_to?(:no_alias) && inst.no_alias
              @no_alias_ids << inst.id
            end

          when Alloc
            if inst.responds_to?(:no_alias) && inst.no_alias
              @no_alias_ids << inst.id
            end
          end
        end
      end
    end

    private def canonical_ptr(ptr : ValueId) : ValueId
      current = ptr
      seen = Set(ValueId).new

      while (aliased = @alias_map[current]?) && !seen.includes?(current)
        seen << current
        current = aliased
      end

      current
    end

    private def log(msg : String)
      puts msg if @debug
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # Integration with Function
  # ═══════════════════════════════════════════════════════════════════════════

  class Function
    # Full LTP/WBA optimization with window-based descent
    def optimize_ltp(max_iters : Int32 = 10, debug : Bool = false) : LTPPotential
      engine = LTPEngine.new(self)
      engine.debug = debug
      engine.run(max_iters)
    end
  end
end
