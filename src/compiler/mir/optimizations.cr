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
      when Store, GlobalStore, Free, RCDecrement, ArraySet, ArraySetSize, ArrayNew
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
      when TryBegin, TryEnd
        # Exception handling setup/teardown — must not be removed
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
        return nil unless constants.has_key?(inst.left) && constants.has_key?(inst.right)

        left_const = constants[inst.left]
        right_const = constants[inst.right]
        result = fold_binary(inst.op, left_const, right_const)
        if ENV["MIR_CF_DEBUG"]?
          STDERR.puts "[MIR_CF] op=#{inst.op} left=#{left_const} left_class=#{left_const.class} right=#{right_const} right_class=#{right_const.class} result=#{result.inspect}"
        end
        if !result.nil?
          return Constant.new(inst.id, inst.type, result)
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
      when {UInt64, UInt64}
        fold_uint_op(op, left, right)
      when {Float64, Float64}
        fold_float_op(op, left, right)
      when {Bool, Bool}
        fold_bool_op(op, left, right)
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

    private def fold_uint_op(op : BinOp, left : UInt64, right : UInt64) : (UInt64 | Bool)?
      case op
      when .add? then left + right
      when .sub? then left - right
      when .mul? then left * right
      when .div? then right != 0_u64 ? left // right : nil
      when .rem? then right != 0_u64 ? left % right : nil
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

    private def fold_bool_op(op : BinOp, left : Bool, right : Bool) : (Bool)?
      case op
      when .and? then left && right
      when .or?  then left || right
      when .xor? then left != right
      when .eq?  then left == right
      when .ne?  then left != right
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
  # LOCAL CSE (pure ops within a block)
  # ═══════════════════════════════════════════════════════════════════════════
  #
  # Identifies duplicate pure instructions within the same block and rewrites
  # later uses to the first occurrence. DCE removes redundant instructions.
  class LocalCSEPass
    getter function : Function
    getter eliminated : Int32 = 0

    def initialize(@function : Function)
    end

    def run : Int32
      @eliminated = 0
      replacements = {} of ValueId => ValueId

      @function.blocks.each do |block|
        binary_seen = {} of Tuple(BinOp, TypeRef, ValueId, ValueId) => ValueId
        unary_seen = {} of Tuple(UnOp, TypeRef, ValueId) => ValueId
        cast_seen = {} of Tuple(CastKind, TypeRef, ValueId) => ValueId
        gep_seen = {} of Tuple(TypeRef, TypeRef, ValueId, Array(UInt32)) => ValueId
        gepd_seen = {} of Tuple(TypeRef, TypeRef, ValueId, ValueId) => ValueId

        block.instructions.each do |inst|
          case inst
          when BinaryOp
            left_id = canonical(inst.left, replacements)
            right_id = canonical(inst.right, replacements)
            key = {inst.op, inst.type, left_id, right_id}
            if existing = binary_seen[key]?
              replacements[inst.id] = existing if existing != inst.id
            else
              binary_seen[key] = inst.id
            end
          when UnaryOp
            value_id = canonical(inst.operand, replacements)
            key = {inst.op, inst.type, value_id}
            if existing = unary_seen[key]?
              replacements[inst.id] = existing if existing != inst.id
            else
              unary_seen[key] = inst.id
            end
          when Cast
            value_id = canonical(inst.value, replacements)
            key = {inst.kind, inst.type, value_id}
            if existing = cast_seen[key]?
              replacements[inst.id] = existing if existing != inst.id
            else
              cast_seen[key] = inst.id
            end
          when GetElementPtr
            base_id = canonical(inst.base, replacements)
            key = {inst.type, inst.base_type, base_id, inst.indices}
            if existing = gep_seen[key]?
              replacements[inst.id] = existing if existing != inst.id
            else
              gep_seen[key] = inst.id
            end
          when GetElementPtrDynamic
            base_id = canonical(inst.base, replacements)
            index_id = canonical(inst.index, replacements)
            key = {inst.type, inst.element_type, base_id, index_id}
            if existing = gepd_seen[key]?
              replacements[inst.id] = existing if existing != inst.id
            else
              gepd_seen[key] = inst.id
            end
          end
        end
      end

      @eliminated = replacements.size
      CopyPropagationPass.new(@function).apply_replacements(replacements, assume_dominates: true)
      @eliminated
    end

    private def canonical(id : ValueId, replacements : Hash(ValueId, ValueId)) : ValueId
      current = id
      hops = 0

      while next_id = replacements[current]?
        break if next_id == current
        current = next_id
        hops += 1
        break if hops >= 64
      end

      replacements[id] = current if current != id
      current
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
    @assume_dominates : Bool = false
    @@cp_phase_timing_enabled : Bool = ENV["CRYSTAL_V2_CP_PHASE_TIMING"]? == "1"
    @@cp_phase_time_totals : Hash(String, Float64) = Hash(String, Float64).new(0.0)
    @@cp_phase_call_counts : Hash(String, Int32) = Hash(String, Int32).new(0)
    @@cp_phase_timing_lock : Mutex = Mutex.new

    def initialize(@function : Function)
    end

    def self.cp_phase_timing_enabled? : Bool
      @@cp_phase_timing_enabled
    end

    def self.reset_cp_phase_timing : Nil
      return unless @@cp_phase_timing_enabled
      @@cp_phase_timing_lock.synchronize do
        @@cp_phase_time_totals.clear
        @@cp_phase_call_counts.clear
      end
    end

    def self.cp_phase_timing_snapshot : Array(Tuple(String, Float64, Int32))
      return [] of Tuple(String, Float64, Int32) unless @@cp_phase_timing_enabled
      @@cp_phase_timing_lock.synchronize do
        @@cp_phase_time_totals.map do |phase_name, total_ms|
          {phase_name, total_ms, @@cp_phase_call_counts[phase_name]}
        end
      end
    end

    def self.record_cp_phase_timing(phase_name : String, elapsed_ms : Float64) : Nil
      return unless @@cp_phase_timing_enabled
      @@cp_phase_timing_lock.synchronize do
        @@cp_phase_time_totals[phase_name] += elapsed_ms
        @@cp_phase_call_counts[phase_name] += 1
      end
    end

    def run : Int32
      @propagated = 0
      value_types = {} of ValueId => TypeRef
      value_nodes = {} of ValueId => Value
      constants = {} of ValueId => (Int64 | UInt64 | Float64 | Bool | Nil)
      alloc_site = {} of ValueId => ValueId
      no_alias_sites = Set(ValueId).new

      timed_cp_phase("run_collect_state") do
        @function.params.each do |param|
          value_types[param.index] = param.type
        end

        @function.blocks.each do |block|
          block.instructions.each do |inst|
            value_types[inst.id] = inst.type
            value_nodes[inst.id] = inst
            if inst.is_a?(Constant)
              case v = inst.value
              when Int64, UInt64, Float64, Bool, Nil
                constants[inst.id] = v
              end
            end
            if inst.is_a?(Alloc) && inst.no_alias
              no_alias_sites << inst.id
              alloc_site[inst.id] = inst.id
            end
          end
        end
      end

      replacements = {} of ValueId => ValueId

      timed_cp_phase("run_find_replacements") do
        @function.blocks.each do |block|
          last_store = {} of ValueId => ValueId
          block.instructions.each do |inst|
            case inst
          when Alloc
            if inst.no_alias
              no_alias_sites << inst.id
              alloc_site[inst.id] = inst.id
            end
          when GetElementPtr
            base_id = canonical(inst.base, replacements)
            if site = alloc_site[base_id]?
              alloc_site[inst.id] = site
            end
          when GetElementPtrDynamic
            base_id = canonical(inst.base, replacements)
            if site = alloc_site[base_id]?
              alloc_site[inst.id] = site
            end
          when Cast
            src_id = canonical(inst.value, replacements)
            src_type = value_types[src_id]?
            if inst.kind.bitcast?
              if site = alloc_site[src_id]?
                alloc_site[inst.id] = site
              end
            end
            if inst.kind.bitcast? && src_type == inst.type && src_id != inst.id
              replacements[inst.id] = src_id
            end
          when Store
            ptr_id = canonical(inst.ptr, replacements)
            value_id = canonical(inst.value, replacements)
            if site = alloc_site[ptr_id]?
              if no_alias_sites.includes?(site)
                last_store.reject! { |key, _| alloc_site[key]? == site }
              else
                last_store.clear
              end
            else
              last_store.clear
            end
            last_store[ptr_id] = value_id
          when Load
            ptr_id = canonical(inst.ptr, replacements)
            if stored = last_store[ptr_id]?
              replacements[inst.id] = stored if stored != inst.id
            end
          when Call, IndirectCall, ExternCall, AtomicStore, AtomicRMW, AtomicCAS
            last_store.clear
          when Select
            then_val = canonical(inst.then_value, replacements)
            else_val = canonical(inst.else_value, replacements)
            if then_val == else_val
              replacements[inst.id] = then_val if then_val != inst.id
            else
              cond_id = canonical(inst.condition, replacements)
              cond_const = constants[cond_id]?
              if cond_const.is_a?(Bool)
                chosen = cond_const ? then_val : else_val
                replacements[inst.id] = chosen if chosen != inst.id
              end
            end
          when Phi
            incoming = inst.incoming.map { |(_, v)| canonical(v, replacements) }
            next if incoming.empty?
            if incoming.uniq.size == 1
              candidate = incoming.first
              next if candidate == inst.id
              node = value_nodes[candidate]?
              next if node.is_a?(Undef)
              replacements[inst.id] = candidate
            end
          when BinaryOp
            left_id = canonical(inst.left, replacements)
            right_id = canonical(inst.right, replacements)
            left_const = constants[left_id]?
            right_const = constants[right_id]?
            next unless left_const || right_const

            bool_type = inst.type == TypeRef::BOOL

            case inst.op
            when .add?
              if const_zero?(left_const)
                replacements[inst.id] = right_id if right_id != inst.id
              elsif const_zero?(right_const)
                replacements[inst.id] = left_id if left_id != inst.id
              end
            when .mul?
              if const_zero?(left_const)
                replacements[inst.id] = left_id if left_id != inst.id
              elsif const_zero?(right_const)
                replacements[inst.id] = right_id if right_id != inst.id
              elsif const_one?(left_const)
                replacements[inst.id] = right_id if right_id != inst.id
              elsif const_one?(right_const)
                replacements[inst.id] = left_id if left_id != inst.id
              end
            when .or?
              if bool_type
                if bool_true?(left_const)
                  replacements[inst.id] = left_id if left_id != inst.id
                elsif bool_true?(right_const)
                  replacements[inst.id] = right_id if right_id != inst.id
                elsif bool_false?(left_const)
                  replacements[inst.id] = right_id if right_id != inst.id
                elsif bool_false?(right_const)
                  replacements[inst.id] = left_id if left_id != inst.id
                end
              else
                if const_zero?(left_const)
                  replacements[inst.id] = right_id if right_id != inst.id
                elsif const_zero?(right_const)
                  replacements[inst.id] = left_id if left_id != inst.id
                end
              end
            when .and?
              if bool_type
                if bool_true?(left_const)
                  replacements[inst.id] = right_id if right_id != inst.id
                elsif bool_true?(right_const)
                  replacements[inst.id] = left_id if left_id != inst.id
                elsif bool_false?(left_const)
                  replacements[inst.id] = left_id if left_id != inst.id
                elsif bool_false?(right_const)
                  replacements[inst.id] = right_id if right_id != inst.id
                end
              else
                if const_zero?(left_const)
                  replacements[inst.id] = left_id if left_id != inst.id
                elsif const_zero?(right_const)
                  replacements[inst.id] = right_id if right_id != inst.id
                elsif const_all_ones?(left_const)
                  replacements[inst.id] = right_id if right_id != inst.id
                elsif const_all_ones?(right_const)
                  replacements[inst.id] = left_id if left_id != inst.id
                end
              end
            end
          end
        end
      end
      end

      timed_cp_phase("run_apply_replacements") do
        apply_replacements(replacements)
      end
    end

    def apply_replacements(replacements : Hash(ValueId, ValueId), *, assume_dominates : Bool = false) : Int32
      return 0 if replacements.empty?

      @propagated = replacements.size
      @assume_dominates = assume_dominates
      if ENV["MIR_CP_DEBUG"]?
        STDERR.puts "[MIR_CP] replacements=#{replacements}"
      end

      replacement_keys = replacements.keys.to_set
      affected_block_ids = timed_cp_phase("apply_collect_affected_blocks") do
        affected = Set(BlockId).new
        @function.blocks.each do |block|
          if block_uses_replacements?(block, replacement_keys)
            affected << block.id
          end
        end
        affected
      end

      # Nothing reads replaced values, so rewriting is a guaranteed no-op.
      return 0 if affected_block_ids.empty?

      def_blocks = {} of ValueId => BlockId
      def_index = {} of ValueId => Int32
      dominators = {} of BlockId => Set(BlockId)
      unless @assume_dominates
        timed_cp_phase("apply_build_dominators") do
          def_blocks, def_index = build_def_maps
          if can_skip_dominators_for_local_replacements?(replacements, replacement_keys, affected_block_ids, def_blocks, def_index)
            dominators = {} of BlockId => Set(BlockId)
          else
            dominators = compute_dominators
          end
        end
      end
      block_sizes = timed_cp_phase("apply_build_block_sizes") do
        sizes = {} of BlockId => Int32
        @function.blocks.each do |block|
          sizes[block.id] = block.instructions.size
        end
        sizes
      end

      timed_cp_phase("apply_rewrite_blocks") do
        @function.blocks.each do |block|
          next unless affected_block_ids.includes?(block.id)

          block_changed = false
          block.instructions.each_with_index do |inst, idx|
            rewritten = rewrite_instruction(inst, replacements, block.id, idx, def_blocks, def_index, dominators, block_sizes)
            next if rewritten == inst

            block.instructions[idx] = rewritten
            block_changed = true
          end

          rewritten_term = rewrite_terminator(block.terminator, replacements, block.id, block.instructions.size, def_blocks, def_index, dominators)
          if rewritten_term != block.terminator
            block.terminator = rewritten_term
            block_changed = true
          end

          if ENV["MIR_CP_DEBUG"]?
            STDERR.puts "[MIR_CP] block=#{block.id} changed=#{block_changed} terminator=#{block.terminator}"
          end
        end
      end

      @propagated
    end

    private def timed_cp_phase(phase_name : String, &)
      return yield unless self.class.cp_phase_timing_enabled?
      started_at = Time.instant
      result = yield
      self.class.record_cp_phase_timing(phase_name, (Time.instant - started_at).total_milliseconds)
      result
    end

    private def can_skip_dominators_for_local_replacements?(
      replacements : Hash(ValueId, ValueId),
      replacement_keys : Set(ValueId),
      affected_block_ids : Set(BlockId),
      def_blocks : Hash(ValueId, BlockId),
      def_index : Hash(ValueId, Int32)
    ) : Bool
      return false unless affected_blocks_use_only_local_replacements?(affected_block_ids, replacement_keys, def_blocks)

      replacements.each do |source_id, target_id|
        source_block = def_blocks[source_id]?
        target_block = def_blocks[target_id]?
        return false unless source_block && target_block && source_block == target_block

        source_pos = def_index[source_id]?
        target_pos = def_index[target_id]?
        return false unless source_pos && target_pos && target_pos <= source_pos
      end

      true
    end

    private def affected_blocks_use_only_local_replacements?(
      affected_block_ids : Set(BlockId),
      replacement_keys : Set(ValueId),
      def_blocks : Hash(ValueId, BlockId)
    ) : Bool
      @function.blocks.each do |block|
        next unless affected_block_ids.includes?(block.id)
        return false unless block_uses_only_local_replacements?(block, replacement_keys, def_blocks)
      end

      true
    end

    private def block_uses_only_local_replacements?(
      block : BasicBlock,
      replacement_keys : Set(ValueId),
      def_blocks : Hash(ValueId, BlockId)
    ) : Bool
      block.instructions.each do |inst|
        inst.operands.each do |operand|
          next unless replacement_keys.includes?(operand)
          return false unless def_blocks[operand]? == block.id
        end
      end

      case term = block.terminator
      when Branch
        if replacement_keys.includes?(term.condition)
          return false unless def_blocks[term.condition]? == block.id
        end
      when Switch
        if replacement_keys.includes?(term.value)
          return false unless def_blocks[term.value]? == block.id
        end
      when Return
        if value = term.value
          if replacement_keys.includes?(value)
            return false unless def_blocks[value]? == block.id
          end
        end
      end

      true
    end

    private def block_uses_replacements?(block : BasicBlock, replacement_keys : Set(ValueId)) : Bool
      block.instructions.each do |inst|
        inst.operands.each do |operand|
          return true if replacement_keys.includes?(operand)
        end
      end

      case term = block.terminator
      when Branch
        return true if replacement_keys.includes?(term.condition)
      when Switch
        return true if replacement_keys.includes?(term.value)
      when Return
        if value = term.value
          return true if replacement_keys.includes?(value)
        end
      end

      false
    end

    private def canonical(id : ValueId, replacements : Hash(ValueId, ValueId)) : ValueId
      current = id
      hops = 0

      while next_id = replacements[current]?
        break if next_id == current
        current = next_id
        hops += 1
        break if hops >= 64
      end

      replacements[id] = current if current != id
      current
    end

    private def const_zero?(value) : Bool
      case value
      when Int64
        value == 0_i64
      when UInt64
        value == 0_u64
      when Float64
        value == 0.0
      when Bool
        value == false
      else
        false
      end
    end

    private def const_one?(value) : Bool
      case value
      when Int64
        value == 1_i64
      when UInt64
        value == 1_u64
      when Float64
        value == 1.0
      when Bool
        value == true
      else
        false
      end
    end

    private def const_all_ones?(value) : Bool
      case value
      when Int64
        value == -1_i64
      when UInt64
        value == UInt64::MAX
      else
        false
      end
    end

    private def bool_true?(value) : Bool
      value.is_a?(Bool) && value
    end

    private def bool_false?(value) : Bool
      value.is_a?(Bool) && !value
    end

    private def rewrite_instruction(
      inst : Value,
      replacements : Hash(ValueId, ValueId),
      block_id : BlockId,
      inst_index : Int32,
      def_blocks : Hash(ValueId, BlockId),
      def_index : Hash(ValueId, Int32),
      dominators : Hash(BlockId, Set(BlockId)),
      block_sizes : Hash(BlockId, Int32)
    ) : Value
      case inst
      when Free
        ptr = resolve(inst.ptr, replacements, block_id, inst_index, def_blocks, def_index, dominators)
        return inst if ptr == inst.ptr
        Free.new(inst.id, ptr, inst.strategy)
      when RCIncrement
        ptr = resolve(inst.ptr, replacements, block_id, inst_index, def_blocks, def_index, dominators)
        return inst if ptr == inst.ptr
        RCIncrement.new(inst.id, ptr, inst.atomic)
      when RCDecrement
        ptr = resolve(inst.ptr, replacements, block_id, inst_index, def_blocks, def_index, dominators)
        return inst if ptr == inst.ptr
        RCDecrement.new(inst.id, ptr, inst.atomic, inst.destructor)
      when AtomicLoad
        ptr = resolve(inst.ptr, replacements, block_id, inst_index, def_blocks, def_index, dominators)
        return inst if ptr == inst.ptr
        AtomicLoad.new(inst.id, inst.type, ptr, inst.ordering)
      when AtomicStore
        ptr = resolve(inst.ptr, replacements, block_id, inst_index, def_blocks, def_index, dominators)
        val = resolve(inst.value, replacements, block_id, inst_index, def_blocks, def_index, dominators)
        return inst if ptr == inst.ptr && val == inst.value
        AtomicStore.new(inst.id, ptr, val, inst.ordering)
      when AtomicCAS
        ptr = resolve(inst.ptr, replacements, block_id, inst_index, def_blocks, def_index, dominators)
        expected = resolve(inst.expected, replacements, block_id, inst_index, def_blocks, def_index, dominators)
        desired = resolve(inst.desired, replacements, block_id, inst_index, def_blocks, def_index, dominators)
        return inst if ptr == inst.ptr && expected == inst.expected && desired == inst.desired
        AtomicCAS.new(inst.id, inst.type, ptr, expected, desired, inst.success_ordering, inst.failure_ordering)
      when AtomicRMW
        ptr = resolve(inst.ptr, replacements, block_id, inst_index, def_blocks, def_index, dominators)
        val = resolve(inst.value, replacements, block_id, inst_index, def_blocks, def_index, dominators)
        return inst if ptr == inst.ptr && val == inst.value
        AtomicRMW.new(inst.id, inst.type, inst.op, ptr, val, inst.ordering)
      when MutexLock
        ptr = resolve(inst.mutex_ptr, replacements, block_id, inst_index, def_blocks, def_index, dominators)
        return inst if ptr == inst.mutex_ptr
        MutexLock.new(inst.id, ptr)
      when MutexUnlock
        ptr = resolve(inst.mutex_ptr, replacements, block_id, inst_index, def_blocks, def_index, dominators)
        return inst if ptr == inst.mutex_ptr
        MutexUnlock.new(inst.id, ptr)
      when MutexTryLock
        ptr = resolve(inst.mutex_ptr, replacements, block_id, inst_index, def_blocks, def_index, dominators)
        return inst if ptr == inst.mutex_ptr
        MutexTryLock.new(inst.id, ptr)
      when ChannelSend
        chan = resolve(inst.channel_ptr, replacements, block_id, inst_index, def_blocks, def_index, dominators)
        val = resolve(inst.value, replacements, block_id, inst_index, def_blocks, def_index, dominators)
        return inst if chan == inst.channel_ptr && val == inst.value
        ChannelSend.new(inst.id, chan, val)
      when ChannelReceive
        chan = resolve(inst.channel_ptr, replacements, block_id, inst_index, def_blocks, def_index, dominators)
        return inst if chan == inst.channel_ptr
        ChannelReceive.new(inst.id, inst.type, chan)
      when ChannelClose
        chan = resolve(inst.channel_ptr, replacements, block_id, inst_index, def_blocks, def_index, dominators)
        return inst if chan == inst.channel_ptr
        ChannelClose.new(inst.id, chan)
      when Load
        ptr = resolve(inst.ptr, replacements, block_id, inst_index, def_blocks, def_index, dominators)
        return inst if ptr == inst.ptr
        new_inst = Load.new(inst.id, inst.type, ptr)
        new_inst.no_alias = inst.no_alias
        new_inst
      when Store
        ptr = resolve(inst.ptr, replacements, block_id, inst_index, def_blocks, def_index, dominators)
        val = resolve(inst.value, replacements, block_id, inst_index, def_blocks, def_index, dominators)
        return inst if ptr == inst.ptr && val == inst.value
        Store.new(inst.id, ptr, val)
      when GetElementPtr
        base = resolve(inst.base, replacements, block_id, inst_index, def_blocks, def_index, dominators)
        return inst if base == inst.base
        GetElementPtr.new(inst.id, inst.type, base, inst.indices, inst.base_type)
      when GetElementPtrDynamic
        base = resolve(inst.base, replacements, block_id, inst_index, def_blocks, def_index, dominators)
        index = resolve(inst.index, replacements, block_id, inst_index, def_blocks, def_index, dominators)
        return inst if base == inst.base && index == inst.index
        GetElementPtrDynamic.new(inst.id, inst.type, base, index, inst.element_type)
      when BinaryOp
        left = resolve(inst.left, replacements, block_id, inst_index, def_blocks, def_index, dominators)
        right = resolve(inst.right, replacements, block_id, inst_index, def_blocks, def_index, dominators)
        return inst if left == inst.left && right == inst.right
        BinaryOp.new(inst.id, inst.type, inst.op, left, right)
      when UnaryOp
        operand = resolve(inst.operand, replacements, block_id, inst_index, def_blocks, def_index, dominators)
        return inst if operand == inst.operand
        UnaryOp.new(inst.id, inst.type, inst.op, operand)
      when Cast
        value = resolve(inst.value, replacements, block_id, inst_index, def_blocks, def_index, dominators)
        return inst if value == inst.value
        Cast.new(inst.id, inst.type, inst.kind, value)
      when Phi
        changed = false
        phi = Phi.new(inst.id, inst.type)
        inst.incoming.each do |block_id, val|
          use_index = block_sizes[block_id]? || 0
          new_val = resolve(val, replacements, block_id, use_index, def_blocks, def_index, dominators)
          changed ||= new_val != val
          phi.add_incoming(from: block_id, value: new_val)
        end
        return inst unless changed
        phi
      when Select
        cond = resolve(inst.condition, replacements, block_id, inst_index, def_blocks, def_index, dominators)
        then_val = resolve(inst.then_value, replacements, block_id, inst_index, def_blocks, def_index, dominators)
        else_val = resolve(inst.else_value, replacements, block_id, inst_index, def_blocks, def_index, dominators)
        return inst if cond == inst.condition && then_val == inst.then_value && else_val == inst.else_value
        Select.new(inst.id, inst.type, cond, then_val, else_val)
      when UnionWrap
        val = resolve(inst.value, replacements, block_id, inst_index, def_blocks, def_index, dominators)
        return inst if val == inst.value
        UnionWrap.new(inst.id, inst.type, val, inst.variant_type_id, inst.union_type)
      when UnionUnwrap
        val = resolve(inst.union_value, replacements, block_id, inst_index, def_blocks, def_index, dominators)
        return inst if val == inst.union_value
        UnionUnwrap.new(inst.id, inst.type, val, inst.variant_type_id, inst.safe)
      when UnionTypeIdGet
        val = resolve(inst.union_value, replacements, block_id, inst_index, def_blocks, def_index, dominators)
        return inst if val == inst.union_value
        UnionTypeIdGet.new(inst.id, val)
      when UnionIs
        val = resolve(inst.union_value, replacements, block_id, inst_index, def_blocks, def_index, dominators)
        return inst if val == inst.union_value
        UnionIs.new(inst.id, val, inst.variant_type_id)
      when ArrayLiteral
        new_elements = inst.elements.map { |e| resolve(e, replacements, block_id, inst_index, def_blocks, def_index, dominators) }
        return inst if new_elements == inst.elements
        ArrayLiteral.new(inst.id, inst.element_type, new_elements)
      when ArraySize
        array_val = resolve(inst.array_value, replacements, block_id, inst_index, def_blocks, def_index, dominators)
        return inst if array_val == inst.array_value
        ArraySize.new(inst.id, array_val)
      when ArrayGet
        array_val = resolve(inst.array_value, replacements, block_id, inst_index, def_blocks, def_index, dominators)
        index_val = resolve(inst.index_value, replacements, block_id, inst_index, def_blocks, def_index, dominators)
        return inst if array_val == inst.array_value && index_val == inst.index_value
        ArrayGet.new(inst.id, inst.element_type, array_val, index_val)
      when ArraySet
        array_val = resolve(inst.array_value, replacements, block_id, inst_index, def_blocks, def_index, dominators)
        index_val = resolve(inst.index_value, replacements, block_id, inst_index, def_blocks, def_index, dominators)
        value_id = resolve(inst.value_id, replacements, block_id, inst_index, def_blocks, def_index, dominators)
        return inst if array_val == inst.array_value && index_val == inst.index_value && value_id == inst.value_id
        ArraySet.new(inst.id, inst.element_type, array_val, index_val, value_id)
      when ArraySetSize
        array_val = resolve(inst.array_value, replacements, block_id, inst_index, def_blocks, def_index, dominators)
        size_val = resolve(inst.size_value, replacements, block_id, inst_index, def_blocks, def_index, dominators)
        return inst if array_val == inst.array_value && size_val == inst.size_value
        ArraySetSize.new(inst.id, array_val, size_val)
      when ArrayNew
        cap_val = resolve(inst.capacity_value, replacements, block_id, inst_index, def_blocks, def_index, dominators)
        return inst if cap_val == inst.capacity_value
        ArrayNew.new(inst.id, inst.element_type_ref, cap_val)
      when StringInterpolation
        new_parts = inst.parts.map { |p| resolve(p, replacements, block_id, inst_index, def_blocks, def_index, dominators) }
        return inst if new_parts == inst.parts
        StringInterpolation.new(inst.id, new_parts)
      when GlobalStore
        value = resolve(inst.value, replacements, block_id, inst_index, def_blocks, def_index, dominators)
        return inst if value == inst.value
        GlobalStore.new(inst.id, inst.type, inst.global_name, value)
      when Call
        new_args = inst.args.map { |a| resolve(a, replacements, block_id, inst_index, def_blocks, def_index, dominators) }
        return inst if new_args == inst.args
        Call.new(inst.id, inst.type, inst.callee, new_args)
      when ExternCall
        new_args = inst.args.map { |a| resolve(a, replacements, block_id, inst_index, def_blocks, def_index, dominators) }
        return inst if new_args == inst.args
        ExternCall.new(inst.id, inst.type, inst.extern_name, new_args)
      when AddressOf
        operand = resolve(inst.operand, replacements, block_id, inst_index, def_blocks, def_index, dominators)
        return inst if operand == inst.operand
        AddressOf.new(inst.id, inst.type, operand)
      when IndirectCall
        callee_ptr = resolve(inst.callee_ptr, replacements, block_id, inst_index, def_blocks, def_index, dominators)
        new_args = inst.args.map { |a| resolve(a, replacements, block_id, inst_index, def_blocks, def_index, dominators) }
        return inst if callee_ptr == inst.callee_ptr && new_args == inst.args
        IndirectCall.new(inst.id, inst.type, callee_ptr, new_args)
      else
        inst
      end
    end

    private def rewrite_terminator(
      term : Terminator,
      replacements : Hash(ValueId, ValueId),
      block_id : BlockId,
      use_index : Int32,
      def_blocks : Hash(ValueId, BlockId),
      def_index : Hash(ValueId, Int32),
      dominators : Hash(BlockId, Set(BlockId))
    ) : Terminator
      case term
      when Return
        if value = term.value
          new_value = resolve(value, replacements, block_id, use_index, def_blocks, def_index, dominators)
          if ENV["MIR_CP_DEBUG"]?
            STDERR.puts "[MIR_CP] ret value=#{value} new_value=#{new_value}"
          end
          return term if new_value == value
          return Return.new(new_value)
        end
        term
      when Branch
        cond = resolve(term.condition, replacements, block_id, use_index, def_blocks, def_index, dominators)
        return term if cond == term.condition
        Branch.new(cond, term.then_block, term.else_block)
      when Switch
        value = resolve(term.value, replacements, block_id, use_index, def_blocks, def_index, dominators)
        return term if value == term.value
        Switch.new(value, term.cases, term.default_block)
      else
        term
      end
    end

    private def resolve(
      id : ValueId,
      replacements : Hash(ValueId, ValueId),
      use_block : BlockId,
      use_index : Int32,
      def_blocks : Hash(ValueId, BlockId),
      def_index : Hash(ValueId, Int32),
      dominators : Hash(BlockId, Set(BlockId))
    ) : ValueId
      current = id
      hops = 0

      while next_id = replacements[current]?
        break if next_id == current
        break if hops >= 64
        break unless @assume_dominates || dominates_use?(next_id, use_block, use_index, def_blocks, def_index, dominators)
        current = next_id
        hops += 1
      end

      current
    end

    private def dominates_use?(
      def_id : ValueId,
      use_block : BlockId,
      use_index : Int32,
      def_blocks : Hash(ValueId, BlockId),
      def_index : Hash(ValueId, Int32),
      dominators : Hash(BlockId, Set(BlockId))
    ) : Bool
      def_block = def_blocks[def_id]?
      return false unless def_block

      if def_block == use_block
        idx = def_index[def_id]?
        return false unless idx
        return idx < use_index
      end

      if dom_set = dominators[use_block]?
        dom_set.includes?(def_block)
      else
        false
      end
    end

    private def build_def_maps : Tuple(Hash(ValueId, BlockId), Hash(ValueId, Int32))
      def_blocks = {} of ValueId => BlockId
      def_index = {} of ValueId => Int32
      entry = @function.entry_block

      @function.params.each do |param|
        def_blocks[param.index] = entry
        def_index[param.index] = -1
      end

      @function.blocks.each do |block|
        block.instructions.each_with_index do |inst, idx|
          def_blocks[inst.id] = block.id
          def_index[inst.id] = idx
        end
      end

      {def_blocks, def_index}
    end

    private def compute_dominators : Hash(BlockId, Set(BlockId))
      @function.compute_predecessors

      block_ids = @function.blocks.map(&.id)
      all_blocks = Set(BlockId).new
      block_ids.each { |id| all_blocks << id }

      dom = {} of BlockId => Set(BlockId)
      entry = @function.entry_block
      block_ids.each do |id|
        if id == entry
          dom[id] = Set(BlockId).new([id])
        else
          dom[id] = all_blocks.dup
        end
      end

      changed = true
      while changed
        changed = false
        @function.blocks.each do |block|
          next if block.id == entry
          preds = block.predecessors
          new_dom = Set(BlockId).new

          if preds.empty?
            new_dom << block.id
          else
            intersection = dom[preds.first].dup
            preds.each_with_index do |pred, idx|
              next if idx == 0
              intersection = intersection & dom[pred]
            end
            new_dom = intersection
            new_dom << block.id
          end

          if new_dom != dom[block.id]
            dom[block.id] = new_dom
            changed = true
          end
        end
      end

      dom
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # PEEPHOLE (small local cleanups)
  # ═══════════════════════════════════════════════════════════════════════════
  #
  # Removes trivial no-op casts/phis and turns constant branches into jumps.
  class PeepholePass
    getter function : Function
    getter simplified : Int32 = 0

    def initialize(@function : Function)
    end

    def run : Int32
      @simplified = 0
      value_types = {} of ValueId => TypeRef
      value_nodes = {} of ValueId => Value
      constants = {} of ValueId => (Int64 | UInt64 | Float64 | Bool | Nil | String)

      @function.params.each do |param|
        value_types[param.index] = param.type
      end

      @function.blocks.each do |block|
        block.instructions.each do |inst|
          value_types[inst.id] = inst.type
          value_nodes[inst.id] = inst
          if inst.is_a?(Constant)
            case v = inst.value
            when Int64, UInt64, Float64, Bool, Nil, String
              constants[inst.id] = v
            end
          end
        end
      end

      replacements = {} of ValueId => ValueId

      @function.blocks.each do |block|
        block.instructions.each do |inst|
          case inst
          when Cast
            src_id = canonical(inst.value, replacements)
            src_type = value_types[src_id]?
            if src_type == inst.type && src_id != inst.id
              replacements[inst.id] = src_id
            end
          when Phi
            incoming = inst.incoming.map { |(_, v)| canonical(v, replacements) }
            next if incoming.empty?
            if incoming.uniq.size == 1
              candidate = incoming.first
              next if candidate == inst.id
              node = value_nodes[candidate]?
              next if node.is_a?(Undef)
              replacements[inst.id] = candidate
            end
          end
        end
      end

      unless replacements.empty?
        if ENV["MIR_PEEPHOLE_DEBUG"]?
          STDERR.puts "[MIR_PEEPHOLE] replacements=#{replacements}"
        end
        @simplified += replacements.size
        CopyPropagationPass.new(@function).apply_replacements(replacements)
      end

      @function.blocks.each do |block|
        case term = block.terminator
        when Branch
          cond_id = term.condition
          cond_value = constants[cond_id]?
          next unless cond_value.is_a?(Bool)

          target = cond_value ? term.then_block : term.else_block
          block.terminator = Jump.new(target)
          @simplified += 1
        end
      end

      @simplified
    end

    private def canonical(id : ValueId, replacements : Hash(ValueId, ValueId)) : ValueId
      current = id
      hops = 0

      while next_id = replacements[current]?
        break if next_id == current
        current = next_id
        hops += 1
        break if hops >= 64
      end

      replacements[id] = current if current != id
      current
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
    property cse_eliminated : Int32 = 0
    property peephole_simplified : Int32 = 0
    property locks_elided : Int32 = 0

    def total : Int32
      rc_eliminated + dead_eliminated + constants_folded + copies_propagated + cse_eliminated + peephole_simplified + locks_elided
    end

    def to_s(io : IO)
      io << "Optimizations: "
      io << rc_eliminated << " RC ops, "
      io << dead_eliminated << " dead insts, "
      io << constants_folded << " constants folded, "
      io << copies_propagated << " copies propagated, "
      io << cse_eliminated << " CSEs, "
      io << peephole_simplified << " peephole, "
      io << locks_elided << " locks elided"
    end
  end

  class OptimizationPipeline
    private record OptimizationHints,
      has_binary : Bool,
      has_cse_candidate : Bool,
      has_rc_ops : Bool,
      has_cp_candidate : Bool,
      has_lock_ops : Bool

    getter function : Function
    getter stats : OptimizationStats
    @@pass_timing_enabled : Bool = ENV["CRYSTAL_V2_MIR_PASS_TIMING"]? == "1"
    @@pass_time_totals : Hash(String, Float64) = Hash(String, Float64).new(0.0)
    @@pass_call_counts : Hash(String, Int32) = Hash(String, Int32).new(0)
    @@pass_timing_lock : Mutex = Mutex.new

    def initialize(@function : Function)
      @stats = OptimizationStats.new
    end

    def self.pass_timing_enabled? : Bool
      @@pass_timing_enabled
    end

    def self.reset_pass_timing : Nil
      return unless @@pass_timing_enabled
      @@pass_timing_lock.synchronize do
        @@pass_time_totals.clear
        @@pass_call_counts.clear
      end
    end

    def self.pass_timing_snapshot : Array(Tuple(String, Float64, Int32))
      return [] of Tuple(String, Float64, Int32) unless @@pass_timing_enabled
      @@pass_timing_lock.synchronize do
        @@pass_time_totals.map do |pass_name, total_ms|
          {pass_name, total_ms, @@pass_call_counts[pass_name]}
        end
      end
    end

    def self.record_pass_timing(pass_name : String, elapsed_ms : Float64) : Nil
      return unless @@pass_timing_enabled
      @@pass_timing_lock.synchronize do
        @@pass_time_totals[pass_name] += elapsed_ms
        @@pass_call_counts[pass_name] += 1
      end
    end

    # Run all optimization passes
    def run : OptimizationStats
      hints = collect_hints

      # Pass 1: Constant folding (enables more DCE)
      if hints.has_binary
        @stats.constants_folded = timed_pass("constant_folding") do
          ConstantFoldingPass.new(@function).run
        end
      end

      # Pass 1.5: Local CSE for pure ops within blocks
      if hints.has_cse_candidate
        @stats.cse_eliminated = timed_pass("local_cse") do
          LocalCSEPass.new(@function).run
        end
      end

      # Pass 2: RC elision (Crystal-specific)
      if hints.has_rc_ops
        @stats.rc_eliminated = timed_pass("rc_elision") do
          RCElisionPass.new(@function).run
        end
      end

      # Pass 2.5: Copy propagation (light)
      if hints.has_cp_candidate
        @stats.copies_propagated = timed_pass("copy_propagation") do
          CopyPropagationPass.new(@function).run
        end
      end

      # Pass 2.75: Peephole simplifications
      @stats.peephole_simplified = timed_pass("peephole") do
        PeepholePass.new(@function).run
      end

      # Pass 3: Lock elision (thread-safety optimization)
      if hints.has_lock_ops
        @stats.locks_elided = timed_pass("lock_elision") do
          LockElisionPass.new(@function).run
        end
      end

      # Pass 4: Dead code elimination
      @stats.dead_eliminated = timed_pass("dce") do
        DeadCodeEliminationPass.new(@function).run
      end

      # Pass 5: DCE again only when the first pass changed the block graph.
      # If pass 4 removed nothing, pass 5 is guaranteed to be a no-op.
      if @stats.dead_eliminated > 0
        @stats.dead_eliminated += timed_pass("dce_2") do
          DeadCodeEliminationPass.new(@function).run
        end
      end

      @stats
    end

    private def timed_pass(pass_name : String, & : -> Int32) : Int32
      return yield unless @@pass_timing_enabled
      started_at = Time.instant
      result = yield
      self.class.record_pass_timing(pass_name, (Time.instant - started_at).total_milliseconds)
      result
    end

    private def collect_hints : OptimizationHints
      has_binary = false
      has_cse_candidate = false
      has_rc_ops = false
      has_cp_candidate = false
      has_lock_ops = false

      @function.blocks.each do |block|
        block.instructions.each do |inst|
          case inst
          when BinaryOp
            has_binary = true
            has_cse_candidate = true
            has_cp_candidate = true
          when UnaryOp
            has_cse_candidate = true
          when Cast
            has_cse_candidate = true
            has_cp_candidate = true
          when GetElementPtr, GetElementPtrDynamic
            has_cse_candidate = true
          when Load, Select, Phi
            has_cp_candidate = true
          when RCIncrement, RCDecrement
            has_rc_ops = true
          when MutexLock, MutexUnlock, MutexTryLock
            has_lock_ops = true
          end

          if has_binary && has_cse_candidate && has_rc_ops && has_cp_candidate && has_lock_ops
            return OptimizationHints.new(
              has_binary: has_binary,
              has_cse_candidate: has_cse_candidate,
              has_rc_ops: has_rc_ops,
              has_cp_candidate: has_cp_candidate,
              has_lock_ops: has_lock_ops
            )
          end
        end
      end

      OptimizationHints.new(
        has_binary: has_binary,
        has_cse_candidate: has_cse_candidate,
        has_rc_ops: has_rc_ops,
        has_cp_candidate: has_cp_candidate,
        has_lock_ops: has_lock_ops
      )
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

    def run_cse : Int32
      LocalCSEPass.new(@function).run
    end

    def run_peephole : Int32
      PeepholePass.new(@function).run
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

  # Frame kind for dual-frame potentials.
  enum FrameKind
    Primary
    Curvature
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
    getter potential_trace : Array(LTPPotential)
    property debug : Bool
    property frame_kind : FrameKind

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
      @potential_trace = [] of LTPPotential
      @debug = false
      @frame_kind = FrameKind::Primary
      @use_map = Hash(ValueId, Array(Tuple(BasicBlock, Int32, Value))).new
      @alias_map = Hash(ValueId, ValueId).new
      @no_alias_ids = Set(ValueId).new
    end

    # ═══════════════════════════════════════════════════════════════════════════
    # Main Entry Point
    # ═══════════════════════════════════════════════════════════════════════════

    def run(max_iters : Int32 = 10) : LTPPotential
      build_analysis_maps
      @final_potential = compute_frame_potential
      @potential_trace << @final_potential

      log "LTP Engine start: #{@final_potential}"

      while @iterations < max_iters
        # BR-1: Find trigger window
        window = find_window
        unless window
          log "  Iter #{@iterations}: No window found. Trying dual frame..."
          if try_dual_frame
            build_analysis_maps
            @final_potential = compute_frame_potential
            @potential_trace << @final_potential
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
          new_potential = compute_frame_potential

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
          @potential_trace << @final_potential
          log "    New potential: #{@final_potential}"
        else
          log "    No legal move found. Trying Collapse..."
          # Try Collapse (DCE) as fallback
          collapsed = try_collapse_move
          if collapsed > 0
            log "    Collapsed #{collapsed} dead instructions"
            build_analysis_maps
            @final_potential = compute_frame_potential
            @potential_trace << @final_potential
          else
            log "    Nothing to collapse. Trying dual frame..."
            if try_dual_frame
              build_analysis_maps
              @final_potential = compute_frame_potential
              @potential_trace << @final_potential
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
      # Dual frame: switch to escape-based analysis, then curvature/lifetime.
      return true if try_escape_frame
      return true if try_curvature_frame
      false
    end

    private def try_escape_frame : Bool
      pre = compute_frame_potential
      cf = ConstantFoldingPass.new(@function)
      folded = cf.run
      return false if folded == 0

      build_analysis_maps
      post = compute_frame_potential
      if post < pre
        log "    Dual frame (CF): folded #{folded} constants, Φ #{pre} → #{post}"
        return true
      end

      log "    Dual frame (CF) made changes but did not decrease Φ"
      false
    end

    private def try_curvature_frame : Bool
      pre = compute_curvature_potential

      rc = RCElisionPass.new(@function).run
      dce = DeadCodeEliminationPass.new(@function).run

      return false if rc == 0 && dce == 0

      build_analysis_maps
      post = compute_curvature_potential

      if post < pre
        log "    Dual frame (curvature): rc=#{rc}, dce=#{dce}, Φ #{pre} → #{post}"
        @frame_kind = FrameKind::Curvature
        return true
      end

      log "    Dual frame (curvature) made changes but did not decrease Φ"
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

    private def compute_frame_potential : LTPPotential
      case @frame_kind
      when FrameKind::Primary
        compute_ltp_potential
      when FrameKind::Curvature
        compute_curvature_potential
      else
        compute_ltp_potential
      end
    end

    # Curvature/Lifetime potential: adds corridor-length and lifetime pressure to P.
    private def compute_curvature_potential : LTPPotential
      base = compute_ltp_potential
      curvature = 0
      lifetime_pressure = 0

      @function.blocks.each do |block|
        block.instructions.each_with_index do |inst, idx|
          next unless inst.is_a?(RCIncrement)

          ptr = canonical_ptr(inst.ptr)
          next unless @no_alias_ids.includes?(ptr)

          exposure = (@use_map[ptr]?.try(&.size) || 0)
          window = Window.new(inst, block, idx, exposure, ptr)
          corridor = trace_corridor(window)

          case corridor.exit_type
          when CorridorExit::Elision
            if corridor.path.size > 2
              lifetime_pressure += corridor.path.size - 2
            end
          when CorridorExit::Escape, CorridorExit::Store
            curvature += 2
            lifetime_pressure += corridor.path.size
          when CorridorExit::Unknown
            curvature += 1
            lifetime_pressure += corridor.path.size
          end
        end
      end

      LTPPotential.new(
        base.window_overlap,
        base.tie_plateau,
        base.corner_mismatch + curvature + lifetime_pressure,
        base.area
      )
    end

    # Debug/testing hook: compute curvature potential with fresh maps.
    def curvature_potential : LTPPotential
      build_analysis_maps
      compute_curvature_potential
    end

    # Debug/testing hook: compute frame potential with fresh maps.
    def frame_potential : LTPPotential
      build_analysis_maps
      compute_frame_potential
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
      hops = 0

      while aliased = @alias_map[current]?
        break if aliased == current
        current = aliased
        hops += 1
        break if hops >= 64
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
