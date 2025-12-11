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

      instructions = block.instructions
      to_remove = Set(Int32).new

      instructions.each_with_index do |inst, idx|
        case inst
        when RCIncrement
          # Track this inc
          ptr = canonical_ptr(inst.ptr, alias_map)
          (pending_incs[ptr] ||= [] of Int32) << idx
          # Add a conservative MustAlias marker for identical ptrs within block
          @must_alias << {ptr, ptr}

        when RCDecrement
          # Check if we can elide with a pending inc
          ptr = canonical_ptr(inst.ptr, alias_map)
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
            inst.args.each { |arg| pending_incs.delete(arg) }
          when IndirectCall
            inst.args.each { |arg| pending_incs.delete(arg) }
            pending_incs.delete(inst.callee_ptr)
          end

        when Store
          # Stores may transfer ownership
          pending_incs.delete(inst.value)
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
      when Store, Free, RCDecrement
        true
      when Call, IndirectCall
        # Calls always have potential side effects
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

    def total : Int32
      rc_eliminated + dead_eliminated + constants_folded + copies_propagated
    end

    def to_s(io : IO)
      io << "Optimizations: "
      io << rc_eliminated << " RC ops, "
      io << dead_eliminated << " dead insts, "
      io << constants_folded << " constants folded"
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

      # Pass 3: Dead code elimination
      dce = DeadCodeEliminationPass.new(@function)
      @stats.dead_eliminated = dce.run

      # Pass 4: DCE again (RC elision may have created more dead code)
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
  end

  # Lexicographic potential for LTP-style local optimization.
  # Lower is better. Order: RC ops → instruction count → unsafe sites.
  record PotentialMetrics,
    rc_ops : Int32,
    instructions : Int32,
    unsafe_sites : Int32 do
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
    # Runs a small pipeline repeatedly while the potential decreases.
    def optimize_with_potential(max_iters : Int32 = 4) : Tuple(OptimizationStats, PotentialMetrics)
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

      {aggregate, last_potential}
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
end
