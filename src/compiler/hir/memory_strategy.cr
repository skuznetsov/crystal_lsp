# Memory Strategy Assignment for HIR
#
# Combines escape analysis + taint analysis to determine the optimal
# memory management strategy for each allocation.
#
# See docs/codegen_architecture.md Section 8 for full specification.

require "./hir"
require "./escape_analysis"
require "./taint_analysis"

module Crystal::HIR
  # Extend Allocate with strategy slot (only when memory_strategy is loaded)
  class Allocate
    property memory_strategy : MemoryStrategy? = nil
  end

  # Memory management strategy for an allocation
  enum MemoryStrategy
    Stack       # LLVM alloca, automatic cleanup
    Slab        # Fiber-local arena, bump allocation
    ARC         # Reference counting (non-atomic)
    AtomicARC   # Reference counting (atomic, for thread-shared)
    GC          # Garbage collected (Boehm GC)
    Unknown     # Not yet determined

    def to_s : String
      case self
      when Stack     then "stack"
      when Slab      then "slab"
      when ARC       then "arc"
      when AtomicARC then "atomic_arc"
      when GC        then "gc"
      else                "unknown"
      end
    end
  end

  # Configuration for memory strategy decisions
  struct MemoryConfig
    # Maximum size for stack allocation (bytes)
    getter stack_threshold : UInt32

    # Memory mode: conservative, balanced, aggressive
    getter mode : Mode

    enum Mode
      Conservative  # Prefer GC, maximize safety
      Balanced      # Auto-infer best strategy
      Aggressive    # Prefer stack/ARC, maximize speed
    end

    def initialize(
      @stack_threshold : UInt32 = 4096_u32,
      @mode : Mode = Mode::Balanced
    )
    end

    # Preset configurations
    def self.conservative
      new(stack_threshold: 256_u32, mode: Mode::Conservative)
    end

    def self.balanced
      new(stack_threshold: 4096_u32, mode: Mode::Balanced)
    end

    def self.aggressive
      new(stack_threshold: 16384_u32, mode: Mode::Aggressive)
    end
  end

  # Result of memory strategy analysis for a function
  class MemoryStrategyResult
    # Strategy assigned to each allocation (ValueId → Strategy)
    getter strategies : Hash(ValueId, MemoryStrategy)

    # Statistics
    getter stats : Stats

    struct Stats
      property stack_count : Int32 = 0
      property slab_count : Int32 = 0
      property arc_count : Int32 = 0
      property atomic_arc_count : Int32 = 0
      property gc_count : Int32 = 0

      def total : Int32
        stack_count + slab_count + arc_count + atomic_arc_count + gc_count
      end

      def to_s(io : IO)
        io << "Stack: " << stack_count
        io << ", Slab: " << slab_count
        io << ", ARC: " << arc_count
        io << ", AtomicARC: " << atomic_arc_count
        io << ", GC: " << gc_count
      end
    end

    def initialize
      @strategies = {} of ValueId => MemoryStrategy
      @stats = Stats.new
    end

    def add(value_id : ValueId, strategy : MemoryStrategy)
      @strategies[value_id] = strategy
      case strategy
      when .stack?      then @stats.stack_count += 1
      when .slab?       then @stats.slab_count += 1
      when .arc?        then @stats.arc_count += 1
      when .atomic_arc? then @stats.atomic_arc_count += 1
      when .gc?         then @stats.gc_count += 1
      end
    end

    def [](value_id : ValueId) : MemoryStrategy
      @strategies[value_id]? || MemoryStrategy::Unknown
    end
  end

  # Assigns memory strategies by combining escape and taint analysis
  class MemoryStrategyAssigner
    getter function : Function
    getter config : MemoryConfig
    getter result : MemoryStrategyResult

    # Cached analysis results
    @escape_summary : EscapeSummary?
    @taint_analyzer : TaintAnalyzer?
    @effect_provider : MethodEffectProvider?

    # Type size estimates (in bytes) - would come from type system in real impl
    TYPE_SIZE_ESTIMATES = {
      TypeRef::VOID    => 0_u32,
      TypeRef::NIL     => 0_u32,
      TypeRef::BOOL    => 1_u32,
      TypeRef::INT8    => 1_u32,
      TypeRef::INT16   => 2_u32,
      TypeRef::INT32   => 4_u32,
      TypeRef::INT64   => 8_u32,
      TypeRef::INT128  => 16_u32,
      TypeRef::UINT8   => 1_u32,
      TypeRef::UINT16  => 2_u32,
      TypeRef::UINT32  => 4_u32,
      TypeRef::UINT64  => 8_u32,
      TypeRef::UINT128 => 16_u32,
      TypeRef::FLOAT32 => 4_u32,
      TypeRef::FLOAT64 => 8_u32,
      TypeRef::CHAR    => 4_u32,
      TypeRef::STRING  => 24_u32,  # String header
      TypeRef::SYMBOL  => 8_u32,
    }

    # Default size for user types
    DEFAULT_TYPE_SIZE = 64_u32

    def initialize(
      @function : Function,
      @config : MemoryConfig = MemoryConfig.balanced,
      type_info : TypeInfoProvider? = nil,
      @effect_provider : MethodEffectProvider? = nil
    )
      @result = MemoryStrategyResult.new
      @type_info = type_info
    end

    # Run strategy assignment
    def assign : MemoryStrategyResult
      # Run escape analysis
      escape_analyzer = EscapeAnalyzer.new(@function, @type_info, @effect_provider)
      @escape_summary = escape_analyzer.analyze

      # Run taint analysis
      @taint_analyzer = TaintAnalyzer.new(@function, @type_info, @effect_provider)
      @taint_analyzer.not_nil!.analyze

      # Assign strategy to each allocation
      @function.blocks.each do |block|
        block.instructions.each do |value|
          if value.is_a?(Allocate)
            strategy = if explicit = value.memory_strategy
                         explicit == MemoryStrategy::Unknown ? determine_strategy(value) : explicit
                       else
                         determine_strategy(value)
                       end
            @result.add(value.id, strategy)
            # Propagate strategy to value for downstream lowering
            value.memory_strategy = strategy if value.responds_to?(:memory_strategy=)
          end
        end
      end

      @result
    end

    # Core decision logic
    private def determine_strategy(alloc : Allocate) : MemoryStrategy
      lifetime = alloc.lifetime
      taints = alloc.taints
      type_size = estimate_size(alloc.type)

      # Decision tree from architecture doc
      case @config.mode
      when .conservative?
        determine_conservative(lifetime, taints, type_size)
      when .balanced?
        determine_balanced(lifetime, taints, type_size)
      when .aggressive?
        determine_aggressive(lifetime, taints, type_size)
      else
        MemoryStrategy::GC
      end
    end

    # Conservative mode: prefer GC for safety
    private def determine_conservative(lifetime : LifetimeTag, taints : Taint, size : UInt32) : MemoryStrategy
      # Only stack-allocate primitives that definitely don't escape
      if lifetime.stack_local? && size <= 64 && taints == Taint::None
        return MemoryStrategy::Stack
      end

      # Everything else goes to GC
      MemoryStrategy::GC
    end

    # Balanced mode: auto-infer best strategy
    private def determine_balanced(lifetime : LifetimeTag, taints : Taint, size : UInt32) : MemoryStrategy
      # Rule 1: Cyclic or FFI → GC (safest)
      if taints.cyclic? || taints.ffi_exposed?
        return MemoryStrategy::GC
      end

      # Rule 2: ThreadShared → AtomicARC (check BEFORE stack allocation)
      # Thread-shared values need atomic ref counting even if small/local
      if taints.thread_shared?
        return MemoryStrategy::AtomicARC
      end

      # Rule 3: StackLocal + small size → Stack
      if lifetime.stack_local? && size <= @config.stack_threshold
        return MemoryStrategy::Stack
      end

      # Rule 4: StackLocal + larger → Slab (if in fiber context)
      # For now, we don't track fiber context, so skip Slab
      if lifetime.stack_local?
        # Could be Slab in fiber context, but fallback to Stack if small
        if size <= @config.stack_threshold
          return MemoryStrategy::Stack
        end
        # Large stack-local → ARC (will be freed when scope ends)
        return MemoryStrategy::ARC
      end

      # Rule 5: HeapEscape without cycles → ARC
      if lifetime.heap_escape? || lifetime.arg_escape?
        return MemoryStrategy::ARC
      end

      # Rule 6: GlobalEscape → depends on thread-safety
      if lifetime.global_escape?
        # Already checked thread_shared above, so just ARC here
        return MemoryStrategy::ARC
      end

      # Fallback
      MemoryStrategy::GC
    end

    # Aggressive mode: maximize performance
    private def determine_aggressive(lifetime : LifetimeTag, taints : Taint, size : UInt32) : MemoryStrategy
      # Cyclic or FFI still needs GC
      if taints.cyclic? || taints.ffi_exposed?
        return MemoryStrategy::GC
      end

      # ThreadShared needs atomic (check BEFORE stack allocation)
      if taints.thread_shared?
        return MemoryStrategy::AtomicARC
      end

      # Aggressively stack-allocate larger objects
      if lifetime.stack_local? && size <= @config.stack_threshold
        return MemoryStrategy::Stack
      end

      # Everything else → ARC (fast, deterministic)
      MemoryStrategy::ARC
    end

    private def estimate_size(type_ref : TypeRef) : UInt32
      TYPE_SIZE_ESTIMATES[type_ref]? || DEFAULT_TYPE_SIZE
    end

    # =========================================================================
    # PUBLIC HELPERS
    # =========================================================================

    # Get strategy for a specific allocation
    def strategy_for(value_id : ValueId) : MemoryStrategy
      @result[value_id]
    end

    # Check if any allocations use GC
    def uses_gc? : Bool
      @result.stats.gc_count > 0
    end

    # Check if any allocations use ARC
    def uses_arc? : Bool
      @result.stats.arc_count > 0 || @result.stats.atomic_arc_count > 0
    end

    # Get summary string
    def summary : String
      String.build do |io|
        io << "Memory Strategy Summary:\n"
        io << "  " << @result.stats << "\n"
        io << "  Total allocations: " << @result.stats.total << "\n"

        if @result.stats.total > 0
          stack_pct = (@result.stats.stack_count * 100) // @result.stats.total
          arc_pct = ((@result.stats.arc_count + @result.stats.atomic_arc_count) * 100) // @result.stats.total
          gc_pct = (@result.stats.gc_count * 100) // @result.stats.total

          io << "  Stack: " << stack_pct << "%"
          io << ", ARC: " << arc_pct << "%"
          io << ", GC: " << gc_pct << "%\n"
        end
      end
    end
  end

  # Convenience method on Function
  class Function
    def assign_memory_strategies(
      config : MemoryConfig = MemoryConfig.balanced,
      type_info : TypeInfoProvider? = nil,
      effect_provider : MethodEffectProvider? = nil
    ) : MemoryStrategyResult
      assigner = MemoryStrategyAssigner.new(self, config, type_info, effect_provider)
      assigner.assign
    end
  end
end
