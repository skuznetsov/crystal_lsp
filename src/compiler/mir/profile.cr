# Profile-Guided Optimization for MIR
#
# Collects and uses runtime statistics for optimization decisions:
#
# Usage:
#   1. Compile with --mm=profile-gen to instrument code
#   2. Run program to collect profile data (writes to .crystal_profile)
#   3. Recompile with --mm=profile-use to optimize based on data
#
# Collected statistics:
#
# Memory (per allocation site):
#   - Allocation count, lifetime, escape frequency
#   - Thread sharing frequency
#   - Reference count patterns
#
# Control Flow (per branch):
#   - Branch taken/not-taken counts
#   - Branch probability for layout optimization
#
# Loops (per loop header):
#   - Total iterations, average iterations per entry
#   - Trip count distribution (for unroll decisions)
#
# Calls (per call site):
#   - Call frequency (for inline decisions)
#   - Hot/cold path identification

require "./mir"

module Crystal::MIR
  # ═══════════════════════════════════════════════════════════════════════════
  # PROFILE DATA STRUCTURES
  # ═══════════════════════════════════════════════════════════════════════════

  # Statistics for a single allocation site
  class AllocationSiteStats
    # Unique identifier for this allocation site
    property site_id : UInt64 = 0

    # Function containing this allocation
    property function_name : String = ""

    # Source location (for debugging)
    property source_file : String = ""
    property source_line : UInt32 = 0

    # Allocation counts
    property alloc_count : UInt64 = 0
    property current_live : UInt64 = 0
    property max_live : UInt64 = 0

    # Lifetime statistics (in "ticks" - could be instructions or time)
    property total_lifetime : UInt64 = 0
    property max_lifetime : UInt64 = 0
    property min_lifetime : UInt64 = UInt64::MAX

    # Escape statistics
    property escape_count : UInt64 = 0       # Times value escaped scope
    property return_escape_count : UInt64 = 0
    property closure_escape_count : UInt64 = 0
    property container_escape_count : UInt64 = 0

    # Thread sharing
    property thread_share_count : UInt64 = 0

    # Reference counting operations
    property total_rc_inc : UInt64 = 0
    property total_rc_dec : UInt64 = 0
    property max_refcount : UInt64 = 0

    # Computed metrics
    def avg_lifetime : Float64
      return 0.0 if alloc_count == 0
      total_lifetime.to_f64 / alloc_count.to_f64
    end

    def escape_rate : Float64
      return 0.0 if alloc_count == 0
      escape_count.to_f64 / alloc_count.to_f64
    end

    def thread_share_rate : Float64
      return 0.0 if alloc_count == 0
      thread_share_count.to_f64 / alloc_count.to_f64
    end

    def avg_rc_ops : Float64
      return 0.0 if alloc_count == 0
      (total_rc_inc + total_rc_dec).to_f64 / alloc_count.to_f64
    end

    # Recommended memory strategy based on profile data
    def recommended_strategy : MemoryStrategy
      # High thread sharing → AtomicARC or GC
      if thread_share_rate > 0.1
        return MemoryStrategy::AtomicARC
      end

      # Never escapes and short lifetime → Stack
      if escape_rate < 0.01 && avg_lifetime < 1000
        return MemoryStrategy::Stack
      end

      # Low escape rate, moderate lifetime → Slab
      if escape_rate < 0.05 && avg_lifetime < 10000
        return MemoryStrategy::Slab
      end

      # High RC operations but no thread sharing → ARC
      if avg_rc_ops > 2 && thread_share_rate < 0.01
        return MemoryStrategy::ARC
      end

      # Default to GC for complex cases
      MemoryStrategy::GC
    end

    def confidence : Float64
      # More samples = higher confidence
      # 1000+ samples = high confidence
      Math.min(1.0, alloc_count.to_f64 / 1000.0)
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # BRANCH STATISTICS (for block layout optimization)
  # ═══════════════════════════════════════════════════════════════════════════

  class BranchStats
    # Unique identifier for this branch (function_id << 32 | block_id)
    property branch_id : UInt64 = 0

    # Function and block info
    property function_name : String = ""
    property block_id : UInt32 = 0

    # Branch execution counts
    property taken_count : UInt64 = 0      # Times branch was taken (then-branch)
    property not_taken_count : UInt64 = 0  # Times branch was not taken (else-branch)

    # Computed metrics
    def total_executions : UInt64
      taken_count + not_taken_count
    end

    def taken_probability : Float64
      return 0.5 if total_executions == 0
      taken_count.to_f64 / total_executions.to_f64
    end

    def not_taken_probability : Float64
      1.0 - taken_probability
    end

    # Is this a biased branch? (>80% one way)
    def biased? : Bool
      total_executions > 100 && (taken_probability > 0.8 || taken_probability < 0.2)
    end

    # Should we layout the hot path first?
    def hot_path_is_taken? : Bool
      taken_probability > 0.5
    end

    def confidence : Float64
      Math.min(1.0, total_executions.to_f64 / 1000.0)
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # LOOP STATISTICS (for unrolling decisions)
  # ═══════════════════════════════════════════════════════════════════════════

  class LoopStats
    # Unique identifier for this loop header
    property loop_id : UInt64 = 0

    # Function and block info
    property function_name : String = ""
    property header_block_id : UInt32 = 0

    # Loop execution counts
    property entry_count : UInt64 = 0       # Times loop was entered
    property iteration_count : UInt64 = 0   # Total iterations across all entries
    property exit_count : UInt64 = 0        # Times loop was exited

    # Trip count distribution (for unroll factor selection)
    property trip_counts : Hash(UInt64, UInt64) = {} of UInt64 => UInt64  # trip_count → frequency

    # Min/max observed trip counts
    property min_trip_count : UInt64 = UInt64::MAX
    property max_trip_count : UInt64 = 0

    def avg_trip_count : Float64
      return 0.0 if entry_count == 0
      iteration_count.to_f64 / entry_count.to_f64
    end

    # Is this loop worth unrolling?
    def should_unroll? : Bool
      return false if entry_count < 100  # Not enough samples
      return false if avg_trip_count < 2  # Too few iterations

      # Unroll if average trip count is small and consistent
      avg_trip_count >= 2 && avg_trip_count <= 16 && max_trip_count <= 32
    end

    # Suggested unroll factor (2, 4, 8, or 0 for no unroll)
    def suggested_unroll_factor : UInt32
      return 0_u32 unless should_unroll?

      avg = avg_trip_count
      if avg <= 4
        2_u32
      elsif avg <= 8
        4_u32
      else
        8_u32
      end
    end

    # Is this a hot loop? (>10% of total iterations)
    def hot? : Bool
      entry_count > 1000 && avg_trip_count > 4
    end

    def confidence : Float64
      Math.min(1.0, entry_count.to_f64 / 1000.0)
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # CALL SITE STATISTICS (for inline decisions)
  # ═══════════════════════════════════════════════════════════════════════════

  class CallSiteStats
    # Unique identifier for this call site
    property call_site_id : UInt64 = 0

    # Caller info
    property caller_function : String = ""
    property call_block_id : UInt32 = 0

    # Callee info
    property callee_function : String = ""
    property is_virtual : Bool = false

    # Call frequency
    property call_count : UInt64 = 0

    # For virtual calls: dispatch target distribution
    property target_distribution : Hash(String, UInt64) = {} of String => UInt64

    # Timing (optional, if available)
    property total_cycles : UInt64 = 0  # CPU cycles spent in callee

    def avg_cycles_per_call : Float64
      return 0.0 if call_count == 0
      total_cycles.to_f64 / call_count.to_f64
    end

    # Is this a hot call site?
    def hot? : Bool
      call_count > 1000
    end

    # Should we inline this call?
    def should_inline? : Bool
      return false unless hot?
      return false if is_virtual && target_distribution.size > 2

      # Inline if frequently called and not too expensive
      call_count > 100 && avg_cycles_per_call < 1000
    end

    # For virtual calls: is there a dominant target?
    def dominant_target : {String, Float64}?
      return nil if target_distribution.empty?

      total = target_distribution.values.sum
      return nil if total == 0

      max_target = target_distribution.max_by { |_, count| count }
      probability = max_target[1].to_f64 / total.to_f64

      {max_target[0], probability} if probability >= 0.8
    end

    def confidence : Float64
      Math.min(1.0, call_count.to_f64 / 1000.0)
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # BASIC BLOCK STATISTICS (for hot/cold path separation)
  # ═══════════════════════════════════════════════════════════════════════════

  class BlockStats
    property block_id : UInt64 = 0
    property function_name : String = ""
    property execution_count : UInt64 = 0

    def hot?(threshold : UInt64 = 10000) : Bool
      execution_count > threshold
    end

    def cold?(threshold : UInt64 = 100) : Bool
      execution_count < threshold
    end
  end

  # Aggregated profile data for entire program
  class ProfileData
    MAGIC = "CRPF"  # Crystal Profile
    VERSION = 3_u32  # v3: full serialization of branches/loops/blocks/call_sites

    # Memory statistics
    getter sites : Hash(UInt64, AllocationSiteStats)
    property total_allocations : UInt64

    # Control flow statistics
    getter branches : Hash(UInt64, BranchStats)
    getter loops : Hash(UInt64, LoopStats)
    getter blocks : Hash(UInt64, BlockStats)

    # Call statistics
    getter call_sites : Hash(UInt64, CallSiteStats)

    # Metadata
    property profile_runs : UInt32
    property last_updated : Time

    def initialize
      @sites = {} of UInt64 => AllocationSiteStats
      @branches = {} of UInt64 => BranchStats
      @loops = {} of UInt64 => LoopStats
      @blocks = {} of UInt64 => BlockStats
      @call_sites = {} of UInt64 => CallSiteStats
      @total_allocations = 0_u64
      @profile_runs = 0_u32
      @last_updated = Time.utc
    end

    def add_site(site_id : UInt64, function_name : String, source_file : String = "", source_line : UInt32 = 0) : AllocationSiteStats
      unless @sites.has_key?(site_id)
        site = AllocationSiteStats.new
        site.site_id = site_id
        site.function_name = function_name
        site.source_file = source_file
        site.source_line = source_line
        @sites[site_id] = site
      end
      @sites[site_id]
    end

    def get_site(site_id : UInt64) : AllocationSiteStats?
      @sites[site_id]?
    end

    def record_allocation(site_id : UInt64)
      if site = @sites[site_id]?
        site.alloc_count += 1
        site.current_live += 1
        site.max_live = Math.max(site.max_live, site.current_live)
        @total_allocations += 1
      end
    end

    def record_deallocation(site_id : UInt64, lifetime : UInt64)
      if site = @sites[site_id]?
        site.current_live -= 1 if site.current_live > 0
        site.total_lifetime += lifetime
        site.max_lifetime = Math.max(site.max_lifetime, lifetime)
        site.min_lifetime = Math.min(site.min_lifetime, lifetime)
      end
    end

    def record_escape(site_id : UInt64, kind : EscapeKind)
      if site = @sites[site_id]?
        site.escape_count += 1
        case kind
        when .return_escape?    then site.return_escape_count += 1
        when .closure_escape?   then site.closure_escape_count += 1
        when .container_escape? then site.container_escape_count += 1
        end
      end
    end

    def record_thread_share(site_id : UInt64)
      if site = @sites[site_id]?
        site.thread_share_count += 1
      end
    end

    def record_rc_inc(site_id : UInt64)
      if site = @sites[site_id]?
        site.total_rc_inc += 1
      end
    end

    def record_rc_dec(site_id : UInt64)
      if site = @sites[site_id]?
        site.total_rc_dec += 1
      end
    end

    # =========================================================================
    # BRANCH RECORDING
    # =========================================================================

    def add_branch(branch_id : UInt64, function_name : String, block_id : UInt32) : BranchStats
      @branches[branch_id] ||= BranchStats.new.tap do |b|
        b.branch_id = branch_id
        b.function_name = function_name
        b.block_id = block_id
      end
      @branches[branch_id]
    end

    def get_branch(branch_id : UInt64) : BranchStats?
      @branches[branch_id]?
    end

    def record_branch_taken(branch_id : UInt64)
      if branch = @branches[branch_id]?
        branch.taken_count += 1
      end
    end

    def record_branch_not_taken(branch_id : UInt64)
      if branch = @branches[branch_id]?
        branch.not_taken_count += 1
      end
    end

    # =========================================================================
    # LOOP RECORDING
    # =========================================================================

    def add_loop(loop_id : UInt64, function_name : String, header_block_id : UInt32) : LoopStats
      @loops[loop_id] ||= LoopStats.new.tap do |l|
        l.loop_id = loop_id
        l.function_name = function_name
        l.header_block_id = header_block_id
      end
      @loops[loop_id]
    end

    def get_loop(loop_id : UInt64) : LoopStats?
      @loops[loop_id]?
    end

    def record_loop_entry(loop_id : UInt64)
      if loop = @loops[loop_id]?
        loop.entry_count += 1
      end
    end

    def record_loop_iteration(loop_id : UInt64)
      if loop = @loops[loop_id]?
        loop.iteration_count += 1
      end
    end

    def record_loop_exit(loop_id : UInt64, trip_count : UInt64)
      if loop = @loops[loop_id]?
        loop.exit_count += 1
        loop.min_trip_count = Math.min(loop.min_trip_count, trip_count)
        loop.max_trip_count = Math.max(loop.max_trip_count, trip_count)
        loop.trip_counts[trip_count] = (loop.trip_counts[trip_count]? || 0_u64) + 1
      end
    end

    # =========================================================================
    # BLOCK RECORDING
    # =========================================================================

    def add_block(block_id : UInt64, function_name : String) : BlockStats
      @blocks[block_id] ||= BlockStats.new.tap do |b|
        b.block_id = block_id
        b.function_name = function_name
      end
      @blocks[block_id]
    end

    def record_block_execution(block_id : UInt64)
      if block = @blocks[block_id]?
        block.execution_count += 1
      end
    end

    # =========================================================================
    # CALL SITE RECORDING
    # =========================================================================

    def add_call_site(call_site_id : UInt64, caller_function : String, callee_function : String, is_virtual : Bool = false) : CallSiteStats
      @call_sites[call_site_id] ||= CallSiteStats.new.tap do |c|
        c.call_site_id = call_site_id
        c.caller_function = caller_function
        c.callee_function = callee_function
        c.is_virtual = is_virtual
      end
      @call_sites[call_site_id]
    end

    def get_call_site(call_site_id : UInt64) : CallSiteStats?
      @call_sites[call_site_id]?
    end

    def record_call(call_site_id : UInt64, actual_target : String? = nil, cycles : UInt64 = 0)
      if cs = @call_sites[call_site_id]?
        cs.call_count += 1
        cs.total_cycles += cycles

        # Track virtual dispatch targets
        if cs.is_virtual && actual_target
          cs.target_distribution[actual_target] = (cs.target_distribution[actual_target]? || 0_u64) + 1
        end
      end
    end

    def merge(other : ProfileData)
      other.sites.each do |site_id, other_site|
        if site = @sites[site_id]?
          # Merge statistics
          site.alloc_count += other_site.alloc_count
          site.max_live = Math.max(site.max_live, other_site.max_live)
          site.total_lifetime += other_site.total_lifetime
          site.max_lifetime = Math.max(site.max_lifetime, other_site.max_lifetime)
          site.min_lifetime = Math.min(site.min_lifetime, other_site.min_lifetime)
          site.escape_count += other_site.escape_count
          site.return_escape_count += other_site.return_escape_count
          site.closure_escape_count += other_site.closure_escape_count
          site.container_escape_count += other_site.container_escape_count
          site.thread_share_count += other_site.thread_share_count
          site.total_rc_inc += other_site.total_rc_inc
          site.total_rc_dec += other_site.total_rc_dec
          site.max_refcount = Math.max(site.max_refcount, other_site.max_refcount)
        else
          @sites[site_id] = other_site
        end
      end
      @total_allocations += other.total_allocations
      @profile_runs += other.profile_runs
      @last_updated = Time.utc
    end

    def finalize_run
      @profile_runs += 1
      @last_updated = Time.utc
    end

    # =========================================================================
    # SERIALIZATION
    # =========================================================================

    def to_binary(io : IO)
      # Header
      io.write(MAGIC.to_slice)
      io.write_bytes(VERSION, IO::ByteFormat::LittleEndian)
      io.write_bytes(@total_allocations, IO::ByteFormat::LittleEndian)
      io.write_bytes(@profile_runs, IO::ByteFormat::LittleEndian)
      io.write_bytes(@last_updated.to_unix, IO::ByteFormat::LittleEndian)

      # Sites
      io.write_bytes(@sites.size.to_u32, IO::ByteFormat::LittleEndian)
      @sites.each_value { |site| write_site(io, site) }

      # Branches
      io.write_bytes(@branches.size.to_u32, IO::ByteFormat::LittleEndian)
      @branches.each_value { |b| write_branch(io, b) }

      # Loops
      io.write_bytes(@loops.size.to_u32, IO::ByteFormat::LittleEndian)
      @loops.each_value { |l| write_loop(io, l) }

      # Blocks
      io.write_bytes(@blocks.size.to_u32, IO::ByteFormat::LittleEndian)
      @blocks.each_value { |b| write_block(io, b) }

      # Call sites
      io.write_bytes(@call_sites.size.to_u32, IO::ByteFormat::LittleEndian)
      @call_sites.each_value { |c| write_call_site(io, c) }
    end

    private def write_site(io : IO, site : AllocationSiteStats)
      io.write_bytes(site.site_id, IO::ByteFormat::LittleEndian)

      # Strings with length prefix
      write_string(io, site.function_name)
      write_string(io, site.source_file)
      io.write_bytes(site.source_line, IO::ByteFormat::LittleEndian)

      # Stats
      io.write_bytes(site.alloc_count, IO::ByteFormat::LittleEndian)
      io.write_bytes(site.max_live, IO::ByteFormat::LittleEndian)
      io.write_bytes(site.total_lifetime, IO::ByteFormat::LittleEndian)
      io.write_bytes(site.max_lifetime, IO::ByteFormat::LittleEndian)
      io.write_bytes(site.min_lifetime, IO::ByteFormat::LittleEndian)
      io.write_bytes(site.escape_count, IO::ByteFormat::LittleEndian)
      io.write_bytes(site.return_escape_count, IO::ByteFormat::LittleEndian)
      io.write_bytes(site.closure_escape_count, IO::ByteFormat::LittleEndian)
      io.write_bytes(site.container_escape_count, IO::ByteFormat::LittleEndian)
      io.write_bytes(site.thread_share_count, IO::ByteFormat::LittleEndian)
      io.write_bytes(site.total_rc_inc, IO::ByteFormat::LittleEndian)
      io.write_bytes(site.total_rc_dec, IO::ByteFormat::LittleEndian)
      io.write_bytes(site.max_refcount, IO::ByteFormat::LittleEndian)
    end

    private def write_string(io : IO, str : String)
      io.write_bytes(str.bytesize.to_u32, IO::ByteFormat::LittleEndian)
      io.write(str.to_slice)
    end

    private def write_branch(io : IO, b : BranchStats)
      io.write_bytes(b.branch_id, IO::ByteFormat::LittleEndian)
      write_string(io, b.function_name)
      io.write_bytes(b.block_id, IO::ByteFormat::LittleEndian)
      io.write_bytes(b.taken_count, IO::ByteFormat::LittleEndian)
      io.write_bytes(b.not_taken_count, IO::ByteFormat::LittleEndian)
    end

    private def write_loop(io : IO, l : LoopStats)
      io.write_bytes(l.loop_id, IO::ByteFormat::LittleEndian)
      write_string(io, l.function_name)
      io.write_bytes(l.header_block_id, IO::ByteFormat::LittleEndian)
      io.write_bytes(l.entry_count, IO::ByteFormat::LittleEndian)
      io.write_bytes(l.iteration_count, IO::ByteFormat::LittleEndian)
      io.write_bytes(l.exit_count, IO::ByteFormat::LittleEndian)
      io.write_bytes(l.min_trip_count, IO::ByteFormat::LittleEndian)
      io.write_bytes(l.max_trip_count, IO::ByteFormat::LittleEndian)
      # Trip count distribution
      io.write_bytes(l.trip_counts.size.to_u32, IO::ByteFormat::LittleEndian)
      l.trip_counts.each do |trip, freq|
        io.write_bytes(trip, IO::ByteFormat::LittleEndian)
        io.write_bytes(freq, IO::ByteFormat::LittleEndian)
      end
    end

    private def write_block(io : IO, b : BlockStats)
      io.write_bytes(b.block_id, IO::ByteFormat::LittleEndian)
      write_string(io, b.function_name)
      io.write_bytes(b.execution_count, IO::ByteFormat::LittleEndian)
    end

    private def write_call_site(io : IO, c : CallSiteStats)
      io.write_bytes(c.call_site_id, IO::ByteFormat::LittleEndian)
      write_string(io, c.caller_function)
      io.write_bytes(c.call_block_id, IO::ByteFormat::LittleEndian)
      write_string(io, c.callee_function)
      io.write_bytes(c.is_virtual ? 1_u8 : 0_u8, IO::ByteFormat::LittleEndian)
      io.write_bytes(c.call_count, IO::ByteFormat::LittleEndian)
      io.write_bytes(c.total_cycles, IO::ByteFormat::LittleEndian)
      # Target distribution
      io.write_bytes(c.target_distribution.size.to_u32, IO::ByteFormat::LittleEndian)
      c.target_distribution.each do |target, count|
        write_string(io, target)
        io.write_bytes(count, IO::ByteFormat::LittleEndian)
      end
    end

    def self.from_binary(io : IO) : ProfileData?
      # Check magic
      magic = Bytes.new(4)
      return nil unless io.read_fully?(magic)
      return nil unless String.new(magic) == MAGIC

      # Version check
      version = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian)
      return nil unless version == VERSION

      data = ProfileData.new
      data.total_allocations = io.read_bytes(UInt64, IO::ByteFormat::LittleEndian)
      data.profile_runs = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian)
      unix_time = io.read_bytes(Int64, IO::ByteFormat::LittleEndian)
      data.last_updated = Time.unix(unix_time)

      # Sites
      site_count = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian)
      site_count.times do
        site = read_site(io)
        data.sites[site.site_id] = site if site
      end

      # Branches
      branch_count = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian)
      branch_count.times do
        b = read_branch(io)
        data.branches[b.branch_id] = b if b
      end

      # Loops
      loop_count = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian)
      loop_count.times do
        l = read_loop(io)
        data.loops[l.loop_id] = l if l
      end

      # Blocks
      block_count = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian)
      block_count.times do
        b = read_block(io)
        data.blocks[b.block_id] = b if b
      end

      # Call sites
      call_site_count = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian)
      call_site_count.times do
        c = read_call_site(io)
        data.call_sites[c.call_site_id] = c if c
      end

      data
    rescue
      nil
    end

    private def self.read_site(io : IO) : AllocationSiteStats?
      site = AllocationSiteStats.new
      site.site_id = io.read_bytes(UInt64, IO::ByteFormat::LittleEndian)

      site.function_name = read_string(io)
      site.source_file = read_string(io)
      site.source_line = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian)

      site.alloc_count = io.read_bytes(UInt64, IO::ByteFormat::LittleEndian)
      site.max_live = io.read_bytes(UInt64, IO::ByteFormat::LittleEndian)
      site.total_lifetime = io.read_bytes(UInt64, IO::ByteFormat::LittleEndian)
      site.max_lifetime = io.read_bytes(UInt64, IO::ByteFormat::LittleEndian)
      site.min_lifetime = io.read_bytes(UInt64, IO::ByteFormat::LittleEndian)
      site.escape_count = io.read_bytes(UInt64, IO::ByteFormat::LittleEndian)
      site.return_escape_count = io.read_bytes(UInt64, IO::ByteFormat::LittleEndian)
      site.closure_escape_count = io.read_bytes(UInt64, IO::ByteFormat::LittleEndian)
      site.container_escape_count = io.read_bytes(UInt64, IO::ByteFormat::LittleEndian)
      site.thread_share_count = io.read_bytes(UInt64, IO::ByteFormat::LittleEndian)
      site.total_rc_inc = io.read_bytes(UInt64, IO::ByteFormat::LittleEndian)
      site.total_rc_dec = io.read_bytes(UInt64, IO::ByteFormat::LittleEndian)
      site.max_refcount = io.read_bytes(UInt64, IO::ByteFormat::LittleEndian)

      site
    rescue
      nil
    end

    private def self.read_string(io : IO) : String
      len = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian)
      bytes = Bytes.new(len)
      io.read_fully(bytes)
      String.new(bytes)
    end

    private def self.read_branch(io : IO) : BranchStats?
      b = BranchStats.new
      b.branch_id = io.read_bytes(UInt64, IO::ByteFormat::LittleEndian)
      b.function_name = read_string(io)
      b.block_id = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian)
      b.taken_count = io.read_bytes(UInt64, IO::ByteFormat::LittleEndian)
      b.not_taken_count = io.read_bytes(UInt64, IO::ByteFormat::LittleEndian)
      b
    rescue
      nil
    end

    private def self.read_loop(io : IO) : LoopStats?
      l = LoopStats.new
      l.loop_id = io.read_bytes(UInt64, IO::ByteFormat::LittleEndian)
      l.function_name = read_string(io)
      l.header_block_id = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian)
      l.entry_count = io.read_bytes(UInt64, IO::ByteFormat::LittleEndian)
      l.iteration_count = io.read_bytes(UInt64, IO::ByteFormat::LittleEndian)
      l.exit_count = io.read_bytes(UInt64, IO::ByteFormat::LittleEndian)
      l.min_trip_count = io.read_bytes(UInt64, IO::ByteFormat::LittleEndian)
      l.max_trip_count = io.read_bytes(UInt64, IO::ByteFormat::LittleEndian)
      trip_count_size = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian)
      trip_count_size.times do
        trip = io.read_bytes(UInt64, IO::ByteFormat::LittleEndian)
        freq = io.read_bytes(UInt64, IO::ByteFormat::LittleEndian)
        l.trip_counts[trip] = freq
      end
      l
    rescue
      nil
    end

    private def self.read_block(io : IO) : BlockStats?
      b = BlockStats.new
      b.block_id = io.read_bytes(UInt64, IO::ByteFormat::LittleEndian)
      b.function_name = read_string(io)
      b.execution_count = io.read_bytes(UInt64, IO::ByteFormat::LittleEndian)
      b
    rescue
      nil
    end

    private def self.read_call_site(io : IO) : CallSiteStats?
      c = CallSiteStats.new
      c.call_site_id = io.read_bytes(UInt64, IO::ByteFormat::LittleEndian)
      c.caller_function = read_string(io)
      c.call_block_id = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian)
      c.callee_function = read_string(io)
      c.is_virtual = io.read_bytes(UInt8, IO::ByteFormat::LittleEndian) != 0
      c.call_count = io.read_bytes(UInt64, IO::ByteFormat::LittleEndian)
      c.total_cycles = io.read_bytes(UInt64, IO::ByteFormat::LittleEndian)
      target_count = io.read_bytes(UInt32, IO::ByteFormat::LittleEndian)
      target_count.times do
        target = read_string(io)
        count = io.read_bytes(UInt64, IO::ByteFormat::LittleEndian)
        c.target_distribution[target] = count
      end
      c
    rescue
      nil
    end

    # =========================================================================
    # FILE I/O
    # =========================================================================

    DEFAULT_PATH = ".crystal_profile"

    def save(path : String = DEFAULT_PATH)
      File.open(path, "wb") do |f|
        to_binary(f)
      end
    end

    def self.load(path : String = DEFAULT_PATH) : ProfileData?
      return nil unless File.exists?(path)
      File.open(path, "rb") do |f|
        from_binary(f)
      end
    end

    # =========================================================================
    # REPORTS
    # =========================================================================

    def summary : String
      String.build do |io|
        io << "Profile Summary\n"
        io << "═══════════════════════════════════════════════════════════\n"
        io << "Total allocations: " << @total_allocations << "\n"
        io << "Profile runs: " << @profile_runs << "\n"
        io << "Allocation sites: " << @sites.size << "\n"
        io << "Last updated: " << @last_updated << "\n"
        io << "\n"

        # Top allocation sites by count
        io << "Top 10 Allocation Sites by Count:\n"
        io << "───────────────────────────────────────────────────────────\n"
        top_sites = @sites.values.sort_by { |s| -s.alloc_count.to_i64 }.first(10)
        top_sites.each_with_index do |site, idx|
          io << "#{idx + 1}. #{site.function_name} (#{site.source_file}:#{site.source_line})\n"
          io << "   Allocations: #{site.alloc_count}, Escape: #{(site.escape_rate * 100).round(1)}%\n"
          io << "   Avg lifetime: #{site.avg_lifetime.round(1)}, Recommended: #{site.recommended_strategy}\n"
        end

        # Hot spots (high escape rate)
        io << "\nHigh Escape Rate Sites (>50%):\n"
        io << "───────────────────────────────────────────────────────────\n"
        escapers = @sites.values.select { |s| s.escape_rate > 0.5 && s.alloc_count > 100 }
        escapers.sort_by { |s| -s.escape_rate }.first(5).each do |site|
          io << "• #{site.function_name}: #{(site.escape_rate * 100).round(1)}% escape\n"
        end

        # Thread-shared sites
        io << "\nThread-Shared Sites:\n"
        io << "───────────────────────────────────────────────────────────\n"
        shared = @sites.values.select { |s| s.thread_share_rate > 0.01 }
        shared.sort_by { |s| -s.thread_share_rate }.first(5).each do |site|
          io << "• #{site.function_name}: #{(site.thread_share_rate * 100).round(1)}% shared\n"
        end
      end
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # ESCAPE KINDS (for profile tracking)
  # ═══════════════════════════════════════════════════════════════════════════

  enum EscapeKind
    ReturnEscape
    ClosureEscape
    ContainerEscape
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # INSTRUMENTATION INSTRUCTIONS
  # ═══════════════════════════════════════════════════════════════════════════

  # Profile instrumentation call - inserted for --mm=profile-gen
  class ProfileInstrument < Value
    enum Kind
      Alloc         # Record allocation
      Dealloc       # Record deallocation with lifetime
      Escape        # Record escape event
      ThreadShare   # Record thread sharing
      RCInc         # Record RC increment
      RCDec         # Record RC decrement
    end

    getter kind : Kind
    getter site_id : UInt64
    getter extra : ValueId?  # For lifetime tracking

    def initialize(id : ValueId, @kind : Kind, @site_id : UInt64, @extra : ValueId? = nil)
      super(id, TypeRef::VOID)
    end

    def operands : Array(ValueId)
      if e = @extra
        [e]
      else
        [] of ValueId
      end
    end

    def to_s(io : IO) : Nil
      io << "%" << @id << " = profile." << @kind.to_s.downcase
      io << " site=" << @site_id
      if e = @extra
        io << ", %" << e
      end
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # INSTRUMENTATION PASS
  # ═══════════════════════════════════════════════════════════════════════════

  # Inserts profile instrumentation into MIR
  class ProfileInstrumentationPass
    getter function : Function
    getter profile_data : ProfileData
    getter instrumented : Int32 = 0

    @site_counter : UInt64 = 0

    def initialize(@function : Function, @profile_data : ProfileData)
    end

    def run : Int32
      @instrumented = 0

      @function.blocks.each do |block|
        instrument_block(block)
      end

      @instrumented
    end

    private def instrument_block(block : BasicBlock)
      new_instructions = [] of Value

      block.instructions.each do |inst|
        case inst
        when Alloc
          site_id = next_site_id
          # Register site in profile data
          @profile_data.add_site(site_id, @function.name)

          # Insert instrumentation before alloc
          profile_inst = ProfileInstrument.new(
            @function.next_value_id,
            ProfileInstrument::Kind::Alloc,
            site_id
          )
          new_instructions << profile_inst
          @instrumented += 1

          # Tag the allocation with site_id for later tracking
          # (In real impl, would add metadata to the allocation)

        when RCIncrement
          # Track RC operations
          # Need to correlate with allocation site - simplified for now
          # In real impl, would trace back to allocation

        when RCDecrement
          # Track RC operations and potential deallocation
          # Simplified - real impl needs lifetime tracking
        end

        new_instructions << inst
      end

      block.instructions.clear
      new_instructions.each { |i| block.add(i) }
    end

    private def next_site_id : UInt64
      id = @site_counter
      @site_counter += 1
      # Include function ID for uniqueness across functions
      (@function.id.to_u64 << 32) | id
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # PROFILE-GUIDED OPTIMIZATION
  # ═══════════════════════════════════════════════════════════════════════════

  # Uses profile data to improve memory strategy decisions
  class ProfileGuidedOptimizer
    getter profile_data : ProfileData
    getter optimizations_applied : Int32 = 0

    # Confidence threshold for using profile data
    CONFIDENCE_THRESHOLD = 0.5

    # Minimum samples needed to trust profile
    MIN_SAMPLES = 100

    def initialize(@profile_data : ProfileData)
    end

    # Get recommended strategy for an allocation site
    def recommended_strategy(site_id : UInt64, default : MemoryStrategy) : MemoryStrategy
      site = @profile_data.get_site(site_id)
      return default unless site

      # Not enough data
      return default if site.alloc_count < MIN_SAMPLES

      # Low confidence
      return default if site.confidence < CONFIDENCE_THRESHOLD

      site.recommended_strategy
    end

    # Optimize a function using profile data
    def optimize_function(func : Function) : Int32
      @optimizations_applied = 0

      func.blocks.each do |block|
        block.instructions.each do |inst|
          case inst
          when Alloc
            # Try to find profile data for this allocation
            # In real impl, would use metadata to map to site_id
            # For now, just demonstrate the pattern
          end
        end
      end

      @optimizations_applied
    end

    # Summary of profile-guided decisions
    def optimization_summary : String
      String.build do |io|
        io << "Profile-Guided Optimization Summary\n"
        io << "═══════════════════════════════════════════════════════════\n"

        strategies = Hash(MemoryStrategy, Int32).new(0)
        high_confidence = 0
        low_confidence = 0

        @profile_data.sites.each_value do |site|
          next if site.alloc_count < MIN_SAMPLES

          if site.confidence >= CONFIDENCE_THRESHOLD
            high_confidence += 1
            strategies[site.recommended_strategy] += 1
          else
            low_confidence += 1
          end
        end

        io << "High confidence sites: #{high_confidence}\n"
        io << "Low confidence sites: #{low_confidence}\n"
        io << "\nRecommended strategies:\n"
        strategies.each do |strategy, count|
          io << "  #{strategy}: #{count} sites\n"
        end
      end
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # COMPILER FLAGS
  # ═══════════════════════════════════════════════════════════════════════════

  enum ProfileMode
    None       # No profiling
    Generate   # --mm=profile-gen: Instrument code to collect profile
    Use        # --mm=profile-use: Use profile data for optimization
    Both       # --mm=profile: Generate and use if profile exists
  end

  struct CompilerFlags
    property mm_mode : String = "balanced"
    property profile_mode : ProfileMode = ProfileMode::None
    property profile_path : String = ProfileData::DEFAULT_PATH

    def self.from_args(args : Array(String)) : CompilerFlags
      flags = CompilerFlags.new

      args.each do |arg|
        case arg
        when "--mm=conservative"
          flags.mm_mode = "conservative"
        when "--mm=balanced"
          flags.mm_mode = "balanced"
        when "--mm=aggressive"
          flags.mm_mode = "aggressive"
        when "--mm=profile-gen"
          flags.profile_mode = ProfileMode::Generate
        when "--mm=profile-use"
          flags.profile_mode = ProfileMode::Use
        when "--mm=profile"
          flags.profile_mode = ProfileMode::Both
        when .starts_with?("--profile-path=")
          flags.profile_path = arg.sub("--profile-path=", "")
        end
      end

      flags
    end

    def should_instrument? : Bool
      profile_mode.generate? || profile_mode.both?
    end

    def should_use_profile? : Bool
      profile_mode.use? || profile_mode.both?
    end
  end

  # Convenience method on Function
  class Function
    def instrument_for_profile(profile_data : ProfileData) : Int32
      pass = ProfileInstrumentationPass.new(self, profile_data)
      pass.run
    end
  end
end
