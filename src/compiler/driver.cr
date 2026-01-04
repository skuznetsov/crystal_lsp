# Crystal v2 Compiler Driver
#
# End-to-end compilation: Crystal source → HIR → MIR → LLVM IR → binary
#
# Usage:
#   crystal run src/compiler/driver.cr -- input.cr -o output

require "digest/sha256"
require "./frontend/lexer"
require "./frontend/parser"
require "./hir/hir"
require "./hir/ast_to_hir"
require "./hir/escape_analysis"
require "./hir/memory_strategy"
require "./hir/class_info_type_provider"
require "./mir/mir"
require "./mir/optimizations"
require "./mir/hir_to_mir"
require "./mir/llvm_backend"
require "./lsp/ast_cache"
require "../runtime"

module Crystal::V2
  class CompilerDriver
    # Standard library path - relative to compiler source
    STDLIB_PATH = File.expand_path("../stdlib", File.dirname(__FILE__))

    property input_file : String = ""
    property output_file : String = "a.out"
    property emit_llvm : Bool = false
    property emit_hir : Bool = false
    property emit_mir : Bool = false
    property verbose : Bool = false
    property optimize : Int32 = 0
    property no_prelude : Bool = false  # Skip automatic prelude loading
    property ltp_opt : Bool = true
    property lto : Bool = false
    property pgo_generate : Bool = false
    property pgo_profile : String = ""
    property mm_mode : String = "balanced"
    property mm_stack_threshold : UInt32 = 4096_u32
    property no_gc : Bool = false
    property link_libraries : Array(String) = [] of String
    @trace_driver : Bool = false

    def initialize
      @trace_driver = ENV.has_key?("CRYSTAL_V2_DRIVER_TRACE")
    end

    def parse_args(args : Array(String))
      i = 0
      while i < args.size
        arg = args[i]
        case arg
        when "-o"
          i += 1
          @output_file = args[i]? || "a.out"
        when "--emit-llvm"
          @emit_llvm = true
        when "--emit-hir"
          @emit_hir = true
        when "--emit-mir"
          @emit_mir = true
        when "-v", "--verbose"
          @verbose = true
        when "-O0"
          @optimize = 0
        when "-O1"
          @optimize = 1
        when "-O2"
          @optimize = 2
        when "-O3"
          @optimize = 3
        when "--no-prelude"
          @no_prelude = true
        when "--no-ltp"
          @ltp_opt = false
        when "--no-gc"
          @no_gc = true
        when .starts_with?("--mm=")
          @mm_mode = arg.sub("--mm=", "")
        when "--mm-stack-threshold"
          i += 1
          value = args[i]?
          threshold = value.try(&.to_u32?)
          unless threshold
            STDERR.puts "Error: --mm-stack-threshold expects an integer"
            exit 1
          end
          @mm_stack_threshold = threshold
        when "--lto"
          @lto = true
        when "--pgo-gen"
          @pgo_generate = true
        when "--pgo-use"
          i += 1
          @pgo_profile = args[i]? || ""
        when /^-/
          STDERR.puts "Unknown option: #{arg}"
          exit 1
        else
          @input_file = arg
        end
        i += 1
      end

      if @input_file.empty?
        STDERR.puts "Usage: driver <input.cr> [-o output] [--emit-llvm] [--no-prelude] [--no-ltp] [--lto] [--pgo-gen|--pgo-use FILE] [-v]"
        exit 1
      end
      unless valid_mm_mode?(@mm_mode)
        STDERR.puts "Error: Unknown --mm mode: #{@mm_mode}"
        exit 1
      end

      if @pgo_generate && !@pgo_profile.empty?
        STDERR.puts "Error: --pgo-gen and --pgo-use are mutually exclusive"
        exit 1
      end

      if !@pgo_profile.empty? && !File.exists?(@pgo_profile)
        STDERR.puts "Error: PGO profile not found: #{@pgo_profile}"
        exit 1
      end
    end

    def compile
      STDERR.puts "[USE_LIBICONV] debug env active" if ENV["DEBUG_USE_LIBICONV"]?
      trace_driver("[DRIVER_TRACE] compile() started")
      log "=== Crystal v2 Compiler ==="
      log "Input: #{@input_file}"
      log "Output: #{@output_file}"

      # Step 1: Parse source (with require support)
      log "\n[1/5] Parsing..."
      trace_driver("[DRIVER_TRACE] [1/5] Parsing...")

      # Track loaded files to avoid duplicates
      loaded_files = Set(String).new

      # Parse all files recursively (require support)
      all_arenas = [] of Tuple(CrystalV2::Compiler::Frontend::ArenaLike, Array(CrystalV2::Compiler::Frontend::ExprId), String, String)

      # Load prelude first (unless --no-prelude)
      unless @no_prelude
        prelude_path = File.join(STDLIB_PATH, "prelude.cr")
        if File.exists?(prelude_path)
          trace_driver("[DRIVER_TRACE] Loading prelude...")
          log "  Loading prelude: #{prelude_path}"
          parse_file_recursive(prelude_path, all_arenas, loaded_files)
          trace_driver("[DRIVER_TRACE] Prelude loaded, all_arenas.size=#{all_arenas.size}")
        end
      end

      # Parse user's input file
      trace_driver("[DRIVER_TRACE] Parsing user file...")
      parse_file_recursive(@input_file, all_arenas, loaded_files)
      trace_driver("[DRIVER_TRACE] User file parsed")

      total_exprs = all_arenas.sum { |t| t[1].size }
      log "  Files: #{all_arenas.size}, Expressions: #{total_exprs}"

      if ENV.has_key?("CRYSTAL_V2_STOP_AFTER_PARSE")
        trace_driver("[DRIVER_TRACE] stop after parse (CRYSTAL_V2_STOP_AFTER_PARSE)")
        return
      end

      # Step 2: Lower to HIR
      trace_driver("[DRIVER_TRACE] [2/5] Lowering to HIR...")
      log "\n[2/5] Lowering to HIR..."

      # Use first file's arena for the converter (it merges all)
      first_arena = all_arenas[0][0]
      sources_by_arena = {} of CrystalV2::Compiler::Frontend::ArenaLike => String
      paths_by_arena = {} of CrystalV2::Compiler::Frontend::ArenaLike => String
      all_arenas.each do |arena, _exprs, path, source|
        sources_by_arena[arena] = source
        paths_by_arena[arena] = path
      end
      hir_converter = HIR::AstToHir.new(first_arena, @input_file, sources_by_arena, paths_by_arena)

      # Collect all DefNodes, ClassNodes, ModuleNodes, EnumNodes, MacroDefNodes, and top-level expressions
      def_nodes = [] of Tuple(CrystalV2::Compiler::Frontend::DefNode, CrystalV2::Compiler::Frontend::ArenaLike)
      class_nodes = [] of Tuple(CrystalV2::Compiler::Frontend::ClassNode, CrystalV2::Compiler::Frontend::ArenaLike)
      module_nodes = [] of Tuple(CrystalV2::Compiler::Frontend::ModuleNode, CrystalV2::Compiler::Frontend::ArenaLike)
      enum_nodes = [] of Tuple(CrystalV2::Compiler::Frontend::EnumNode, CrystalV2::Compiler::Frontend::ArenaLike)
      macro_nodes = [] of Tuple(CrystalV2::Compiler::Frontend::MacroDefNode, CrystalV2::Compiler::Frontend::ArenaLike)
      alias_nodes = [] of Tuple(CrystalV2::Compiler::Frontend::AliasNode, CrystalV2::Compiler::Frontend::ArenaLike)
      lib_nodes = [] of Tuple(
        CrystalV2::Compiler::Frontend::LibNode,
        CrystalV2::Compiler::Frontend::ArenaLike,
        Array(Tuple(CrystalV2::Compiler::Frontend::AnnotationNode, CrystalV2::Compiler::Frontend::ArenaLike))
      )
      constant_exprs = [] of Tuple(CrystalV2::Compiler::Frontend::ExprId, CrystalV2::Compiler::Frontend::ArenaLike)
      # Top-level expressions that form the main function
      main_exprs = [] of Tuple(CrystalV2::Compiler::Frontend::ExprId, CrystalV2::Compiler::Frontend::ArenaLike)
      acyclic_types = Set(String).new
      debug_hir_timings = ENV.has_key?("DEBUG_HIR_TIMINGS")

      flags = CrystalV2::Runtime.target_flags
      collect_start = Time.monotonic if debug_hir_timings
      all_arenas.each do |arena, exprs, file_path, source|
        pending_annotations = [] of Tuple(CrystalV2::Compiler::Frontend::AnnotationNode, CrystalV2::Compiler::Frontend::ArenaLike)
        exprs.each do |expr_id|
            collect_top_level_nodes(
              arena,
              expr_id,
              def_nodes,
              class_nodes,
              module_nodes,
              enum_nodes,
              macro_nodes,
              alias_nodes,
              lib_nodes,
              constant_exprs,
              main_exprs,
              pending_annotations,
              acyclic_types,
              flags,
              sources_by_arena,
              source
            )
          end
        end
      if debug_hir_timings && collect_start
        elapsed = (Time.monotonic - collect_start).total_milliseconds
        STDERR.puts "[HIR_TIMING] collect_top_level_nodes #{elapsed.round(1)}ms"
      end

      top_level_type_names = Set(String).new
      class_nodes.each { |node, _| top_level_type_names.add(String.new(node.name)) }
      module_nodes.each { |node, _| top_level_type_names.add(String.new(node.name)) }
      enum_nodes.each { |node, _| top_level_type_names.add(String.new(node.name)) }
      alias_nodes.each { |node, _| top_level_type_names.add(String.new(node.name)) }
      lib_nodes.each { |node, _, _| top_level_type_names.add(String.new(node.name)) }
      hir_converter.seed_top_level_type_names(top_level_type_names)

      # Three-pass approach:
      # Pass 1: Register all enums, modules, class types and their methods
      pass1_start = Time.monotonic if debug_hir_timings
      if ENV.has_key?("DEBUG_NESTED_CLASS")
        STDERR.puts "[DEBUG_DRIVER] class_nodes: #{class_nodes.size}, module_nodes: #{module_nodes.size}"
        module_nodes.each do |module_node, arena|
          name = String.new(module_node.name)
          if name == "IO" || name.includes?("FileDescriptor")
            STDERR.puts "[DEBUG_DRIVER] Module: #{name}"
          end
        end
        class_nodes.each do |class_node, arena|
          name = String.new(class_node.name)
          if name == "IO" || name.includes?("FileDescriptor")
            STDERR.puts "[DEBUG_DRIVER] Class: #{name}"
          end
        end
      end
      enum_nodes.each do |enum_node, arena|
        hir_converter.arena = arena
        hir_converter.register_enum(enum_node)
      end
      lib_nodes.each do |lib_node, arena, annotations|
        hir_converter.arena = arena
        hir_converter.register_lib(lib_node, annotations)
      end
      macro_nodes.each do |macro_node, arena|
        hir_converter.arena = arena
        hir_converter.register_macro(macro_node)
      end
      alias_nodes.each do |alias_node, arena|
        hir_converter.arena = arena
        hir_converter.register_alias(alias_node)
      end
      trace_driver("[DRIVER_TRACE] total module_nodes to register: #{module_nodes.size}")
      module_nodes.each do |module_node, arena|
        mod_name = String.new(module_node.name)
        if mod_name.includes?("Thread") || mod_name.includes?("System") || mod_name.includes?("Crystal")
          trace_driver("[DRIVER_TRACE] register module: #{mod_name}")
        end
        hir_converter.arena = arena
        hir_converter.register_module(module_node)
      end
      class_nodes.each do |class_node, arena|
        hir_converter.arena = arena
        hir_converter.register_class(class_node)
      end
      if debug_hir_timings && pass1_start
        elapsed = (Time.monotonic - pass1_start).total_milliseconds
        STDERR.puts "[HIR_TIMING] register_types #{elapsed.round(1)}ms"
      end

      constant_exprs.each do |expr_id, arena|
        hir_converter.arena = arena
        node = arena[expr_id]
        case node
        when CrystalV2::Compiler::Frontend::ConstantNode
          hir_converter.register_constant(node)
        when CrystalV2::Compiler::Frontend::AssignNode
          target = arena[node.target]
          if target.is_a?(CrystalV2::Compiler::Frontend::ConstantNode)
            hir_converter.register_constant_value(String.new(target.name), node.value, arena)
          end
        end
      end

      # Flush pending monomorphizations now that all templates are registered
      puts "  Flushing pending monomorphizations..." if @verbose
      mono_start = Time.monotonic if debug_hir_timings
      hir_converter.flush_pending_monomorphizations
      if debug_hir_timings && mono_start
        elapsed = (Time.monotonic - mono_start).total_milliseconds
        STDERR.puts "[HIR_TIMING] flush_pending_monomorphizations #{elapsed.round(1)}ms"
      end

      # Refresh union descriptors now that all types are registered
      hir_converter.refresh_union_descriptors

      # Pass 2: Register all top-level function signatures
      pass2_start = Time.monotonic if debug_hir_timings
      def_nodes.each do |node, arena|
        hir_converter.arena = arena
        hir_converter.register_function(node)
      end
      if debug_hir_timings && pass2_start
        elapsed = (Time.monotonic - pass2_start).total_milliseconds
        STDERR.puts "[HIR_TIMING] register_functions #{elapsed.round(1)}ms"
      end

      # Pass 3: Lower all function and method bodies
      func_count = 0
      pass3_start = Time.monotonic if debug_hir_timings
      slow_ms = ENV["DEBUG_HIR_SLOW_MS"]?.try(&.to_f)
      unless ENV.has_key?("CRYSTAL_V2_LAZY_HIR")
        module_nodes.each do |module_node, arena|
          hir_converter.arena = arena
          if slow_ms
            start = Time.monotonic
            hir_converter.lower_module(module_node)
            elapsed = (Time.monotonic - start).total_milliseconds
            if elapsed >= slow_ms
              name = String.new(module_node.name)
              source_path = paths_by_arena[arena]?
              STDERR.puts "[HIR_SLOW] module #{name} #{elapsed.round(1)}ms#{source_path ? " file=#{source_path}" : ""}"
            end
          else
            hir_converter.lower_module(module_node)
          end
          func_count += 1
        end
        class_nodes.each do |class_node, arena|
          hir_converter.arena = arena
          if slow_ms
            start = Time.monotonic
            hir_converter.lower_class(class_node)
            elapsed = (Time.monotonic - start).total_milliseconds
            if elapsed >= slow_ms
              name = String.new(class_node.name)
              source_path = paths_by_arena[arena]?
              STDERR.puts "[HIR_SLOW] class #{name} #{elapsed.round(1)}ms#{source_path ? " file=#{source_path}" : ""}"
            end
          else
            hir_converter.lower_class(class_node)
          end
          func_count += 1
        end
      else
        trace_driver("[DRIVER_TRACE] CRYSTAL_V2_LAZY_HIR=1; skipping eager module/class lowering")
      end
      if debug_hir_timings && pass3_start
        elapsed = (Time.monotonic - pass3_start).total_milliseconds
        STDERR.puts "[HIR_TIMING] lower_modules_classes #{elapsed.round(1)}ms"
      end

      # Create synthetic main function from top-level expressions (or user-defined main)
      STDERR.puts "[HIR_TIMING] start lower_main" if debug_hir_timings
      main_start = Time.monotonic if debug_hir_timings
      if main_exprs.size > 0
        hir_converter.lower_main(main_exprs)
        func_count += 1
      elsif main_def = def_nodes.find { |(n, _)| String.new(n.name) == "main" && !(n.receiver.try { |recv| String.new(recv) == HIR::AstToHir::FUN_DEF_RECEIVER } || false) }
        hir_converter.arena = main_def[1]
        hir_converter.lower_main_from_def(main_def[0])
        func_count += 1
      end
      if debug_hir_timings && main_start
        elapsed = (Time.monotonic - main_start).total_milliseconds
        STDERR.puts "[HIR_TIMING] lower_main #{elapsed.round(1)}ms"
      end

      # Ensure top-level `fun main` is lowered as an entrypoint when present.
      STDERR.puts "[HIR_TIMING] start lower_fun_main" if debug_hir_timings
      fun_main_start = Time.monotonic if debug_hir_timings
      if fun_main = def_nodes.find { |(n, _)| n.receiver.try { |recv| String.new(recv) == HIR::AstToHir::FUN_DEF_RECEIVER } || false }
        hir_converter.arena = fun_main[1]
        hir_converter.lower_def(fun_main[0])
        func_count += 1
      end
      if debug_hir_timings && fun_main_start
        elapsed = (Time.monotonic - fun_main_start).total_milliseconds
        STDERR.puts "[HIR_TIMING] lower_fun_main #{elapsed.round(1)}ms"
      end

      # Lower remaining top-level function bodies after main to allow
      # call-site types to guide inference for used functions.
      STDERR.puts "[HIR_TIMING] start lower_defs" if debug_hir_timings
      defs_start = Time.monotonic if debug_hir_timings
      def_nodes.each do |node, arena|
        hir_converter.arena = arena
        if slow_ms
          start = Time.monotonic
          hir_converter.lower_def(node)
          elapsed = (Time.monotonic - start).total_milliseconds
          if elapsed >= slow_ms
            name = String.new(node.name)
            source_path = paths_by_arena[arena]?
            STDERR.puts "[HIR_SLOW] #{name} #{elapsed.round(1)}ms#{source_path ? " file=#{source_path}" : ""}"
          end
        else
          hir_converter.lower_def(node)
        end
        func_count += 1
      end
      if debug_hir_timings && defs_start
        elapsed = (Time.monotonic - defs_start).total_milliseconds
        STDERR.puts "[HIR_TIMING] lower_defs #{elapsed.round(1)}ms"
      end

      hir_module = hir_converter.module
      @link_libraries = hir_module.link_libraries.dup
      log "  Functions: #{hir_module.functions.size}"

      if @emit_hir
        hir_file = @output_file.gsub(/\.[^.]+$/, ".hir")
        File.write(hir_file, hir_module.to_s)
        log "  Wrote: #{hir_file}"
      end

      if ENV.has_key?("CRYSTAL_V2_STOP_AFTER_HIR")
        trace_driver("[DRIVER_TRACE] stop after HIR (CRYSTAL_V2_STOP_AFTER_HIR)")
        return
      end

      # Step 3: Escape analysis
      log "\n[3/5] Escape analysis..."
      mm_config = memory_config
      type_provider = HIR::ClassInfoTypeProvider.new(hir_module, hir_converter.class_info, acyclic_types)
      total_ms_stats = HIR::MemoryStrategyResult::Stats.new
      gc_functions = [] of Tuple(String, Int32)
      gc_details = [] of String
      total_gc = 0
      hir_module.functions.each do |func|
        escape = HIR::EscapeAnalyzer.new(func, type_provider, hir_module)
        escape.analyze
        # Memory strategy (includes taint/thread_shared)
        ms = HIR::MemoryStrategyAssigner.new(func, mm_config, type_provider, hir_module)
        result = ms.assign
        stats = result.stats
        total_ms_stats.stack_count += stats.stack_count
        total_ms_stats.slab_count += stats.slab_count
        total_ms_stats.arc_count += stats.arc_count
        total_ms_stats.atomic_arc_count += stats.atomic_arc_count
        total_ms_stats.gc_count += stats.gc_count
        if @no_gc && stats.gc_count > 0
          total_gc += stats.gc_count
          gc_functions << {func.name, stats.gc_count}
          gc_details.concat(gc_allocation_details(func, result, hir_module, mm_config))
        end
      end
      log "  Memory strategies: #{format_memory_stats(total_ms_stats)}"
      if @no_gc && total_gc > 0
        STDERR.puts "error: --no-gc requested but #{total_gc} allocation(s) require GC"
        gc_functions.sort_by { |(_, count)| -count }.first(10).each do |(name, count)|
          STDERR.puts "  #{name}: #{count}"
        end
        gc_details.first(20).each do |detail|
          STDERR.puts "  #{detail}"
        end
        if gc_details.size > 20
          STDERR.puts "  ... #{gc_details.size - 20} more GC allocation(s)"
        end
        exit 1
      end

      # Step 4: Lower to MIR
      log "\n[4/5] Lowering to MIR..."
      mir_lowering = MIR::HIRToMIRLowering.new(hir_module)

      # Collect class variables as globals
      globals = [] of Tuple(String, HIR::TypeRef, Int64?)
      hir_converter.class_info.each do |class_name, info|
        info.class_vars.each do |cvar|
          global_name = "#{class_name}_#{cvar.name}"
          globals << {global_name, cvar.type, cvar.initial_value}
        end
      end
      mir_lowering.register_globals(globals)

      # Register union types from AST conversion
      mir_lowering.register_union_types(hir_converter.union_descriptors)

      # Register class/struct types with their fields
      mir_lowering.register_class_types(hir_converter.class_info)

      mir_module = mir_lowering.lower
      log "  Functions: #{mir_module.functions.size}"

      # Step 4.1: Local MIR optimizations with monotone potential
      log "  Optimizing MIR (local LTP loop)..."
      mir_module.functions.each do |func|
        if @ltp_opt
          stats, ltp_potential = func.optimize_with_potential
          log "    #{@verbose ? "#{func.name} -> #{stats.total} changes, potential #{ltp_potential}" : "optimized #{func.name}"}" if @verbose
        else
          stats = func.optimize
          log "    #{@verbose ? "#{func.name} -> #{stats.total} changes (legacy)" : "optimized #{func.name} (legacy)"}" if @verbose
        end
      end

      if @emit_mir
        mir_file = @output_file.gsub(/\.[^.]+$/, ".mir")
        File.write(mir_file, mir_module.to_s)
        log "  Wrote: #{mir_file}"
      end

      if ENV.has_key?("CRYSTAL_V2_STOP_AFTER_MIR")
        trace_driver("[DRIVER_TRACE] stop after MIR (CRYSTAL_V2_STOP_AFTER_MIR)")
        return
      end

      # Step 5: Generate LLVM IR
      log "\n[5/5] Generating LLVM IR..."
      llvm_gen = MIR::LLVMIRGenerator.new(mir_module)
      llvm_gen.emit_type_metadata = true
      llvm_gen.reachability = true  # Only emit reachable functions from main
      llvm_ir = llvm_gen.generate
      log "  LLVM IR size: #{llvm_ir.size} bytes"

      ll_file = if @output_file =~ /\.[^.]+$/
                   @output_file.gsub(/\.[^.]+$/, ".ll")
                 else
                   @output_file + ".ll"
                 end
      File.write(ll_file, llvm_ir)
      log "  Wrote: #{ll_file}"

      if @emit_llvm
        puts llvm_ir
        return
      end

      # Compile to binary
      log "\n[6/5] Compiling to binary..."
      compile_llvm_ir(ll_file)

      log "\n=== Compilation complete ==="
      log "Output: #{@output_file}"
    end

    private def compile_llvm_ir(ll_file : String)
      obj_file = ll_file.gsub(/\.ll$/, ".o")

      # Run llc to compile LLVM IR to object file
      opt_flag = case @optimize
                 when 0 then "-O0"
                 when 1 then "-O1"
                 when 2 then "-O2"
                 else        "-O3"
                 end

      use_clang_link = @lto || @pgo_generate || !@pgo_profile.empty?

      # Find runtime stub relative to driver location
      runtime_dir = File.dirname(File.dirname(__FILE__))
      runtime_stub = File.join(runtime_dir, "..", "runtime_stub.o")

      # Compile runtime stub if needed
      runtime_src = runtime_stub.gsub(/\.o$/, ".c")
      if File.exists?(runtime_src) && (!File.exists?(runtime_stub) ||
         File.info(runtime_src).modification_time > File.info(runtime_stub).modification_time)
        `cc -c #{runtime_src} -o #{runtime_stub} 2>&1`
      end

      if use_clang_link
        pgo_flags = [] of String
        if @pgo_generate
          pgo_flags << "-fprofile-instr-generate"
        elsif !@pgo_profile.empty?
          pgo_flags << "-fprofile-instr-use=#{@pgo_profile}"
        end

        lto_flag = @lto ? "-flto" : ""
        link_flags = build_link_flags
        link_flags_str = link_flags.join(" ")
        clang_cmd = "clang #{opt_flag} #{lto_flag} #{pgo_flags.join(" ")} -o #{@output_file} #{ll_file}"
        clang_cmd += " #{runtime_stub}" if File.exists?(runtime_stub)
        clang_cmd += " #{link_flags_str}" unless link_flags_str.empty?
        clang_cmd += " 2>&1"

        log "  $ #{clang_cmd}"
        clang_result = `#{clang_cmd}`
        unless $?.success?
          STDERR.puts "clang failed:"
          STDERR.puts clang_result
          exit 1
        end
      else
        llc_cmd = "llc #{opt_flag} -filetype=obj -o #{obj_file} #{ll_file} 2>&1"
        log "  $ #{llc_cmd}"
        llc_result = `#{llc_cmd}`
        unless $?.success?
          STDERR.puts "llc failed:"
          STDERR.puts llc_result
          exit 1
        end

        link_objs = [obj_file]
        link_objs << runtime_stub if File.exists?(runtime_stub)

        link_flags = build_link_flags
        link_flags_str = link_flags.join(" ")
        link_cmd = "cc -o #{@output_file} #{link_objs.join(" ")}"
        link_cmd += " #{link_flags_str}" unless link_flags_str.empty?
        link_cmd += " 2>&1"
        log "  $ #{link_cmd}"
        link_result = `#{link_cmd}`
        unless $?.success?
          STDERR.puts "Linking failed:"
          STDERR.puts link_result
          exit 1
        end

        # Clean up intermediate files
        File.delete(obj_file) if File.exists?(obj_file)
      end
    end

    private def build_link_flags : Array(String)
      flags = [] of String
      seen = Set(String).new
      @link_libraries.each do |entry|
        key, value = parse_link_entry(entry)
        case key
        when "pkg_config"
          next if value.empty?
          cmd = "pkg-config --libs #{value} 2>/dev/null"
          output = `#{cmd}`.strip
          if $?.success? && !output.empty?
            output.split.each do |flag|
              next if seen.includes?(flag)
              flags << flag
              seen << flag
            end
          else
            log "  Warning: pkg-config failed for #{value}"
          end
        when "framework"
          next if value.empty?
          marker = "-framework #{value}"
          next if seen.includes?(marker)
          flags << "-framework"
          flags << value
          seen << marker
        when "dll"
          unless CrystalV2::Runtime.target_flags.includes?("win32") || CrystalV2::Runtime.target_flags.includes?("windows")
            next
          end
          next if value.empty?
          flag = "-l#{value}"
          next if seen.includes?(flag)
          flags << flag
          seen << flag
        else
          lib_name = value.empty? ? entry : value
          next if lib_name.empty?
          flag = "-l#{lib_name}"
          next if seen.includes?(flag)
          flags << flag
          seen << flag
        end
      end
      flags
    end

    private def parse_link_entry(entry : String) : {String, String}
      if idx = entry.index(':')
        key = entry[0, idx]
        value = entry[(idx + 1)..-1]
        {key, value}
      else
        {"", entry}
      end
    end

    private def annotation_name_from_expr(
      arena : CrystalV2::Compiler::Frontend::ArenaLike,
      expr_id : CrystalV2::Compiler::Frontend::ExprId
    ) : String
      node = arena[expr_id]
      case node
      when CrystalV2::Compiler::Frontend::IdentifierNode
        String.new(node.name)
      when CrystalV2::Compiler::Frontend::PathNode
        annotation_name_from_expr(arena, node.right)
      else
        "Unknown"
      end
    end

    private def log(msg : String)
      puts msg if @verbose
    end

    private def valid_mm_mode?(mode : String) : Bool
      mode == "conservative" || mode == "balanced" || mode == "aggressive"
    end

    private def memory_config : HIR::MemoryConfig
      mode = case @mm_mode
             when "conservative" then HIR::MemoryConfig::Mode::Conservative
             when "balanced" then HIR::MemoryConfig::Mode::Balanced
             when "aggressive" then HIR::MemoryConfig::Mode::Aggressive
             else
               HIR::MemoryConfig::Mode::Balanced
             end
      HIR::MemoryConfig.new(stack_threshold: @mm_stack_threshold, mode: mode)
    end

    private def format_memory_stats(stats : HIR::MemoryStrategyResult::Stats) : String
      String.build { |io| stats.to_s(io) }
    end

    private def gc_allocation_details(
      func : HIR::Function,
      result : HIR::MemoryStrategyResult,
      hir_module : HIR::Module,
      config : HIR::MemoryConfig
    ) : Array(String)
      details = [] of String
      func.blocks.each do |block|
        block.instructions.each do |value|
          next unless value.is_a?(HIR::Allocate)
          next unless result[value.id].gc?
          type_name = type_name_for(value.type, hir_module)
          reason = gc_reason_for(value, config)
          location = func.value_location(value.id)
          loc_prefix = location ? "#{location} " : ""
          details << "#{func.name}: #{loc_prefix}alloc %#{value.id} #{type_name} reason=#{reason} lifetime=#{value.lifetime} taints=#{value.taints}"
        end
      end
      details
    end

    private def gc_reason_for(alloc : HIR::Allocate, config : HIR::MemoryConfig) : String
      reasons = [] of String
      reasons << "cyclic" if alloc.taints.cyclic?
      reasons << "ffi_exposed" if alloc.taints.ffi_exposed?
      reasons << "thread_shared" if alloc.taints.thread_shared?
      if reasons.empty?
        if config.mode.conservative?
          reasons << "conservative"
        else
          reasons << "lifetime=#{alloc.lifetime}"
        end
      end
      reasons.join("+")
    end

    private def type_name_for(type_ref : HIR::TypeRef, hir_module : HIR::Module) : String
      if desc = hir_module.get_type_descriptor(type_ref)
        return desc.name
      end
      case type_ref
      when HIR::TypeRef::VOID then "Void"
      when HIR::TypeRef::NIL then "Nil"
      when HIR::TypeRef::BOOL then "Bool"
      when HIR::TypeRef::INT8 then "Int8"
      when HIR::TypeRef::INT16 then "Int16"
      when HIR::TypeRef::INT32 then "Int32"
      when HIR::TypeRef::INT64 then "Int64"
      when HIR::TypeRef::INT128 then "Int128"
      when HIR::TypeRef::UINT8 then "UInt8"
      when HIR::TypeRef::UINT16 then "UInt16"
      when HIR::TypeRef::UINT32 then "UInt32"
      when HIR::TypeRef::UINT64 then "UInt64"
      when HIR::TypeRef::UINT128 then "UInt128"
      when HIR::TypeRef::FLOAT32 then "Float32"
      when HIR::TypeRef::FLOAT64 then "Float64"
      when HIR::TypeRef::CHAR then "Char"
      when HIR::TypeRef::STRING then "String"
      when HIR::TypeRef::SYMBOL then "Symbol"
      when HIR::TypeRef::POINTER then "Pointer"
      else
        "TypeRef(#{type_ref.id})"
      end
    end

    private def trace_driver(message : String) : Nil
      return unless @trace_driver
      STDERR.puts message
      STDERR.flush
    end

    private def ast_cache_enabled? : Bool
      ENV["CRYSTAL_V2_AST_CACHE"]? != "0"
    end

    private def digest_string(value : String) : String
      digest = Digest::SHA256.new
      digest.update(value.to_slice)
      digest.hexfinal
    end

    private def require_cache_path(file_path : String) : String
      cache_dir = ENV["XDG_CACHE_HOME"]? || File.join(ENV["HOME"]? || "/tmp", ".cache")
      hash = digest_string("v3:#{file_path}")
      File.join(cache_dir, "crystal_v2", "requires", "#{hash}.req")
    end

    private def load_require_cache(file_path : String) : Array(String)?
      cache_path = require_cache_path(file_path)
      return nil unless File.exists?(cache_path)
      return nil unless File.exists?(file_path)

      cache_mtime = File.info(cache_path).modification_time
      file_mtime = File.info(file_path).modification_time
      return nil if file_mtime > cache_mtime

      lines = File.read_lines(cache_path)
      lines.reject { |line| line.empty? || line.starts_with?("#") }
    rescue ex
      nil
    end

    private def save_require_cache(file_path : String, requires : Array(String)) : Nil
      unique = requires.uniq
      return if unique.empty?

      cache_path = require_cache_path(file_path)
      Dir.mkdir_p(File.dirname(cache_path))
      File.write(cache_path, unique.join("\n") + "\n")
    rescue ex
      nil
    end

    # Recursively parse files, handling require statements
    private def parse_file_recursive(
      file_path : String,
      results : Array(Tuple(CrystalV2::Compiler::Frontend::ArenaLike, Array(CrystalV2::Compiler::Frontend::ExprId), String, String)),
      loaded : Set(String)
    )
      # Normalize and resolve path
      abs_path = File.expand_path(file_path)

      # Skip if already loaded
      return if loaded.includes?(abs_path)
      loaded << abs_path
      trace_driver("[DRIVER_TRACE] parse_file: #{abs_path}")

      unless File.exists?(abs_path)
        STDERR.puts "File not found: #{abs_path}"
        return
      end

      source = File.read(abs_path)
      if ast_cache_enabled?
        if cached = CrystalV2::Compiler::LSP::AstCache.load(abs_path)
          arena = cached.arena
          exprs = cached.roots
          base_dir = File.dirname(abs_path)
          if cached_requires = load_require_cache(abs_path)
            cached_requires.each do |req_path|
              parse_file_recursive(req_path, results, loaded)
            end
          else
            requires = [] of String
            exprs.each do |expr_id|
              process_require_node(arena, expr_id, base_dir, results, loaded, requires)
            end
            save_require_cache(abs_path, requires)
          end
          results << {arena, exprs, abs_path, source}
          trace_driver("[DRIVER_TRACE] AST cache hit: #{abs_path}")
          return
        end
      end

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program
      arena = program.arena
      exprs = program.roots
      trace_driver("[DRIVER_TRACE] parsed #{abs_path}, #{exprs.size} exprs")

      # Extract require paths and process them first (dependencies before dependents)
      base_dir = File.dirname(abs_path)
      requires = [] of String
      exprs.each do |expr_id|
        process_require_node(arena, expr_id, base_dir, results, loaded, requires)
      end
      trace_driver("[DRIVER_TRACE] done requires for #{File.basename(abs_path)}")

      # Add this file's results (after dependencies)
      results << {arena, exprs, abs_path, source}

      if ast_cache_enabled? && arena.is_a?(CrystalV2::Compiler::Frontend::AstArena)
        begin
          cache = CrystalV2::Compiler::LSP::AstCache.new(arena, exprs, lexer.string_pool)
          cache.save(abs_path)
          save_require_cache(abs_path, requires)
          trace_driver("[DRIVER_TRACE] AST cache saved: #{abs_path}")
        rescue ex
          trace_driver("[DRIVER_TRACE] AST cache save failed: #{ex.message}")
        end
      end
    end

    private def process_require_node(
      arena : CrystalV2::Compiler::Frontend::ArenaLike,
      expr_id : CrystalV2::Compiler::Frontend::ExprId,
      base_dir : String,
      results : Array(Tuple(CrystalV2::Compiler::Frontend::ArenaLike, Array(CrystalV2::Compiler::Frontend::ExprId), String, String)),
      loaded : Set(String),
      requires_out : Array(String)? = nil
    )
      node = arena[expr_id]
      # Uncomment for debug: STDERR.puts "[DRIVER_TRACE] process_require_node: #{node.class}"
      case node
      when CrystalV2::Compiler::Frontend::ModuleNode
        if body = node.body
          body.each do |child_id|
            process_require_node(arena, child_id, base_dir, results, loaded, requires_out)
          end
        end
      when CrystalV2::Compiler::Frontend::RequireNode
        path_node = arena[node.path]
        if path_node.is_a?(CrystalV2::Compiler::Frontend::StringNode)
          req_path = String.new(path_node.value)
          trace_driver("[DRIVER_TRACE] require '#{req_path}' from #{base_dir}")
          resolved = resolve_require_path(req_path, base_dir)
          trace_driver("[DRIVER_TRACE] resolved to: #{resolved || "nil"}")
          if resolved
            requires_out << resolved if requires_out
            if loaded.includes?(resolved)
              trace_driver("[DRIVER_TRACE] already loaded, skipping")
            else
              trace_driver("[DRIVER_TRACE] will parse: #{resolved}")
            end
            parse_file_recursive(resolved, results, loaded)
          else
            trace_driver("[DRIVER_TRACE] WARN: Could not resolve require '#{req_path}'")
          end
        end
      when CrystalV2::Compiler::Frontend::MacroIfNode
        trace_driver("[DRIVER_TRACE] MacroIfNode processing")
        condition = evaluate_macro_condition(arena, node.condition, CrystalV2::Runtime.target_flags)
        trace_driver("[DRIVER_TRACE] MacroIfNode condition=#{condition.inspect}")
        if condition == true
          process_require_node(arena, node.then_body, base_dir, results, loaded, requires_out)
        elsif condition == false
          if else_body = node.else_body
            process_require_node(arena, else_body, base_dir, results, loaded, requires_out)
          end
        else
          process_require_node(arena, node.then_body, base_dir, results, loaded, requires_out)
          if else_body = node.else_body
            process_require_node(arena, else_body, base_dir, results, loaded, requires_out)
          end
        end
        trace_driver("[DRIVER_TRACE] MacroIfNode done")
      when CrystalV2::Compiler::Frontend::MacroLiteralNode
        trace_driver("[DRIVER_TRACE] MacroLiteralNode processing (#{node.pieces.size} pieces)")
        macro_literal_require_texts(arena, node, CrystalV2::Runtime.target_flags).each do |text|
          next unless text.includes?("require")
          text.scan(/\brequire\s*["']?([^"'\s]+)["']?/) do |match|
            req_path = match[1]
            resolved = resolve_require_path(req_path, base_dir)
            if resolved
              requires_out << resolved if requires_out
              parse_file_recursive(resolved, results, loaded)
            end
          end
        end
      end
    end

    private def collect_top_level_nodes(
      arena : CrystalV2::Compiler::Frontend::ArenaLike,
      expr_id : CrystalV2::Compiler::Frontend::ExprId,
      def_nodes : Array(Tuple(CrystalV2::Compiler::Frontend::DefNode, CrystalV2::Compiler::Frontend::ArenaLike)),
      class_nodes : Array(Tuple(CrystalV2::Compiler::Frontend::ClassNode, CrystalV2::Compiler::Frontend::ArenaLike)),
      module_nodes : Array(Tuple(CrystalV2::Compiler::Frontend::ModuleNode, CrystalV2::Compiler::Frontend::ArenaLike)),
      enum_nodes : Array(Tuple(CrystalV2::Compiler::Frontend::EnumNode, CrystalV2::Compiler::Frontend::ArenaLike)),
      macro_nodes : Array(Tuple(CrystalV2::Compiler::Frontend::MacroDefNode, CrystalV2::Compiler::Frontend::ArenaLike)),
      alias_nodes : Array(Tuple(CrystalV2::Compiler::Frontend::AliasNode, CrystalV2::Compiler::Frontend::ArenaLike)),
      lib_nodes : Array(Tuple(CrystalV2::Compiler::Frontend::LibNode, CrystalV2::Compiler::Frontend::ArenaLike, Array(Tuple(CrystalV2::Compiler::Frontend::AnnotationNode, CrystalV2::Compiler::Frontend::ArenaLike)))),
      constant_exprs : Array(Tuple(CrystalV2::Compiler::Frontend::ExprId, CrystalV2::Compiler::Frontend::ArenaLike)),
      main_exprs : Array(Tuple(CrystalV2::Compiler::Frontend::ExprId, CrystalV2::Compiler::Frontend::ArenaLike)),
      pending_annotations : Array(Tuple(CrystalV2::Compiler::Frontend::AnnotationNode, CrystalV2::Compiler::Frontend::ArenaLike)),
      acyclic_types : Set(String),
      flags : Set(String),
      sources_by_arena : Hash(CrystalV2::Compiler::Frontend::ArenaLike, String),
      source : String,
      depth : Int32 = 0,
      collect_main_exprs : Bool = true
    ) : Nil
      return if depth > 4
      node = arena[expr_id]
      case node
      when CrystalV2::Compiler::Frontend::DefNode
        def_nodes << {node, arena}
        pending_annotations.clear
      when CrystalV2::Compiler::Frontend::ClassNode
        class_nodes << {node, arena}
        if pending_annotation_has?(pending_annotations, "Acyclic")
          acyclic_types << String.new(node.name)
        end
        pending_annotations.clear
      when CrystalV2::Compiler::Frontend::ModuleNode
        module_nodes << {node, arena}
        pending_annotations.clear
      when CrystalV2::Compiler::Frontend::EnumNode
        enum_nodes << {node, arena}
        pending_annotations.clear
      when CrystalV2::Compiler::Frontend::MacroDefNode
        macro_nodes << {node, arena}
        pending_annotations.clear
      when CrystalV2::Compiler::Frontend::AliasNode
        alias_nodes << {node, arena}
        pending_annotations.clear
      when CrystalV2::Compiler::Frontend::LibNode
        lib_nodes << {node, arena, pending_annotations.dup}
        pending_annotations.clear
      when CrystalV2::Compiler::Frontend::ConstantNode
        constant_exprs << {expr_id, arena}
        main_exprs << {expr_id, arena} if collect_main_exprs
        if ENV["DEBUG_USE_LIBICONV"]? && String.new(node.name) == "USE_LIBICONV"
          STDERR.puts "[USE_LIBICONV] collect_top_level_nodes constant bytes=#{source.bytesize}"
        end
        pending_annotations.clear
      when CrystalV2::Compiler::Frontend::AnnotationNode
        pending_annotations << {node, arena}
      when CrystalV2::Compiler::Frontend::RequireNode
        # Skip require nodes - already processed
      when CrystalV2::Compiler::Frontend::MacroExpressionNode
        collect_top_level_nodes(arena, node.expression, def_nodes, class_nodes, module_nodes, enum_nodes, macro_nodes, alias_nodes, lib_nodes, constant_exprs, main_exprs, pending_annotations, acyclic_types, flags, sources_by_arena, source, depth, collect_main_exprs)
      when CrystalV2::Compiler::Frontend::VisibilityModifierNode
        collect_top_level_nodes(arena, node.expression, def_nodes, class_nodes, module_nodes, enum_nodes, macro_nodes, alias_nodes, lib_nodes, constant_exprs, main_exprs, pending_annotations, acyclic_types, flags, sources_by_arena, source, depth, collect_main_exprs)
      when CrystalV2::Compiler::Frontend::MacroIfNode
        if ENV["DEBUG_USE_LIBICONV"]? && source.includes?("USE_LIBICONV")
          STDERR.puts "[USE_LIBICONV] collect_top_level_nodes macro_if depth=#{depth}"
        end
        if raw_text = macro_if_raw_text(node, source)
          parsed_any = false
          combined = macro_literal_texts_from_raw(raw_text, flags).join
          unless combined.strip.empty? || combined.includes?("{%")
            if parsed = parse_macro_literal_program(combined)
              program, sanitized = parsed
              parsed_any = true
              sources_by_arena[program.arena] = sanitized
              program.roots.each do |inner_id|
                collect_top_level_nodes(program.arena, inner_id, def_nodes, class_nodes, module_nodes, enum_nodes, macro_nodes, alias_nodes, lib_nodes, constant_exprs, main_exprs, pending_annotations, acyclic_types, flags, sources_by_arena, sanitized, depth + 1, false)
              end
            end
          end
          return if parsed_any
        end
        condition = evaluate_macro_condition(arena, node.condition, flags)
        if condition == true
          collect_top_level_nodes(arena, node.then_body, def_nodes, class_nodes, module_nodes, enum_nodes, macro_nodes, alias_nodes, lib_nodes, constant_exprs, main_exprs, pending_annotations, acyclic_types, flags, sources_by_arena, source, depth, collect_main_exprs)
        elsif condition == false
          if else_body = node.else_body
            collect_top_level_nodes(arena, else_body, def_nodes, class_nodes, module_nodes, enum_nodes, macro_nodes, alias_nodes, lib_nodes, constant_exprs, main_exprs, pending_annotations, acyclic_types, flags, sources_by_arena, source, depth, collect_main_exprs)
          end
        else
          collect_top_level_nodes(arena, node.then_body, def_nodes, class_nodes, module_nodes, enum_nodes, macro_nodes, alias_nodes, lib_nodes, constant_exprs, main_exprs, pending_annotations, acyclic_types, flags, sources_by_arena, source, depth, collect_main_exprs)
          if else_body = node.else_body
            collect_top_level_nodes(arena, else_body, def_nodes, class_nodes, module_nodes, enum_nodes, macro_nodes, alias_nodes, lib_nodes, constant_exprs, main_exprs, pending_annotations, acyclic_types, flags, sources_by_arena, source, depth, collect_main_exprs)
          end
        end
      when CrystalV2::Compiler::Frontend::MacroLiteralNode
        if raw_text = macro_literal_raw_text(node, source)
          if ENV["DEBUG_USE_LIBICONV"]? && raw_text.includes?("USE_LIBICONV")
            STDERR.puts "[USE_LIBICONV] macro_literal_raw_text hit"
          end
          combined = macro_literal_texts_from_raw(raw_text, flags).join
          unless combined.strip.empty? || combined.includes?("{%")
            if parsed = parse_macro_literal_program(combined)
              program, sanitized = parsed
              sources_by_arena[program.arena] = sanitized
              program.roots.each do |inner_id|
                collect_top_level_nodes(program.arena, inner_id, def_nodes, class_nodes, module_nodes, enum_nodes, macro_nodes, alias_nodes, lib_nodes, constant_exprs, main_exprs, pending_annotations, acyclic_types, flags, sources_by_arena, sanitized, depth + 1, false)
              end
            end
          end
        end
      when CrystalV2::Compiler::Frontend::AssignNode
        target = arena[node.target]
        if target.is_a?(CrystalV2::Compiler::Frontend::ConstantNode)
          constant_exprs << {expr_id, arena}
          if ENV["DEBUG_USE_LIBICONV"]? && String.new(target.name) == "USE_LIBICONV"
            STDERR.puts "[USE_LIBICONV] collect_top_level_nodes assign bytes=#{source.bytesize}"
          end
        end
        main_exprs << {expr_id, arena} if collect_main_exprs
      else
        main_exprs << {expr_id, arena} if collect_main_exprs
      end
    end

    private def pending_annotation_has?(
      pending_annotations : Array(Tuple(CrystalV2::Compiler::Frontend::AnnotationNode, CrystalV2::Compiler::Frontend::ArenaLike)),
      name : String
    ) : Bool
      pending_annotations.any? do |ann_node, ann_arena|
        annotation_name_from_expr(ann_arena, ann_node.name) == name
      end
    end

    private def collect_macro_literal_exprs(
      arena : CrystalV2::Compiler::Frontend::ArenaLike,
      node : CrystalV2::Compiler::Frontend::MacroLiteralNode,
      flags : Set(String)
    ) : Array(CrystalV2::Compiler::Frontend::ExprId)
      exprs = [] of CrystalV2::Compiler::Frontend::ExprId
      control_stack = [] of {Bool, Bool, Bool} # {parent_active, branch_taken, active}
      active = true

      node.pieces.each do |piece|
        case piece.kind
        when CrystalV2::Compiler::Frontend::MacroPiece::Kind::Expression
          if active && (expr = piece.expr)
            exprs << expr
          end
        when CrystalV2::Compiler::Frontend::MacroPiece::Kind::ControlStart
          keyword = piece.control_keyword || ""
          cond_expr = piece.expr
          cond = cond_expr ? evaluate_macro_condition(arena, cond_expr, flags) : nil
          if keyword == "unless"
            cond = cond.nil? ? nil : !cond
          end
          parent_active = active
          branch_active = if cond == true
                            parent_active
                          elsif cond == false
                            false
                          else
                            parent_active
                          end
          branch_taken = cond == true
          control_stack << {parent_active, branch_taken, branch_active}
          active = branch_active
        when CrystalV2::Compiler::Frontend::MacroPiece::Kind::ControlElseIf
          next if control_stack.empty?
          parent_active, branch_taken, _ = control_stack[-1]
          cond_expr = piece.expr
          cond = cond_expr ? evaluate_macro_condition(arena, cond_expr, flags) : nil
          take = !branch_taken && cond == true
          branch_active = if cond == false
                            false
                          elsif cond == true
                            parent_active && take
                          else
                            parent_active
                          end
          branch_taken = true if cond == true
          control_stack[-1] = {parent_active, branch_taken, branch_active}
          active = branch_active
        when CrystalV2::Compiler::Frontend::MacroPiece::Kind::ControlElse
          next if control_stack.empty?
          parent_active, branch_taken, _ = control_stack[-1]
          branch_active = parent_active && !branch_taken
          control_stack[-1] = {parent_active, true, branch_active}
          active = branch_active
        when CrystalV2::Compiler::Frontend::MacroPiece::Kind::ControlEnd
          if control_stack.empty?
            active = true
          else
            parent_active, _, _ = control_stack.pop
            active = parent_active
          end
        else
          # ignore text/vars for top-level node collection
        end
      end

      exprs
    end


    private def macro_literal_require_texts(
      arena : CrystalV2::Compiler::Frontend::ArenaLike,
      node : CrystalV2::Compiler::Frontend::MacroLiteralNode,
      flags : Set(String)
    ) : Array(String)
      if node.pieces.size == 1 && node.pieces[0].kind == CrystalV2::Compiler::Frontend::MacroPiece::Kind::Text
        if text = node.pieces[0].text
          return [] of String unless text.includes?("require") || text.includes?("skip_file")
          return macro_literal_texts_from_raw(text, flags) if text.includes?("{%")
        end
      end

      texts = [] of String
      control_stack = [] of {Bool, Bool, Bool} # {parent_active, branch_taken, active}
      active = true

      node.pieces.each do |piece|
        case piece.kind
        when CrystalV2::Compiler::Frontend::MacroPiece::Kind::Text
          if active && (text = piece.text)
            texts << text
          end
        when CrystalV2::Compiler::Frontend::MacroPiece::Kind::ControlStart
          keyword = piece.control_keyword || ""
          cond_expr = piece.expr
          cond = cond_expr ? evaluate_macro_condition(arena, cond_expr, flags) : nil
          if keyword == "unless"
            cond = cond.nil? ? nil : !cond
          end

          parent_active = active
          branch_active = if cond == true
                            parent_active
                          elsif cond == false
                            false
                          else
                            parent_active
                          end
          branch_taken = cond == true
          control_stack << {parent_active, branch_taken, branch_active}
          active = branch_active
        when CrystalV2::Compiler::Frontend::MacroPiece::Kind::ControlElseIf
          next if control_stack.empty?
          parent_active, branch_taken, _ = control_stack[-1]
          cond_expr = piece.expr
          cond = cond_expr ? evaluate_macro_condition(arena, cond_expr, flags) : nil
          take = !branch_taken && cond == true
          branch_active = if cond == false
                            false
                          elsif cond == true
                            parent_active && take
                          else
                            parent_active
                          end
          branch_taken = true if cond == true
          control_stack[-1] = {parent_active, branch_taken, branch_active}
          active = branch_active
        when CrystalV2::Compiler::Frontend::MacroPiece::Kind::ControlElse
          next if control_stack.empty?
          parent_active, branch_taken, _ = control_stack[-1]
          branch_active = parent_active && !branch_taken
          control_stack[-1] = {parent_active, true, branch_active}
          active = branch_active
        when CrystalV2::Compiler::Frontend::MacroPiece::Kind::ControlEnd
          if control_stack.empty?
            active = true
          else
            parent_active, _, _ = control_stack.pop
            active = parent_active
          end
        else
          # Ignore expression/macro var pieces for require scanning.
        end
      end

      texts
    end

    private def macro_literal_raw_text(
      node : CrystalV2::Compiler::Frontend::MacroLiteralNode,
      source : String
    ) : String?
      return nil if node.pieces.empty?
      builder = String::Builder.new
      bytesize = source.bytesize
      node.pieces.each do |piece|
        if span = piece.span
          start = span.start_offset
          length = span.end_offset - span.start_offset
          next if length <= 0
          next if start < 0 || start >= bytesize
          if start + length > bytesize
            length = bytesize - start
          end
          builder << source.byte_slice(start, length)
        elsif text = piece.text
          builder << text
        end
      end
      builder.to_s
    end

    private def macro_if_raw_text(
      node : CrystalV2::Compiler::Frontend::MacroIfNode,
      source : String
    ) : String?
      span = node.span
      start = span.start_offset
      length = span.end_offset - span.start_offset
      return nil if length <= 0
      return nil if start < 0 || start >= source.bytesize
      if start + length > source.bytesize
        length = source.bytesize - start
      end
      source.byte_slice(start, length)
    end

    private def parse_macro_literal_program(text : String) : {CrystalV2::Compiler::Frontend::Program, String}?
      sanitized = sanitize_macro_literal_text(text)
      trimmed = sanitized.strip
      return nil if trimmed.empty?
      return nil if sanitized.includes?("{%") || sanitized.includes?("{{")

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(sanitized)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer, recovery_mode: true)
      program = parser.parse_program
      return nil if program.roots.empty?
      {program, sanitized}
    end

    private def sanitize_macro_literal_text(text : String) : String
      return text unless text.includes?("{{")
      builder = String::Builder.new
      idx = 0
      while idx < text.size
        start = text.index("{{", idx)
        break unless start
        builder << text[idx, start - idx] if start > idx
        stop = text.index("}}", start + 2)
        if stop.nil?
          builder << text[start, text.size - start]
          idx = text.size
          break
        end
        placeholder_len = stop + 2 - start
        builder << (" " * placeholder_len)
        idx = stop + 2
      end
      builder << text[idx, text.size - idx] if idx < text.size
      builder.to_s
    end

    private def macro_literal_texts_from_raw(text : String, flags : Set(String)) : Array(String)
      texts = [] of String
      control_stack = [] of {Bool, Bool, Bool} # {parent_active, branch_taken, active}
      active = true
      idx = 0
      trace_driver("[DRIVER_TRACE] macro_literal_texts_from_raw: text.size=#{text.size}")

      while idx < text.size
        tag_start = text.index("{%", idx)
        if tag_start.nil?
          texts << text[idx, text.size - idx] if active
          break
        end

        if tag_start > idx && active
          texts << text[idx, tag_start - idx]
        end

        tag_end = text.index("%}", tag_start + 2)
        break unless tag_end

        tag = text[tag_start + 2, tag_end - tag_start - 2]
        tag = tag.strip
        tag = tag.lstrip('-').lstrip('~').rstrip('-').rstrip('~').strip

        if tag.starts_with?("skip_file")
          cond_text = tag.sub(/^skip_file/, "").strip
          skip = if cond_text.starts_with?("if ")
                   evaluate_macro_condition_text(cond_text.lstrip("if").strip, flags)
                 elsif cond_text.starts_with?("unless ")
                   val = evaluate_macro_condition_text(cond_text.lstrip("unless").strip, flags)
                   val.nil? ? nil : !val
                 else
                   true
                 end
          return [] of String if skip == true
        elsif tag.starts_with?("if ")
          cond = evaluate_macro_condition_text(tag.lstrip("if").strip, flags)
          parent_active = active
          branch_active = if cond == true
                            parent_active
                          elsif cond == false
                            false
                          else
                            parent_active
                          end
          branch_taken = cond == true
          control_stack << {parent_active, branch_taken, branch_active}
          active = branch_active
        elsif tag.starts_with?("unless ")
          cond = evaluate_macro_condition_text(tag.lstrip("unless").strip, flags)
          cond = cond.nil? ? nil : !cond
          parent_active = active
          branch_active = if cond == true
                            parent_active
                          elsif cond == false
                            false
                          else
                            parent_active
                          end
          branch_taken = cond == true
          control_stack << {parent_active, branch_taken, branch_active}
          active = branch_active
        elsif tag.starts_with?("elsif ")
          if control_stack.empty?
            idx = tag_end + 2
            next
          end
          parent_active, branch_taken, _ = control_stack[-1]
          cond = evaluate_macro_condition_text(tag.lstrip("elsif").strip, flags)
          take = !branch_taken && cond == true
          branch_active = if cond == false
                            false
                          elsif cond == true
                            parent_active && take
                          else
                            parent_active
                          end
          branch_taken = true if cond == true
          control_stack[-1] = {parent_active, branch_taken, branch_active}
          active = branch_active
        elsif tag == "else"
          if control_stack.empty?
            idx = tag_end + 2
            next
          end
          parent_active, branch_taken, _ = control_stack[-1]
          branch_active = parent_active && !branch_taken
          control_stack[-1] = {parent_active, true, branch_active}
          active = branch_active
        elsif tag == "end"
          if control_stack.empty?
            active = true
          else
            parent_active, _, _ = control_stack.pop
            active = parent_active
          end
        end

        idx = tag_end + 2
      end

      texts
    end

    private def evaluate_macro_condition_text(text : String, flags : Set(String)) : Bool?
      MacroConditionScanner.new(text, flags).parse
    end

    private class MacroConditionScanner
      def initialize(@input : String, @flags : Set(String))
        @index = 0
      end

      def parse : Bool?
        skip_ws
        result = parse_or
        skip_ws
        result
      end

      private def parse_or : Bool?
        left = parse_and
        loop do
          skip_ws
          break unless peek_two == "||"
          advance(2)
          right = parse_and
          left = merge_or(left, right)
        end
        left
      end

      private def parse_and : Bool?
        left = parse_unary
        loop do
          skip_ws
          break unless peek_two == "&&"
          advance(2)
          right = parse_unary
          left = merge_and(left, right)
        end
        left
      end

      private def parse_unary : Bool?
        skip_ws
        if peek_char == '!'
          advance(1)
          value = parse_unary
          return value.nil? ? nil : !value
        end
        parse_primary
      end

      private def parse_primary : Bool?
        skip_ws
        if peek_char == '('
          advance(1)
          value = parse_or
          skip_ws
          advance(1) if peek_char == ')'
          return value
        end

        ident = read_identifier
        return nil unless ident

        case ident
        when "true"
          true
        when "false", "nil"
          false
        when "flag?"
          parse_flag_call
        else
          nil
        end
      end

      private def parse_flag_call : Bool?
        skip_ws
        return nil unless peek_char == '('
        advance(1)
        skip_ws
        flag_name = parse_flag_name
        skip_ws
        advance(1) if peek_char == ')'
        return nil unless flag_name
        @flags.includes?(flag_name)
      end

      private def parse_flag_name : String?
        if peek_char == ':'
          advance(1)
          read_identifier
        elsif peek_char == '"' || peek_char == '\''
          quote = peek_char
          advance(1)
          start = @index
          while @index < @input.size && @input[@index] != quote
            @index += 1
          end
          return nil if @index <= start
          value = @input[start, @index - start]
          advance(1) if @index < @input.size
          value
        else
          nil
        end
      end

      private def read_identifier : String?
        skip_ws
        start = @index
        while @index < @input.size
          ch = @input[@index]
          break unless ch.alphanumeric? || ch == '_' || ch == '?'
          @index += 1
        end
        return nil if @index == start
        @input[start, @index - start]
      end

      private def skip_ws
        while @index < @input.size && @input[@index].whitespace?
          @index += 1
        end
      end

      private def peek_char : Char?
        return nil if @index >= @input.size
        @input[@index]
      end

      private def peek_two : String?
        return nil if @index + 1 >= @input.size
        @input[@index, 2]
      end

      private def advance(count : Int32)
        @index += count
      end

      private def merge_or(left : Bool?, right : Bool?) : Bool?
        return true if left == true || right == true
        return false if left == false && right == false
        nil
      end

      private def merge_and(left : Bool?, right : Bool?) : Bool?
        return false if left == false || right == false
        return true if left == true && right == true
        nil
      end
    end

    private def evaluate_macro_condition(
      arena : CrystalV2::Compiler::Frontend::ArenaLike,
      expr_id : CrystalV2::Compiler::Frontend::ExprId,
      flags : Set(String)
    ) : Bool?
      node = arena[expr_id]
      case node
      when CrystalV2::Compiler::Frontend::BoolNode
        node.value
      when CrystalV2::Compiler::Frontend::NilNode
        false
      when CrystalV2::Compiler::Frontend::MacroExpressionNode
        evaluate_macro_condition(arena, node.expression, flags)
      when CrystalV2::Compiler::Frontend::GroupingNode
        evaluate_macro_condition(arena, node.expression, flags)
      when CrystalV2::Compiler::Frontend::UnaryNode
        op = String.new(node.operator)
        return nil unless op == "!"
        value = evaluate_macro_condition(arena, node.operand, flags)
        value.nil? ? nil : !value
      when CrystalV2::Compiler::Frontend::BinaryNode
        op = String.new(node.operator)
        left = evaluate_macro_condition(arena, node.left, flags)
        right = evaluate_macro_condition(arena, node.right, flags)
        case op
        when "&&"
          return false if left == false || right == false
          return true if left == true && right == true
          nil
        when "||"
          return true if left == true || right == true
          return false if left == false && right == false
          nil
        else
          nil
        end
      when CrystalV2::Compiler::Frontend::CallNode
        macro_flag_call?(arena, node, flags)
      else
        nil
      end
    end

    private def macro_flag_call?(
      arena : CrystalV2::Compiler::Frontend::ArenaLike,
      node : CrystalV2::Compiler::Frontend::CallNode,
      flags : Set(String)
    ) : Bool?
      callee = arena[node.callee]
      callee_name = case callee
                    when CrystalV2::Compiler::Frontend::IdentifierNode
                      String.new(callee.name)
                    else
                      nil
                    end
      return nil unless callee_name == "flag?"
      return nil unless node.args.size == 1
      arg = arena[node.args[0]]
      flag_name = case arg
                  when CrystalV2::Compiler::Frontend::SymbolNode
                    String.new(arg.name)
                  when CrystalV2::Compiler::Frontend::StringNode
                    String.new(arg.value)
                  else
                    nil
                  end
      return nil unless flag_name
      flag_name = flag_name.strip.gsub(/^[:"']|["']$/, "")
      flags.includes?(flag_name)
    end

    # Resolve require path to absolute file path
    private def resolve_require_path(req_path : String, base_dir : String) : String?
      # Handle relative paths
      if req_path.starts_with?("./") || req_path.starts_with?("../")
        full_path = File.expand_path(req_path, base_dir)
        # Try with .cr extension first (most common case)
        if File.exists?(full_path + ".cr") && File.file?(full_path + ".cr")
          return full_path + ".cr"
        elsif File.exists?(full_path) && File.file?(full_path)
          return full_path
        end
      else
        # Try relative to current file first
        rel_path = File.expand_path(req_path, base_dir)
        if File.exists?(rel_path + ".cr") && File.file?(rel_path + ".cr")
          return rel_path + ".cr"
        elsif File.exists?(rel_path) && File.file?(rel_path)
          return rel_path
        end

        # Try relative to input file's directory
        input_dir = File.dirname(File.expand_path(@input_file))
        input_rel_path = File.expand_path(req_path, input_dir)
        if File.exists?(input_rel_path + ".cr") && File.file?(input_rel_path + ".cr")
          return input_rel_path + ".cr"
        elsif File.exists?(input_rel_path) && File.file?(input_rel_path)
          return input_rel_path
        end

        # Try in stdlib directory (for require "int", require "pointer", etc.)
        stdlib_path = File.expand_path(req_path, STDLIB_PATH)
        if File.exists?(stdlib_path + ".cr") && File.file?(stdlib_path + ".cr")
          return stdlib_path + ".cr"
        elsif File.exists?(stdlib_path) && File.file?(stdlib_path)
          return stdlib_path
        end
      end

      nil
    end
  end
end

# Main entry point
driver = Crystal::V2::CompilerDriver.new
driver.parse_args(ARGV)
driver.compile
