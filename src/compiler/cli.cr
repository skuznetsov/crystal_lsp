require "option_parser"
require "digest/sha256"
require "file_utils"
require "./frontend/diagnostic_formatter"
require "./frontend/lexer"
require "./frontend/parser"
require "./semantic/analyzer"
require "./semantic/diagnostic_formatter"
require "./hir/hir"
require "./hir/ast_to_hir"
require "./hir/escape_analysis"
require "./hir/memory_strategy"
require "./mir/mir"
require "./mir/optimizations"
require "./mir/hir_to_mir"
require "./mir/llvm_backend"
require "./lsp/ast_cache"
require "../runtime"

# Module aliases for convenience
alias HIR = Crystal::HIR
alias MIR = Crystal::MIR

module CrystalV2
  module Compiler
    VERSION = "0.1.0-dev"

    # Standard library path - relative to compiler source
    STDLIB_PATH = File.expand_path("../stdlib", File.dirname(__FILE__))

    class CLI
      @ast_cache_hits : Int32 = 0
      @ast_cache_misses : Int32 = 0
      @llvm_cache_hits : Int32 = 0
      @llvm_cache_misses : Int32 = 0

      def initialize(@args : Array(String))
      end

      def run(*, out_io : IO = STDOUT, err_io : IO = STDERR) : Int32
        options = Options.new

        parser = OptionParser.new do |p|
          p.banner = "Usage: crystal_v2 [options] <source.cr>\n\nOptions:"

          # Output options (Crystal-compatible)
          p.on("-o FILE", "--output FILE", "Output file name") { |f| options.output = f }

          # Optimization (Crystal-compatible)
          p.on("--release", "Compile in release mode (-O3)") { options.optimize = 3; options.release = true }
          p.on("-O LEVEL", "Optimization mode: 0 (default), 1, 2, 3") do |level|
            options.optimize = level.to_i? || 0
          end

          # Emit options (Crystal-compatible)
          p.on("--emit TYPE", "Emit: llvm-ir, hir, mir") do |t|
            case t
            when "llvm-ir" then options.emit_llvm = true
            when "hir"     then options.emit_hir = true
            when "mir"     then options.emit_mir = true
            end
          end

          # Prelude (Crystal-compatible)
          p.on("--prelude FILE", "Use given file as prelude") { |f| options.prelude_file = f }
          p.on("--no-prelude", "Don't load prelude") { options.no_prelude = true }

          # Codegen control (Crystal-compatible)
          p.on("--no-codegen", "Don't do code generation (semantic check only)") { options.check_only = true }

          # Debug info
          p.on("-d", "--debug", "Add symbolic debug info") { options.debug = true }
          p.on("--no-debug", "Skip symbolic debug info") { options.debug = false }

          # Verbose / progress (Crystal-compatible)
          p.on("--verbose", "Display executed commands") { options.verbose = true }
          p.on("-s", "--stats", "Enable statistics output") { options.stats = true }
          p.on("-p", "--progress", "Enable progress output") { options.progress = true }

          # Additional
          p.on("--dump-symbols", "Dump symbol table") { options.dump_symbols = true }
          p.on("--ast-cache", "Enable AST cache (file-based)") { options.ast_cache = true }
          p.on("--no-ast-cache", "Disable AST cache (file-based)") { options.ast_cache = false }
          p.on("--no-llvm-opt", "Skip LLVM opt (faster, less optimized)") { options.llvm_opt = false }
          p.on("--llvm-cache", "Enable LLVM opt/llc cache") { options.llvm_cache = true }
          p.on("--no-llvm-cache", "Disable LLVM opt/llc cache") { options.llvm_cache = false }
          p.on("--no-llvm-metadata", "Disable LLVM type metadata (faster, less debug info)") { options.emit_type_metadata = false }
          p.on("--lto", "Enable LLVM LTO at link time (clang)") { options.lto = true }
          p.on("--pgo-gen", "Enable LLVM PGO instrumentation (clang)") { options.pgo_generate = true }
          p.on("--pgo-use FILE", "Use LLVM PGO profile data (clang)") { |f| options.pgo_profile = f }
          p.on("--no-link", "Skip final link step (leave .o file)") { options.link = false }
          p.on("--no-ltp", "Disable LTP/WBA MIR optimization (benchmarking)") { options.ltp_opt = false }
          p.on("--slab-frame", "Use slab frame for no-escape functions (experimental)") { options.slab_frame = true }

          # Help/version (Crystal-compatible)
          p.on("--version", "Show version") { options.show_version = true }
          p.on("-h", "--help", "Show this message") { options.show_help = true; options.help_text = p.to_s }
        end

        begin
          parser.parse(@args)
        rescue ex : OptionParser::InvalidOption
          err_io.puts "Error: #{ex.message}"
          err_io.puts parser
          return 1
        end

        if options.show_version
          out_io.puts "crystal_v2 #{VERSION}"
          return 0
        end

        if options.show_help
          out_io.puts options.help_text
          return 0
        end

        if options.pgo_generate && !options.pgo_profile.empty?
          err_io.puts "Error: --pgo-gen and --pgo-use are mutually exclusive"
          return 1
        end
        if !options.pgo_profile.empty? && !File.exists?(options.pgo_profile)
          err_io.puts "Error: PGO profile not found: #{options.pgo_profile}"
          return 1
        end

        # Get input file from remaining args
        input_file = @args.find { |a| !a.starts_with?("-") && a.ends_with?(".cr") }
        unless input_file
          err_io.puts "Error: No input file specified"
          err_io.puts parser
          return 1
        end

        options.input = input_file
        options.output = input_file.gsub(/\.cr$/, "") if options.output.empty?

        if options.verbose || options.progress || options.stats
          STDOUT.sync = true
          STDERR.sync = true
        end

        if options.check_only
          return run_check(input_file, options, out_io, err_io)
        else
          return compile(input_file, options, out_io, err_io)
        end
      end

      struct Options
        property input : String = ""
        property output : String = ""
        property optimize : Int32 = 0
        property release : Bool = false
        property emit_llvm : Bool = false
        property emit_hir : Bool = false
        property emit_mir : Bool = false
        property prelude_file : String = ""
        property no_prelude : Bool = false
        property debug : Bool = false
        property verbose : Bool = false
        property stats : Bool = false
        property progress : Bool = false
        property check_only : Bool = false
        property dump_symbols : Bool = false
        property ast_cache : Bool = ENV["CRYSTAL_V2_AST_CACHE"]? != "0"
        property llvm_opt : Bool = true
        property llvm_cache : Bool = ENV["CRYSTAL_V2_LLVM_CACHE"]? != "0"
        property link : Bool = true
        property emit_type_metadata : Bool = true
        property ltp_opt : Bool = true
        property slab_frame : Bool = false
        property lto : Bool = false
        property pgo_generate : Bool = false
        property pgo_profile : String = ""
        property link_libraries : Array(String) = [] of String
        property show_version : Bool = false
        property show_help : Bool = false
        property help_text : String = ""
      end

      # Semantic check only (original cli.cr functionality)
      private def run_check(path : String, options : Options, out_io : IO, err_io : IO) : Int32
        source = File.read(path)
        lexer = Frontend::Lexer.new(source)
        parser = Frontend::Parser.new(lexer)
        program = parser.parse_program

        if parser.diagnostics.empty?
          analyzer = Semantic::Analyzer.new(program)
          analyzer.collect_symbols

          result = analyzer.resolve_names
          report_collector_diagnostics(analyzer.semantic_diagnostics, source, err_io)
          report_resolution_diagnostics(result.diagnostics, source, err_io)

          if analyzer.semantic_errors?
            err_io.puts "\nerror: compilation failed due to semantic errors"
            return 1
          end

          if result.diagnostics.any?
            err_io.puts "\nerror: compilation failed due to name resolution errors"
            return 1
          end

          analyzer.infer_types(result.identifier_symbols)
          report_type_inference_diagnostics(analyzer.type_inference_diagnostics, source, err_io)

          if analyzer.type_inference_errors?
            err_io.puts "\nerror: compilation failed due to type errors"
            return 1
          end

          out_io.puts "Parsed #{program.roots.size} top-level expressions"
          if options.dump_symbols
            dump_symbols(program, analyzer.global_context.symbol_table, out_io)
          end
          return 0
        else
          parser.diagnostics.each do |diag|
            err_io.puts Frontend::DiagnosticFormatter.format(source, diag)
          end
          err_io.puts "\nerror: compilation failed due to parser errors"
          return 1
        end
      rescue File::NotFoundError
        err_io.puts "error: file not found #{path}"
        return 1
      end

      # Full compilation (driver.cr functionality)
      private def compile(input_file : String, options : Options, out_io : IO, err_io : IO) : Int32
        timings = {} of String => Float64
        total_start = Time.monotonic
        @ast_cache_hits = 0
        @ast_cache_misses = 0
        @llvm_cache_hits = 0
        @llvm_cache_misses = 0

        log(options, out_io, "=== Crystal v2 Compiler ===")
        log(options, out_io, "Input: #{input_file}")
        log(options, out_io, "Output: #{options.output}")

        # Step 1: Parse source (with require support)
        log(options, out_io, "\n[1/6] Parsing...")
        parse_start = Time.monotonic

        loaded_files = Set(String).new
        all_arenas = [] of Tuple(Frontend::ArenaLike, Array(Frontend::ExprId), String)

        # Load prelude first (unless --no-prelude)
        unless options.no_prelude
          prelude_path = if options.prelude_file.empty?
                           File.join(STDLIB_PATH, "prelude.cr")
                         else
                           options.prelude_file
                         end
          if File.exists?(prelude_path)
            log(options, out_io, "  Loading prelude: #{prelude_path}")
            prelude_start = Time.monotonic
            parse_file_recursive(prelude_path, all_arenas, loaded_files, input_file, options, out_io)
            if options.stats
              timings["parse_prelude"] = (Time.monotonic - prelude_start).total_milliseconds
            end
          end
        end

        # Parse user's input file
        user_parse_start = Time.monotonic
        parse_file_recursive(input_file, all_arenas, loaded_files, input_file, options, out_io)
        if options.stats
          timings["parse_user"] = (Time.monotonic - user_parse_start).total_milliseconds
          timings["parse_total"] = (Time.monotonic - parse_start).total_milliseconds
        end

        if all_arenas.empty?
          err_io.puts "error: no valid source files found"
          emit_timings(options, out_io, timings, total_start)
          return 1
        end

        total_exprs = all_arenas.sum { |t| t[1].size }
        log(options, out_io, "  Files: #{all_arenas.size}, Expressions: #{total_exprs}")

        link_libs = collect_link_libraries(all_arenas, options, out_io)

        # Step 2: Lower to HIR
        log(options, out_io, "\n[2/6] Lowering to HIR...")
        hir_start = Time.monotonic

        first_arena = all_arenas[0][0]
        hir_converter = HIR::AstToHir.new(first_arena, input_file)
        link_libs.each { |lib_name| hir_converter.module.add_link_library(lib_name) }

        # Collect nodes by type
        def_nodes = [] of Tuple(Frontend::DefNode, Frontend::ArenaLike)
        class_nodes = [] of Tuple(Frontend::ClassNode, Frontend::ArenaLike)
        module_nodes = [] of Tuple(Frontend::ModuleNode, Frontend::ArenaLike)
        enum_nodes = [] of Tuple(Frontend::EnumNode, Frontend::ArenaLike)
        macro_nodes = [] of Tuple(Frontend::MacroDefNode, Frontend::ArenaLike)
        alias_nodes = [] of Tuple(Frontend::AliasNode, Frontend::ArenaLike)
        lib_nodes = [] of Tuple(Frontend::LibNode, Frontend::ArenaLike)
        main_exprs = [] of Tuple(Frontend::ExprId, Frontend::ArenaLike)

        all_arenas.each do |arena, exprs, file_path|
          exprs.each do |expr_id|
            node = arena[expr_id]
            case node
            when Frontend::DefNode
              def_nodes << {node, arena}
            when Frontend::ClassNode
              # Parser creates ClassNode with is_struct=true for struct keyword
              class_nodes << {node, arena}
            when Frontend::ModuleNode
              module_nodes << {node, arena}
            when Frontend::EnumNode
              enum_nodes << {node, arena}
            when Frontend::MacroDefNode
              macro_nodes << {node, arena}
            when Frontend::AliasNode
              alias_nodes << {node, arena}
            when Frontend::LibNode
              lib_nodes << {node, arena}
            when Frontend::RequireNode
              # Skip - already processed
            else
              main_exprs << {expr_id, arena}
            end
          end
        end

        # Pass 1: Register types
        if ENV.has_key?("DEBUG_NESTED_CLASS")
          STDERR.puts "[DEBUG_CLI] class_nodes: #{class_nodes.size}, module_nodes: #{module_nodes.size}"
          module_nodes.each do |module_node, arena|
            name = String.new(module_node.name)
            STDERR.puts "[DEBUG_CLI] Module: #{name}, body_size=#{module_node.body.try(&.size) || 0}"
          end
          class_nodes.each do |class_node, arena|
            name = String.new(class_node.name)
            if name == "IO" || name.includes?("FileDescriptor")
              STDERR.puts "[DEBUG_CLI] Class: #{name}"
            end
          end
        end
        log(options, out_io, "  Pass 1: Registering types...")
        log(options, out_io, "    Enums: #{enum_nodes.size}")
        enum_nodes.each { |n, a| hir_converter.arena = a; hir_converter.register_enum(n) }
        log(options, out_io, "    Libs: #{lib_nodes.size}")
        lib_nodes.each { |n, a| hir_converter.arena = a; hir_converter.register_lib(n) }
        log(options, out_io, "    Aliases: #{alias_nodes.size}")
        alias_nodes.each { |n, a| hir_converter.arena = a; hir_converter.register_alias(n) }
        log(options, out_io, "    Modules: #{module_nodes.size}")
        module_nodes.each { |n, a| hir_converter.arena = a; hir_converter.register_module(n) }
        log(options, out_io, "    Classes: #{class_nodes.size}")
        class_nodes.each_with_index do |(n, a), i|
          hir_converter.arena = a
          hir_converter.register_class(n)
          STDERR.print "\r    Registered class #{i+1}/#{class_nodes.size}" if options.progress && (i % 10 == 0 || i == class_nodes.size - 1)
        end
        STDERR.puts if options.progress
        log(options, out_io, "    Macros: #{macro_nodes.size}")
        macro_nodes.each { |n, a| hir_converter.arena = a; hir_converter.register_macro(n) }

        # Flush pending monomorphizations now that all templates are registered
        log(options, out_io, "  Flushing pending monomorphizations...")
        hir_converter.flush_pending_monomorphizations

        # Pass 2: Register function signatures
        log(options, out_io, "  Pass 2: Registering #{def_nodes.size} function signatures...")
        def_nodes.each_with_index do |(n, a), i|
          hir_converter.arena = a
          hir_converter.register_function(n)
          STDERR.print "\r    Registered function #{i+1}/#{def_nodes.size}" if options.progress && (i % 50 == 0 || i == def_nodes.size - 1)
        end
        STDERR.puts if options.progress

        # Pass 3: Lower bodies (lazy)
        # Only lower top-level expressions; function bodies are lowered on demand.
        log(options, out_io, "  Pass 3: Lowering bodies (lazy)...")

        # Create main function from top-level expressions (or user-defined main)
        STDERR.puts "  Creating main function..." if options.progress
        if main_exprs.size > 0
          hir_converter.lower_main(main_exprs)
        elsif main_def = def_nodes.find { |(n, _)| String.new(n.name) == "main" && !(n.receiver.try { |recv| String.new(recv) == HIR::AstToHir::FUN_DEF_RECEIVER } || false) }
          hir_converter.arena = main_def[1]
          hir_converter.lower_main_from_def(main_def[0])
        end

        # Ensure top-level `fun main` is lowered as a real entrypoint (C ABI).
        if fun_main = def_nodes.find { |(n, _)| n.receiver.try { |recv| String.new(recv) == HIR::AstToHir::FUN_DEF_RECEIVER } || false }
          hir_converter.arena = fun_main[1]
          hir_converter.lower_def(fun_main[0])
        end
        STDERR.puts "  Main function created" if options.progress

        STDERR.puts "  Getting HIR module..." if options.progress
        hir_module = hir_converter.module
        STDERR.puts "  Got HIR module with #{hir_module.functions.size} functions" if options.progress
        options.link_libraries = hir_module.link_libraries.dup
        log(options, out_io, "  Functions: #{hir_module.functions.size}")
        timings["hir"] = (Time.monotonic - hir_start).total_milliseconds if options.stats
        timings["hir_funcs"] = hir_module.functions.size.to_f if options.stats

        # Reduce later phases by keeping only functions reachable from entrypoints.
        reachable = hir_module.reachable_function_names(["__crystal_main", "main"])
        if !reachable.empty? && reachable.size < hir_module.functions.size
          total_before = hir_module.functions.size
          hir_module.functions.select! { |func| reachable.includes?(func.name) }
          log(options, out_io, "  Reachable functions: #{hir_module.functions.size}/#{total_before}")
        end
        timings["hir_reachable_funcs"] = hir_module.functions.size.to_f if options.stats

        if options.emit_hir
          hir_file = options.output + ".hir"
          File.write(hir_file, hir_module.to_s)
          log(options, out_io, "  Wrote: #{hir_file}")
        end

        # Step 3: Escape analysis
        log(options, out_io, "\n[3/6] Escape analysis...")
        escape_start = Time.monotonic
        total_funcs = hir_module.functions.size
        hir_module.functions.each_with_index do |func, idx|
          if options.progress && (idx % 1000 == 0 || idx == total_funcs - 1)
            STDERR.puts "  Escape analysis: #{idx + 1}/#{total_funcs}..."
          end
          escape = HIR::EscapeAnalyzer.new(func)
          escape.analyze
          ms = HIR::MemoryStrategyAssigner.new(func)
          ms.assign
        end
        timings["escape"] = (Time.monotonic - escape_start).total_milliseconds if options.stats

        # Step 4: Lower to MIR
        log(options, out_io, "\n[4/6] Lowering to MIR...")
        mir_start = Time.monotonic
        mir_lowering = MIR::HIRToMIRLowering.new(hir_module, slab_frame: options.slab_frame)

        # Register globals from class variables
        globals = [] of Tuple(String, HIR::TypeRef, Int64?)
        hir_converter.class_info.each do |class_name, info|
          info.class_vars.each do |cvar|
            global_name = "#{class_name}_#{cvar.name}"
            globals << {global_name, cvar.type, cvar.initial_value}
          end
        end
        mir_lowering.register_globals(globals)
        mir_lowering.register_union_types(hir_converter.union_descriptors)
        mir_lowering.register_class_types(hir_converter.class_info)

        STDERR.puts "  Lowering #{hir_module.functions.size} functions to MIR..." if options.progress
        mir_module = mir_lowering.lower(options.progress)
        log(options, out_io, "  Functions: #{mir_module.functions.size}")
        timings["mir"] = (Time.monotonic - mir_start).total_milliseconds if options.stats
        timings["mir_funcs"] = mir_module.functions.size.to_f if options.stats

        # Optimize MIR
        log(options, out_io, "  Optimizing MIR...")
        mir_opt_start = Time.monotonic
        mir_module.functions.each_with_index do |func, idx|
          begin
            STDERR.puts "  Optimizing #{idx + 1}/#{mir_module.functions.size}: #{func.name}..." if options.progress
            if options.ltp_opt
              stats, ltp_potential = func.optimize_with_potential
              if options.verbose
                log(options, out_io, "    #{func.name} -> #{stats.total} changes, potential #{ltp_potential}")
              end
            else
              stats = func.optimize
              log(options, out_io, "    #{func.name} -> #{stats.total} changes (legacy)") if options.verbose
            end
          rescue ex : IndexError
            raise "Index error in optimize for: #{func.name}\n#{ex.message}\n#{ex.backtrace.join("\n")}"
          end
        end
        timings["mir_opt"] = (Time.monotonic - mir_opt_start).total_milliseconds if options.stats

        if options.emit_mir
          mir_file = options.output + ".mir"
          File.write(mir_file, mir_module.to_s)
          log(options, out_io, "  Wrote: #{mir_file}")
        end

        # Step 5: Generate LLVM IR
        log(options, out_io, "\n[5/6] Generating LLVM IR...")
        llvm_start = Time.monotonic
        llvm_gen = MIR::LLVMIRGenerator.new(mir_module)
        llvm_gen.emit_type_metadata = options.emit_type_metadata
        llvm_gen.progress = options.progress
        llvm_gen.reachability = true  # Only emit reachable functions from main
        llvm_ir = llvm_gen.generate
        log(options, out_io, "  LLVM IR size: #{llvm_ir.size} bytes")
        timings["llvm"] = (Time.monotonic - llvm_start).total_milliseconds if options.stats

        ll_file = options.output + ".ll"
        File.write(ll_file, llvm_ir)
        log(options, out_io, "  Wrote: #{ll_file}")

        if options.emit_llvm
          emit_timings(options, out_io, timings, total_start)
          out_io.puts llvm_ir
          return 0
        end

        # Step 6: Compile to binary
        log(options, out_io, "\n[6/6] Compiling to binary...")
        compile_start = Time.monotonic
        result = compile_llvm_ir(ll_file, options, out_io, err_io, timings)
        timings["compile"] = (Time.monotonic - compile_start).total_milliseconds if options.stats
        if result != 0
          emit_timings(options, out_io, timings, total_start)
          return result
        end

        log(options, out_io, "\n=== Compilation complete ===")
        log(options, out_io, "Output: #{options.output}")
        emit_timings(options, out_io, timings, total_start)
        return 0

      rescue ex : File::NotFoundError
        err_io.puts "error: file not found - #{ex.message}"
        emit_timings(options, out_io, timings, total_start)
        return 1
      rescue ex
        err_io.puts "error: #{ex.message}"
        err_io.puts ex.backtrace.join("\n") if options.verbose
        emit_timings(options, out_io, timings, total_start)
        return 1
      end

      private def compile_llvm_ir(
        ll_file : String,
        options : Options,
        out_io : IO,
        err_io : IO,
        timings : Hash(String, Float64)
      ) : Int32
        obj_file = ll_file.gsub(/\.ll$/, ".o")

        opt_flag = case options.optimize
                   when 0 then "-O0"
                   when 1 then "-O1"
                   when 2 then "-O2"
                   else        "-O3"
                   end

        cache_dir = File.expand_path("tmp/llvm_cache", Dir.current)
        base_hash = ""
        if options.llvm_cache
          FileUtils.mkdir_p(cache_dir)
          base_hash = file_sha256(ll_file)
        end

        opt_tag = options.llvm_opt ? "opt=#{opt_flag}" : "opt=none"
        llc_tag = "llc=#{opt_flag}"
        opt_cache_file = options.llvm_cache ? File.join(cache_dir, "#{digest_string("#{base_hash}|#{opt_tag}")}.opt.ll") : ""
        obj_cache_file = options.llvm_cache ? File.join(cache_dir, "#{digest_string("#{base_hash}|#{opt_tag}|#{llc_tag}")}.o") : ""

        opt_ll_file = ll_file
        if options.llvm_opt
          opt_ll_file = "#{ll_file}.opt.ll"
          opt_start = Time.monotonic
          if options.llvm_cache && File.exists?(opt_cache_file)
            FileUtils.cp(opt_cache_file, opt_ll_file)
            @llvm_cache_hits += 1
          else
            opt_cmd = "opt #{opt_flag} -S -o #{opt_ll_file} #{ll_file} 2>&1"
            log(options, out_io, "  $ #{opt_cmd}")
            opt_result = `#{opt_cmd}`
            unless $?.success?
              err_io.puts "opt failed:"
              err_io.puts opt_result
              return 1
            end
            if options.llvm_cache
              FileUtils.cp(opt_ll_file, opt_cache_file)
              @llvm_cache_misses += 1
            end
          end
          timings["opt"] = (Time.monotonic - opt_start).total_milliseconds if options.stats
        end

        use_clang_link = options.lto || options.pgo_generate || !options.pgo_profile.empty?
        if use_clang_link && !options.link
          err_io.puts "error: --lto/--pgo-* require linking (remove --no-link)"
          return 1
        end

        llc_start = Time.monotonic
        unless use_clang_link
          if options.llvm_cache && File.exists?(obj_cache_file)
            FileUtils.cp(obj_cache_file, obj_file)
            @llvm_cache_hits += 1
          else
            llc_cmd = "llc #{opt_flag} -filetype=obj -o #{obj_file} #{opt_ll_file} 2>&1"
            log(options, out_io, "  $ #{llc_cmd}")
            llc_result = `#{llc_cmd}`
            unless $?.success?
              err_io.puts "llc failed:"
              err_io.puts llc_result
              return 1
            end
            if options.llvm_cache
              FileUtils.cp(obj_file, obj_cache_file)
              @llvm_cache_misses += 1
            end
          end
          timings["llc"] = (Time.monotonic - llc_start).total_milliseconds if options.stats
        end

        # Find runtime stub
        runtime_dir = File.dirname(File.dirname(__FILE__))
        runtime_stub = File.join(runtime_dir, "..", "runtime_stub.o")
        runtime_src = runtime_stub.gsub(/\.o$/, ".c")

        # Compile runtime stub if needed
        if File.exists?(runtime_src) && (!File.exists?(runtime_stub) ||
           File.info(runtime_src).modification_time > File.info(runtime_stub).modification_time)
          `cc -c #{runtime_src} -o #{runtime_stub} 2>&1`
        end

        link_objs = [obj_file]
        link_objs << runtime_stub if File.exists?(runtime_stub)

        if options.link
          link_start = Time.monotonic
          link_flags = build_link_flags(options, out_io)
          link_flags_str = link_flags.join(" ")
          if use_clang_link
            pgo_flags = [] of String
            if options.pgo_generate
              pgo_flags << "-fprofile-instr-generate"
            elsif !options.pgo_profile.empty?
              pgo_flags << "-fprofile-instr-use=#{options.pgo_profile}"
            end

            lto_flag = options.lto ? "-flto" : ""
            clang_cmd = "clang #{opt_flag} #{lto_flag} #{pgo_flags.join(" ")} -o #{options.output} #{opt_ll_file}"
            clang_cmd += " #{runtime_stub}" if File.exists?(runtime_stub)
            clang_cmd += " #{link_flags_str}" unless link_flags_str.empty?
            clang_cmd += " 2>&1"

            log(options, out_io, "  $ #{clang_cmd}")
            clang_result = `#{clang_cmd}`
            unless $?.success?
              err_io.puts "clang failed:"
              err_io.puts clang_result
              return 1
            end
          else
            link_cmd = "cc -o #{options.output} #{link_objs.join(" ")}"
            link_cmd += " #{link_flags_str}" unless link_flags_str.empty?
            link_cmd += " 2>&1"
            log(options, out_io, "  $ #{link_cmd}")
            link_result = `#{link_cmd}`
            unless $?.success?
              err_io.puts "Linking failed:"
              err_io.puts link_result
              return 1
            end
          end
          timings["link"] = (Time.monotonic - link_start).total_milliseconds if options.stats
        else
          log(options, out_io, "  Skipping link (--no-link)")
          return 0
        end

        # Clean up intermediate files
        File.delete(obj_file) if File.exists?(obj_file)
        return 0
      end

      private def build_link_flags(options : Options, out_io : IO) : Array(String)
        flags = [] of String
        seen = Set(String).new
        options.link_libraries.each do |entry|
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
              log(options, out_io, "  Warning: pkg-config failed for #{value}")
            end
          when "framework"
            next if value.empty?
            marker = "-framework #{value}"
            next if seen.includes?(marker)
            flags << "-framework"
            flags << value
            seen << marker
          when "dll"
            unless Runtime.target_flags.includes?("win32") || Runtime.target_flags.includes?("windows")
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

      private def file_sha256(path : String) : String
        digest = Digest::SHA256.new
        File.open(path) do |io|
          buffer = Bytes.new(64 * 1024)
          while (read_bytes = io.read(buffer)) > 0
            digest.update(buffer[0, read_bytes])
          end
        end
        digest.hexfinal
      end

      private def digest_string(value : String) : String
        digest = Digest::SHA256.new
        digest.update(value.to_slice)
        digest.hexfinal
      end

      private def parse_file_recursive(
        file_path : String,
        results : Array(Tuple(Frontend::ArenaLike, Array(Frontend::ExprId), String)),
        loaded : Set(String),
        input_file : String,
        options : Options,
        out_io : IO
      )
        abs_path = File.expand_path(file_path)
        return if loaded.includes?(abs_path)
        loaded << abs_path
        log(options, out_io, "  Loading: #{abs_path}") if options.verbose

        unless File.exists?(abs_path)
          log(options, out_io, "  Warning: File not found: #{abs_path}")
          return
        end

        if options.ast_cache
          if cached = LSP::AstCache.load(abs_path)
            @ast_cache_hits += 1
            arena = cached.arena
            exprs = cached.roots
            base_dir = File.dirname(abs_path)
            if cached_requires = load_require_cache(abs_path)
              log(options, out_io, "  Require cache hit (#{cached_requires.size}): #{abs_path}") if options.verbose
              cached_requires.each do |req_path|
                parse_file_recursive(req_path, results, loaded, input_file, options, out_io)
              end
            else
              log(options, out_io, "  Require cache miss: #{abs_path}") if options.verbose
              requires = [] of String
              exprs.each do |expr_id|
                process_require_node(arena, expr_id, base_dir, input_file, results, loaded, options, out_io, requires)
              end
              save_require_cache(abs_path, requires)
            end
            results << {arena, exprs, abs_path}
            log(options, out_io, "  AST cache hit: #{abs_path}") if options.verbose
            return
          end
          @ast_cache_misses += 1
        end

        source = File.read(abs_path)
        lexer = Frontend::Lexer.new(source)
        parser = Frontend::Parser.new(lexer)
        program = parser.parse_program
        arena = program.arena
        exprs = program.roots

        # Process requires first
        base_dir = File.dirname(abs_path)
        requires = [] of String
        exprs.each do |expr_id|
          process_require_node(arena, expr_id, base_dir, input_file, results, loaded, options, out_io, requires)
        end

        results << {arena, exprs, abs_path}

        if options.ast_cache && arena.is_a?(Frontend::AstArena)
          begin
            cache = LSP::AstCache.new(arena, exprs, lexer.string_pool)
            cache.save(abs_path)
            save_require_cache(abs_path, requires)
            log(options, out_io, "  AST cache saved: #{abs_path}") if options.verbose
          rescue ex
            log(options, out_io, "  AST cache save failed: #{ex.message}") if options.verbose
          end
        end
      end

      # Process a node for require statements (recursively handles macro bodies)
      private def process_require_node(
        arena : Frontend::ArenaLike,
        expr_id : Frontend::ExprId,
        base_dir : String,
        input_file : String,
        results : Array(Tuple(Frontend::ArenaLike, Array(Frontend::ExprId), String)),
        loaded : Set(String),
        options : Options,
        out_io : IO,
        requires_out : Array(String)? = nil
      )
        node = arena[expr_id]
        case node
        when Frontend::RequireNode
          path_node = arena[node.path]
          if path_node.is_a?(Frontend::StringNode)
            req_path = String.new(path_node.value)
            resolved = resolve_require_path(req_path, base_dir, input_file)
            case resolved
            when String
              requires_out << resolved if requires_out
              parse_file_recursive(resolved, results, loaded, input_file, options, out_io)
            when Array
              resolved.each do |file|
                requires_out << file if requires_out
                parse_file_recursive(file, results, loaded, input_file, options, out_io)
              end
            else
              log(options, out_io, "  Warning: Could not resolve require '#{req_path}'")
            end
          end
        when Frontend::MacroIfNode
          # Evaluate flag? conditions when possible to avoid loading the wrong platform branch.
          # Fall back to both branches if the condition is unknown.
          condition = evaluate_macro_condition(arena, node.condition, Runtime.target_flags)
          if condition == true
            process_require_node(arena, node.then_body, base_dir, input_file, results, loaded, options, out_io, requires_out)
          elsif condition == false
            if else_body = node.else_body
              process_require_node(arena, else_body, base_dir, input_file, results, loaded, options, out_io, requires_out)
            end
          else
            process_require_node(arena, node.then_body, base_dir, input_file, results, loaded, options, out_io, requires_out)
            if else_body = node.else_body
              process_require_node(arena, else_body, base_dir, input_file, results, loaded, options, out_io, requires_out)
            end
          end
        when Frontend::MacroLiteralNode
          macro_literal_require_texts(arena, node, Runtime.target_flags).each do |text|
            next unless text.includes?("require")
            # Extract require paths from text (handles optional whitespace/quotes).
            text.scan(/\brequire\s*["']?([^"'\s]+)["']?/) do |match|
              req_path = match[1]
              resolved = resolve_require_path(req_path, base_dir, input_file)
              case resolved
              when String
                requires_out << resolved if requires_out
                parse_file_recursive(resolved, results, loaded, input_file, options, out_io)
              when Array
                resolved.each do |file|
                  requires_out << file if requires_out
                  parse_file_recursive(file, results, loaded, input_file, options, out_io)
                end
              end
            end
          end
        end
      end

      private def require_cache_path(file_path : String) : String
        cache_dir = ENV["XDG_CACHE_HOME"]? || File.join(ENV["HOME"]? || "/tmp", ".cache")
        hash = digest_string(file_path)
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

      private def save_require_cache(file_path : String, requires : Array(String))
        unique = requires.uniq
        return if unique.empty?

        cache_path = require_cache_path(file_path)
        Dir.mkdir_p(File.dirname(cache_path))
        File.write(cache_path, unique.join("\n") + "\n")
      rescue ex
        nil
      end

      private def collect_link_libraries(
        all_arenas : Array(Tuple(Frontend::ArenaLike, Array(Frontend::ExprId), String)),
        options : Options,
        out_io : IO
      ) : Array(String)
        libraries = [] of String
        all_arenas.each do |arena, exprs, _|
          exprs.each do |expr_id|
            collect_link_libraries_from_expr(arena, expr_id, libraries, options, out_io)
          end
        end
        libraries
      end

      private def collect_link_libraries_from_expr(
        arena : Frontend::ArenaLike,
        expr_id : Frontend::ExprId,
        libraries : Array(String),
        options : Options,
        out_io : IO
      )
        node = arena[expr_id]
        case node
        when Frontend::AnnotationNode
          annotation_name = annotation_name_from_expr(arena, node.name)
          if annotation_name == "Link"
            extract_link_libraries_from_annotation(arena, node).each do |lib_name|
              libraries << lib_name
            end
          end
        when Frontend::MacroIfNode
          condition = evaluate_macro_condition(arena, node.condition, Runtime.target_flags)
          if condition == true
            collect_link_libraries_from_expr(arena, node.then_body, libraries, options, out_io)
          elsif condition == false
            if else_body = node.else_body
              collect_link_libraries_from_expr(arena, else_body, libraries, options, out_io)
            end
          else
            collect_link_libraries_from_expr(arena, node.then_body, libraries, options, out_io)
            if else_body = node.else_body
              collect_link_libraries_from_expr(arena, else_body, libraries, options, out_io)
            end
          end
        when Frontend::MacroLiteralNode
          macro_literal_active_texts(arena, node, Runtime.target_flags).each do |text|
            extract_link_libraries_from_text(text).each do |lib_name|
              libraries << lib_name
            end
          end
        end
      end

      private def extract_link_libraries_from_annotation(
        arena : Frontend::ArenaLike,
        node : Frontend::AnnotationNode
      ) : Array(String)
        libraries = [] of String
        node.args.each do |arg_id|
          arg_node = arena[arg_id]
          if arg_node.is_a?(Frontend::StringNode)
            libraries << String.new(arg_node.value)
          end
        end

        if named_args = node.named_args
          named_args.each do |named_arg|
            value_node = arena[named_arg.value]
            next unless value_node.is_a?(Frontend::StringNode)

            named_lib_name = String.new(value_node.value)
            named_key = String.new(named_arg.name)
            prefix = case named_key
                     when "pkg_config" then "pkg_config:"
                     when "framework"  then "framework:"
                     when "dll"        then "dll:"
                     else "#{named_key}:"
                     end
            libraries << "#{prefix}#{named_lib_name}"
          end
        end

        libraries
      end

      private def extract_link_libraries_from_text(text : String) : Array(String)
        libraries = [] of String
        text.scan(/@\[\s*Link\s*\((.*?)\)\s*\]/m) do |match|
          args_text = match[1]
          libraries.concat(parse_link_annotation_args(args_text))
        end
        libraries
      end

      private def parse_link_annotation_args(text : String) : Array(String)
        libraries = [] of String
        working = text.dup

        working.scan(/(\w+)\s*:\s*["']([^"']*)["']/) do |match|
          key = match[1]
          value = match[2]
          prefix = case key
                   when "pkg_config" then "pkg_config:"
                   when "framework"  then "framework:"
                   when "dll"        then "dll:"
                   else "#{key}:"
                   end
          libraries << "#{prefix}#{value}"
        end

        working = working.gsub(/(\w+)\s*:\s*["'][^"']*["']/, "")
        working.scan(/["']([^"']*)["']/) do |match|
          libraries << match[1]
        end

        libraries
      end

      private def macro_literal_active_texts(
        arena : Frontend::ArenaLike,
        node : Frontend::MacroLiteralNode,
        flags : Set(String)
      ) : Array(String)
        if node.pieces.size == 1 && node.pieces[0].kind == Frontend::MacroPiece::Kind::Text
          if text = node.pieces[0].text
            return macro_literal_texts_from_raw(text, flags) if text.includes?("{%")
            return [text]
          end
        end

        texts = [] of String
        control_stack = [] of {Bool, Bool, Bool} # {parent_active, branch_taken, active}
        active = true

        node.pieces.each do |piece|
          case piece.kind
          when Frontend::MacroPiece::Kind::Text
            if active && (text = piece.text)
              texts << text
            end
          when Frontend::MacroPiece::Kind::ControlStart
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
          when Frontend::MacroPiece::Kind::ControlElseIf
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
          when Frontend::MacroPiece::Kind::ControlElse
            next if control_stack.empty?
            parent_active, branch_taken, _ = control_stack[-1]
            branch_active = parent_active && !branch_taken
            control_stack[-1] = {parent_active, true, branch_active}
            active = branch_active
          when Frontend::MacroPiece::Kind::ControlEnd
            if control_stack.empty?
              active = true
            else
              parent_active, _, _ = control_stack.pop
              active = parent_active
            end
          else
            # Ignore expression/macro var pieces.
          end
        end

        texts
      end

      private def annotation_name_from_expr(
        arena : Frontend::ArenaLike,
        expr_id : Frontend::ExprId
      ) : String
        node = arena[expr_id]
        case node
        when Frontend::IdentifierNode
          String.new(node.name)
        when Frontend::PathNode
          annotation_name_from_expr(arena, node.right)
        else
          "Unknown"
        end
      end

      private def macro_literal_require_texts(
        arena : Frontend::ArenaLike,
        node : Frontend::MacroLiteralNode,
        flags : Set(String)
      ) : Array(String)
        if node.pieces.size == 1 && node.pieces[0].kind == Frontend::MacroPiece::Kind::Text
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
          when Frontend::MacroPiece::Kind::Text
            if active && (text = piece.text)
              texts << text
            end
          when Frontend::MacroPiece::Kind::ControlStart
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
          when Frontend::MacroPiece::Kind::ControlElseIf
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
          when Frontend::MacroPiece::Kind::ControlElse
            next if control_stack.empty?
            parent_active, branch_taken, _ = control_stack[-1]
            branch_active = parent_active && !branch_taken
            control_stack[-1] = {parent_active, true, branch_active}
            active = branch_active
          when Frontend::MacroPiece::Kind::ControlEnd
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

      private def macro_literal_texts_from_raw(text : String, flags : Set(String)) : Array(String)
        texts = [] of String
        control_stack = [] of {Bool, Bool, Bool} # {parent_active, branch_taken, active}
        active = true
        idx = 0
        segment_start = 0
        bytes = text.to_slice
        size = bytes.size

        missing_end = false
        while idx + 1 < size
          if bytes[idx] == '{'.ord && bytes[idx + 1] == '%'.ord
            if idx > segment_start && active
              texts << text.byte_slice(segment_start, idx - segment_start)
            end
            idx += 2
            tag_start = idx
            while idx + 1 < size && !(bytes[idx] == '%'.ord && bytes[idx + 1] == '}'.ord)
              idx += 1
            end
            if idx + 1 >= size
              missing_end = true
              break
            end

            tag = text.byte_slice(tag_start, idx - tag_start)
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
              next if control_stack.empty?
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
              next if control_stack.empty?
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

            idx += 2
            segment_start = idx
            next
          else
            idx += 1
          end
        end

        if !missing_end && segment_start < size && active
          texts << text.byte_slice(segment_start, size - segment_start)
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
        arena : Frontend::ArenaLike,
        expr_id : Frontend::ExprId,
        flags : Set(String)
      ) : Bool?
        node = arena[expr_id]
        case node
        when Frontend::BoolNode
          node.value
        when Frontend::NilNode
          false
        when Frontend::MacroExpressionNode
          evaluate_macro_condition(arena, node.expression, flags)
        when Frontend::UnaryNode
          op = String.new(node.operator)
          return nil unless op == "!"
          value = evaluate_macro_condition(arena, node.operand, flags)
          value.nil? ? nil : !value
        when Frontend::BinaryNode
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
        when Frontend::CallNode
          macro_flag_call?(arena, node, flags)
        else
          nil
        end
      end

      private def macro_flag_call?(
        arena : Frontend::ArenaLike,
        node : Frontend::CallNode,
        flags : Set(String)
      ) : Bool?
        callee = arena[node.callee]
        callee_name = case callee
                      when Frontend::IdentifierNode
                        String.new(callee.name)
                      else
                        nil
                      end
        return nil unless callee_name == "flag?"
        return nil unless node.args.size == 1
        arg = arena[node.args[0]]
        flag_name = case arg
                    when Frontend::SymbolNode
                      String.new(arg.name)
                    when Frontend::StringNode
                      String.new(arg.value)
                    else
                      nil
                    end
        return nil unless flag_name
        flags.includes?(flag_name)
      end

      private def resolve_require_path(req_path : String, base_dir : String, input_file : String) : String | Array(String) | Nil
        # Handle wildcards (/* and /**)
        if req_path.ends_with?("/*") || req_path.ends_with?("/**")
          return resolve_wildcard_require(req_path, base_dir)
        end

        # Handle special "c/*" requires that map to lib_c/TARGET/c/*.cr
        # This is Crystal's way of handling platform-specific C library bindings
        if req_path.starts_with?("c/")
          # On macOS aarch64, resolve to lib_c/aarch64-darwin/c/*
          # TODO: Detect actual platform
          platform = "aarch64-darwin"
          platform_path = File.join(STDLIB_PATH, "lib_c", platform, req_path)
          result = try_require_path(platform_path)
          return result if result
        end

        # Relative paths
        if req_path.starts_with?("./") || req_path.starts_with?("../")
          full_path = File.expand_path(req_path, base_dir)
          result = try_require_path(full_path)
          return result if result
        else
          # Try relative to current file
          rel_path = File.expand_path(req_path, base_dir)
          result = try_require_path(rel_path)
          return result if result

          # Try relative to input file
          input_dir = File.dirname(File.expand_path(input_file))
          input_rel = File.expand_path(req_path, input_dir)
          result = try_require_path(input_rel)
          return result if result

          # Try stdlib
          stdlib_path = File.expand_path(req_path, STDLIB_PATH)
          result = try_require_path(stdlib_path)
          return result if result
        end
        nil
      end

      # Try to resolve a require path, handling both files and directories
      # Crystal convention: require "foo" looks for foo.cr, then foo/foo.cr
      private def try_require_path(path : String) : String | Array(String) | Nil
        # Try with .cr extension first
        cr_path = path + ".cr"
        if File.file?(cr_path)
          # For crystal/system modules, also load the unix variant
          # (since macro conditionals aren't evaluated, we load all platform files)
          if path.includes?("/crystal/system/") && !path.includes?("/unix/") && !path.includes?("/win32/") && !path.includes?("/wasi/")
            unix_path = path.gsub("/crystal/system/", "/crystal/system/unix/") + ".cr"
            if File.file?(unix_path)
              return [cr_path, unix_path]
            end
          end
          return cr_path
        end

        # If path exists as a directory, look for dir/basename.cr inside it
        if Dir.exists?(path)
          basename = File.basename(path)
          inner_path = File.join(path, basename + ".cr")
          if File.file?(inner_path)
            # For crystal/system modules, also load the unix variant
            if path.includes?("/crystal/system/") && !path.includes?("/unix/") && !path.includes?("/win32/") && !path.includes?("/wasi/")
              unix_path = path.gsub("/crystal/system/", "/crystal/system/unix/")
              unix_inner = File.join(unix_path, basename + ".cr")
              if File.file?(unix_inner)
                return [inner_path, unix_inner]
              end
            end
            return inner_path
          end
        end

        # Try exact path if it's a file
        return path if File.file?(path)

        nil
      end

      # Resolve wildcard require patterns like ./io/* or ./io/**
      private def resolve_wildcard_require(req_path : String, base_dir : String) : Array(String)?
        recursive = req_path.ends_with?("/**")
        pattern = req_path.rchop(recursive ? "/**" : "/*")

        # Handle relative paths
        if pattern.starts_with?("./") || pattern.starts_with?("../")
          full_dir = File.expand_path(pattern, base_dir)
        else
          full_dir = File.expand_path(pattern, STDLIB_PATH)
        end

        return nil unless Dir.exists?(full_dir)

        files = [] of String
        gather_crystal_files(full_dir, files, recursive)
        files.empty? ? nil : files.sort  # Sort for deterministic order
      end

      # Gather all .cr files in a directory
      private def gather_crystal_files(dir : String, accumulator : Array(String), recursive : Bool)
        Dir.each_child(dir) do |entry|
          full_path = File.join(dir, entry)
          if File.directory?(full_path)
            gather_crystal_files(full_path, accumulator, true) if recursive
          elsif entry.ends_with?(".cr")
            accumulator << File.expand_path(full_path)
          end
        end
      end

      private def log(options : Options, out_io : IO, msg : String)
        out_io.puts msg if options.verbose
      end

      private def emit_timings(options : Options, out_io : IO, timings : Hash(String, Float64)?, total_start : Time::Span?)
        return unless options.stats
        return unless timings
        return unless total_start

        total_ms = (Time.monotonic - total_start).total_milliseconds
        parts = [] of String
        if (parse = timings["parse_total"]?)
          parts << "parse=#{parse.round(1)}"
        end
        if (prelude = timings["parse_prelude"]?)
          parts << "prelude=#{prelude.round(1)}"
        end
        if (user = timings["parse_user"]?)
          parts << "user=#{user.round(1)}"
        end
        if (hir = timings["hir"]?)
          parts << "hir=#{hir.round(1)}"
        end
        if (hir_funcs = timings["hir_funcs"]?)
          parts << "hir_funcs=#{hir_funcs.to_i}"
        end
        if (hir_reach = timings["hir_reachable_funcs"]?)
          parts << "hir_reach=#{hir_reach.to_i}"
        end
        if (escape = timings["escape"]?)
          parts << "escape=#{escape.round(1)}"
        end
        if (mir = timings["mir"]?)
          parts << "mir=#{mir.round(1)}"
        end
        if (mir_funcs = timings["mir_funcs"]?)
          parts << "mir_funcs=#{mir_funcs.to_i}"
        end
        if (opt = timings["mir_opt"]?)
          parts << "mir_opt=#{opt.round(1)}"
        end
        if (opt = timings["opt"]?)
          parts << "opt=#{opt.round(1)}"
        end
        if (llc = timings["llc"]?)
          parts << "llc=#{llc.round(1)}"
        end
        if (link = timings["link"]?)
          parts << "link=#{link.round(1)}"
        end
        if (llvm = timings["llvm"]?)
          parts << "llvm=#{llvm.round(1)}"
        end
        if (compile = timings["compile"]?)
          parts << "compile=#{compile.round(1)}"
        end
        parts << "total=#{total_ms.round(1)}"
        parts << "ast_cache=#{@ast_cache_hits} hit/#{@ast_cache_misses} miss" if options.ast_cache
        parts << "llvm_cache=#{@llvm_cache_hits} hit/#{@llvm_cache_misses} miss" if options.llvm_cache
        out_io.puts "Timing (ms): #{parts.join(" ")}"
      end

      private def dump_symbols(program, table, out_io, indent = 0)
        table.each_local_symbol do |name, symbol|
          indentation = "  " * indent
          label, details = case symbol
          when Semantic::MacroSymbol
            {"macro", nil}
          when Semantic::MethodSymbol
            params = symbol.params
            extra = params.empty? ? nil : "(params: #{params.compact_map { |p| p.name.try { |n| String.new(n) } }.join(", ")})"
            {"method", extra}
          when Semantic::ClassSymbol
            super_name = symbol.superclass_name
            extra = super_name ? "(super: #{super_name})" : nil
            {"class", extra}
          when Semantic::VariableSymbol
            {"variable", nil}
          else
            {"symbol", nil}
          end

          span_info = span_summary(program, symbol)
          line = "#{indentation}#{label} #{name}"
          line += " #{details}" if details
          line += " #{span_info}" unless span_info.empty?
          out_io.puts line

          case symbol
          when Semantic::MethodSymbol
            dump_symbols(program, symbol.scope, out_io, indent + 1)
          when Semantic::ClassSymbol
            dump_symbols(program, symbol.scope, out_io, indent + 1)
          end
        end
      end

      private def span_summary(program, symbol)
        node_id = symbol.node_id
        return "" if node_id.invalid?
        node = program.arena[node_id]
        span = node.span
        "[#{span.start_line}:#{span.start_column}-#{span.end_line}:#{span.end_column}]"
      end

      private def report_collector_diagnostics(diagnostics, source, err_io)
        diagnostics.each { |d| err_io.puts Semantic::DiagnosticFormatter.format(source, d) }
      end

      private def report_resolution_diagnostics(diagnostics, source, err_io)
        diagnostics.each { |d| err_io.puts Frontend::DiagnosticFormatter.format(source, d) }
      end

      private def report_type_inference_diagnostics(diagnostics, source, err_io)
        diagnostics.each { |d| err_io.puts Semantic::DiagnosticFormatter.format(source, d) }
      end
    end
  end
end

# Main entry point
exit CrystalV2::Compiler::CLI.new(ARGV).run
