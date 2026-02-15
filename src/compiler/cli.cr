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
require "./hir/class_info_type_provider"
require "./mir/mir"
require "./mir/optimizations"
require "./mir/hir_to_mir"
require "./mir/llvm_backend"
{% unless flag?(:bootstrap_fast) %}
require "./lsp/ast_cache"
{% end %}
require "../runtime"

# Module aliases for convenience
alias HIR = Crystal::HIR
alias MIR = Crystal::MIR

module CrystalV2
  module Compiler
    VERSION = "0.1.0-dev"

    # Standard library path - relative to compiler source
    STDLIB_PATH = File.expand_path("../stdlib", File.dirname(__FILE__))

    # Original Crystal compiler source path - for require'ing compiler modules
    # (e.g., require "compiler/crystal/syntax/lexer")
    CRYSTAL_SRC_PATH = File.expand_path("../../../crystal/src", File.dirname(__FILE__))

    class CLI
      @ast_cache_hits : Int32 = 0
      @ast_cache_misses : Int32 = 0
      @llvm_cache_hits : Int32 = 0
      @llvm_cache_misses : Int32 = 0
      @pipeline_cache_hits : Int32 = 0
      @pipeline_cache_misses : Int32 = 0
      # Top-level macro variable assignments (e.g., {% nums = %w(Int8 ...) %})
      @macro_text_vars = {} of String => String

      def initialize(@args : Array(String))
      end

      {% if flag?(:debug_hooks) %}
      private def setup_debug_hooks : Nil
        return unless ENV["CRYSTAL_V2_DEBUG_HOOKS"]?

        filter = ENV["CRYSTAL_V2_DEBUG_HOOKS_FILTER"]?
        io = STDERR
        emit = ->(event : String, data : String) do
          if filter && !event.includes?(filter) && !data.includes?(filter)
            nil
          else
            io.puts "[HOOK] #{event} #{data}"
          end
        end

        DebugHooks.on_debug = ->(event : String, data : String) { emit.call(event, data) }
        DebugHooks.on_method_register = ->(full_name : String, class_name : String, method_name : String) do
          emit.call("method.register", "full=#{full_name} class=#{class_name} name=#{method_name}")
        end
        DebugHooks.on_type_resolve = ->(name : String, context : String, result : String) do
          emit.call("type.resolve", "name=#{name} context=#{context} result=#{result}")
        end
        DebugHooks.on_class_register = ->(class_name : String, parent : String?) do
          emit.call("class.register", "class=#{class_name} parent=#{parent}")
        end
        DebugHooks.on_enum_register = ->(enum_name : String, base_type : String) do
          emit.call("enum.register", "enum=#{enum_name} base=#{base_type}")
        end
      end
      {% end %}

      def run(*, out_io : IO = STDOUT, err_io : IO = STDERR) : Int32
        options = Options.new
        mm_stack_threshold_invalid = false

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
          {% if flag?(:bootstrap_fast) %}
          p.on("--ast-cache", "Ignored in -Dbootstrap_fast (AST cache is compiled out)") { options.ast_cache = false }
          p.on("--no-ast-cache", "Ignored in -Dbootstrap_fast (AST cache is compiled out)") { options.ast_cache = false }
          {% else %}
          p.on("--ast-cache", "Enable AST cache (file-based)") { options.ast_cache = true }
          p.on("--no-ast-cache", "Disable AST cache (file-based)") { options.ast_cache = false }
          {% end %}
          p.on("--no-llvm-opt", "Skip LLVM opt (faster, less optimized)") { options.llvm_opt = false }
          p.on("--llvm-cache", "Enable LLVM opt/llc cache") { options.llvm_cache = true }
          p.on("--no-llvm-cache", "Disable LLVM opt/llc cache") { options.llvm_cache = false }
          p.on("--no-llvm-metadata", "Disable LLVM type metadata (faster, less debug info)") { options.emit_type_metadata = false }
          p.on("--lto", "Enable LLVM LTO at link time (clang)") { options.lto = true }
          p.on("--pgo-gen", "Enable LLVM PGO instrumentation (clang)") { options.pgo_generate = true }
          p.on("--pgo-use FILE", "Use LLVM PGO profile data (clang)") { |f| options.pgo_profile = f }
          p.on("--no-link", "Skip final link step (leave .o file)") { options.link = false }
          p.on("--no-mir-opt", "Skip MIR optimization passes (faster, less optimized)") { options.mir_opt = false }
          p.on("--no-ltp", "Disable LTP/WBA MIR optimization (benchmarking)") { options.ltp_opt = false }
          p.on("--slab-frame", "Use slab frame for no-escape functions (experimental)") { options.slab_frame = true }
          p.on("--mm=MODE", "Memory mode: conservative, balanced, aggressive") { |mode| options.mm_mode = mode }
          p.on("--mm-stack-threshold BYTES", "Max bytes for stack allocation in MM mode") do |bytes|
            if value = bytes.to_u32?
              options.mm_stack_threshold = value
            else
              mm_stack_threshold_invalid = true
            end
          end
          p.on("--no-gc", "Fail if any allocation uses GC") { options.no_gc = true }

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

        {% if flag?(:debug_hooks) %}
          setup_debug_hooks
        {% end %}

        if mm_stack_threshold_invalid
          err_io.puts "Error: --mm-stack-threshold expects an integer"
          err_io.puts parser
          return 1
        end
        unless valid_mm_mode?(options.mm_mode)
          err_io.puts "Error: Unknown --mm mode: #{options.mm_mode}"
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
        # Enabled by default (except in bootstrap_fast): stdlib/prelude parsing dominates
        # cold starts, and cache is invalidated by source mtime + compiler mtime.
        {% if flag?(:bootstrap_fast) %}
        property ast_cache : Bool = false
        {% else %}
        property ast_cache : Bool = ENV["CRYSTAL_V2_AST_CACHE"]? != "0"
        {% end %}
        property llvm_opt : Bool = true
        property llvm_cache : Bool = ENV["CRYSTAL_V2_LLVM_CACHE"]? != "0"
        property pipeline_cache : Bool = ENV["CRYSTAL_V2_PIPELINE_CACHE"]? != "0"
        property link : Bool = true
        property emit_type_metadata : Bool = true
        property ltp_opt : Bool = true
        property mir_opt : Bool = true
        property slab_frame : Bool = false
        property mm_mode : String = "balanced"
        property mm_stack_threshold : UInt32 = 4096_u32
        property no_gc : Bool = false
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
        total_start = Time.instant
        @ast_cache_hits = 0
        @ast_cache_misses = 0
        @llvm_cache_hits = 0
        @llvm_cache_misses = 0
        @pipeline_cache_hits = 0
        @pipeline_cache_misses = 0

        log(options, out_io, "=== Crystal v2 Compiler ===")
        log(options, out_io, "Input: #{input_file}")
        log(options, out_io, "Output: #{options.output}")

        # Step 1: Parse source (with require support)
        log(options, out_io, "\n[1/6] Parsing...")
        parse_start = Time.instant

        loaded_files = Set(String).new
        all_arenas = [] of Tuple(Frontend::ArenaLike, Array(Frontend::ExprId), String, String)

        # Load prelude first (unless --no-prelude)
        unless options.no_prelude
          prelude_path = if options.prelude_file.empty?
                           File.join(STDLIB_PATH, "prelude.cr")
                         elsif !options.prelude_file.includes?(File::SEPARATOR) && !options.prelude_file.ends_with?(".cr")
                           # Short name like "prelude" -> resolve to stdlib path
                           File.join(STDLIB_PATH, "#{options.prelude_file}.cr")
                         else
                           options.prelude_file
                         end
          if File.exists?(prelude_path)
            log(options, out_io, "  Loading prelude: #{prelude_path}")
            prelude_start = Time.instant
            parse_file_recursive(prelude_path, all_arenas, loaded_files, input_file, options, out_io)
            if options.stats
              timings["parse_prelude"] = (Time.instant - prelude_start).total_milliseconds
            end
          end
        end

        # Parse user's input file
        user_parse_start = Time.instant
        parse_file_recursive(input_file, all_arenas, loaded_files, input_file, options, out_io)
        if options.stats
          timings["parse_user"] = (Time.instant - user_parse_start).total_milliseconds
          timings["parse_total"] = (Time.instant - parse_start).total_milliseconds
        end

        if all_arenas.empty?
          err_io.puts "error: no valid source files found"
          emit_timings(options, out_io, timings, total_start)
          return 1
        end

        total_exprs = all_arenas.sum { |t| t[1].size }
        log(options, out_io, "  Files: #{all_arenas.size}, Expressions: #{total_exprs}")

        link_libs = collect_link_libraries(all_arenas, options, out_io)

        if ENV.has_key?("CRYSTAL_V2_STOP_AFTER_PARSE")
          log(options, out_io, "  Stop after parse (CRYSTAL_V2_STOP_AFTER_PARSE)")
          emit_timings(options, out_io, timings, total_start)
          return 0
        end

        # Pipeline cache: hash all source files → skip HIR/MIR/LLVM on hit
        pipeline_cache_hit = false
        pipeline_cache_file = ""
        ll_file = options.output + ".ll"

        if options.pipeline_cache
          pipeline_cache_dir = File.expand_path("tmp/pipeline_cache", Dir.current)
          FileUtils.mkdir_p(pipeline_cache_dir)
          digest = Digest::SHA256.new
          loaded_files.to_a.sort.each do |f|
            digest.update(f.to_slice)
            digest.update(File.read(f).to_slice)
          end
          digest.update("v2|mm=#{options.mm_mode}|thresh=#{options.mm_stack_threshold}|slab=#{options.slab_frame}|opt=#{options.optimize}".to_slice)
          # Include compiler binary fingerprint to invalidate cache when compiler changes.
          # Seconds-level mtime alone is not enough (quick rebuilds can collide).
          if exe_path = Process.executable_path
            begin
              if exe_info = File.info?(exe_path)
                digest.update("compiler_path=#{exe_path}".to_slice)
                digest.update("compiler_size=#{exe_info.size}".to_slice)
                digest.update("compiler_mtime_ns=#{exe_info.modification_time.to_unix_ns}".to_slice)
              end
            rescue
            end
          end
          pipeline_hash = digest.hexfinal
          pipeline_cache_file = File.join(pipeline_cache_dir, "#{pipeline_hash}.ll")

          pipeline_cache_libs_file = pipeline_cache_file + ".libs"
          if File.exists?(pipeline_cache_file) && File.exists?(pipeline_cache_libs_file)
            pipeline_cache_hit = true
            FileUtils.cp(pipeline_cache_file, ll_file)
            options.link_libraries = File.read_lines(pipeline_cache_libs_file).reject(&.empty?)
            @pipeline_cache_hits += 1
            log(options, out_io, "  Pipeline cache HIT (#{pipeline_hash[0, 12]})")
            timings["pipeline_cache_hit"] = 1.0 if options.stats
          end
        end

        unless pipeline_cache_hit

        # Step 2: Lower to HIR
        log(options, out_io, "\n[2/6] Lowering to HIR...")
        hir_start = Time.instant

        first_arena = all_arenas[0][0]
        sources_by_arena = {} of Frontend::ArenaLike => String
        paths_by_arena = {} of Frontend::ArenaLike => String
        all_arenas.each do |arena, _exprs, path, source|
          sources_by_arena[arena] = source
          paths_by_arena[arena] = path
        end
        force_inline_yield = case force = ENV["CRYSTAL_V2_FORCE_INLINE_YIELD"]?
                             when nil, "", "0", "false", "False", "FALSE" then false
                             else                                                true
                             end
        auto_disable_inline_yield = !options.link && !force_inline_yield
        disable_inline_yield = ENV.has_key?("CRYSTAL_V2_DISABLE_INLINE_YIELD") || auto_disable_inline_yield
        if auto_disable_inline_yield && !ENV.has_key?("CRYSTAL_V2_DISABLE_INLINE_YIELD")
          log(options, out_io, "  Auto: disabling inline-yield for --no-link (set CRYSTAL_V2_FORCE_INLINE_YIELD=1 to override)")
        end
        hir_converter = HIR::AstToHir.new(
          first_arena,
          input_file,
          sources_by_arena,
          paths_by_arena,
          disable_inline_yield: disable_inline_yield
        )
        link_libs.each { |lib_name| hir_converter.module.add_link_library(lib_name) }

        # Collect nodes by type
        def_nodes = [] of Tuple(Frontend::DefNode, Frontend::ArenaLike)
        class_nodes = [] of Tuple(Frontend::ClassNode, Frontend::ArenaLike)
        module_nodes = [] of Tuple(Frontend::ModuleNode, Frontend::ArenaLike)
        enum_nodes = [] of Tuple(Frontend::EnumNode, Frontend::ArenaLike)
        macro_nodes = [] of Tuple(Frontend::MacroDefNode, Frontend::ArenaLike)
        alias_nodes = [] of Tuple(Frontend::AliasNode, Frontend::ArenaLike)
        lib_nodes = [] of Tuple(Frontend::LibNode, Frontend::ArenaLike, Array(Tuple(Frontend::AnnotationNode, Frontend::ArenaLike)))
        constant_exprs = [] of Tuple(Frontend::ExprId, Frontend::ArenaLike)
        main_exprs = [] of Tuple(Frontend::ExprId, Frontend::ArenaLike)
        acyclic_types = Set(String).new

        flags = Runtime.target_flags
        all_arenas.each do |arena, exprs, file_path, source|
          next if skip_file_directive?(source, flags)
          pending_annotations = [] of Tuple(Frontend::AnnotationNode, Frontend::ArenaLike)
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

        top_level_type_names = Set(String).new
        class_nodes.each { |node, _| top_level_type_names.add(String.new(node.name)) }
        module_nodes.each { |node, _| top_level_type_names.add(String.new(node.name)) }
        enum_nodes.each { |node, _| top_level_type_names.add(String.new(node.name)) }
        alias_nodes.each { |node, _| top_level_type_names.add(String.new(node.name)) }
        lib_nodes.each { |node, _, _| top_level_type_names.add(String.new(node.name)) }
        hir_converter.seed_top_level_type_names(top_level_type_names)
        top_level_class_kinds = {} of String => Bool
        class_nodes.each do |node, _|
          name = String.new(node.name)
          top_level_class_kinds[name] = node.is_struct == true
        end
        hir_converter.seed_top_level_class_kinds(top_level_class_kinds)

        # Pre-scan constant definitions so nested classes can resolve outer constants
        # across reopened types (require order interleaves files).
        debug_filter = ENV["DEBUG_PRE_SCAN_CONST"]?
        scan_constants_in_body = ->(owner : String, arena : Frontend::ArenaLike, body : Array(Frontend::ExprId)) do
          stack = [body]
          while current = stack.pop?
            current.each do |expr_id|
              expr_node = arena[expr_id]
              while expr_node.is_a?(Frontend::VisibilityModifierNode)
                expr_node = arena[expr_node.expression]
              end
              case expr_node
              when Frontend::BlockNode
                stack << expr_node.body
              when Frontend::ConstantNode
                if debug_filter && (debug_filter == "1" || String.new(expr_node.name) == debug_filter)
                  path = paths_by_arena[arena]? || "(unknown)"
                  STDERR.puts "[PRE_SCAN_CONST] owner=#{owner} name=#{String.new(expr_node.name)} file=#{path}"
                end
                hir_converter.register_constant(expr_node, owner)
              when Frontend::AssignNode
                target = arena[expr_node.target]
                if target.is_a?(Frontend::ConstantNode)
                  if debug_filter && (debug_filter == "1" || String.new(target.name) == debug_filter)
                    path = paths_by_arena[arena]? || "(unknown)"
                    STDERR.puts "[PRE_SCAN_CONST] owner=#{owner} name=#{String.new(target.name)} file=#{path}"
                  end
                  hir_converter.register_constant_value(String.new(target.name), expr_node.value, arena, owner)
                end
              end
            end
          end
        end

        scan_module_body = ->(prefix : String, arena : Frontend::ArenaLike, body : Array(Frontend::ExprId)) do
          stack = [{prefix: prefix, body: body}]
          while current = stack.pop?
            current[:body].each do |expr_id|
              expr_node = arena[expr_id]
              while expr_node.is_a?(Frontend::VisibilityModifierNode)
                expr_node = arena[expr_node.expression]
              end
              case expr_node
              when Frontend::ModuleNode
                next unless mod_body = expr_node.body
                name = String.new(expr_node.name)
                full_name = current[:prefix].empty? ? name : "#{current[:prefix]}::#{name}"
                stack << {prefix: full_name, body: mod_body}
              when Frontend::ClassNode
                next unless class_body = expr_node.body
                name = String.new(expr_node.name)
                full_name = if current[:prefix].empty? || name.includes?("::")
                              name
                            else
                              "#{current[:prefix]}::#{name}"
                            end
                scan_constants_in_body.call(full_name, arena, class_body)
              end
            end
          end
        end

        class_nodes.each do |class_node, arena|
          next unless body = class_node.body
          hir_converter.arena = arena
          class_name = String.new(class_node.name)
          scan_constants_in_body.call(class_name, arena, body)
        end

        module_nodes.each do |module_node, arena|
          next unless body = module_node.body
          hir_converter.arena = arena
          module_name = String.new(module_node.name)
          scan_module_body.call(module_name, arena, body)
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
        lib_nodes.each { |n, a, annotations| hir_converter.arena = a; hir_converter.register_lib(n, annotations) }
        log(options, out_io, "    Aliases: #{alias_nodes.size}")
        alias_nodes.each { |n, a| hir_converter.arena = a; hir_converter.register_alias(n) }
        log(options, out_io, "    Macros: #{macro_nodes.size}")
        macro_nodes.each_with_index do |(n, a), i|
          STDERR.print "\r    Registered macro #{i+1}/#{macro_nodes.size}" if options.progress
          hir_converter.arena = a
          hir_converter.register_macro(n)
        end
        STDERR.puts if options.progress
        log(options, out_io, "    Modules: #{module_nodes.size}")
        module_nodes.each_with_index do |(n, a), i|
          hir_converter.arena = a
          if options.progress && ENV["CRYSTAL_V2_PROGRESS_MODULE_NAMES"]?
            STDERR.puts "\n    Module #{i + 1}/#{module_nodes.size}: #{String.new(n.name)}"
          end
          hir_converter.register_module(n)
          if options.progress && (i % 10 == 0 || i == module_nodes.size - 1)
            STDERR.print "\r    Registered module #{i + 1}/#{module_nodes.size}"
          end
        end
        STDERR.puts if options.progress
        log(options, out_io, "    Classes: #{class_nodes.size}")
        class_nodes.each_with_index do |(n, a), i|
          hir_converter.arena = a
          hir_converter.register_class(n)
          STDERR.print "\r    Registered class #{i+1}/#{class_nodes.size}" if options.progress && (i % 10 == 0 || i == class_nodes.size - 1)
        end
        STDERR.puts if options.progress

        log(options, out_io, "    Constants: #{constant_exprs.size}")
        constant_exprs.each do |expr_id, arena|
          hir_converter.arena = arena
          node = arena[expr_id]
          case node
          when Frontend::ConstantNode
            hir_converter.register_constant(node)
          when Frontend::AssignNode
            target = arena[node.target]
            if target.is_a?(Frontend::ConstantNode)
              hir_converter.register_constant_value(String.new(target.name), node.value, arena)
            end
          end
        end

        # Flush pending monomorphizations now that all templates are registered
        log(options, out_io, "  Flushing pending monomorphizations...")
        hir_converter.flush_pending_monomorphizations

        # Refresh union descriptors now that all types are registered
        hir_converter.refresh_union_descriptors
        # Refresh generic type params that were captured as VOID before aliases existed
        hir_converter.refresh_void_type_params

        # Pass 2: Register function signatures
        log(options, out_io, "  Pass 2: Registering #{def_nodes.size} function signatures...")
        def_nodes.each_with_index do |(n, a), i|
          hir_converter.arena = a
          hir_converter.register_function(n)
          STDERR.print "\r    Registered function #{i+1}/#{def_nodes.size}" if options.progress && (i % 50 == 0 || i == def_nodes.size - 1)
        end
        STDERR.puts if options.progress

        # Fix inherited ivars: ensure subclasses include parent ivars with correct offsets.
        # Must run before function lowering since GEP offsets are baked into HIR instructions.
        hir_converter.fixup_inherited_ivars

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
        else
          # Keep runtime contract stable: Crystal.main_user_code always expects
          # __crystal_main(argc, argv), even when user code has no top-level
          # expressions and no explicit main definition.
          hir_converter.lower_main(main_exprs)
        end

        after_lower_main = hir_converter.module.function_count
        STDERR.puts "[PHASE_STATS] After lower_main: #{after_lower_main} functions" if ENV.has_key?("CRYSTAL_V2_PHASE_STATS")

        # Pass 2.5: AST reachability pre-filter (experimental, opt-in)
        # AST reachability pre-filter: skip functions whose method name was never
        # seen transitively from main. Enable with CRYSTAL_V2_AST_FILTER=1.
        if ENV.has_key?("CRYSTAL_V2_AST_FILTER")
          ast_filter_start = Time.instant
          ast_result = hir_converter.compute_ast_reachable_functions(main_exprs)
          hir_converter.set_ast_reachable_filter(ast_result[:defs], ast_result[:method_names], ast_result[:owner_types], ast_result[:method_bases])
          if ENV.has_key?("CRYSTAL_V2_PHASE_STATS")
            STDERR.puts "[PHASE_STATS] AST filter: #{ast_result[:defs].size}/#{hir_converter.function_defs_count} defs reachable, #{ast_result[:method_names].size} method names in #{(Time.instant - ast_filter_start).total_milliseconds.round(1)}ms"
          end
        elsif ENV.has_key?("CRYSTAL_V2_PHASE_STATS")
          # Just compute for analysis, don't activate filter
          ast_filter_start = Time.instant
          ast_result = hir_converter.compute_ast_reachable_functions(main_exprs)
          STDERR.puts "[PHASE_STATS] AST analysis: #{ast_result[:defs].size}/#{hir_converter.function_defs_count} defs reachable, #{ast_result[:method_names].size} method names (analysis-only, #{(Time.instant - ast_filter_start).total_milliseconds.round(1)}ms)"
        end

        force_safety_nets = case force = ENV["CRYSTAL_V2_FORCE_SAFETY_NETS"]?
                            when nil, "", "0", "false", "False", "FALSE" then false
                            else                                                 true
                            end
        skip_safety_nets = case skip = ENV["CRYSTAL_V2_SKIP_SAFETY_NETS"]?
                           when nil, "", "0", "false", "False", "FALSE" then false
                           else                                               true
                           end
        run_safety_nets = true
        if skip_safety_nets && !force_safety_nets
          run_safety_nets = false
          log(options, out_io, "  Warning: skipping HIR safety-net lowering (experimental, may produce invalid IR)")
        end

        # Ensure top-level `fun main` is lowered as a real entrypoint (C ABI).
        did_flush = false
        if fun_main = def_nodes.find { |(n, _)| n.receiver.try { |recv| String.new(recv) == HIR::AstToHir::FUN_DEF_RECEIVER } && String.new(n.name) == "main" }
          hir_converter.arena = fun_main[1]
          hir_converter.lower_def(fun_main[0])
          # Process any pending functions from fun main lowering (e.g., Crystal.init_runtime)
          hir_converter.flush_pending_functions(run_safety_nets)
          did_flush = true
        end
        hir_converter.flush_pending_functions(run_safety_nets) unless did_flush
        STDERR.puts "  Main function created" if options.progress

        # Refresh generic type params that were captured as VOID after lowering.
        hir_converter.refresh_void_type_params

        STDERR.puts "  Getting HIR module..." if options.progress
        hir_module = hir_converter.module
        STDERR.puts "  Got HIR module with #{hir_module.functions.size} functions" if options.progress
        options.link_libraries = hir_module.link_libraries.dup
        log(options, out_io, "  Functions: #{hir_module.functions.size}")
        timings["hir"] = (Time.instant - hir_start).total_milliseconds if options.stats
        timings["hir_funcs"] = hir_module.functions.size.to_f if options.stats

        # Reduce later phases by keeping only functions reachable from entrypoints.
        reachable = hir_module.reachable_function_names(["__crystal_main", "main"])
        if !reachable.empty? && reachable.size < hir_module.functions.size
          total_before = hir_module.functions.size
          hir_module.functions.select! { |func| reachable.includes?(func.name) }
          discarded = total_before - hir_module.functions.size
          rta_msg = "  Reachable functions: #{hir_module.functions.size}/#{total_before} (discarded #{discarded}, #{(discarded * 100.0 / total_before).round(1)}%)"
          log(options, out_io, rta_msg)
          STDERR.puts "[PHASE_STATS] RTA: #{rta_msg.strip}" if ENV.has_key?("CRYSTAL_V2_PHASE_STATS")
        end
        timings["hir_reachable_funcs"] = hir_module.functions.size.to_f if options.stats

        if options.emit_hir
          hir_file = options.output + ".hir"
          File.write(hir_file, hir_module.to_s)
          log(options, out_io, "  Wrote: #{hir_file}")
        end

        if ENV.has_key?("CRYSTAL_V2_STOP_AFTER_HIR")
          log(options, out_io, "  Stop after HIR (CRYSTAL_V2_STOP_AFTER_HIR)")
          emit_timings(options, out_io, timings, total_start)
          return 0
        end

        # Step 3: Escape analysis
        log(options, out_io, "\n[3/6] Escape analysis...")
        escape_start = Time.instant
        total_funcs = hir_module.functions.size
        memory_config = memory_config_for(options)
        type_provider = HIR::ClassInfoTypeProvider.new(hir_module, hir_converter.class_info, acyclic_types)
        total_ms_stats = HIR::MemoryStrategyResult::Stats.new
        gc_functions = [] of Tuple(String, Int32)
        gc_details = [] of String
        total_gc = 0
        ea_skipped = 0
        hir_module.functions.each_with_index do |func, idx|
          if options.progress && (idx % 1000 == 0 || idx == total_funcs - 1)
            STDERR.puts "  Escape analysis: #{idx + 1}/#{total_funcs}..."
          end

          # Fast-path: skip EA for functions with no allocation instructions
          has_alloc = false
          func.blocks.each do |block|
            block.instructions.each do |inst|
              if inst.is_a?(HIR::Allocate) || inst.is_a?(HIR::ArrayLiteral) || inst.is_a?(HIR::StringInterpolation)
                has_alloc = true
                break
              end
            end
            break if has_alloc
          end
          unless has_alloc
            ea_skipped += 1
            next
          end

          ms = HIR::MemoryStrategyAssigner.new(func, memory_config, type_provider, hir_module)
          result = ms.assign
          stats = result.stats
          total_ms_stats.stack_count += stats.stack_count
          total_ms_stats.slab_count += stats.slab_count
          total_ms_stats.arc_count += stats.arc_count
          total_ms_stats.atomic_arc_count += stats.atomic_arc_count
          total_ms_stats.gc_count += stats.gc_count
          if options.no_gc && stats.gc_count > 0
            total_gc += stats.gc_count
            gc_functions << {func.name, stats.gc_count}
            gc_allocation_details(func, result, hir_module, memory_config).each do |detail|
              gc_details << detail
            end
          end
        end
        timings["escape"] = (Time.instant - escape_start).total_milliseconds if options.stats
        timings["ea_skipped"] = ea_skipped.to_f if options.stats
        if options.stats
          timings["mm_stack"] = total_ms_stats.stack_count.to_f
          timings["mm_slab"] = total_ms_stats.slab_count.to_f
          timings["mm_arc"] = total_ms_stats.arc_count.to_f
          timings["mm_atomic"] = total_ms_stats.atomic_arc_count.to_f
          timings["mm_gc"] = total_ms_stats.gc_count.to_f
        end
        if options.no_gc && total_gc > 0
          err_io.puts "error: --no-gc requested but #{total_gc} allocation(s) require GC"
          gc_functions.sort_by { |(_, count)| -count }.first(10).each do |(name, count)|
            err_io.puts "  #{name}: #{count}"
          end
          gc_details.first(20).each do |detail|
            err_io.puts "  #{detail}"
          end
          if gc_details.size > 20
            err_io.puts "  ... #{gc_details.size - 20} more GC allocation(s)"
          end
          emit_timings(options, out_io, timings, total_start)
          return 1
        end

        # Step 4: Lower to MIR
        log(options, out_io, "\n[4/6] Lowering to MIR...")
        mir_start = Time.instant
        mir_lowering = MIR::HIRToMIRLowering.new(hir_module, slab_frame: options.slab_frame)

        # Register globals from class variables
        globals = [] of Tuple(String, HIR::TypeRef, Int64?)
        hir_converter.class_info.each do |class_name, info|
          info.class_vars.each do |cvar|
            global_name = MIR::HIRToMIRLowering.class_var_global_name(class_name, cvar.name)
            globals << {global_name, cvar.type, cvar.initial_value}
          end
        end

        # Register lib/module/class constants as globals with initial values
        # Without this, constants like LibC::EVFILT_USER are zero-initialized
        registered_globals = globals.map { |g| g[0] }.to_set
        hir_converter.constant_literal_values.each do |full_name, macro_value|
          next unless macro_value.is_a?(CrystalV2::Compiler::Semantic::MacroNumberValue)
          # Skip float constants — they can't be stored as Int64 initial values
          next if macro_value.value.is_a?(Float64)
          if idx = full_name.rindex("::")
            owner = full_name[0, idx]
            const_name = full_name[(idx + 2)..]
          else
            owner = "Object"
            const_name = full_name
          end
          global_name = MIR::HIRToMIRLowering.class_var_global_name(owner, const_name)
          next if registered_globals.includes?(global_name)
          const_type = hir_converter.constant_types[full_name]? || HIR::TypeRef::INT32
          globals << {global_name, const_type, macro_value.value.as(Int64)}
          registered_globals.add(global_name)
        end

        mir_lowering.register_globals(globals)
        mir_lowering.register_extern_globals(hir_module.extern_globals)
        mir_lowering.register_union_types(hir_converter.union_descriptors)
        mir_lowering.register_class_types(hir_converter.class_info)
        mir_lowering.register_enum_types(hir_converter.enum_names, hir_module.types)
        mir_lowering.register_tuple_types(hir_module.types)

        STDERR.puts "  Lowering #{hir_module.functions.size} functions to MIR..." if options.progress
        mir_module = mir_lowering.lower(options.progress)
        log(options, out_io, "  Functions: #{mir_module.functions.size}")
        timings["mir"] = (Time.instant - mir_start).total_milliseconds if options.stats
        timings["mir_funcs"] = mir_module.functions.size.to_f if options.stats

        # Optimize MIR
        if options.mir_opt
          log(options, out_io, "  Optimizing MIR...")
          mir_opt_start = Time.instant
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
          timings["mir_opt"] = (Time.instant - mir_opt_start).total_milliseconds if options.stats
        else
          log(options, out_io, "  Skipping MIR optimizations (--no-mir-opt)")
          timings["mir_opt"] = 0.0 if options.stats
        end

        if options.emit_mir
          mir_file = options.output + ".mir"
          File.write(mir_file, mir_module.to_s)
          log(options, out_io, "  Wrote: #{mir_file}")
        end

        if ENV.has_key?("CRYSTAL_V2_STOP_AFTER_MIR")
          log(options, out_io, "  Stop after MIR (CRYSTAL_V2_STOP_AFTER_MIR)")
          emit_timings(options, out_io, timings, total_start)
          return 0
        end

        # Step 5: Generate LLVM IR
        log(options, out_io, "\n[5/6] Generating LLVM IR...")
        llvm_start = Time.instant
        llvm_gen = MIR::LLVMIRGenerator.new(mir_module)
        llvm_gen.emit_type_metadata = options.emit_type_metadata
        llvm_gen.progress = options.progress
        llvm_gen.reachability = true  # Only emit reachable functions from main
        llvm_gen.no_prelude = options.no_prelude

        # Pass constant literal values for global initialization (e.g., Math::PI)
        const_init = {} of String => (Float64 | Int64)
        hir_converter.constant_literal_values.each do |name, value|
          if value.is_a?(CrystalV2::Compiler::Semantic::MacroNumberValue)
            # Convert constant name (Math::PI) to global name (Math__classvar__PI)
            if idx = name.rindex("::")
              owner = name[0, idx]
              const_name = name[(idx + 2)..-1]
            else
              owner = "Object"
              const_name = name
            end
            global_name = "#{owner}__classvar__#{const_name}"
            const_init[global_name] = value.value
          end
        end
        llvm_gen.constant_initial_values = const_init unless const_init.empty?

        llvm_ir = llvm_gen.generate
        log(options, out_io, "  LLVM IR size: #{llvm_ir.size} bytes")
        timings["llvm"] = (Time.instant - llvm_start).total_milliseconds if options.stats

        ll_file = options.output + ".ll"
        File.write(ll_file, llvm_ir)
        log(options, out_io, "  Wrote: #{ll_file}")

        if options.emit_llvm
          emit_timings(options, out_io, timings, total_start)
          out_io.puts llvm_ir
          return 0
        end

        # Save to pipeline cache on miss
        if options.pipeline_cache && !pipeline_cache_file.empty?
          FileUtils.cp(ll_file, pipeline_cache_file)
          File.write(pipeline_cache_file + ".libs", options.link_libraries.join("\n") + "\n")
          @pipeline_cache_misses += 1
          log(options, out_io, "  Pipeline cache MISS → saved")
        end

        end # unless pipeline_cache_hit

        # Step 6: Compile to binary
        log(options, out_io, "\n[6/6] Compiling to binary...")
        compile_start = Time.instant
        result = compile_llvm_ir(ll_file, options, out_io, err_io, timings)
        timings["compile"] = (Time.instant - compile_start).total_milliseconds if options.stats
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
          opt_start = Time.instant
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
          timings["opt"] = (Time.instant - opt_start).total_milliseconds if options.stats
        end

        use_clang_link = options.lto || options.pgo_generate || !options.pgo_profile.empty?
        if use_clang_link && !options.link
          err_io.puts "error: --lto/--pgo-* require linking (remove --no-link)"
          return 1
        end

        llc_start = Time.instant
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
          timings["llc"] = (Time.instant - llc_start).total_milliseconds if options.stats
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
          link_start = Time.instant
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
          timings["link"] = (Time.instant - link_start).total_milliseconds if options.stats
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
          when "ldflags"
            next if value.empty?
            # Execute backtick commands if present
            expanded = if value.starts_with?("`") && value.ends_with?("`")
                         cmd = value[1..-2]
                         `sh -c #{cmd.inspect}`.strip
                       else
                         value
                       end
            next if expanded.empty?
            # Add each flag separately
            expanded.split.each do |flag|
              next if seen.includes?(flag)
              flags << flag
              seen << flag
            end
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
        results : Array(Tuple(Frontend::ArenaLike, Array(Frontend::ExprId), String, String)),
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

        source = File.read(abs_path)

        {% unless flag?(:bootstrap_fast) %}
        if options.ast_cache
          if cached = LSP::AstCache.load(abs_path)
            @ast_cache_hits += 1
            arena = cached.arena
            exprs = cached.roots
            base_dir = File.dirname(abs_path)
            if cached_requires = load_require_cache(abs_path)
              if source_has_glob_require?(source) || cached_requires.any? { |path| !File.exists?(path) }
                cached_requires = nil
              end
            end
            if cached_requires
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
            results << {arena, exprs, abs_path, source}
            log(options, out_io, "  AST cache hit: #{abs_path}") if options.verbose
            return
          end
          @ast_cache_misses += 1
        end
        {% end %}

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

        results << {arena, exprs, abs_path, source}

        {% unless flag?(:bootstrap_fast) %}
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
        {% end %}
      end

      # Process a node for require statements (recursively handles macro bodies)
      private def process_require_node(
        arena : Frontend::ArenaLike,
        expr_id : Frontend::ExprId,
        base_dir : String,
        input_file : String,
        results : Array(Tuple(Frontend::ArenaLike, Array(Frontend::ExprId), String, String)),
        loaded : Set(String),
        options : Options,
        out_io : IO,
        requires_out : Array(String)? = nil
      )
        node = arena[expr_id]
        case node
        when Frontend::ModuleNode
          if body = node.body
            body.each do |child_id|
              process_require_node(arena, child_id, base_dir, input_file, results, loaded, options, out_io, requires_out)
            end
          end
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

      private def source_has_glob_require?(source : String) : Bool
        source.each_line do |line|
          next unless line.includes?("require")
          if line.matches?(/\brequire\s+["'][^"']*[\*\?\[]/)
            return true
          end
        end
        false
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
        all_arenas : Array(Tuple(Frontend::ArenaLike, Array(Frontend::ExprId), String, String)),
        options : Options,
        out_io : IO
      ) : Array(String)
        libraries = [] of String
        all_arenas.each do |arena, exprs, _, _|
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

      private def collect_top_level_nodes(
        arena : Frontend::ArenaLike,
        expr_id : Frontend::ExprId,
        def_nodes : Array(Tuple(Frontend::DefNode, Frontend::ArenaLike)),
        class_nodes : Array(Tuple(Frontend::ClassNode, Frontend::ArenaLike)),
        module_nodes : Array(Tuple(Frontend::ModuleNode, Frontend::ArenaLike)),
        enum_nodes : Array(Tuple(Frontend::EnumNode, Frontend::ArenaLike)),
        macro_nodes : Array(Tuple(Frontend::MacroDefNode, Frontend::ArenaLike)),
        alias_nodes : Array(Tuple(Frontend::AliasNode, Frontend::ArenaLike)),
        lib_nodes : Array(Tuple(Frontend::LibNode, Frontend::ArenaLike, Array(Tuple(Frontend::AnnotationNode, Frontend::ArenaLike)))),
        constant_exprs : Array(Tuple(Frontend::ExprId, Frontend::ArenaLike)),
        main_exprs : Array(Tuple(Frontend::ExprId, Frontend::ArenaLike)),
        pending_annotations : Array(Tuple(Frontend::AnnotationNode, Frontend::ArenaLike)),
        acyclic_types : Set(String),
        flags : Set(String),
        sources_by_arena : Hash(Frontend::ArenaLike, String),
        source : String,
        depth : Int32 = 0,
        collect_main_exprs : Bool = true
      ) : Nil
        return if depth > 4
        node = arena[expr_id]
        case node
        when Frontend::DefNode
          def_nodes << {node, arena}
          pending_annotations.clear
        when Frontend::ClassNode
          class_nodes << {node, arena}
          if pending_annotation_has?(pending_annotations, "Acyclic")
            acyclic_types << String.new(node.name)
          end
          pending_annotations.clear
        when Frontend::ModuleNode
          module_nodes << {node, arena}
          pending_annotations.clear
        when Frontend::EnumNode
          enum_nodes << {node, arena}
          pending_annotations.clear
        when Frontend::MacroDefNode
          macro_nodes << {node, arena}
          pending_annotations.clear
        when Frontend::ConstantNode
          constant_exprs << {expr_id, arena}
          main_exprs << {expr_id, arena} if collect_main_exprs
          pending_annotations.clear
        when Frontend::AliasNode
          alias_nodes << {node, arena}
          pending_annotations.clear
        when Frontend::LibNode
          lib_nodes << {node, arena, pending_annotations.dup}
          pending_annotations.clear
        when Frontend::AnnotationNode
          pending_annotations << {node, arena}
        when Frontend::RequireNode
          # Skip - already processed
        when Frontend::MacroExpressionNode
          collect_top_level_nodes(arena, node.expression, def_nodes, class_nodes, module_nodes, enum_nodes, macro_nodes, alias_nodes, lib_nodes, constant_exprs, main_exprs, pending_annotations, acyclic_types, flags, sources_by_arena, source, depth, collect_main_exprs)
        when Frontend::VisibilityModifierNode
          collect_top_level_nodes(arena, node.expression, def_nodes, class_nodes, module_nodes, enum_nodes, macro_nodes, alias_nodes, lib_nodes, constant_exprs, main_exprs, pending_annotations, acyclic_types, flags, sources_by_arena, source, depth, collect_main_exprs)
        when Frontend::MacroIfNode
          if ENV["DEBUG_MACRO_EXPAND"]?
            STDERR.puts "[DEBUG_MACRO_EXPAND] MacroIfNode condition=#{evaluate_macro_condition(arena, node.condition, flags).inspect}"
          end
          if raw_text = macro_if_raw_text(node, source)
            # If the raw text contains {% for %} loops, don't use macro_literal_texts_from_raw
            # which doesn't handle for-loops. Instead fall through to MacroLiteralNode processing.
            has_for_loop = raw_text.includes?("{% for") || raw_text.includes?("{%- for") || raw_text.includes?("{%~ for")
            unless has_for_loop
              parsed_any = false
              combined = macro_literal_texts_from_raw(raw_text, flags).join
              if ENV["DEBUG_MACRO_EXPAND"]?
                STDERR.puts "[DEBUG_MACRO_EXPAND] MacroIfNode combined empty=#{combined.strip.empty?} has_percent=#{combined.includes?("{%")} size=#{combined.size}"
                if combined.size < 200
                  STDERR.puts "[DEBUG_MACRO_EXPAND] MacroIfNode combined content=#{combined.inspect}"
                end
              end
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
              if ENV["DEBUG_MACRO_EXPAND"]? && parsed_any
                STDERR.puts "[DEBUG_MACRO_EXPAND] MacroIfNode early return (parsed_any)"
              end
              return if parsed_any
            end
          end
          if ENV["DEBUG_MACRO_EXPAND"]?
            STDERR.puts "[DEBUG_MACRO_EXPAND] MacroIfNode continuing to condition check (raw_text exists=#{!raw_text.nil?})"
          end
          condition = evaluate_macro_condition(arena, node.condition, flags)
          if condition == true
            if ENV["DEBUG_MACRO_EXPAND"]?
              then_node = arena[node.then_body]
              STDERR.puts "[DEBUG_MACRO_EXPAND] MacroIfNode then_body type=#{then_node.class}"
            end
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
        when Frontend::MacroLiteralNode
          # Check if this literal has macro control flow ({% for %}, {% begin %}, etc.)
          has_control_flow = node.pieces.any? { |p| p.kind.control_start? }
          if ENV["DEBUG_MACRO_EXPAND"]?
            STDERR.puts "[DEBUG_MACRO_EXPAND] MacroLiteralNode has_control_flow=#{has_control_flow} pieces=#{node.pieces.size}"
            node.pieces.each_with_index do |p, i|
              STDERR.puts "[DEBUG_MACRO_EXPAND]   piece[#{i}] kind=#{p.kind} keyword=#{p.control_keyword.inspect}"
            end
          end
          if has_control_flow
            # Use MacroExpander for full expansion of {% for %} loops, variable assignments, etc.
            if expanded = expand_macro_literal_via_expander(expr_id, arena, source, flags)
              if ENV["DEBUG_MACRO_EXPAND"]?
                STDERR.puts "[DEBUG_MACRO_EXPAND] expanded=#{expanded[0, [expanded.size, 200].min].inspect}"
              end
              unless expanded.strip.empty?
                if parsed = parse_top_level_macro_expansion(expanded)
                  program, exp_source = parsed
                  sources_by_arena[program.arena] = exp_source
                  program.roots.each do |inner_id|
                    collect_top_level_nodes(program.arena, inner_id, def_nodes, class_nodes, module_nodes, enum_nodes, macro_nodes, alias_nodes, lib_nodes, constant_exprs, main_exprs, pending_annotations, acyclic_types, flags, sources_by_arena, exp_source, depth + 1, false)
                  end
                end
              end
            end
          elsif raw_text = macro_literal_raw_text(node, source)
            # Track macro variable assignments (e.g., {% nums = %w(Int8 ...) %})
            track_macro_var_assignment(raw_text)
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
        when Frontend::MacroForNode
          expand_top_level_macro_for(node, arena, source, def_nodes, class_nodes, module_nodes, enum_nodes, macro_nodes, alias_nodes, lib_nodes, constant_exprs, main_exprs, pending_annotations, acyclic_types, flags, sources_by_arena, depth)
        when Frontend::AssignNode
          target = arena[node.target]
          if target.is_a?(Frontend::ConstantNode)
            constant_exprs << {expr_id, arena}
          end
          main_exprs << {expr_id, arena} if collect_main_exprs
        else
          main_exprs << {expr_id, arena} if collect_main_exprs
        end
      end

      # Track macro variable assignments from raw text like {% nums = %w(Int8 Int16 ...) %}
      private def track_macro_var_assignment(raw_text : String) : Nil
        text = raw_text.strip
        return unless text.starts_with?("{%") && text.ends_with?("%}")
        inner = text[2, text.size - 4].strip
        inner = inner.lstrip('-').lstrip('~').rstrip('-').rstrip('~').strip
        if eq_idx = inner.index('=')
          name = inner[0, eq_idx].strip
          value = inner[eq_idx + 1, inner.size - eq_idx - 1].strip
          # Only track simple identifiers
          if name.matches?(/\A[a-z_][a-z0-9_]*\z/)
            @macro_text_vars[name] = value
          end
        end
      end

      # Expand a top-level {% for %} macro loop (e.g., in primitives.cr)
      private def expand_top_level_macro_for(
        node : Frontend::MacroForNode,
        arena : Frontend::ArenaLike,
        source : String,
        def_nodes : Array(Tuple(Frontend::DefNode, Frontend::ArenaLike)),
        class_nodes : Array(Tuple(Frontend::ClassNode, Frontend::ArenaLike)),
        module_nodes : Array(Tuple(Frontend::ModuleNode, Frontend::ArenaLike)),
        enum_nodes : Array(Tuple(Frontend::EnumNode, Frontend::ArenaLike)),
        macro_nodes : Array(Tuple(Frontend::MacroDefNode, Frontend::ArenaLike)),
        alias_nodes : Array(Tuple(Frontend::AliasNode, Frontend::ArenaLike)),
        lib_nodes : Array(Tuple(Frontend::LibNode, Frontend::ArenaLike, Array(Tuple(Frontend::AnnotationNode, Frontend::ArenaLike)))),
        constant_exprs : Array(Tuple(Frontend::ExprId, Frontend::ArenaLike)),
        main_exprs : Array(Tuple(Frontend::ExprId, Frontend::ArenaLike)),
        pending_annotations : Array(Tuple(Frontend::AnnotationNode, Frontend::ArenaLike)),
        acyclic_types : Set(String),
        flags : Set(String),
        sources_by_arena : Hash(Frontend::ArenaLike, String),
        depth : Int32
      ) : Nil
        return if depth > 3

        iter_vars = node.iter_vars.map { |name| String.new(name) }
        return if iter_vars.empty?

        # Get body raw text from source
        body_node = arena[node.body]
        body_text = extract_span_text(body_node.span, source)
        return unless body_text

        # Resolve iterable to list of string values
        values = resolve_top_level_macro_iterable(arena, node.iterable, source)
        return unless values

        if ENV["DEBUG_MACRO_FOR"]?
          STDERR.puts "[DEBUG_MACRO_FOR] expand_top_level_macro_for: var=#{iter_vars.first} values=#{values.size} body_size=#{body_text.size}"
        end

        # Expand body for each value
        expanded = String.build do |io|
          values.each do |value|
            text = body_text
            if iter_vars.size == 1
              var_name = iter_vars[0]
              text = text.gsub("{{#{var_name}.id}}", value)
              text = text.gsub("{{ #{var_name}.id }}", value)
              text = text.gsub("{{#{var_name}}}", value)
              text = text.gsub("{{ #{var_name} }}", value)
            end
            io << text
            io << "\n"
          end
        end

        return if expanded.strip.empty?

        # Parse the expanded text (which may contain {% %} / {{ }} inside struct bodies)
        if parsed = parse_top_level_macro_expansion(expanded)
          program, exp_source = parsed
          sources_by_arena[program.arena] = exp_source
          program.roots.each do |inner_id|
            collect_top_level_nodes(program.arena, inner_id, def_nodes, class_nodes, module_nodes, enum_nodes, macro_nodes, alias_nodes, lib_nodes, constant_exprs, main_exprs, pending_annotations, acyclic_types, flags, sources_by_arena, exp_source, depth + 1, false)
          end
        end
      end

      # Extract raw text from a span in the source
      private def extract_span_text(span : Frontend::Span, source : String) : String?
        start = span.start_offset
        length = span.end_offset - span.start_offset
        return nil if length <= 0 || start < 0 || start >= source.bytesize
        length = source.bytesize - start if start + length > source.bytesize
        source.byte_slice(start, length)
      end

      # Resolve a macro for-loop iterable to a list of string values
      private def resolve_top_level_macro_iterable(
        arena : Frontend::ArenaLike,
        iterable_id : Frontend::ExprId,
        source : String
      ) : Array(String)?
        node = arena[iterable_id]

        # Unwrap MacroExpressionNode
        if node.is_a?(Frontend::MacroExpressionNode)
          return resolve_top_level_macro_iterable(arena, node.expression, source)
        end

        # Try to get raw text of the iterable expression
        if iterable_text = extract_span_text(node.span, source)
          iterable_text = iterable_text.strip

          # Direct %w() word list
          if iterable_text.starts_with?("%w(") && iterable_text.ends_with?(")")
            inner = iterable_text[3, iterable_text.size - 4]
            return inner.split(/\s+/).reject(&.empty?)
          end
          if iterable_text.starts_with?("%w[") && iterable_text.ends_with?("]")
            inner = iterable_text[3, iterable_text.size - 4]
            return inner.split(/\s+/).reject(&.empty?)
          end
          if iterable_text.starts_with?("%w{") && iterable_text.ends_with?("}")
            inner = iterable_text[3, iterable_text.size - 4]
            return inner.split(/\s+/).reject(&.empty?)
          end

          # Variable reference - look up in tracked macro vars
          if iterable_text.matches?(/\A[a-z_][a-z0-9_]*\z/)
            if var_value = @macro_text_vars[iterable_text]?
              return resolve_macro_text_value(var_value)
            end
          end
        end

        nil
      end

      # Parse a macro variable value text into a list of strings
      private def resolve_macro_text_value(text : String) : Array(String)?
        text = text.strip
        # %w() word list
        if text.starts_with?("%w(") && text.ends_with?(")")
          inner = text[3, text.size - 4]
          return inner.split(/\s+/).reject(&.empty?)
        end
        if text.starts_with?("%w[") && text.ends_with?("]")
          inner = text[3, text.size - 4]
          return inner.split(/\s+/).reject(&.empty?)
        end
        if text.starts_with?("%w{") && text.ends_with?("}")
          inner = text[3, text.size - 4]
          return inner.split(/\s+/).reject(&.empty?)
        end
        # Variable reference to another macro var
        if text.matches?(/\A[a-z_][a-z0-9_]*\z/)
          if var_value = @macro_text_vars[text]?
            return resolve_macro_text_value(var_value)
          end
        end
        nil
      end

      # Use the MacroExpander to fully expand a MacroLiteralNode that contains
      # {% for %}, {% if %}, {{ expr }}, and variable assignments.
      private def expand_macro_literal_via_expander(
        body_id : Frontend::ExprId,
        arena : Frontend::ArenaLike,
        source : String,
        flags : Set(String)
      ) : String?
        dummy_program = Frontend::Program.new(arena, [] of Frontend::ExprId)
        expander = Semantic::MacroExpander.new(
          dummy_program, arena, flags,
          recovery_mode: true,
          macro_source: source
        )
        expanded = expander.expand_literal(
          body_id,
          variables: {} of String => Semantic::MacroValue
        )
        if ENV["DEBUG_MACRO_EXPAND"]?
          STDERR.puts "[DEBUG_MACRO_EXPAND] expand_literal returned #{expanded.bytesize} bytes, empty=#{expanded.strip.empty?}"
          if expanded.bytesize > 0 && expanded.bytesize < 500
            STDERR.puts "[DEBUG_MACRO_EXPAND] content=#{expanded.inspect}"
          end
          if expander.diagnostics.any?
            expander.diagnostics.each do |d|
              STDERR.puts "[DEBUG_MACRO_EXPAND] diagnostic: #{d.message}"
            end
          end
        end
        return nil if expanded.strip.empty?
        expanded
      end

      # Parse expanded macro text allowing {% %} and {{ }} inside struct/class bodies
      private def parse_top_level_macro_expansion(text : String) : {Frontend::Program, String}?
        trimmed = text.strip
        return nil if trimmed.empty?
        lexer = Frontend::Lexer.new(text)
        parser = Frontend::Parser.new(lexer, recovery_mode: true)
        program = parser.parse_program
        return nil if program.roots.empty?
        {program, text}
      end

      private def skip_file_directive?(source : String, flags : Set(String)) : Bool
        bytes = source.to_slice
        size = bytes.size
        idx = 0
        loop do
          while idx < size && (bytes[idx] == ' '.ord || bytes[idx] == '\t'.ord || bytes[idx] == '\r'.ord || bytes[idx] == '\n'.ord)
            idx += 1
          end
          break if idx >= size
          if bytes[idx] == '#'.ord
            while idx < size && bytes[idx] != '\n'.ord
              idx += 1
            end
            next
          end
          break
        end
        return false unless idx + 1 < size && bytes[idx] == '{'.ord && bytes[idx + 1] == '%'.ord

        tag_end = source.index("%}", idx)
        return false unless tag_end

        tag_start = idx + 2
        tag = source.byte_slice(tag_start, tag_end - tag_start)
        tag = tag.strip
        tag = tag.lstrip('-').lstrip('~').rstrip('-').rstrip('~').strip
        return false unless tag.starts_with?("skip_file")

        cond_text = tag.sub(/^skip_file/, "").strip
        return true if cond_text.empty?

        if cond_text.starts_with?("if ")
          evaluate_macro_condition_text(cond_text.lstrip("if").strip, flags) == true
        elsif cond_text.starts_with?("unless ")
          cond = evaluate_macro_condition_text(cond_text.lstrip("unless").strip, flags)
          cond.nil? ? false : !cond
        else
          false
        end
      end

      private def collect_macro_literal_exprs(
        arena : Frontend::ArenaLike,
        node : Frontend::MacroLiteralNode,
        flags : Set(String)
      ) : Array(Frontend::ExprId)
        exprs = [] of Frontend::ExprId
        control_stack = [] of {Bool, Bool, Bool} # {parent_active, branch_taken, active}
        active = true

        node.pieces.each do |piece|
          case piece.kind
          when Frontend::MacroPiece::Kind::Expression
            if active && (expr = piece.expr)
              exprs << expr
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
            # ignore text/vars for top-level node collection
          end
        end

        exprs
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
          parse_link_annotation_args(args_text).each do |entry|
            libraries << entry
          end
        end
        libraries
      end

      private def parse_link_annotation_args(text : String) : Array(String)
        libraries = [] of String
        working = text.dup

        # Match named args with quoted values: key: "value" or key: 'value'
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

        # Match named args with backtick values: key: `command`
        working.scan(/(\w+)\s*:\s*`([^`]*)`/) do |match|
          key = match[1]
          value = "`#{match[2]}`"  # Re-add backticks for execution
          prefix = case key
                   when "pkg_config" then "pkg_config:"
                   when "framework"  then "framework:"
                   when "dll"        then "dll:"
                   else "#{key}:"
                   end
          libraries << "#{prefix}#{value}"
        end

        working = working.gsub(/(\w+)\s*:\s*["'][^"']*["']/, "")
        working = working.gsub(/(\w+)\s*:\s*`[^`]*`/, "")
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

      private def macro_literal_raw_text(
        node : Frontend::MacroLiteralNode,
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
        node : Frontend::MacroIfNode,
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

      private def parse_macro_literal_program(text : String) : {Frontend::Program, String}?
        sanitized = sanitize_macro_literal_text(text)
        trimmed = sanitized.strip
        return nil if trimmed.empty?
        return nil if sanitized.includes?("{%") || sanitized.includes?("{{")

        lexer = Frontend::Lexer.new(sanitized)
        parser = Frontend::Parser.new(lexer, recovery_mode: true)
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
        segment_start = 0
        bytes = text.to_slice
        size = bytes.size
        in_line_comment = false
        in_string = false
        in_char = false
        escape = false

        missing_end = false
        while idx + 1 < size
          if in_line_comment
            if bytes[idx] == '\n'.ord
              in_line_comment = false
            end
            idx += 1
            next
          elsif in_string
            if escape
              escape = false
            elsif bytes[idx] == '\\'.ord
              escape = true
            elsif bytes[idx] == '"'.ord
              in_string = false
            end
            idx += 1
            next
          elsif in_char
            if escape
              escape = false
            elsif bytes[idx] == '\\'.ord
              escape = true
            elsif bytes[idx] == '\''.ord
              in_char = false
            end
            idx += 1
            next
          else
            if bytes[idx] == '#'.ord
              in_line_comment = true
              idx += 1
              next
            elsif bytes[idx] == '"'.ord
              in_string = true
              idx += 1
              next
            elsif bytes[idx] == '\''.ord
              in_char = true
              idx += 1
              next
            end
          end

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
        flag_name = flag_name.strip.gsub(/^[:"']|["']$/, "")
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

          # Try original Crystal compiler source (for compiler modules like lexer/parser)
          if File.directory?(CRYSTAL_SRC_PATH)
            crystal_src = File.expand_path(req_path, CRYSTAL_SRC_PATH)
            result = try_require_path(crystal_src)
            return result if result
          end
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

      private def pending_annotation_has?(
        pending_annotations : Array(Tuple(Frontend::AnnotationNode, Frontend::ArenaLike)),
        name : String
      ) : Bool
        pending_annotations.any? do |ann_node, ann_arena|
          annotation_name_from_expr(ann_arena, ann_node.name) == name
        end
      end

      private def valid_mm_mode?(mode : String) : Bool
        mode == "conservative" || mode == "balanced" || mode == "aggressive"
      end

      private def memory_config_for(options : Options) : HIR::MemoryConfig
        mode = case options.mm_mode
               when "conservative" then HIR::MemoryConfig::Mode::Conservative
               when "balanced" then HIR::MemoryConfig::Mode::Balanced
               when "aggressive" then HIR::MemoryConfig::Mode::Aggressive
               else
                 HIR::MemoryConfig::Mode::Balanced
               end
        HIR::MemoryConfig.new(stack_threshold: options.mm_stack_threshold, mode: mode)
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

      private def log(options : Options, out_io : IO, msg : String)
        out_io.puts msg if options.verbose
      end

      private def emit_timings(options : Options, out_io : IO, timings : Hash(String, Float64)?, total_start : Time::Instant?)
        return unless options.stats
        return unless timings
        return unless total_start

        total_ms = (Time.instant - total_start).total_milliseconds
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
        if (ea_skip = timings["ea_skipped"]?)
          parts << "ea_skipped=#{ea_skip.to_i}"
        end
        if (mir = timings["mir"]?)
          parts << "mir=#{mir.round(1)}"
        end
        if (mm_stack = timings["mm_stack"]?)
          parts << "mm_stack=#{mm_stack.to_i}"
        end
        if (mm_slab = timings["mm_slab"]?)
          parts << "mm_slab=#{mm_slab.to_i}"
        end
        if (mm_arc = timings["mm_arc"]?)
          parts << "mm_arc=#{mm_arc.to_i}"
        end
        if (mm_atomic = timings["mm_atomic"]?)
          parts << "mm_atomic=#{mm_atomic.to_i}"
        end
        if (mm_gc = timings["mm_gc"]?)
          parts << "mm_gc=#{mm_gc.to_i}"
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
        parts << "pipeline_cache=#{@pipeline_cache_hits} hit/#{@pipeline_cache_misses} miss" if options.pipeline_cache
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

# Main entry point - only run when this file is the entry point, not when loaded as a library
# Use crystal_v2.cr or main.cr as entry points instead
