require "option_parser"
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

# Module aliases for convenience
alias HIR = Crystal::HIR
alias MIR = Crystal::MIR

module CrystalV2
  module Compiler
    VERSION = "0.1.0-dev"

    # Standard library path - relative to compiler source
    STDLIB_PATH = File.expand_path("../stdlib", File.dirname(__FILE__))

    class CLI
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

        # Get input file from remaining args
        input_file = @args.find { |a| !a.starts_with?("-") && a.ends_with?(".cr") }
        unless input_file
          err_io.puts "Error: No input file specified"
          err_io.puts parser
          return 1
        end

        options.input = input_file
        options.output = input_file.gsub(/\.cr$/, "") if options.output.empty?

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
        log(options, out_io, "=== Crystal v2 Compiler ===")
        log(options, out_io, "Input: #{input_file}")
        log(options, out_io, "Output: #{options.output}")

        # Step 1: Parse source (with require support)
        log(options, out_io, "\n[1/6] Parsing...")

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
            parse_file_recursive(prelude_path, all_arenas, loaded_files, input_file, options, out_io)
          end
        end

        # Parse user's input file
        parse_file_recursive(input_file, all_arenas, loaded_files, input_file, options, out_io)

        if all_arenas.empty?
          err_io.puts "error: no valid source files found"
          return 1
        end

        total_exprs = all_arenas.sum { |t| t[1].size }
        log(options, out_io, "  Files: #{all_arenas.size}, Expressions: #{total_exprs}")

        # Step 2: Lower to HIR
        log(options, out_io, "\n[2/6] Lowering to HIR...")

        first_arena = all_arenas[0][0]
        hir_converter = HIR::AstToHir.new(first_arena, input_file)

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
        log(options, out_io, "    Aliases: #{alias_nodes.size}")
        alias_nodes.each { |n, a| hir_converter.arena = a; hir_converter.register_alias(n) }

        # Pass 2: Register function signatures
        log(options, out_io, "  Pass 2: Registering #{def_nodes.size} function signatures...")
        def_nodes.each_with_index do |(n, a), i|
          hir_converter.arena = a
          hir_converter.register_function(n)
          STDERR.print "\r    Registered function #{i+1}/#{def_nodes.size}" if options.progress && (i % 50 == 0 || i == def_nodes.size - 1)
        end
        STDERR.puts if options.progress

        # Pass 3: Lower bodies
        log(options, out_io, "  Pass 3: Lowering bodies...")
        log(options, out_io, "    Modules: #{module_nodes.size}")
        module_nodes.each_with_index do |(n, a), i|
          name = String.new(n.name)
          # Skip huge modules for now (Enumerable has 126 methods)
          skip_modules = ["Enumerable", "Indexable", "Iterator"]
          if skip_modules.includes?(name)
            STDERR.puts "    Skipping module #{i+1}/#{module_nodes.size}: #{name} (too many methods)" if options.progress
            next
          end
          STDERR.puts "    Lowering module #{i+1}/#{module_nodes.size}: #{name}" if options.progress
          STDERR.flush if options.progress
          hir_converter.arena = a
          hir_converter.lower_module(n)
        end
        log(options, out_io, "    Classes: #{class_nodes.size}")
        class_nodes.each_with_index do |(n, a), i|
          hir_converter.arena = a
          hir_converter.lower_class(n)
          STDERR.print "\r    Lowered class #{i+1}/#{class_nodes.size}" if options.progress && (i % 10 == 0 || i == class_nodes.size - 1)
        end
        STDERR.puts if options.progress
        log(options, out_io, "    Functions: #{def_nodes.size}")
        def_nodes.each_with_index do |(n, a), i|
          hir_converter.arena = a
          hir_converter.lower_def(n)
          STDERR.print "\r    Lowered function #{i+1}/#{def_nodes.size}" if options.progress && (i % 50 == 0 || i == def_nodes.size - 1)
        end
        STDERR.puts if options.progress

        # Create main function from top-level expressions
        STDERR.puts "  Creating main function..." if options.progress
        if main_exprs.size > 0
          hir_converter.lower_main(main_exprs)
        end
        STDERR.puts "  Main function created" if options.progress

        STDERR.puts "  Getting HIR module..." if options.progress
        hir_module = hir_converter.module
        STDERR.puts "  Got HIR module with #{hir_module.functions.size} functions" if options.progress
        log(options, out_io, "  Functions: #{hir_module.functions.size}")

        if options.emit_hir
          hir_file = options.output + ".hir"
          File.write(hir_file, hir_module.to_s)
          log(options, out_io, "  Wrote: #{hir_file}")
        end

        # Step 3: Escape analysis
        log(options, out_io, "\n[3/6] Escape analysis...")
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

        # Step 4: Lower to MIR
        log(options, out_io, "\n[4/6] Lowering to MIR...")
        mir_lowering = MIR::HIRToMIRLowering.new(hir_module)

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

        # Optimize MIR
        log(options, out_io, "  Optimizing MIR...")
        mir_module.functions.each_with_index do |func, idx|
          begin
            STDERR.puts "  Optimizing #{idx + 1}/#{mir_module.functions.size}: #{func.name}..." if options.progress
            stats, potential = func.optimize_with_potential
            log(options, out_io, "    #{func.name} -> #{stats.total} changes") if options.verbose
          rescue ex : IndexError
            raise "Index error in optimize for: #{func.name}\n#{ex.message}\n#{ex.backtrace.join("\n")}"
          end
        end

        if options.emit_mir
          mir_file = options.output + ".mir"
          File.write(mir_file, mir_module.to_s)
          log(options, out_io, "  Wrote: #{mir_file}")
        end

        # Step 5: Generate LLVM IR
        log(options, out_io, "\n[5/6] Generating LLVM IR...")
        llvm_gen = MIR::LLVMIRGenerator.new(mir_module)
        llvm_gen.emit_type_metadata = true
        llvm_gen.progress = options.progress
        llvm_gen.reachability = true  # Only emit reachable functions from main
        llvm_ir = llvm_gen.generate
        log(options, out_io, "  LLVM IR size: #{llvm_ir.size} bytes")

        ll_file = options.output + ".ll"
        File.write(ll_file, llvm_ir)
        log(options, out_io, "  Wrote: #{ll_file}")

        if options.emit_llvm
          out_io.puts llvm_ir
          return 0
        end

        # Step 6: Compile to binary
        log(options, out_io, "\n[6/6] Compiling to binary...")
        result = compile_llvm_ir(ll_file, options, out_io, err_io)
        return result unless result == 0

        log(options, out_io, "\n=== Compilation complete ===")
        log(options, out_io, "Output: #{options.output}")
        return 0

      rescue ex : File::NotFoundError
        err_io.puts "error: file not found - #{ex.message}"
        return 1
      rescue ex
        err_io.puts "error: #{ex.message}"
        err_io.puts ex.backtrace.join("\n") if options.verbose
        return 1
      end

      private def compile_llvm_ir(ll_file : String, options : Options, out_io : IO, err_io : IO) : Int32
        obj_file = ll_file.gsub(/\.ll$/, ".o")

        opt_flag = case options.optimize
                   when 0 then "-O0"
                   when 1 then "-O1"
                   when 2 then "-O2"
                   else        "-O3"
                   end

        # Run opt to clean up and fix any IR issues
        # mem2reg promotes allocas, -O1 includes more fixes
        opt_ll_file = "#{ll_file}.opt.ll"
        opt_cmd = "opt -O1 -S -o #{opt_ll_file} #{ll_file} 2>&1"
        log(options, out_io, "  $ #{opt_cmd}")
        opt_result = `#{opt_cmd}`
        unless $?.success?
          err_io.puts "opt failed:"
          err_io.puts opt_result
          return 1
        end

        llc_cmd = "llc #{opt_flag} -filetype=obj -o #{obj_file} #{opt_ll_file} 2>&1"
        log(options, out_io, "  $ #{llc_cmd}")
        llc_result = `#{llc_cmd}`
        unless $?.success?
          err_io.puts "llc failed:"
          err_io.puts llc_result
          return 1
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

        link_cmd = "cc -o #{options.output} #{link_objs.join(" ")} 2>&1"
        log(options, out_io, "  $ #{link_cmd}")
        link_result = `#{link_cmd}`
        unless $?.success?
          err_io.puts "Linking failed:"
          err_io.puts link_result
          return 1
        end

        # Clean up intermediate files
        File.delete(obj_file) if File.exists?(obj_file)
        return 0
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

        unless File.exists?(abs_path)
          log(options, out_io, "  Warning: File not found: #{abs_path}")
          return
        end

        source = File.read(abs_path)
        lexer = Frontend::Lexer.new(source)
        parser = Frontend::Parser.new(lexer)
        program = parser.parse_program
        arena = program.arena
        exprs = program.roots

        # Process requires first
        base_dir = File.dirname(abs_path)
        exprs.each do |expr_id|
          process_require_node(arena, expr_id, base_dir, input_file, results, loaded, options, out_io)
        end

        results << {arena, exprs, abs_path}
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
        out_io : IO
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
              parse_file_recursive(resolved, results, loaded, input_file, options, out_io)
            when Array
              resolved.each do |file|
                parse_file_recursive(file, results, loaded, input_file, options, out_io)
              end
            else
              log(options, out_io, "  Warning: Could not resolve require '#{req_path}'")
            end
          end
        when Frontend::MacroIfNode
          # Process requires in both branches of macro conditionals
          # Since we don't evaluate macros, load both branches to get all possible requires
          if then_body = node.then_body
            process_require_node(arena, then_body, base_dir, input_file, results, loaded, options, out_io)
          end
          if else_body = node.else_body
            process_require_node(arena, else_body, base_dir, input_file, results, loaded, options, out_io)
          end
        when Frontend::MacroLiteralNode
          # Macro literal - check pieces for require text patterns
          node.pieces.each do |piece|
            if piece.kind == Frontend::MacroPiece::Kind::Text
              text = piece.text
              if text && text.includes?("require ")
                # Extract require paths from text (e.g., "require \"./unix/file_descriptor\"")
                text.scan(/require\s+["']([^"']+)["']/) do |match|
                  req_path = match[1]
                  resolved = resolve_require_path(req_path, base_dir, input_file)
                  case resolved
                  when String
                    parse_file_recursive(resolved, results, loaded, input_file, options, out_io)
                  when Array
                    resolved.each do |file|
                      parse_file_recursive(file, results, loaded, input_file, options, out_io)
                    end
                  end
                end
              end
            end
          end
        end
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
