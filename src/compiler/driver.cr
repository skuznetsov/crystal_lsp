# Crystal v2 Compiler Driver
#
# End-to-end compilation: Crystal source → HIR → MIR → LLVM IR → binary
#
# Usage:
#   crystal run src/compiler/driver.cr -- input.cr -o output

require "./frontend/lexer"
require "./frontend/parser"
require "./hir/hir"
require "./hir/ast_to_hir"
require "./hir/escape_analysis"
require "./hir/memory_strategy"
require "./mir/mir"
require "./mir/optimizations"
require "./mir/hir_to_mir"
require "./mir/llvm_backend"

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

    def initialize
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
        when /^-/
          STDERR.puts "Unknown option: #{arg}"
          exit 1
        else
          @input_file = arg
        end
        i += 1
      end

      if @input_file.empty?
        STDERR.puts "Usage: driver <input.cr> [-o output] [--emit-llvm] [--no-prelude] [--no-ltp] [-v]"
        exit 1
      end
    end

    def compile
      log "=== Crystal v2 Compiler ==="
      log "Input: #{@input_file}"
      log "Output: #{@output_file}"

      # Step 1: Parse source (with require support)
      log "\n[1/5] Parsing..."

      # Track loaded files to avoid duplicates
      loaded_files = Set(String).new

      # Parse all files recursively (require support)
      all_arenas = [] of Tuple(CrystalV2::Compiler::Frontend::ArenaLike, Array(CrystalV2::Compiler::Frontend::ExprId), String)

      # Load prelude first (unless --no-prelude)
      unless @no_prelude
        prelude_path = File.join(STDLIB_PATH, "prelude.cr")
        if File.exists?(prelude_path)
          log "  Loading prelude: #{prelude_path}"
          parse_file_recursive(prelude_path, all_arenas, loaded_files)
        end
      end

      # Parse user's input file
      parse_file_recursive(@input_file, all_arenas, loaded_files)

      total_exprs = all_arenas.sum { |t| t[1].size }
      log "  Files: #{all_arenas.size}, Expressions: #{total_exprs}"

      # Step 2: Lower to HIR
      log "\n[2/5] Lowering to HIR..."

      # Use first file's arena for the converter (it merges all)
      first_arena = all_arenas[0][0]
      hir_converter = HIR::AstToHir.new(first_arena, @input_file)

      # Collect all DefNodes, ClassNodes, ModuleNodes, EnumNodes, MacroDefNodes, and top-level expressions
      def_nodes = [] of Tuple(CrystalV2::Compiler::Frontend::DefNode, CrystalV2::Compiler::Frontend::ArenaLike)
      class_nodes = [] of Tuple(CrystalV2::Compiler::Frontend::ClassNode, CrystalV2::Compiler::Frontend::ArenaLike)
      module_nodes = [] of Tuple(CrystalV2::Compiler::Frontend::ModuleNode, CrystalV2::Compiler::Frontend::ArenaLike)
      enum_nodes = [] of Tuple(CrystalV2::Compiler::Frontend::EnumNode, CrystalV2::Compiler::Frontend::ArenaLike)
      macro_nodes = [] of Tuple(CrystalV2::Compiler::Frontend::MacroDefNode, CrystalV2::Compiler::Frontend::ArenaLike)
      # Top-level expressions that form the main function
      main_exprs = [] of Tuple(CrystalV2::Compiler::Frontend::ExprId, CrystalV2::Compiler::Frontend::ArenaLike)

      all_arenas.each do |arena, exprs, file_path|
        exprs.each do |expr_id|
          node = arena[expr_id]
          case node
          when CrystalV2::Compiler::Frontend::DefNode
            def_nodes << {node, arena}
          when CrystalV2::Compiler::Frontend::ClassNode
            # Parser creates ClassNode with is_struct=true for struct keyword
            class_nodes << {node, arena}
          when CrystalV2::Compiler::Frontend::ModuleNode
            module_nodes << {node, arena}
          when CrystalV2::Compiler::Frontend::EnumNode
            enum_nodes << {node, arena}
          when CrystalV2::Compiler::Frontend::MacroDefNode
            macro_nodes << {node, arena}
          when CrystalV2::Compiler::Frontend::RequireNode
            # Skip require nodes - already processed
          else
            # Top-level expression: collect for main function
            main_exprs << {expr_id, arena}
          end
        end
      end

      # Three-pass approach:
      # Pass 1: Register all enums, modules, class types and their methods
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
      module_nodes.each do |module_node, arena|
        hir_converter.arena = arena
        hir_converter.register_module(module_node)
      end
      class_nodes.each do |class_node, arena|
        hir_converter.arena = arena
        hir_converter.register_class(class_node)
      end
      macro_nodes.each do |macro_node, arena|
        hir_converter.arena = arena
        hir_converter.register_macro(macro_node)
      end

      # Pass 2: Register all top-level function signatures
      def_nodes.each do |node, arena|
        hir_converter.arena = arena
        hir_converter.register_function(node)
      end

      # Pass 3: Lower all function and method bodies
      func_count = 0
      module_nodes.each do |module_node, arena|
        hir_converter.arena = arena
        hir_converter.lower_module(module_node)
        func_count += 1
      end
      class_nodes.each do |class_node, arena|
        hir_converter.arena = arena
        hir_converter.lower_class(class_node)
        func_count += 1
      end
      def_nodes.each do |node, arena|
        hir_converter.arena = arena
        hir_converter.lower_def(node)
        func_count += 1
      end

      # Create synthetic main function from top-level expressions (or user-defined main)
      if main_exprs.size > 0
        hir_converter.lower_main(main_exprs)
        func_count += 1
      elsif main_def = def_nodes.find { |(n, _)| String.new(n.name) == "main" && !(n.receiver.try { |recv| String.new(recv) == HIR::AstToHir::FUN_DEF_RECEIVER } || false) }
        hir_converter.arena = main_def[1]
        hir_converter.lower_main_from_def(main_def[0])
        func_count += 1
      end

      # Ensure top-level `fun main` is lowered as an entrypoint when present.
      if fun_main = def_nodes.find { |(n, _)| n.receiver.try { |recv| String.new(recv) == HIR::AstToHir::FUN_DEF_RECEIVER } || false }
        hir_converter.arena = fun_main[1]
        hir_converter.lower_def(fun_main[0])
        func_count += 1
      end

      hir_module = hir_converter.module
      log "  Functions: #{hir_module.functions.size}"

      if @emit_hir
        hir_file = @output_file.gsub(/\.[^.]+$/, ".hir")
        File.write(hir_file, hir_module.to_s)
        log "  Wrote: #{hir_file}"
      end

      # Step 3: Escape analysis
      log "\n[3/5] Escape analysis..."
      hir_module.functions.each do |func|
        escape = HIR::EscapeAnalyzer.new(func)
        escape.analyze
        # Memory strategy (includes taint/thread_shared)
        ms = HIR::MemoryStrategyAssigner.new(func)
        ms.assign
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

      llc_cmd = "llc #{opt_flag} -filetype=obj -o #{obj_file} #{ll_file} 2>&1"
      log "  $ #{llc_cmd}"
      llc_result = `#{llc_cmd}`
      unless $?.success?
        STDERR.puts "llc failed:"
        STDERR.puts llc_result
        exit 1
      end

      # Link with system linker and runtime
      # Find runtime stub relative to driver location
      runtime_dir = File.dirname(File.dirname(__FILE__))
      runtime_stub = File.join(runtime_dir, "..", "runtime_stub.o")

      # Compile runtime stub if needed
      runtime_src = runtime_stub.gsub(/\.o$/, ".c")
      if File.exists?(runtime_src) && (!File.exists?(runtime_stub) ||
         File.info(runtime_src).modification_time > File.info(runtime_stub).modification_time)
        `cc -c #{runtime_src} -o #{runtime_stub} 2>&1`
      end

      link_objs = [obj_file]
      link_objs << runtime_stub if File.exists?(runtime_stub)

      link_cmd = "cc -o #{@output_file} #{link_objs.join(" ")} 2>&1"
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

    private def log(msg : String)
      puts msg if @verbose
    end

    # Recursively parse files, handling require statements
    private def parse_file_recursive(
      file_path : String,
      results : Array(Tuple(CrystalV2::Compiler::Frontend::ArenaLike, Array(CrystalV2::Compiler::Frontend::ExprId), String)),
      loaded : Set(String)
    )
      # Normalize and resolve path
      abs_path = File.expand_path(file_path)

      # Skip if already loaded
      return if loaded.includes?(abs_path)
      loaded << abs_path

      unless File.exists?(abs_path)
        STDERR.puts "File not found: #{abs_path}"
        return
      end

      # Parse the file
      source = File.read(abs_path)
      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program
      arena = program.arena
      exprs = program.roots

      # Extract require paths and process them first (dependencies before dependents)
      base_dir = File.dirname(abs_path)
      exprs.each do |expr_id|
        node = arena[expr_id]
        if node.is_a?(CrystalV2::Compiler::Frontend::RequireNode)
          path_node = arena[node.path]
          if path_node.is_a?(CrystalV2::Compiler::Frontend::StringNode)
            req_path = String.new(path_node.value)

            # Resolve the require path
            resolved = resolve_require_path(req_path, base_dir)
            if resolved
              parse_file_recursive(resolved, results, loaded)
            else
              log "  Warning: Could not resolve require '#{req_path}'"
            end
          end
        end
      end

      # Add this file's results (after dependencies)
      results << {arena, exprs, abs_path}
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
