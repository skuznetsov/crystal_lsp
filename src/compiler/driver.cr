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
require "./mir/mir"
require "./mir/hir_to_mir"
require "./mir/llvm_backend"

module Crystal::V2
  class CompilerDriver
    property input_file : String = ""
    property output_file : String = "a.out"
    property emit_llvm : Bool = false
    property emit_hir : Bool = false
    property emit_mir : Bool = false
    property verbose : Bool = false
    property optimize : Int32 = 0

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
        when /^-/
          STDERR.puts "Unknown option: #{arg}"
          exit 1
        else
          @input_file = arg
        end
        i += 1
      end

      if @input_file.empty?
        STDERR.puts "Usage: driver <input.cr> [-o output] [--emit-llvm] [-v]"
        exit 1
      end
    end

    def compile
      log "=== Crystal v2 Compiler ==="
      log "Input: #{@input_file}"
      log "Output: #{@output_file}"

      # Step 1: Parse source
      log "\n[1/5] Parsing..."
      source = File.read(@input_file)
      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      result = parser.parse_program
      arena = result.arena
      exprs = result.roots

      log "  Expressions: #{exprs.size}"

      # Step 2: Lower to HIR
      log "\n[2/5] Lowering to HIR..."
      hir_converter = HIR::AstToHir.new(arena, @input_file)

      # Collect all DefNodes
      def_nodes = [] of CrystalV2::Compiler::Frontend::DefNode
      exprs.each do |expr_id|
        node = arena[expr_id]
        if node.is_a?(CrystalV2::Compiler::Frontend::DefNode)
          def_nodes << node
        end
      end

      # Two-pass approach for forward references:
      # Pass 1: Register all function signatures
      def_nodes.each do |node|
        hir_converter.register_function(node)
      end

      # Pass 2: Lower function bodies
      func_count = 0
      def_nodes.each do |node|
        hir_converter.lower_def(node)
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
      end

      # Step 4: Lower to MIR
      log "\n[4/5] Lowering to MIR..."
      mir_lowering = MIR::HIRToMIRLowering.new(hir_module)
      mir_module = mir_lowering.lower
      log "  Functions: #{mir_module.functions.size}"

      if @emit_mir
        mir_file = @output_file.gsub(/\.[^.]+$/, ".mir")
        File.write(mir_file, mir_module.to_s)
        log "  Wrote: #{mir_file}"
      end

      # Step 5: Generate LLVM IR
      log "\n[5/5] Generating LLVM IR..."
      llvm_gen = MIR::LLVMIRGenerator.new(mir_module)
      llvm_gen.emit_type_metadata = true
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

      # Link with system linker
      link_cmd = "cc -o #{@output_file} #{obj_file} 2>&1"
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
  end
end

# Main entry point
driver = Crystal::V2::CompilerDriver.new
driver.parse_args(ARGV)
driver.compile
