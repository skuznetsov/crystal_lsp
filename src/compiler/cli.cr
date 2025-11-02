require "option_parser"
require "./frontend/diagnostic_formatter"
require "./frontend/lexer"
require "./frontend/parser"
require "./semantic/analyzer"
require "./semantic/diagnostic_formatter"

module CrystalV2
  module Compiler
    VERSION = "0.1.0-dev"

    class CLI
      def initialize(@args : Array(String))
      end

      def run(*, out_io : IO = STDOUT, err_io : IO = STDERR) : Int32
        options = Options.new
        show_version = false

        parser = OptionParser.new do |p|
          p.banner = "Usage: crystal-v2 [options] <source>"

          p.on("-I PATH", "Add include path") { |path| options.includes << path }
          p.on("--emit-stages", "Print pipeline stages") { options.emit_stages = true }
          p.on("--dump-symbols", "Collect and print global symbols") { options.dump_symbols = true }
          p.on("--check", "Run semantic analysis and report diagnostics") { options.check_semantics = true }
          p.on("--version", "Show version") { show_version = true }
        end

        parser.parse(@args)

        if show_version
          out_io.puts VERSION
          return 0
        end

        unless options.emit_stages?
          out_io.puts "crystal_v2 compiler bootstrap"
          if source_path = @args.first?
            return compile_file(source_path, options, out_io, err_io)
          else
            out_io.puts "no input specified"
            return 0
          end
        else
          Pipeline.describe.each { |stage| out_io.puts stage }
          return 0
        end
      end

      struct Options
        getter includes = [] of String
        property? emit_stages = false
        property dump_symbols = false
        property check_semantics = false
      end

      module Pipeline
        extend self

        def describe
          [
            "Stage 1: Lexing (streaming)",
            "Stage 2: Incremental parsing / AST persistence",
            "Stage 3: Semantic scheduling",
            "Stage 4: Typed SSA lowering",
            "Stage 5: Backend emission",
          ]
        end
      end

      private def compile_file(path, options, out_io, err_io) : Int32
        source = File.read(path)
        lexer = Frontend::Lexer.new(source)
        parser = Frontend::Parser.new(lexer)
        program = parser.parse_program

        if parser.diagnostics.empty?
          analyzer = Semantic::Analyzer.new(program)
          analyzer.collect_symbols
          if options.check_semantics
            result = analyzer.resolve_names
            report_collector_diagnostics(analyzer.semantic_diagnostics, source, err_io)
            report_resolution_diagnostics(result.diagnostics, source, err_io)

            # Return error code if semantic analysis found errors
            if analyzer.semantic_errors?
              err_io.puts "\nerror: compilation failed due to semantic errors"
              return 1
            end

            # Check for name resolution errors (undefined variables/methods)
            if result.diagnostics.any?
              err_io.puts "\nerror: compilation failed due to name resolution errors"
              return 1
            end

            # Run type inference if name resolution succeeded
            analyzer.infer_types(result.identifier_symbols)
            report_type_inference_diagnostics(analyzer.type_inference_diagnostics, source, err_io)

            # Check for type inference errors
            if analyzer.type_inference_errors?
              err_io.puts "\nerror: compilation failed due to type errors"
              return 1
            end
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
          # Return error code if parsing failed and --check is enabled
          if options.check_semantics
            err_io.puts "\nerror: compilation failed due to parser errors"
            return 1
          end
          return 0
        end
      rescue File::NotFoundError
        err_io.puts "error: file not found #{path}"
        return 1
      end

      private def dump_symbols(program, table, out_io, indent = 0)
        table.each_local_symbol do |name, symbol|
          indentation = "  " * indent
          label, details = case symbol
          when Semantic::MacroSymbol
            {"macro", nil}
          when Semantic::MethodSymbol
            params = symbol.params
            extra = params.empty? ? nil : "(params: #{params.map { |p| String.new(p.name) }.join(", ")})"
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
        start_part = "#{span.start_line}:#{span.start_column}"
        end_part = "#{span.end_line}:#{span.end_column}"
        "[#{start_part}-#{end_part}]"
      end

      private def report_collector_diagnostics(diagnostics, source, err_io)
        return if diagnostics.empty?
        diagnostics.each do |diag|
          err_io.puts Semantic::DiagnosticFormatter.format(source, diag)
        end
      end

      private def report_resolution_diagnostics(diagnostics, source, err_io)
        return if diagnostics.empty?
        diagnostics.each do |diag|
          err_io.puts Frontend::DiagnosticFormatter.format(source, diag)
        end
      end

      private def report_type_inference_diagnostics(diagnostics, source, err_io)
        return if diagnostics.empty?
        diagnostics.each do |diag|
          err_io.puts Semantic::DiagnosticFormatter.format(source, diag)
        end
      end
    end
  end
end
