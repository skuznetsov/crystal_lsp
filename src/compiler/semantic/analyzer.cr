require "../frontend/ast"
require "./symbol_table"
require "./context"
require "./collectors/symbol_collector"
require "./resolvers/name_resolver"
require "./type_inference_engine"
require "./diagnostic"
require "../hir/debug_hooks"

module CrystalV2
  module Compiler
    module Semantic
      class Analyzer
        alias Program = Frontend::Program

        getter program : Program
        getter global_context : Context
        getter semantic_diagnostics : Array(Diagnostic)
        getter name_resolver_diagnostics : Array(Frontend::Diagnostic)
        getter type_inference_diagnostics : Array(Diagnostic)

        def initialize(@program : Program, context : Context? = nil)
          @global_context = context || Context.new(SymbolTable.new)
          @semantic_diagnostics = [] of Diagnostic
          @name_resolver_diagnostics = [] of Frontend::Diagnostic
          @type_inference_diagnostics = [] of Diagnostic
        end

        def collect_symbols
          debug_hook("analyzer.symbols.start", "roots=#{@program.roots.size}")
          collector = SymbolCollector.new(@program, @global_context)
          collector.collect
          @semantic_diagnostics = collector.diagnostics
          debug_hook("analyzer.symbols.finish", "diagnostics=#{@semantic_diagnostics.size}")
          self
        end

        def resolve_names
          debug_hook("analyzer.resolve.start", "roots=#{@program.roots.size}")
          result = NameResolver.new(@program, @global_context.symbol_table).resolve
          @name_resolver_diagnostics = result.diagnostics
          debug_hook("analyzer.resolve.finish", "diagnostics=#{@name_resolver_diagnostics.size}")
          result
        end

        def infer_types(identifier_symbols : Hash(ExprId, Symbol))
          debug_hook("analyzer.infer.start", "symbols=#{identifier_symbols.size}")
          engine = TypeInferenceEngine.new(@program, identifier_symbols, @global_context.symbol_table)
          engine.infer_types
          @type_inference_diagnostics = engine.diagnostics
          debug_hook("analyzer.infer.finish", "diagnostics=#{@type_inference_diagnostics.size}")
          engine
        end

        def semantic_errors?
          @semantic_diagnostics.any? { |diag| diag.level == DiagnosticLevel::Error }
        end

        def type_inference_errors?
          @type_inference_diagnostics.any? { |diag| diag.level == DiagnosticLevel::Error }
        end
      end
    end
  end
end
