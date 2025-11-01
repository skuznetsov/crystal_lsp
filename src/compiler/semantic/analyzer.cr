require "../frontend/ast"
require "./symbol_table"
require "./context"
require "./collectors/symbol_collector"
require "./resolvers/name_resolver"
require "./diagnostic"

module CrystalV2
  module Compiler
    module Semantic
      class Analyzer
        alias Program = Frontend::Program

        getter program : Program
        getter global_context : Context
        getter semantic_diagnostics : Array(Diagnostic)
        getter name_resolver_diagnostics : Array(Frontend::Diagnostic)

        def initialize(@program : Program)
          @global_context = Context.new(SymbolTable.new)
          @semantic_diagnostics = [] of Diagnostic
          @name_resolver_diagnostics = [] of Frontend::Diagnostic
        end

        def collect_symbols
          collector = SymbolCollector.new(@program, @global_context)
          collector.collect
          @semantic_diagnostics = collector.diagnostics
          self
        end

        def resolve_names
          result = NameResolver.new(@program, @global_context.symbol_table).resolve
          @name_resolver_diagnostics = result.diagnostics
          result
        end

        def semantic_errors?
          @semantic_diagnostics.any? { |diag| diag.level == DiagnosticLevel::Error }
        end
      end
    end
  end
end
