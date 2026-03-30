require "../frontend/ast"
require "./symbol_table"
require "./context"
require "./collectors/symbol_collector"
require "./resolvers/name_resolver"
require "./type_inference_engine"
require "./diagnostic"
require "./generated_overlay"
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
        getter generated_overlay : GeneratedOverlay

        def initialize(@program : Program, context : Context? = nil)
          @global_context = context || Context.new(SymbolTable.new)
          @semantic_diagnostics = [] of Diagnostic
          @name_resolver_diagnostics = [] of Frontend::Diagnostic
          @type_inference_diagnostics = [] of Diagnostic
          @generated_overlay = GeneratedOverlay.new(
            {} of Int32 => String,
            [] of ExprId,
            {} of Int32 => String,
            {} of Int32 => Int32,
            {} of Int32 => ExprId,
            {} of Int32 => ExprId,
          )
        end

        def collect_symbols(node_file_path_provider : Proc(ExprId, String?)? = nil, source_for_path_provider : Proc(String, String?)? = nil)
          debug_hook("analyzer.symbols.start", "roots=#{@program.roots.size}")
          collector = SymbolCollector.new(@program, @global_context, node_file_path_provider: node_file_path_provider, source_for_path_provider: source_for_path_provider)
          collector.collect
          @semantic_diagnostics = collector.diagnostics
          @generated_overlay = collector.generated_overlay
          debug_hook("analyzer.symbols.finish", "diagnostics=#{@semantic_diagnostics.size}")
          self
        end

        def resolve_names
          debug_hook("analyzer.resolve.start", "roots=#{analysis_root_count}")
          result = NameResolver.new(@program, @global_context.symbol_table, extra_roots: @generated_overlay.top_level_roots).resolve
          @name_resolver_diagnostics = result.diagnostics
          debug_hook("analyzer.resolve.finish", "diagnostics=#{@name_resolver_diagnostics.size}")
          result
        end

        def infer_types(identifier_symbols : Hash(ExprId, Symbol))
          debug_hook("analyzer.infer.start", "symbols=#{identifier_symbols.size} roots=#{analysis_root_count}")
          engine = TypeInferenceEngine.new(@program, identifier_symbols, @global_context.symbol_table, extra_roots: @generated_overlay.top_level_roots)
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

        private def analysis_root_count : Int32
          @program.roots.size + @generated_overlay.top_level_roots.size
        end

        def generated_node_file_paths : Hash(Int32, String)
          @generated_overlay.node_file_paths
        end

        def generated_top_level_roots : Array(ExprId)
          @generated_overlay.top_level_roots
        end

        def generated_root_sources : Hash(Int32, String)
          @generated_overlay.root_sources
        end

        def generated_root_by_node : Hash(Int32, Int32)
          @generated_overlay.root_by_node
        end

        def generated_root_origins : Hash(Int32, ExprId)
          @generated_overlay.root_origins
        end

        def generated_root_macro_defs : Hash(Int32, ExprId)
          @generated_overlay.root_macro_defs
        end

        def generated_info_for(node_id : ExprId) : GeneratedNodeInfo?
          @generated_overlay.generated_info_for(node_id)
        end

        def generated_source_for(node_id : ExprId) : String?
          @generated_overlay.generated_source_for(node_id)
        end

        def generated_origin_for(node_id : ExprId) : ExprId?
          @generated_overlay.generated_origin_for(node_id)
        end

        def generated_node?(node_id : ExprId) : Bool
          @generated_overlay.generated_node?(node_id)
        end

        def generated_macro_definition_for(node_id : ExprId) : ExprId?
          @generated_overlay.generated_macro_definition_for(node_id)
        end
      end
    end
  end
end
