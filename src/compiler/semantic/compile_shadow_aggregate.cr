require "../frontend/ast"
require "../frontend/diagnostic_formatter"
require "../frontend/parser/diagnostic"
require "../frontend/lexer"
require "../frontend/parser"
require "../frontend/dispatch"
require "./diagnostic"
require "./diagnostic_formatter"
require "./generated_overlay"

module CrystalV2
  module Compiler
    module Semantic
      class CompileShadowAggregate
        record GeneratedDiagnosticContext,
          display_path : String?,
          source : String,
          related_spans : Array(Frontend::RelatedSpan),
          secondary_spans : Array(Semantic::SecondarySpan) do
          def apply(diagnostic : Frontend::Diagnostic) : Frontend::Diagnostic
            diagnostic.with_file_path(display_path, diagnostic.related_spans + related_spans)
          end

          def apply(diagnostic : Semantic::Diagnostic) : Semantic::Diagnostic
            diagnostic.with_paths(display_path, diagnostic.secondary_spans + secondary_spans)
          end

          def sources_with_generated(base_sources : Hash(String, String)) : Hash(String, String)
            sources = base_sources.dup
            if display_path = self.display_path
              sources[display_path] = source
            end
            sources
          end
        end

        record UnitSummary,
          unit_index : Int32,
          path : String,
          source : String,
          roots : Array(Frontend::ExprId),
          node_count : Int32

        getter program : Frontend::Program
        getter unit_summaries : Array(UnitSummary)

        def self.build(units : Array(NamedTuple(path: String, source: String))) : self
          aggregate_arena = Frontend::AstArena.new
          merged_roots = [] of Frontend::ExprId
          unit_summaries = [] of UnitSummary
          unit_index_by_node = [] of Int32

          units.each_with_index do |unit, unit_index|
            lexer = Frontend::Lexer.new(unit[:source])
            parser = Frontend::Parser.new(lexer, aggregate_arena)
            roots = parser.parse_program_roots
            merged_roots.concat(roots)
            grow_index_owner_map(unit_index_by_node, aggregate_arena.size)
            node_count = assign_unit_nodes(aggregate_arena, roots, unit_index.to_i32, unit_index_by_node)
            unit_summaries << UnitSummary.new(
              unit_index: unit_index.to_i32,
              path: unit[:path],
              source: unit[:source],
              roots: roots,
              node_count: node_count,
            )
          end

          new(
            Frontend::Program.new(aggregate_arena, merged_roots),
            unit_summaries,
            unit_index_by_node,
          )
        end

        def initialize(
          @program : Frontend::Program,
          @unit_summaries : Array(UnitSummary),
          @unit_index_by_node : Array(Int32),
        )
          @unit_index_by_path = {} of String => Int32
          @unit_summaries.each do |unit_summary|
            @unit_index_by_path[unit_summary.path] = unit_summary.unit_index
          end
          @generated_overlay = GeneratedOverlay.empty
          @generated_node_count_by_unit = Array(Int32).new(@unit_summaries.size, 0)
          @generated_root_count_by_unit = Array(Int32).new(@unit_summaries.size, 0)
        end

        def generated_overlay : GeneratedOverlay
          @generated_overlay.dup
        end

        def unit_index_for(expr_id : Frontend::ExprId) : Int32?
          return nil if expr_id.invalid?
          index = expr_id.index
          return nil if index < 0 || index >= @unit_index_by_node.size
          unit_index = @unit_index_by_node.unsafe_fetch(index)
          unit_index < 0 ? nil : unit_index
        end

        def unit_for(expr_id : Frontend::ExprId) : UnitSummary?
          if unit_index = unit_index_for(expr_id)
            @unit_summaries.unsafe_fetch(unit_index)
          else
            nil
          end
        end

        def path_for(expr_id : Frontend::ExprId) : String?
          unit_for(expr_id).try(&.path)
        end

        def source_for_path(path : String) : String?
          if unit_index = @unit_index_by_path[path]?
            @unit_summaries.unsafe_fetch(unit_index).source
          else
            nil
          end
        end

        def sources_by_path : Hash(String, String)
          sources = {} of String => String
          @unit_summaries.each do |unit_summary|
            sources[unit_summary.path] = unit_summary.source
          end
          sources
        end

        private def attach_generated_node_paths(generated_node_file_paths : Hash(Int32, String)) : Nil
          generated_node_file_paths.each do |node_index, file_path|
            next unless unit_index = @unit_index_by_path[file_path]?
            while @unit_index_by_node.size < node_index + 1
              @unit_index_by_node << -1
            end

            current = @unit_index_by_node.unsafe_fetch(node_index)
            next if current == unit_index

            if current >= 0
              if current < @generated_node_count_by_unit.size && @generated_node_count_by_unit[current] > 0
                @generated_node_count_by_unit[current] -= 1
              end
            end

            @unit_index_by_node[node_index] = unit_index
            @generated_node_count_by_unit[unit_index] += 1
          end
        end

        def attach_generated_overlay(
          overlay : GeneratedOverlay
        ) : Nil
          detach_generated_overlay
          attach_generated_node_paths(overlay.node_file_paths)
          @generated_overlay = overlay.dup
          overlay.top_level_roots.each do |root_id|
            if unit_index = unit_index_for(root_id)
              @generated_root_count_by_unit[unit_index] += 1
            end
          end
        end

        def generated_info_for(expr_id : Frontend::ExprId) : GeneratedNodeInfo?
          @generated_overlay.generated_info_for(expr_id)
        end

        def generated_source_for(expr_id : Frontend::ExprId) : String?
          @generated_overlay.generated_source_for(expr_id)
        end

        def generated_origin_for(expr_id : Frontend::ExprId) : Frontend::ExprId?
          @generated_overlay.generated_origin_for(expr_id)
        end

        def generated_node?(expr_id : Frontend::ExprId) : Bool
          @generated_overlay.generated_node?(expr_id)
        end

        def generated_macro_definition_for(expr_id : Frontend::ExprId) : Frontend::ExprId?
          @generated_overlay.generated_macro_definition_for(expr_id)
        end

        def generated_related_spans_for(expr_id : Frontend::ExprId) : Array(Frontend::RelatedSpan)
          related_spans = [] of Frontend::RelatedSpan
          if related = generated_origin_related_span(expr_id)
            related_spans << related
          end
          if related = generated_macro_definition_related_span(expr_id)
            related_spans << related
          end
          related_spans
        end

        def generated_secondary_spans_for(expr_id : Frontend::ExprId) : Array(Semantic::SecondarySpan)
          generated_related_spans_for(expr_id).map do |related|
            Semantic::SecondarySpan.new(related.span, related.label, related.node_id, related.file_path)
          end
        end

        def generated_diagnostic_context_for(expr_id : Frontend::ExprId) : GeneratedDiagnosticContext?
          return nil unless info = generated_info_for(expr_id)
          return nil unless generated_source = info.source

          related_spans = generated_related_spans_for(expr_id)
          GeneratedDiagnosticContext.new(
            generated_display_path_for(expr_id),
            generated_source,
            related_spans,
            generated_related_spans_to_secondary_spans(related_spans),
          )
        end

        def enrich_shadow_diagnostic(diagnostic : Frontend::Diagnostic) : Frontend::Diagnostic
          related_spans = diagnostic.related_spans.map do |related|
            next related if related.file_path
            next related unless related_node_id = related.node_id
            related.with_file_path(path_for(related_node_id))
          end

          primary_file_path = diagnostic.file_path
          if primary_file_path.nil?
            if node_id = diagnostic.node_id
              primary_file_path = path_for(node_id)
            end
          end

          diagnostic.with_file_path(primary_file_path, related_spans)
        end

        def enrich_shadow_diagnostic(diagnostic : Semantic::Diagnostic) : Semantic::Diagnostic
          primary_file_path = diagnostic.primary_file_path
          if primary_file_path.nil?
            if primary_node_id = diagnostic.primary_node_id
              primary_file_path = path_for(primary_node_id)
            end
          end

          secondary_spans = diagnostic.secondary_spans.map do |secondary|
            next secondary if secondary.file_path
            next secondary unless secondary_node_id = secondary.node_id
            secondary.with_file_path(path_for(secondary_node_id))
          end

          diagnostic.with_paths(primary_file_path, secondary_spans)
        end

        def format_shadow_diagnostic(
          diagnostic : Frontend::Diagnostic,
          base_sources : Hash(String, String)
        ) : String
          if node_id = diagnostic.node_id
            if context = generated_diagnostic_context_for(node_id)
              return Frontend::DiagnosticFormatter.format(
                context.sources_with_generated(base_sources),
                context.apply(diagnostic)
              )
            end
          end
          Frontend::DiagnosticFormatter.format(base_sources, diagnostic)
        end

        def format_shadow_diagnostic(
          diagnostic : Semantic::Diagnostic,
          base_sources : Hash(String, String)
        ) : String
          if primary_node_id = diagnostic.primary_node_id
            if context = generated_diagnostic_context_for(primary_node_id)
              return Semantic::DiagnosticFormatter.format(
                context.sources_with_generated(base_sources),
                context.apply(diagnostic)
              )
            end
          end
          Semantic::DiagnosticFormatter.format(base_sources, diagnostic)
        end

        def generated_top_level_roots : Array(Frontend::ExprId)
          @generated_overlay.top_level_roots.dup
        end

        def generated_node_file_paths : Hash(Int32, String)
          @generated_overlay.node_file_paths.dup
        end

        private def detach_generated_overlay : Nil
          @generated_root_count_by_unit.fill(0)
          @generated_overlay.node_file_paths.each_key do |node_index|
            next if node_index < 0 || node_index >= @unit_index_by_node.size

            current = @unit_index_by_node.unsafe_fetch(node_index)
            if current >= 0 && current < @generated_node_count_by_unit.size && @generated_node_count_by_unit[current] > 0
              @generated_node_count_by_unit[current] -= 1
            end
            @unit_index_by_node[node_index] = -1
          end
        end

        def generated_node_count_for_unit(unit_index : Int32) : Int32
          return 0 if unit_index < 0 || unit_index >= @generated_node_count_by_unit.size
          @generated_node_count_by_unit.unsafe_fetch(unit_index)
        end

        def generated_root_count_for_unit(unit_index : Int32) : Int32
          return 0 if unit_index < 0 || unit_index >= @generated_root_count_by_unit.size
          @generated_root_count_by_unit.unsafe_fetch(unit_index)
        end

        def owned_node_count_for_unit(unit_index : Int32) : Int32
          return 0 if unit_index < 0 || unit_index >= @unit_summaries.size
          unit_summary = @unit_summaries.unsafe_fetch(unit_index)
          unit_summary.node_count + generated_node_count_for_unit(unit_index)
        end

        private def generated_origin_related_span(
          expr_id : Frontend::ExprId
        ) : Frontend::RelatedSpan?
          return nil unless info = generated_info_for(expr_id)
          return nil unless origin_node_id = info.origin_node_id
          return nil unless origin_path = path_for(origin_node_id)
          origin_span = @program.arena[origin_node_id].span
          Frontend::RelatedSpan.new(origin_span, "expanded from macro call here", origin_node_id, origin_path)
        end

        private def generated_macro_definition_related_span(
          expr_id : Frontend::ExprId
        ) : Frontend::RelatedSpan?
          return nil unless info = generated_info_for(expr_id)
          return nil unless macro_def_node_id = info.macro_definition_node_id
          return nil unless macro_def_path = path_for(macro_def_node_id)
          if origin_node_id = info.origin_node_id
            if origin_path = path_for(origin_node_id)
              return nil if origin_path == macro_def_path
            end
          end
          macro_def_span = @program.arena[macro_def_node_id].span
          Frontend::RelatedSpan.new(macro_def_span, "macro defined here", macro_def_node_id, macro_def_path)
        end

        private def generated_display_path_for(expr_id : Frontend::ExprId) : String?
          return nil unless file_path = path_for(expr_id)
          "#{file_path} [generated]"
        end

        private def generated_related_spans_to_secondary_spans(
          related_spans : Array(Frontend::RelatedSpan)
        ) : Array(Semantic::SecondarySpan)
          related_spans.map do |related|
            Semantic::SecondarySpan.new(related.span, related.label, related.node_id, related.file_path)
          end
        end

        private def self.grow_index_owner_map(unit_index_by_node : Array(Int32), arena_size : Int32) : Nil
          while unit_index_by_node.size < arena_size
            unit_index_by_node << -1
          end
        end

        private def self.assign_unit_nodes(
          arena : Frontend::AstArena,
          roots : Array(Frontend::ExprId),
          unit_index : Int32,
          unit_index_by_node : Array(Int32)
        ) : Int32
          stack = [] of Frontend::ExprId
          roots.each { |root| stack << root unless root.invalid? }
          assigned = 0

          until stack.empty?
            expr_id = stack.pop
            next if expr_id.invalid?
            index = expr_id.index
            next if index < 0 || index >= unit_index_by_node.size
            next if unit_index_by_node.unsafe_fetch(index) == unit_index

            if unit_index_by_node.unsafe_fetch(index) < 0
              unit_index_by_node[index] = unit_index
              assigned += 1
            end

            Frontend::NodeDispatch.each_child_expr(arena, expr_id) do |child|
              stack << child unless child.invalid?
            end
          end

          assigned
        end
      end
    end
  end
end
