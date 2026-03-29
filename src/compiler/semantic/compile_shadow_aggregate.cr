require "../frontend/ast"
require "../frontend/lexer"
require "../frontend/parser"
require "../frontend/dispatch"

module CrystalV2
  module Compiler
    module Semantic
      class CompileShadowAggregate
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
