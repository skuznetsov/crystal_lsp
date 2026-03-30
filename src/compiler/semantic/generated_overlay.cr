require "../frontend/ast"

module CrystalV2
  module Compiler
    module Semantic
      record GeneratedNodeInfo,
        root_id : Frontend::ExprId,
        source : String?,
        origin_node_id : Frontend::ExprId?,
        macro_definition_node_id : Frontend::ExprId?

      record GeneratedOverlay,
        node_file_paths : Hash(Int32, String),
        top_level_roots : Array(Frontend::ExprId),
        root_sources : Hash(Int32, String),
        root_by_node : Hash(Int32, Int32),
        root_origins : Hash(Int32, Frontend::ExprId),
        root_macro_defs : Hash(Int32, Frontend::ExprId) do
        def generated_info_for(node_id : Frontend::ExprId) : GeneratedNodeInfo?
          return nil if node_id.invalid?
          node_index = node_id.index

          root_index = if root_sources.has_key?(node_index) ||
                          root_origins.has_key?(node_index) ||
                          root_macro_defs.has_key?(node_index)
                         node_index
                       else
                         root_by_node[node_index]?
                       end
          return nil unless root_index

          GeneratedNodeInfo.new(
            Frontend::ExprId.new(root_index),
            root_sources[root_index]?,
            root_origins[root_index]?,
            root_macro_defs[root_index]?,
          )
        end

        def generated_source_for(node_id : Frontend::ExprId) : String?
          generated_info_for(node_id).try(&.source)
        end

        def generated_origin_for(node_id : Frontend::ExprId) : Frontend::ExprId?
          generated_info_for(node_id).try(&.origin_node_id)
        end

        def generated_node?(node_id : Frontend::ExprId) : Bool
          !generated_info_for(node_id).nil?
        end

        def generated_macro_definition_for(node_id : Frontend::ExprId) : Frontend::ExprId?
          generated_info_for(node_id).try(&.macro_definition_node_id)
        end
      end
    end
  end
end
