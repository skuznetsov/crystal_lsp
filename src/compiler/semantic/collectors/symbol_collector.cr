require "../../frontend/ast"
require "../symbol"
require "../context"
require "../symbol_table"
require "../diagnostic"
require "../generated_overlay"
require "../macro_expander"

module CrystalV2
  module Compiler
    module Semantic
      class SymbolCollector
        alias Program = Frontend::Program
        alias TypedNode = Frontend::TypedNode

        getter diagnostics : Array(Diagnostic)
        getter generated_file_paths : Hash(Int32, String)
        getter generated_top_level_roots : Array(Frontend::ExprId)
        getter generated_root_sources : Hash(Int32, String)
        getter generated_root_by_node : Hash(Int32, Int32)
        getter generated_root_origins : Hash(Int32, Frontend::ExprId)
        getter generated_root_macro_defs : Hash(Int32, Frontend::ExprId)
        @virtual_arena : Frontend::VirtualArena?

      def initialize(@program : Program, context : Context, @node_file_path_provider : Proc(Frontend::ExprId, String?)? = nil, @source_for_path_provider : Proc(String, String?)? = nil)
        program_arena = @program.arena
        @arena = program_arena.as(Frontend::AstArena)
        @string_pool = @program.string_pool
        @virtual_arena = nil
        @table_stack = [context.symbol_table]
        @diagnostics = [] of Diagnostic
        @source_cache = {} of String => String
        @generated_file_paths = {} of Int32 => String
        @generated_top_level_roots = [] of Frontend::ExprId
        @generated_root_sources = {} of Int32 => String
        @generated_root_by_node = {} of Int32 => Int32
        @generated_root_origins = {} of Int32 => Frontend::ExprId
        @generated_root_macro_defs = {} of Int32 => Frontend::ExprId
        @macro_expander = MacroExpander.new(
          @program,
          @arena,
          context.flags,
          symbol_table: context.symbol_table,
          source_provider: ->(block_id : Frontend::ExprId) { macro_block_source(block_id) }
        )
        @macro_expander.macro_source_provider = ->(node_id : Frontend::ExprId) {
          macro_source_for(node_id)
        }
        @class_stack = [] of ClassSymbol
        @module_stack = [] of ModuleSymbol
        @enum_stack = [] of EnumSymbol
          # Pending root-level annotations (for example,
          # @[JSON::Serializable::Options] immediately before a class
          # definition). These annotations are attached to the next class we
          # see at the top level.
          @pending_root_annotations = [] of Frontend::ExprId
        end

        def collect
          @program.roots.each do |root_id|
            node = arena[root_id]

            case node
            when Frontend::AnnotationNode
              # Root-level annotation to be attached to the next class/module.
              @pending_root_annotations << root_id
            when Frontend::IdentifierNode
              unless handle_root_macro_identifier(root_id, node)
                visit(root_id)
              end
            when Frontend::CallNode
              unless handle_root_macro_call(root_id, node)
                visit(root_id)
              end
            when Frontend::MacroLiteralNode, Frontend::MacroIfNode, Frontend::MacroForNode
              handle_root_macro_expansion(root_id)
            else
              visit(root_id)
            end
          end
          self
        end

        def generated_overlay : GeneratedOverlay
          GeneratedOverlay.snapshot(
            @generated_file_paths,
            @generated_top_level_roots,
            @generated_root_sources,
            @generated_root_by_node,
            @generated_root_origins,
            @generated_root_macro_defs,
          )
        end

        private def arena : Frontend::AstArena
          @arena
        end

        private def current_table
          @table_stack.last
        end

        private def root_table
          @table_stack.first
        end

        private def current_mixin_owner : Symbol?
          @class_stack.last? || @module_stack.last?
        end

        private def push_table(table : SymbolTable)
          @table_stack << table
        end

        private def pop_table
          @table_stack.pop
        end

        private def file_path_for(node_id : Frontend::ExprId) : String?
          if generated_path = @generated_file_paths[node_id.index]?
            return generated_path
          end
          if provider = @node_file_path_provider
            if path = provider.call(node_id)
              return path
            end
          end
          return nil unless @virtual_arena
          @virtual_arena.not_nil!.file_for_id(node_id)
        rescue
          nil
        end

        private def assign_symbol_file(symbol : Symbol, node_id : Frontend::ExprId)
          if path = file_path_for(node_id)
            symbol.file_path = path
          end
          if origin_node_id = generated_origin_for(node_id)
            symbol.mark_generated_declaration_origin
            symbol.generated_origin_node_id = origin_node_id
          else
            symbol.mark_direct_declaration_origin
          end
          if macro_def_node_id = generated_macro_definition_for(node_id)
            symbol.generated_macro_definition_node_id = macro_def_node_id
          end
        end

        private def generated_origin_for(node_id : Frontend::ExprId) : Frontend::ExprId?
          return nil if node_id.invalid?
          node_index = node_id.index
          if origin = @generated_root_origins[node_index]?
            return origin
          end
          return nil unless root_index = @generated_root_by_node[node_index]?
          @generated_root_origins[root_index]?
        end

        private def generated_macro_definition_for(node_id : Frontend::ExprId) : Frontend::ExprId?
          return nil if node_id.invalid?
          node_index = node_id.index
          if macro_def = @generated_root_macro_defs[node_index]?
            return macro_def
          end
          return nil unless root_index = @generated_root_by_node[node_index]?
          @generated_root_macro_defs[root_index]?
        end

        private def intern_name(slice : Slice(UInt8)) : String
          @string_pool.intern_string(slice)
        end

        private def sources_for_arena(arena : Frontend::ArenaLike) : Array(String)
          case arena
          when Frontend::AstArena
            arena.extra_sources
          when Frontend::VirtualArena
            arena.extra_sources
          when Frontend::PageArena
            arena.extra_sources
          else
            [] of String
          end
        end

        private def source_for_span(arena : Frontend::ArenaLike, span : Frontend::Span) : String?
          sources = sources_for_arena(arena)
          return nil if sources.empty?
          sources.find { |source| span.end_offset <= source.bytesize } || sources.first?
        end

        private def macro_block_source(block_id : Frontend::ExprId) : String?
          block_node = arena[block_id]
          return nil unless block_node.is_a?(Frontend::BlockNode)
          body = block_node.body
          return nil if body.empty?

          spans = body.map { |expr_id| arena[expr_id].span }
          span = Frontend::Span.cover_all(spans)
          source = nil
          if path = file_path_for(block_id)
            source = @source_cache[path]?
            unless source
              begin
                source = File.read(path)
                @source_cache[path] = source
              rescue
                source = nil
              end
            end
          end
          source ||= source_for_span(arena, span)
          return nil unless source

          start = span.start_offset
          finish = span.end_offset
          return nil if start < 0 || finish <= start || start >= source.bytesize
          length = finish - start
          length = source.bytesize - start if start + length > source.bytesize
          source.byte_slice(start, length)
        end

        private def macro_source_for(node_id : Frontend::ExprId) : String?
          return nil if node_id.invalid?

          if source = @generated_root_sources[node_id.index]?
            return source
          end

          if root_index = @generated_root_by_node[node_id.index]?
            if source = @generated_root_sources[root_index]?
              return source
            end
          end

          if path = file_path_for(node_id)
            source = @source_cache[path]?
            unless source
              begin
                source = File.read(path)
                @source_cache[path] = source
              rescue
                source = nil
              end
            end
            return source if source
          end

          source_for_span(arena, arena[node_id].span)
        end

        private def visit(node_id : Frontend::ExprId)
          return if node_id.invalid?

          node = arena[node_id]

          case node
          when Frontend::AnnotationNode
            @pending_root_annotations << node_id if @table_stack.size == 1 && @class_stack.empty?
          when Frontend::VisibilityModifierNode
            visit(node.expression)
          when Frontend::BeginNode
            node.body.each { |expr_id| visit(expr_id) }
          when Frontend::MacroDefNode
            handle_macro_def(node_id, node)
        when Frontend::DefNode
          handle_def(node_id, node)
        when Frontend::FunNode
          handle_fun(node_id, node)
        when Frontend::ClassNode
          # Note: Structs are also parsed as ClassNode with is_struct=true
          handle_class(node_id, node)
        when Frontend::ModuleNode
          handle_module(node_id, node)
        when Frontend::EnumNode
          handle_enum(node_id, node)
        when Frontend::LibNode
          handle_lib(node_id, node)
        when Frontend::AliasNode
          handle_alias(node_id, node)
        when Frontend::ConstantNode
          handle_constant(node_id, node)
        when Frontend::IncludeNode
          handle_include(node_id, node)
        when Frontend::ExtendNode
          handle_extend(node)
        when Frontend::GlobalVarDeclNode
          handle_global_var_decl(node_id, node)
        when Frontend::AssignNode
          unless handle_constant_assignment(node_id, node)
            handle_global_assignment(node)
          end
        when Frontend::GetterNode, Frontend::SetterNode, Frontend::PropertyNode
          # Phase 87B-1: Expand accessor macros to method definitions
          expand_accessor_macro(node_id, node)
        when Frontend::CallNode
          # Phase 87B-2: Check if call is actually a macro invocation
            handle_potential_macro_call(node_id, node)
          end
        end

        private def handle_root_macro_expansion(node_id : Frontend::ExprId)
          expanded_id = track_generated_nodes(node_id) do
            @macro_expander.expand_top_level(node_id)
          end
          @macro_expander.diagnostics.each { |entry| @diagnostics << entry }
          remember_generated_top_level_root(expanded_id)
          visit(expanded_id) unless expanded_id.invalid?
        end

        private def handle_root_macro_call(node_id : Frontend::ExprId, node : Frontend::CallNode) : Bool
          symbol, owner_type = macro_call_target(node, nil)
          return false unless symbol.is_a?(MacroSymbol)

          expanded_id = track_generated_nodes(node_id, symbol.node_id) do
            @macro_expander.expand(
              symbol,
              node.args,
              owner_type,
              named_args: node.named_args,
              block_id: node.block
            )
          end
          @macro_expander.diagnostics.each { |entry| @diagnostics << entry }
          remember_generated_top_level_root(expanded_id)
          visit(expanded_id) unless expanded_id.invalid?
          true
        end

        private def handle_root_macro_identifier(node_id : Frontend::ExprId, node : Frontend::IdentifierNode) : Bool
          name = intern_name(node.name)
          symbol = current_table.lookup_macro(name)
          return false unless symbol.is_a?(MacroSymbol)

          expanded_id = track_generated_nodes(node_id, symbol.node_id) do
            @macro_expander.expand(symbol, [] of Frontend::ExprId, nil)
          end
          @macro_expander.diagnostics.each { |entry| @diagnostics << entry }
          remember_generated_top_level_root(expanded_id)
          visit(expanded_id) unless expanded_id.invalid?
          true
        end

        private def remember_generated_top_level_root(node_id : Frontend::ExprId) : Nil
          return if node_id.invalid?
          return if @generated_top_level_roots.includes?(node_id)
          @generated_top_level_roots << node_id
        end

        private def track_generated_nodes(origin_node_id : Frontend::ExprId, macro_def_node_id : Frontend::ExprId? = nil, &)
          generated_start = arena.size
          origin_path = file_path_for(origin_node_id)
          expanded_id = yield
          generated_output = @macro_expander.last_output

          if origin_path
            generated_index = generated_start
            while generated_index < arena.size
              @generated_file_paths[generated_index] = origin_path
              generated_index += 1
            end
          end

          unless expanded_id.invalid?
            if generated_output && !generated_output.empty?
              @generated_root_sources[expanded_id.index] = generated_output
            end
            @generated_root_origins[expanded_id.index] = origin_node_id
            if macro_def_node_id
              @generated_root_macro_defs[expanded_id.index] = macro_def_node_id.not_nil!
            end

            generated_index = generated_start
            while generated_index < arena.size
              @generated_root_by_node[generated_index] = expanded_id.index
              generated_index += 1
            end
          end

          expanded_id
        end

        private def handle_macro_def(node_id : Frontend::ExprId, node : Frontend::MacroDefNode)
          name_slice = node.name
          return unless name_slice

          name = intern_name(name_slice)
          body_id = node.body
          return unless body_id

          body_node = arena[body_id]
          unless body_node.is_a?(Frontend::MacroLiteralNode)
            return
          end

          symbol = MacroSymbol.new(name, node_id, body_id, node.params.map(&.name), node.params)
          assign_symbol_file(symbol, node_id)

          table = current_table
          if existing = table.lookup_local_macro(name)
            handle_macro_redefinition(name, symbol, existing, table)
          else
            table.define_macro(name, symbol)
          end
        end

        private def handle_def(node_id : Frontend::ExprId, node : Frontend::DefNode)
          name_slice = node.name
          return unless name_slice

          name = intern_name(name_slice)
          params = node.params || [] of Frontend::Parameter
          return_annotation = node.return_type.try { |slice| intern_name(slice) }

          # Week 1 Day 2: Detect generic type parameters from method signature
          type_params = detect_generic_type_parameters(params, return_annotation)

          receiver = node.receiver
          target_table = current_table
          is_class_method = false
          if receiver && intern_name(receiver) == "self"
            if enum_owner = @enum_stack.last?
              target_table = enum_owner.scope
            else
              target_table = @class_stack.last?.try(&.class_scope) || current_table
            end
            is_class_method = true
          end

          method_scope = SymbolTable.new(target_table)
          method_scope.owner_module = target_table.owner_module
          method_symbol = MethodSymbol.new(name, node_id, params: params, return_annotation: return_annotation, scope: method_scope, type_parameters: type_params, is_class_method: is_class_method)
          assign_symbol_file(method_symbol, node_id)

          table = target_table
          if existing = table.lookup_local(name)
            handle_method_redefinition(name, method_symbol, existing, table)
          else
            table.define(name, method_symbol)
          end

          push_table(method_scope)

          params.each do |param|
            # Phase BLOCK_CAPTURE: Skip anonymous block parameter (has no name)
            next unless param_name = param.name

            # TIER 2.1: Convert Slice(UInt8) to String for symbol table
            param_name_str = intern_name(param_name)
            param_type_str = if type_ann = param.type_annotation
              intern_name(type_ann)
            else
              nil
            end

            param_symbol = VariableSymbol.new(param_name_str, node_id, declared_type: param_type_str)

            if existing_param = method_scope.lookup_local(param_name_str)
              emit_duplicate_variable(param_name_str, param_symbol, existing_param)
            else
              if shadowed = lookup_variable_in_ancestors(method_scope.parent, param_name_str)
                emit_shadowing_warning(param_name_str, param_symbol, shadowed)
              end
              method_scope.define(param_name_str, param_symbol)
            end
          end

          type_params.try &.each do |type_param_name|
            next if method_scope.lookup_local(type_param_name)
            method_scope.define(type_param_name, VariableSymbol.new(type_param_name, node_id))
          end

          (node.body || [] of Frontend::ExprId).each do |expr_id|
            visit(expr_id)
          end

          pop_table
        end

        private def handle_fun(node_id : Frontend::ExprId, node : Frontend::FunNode)
          name = intern_name(node.name)
          params = (node.params || [] of Frontend::Parameter).dup
          if node.varargs
            params << Frontend::Parameter.new(
              name: "varargs".to_slice,
              type_annotation: "_".to_slice,
              is_splat: true
            )
          end
          return_annotation = node.return_type.try { |slice| intern_name(slice) }

          method_scope = SymbolTable.new(current_table)
          method_symbol = MethodSymbol.new(
            name,
            node_id,
            params: params,
            return_annotation: return_annotation,
            scope: method_scope,
            is_class_method: false
          )
          assign_symbol_file(method_symbol, node_id)

          table = current_table
          if existing = table.lookup_local(name)
            handle_method_redefinition(name, method_symbol, existing, table)
          else
            table.define(name, method_symbol)
          end

          params.each do |param|
            next unless param_name = param.name

            param_name_str = intern_name(param_name)
            param_type_str = param.type_annotation.try { |type_ann| intern_name(type_ann) }
            param_symbol = VariableSymbol.new(param_name_str, node_id, declared_type: param_type_str)

            unless method_scope.lookup_local(param_name_str)
              method_scope.define(param_name_str, param_symbol)
            end
          end
        end

        private def handle_class(node_id : Frontend::ExprId, node : Frontend::ClassNode)
          name_slice = node.name
          return unless name_slice

          name = intern_name(name_slice)
          super_name = node.super_name.try { |slice| intern_name(slice) }

          # Week 1: Parse generic type parameters (e.g., class Box(T))
          type_params = node.type_params.try do |params|
            params.map { |param_slice| intern_name(param_slice) }
          end

          table = node.absolute ? root_table : current_table
          existing = table.lookup_local(name)
          # Reuse existing scope if available (from ClassSymbol or ModuleSymbol)
          # This handles reopening scenarios and parser quirks with path-based module names
          instance_scope = case existing
                           when ClassSymbol
                             existing.scope
                           when ModuleSymbol
                             existing.scope  # Reuse module scope when class/struct overwrites module
                           else
                             SymbolTable.new(table)
                           end
          meta_scope = existing.is_a?(ClassSymbol) ? existing.class_scope : SymbolTable.new(table)

          # Check if this is a struct (is_struct flag set by parser for `struct X` syntax)
          is_struct = node.is_struct == true
          # Check if this is an abstract class
          is_abstract = node.is_abstract == true
          effective_super = super_name || implicit_superclass_name(name, is_struct)
          class_symbol = ClassSymbol.new(
            name,
            node_id,
            scope: instance_scope,
            class_scope: meta_scope,
            superclass_name: effective_super,
            type_parameters: type_params,
            is_struct: is_struct,
            is_abstract: is_abstract,
            explicit_superclass: !super_name.nil?
          )
          assign_symbol_file(class_symbol, node_id)

          if existing
            handle_class_redefinition(name, class_symbol, existing, table)
          else
            table.define(name, class_symbol)
          end

          push_table(instance_scope)

          type_params.try &.each do |type_param_name|
            unless instance_scope.lookup_local(type_param_name)
              instance_scope.define(type_param_name, VariableSymbol.new(type_param_name, node_id))
            end
            unless meta_scope.lookup_local(type_param_name)
              meta_scope.define(type_param_name, VariableSymbol.new(type_param_name, node_id))
            end
          end

          # Phase 5A: Collect instance variable declarations
          # Get the final class symbol from table (may have been redefined)
          final_class_symbol = table.lookup_local(name)
          pushed_owner = false
          if final_class_symbol.is_a?(ClassSymbol)
            ensure_implicit_constructor(final_class_symbol)
            # Attach any pending root-level annotations to this class
            unless @pending_root_annotations.empty?
              attach_class_annotations(final_class_symbol, @pending_root_annotations)
              @pending_root_annotations.clear
            end
            @class_stack << final_class_symbol
            pushed_owner = true
            collect_instance_vars(final_class_symbol, node.body || [] of Frontend::ExprId)
            collect_class_vars(final_class_symbol, node.body || [] of Frontend::ExprId)
          end

          # Walk the class body in order so that annotations like
          # @[JSON::Field] apply to the immediately following declaration
          # (instance variable, property, etc.).
          collect_class_body(final_class_symbol.as?(ClassSymbol), node.body || [] of Frontend::ExprId)

          @class_stack.pop if pushed_owner
          pop_table
        end

        private def handle_module(node_id : Frontend::ExprId, node : Frontend::ModuleNode)
          name_slice = node.name
          return unless name_slice

          name = intern_name(name_slice)
          type_params = node.type_params.try(&.map { |param| intern_name(param) })
          table = node.absolute ? root_table : current_table

          symbol = table.lookup_local(name)

          # If there's already a ClassSymbol (e.g., from struct Time), reuse its scope
          # This handles reopening scenarios where `module Foo` appears after `class Foo`
          if symbol.is_a?(ClassSymbol)
            # Reuse the existing class symbol's scope for module-style reopening
            push_table(symbol.scope)
            collect_scoped_body(node.body || [] of Frontend::ExprId)
            pop_table
            return
          end

          module_symbol = case symbol
          when ModuleSymbol
            symbol.node_id = node_id
            symbol.type_parameters ||= type_params
            symbol
          else
            new_scope = SymbolTable.new(table)
            created = ModuleSymbol.new(name, node_id, scope: new_scope, type_parameters: type_params)
            if symbol
              table.redefine(name, created)
            else
              table.define(name, created)
            end
            created
          end
          assign_symbol_file(module_symbol, node_id)
          module_symbol.scope.owner_module = module_symbol

          @module_stack << module_symbol
          push_table(module_symbol.scope)
          type_params.try &.each do |type_param_name|
            unless module_symbol.scope.lookup_local(type_param_name)
              module_symbol.scope.define(type_param_name, VariableSymbol.new(type_param_name, node_id))
            end
          end
          collect_scoped_body(node.body || [] of Frontend::ExprId)
          pop_table
          @module_stack.pop
        end

        private def ensure_implicit_constructor(class_symbol : ClassSymbol)
          return if class_symbol.class_scope.lookup_local("new")

          constructor_scope = SymbolTable.new(class_symbol.class_scope)
          constructor = MethodSymbol.new(
            "new",
            class_symbol.node_id,
            return_annotation: class_symbol.name,
            scope: constructor_scope,
            is_class_method: true
          )
          assign_symbol_file(constructor, class_symbol.node_id)
          class_symbol.class_scope.define("new", constructor)
        end

        private def implicit_superclass_name(name : String, is_struct : Bool) : String?
          if is_struct
            case name
            when "Value" then "Object"
            when "Struct" then "Value"
            when "Number" then "Value"
            when "Int" then "Number"
            when "Int8", "Int16", "Int32", "Int64", "Int128",
                 "UInt8", "UInt16", "UInt32", "UInt64", "UInt128"
              "Int"
            when "Float" then "Number"
            when "Float32", "Float64" then "Float"
            else
              "Struct"
            end
          else
            case name
            when "Object" then nil
            when "Reference" then "Object"
            else
              "Reference"
            end
          end
        end

        # Phase 102: Handle enum definitions
        private def handle_enum(node_id : Frontend::ExprId, node : Frontend::EnumNode, annotation_ids : Array(Frontend::ExprId)? = nil)
          name_slice = node.name
          return unless name_slice

          name = intern_name(name_slice)
          table = current_table

          # Parse base type if specified
          base_type = if bt = node.base_type
            intern_name(bt)
          else
            "Int32"
          end

          # Collect enum members with their values
          members = {} of String => Int64
          next_value = 0i64
          node.members.each do |member|
            member_name = intern_name(member.name)
            if val_id = member.value
              # Member has explicit value - try to evaluate it
              val_node = arena[val_id]
              if val_node.is_a?(Frontend::NumberNode)
                members[member_name] = intern_name(val_node.value).to_i64? || next_value
                next_value = members[member_name] + 1
              else
                members[member_name] = next_value
                next_value += 1
              end
            else
              members[member_name] = next_value
              next_value += 1
            end
          end

          enum_scope = SymbolTable.new(table)
          enum_symbol = EnumSymbol.new(name, node_id, scope: enum_scope, members: members, base_type: base_type)
          assign_symbol_file(enum_symbol, node_id)

          if annotation_ids
            attach_enum_annotations(enum_symbol, annotation_ids)
          elsif !@pending_root_annotations.empty?
            attach_enum_annotations(enum_symbol, @pending_root_annotations)
            @pending_root_annotations.clear
          end

          if existing = table.lookup_local(name)
            enum_symbol.merge_declaration_origins_from(existing) if existing.is_a?(EnumSymbol)
            table.redefine(name, enum_symbol)
          else
            table.define(name, enum_symbol)
          end

          node.members.each do |member|
            member_name = intern_name(member.name)
            member_value_id = member.value || Frontend::ExprId.new(-1)
            member_symbol = ConstantSymbol.new(member_name, member_value_id, member_value_id)
            member_symbol.file_path = enum_symbol.file_path
            member_symbol.mark_direct_declaration_origin
            if existing = enum_scope.lookup_local(member_name)
              member_symbol.merge_declaration_origins_from(existing) if existing.is_a?(ConstantSymbol)
              enum_scope.redefine(member_name, member_symbol)
            else
              enum_scope.define(member_name, member_symbol)
            end
          end

          push_table(enum_scope)
          @enum_stack << enum_symbol
          (node.body || [] of Frontend::ExprId).each { |expr_id| visit(expr_id) }
          @enum_stack.pop
          pop_table
        end

        private def handle_lib(node_id : Frontend::ExprId, node : Frontend::LibNode)
          name = intern_name(node.name)
          table = current_table
          symbol = table.lookup_local(name)

          lib_symbol = case symbol
                       when ModuleSymbol
                         symbol.node_id = node_id
                         symbol
                       else
                         scope = SymbolTable.new(table)
                         created = ModuleSymbol.new(name, node_id, scope: scope)
                         if symbol
                           table.redefine(name, created)
                         else
                           table.define(name, created)
                         end
                         created
                       end
          assign_symbol_file(lib_symbol, node_id)

          push_table(lib_symbol.scope)
          collect_scoped_body(node.body || [] of Frontend::ExprId)
          pop_table
        end

        private def handle_alias(node_id : Frontend::ExprId, node : Frontend::AliasNode)
          name = intern_name(node.name)
          target = intern_name(node.value)
          alias_symbol = AliasSymbol.new(name, node_id, target)
          assign_symbol_file(alias_symbol, node_id)

          table = current_table
          if existing = table.lookup_local(name)
            alias_symbol.merge_declaration_origins_from(existing) if existing.is_a?(AliasSymbol)
            table.redefine(name, alias_symbol)
          else
            table.define(name, alias_symbol)
          end
        end

        private def handle_constant(node_id : Frontend::ExprId, node : Frontend::ConstantNode)
          name_slice = node.name
          return unless name_slice

          name = intern_name(name_slice)
          value_id = node.value

          const_symbol = ConstantSymbol.new(name, node_id, value_id)
          assign_symbol_file(const_symbol, node_id)

          table = current_table
          if existing = table.lookup_local(name)
            const_symbol.merge_declaration_origins_from(existing) if existing.is_a?(ConstantSymbol)
            table.redefine(name, const_symbol)
          else
            table.define(name, const_symbol)
          end

          # Visit constant value to collect nested definitions
          visit(value_id)
        end

        private def handle_global_var_decl(node_id : Frontend::ExprId, node : Frontend::GlobalVarDeclNode)
          name = intern_name(node.name)
          type_ann = node.type.try { |slice| intern_name(slice) }
          define_global_var_symbol(name, type_ann, node_id)
        end

        private def handle_global_assignment(node : Frontend::AssignNode)
          target_id = node.target
          target_node = arena[target_id]
          case target_node
          when Frontend::GlobalNode
            name = intern_name(target_node.name)
            define_global_var_symbol(name, nil, target_id)
          end
        end

        private def handle_constant_assignment(node_id : Frontend::ExprId, node : Frontend::AssignNode) : Bool
          target_node = arena[node.target]
          return false unless target_node.is_a?(Frontend::IdentifierNode)

          name = intern_name(target_node.name)
          return false unless constant_like_name?(name)

          const_symbol = ConstantSymbol.new(name, node_id, node.value)
          assign_symbol_file(const_symbol, node_id)

          table = current_table
          if existing = table.lookup_local(name)
            const_symbol.merge_declaration_origins_from(existing) if existing.is_a?(ConstantSymbol)
            table.redefine(name, const_symbol)
          else
            table.define(name, const_symbol)
          end

          visit(node.value)
          true
        end

        private def collect_scoped_body(body : Array(Frontend::ExprId))
          body.each do |expr_id|
            node = arena[expr_id]
            case node
            when Frontend::ClassVarDeclNode
              handle_scoped_class_var_decl(expr_id, node)
            when Frontend::AssignNode
              unless handle_scoped_class_var_assignment(node_id: expr_id, node: node)
                visit(expr_id)
              end
            when Frontend::MacroLiteralNode, Frontend::MacroIfNode, Frontend::MacroForNode
              handle_scoped_body_macro_expansion(expr_id)
            else
              visit(expr_id)
            end
          end
        end

        private def handle_scoped_body_macro_expansion(node_id : Frontend::ExprId)
          expanded_id = track_generated_nodes(node_id) do
            if owner_name = current_module_qualified_name
              output = @macro_expander.expand_top_level_text(node_id, scope: current_table)
              if output.strip.empty?
                Frontend::ExprId.new(-1)
              else
                @macro_expander.reparse_output(wrap_module_body_macro_output(owner_name, output), node_id)
              end
            else
              @macro_expander.expand_top_level(node_id, scope: current_table)
            end
          end
          @macro_expander.diagnostics.each { |entry| @diagnostics << entry }
          return if expanded_id.invalid?

          remember_generated_top_level_root(expanded_id)

          if owner_name = current_module_qualified_name
            visit_wrapped_module_body(expanded_id, owner_name)
          else
            visit(expanded_id)
          end
        end

        private def current_module_qualified_name : String?
          return nil if @class_stack.empty? && @module_stack.empty?

          String.build do |builder|
            scope_parts = [] of String
            @class_stack.each { |class_symbol| scope_parts << class_symbol.name }
            @module_stack.each { |module_symbol| scope_parts << module_symbol.name }

            scope_parts.each_with_index do |part_name, index|
              builder << "::" unless index == 0
              builder << part_name
            end
          end
        end

        private def handle_include(node_id : Frontend::ExprId, node : Frontend::IncludeNode)
          include_ref = resolve_include_target(node)
          return unless include_ref
          current_table.include_module(include_ref.symbol, include_ref.type_arg_names)
          if owner = current_mixin_owner
            include_ref.symbol.register_instance_includer(owner)
          end
          expand_include_hook(node_id, include_ref.symbol)
        end

        private def handle_extend(node : Frontend::ExtendNode)
          mixin_ref = resolve_mixin_target(node.target, node.name)
          return unless mixin_ref

          # If we're inside a class, extend should affect the metaclass/class_scope
          if owner = @class_stack.last?
            owner.class_scope.include_module(mixin_ref.symbol, mixin_ref.type_arg_names)
          else
            current_table.include_module(mixin_ref.symbol, mixin_ref.type_arg_names)
          end
        end

        private def expand_include_hook(origin_node_id : Frontend::ExprId, module_symbol : ModuleSymbol) : Nil
          included_macro = module_symbol.scope.lookup_local_macro("included")
          return unless included_macro

          owner_type = @class_stack.last?
          expanded_id = track_generated_nodes(origin_node_id, included_macro.node_id) do
            @macro_expander.expand(
              included_macro,
              [] of Frontend::ExprId,
              owner_type,
              preserve_unresolved_expressions: true
            )
          end
          @macro_expander.diagnostics.each { |entry| @diagnostics << entry }
          visit(expanded_id) unless expanded_id.invalid?
        end

        # Phase 87B-1: Expand accessor macros to method definitions
        #
        # Transforms:
        #   getter name : String    → def name : String; @name; end
        #   setter name : String    → def name=(value : String); @name = value; end
        #   property name : String  → both getter and setter
        #
        # Uses immediate visit() pattern - generates AST node and immediately
        # processes it to register as MethodSymbol. No AST mutation needed.
        private def expand_accessor_macro(node_id : Frontend::ExprId, node : Frontend::TypedNode)
          specs = Frontend.node_accessor_specs(node)
          return unless specs

          specs.each do |spec|
            case node
            when Frontend::GetterNode
              # Generate: def name : Type; @name; end
              def_node = build_getter_def(spec, node.span)
              def_id = arena.add_typed(def_node)
              visit(def_id)  # Immediately register as MethodSymbol

            when Frontend::SetterNode
              # Generate: def name=(value : Type); @name = value; end
              def_node = build_setter_def(spec, node.span)
              def_id = arena.add_typed(def_node)
              visit(def_id)

            when Frontend::PropertyNode
              # Generate both getter and setter
              getter_node = build_getter_def(spec, node.span)
              setter_node = build_setter_def(spec, node.span)
              visit(arena.add_typed(getter_node))
              visit(arena.add_typed(setter_node))
            end
          end
        end

        # Build getter method AST node
        #
        # Input:  getter name : String
        # Output: def name : String
        #           @name
        #         end
        private def build_getter_def(spec : Frontend::AccessorSpec, base_span : Frontend::Span) : Frontend::DefNode
          # TIER 2.2: spec.name is already Slice(UInt8), spec.type_annotation is Slice(UInt8)?
          # Create instance variable access node: @name
          spec_name_str = intern_name(spec.name)  # Convert for interpolation
          ivar_name = "@#{spec_name_str}"
          ivar_bytes = ivar_name.to_slice
          ivar_node = Frontend::InstanceVarNode.new(
            spec.name_span,
            ivar_bytes
          )
          ivar_id = arena.add_typed(ivar_node)

          # Create def node with instance variable as body
          method_name_bytes = if spec.predicate
                                "#{spec_name_str}?".to_slice
                              else
                                spec.name  # Already Slice(UInt8)
                              end
          return_type_bytes = spec.type_annotation  # Already Slice(UInt8)?

          Frontend::DefNode.new(
            base_span,
            method_name_bytes,
            nil,
            return_type_bytes,
            [ivar_id]
          )
        end

        # Build setter method AST node
        #
        # Input:  setter name : String
        # Output: def name=(value : String)
        #           @name = value
        #         end
        private def build_setter_def(spec : Frontend::AccessorSpec, base_span : Frontend::Span) : Frontend::DefNode
          # Create parameter: value : Type
          # TIER 2.2: spec.name and spec.type_annotation are already Slice(UInt8)
          param_name_slice = "value".to_slice
          param_type_slice = spec.type_annotation  # Already Slice(UInt8)?

          param = Frontend::Parameter.new(
            param_name_slice,
            nil,              # external_name
            param_type_slice, # type_annotation
            nil,              # default_value
            spec.name_span,   # span
            spec.name_span    # name_span
          )

          # Create instance variable node: @name
          spec_name_str = intern_name(spec.name)  # Convert for interpolation
          ivar_name = "@#{spec_name_str}"
          ivar_bytes = ivar_name.to_slice
          ivar_node = Frontend::InstanceVarNode.new(
            spec.name_span,
            ivar_bytes
          )
          ivar_id = arena.add_typed(ivar_node)

          # Create identifier node: value
          value_bytes = "value".to_slice
          value_node = Frontend::IdentifierNode.new(
            spec.name_span,
            value_bytes
          )
          value_id = arena.add_typed(value_node)

          # Create assignment: @name = value
          assign_node = Frontend::AssignNode.new(
            base_span,
            ivar_id,
            value_id
          )
          assign_id = arena.add_typed(assign_node)

          # Create def node with assignment as body
          spec_name_str2 = intern_name(spec.name)  # Convert for interpolation
          setter_name = "#{spec_name_str2}="
          setter_name_bytes = setter_name.to_slice

          Frontend::DefNode.new(
            base_span,
            setter_name_bytes,
            [param],
            param_type_slice,  # FIXED: Was param_type_bytes
            [assign_id]
          )
        end

        # Phase 87B-2: Handle potential macro calls
        #
        # Checks if a method call is actually a macro invocation.
        # If yes: expands the macro and visits the result.
        # If no: ignores (will be handled during type inference).
        private def handle_potential_macro_call(node_id : Frontend::ExprId, node : Frontend::TypedNode)
          callee_name = macro_call_name(node)
          return unless callee_name

          owner_type = @class_stack.empty? ? nil : @class_stack.last
          symbol, macro_owner_type = macro_call_target(node, owner_type)

          if symbol.is_a?(MacroSymbol)
            # Get arguments
            args = Frontend.node_args(node) || [] of Frontend::ExprId

            # Expand macro with optional owner_type
            expanded_id = track_generated_nodes(node_id, symbol.node_id) do
              @macro_expander.expand(
                symbol,
                args,
                macro_owner_type,
                named_args: node.named_args,
                block_id: node.block
              )
            end

            # Collect diagnostics from expander
            @macro_expander.diagnostics.each { |entry| @diagnostics << entry }

            # Visit expanded result (if valid)
            visit(expanded_id) unless expanded_id.invalid?
          end

          # If not a macro, ignore - will be handled during type inference
        end

        private def macro_call_target(node : Frontend::TypedNode, lexical_owner_type : ClassSymbol?) : {MacroSymbol?, ClassSymbol?}
          callee_name = macro_call_name(node)
          return {nil, lexical_owner_type} unless callee_name

          if symbol = lookup_macro_for_current_context(callee_name, lexical_owner_type)
            return {symbol, lexical_owner_type}
          end

          receiver_owner = resolve_macro_receiver_owner(node)
          return {nil, lexical_owner_type} unless receiver_owner

          case receiver_owner
          when ClassSymbol
            {lookup_macro_in_class_hierarchy(receiver_owner, callee_name), receiver_owner}
          when ModuleSymbol
            {receiver_owner.scope.lookup_macro(callee_name), lexical_owner_type}
          else
            {nil, lexical_owner_type}
          end
        end

        private def resolve_macro_receiver_owner(node : Frontend::TypedNode) : Symbol?
          return nil unless node.is_a?(Frontend::CallNode)

          callee_node = arena[node.callee]
          return nil unless callee_node.is_a?(Frontend::MemberAccessNode)

          resolve_symbol_from_expr(callee_node.object)
        end

        private def lookup_macro_for_current_context(name : String, owner_type : ClassSymbol?) : MacroSymbol?
          if symbol = current_table.lookup_macro(name)
            return symbol
          end

          if owner_type
            if symbol = lookup_macro_in_class_hierarchy(owner_type, name)
              return symbol
            end
          end

          nil
        end

        private def lookup_macro_in_class_hierarchy(class_symbol : ClassSymbol, name : String, visited = Set(String).new) : MacroSymbol?
          return nil unless visited.add?(class_symbol.name)

          if symbol = class_symbol.scope.lookup_macro(name)
            return symbol
          end

          superclass_name = class_symbol.superclass_name
          return nil unless superclass_name

          superclass_symbol = resolve_root_class_symbol(superclass_name)
          return nil unless superclass_symbol

          lookup_macro_in_class_hierarchy(superclass_symbol, name, visited)
        end

        private def resolve_root_class_symbol(name : String) : ClassSymbol?
          segments = name.split("::").reject(&.empty?)
          return nil if segments.empty?

          root = @table_stack.first
          symbol = resolve_symbol_by_segments(segments, root) || root.lookup(name)
          symbol.as?(ClassSymbol)
        end

        private def macro_call_name(node : Frontend::TypedNode) : String?
          if callee_slice = Frontend.node_member(node)
            return intern_name(callee_slice)
          end

          return nil unless node.is_a?(Frontend::CallNode)
          callee_node = arena[node.callee]
          case callee_node
          when Frontend::IdentifierNode
            intern_name(callee_node.name)
          when Frontend::MemberAccessNode
            intern_name(callee_node.member)
          else
            nil
          end
        end

        # Phase 5A: Scan class body for instance variable assignments
        private def collect_instance_vars(class_symbol : ClassSymbol, body : Array(Frontend::ExprId))
          body.each do |expr_id|
            scan_for_instance_vars(class_symbol, expr_id)
          end
        end

        # Collect class variable declarations/assignments (@@var)
        private def collect_class_vars(class_symbol : ClassSymbol, body : Array(Frontend::ExprId))
          body.each do |expr_id|
            scan_for_class_vars(class_symbol, expr_id)
          end
        end

        # Attach root-level annotations (e.g., @[JSON::Serializable::Options])
        # to the owning class symbol.
        private def attach_class_annotations(class_symbol : ClassSymbol, annotation_ids : Array(Frontend::ExprId))
          annotation_ids.each do |ann_id|
            node = arena[ann_id]
            next unless node.is_a?(Frontend::AnnotationNode)

            if info = build_annotation_info(node)
              class_symbol.add_annotation(info)
            end
          end
        end

        private def attach_enum_annotations(enum_symbol : EnumSymbol, annotation_ids : Array(Frontend::ExprId))
          annotation_ids.each do |ann_id|
            node = arena[ann_id]
            next unless node.is_a?(Frontend::AnnotationNode)

            if info = build_annotation_info(node)
              enum_symbol.add_annotation(info)
            end
          end
        end

        # Attach pending annotations to an explicit instance variable
        # declaration inside a class body.
        private def attach_ivar_annotations(class_symbol : ClassSymbol, node : Frontend::InstanceVarDeclNode, annotation_ids : Array(Frontend::ExprId))
          return if annotation_ids.empty?

          ivar_name = intern_name(node.name)
          ivar_name = ivar_name[1..-1] if ivar_name.starts_with?("@")

          annotation_ids.each do |ann_id|
            ann_node = arena[ann_id]
            next unless ann_node.is_a?(Frontend::AnnotationNode)

            if info = build_annotation_info(ann_node)
              class_symbol.add_ivar_annotation(ivar_name, info)
            end
          end
        end

        # Attach pending annotations to accessor macros (getter/setter/property)
        # by mapping them to the underlying instance variable name(s).
        private def attach_accessor_annotations(class_symbol : ClassSymbol, node : Frontend::TypedNode, annotation_ids : Array(Frontend::ExprId))
          return if annotation_ids.empty?

          specs = Frontend.node_accessor_specs(node)
          return unless specs

          # Build all annotation infos up front to reuse for each spec
          infos = annotation_ids.compact_map do |ann_id|
            ann_node = arena[ann_id]
            next unless ann_node.is_a?(Frontend::AnnotationNode)
            build_annotation_info(ann_node)
          end

          return if infos.empty?

          specs.each do |spec|
            spec_name = intern_name(spec.name)
            infos.each do |info|
              class_symbol.add_ivar_annotation(spec_name, info)
            end
          end
        end

        # Convert an AnnotationNode into AnnotationInfo, extracting the
        # fully-qualified name and argument expressions.
        private def build_annotation_info(node : Frontend::AnnotationNode) : AnnotationInfo?
          full_name = annotation_full_name(node.name)
          return nil if full_name.empty?

          args = node.args || [] of Frontend::ExprId

          named_args_hash = {} of String => ExprId
          if named_args = node.named_args
            named_args.each do |named_arg|
              key = intern_name(named_arg.name)
              named_args_hash[key] = named_arg.value
            end
          end

          AnnotationInfo.new(full_name, args, named_args_hash)
        end

        # Build a fully-qualified annotation name from its name expression.
        # Handles simple identifiers and nested PathNode chains such as
        # JSON::Serializable::Options.
        private def annotation_full_name(name_expr_id : Frontend::ExprId) : String
          node = arena[name_expr_id]

          case node
          when Frontend::IdentifierNode
            intern_name(node.name)
          when Frontend::PathNode
            parts = [] of String
            current_id = name_expr_id

            # Traverse left-associative PathNode chain
            while true
              current = arena[current_id]
              case current
              when Frontend::PathNode
                right_id = current.right
                right_name = annotation_full_name(right_id)
                parts << right_name unless right_name.empty?

                if left_id = current.left
                  current_id = left_id
                else
                  break
                end
              when Frontend::IdentifierNode
                parts << intern_name(current.name)
                break
              else
                literal = Frontend.node_literal_string(current)
                parts << literal if literal
                break
              end
            end

            parts.reverse.join("::")
          else
            Frontend.node_literal_string(node) || ""
          end
        end

        # Sequential pass over class body to associate annotations with
        # following declarations (instance variable declarations, accessor
        # macros, etc.), and to dispatch existing handlers for defs, macros,
        # and calls.
        private def collect_class_body(owner : ClassSymbol?, body : Array(Frontend::ExprId))
          pending_annotations = [] of Frontend::ExprId

          body.each do |expr_id|
            node = arena[expr_id]

            case node
            when Frontend::AnnotationNode
              # Record annotation to be attached to the next relevant node.
              pending_annotations << expr_id

            when Frontend::InstanceVarDeclNode
              if owner
                attach_ivar_annotations(owner, node, pending_annotations)
              end
              pending_annotations.clear

            when Frontend::GetterNode, Frontend::SetterNode, Frontend::PropertyNode
              if owner
                attach_accessor_annotations(owner, node, pending_annotations)
              end
              pending_annotations.clear
              # Reuse existing accessor macro expansion logic
              expand_accessor_macro(expr_id, node)

            when Frontend::DefNode
              # Method-level definitions use existing handler
              pending_annotations.clear
              handle_def(expr_id, node)

            when Frontend::MacroDefNode
              pending_annotations.clear
              handle_macro_def(expr_id, node)

            when Frontend::ClassNode
              # Nested class: clear pending annotations and recurse
              pending_annotations.clear
              handle_class(expr_id, node)

            when Frontend::ModuleNode
              pending_annotations.clear
              handle_module(expr_id, node)

            when Frontend::EnumNode
              handle_enum(expr_id, node, pending_annotations)
              pending_annotations.clear

            when Frontend::AliasNode
              pending_annotations.clear
              handle_alias(expr_id, node)

            when Frontend::ConstantNode
              pending_annotations.clear
              handle_constant(expr_id, node)

            when Frontend::AssignNode
              pending_annotations.clear
              handle_constant_assignment(expr_id, node)

            when Frontend::CallNode
              pending_annotations.clear
              handle_potential_macro_call(expr_id, node)

            when Frontend::MacroLiteralNode, Frontend::MacroIfNode, Frontend::MacroForNode
              pending_annotations.clear
              handle_class_body_macro_expansion(expr_id, owner)

            when Frontend::VisibilityModifierNode
              pending_annotations.clear
              visit(node.expression)

            when Frontend::IncludeNode
              pending_annotations.clear
              handle_include(expr_id, node)

            when Frontend::ExtendNode
              pending_annotations.clear
              handle_extend(node)

            else
              # Any other node breaks the annotation chain
              pending_annotations.clear
            end
          end
        end

        private def handle_class_body_macro_expansion(node_id : Frontend::ExprId, owner_type : ClassSymbol?) : Nil
          expanded_id = track_generated_nodes(node_id) do
            if owner_type
              output = @macro_expander.expand_top_level_text(node_id, owner_type: owner_type, scope: owner_type.scope)
              if output.strip.empty?
                Frontend::ExprId.new(-1)
              else
                @macro_expander.reparse_output(wrap_class_body_macro_output(output), node_id)
              end
            else
              @macro_expander.expand_top_level(node_id, owner_type: owner_type, scope: current_table)
            end
          end
          @macro_expander.diagnostics.each { |entry| @diagnostics << entry }
          return if expanded_id.invalid?

          expanded_node = arena[expanded_id]
          if owner_type && expanded_node.is_a?(Frontend::ClassNode)
            (expanded_node.body || [] of Frontend::ExprId).each do |expr_id|
              visit(expr_id)
            end
          else
            visit(expanded_id)
          end
        end

        private def wrap_class_body_macro_output(output : String) : String
          String.build do |builder|
            builder << "class __ShadowMacroBody__\n"
            builder << output
            builder << '\n' unless output.ends_with?('\n')
            builder << "end\n"
          end
        end

        private def wrap_module_body_macro_output(owner_name : String, output : String) : String
          String.build do |builder|
            builder << "module "
            builder << owner_name
            builder << '\n'
            builder << output
            builder << '\n' unless output.ends_with?('\n')
            builder << "end\n"
          end
        end

        private def visit_wrapped_module_body(expanded_id : Frontend::ExprId, owner_name : String) : Nil
          node = arena[expanded_id]
          body = wrapped_module_body(node, owner_name)
          return unless body

          collect_scoped_body(body)
        end

        private def wrapped_module_body(node : Frontend::TypedNode, owner_name : String) : Array(Frontend::ExprId)?
          segments = owner_name.split("::").reject(&.empty?)
          return nil if segments.empty?

          current = node
          segments.each_with_index do |segment, index|
            return nil unless current.is_a?(Frontend::ModuleNode)
            return nil unless intern_name(current.name) == segment

            body = current.body
            return body if index == segments.size - 1
            return nil unless body && body.size == 1

            current = arena[body.first]
          end

          nil
        end

        private def constant_like_name?(name : String) : Bool
          return false if name.empty?
          first = name.byte_at(0)
          (first >= 'A'.ord.to_u8 && first <= 'Z'.ord.to_u8) || name.starts_with?("__")
        end

        private def handle_scoped_class_var_decl(node_id : Frontend::ExprId, node : Frontend::ClassVarDeclNode) : Nil
          var_name = intern_name(node.name)
          var_name = var_name[2..-1] if var_name.starts_with?("@@")
          type_annotation = node.type.try { |slice| intern_name(slice) }
          define_scoped_class_var_symbol(var_name, type_annotation, node_id)
        end

        private def handle_scoped_class_var_assignment(node_id : Frontend::ExprId, node : Frontend::AssignNode) : Bool
          target_node = arena[node.target]
          return false unless target_node.is_a?(Frontend::ClassVarNode)

          var_name = intern_name(target_node.name)
          var_name = var_name[2..-1] if var_name.starts_with?("@@")
          define_scoped_class_var_symbol(var_name, nil, node.target)
          visit(node.value)
          true
        end

        private def define_scoped_class_var_symbol(name : String, type_annotation : String?, node_id : Frontend::ExprId) : Nil
          if owner = @class_stack.last?
            define_class_var_symbol(owner, name, type_annotation, node_id)
            return
          end

          sym_name = name.starts_with?("@@") ? name : "@@#{name}"
          sym = ClassVarSymbol.new(sym_name, node_id, type_annotation)
          assign_symbol_file(sym, node_id)

          if existing = current_table.lookup_local(sym_name)
            current_table.redefine(sym_name, sym)
          else
            begin
              current_table.define(sym_name, sym)
            rescue SymbolRedefinitionError
              current_table.redefine(sym_name, sym)
            end
          end
        end

        private def scan_for_instance_vars(class_symbol : ClassSymbol, expr_id : Frontend::ExprId, current_method : Frontend::DefNode? = nil)
          return if expr_id.invalid?
          node = arena[expr_id]

          case node
          when Frontend::InstanceVarDeclNode
            # Phase 5C: Handle explicit type annotations (@var : Type = default)
            var_name = intern_name(node.name)
            var_name = var_name[1..-1] if var_name.starts_with?("@")
            type_annotation = node.type.try { |slice| intern_name(slice) }
            default_value = node.value
            has_default = !default_value.nil?
            class_symbol.add_instance_var(var_name, type_annotation, default_value, has_default)
            define_instance_var_symbol(class_symbol, var_name, type_annotation, expr_id)
          when Frontend::AssignNode
            # Check if assignment target is instance variable
            target_id = node.target
            target_node = arena[target_id]
            if target_node.is_a?(Frontend::InstanceVarNode)
              var_name = intern_name(target_node.name)
              var_name = var_name[1..-1] if var_name.starts_with?("@")
              unless class_symbol.get_instance_var_type(var_name)
                # Week 1: Try to infer type from RHS if it's a parameter reference
                type_annotation = infer_ivar_type_from_assignment(node.value, current_method)
                # Assignment in initialize is a form of default value
                default_value = node.value
                has_default = current_method.try { |m| intern_name(m.name.not_nil!) == "initialize" } || false
                class_symbol.add_instance_var(var_name, type_annotation, default_value, has_default)
              end
              define_instance_var_symbol(class_symbol, var_name, nil, target_id)
            end
          when Frontend::DefNode
            # Scan method body for instance variable assignments, passing method context
            (node.body || [] of Frontend::ExprId).each do |body_expr_id|
              scan_for_instance_vars(class_symbol, body_expr_id, node)
            end
          when Frontend::IfNode
            (node.then_body || [] of Frontend::ExprId).each { |e| scan_for_instance_vars(class_symbol, e, current_method) }

            (node.elsifs || [] of Frontend::ElsifBranch).each do |elsif_branch|
              elsif_branch.body.each { |e| scan_for_instance_vars(class_symbol, e, current_method) }
            end

            (node.else_body || [] of Frontend::ExprId).each { |e| scan_for_instance_vars(class_symbol, e, current_method) }
          when Frontend::WhileNode
            node.body.each { |e| scan_for_instance_vars(class_symbol, e, current_method) }
          end
        end

        private def scan_for_class_vars(class_symbol : ClassSymbol, expr_id : Frontend::ExprId, current_method : Frontend::DefNode? = nil)
          return if expr_id.invalid?
          node = arena[expr_id]

          case node
          when Frontend::ClassVarDeclNode
            var_name = intern_name(node.name)
            var_name = var_name[2..-1] if var_name.starts_with?("@@")
            type_annotation = node.type.try { |slice| intern_name(slice) }
            define_class_var_symbol(class_symbol, var_name, type_annotation, expr_id)
          when Frontend::AssignNode
            target_id = node.target
            target_node = arena[target_id]
            if target_node.is_a?(Frontend::ClassVarNode)
              var_name = intern_name(target_node.name)
              var_name = var_name[2..-1] if var_name.starts_with?("@@")
              type_annotation = nil
              define_class_var_symbol(class_symbol, var_name, type_annotation, target_id)
            end
          when Frontend::DefNode
            (node.body || [] of Frontend::ExprId).each do |body_expr_id|
              scan_for_class_vars(class_symbol, body_expr_id, node)
            end
          when Frontend::IfNode
            (node.then_body || [] of Frontend::ExprId).each { |e| scan_for_class_vars(class_symbol, e, current_method) }
            (node.elsifs || [] of Frontend::ElsifBranch).each do |elsif_branch|
              elsif_branch.body.each { |e| scan_for_class_vars(class_symbol, e, current_method) }
            end
            (node.else_body || [] of Frontend::ExprId).each { |e| scan_for_class_vars(class_symbol, e, current_method) }
          when Frontend::WhileNode
            node.body.each { |e| scan_for_class_vars(class_symbol, e, current_method) }
          end
        end

        # Week 1: Infer instance variable type from assignment RHS
        # For @value = param where param has type annotation, return that type
        private def infer_ivar_type_from_assignment(value_expr_id : Frontend::ExprId, current_method : Frontend::DefNode?) : String?
          return nil unless current_method

          value_node = arena[value_expr_id]
          # If RHS is an identifier (e.g., parameter name)
          if value_node.is_a?(Frontend::IdentifierNode)
            param_name = intern_name(value_node.name)
            # Look for matching parameter
            current_method.params.try do |params|
              params.each do |param|
                # Phase BLOCK_CAPTURE: Skip anonymous block parameter
                next unless p_name = param.name
                if intern_name(p_name) == param_name
                  return param.type_annotation.try { |slice| intern_name(slice) }
                end
              end
            end
          end
          nil
        end

        private def handle_macro_redefinition(name : String, new_symbol : MacroSymbol, existing : Symbol, table : SymbolTable)
          case existing
          when MacroSymbol
            new_symbol.merge_declaration_origins_from(existing)
            table.redefine_macro(name, new_symbol)
          else
            emit_incompatible_redefinition(name, new_symbol, existing)
          end
        end

        private def handle_method_redefinition(name : String, new_symbol : MethodSymbol, existing : Symbol, table : SymbolTable)
          case existing
          when MethodSymbol
            # Phase 4B: Create OverloadSet for multiple methods with same name
            overload_set = OverloadSetSymbol.new(name, existing.node_id, [existing, new_symbol])
            table.redefine(name, overload_set)
          when OverloadSetSymbol
            # Phase 4B: Add to existing overload set
            existing.add_overload(new_symbol)
          when ClassSymbol, MacroSymbol, VariableSymbol
            emit_incompatible_redefinition(name, new_symbol, existing)
          else
            emit_incompatible_redefinition(name, new_symbol, existing)
          end
        end

        private def handle_class_redefinition(name : String, new_symbol : ClassSymbol, existing : Symbol, table : SymbolTable)
          case existing
          when ClassSymbol
            verify_superclass_consistency(name, new_symbol, existing)
            new_symbol = ClassSymbol.new(
              name,
              new_symbol.node_id,
              scope: existing.scope,
              class_scope: existing.class_scope,
              superclass_name: new_symbol.superclass_name || existing.superclass_name,
              type_parameters: new_symbol.type_parameters || existing.type_parameters,
              is_struct: new_symbol.is_struct? || existing.is_struct?,
              is_abstract: new_symbol.is_abstract? || existing.is_abstract?,
              explicit_superclass: new_symbol.explicit_superclass? || existing.explicit_superclass?
            )
            assign_symbol_file(new_symbol, new_symbol.node_id)
            new_symbol.merge_declaration_origins_from(existing)
            table.redefine(name, new_symbol)
          when ModuleSymbol
            # Handle the case where a class/struct definition replaces a module
            # This happens when parser creates `module X` for `module A::B::X`
            # (losing the path prefix), and later `struct X` is properly defined.
            # Reuse the module's scope as the class's instance scope.
            assign_symbol_file(new_symbol, new_symbol.node_id)
            new_symbol.merge_declaration_origins_from(existing)
            table.redefine(name, new_symbol)
          when MethodSymbol, MacroSymbol, VariableSymbol
            emit_incompatible_redefinition(name, new_symbol, existing)
          else
            emit_incompatible_redefinition(name, new_symbol, existing)
          end
        end

        private def verify_superclass_consistency(name : String, new_symbol : ClassSymbol, existing : ClassSymbol)
          previous_super = existing.superclass_name
          current_super = new_symbol.superclass_name
          default_super = implicit_superclass_name(name, new_symbol.is_struct? || existing.is_struct?)
          previous_is_default = default_super && previous_super == default_super
          current_is_default = default_super && current_super == default_super

          if previous_super && current_super && previous_super != current_super &&
             existing.explicit_superclass? && new_symbol.explicit_superclass? &&
             !previous_is_default && !current_is_default
            @diagnostics << Diagnostic.new(
              DiagnosticLevel::Error,
              "E2003",
              "class '#{name}' already defined with superclass '#{previous_super}'",
              span_for(new_symbol.node_id),
              [SecondarySpan.new(span_for(existing.node_id), "previous superclass declared here", existing.node_id, file_path_for(existing.node_id))],
              new_symbol.node_id,
              file_path_for(new_symbol.node_id)
            )
          end
        end

        private def emit_incompatible_redefinition(name : String, new_symbol : Symbol, existing : Symbol)
          @diagnostics << Diagnostic.new(
            DiagnosticLevel::Error,
            "E2001",
            "cannot redefine #{symbol_kind(existing)} '#{name}' as #{symbol_kind(new_symbol)}",
            span_for(new_symbol.node_id),
            [SecondarySpan.new(span_for(existing.node_id), "previous #{symbol_kind(existing)} defined here", existing.node_id, file_path_for(existing.node_id))],
            new_symbol.node_id,
            file_path_for(new_symbol.node_id)
          )
        end

        private def emit_duplicate_variable(name : String, new_symbol : VariableSymbol, existing : Symbol)
          @diagnostics << Diagnostic.new(
            DiagnosticLevel::Error,
            "E2002",
            "variable '#{name}' is already defined in this scope",
            span_for(new_symbol.node_id),
            [SecondarySpan.new(span_for(existing.node_id), "previous definition here", existing.node_id, file_path_for(existing.node_id))],
            new_symbol.node_id,
            file_path_for(new_symbol.node_id)
          )
        end

        private def emit_shadowing_warning(name : String, new_symbol : VariableSymbol, outer_symbol : VariableSymbol)
          @diagnostics << Diagnostic.new(
            DiagnosticLevel::Warning,
            "W2001",
            "variable '#{name}' shadows outer scope variable",
            span_for(new_symbol.node_id),
            [SecondarySpan.new(span_for(outer_symbol.node_id), "outer scope definition here", outer_symbol.node_id, file_path_for(outer_symbol.node_id))],
            new_symbol.node_id,
            file_path_for(new_symbol.node_id)
          )
        end

        private def lookup_variable_in_ancestors(table : SymbolTable?, name : String) : VariableSymbol?
          current = table
          while current
            if symbol = current.lookup_local(name)
              return symbol if symbol.is_a?(VariableSymbol)
            end
            current = current.parent
          end
          nil
        end

        private def span_for(node_id : Frontend::ExprId) : Frontend::Span
          arena[node_id].span
        end

        # Week 1 Day 2: Detect generic type parameters from method signature
        # Returns nil if no generic params, or Array(String) of param names like ["T", "U"]
        private def detect_generic_type_parameters(params : Array(Frontend::Parameter), return_annotation : String?) : Array(String)?
          type_param_names = Set(String).new

          # Check parameter types
          params.each do |param|
            if type_ann = param.type_annotation
              # Extract generic type parameters from type annotation
              # Examples: "T" → ["T"], "Box(T)" → ["T"], "Pair(K,V)" → ["K", "V"]
              extract_type_parameters(type_ann, type_param_names)
            end
          end

          # Check return type annotation
          if ret_ann = return_annotation
            extract_type_parameters(ret_ann.to_slice, type_param_names)
          end

          type_param_names.empty? ? nil : type_param_names.to_a.sort
        end

        # Extract type parameters from type annotation (zero-copy)
        # Examples: "T" → add "T", "Box(T)" → add "T", "Pair(K,V)" → add "K","V"
        private def extract_type_parameters(type_ann : Slice(UInt8), result : Set(String)) : Nil
          extract_type_parameters_from_name(intern_name(type_ann).strip, result)
        end

        private def extract_type_parameters_from_name(type_name : String, result : Set(String)) : Nil
          return if type_name.empty?

          if arrow_index = top_level_arrow_index(type_name)
            params_part = type_name[0...arrow_index].strip
            return_part = type_name[(arrow_index + 2)...].strip

            split_top_level_type_args(params_part).each do |part|
              extract_type_parameters_from_name(part, result)
            end
            extract_type_parameters_from_name(return_part, result) unless return_part.empty? || return_part == "_"
            return
          end

          if type_name.ends_with?("?") && type_name.size > 1
            extract_type_parameters_from_name(type_name[0...-1], result)
            return
          end

          if type_name.ends_with?("*") && type_name.size > 1
            extract_type_parameters_from_name(type_name[0...-1], result)
            return
          end

          if type_name.ends_with?(".class") && type_name.size > 6
            extract_type_parameters_from_name(type_name[0...-6], result)
            return
          end

          if type_name.includes?(" | ")
            union_parts = split_top_level_union(type_name)
            if union_parts.size > 1
              union_parts.each do |part|
                extract_type_parameters_from_name(part, result)
              end
              return
            end
          end

          if type_name.starts_with?('{') && type_name.ends_with?('}') && type_name.size >= 2
            inner = type_name[1...-1].strip
            unless inner.empty?
              split_top_level_type_args(inner).each do |part|
                if colon_index = top_level_named_tuple_separator(part)
                  extract_type_parameters_from_name(part[(colon_index + 1)...].strip, result)
                else
                  extract_type_parameters_from_name(part, result)
                end
              end
            end
            return
          end

          if paren_start = type_name.index('(')
            if paren_end = type_name.rindex(')')
              if paren_end > paren_start
                inner = type_name[(paren_start + 1)...paren_end]
                split_top_level_type_args(inner).each do |part|
                  extract_type_parameters_from_name(part, result)
                end
                return
              end
            end
          end

          if is_generic_type_parameter?(type_name)
            result << type_name
          end
        end

        private def top_level_arrow_index(type_name : String) : Int32?
          depth = 0
          limit = type_name.bytesize - 1
          i = 0
          while i < limit
            case type_name.byte_at(i)
            when 40_u8
              depth += 1
            when 41_u8
              depth -= 1 if depth > 0
            when 45_u8
              if depth == 0 && type_name.byte_at(i + 1) == 62_u8
                return i
              end
            end
            i += 1
          end
          nil
        end

        private def split_top_level_type_args(type_name : String) : Array(String)
          return [] of String if type_name.empty?

          parts = [] of String
          paren_depth = 0
          brace_depth = 0
          bracket_depth = 0
          start = 0
          i = 0
          while i < type_name.bytesize
            case type_name.byte_at(i)
            when 40_u8
              paren_depth += 1
            when 41_u8
              paren_depth -= 1 if paren_depth > 0
            when 123_u8
              brace_depth += 1
            when 125_u8
              brace_depth -= 1 if brace_depth > 0
            when 91_u8
              bracket_depth += 1
            when 93_u8
              bracket_depth -= 1 if bracket_depth > 0
            when 44_u8
              if paren_depth == 0 && brace_depth == 0 && bracket_depth == 0
                parts << type_name[start...i].strip
                start = i + 1
              end
            end
            i += 1
          end

          parts << type_name[start..].strip if start < type_name.bytesize
          parts.reject(&.empty?)
        end

        private def split_top_level_union(type_name : String) : Array(String)
          parts = [] of String
          depth = 0
          start = 0
          i = 0
          while i + 2 < type_name.bytesize
            case type_name.byte_at(i)
            when 40_u8
              depth += 1
            when 41_u8
              depth -= 1 if depth > 0
            when 32_u8
              if depth == 0 && type_name.byte_at(i + 1) == 124_u8 && type_name.byte_at(i + 2) == 32_u8
                parts << type_name[start...i].strip
                i += 3
                start = i
                next
              end
            end
            i += 1
          end

          parts << type_name[start..].strip if start < type_name.bytesize
          parts.reject(&.empty?)
        end

        # Check if a type name represents a generic type parameter
        # Returns true if it's NOT a builtin type and NOT a defined class
        private def is_generic_type_parameter?(type_name : String) : Bool
          return false unless constant_like_type_name?(type_name)

          # Builtin types
          return false if ["Int32", "Int64", "Float64", "String", "Bool", "Nil", "Char"].includes?(type_name)

          # Check if it's a defined class in current scope
          table = current_table
          while table
            if symbol = table.lookup_local(type_name)
              # If it's a ClassSymbol, it's not a generic parameter
              return false if symbol.is_a?(ClassSymbol)
            end
            table = table.parent
          end

          # Not a builtin, not a defined class → generic type parameter
          true
        end

        private def top_level_named_tuple_separator(type_name : String) : Int32?
          paren_depth = 0
          brace_depth = 0
          bracket_depth = 0
          i = 0
          while i < type_name.bytesize
            case type_name.byte_at(i)
            when 40_u8
              paren_depth += 1
            when 41_u8
              paren_depth -= 1 if paren_depth > 0
            when 123_u8
              brace_depth += 1
            when 125_u8
              brace_depth -= 1 if brace_depth > 0
            when 91_u8
              bracket_depth += 1
            when 93_u8
              bracket_depth -= 1 if bracket_depth > 0
            when 58_u8
              return i if paren_depth == 0 && brace_depth == 0 && bracket_depth == 0
            end
            i += 1
          end
          nil
        end

        private def constant_like_type_name?(type_name : String) : Bool
          return false if type_name.empty?

          first = type_name.byte_at(0)
          return false unless ascii_uppercase?(first)

          i = 1
          while i < type_name.bytesize
            byte = type_name.byte_at(i)
            unless ascii_uppercase?(byte) || ascii_lowercase?(byte) || ascii_digit?(byte) || byte == 95_u8
              return false
            end
            i += 1
          end

          true
        end

        private def ascii_uppercase?(byte : UInt8) : Bool
          byte >= 65_u8 && byte <= 90_u8
        end

        private def ascii_lowercase?(byte : UInt8) : Bool
          byte >= 97_u8 && byte <= 122_u8
        end

        private def ascii_digit?(byte : UInt8) : Bool
          byte >= 48_u8 && byte <= 57_u8
        end

        private def resolve_include_target(node : Frontend::IncludeNode) : IncludedModuleRef?
          if target_id = node.target
            resolve_mixin_target(target_id, nil)
          elsif name_slice = node.name
            resolve_mixin_target(nil, name_slice)
          else
            nil
          end
        end

        private def resolve_mixin_target(target_id : Frontend::ExprId?, name_slice : Slice(UInt8)?) : IncludedModuleRef?
          if target_id
            resolve_module_ref_from_expr(target_id)
          elsif name_slice
            if symbol = current_table.lookup(intern_name(name_slice))
              return IncludedModuleRef.new(symbol, nil) if symbol.is_a?(ModuleSymbol)
            end
            nil
          else
            nil
          end
        end

        private def resolve_module_ref_from_expr(expr_id : Frontend::ExprId) : IncludedModuleRef?
          node = arena[expr_id]
          case node
          when Frontend::GenericNode
            symbol = resolve_symbol_from_expr(node.base_type)
            return nil unless symbol.is_a?(ModuleSymbol)

            type_arg_names = Array(String).new(node.type_args.size)
            node.type_args.each do |arg_id|
              type_arg_names << type_expr_name_from_expr(arg_id)
            end
            IncludedModuleRef.new(symbol, type_arg_names)
          else
            symbol = resolve_symbol_from_expr(expr_id)
            return nil unless symbol.is_a?(ModuleSymbol)
            IncludedModuleRef.new(symbol, nil)
          end
        end

        private def type_expr_name_from_expr(expr_id : Frontend::ExprId) : String
          node = arena[expr_id]
          case node
          when Frontend::IdentifierNode
            intern_name(node.name)
          when Frontend::PathNode
            segments = [] of String
            collect_path_segments(node, segments)
            segments.join("::")
          when Frontend::GenericNode
            base = type_expr_name_from_expr(node.base_type)
            args = node.type_args.map { |arg_id| type_expr_name_from_expr(arg_id) }
            "#{base}(#{args.join(", ")})"
          when Frontend::TypeofNode
            args = node.args.map { |arg_id| type_expr_name_from_expr(arg_id) }
            "typeof(#{args.join(", ")})"
          when Frontend::CallNode
            callee_name = type_expr_name_from_expr(node.callee)
            args = node.args.map { |arg_id| type_expr_name_from_expr(arg_id) }
            "#{callee_name}(#{args.join(", ")})"
          when Frontend::MemberAccessNode
            object_name = type_expr_name_from_expr(node.object)
            "#{object_name}.#{intern_name(node.member)}"
          else
            node.to_s
          end
        end

        private def resolve_symbol_from_expr(expr_id : Frontend::ExprId) : Symbol?
          node = arena[expr_id]
          case node
          when Frontend::PathNode
            segments = [] of String
            collect_path_segments(node, segments)
            resolve_symbol_by_segments(segments, current_table) || resolve_symbol_by_segments(segments, @table_stack.first)
          when Frontend::IdentifierNode
            current_table.lookup(intern_name(node.name)) || @table_stack.first.lookup(intern_name(node.name))
          when Frontend::GenericNode
            resolve_symbol_from_expr(node.base_type)
          when Frontend::CallNode
            resolve_symbol_from_expr(node.callee)
          else
            nil
          end
        end

        private def collect_path_segments(node : Frontend::PathNode, segments : Array(String))
          if left_id = node.left
            left_node = arena[left_id]
            case left_node
            when Frontend::PathNode
              collect_path_segments(left_node, segments)
            when Frontend::IdentifierNode
              segments << intern_name(left_node.name)
            when Frontend::ConstantNode
              segments << intern_name(left_node.name)
            end
          end

          right_node = arena[node.right]
          case right_node
          when Frontend::IdentifierNode
            segments << intern_name(right_node.name)
          when Frontend::ConstantNode
            segments << intern_name(right_node.name)
          when Frontend::PathNode
            collect_path_segments(right_node, segments)
          end
        end

        private def resolve_symbol_by_segments(segments : Array(String), table : SymbolTable) : Symbol?
          return nil if segments.empty?
          symbol : Symbol? = nil
          current_table = table

          segments.each_with_index do |segment, index|
            symbol = current_table.lookup(segment)
            return nil unless symbol

            if index < segments.size - 1
              current_table = case symbol
                              when ModuleSymbol
                                symbol.scope
                              when ClassSymbol
                                symbol.scope
                              else
                                return nil
                              end
            end
          end

          symbol
        end

        private def define_instance_var_symbol(class_symbol : ClassSymbol, name : String, type_annotation : String?, node_id : Frontend::ExprId)
          sym_name = name.starts_with?("@") ? name : "@#{name}"
          sym = InstanceVarSymbol.new(sym_name, node_id, type_annotation)
          assign_symbol_file(sym, node_id)

          scope = class_symbol.scope
          if existing = scope.lookup_local(sym_name)
            scope.redefine(sym_name, sym)
          else
            begin
              scope.define(sym_name, sym)
            rescue SymbolRedefinitionError
              scope.redefine(sym_name, sym)
            end
          end
        end

        private def define_class_var_symbol(class_symbol : ClassSymbol, name : String, type_annotation : String?, node_id : Frontend::ExprId)
          sym_name = name.starts_with?("@@") ? name : "@@#{name}"
          sym = ClassVarSymbol.new(sym_name, node_id, type_annotation)
          assign_symbol_file(sym, node_id)

          {class_symbol.scope, class_symbol.class_scope}.each do |scope|
            next unless scope
            if existing = scope.lookup_local(sym_name)
              scope.redefine(sym_name, sym)
            else
              begin
                scope.define(sym_name, sym)
              rescue SymbolRedefinitionError
                scope.redefine(sym_name, sym)
              end
            end
          end
        end

        private def define_global_var_symbol(name : String, declared_type : String?, node_id : Frontend::ExprId)
          sym_name = name.starts_with?("$") ? name : "$#{name}"
          sym = GlobalVarSymbol.new(sym_name, node_id, declared_type)
          assign_symbol_file(sym, node_id)

          root = @table_stack.first
          if existing = root.lookup_local(sym_name)
            root.redefine(sym_name, sym)
          else
            begin
              root.define(sym_name, sym)
            rescue SymbolRedefinitionError
              root.redefine(sym_name, sym)
            end
          end
        end

        private def symbol_kind(symbol : Symbol) : String
          case symbol
          when MacroSymbol
            "macro"
          when MethodSymbol
            "method"
          when ClassSymbol
            "class"
          when VariableSymbol
            "variable"
          else
            "symbol"
          end
        end
      end
    end
  end
end
