require "../../frontend/ast"
require "../symbol"
require "../context"
require "../symbol_table"
require "../diagnostic"
require "../macro_expander"

module CrystalV2
  module Compiler
    module Semantic
      class SymbolCollector
        alias Program = Frontend::Program
        alias TypedNode = Frontend::TypedNode

        getter diagnostics : Array(Diagnostic)

      def initialize(@program : Program, context : Context)
        @arena = @program.arena
        @virtual_arena = @arena.is_a?(Frontend::VirtualArena) ? @arena.as(Frontend::VirtualArena) : nil
        @table_stack = [context.symbol_table]
        @diagnostics = [] of Diagnostic
          @macro_expander = MacroExpander.new(@program, @arena, context.flags)
          @class_stack = [] of ClassSymbol
          # Pending root-level annotations (for example,
          # @[JSON::Serializable::Options] immediately before a class
          # definition). These annotations are attached to the next class we
          # see at the top level.
          @pending_root_annotations = [] of Frontend::ExprId
        end

        def collect
          @program.roots.each do |root_id|
            node = @arena[root_id]

            case node
            when Frontend::AnnotationNode
              # Root-level annotation to be attached to the next class/module.
              @pending_root_annotations << root_id
            else
              visit(root_id)
            end
          end
          self
        end

        private def current_table
          @table_stack.last
        end

        private def push_table(table : SymbolTable)
          @table_stack << table
        end

        private def pop_table
          @table_stack.pop
        end

        private def file_path_for(node_id : Frontend::ExprId) : String?
          return nil unless @virtual_arena
          @virtual_arena.not_nil!.file_for_id(node_id)
        rescue
          nil
        end

        private def assign_symbol_file(symbol : Symbol, node_id : Frontend::ExprId)
          if path = file_path_for(node_id)
            symbol.file_path = path
          end
        end

      private def visit(node_id : Frontend::ExprId)
        return if node_id.invalid?

        node = @arena[node_id]

        case node
        when Frontend::MacroDefNode
          handle_macro_def(node_id, node)
        when Frontend::DefNode
          handle_def(node_id, node)
        when Frontend::ClassNode
          # Note: Structs are also parsed as ClassNode with is_struct=true
          handle_class(node_id, node)
        when Frontend::ModuleNode
          handle_module(node_id, node)
        when Frontend::EnumNode
          handle_enum(node_id, node)
        when Frontend::ConstantNode
          handle_constant(node_id, node)
        when Frontend::IncludeNode
          handle_include(node)
        when Frontend::ExtendNode
          handle_extend(node)
        when Frontend::GlobalVarDeclNode
          handle_global_var_decl(node_id, node)
        when Frontend::AssignNode
          handle_global_assignment(node)
        when Frontend::GetterNode, Frontend::SetterNode, Frontend::PropertyNode
          # Phase 87B-1: Expand accessor macros to method definitions
          expand_accessor_macro(node_id, node)
        when Frontend::CallNode
          # Phase 87B-2: Check if call is actually a macro invocation
            handle_potential_macro_call(node_id, node)
          end
        end

        private def handle_macro_def(node_id : Frontend::ExprId, node : Frontend::MacroDefNode)
          name_slice = node.name
          return unless name_slice

          name = String.new(name_slice)
          body_id = node.body
          return unless body_id

          body_node = @arena[body_id]
          unless body_node.is_a?(Frontend::MacroLiteralNode)
            return
          end

          symbol = MacroSymbol.new(name, node_id, body_id)
          assign_symbol_file(symbol, node_id)

          table = current_table
          if existing = table.lookup_local(name)
            handle_macro_redefinition(name, symbol, existing, table)
          else
            table.define(name, symbol)
          end
        end

        private def handle_def(node_id : Frontend::ExprId, node : Frontend::DefNode)
          name_slice = node.name
          return unless name_slice

          name = String.new(name_slice)
          params = node.params || [] of Frontend::Parameter
          return_annotation = node.return_type.try { |slice| String.new(slice) }

          # Week 1 Day 2: Detect generic type parameters from method signature
          type_params = detect_generic_type_parameters(params, return_annotation)

          receiver = node.receiver
          target_table = current_table
          is_class_method = false
          if receiver && String.new(receiver) == "self"
            target_table = @class_stack.last?.try(&.class_scope) || current_table
            is_class_method = true
          end

          method_scope = SymbolTable.new(target_table)
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
            param_name_str = String.new(param_name)
            param_type_str = if type_ann = param.type_annotation
              String.new(type_ann)
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

          (node.body || [] of Frontend::ExprId).each do |expr_id|
            visit(expr_id)
          end

          pop_table
        end

        private def handle_class(node_id : Frontend::ExprId, node : Frontend::ClassNode)
          name_slice = node.name
          return unless name_slice

          name = String.new(name_slice)
          super_name = node.super_name.try { |slice| String.new(slice) }

          # Week 1: Parse generic type parameters (e.g., class Box(T))
          type_params = node.type_params.try do |params|
            params.map { |param_slice| String.new(param_slice) }
          end

          table = current_table
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
          # For structs without explicit superclass, default to "Struct"
          effective_super = is_struct && super_name.nil? ? "Struct" : super_name
          class_symbol = ClassSymbol.new(name, node_id, scope: instance_scope, class_scope: meta_scope, superclass_name: effective_super, type_parameters: type_params, is_struct: is_struct, is_abstract: is_abstract)
          assign_symbol_file(class_symbol, node_id)

          if existing
            handle_class_redefinition(name, class_symbol, existing, table)
          else
            table.define(name, class_symbol)
          end

          push_table(instance_scope)

          # Phase 5A: Collect instance variable declarations
          # Get the final class symbol from table (may have been redefined)
          final_class_symbol = table.lookup_local(name)
          pushed_owner = false
          if final_class_symbol.is_a?(ClassSymbol)
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

          name = String.new(name_slice)
          table = current_table

          symbol = table.lookup_local(name)

          # If there's already a ClassSymbol (e.g., from struct Time), reuse its scope
          # This handles reopening scenarios where `module Foo` appears after `class Foo`
          if symbol.is_a?(ClassSymbol)
            # Reuse the existing class symbol's scope for module-style reopening
            push_table(symbol.scope)
            (node.body || [] of Frontend::ExprId).each { |expr_id| visit(expr_id) }
            pop_table
            return
          end

          module_symbol = case symbol
          when ModuleSymbol
            symbol.node_id = node_id
            symbol
          else
            new_scope = SymbolTable.new(table)
            created = ModuleSymbol.new(name, node_id, scope: new_scope)
            if symbol
              table.redefine(name, created)
            else
              table.define(name, created)
            end
            created
          end
          assign_symbol_file(module_symbol, node_id)

          push_table(module_symbol.scope)
          (node.body || [] of Frontend::ExprId).each { |expr_id| visit(expr_id) }
          pop_table
        end

        # Phase 102: Handle enum definitions
        private def handle_enum(node_id : Frontend::ExprId, node : Frontend::EnumNode)
          name_slice = node.name
          return unless name_slice

          name = String.new(name_slice)
          table = current_table

          # Parse base type if specified
          base_type = if bt = node.base_type
            String.new(bt)
          else
            "Int32"
          end

          # Collect enum members with their values
          members = {} of String => Int64
          next_value = 0i64
          node.members.each do |member|
            member_name = String.new(member.name)
            if val_id = member.value
              # Member has explicit value - try to evaluate it
              val_node = @program.arena[val_id]
              if val_node.is_a?(Frontend::NumberNode)
                members[member_name] = String.new(val_node.value).to_i64? || next_value
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

          if existing = table.lookup_local(name)
            table.redefine(name, enum_symbol)
          else
            table.define(name, enum_symbol)
          end
        end

        private def handle_constant(node_id : Frontend::ExprId, node : Frontend::ConstantNode)
          name_slice = node.name
          return unless name_slice

          name = String.new(name_slice)
          value_id = node.value

          const_symbol = ConstantSymbol.new(name, node_id, value_id)
          assign_symbol_file(const_symbol, node_id)

          table = current_table
          if existing = table.lookup_local(name)
            table.redefine(name, const_symbol)
          else
            table.define(name, const_symbol)
          end

          # Visit constant value to collect nested definitions
          visit(value_id)
        end

        private def handle_global_var_decl(node_id : Frontend::ExprId, node : Frontend::GlobalVarDeclNode)
          name = String.new(node.name)
          type_ann = node.type.try { |slice| String.new(slice) }
          define_global_var_symbol(name, type_ann, node_id)
        end

        private def handle_global_assignment(node : Frontend::AssignNode)
          target_id = node.target
          target_node = @arena[target_id]
          case target_node
          when Frontend::GlobalNode
            name = String.new(target_node.name)
            define_global_var_symbol(name, nil, target_id)
          end
        end

        private def handle_include(node : Frontend::IncludeNode)
          symbol = resolve_include_target(node)
          return unless symbol.is_a?(ModuleSymbol)
          # debug output?
          current_table.include_module(symbol)
        end

        private def handle_extend(node : Frontend::ExtendNode)
          symbol = resolve_mixin_target(node.target, node.name)
          return unless symbol.is_a?(ModuleSymbol)

          # If we're inside a class, extend should affect the metaclass/class_scope
          if owner = @class_stack.last?
            owner.class_scope.include_module(symbol)
          else
            current_table.include_module(symbol)
          end
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
              def_id = @arena.add_typed(def_node)
              visit(def_id)  # Immediately register as MethodSymbol

            when Frontend::SetterNode
              # Generate: def name=(value : Type); @name = value; end
              def_node = build_setter_def(spec, node.span)
              def_id = @arena.add_typed(def_node)
              visit(def_id)

            when Frontend::PropertyNode
              # Generate both getter and setter
              getter_node = build_getter_def(spec, node.span)
              setter_node = build_setter_def(spec, node.span)
              visit(@arena.add_typed(getter_node))
              visit(@arena.add_typed(setter_node))
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
          spec_name_str = String.new(spec.name)  # Convert for interpolation
          ivar_name = "@#{spec_name_str}"
          ivar_bytes = ivar_name.to_slice
          ivar_node = Frontend::InstanceVarNode.new(
            spec.name_span,
            ivar_bytes
          )
          ivar_id = @arena.add_typed(ivar_node)

          # Create def node with instance variable as body
          method_name_bytes = spec.name  # Already Slice(UInt8)
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
          spec_name_str = String.new(spec.name)  # Convert for interpolation
          ivar_name = "@#{spec_name_str}"
          ivar_bytes = ivar_name.to_slice
          ivar_node = Frontend::InstanceVarNode.new(
            spec.name_span,
            ivar_bytes
          )
          ivar_id = @arena.add_typed(ivar_node)

          # Create identifier node: value
          value_bytes = "value".to_slice
          value_node = Frontend::IdentifierNode.new(
            spec.name_span,
            value_bytes
          )
          value_id = @arena.add_typed(value_node)

          # Create assignment: @name = value
          assign_node = Frontend::AssignNode.new(
            base_span,
            ivar_id,
            value_id
          )
          assign_id = @arena.add_typed(assign_node)

          # Create def node with assignment as body
          spec_name_str2 = String.new(spec.name)  # Convert for interpolation
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
          # Extract method name from call
          callee_slice = Frontend.node_member(node)
          return unless callee_slice

          callee_name = String.new(callee_slice)

          # Look up in current scope
          table = current_table
          symbol = table.lookup(callee_name)

          # Check if it's a macro
          if symbol.is_a?(MacroSymbol)
            # Get arguments
            args = Frontend.node_args(node) || [] of Frontend::ExprId

            # Determine owner type (current class) for @type reflection
            owner_type = @class_stack.empty? ? nil : @class_stack.last

            # Expand macro with optional owner_type
            expanded_id = @macro_expander.expand(symbol, args, owner_type)

            # Collect diagnostics from expander
            @diagnostics.concat(@macro_expander.diagnostics)

            # Visit expanded result (if valid)
            visit(expanded_id) unless expanded_id.invalid?
          end

          # If not a macro, ignore - will be handled during type inference
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
            node = @arena[ann_id]
            next unless node.is_a?(Frontend::AnnotationNode)

            if info = build_annotation_info(node)
              class_symbol.add_annotation(info)
            end
          end
        end

        # Attach pending annotations to an explicit instance variable
        # declaration inside a class body.
        private def attach_ivar_annotations(class_symbol : ClassSymbol, node : Frontend::InstanceVarDeclNode, annotation_ids : Array(Frontend::ExprId))
          return if annotation_ids.empty?

          ivar_name = String.new(node.name)
          ivar_name = ivar_name[1..-1] if ivar_name.starts_with?("@")

          annotation_ids.each do |ann_id|
            ann_node = @arena[ann_id]
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
            ann_node = @arena[ann_id]
            next unless ann_node.is_a?(Frontend::AnnotationNode)
            build_annotation_info(ann_node)
          end

          return if infos.empty?

          specs.each do |spec|
            spec_name = String.new(spec.name)
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
              key = String.new(named_arg.name)
              named_args_hash[key] = named_arg.value
            end
          end

          AnnotationInfo.new(full_name, args, named_args_hash)
        end

        # Build a fully-qualified annotation name from its name expression.
        # Handles simple identifiers and nested PathNode chains such as
        # JSON::Serializable::Options.
        private def annotation_full_name(name_expr_id : Frontend::ExprId) : String
          node = @arena[name_expr_id]

          case node
          when Frontend::IdentifierNode
            String.new(node.name)
          when Frontend::PathNode
            parts = [] of String
            current_id = name_expr_id

            # Traverse left-associative PathNode chain
            while true
              current = @arena[current_id]
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
                parts << String.new(current.name)
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
            node = @arena[expr_id]

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

            when Frontend::CallNode
              pending_annotations.clear
              handle_potential_macro_call(expr_id, node)

            when Frontend::IncludeNode
              pending_annotations.clear
              handle_include(node)

            when Frontend::ExtendNode
              pending_annotations.clear
              handle_extend(node)

            else
              # Any other node breaks the annotation chain
              pending_annotations.clear
            end
          end
        end

        private def scan_for_instance_vars(class_symbol : ClassSymbol, expr_id : Frontend::ExprId, current_method : Frontend::DefNode? = nil)
          return if expr_id.invalid?
          node = @arena[expr_id]

          case node
          when Frontend::InstanceVarDeclNode
            # Phase 5C: Handle explicit type annotations (@var : Type = default)
            var_name = String.new(node.name)
            var_name = var_name[1..-1] if var_name.starts_with?("@")
            type_annotation = node.type.try { |slice| String.new(slice) }
            default_value = node.value
            has_default = !default_value.nil?
            class_symbol.add_instance_var(var_name, type_annotation, default_value, has_default)
            define_instance_var_symbol(class_symbol, var_name, type_annotation, expr_id)
          when Frontend::AssignNode
            # Check if assignment target is instance variable
            target_id = node.target
            target_node = @arena[target_id]
            if target_node.is_a?(Frontend::InstanceVarNode)
              var_name = String.new(target_node.name)
              var_name = var_name[1..-1] if var_name.starts_with?("@")
              unless class_symbol.get_instance_var_type(var_name)
                # Week 1: Try to infer type from RHS if it's a parameter reference
                type_annotation = infer_ivar_type_from_assignment(node.value, current_method)
                # Assignment in initialize is a form of default value
                default_value = node.value
                has_default = current_method.try { |m| String.new(m.name.not_nil!) == "initialize" } || false
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
          node = @arena[expr_id]

          case node
          when Frontend::ClassVarDeclNode
            var_name = String.new(node.name)
            var_name = var_name[2..-1] if var_name.starts_with?("@@")
            type_annotation = node.type.try { |slice| String.new(slice) }
            define_class_var_symbol(class_symbol, var_name, type_annotation, expr_id)
          when Frontend::AssignNode
            target_id = node.target
            target_node = @arena[target_id]
            if target_node.is_a?(Frontend::ClassVarNode)
              var_name = String.new(target_node.name)
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

          value_node = @arena[value_expr_id]
          # If RHS is an identifier (e.g., parameter name)
          if value_node.is_a?(Frontend::IdentifierNode)
            param_name = String.new(value_node.name)
            # Look for matching parameter
            current_method.params.try do |params|
              params.each do |param|
                # Phase BLOCK_CAPTURE: Skip anonymous block parameter
                next unless p_name = param.name
                if String.new(p_name) == param_name
                  return param.type_annotation.try { |slice| String.new(slice) }
                end
              end
            end
          end
          nil
        end

        private def handle_macro_redefinition(name : String, new_symbol : MacroSymbol, existing : Symbol, table : SymbolTable)
          case existing
          when MacroSymbol
            table.redefine(name, new_symbol)
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
              is_abstract: new_symbol.is_abstract? || existing.is_abstract?
            )
            assign_symbol_file(new_symbol, new_symbol.node_id)
            table.redefine(name, new_symbol)
          when ModuleSymbol
            # Handle the case where a class/struct definition replaces a module
            # This happens when parser creates `module X` for `module A::B::X`
            # (losing the path prefix), and later `struct X` is properly defined.
            # Reuse the module's scope as the class's instance scope.
            assign_symbol_file(new_symbol, new_symbol.node_id)
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

          if previous_super && current_super && previous_super != current_super
            @diagnostics << Diagnostic.new(
              DiagnosticLevel::Error,
              "E2003",
              "class '#{name}' already defined with superclass '#{previous_super}'",
              span_for(new_symbol.node_id),
              [SecondarySpan.new(span_for(existing.node_id), "previous superclass declared here")]
            )
          end
        end

        private def emit_incompatible_redefinition(name : String, new_symbol : Symbol, existing : Symbol)
          @diagnostics << Diagnostic.new(
            DiagnosticLevel::Error,
            "E2001",
            "cannot redefine #{symbol_kind(existing)} '#{name}' as #{symbol_kind(new_symbol)}",
            span_for(new_symbol.node_id),
            [SecondarySpan.new(span_for(existing.node_id), "previous #{symbol_kind(existing)} defined here")]
          )
        end

        private def emit_duplicate_variable(name : String, new_symbol : VariableSymbol, existing : Symbol)
          @diagnostics << Diagnostic.new(
            DiagnosticLevel::Error,
            "E2002",
            "variable '#{name}' is already defined in this scope",
            span_for(new_symbol.node_id),
            [SecondarySpan.new(span_for(existing.node_id), "previous definition here")]
          )
        end

        private def emit_shadowing_warning(name : String, new_symbol : VariableSymbol, outer_symbol : VariableSymbol)
          @diagnostics << Diagnostic.new(
            DiagnosticLevel::Warning,
            "W2001",
            "variable '#{name}' shadows outer scope variable",
            span_for(new_symbol.node_id),
            [SecondarySpan.new(span_for(outer_symbol.node_id), "outer scope definition here")]
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
          @arena[node_id].span
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
          type_name = String.new(type_ann)

          # Check for generic syntax: Box(T), Pair(K,V)
          if paren_start = type_ann.index('('.ord.to_u8)
            if paren_end = type_ann.rindex(')'.ord.to_u8)
              if paren_end > paren_start
                # Extract inner type parameters using zero-copy pointer arithmetic
                start_ptr = type_ann.to_unsafe + paren_start + 1
                length = paren_end - paren_start - 1
                inner_slice = Slice.new(start_ptr, length)

                # Split by comma for multiple type params: "K,V" → ["K", "V"]
                current_start = 0
                inner_slice.each_with_index do |byte, i|
                  if byte == ','.ord.to_u8
                    # Extract parameter (zero-copy)
                    param_slice = Slice.new(inner_slice.to_unsafe + current_start, i - current_start)
                    param_name = String.new(param_slice).strip
                    if is_generic_type_parameter?(param_name)
                      result << param_name
                    end
                    current_start = i + 1
                  end
                end

                # Last parameter (or only parameter)
                if current_start < inner_slice.size
                  param_slice = Slice.new(inner_slice.to_unsafe + current_start, inner_slice.size - current_start)
                  param_name = String.new(param_slice).strip
                  if is_generic_type_parameter?(param_name)
                    result << param_name
                  end
                end

                return
              end
            end
          end

          # Simple type without parens - check if it's a type parameter
          if is_generic_type_parameter?(type_name)
            result << type_name
          end
        end

        # Check if a type name represents a generic type parameter
        # Returns true if it's NOT a builtin type and NOT a defined class
        private def is_generic_type_parameter?(type_name : String) : Bool
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

        private def resolve_include_target(node : Frontend::IncludeNode) : Symbol?
          if target_id = node.target
            resolve_symbol_from_expr(target_id)
          elsif name_slice = node.name
            current_table.lookup(String.new(name_slice))
          else
            nil
          end
        end

        private def resolve_mixin_target(target_id : Frontend::ExprId?, name_slice : Slice(UInt8)?) : Symbol?
          if target_id
            resolve_symbol_from_expr(target_id)
          elsif name_slice
            current_table.lookup(String.new(name_slice))
          else
            nil
          end
        end

        private def resolve_symbol_from_expr(expr_id : Frontend::ExprId) : Symbol?
          node = @arena[expr_id]
          case node
          when Frontend::PathNode
            segments = [] of String
            collect_path_segments(node, segments)
            resolve_symbol_by_segments(segments)
          when Frontend::IdentifierNode
            resolve_symbol_by_segments([String.new(node.name)])
          else
            nil
          end
        end

        private def collect_path_segments(node : Frontend::PathNode, segments : Array(String))
          if left_id = node.left
            left_node = @arena[left_id]
            case left_node
            when Frontend::PathNode
              collect_path_segments(left_node, segments)
            when Frontend::IdentifierNode
              segments << String.new(left_node.name)
            end
          end

          right_node = @arena[node.right]
          case right_node
          when Frontend::IdentifierNode
            segments << String.new(right_node.name)
          when Frontend::PathNode
            collect_path_segments(right_node, segments)
          end
        end

        private def resolve_symbol_by_segments(segments : Array(String)) : Symbol?
          return nil if segments.empty?
          table = @table_stack.first
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
