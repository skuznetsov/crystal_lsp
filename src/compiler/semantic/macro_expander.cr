require "set"
require "../frontend/ast"
require "../frontend/lexer"
require "../frontend/parser"
require "./symbol"
require "./diagnostic"

module CrystalV2
  module Compiler
    module Semantic
      # Phase 87B-4A: General Macro Expansion Engine (with Control Flow + Range Iteration)
      #
      # SCOPE (Phase 87B-2 + 87B-3 + 87B-4A):
      # ✅ Macro call detection (method call → macro resolution)
      # ✅ Basic {{ expr }} evaluation (literals, constants, identifiers)
      # ✅ Simple parameter substitution
      # ✅ Recursion depth limit (100)
      # ✅ {% if %} / {% elsif %} / {% else %} control flow
      # ✅ {% for %} iteration (ArrayLiteral + Range)
      # ✅ Range iteration (1..10, 1...10) with integer bounds
      # ✅ Range size limit (max 10000 elements)
      # ✅ Crystal truthiness (false/nil falsy, everything else truthy)
      # ✅ Nested control flow with depth tracking
      #
      # OUT OF SCOPE (Future Phases):
      # ❌ TypeNode reflection → Phase 87B-4B+
      # ❌ Macro methods (.stringify, .id) → Phase 87B-6
      class MacroExpander
        alias Program = Frontend::Program
        alias TypedNode = Frontend::TypedNode
        alias ExprId = Frontend::ExprId
        alias MacroPiece = Frontend::MacroPiece

        # Maximum macro expansion depth (prevents infinite recursion)
        MAX_DEPTH = 100

        # Maximum range size in for loops (Phase 87B-4A: prevent compilation DOS)
        MAX_RANGE_SIZE = 10000

        getter diagnostics : Array(Diagnostic)

        def initialize(@program : Program, @arena : Frontend::ArenaLike, flags : Set(String)? = nil, *, recovery_mode : Bool = false)
          @diagnostics = [] of Diagnostic
          @depth = 0
          @macro_var_counter = 0
          @flags = flags || Set(String).new
          @recovery_mode = recovery_mode
        end

        # Expansion context for macro evaluation
        class Context
          getter variables : Hash(String, String)
          getter macro_vars : Hash(String, String)
          # Optional owner type for @type reflection macros
          getter owner_type : ClassSymbol?
          getter depth : Int32
          getter flags : Set(String)

          def initialize(
            @variables = {} of String => String,
            @macro_vars = {} of String => String,
            @owner_type : ClassSymbol? = nil,
            @depth = 0,
            @flags = Set(String).new
          )
          end

          def with_depth(new_depth : Int32) : Context
            Context.new(@variables, @macro_vars, @owner_type, new_depth, @flags)
          end

          def with_variable(name : String, value : String) : Context
            new_vars = @variables.dup
            new_vars[name] = value
            Context.new(new_vars, @macro_vars, @owner_type, @depth, @flags)
          end

          def with_owner_type(owner : ClassSymbol?) : Context
            Context.new(@variables, @macro_vars, owner, @depth, @flags)
          end

          def set_macro_var(name : String, value : String)
            @macro_vars[name] = value
          end

          def macro_var(name : String) : String?
            @macro_vars[name]?
          end

          def flag?(name : String) : Bool
            @flags.includes?(name)
          end
        end

        # Simple value representation during evaluation
        # For Phase 87B-2: Just use String (empty string = undefined)
        # Future: Richer type system with numbers, bools, arrays
        alias Value = String

        # Main expansion entry point
        #
        # Takes a MacroSymbol and arguments, returns expanded AST node.
        # Optional owner_type is the class in which the macro is expanded and
        # is used to support @type.* reflection macros.
        def expand(macro_symbol : MacroSymbol, args : Array(ExprId), owner_type : ClassSymbol? = nil) : ExprId
          # Clear diagnostics from previous expansions
          @diagnostics.clear

          # Check recursion depth
          if @depth >= MAX_DEPTH
            emit_error("Macro recursion depth exceeded (#{MAX_DEPTH})")
            return ExprId.new(-1)
          end

          @depth += 1
          begin
            # Bind parameters to arguments
            context = build_context(macro_symbol, args, owner_type)

            # Evaluate macro body
            output = evaluate_macro_body(macro_symbol.body, context)

            # Parse result back to AST
            reparse(output, macro_symbol.node_id)
          ensure
            @depth -= 1
          end
        end

        private def build_context(macro_symbol : MacroSymbol, args : Array(ExprId), owner_type : ClassSymbol?) : Context
          variables = {} of String => String
          params = macro_symbol.params || [] of String

          # Bind each parameter to its argument value
          params.each_with_index do |param_name, index|
            if index < args.size
              # Evaluate argument to string
              arg_value = stringify_expr(args[index])
              variables[param_name] = arg_value
            else
              # Missing argument - leave undefined (empty string)
              variables[param_name] = ""
            end
          end

          Context.new(variables, {} of String => String, owner_type, @depth, @flags)
        end

        # Convert expression to string representation (for parameter binding)
        private def stringify_expr(expr_id : ExprId) : String
          node = @arena[expr_id]

          case Frontend.node_kind(node)
          when .number?, .string?, .identifier?, .bool?
            Frontend.node_literal_string(node) || ""
          when .nil?
            "nil"
          else
            # Complex expression - return source representation
            # For Phase 87B-2: Just return identifier or empty
            Frontend.node_literal_string(node) || ""
          end
        end

        private def evaluate_macro_body(body_id : ExprId, context : Context) : String
          # Get MacroLiteral node (works with typed or legacy nodes)
          body_node = @arena[body_id]
          return "" unless body_node.is_a?(Frontend::MacroLiteralNode)
          pieces = body_node.pieces

          # Phase 87B-3: Use indexed loop to handle control flow jumps
          String.build do |str|
            index = 0

            while index < pieces.size
              piece = pieces[index]

              case piece.kind
              when .text?
                # Plain text - append as-is
                str << piece.text if piece.text
                index += 1

              when .expression?
                # {{ expr }} - evaluate and stringify
                if expr_id = piece.expr
                  value = evaluate_expression(expr_id, context)
                  str << value
                end
                index += 1
              when .macro_var?
                if name = piece.macro_var_name
                  value = context.macro_var(name)
                  unless value
                    fresh = fresh_macro_var_name(name)
                    context.set_macro_var(name, fresh)
                    value = fresh
                  end
                  str << value
                end
                index += 1

              when .control_start?
                # {% if %} or {% for %} - delegate to specialized handlers
                keyword = piece.control_keyword

                if keyword == "if"
                  output_part, new_index = evaluate_if_block(pieces, index, context)
                  str << output_part
                  index = new_index
                elsif keyword == "for"
                  output_part, new_index = evaluate_for_block(pieces, index, context)
                  str << output_part
                  index = new_index
                else
                  # Unknown control keyword
                  emit_warning("Unknown control keyword: #{keyword}", body_id)
                  index += 1
                end

              else
                # Skip standalone control flow markers (elsif, else, end)
                # These are handled inside evaluate_if_block
                index += 1
              end
            end
          end
        end

        private def reparse(output : String, location : ExprId) : ExprId
          # Create lexer from generated string
          lexer = Frontend::Lexer.new(output)

          # Create parser with existing arena (uses Phase 87B-2 constructor)
          parser = Frontend::Parser.new(lexer, @arena, recovery_mode: @recovery_mode)

          # Parse as expression
          # Use precedence 0 to parse full expression
          begin
            result_id = parser.parse_expression(0)

            # Check for parse errors
            if parser.diagnostics.any?
              # Emit diagnostic: macro generated invalid syntax
              parse_errors = parser.diagnostics.map(&.message).join("; ")
              emit_error("Macro expansion generated invalid syntax: #{parse_errors}. Generated code: \"#{output}\"", location)
              return ExprId.new(-1)
            end

            result_id
          rescue ex
            # Catch any parser exceptions
            emit_error("Failed to parse macro expansion: #{ex.message}. Generated code: \"#{output}\"", location)
            ExprId.new(-1)
          end
        end

        private def evaluate_expression(expr_id : ExprId, context : Context) : Value
          node = @arena[expr_id]

          case Frontend.node_kind(node)
          when .number?
            # Number literal: 42, 3.14
            Frontend.node_literal_string(node) || ""

          when .string?
            # String literal: "hello" (preserve quotes)
            literal = Frontend.node_literal_string(node) || ""
            literal.inspect
          when .char?
            # Char literal: 'a'
            literal = Frontend.node_literal_string(node) || ""
            literal.empty? ? "" : "'#{literal}'"
          when .symbol?
            # Symbol literal: :foo
            literal = Frontend.node_literal_string(node) || ""
            return "" if literal.empty?
            literal.starts_with?(":") ? literal : ":#{literal}"

          when .macro_var?
            literal = Frontend.node_literal_string(node)
            return "" unless literal
            if value = context.macro_var(literal)
              value
            else
              fresh = fresh_macro_var_name(literal)
              context.set_macro_var(literal, fresh)
              fresh
            end

          when .identifier?
            # Variable reference: look up in context
            if name = Frontend.node_literal_string(node)
              context.variables[name]? || ""
            else
              ""
            end

          when .bool?
            # Boolean: true/false
            Frontend.node_literal_string(node) || ""

          when .nil?
            # Nil literal
            ""

          else
            # Additional support for type-reflection and simple member chains
            if node.is_a?(Frontend::InstanceVarNode)
              evaluate_instance_var_expression(node, context)
            elsif node.is_a?(Frontend::MemberAccessNode)
              evaluate_member_access_expression(node, context)
            elsif node.is_a?(Frontend::CallNode)
              evaluate_call_expression(node, context)
            elsif node.is_a?(Frontend::PathNode)
              path_to_string(node)
            else
              # Unsupported expression type for Phase 87B-2
              # Return empty string (graceful degradation)
              ""
            end
          end
        end

        # Evaluate instance variable expressions used in macros
        # Currently only supports @type for type-reflection macros.
        private def evaluate_instance_var_expression(node, context : Context) : Value
          name_slice = node.name
          name = String.new(name_slice)
          if name == "@type"
            if owner = context.owner_type
              return owner.name
            end
          end
          ""
        end

        # Evaluate simple member access chains used in macros, such as
        #   @type.name
        #   @type.name.id.stringify
        #   ivar.id
        private def evaluate_member_access_expression(node, context : Context) : Value
          # Resolve base value
          base_value = case obj = @arena[node.object]
                       when Frontend::InstanceVarNode
                         evaluate_instance_var_expression(obj, context)
                       when Frontend::IdentifierNode
                         var_name = Frontend.node_literal_string(obj) || ""
                         context.variables[var_name]? || ""
                       when Frontend::MemberAccessNode
                         evaluate_member_access_expression(obj, context)
                       else
                         ""
                       end

          member = String.new(node.member)

          # For @type.name and chained calls, just return the type name
          if base_value != "" && context.owner_type && member == "name"
            return base_value
          end

          # For ivar.id / ivar.name / ivar.stringify, propagate base value
          if member == "id" || member == "name" || member == "stringify"
            return base_value
          end

          base_value
        end

        # Evaluate simple call expressions, primarily to support
        #   @type.name.stringify
        #   @type.name(generic_args: false)
        #   @type.size
        #   @type.annotation(Foo)
        #   ivar.id.stringify
        private def evaluate_call_expression(node, context : Context) : Value
          callee_id = node.callee
          return "" unless callee_id
          callee = @arena[callee_id]

          # Handle type-reflection and annotation helpers
          if callee.is_a?(Frontend::MemberAccessNode)
            obj = callee.object
            member = String.new(callee.member)

            # Basic support for annotation queries:
            #   @type.annotation(Foo)
            #   @type.annotations(Foo)
            #   ivar.annotation(Foo)   (where ivar is a macro variable name)
            #
            # Это делает только одно: отвечает на вопрос "есть ли такая
            # аннотация?", возвращая "1" если да и "" если нет. Этого
            # достаточно для truthiness в условиях if/unless. Детальный доступ
            # по индексам/ключам пока не реализуем.
            if member == "annotation" || member == "annotations"
              # @type.annotation(Foo)
              if obj.is_a?(Frontend::InstanceVarNode)
                ivar_name = String.new(obj.name)
                if ivar_name == "@type" && context.owner_type
                  class_symbol = context.owner_type.as(ClassSymbol)

                  filter_name = nil
                  if arg0_id = node.args[0]?
                    filter_name = annotation_type_full_name(arg0_id)
                  end

                  matching = if filter_name
                    class_symbol.annotations.select { |info| info.full_name == filter_name }
                  else
                    class_symbol.annotations
                  end

                  return matching.empty? ? "" : "1"
                end

                # Любые другие instance var (не @type) пока не поддерживаем.
                return ""
              end

              # ivar.annotation(Foo) где ivar — макро-переменная с именем
              # инстанс-поля (взятым из @type.instance_vars).
              if obj.is_a?(Frontend::IdentifierNode) && context.owner_type
                id_name = Frontend.node_literal_string(obj) || ""
                base_name = context.variables[id_name]? || ""
                if base_name.empty?
                  return ""
                end

                class_symbol = context.owner_type.as(ClassSymbol)

                filter_name = nil
                if arg0_id = node.args[0]?
                  filter_name = annotation_type_full_name(arg0_id)
                end

                infos = class_symbol.ivar_annotations[base_name]? || [] of AnnotationInfo
                matching = if filter_name
                  infos.select { |info| info.full_name == filter_name }
                else
                  infos
                end

                return matching.empty? ? "" : "1"
              end

              # Остальные приёмники для .annotation/.annotations пока не
              # интерпретируем.
              return ""
            end

            # Support @type.overrides?(BaseType, "method") in a lightweight way
            # by checking whether the current owner type defines a method with
            # the given name in its own scope. This approximates the original
            # semantics enough for macros like @type.overrides?(Reference,
            # "inspect") and @type.overrides?(Struct, "inspect").
            if obj.is_a?(Frontend::InstanceVarNode)
              name = String.new(obj.name)
              if name == "@type" && context.owner_type && member == "overrides?"
                class_symbol = context.owner_type.as(ClassSymbol)

                # Arguments: (base_type, method_name) or just (method_name)
                method_name = nil
                if node.args.size >= 2
                  method_name = evaluate_expression(node.args[1], context)
                elsif node.args.size == 1
                  method_name = evaluate_expression(node.args[0], context)
                end

                if method_name && !method_name.empty?
                  if sym = class_symbol.scope.lookup(method_name)
                    if sym.is_a?(MethodSymbol) || sym.is_a?(OverloadSetSymbol)
                      return "1"
                    end
                  end
                end

                # No matching method found on this type
                return ""
              end
            end

            # @type.name(...) / @type.size
            if obj.is_a?(Frontend::InstanceVarNode)
              name = String.new(obj.name)
              if name == "@type" && context.owner_type
                class_symbol = context.owner_type.as(ClassSymbol)

                case member
                when "name"
                  # In Crystal, @type.name(generic_args: false) strips generic
                  # arguments from the printed name. We approximate this by
                  # string processing on the class symbol name.
                  generic_args_false = false
                  if named_args = node.named_args
                    named_args.each do |named_arg|
                      arg_name = String.new(named_arg.name)
                      next unless arg_name == "generic_args"

                      value_str = evaluate_expression(named_arg.value, context)
                      generic_args_false = (value_str == "false")
                    end
                  end

                  full_name = class_symbol.name
                  if generic_args_false
                    # Strip trailing generic arguments: Foo(T, U) → Foo
                    if idx = full_name.index('(')
                      return full_name[0, idx]
                    end
                  end
                  return full_name

                when "size"
                  # Approximate @type.size as the number of generic type
                  # parameters declared on the owning class.
                  type_params = class_symbol.type_parameters
                  return (type_params ? type_params.size : 0).to_s
                end
              end
            end

            # Fallback to member access evaluation (handles .id, .stringify, etc.)
            return evaluate_member_access_expression(callee, context)
          elsif callee.is_a?(Frontend::IdentifierNode)
            name = Frontend.node_literal_string(callee) || ""
            if name == "flag?"
              return evaluate_flag_call(node, context)
            end
          end

          ""
        end

        private def evaluate_flag_call(node : Frontend::CallNode, context : Context) : Value
          arg_id = node.args.first?
          return "" unless arg_id

          raw_value = evaluate_expression(arg_id, context)
          flag_name = raw_value.strip
          if flag_name.starts_with?(":")
            flag_name = flag_name[1..-1]
          end
          if flag_name.starts_with?("\"") && flag_name.ends_with?("\"")
            flag_name = flag_name[1...-1]
          end

          context.flag?(flag_name) ? "true" : ""
        end

        # Build a fully-qualified annotation type name from its expression,
        # mirroring the behavior used when collecting AnnotationInfo. Handles
        # simple identifiers and nested PathNode chains such as
        # JSON::Serializable::Options.
        private def annotation_type_full_name(expr_id : ExprId) : String
          node = @arena[expr_id]

          case node
          when Frontend::IdentifierNode
            String.new(node.name)
          when Frontend::PathNode
            parts = [] of String
            current_id = expr_id

            while true
              current = @arena[current_id]
              case current
              when Frontend::PathNode
                right_id = current.right
                right_name = annotation_type_full_name(right_id)
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

        private def fresh_macro_var_name(base : String) : String
          sanitized = String.build do |builder|
            base.each_char do |ch|
              if ch.alphanumeric? || ch == '_'
                builder << ch
              else
                builder << '_'
              end
            end
          end

          sanitized = "tmp" if sanitized.empty?
          sanitized = "_#{sanitized}" if sanitized[0].number?

          @macro_var_counter += 1
          "__macro_#{sanitized}_#{@macro_var_counter}"
        end

        # Phase 87B-3: Control Flow Methods
        # ====================================

        # Evaluate condition expression to boolean (Crystal truthiness)
        # CRITICAL: false/nil/Nop are falsy. For other expressions we fall back
        # to evaluating them to a string and treating empty string, "false" and
        # "nil" as falsy; everything else is truthy. This keeps semantics close
        # to Crystal while allowing graceful degradation when we cannot fully
        # interpret an expression.
        private def evaluate_condition(expr_id : ExprId, context : Context) : Bool
          node = @arena[expr_id]

          # Handle boolean connectives && / || at the AST level so we can
          # compose more precise conditions from simpler ones.
          if node.is_a?(Frontend::BinaryNode)
            op = String.new(node.operator)

            case op
            when "&&"
              left_cond = evaluate_condition(node.left, context)
              return false unless left_cond
              return evaluate_condition(node.right, context)
            when "||"
              left_cond = evaluate_condition(node.left, context)
              return true if left_cond
              return evaluate_condition(node.right, context)
            else
              # Lightweight numeric comparison support (i > 0, i == 0, etc.).
              left_val = evaluate_expression(node.left, context)
              right_val = evaluate_expression(node.right, context)

              if (left_int = left_val.to_i?) && (right_int = right_val.to_i?)
                case op
                when ">"
                  return left_int > right_int
                when ">="
                  return left_int >= right_int
                when "<"
                  return left_int < right_int
                when "<="
                  return left_int <= right_int
                when "=="
                  return left_int == right_int
                when "!="
                  return left_int != right_int
                end
              end
            end
          end

          # Treat identifier conditions based on the bound macro variable value,
          # so constructs like `if ann` or `if flag` behave reasonably.
          if node.is_a?(Frontend::IdentifierNode)
            name = Frontend.node_literal_string(node)
            if name && (value = context.variables[name]?)
              # Consider empty string, "false", and "nil" as falsy.
              return false if value.empty? || value == "false" || value == "nil"
              return true
            else
              # Unbound macro variable is treated as falsy.
              return false
            end
          end

          case Frontend.node_kind(node)
          when .bool?
            # Parse "true" or "false"
            literal = Frontend.node_literal_string(node)
            if literal
              return false if literal == "false"
              return true  # "true"
            end
            # Default to true if it's a bool node without literal (shouldn't happen)
            return true

          when .nil?
            # nil is falsy
            return false

          else
            # Fallback: evaluate expression to a string and apply Crystal-like
            # truthiness. Unsupported expressions tend to evaluate to empty
            # string, which we treat as falsy, avoiding spurious "true".
            value = evaluate_expression(expr_id, context)
            return false if value.empty? || value == "false" || value == "nil"
            return true
          end
        end

        private def path_to_string(node : Frontend::PathNode) : String
          parts = [] of String
          current = node
          loop do
            right_node = @arena[current.right]
            parts << (Frontend.node_literal_string(right_node) || "")
            if left_id = current.left
              left_node = @arena[left_id]
              if left_node.is_a?(Frontend::PathNode)
                current = left_node
              else
                parts << (Frontend.node_literal_string(left_node) || "")
                break
              end
            else
              break
            end
          end
          parts.reverse.join("::")
        end

        # Find matching {% end %} for given {% if %} or {% for %}
        # Handles nested control flow via depth tracking
        private def find_matching_end(pieces : Array(MacroPiece), start_index : Int32) : Int32
          depth = 1
          index = start_index + 1

          while index < pieces.size
            piece = pieces[index]

            case piece.kind
            when .control_start?
              # Nested control structure
              depth += 1

            when .control_end?
              depth -= 1
              return index if depth == 0
            end

            index += 1
          end

          # Missing {% end %} - emit error
          emit_error("Unmatched control flow block (missing {% end %})")
          return pieces.size  # Return end of array (graceful degradation)
        end

        # Find next {% elsif %} / {% else %} / {% end %} at same depth
        private def find_next_branch_or_end(
          pieces : Array(MacroPiece),
          start : Int32,
          end_limit : Int32
        ) : Int32
          depth = 0
          index = start

          while index <= end_limit
            piece = pieces[index]

            case piece.kind
            when .control_start?
              depth += 1

            when .control_end?
              return index if depth == 0
              depth -= 1

            when .control_else_if?, .control_else?
              return index if depth == 0
            end

            index += 1
          end

          return end_limit
        end

        # Evaluate pieces in given range [start, end_index] inclusive
        # Handles nested control flow recursively
        private def evaluate_pieces_range(
          pieces : Array(MacroPiece),
          start : Int32,
          end_index : Int32,
          context : Context
        ) : String
          String.build do |str|
            index = start

            while index <= end_index && index < pieces.size
              piece = pieces[index]

              case piece.kind
              when .text?
                str << piece.text if piece.text
                index += 1

              when .expression?
                if expr_id = piece.expr
                  value = evaluate_expression(expr_id, context)
                  str << value
                end
                index += 1
              when .macro_var?
                if name = piece.macro_var_name
                  value = context.macro_var(name)
                  unless value
                    fresh = fresh_macro_var_name(name)
                    context.set_macro_var(name, fresh)
                    value = fresh
                  end
                  str << value
                end
                index += 1

              when .control_start?
                # Nested control flow
                keyword = piece.control_keyword

                if keyword == "if"
                  output_part, new_index = evaluate_if_block(pieces, index, context)
                  str << output_part
                  index = new_index
                elsif keyword == "for"
                  output_part, new_index = evaluate_for_block(pieces, index, context)
                  str << output_part
                  index = new_index
                else
                  index += 1
                end

              else
                # Skip control flow markers (elsif, else, end)
                index += 1
              end
            end
          end
        end

        # Evaluate {% if %} / {% elsif %} / {% else %} / {% end %} structure
        # Returns {output, next_index} where next_index points AFTER {% end %}
        private def evaluate_if_block(
          pieces : Array(MacroPiece),
          start_index : Int32,
          context : Context
        ) : {String, Int32}
          # Get condition from start piece
          start_piece = pieces[start_index]
          condition_expr = start_piece.expr

          unless condition_expr
            emit_error("Missing condition in {% if %} block")
            end_index = find_matching_end(pieces, start_index)
            return {"", end_index + 1}
          end

          # Evaluate condition
          condition_result = evaluate_condition(condition_expr, context)

          # Find matching end
          end_index = find_matching_end(pieces, start_index)

          if condition_result
            # Condition is true - evaluate if body
            next_branch = find_next_branch_or_end(pieces, start_index + 1, end_index)

            # Evaluate range [start_index+1, next_branch-1]
            output = evaluate_pieces_range(pieces, start_index + 1, next_branch - 1, context)

            return {output, end_index + 1}
          else
            # Condition is false - search for {% elsif %} or {% else %}
            current = start_index + 1

            while current < end_index
              piece = pieces[current]

              if piece.kind.control_else_if?
                # Evaluate elsif condition
                elsif_cond = piece.expr

                if elsif_cond && evaluate_condition(elsif_cond, context)
                  # Found true branch
                  next_branch = find_next_branch_or_end(pieces, current + 1, end_index)
                  output = evaluate_pieces_range(pieces, current + 1, next_branch - 1, context)
                  return {output, end_index + 1}
                end

                current += 1

              elsif piece.kind.control_else?
                # No conditions matched, use else
                output = evaluate_pieces_range(pieces, current + 1, end_index - 1, context)
                return {output, end_index + 1}

              else
                current += 1
              end
            end

            # No branch matched, return empty
            return {"", end_index + 1}
          end
        end

        # Evaluate {% for VAR in ARRAY %} loop
        # Phase 87B-3: ArrayLiteral only (ranges deferred to Phase 87B-4)
        # Extended: supports a simple subset of type-reflection loops such as
        #   {% for ivar, i in @type.instance_vars %}
        # Returns {output, next_index} where next_index points AFTER {% end %}
        private def evaluate_for_block(
          pieces : Array(MacroPiece),
          start_index : Int32,
          context : Context
        ) : {String, Int32}
          # Get loop metadata
          start_piece = pieces[start_index]
          iter_vars = start_piece.iter_vars
          iterable_expr = start_piece.iterable

          unless iter_vars && iterable_expr
            emit_error("Missing loop variable or iterable in {% for %} block")
            end_index = find_matching_end(pieces, start_index)
            return {"", end_index + 1}
          end

          # Support 1 or 2 loop variables: value and optional index
          value_var = iter_vars[0]?
          index_var = iter_vars[1]?
          unless value_var
            emit_error("Missing loop variable in {% for %} block")
            end_index = find_matching_end(pieces, start_index)
            return {"", end_index + 1}
          end

          # Evaluate iterable → get element strings
          iterable_node = @arena[iterable_expr]

          elem_values = case iterable_node
          when Frontend::ArrayLiteralNode
            # Phase 87B-3: Array path
            iterable_node.elements.map { |elem_id| stringify_expr(elem_id) }

          when Frontend::RangeNode
            # Phase 87B-4A: Range path
            expand_range_to_strings(iterable_node)

          when Frontend::MemberAccessNode
            # Minimal support for @type.instance_vars in type-reflection macros
            if context.owner_type && iterable_node.object.is_a?(Frontend::InstanceVarNode)
              ivar_obj = iterable_node.object.as(Frontend::InstanceVarNode)
              name = String.new(ivar_obj.name)
              member = String.new(iterable_node.member)
              if name == "@type" && member == "instance_vars" && context.owner_type
                # Use collected instance variable names from the owning class symbol
                instance_vars = context.owner_type.not_nil!.instance_vars
                instance_vars.keys.sort
              else
                emit_error("Unsupported member access in for-loop iterable: #{name}.#{member}")
                nil
              end
            else
              emit_error("For loop iterable must be Array, Range, or @type.instance_vars (Phase 87B-4A)")
              nil
            end

          else
            emit_error("For loop requires ArrayLiteral or Range (Phase 87B-4A)")
            nil
          end

          # Handle error case
          unless elem_values
            end_index = find_matching_end(pieces, start_index)
            return {"", end_index + 1}
          end

          # Find loop body range
          end_index = find_matching_end(pieces, start_index)
          body_start = start_index + 1
          body_end = end_index - 1

          # Iterate over element values (same for arrays, ranges, and instance_vars)
          output = String.build do |str|
            elem_values.each_with_index do |elem_value, idx|
              loop_context = context.with_variable(value_var, elem_value)
              if index_var
                loop_context = loop_context.with_variable(index_var, idx.to_s)
              end
              body_output = evaluate_pieces_range(pieces, body_start, body_end, loop_context)
              str << body_output
            end
          end

          return {output, end_index + 1}
        end

        # Phase 87B-4A: Expand range to array of string values
        # Returns Array(String) if successful, nil if error (diagnostic emitted)
        private def expand_range_to_strings(range_node : Frontend::RangeNode) : Array(String)?
          range_begin = range_node.begin_expr
          range_end = range_node.end_expr

          # Evaluate bounds to strings (use empty context for literals)
          empty_context = Context.new(flags: @flags)
          start_str = evaluate_expression(range_begin, empty_context)
          end_str = evaluate_expression(range_end, empty_context)

          # Parse to integers
          start_val = start_str.to_i?
          end_val = end_str.to_i?

          unless start_val && end_val
            emit_error("Range bounds must be integers in Phase 87B-4A (got: #{start_str}..#{end_str})")
            return nil
          end

          # Handle reverse ranges (Crystal behavior: empty)
          if start_val > end_val
            return [] of String
          end

          # Calculate size (helpers normalize RangeNode.exclusive semantics)
          exclusive = range_node.exclusive
          size = if exclusive
            end_val - start_val
          else
            end_val - start_val + 1
          end

          # Check size limit (prevent compilation DOS)
          if size > MAX_RANGE_SIZE
            emit_error("Range too large: #{size} elements (max #{MAX_RANGE_SIZE})")
            return nil
          end

          # Generate sequence
          result = [] of String
          current = start_val
          while current < end_val || (!exclusive && current == end_val)
            result << current.to_s
            current += 1
          end

          result
        end

        private def emit_error(message : String, location : ExprId? = nil)
          span = if location
            @arena[location].span
          else
            Frontend::Span.new(0, 0, 1, 1, 1, 1)
          end

          @diagnostics << Diagnostic.new(
            DiagnosticLevel::Error,
            "E4001",  # Macro error codes start at E4xxx
            message,
            span
          )
        end

        private def emit_warning(message : String, location : ExprId? = nil)
          span = if location
            @arena[location].span
          else
            Frontend::Span.new(0, 0, 1, 1, 1, 1)
          end

          @diagnostics << Diagnostic.new(
            DiagnosticLevel::Warning,
            "W4001",  # Macro warning codes
            message,
            span
          )
        end
      end
    end
  end
end
