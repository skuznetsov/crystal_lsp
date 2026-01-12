require "set"
require "../frontend/ast"
require "../frontend/lexer"
require "../frontend/parser"
require "./symbol"
require "./diagnostic"
require "./macro_value"

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
        getter last_output : String? = nil

        def initialize(
          @program : Program,
          @arena : Frontend::ArenaLike,
          flags : Set(String)? = nil,
          *,
          recovery_mode : Bool = false,
          source_provider : Proc(ExprId, String?)? = nil,
          macro_source : String? = nil,
          source_sink : Proc(String, Nil)? = nil
        )
          @diagnostics = [] of Diagnostic
          @depth = 0
          @macro_var_counter = 0
          @flags = flags || Set(String).new
          @recovery_mode = recovery_mode
          @source_provider = source_provider
          @macro_source = macro_source
          @source_sink = source_sink
          @string_pool = @program.string_pool
        end

        # Expansion context for macro evaluation
        class Context
          getter variables : Hash(String, MacroValue)
          getter macro_vars : Hash(String, String)
          # Optional owner type for @type reflection macros
          getter owner_type : ClassSymbol?
          getter depth : Int32
          getter flags : Set(String)
          getter block_id : ExprId?

          def initialize(
            @variables = {} of String => MacroValue,
            @macro_vars = {} of String => String,
            @owner_type : ClassSymbol? = nil,
            @depth = 0,
            @flags = Set(String).new,
            @block_id : ExprId? = nil,
          )
          end

          def with_depth(new_depth : Int32) : Context
            Context.new(@variables, @macro_vars, @owner_type, new_depth, @flags, @block_id)
          end

          def with_variable(name : String, value : MacroValue) : Context
            new_vars = @variables.dup
            new_vars[name] = value
            Context.new(new_vars, @macro_vars, @owner_type, @depth, @flags, @block_id)
          end

          def with_owner_type(owner : ClassSymbol?) : Context
            Context.new(@variables, @macro_vars, owner, @depth, @flags, @block_id)
          end

          def with_block(block_id : ExprId?) : Context
            Context.new(@variables, @macro_vars, @owner_type, @depth, @flags, block_id)
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

          # Get variable as string (for backwards compat in some places)
          def variable_string(name : String) : String
            if v = @variables[name]?
              v.to_macro_output
            else
              ""
            end
          end
        end

        # Main expansion entry point
        #
        # Takes a MacroSymbol and arguments, returns expanded AST node.
        # Optional owner_type is the class in which the macro is expanded and
        # is used to support @type.* reflection macros.
        def expand(macro_symbol : MacroSymbol, args : Array(ExprId), owner_type : ClassSymbol? = nil, *, named_args : Array(Frontend::NamedArgument)? = nil, block_id : ExprId? = nil) : ExprId
          # Clear diagnostics from previous expansions
          @diagnostics.clear
          @last_output = nil

          # Check recursion depth
          if @depth >= MAX_DEPTH
            emit_error("Macro recursion depth exceeded (#{MAX_DEPTH})")
            return ExprId.new(-1)
          end

          @depth += 1
          begin
            # Bind parameters to arguments
            context = build_context(macro_symbol, args, owner_type, named_args, block_id)

            # Evaluate macro body
            output = evaluate_macro_body(macro_symbol.body, context)
            @last_output = output
            if (match = ENV["DEBUG_MACRO_OUTPUT_MATCH"]?) && output.includes?(match)
              STDERR.puts "[MACRO_OUTPUT_MATCH] name=#{macro_symbol.name} match=#{match.inspect} bytes=#{output.bytesize}"
              output.each_line do |line|
                next unless line.includes?(match)
                STDERR.puts "[MACRO_OUTPUT_LINE] #{line.rstrip}"
              end
            end

            # Parse result back to AST
            reparse(output, macro_symbol.node_id)
          ensure
            @depth -= 1
          end
        end

        # Expand a standalone MacroLiteral with a provided variable context.
        # Returns the expanded text (no AST reparse).
        def expand_literal(body_id : ExprId, *, variables : Hash(String, MacroValue), owner_type : ClassSymbol? = nil) : String
          @diagnostics.clear
          @last_output = nil

          if @depth >= MAX_DEPTH
            emit_error("Macro recursion depth exceeded (#{MAX_DEPTH})")
            return ""
          end

          @depth += 1
          begin
            context = Context.new(variables, {} of String => String, owner_type, @depth, @flags, nil)
            output = evaluate_macro_body(body_id, context)
            @last_output = output
            if (match = ENV["DEBUG_MACRO_OUTPUT_MATCH"]?) && output.includes?(match)
              STDERR.puts "[MACRO_OUTPUT_MATCH] literal match=#{match.inspect} bytes=#{output.bytesize}"
              output.each_line do |line|
                next unless line.includes?(match)
                STDERR.puts "[MACRO_OUTPUT_LINE] #{line.rstrip}"
              end
            end
            output
          ensure
            @depth -= 1
          end
        end

        private def build_context(
          macro_symbol : MacroSymbol,
          args : Array(ExprId),
          owner_type : ClassSymbol?,
          named_args : Array(Frontend::NamedArgument)?,
          block_id : ExprId?
        ) : Context
          variables = {} of String => MacroValue
          params = macro_symbol.params || [] of String
          named_values = {} of String => MacroValue

          named_args.try do |list|
            list.each do |named_arg|
              named_values[intern_name(named_arg.name)] = expr_to_macro_value(named_arg.value)
            end
          end

          # Bind parameters to arguments (supports *args and **kwargs).
          arg_index = 0
          params.each do |raw_name|
            if raw_name.starts_with?("**")
              name = raw_name[2..]
              variables[name] = MacroNamedTupleValue.new(named_values)
              named_values.clear
              next
            end

            if raw_name.starts_with?("*")
              name = raw_name[1..]
              values = [] of MacroValue
              while arg_index < args.size
                values << expr_to_macro_value(args[arg_index])
                arg_index += 1
              end
              variables[name] = MacroArrayValue.new(values)
              next
            end

            if value = named_values[raw_name]?
              variables[raw_name] = value
              named_values.delete(raw_name)
            elsif arg_index < args.size
              variables[raw_name] = expr_to_macro_value(args[arg_index])
              arg_index += 1
            else
              variables[raw_name] = MACRO_NIL
            end
          end

          # Provide a truthy `block` variable when macro call includes a block.
          variables["block"] = block_id ? MacroBoolValue.new(true) : MACRO_NIL

          Context.new(variables, {} of String => String, owner_type, @depth, @flags, block_id)
        end

        # Convert expression to string representation (for legacy/simple use)
        private def stringify_expr(expr_id : ExprId) : String
          node = @arena[expr_id]

          case Frontend.node_kind(node)
          when .number?, .string?, .identifier?, .bool?
            Frontend.node_literal_string(node) || ""
          when .nil?
            "nil"
          else
            # Complex expression - return source representation
            Frontend.node_literal_string(node) || ""
          end
        end

        # Convert AST expression to MacroValue (for simple literals only)
        private def expr_to_macro_value(expr_id : ExprId) : MacroValue
          node = @arena[expr_id]

          case Frontend.node_kind(node)
          when .number?
            literal = Frontend.node_literal_string(node) || "0"
            if literal.includes?(".")
              MacroNumberValue.new(literal.to_f64? || 0.0)
            else
              MacroNumberValue.new(literal.to_i64? || 0_i64)
            end
          when .string?
            MacroStringValue.new(Frontend.node_literal_string(node) || "")
          when .symbol?
            MacroSymbolValue.new(Frontend.node_literal_string(node) || "")
          when .char?
            literal = Frontend.node_literal_string(node) || ""
            MacroStringValue.new(literal) # Treat char as string for simplicity
          when .bool?
            literal = Frontend.node_literal_string(node)
            MacroBoolValue.new(literal == "true")
          when .nil?
            MACRO_NIL
          when .identifier?
            MacroIdValue.new(Frontend.node_literal_string(node) || "")
          else
            if node.is_a?(Frontend::TypeDeclarationNode) || node.is_a?(Frontend::AssignNode)
              MacroNodeValue.new(expr_id, @arena, @string_pool)
            elsif node.is_a?(Frontend::PathNode)
              name = path_to_string(node)
              name.empty? ? MacroIdValue.new("") : MacroIdValue.new(name)
            else
              # Complex expressions - return as MacroId
              MacroIdValue.new(Frontend.node_literal_string(node) || "")
            end
          end
        end

        # Evaluate expression and return MacroValue (supports complex expressions)
        # Used for annotation access: {% if ann = @type.annotation(Foo) %}
        private def evaluate_to_macro_value(expr_id : ExprId, context : Context) : MacroValue
          node = @arena[expr_id]

          # Unwrap MacroExpressionNode
          if node.is_a?(Frontend::MacroExpressionNode)
            return evaluate_to_macro_value(node.expression, context)
          end

          if node.is_a?(Frontend::GroupingNode)
            return evaluate_to_macro_value(node.expression, context)
          end

          if node.is_a?(Frontend::MacroVarNode)
            literal = Frontend.node_literal_string(node)
            if literal && (value = context.macro_var(literal))
              return MacroIdValue.new(value)
            end
            return MacroIdValue.new(literal || "")
          end

          case Frontend.node_kind(node)
          when .number?
            literal = Frontend.node_literal_string(node) || "0"
            if literal.includes?(".")
              MacroNumberValue.new(literal.to_f64? || 0.0)
            else
              MacroNumberValue.new(literal.to_i64? || 0_i64)
            end
          when .string?
            MacroStringValue.new(Frontend.node_literal_string(node) || "")
          when .symbol?
            MacroSymbolValue.new(Frontend.node_literal_string(node) || "")
          when .char?
            MacroStringValue.new(Frontend.node_literal_string(node) || "")
          when .bool?
            literal = Frontend.node_literal_string(node)
            MacroBoolValue.new(literal == "true")
          when .nil?
            MACRO_NIL
          when .identifier?
            name = Frontend.node_literal_string(node)
            if name && (macro_val = context.variables[name]?)
              macro_val
            else
              MacroIdValue.new(name || "")
            end
          else
            if node.is_a?(Frontend::StringInterpolationNode)
              return MacroStringValue.new(evaluate_string_interpolation(node, context))
            elsif node.is_a?(Frontend::ArrayLiteralNode)
              values = node.elements.map { |elem| evaluate_to_macro_value(elem, context) }
              return MacroArrayValue.new(values)
            elsif node.is_a?(Frontend::NamedTupleLiteralNode)
              entries = {} of String => MacroValue
              node.entries.each do |entry|
                key = intern_name(entry.key)
                entries[key] = evaluate_to_macro_value(entry.value, context)
              end
              return MacroNamedTupleValue.new(entries)
            elsif node.is_a?(Frontend::BinaryNode)
              return evaluate_binary_to_macro_value(node, context)
            elsif node.is_a?(Frontend::MemberAccessNode)
              return evaluate_member_access_to_macro_value(node, context)
            elsif node.is_a?(Frontend::CallNode)
              return evaluate_call_to_macro_value(node, context)
            elsif node.is_a?(Frontend::YieldNode)
              text = macro_block_text(context)
              return text.empty? ? MACRO_NIL : MacroIdValue.new(text)
            elsif node.is_a?(Frontend::IfNode)
              cond_result, cond_ctx = evaluate_condition(node.condition, context)
              if cond_result
                return evaluate_body_expressions(node.then_body, cond_ctx)
              end
              if elsifs = node.elsifs
                elsifs.each do |elsif_branch|
                  branch_result, branch_ctx = evaluate_condition(elsif_branch.condition, context)
                  if branch_result
                    return evaluate_body_expressions(elsif_branch.body, branch_ctx)
                  end
                end
              end
              if else_body = node.else_body
                return evaluate_body_expressions(else_body, context)
              end
              return MACRO_NIL
            elsif node.is_a?(Frontend::TypeDeclarationNode) || node.is_a?(Frontend::AssignNode)
              return MacroNodeValue.new(expr_id, @arena, @string_pool)
            elsif node.is_a?(Frontend::PathNode)
              name = path_to_string(node)
              return name.empty? ? MACRO_NIL : MacroIdValue.new(name)
            end

            MacroNodeValue.new(expr_id, @arena, @string_pool)
          end
        end

        private def evaluate_string_interpolation(node : Frontend::StringInterpolationNode, context : Context) : String
          source_segments = nil
          if source = @macro_source
            span = node.span
            start = span.start_offset
            length = span.end_offset - span.start_offset
            if length > 2 && start >= 0 && start < source.bytesize
              length = source.bytesize - start if start + length > source.bytesize
              raw = source.byte_slice(start, length)
              if raw.starts_with?('"') && raw.ends_with?('"') && raw.size >= 2
                content = raw[1..-2]
                segments = [] of String
                i = 0
                while i < content.size
                  text_start = i
                  while i < content.size
                    break if i + 1 < content.size && content[i] == '#' && content[i + 1] == '{'
                    i += 1
                  end
                  segments << content[text_start...i]
                  break if i >= content.size
                  i += 2
                  brace_depth = 1
                while i < content.size && brace_depth > 0
                  if content[i] == '{'
                    brace_depth += 1
                  elsif content[i] == '}'
                    brace_depth -= 1
                  end
                  i += 1 if brace_depth > 0
                end
                i += 1 if brace_depth == 0 && i < content.size
              end
                text_piece_count = node.pieces.count { |piece| piece.kind.text? }
                source_segments = segments if segments.size >= text_piece_count
              end
            end
          end

          text_index_start = 0
          if source_segments && (first_piece = node.pieces.first?)
            text_index_start = first_piece.kind.expression? ? 1 : 0
          end
          value = String.build do |io|
            text_index = text_index_start
            node.pieces.each do |piece|
              case piece.kind
              when Frontend::StringPiece::Kind::Text
                text = source_segments ? source_segments[text_index]? : piece.text
                text_index += 1
                io << (text || "")
              when Frontend::StringPiece::Kind::Expression
                if expr_id = piece.expr
                  piece_value = evaluate_to_macro_value(expr_id, context)
                  if piece_value.is_a?(MacroStringValue)
                    io << piece_value.value
                  else
                    io << piece_value.to_id
                  end
                end
              end
            end
          end
          if ENV["DEBUG_MACRO_INTERP"]?
            pieces = node.pieces.map_with_index do |piece, idx|
              case piece.kind
              when Frontend::StringPiece::Kind::Text
                "text#{idx}=#{piece.text.inspect}"
              when Frontend::StringPiece::Kind::Expression
                expr_id = piece.expr
                expr = expr_id ? @arena[expr_id] : nil
                literal = expr ? Frontend.node_literal_string(expr) : nil
                detail = "expr#{idx}=#{expr.class} literal=#{literal.inspect}"
                if expr.is_a?(Frontend::MemberAccessNode)
                  detail += " member=#{intern_name(expr.member)}"
                  obj = @arena[expr.object]
                  detail += " obj=#{obj.class}"
                elsif expr.is_a?(Frontend::CallNode)
                  callee = @arena[expr.callee]
                  detail += " callee=#{callee.class} args=#{expr.args.size}"
                  expr.args.each_with_index do |arg_id, arg_idx|
                    arg_node = @arena[arg_id]
                    detail += " arg#{arg_idx}=#{arg_node.class}"
                    if arg_node.is_a?(Frontend::CallNode)
                      inner_callee = @arena[arg_node.callee]
                      detail += " inner_callee=#{inner_callee.class}"
                      if inner_callee.is_a?(Frontend::MemberAccessNode)
                        detail += " inner_member=#{intern_name(inner_callee.member)}"
                        inner_obj = @arena[inner_callee.object]
                        detail += " inner_obj=#{inner_obj.class}"
                      end
                    elsif arg_node.is_a?(Frontend::MemberAccessNode)
                      detail += " arg_member=#{intern_name(arg_node.member)}"
                    end
                  end
                end
                detail
              else
                "piece#{idx}=unknown"
              end
            end
            span = node.span
            source_slice = nil
            if source = @macro_source
              start = span.start_offset
              length = span.end_offset - span.start_offset
              if length > 0 && start >= 0 && start < source.bytesize
                length = source.bytesize - start if start + length > source.bytesize
                source_slice = source.byte_slice(start, length)
              end
            end
            segments_debug = source_segments ? source_segments.map(&.inspect).join(",") : "nil"
            STDERR.puts "[MACRO_INTERP] span=#{span.start_offset}-#{span.end_offset} src=#{source_slice.inspect} value=#{value.inspect} text_index_start=#{text_index_start} segments=[#{segments_debug}] pieces=#{pieces.join(" | ")}"
          end
          value
        end

        private def evaluate_member_access_to_macro_value(node : Frontend::MemberAccessNode, context : Context) : MacroValue
          obj = @arena[node.object]
          member = intern_name(node.member)

          base_value = if obj.is_a?(Frontend::IdentifierNode)
                         name = Frontend.node_literal_string(obj)
                         name ? context.variables[name]? : nil
                       else
                         evaluate_to_macro_value(node.object, context)
                       end
          if ENV["DEBUG_MACRO_SPLAT"]? && member == "splat"
            STDERR.puts "[MACRO_SPLAT] obj=#{obj.class} base=#{base_value.class_name if base_value}"
          end

          if base_value
            result = base_value.call_method(member, [] of MacroValue, nil)
            return result
          end

          fallback = evaluate_member_access_expression(node, context)
          fallback.empty? ? MACRO_NIL : MacroIdValue.new(fallback)
        end

        private def evaluate_binary_to_macro_value(node : Frontend::BinaryNode, context : Context) : MacroValue
          left = evaluate_to_macro_value(node.left, context)
          right = evaluate_to_macro_value(node.right, context)
          op = String.new(node.operator)

          if ENV["DEBUG_MACRO_ASSIGN"]? && (op == "**" || op == "//")
            STDERR.puts "[MACRO_BIN] op=#{op} left=#{left.class_name} right=#{right.class_name}"
          end

          case op
          when "+"
            if left.is_a?(MacroArrayValue) && right.is_a?(MacroArrayValue)
              return MacroArrayValue.new(left.elements + right.elements)
            end
            if left.is_a?(MacroStringValue) && right.is_a?(MacroStringValue)
              return MacroStringValue.new(left.value + right.value)
            end
          end

          # Delegate numeric operations to MacroNumberValue when possible.
          left.call_method(op, [right], nil)
        end

        # Evaluate call expression and return MacroValue
        # Supports annotation() calls returning MacroAnnotationValue
        private def evaluate_call_to_macro_value(node : Frontend::CallNode, context : Context) : MacroValue
          callee_id = node.callee
          return MACRO_NIL unless callee_id
          callee = @arena[callee_id]
          if ENV["DEBUG_MACRO_CALL"]?
            literal = Frontend.node_literal_string(callee) if callee.is_a?(Frontend::IdentifierNode)
            STDERR.puts "[MACRO_CALL] callee=#{callee.class} literal=#{literal.inspect if literal}"
            if callee.is_a?(Frontend::MemberAccessNode)
              STDERR.puts "[MACRO_CALL] member=#{intern_name(callee.member)}"
            end
          end

          if implicit = unwrap_implicit_receiver_call(node, context)
            receiver, member, args, named_arg_values, block = implicit
            if block
              return evaluate_blocked_macro_call(receiver, member, args, named_arg_values, block, context)
            end
            result = receiver.call_method(member, args, named_arg_values)
            return result unless result.is_a?(MacroNilValue)
          end

          if callee.is_a?(Frontend::MemberAccessNode)
            obj = callee.object
            member = intern_name(callee.member)

            # Handle annotation/annotations returning MacroAnnotationValue
            if member == "annotation" || member == "annotations"
              return evaluate_annotation_to_macro_value(obj, node, member, context)
            end

            # General MacroValue method calls (properties.map, kwargs.empty?, etc.)
            receiver = evaluate_to_macro_value(obj, context)
            if !receiver.is_a?(MacroNilValue)
              args = node.args.map { |arg| evaluate_to_macro_value(arg, context) }
              named_arg_values = node.named_args.try do |named|
                values = {} of String => MacroValue
                named.each do |named_arg|
                  values[intern_name(named_arg.name)] = evaluate_to_macro_value(named_arg.value, context)
                end
                values
              end

              if blk_id = node.block
                blk_node = @arena[blk_id].as(Frontend::BlockNode)
                return evaluate_blocked_macro_call(receiver, member, args, named_arg_values, blk_node, context)
              end

              result = receiver.call_method(member, args, named_arg_values)
              return result unless result.is_a?(MacroNilValue)
            end
          end

          # Fallback: evaluate as string and wrap
          str_val = evaluate_call_expression(node, context)
          if str_val.empty?
            MACRO_NIL
          else
            MacroIdValue.new(str_val)
          end
        end

        private def unwrap_implicit_receiver_call(
          node : Frontend::CallNode,
          context : Context
        ) : {MacroValue, String, Array(MacroValue), Hash(String, MacroValue)?, Frontend::BlockNode?}?
          callee = @arena[node.callee]
          return nil unless callee.is_a?(Frontend::IdentifierNode)
          return nil unless node.args.size == 1

          inner_node = @arena[node.args[0]]
          return nil unless inner_node.is_a?(Frontend::CallNode)

          inner_callee = @arena[inner_node.callee]
          return nil unless inner_callee.is_a?(Frontend::MemberAccessNode)

          inner_obj = @arena[inner_callee.object]
          return nil unless inner_obj.is_a?(Frontend::ImplicitObjNode)

          receiver = evaluate_to_macro_value(node.callee, context)
          member = intern_name(inner_callee.member)
          args = inner_node.args.map { |arg| evaluate_to_macro_value(arg, context) }
          named_arg_values = inner_node.named_args.try do |named|
            values = {} of String => MacroValue
            named.each do |named_arg|
              values[intern_name(named_arg.name)] = evaluate_to_macro_value(named_arg.value, context)
            end
            values
          end
          block_node = inner_node.block.try { |blk| @arena[blk].as(Frontend::BlockNode) }

          {receiver, member, args, named_arg_values, block_node}
        end

        private def evaluate_blocked_macro_call(
          receiver : MacroValue,
          member : String,
          args : Array(MacroValue),
          named_args : Hash(String, MacroValue)?,
          block : Frontend::BlockNode,
          context : Context
        ) : MacroValue
          case member
          when "map", "select", "reject"
            return evaluate_macro_block_map(receiver, member, block, context)
          else
            receiver.call_method(member, args, named_args)
          end
        end

        private def evaluate_macro_block_map(
          receiver : MacroValue,
          member : String,
          block : Frontend::BlockNode,
          context : Context
        ) : MacroValue
          case receiver
          when MacroArrayValue
            results = [] of MacroValue
            receiver.elements.each_with_index do |elem, idx|
              values = [elem] of MacroValue
              if block.params && block.params.not_nil!.size > 1
                values << MacroNumberValue.new(idx.to_i64)
              end
              predicate = evaluate_block_body(block, context, values)
              case member
              when "map"
                results << predicate
              when "select"
                results << elem if predicate.truthy?
              when "reject"
                results << elem unless predicate.truthy?
              end
            end
            if ENV["DEBUG_MACRO_MAP"]? && member == "map"
              mapped = results.map(&.to_id)
              STDERR.puts "[MACRO_MAP] size=#{mapped.size} values=#{mapped.inspect}"
            end
            return MacroArrayValue.new(results)
          when MacroNamedTupleValue
            results = [] of MacroValue
            receiver.entries.each do |key, value|
              values = [MacroIdValue.new(key).as(MacroValue), value]
              predicate = evaluate_block_body(block, context, values)
              case member
              when "map"
                results << predicate
              when "select"
                results << value if predicate.truthy?
              when "reject"
                results << value unless predicate.truthy?
              end
            end
            return MacroArrayValue.new(results)
          else
            return MACRO_NIL
          end
        end

        private def evaluate_block_body(
          block : Frontend::BlockNode,
          context : Context,
          values : Array(MacroValue)
        ) : MacroValue
          scoped = context
          if params = block.params
            params.each_with_index do |param, idx|
              next unless param_name = param.name
              value = values[idx]? || MACRO_NIL
              scoped = scoped.with_variable(intern_name(param_name), value)
            end
          end

          if ENV["DEBUG_MACRO_BLOCK"]?
            names = block.params.try(&.map do |param|
              name = param.name
              case name
              when Slice(UInt8)
                intern_name(name)
              when String
                name
              else
                ""
              end
            end) || [] of String
            value_desc = values.map(&.to_macro_output)
            STDERR.puts "[MACRO_BLOCK] params=#{names.join(",")} values=#{value_desc.inspect}"
            block.body.each_with_index do |expr_id, idx|
              expr = @arena[expr_id]
              literal = Frontend.node_literal_string(expr)
              STDERR.puts "[MACRO_BLOCK] body#{idx}=#{expr.class} literal=#{literal.inspect}"
              if expr.is_a?(Frontend::CallNode)
                callee = @arena[expr.callee]
                callee_lit = Frontend.node_literal_string(callee)
                STDERR.puts "[MACRO_BLOCK]   call callee=#{callee.class} literal=#{callee_lit.inspect} args=#{expr.args.size}"
              elsif expr.is_a?(Frontend::MemberAccessNode)
                obj = @arena[expr.object]
                STDERR.puts "[MACRO_BLOCK]   member=#{intern_name(expr.member)} obj=#{obj.class}"
              end
            end
          end

          result = MACRO_NIL
          block.body.each do |expr_id|
            result = evaluate_to_macro_value(expr_id, scoped)
          end
          result
        end

        private def evaluate_body_expressions(exprs : Array(ExprId), context : Context) : MacroValue
          result = MACRO_NIL
          exprs.each do |expr_id|
            result = evaluate_to_macro_value(expr_id, context)
          end
          result
        end

        private def macro_block_text(context : Context) : String
          return "" unless block_id = context.block_id
          provider = @source_provider
          return "" unless provider

          provider.call(block_id) || ""
        end

        private def macro_piece_text(piece : MacroPiece) : String?
          if source = @macro_source
            if span = piece.span
              start = span.start_offset
              length = span.end_offset - span.start_offset
              if length > 0 && start >= 0 && start < source.bytesize
                if start + length > source.bytesize
                  length = source.bytesize - start
                end
                return source.byte_slice(start, length)
              end
            end
          end
          piece.text
        end

        # Evaluate annotation() call and return MacroAnnotationValue or MacroArrayValue
        private def evaluate_annotation_to_macro_value(
          obj,
          node : Frontend::CallNode,
          member : String,
          context : Context
        ) : MacroValue
          # @type.annotation(Foo)
          if obj.is_a?(Frontend::InstanceVarNode)
            ivar_name = intern_name(obj.name)
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

              if member == "annotation"
                if ann = matching.first?
                  ann_val = MacroAnnotationValue.new(ann)
                  ann_val.arena = @arena
                  return ann_val
                end
                return MACRO_NIL
              else # annotations
                arr = matching.map do |ann|
                  ann_val = MacroAnnotationValue.new(ann)
                  ann_val.arena = @arena
                  ann_val.as(MacroValue)
                end
                return MacroArrayValue.new(arr)
              end
            end
            return MACRO_NIL
          end

          # ivar.annotation(Foo) where ivar is a macro variable
          if obj.is_a?(Frontend::IdentifierNode) && context.owner_type
            id_name = Frontend.node_literal_string(obj) || ""
            macro_val = context.variables[id_name]?

            # If it's a MacroMetaVarValue, use its annotation method
            if macro_val.is_a?(MacroMetaVarValue)
              filter_name = nil
              if arg0_id = node.args[0]?
                filter_name = annotation_type_full_name(arg0_id)
              end
              filter_arg = filter_name ? MacroIdValue.new(filter_name) : nil
              args = filter_arg ? [filter_arg.as(MacroValue)] : [] of MacroValue
              return macro_val.call_method(member, args, nil)
            end

            # Legacy: base_name is ivar name string
            base_name = macro_val ? macro_val.to_macro_output : ""
            if !base_name.empty?
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

              if member == "annotation"
                if ann = matching.first?
                  ann_val = MacroAnnotationValue.new(ann)
                  ann_val.arena = @arena
                  return ann_val
                end
                return MACRO_NIL
              else # annotations
                arr = matching.map do |ann|
                  ann_val = MacroAnnotationValue.new(ann)
                  ann_val.arena = @arena
                  ann_val.as(MacroValue)
                end
                return MacroArrayValue.new(arr)
              end
            end
          end

          MACRO_NIL
        end

        private def evaluate_macro_body(body_id : ExprId, context : Context) : String
          # Get MacroLiteral node (works with typed or legacy nodes)
          body_node = @arena[body_id]
          return "" unless body_node.is_a?(Frontend::MacroLiteralNode)
          pieces = body_node.pieces
          source = @macro_source
          prev_span_end : Int32? = nil

          # Phase 87B-3: Use indexed loop to handle control flow jumps
          String.build do |str|
            index = 0

            while index < pieces.size
              piece = pieces[index]
              span = source ? piece.span : nil
              if source && span && prev_span_end
                gap = span.start_offset - prev_span_end
                if gap > 0 && prev_span_end < source.bytesize
                  length = gap
                  if prev_span_end + length > source.bytesize
                    length = source.bytesize - prev_span_end
                  end
                  gap_slice = source.byte_slice(prev_span_end, length)
                  str << gap_slice if gap_slice.bytes.all? { |byte| byte <= 32 }
                end
              end

              case piece.kind
              when .text?
                # Plain text - append as-is
                if text = macro_piece_text(piece)
                  str << text
                end
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
                # {% if %}, {% for %}, {% begin %} - delegate to specialized handlers
                keyword = piece.control_keyword

                if keyword == "if"
                  output_part, new_index = evaluate_if_block(pieces, index, context)
                  str << output_part
                  index = new_index
                elsif keyword == "for"
                  output_part, new_index = evaluate_for_block(pieces, index, context)
                  str << output_part
                  index = new_index
                elsif keyword == "begin"
                  output_part, new_index = evaluate_begin_block(pieces, index, context)
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
              prev_span_end = span.end_offset if source && span
            end
          end
        end

        private def reparse(output : String, location : ExprId) : ExprId
          @source_sink.try(&.call(output))
          # Create lexer from generated string
          lexer = Frontend::Lexer.new(output)

          # Create parser with existing arena (uses Phase 87B-2 constructor)
          parser = Frontend::Parser.new(lexer, @arena, recovery_mode: @recovery_mode)

          begin
            program = parser.parse_program

            # Check for parse errors
            if parser.diagnostics.any?
              # Emit diagnostic: macro generated invalid syntax
              parse_errors = parser.diagnostics.map(&.message).join("; ")
              emit_error("Macro expansion generated invalid syntax: #{parse_errors}. Generated code: \"#{output}\"", location)
              return ExprId.new(-1)
            end

            roots = program.roots
            if roots.empty?
              ExprId.new(-1)
            elsif roots.size == 1
              roots.first
            else
              block = Frontend::BlockNode.new(Frontend::Span.zero, nil, roots)
              @arena.add_typed(block)
            end
          rescue ex
            # Catch any parser exceptions
            emit_error("Failed to parse macro expansion: #{ex.message}. Generated code: \"#{output}\"", location)
            ExprId.new(-1)
          end
        end

        private def evaluate_expression(expr_id : ExprId, context : Context) : String
          node = @arena[expr_id]

          # Unwrap MacroExpressionNode ({{ expr }}) to get inner expression
          if node.is_a?(Frontend::MacroExpressionNode)
            return evaluate_expression(node.expression, context)
          end

          if node.is_a?(Frontend::GroupingNode)
            return evaluate_expression(node.expression, context)
          end

          # Handle assignment in macro expressions: {% var = expr %}
          if node.is_a?(Frontend::AssignNode)
            target = @arena[node.target]
            if target.is_a?(Frontend::IdentifierNode)
              if name = Frontend.node_literal_string(target)
                context.variables[name] = evaluate_to_macro_value(node.value, context)
                if ENV["DEBUG_MACRO_ASSIGN"]? && name == "bytesize"
                  STDERR.puts "[MACRO_ASSIGN] bytesize=#{context.variables[name].to_macro_output}"
                end
              end
            end
            return ""
          end

          # Complex nodes that benefit from MacroValue evaluation
          if node.is_a?(Frontend::StringInterpolationNode) ||
             node.is_a?(Frontend::BinaryNode) ||
             node.is_a?(Frontend::CallNode)
            return evaluate_to_macro_value(expr_id, context).to_macro_output
          end

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
              if ENV["DEBUG_MACRO_VAR"]? && name == "property"
                if macro_val = context.variables[name]?
                  STDERR.puts "[MACRO_VAR] property type=#{macro_val.class_name} value=#{macro_val.to_macro_output.inspect}"
                else
                  STDERR.puts "[MACRO_VAR] property missing"
                end
              end
              if ENV["DEBUG_MACRO_ASSIGN"]? && name == "bytesize"
                if macro_val = context.variables[name]?
                  STDERR.puts "[MACRO_VAR] bytesize=#{macro_val.to_macro_output}"
                else
                  STDERR.puts "[MACRO_VAR] bytesize missing"
                end
              end
              if macro_val = context.variables[name]?
                macro_val.to_macro_output
              else
                ""
              end
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
            elsif node.is_a?(Frontend::PathNode)
              path_to_string(node)
            elsif node.is_a?(Frontend::IndexNode)
              # Handle ann[:key] index access
              evaluate_index_expression(node, context)
            elsif node.is_a?(Frontend::TypeofNode)
              # typeof(expr) - return type name as string for macro output
              evaluate_typeof_expression(node, context)
            elsif node.is_a?(Frontend::SizeofNode)
              # sizeof(Type) - return size placeholder for macro output
              evaluate_sizeof_expression(node, context)
            elsif node.is_a?(Frontend::AlignofNode)
              # alignof(Type) - return alignment placeholder
              evaluate_alignof_expression(node, context)
            elsif node.is_a?(Frontend::InstanceAlignofNode)
              # instance_alignof(Type) - return instance alignment placeholder
              evaluate_instance_alignof_expression(node, context)
            else
              # Unsupported expression type for Phase 87B-2
              # Return empty string (graceful degradation)
              ""
            end
          end
        end

        # Evaluate index expression: ann[:key] or ann[0]
        private def evaluate_index_expression(node : Frontend::IndexNode, context : Context) : String
          obj = @arena[node.object]

          # Get base object as MacroValue
          base_value : MacroValue? = nil
          if obj.is_a?(Frontend::IdentifierNode)
            name = Frontend.node_literal_string(obj)
            if name
              base_value = context.variables[name]?
            end
          end

          unless base_value
            return ""
          end

          # Get first index (we only support single index for now)
          index_expr_id = node.indexes.first?
          return "" unless index_expr_id

          # Get index as MacroValue
          index_value = expr_to_macro_value(index_expr_id)

          # Call [] method on MacroValue
          result = base_value.call_method("[]", [index_value], nil)
          result.to_macro_output
        end

        # Evaluate instance variable expressions used in macros
        # Currently only supports @type for type-reflection macros.
        private def evaluate_instance_var_expression(node, context : Context) : String
          name_slice = node.name
          name = intern_name(name_slice)
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
        #   ivar.type
        #   ivar.has_default_value?
        private def evaluate_member_access_expression(node, context : Context) : String
          obj = @arena[node.object]
          member = intern_name(node.member)

          # Phase 87B-6: Handle .class_name before evaluating base
          # Need to know the AST node type, not the evaluated value
          if member == "class_name"
            return macro_class_name(obj, context)
          end

          # Handle @type.* type introspection methods directly
          if obj.is_a?(Frontend::InstanceVarNode)
            ivar_name = intern_name(obj.name)
            if ivar_name == "@type" && context.owner_type
              class_symbol = context.owner_type.not_nil!
              case member
              when "class?"
                return class_symbol.is_struct? ? "" : "true"
              when "struct?"
                return class_symbol.is_struct? ? "true" : ""
              when "module?"
                return ""  # ClassSymbol is not a module
              when "abstract?"
                return class_symbol.is_abstract? ? "true" : ""
              when "name"
                # @type.name returns full name with generic parameters by default
                base_name = class_symbol.name
                type_params = class_symbol.type_parameters
                if type_params && !type_params.empty?
                  return "#{base_name}(#{type_params.join(", ")})"
                else
                  return base_name
                end
              when "superclass"
                return class_symbol.superclass_name || ""
              end
            end
          end

          # Resolve base value (string or MacroValue)
          base_value, base_macro_value = case obj
                                         when Frontend::InstanceVarNode
                                           {evaluate_instance_var_expression(obj, context), nil}
                                         when Frontend::IdentifierNode
                                           var_name = Frontend.node_literal_string(obj) || ""
                                           if mv = context.variables[var_name]?
                                             {mv.to_macro_output, mv}
                                           else
                                             {"", nil}
                                           end
                                         when Frontend::CallNode
                                           mv = evaluate_call_to_macro_value(obj, context)
                                           {mv.to_macro_output, mv}
                                         when Frontend::MemberAccessNode
                                           {evaluate_member_access_expression(obj, context), nil}
                                         else
                                           {"", nil}
                                         end

          if base_macro_value
            result = base_macro_value.call_method(member, [] of MacroValue, nil)
            return result.to_macro_output unless result.is_a?(MacroNilValue)
          end

          # For @type.name and chained calls, just return the type name
          if base_value != "" && context.owner_type && member == "name"
            return base_value
          end

          # Phase 87B-6: .stringify - convert to string literal (add quotes)
          if member == "stringify"
            return macro_stringify(base_value)
          end

          # Phase 87B-6: .id - convert to identifier (remove quotes/symbol prefix)
          if member == "id"
            return macro_id(base_value)
          end

          # For .name, just return base value (ivar name or type name)
          if member == "name"
            return base_value
          end

          # Handle ivar metadata access via MacroMetaVarValue
          if base_macro_value.is_a?(MacroMetaVarValue)
            meta_var = base_macro_value.as(MacroMetaVarValue)
            result = meta_var.call_method(member, [] of MacroValue, nil)
            return result.to_macro_output
          end

          # Legacy: Handle ivar metadata access when base_value is an instance var name
          if context.owner_type && obj.is_a?(Frontend::IdentifierNode)
            var_name = Frontend.node_literal_string(obj) || ""
            if mv = context.variables[var_name]?
              ivar_name = mv.to_macro_output
              if !ivar_name.empty?
                class_symbol = context.owner_type.not_nil!
                if ivar_info = class_symbol.get_instance_var_info(ivar_name)
                  case member
                  when "type"
                    return ivar_info.type_annotation || ""
                  when "has_default_value?"
                    return ivar_info.has_default? ? "true" : ""
                  when "default_value"
                    if default_id = ivar_info.default_value
                      return stringify_expr(default_id)
                    end
                    return ""
                  end
                end
              end
            end
          end

          base_value
        end

        # Phase 87B-6: .stringify - wrap value in quotes to make it a string literal
        # "foo".stringify → "\"foo\""
        # foo.stringify → "\"foo\""
        # 42.stringify → "\"42\""
        private def macro_stringify(value : String) : String
          # If already a string literal (starts and ends with quotes), keep as is
          if value.starts_with?('"') && value.ends_with?('"')
            return value
          end
          # Otherwise wrap in quotes
          value.inspect
        end

        # Phase 87B-6: .id - remove quotes/symbol prefix to make identifier
        # "foo".id → foo
        # :foo.id → foo
        # foo.id → foo (no change)
        private def macro_id(value : String) : String
          # Remove string quotes
          if value.starts_with?('"') && value.ends_with?('"') && value.size >= 2
            return value[1..-2]
          end
          # Remove symbol prefix
          if value.starts_with?(':')
            return value[1..]
          end
          # Already an identifier
          value
        end

        # Phase 87B-6: .class_name - return AST node type name
        private def macro_class_name(node, context : Context) : String
          # For macro variables, resolve first
          if node.is_a?(Frontend::IdentifierNode)
            var_name = Frontend.node_literal_string(node) || ""
            if context.variables.has_key?(var_name)
              # This is a macro parameter - we don't track original node type
              # Return generic "MacroId" for now (matches Crystal behavior for resolved params)
              return "\"MacroId\""
            end
          end

          # Return node type name based on AST kind
          type_name = case Frontend.node_kind(node)
                      when .number?     then "NumberLiteral"
                      when .string?     then "StringLiteral"
                      when .symbol?     then "SymbolLiteral"
                      when .char?       then "CharLiteral"
                      when .bool?       then "BoolLiteral"
                      when .nil?        then "NilLiteral"
                      when .identifier? then "MacroId"
                      else                   "ASTNode"
                      end
          "\"#{type_name}\""
        end

        # Evaluate simple call expressions, primarily to support
        #   @type.name.stringify
        #   @type.name(generic_args: false)
        #   @type.size
        #   @type.annotation(Foo)
        #   ivar.id.stringify
        private def evaluate_call_expression(node, context : Context) : String
          callee_id = node.callee
          return "" unless callee_id
          callee = @arena[callee_id]

          # Handle type-reflection and annotation helpers
          if callee.is_a?(Frontend::MemberAccessNode)
            obj = callee.object
            member = intern_name(callee.member)

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
                ivar_name = intern_name(obj.name)
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
                macro_val = context.variables[id_name]?
                base_name = macro_val ? macro_val.to_macro_output : ""
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
              name = intern_name(obj.name)
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
              name = intern_name(obj.name)
              if name == "@type" && context.owner_type
                class_symbol = context.owner_type.as(ClassSymbol)

                case member
                when "name"
                  # @type.name returns the full type name with generic parameters by default.
                  # @type.name(generic_args: false) strips generic parameters.
                  #
                  # Example: for `class Foo(T, U)`:
                  #   @type.name                      → "Foo(T, U)"
                  #   @type.name(generic_args: true)  → "Foo(T, U)"
                  #   @type.name(generic_args: false) → "Foo"
                  include_generic_args = true
                  if named_args = node.named_args
                    named_args.each do |named_arg|
                      arg_name = intern_name(named_arg.name)
                      next unless arg_name == "generic_args"

                      value_str = evaluate_expression(named_arg.value, context)
                      include_generic_args = (value_str != "false")
                    end
                  end

                  base_name = class_symbol.name
                  type_params = class_symbol.type_parameters

                  if include_generic_args && type_params && !type_params.empty?
                    # Construct full name with type parameters: Foo(T, U)
                    return "#{base_name}(#{type_params.join(", ")})"
                  else
                    # Return base name without generic parameters
                    return base_name
                  end
                when "size"
                  # Approximate @type.size as the number of generic type
                  # parameters declared on the owning class.
                  type_params = class_symbol.type_parameters
                  return (type_params ? type_params.size : 0).to_s
                when "methods"
                  # Phase 87B-4B: @type.methods - return list of method names
                  # Returns array-like string for use in for-loops
                  return class_symbol.methods.join(", ")
                when "has_method?"
                  # Phase 87B-4B: @type.has_method?("name") - check if method exists
                  if arg0_id = node.args[0]?
                    method_name = evaluate_expression(arg0_id, context)
                    # Strip quotes if present
                    if method_name.starts_with?('"') && method_name.ends_with?('"')
                      method_name = method_name[1..-2]
                    end
                    return class_symbol.has_method?(method_name) ? "true" : ""
                  end
                  return ""
                when "superclass"
                  # Phase 87B-4B: @type.superclass - return superclass name
                  if sc = class_symbol.superclass_name
                    return sc
                  end
                  return ""
                when "class?"
                  # @type.class? - true if this is a class (not struct/module/enum)
                  return class_symbol.is_struct? ? "" : "true"
                when "struct?"
                  # @type.struct? - true if this is a struct
                  return class_symbol.is_struct? ? "true" : ""
                when "module?"
                  # @type.module? - false for ClassSymbol (would be true for ModuleSymbol)
                  return ""
                when "abstract?"
                  # @type.abstract? - check if class is abstract
                  # TODO: track abstract flag in ClassSymbol
                  return ""
                end
              end
            end

            # Handle ivar.type.nilable?() - check if type is nilable
            if member == "nilable?"
              # Check if obj is itself a member access (ivar.type)
              if obj.is_a?(Frontend::MemberAccessNode)
                inner_obj = @arena[obj.object]
                inner_member = intern_name(obj.member)
                if inner_member == "type" && inner_obj.is_a?(Frontend::IdentifierNode)
                  var_name = Frontend.node_literal_string(inner_obj) || ""
                  if macro_val = context.variables[var_name]?
                    ivar_name = macro_val.to_macro_output
                    if !ivar_name.empty? && context.owner_type
                      class_symbol = context.owner_type.not_nil!
                      if ivar_info = class_symbol.get_instance_var_info(ivar_name)
                        return ivar_info.nilable? ? "true" : ""
                      end
                    end
                  end
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

        private def evaluate_flag_call(node : Frontend::CallNode, context : Context) : String
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
            intern_name(node.name)
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
        # Returns {Bool, Context} where Context may have new bindings from assignment
        # CRITICAL: false/nil/Nop are falsy. For other expressions we fall back
        # to evaluating them to a string and treating empty string, "false" and
        # "nil" as falsy; everything else is truthy. This keeps semantics close
        # to Crystal while allowing graceful degradation when we cannot fully
        # interpret an expression.
        private def evaluate_condition(expr_id : ExprId, context : Context) : {Bool, Context}
          node = @arena[expr_id]

          # Handle assignment in condition: {% if ann = expr %}
          # This binds the variable and checks truthiness of the assigned value
          if node.is_a?(Frontend::AssignNode)
            target = @arena[node.target]
            if target.is_a?(Frontend::IdentifierNode)
              var_name = Frontend.node_literal_string(target)
              if var_name
                # Evaluate value as MacroValue
                value = evaluate_to_macro_value(node.value, context)
                # Bind variable in new context
                new_context = context.with_variable(var_name, value)
                # Return truthiness and new context
                return {value.truthy?, new_context}
              end
            end
          end

          if node.is_a?(Frontend::IsANode)
            receiver = evaluate_to_macro_value(node.expression, context)
            type_name = intern_name(node.target_type)
            result = receiver.call_method("is_a?", [MacroIdValue.new(type_name)], nil)
            if ENV["DEBUG_MACRO_COND"]?
              STDERR.puts "[MACRO_COND] is_a? recv=#{receiver.class_name} expected=#{type_name} -> #{result.truthy?}"
            end
            return {result.truthy?, context}
          end

          # Handle boolean connectives && / || at the AST level so we can
          # compose more precise conditions from simpler ones.
          if node.is_a?(Frontend::BinaryNode)
            op = String.new(node.operator)

            case op
            when "&&"
              left_result, left_ctx = evaluate_condition(node.left, context)
              return {false, context} unless left_result
              return evaluate_condition(node.right, left_ctx)
            when "||"
              left_result, left_ctx = evaluate_condition(node.left, context)
              return {true, left_ctx} if left_result
              return evaluate_condition(node.right, context)
            else
              # Lightweight numeric comparison support (i > 0, i == 0, etc.).
              left_val = evaluate_expression(node.left, context)
              right_val = evaluate_expression(node.right, context)

              if (left_int = left_val.to_i?) && (right_int = right_val.to_i?)
                result = case op
                         when ">"  then left_int > right_int
                         when ">=" then left_int >= right_int
                         when "<"  then left_int < right_int
                         when "<=" then left_int <= right_int
                         when "==" then left_int == right_int
                         when "!=" then left_int != right_int
                         else           false
                         end
                return {result, context}
              end
            end
          end

          if ENV["DEBUG_MACRO_CALL_COND"]? && node.is_a?(Frontend::CallNode)
            callee = @arena[node.callee]
            callee_literal = Frontend.node_literal_string(callee)
            STDERR.puts "[MACRO_COND] call callee=#{callee.class} literal=#{callee_literal.inspect} args=#{node.args.size}"
            node.args.each_with_index do |arg, idx|
              arg_node = @arena[arg]
              STDERR.puts "[MACRO_COND]   arg#{idx}=#{arg_node.class} literal=#{Frontend.node_literal_string(arg_node).inspect}"
              if arg_node.is_a?(Frontend::CallNode)
                inner_callee = @arena[arg_node.callee]
                inner_lit = Frontend.node_literal_string(inner_callee)
                STDERR.puts "[MACRO_COND]     inner callee=#{inner_callee.class} literal=#{inner_lit.inspect} args=#{arg_node.args.size}"
                if inner_callee.is_a?(Frontend::MemberAccessNode)
                  STDERR.puts "[MACRO_COND]     inner member=#{intern_name(inner_callee.member)}"
                  inner_obj = @arena[inner_callee.object]
                  STDERR.puts "[MACRO_COND]     inner object=#{inner_obj.class} literal=#{Frontend.node_literal_string(inner_obj).inspect}"
                end
              end
            end
          end

          # Treat identifier conditions based on the bound macro variable value,
          # so constructs like `if ann` or `if flag` behave reasonably.
          if node.is_a?(Frontend::IdentifierNode)
            name = Frontend.node_literal_string(node)
            if name && (macro_val = context.variables[name]?)
              # Use MacroValue.truthy? for proper Crystal truthiness
              return {macro_val.truthy?, context}
            else
              # Unbound macro variable is treated as falsy.
              return {false, context}
            end
          end

          case Frontend.node_kind(node)
          when .bool?
            # Parse "true" or "false"
            literal = Frontend.node_literal_string(node)
            if literal
              return {false, context} if literal == "false"
              return {true, context} # "true"
            end
            # Default to true if it's a bool node without literal (shouldn't happen)
            return {true, context}
          when .nil?
            # nil is falsy
            return {false, context}
          else
            # Fallback: evaluate expression to a string and apply Crystal-like
            # truthiness. Unsupported expressions tend to evaluate to empty
            # string, which we treat as falsy, avoiding spurious "true".
            value = evaluate_expression(expr_id, context)
            result = !(value.empty? || value == "false" || value == "nil")
            return {result, context}
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
          return pieces.size # Return end of array (graceful degradation)
        end

        # Find next {% elsif %} / {% else %} / {% end %} at same depth
        private def find_next_branch_or_end(
          pieces : Array(MacroPiece),
          start : Int32,
          end_limit : Int32,
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
          context : Context,
        ) : String
          String.build do |str|
            index = start
            source = @macro_source
            prev_span_end : Int32? = nil

            while index <= end_index && index < pieces.size
              piece = pieces[index]
              span = source ? piece.span : nil
              if source && span && prev_span_end
                gap = span.start_offset - prev_span_end
                if gap > 0 && prev_span_end < source.bytesize
                  length = gap
                  if prev_span_end + length > source.bytesize
                    length = source.bytesize - prev_span_end
                  end
                  gap_slice = source.byte_slice(prev_span_end, length)
                  str << gap_slice if gap_slice.bytes.all? { |byte| byte <= 32 }
                end
              end

              case piece.kind
              when .text?
                if text = macro_piece_text(piece)
                  str << text
                end
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
                elsif keyword == "begin"
                  output_part, new_index = evaluate_begin_block(pieces, index, context)
                  str << output_part
                  index = new_index
                else
                  index += 1
                end
              else
                # Skip control flow markers (elsif, else, end)
                index += 1
              end
              prev_span_end = span.end_offset if source && span
            end
          end
        end

        # Evaluate {% begin %} ... {% end %} block
        # Returns {output, next_index} where next_index points AFTER {% end %}
        private def evaluate_begin_block(
          pieces : Array(MacroPiece),
          start_index : Int32,
          context : Context,
        ) : {String, Int32}
          end_index = find_matching_end(pieces, start_index)
          output = evaluate_pieces_range(pieces, start_index + 1, end_index - 1, context)
          {output, end_index + 1}
        end

        # Evaluate {% if %} / {% elsif %} / {% else %} / {% end %} structure
        # Returns {output, next_index} where next_index points AFTER {% end %}
        private def evaluate_if_block(
          pieces : Array(MacroPiece),
          start_index : Int32,
          context : Context,
        ) : {String, Int32}
          # Get condition from start piece
          start_piece = pieces[start_index]
          condition_expr = start_piece.expr

          unless condition_expr
            emit_error("Missing condition in {% if %} block")
            end_index = find_matching_end(pieces, start_index)
            return {"", end_index + 1}
          end

          # Evaluate condition (may bind new variables via assignment)
          condition_result, body_context = evaluate_condition(condition_expr, context)

          # Find matching end
          end_index = find_matching_end(pieces, start_index)

          if condition_result
            # Condition is true - evaluate if body with potentially updated context
            next_branch = find_next_branch_or_end(pieces, start_index + 1, end_index)

            # Evaluate range [start_index+1, next_branch-1] with body_context
            output = evaluate_pieces_range(pieces, start_index + 1, next_branch - 1, body_context)

            return {output, end_index + 1}
          else
            # Condition is false - search for {% elsif %} or {% else %}
            current = start_index + 1

            while current < end_index
              piece = pieces[current]

              if piece.kind.control_else_if?
                # Evaluate elsif condition
                elsif_cond = piece.expr

                if elsif_cond
                  elsif_result, elsif_ctx = evaluate_condition(elsif_cond, context)
                  if elsif_result
                    # Found true branch
                    next_branch = find_next_branch_or_end(pieces, current + 1, end_index)
                    output = evaluate_pieces_range(pieces, current + 1, next_branch - 1, elsif_ctx)
                    return {output, end_index + 1}
                  end
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
          context : Context,
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

          # Evaluate iterable → get MacroValue elements
          iterable_node = @arena[iterable_expr]

          elem_values = evaluate_iterable_to_macro_values(iterable_node, context)

          # Handle error case
          unless elem_values
            end_index = find_matching_end(pieces, start_index)
            return {"", end_index + 1}
          end

          # Find loop body range
          end_index = find_matching_end(pieces, start_index)
          body_start = start_index + 1
          body_end = end_index - 1

          # Iterate over MacroValue elements
          if ENV["DEBUG_MACRO_FOR"]? && value_var == "property"
            elem_values.each_with_index do |elem_value, idx|
              STDERR.puts "[MACRO_FOR] var=#{value_var} idx=#{idx} type=#{elem_value.class_name} value=#{elem_value.to_macro_output.inspect}"
            end
          end
          output = String.build do |str|
            elem_values.each_with_index do |elem_value, idx|
              loop_context = context.with_variable(value_var, elem_value)
              if index_var
                loop_context = loop_context.with_variable(index_var, MacroNumberValue.new(idx.to_i64))
              end
              body_output = evaluate_pieces_range(pieces, body_start, body_end, loop_context)
              str << body_output
            end
          end

          return {output, end_index + 1}
        end

        # Evaluate an iterable expression to an array of MacroValue
        private def evaluate_iterable_to_macro_values(iterable_node, context : Context) : Array(MacroValue)?
          case iterable_node
          when Frontend::ArrayLiteralNode
            # Array literal: [1, 2, 3]
            iterable_node.elements.map { |elem_id| evaluate_to_macro_value(elem_id, context) }
          when Frontend::RangeNode
            # Range: 1..10
            expand_range_to_macro_values(iterable_node)
          when Frontend::IdentifierNode
            name = Frontend.node_literal_string(iterable_node)
            if name && (macro_val = context.variables[name]?)
              if macro_val.is_a?(MacroArrayValue)
                macro_val.elements
              else
                emit_error("For loop iterable #{name} is not an ArrayLiteral")
                nil
              end
            else
              emit_error("For loop requires ArrayLiteral, Range, or @type.instance_vars/@type.methods")
              nil
            end
          when Frontend::MemberAccessNode
            # Support for @type.instance_vars and @type.methods
            evaluate_member_iterable(iterable_node, context)
          else
            emit_error("For loop requires ArrayLiteral, Range, or @type.instance_vars/@type.methods")
            nil
          end
        end

        # Evaluate @type.instance_vars or @type.methods to array of MacroValue
        private def evaluate_member_iterable(node : Frontend::MemberAccessNode, context : Context) : Array(MacroValue)?
          obj = node.object
          unless obj.is_a?(Frontend::InstanceVarNode)
            emit_error("For loop member access must be on @type")
            return nil
          end

          ivar_name = intern_name(obj.name)
          unless ivar_name == "@type" && context.owner_type
            emit_error("For loop member access requires @type with owner context")
            return nil
          end

          class_symbol = context.owner_type.not_nil!
          member = intern_name(node.member)

          case member
          when "instance_vars"
            # Return MacroMetaVarValue for each instance variable
            class_symbol.instance_var_infos.map do |ivar_name, info|
              MacroMetaVarValue.new(ivar_name, info, class_symbol).as(MacroValue)
            end
          when "methods"
            # Return MacroIdValue for each method name
            class_symbol.methods.map { |m| MacroIdValue.new(m).as(MacroValue) }
          else
            emit_error("Unsupported @type member in for-loop: #{member}")
            nil
          end
        end

        # Expand range to array of MacroNumberValue
        private def expand_range_to_macro_values(range_node : Frontend::RangeNode) : Array(MacroValue)?
          range_begin = range_node.begin_expr
          range_end = range_node.end_expr

          # Evaluate bounds
          empty_context = Context.new(flags: @flags)
          start_str = evaluate_expression(range_begin, empty_context)
          end_str = evaluate_expression(range_end, empty_context)

          start_val = start_str.to_i?
          end_val = end_str.to_i?

          unless start_val && end_val
            emit_error("Range bounds must be integers (got: #{start_str}..#{end_str})")
            return nil
          end

          # Handle reverse ranges (Crystal behavior: empty)
          if start_val > end_val
            return [] of MacroValue
          end

          exclusive = range_node.exclusive
          size = exclusive ? (end_val - start_val) : (end_val - start_val + 1)

          if size > MAX_RANGE_SIZE
            emit_error("Range too large: #{size} elements (max #{MAX_RANGE_SIZE})")
            return nil
          end

          result = [] of MacroValue
          current = start_val
          while current < end_val || (!exclusive && current == end_val)
            result << MacroNumberValue.new(current.to_i64)
            current += 1
          end

          result
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
            "E4001", # Macro error codes start at E4xxx
            message,
            span
          )
        end

        # Evaluate typeof(expr) - returns the type of an expression as a string
        # In macro context, we attempt to infer the type. For complex cases,
        # we emit a placeholder that the user can replace.
        private def evaluate_typeof_expression(node : Frontend::TypeofNode, context : Context) : String
          return "" if node.args.empty?

          # Get the first argument expression
          arg_id = node.args.first
          arg_node = @arena[arg_id]

          # Try to infer the type from the expression
          inferred_type = infer_expression_type(arg_node, context)

          if inferred_type
            # Return the inferred type name
            inferred_type
          else
            # Return typeof(...) as a placeholder - will be resolved at compile time
            "typeof(...)"
          end
        end

        # Evaluate sizeof(Type) - returns the size of a type
        # In macro context, we can return known sizes for primitive types,
        # or emit a placeholder for complex types.
        private def evaluate_sizeof_expression(node : Frontend::SizeofNode, context : Context) : String
          return "0" if node.args.empty?

          # Get the type argument
          arg_id = node.args.first
          arg_node = @arena[arg_id]

          # Try to get the type name
          type_name = get_type_name_from_node(arg_node)

          if type_name
            # Return known sizes for primitive types
            size = primitive_type_size(type_name)
            return size.to_s if size
          end

          # For unknown types, return placeholder
          "sizeof(...)"
        end

        # Evaluate alignof(Type) - returns the alignment of a type
        private def evaluate_alignof_expression(node : Frontend::AlignofNode, context : Context) : String
          return "0" if node.args.empty?

          # Get the type argument
          arg_id = node.args.first
          arg_node = @arena[arg_id]

          # Try to get the type name
          type_name = get_type_name_from_node(arg_node)

          if type_name
            # Return known alignments for primitive types (typically same as size for primitives)
            align = primitive_type_alignment(type_name)
            return align.to_s if align
          end

          # For unknown types, return placeholder
          "alignof(...)"
        end

        # Evaluate instance_alignof(Type) - returns alignment for instance of class type
        private def evaluate_instance_alignof_expression(node : Frontend::InstanceAlignofNode, context : Context) : String
          return "0" if node.args.empty?

          # Get the type argument
          arg_id = node.args.first
          arg_node = @arena[arg_id]

          # Try to get the type name
          type_name = get_type_name_from_node(arg_node)

          # Instance alignment is typically pointer alignment (8 bytes on 64-bit)
          if type_name
            # For class instances, return pointer alignment
            "8"
          else
            "instance_alignof(...)"
          end
        end

        # Attempt to infer the type of an expression for typeof()
        private def infer_expression_type(node, context : Context) : String?
          case node
          when Frontend::NumberNode
            # Infer numeric type from literal
            literal = Frontend.node_literal_string(node)
            return nil unless literal

            if literal.includes?(".")
              "Float64"
            elsif literal.ends_with?("_i8") || literal.ends_with?("i8")
              "Int8"
            elsif literal.ends_with?("_i16") || literal.ends_with?("i16")
              "Int16"
            elsif literal.ends_with?("_i32") || literal.ends_with?("i32")
              "Int32"
            elsif literal.ends_with?("_i64") || literal.ends_with?("i64")
              "Int64"
            elsif literal.ends_with?("_u8") || literal.ends_with?("u8")
              "UInt8"
            elsif literal.ends_with?("_u16") || literal.ends_with?("u16")
              "UInt16"
            elsif literal.ends_with?("_u32") || literal.ends_with?("u32")
              "UInt32"
            elsif literal.ends_with?("_u64") || literal.ends_with?("u64")
              "UInt64"
            elsif literal.ends_with?("_f32") || literal.ends_with?("f32")
              "Float32"
            elsif literal.ends_with?("_f64") || literal.ends_with?("f64")
              "Float64"
            else
              "Int32" # Default integer type
            end
          when Frontend::StringNode
            "String"
          when Frontend::SymbolNode
            "Symbol"
          when Frontend::BoolNode
            "Bool"
          when Frontend::NilNode
            "Nil"
          when Frontend::CharNode
            "Char"
          when Frontend::ArrayLiteralNode
            "Array"
          when Frontend::HashLiteralNode
            "Hash"
          when Frontend::TupleLiteralNode
            "Tuple"
          when Frontend::RangeNode
            "Range"
          when Frontend::RegexNode
            "Regex"
          when Frontend::IdentifierNode
            # Look up variable type from context
            name = Frontend.node_literal_string(node)
            if name && (macro_val = context.variables[name]?)
              macro_val.type_name
            else
              nil
            end
          else
            nil
          end
        end

        # Get type name from a node (for sizeof/alignof)
        private def get_type_name_from_node(node) : String?
          case node
          when Frontend::IdentifierNode
            Frontend.node_literal_string(node)
          when Frontend::PathNode
            path_to_string(node)
          else
            nil
          end
        end

        # Return known sizes for primitive types (in bytes)
        private def primitive_type_size(type_name : String) : Int32?
          case type_name
          when "Int8", "UInt8", "Bool"
            1
          when "Int16", "UInt16"
            2
          when "Int32", "UInt32", "Float32", "Char"
            4
          when "Int64", "UInt64", "Float64"
            8
          when "Int128", "UInt128"
            16
          when "Pointer", "Reference", "String", "Array", "Hash", "Class"
            8 # Pointer size on 64-bit
          else
            nil
          end
        end

        # Return known alignments for primitive types
        private def primitive_type_alignment(type_name : String) : Int32?
          # For primitive types, alignment typically equals size (up to pointer size)
          size = primitive_type_size(type_name)
          return nil unless size
          {size, 8}.min # Max alignment is pointer size
        end

        private def emit_warning(message : String, location : ExprId? = nil)
          span = if location
                   @arena[location].span
                 else
                   Frontend::Span.new(0, 0, 1, 1, 1, 1)
                 end

          @diagnostics << Diagnostic.new(
            DiagnosticLevel::Warning,
            "W4001", # Macro warning codes
            message,
            span
          )
        end

        private def intern_name(slice : Slice(UInt8)) : String
          @string_pool.intern_string(slice)
        end
      end
    end
  end
end
