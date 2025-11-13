require "./ast"
require "./lexer"
require "./lexer/token"
require "./parser/diagnostic"
require "./small_vec"
require "./watchdog"

module CrystalV2
  module Compiler
    module Frontend
      class Parser
        PREFIX_ERROR = ExprId.new(-1)
        UNARY_PRECEDENCE = 30

        @macro_terminator : Symbol?
        @previous_token : Token?
        @temp_var_counter : Int32  # Phase 101: for generating temp variable names in block shorthand
        # Phase 103: Delimiter depth tracking for multi-line expressions
        @paren_depth : Int32
        @bracket_depth : Int32
        @brace_depth : Int32
        # Phase 103: Type declaration control flag (like original Crystal parser)
        # When > 0, disables type annotation parsing (e.g., in ternary operator ? :)
        @no_type_declaration : Int32
        @string_pool : StringPool  # Week 1 Day 2: for interning generic type annotations
        @debug_enabled : Bool  # Debug output control
        @parsing_call_args : Int32  # Prevent nested calls without parens during argument parsing
        @macro_mode : Int32
        @in_macro_expression : Bool  # Flag to track parsing inside {% ... %} expressions
        # Streaming tokenization support
        @streaming : Bool
        @lexer : Lexer?
        @keep_trivia : Bool
        @expect_context : String?
        # Parser context flags
        @parsing_method_params : Bool

        def initialize(lexer : Lexer)
          @tokens = [] of Token
          @index = 0
          # Choose arena implementation (default: AstArena; PageArena via env)
          if ENV["CRYSTAL_V2_PAGE_ARENA"]?
            @arena = PageArena.new
          else
            @arena = AstArena.new
          end
          @diagnostics = [] of Diagnostic
          @macro_terminator = nil
          @previous_token = nil
          @temp_var_counter = 0
          @paren_depth = 0
          @bracket_depth = 0
          @brace_depth = 0
          @no_type_declaration = 0  # Phase 103: Type annotations enabled by default
          @string_pool = lexer.string_pool  # Week 1 Day 2: share string pool for deduplication
          @debug_enabled = ENV["PARSER_DEBUG"]? == "1"  # Enable debug via PARSER_DEBUG=1
          @parsing_call_args = 0  # Not parsing call args initially
          @macro_mode = 0
          @in_macro_expression = false  # Not in macro expression initially
          @streaming = ENV["CRYSTAL_V2_PARSER_STREAM"]? != nil
          @expect_context = nil
          @parsing_method_params = false
          if @streaming
            @lexer = lexer
            @keep_trivia = ENV["CRYSTAL_V2_PARSER_KEEP_TRIVIA"]? != nil
          else
            keep_trivia = ENV["CRYSTAL_V2_PARSER_KEEP_TRIVIA"]? != nil
            lexer.each_token(skip_trivia: !keep_trivia) { |token| @tokens << token }
            @lexer = nil
            @keep_trivia = keep_trivia
            # Pre-size arena capacity heuristically based on token count to reduce reallocations
            token_count = @tokens.size
            # Heuristic: approx half token count, clamped
            capacity = token_count // 2
            if capacity < 512
              capacity = 512
            elsif capacity > 32768
              capacity = 32768
            end
            # Pre-size AstArena capacity heuristically; skip when using PageArena
            unless ENV["CRYSTAL_V2_PAGE_ARENA"]?
              @arena = AstArena.new(capacity)
            end
          end
        end

        # Recognize accessor-like macros where arguments may be type declarations
        private def accessor_macro_callee?(callee_token : Token) : Bool
          return false unless callee_token.kind == Token::Kind::Identifier
          slice = callee_token.slice
          # Allow optional '?' suffix for predicate accessors (e.g., property?)
          base = slice
          if base.size > 0 && base[base.size - 1] == '?'.ord.to_u8
            base = Slice.new(base.to_unsafe, base.size - 1)
          end
          slice_eq?(base, "getter") || slice_eq?(base, "setter") || slice_eq?(base, "property") ||
            slice_eq?(base, "class_getter") || slice_eq?(base, "class_setter") || slice_eq?(base, "class_property")
        end

        # Recognize macros that accept typed field lists: name : Type [= value]
        private def typed_macro_args_callee?(callee_token : Token) : Bool
          return false unless callee_token.kind == Token::Kind::Identifier
          slice = callee_token.slice
          accessor_macro_callee?(callee_token) || slice_eq?(slice, "record")
        end

        private def parse_block_body_with_optional_rescue : Tuple(Array(ExprId), Array(RescueClause)?, Array(ExprId)?)
          body_ids_b = SmallVec(ExprId, 4).new
          loop do
            consume_newlines
            token = current_token
            break if token.kind == Token::Kind::Rescue || token.kind == Token::Kind::Ensure || token.kind == Token::Kind::End
            break if token.kind == Token::Kind::EOF
            break if macro_terminator_reached?(token)  # Phase 103K: Stop at macro terminators

            expr = parse_statement
            body_ids_b << expr unless expr.invalid?
            consume_newlines
          end

          rescue_clauses = nil
          ensure_body = nil

          if current_token.kind == Token::Kind::Rescue || current_token.kind == Token::Kind::Ensure
            rescue_clauses, ensure_body = parse_rescue_sections
          end

          {body_ids_b.to_a, rescue_clauses, ensure_body}
        end

        # Phase 87B-2: Constructor for reparsing with existing arena
        # Used by macro expander to add parsed nodes to existing arena
        def initialize(lexer : Lexer, @arena : ArenaLike)
          @tokens = [] of Token
          @index = 0
          @diagnostics = [] of Diagnostic
          @macro_terminator = nil
          @previous_token = nil
          @temp_var_counter = 0
          @paren_depth = 0
          @bracket_depth = 0
          @brace_depth = 0
          @no_type_declaration = 0  # Phase 103: Type annotations enabled by default
          @string_pool = lexer.string_pool  # Week 1 Day 2: share string pool for deduplication
          @debug_enabled = ENV["PARSER_DEBUG"]? == "1"  # Enable debug via PARSER_DEBUG=1
          @parsing_call_args = 0  # Not parsing call args initially
          @macro_mode = 0
          @in_macro_expression = false  # Not in macro expression initially
          @streaming = ENV["CRYSTAL_V2_PARSER_STREAM"]? != nil
          @expect_context = nil
          @parsing_method_params = false
          if @streaming
            @lexer = lexer
            @keep_trivia = ENV["CRYSTAL_V2_PARSER_KEEP_TRIVIA"]? != nil
          else
            keep_trivia = ENV["CRYSTAL_V2_PARSER_KEEP_TRIVIA"]? != nil
            lexer.each_token(skip_trivia: !keep_trivia) { |token| @tokens << token }
            @lexer = nil
            @keep_trivia = keep_trivia
          end
        end

        def parse_program : Program
          # Use SmallVec to reduce heap churn when collecting roots
          roots_builder = SmallVec(ExprId, 64).new
          while current_token.kind != Token::Kind::EOF
            skip_statement_end
            break if current_token.kind == Token::Kind::EOF

            if macro_definition_start?
              macro_def = parse_macro_definition
              roots_builder << macro_def unless macro_def.invalid?
              consume_newlines
              next
            end

          # Phase 103J: Check for macro control ({% if %}, {% for %}, etc.)
          debug("parse_program: current=#{current_token.kind}, checking macro_control_start?")
          if macro_control_start?
            # Special-case top-level skip_file: {% skip_file ... %}
            if roots_builder.size == 0
              if keyword = peek_macro_keyword
                if keyword == "skip_file"
                  # Consume control start
                  control_span = consume_macro_control_start
                  if macro_trim_token?(current_token)
                    advance
                  end
                  skip_trivia
                  # consume keyword 'skip_file'
                  advance
                  skip_trivia
                  # consume closing %}
                  consume_macro_close_span("Expected '%}' after skip_file")
                  # Short-circuit: ignore rest of file for LSP parsing
                  break
                end
              end
            end
            debug("parse_program: macro_control_start? returned true, calling parse_percent_macro_control")
            macro_ctrl = parse_percent_macro_control
            roots_builder << macro_ctrl unless macro_ctrl.invalid?
            consume_newlines
            next
          end
            debug("parse_program: macro_control_start? returned false, proceeding normally")

            # Fast path: handle top-level definitions directly (robust against stray separators)
            token = current_token
            case token.kind
            when Token::Kind::Def      then node = parse_def
            when Token::Kind::Macro    then node = parse_macro_definition
            when Token::Kind::Class    then node = parse_class
            when Token::Kind::Module   then node = parse_module
            when Token::Kind::Struct   then node = parse_struct
            when Token::Kind::Union    then node = parse_union
            when Token::Kind::Enum     then node = parse_enum
            when Token::Kind::Alias    then node = parse_alias
            when Token::Kind::Annotation then node = parse_annotation_def
            when Token::Kind::Abstract then node = parse_abstract
            when Token::Kind::Private  then node = parse_private
            when Token::Kind::Protected then node = parse_protected
            when Token::Kind::Lib      then node = parse_lib
            when Token::Kind::Fun      then node = parse_fun
            else
              node = nil
            end

            if node
              roots_builder << node unless node.invalid?
              skip_statement_end
              next
            end

            if definition_start?
              node = case current_token.kind
                when Token::Kind::Def
                  parse_def
                when Token::Kind::Fun
                  parse_fun
                when Token::Kind::Class
                  parse_class
                when Token::Kind::Module
                  parse_module
                when Token::Kind::Struct
                  parse_struct
                when Token::Kind::Union
                  parse_union
                when Token::Kind::Enum
                  parse_enum
                when Token::Kind::Alias
                  parse_alias
                when Token::Kind::Annotation
                  # Phase 92: annotation definition
                  parse_annotation_def
                when Token::Kind::Abstract
                  # Phase 36: abstract class/def
                  parse_abstract
                when Token::Kind::Private
                  # Phase 37: private def
                  parse_private
                when Token::Kind::Protected
                  # Phase 37: protected def
                  parse_protected
                when Token::Kind::Lib
                  # Phase 38: lib (C bindings)
                  parse_lib
                else
                  PREFIX_ERROR
                end
              roots_builder << node unless node.invalid?
              skip_statement_end
              next
            end

            expr = parse_statement
            roots_builder << expr unless expr.invalid?
            skip_statement_end
          end
          Program.new(@arena, roots_builder.to_a)
        end

        # Parse a statement (assignment or expression)
        private def parse_statement : ExprId
          # Normalize leading trivia at statement start
          skip_trivia
          # Phase 103J: Check for macro control ({% if %}, {% for %}, etc.)
          debug("parse_statement: current=#{current_token.kind}, checking macro_control_start?")
          if macro_control_start?
            debug("parse_statement: macro_control_start? returned true, calling parse_percent_macro_control")
            return parse_percent_macro_control
          end
          debug("parse_statement: macro_control_start? returned false, proceeding normally")

          # Annotate expectation context for better diagnostics control
          @expect_context = "statement"

          # Treat a leading semicolon as a pure statement separator and skip it.
          if current_token.kind == Token::Kind::Semicolon
            advance
            skip_statement_end
            return parse_statement
          end

          # Robustness: if a stray closing parenthesis appears at statement start,
          # advance past it to recover and continue parsing. This avoids emitting
          # a spurious 'unexpected RParen' when previous constructs already
          # consumed the corresponding opening parenthesis (e.g., complex proc
          # types in method headers).
          if current_token.kind == Token::Kind::RParen
            advance
            # Allow newline after '=' in assignments
            skip_statement_end
            return parse_statement
          end

          # Check for definition keywords (def, class, etc.)
          # These can appear in blocks that get yielded to macros (like record)
          if definition_start?
            node = case current_token.kind
              when Token::Kind::Def
                parse_def
              when Token::Kind::Macro
                parse_macro_definition
              when Token::Kind::Class
                parse_class
              when Token::Kind::Module
                parse_module
              when Token::Kind::Struct
                parse_struct
              when Token::Kind::Union
                parse_union
              when Token::Kind::Enum
                parse_enum
              when Token::Kind::Alias
                parse_alias
              when Token::Kind::Annotation
                parse_annotation
              when Token::Kind::Abstract
                parse_abstract
              when Token::Kind::Private
                parse_private
              when Token::Kind::Protected
                parse_protected
              when Token::Kind::Lib
                parse_lib
              when Token::Kind::Fun
                parse_fun
              else
                PREFIX_ERROR
              end
            return node
          end
          # Phase 6: Check for return statement
          if current_token.kind == Token::Kind::Return
            stmt = parse_return
            return parse_postfix_if_modifier(stmt)
          end

          # Phase 29: Check for raise statement
          if current_token.kind == Token::Kind::Raise
            stmt = parse_raise
            return parse_postfix_if_modifier(stmt)
          end

          # Phase 10: yield statements
          if current_token.kind == Token::Kind::Yield
            stmt = parse_yield
            return parse_postfix_if_modifier(stmt)
          end

          # Phase 39: super as statement (removed)
          # Upstream Crystal treats `super` as an expression, which allows
          # constructs like `super || fallback` to parse as a binary
          # expression. Special-casing `super` here short-circuits statement
          # parsing and prevents the binary/infix layer from seeing the
          # following operator, leading to diagnostics such as
          # "unexpected OrOr". We therefore do not handle `super` in
          # parse_statement and let it flow through the regular
          # expression/infix pipeline via `parse_op_assign` -> `parse_expression`.
          # Postfix modifiers (if/unless) are still handled in `parse_prefix`.

          # Phase 96: previous_def statements
          if current_token.kind == Token::Kind::PreviousDef
            stmt = parse_previous_def
            return parse_postfix_if_modifier(stmt)
          end

          # Phase 12: break statements
          if current_token.kind == Token::Kind::Break
            stmt = parse_break
            return parse_postfix_if_modifier(stmt)
          end

          # Phase 12: next statements
          if current_token.kind == Token::Kind::Next
            stmt = parse_next
            return parse_postfix_if_modifier(stmt)
          end

          # Phase 31: include statements
          if current_token.kind == Token::Kind::Include
            return parse_include
          end

          # Phase 31: extend statements
          if current_token.kind == Token::Kind::Extend
            return parse_extend
          end

          # Fast-path: tuple/hash literal at statement start
          if current_token.kind == Token::Kind::LBrace
            nxt = peek_token
            unless nxt.kind == Token::Kind::Percent || nxt.kind == Token::Kind::LBrace
              # Not a macro; try tolerant tuple first, then full disambiguation
              saved_index = @index
              saved_prev = @previous_token
              saved_brace = @brace_depth
              tuple_try = parse_brace_tuple_fallback
              unless tuple_try.invalid?
                return tuple_try
              end
              @index = saved_index
              @previous_token = saved_prev
              @brace_depth = saved_brace
              return parse_hash_or_tuple
            end
          end

          # Parse expression or assignment (like original Crystal's parse_op_assign)
          # This handles: assignments, multiple assignments, type declarations
          left = parse_op_assign
          return PREFIX_ERROR if left.invalid?

          skip_whitespace_and_optional_newlines
          token = current_token

          # Phase 73: Check for multiple assignment: a, b = ...
          if token.kind == Token::Kind::Comma
            # Parse remaining targets
            targets = [left]
            loop do
              advance  # consume comma
              skip_trivia

              target = parse_expression(0)
              return PREFIX_ERROR if target.invalid?
              targets << target

              skip_trivia
              break unless current_token.kind == Token::Kind::Comma
            end

            # Expect =
            unless current_token.kind == Token::Kind::Eq
              emit_unexpected(current_token)
              return PREFIX_ERROR
            end
            advance  # consume =
            skip_trivia

            # Parse right side (tuple literal or comma-separated expressions)
            first_value = parse_expression(0)
            return PREFIX_ERROR if first_value.invalid?
            skip_trivia

            # Check if right side has multiple values (implicit tuple)
            value = if current_token.kind == Token::Kind::Comma
              # Multiple values: 1, 2, 3 → create implicit tuple
              values = [first_value]
              loop do
                advance  # consume comma
                skip_trivia

                val = parse_expression(0)
                return PREFIX_ERROR if val.invalid?
                values << val

                skip_trivia
                break unless current_token.kind == Token::Kind::Comma
              end

              # Create implicit TupleLiteral node
              first_val_span = @arena[values[0]].span
              last_val_span = @arena[values.last].span
              tuple_span = first_val_span.cover(last_val_span)

              @arena.add_typed(TupleLiteralNode.new(
                tuple_span,
                values
              ))
            else
              # Single value or explicit tuple
              first_value
            end

            # Calculate span
            first_target_span = @arena[targets[0]].span
            value_span = @arena[value].span
            multi_assign_span = first_target_span.cover(value_span)

            # Create MultipleAssign node
            return @arena.add_typed(MultipleAssignNode.new(
              multi_assign_span,
              targets,
              value
            ))
          end

          # Phase 66/77: Check for type declaration (Global variables only)
          # Phase 103: Identifier type declarations moved to parse_prefix
          # Note: @instance_var and @@class_var type declarations remain here
          if operator_token?(token, Token::Kind::Colon)
            left_node = @arena[left]
            # Phase 77: Global variable declaration: $var : Type
            if Frontend.node_kind(left_node) == Frontend::NodeKind::Global
              advance  # consume ':'
              skip_trivia

              # Phase 103: Parse type annotation (supports complex types)
              type_annotation = parse_type_annotation
              if type_annotation.empty?
                emit_unexpected(current_token)
                return PREFIX_ERROR
              end

              # Check if followed by = (that would be type-annotated assignment, handle differently)
              if current_token.kind == Token::Kind::Eq
                # For now, emit error (will handle type-annotated assignment separately)
                emit_unexpected(current_token)
                return PREFIX_ERROR
              end

              # It's a global variable declaration: $var : Type
              decl_span = left_node.span.cover(previous_token.not_nil!.span)
              return @arena.add_typed(
                GlobalVarDeclNode.new(
                  decl_span,
                  Frontend.node_literal(left_node).not_nil!,        # $var
                  type_annotation  # Phase 103: Already Slice(UInt8) from parse_type_annotation
                )
              )
            end
          end

          # Check for assignment: identifier = value or compound assignment (+=, -=, etc.)
          if token.kind == Token::Kind::Eq ||
             token.kind == Token::Kind::PlusEq ||
             token.kind == Token::Kind::MinusEq ||
             token.kind == Token::Kind::StarEq ||
             token.kind == Token::Kind::SlashEq ||
             token.kind == Token::Kind::FloorDivEq ||
             token.kind == Token::Kind::PercentEq ||
             token.kind == Token::Kind::StarStarEq ||
             token.kind == Token::Kind::OrOrEq ||
             token.kind == Token::Kind::AndAndEq ||
             token.kind == Token::Kind::AmpEq ||
             token.kind == Token::Kind::AmpPlusEq ||      # Phase 89
             token.kind == Token::Kind::AmpMinusEq ||     # Phase 89
             token.kind == Token::Kind::AmpStarEq ||      # Phase 89
             token.kind == Token::Kind::AmpStarStarEq ||  # Phase 89
             token.kind == Token::Kind::PipeEq ||
             token.kind == Token::Kind::CaretEq ||
             token.kind == Token::Kind::LShiftEq ||
             token.kind == Token::Kind::RShiftEq ||
             token.kind == Token::Kind::NilCoalesceEq  # Phase 82
            # Phase 35: Check if this is a constant declaration (uppercase identifier + =)
            left_node = @arena[left]
            if token.kind == Token::Kind::Eq &&
               Frontend.node_kind(left_node) == Frontend::NodeKind::Identifier &&
               Frontend.node_literal(left_node) && is_constant_name?(Frontend.node_literal(left_node).not_nil!)
              # This is a constant declaration
              advance  # Skip =
              skip_trivia
              value_expr = parse_expression(0)
              return PREFIX_ERROR if value_expr.invalid?

              constant_span = left_node.span.cover(@arena[value_expr].span)
              return @arena.add_typed(
                ConstantNode.new(
                  constant_span,
                  Frontend.node_literal(left_node).not_nil!,
                  value_expr
                )
              )
            end

            # Verify left side is valid assignment target
            # Phase 14B: hash/array assignment (Index)
            # Phase PERCENT_LITERALS: property assignment (MemberAccess)
            left_kind = Frontend.node_kind(left_node)
            unless left_kind == Frontend::NodeKind::Identifier ||
                   left_kind == Frontend::NodeKind::InstanceVar ||
                   left_kind == Frontend::NodeKind::ClassVar ||
                   left_kind == Frontend::NodeKind::Global ||
                   left_kind == Frontend::NodeKind::Index ||
                   left_kind == Frontend::NodeKind::MemberAccess  # Phase PERCENT_LITERALS: property assignment (obj.prop = value)
              @diagnostics << Diagnostic.new("Assignment target must be an identifier, instance variable, class variable, global variable, or index expression", token.span)
              return PREFIX_ERROR
            end

            # Consume assignment token
            assign_token = token
            is_compound = assign_token.kind != Token::Kind::Eq
            advance
            # Allow newline after '=' in assignments
            skip_statement_end

            # Parse right-hand side expression
            rhs = parse_expression(0)
            return PREFIX_ERROR if rhs.invalid?

            # Phase 20/51/52/89: Desugar compound assignment
            # x += 5  =>  x = x + 5
            # x ||= 5 =>  x = x || 5
            # x &= 3  =>  x = x & 3
            # x &+= 5 =>  x = x &+ 5  # Phase 89: wrapping
            value = if is_compound
              # Map compound token to operator
              operator = case assign_token.kind
              when Token::Kind::PlusEq     then "+"
              when Token::Kind::MinusEq    then "-"
              when Token::Kind::StarEq     then "*"
              when Token::Kind::SlashEq    then "/"
              when Token::Kind::FloorDivEq then "//"  # Phase 78
              when Token::Kind::PercentEq  then "%"
              when Token::Kind::StarStarEq then "**"
              when Token::Kind::OrOrEq     then "||"  # Phase 51
              when Token::Kind::AndAndEq   then "&&"  # Phase 51
              when Token::Kind::AmpEq      then "&"   # Phase 52
              when Token::Kind::AmpPlusEq  then "&+"  # Phase 89
              when Token::Kind::AmpMinusEq then "&-"  # Phase 89
              when Token::Kind::AmpStarEq  then "&*"  # Phase 89
              when Token::Kind::AmpStarStarEq then "&**"  # Phase 89
              when Token::Kind::PipeEq     then "|"   # Phase 52
              when Token::Kind::CaretEq    then "^"   # Phase 52
              when Token::Kind::LShiftEq   then "<<"  # Phase 52
              when Token::Kind::RShiftEq   then ">>"  # Phase 52
              when Token::Kind::NilCoalesceEq then "??"  # Phase 82
              else
                ""
              end

              # Create binary expression: left op rhs
              # Use left node's span for the cloned left reference
              rhs_span = node_span(rhs)
              binary_span = left_node.span.cover(rhs_span)

              @arena.add_typed(
                BinaryNode.new(
                  binary_span,
                  operator.to_slice,
                  left,
                  rhs
                )
              )
            else
              # Regular assignment: just use rhs
              rhs
            end

            # Phase PERCENT_LITERALS: Handle property assignment (obj.prop = value → obj.prop=(value))
            # Phase 14B: Handle index assignment (obj[args] = value → obj.[]=(args..., value))
            if left_kind == Frontend::NodeKind::MemberAccess
              # Property assignment: transform to setter call
              # obj.prop = value → obj.prop=(value)
              member_node = left_node.as(MemberAccessNode)

              # Create setter method name by appending "="
              setter_name = String.build do |io|
                io.write(member_node.member)
                io << "="
              end
              setter_slice = @string_pool.intern(setter_name.to_slice)

              # Create setter member access node: obj.prop=
              setter_member = @arena.add_typed(MemberAccessNode.new(
                member_node.span,
                member_node.object,  # same receiver
                setter_slice          # property + "="
              ))

              # Create call node with setter as callee and value as argument
              value_span = node_span(value)
              assign_span = left_node.span.cover(value_span)

              stmt = @arena.add_typed(CallNode.new(
                assign_span,
                setter_member,  # callee: obj.prop=
                [value],        # args: value
                nil,            # no block
                nil             # no named args
              ))
            elsif left_kind == Frontend::NodeKind::Index
              # Index assignment: transform to call of []=
              # obj[args] = value → obj.[]=(args..., value)
              target = Frontend.node_left(left_node).not_nil!
              index_args = Frontend.node_args(left_node).not_nil!

              setter_slice = @string_pool.intern("[]=".to_slice)
              setter_member = @arena.add_typed(MemberAccessNode.new(
                left_node.span,
                target,
                setter_slice
              ))

              value_span = node_span(value)
              assign_span = left_node.span.cover(value_span)

              call_args = index_args + [value]
              stmt = @arena.add_typed(CallNode.new(
                assign_span,
                setter_member,
                call_args,
                nil,
                nil
              ))
            else
              # Regular assignment
              value_span = node_span(value)
              assign_span = left_node.span.cover(value_span)

              stmt = @arena.add_typed(AssignNode.new(
                assign_span,
                left,
                value
              ))
            end
            return parse_postfix_if_modifier(stmt)
          end

          # Not an assignment, check for postfix if
          parse_postfix_if_modifier(left)
        end

        def arena
          @arena
        end

        def diagnostics
          @diagnostics
        end

        # Phase 103: Inline hot path - called thousands of times
        @[AlwaysInline]
        private def current_token
          ensure_token(@index)
          @tokens[@index]
        end

        @[AlwaysInline]
        private def previous_token
          @previous_token
        end

        # Phase 103: Inline hot path - called after every token
        @[AlwaysInline]
        private def advance
          Watchdog.check!
          @previous_token = current_token
          if @streaming
            @index += 1
            ensure_token(@index)
          else
            @index += 1 if @index < @tokens.size - 1
          end
        end

        # Safely step back one token without breaking invariants
        private def unadvance
          unadvance(1)
        end

        # Rewind N tokens safely (used by lookahead backtracking)
        private def unadvance(n : Int32)
          return if n <= 0
          # Saturating rewind: clamp to 0 and clear previous_token
          target = @index - n
          @index = target >= 0 ? target : 0
          @previous_token = nil
        end

        # Phase 101: Generate temporary variable name for block shorthand
        # Returns "__arg0", "__arg1", etc.
        private def temp_var_name : String
          name = "__arg#{@temp_var_counter}"
          @temp_var_counter += 1
          name
        end

        # Phase 103: Inline hot path - called after every token advance
        @[AlwaysInline]
        private def skip_trivia
          loop do
            case current_token.kind
            when Token::Kind::Whitespace, Token::Kind::Comment
              advance
            when Token::Kind::Newline
              # Skip newlines in macro contexts OR when parsing macro expressions
              # This matches original parser's behavior: skip_space_or_newline inside macro expressions
              if macro_context? || @in_macro_expression
                advance
              else
                break
              end
            else
              break
            end
          end
        end

        # Skip statement end: whitespace, comments, newlines, and semicolons
        # Following original parser's skip_statement_end pattern
        private def skip_statement_end
          loop do
            case current_token.kind
            when Token::Kind::Whitespace, Token::Kind::Comment, Token::Kind::Newline, Token::Kind::Semicolon
              advance
            else
              break
            end
          end
        end

        # Ensure token at index exists (fill from lexer if streaming)
        private def ensure_token(idx : Int32)
          return if idx < @tokens.size
          return unless @streaming
          # Pull tokens until we reach idx or EOF
          while @tokens.size <= idx
            tok = @lexer.not_nil!.next_token
            if !@keep_trivia && (tok.kind == Token::Kind::Whitespace || tok.kind == Token::Kind::Comment)
              next
            end
            @tokens << tok
            break if tok.kind == Token::Kind::EOF
          end
        end

        # Lookahead: check if a colon follows on the same line AND is separated by space
        # Used to distinguish "x : Type" (type restriction) from "x: value" (named arg)
        # Returns true if colon is present with at least one space before it on the same line.
        # Does NOT consume any tokens.
        private def next_comes_colon_space? : Bool
          saved_pos = @index
          current = @tokens[@index]
          i = @index + 1
          # Skip comments/whitespace if present (compat mode), but not newlines
          while i < @tokens.size && (@tokens[i].kind == Token::Kind::Whitespace || @tokens[i].kind == Token::Kind::Comment)
            i += 1
          end
          result = false
          if i < @tokens.size && @tokens[i].kind == Token::Kind::Colon
            colon_tok = @tokens[i]
            if colon_tok.span.start_line == current.span.end_line
              # Require at least one space between identifier and colon
              result = colon_tok.span.start_column > current.span.end_column
            end
          end
          # Restore position
          @index = saved_pos
          result
        end

        private def consume_newlines
          loop do
            case current_token.kind
            when Token::Kind::Newline
              advance
            when Token::Kind::Whitespace, Token::Kind::Comment
              advance
            else
              break
            end
          end
        end

        # Phase 103: Multi-line expression support
        # Check if we're inside delimiters (parentheses, brackets, or braces)
        private def inside_delimiters? : Bool
          @paren_depth > 0 || @bracket_depth > 0 || @brace_depth > 0
        end

        # Phase 103: Skip whitespace/comments, and optionally newlines if inside delimiters
        # This allows multi-line expressions like:
        #   foo(
        #     arg1,
        #     arg2
        #   )
        private def skip_whitespace_and_optional_newlines
          loop do
            case current_token.kind
            when Token::Kind::Whitespace, Token::Kind::Comment
              advance
            when Token::Kind::Newline
              if inside_delimiters?
                advance  # Skip newlines inside delimiters
              else
                break  # Newline is statement separator outside delimiters
              end
            else
              break
            end
          end
        end

        # Parse annotation: @[Name] or @[Name(args)] or @[Name(key: value)]
        # Examples: @[Link], @[JSON::Field(key: "test")]
        # Returns: ExprId for AnnotationNode
        #
        # Note: Unlike original Crystal parser which has .op_at_lsquare? token,
        # we have separate @ (Operator) and [ (LBracket) tokens
        private def parse_annotation : ExprId
          start_token = current_token

          # Consume @ operator
          unless current_token.kind == Token::Kind::Operator && slice_eq?(current_token.slice, "@")
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end
          advance  # consume @

          skip_trivia  # Skip whitespace between @ and [

          # Expect [
          unless current_token.kind == Token::Kind::LBracket
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end
          advance  # consume [
          @bracket_depth += 1

          skip_whitespace_and_optional_newlines  # Allow newlines after [

          # Parse annotation name (identifier or path like JSON::Field)
          name_expr = parse_path_or_identifier

          skip_whitespace_and_optional_newlines

          # Parse optional arguments
          args_b = SmallVec(ExprId, 4).new
          named_b = nil.as(SmallVec(NamedArgument, 2)?)

          if current_token.kind == Token::Kind::LParen
            advance  # consume (
            @paren_depth += 1
            skip_whitespace_and_optional_newlines

            # Parse arguments until )
            until current_token.kind == Token::Kind::RParen || current_token.kind == Token::Kind::EOF
              # Check if this is a named argument (identifier followed by colon)
              if current_token.kind == Token::Kind::Identifier
                # Peek ahead to see if there's a colon
                next_idx = @index + 1
                if next_idx < @tokens.size && @tokens[next_idx].kind == Token::Kind::Colon
                  # This is a named argument
                  named_b ||= SmallVec(NamedArgument, 2).new

                  # Get identifier name (zero-copy slice)
                  name_token = current_token
                  name_slice = name_token.slice
                  name_span = name_token.span
                  advance  # consume identifier

                  # Consume colon
                  advance  # consume ':'
                  skip_whitespace_and_optional_newlines

                  # Parse value expression
                  value_expr = parse_expression(0)
                  if value_expr.invalid?
                    return PREFIX_ERROR
                  end

                  value_span = @arena[value_expr].span

                  named_b.not_nil! << NamedArgument.new(name_slice, value_expr, name_span, value_span)
                else
                  # Regular positional argument
                  args_b << parse_expression(0)
                end
              else
                # Regular positional argument
                args_b << parse_expression(0)
              end

              skip_whitespace_and_optional_newlines

              if current_token.kind == Token::Kind::Comma
                advance  # consume comma
                skip_whitespace_and_optional_newlines
              elsif current_token.kind != Token::Kind::RParen
                break  # Stop if not comma and not closing paren
              end
            end

            unless current_token.kind == Token::Kind::RParen
              emit_unexpected(current_token)
              return PREFIX_ERROR
            end
            advance  # consume )
            @paren_depth -= 1

            skip_trivia  # Skip whitespace between ) and ]
          end

          # Expect ]
          unless current_token.kind == Token::Kind::RBracket
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end
          end_token = current_token
          advance  # consume ]
          @bracket_depth -= 1

          # Create AnnotationNode
          span = start_token.span.cover(end_token.span)
          args = args_b.to_a
          named_args = named_b && !named_b.empty? ? named_b.to_a : nil
          node = AnnotationNode.new(span, name_expr, args, named_args)
          @arena.add(node)
        end

        # Helper: parse identifier or path (e.g., Link or JSON::Field)
        private def parse_path_or_identifier : ExprId
          unless current_token.kind == Token::Kind::Identifier
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end

          start_token = current_token
          name_slice = start_token.slice
          span = start_token.span
          node = IdentifierNode.new(span, @string_pool.intern(name_slice))
          left = @arena.add(node)
          advance

          # Allow whitespace before :: segments (e.g., Foo :: Bar)
          skip_whitespace_and_optional_newlines

          # Handle :: for paths like JSON::Field or A::B::C
          while current_token.kind == Token::Kind::ColonColon
            left = parse_path(left)
            skip_trivia
          end

          left
        end

        # Phase 103: Parse type annotation (supports namespaces, generics, unions, suffixes)
        # Examples: Int32, Token::Kind, Array(Int32), Int32 | String, Int32?
        # Returns: Slice from source covering the entire type (zero-copy!)
        private def parse_type_annotation : Slice(UInt8)
          start_token = current_token
          last_type_token = start_token
          paren_depth = 0
          bracket_depth = 0
          brace_depth = 0

          loop do
            token = current_token

            # Stop conditions (when not inside parentheses/brackets)
            if paren_depth == 0 && bracket_depth == 0 && brace_depth == 0
              break if token.kind == Token::Kind::Eq
              break if token.kind == Token::Kind::Comma
              break if operator_token?(token, Token::Kind::RParen)
              break if token.kind == Token::Kind::Arrow
              break if token.kind == Token::Kind::Newline
              break if token.kind == Token::Kind::EOF
              # Stop before universal quantification tail in method headers: "forall U, V"
              if token.kind == Token::Kind::Identifier && slice_eq?(token.slice, "forall")
                break
              end
            end

            # Track parenthesis depth (for generics like Array(Int32))
            if operator_token?(token, Token::Kind::LParen)
              paren_depth += 1
            elsif operator_token?(token, Token::Kind::RParen)
              break if paren_depth == 0  # Closing paren of parameter list
              paren_depth -= 1
            end

            # Track bracket depth (for static arrays like Int32[10])
            if operator_token?(token, Token::Kind::LBracket)
              bracket_depth += 1
            elsif operator_token?(token, Token::Kind::RBracket)
              bracket_depth -= 1
            end

            # Track brace depth (tuple/named tuple type literals like {Int32, String})
            if operator_token?(token, Token::Kind::LBrace)
              # In type annotation context, '{' starts a tuple/named tuple type
              brace_depth += 1
            elsif operator_token?(token, Token::Kind::RBrace)
              # Only break if this '}' would close outer context (shouldn't happen inside type)
              if brace_depth > 0
                brace_depth -= 1
              else
                break
              end
            end

            # Check if this token is part of type annotation
            is_type_token = case token.kind
            when Token::Kind::Identifier, Token::Kind::Number,
                 Token::Kind::ColonColon, Token::Kind::Operator,
                 Token::Kind::ThinArrow, Token::Kind::Self,  # Phase 103C: self as type
                 Token::Kind::Typeof,                         # typeof(...) in type position
                 Token::Kind::Pipe,  # Phase 103I: union types (String | Nil)
                 Token::Kind::LParen, Token::Kind::RParen,  # Phase 30: generics like Array(Int32)
                 Token::Kind::Comma,  # Phase 30: multiple generic params Hash(K, V)
                 Token::Kind::LBracket, Token::Kind::RBracket,  # Static arrays Type[N]
                 Token::Kind::LBrace, Token::Kind::RBrace,  # Tuple/named tuple literals {A, B} / {k: V}
                 Token::Kind::Colon,  # Named tuple key separator in type context
                 Token::Kind::Question,  # Nullable types Type?
                 Token::Kind::Star, Token::Kind::StarStar  # Pointer suffixes Type*, Type**
              true
            when Token::Kind::Class
              # Allow suffix ".class" in type contexts like is_a?(Alone.class)
              last_type_token.kind == Token::Kind::Operator && slice_eq?(last_type_token.slice, ".")
            when Token::Kind::Whitespace
              # Skip whitespace but continue parsing
              advance
              next
            else
              # Unknown token in type context
              false
            end

            break unless is_type_token

            last_type_token = token
            advance
          end

          # Return slice from start to end of type annotation (zero-copy!)
          # This includes whitespace between tokens, which is fine
          start_ptr = start_token.slice.to_unsafe
          end_ptr = last_type_token.slice.to_unsafe + last_type_token.slice.size
          Slice.new(start_ptr, end_ptr - start_ptr)
        end

        # Phase 103: Parse type declaration from identifier: x : Type = value
        # Called from parse_prefix when identifier followed by " : " (space + colon)
        private def parse_type_declaration_from_identifier(identifier_token : Token) : ExprId
          var_span = identifier_token.span

          # Create var node (Identifier)
          var = @arena.add_typed(IdentifierNode.new(var_span, @string_pool.intern(identifier_token.slice)))

          # Skip whitespace and consume ':'
          skip_whitespace_and_optional_newlines
          unless current_token.kind == Token::Kind::Colon
            @diagnostics << Diagnostic.new("Expected ':' in type declaration", current_token.span)
            return PREFIX_ERROR
          end
          advance  # consume ':'
          skip_whitespace_and_optional_newlines

          # Parse type annotation (supports namespaces, generics, suffixes)
          type_start_token = current_token
          declared_type_slice = parse_type_annotation
          type_end_token = previous_token

          skip_trivia

          # Check for optional '= value'
          value = nil
          if current_token.kind == Token::Kind::Eq
            advance  # consume '='
            skip_whitespace_and_optional_newlines
            value = parse_expression(0)
            return PREFIX_ERROR if value.invalid?
          end

          # Create TypeDeclarationNode
          # Extract name from var node (should be Identifier)
          var_node = @arena[var]
          var_literal = Frontend.node_literal(var_node)
          unless var_literal
            @diagnostics << Diagnostic.new("Type declaration variable must be a simple identifier", var_span)
            return PREFIX_ERROR
          end

          type_span = type_end_token ? type_start_token.span.cover(type_end_token.span) : type_start_token.span
          value_span = value ? @arena[value].span : type_span
          full_span = var_span.cover(value_span)

          @arena.add_typed(TypeDeclarationNode.new(
            full_span,
            var_literal,
            declared_type_slice,
            value
          ))
        end

        # Parse type suffixes: ?, *, **, []
        # These are postfix operators in type context (not infix like ternary)
        # Returns the modified type expression with suffixes applied
        private def parse_type_suffix(type : ExprId) : ExprId
          loop do
            case current_token.kind
            when Token::Kind::Question
              # Nilable type: Type?
              # Represented as UnaryNode with operator "?"
              start_span = @arena[type].span
              advance  # consume '?'
              end_span = @tokens[@index - 1].span
              full_span = start_span.cover(end_span)

              operator = "?".to_slice
              type = @arena.add_typed(UnaryNode.new(full_span, operator, type))
            when Token::Kind::Star
              # Pointer type: Type*
              start_span = @arena[type].span
              advance  # consume '*'
              end_span = @tokens[@index - 1].span
              full_span = start_span.cover(end_span)

              operator = "*".to_slice
              type = @arena.add_typed(UnaryNode.new(full_span, operator, type))
            when Token::Kind::StarStar
              # Double pointer: Type**
              start_span = @arena[type].span
              advance  # consume '**'
              end_span = @tokens[@index - 1].span
              full_span = start_span.cover(end_span)

              operator = "**".to_slice
              type = @arena.add_typed(UnaryNode.new(full_span, operator, type))
            when Token::Kind::LBracket
              # Static array: Type[N]
              # For now, skip implementation - would need to parse size expression
              # and create appropriate node
              break
            else
              # No more suffixes
              break
            end
          end

          type
        end

        # Phase 100: macro upgraded from identifier to keyword
        private def macro_definition_start?
          current_token.kind == Token::Kind::Macro
        end

        # Phase 100: Added Macro to definition_start?
        private def definition_start?
          token = current_token

          # Phase 103F: Distinguish 'def method' from 'def : Type' (identifier usage)
          # Method definition: def name, def self.name, def []
          # Identifier usage: def : Type (no name between def and :)
          if token.kind == Token::Kind::Def
            # Check what comes after 'def' keyword
            next_tok = peek_next_non_trivia
            # If directly followed by : (no identifier), it's identifier usage: getter def : Type
            # Otherwise it's method definition: def foo : Type
            return false if next_tok.kind == Token::Kind::Colon
          end

          token.kind == Token::Kind::Def || token.kind == Token::Kind::Macro || token.kind == Token::Kind::Class || token.kind == Token::Kind::Module || token.kind == Token::Kind::Struct || token.kind == Token::Kind::Union || token.kind == Token::Kind::Enum || token.kind == Token::Kind::Alias || token.kind == Token::Kind::Annotation || token.kind == Token::Kind::Abstract || token.kind == Token::Kind::Private || token.kind == Token::Kind::Protected || token.kind == Token::Kind::Lib || token.kind == Token::Kind::Fun
        end

        # Phase 35: Check if identifier is a constant (uppercase first letter)
        private def is_constant_name?(slice : Slice(UInt8)) : Bool
          return false if slice.empty?
          first_char = slice[0].chr
          first_char.uppercase? && first_char.ascii_letter?
        end

        private def parse_macro_definition : ExprId
          macro_token = current_token
          advance
          skip_trivia

          name_token = current_token
          macro_name_slice : Slice(UInt8)

          case name_token.kind
          when Token::Kind::Identifier
            macro_name_slice = name_token.slice
            advance

            # Support setter-like macro names foo=
            if current_token.kind == Token::Kind::Eq
              advance
              setter_name = String.build do |io|
                io.write(macro_name_slice)
                io.write_byte('='.ord.to_u8)
              end
              macro_name_slice = @string_pool.intern(setter_name.to_slice)
            end
          when Token::Kind::LBracket
            bracket_start = name_token
            advance
            skip_trivia

            unless current_token.kind == Token::Kind::RBracket
              emit_unexpected(current_token)
              return PREFIX_ERROR
            end
            bracket_end = current_token
            advance
            skip_trivia

            if current_token.kind == Token::Kind::Eq
              advance
              macro_name_slice = @string_pool.intern("[]=".to_slice)
            else
              macro_name_slice = @string_pool.intern("[]".to_slice)
            end

            # For span calculations below keep original LBracket token
            name_token = bracket_start
          when Token::Kind::Plus, Token::Kind::Minus, Token::Kind::Star, Token::Kind::Slash,
               Token::Kind::FloorDiv, Token::Kind::Percent, Token::Kind::StarStar,
               Token::Kind::Less, Token::Kind::Greater, Token::Kind::LessEq, Token::Kind::GreaterEq,
               Token::Kind::EqEq, Token::Kind::EqEqEq, Token::Kind::NotEq, Token::Kind::Spaceship,
               Token::Kind::Match, Token::Kind::NotMatch,
               Token::Kind::Amp, Token::Kind::Pipe, Token::Kind::Caret, Token::Kind::Tilde,
               Token::Kind::LShift, Token::Kind::RShift,
               Token::Kind::DotDot, Token::Kind::DotDotDot,
               Token::Kind::AmpPlus, Token::Kind::AmpMinus, Token::Kind::AmpStar, Token::Kind::AmpStarStar
            macro_name_slice = name_token.slice
            advance
          else
            emit_unexpected(name_token)
            return PREFIX_ERROR
          end

          skip_macro_parameters
          # Allow immediate separators after header (e.g., `struct X; end`)
          skip_statement_end

          pieces, trim_left, trim_right = parse_macro_body
          # Allow trailing separators before 'end'
          skip_statement_end
          expect_identifier("end")
          end_token = previous_token
          # Allow immediate separators after header (e.g., `struct X; end`)
          skip_statement_end

          end_span = end_token.try(&.span)
          body_span = end_span ? name_token.span.cover(end_span) : name_token.span
          macro_span = end_span ? macro_token.span.cover(end_span) : macro_token.span

          body_id = @arena.add_typed(
            MacroLiteralNode.new(
              body_span,
              pieces,
              trim_left,
              trim_right
            )
          )

          @arena.add_typed(
            MacroDefNode.new(
              macro_span,
              macro_name_slice,
              body_id
            )
          )
        end

        # Phase PERCENT_LITERALS: Parse optional receiver for class/singleton methods
        # Returns (receiver_token, dot_token) or (nil, nil)
        # Handles: def self.foo, def obj.foo
        private def parse_def_receiver : {Token?, Token?}
          case current_token.kind
          when Token::Kind::Self
            receiver = current_token
            advance
            skip_whitespace_and_optional_newlines

            if current_token.kind == Token::Kind::Operator && slice_eq?(current_token.slice, ".")
              dot = current_token
              advance
              skip_trivia
              {receiver, dot}
            else
              # self without dot - not a receiver, rewind
              unadvance
              {nil, nil}
            end

          when Token::Kind::Identifier
            # Lookahead for .
            saved_index = @index
            name_token = current_token
            advance
            skip_trivia

            if current_token.kind == Token::Kind::Operator && slice_eq?(current_token.slice, ".")
              # It's receiver.method (singleton method)
              dot = current_token
              advance
              skip_trivia
              {name_token, dot}
            else
              # Just method name, rewind
              @index = saved_index
              {nil, nil}
            end

          else
            # No receiver (operator methods or error)
            {nil, nil}
          end
        end

        # Phase 36: Modified to support abstract modifier
        # Phase 37: Modified to support visibility modifier
        private def parse_def(is_abstract : Bool = false, visibility : Visibility? = nil) : ExprId
          def_token = current_token
          advance
          skip_trivia

          # Phase PERCENT_LITERALS: Parse optional receiver (self.method or obj.method)
          receiver_token, dot_token = parse_def_receiver

          # Phase OPERATOR_METHODS: Parse method name (identifier or operator)
          name_token = current_token
          method_name_slice : Slice(UInt8)

          case name_token.kind
          when Token::Kind::Identifier
            # Regular method name OR setter method (foo=)
            method_name_slice = name_token.slice
            advance

            # Phase SETTER_METHODS: Check if this is setter (identifier immediately followed by =)
            # Pattern: def foo=(value)
            # Important: NO whitespace allowed between identifier and =
            # Valid: def foo=(x)
            # Invalid: def foo =(x)  (space before = is syntax error)
            if current_token.kind == Token::Kind::Eq
              advance  # consume '='
              # Combine "foo" + "=" into "foo=" via string pool
              setter_name = String.build do |io|
                io.write(method_name_slice)
                io.write_byte('='.ord.to_u8)
              end
              method_name_slice = @string_pool.intern(setter_name.to_slice)
            end

          when Token::Kind::LBracket
            # [] / []= / []? / []?= operator method names
            bracket_start = name_token
            advance
            skip_trivia

            # Expect ]
            unless current_token.kind == Token::Kind::RBracket
              emit_unexpected(current_token)
              return PREFIX_ERROR
            end
            bracket_end = current_token
            advance
            skip_trivia

            # Optional '?' after [] to support predicate indexer ([]?)
            has_question = false
            if current_token.kind == Token::Kind::Question
              has_question = true
              advance
              skip_trivia
            end

            # Optional '=' to support setter variants ([]= and []?=)
            if current_token.kind == Token::Kind::Eq
              advance
              # Combine into single method name via string pool
              if has_question
                method_name_slice = @string_pool.intern("[]?=".to_slice)
              else
                method_name_slice = @string_pool.intern("[]=".to_slice)
              end
            else
              # No '=', just reader variants
              if has_question
                method_name_slice = @string_pool.intern("[]?".to_slice)
              else
                method_name_slice = @string_pool.intern("[]".to_slice)
              end
            end

          when Token::Kind::Plus, Token::Kind::Minus, Token::Kind::Star, Token::Kind::Slash,
               Token::Kind::FloorDiv, Token::Kind::Percent, Token::Kind::StarStar,
               Token::Kind::Less, Token::Kind::Greater, Token::Kind::LessEq, Token::Kind::GreaterEq,
               Token::Kind::EqEq, Token::Kind::EqEqEq, Token::Kind::NotEq, Token::Kind::Spaceship,
               Token::Kind::Match, Token::Kind::NotMatch,
               Token::Kind::Amp, Token::Kind::Pipe, Token::Kind::Caret, Token::Kind::Tilde,
               Token::Kind::LShift, Token::Kind::RShift,
               Token::Kind::DotDot, Token::Kind::DotDotDot,
               Token::Kind::AmpPlus, Token::Kind::AmpMinus, Token::Kind::AmpStar, Token::Kind::AmpStarStar
            # Single-token operator method (e.g., +, -, *, ==, <<, etc.)
            method_name_slice = name_token.slice
            advance

          when Token::Kind::Operator
            # Support backtick method name: def `(command)
            if slice_eq?(name_token.slice, "`")
              method_name_slice = name_token.slice
              advance
            else
              emit_unexpected(name_token)
              return PREFIX_ERROR
            end

          else
            # Allow keywords as method names (e.g., `def select`, `def class`, etc.)
            # Treat any identifier-like slice as a valid method name
            slice = name_token.slice
            if slice.size > 0
              first = slice[0]
              if (first >= 'a'.ord.to_u8 && first <= 'z'.ord.to_u8) ||
                 (first >= 'A'.ord.to_u8 && first <= 'Z'.ord.to_u8) ||
                  first == '_'.ord.to_u8
                method_name_slice = slice
                advance
                # Support setter-like keywords: def private=(...)
                if current_token.kind == Token::Kind::Eq
                  advance
                  setter_name = String.build do |io|
                    io.write(method_name_slice)
                    io.write_byte('='.ord.to_u8)
                  end
                  method_name_slice = @string_pool.intern(setter_name.to_slice)
                end
              else
                emit_unexpected(name_token)
                return PREFIX_ERROR
              end
            else
              emit_unexpected(name_token)
              return PREFIX_ERROR
            end
          end

          params = parse_method_params
          return PREFIX_ERROR if params.is_a?(ExprId)  # Phase 71: Handle error from default value parsing

          # Parse optional return type annotation: : ReturnType
          return_type = nil
          skip_trivia
          if operator_token?(current_token, Token::Kind::Colon)
            advance  # consume ':'
            skip_trivia

            type_start_token = current_token
            start_index = @index
            parsed_return_type = parse_type_annotation
            if @index == start_index
              # Fallback: handle atomic types like tuple types {A, B}
              if current_token.kind == Token::Kind::LBrace
                atomic = parse_atomic_type_for_annotation
                if atomic
                  return_type = atomic
                else
                  emit_unexpected(type_start_token)
                end
              else
                emit_unexpected(type_start_token)
              end
            else
              return_type = parsed_return_type
            end
          end

          # Support optional universal quantification: forall U, V
          # Syntax appears after parameter list and optional return type
          # Example: def foo(x : T) : U forall U, T
          if current_token.kind == Token::Kind::Identifier && slice_eq?(current_token.slice, "forall")
            advance
            skip_trivia
            # Parse one or more identifiers separated by commas
            if current_token.kind == Token::Kind::Identifier
              advance
              skip_trivia
              while current_token.kind == Token::Kind::Comma
                advance
                skip_trivia
                break unless current_token.kind == Token::Kind::Identifier
                advance
                skip_trivia
              end
            end
          end

          # Allow separators after header (supports one-liners like `struct X; end`)
          skip_statement_end

        # Phase 36: Abstract methods have no body
        body_ids = nil
        if !is_abstract
          body_ids_arr, rescue_clauses, ensure_body = parse_block_body_with_optional_rescue

          if current_token.kind == Token::Kind::End
            next_token = peek_next_non_trivia
            if next_token.kind == Token::Kind::Rescue || next_token.kind == Token::Kind::Ensure
              advance
              skip_statement_end
            end
          end

          if rescue_clauses.nil? && ensure_body.nil? &&
             (current_token.kind == Token::Kind::Rescue || current_token.kind == Token::Kind::Ensure)
            rescue_clauses, ensure_body = parse_rescue_sections
          end

          if rescue_clauses || ensure_body
            begin_start_span = if body_ids_arr.empty?
              def_token.span
            else
              @arena[body_ids_arr.first].span
            end

            begin_node_id = @arena.add_typed(
              BeginNode.new(
                begin_start_span,
                body_ids_arr,
                rescue_clauses,
                ensure_body
              )
            )
            body_ids = [begin_node_id]
          else
            body_ids = body_ids_arr
          end

          previous_context = @expect_context
          method_debug_name = method_name_slice ? String.new(method_name_slice) : "<anonymous>"
          @expect_context = "def #{method_debug_name}"
          expect_identifier("end")
          @expect_context = previous_context
          end_token = previous_token
          # Allow separators after header (supports one-liners like `struct X; end`)
          skip_statement_end
        else
          # Abstract methods have no body, no 'end' keyword
          end_token = nil
          body_ids = nil
        end

          def_span = if end_token
            def_token.span.cover(end_token.span)
          else
            def_token.span
          end

          # Phase PERCENT_LITERALS: Extract receiver slice if present
          receiver_slice = receiver_token ? receiver_token.slice : nil

          @arena.add_typed(
            DefNode.new(
              def_span,
              method_name_slice,  # Phase OPERATOR_METHODS: use parsed name (identifier or operator)
              params,
              return_type,
              body_ids,
              is_abstract,
              visibility,
              receiver_slice
            )
          )
        end

        # Phase 64: Parse fun (C function declaration)
        # Grammar: fun name(params) : ReturnType
        # No body - external linkage
        private def parse_fun : ExprId
          fun_token = current_token
          advance
          skip_trivia

          # Parse function name
          name_token = current_token
          unless name_token.kind == Token::Kind::Identifier
            emit_unexpected(name_token)
            return PREFIX_ERROR
          end
          advance
          # Support namespaced class/struct: class Foo::Bar
          while current_token.kind == Token::Kind::ColonColon
            advance
            skip_trivia
            if current_token.kind != Token::Kind::Identifier
              emit_unexpected(current_token)
              return PREFIX_ERROR
            end
            name_token = current_token
            advance
          end
          skip_trivia

          # Check for alias: fun name = real_name
          real_name_token : Token? = nil
          if operator_token?(current_token, Token::Kind::Eq)
            advance  # consume =
            skip_trivia

            # Parse real name (can be identifier or string literal)
            real_name_token = current_token
            case real_name_token.kind
            when Token::Kind::Identifier
              advance
              skip_trivia
            when Token::Kind::String
              # String literal for real name (e.g., fun foo = "actual_name")
              advance
              skip_trivia
            else
              emit_unexpected(current_token)
              return PREFIX_ERROR
            end
          end

          # Phase 103K: Parse parameters with proc type support
          params_result = parse_fun_params
          return PREFIX_ERROR if params_result.is_a?(ExprId)
          params, varargs = params_result

          # Parse optional return type annotation: : ReturnType
          # Phase 103K: Use parse_bare_proc_type for full type support
          return_type : Slice(UInt8)? = nil
          skip_trivia
          if operator_token?(current_token, Token::Kind::Colon)
            advance  # consume ':'
            skip_trivia

            # Parse return type using parse_bare_proc_type
            # This handles: Int32, Int -> Void, (Int, String) -> Bool, etc.
            return_type = parse_bare_proc_type
            if return_type.nil?
              emit_unexpected(current_token)
              return PREFIX_ERROR
            end
          end

          # Fun declarations have no body (external linkage)
          fun_span = fun_token.span.cover(current_token.span)
          @arena.add_typed(
            FunNode.new(
              fun_span,
              name_token.slice,
              real_name_token.try(&.slice),  # real_name (nil if same as name)
              params,
              return_type,
              varargs
            )
          )
        end

        private def parse_method_params
          @parsing_method_params = true
          params_b = SmallVec(Parameter, 2).new
          skip_trivia
          unless operator_token?(current_token, Token::Kind::LParen)
            @parsing_method_params = false
            return params_b.to_a
          end

          advance
          @paren_depth += 1  # Track that we're inside parameter list delimiters
          skip_whitespace_and_optional_newlines  # Allow newlines after opening paren
          unless operator_token?(current_token, Token::Kind::RParen)
            loop do
              break if operator_token?(current_token, Token::Kind::RParen)
              # Phase 68: Check for splat operators (* or **)
              # Phase 103: Check for block parameter (&)
              is_splat = false
              is_double_splat = false
              is_block = false
              is_instance_var = false
              prefix_token = nil

              if current_token.kind == Token::Kind::Amp
                # Phase 103: Block parameter prefix
                is_block = true
                prefix_token = current_token
                advance
                skip_trivia
              elsif current_token.kind == Token::Kind::StarStar
                is_double_splat = true
                prefix_token = current_token
                advance
                skip_trivia
              elsif current_token.kind == Token::Kind::Star
                is_splat = true
                prefix_token = current_token
                advance
                skip_trivia
              end

              # Parse parameter name (Identifier or InstanceVar for shorthand syntax)
              # Phase BLOCK_CAPTURE: For capture block (&), name is optional
              # Phase 103J: For splat (*), name is optional (named-only separator)
              # Phase 103K: External parameter names (e.g., "to limit")
              name_token = current_token
              param_name : Slice(UInt8)?
              param_name_span : Span?
              external_name : Slice(UInt8)? = nil
              external_name_span : Span? = nil
              is_instance_var = false

              # Phase 103J: Check for anonymous splat (named-only separator)
              # Example: def foo(*, a : Int) - the * separates positional from named-only params
              if is_splat && (current_token.kind == Token::Kind::Comma || operator_token?(current_token, Token::Kind::RParen))
                # Anonymous splat separator: just '*' without name
                param_name = nil
                param_name_span = nil
                param_start_span = prefix_token.not_nil!.span
                # Don't advance - comma/rparen will be handled below
              # Phase BLOCK_CAPTURE: Check if this is anonymous block capture (&)
              elsif is_block && (current_token.kind == Token::Kind::Comma || operator_token?(current_token, Token::Kind::RParen))
                # Anonymous block capture: just '&' without name
                param_name = nil
                param_name_span = nil
                param_start_span = prefix_token.not_nil!.span
                # Don't advance - comma/rparen will be handled below
              elsif is_block && current_token.kind == Token::Kind::Colon
                # Anonymous typed block parameter: & : ProcType
                # Don't require a name; leave param_name nil and let the
                # type-annotation branch below consume ':' and the proc type.
                param_name = nil
                param_name_span = nil
                param_start_span = prefix_token.not_nil!.span
                # Intentionally do not advance here; ':' is consumed by the
                # type annotation parser below.
              else
                # Regular parameter or named block parameter
                # Phase KEYWORD_PARAMS: Allow keywords as parameter names (e.g., 'of', 'as', 'in')
                # Phase 103K: Check for external parameter names (e.g., "to limit")

                # Safety: if we reached closing paren, stop parsing parameters gracefully
                if operator_token?(name_token, Token::Kind::RParen)
                  break
                end

                case name_token.kind
                when Token::Kind::Identifier, Token::Kind::InstanceVar
                  # Standard parameter names
                when Token::Kind::Of, Token::Kind::As, Token::Kind::In, Token::Kind::Out,
                     Token::Kind::Do, Token::Kind::End, Token::Kind::If, Token::Kind::Unless,
                     Token::Kind::For
                  # Phase KEYWORD_PARAMS: Common keywords that can be parameter names (allow using 'for' as external name)
                else
                  emit_unexpected(name_token)
                  break
                end

                # Phase 103K: External parameter name detection
                # Pattern: "external internal" where external is identifier/keyword, internal is identifier/instance_var
                # Examples: "to limit", "calculation @time"
                if name_token.kind == Token::Kind::Identifier ||
                   name_token.kind == Token::Kind::Of || name_token.kind == Token::Kind::As ||
                   name_token.kind == Token::Kind::In || name_token.kind == Token::Kind::Out ||
                   name_token.kind == Token::Kind::Do || name_token.kind == Token::Kind::End ||
                   name_token.kind == Token::Kind::If || name_token.kind == Token::Kind::Unless ||
                   name_token.kind == Token::Kind::For

                  # Save potential external name
                  potential_external_name = name_token.slice
                  potential_external_span = name_token.span
                  advance

                  # Check if there's visible gap followed by another identifier or instance var
                  # Gap indicates: "to limit" or "calculation @time" pattern
                  if gap_after_span?(potential_external_span, current_token)
                    skip_trivia if @keep_trivia

                    # Check if next token is identifier or instance var (internal name)
                    if current_token.kind == Token::Kind::Identifier
                      # External name pattern confirmed: "to limit"
                      external_name = potential_external_name
                      external_name_span = potential_external_span
                      param_name = current_token.slice
                      param_name_span = current_token.span
                      param_start_span = prefix_token ? prefix_token.span : potential_external_span
                      is_instance_var = false
                      advance
                      skip_trivia
                    elsif current_token.kind == Token::Kind::InstanceVar
                      # External name + instance var: "calculation @time"
                      external_name = potential_external_name
                      external_name_span = potential_external_span
                      param_name = slice_without_prefix(current_token.slice, 1)
                      param_name_span = current_token.span
                      param_start_span = prefix_token ? prefix_token.span : potential_external_span
                      is_instance_var = true
                      advance
                      skip_trivia
                    else
                      # Not external name pattern, just regular parameter
                      param_name = potential_external_name
                      param_name_span = potential_external_span
                      param_start_span = prefix_token ? prefix_token.span : potential_external_span
                      is_instance_var = false
                      # Already advanced and skipped trivia
                    end
                  else
                    # No space after, just regular parameter name
                    param_name = potential_external_name
                    param_name_span = potential_external_span
                    param_start_span = prefix_token ? prefix_token.span : potential_external_span
                    is_instance_var = false
                    skip_trivia
                  end
                else
                  # Instance variable shorthand without external name: @value : T
                  is_instance_var = (name_token.kind == Token::Kind::InstanceVar)
                  param_name = is_instance_var ? slice_without_prefix(name_token.slice, 1) : name_token.slice
                  param_name_span = name_token.span
                  param_start_span = prefix_token ? prefix_token.span : name_token.span
                  advance
                  skip_trivia
                end
              end

              # Parse optional type annotation: : Type
              # Phase 103: For block parameters, parse proc type (Token ->)
              type_annotation : Slice(UInt8)? = nil  # TIER 2.1: Zero-copy slice
              param_type_span = nil
              if operator_token?(current_token, Token::Kind::Colon)
                advance  # consume ':'
                skip_trivia

                if is_block
                  # Block parameter: parse full proc type (supports: A, B -> C, Tuple(K,V), etc.)
                  type_start_token = current_token
                  parsed_type = parse_bare_proc_type
                  if parsed_type.nil?
                    # Fallback to general type annotation parser (should rarely happen)
                    parsed_type = parse_type_annotation
                  end

                  if parsed_type.nil?
                    emit_unexpected(type_start_token)
                  else
                    type_annotation = parsed_type
                    type_end_token = previous_token
                    if type_end_token
                      param_type_span = type_start_token.span.cover(type_end_token.span)
                    else
                      param_type_span = type_start_token.span
                    end
                  end
                else
                  # Regular parameter - parse full type including possible proc types
                  # Use parse_bare_proc_type to allow comma-separated inputs before '->'
                  type_start_token = current_token
                  parsed_type = parse_bare_proc_type
                  if parsed_type.nil?
                    # Fallback to generic type annotation parser to avoid regressions
                    parsed_type = parse_type_annotation
                  end

                  if parsed_type.nil?
                    emit_unexpected(type_start_token)
                  else
                    type_annotation = parsed_type
                    type_end_token = previous_token
                    if type_end_token
                      param_type_span = type_start_token.span.cover(type_end_token.span)
                    else
                      param_type_span = type_start_token.span
                    end
                  end
                end
              end

              # Skip whitespace and newlines after type annotation
              # This allows multiline parameter lists: def initialize(\n  @x : Int32,\n  @y : String\n)
              skip_whitespace_and_optional_newlines

              # Phase 71: Parse optional default value: = expression
              default_value = nil
              default_value_span = nil
              if operator_token?(current_token, Token::Kind::Eq)
                advance  # consume '='
                skip_trivia

                # Parse default value expression
                default_value = parse_expression(0)
                return PREFIX_ERROR if default_value.invalid?
                default_value_span = @arena[default_value].span
                skip_whitespace_and_optional_newlines  # Allow newlines after default value
              end

              # Calculate full parameter span
              param_span = if default_value_span
                param_start_span.cover(default_value_span)
              elsif param_type_span
                param_start_span.cover(param_type_span)
              else
                param_start_span
              end

              params_b << Parameter.new(
                param_name,
                external_name,  # Phase 103K: External parameter name
                type_annotation,
                default_value,
                param_span,
                param_name_span,
                external_name_span,  # Phase 103K: External name span
                param_type_span,
                default_value_span,
                is_splat,
                is_double_splat,
                is_block,  # Phase 103: block parameter flag
                is_instance_var  # Instance variable parameter shorthand: @value : T
              )

              if operator_token?(current_token, Token::Kind::Comma)
                advance
                skip_whitespace_and_optional_newlines  # Allow newlines after comma in parameter lists
                next
              end

              break
            end
          end

          expect_operator(Token::Kind::RParen)
          @paren_depth -= 1  # Exiting parameter list delimiters
          @parsing_method_params = false
          params_b.to_a
        end

        private def gap_after_span?(span : Span, token : Token) : Bool
          if @keep_trivia
            token.kind == Token::Kind::Whitespace || token.kind == Token::Kind::Newline
          else
            token.span.start_offset > span.end_offset
          end
        end

        private def slice_without_prefix(slice : Slice(UInt8), bytes : Int32) : Slice(UInt8)
          return slice if slice.size <= bytes
          Slice.new(slice.to_unsafe + bytes, slice.size - bytes)
        end

        # Phase 2: Parse if/elsif/else
        # Phase 103: Updated to support assignment in condition
        # Grammar: if CONDITION [then] BODY [elsif CONDITION [then] BODY]* [else BODY] end
        # CONDITION can be assignment: if x = compute()
        private def parse_if : ExprId
          if_token = current_token
          advance
          skip_trivia

          # Parse condition (can be expression or assignment)
          condition = parse_statement
          return PREFIX_ERROR if condition.invalid?

          skip_trivia
          # Optional "then" keyword
          token = current_token
          if token.kind == Token::Kind::Then
            advance
          end
          # Allow separators after header (supports one-liners like `struct X; end`)
          skip_statement_end

          # Parse then body
          then_body_b = SmallVec(ExprId, 4).new
          loop do
            skip_trivia
            token = current_token
            break if token.kind == Token::Kind::Elsif || token.kind == Token::Kind::Else || token.kind == Token::Kind::End
            break if token.kind == Token::Kind::EOF
            break if macro_terminator_reached?(token)  # Phase 103K: Stop at macro terminators

            expr = parse_statement
            then_body_b << expr unless expr.invalid?
            consume_newlines
          end

          # Parse optional elsif branches
          elsifs = [] of ElsifBranch
          loop do
            skip_trivia
            token = current_token
            break unless token.kind == Token::Kind::Elsif

            # Save elsif token for span
            elsif_token = token
            advance
            skip_trivia

            # Parse elsif condition (can be expression or assignment)
            elsif_condition = parse_statement
            return PREFIX_ERROR if elsif_condition.invalid?

            skip_trivia
            # Optional "then" keyword
            token = current_token
            if token.kind == Token::Kind::Then
              advance
            end
            consume_newlines

            # Parse elsif body
            elsif_body_b = SmallVec(ExprId, 2).new
            loop do
              skip_trivia
              token = current_token
              break if token.kind == Token::Kind::Elsif || token.kind == Token::Kind::Else || token.kind == Token::Kind::End
              break if token.kind == Token::Kind::EOF
              break if macro_terminator_reached?(token)  # Phase 103K: Stop at macro terminators

              expr = parse_statement
              elsif_body_b << expr unless expr.invalid?
              consume_newlines
            end

            # Capture elsif span (from elsif keyword to last expression)
            elsif_span = if elsif_body_b.size > 0
              last_expr = @arena[elsif_body_b.last]
              elsif_token.span.cover(last_expr.span)
            else
              elsif_token.span
            end

            elsifs << ElsifBranch.new(elsif_condition, elsif_body_b.to_a, elsif_span)
          end

          # Parse optional else body
          else_body_b = nil
          token = current_token
          if token.kind == Token::Kind::Else
            advance
            consume_newlines

            else_body_b = SmallVec(ExprId, 4).new
            loop do
              skip_trivia
              token = current_token
              break if token.kind == Token::Kind::End
              break if token.kind == Token::Kind::EOF
              break if macro_terminator_reached?(token)  # Phase 103K: Stop at macro terminators

              expr = parse_statement
              else_body_b << expr unless expr.invalid?
              consume_newlines
            end
          end

          expect_identifier("end")
          end_token = previous_token
          # Allow separators after header (supports one-liners like `struct X; end`)
          skip_statement_end

          if_span = if end_token
            if_token.span.cover(end_token.span)
          else
            if_token.span
          end

          # Materialize then/else arrays from builders
          then_arr = then_body_b.to_a
          else_arr = else_body_b ? else_body_b.to_a : nil
          elsifs_field = elsifs.size > 0 ? elsifs : nil

          @arena.add_typed(
            IfNode.new(
              if_span,
              condition,
              then_arr,
              elsifs_field,
              else_arr
            )
          )
        end

        # Phase 24: Parse unless expression (similar to if but without elsif)
        # Phase 24: Parse unless (inverse of if)
        # Phase 103: Updated to support assignment in condition
        # Grammar: unless CONDITION [then] BODY [else BODY] end
        # CONDITION can be assignment: unless x = compute()
        private def parse_unless : ExprId
          unless_token = current_token
          advance
          skip_trivia

          # Parse condition (can be expression or assignment)
          condition = parse_statement
          return PREFIX_ERROR if condition.invalid?

          skip_trivia
          # Optional "then" keyword
          token = current_token
          if token.kind == Token::Kind::Then
            advance
          end
          # Allow separators after header (supports one-liners like `struct X; end`)
          skip_statement_end

          # Parse then body (executed when condition is false)
          then_body_b = SmallVec(ExprId, 4).new
          loop do
            skip_trivia
            token = current_token
            break if token.kind == Token::Kind::Else || token.kind == Token::Kind::End
            break if token.kind == Token::Kind::EOF
            break if macro_terminator_reached?(token)  # Phase 103K: Stop at macro terminators

            expr = parse_statement
            then_body_b << expr unless expr.invalid?
            consume_newlines
          end

          # Parse optional else body (executed when condition is true)
            else_body_b = nil
          token = current_token
          if token.kind == Token::Kind::Else
            advance
            consume_newlines

            else_body_b = SmallVec(ExprId, 4).new
            loop do
              skip_trivia
              token = current_token
              break if token.kind == Token::Kind::End
              break if token.kind == Token::Kind::EOF
              break if macro_terminator_reached?(token)  # Phase 103K: Stop at macro terminators

              expr = parse_statement
              else_body_b << expr unless expr.invalid?
              consume_newlines
            end
          end

          expect_identifier("end")
          end_token = previous_token
          # Allow separators after header (supports one-liners like `struct X; end`)
          skip_statement_end

          unless_span = if end_token
            unless_token.span.cover(end_token.span)
          else
            unless_token.span
          end

          # Materialize arrays from builders
          then_arr = then_body_b.to_a
          else_arr = else_body_b ? else_body_b.to_a : nil
          @arena.add_typed(
            UnlessNode.new(
              unless_span,
              condition,
              then_arr,
              else_arr
            )
          )
        end

        # Phase 11: Parse case/when expression
        # Grammar: case <value>
        #          when <cond1>, <cond2> [then]
        #            <body>
        #          [else
        #            <body>]
        #          end
        # Phase 11: case/when pattern matching
        # Phase 103: Updated to support bare case (no value) and multi-line
        # Grammar: case [VALUE]
        #            when COND1, COND2
        #              BODY
        #            [else
        #              BODY]
        #          end
        private def parse_case : ExprId
          case_token = current_token
          advance
          consume_newlines  # Skip trivia AND newlines after 'case'

          # Parse optional case value (bare case has no value)
          # If next token is 'when', it's bare case (no value)
          value : ExprId? = nil
          if current_token.kind != Token::Kind::When
            # Has value: case EXPR (can be assignment like: case x = foo())
            val = parse_op_assign
            return PREFIX_ERROR if val.invalid?
            value = val
            consume_newlines
          end

          # Phase PERCENT_LITERALS: Parse when/in branches
          # `when` for traditional case, `in` for pattern matching
          when_branches_b = SmallVec(WhenBranch, 4).new
          in_branches = nil

          # Parse when branches
          loop do
            consume_newlines
            token = current_token
            break unless token.kind == Token::Kind::When

            when_token = token
            advance
            skip_trivia

            # Parse when conditions (comma-separated)
            # Phase 103: Multi-line when clauses - allow newlines after commas
            conditions_b = SmallVec(ExprId, 2).new
            loop do
              cond = parse_expression(0)
              return PREFIX_ERROR if cond.invalid?
              conditions_b << cond

              skip_trivia
              break unless current_token.kind == Token::Kind::Comma
              advance  # consume comma
            # Allow statement separators (newlines/semicolons) after comma
            skip_statement_end
            end

            skip_trivia

            # Optional "then" keyword
            if current_token.kind == Token::Kind::Then
              advance
            end

            # Allow statement separators before the branch body
            skip_statement_end

            # Parse when body
            when_body_b = SmallVec(ExprId, 2).new
            loop do
              skip_trivia
              token = current_token
              break if token.kind.in?(Token::Kind::When, Token::Kind::Else, Token::Kind::End, Token::Kind::EOF)

              stmt = parse_statement
              when_body_b << stmt unless stmt.invalid?
              # Allow statement separators between statements in branch body
              skip_statement_end
            end

            # Capture when span
            when_span = if when_body_b.size > 0
              last_expr = @arena[when_body_b.last]
              when_token.span.cover(last_expr.span)
            else
              when_token.span
            end

            when_branches_b << WhenBranch.new(conditions_b.to_a, when_body_b.to_a, when_span)
          end

          # Phase PERCENT_LITERALS: Parse `in` branches (pattern matching)
          # Same structure as `when`, but keyword is `in`
          # Check if we have any `in` branches after when branches
            in_branches_b = SmallVec(WhenBranch, 2).new

            loop do
              consume_newlines  # skip newlines before in
              token = current_token
              break unless token.kind == Token::Kind::In

              in_token = token
              advance
              skip_trivia

              # Parse in pattern list. Accept both regular expressions and
              # dot-predicate shorthand (e.g., `in .i8? then ...`).
              # In the dot-predicate form we attach the member access to the
              # case value when present so the AST remains consistent.
              patterns_b = SmallVec(ExprId, 2).new
              loop do
                pattern = parse_in_pattern_expr(value)
                return PREFIX_ERROR if pattern.invalid?
                patterns_b << pattern

                skip_trivia
                break unless current_token.kind == Token::Kind::Comma
                advance  # consume comma
                # Allow statement separators after comma in patterns list
                skip_statement_end
              end

              skip_trivia

              # Optional "then" keyword
              if current_token.kind == Token::Kind::Then
                advance
              end

              # Allow statement separators before the branch body
              skip_statement_end

              # Parse in body
              in_body_b = SmallVec(ExprId, 2).new
              loop do
                skip_trivia
                token = current_token
                break if token.kind.in?(Token::Kind::In, Token::Kind::Else, Token::Kind::End, Token::Kind::EOF)

                stmt = parse_statement
                in_body_b << stmt unless stmt.invalid?
                # Allow statement separators between statements in branch body
                skip_statement_end
              end

              # Capture in span
              in_span = if in_body_b.size > 0
                last_expr = @arena[in_body_b.last]
                in_token.span.cover(last_expr.span)
              else
                in_token.span
              end

              in_branches_b << WhenBranch.new(patterns_b.to_a, in_body_b.to_a, in_span)
            end

            in_branches = in_branches_b.size > 0 ? in_branches_b.to_a : nil

          # Parse optional else body
          else_body_b = nil
          token = current_token
          if token.kind == Token::Kind::Else
            advance
            # Allow statement separators before else body
            skip_statement_end

            else_body_b = SmallVec(ExprId, 4).new
            loop do
              skip_trivia
              token = current_token
              break if token.kind == Token::Kind::End
              break if token.kind == Token::Kind::EOF
              break if macro_terminator_reached?(token)  # Phase 103K: Stop at macro terminators

              expr = parse_statement
              else_body_b << expr unless expr.invalid?
              # Allow statement separators between statements in else body
              skip_statement_end
            end
          end

          # Expect 'end' keyword
          unless current_token.kind == Token::Kind::End
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end
          end_token = current_token
          advance

          case_span = case_token.span.cover(end_token.span)

          # Materialize else body if present
          else_body = else_body_b ? else_body_b.to_a : nil
          @arena.add_typed(
            CaseNode.new(
              case_span,
              value,
              when_branches_b.to_a,
              else_body,
              in_branches  # Phase PERCENT_LITERALS: pattern matching branches
            )
          )
        end

        # Parse a single `in`-pattern expression.
        # Supports:
        #  - Regular expression pattern (fallback to parse_expression)
        #  - Dot-predicate shorthand: `.pred?` which is desugared into
        #    a member access on the case value (when provided).
        private def parse_in_pattern_expr(case_value : ExprId?) : ExprId
          # Accept leading dot shorthand only when a case value exists
          if case_value && current_token.kind == Token::Kind::Operator && slice_eq?(current_token.slice, ".")
            dot = current_token
            advance
            skip_trivia

            # Expect an identifier after the dot (supports suffix ?/!)
            name = current_token
            unless name.kind == Token::Kind::Identifier
              emit_unexpected(name)
              return PREFIX_ERROR
            end
            member_span = @arena[case_value.not_nil!].span.cover(dot.span).cover(name.span)
            node = @arena.add_typed(
              MemberAccessNode.new(
                member_span,
                case_value.not_nil!,
                name.slice
              )
            )
            advance
            return node
          end

          # Fallback: regular expression
          parse_expression(0)
        end

        # Phase 90A: Parse select (concurrent channel operation selection)
        # Grammar: select when condition body... [else body...] end
        # Note: Unlike case, select has NO value expression after 'select' keyword
        private def parse_select : ExprId
          select_token = current_token
          advance
          # Allow separators after header (supports one-liners like `struct X; end`)
          skip_statement_end

          # Parse when branches
          select_branches_b = SmallVec(SelectBranch, 4).new
          loop do
            skip_trivia
            token = current_token
            break unless token.kind == Token::Kind::When

            when_token = token
            advance
            skip_trivia

            # Parse when condition (single expression or assignment)
            # Examples: channel.receive, channel.send(x), timeout(5.seconds)
            # Can also be assignment: msg = channel.receive
            # Use parse_statement to support assignments
            condition = parse_statement
            return PREFIX_ERROR if condition.invalid?

            skip_trivia

            # Optional "then" keyword
            if current_token.kind == Token::Kind::Then
              advance
            end

            consume_newlines

            # Parse when body
            when_body_b = SmallVec(ExprId, 2).new
            loop do
              skip_trivia
              token = current_token
              break if token.kind.in?(Token::Kind::When, Token::Kind::Else, Token::Kind::End, Token::Kind::EOF)

              stmt = parse_statement
              when_body_b << stmt unless stmt.invalid?
              consume_newlines
            end

            # Capture when span
            when_span = if when_body_b.size > 0
              last_expr = @arena[when_body_b.last]
              when_token.span.cover(last_expr.span)
            else
              when_token.span
            end

            select_branches_b << SelectBranch.new(condition, when_body_b.to_a, when_span)
          end

          # Parse optional else body (non-blocking fallback)
          # Use a SmallVec builder to avoid heap churn; materialize after parsing.
          else_body_b = nil
          token = current_token
          if token.kind == Token::Kind::Else
            advance
            consume_newlines

            else_body_b = SmallVec(ExprId, 4).new
            loop do
              skip_trivia
              token = current_token
              break if token.kind == Token::Kind::End
              break if token.kind == Token::Kind::EOF
              break if macro_terminator_reached?(token)  # Phase 103K: Stop at macro terminators

              expr = parse_statement
              else_body_b << expr unless expr.invalid?
              consume_newlines
            end
          end

          expect_identifier("end")
          end_token = previous_token
          # Allow separators after header (supports one-liners like `struct X; end`)
          skip_statement_end

          select_span = if end_token
            select_token.span.cover(end_token.span)
          else
            select_token.span
          end

          else_body = else_body_b ? else_body_b.to_a : nil
          @arena.add_typed(
            SelectNode.new(
              select_span,
              select_branches_b.to_a,
              else_body
            )
          )
        end

        # Phase 22: Parse while loop
        # Phase 103: Updated to support assignment in condition
        # Grammar: while CONDITION [do] BODY end
        # CONDITION can be assignment: while x = next_token
        private def parse_while : ExprId
          while_token = current_token
          advance
          skip_trivia

          # Parse condition (can be expression or assignment)
          condition = parse_statement
          return PREFIX_ERROR if condition.invalid?

          skip_trivia
          # Optional "do" keyword
          token = current_token
          if token.kind == Token::Kind::Do
            advance
          end
          consume_newlines

          # Parse body
          body_ids_b = SmallVec(ExprId, 4).new
          loop do
            # Allow statement separators inside class/struct bodies
            skip_statement_end
            token = current_token
            break if token.kind == Token::Kind::End
            break if token.kind == Token::Kind::EOF

            expr = parse_statement
            body_ids_b << expr unless expr.invalid?
            # Allow separators between members
            skip_statement_end
          end

          expect_identifier("end")
          end_token = previous_token
          consume_newlines

          while_span = if end_token
            while_token.span.cover(end_token.span)
          else
            while_token.span
          end

          @arena.add_typed(
            WhileNode.new(
              while_span,
              condition,
              body_ids_b.to_a
            )
          )
        end

        # Phase 83: infinite loop (loop do...end)
        private def parse_loop : ExprId
          loop_token = current_token
          advance
          skip_trivia

          # Support both `loop do ... end` and `loop { ... }`
          if current_token.kind == Token::Kind::Do
            advance
            consume_newlines

            body_ids, rescue_clauses, ensure_body = parse_block_body_with_optional_rescue

            if rescue_clauses || ensure_body
              begin_span = if body_ids.empty?
                loop_token.span
              else
                first_span = @arena[body_ids.first].span
                last_span = @arena[body_ids.last].span
                first_span.cover(last_span)
              end

              begin_node_id = @arena.add_typed(
                BeginNode.new(
                  begin_span,
                  body_ids,
                  rescue_clauses,
                  ensure_body
                )
              )
              body_ids = [begin_node_id]
            end

            expect_identifier("end")
            end_token = previous_token
            consume_newlines

            loop_span = if end_token
              loop_token.span.cover(end_token.span)
            else
              loop_token.span
            end

            return @arena.add_typed(LoopNode.new(loop_span, body_ids))

          elsif current_token.kind == Token::Kind::LBrace
            # Brace form: parse a block and reuse its body as loop body
            block_id = parse_block
            return PREFIX_ERROR if block_id.invalid?
            block_node = @arena[block_id]
            body = Frontend.node_block_body(block_node) || [] of ExprId
            loop_span = loop_token.span.cover(block_node.span)
            return @arena.add_typed(LoopNode.new(loop_span, body))
          else
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end
        end

        # Phase 84: spawn fiber (concurrency)
        # Syntax: spawn do...end | spawn expression
        private def parse_spawn : ExprId
          spawn_token = current_token
          advance
          skip_trivia

          token = current_token

          # Check if it's block form (spawn do...end) or expression form (spawn expr)
          if token.kind == Token::Kind::Do
            # Block form: spawn do...end
            advance
            consume_newlines

            # Parse body
            body_ids_b = SmallVec(ExprId, 4).new
            loop do
              skip_trivia
              token = current_token
              break if token.kind == Token::Kind::End
              break if token.kind == Token::Kind::EOF

              expr = parse_statement
              body_ids_b << expr unless expr.invalid?
              consume_newlines
            end

            expect_identifier("end")
            end_token = previous_token
            consume_newlines

            spawn_span = if end_token
              spawn_token.span.cover(end_token.span)
            else
              spawn_token.span
            end

            @arena.add_typed(
              SpawnNode.new(
                spawn_span,
                nil,
                body_ids_b.to_a
              )
            )
          else
            # Expression form: spawn expression
            expr = parse_expression(0)
            return PREFIX_ERROR if expr.invalid?

            expr_span = node_span(expr)
            spawn_span = spawn_token.span.cover(expr_span)

            @arena.add_typed(
              SpawnNode.new(
                spawn_span,
                expr,
                nil
              )
            )
          end
        end

        # Phase 25: Parse until loop (inverse of while)
        # Phase 23: Parse until loop
        # Phase 103: Updated to support assignment in condition
        # Grammar: until CONDITION [do] BODY end
        # CONDITION can be assignment: until x = compute()
        private def parse_until : ExprId
          until_token = current_token
          advance
          skip_trivia

          # Parse condition (can be expression or assignment)
          condition = parse_statement
          return PREFIX_ERROR if condition.invalid?

          skip_trivia
          # Optional "do" keyword
          token = current_token
          if token.kind == Token::Kind::Do
            advance
          end
          consume_newlines

          # Parse body
          body_ids_b = SmallVec(ExprId, 4).new
          loop do
            # Tolerate newlines and semicolons between members
            skip_statement_end
            token = current_token
            break if token.kind == Token::Kind::End
            break if token.kind == Token::Kind::EOF

            expr = parse_statement
            body_ids_b << expr unless expr.invalid?
            consume_newlines
          end

          expect_identifier("end")
          end_token = previous_token
          consume_newlines

          until_span = if end_token
            until_token.span.cover(end_token.span)
          else
            until_token.span
          end

          @arena.add_typed(
            UntilNode.new(
              until_span,
              condition,
              body_ids_b.to_a
            )
          )
        end

        # Phase 99: Parse for loop (iteration)
        # Grammar: for var in collection ... end
        private def parse_for : ExprId
          for_token = current_token
          advance
          skip_trivia

          # Parse variable name (identifier)
          variable_token = current_token
          unless variable_token.kind == Token::Kind::Identifier
            emit_unexpected(variable_token)
            return PREFIX_ERROR
          end
          advance
          skip_trivia

          # Expect 'in' keyword
          unless current_token.kind == Token::Kind::In
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end
          advance
          skip_trivia

          # Parse collection expression
          collection = parse_expression(0)
          return PREFIX_ERROR if collection.invalid?

          skip_trivia
          # Optional "do" keyword
          token = current_token
          if token.kind == Token::Kind::Do
            advance
          end
          consume_newlines

          # Parse body
          body_ids_b = SmallVec(ExprId, 4).new
          loop do
            # Tolerate newlines and semicolons between members
            skip_statement_end
            token = current_token
            break if token.kind == Token::Kind::End
            break if token.kind == Token::Kind::EOF

            expr = parse_statement
            body_ids_b << expr unless expr.invalid?
            consume_newlines
          end

          expect_identifier("end")
          end_token = previous_token
          consume_newlines

          for_span = if end_token
            for_token.span.cover(end_token.span)
          else
            for_token.span
          end

          @arena.add_typed(
            ForNode.new(
              for_span,
              variable_token.slice,
              collection,
              body_ids_b.to_a
            )
          )
        end

        # Phase 28/29: Parse begin/end block with optional rescue/ensure
        # Grammar: begin <body> [rescue [type] [=> var] <rescue_body>]* [ensure <ensure_body>] end
        # Returns the value of the last expression in the body (or rescue if exception)
        private def parse_begin : ExprId
          begin_token = current_token
          advance  # consume 'begin'
          consume_newlines

          body_ids, rescue_clauses, ensure_body = parse_block_body_with_optional_rescue

          expect_identifier("end")
          end_token = previous_token
          consume_newlines

          begin_span = if end_token
            begin_token.span.cover(end_token.span)
          else
            begin_token.span
          end

          @arena.add_typed(
            BeginNode.new(
              begin_span,
              body_ids,
              rescue_clauses,
              ensure_body
            )
          )
        end

        private def parse_rescue_sections : Tuple(Array(RescueClause)?, Array(ExprId)?)
          debug("parse_rescue_sections: entering with token=#{current_token.kind}") if @debug_enabled
          rescue_clauses = nil
          while current_token.kind == Token::Kind::Rescue
            rescue_clauses ||= [] of RescueClause
            rescue_start = current_token
            advance  # consume 'rescue'
            skip_trivia

            exception_type : Slice(UInt8)? = nil
            variable_name : Slice(UInt8)? = nil

            token = current_token
            if token.kind == Token::Kind::Identifier
              identifier_token = token
              advance
              skip_trivia

              if current_token.kind == Token::Kind::Colon
                variable_name = identifier_token.slice
                advance  # consume ':'
                skip_trivia

                type_slice = parse_type_annotation
                if type_slice.empty?
                  emit_unexpected(current_token)
                else
                  exception_type = type_slice
                end
                skip_trivia
                token = current_token
              else
                exception_type = identifier_token.slice
                token = current_token
              end
            end

            if token.kind == Token::Kind::Arrow
              advance  # consume '=>'
              skip_trivia
              token = current_token
              if token.kind == Token::Kind::Identifier
                variable_name = token.slice
                advance
              end
            end

            consume_newlines

            rescue_body_b = SmallVec(ExprId, 2).new
            loop do
              skip_trivia
              token = current_token
              break if token.kind == Token::Kind::Rescue || token.kind == Token::Kind::Ensure || token.kind == Token::Kind::End
              break if token.kind == Token::Kind::EOF

              expr = parse_statement
              rescue_body_b << expr unless expr.invalid?
              consume_newlines
            end

            rescue_span = rescue_start.span
            rescue_clauses << RescueClause.new(exception_type, variable_name, rescue_body_b.to_a, rescue_span)
          end

          ensure_body = nil
          if current_token.kind == Token::Kind::Ensure
            advance  # consume 'ensure'
            consume_newlines

            ensure_body_b = SmallVec(ExprId, 2).new
            loop do
              skip_trivia
              token = current_token
              break if token.kind == Token::Kind::End
              break if token.kind == Token::Kind::EOF

              expr = parse_statement
              ensure_body_b << expr unless expr.invalid?
              consume_newlines
            end
            ensure_body = ensure_body_b.to_a
          end

          {rescue_clauses, ensure_body}
        end

        # Phase 29: Parse raise statement
        # Grammar: raise <expression> | raise(args...) | raise
        # Note: bare 'raise' (re-raise) is only valid in rescue blocks
        # In Crystal, 'raise' is actually a method, not a keyword, so it supports:
        #   raise(...)              - call with parentheses
        #   raise "msg", arg1, arg2 - call without parentheses
        #   raise expr              - single argument call
        private def parse_raise : ExprId
          raise_token = current_token
          advance
          skip_trivia

          token = current_token
          if token.kind == Token::Kind::LParen
            # Method call with parentheses: raise(...)
            raise_id = @arena.add_typed(
              IdentifierNode.new(
                raise_token.span,
                @string_pool.intern(Bytes.new("raise".to_unsafe, 5))
              )
            )
            parse_parenthesized_call(raise_id)
          elsif token.kind.in?(Token::Kind::Newline, Token::Kind::EOF, Token::Kind::End, Token::Kind::Else, Token::Kind::Elsif, Token::Kind::Rescue, Token::Kind::Ensure)
            # Bare raise (re-raise current exception)
            @arena.add_typed(
              RaiseNode.new(
                raise_token.span,
                nil
              )
            )
          else
            # Method call without parentheses: raise "msg", name: value
            # Create identifier callee for "raise"
            callee = @arena.add_typed(
              IdentifierNode.new(
                raise_token.span,
                @string_pool.intern(Bytes.new("raise".to_unsafe, 5))
              )
            )

            args_b = SmallVec(ExprId, 2).new
            named_b = SmallVec(NamedArgument, 2).new

            # Parse first argument
            first_arg = parse_op_assign
            return PREFIX_ERROR if first_arg.invalid?
            args_b << first_arg
            skip_trivia

            # Consume additional arguments separated by commas
            loop do
              if current_token.kind == Token::Kind::Comma
                advance
                skip_trivia
                # Named argument?
                if named_arg_start?
                  name_token = current_token
                  name_slice = name_token.slice
                  name_span = name_token.span
                  advance
                  skip_trivia
                  # Expect ':'
                  unless current_token.kind == Token::Kind::Colon
                    emit_unexpected(current_token)
                    return PREFIX_ERROR
                  end
                  advance
                  skip_trivia
                  value_expr = parse_op_assign
                  return PREFIX_ERROR if value_expr.invalid?
                  value_span = @arena[value_expr].span
                  named_b << NamedArgument.new(name_slice, value_expr, name_span, value_span)
                  skip_trivia
                else
                  # Positional argument
                  nxt = parse_op_assign
                  return PREFIX_ERROR if nxt.invalid?
                  args_b << nxt
                  skip_trivia
                end
              else
                break
              end
            end

            args = args_b.to_a
            named_args = named_b.empty? ? nil : named_b.to_a
            last_span = if named_args
              @arena[named_b.last.value].span
            else
              @arena[args.last].span
            end
            call_span = raise_token.span.cover(last_span)
            @arena.add_typed(
              CallNode.new(
                call_span,
                callee,
                args,
                nil,
                named_args
              )
            )
          end
        end

        # Phase 67: Parse with (context block)
        # Grammar: with receiver ... end
        # Changes self context to receiver within the block
        private def parse_with : ExprId
          with_token = current_token
          advance  # consume 'with'
          skip_trivia

          # Parse receiver expression
          receiver = parse_expression(0)
          return PREFIX_ERROR if receiver.invalid?

          consume_newlines

          # Parse body
          body_ids_b = SmallVec(ExprId, 4).new
          loop do
            # Tolerate newlines and semicolons between members
            skip_statement_end
            token = current_token
            break if token.kind == Token::Kind::End
            break if token.kind == Token::Kind::EOF

            expr = parse_statement
            body_ids_b << expr unless expr.invalid?
            # Allow separators between members
            skip_statement_end
          end

          expect_identifier("end")
          end_token = previous_token
          consume_newlines

          with_span = if end_token
            with_token.span.cover(end_token.span)
          else
            with_token.span
          end

          @arena.add_typed(
            WithNode.new(
              with_span,
              receiver,
              body_ids_b.to_a
            )
          )
        end

        # Phase 65: Parse require (import file/library)
        # Grammar: require "path" | require "./local" | require expr
        # Require MUST have a path argument (unlike raise which can be bare)
        private def parse_require : ExprId
          require_token = current_token
          advance
          skip_trivia

          # Require must have a path (string literal or expression)
          token = current_token
          if token.kind.in?(Token::Kind::Newline, Token::Kind::EOF, Token::Kind::End, Token::Kind::Else, Token::Kind::Elsif)
            # Missing path - syntax error
            emit_unexpected(token)
            return PREFIX_ERROR
          end

          # Parse path expression (typically a string literal)
          path = parse_expression(0)
          return PREFIX_ERROR if path.invalid?

          path_span = node_span(path)
          require_span = require_token.span.cover(path_span)

          @arena.add_typed(
            RequireNode.new(
              require_span,
              path
            )
          )
        end

        # Phase 6: Parse return statement
        # Grammar: return | return <expression>
        private def parse_return : ExprId
          return_token = current_token
          advance
          skip_trivia

          # Check if there's a return value
          # return without value if: newline, EOF, end, else, elsif, if (for postfix)
          token = current_token
          if token.kind.in?(Token::Kind::Newline, Token::Kind::EOF, Token::Kind::End, Token::Kind::Else, Token::Kind::Elsif, Token::Kind::If, Token::Kind::Unless, Token::Kind::While, Token::Kind::Until)
            # Return without value (implicit nil)
            @arena.add_typed(ReturnNode.new(return_token.span, nil))
          else
            # Return with value (support multiple values → implicit tuple)
            first_value = parse_expression(0)
            return PREFIX_ERROR if first_value.invalid?

            skip_trivia
            if current_token.kind == Token::Kind::Comma
              values = [first_value]
              loop do
                advance  # consume comma
                skip_trivia
                val = parse_expression(0)
                return PREFIX_ERROR if val.invalid?
                values << val
                skip_trivia
                break unless current_token.kind == Token::Kind::Comma
              end

              # Create implicit tuple literal of return values
              first_span = @arena[values.first].span
              last_span = @arena[values.last].span
              tuple_span = first_span.cover(last_span)
              tuple_id = @arena.add_typed(TupleLiteralNode.new(tuple_span, values))

              value_span = node_span(tuple_id)
              return_span = return_token.span.cover(value_span)
              @arena.add_typed(ReturnNode.new(return_span, tuple_id))
            else
              value_span = node_span(first_value)
              return_span = return_token.span.cover(value_span)
              @arena.add_typed(ReturnNode.new(return_span, first_value))
            end
          end
        end

        # (removed duplicate parse_raise; unified earlier implementation supports no-parens calls and named args)

        # Phase 12: Parse break expression
        # Grammar: break [value]
        private def parse_break : ExprId
          break_token = current_token
          advance
          skip_trivia

          # Check if there's a break value
          # break without value if: newline, EOF, end, else, elsif, postfix modifiers
          token = current_token
          if token.kind.in?(Token::Kind::Newline, Token::Kind::EOF, Token::Kind::End, Token::Kind::Else, Token::Kind::Elsif, Token::Kind::If, Token::Kind::Unless, Token::Kind::While, Token::Kind::Until)
            # Break without value (returns nil from loop)
            @arena.add_typed(BreakNode.new(break_token.span, nil))
          else
            # Break with value
            value = parse_expression(0)
            return PREFIX_ERROR if value.invalid?

            value_span = node_span(value)
            break_span = break_token.span.cover(value_span)

            @arena.add_typed(BreakNode.new(break_span, value))
          end
        end

        # Phase 12: Parse next expression
        # Grammar: next
        private def parse_next : ExprId
          next_token = current_token
          advance

          # Next has no value in Crystal
          @arena.add_typed(NextNode.new(next_token.span))
        end

        # Phase 10: Parse yield expression
        # Grammar: yield | yield(arg1, arg2, ...) | yield arg1, arg2, ...
        private def parse_yield : ExprId
          yield_token = current_token
          advance
          skip_trivia

          # Parenthesized argument list: yield(...)
          if current_token.kind == Token::Kind::LParen
            advance  # consume (
            skip_trivia

            args_b = SmallVec(ExprId, 2).new

            # Empty yield: yield()
            if current_token.kind == Token::Kind::RParen
              rparen = current_token
              advance
              return @arena.add_typed(YieldNode.new(yield_token.span.cover(rparen.span), [] of ExprId))
            end

            loop do
              if current_token.kind == Token::Kind::Star
                star_token = current_token
                advance
                skip_trivia
                value = parse_expression(0)
                return PREFIX_ERROR if value.invalid?
                span = star_token.span.cover(@arena[value].span)
                arg = @arena.add_typed(SplatNode.new(span, value))
              else
                arg = parse_expression(0)
              end
              return PREFIX_ERROR if arg.invalid?
              args_b << arg

              skip_trivia
              break if current_token.kind != Token::Kind::Comma
              advance
              skip_trivia
            end

            unless current_token.kind == Token::Kind::RParen
              emit_unexpected(current_token)
              return PREFIX_ERROR
            end
            rparen = current_token
            advance
            args = args_b.to_a
            last_span = args.empty? ? rparen.span : node_span(args.last)
            return @arena.add_typed(YieldNode.new(yield_token.span.cover(last_span), args))
          end

          # Non-parenthesized arguments: yield arg1, arg2, ...
          token = current_token
          if token.kind.in?(Token::Kind::Newline, Token::Kind::EOF, Token::Kind::End, Token::Kind::Else, Token::Kind::Elsif, Token::Kind::If, Token::Kind::Unless, Token::Kind::While, Token::Kind::Until, Token::Kind::Do, Token::Kind::RBrace)
            # Yield without args
            @arena.add_typed(YieldNode.new(yield_token.span, nil))
          else
            args_b = SmallVec(ExprId, 2).new
            loop do
              if current_token.kind == Token::Kind::Star
                star_token = current_token
                advance
                skip_trivia
                value = parse_expression(0)
                return PREFIX_ERROR if value.invalid?
                span = star_token.span.cover(@arena[value].span)
                arg = @arena.add_typed(SplatNode.new(span, value))
              else
                arg = parse_expression(0)
              end
              return PREFIX_ERROR if arg.invalid?
              args_b << arg

              skip_trivia
              break if current_token.kind != Token::Kind::Comma
              advance
              skip_whitespace_and_optional_newlines
            end
            args = args_b.to_a
            last_arg_span = args.empty? ? yield_token.span : node_span(args.last)
            yield_span = yield_token.span.cover(last_arg_span)
            @arena.add_typed(YieldNode.new(yield_span, args))
          end
        end

        # Phase 39: Parse super (call parent method)
        # Grammar: super | super() | super(arg1, arg2, ...)
        private def parse_super : ExprId
          super_token = current_token
          advance
          skip_trivia

          # Check if there are parentheses
          token = current_token
          if token.kind == Token::Kind::LParen
            # Explicit argument list: super() or super(args)
            advance  # consume (
            skip_trivia

            args_b = SmallVec(ExprId, 2).new

            # Check for empty parens: super()
            if current_token.kind == Token::Kind::RParen
              rparen_token = current_token
              advance  # consume )
              return @arena.add_typed(
                SuperNode.new(
                  super_token.span.cover(rparen_token.span),
                  args_b.to_a  # Empty array = explicit no args
                )
              )
            end

            # Parse arguments
            loop do
              # Disable type declarations to allow identifier: syntax for named args
              @no_type_declaration += 1
              arg = parse_expression(0)
              @no_type_declaration -= 1
              return PREFIX_ERROR if arg.invalid?
              args_b << arg

              skip_trivia
              break if current_token.kind != Token::Kind::Comma

              advance  # consume comma
              skip_trivia
            end

            # Expect closing paren
            unless current_token.kind == Token::Kind::RParen
              emit_unexpected(current_token)
              return PREFIX_ERROR
            end
            rparen_token = current_token
            advance

            @arena.add_typed(
              SuperNode.new(
                super_token.span.cover(rparen_token.span),
                args_b.to_a
              )
            )
          else
            # No parentheses: super (implicit - pass all method args)
            @arena.add_typed(
              SuperNode.new(
                super_token.span,
                nil  # nil = implicit args (pass all)
              )
            )
          end
        end

        # Phase 96: Parse previous_def (call previous definition before reopening/redefining)
        # Grammar: previous_def | previous_def() | previous_def(arg1, arg2, ...)
        private def parse_previous_def : ExprId
          previous_def_token = current_token
          advance
          skip_trivia

          # Check if there are parentheses
          token = current_token
          if token.kind == Token::Kind::LParen
            # Explicit argument list: previous_def() or previous_def(args)
            advance  # consume (
            skip_trivia

            args_b = SmallVec(ExprId, 2).new

            # Check for empty parens: previous_def()
            if current_token.kind == Token::Kind::RParen
              rparen_token = current_token
              advance  # consume )
              return @arena.add_typed(
                PreviousDefNode.new(
                  previous_def_token.span.cover(rparen_token.span),
                  args_b.to_a  # Empty array = explicit no args
                )
              )
            end

            # Parse arguments
            loop do
              # Disable type declarations to allow identifier: syntax for named args
              @no_type_declaration += 1
              arg = parse_expression(0)
              @no_type_declaration -= 1
              return PREFIX_ERROR if arg.invalid?
              args_b << arg

              skip_trivia
              break if current_token.kind != Token::Kind::Comma

              advance  # consume comma
              skip_trivia
            end

            # Expect closing paren
            unless current_token.kind == Token::Kind::RParen
              emit_unexpected(current_token)
              return PREFIX_ERROR
            end
            rparen_token = current_token
            advance

            @arena.add_typed(
              PreviousDefNode.new(
                previous_def_token.span.cover(rparen_token.span),
                args_b.to_a
              )
            )
          else
            # No parentheses: previous_def (implicit - pass all method args)
            @arena.add_typed(
              PreviousDefNode.new(
                previous_def_token.span,
                nil  # nil = implicit args (pass all)
              )
            )
          end
        end

        # Phase 40: Parse typeof (type introspection)
        # Grammar: typeof(expr) | typeof(expr1, expr2, ...)
        private def parse_typeof : ExprId
          typeof_token = current_token
          advance
          skip_trivia

          # Expect opening parenthesis
          unless current_token.kind == Token::Kind::LParen
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end
          advance  # consume (
          skip_trivia

          args_b = SmallVec(ExprId, 2).new

          # Parse at least one argument (expression or type annotation like Void*)
          loop do
            # Try to parse a type annotation (e.g., Void*, LibC::X, {Int32, Int32})
            type_start = current_token
            type_slice = parse_type_annotation
            if previous_token && (type_start.span != current_token.span)
              # Consumed something as type; wrap as IdentifierNode carrying the slice
              arg = @arena.add_typed(IdentifierNode.new(type_start.span.cover(previous_token.not_nil!.span), @string_pool.intern(type_slice)))
            else
              # Fallback to expression parsing
              arg = parse_expression(0)
              return PREFIX_ERROR if arg.invalid?
            end
            args_b << arg

            skip_trivia
            break if current_token.kind != Token::Kind::Comma

            advance  # consume comma
            skip_trivia
          end

          # Expect closing parenthesis
          unless current_token.kind == Token::Kind::RParen
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end
          rparen_token = current_token
          advance

          @arena.add_typed(
            TypeofNode.new(
              typeof_token.span.cover(rparen_token.span),
              args_b.to_a
            )
          )
        end

        # Phase 41: Parse sizeof (size in bytes)
        # Grammar: sizeof(type) | sizeof(expr) | sizeof(arg1, arg2, ...)
        # Each arg may be either a type annotation (e.g., Void*, LibC::X, {Int32, Int32})
        # or a regular expression. This mirrors typeof() parsing strategy but
        # materializes a SizeofNode.
        private def parse_sizeof : ExprId
          sizeof_token = current_token
          advance
          skip_trivia

          # Expect opening parenthesis
          unless current_token.kind == Token::Kind::LParen
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end
          advance  # consume (
          skip_trivia

          args_b = SmallVec(ExprId, 2).new

          # Parse at least one argument (type-or-expr)
          loop do
            # Try to parse a type annotation first
            type_start = current_token
            type_slice = parse_type_annotation
            if previous_token && (type_start.span != current_token.span)
              # Consumed something as type; wrap as IdentifierNode carrying the slice
              arg = @arena.add_typed(IdentifierNode.new(type_start.span.cover(previous_token.not_nil!.span), @string_pool.intern(type_slice)))
            else
              # Fallback to expression parsing
              arg = parse_expression(0)
              return PREFIX_ERROR if arg.invalid?
            end
            args_b << arg

            skip_trivia
            break if current_token.kind != Token::Kind::Comma

            advance  # consume comma
            skip_trivia
          end

          # Expect closing parenthesis
          unless current_token.kind == Token::Kind::RParen
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end
          rparen_token = current_token
          advance

          @arena.add_typed(
            SizeofNode.new(
              sizeof_token.span.cover(rparen_token.span),
              args_b.to_a
            )
          )
        end

        # Phase 42: Parse pointerof (pointer to variable/expression)
        # Grammar: pointerof(expr)
        private def parse_pointerof : ExprId
          pointerof_token = current_token
          advance
          skip_trivia

          # Expect opening parenthesis
          unless current_token.kind == Token::Kind::LParen
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end
          advance  # consume (
          skip_trivia

          args_b = SmallVec(ExprId, 2).new

          # Parse at least one argument
          loop do
            arg = parse_expression(0)
            return PREFIX_ERROR if arg.invalid?
            args_b << arg

            skip_trivia
            break if current_token.kind != Token::Kind::Comma

            advance  # consume comma
            skip_trivia
          end

          # Expect closing parenthesis
          unless current_token.kind == Token::Kind::RParen
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end
          rparen_token = current_token
          advance

          @arena.add_typed(
            PointerofNode.new(
              pointerof_token.span.cover(rparen_token.span),
              args_b.to_a
            )
          )
        end

        # Phase 85: Parse uninitialized (uninitialized variable)
        # Grammar: uninitialized(Type)
        private def parse_uninitialized : ExprId
          uninitialized_token = current_token
          advance
          skip_trivia

          # Expect opening parenthesis
          unless current_token.kind == Token::Kind::LParen
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end
          advance  # consume (
          skip_trivia

          # Parse type expression (single argument)
          type_expr = parse_expression(0)
          return PREFIX_ERROR if type_expr.invalid?

          skip_trivia

          # Expect closing parenthesis
          unless current_token.kind == Token::Kind::RParen
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end
          rparen_token = current_token
          advance

          @arena.add_typed(
            UninitializedNode.new(
              uninitialized_token.span.cover(rparen_token.span),
              type_expr
            )
          )
        end

        # Phase 86: Parse offsetof (field offset in type)
        # Grammar: offsetof(Type, :field)
        private def parse_offsetof : ExprId
          offsetof_token = current_token
          advance
          skip_trivia

          # Expect opening parenthesis
          unless current_token.kind == Token::Kind::LParen
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end
          advance  # consume (
          skip_trivia

          # Parse first argument (type)
          type_arg = parse_expression(0)
          return PREFIX_ERROR if type_arg.invalid?

          skip_trivia

          # Expect comma
          unless current_token.kind == Token::Kind::Comma
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end
          advance  # consume comma
          skip_trivia

          # Parse second argument (field)
          field_arg = parse_expression(0)
          return PREFIX_ERROR if field_arg.invalid?

          skip_trivia

          # Expect closing parenthesis
          unless current_token.kind == Token::Kind::RParen
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end
          rparen_token = current_token
          advance

          @arena.add_typed(
            OffsetofNode.new(
              offsetof_token.span.cover(rparen_token.span),
              [type_arg, field_arg]
            )
          )
        end

        # Phase 88: Parse alignof (ABI alignment in bytes)
        # Grammar: alignof(Type)
        private def parse_alignof : ExprId
          alignof_token = current_token
          advance
          skip_trivia

          # Expect opening parenthesis
          unless current_token.kind == Token::Kind::LParen
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end
          advance  # consume (
          skip_trivia

          args_b = SmallVec(ExprId, 2).new

          # Parse at least one argument
          loop do
            arg = parse_expression(0)
            return PREFIX_ERROR if arg.invalid?
            args_b << arg

            skip_trivia
            break if current_token.kind != Token::Kind::Comma

            advance  # consume comma
            skip_trivia
          end

          # Expect closing parenthesis
          unless current_token.kind == Token::Kind::RParen
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end
          rparen_token = current_token
          advance

          @arena.add_typed(
            AlignofNode.new(
              alignof_token.span.cover(rparen_token.span),
              args_b.to_a
            )
          )
        end

        # Phase 88: Parse instance_alignof (instance alignment)
        # Grammar: instance_alignof(Type)
        private def parse_instance_alignof : ExprId
          instance_alignof_token = current_token
          advance
          skip_trivia

          # Expect opening parenthesis
          unless current_token.kind == Token::Kind::LParen
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end
          advance  # consume (
          skip_trivia

          args_b = SmallVec(ExprId, 2).new

          # Parse at least one argument
          loop do
            arg = parse_expression(0)
            return PREFIX_ERROR if arg.invalid?
            args_b << arg

            skip_trivia
            break if current_token.kind != Token::Kind::Comma

            advance  # consume comma
            skip_trivia
          end

          # Expect closing parenthesis
          unless current_token.kind == Token::Kind::RParen
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end
          rparen_token = current_token
          advance

          @arena.add_typed(
            InstanceAlignofNode.new(
              instance_alignof_token.span.cover(rparen_token.span),
              args_b.to_a
            )
          )
        end

        # Phase 95: Parse asm (inline assembly)
        # Grammar: asm(template, args...)
        # Phase 95A: Parser-only - parse all arguments as expressions
        # Later phases will interpret colon-separated sections
        private def parse_asm : ExprId
          asm_token = current_token
          advance
          skip_trivia

          # Expect opening parenthesis
          unless current_token.kind == Token::Kind::LParen
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end
          advance  # consume (
          skip_trivia

          args_b = SmallVec(ExprId, 4).new

          # Parse at least one argument (template string)
          loop do
            arg = parse_expression(0)
            return PREFIX_ERROR if arg.invalid?
            args_b << arg

            skip_trivia
            break if current_token.kind != Token::Kind::Comma

            advance  # consume comma
            skip_trivia
          end

          # Expect closing parenthesis
          unless current_token.kind == Token::Kind::RParen
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end
          rparen_token = current_token
          advance

          @arena.add_typed(
            AsmNode.new(
              asm_token.span.cover(rparen_token.span),
              args_b.to_a
            )
          )
        end

        # Phase 98: Parse out keyword (output parameter)
        # Grammar: out identifier | out @ivar | out @@cvar | out $gvar
        private def parse_out : ExprId
          out_token = current_token
          advance
          skip_trivia

          # Accept identifier/instance/class/global variable as out target
          id_token = current_token
          case id_token.kind
          when Token::Kind::Identifier, Token::Kind::InstanceVar, Token::Kind::ClassVar, Token::Kind::GlobalVar
            advance
            @arena.add_typed(
              OutNode.new(
                out_token.span.cover(id_token.span),
                id_token.slice
              )
            )
          else
            emit_unexpected(id_token)
            return PREFIX_ERROR
          end
        end

        # Phase 10: Parse block
        # Grammar: { |params| body } or do |params| body end
        private def parse_block : ExprId
          is_brace_form = current_token.kind == Token::Kind::LBrace
          start_token = current_token
          advance  # consume { or do
          skip_trivia

          # Parse optional block parameters: |x, y|, supports splat: |*kv|
          params_b = SmallVec(Parameter, 2).new
          if current_token.kind == Token::Kind::Pipe
            advance  # consume opening |
            skip_trivia

            # Parse parameter list
            loop do
              is_splat = false
              is_double_splat = false

              # Optional splat markers
              if current_token.kind == Token::Kind::StarStar
                is_double_splat = true
                advance
                skip_trivia
              elsif current_token.kind == Token::Kind::Star
                is_splat = true
                advance
                skip_trivia
              end

              # Tuple destructuring: |(a, b)|
              if current_token.kind == Token::Kind::LParen
                advance
                skip_trivia
                loop do
                  name_token = current_token
                  unless name_token.kind == Token::Kind::Identifier
                    emit_unexpected(name_token)
                    return PREFIX_ERROR
                  end
                  # Create simple parameter for destructured identifier
                  param_name = name_token.slice
                  param_name_span = name_token.span
                  param_span = name_token.span
                  advance
                  params_b << Parameter.new(
                    param_name,
                    nil,
                    nil,
                    nil,
                    param_span,
                    param_name_span,
                    nil,
                    nil,
                    nil,
                    is_splat,
                    is_double_splat,
                    false,
                    false
                  )
                  skip_trivia
                  break if current_token.kind == Token::Kind::RParen
                  unless current_token.kind == Token::Kind::Comma
                    emit_unexpected(current_token)
                    return PREFIX_ERROR
                  end
                  advance
                  skip_trivia
                end
                advance  # consume )
                skip_trivia
                # After tuple destructure, continue to comma or pipe handling below
                # (skip type annotation logic for destructured names)
                # fallthrough to trailing comma/pipe handling
              else
                name_token = current_token
                unless name_token.kind == Token::Kind::Identifier
                  emit_unexpected(name_token)
                  return PREFIX_ERROR
                end

                param_name = name_token.slice  # TIER 2.1: Zero-copy slice
                param_name_span = name_token.span
                param_span = name_token.span
                advance
                skip_whitespace_and_optional_newlines

              # Support optional type annotation in block params: |x : Type|
              type_annotation : Slice(UInt8)? = nil
              param_type_span : Span? = nil
              if current_token.kind == Token::Kind::Colon
                advance
                skip_trivia
                type_start_token = current_token
                parsed_type = parse_type_annotation
                unless parsed_type.nil?
                  type_annotation = parsed_type
                  type_end_token = previous_token
                  if type_end_token
                    param_type_span = type_start_token.span.cover(type_end_token.span)
                    param_span = param_span.cover(param_type_span)
                  else
                    param_type_span = type_start_token.span
                    param_span = param_span.cover(param_type_span)
                  end
                else
                  emit_unexpected(type_start_token)
                end
                skip_trivia
              end

              params_b << Parameter.new(
                param_name,
                nil,              # Phase 103K: no external name for block params
                type_annotation,  # optional type annotation
                nil,              # no default value
                param_span,       # full span covers name and optional type
                param_name_span,  # name span
                nil,              # Phase 103K: no external name span
                param_type_span,  # optional type span
                nil,              # no default span
                is_splat,
                is_double_splat,
                false,            # is_block flag: block params are not '&'
                false             # is_instance_var
              )
              end

              # Check for comma or closing |
              if current_token.kind == Token::Kind::Comma
                advance
                skip_trivia
              elsif current_token.kind == Token::Kind::Pipe
                break
              else
                emit_unexpected(current_token)
                return PREFIX_ERROR
              end
            end

            advance  # consume closing |
            skip_trivia
          end

          # Parse block body
          body_b = SmallVec(ExprId, 4).new
          loop do
            skip_trivia

            # Skip newlines in block body
            while current_token.kind == Token::Kind::Newline
              advance
              skip_trivia
            end

            # Special-case: typed empty hash literal inside block: {} of K => V
            if current_token.kind == Token::Kind::LBrace
              save_idx = @index
              lbrace_tok = current_token
              advance
              if current_token.kind == Token::Kind::RBrace
                advance
                skip_trivia
                if current_token.kind == Token::Kind::Of || (current_token.kind == Token::Kind::Identifier && slice_eq?(current_token.slice, "of"))
                  advance
                  skip_trivia
                  key_type = parse_type_annotation
                  skip_trivia
                  if current_token.kind == Token::Kind::Arrow || (current_token.kind == Token::Kind::Operator && slice_eq?(current_token.slice, "=>"))
                    advance
                    skip_trivia
                    value_type = parse_type_annotation
                    # Build node and append
                    span = lbrace_tok.span
                    hash_node = @arena.add_typed(HashLiteralNode.new(span, [] of HashEntry, key_type, value_type))
                    body_b << hash_node
                    next
                  end
                end
              end
              # Not a typed empty hash; rewind and parse normally
              @index = save_idx
            end

            # Check for block terminator
            if is_brace_form
              break if current_token.kind == Token::Kind::RBrace
            else
              break if current_token.kind == Token::Kind::End
            end

            break if current_token.kind == Token::Kind::EOF

            stmt = parse_statement
            return PREFIX_ERROR if stmt.invalid?
            body_b << stmt
            # Allow semicolons/newlines between statements inside blocks
            skip_statement_end
          end

          # Consume closing delimiter
          end_token = current_token
          unless (is_brace_form && current_token.kind == Token::Kind::RBrace) ||
                 (!is_brace_form && current_token.kind == Token::Kind::End)
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end
          advance

          block_span = start_token.span.cover(end_token.span)
          @arena.add_typed(BlockNode.new(
            block_span,
            params_b.to_a,
            body_b.to_a
          ))
        end

        # Phase 74: Parse proc literal: ->(x : Int32) : Int32 { x + 1 }
        private def parse_proc_literal : ExprId
          arrow_token = current_token
          advance  # consume ->
          skip_trivia

          # Parse optional parameters: (x : Type, y : Type)
          params_b = SmallVec(Parameter, 2).new
          return_type : Slice(UInt8)? = nil

          if current_token.kind == Token::Kind::LParen
            advance  # consume (
            skip_trivia

            # Parse parameter list
            unless current_token.kind == Token::Kind::RParen
              loop do
                name_token = current_token
                unless name_token.kind == Token::Kind::Identifier
                  emit_unexpected(name_token)
                  return PREFIX_ERROR
                end

                param_name = name_token.slice  # TIER 2.1: Zero-copy slice
                param_name_span = name_token.span
                param_span = name_token.span
                advance
                skip_trivia

                # Parse optional type annotation: : Type
                type_annotation : Slice(UInt8)? = nil  # TIER 2.1: Zero-copy slice
                type_span : Span? = nil
                if current_token.kind == Token::Kind::Colon
                  advance  # consume :
                  skip_trivia

                  type_start_span = current_token.span
                  type_annotation = parse_type_annotation
                  if type_annotation
                    type_span = type_start_span.cover(previous_token.not_nil!.span)
                    param_span = param_span.cover(type_span)
                  else
                    emit_unexpected(current_token)
                    return PREFIX_ERROR
                  end
                end

                params_b << Parameter.new(
                  param_name,
                  nil,              # Phase 103K: no external name for fun params
                  type_annotation,
                  nil,              # no default value
                  param_span,
                  param_name_span,
                  nil,              # Phase 103K: no external name span
                  type_span,
                  nil               # no default span
                )

                # Check for comma or closing )
                if current_token.kind == Token::Kind::Comma
                  advance
                  skip_trivia
                elsif current_token.kind == Token::Kind::RParen
                  break
                else
                  emit_unexpected(current_token)
                  return PREFIX_ERROR
                end
              end
            end

            advance  # consume )
            skip_trivia
          end

          # Parse optional return type: : ReturnType
          if current_token.kind == Token::Kind::Colon
            advance  # consume :
            skip_trivia

            return_type = parse_type_annotation
            unless return_type
              emit_unexpected(current_token)
              return PREFIX_ERROR
            end
          end

          # Skip any trivia/newlines before body
          skip_statement_end

          # Parse body: { } or do...end
          is_brace_form = current_token.kind == Token::Kind::LBrace
          is_do_form = current_token.kind == Token::Kind::Do

          unless is_brace_form || is_do_form
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end
          start_token = current_token
          advance  # consume { or do
          skip_statement_end  # Skip newlines after { or do

          # Parse proc body - following original parser's parse_expressions pattern
          # Use parse_statement (equivalent to parse_multi_assign) to handle assignments
          body_b = SmallVec(ExprId, 4).new

          # Check for empty body
          if (is_brace_form && current_token.kind == Token::Kind::RBrace) ||
             (!is_brace_form && current_token.kind == Token::Kind::End)
            # Empty body is allowed
          else
            # Parse first statement (handles assignments)
            stmt = parse_statement
            if stmt.invalid?
              return PREFIX_ERROR
            end
            body_b << stmt

            # Skip statement end (newlines + semicolons)
            skip_statement_end

            # Check if we have more statements
            unless (is_brace_form && current_token.kind == Token::Kind::RBrace) ||
                   (!is_brace_form && current_token.kind == Token::Kind::End) ||
                   current_token.kind == Token::Kind::EOF
              # Parse remaining statements in loop
              loop do
                stmt = parse_statement
                if stmt.invalid?
                  return PREFIX_ERROR
                end
                body_b << stmt

                # Skip statement end
                skip_statement_end

                # Break if we hit terminator
                break if (is_brace_form && current_token.kind == Token::Kind::RBrace) ||
                         (!is_brace_form && current_token.kind == Token::Kind::End) ||
                         current_token.kind == Token::Kind::EOF
              end
            end
          end

          # Skip statement end before closing delimiter (handles newlines)
          skip_statement_end

          # Consume closing delimiter
          end_token = current_token
          unless (is_brace_form && current_token.kind == Token::Kind::RBrace) ||
                 (!is_brace_form && current_token.kind == Token::Kind::End)
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end
          advance

          proc_span = arrow_token.span.cover(end_token.span)
          @arena.add_typed(ProcLiteralNode.new(
            proc_span,
            params_b.to_a,
            return_type,
            body_b.to_a
          ))
        end

        # Phase 10: Attach block to method call
        private def attach_block_to_call(call_expr : ExprId) : ExprId
          # Parse the block
          block_id = parse_block
          return PREFIX_ERROR if block_id.invalid?

          # Get the call node
          call_node = @arena[call_expr]
          block_span = node_span(block_id)
          call_span = call_node.span.cover(block_span)

          # If it's an identifier, convert it to a call (e.g., "three_times do |n| ... end")
          if Frontend.node_kind(call_node) == Frontend::NodeKind::Identifier
            return @arena.add_typed(
              CallNode.new(
                call_span,
                call_expr,
                Array(ExprId).new(0),
                block_id
              )
            )
          end

          # Verify it's a Call or MemberAccess
          unless Frontend.node_kind(call_node).in?(Frontend::NodeKind::Call, Frontend::NodeKind::MemberAccess)
            @diagnostics << Diagnostic.new("Block can only be attached to method call or identifier", call_node.span)
            return PREFIX_ERROR
          end

          case call_node
          when CallNode
            @arena.add_typed(
              CallNode.new(
                call_span,
                call_node.callee,
                call_node.args,
                block_id,
                call_node.named_args
              )
            )
          when MemberAccessNode
            @arena.add_typed(
              CallNode.new(
                call_span,
                call_expr,
                Array(ExprId).new(0),
                block_id
              )
            )
          else
            @diagnostics << Diagnostic.new("Block attachment to unsupported node", call_node.span)
            PREFIX_ERROR
          end
        end

        # Phase 6: Handle postfix if modifier
        # Grammar: <statement> if <condition>
        # Phase 26: Parse postfix if/unless modifiers
        # Phase 27: Parse postfix while/until modifiers
        # Supports: stmt if condition, stmt unless condition, stmt while condition, stmt until condition
        private def parse_postfix_if_modifier(stmt : ExprId) : ExprId
          # Postfix modifiers must be on the SAME line as the statement
          # If there's a newline between them, the modifier is a separate statement
          stmt_end_line = node_span(stmt).end_line

          skip_trivia
          token = current_token

          # If modifier is on a different line, it's NOT a postfix modifier
          if token.span.start_line > stmt_end_line
            return stmt
          end

          # Check for postfix if
          if token.kind == Token::Kind::If
            advance  # consume 'if'
            skip_trivia

            # Parse condition
            condition = parse_op_assign
            return PREFIX_ERROR if condition.invalid?

            # Wrap statement in an if node
            stmt_span = node_span(stmt)
            condition_span = node_span(condition)
            if_span = stmt_span.cover(condition_span)

            return @arena.add_typed(
              IfNode.new(
                if_span,
                condition,
                [stmt],
                nil,
                Array(ExprId).new(0)
              )
            )
          end

          # Phase 26: Check for postfix unless
          if token.kind == Token::Kind::Unless
            advance  # consume 'unless'
            skip_trivia

            # Parse condition
            condition = parse_op_assign
            return PREFIX_ERROR if condition.invalid?

            # Wrap statement in an unless node
            stmt_span = node_span(stmt)
            condition_span = node_span(condition)
            unless_span = stmt_span.cover(condition_span)

            return @arena.add_typed(
              UnlessNode.new(
                unless_span,
                condition,
                [stmt],
                Array(ExprId).new(0)
              )
            )
          end

          # Phase 27: Check for postfix while
          if token.kind == Token::Kind::While
            advance  # consume 'while'
            skip_trivia

            # Parse condition
            condition = parse_op_assign
            return PREFIX_ERROR if condition.invalid?

            # Wrap statement in a while node
            stmt_span = node_span(stmt)
            condition_span = node_span(condition)
            while_span = stmt_span.cover(condition_span)

            return @arena.add_typed(
              WhileNode.new(
                while_span,
                condition,
                [stmt]
              )
            )
          end

          # Phase 27: Check for postfix until
          if token.kind == Token::Kind::Until
            advance  # consume 'until'
            skip_trivia

            # Parse condition
            condition = parse_op_assign
            return PREFIX_ERROR if condition.invalid?

            # Wrap statement in an until node
            stmt_span = node_span(stmt)
            condition_span = node_span(condition)
            until_span = stmt_span.cover(condition_span)

            return @arena.add_typed(
              UntilNode.new(
                until_span,
                condition,
                [stmt]
              )
            )
          end

          # No postfix modifier, return statement as-is
          stmt
        end

        # Phase 32: Modified to support both class and struct
        # Phase 36: Modified to support abstract modifier
        private def parse_class(is_struct : Bool = false, is_union : Bool = false, is_abstract : Bool = false) : ExprId
          class_token = current_token
          advance
          skip_trivia

          name_token = current_token
          unless name_token.kind == Token::Kind::Identifier
            emit_unexpected(name_token)
            return PREFIX_ERROR
          end
          advance
          # Support namespaced module: module Foo::Bar
          while current_token.kind == Token::Kind::ColonColon
            advance
            skip_trivia
            if current_token.kind != Token::Kind::Identifier
              emit_unexpected(current_token)
              return PREFIX_ERROR
            end
            name_token = current_token
            advance
          end

          # Phase 61: Parse optional type parameters: (T, K, V)
          skip_trivia
          type_params = parse_type_parameters

          # Parse optional superclass: < SuperClass
          skip_trivia
          super_name_token = nil
          if current_token.kind == Token::Kind::Less
            advance  # Skip <
            skip_trivia
            super_name_token = current_token
            unless super_name_token.kind == Token::Kind::Identifier
              emit_unexpected(super_name_token)
              return PREFIX_ERROR
            end
            advance
          end

          # Allow separators after header (supports one-liners like `struct X; end`)
          skip_statement_end

          body_ids_b = SmallVec(ExprId, 4).new
          loop do
            skip_trivia
            token = current_token
            break if token.kind == Token::Kind::End
            break if token.kind == Token::Kind::EOF

            # Phase 5C: Instance variable declaration (@var : Type)
            # Check for type declaration vs assignment by looking at next non-trivia token
            if token.kind == Token::Kind::InstanceVar
              # Find next non-trivia token to check if it's a colon
              next_token = peek_next_non_trivia
              if next_token.kind == Token::Kind::Colon
                # It's a type declaration: @var : Type
                expr = parse_instance_var_decl
              else
                # It's an assignment or expression: @var = value
                expr = parse_statement
              end
            # Phase 77: Class variable declaration (@@var : Type)
            # Check for type declaration vs assignment by looking at next non-trivia token
            elsif token.kind == Token::Kind::ClassVar
              # Find next non-trivia token to check if it's a colon
              next_token = peek_next_non_trivia
              if next_token.kind == Token::Kind::Colon
                # It's a type declaration: @@var : Type
                expr = parse_class_var_decl
              else
                # It's an assignment or expression: @@var = value
                expr = parse_statement
              end
            # Phase 92: Annotation definition
            elsif token.kind == Token::Kind::Annotation
              expr = parse_annotation_def
            # Phase 30: Accessor macros (getter/setter/property)
            elsif token.kind == Token::Kind::Identifier
              # Check if it's getter, setter, or property
              # Peek ahead to confirm - next token should be identifier or keyword identifier
              next_token = peek_next_non_trivia
              is_accessor = (next_token.kind == Token::Kind::Identifier || is_keyword_identifier?(next_token))

              if is_accessor && accessor_macro_callee?(token)
                name = token.slice
                base = if name.size > 0 && name[name.size - 1] == '?'.ord.to_u8
                  Slice.new(name.to_unsafe, name.size - 1)
                else
                  name
                end
                kind = if slice_eq?(base, "getter") || slice_eq?(base, "class_getter")
                  :getter
                elsif slice_eq?(base, "setter") || slice_eq?(base, "class_setter")
                  :setter
                else
                  :property
                end
                expr = parse_accessor_macro(kind)
              elsif definition_start?
                expr = case current_token.kind
                  when Token::Kind::Def
                    parse_def
                  when Token::Kind::Fun
                    parse_fun
                  when Token::Kind::Class
                    parse_class
                  when Token::Kind::Module
                    parse_module
                  when Token::Kind::Struct
                    parse_struct
                  when Token::Kind::Union
                    parse_union
                  when Token::Kind::Enum
                    parse_enum
                  when Token::Kind::Alias
                    parse_alias
                  when Token::Kind::Annotation
                    # Phase 92: annotation definition
                    parse_annotation_def
                  when Token::Kind::Abstract
                    parse_abstract
                  when Token::Kind::Private
                    parse_private
                  when Token::Kind::Protected
                    parse_protected
                  when Token::Kind::Lib
                    parse_lib
                  else
                    # Phase 5B: Use parse_statement for assignments
                    parse_statement
                  end
              else
                # Phase 5B: Regular statement
                expr = parse_statement
              end
            elsif definition_start?
              # Handle embedded definitions regardless of starting token (def/class/struct/etc.)
              expr = case current_token.kind
                when Token::Kind::Def      then parse_def
                when Token::Kind::Fun      then parse_fun
                when Token::Kind::Class    then parse_class
                when Token::Kind::Module   then parse_module
                when Token::Kind::Struct   then parse_struct
                when Token::Kind::Union    then parse_union
                when Token::Kind::Enum     then parse_enum
                when Token::Kind::Alias    then parse_alias
                when Token::Kind::Annotation then parse_annotation_def
                when Token::Kind::Abstract then parse_abstract
                when Token::Kind::Private  then parse_private
                when Token::Kind::Protected then parse_protected
                when Token::Kind::Lib      then parse_lib
                else
                  parse_statement
                end
            else
              # Phase 5B: Use parse_statement for assignments
              expr = parse_statement
            end
            body_ids_b << expr unless expr.invalid?
            # Allow separators between members
            skip_statement_end
          end

          expect_identifier("end")
          end_token = previous_token
          consume_newlines

          class_span = if end_token
            class_token.span.cover(end_token.span)
          else
            class_token.span
          end

          # Phase 32: Choose kind based on is_struct flag
          # Phase 97: Choose kind based on is_union flag
          # Note: kind preserved in ClassNode via is_struct/is_union flags
          @arena.add_typed(
            ClassNode.new(
              class_span,
              name_token.slice,
              super_name_token.try(&.slice),
              body_ids_b.to_a,
              is_abstract,
              is_struct,
              is_union,
              type_params
            )
          )
        end

        # Phase 32: Parse struct definition
        # Grammar: struct Name < Parent ... end
        # Struct is syntactically identical to class, but represents a value type
        private def parse_struct : ExprId
          parse_class(is_struct: true)
        end

        # Phase 97: Parse union (C bindings union type)
        # Grammar: union Name ... end
        private def parse_union : ExprId
          parse_class(is_union: true)
        end

        # Phase 36: Parse abstract modifier
        # Grammar: abstract class Name ... end | abstract def method_name
        private def parse_abstract : ExprId
          abstract_token = current_token
          advance
          skip_trivia

          case current_token.kind
          when Token::Kind::Class
            parse_class(is_abstract: true)
          when Token::Kind::Struct
            parse_class(is_struct: true, is_abstract: true)
          when Token::Kind::Union
            parse_class(is_union: true, is_abstract: true)
          when Token::Kind::Def
            parse_def(is_abstract: true)
          else
            emit_unexpected(current_token)
            PREFIX_ERROR
          end
        end

        # Phase 37/103J: Parse private visibility modifier
        # Supports: private def, private CONST = ..., private @@var = ...
        # Following original parser design (parser.cr:6196-6219, 5498-5509)
        private def parse_private : ExprId
          visibility_token = current_token
          start_span = visibility_token.span
          advance  # Skip 'private'
          skip_trivia

          # Special case: private def (definition needs separate handling)
          if current_token.kind == Token::Kind::Def
            return parse_def(visibility: Visibility::Private)
          end

          if definition_start?
            node = case current_token.kind
              when Token::Kind::Class
                parse_class
              when Token::Kind::Module
                parse_module
              when Token::Kind::Struct
                parse_struct
              when Token::Kind::Union
                parse_union
              when Token::Kind::Enum
                parse_enum
              when Token::Kind::Alias
                parse_alias
              when Token::Kind::Annotation
                parse_annotation
              when Token::Kind::Lib
                parse_lib
              when Token::Kind::Fun
                parse_fun
              else
                PREFIX_ERROR
              end

            return node if node.invalid?

            full_span = start_span.cover(node_span(node))
            return @arena.add_typed(VisibilityModifierNode.new(full_span, Visibility::Private, node))
          end

          # Parse other expressions (constants, class vars, etc.)
          expr = parse_op_assign
          return PREFIX_ERROR if expr.invalid?

          # Wrap in VisibilityModifierNode
          full_span = start_span.cover(node_span(expr))
          @arena.add_typed(VisibilityModifierNode.new(full_span, Visibility::Private, expr))
        end

        # Phase 37/103J: Parse protected visibility modifier
        # Supports: protected def, protected CONST = ..., protected @@var = ...
        # Following original parser design (parser.cr:6196-6219, 5498-5509)
        private def parse_protected : ExprId
          visibility_token = current_token
          start_span = visibility_token.span
          advance  # Skip 'protected'
          skip_trivia

          # Special case: protected def (definition needs separate handling)
          if current_token.kind == Token::Kind::Def
            return parse_def(visibility: Visibility::Protected)
          end

          if definition_start?
            node = case current_token.kind
              when Token::Kind::Class
                parse_class
              when Token::Kind::Module
                parse_module
              when Token::Kind::Struct
                parse_struct
              when Token::Kind::Union
                parse_union
              when Token::Kind::Enum
                parse_enum
              when Token::Kind::Alias
                parse_alias
              when Token::Kind::Annotation
                parse_annotation
              when Token::Kind::Lib
                parse_lib
              when Token::Kind::Fun
                parse_fun
              else
                PREFIX_ERROR
              end

            return node if node.invalid?

            full_span = start_span.cover(node_span(node))
            return @arena.add_typed(VisibilityModifierNode.new(full_span, Visibility::Protected, node))
          end

          # Parse other expressions (constants, class vars, etc.)
          expr = parse_op_assign
          return PREFIX_ERROR if expr.invalid?

          # Wrap in VisibilityModifierNode
          full_span = start_span.cover(node_span(expr))
          @arena.add_typed(VisibilityModifierNode.new(full_span, Visibility::Protected, expr))
        end

        # Phase 38: Parse lib definition (C bindings)
        # Grammar: lib Name ... end
        private def parse_lib : ExprId
          lib_token = current_token
          advance
          skip_trivia

          name_token = current_token
          unless name_token.kind == Token::Kind::Identifier
            emit_unexpected(name_token)
            return PREFIX_ERROR
          end
          advance

          consume_newlines

          body_ids_b = SmallVec(ExprId, 4).new
          loop do
            skip_trivia
            token = current_token
            break if token.kind == Token::Kind::End
            break if token.kind == Token::Kind::EOF

            if token.kind == Token::Kind::InstanceVar
              next_token = peek_next_non_trivia
              if next_token.kind == Token::Kind::Colon
                expr = parse_instance_var_decl
              else
                expr = parse_statement
              end
            elsif token.kind == Token::Kind::ClassVar
              next_token = peek_next_non_trivia
              if next_token.kind == Token::Kind::Colon
                expr = parse_class_var_decl
              else
                expr = parse_statement
              end
            elsif token.kind == Token::Kind::Identifier && slice_eq?(token.slice, "type")
              expr = parse_lib_type_alias
            elsif definition_start?
              expr = case current_token.kind
                when Token::Kind::Def
                  parse_def
                when Token::Kind::Fun
                  parse_fun
                when Token::Kind::Class
                  parse_class
                when Token::Kind::Module
                  parse_module
                when Token::Kind::Struct
                  parse_struct
                when Token::Kind::Union
                  parse_union
                when Token::Kind::Enum
                  parse_enum
                when Token::Kind::Alias
                  parse_alias
                when Token::Kind::Annotation
                  # Phase 92: annotation definition
                  parse_annotation_def
                when Token::Kind::Abstract
                  parse_abstract
                when Token::Kind::Private
                  parse_private
                when Token::Kind::Protected
                  parse_protected
                else
                  parse_statement
                end
            else
              expr = parse_statement
            end
            body_ids_b << expr unless expr.invalid?
            consume_newlines
          end

          expect_identifier("end")
          end_token = previous_token
          consume_newlines

          lib_span = if end_token
            lib_token.span.cover(end_token.span)
          else
            lib_token.span
          end

          @arena.add_typed(
            LibNode.new(
              lib_span,
              name_token.slice,
              body_ids_b.to_a
            )
          )
        end

        private def parse_lib_type_alias : ExprId
          type_token = current_token
          advance  # consume 'type'
          skip_trivia

          name_token = current_token
          unless name_token.kind == Token::Kind::Identifier
            emit_unexpected(name_token)
            return PREFIX_ERROR
          end
          advance
          skip_trivia

          unless current_token.kind == Token::Kind::Eq
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end
          advance
          skip_trivia

          type_start_token = current_token
          type_slice = parse_type_annotation
          if type_slice.empty?
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end

          type_end_token = previous_token || type_start_token
          alias_span = type_token.span.cover(type_end_token.span)

          @arena.add_typed(
            AliasNode.new(
              alias_span,
              name_token.slice,
              type_slice
            )
          )
        end

        # Phase 33: Parse enum definition
        # Grammar: enum Name [: BaseType] ... end
        # Members: CONSTANT [= value]
        private def parse_enum : ExprId
          enum_token = current_token
          advance
          skip_trivia

          # Parse enum name
          name_token = current_token
          unless name_token.kind == Token::Kind::Identifier
            emit_unexpected(name_token)
            return PREFIX_ERROR
          end
          advance
          skip_trivia

          # Parse optional base type: : Type (supports namespaces/generics)
          base_type_slice : Slice(UInt8)? = nil
          if current_token.kind == Token::Kind::Colon
            advance  # consume ':'
            skip_trivia

            type_slice = parse_type_annotation
            if type_slice.empty?
              emit_unexpected(current_token)
              return PREFIX_ERROR
            end
            base_type_slice = type_slice

            skip_trivia
          end

          consume_newlines

          # Phase 103G: Parse enum members and methods
          members = [] of EnumMember
          method_bodies_b = SmallVec(ExprId, 4).new  # Store method/macro definitions

          loop do
            skip_trivia
            token = current_token
            break if token.kind == Token::Kind::End
            break if token.kind == Token::Kind::EOF

            # Allow macro control blocks inside enums (e.g., {% for %})
            if macro_control_start?
              macro_expr = parse_percent_macro_control
              method_bodies_b << macro_expr unless macro_expr.invalid?
              consume_newlines
              next
            end

            # Phase 103G: Check if this is a method/macro definition
            if definition_start?
              # Parse method or other definition inside enum
              definition_expr = case current_token.kind
              when Token::Kind::Def
                parse_def
              when Token::Kind::Macro
                parse_macro_definition
              else
                # Other definitions (class, module, etc.) - skip for now
                emit_unexpected(current_token)
                advance
                PREFIX_ERROR
              end

              unless definition_expr.invalid?
                method_bodies_b << definition_expr
              end

              consume_newlines
              next
            end

            # Enum members must be CONSTANT identifiers (start with uppercase)
            unless token.kind == Token::Kind::Identifier
              emit_unexpected(token)
              break
            end

            member_name_token = token
            member_name = member_name_token.slice  # TIER 2.3: Zero-copy slice
            member_name_span = member_name_token.span
            advance
            skip_trivia

            # Parse optional value: = expression
            member_value = nil
            member_value_span = nil
            if current_token.kind == Token::Kind::Eq
              advance  # consume '='
              skip_trivia

              value_expr = parse_expression(0)
              return PREFIX_ERROR if value_expr.invalid?
              member_value = value_expr
              member_value_span = node_span(value_expr)
              skip_trivia
            end

            members << EnumMember.new(
              member_name,
              member_value,
              member_name_span,
              member_value_span
            )

            consume_newlines
          end

          expect_identifier("end")
          end_token = previous_token
          consume_newlines

          enum_span = if end_token
            enum_token.span.cover(end_token.span)
          else
            enum_token.span
          end

          @arena.add_typed(
            EnumNode.new(
              enum_span,
              name_token.slice,
              base_type_slice,
              members
            )
          )
        end

        # Phase 34: Parse alias definition
        # Grammar: alias Name = Type
        private def parse_alias : ExprId
          alias_token = current_token
          advance
          skip_trivia

          # Parse alias name
          name_token = current_token
          unless name_token.kind == Token::Kind::Identifier
            emit_unexpected(name_token)
            return PREFIX_ERROR
          end
          advance
          skip_trivia

          # Expect '='
          unless current_token.kind == Token::Kind::Eq
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end
          advance
          skip_trivia

          # Parse full type annotation (supports proc types, tuples, aliases)
          type_start_token = current_token
          type_slice = parse_type_annotation
          if type_slice.empty?
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end

          type_end_token = previous_token || type_start_token
          alias_span = alias_token.span.cover(type_end_token.span)

          @arena.add_typed(
            AliasNode.new(
              alias_span,
              name_token.slice,
              type_slice
            )
          )
        end

        # Phase 92: Parse annotation definition
        # Grammar: annotation Name [; body...] end
        # Phase 92A: Parser only - body ignored/empty
        # Note: This is different from parse_annotation which parses annotation expressions like @[Link]
        private def parse_annotation_def : ExprId
          annotation_token = current_token
          advance
          skip_trivia

          # Parse annotation name
          name_token = current_token
          unless name_token.kind == Token::Kind::Identifier
            emit_unexpected(name_token)
            return PREFIX_ERROR
          end
          name_slice = name_token.slice
          advance
          skip_trivia
          consume_newlines

          # Phase 92A: Skip body for now (annotations can have methods/properties)
          # For now, just expect 'end' immediately
          loop do
            token = current_token
            break if token.kind == Token::Kind::End
            break if token.kind == Token::Kind::EOF

            # Skip any body content (Phase 92B will parse it)
            advance
            consume_newlines
          end

          # Expect 'end'
          unless current_token.kind == Token::Kind::End
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end
          end_token = current_token
          advance

          annotation_span = annotation_token.span.cover(end_token.span)

          # For annotation definitions, we create AnnotationDefNode
          # The body is not yet parsed (Phase 92B will handle that)
          @arena.add_typed(
            AnnotationDefNode.new(
              annotation_span,
              name_slice
            )
          )
        end

        # Phase 30: Parse accessor macro (getter/setter/property)
        # Grammar: getter name [: Type] [= value] [, name2 [: Type2] [= value2]]
        # Returns GetterNode/SetterNode/PropertyNode
        private def parse_accessor_macro(kind : Symbol) : ExprId
          start_token = current_token
          advance  # consume getter/setter/property keyword
          skip_trivia

          specs = [] of AccessorSpec

          # Parse comma-separated list of accessor specs
          loop do
            # Parse name (can be any identifier, including keywords!)
            name_token = current_token
            unless name_token.kind == Token::Kind::Identifier || is_keyword_identifier?(name_token)
              emit_unexpected(name_token)
              return PREFIX_ERROR
            end

            name_slice = name_token.slice
            name_span = name_token.span
            spec_start = name_span
            advance
            skip_trivia

            # Parse optional type annotation
            type_annotation : Slice(UInt8)? = nil
            type_span : Span? = nil
            if current_token.kind == Token::Kind::Colon
              advance  # consume ':'
              skip_trivia
              type_start = current_token
              type_annotation = parse_type_annotation
              type_end = previous_token.not_nil!

              # Phase 103E: parse_type_annotation now handles suffixes (?, *, **, [])
              # automatically since Phase 30 added them to is_type_token list

              type_span = type_start.span.cover(type_end.span)
              skip_trivia
            end

            # Parse optional default value
            default_value : ExprId? = nil
            if current_token.kind == Token::Kind::Eq
              advance  # consume '='
              skip_whitespace_and_optional_newlines
              default_value = parse_expression(0)
            end

            # Capture default span if present
            default_span = default_value ? node_span(default_value.not_nil!) : nil

            specs << AccessorSpec.new(
              name: name_slice,
              type_annotation: type_annotation,
              default_value: default_value,
              name_span: name_span,
              type_span: type_span,
              default_span: default_span
            )

            skip_trivia

            # Check for comma to continue
            if current_token.kind == Token::Kind::Comma
              advance  # consume ','
              skip_whitespace_and_optional_newlines
            else
              break
            end
          end

          # Calculate overall span
          last_spec_span = specs.last.span
          overall_span = start_token.span.cover(last_spec_span)

          # Create appropriate node based on kind
          node = case kind
                 when :getter
                   GetterNode.new(overall_span, specs)
                 when :setter
                   SetterNode.new(overall_span, specs)
                 when :property
                   PropertyNode.new(overall_span, specs)
                 else
                   raise "Unknown accessor kind: #{kind}"
                 end

          result = @arena.add_typed(node)

          # Optional block after accessor macro: getter x : T do ... end
          consume_newlines
          if current_token.kind == Token::Kind::Do || current_token.kind == Token::Kind::LBrace
            blk = parse_block
            blk unless blk.invalid?
          end

          result
        end

        # Helper: Check if token is a keyword that can be used as identifier in some contexts
        private def is_keyword_identifier?(token : Token) : Bool
          case token.kind
          when Token::Kind::End, Token::Kind::Begin, Token::Kind::If, Token::Kind::Unless,
               Token::Kind::While, Token::Kind::Until, Token::Kind::Case, Token::Kind::When,
               Token::Kind::Select, Token::Kind::Else, Token::Kind::Elsif, Token::Kind::Rescue,
               Token::Kind::Ensure, Token::Kind::Return, Token::Kind::Next, Token::Kind::Break,
               Token::Kind::Yield, Token::Kind::With, Token::Kind::Abstract, Token::Kind::Private,
               Token::Kind::Protected, Token::Kind::Include, Token::Kind::Extend,
               Token::Kind::Macro, Token::Kind::Struct, Token::Kind::Enum, Token::Kind::Alias,
               Token::Kind::Typeof, Token::Kind::As,
               Token::Kind::Nil, Token::Kind::True, Token::Kind::False, Token::Kind::Class,
               Token::Kind::Module, Token::Kind::Def, Token::Kind::Union, Token::Kind::Annotation,
               Token::Kind::Lib, Token::Kind::Fun, Token::Kind::Out, Token::Kind::Require,
               Token::Kind::Sizeof, Token::Kind::Pointerof, Token::Kind::Uninitialized,
               Token::Kind::Offsetof, Token::Kind::Alignof, Token::Kind::InstanceAlignof,
               Token::Kind::Loop, Token::Kind::Do, Token::Kind::For, Token::Kind::In,
               Token::Kind::Of, Token::Kind::Self, Token::Kind::Super, Token::Kind::PreviousDef,
               Token::Kind::Spawn
            true
          else
            false
          end
        end

        # Phase 31: Parse module definition
        # Grammar: module Name ... end
        private def parse_module : ExprId
          module_token = current_token
          advance
          skip_trivia

          name_token = current_token
          unless name_token.kind == Token::Kind::Identifier
            emit_unexpected(name_token)
            return PREFIX_ERROR
          end
          advance

          # Phase 61: Parse optional type parameters: (T, K, V)
          skip_trivia
          type_params = parse_type_parameters

          consume_newlines

          body_ids_b = SmallVec(ExprId, 4).new
          loop do
            skip_trivia
            token = current_token
            break if token.kind == Token::Kind::End
            break if token.kind == Token::Kind::EOF

            if token.kind == Token::Kind::InstanceVar
              next_token = peek_next_non_trivia
              if next_token.kind == Token::Kind::Colon
                expr = parse_instance_var_decl
              else
                expr = parse_statement
              end
            elsif token.kind == Token::Kind::ClassVar
              next_token = peek_next_non_trivia
              if next_token.kind == Token::Kind::Colon
                expr = parse_class_var_decl
              else
                expr = parse_statement
              end
            elsif token.kind == Token::Kind::Identifier && slice_eq?(token.slice, "type")
              expr = parse_lib_type_alias
            elsif definition_start?
              expr = case current_token.kind
                when Token::Kind::Def
                  parse_def
                when Token::Kind::Fun
                  parse_fun
                when Token::Kind::Class
                  parse_class
                when Token::Kind::Module
                  parse_module
                when Token::Kind::Struct
                  parse_struct
                when Token::Kind::Union
                  parse_union
                when Token::Kind::Enum
                  parse_enum
                when Token::Kind::Alias
                  parse_alias
                when Token::Kind::Annotation
                  # Phase 92: annotation definition
                  parse_annotation_def
                when Token::Kind::Abstract
                  parse_abstract
                when Token::Kind::Private
                  parse_private
                when Token::Kind::Protected
                  parse_protected
                when Token::Kind::Lib
                  parse_lib
                else
                  parse_statement
                end
            else
              expr = parse_statement
            end
            body_ids_b << expr unless expr.invalid?
            consume_newlines
          end

          expect_identifier("end")
          end_token = previous_token
          consume_newlines

          module_span = if end_token
            module_token.span.cover(end_token.span)
          else
            module_token.span
          end

          @arena.add_typed(
            ModuleNode.new(
              module_span,
              name_token.slice,
              body_ids_b.to_a,
              type_params
            )
          )
        end

        # Phase 31: Parse include statement
        # Grammar: include ModuleName or include Namespace::Module
        private def parse_include : ExprId
          include_token = current_token
          advance
          skip_trivia

          target_expr = parse_path_or_identifier
          # Support generic module include: include Enumerable(T)
          target_expr = parse_generic_application(target_expr)
          return PREFIX_ERROR if target_expr.invalid?

          name_slice = final_identifier_slice(target_expr)
          target_span = @arena[target_expr].span
          include_span = include_token.span.cover(target_span)

          @arena.add_typed(
            IncludeNode.new(
              include_span,
              name_slice,
              target_expr
            )
          )
        end

        # Phase 31: Parse extend statement
        # Grammar: extend ModuleName
        private def parse_extend : ExprId
          extend_token = current_token
          advance
          skip_trivia

          target_expr = parse_path_or_identifier
          target_expr = parse_generic_application(target_expr)
          return PREFIX_ERROR if target_expr.invalid?

          name_slice = final_identifier_slice(target_expr)
          target_span = @arena[target_expr].span
          extend_span = extend_token.span.cover(target_span)

          @arena.add_typed(
            ExtendNode.new(
              extend_span,
              name_slice,
              target_expr
            )
          )
        end

        private def final_identifier_slice(expr_id : ExprId) : Slice(UInt8)
          node = @arena[expr_id]
          case node
          when IdentifierNode
            node.name
          when PathNode
            final_identifier_slice(node.right)
          else
            Slice(UInt8).new(0)
          end
        end

        # Phase 5C: Parse instance variable declaration: @var : Type
        private def parse_instance_var_decl : ExprId
          ivar_token = current_token
          unless ivar_token.kind == Token::Kind::InstanceVar
            emit_unexpected(ivar_token)
            return PREFIX_ERROR
          end
          advance  # consume @var

          skip_trivia

          # Expect colon
          unless current_token.kind == Token::Kind::Colon
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end
          advance  # consume :

          skip_trivia

          # Parse type annotation
          type_start_token = current_token
          type_slice = parse_type_annotation
          type_end_token = previous_token

          end_span = type_end_token ? type_end_token.span : type_start_token.span

          # Optional default value: @ivar : Type = expr
          default_value = nil
          default_span = nil
          if operator_token?(current_token, Token::Kind::Eq)
            advance
            skip_whitespace_and_optional_newlines
            value_expr = parse_expression(0)
            if value_expr.invalid?
              return PREFIX_ERROR
            end
            default_value = value_expr
            default_span = @arena[value_expr].span
          end

          decl_span = if default_span
            ivar_token.span.cover(default_span)
          else
            ivar_token.span.cover(end_span)
          end

          @arena.add_typed(InstanceVarDeclNode.new(
            decl_span,
            ivar_token.slice,
            type_slice,
            default_value
          ))
        end

        # Phase 77: Parse class variable declaration: @@var : Type
        private def parse_class_var_decl : ExprId
          cvar_token = current_token
          unless cvar_token.kind == Token::Kind::ClassVar
            emit_unexpected(cvar_token)
            return PREFIX_ERROR
          end
          advance  # consume @@var

          skip_trivia

          # Expect colon
          unless current_token.kind == Token::Kind::Colon
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end
          advance  # consume :

          skip_trivia

          # Parse type annotation
          type_start_token = current_token
          type_slice = parse_type_annotation
          type_end_token = previous_token

          end_span = type_end_token ? type_end_token.span : type_start_token.span

          # Optional default value: @@var : Type = expr
          default_value = nil
          default_span = nil
          if operator_token?(current_token, Token::Kind::Eq)
            advance
            skip_whitespace_and_optional_newlines
            value_expr = parse_expression(0)
            if value_expr.invalid?
              return PREFIX_ERROR
            end
            default_value = value_expr
            default_span = @arena[value_expr].span
          end

          decl_span = if default_span
            cvar_token.span.cover(default_span)
          else
            cvar_token.span.cover(end_span)
          end

          @arena.add_typed(ClassVarDeclNode.new(
            decl_span,
            cvar_token.slice,
            type_slice,
            default_value
          ))
        end

        # Phase 77: Parse global variable declaration: $var : Type
        private def parse_global_var_decl : ExprId
          gvar_token = current_token
          unless gvar_token.kind == Token::Kind::GlobalVar
            emit_unexpected(gvar_token)
            return PREFIX_ERROR
          end
          advance  # consume $var

          skip_trivia

          # Expect colon
          unless current_token.kind == Token::Kind::Colon
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end
          advance  # consume :

          skip_trivia

          # Parse type annotation
          type_start_token = current_token
          type_slice = parse_type_annotation
          type_end_token = previous_token

          end_span = type_end_token ? type_end_token.span : type_start_token.span
          decl_span = gvar_token.span.cover(end_span)

          @arena.add_typed(
            GlobalVarDeclNode.new(
              decl_span,
              gvar_token.slice,
              type_slice
            )
          )
        end

        private def skip_macro_parameters
          skip_trivia
          return unless current_token.kind == Token::Kind::LParen

          advance
          depth = 1
          while depth > 0 && current_token.kind != Token::Kind::EOF
            case current_token.kind
            when Token::Kind::LParen
              depth += 1
            when Token::Kind::RParen
              depth -= 1
            end
            advance
          end
        end

        private def parse_macro_body
          @macro_mode += 1
          begin
            pieces = Array(MacroPiece).new(16)
            buffer = IO::Memory.new
            buffer_start_token : Token? = nil
            control_depth = 0
            block_depth = 0
            macro_trim_left = false
            macro_trim_right = false
            trim_next_left = false
            trim_final = false

            loop do
              if trim_next_left
                skip_macro_whitespace_after_escape
                trim_final = true  # Remember for final flush
                trim_next_left = false
              end

              token = current_token
              break if token.kind == Token::Kind::EOF

              if macro_control_start?
                left_trim = macro_control_left_trim?
                already_empty = pieces.empty?
                trim_applied = left_trim
                flush_macro_text(buffer, pieces, trim_applied, buffer_start_token, previous_token)
                buffer_start_token = nil
                macro_trim_left ||= already_empty && trim_applied

                piece, effect, skip_whitespace = parse_macro_control_piece(left_trim)
                pieces << piece

                # Special handling for comment blocks - skip content
                if piece.control_keyword == "comment" && effect == :push
                  comment_depth = 1
                  # Skip tokens until matching {% end %}
                  loop do
                    break if comment_depth == 0
                    break if current_token.kind == Token::Kind::EOF

                    if macro_control_start?
                      inner_left_trim = macro_control_left_trim?
                      inner_piece, inner_effect, _ = parse_macro_control_piece(inner_left_trim)
                      case inner_effect
                      when :push
                        comment_depth += 1
                      when :pop
                        comment_depth -= 1
                        if comment_depth == 0
                          pieces << inner_piece  # Add the closing {% end %}
                        end
                      end
                    else
                      # Skip any other tokens
                      advance
                    end
                  end
                  trim_next_left = skip_whitespace
                  next
                end

                case effect
                when :push
                  control_depth += 1
                when :pop
                  break if control_depth == 0
                  control_depth -= 1
                end

                macro_trim_right ||= skip_whitespace
                macro_trim_right ||= skip_whitespace
                trim_next_left = skip_whitespace
                next
              elsif macro_expression_start?
                left_trim = macro_expression_left_trim?
                already_empty = pieces.empty?
                trim_applied = left_trim
                flush_macro_text(buffer, pieces, trim_applied, buffer_start_token, previous_token)
                buffer_start_token = nil
                macro_trim_left ||= already_empty && trim_applied

                piece, skip_whitespace = parse_macro_expression_piece
                pieces << piece

                trim_next_left = skip_whitespace
                next
              elsif macro_variable_start?
                already_empty = pieces.empty?
                flush_macro_text(buffer, pieces, false, buffer_start_token, previous_token)
                buffer_start_token = nil
                macro_trim_left ||= already_empty

                piece, skip_whitespace = parse_macro_variable_piece
                pieces << piece
                trim_next_left = skip_whitespace
                next
              end

              # Track start of text buffer
              if buffer_start_token.nil? && buffer.size == 0
                buffer_start_token = token
              end

              # Maintain block depth for regular language constructs so we don't
              # prematurely terminate the macro body at an inner 'end'. We only
              # stop on the outer macro 'end' when control_depth == 0 and
              # block_depth == 0 and the current token is End.
              case token.kind
              when Token::Kind::Def, Token::Kind::Class, Token::Kind::Module,
                   Token::Kind::Struct, Token::Kind::Enum, Token::Kind::Begin,
                   Token::Kind::If, Token::Kind::Unless, Token::Kind::While,
                   Token::Kind::Until, Token::Kind::Case, Token::Kind::Select,
                   Token::Kind::Lib, Token::Kind::Do
                block_depth += 1
              when Token::Kind::End
                if control_depth == 0 && block_depth == 0
                  # Do not consume macro-def 'end'; leave it for caller
                  break
                else
                  block_depth -= 1 if block_depth > 0
                end
              end

              buffer.write(token.slice)
              advance
            end

            flush_macro_text(buffer, pieces, trim_final, buffer_start_token, previous_token)
            {
              pieces,
              macro_trim_left,
              macro_trim_right,
            }
          ensure
            @macro_mode -= 1
          end
        end
        private def flush_macro_text(buffer, pieces, trim_trailing = false, start_token : Token? = nil, end_token : Token? = nil)
          return if buffer.size == 0
          text = String.new(buffer.to_slice)
          text = text.rstrip if trim_trailing

          # Capture span if we have start and end tokens
          span = if start_token && end_token
            start_token.span.cover(end_token.span)
          elsif start_token
            start_token.span
          end

          pieces << MacroPiece.text(text, span)
          buffer.clear
        end

        # Phase IMPLICIT_RECEIVER: Parse implicit receiver method call
        # Example: .method → ImplicitObj.method
        # Grammar: .identifier or .identifier?
        private def parse_implicit_receiver_call(dot_token : Token) : ExprId
          # Consume the '.' token
          advance
          skip_trivia

          # Expect method name: identifier, keyword-like, or operator (e.g., .>(0))
          method_token = current_token
          case method_token.kind
          when Token::Kind::Identifier
            advance
          else
            # Allow operators and keywords as method names after '.'
            # e.g., .>(0), .<(0), .is_a?(T)
            # Accept any non-empty slice as a method name token
            if method_token.slice.empty?
              emit_unexpected(method_token)
              return PREFIX_ERROR
            end
            advance
          end

          # Create ImplicitObjNode as receiver
          implicit_obj = @arena.add_typed(ImplicitObjNode.new(dot_token.span))

          # Create MemberAccessNode: ImplicitObj.method
          call_span = dot_token.span.cover(method_token.span)
          @arena.add_typed(MemberAccessNode.new(
            call_span,
            implicit_obj,
            method_token.slice
          ))
        end

        # Parse expression or assignment
        # Used for statements and call arguments
        # Example: clone = 42 (in method body)
        # Example: property expansion = false (assignment as macro argument)
        private def parse_op_assign : ExprId
          Watchdog.check!  # Guard against infinite recursion
          debug("parse_op_assign: ENTERING")
          left = parse_expression(0)
          return PREFIX_ERROR if left.invalid?

          skip_trivia
          token = current_token
          debug("parse_op_assign: after parse_expression, current token = #{token.kind}")

          # Check for assignment operators
          if token.kind == Token::Kind::Eq ||
             token.kind == Token::Kind::PlusEq ||
             token.kind == Token::Kind::MinusEq ||
             token.kind == Token::Kind::StarEq ||
             token.kind == Token::Kind::SlashEq ||
             token.kind == Token::Kind::FloorDivEq ||
             token.kind == Token::Kind::PercentEq ||
             token.kind == Token::Kind::StarStarEq ||
             token.kind == Token::Kind::OrOrEq ||
             token.kind == Token::Kind::AndAndEq ||
             token.kind == Token::Kind::AmpEq ||
             token.kind == Token::Kind::PipeEq ||
             token.kind == Token::Kind::CaretEq ||
             token.kind == Token::Kind::LShiftEq ||
             token.kind == Token::Kind::RShiftEq ||
             token.kind == Token::Kind::NilCoalesceEq
            # Parse assignment
            left_node = @arena[left]
            left_kind = Frontend.node_kind(left_node)

            # Verify left side is valid assignment target
            unless left_kind == Frontend::NodeKind::Identifier ||
                   left_kind == Frontend::NodeKind::InstanceVar ||
                   left_kind == Frontend::NodeKind::ClassVar ||
                   left_kind == Frontend::NodeKind::Global ||
                   left_kind == Frontend::NodeKind::Constant ||
                   left_kind == Frontend::NodeKind::MacroVar ||
                   left_kind == Frontend::NodeKind::Index ||
                   left_kind == Frontend::NodeKind::MemberAccess
              @diagnostics << Diagnostic.new("Assignment target must be an identifier, instance variable, class variable, global variable, macro variable, or index expression", token.span)
              return PREFIX_ERROR
            end

            # Consume assignment token
            assign_token = token
            is_compound = assign_token.kind != Token::Kind::Eq
            advance
            skip_trivia

            # Phase 103J: Check for `x = uninitialized Type` syntax (without parens)
            # Only for simple assignment (not compound like +=)
            # Note: uninitialized(Type) with parens is handled by parse_uninitialized in parse_prefix
            if !is_compound && current_token.kind == Token::Kind::Uninitialized
              # Peek ahead to see if there's a parenthesis - if so, use normal expression parsing
              # This prevents treating uninitialized(Type) as uninitialized Type
              peek_next = peek_token
              has_paren = peek_next.kind == Token::Kind::LParen || (peek_next.kind == Token::Kind::Whitespace && peek_token(1).kind == Token::Kind::LParen)

              # Verify left side is a valid variable
              if !has_paren &&
                 (left_kind == Frontend::NodeKind::Identifier ||
                  left_kind == Frontend::NodeKind::InstanceVar ||
                  left_kind == Frontend::NodeKind::ClassVar ||
                  left_kind == Frontend::NodeKind::Global)

                uninitialized_token = current_token
                advance  # skip 'uninitialized'
                skip_trivia

                # Parse type as expression (following original parser pattern)
                # This handles: Int32, String, Foo::Bar, Array(Int32), etc.
                type_expr = parse_expression(0)
                return PREFIX_ERROR if type_expr.invalid?

                # Create UninitializedNode
                uninitialized_span = uninitialized_token.span.cover(@arena[type_expr].span)
                rhs = @arena.add_typed(UninitializedNode.new(
                  uninitialized_span,
                  type_expr
                ))
              elsif has_paren
                # Has parenthesis - parse as normal expression (will call parse_uninitialized)
                rhs = parse_op_assign
                return PREFIX_ERROR if rhs.invalid?
              else
                # Invalid: uninitialized can only be used with variables
                @diagnostics << Diagnostic.new("'uninitialized' can only be used with variables", current_token.span)
                return PREFIX_ERROR
              end
            else
              # Parse right-hand side normally
              rhs = parse_op_assign  # Recursive for chained assignments
              return PREFIX_ERROR if rhs.invalid?
            end

            # Handle compound assignment (expand to: x = x op y)
            value = if is_compound
              operator = case assign_token.kind
              when Token::Kind::PlusEq     then "+"
              when Token::Kind::MinusEq    then "-"
              when Token::Kind::StarEq     then "*"
              when Token::Kind::SlashEq    then "/"
              when Token::Kind::FloorDivEq then "//"
              when Token::Kind::PercentEq  then "%"
              when Token::Kind::StarStarEq then "**"
              when Token::Kind::OrOrEq     then "||"
              when Token::Kind::AndAndEq   then "&&"
              when Token::Kind::AmpEq      then "&"
              when Token::Kind::PipeEq     then "|"
              when Token::Kind::CaretEq    then "^"
              when Token::Kind::LShiftEq   then "<<"
              when Token::Kind::RShiftEq   then ">>"
              when Token::Kind::NilCoalesceEq then "??"  # Phase 82
              else
                @diagnostics << Diagnostic.new("Unknown compound assignment operator", assign_token.span)
                return PREFIX_ERROR
              end

              # Create binary operation: left op rhs
              op_slice = operator.to_slice
              pooled_op = @string_pool.intern(op_slice)
              @arena.add_typed(BinaryNode.new(
                left_node.span.cover(@arena[rhs].span),
                pooled_op,
                left,
                rhs
              ))
            else
              rhs
            end

            # Phase 35: Check if this is a constant declaration (uppercase identifier)
            # Constants are only created for simple assignments (not compound) to identifiers
            if !is_compound && left_kind == Frontend::NodeKind::Identifier
              left_literal = Frontend.node_literal(left_node)
              if left_literal && is_constant_name?(left_literal)
                # Create constant node
                assign_span = left_node.span.cover(@arena[value].span)
                result = @arena.add_typed(ConstantNode.new(
                  assign_span,
                  left_literal,
                  value
                ))
                debug("parse_op_assign: created ConstantNode, returning")
                return result
              end
            end

            # Create assignment node
            assign_span = left_node.span.cover(@arena[value].span)
            result = @arena.add_typed(AssignNode.new(
              assign_span,
              left,
              value
            ))
            debug("parse_op_assign: created AssignNode, returning")
            return result
          end

          # No assignment, just return the expression
          debug("parse_op_assign: no assignment, returning expression")
          left
        end

        # Phase CALLS_WITHOUT_PARENS: Parse call arguments without parentheses
        # Example: def_equals value, kind
        # Returns CallNode or PREFIX_ERROR if can't parse as call
        private def try_parse_call_args_without_parens(callee_token : Token) : ExprId
          debug("try_parse_call_args_without_parens: callee=#{String.new(callee_token.slice)}, current_token=#{current_token.kind}")
          # Check if current token can start an argument
          # Return error for keywords that can't be arguments
          # Phase 103D: Keywords can be identifiers if followed by " : " (type annotation)
          # Example: property else : String, property when : Int32
          case current_token.kind
          when Token::Kind::RBrace, Token::Kind::RParen, Token::Kind::RBracket
            # Closing delimiters are never arguments
            return PREFIX_ERROR
          when Token::Kind::LParen
            # Regular call with parentheses, handled separately
            return PREFIX_ERROR
          when Token::Kind::End
            # 'end' always ends a block/structure, never an argument
            return PREFIX_ERROR unless next_comes_colon_space?
          when Token::Kind::If, Token::Kind::Unless,
               Token::Kind::Else, Token::Kind::Elsif,
               Token::Kind::When, Token::Kind::Then,
               Token::Kind::Rescue, Token::Kind::Ensure,
               Token::Kind::While, Token::Kind::Until
            # These keywords can be identifiers if followed by " : " (type annotation)
            # Note: 'in' is already allowed as identifier (see Identifier case in parse_prefix)
            return PREFIX_ERROR unless next_comes_colon_space?
          when Token::Kind::Newline, Token::Kind::EOF
            return PREFIX_ERROR
          when Token::Kind::ColonColon
            return PREFIX_ERROR
          # Phase 103H: Arithmetic operators (Plus, Minus, Star) need special handling
          # They can be unary (no space after) or binary (space after)
          # Example: "foo +1" is call, "foo + 1" is binary operation
          when Token::Kind::Plus, Token::Kind::Minus, Token::Kind::Star, Token::Kind::StarStar
            # Check if next token is whitespace (binary) or not (unary)
            next_tok = peek_token(1)
            if next_tok.kind == Token::Kind::Whitespace || next_tok.kind == Token::Kind::Newline
              # Binary operator context - don't parse as call argument
              return PREFIX_ERROR
            end
            # Otherwise, allow as unary prefix in argument (foo +1)
          # Don't parse as call if followed by binary/logical operators that can't start an argument
          # Phase 103K: Slash and FloorDiv can't be unary, always return error
          when Token::Kind::Slash, Token::Kind::FloorDiv,
               Token::Kind::Percent,  # Percent literal uses different token
               Token::Kind::OrOr, Token::Kind::AndAnd,
               Token::Kind::Question,  # ternary operator
               Token::Kind::Arrow,     # hash arrow =>
               # Comparison operators
               Token::Kind::EqEq, Token::Kind::EqEqEq, Token::Kind::NotEq,  # Phase 50: === case equality
               Token::Kind::Less, Token::Kind::Greater,
               Token::Kind::LessEq, Token::Kind::GreaterEq,
               Token::Kind::Spaceship,  # <=>
               # Other binary operators that can't be prefix
               Token::Kind::Pipe, Token::Kind::Caret, Token::Kind::Amp,
               Token::Kind::LShift, Token::Kind::RShift,
               Token::Kind::DotDot, Token::Kind::DotDotDot,
               Token::Kind::Match, Token::Kind::NotMatch,  # =~, !~
               Token::Kind::In,  # Phase 79: in operator
               Token::Kind::NilCoalesce,  # Phase 81: ?? operator
               # Phase 89: Wrapping arithmetic operators
               Token::Kind::AmpPlus, Token::Kind::AmpMinus,
               Token::Kind::AmpStar, Token::Kind::AmpStarStar
            return PREFIX_ERROR
          # Compound assignment operators - these mean assignment, not call
          when Token::Kind::PlusEq, Token::Kind::MinusEq, Token::Kind::StarEq,
               Token::Kind::SlashEq, Token::Kind::FloorDivEq, Token::Kind::PercentEq,
               Token::Kind::StarStarEq,
               Token::Kind::OrOrEq, Token::Kind::AndAndEq,
               Token::Kind::AmpEq, Token::Kind::PipeEq, Token::Kind::CaretEq,
               Token::Kind::LShiftEq, Token::Kind::RShiftEq,
               Token::Kind::AmpPlusEq, Token::Kind::AmpMinusEq,
               Token::Kind::AmpStarEq, Token::Kind::AmpStarStarEq,
               Token::Kind::NilCoalesceEq
            return PREFIX_ERROR
          # Colon in ternary operator context (not named argument)
          when Token::Kind::Colon
            # If @no_type_declaration > 0, we're inside ternary operator
            # In this case, Colon is part of ternary syntax, not a named argument
            return PREFIX_ERROR if @no_type_declaration > 0
          # Special case: Eq at top level (not inside nested call args) means this is assignment
          # Example: "clone = 42" should be assignment, not call
          # But "property expansion = false" (inside call args) should parse assignment as argument
          when Token::Kind::Eq
            # If we're NOT inside call args (@parsing_call_args == 0), this is likely assignment
            return PREFIX_ERROR if @parsing_call_args == 0
          end

          # Set flag to prevent nested calls without parens during argument parsing
          @parsing_call_args += 1

          # Parse arguments separated by commas
          # Build arguments in a small inline buffer to reduce heap churn
          args_b = SmallVec(ExprId, 4).new
          named_b = SmallVec(NamedArgument, 2).new

          loop do
            # Check if we're at a block instead of arguments
            # Example: .tap { } or .each do |x|
            # In this case, stop parsing arguments and let block parser handle it
            if current_token.kind == Token::Kind::LBrace || current_token.kind == Token::Kind::Do
              break
            end

            # Handle block shorthand and captures in no-parens calls
            if current_token.kind == Token::Kind::AmpDot
              amp_token = current_token
              advance
              skip_trivia
              arg = parse_block_shorthand(amp_token)
            elsif current_token.kind == Token::Kind::Amp
              amp_token = current_token
              advance
              skip_trivia
              if current_token.kind == Token::Kind::Operator
                # &.method with space
                arg = parse_block_shorthand(amp_token)
              elsif current_token.kind == Token::Kind::Identifier || current_token.kind == Token::Kind::InstanceVar
                # &block variable capture
                ident_token = current_token
                advance
                ident_span = ident_token.span
                ident_node = @arena.add_typed(IdentifierNode.new(ident_span, @string_pool.intern(ident_token.slice)))
                arg_span = amp_token.span.cover(ident_span)
                arg = @arena.add_typed(UnaryNode.new(arg_span, amp_token.slice, ident_node))
              else
                # Not a block shorthand/capture, rewind amp and parse normally
                unadvance
                arg = parse_op_assign
              end
            elsif current_token.kind == Token::Kind::Star || current_token.kind == Token::Kind::StarStar
              # Splat arguments
              star_token = current_token
              advance
              skip_trivia
              value_expr = parse_op_assign
              if value_expr.invalid?
                @parsing_call_args -= 1
                return PREFIX_ERROR
              end
              span = star_token.span.cover(@arena[value_expr].span)
              arg = @arena.add_typed(SplatNode.new(span, value_expr))
            else
              # Parse one argument
              # For typed macro callees (e.g., record), allow name : Type [= value]
              if typed_macro_args_callee?(callee_token) && (current_token.kind == Token::Kind::Identifier || is_keyword_identifier?(current_token))
                # Lookahead for ':' to confirm typed field
                save_idx = @index
                name_tok = current_token
                advance
                if current_token.kind == Token::Kind::Colon
                  # Build declaration directly
                  skip_whitespace_and_optional_newlines
                  type_start = current_token
                  type_slice = parse_type_annotation
                  type_end = previous_token || type_start
                  skip_whitespace_and_optional_newlines
                  typed_value_expr : ExprId? = nil
                  if current_token.kind == Token::Kind::Eq
                    advance
                    skip_whitespace_and_optional_newlines
                    typed_value_expr = parse_op_assign
                    if typed_value_expr.invalid?
                      @parsing_call_args -= 1
                      return PREFIX_ERROR
                    end
                  end
                  full_span = if typed_value_expr
                    name_tok.span.cover(@arena[typed_value_expr].span)
                  else
                    name_tok.span.cover(type_end.span)
                  end
                  arg = @arena.add_typed(
                    TypeDeclarationNode.new(
                      full_span,
                      name_tok.slice,
                      type_slice,
                      typed_value_expr
                    )
                  )
                else
                  # Not a typed field, rewind and parse normally
                  @index = save_idx
                  arg = parse_op_assign
                end
              else
                # Could be: positional arg, assignment as arg, or named arg
                arg = parse_op_assign
              end
            end
            if arg.invalid?
              @parsing_call_args -= 1
              return PREFIX_ERROR
            end

            skip_trivia

            # Check if this is a named argument: identifier followed by colon
            # Accessor-like macro arguments: name : Type [= value]
            if typed_macro_args_callee?(callee_token)
              arg_node = @arena[arg]
              if Frontend.node_kind(arg_node) == Frontend::NodeKind::Identifier && current_token.kind == Token::Kind::Colon
                name_span = arg_node.span
                name_slice = Frontend.node_literal(arg_node).not_nil!

                advance  # consume ':'
                skip_whitespace_and_optional_newlines

                type_start_token = current_token
                type_slice = parse_type_annotation
                type_end_token = previous_token || type_start_token
                skip_whitespace_and_optional_newlines

                decl_value_expr : ExprId? = nil
                if current_token.kind == Token::Kind::Eq
                  advance
                  skip_whitespace_and_optional_newlines
                  decl_value_expr = parse_op_assign
                  return PREFIX_ERROR if decl_value_expr.invalid?
                end

                full_span = if decl_value_expr
                  name_span.cover(@arena[decl_value_expr].span)
                else
                  name_span.cover(type_end_token.span)
                end

                decl = @arena.add_typed(
                  TypeDeclarationNode.new(
                    full_span,
                    name_slice,
                    type_slice,
                    decl_value_expr
                  )
                )
                args_b << decl
                skip_trivia
                next
              end
            end

            # Pattern: name: value
            arg_node = @arena[arg]
            if Frontend.node_kind(arg_node) == Frontend::NodeKind::Identifier &&
               current_token.kind == Token::Kind::Colon
              # This is named argument!
              name_span = arg_node.span
              name_slice = Frontend.node_literal(arg_node).not_nil!  # Zero-copy slice

              advance  # consume ':'
              consume_newlines  # Allow newlines after colon in named arguments

              # Parse value (using parse_op_assign like original Crystal)
              value_expr = parse_op_assign
              if value_expr.invalid?
                @parsing_call_args -= 1
                return PREFIX_ERROR
              end

                    value_span = @arena[value_expr].span

                    named_b << NamedArgument.new(name_slice, value_expr, name_span, value_span)
              skip_trivia
            else
              # Positional argument
              args_b << arg
            end

            # Check for comma (more arguments)
            if current_token.kind == Token::Kind::Comma
              advance  # consume comma
              consume_newlines  # Allow newlines after comma in argument lists
            else
              # No more arguments
              break
            end
          end

          # Parse optional block: do...end or {...}
          # Example: record Point, x : Int32 do ... end
          consume_newlines  # Allow newlines before block
          block_expr : ExprId? = nil
          if current_token.kind == Token::Kind::Do || current_token.kind == Token::Kind::LBrace
            # Allow blocks to contain their own call-without-parens expressions by
            # temporarily releasing the guard.
            @parsing_call_args -= 1
            block_expr = parse_block
            @parsing_call_args += 1
            if block_expr.invalid?
              @parsing_call_args -= 1
              return PREFIX_ERROR
            end
          end

          # Create CallNode
          callee = @arena.add_typed(IdentifierNode.new(callee_token.span, @string_pool.intern(callee_token.slice)))

          # Materialize argument arrays from builders once
          args = args_b.to_a
          named_args = named_b.to_a

          # Calculate span including last argument (positional or named) or block
          call_span = if !block_expr.nil?
            # Include block in span
            block_node = @arena[block_expr]
            callee_token.span.cover(block_node.span)
          elsif named_args.size > 0
            # Last named arg
            callee_token.span.cover(named_args.last.span)
          elsif args.size > 0
            # Last positional arg
            last_arg = @arena[args.last]
            callee_token.span.cover(last_arg.span)
          else
            callee_token.span
          end

          result = @arena.add_typed(CallNode.new(
            call_span,
            callee,
            args,
            block_expr,  # attach block if present
            named_args.empty? ? nil : named_args
          ))

          # Restore flag
          @parsing_call_args -= 1
          result
        end

        protected def parse_expression(precedence : Int32) : ExprId
          Watchdog.check!  # Guard against infinite recursion
          if inside_delimiters?
            skip_whitespace_and_optional_newlines
          else
            skip_trivia
          end

          left = parse_prefix
          debug("parse_expression(#{precedence}): after parse_prefix, left=#{left.invalid? ? "invalid" : "valid"}")
          return PREFIX_ERROR if left.invalid?

          loop do
            if inside_delimiters?
              skip_whitespace_and_optional_newlines
            else
              skip_trivia
            end
            token = current_token
            debug("parse_expression(#{precedence}): postfix loop, token=#{token.kind}")
            # Statement boundary guard: if a new '{' starts on a new line (or after ';')
            # and we're not inside delimiters, treat it as the start of a new statement,
            # not as a block attachment to the current expression.
            if token.kind == Token::Kind::LBrace && !inside_delimiters?
              if prev = previous_token
                if prev.kind == Token::Kind::Newline || prev.kind == Token::Kind::Semicolon
                  break
                end
              end
            end
            if macro_terminator_reached?(token)
              debug("parse_expression(#{precedence}): macro terminator reached")
              break
            end

            case token.kind
            when Token::Kind::LParen
              left = parse_parenthesized_call(left)
              next
            when Token::Kind::LBracket
              left = parse_index(left)
              next
            when Token::Kind::LBrace
              # Phase 10: Block with {} syntax
              # Don't attach blocks when parsing call arguments - let the call parser handle it
              if @parsing_call_args > 0
                break
              end
              # Only attach a block if 'left' can accept one (call-like). Otherwise,
              # treat '{' as start of a new statement (e.g., a tuple/hash literal).
              if can_attach_block_to?(left)
                debug("parse_expression: attaching block to call")
                left = attach_block_to_call(left)
              else
                debug("parse_expression: breaking on '{' to start new statement (not a call)")
                break
              end
              next
            when Token::Kind::Do
              # Phase 10: Block with do/end syntax
              # Don't attach blocks when parsing call arguments - let the call parser handle it
              if @parsing_call_args > 0
                break
              end
              left = attach_block_to_call(left)
              next
            when Token::Kind::AmpDot
              # Phase 47: AmpDot in postfix loop is ALWAYS safe navigation
              # Block shorthand with AmpDot is handled in parse_parenthesized_call
              left = parse_safe_navigation(left)
              next
            when Token::Kind::Amp
              # Phase 101: Amp followed by dot might be block shorthand for method call without parens
              # Only handle block shorthand case; otherwise let Amp be processed as infix operator
              left_node = @arena[left]
              left_kind = Frontend.node_kind(left_node)

              if (left_kind == Frontend::NodeKind::MemberAccess || left_kind == Frontend::NodeKind::Call)
                amp_token = current_token
                advance
                skip_trivia

                if current_token.kind == Token::Kind::Operator
                  # Block shorthand! & followed by .
                  block_arg = parse_block_shorthand(amp_token)
                  if block_arg.invalid?
                    return PREFIX_ERROR
                  end

                  # Convert MemberAccess to Call with block argument
                  callee = if left_kind == Frontend::NodeKind::MemberAccess
                    left
                  else
                    Frontend.node_callee(left_node).not_nil!
                  end

                  span = left_node.span.cover(@arena[block_arg].span)

                  left = @arena.add_typed(
                    CallNode.new(
                      span,
                      callee,
                      [block_arg]
                    )
                  )
                  next
                else
                  # Not block shorthand, rewind
                  unadvance
                end
              end

              # Fall through to let Amp be processed as infix bitwise AND operator
            when Token::Kind::ColonColon
              # Phase 63: Path expression (::)
              left = parse_path(left)
              next
            when Token::Kind::Operator
              # Check for operators not yet converted to enum (e.g., ".")
              if slice_eq?(token.slice, ".")
                left = parse_member_access(left)
                next
              end
            end

            # Check for end tokens (following original parser logic)
            if end_token?(token)
              debug("parse_expression(#{precedence}): end token reached, breaking")
              break
            end

            unless infix?(token)
              debug("parse_expression(#{precedence}): token #{token.kind} is not infix, breaking")
              break
            end
            debug("parse_expression(#{precedence}): token #{token.kind} is infix")
            current_precedence = precedence_for(token)
            debug("parse_expression(#{precedence}): current_precedence=#{current_precedence}, check: #{current_precedence} < #{precedence} = #{current_precedence < precedence}")
            break if current_precedence < precedence

            advance
            consume_newlines  # Allow newlines after binary operators (for multi-line expressions)
            # Disable type declarations for ternary true_branch (identifier: would conflict)
            if token.kind == Token::Kind::Question
              @no_type_declaration += 1
              right = parse_expression(current_precedence + 1)
              @no_type_declaration -= 1
            else
              # Special-case endless ranges: `a..` or `a...` → missing right side
              if (token.kind == Token::Kind::DotDot || token.kind == Token::Kind::DotDotDot) &&
                 (current_token.kind.in?(Token::Kind::RParen, Token::Kind::RBracket, Token::Kind::RBrace,
                                          Token::Kind::Comma, Token::Kind::Semicolon, Token::Kind::End,
                                          Token::Kind::Else, Token::Kind::Elsif, Token::Kind::Do, Token::Kind::LBrace))
                right = @arena.add_typed(NilNode.new(current_token.span))
              else
                right = parse_expression(current_precedence + 1)
              end
            end
            if right.invalid?
              left = PREFIX_ERROR
              break
            end

            # Phase 13: Handle range operators specially
            if token.kind == Token::Kind::DotDot || token.kind == Token::Kind::DotDotDot
              exclusive = token.kind == Token::Kind::DotDotDot
              left = @arena.add_typed(
                RangeNode.new(
                  cover_optional_spans(node_span(left), token.span, node_span(right)),
                  left,
                  right,
                  exclusive
                )
              )
            # Phase 23: Handle ternary operator specially
            elsif token.kind == Token::Kind::Question
              # We have: left ? right (so far)
              # Now need: : false_branch
              skip_trivia
              unless current_token.kind == Token::Kind::Colon
                # Error: expected ':' in ternary operator
                left = PREFIX_ERROR
                break
              end
              colon_token = current_token
              advance  # consume ':'

              # Parse false branch with same precedence (right-associative)
              # Disable type declarations (identifier: would conflict)
              @no_type_declaration += 1
              false_branch = parse_expression(current_precedence)
              @no_type_declaration -= 1
              if false_branch.invalid?
                left = PREFIX_ERROR
                break
              end

              left = @arena.add_typed(
                TernaryNode.new(
                  cover_optional_spans(node_span(left), token.span, node_span(false_branch)),
                  left,
                  right,
                  false_branch
                )
              )
            else
              left = build_binary(left, token, right)
            end
          end

          left
        end

        private def parse_prefix : ExprId
          # Treat leading newlines as whitespace in expression context
          while current_token.kind == Token::Kind::Newline
            advance
          end
          if macro_context?
            token = current_token
            if macro_closing_sequence?(token)
              return PREFIX_ERROR
            elsif macro_variable_start?
              return parse_macro_variable_reference
            end
          end

          token = current_token
          debug("parse_prefix: token=#{token.kind}")
          case token.kind
          when Token::Kind::True, Token::Kind::False
            id = @arena.add_typed(BoolNode.new(token.span, token.kind == Token::Kind::True))
            advance
            id
          when Token::Kind::Nil
            id = @arena.add_typed(NilNode.new(token.span))
            advance
            id
          when Token::Kind::Self
            # Phase 7: self keyword
            id = @arena.add_typed(SelfNode.new(token.span))
            advance
            id
          when Token::Kind::Typeof
            # Phase 40: typeof (type introspection)
            parse_typeof
          when Token::Kind::Sizeof
            # Phase 41: sizeof (size in bytes)
            parse_sizeof
          when Token::Kind::Pointerof
            # Phase 42: pointerof (pointer to variable/expression)
            parse_pointerof
          when Token::Kind::Uninitialized
            # Phase 85: uninitialized variable
            parse_uninitialized
          when Token::Kind::Offsetof
            # Phase 86: offset of field in type
            parse_offsetof
          when Token::Kind::Alignof
            # Phase 88: ABI alignment in bytes
            parse_alignof
          when Token::Kind::InstanceAlignof
            # Phase 88: instance alignment
            parse_instance_alignof
          when Token::Kind::Asm
            # Phase 95: inline assembly
            parse_asm
          when Token::Kind::Out
            # Phase 98: out keyword (C bindings output parameter)
            parse_out
          when Token::Kind::Raise
            # Allow raise in expression context (e.g., x || raise "error")
            stmt = parse_raise
            parse_postfix_if_modifier(stmt)
          when Token::Kind::Return
            # Allow return in expression contexts (e.g., x || return y)
            stmt = parse_return
            parse_postfix_if_modifier(stmt)
          when Token::Kind::If
            # Phase 103D: Check if this is identifier usage (if : Type)
            if next_comes_colon_space?
              # Treat as identifier
              identifier_token = token
              advance
              # With skip_trivia, detect space by span gap
              gap_before_colon = current_token.kind == Token::Kind::Colon &&
                                  current_token.span.start_line == identifier_token.span.end_line &&
                                  current_token.span.start_column > identifier_token.span.end_column
              skip_trivia
              if (gap_before_colon || current_token.kind == Token::Kind::Whitespace) && current_token.kind == Token::Kind::Colon && @no_type_declaration == 0
                parse_type_declaration_from_identifier(identifier_token)
              else
                @arena.add_typed(IdentifierNode.new(identifier_token.span, @string_pool.intern(identifier_token.slice)))
              end
            else
              parse_if
            end
          when Token::Kind::Unless
            # Phase 103D: Check if this is identifier usage (unless : Type)
            if next_comes_colon_space?
              # Treat as identifier
              identifier_token = token
              advance
              gap_before_colon = current_token.kind == Token::Kind::Colon &&
                                  current_token.span.start_line == identifier_token.span.end_line &&
                                  current_token.span.start_column > identifier_token.span.end_column
              skip_trivia
              if (gap_before_colon || current_token.kind == Token::Kind::Whitespace) && current_token.kind == Token::Kind::Colon && @no_type_declaration == 0
                parse_type_declaration_from_identifier(identifier_token)
              else
                @arena.add_typed(IdentifierNode.new(identifier_token.span, @string_pool.intern(identifier_token.slice)))
              end
            else
              # Phase 24: unless condition
              parse_unless
            end
          when Token::Kind::Def
            # Phase 103F: 'def' as identifier in expression context
            # Examples: getter def : Type, foo(def: value), x = def
            # Method definitions are handled in parse_op_assign via definition_start?
            identifier_token = token
            advance
            gap_before_colon = current_token.kind == Token::Kind::Colon &&
                                current_token.span.start_line == identifier_token.span.end_line &&
                                current_token.span.start_column > identifier_token.span.end_column
            skip_trivia
            if (gap_before_colon || current_token.kind == Token::Kind::Whitespace) && current_token.kind == Token::Kind::Colon && @no_type_declaration == 0
              # Type declaration: def : Type = value
              parse_type_declaration_from_identifier(identifier_token)
            else
              # Just an identifier reference or named arg
              @arena.add_typed(IdentifierNode.new(identifier_token.span, @string_pool.intern(identifier_token.slice)))
            end
          when Token::Kind::Case
            # Phase 11: case/when pattern matching
            parse_case
          when Token::Kind::IsA
            # Allow bare is_a?(Type) with implicit self receiver
            parse_is_a_bare
          when Token::Kind::Select
            # Phase 90A: select/when concurrent channel operations
            parse_select
          when Token::Kind::While
            parse_while
          when Token::Kind::Until
            # Phase 25: until loop
            parse_until
          when Token::Kind::For
            # Phase 99: for loop (iteration)
            parse_for
          when Token::Kind::Loop
            # Phase 83: infinite loop
            parse_loop
          when Token::Kind::Spawn
            # Phase 84: spawn fiber
            parse_spawn
          when Token::Kind::Begin
            # Phase 28: begin/end blocks
            parse_begin
          when Token::Kind::With
            # Phase 67: with (context block)
            parse_with
          # Raise removed - it's a regular method, not a keyword
          when Token::Kind::Yield
            # Phase 10: yield (call block)
            parse_yield
          when Token::Kind::Super
            # Allow 'super' as expression in contexts like boolean chains
            stmt = parse_super
            parse_postfix_if_modifier(stmt)
          when Token::Kind::PreviousDef
            stmt = parse_previous_def
            parse_postfix_if_modifier(stmt)
          when Token::Kind::Break
            stmt = parse_break
            parse_postfix_if_modifier(stmt)
          when Token::Kind::Next
            stmt = parse_next
            parse_postfix_if_modifier(stmt)
          when Token::Kind::Require
            # Phase 65: require statement
            parse_require
          when Token::Kind::Identifier,
               Token::Kind::Of, Token::Kind::As, Token::Kind::In
            # Phase 60: Check if this is a generic type instantiation
            # Pattern: UppercaseIdentifier(Type1, Type2)
            # Phase 103: Check for type annotation: identifier : Type = value
            # Phase CALLS_WITHOUT_PARENS: Track whitespace for method calls without parentheses
            # Phase KEYWORD_AS_IDENT: Keywords 'of', 'as', 'in' can be identifiers in expression position
            identifier_token = token
            prev_token = @previous_token
            advance  # Move past identifier

            # Phase CALLS_WITHOUT_PARENS: Check for whitespace before skip_trivia
            # With no-trivia lexing, approximate with span gap where needed
            next_tok = current_token
            space_consumed = (next_tok.kind == Token::Kind::Whitespace)
            unless space_consumed
              if next_tok.span.start_offset > identifier_token.span.end_offset
                space_consumed = true
              else
                same_line = next_tok.span.start_line == identifier_token.span.end_line
                gap = next_tok.span.start_column > identifier_token.span.end_column
                space_consumed = same_line && gap
              end
            end
            skip_trivia

            # Check if uppercase identifier followed by (
            if identifier_token.slice.size > 0 &&
               identifier_token.slice[0] >= 'A'.ord && identifier_token.slice[0] <= 'Z'.ord &&
               current_token.kind == Token::Kind::LParen
              # This is generic instantiation: Box(Int32)
              parse_generic_instantiation(identifier_token)
            # Phase 103: Check for type annotation (if enabled)
            # Must check for space before colon: "x : Type" not "x: value" (named arg)
            # After skip_trivia, we're at the colon; space_consumed tells us if there was space before
            elsif @no_type_declaration == 0 && current_token.kind == Token::Kind::Colon &&
                  (space_consumed || (current_token.span.start_line == identifier_token.span.end_line &&
                   current_token.span.start_column > identifier_token.span.end_column))
              # This is type declaration: x : Type = value
              parse_type_declaration_from_identifier(identifier_token)
          # Phase CALLS_WITHOUT_PARENS: Try to parse call arguments if space was consumed
            # Don't try to parse nested calls when already parsing call arguments
            elsif space_consumed && @parsing_call_args == 0 && current_token.kind != Token::Kind::Colon &&
                  !(prev_token && prev_token.kind == Token::Kind::Operator && slice_eq?(prev_token.slice, "."))
              boundary_token = current_token

              if call_without_parens_disallowed?(boundary_token)
                @arena.add_typed(IdentifierNode.new(identifier_token.span, @string_pool.intern(identifier_token.slice)))
              else
                # Attempt to parse call arguments without parentheses
                # Example: def_equals value, kind
                maybe_call = try_parse_call_args_without_parens(identifier_token)
                if maybe_call.invalid?
                  # Not a call, just an identifier
                  @arena.add_typed(IdentifierNode.new(identifier_token.span, @string_pool.intern(identifier_token.slice)))
                else
                  debug("parse_prefix: converted #{String.new(identifier_token.slice)} into call without parens")
                  maybe_call
                end
              end
            else
              # Regular identifier
              @arena.add_typed(IdentifierNode.new(identifier_token.span, @string_pool.intern(identifier_token.slice)))
            end
          when Token::Kind::InstanceVar
            # Instance variable (@var)
            id = @arena.add_typed(InstanceVarNode.new(token.span, token.slice))
            advance
            id
          when Token::Kind::ClassVar
            # Phase 76: Class variable (@@var)
            id = @arena.add_typed(ClassVarNode.new(token.span, token.slice))
            advance
            id
          when Token::Kind::GlobalVar
            # Phase 75: Global variable ($var)
            id = @arena.add_typed(GlobalNode.new(token.span, token.slice))
            advance
            id
          when Token::Kind::Number
            id = @arena.add_typed(NumberNode.new(token.span, token.slice, token.number_kind.not_nil!))
            advance
            id
          when Token::Kind::String
            if percent_literal_token?(token)
              parse_percent_literal(token)
            else
              id = @arena.add_typed(StringNode.new(token.span, token.slice))
              advance
              id
            end
          when Token::Kind::Char
            # Phase 56: Character literals
            id = @arena.add_typed(CharNode.new(token.span, token.slice))
            advance
            id
          when Token::Kind::Regex
            # Phase 57: Regex literals
            id = @arena.add_typed(RegexNode.new(token.span, token.slice))
            advance
            id
          when Token::Kind::StringInterpolation
            # Phase 8: String interpolation
            parse_string_interpolation(token)
          when Token::Kind::Symbol
            # Phase 16: Symbol literal
            id = @arena.add_typed(SymbolNode.new(token.span, token.slice))
            advance
            id
          when Token::Kind::ColonColon
            # Phase 63: Absolute path (::TopLevel)
            parse_absolute_path
          when Token::Kind::Plus, Token::Kind::Minus, Token::Kind::Not, Token::Kind::Tilde, Token::Kind::AmpPlus, Token::Kind::AmpMinus
            # Unary operators (Phase 21: added Tilde for bitwise NOT)
            # Phase 89: Added AmpPlus and AmpMinus for wrapping unary operators
            op = token
            advance
            right = parse_expression(UNARY_PRECEDENCE)
            return PREFIX_ERROR if right.invalid?
            operand_span = node_span(right)
            unary_span = op.span.cover(operand_span)
            @arena.add_typed(UnaryNode.new(unary_span, op.slice, right))
          when Token::Kind::LParen
            parse_grouping
          when Token::Kind::LBracket
            if percent_literal_token?(token)
              parse_percent_literal(token)
            else
              # Phase 9: Array literal
              parse_array_literal
            end
          when Token::Kind::LBracePercent
            parse_percent_macro_control
          when Token::Kind::LBrace
            # Phase 103B: Check for macro control {% or macro expression {{ only inside macro bodies
            next_tok = peek_token
            if @macro_mode > 0
              case next_tok.kind
              when Token::Kind::Percent
                # {% if/for/... %}...{% end %}
                parse_percent_macro_control
              when Token::Kind::LBrace
                # {{ expression }}
                parse_percent_macro_expression
              else
                # Phase 14/15: Hash/NamedTuple/Tuple literal
                node = parse_hash_or_tuple
                if node.invalid?
                  # Try tolerant named tuple, then tuple
                  saved_index = @index
                  saved_prev  = @previous_token
                  saved_brace = @brace_depth
                  node = parse_brace_named_tuple_fallback
                  if node.invalid?
                    @index = saved_index
                    @previous_token = saved_prev
                    @brace_depth = saved_brace
                    node = parse_brace_tuple_fallback
                  end
                end
                node
              end
            else
              # Outside macro bodies treat {{...}} as nested literals (e.g. tuples)
              node = parse_hash_or_tuple
              if node.invalid?
                saved_index = @index
                saved_prev  = @previous_token
                saved_brace = @brace_depth
                node = parse_brace_named_tuple_fallback
                if node.invalid?
                  @index = saved_index
                  @previous_token = saved_prev
                  @brace_depth = saved_brace
                  node = parse_brace_tuple_fallback
                end
              end
              node
            end
          when Token::Kind::ThinArrow
            # Phase 74: Proc literal (->(x) { ... })
            parse_proc_literal
          when Token::Kind::DotDot, Token::Kind::DotDotDot
            # Beginless/endless/full range literal used as value (e.g., point_range: ..)
            range_token = token
            advance
            nil_begin = @arena.add_typed(NilNode.new(range_token.span))
            nil_end = @arena.add_typed(NilNode.new(range_token.span))
            @arena.add_typed(RangeNode.new(range_token.span, nil_begin, nil_end, range_token.kind == Token::Kind::DotDotDot))
          when Token::Kind::Operator
            # Phase IMPLICIT_RECEIVER: Handle implicit receiver (.method)
            # Phase ANNOTATIONS: Handle annotations (@[...])
            # Generic fallback for unhandled operators (e.g., macro operators)
            if slice_eq?(token.slice, ".")
              # Implicit receiver: .method → ImplicitObj.method
              parse_implicit_receiver_call(token)
            elsif slice_eq?(token.slice, "(")
              parse_grouping
            elsif slice_eq?(token.slice, "@")
              # Annotation: @[Name] or @[Name(args)]
              parse_annotation
            else
              emit_unexpected(token)
              advance
              PREFIX_ERROR
            end
          when Token::Kind::Else, Token::Kind::When, Token::Kind::Then,
               Token::Kind::Rescue, Token::Kind::Ensure
            # Phase 103D: These keywords can be identifiers if followed by " : " (type annotation)
            # Example: property else : String, property when : Int32
            # Note: if/unless are handled separately above (they have existing statement parsing)
            # Note: in/of/as are already allowed as identifiers in the Identifier case
            if next_comes_colon_space?
              # Treat as identifier
              identifier_token = token
              advance
              space_consumed = current_token.kind == Token::Kind::Whitespace
              skip_trivia

              # Check for type annotation: keyword : Type
              # After skip_trivia, we should be at the colon
              if space_consumed && current_token.kind == Token::Kind::Colon && @no_type_declaration == 0
                # This is type declaration: else : Type = value
                parse_type_declaration_from_identifier(identifier_token)
              else
                # Just an identifier reference
                @arena.add_typed(IdentifierNode.new(identifier_token.span, identifier_token.slice))
              end
            else
              # Not followed by " : ", so keyword used incorrectly
              emit_unexpected(token)
              advance
              PREFIX_ERROR
            end
          when Token::Kind::EOF
            PREFIX_ERROR
          else
            emit_unexpected(token)
            advance
            PREFIX_ERROR
          end
        end

        # Bare is_a?(Type) with implicit self receiver
        private def parse_is_a_bare : ExprId
          is_a_token = current_token
          advance
          skip_trivia

          # Expect '('
          unless current_token.kind == Token::Kind::LParen
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end
          lparen = current_token
          advance
          skip_trivia

          # Parse target type using type annotation parser
          type_start = current_token
          target_type = parse_type_annotation
          type_end = previous_token
          unless type_end
            emit_unexpected(type_start)
            return PREFIX_ERROR
          end
          skip_trivia

          # Expect ')'
          unless current_token.kind == Token::Kind::RParen
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end
          rparen = current_token
          advance

          # Receiver is implicit self
          self_node = @arena.add_typed(SelfNode.new(is_a_token.span))

          span = is_a_token.span
            .cover(lparen.span)
            .cover(type_start.span)
            .cover(type_end.span)
            .cover(rparen.span)

          @arena.add_typed(
            IsANode.new(
              span,
              self_node,
              target_type
            )
          )
        end

        # Phase 103H: Grouping (parentheses) - supports assignments like (x = y)
        # and tolerates newlines inside parentheses
        private def parse_grouping : ExprId
          lparen = current_token
          advance
          @paren_depth += 1  # Entering parentheses context
          # Parse first expression (allow assignments)
          expr = parse_op_assign
          return PREFIX_ERROR if expr.invalid?
          # Allow multiple expressions separated by ';' inside parentheses; return last
          loop do
            skip_whitespace_and_optional_newlines
            break unless current_token.kind == Token::Kind::Semicolon
            advance
            skip_whitespace_and_optional_newlines
            next_expr = parse_op_assign
            return PREFIX_ERROR if next_expr.invalid?
            expr = next_expr
          end
          expect_operator(Token::Kind::RParen)
          @paren_depth -= 1  # Exiting parentheses context
          closing_span = previous_token.try(&.span)
          grouping_span = cover_optional_spans(lparen.span, node_span(expr), closing_span)
          @arena.add_typed(GroupingNode.new(grouping_span, expr))
        end

        # Phase 9: Parse array literal [1, 2, 3] or [] of Type
        # Phase 103: Updated to support multi-line arrays
        private def parse_array_literal : ExprId
          lbracket = current_token
          advance
          @bracket_depth += 1  # Phase 103: entering brackets
          skip_whitespace_and_optional_newlines

          elements_b = SmallVec(ExprId, 4).new
          of_type_expr : ExprId? = nil

          # Check for closing bracket (empty array)
          if current_token.kind == Token::Kind::RBracket
            @bracket_depth -= 1  # Phase 103: exiting brackets
            advance
            skip_trivia

            # Phase 91: Check for "of Type" syntax
            if current_token.kind == Token::Kind::Of
              advance
              skip_trivia

              # Parse type expression (supports unions, generics, etc.)
              type_expr = parse_expression(0)
              return PREFIX_ERROR if type_expr.invalid?
              of_type_expr = type_expr
            end

            closing_span = previous_token.try(&.span) || lbracket.span
            array_span = lbracket.span.cover(closing_span)
            return @arena.add_typed(ArrayLiteralNode.new(
              array_span,
              elements_b.to_a,
              of_type_expr
            ))
          end

          # Parse array elements
          loop do
            element = parse_expression(0)
            if element.invalid?
              return PREFIX_ERROR
            end
            elements_b << element

            skip_whitespace_and_optional_newlines
            break if current_token.kind != Token::Kind::Comma

            advance  # consume comma
            skip_whitespace_and_optional_newlines

            # Allow trailing comma
            break if current_token.kind == Token::Kind::RBracket
          end

          # Expect closing bracket
          unless current_token.kind == Token::Kind::RBracket
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end

          @bracket_depth -= 1  # Phase 103: exiting brackets
          closing_bracket = current_token
          advance
          skip_trivia

          # Phase 91: Check for "of Type" after closing bracket
          if current_token.kind == Token::Kind::Of || (current_token.kind == Token::Kind::Identifier && slice_eq?(current_token.slice, "of"))
            advance
            skip_trivia

            # Parse type expression (supports unions, generics, etc.)
            type_expr = parse_expression(0)
            return PREFIX_ERROR if type_expr.invalid?
            of_type_expr = type_expr
          end

          array_span = lbracket.span.cover(closing_bracket.span)
          @arena.add_typed(ArrayLiteralNode.new(
            array_span,
            elements_b.to_a,
            of_type_expr
          ))
        end

        private def percent_literal_token?(token : Token) : Bool
          slice = token.slice
          slice.size >= 2 && slice[0] == '%'.ord
        end

        private def parse_percent_literal(token : Token) : ExprId
          literal_text = String.new(token.slice)
          info = percent_literal_info(literal_text)
          advance

          case info[:kind]
          when :word_array
            words = percent_literal_words(info[:content])
            build_percent_array_literal(words, token.span, as_symbols: false)
          when :symbol_array
            words = percent_literal_words(info[:content])
            build_percent_array_literal(words, token.span, as_symbols: true)
          else
            value = percent_literal_unescape(info[:content])
            slice = @string_pool.intern(value.to_slice)
            @arena.add_typed(StringNode.new(token.span, slice))
          end
        end

        private def percent_literal_info(literal : String) : NamedTuple(kind: Symbol, content: String)
          bytesize = literal.bytesize
          return {kind: :string, content: literal} if bytesize < 3 || literal[0] != '%'

          idx = 1
          type_char = nil
          if idx < bytesize
            char = literal.byte_at(idx).chr
            if char.ascii_letter?
              type_char = char
              idx += 1
            end
          end

          return {kind: :string, content: ""} if idx >= bytesize

          open_char = literal.byte_at(idx).chr
          closing_char = percent_closing_delimiter(open_char)
          idx += 1

          content_length = bytesize - idx - 1
          content_length = 0 if content_length < 0
          content = content_length > 0 ? literal.byte_slice(idx, content_length) : ""

          normalized_type = type_char ? type_char.downcase : nil
          kind = case normalized_type
          when 'w'
            :word_array
          when 'i'
            :symbol_array
          else
            :string
          end

          {kind: kind, content: content}
        rescue
          {kind: :string, content: literal}
        end

        private def percent_closing_delimiter(open_char : Char) : Char
          case open_char
          when '(' then ')'
          when '[' then ']'
          when '{' then '}'
          when '<' then '>'
          when '|' then '|'
          else
            open_char
          end
        end

        private def percent_literal_words(content : String) : Array(String)
          return [] of String if content.empty?

          words = [] of String
          current = String.new
          i = 0
          bytesize = content.bytesize

          while i < bytesize
            char = content.byte_at(i).chr
            if char.ascii_whitespace?
              unless current.empty?
                words << current
                current = String.new
              end
              i += 1
              next
            elsif char == '\\'
              i += 1
              if i < bytesize
                current += content.byte_at(i).chr
              end
              i += 1
              next
            else
              current += char
              i += 1
            end
          end

          words << current unless current.empty?
          words
        end

        private def build_percent_array_literal(words : Array(String), span : Span, as_symbols : Bool) : ExprId
          elements = Array(ExprId).new(words.size)
          words.each do |word|
            if as_symbols
              symbol_text = ":" + word
              slice = @string_pool.intern(symbol_text.to_slice)
              node = @arena.add_typed(SymbolNode.new(span, slice))
              elements << node
            else
              slice = @string_pool.intern(word.to_slice)
              node = @arena.add_typed(StringNode.new(span, slice))
              elements << node
            end
          end

          @arena.add_typed(ArrayLiteralNode.new(span, elements))
        end

        private def percent_literal_unescape(content : String) : String
          return content unless content.includes?('\\')

          String.build do |io|
            i = 0
            bytesize = content.bytesize
            while i < bytesize
              byte = content.byte_at(i)
              if byte == '\\'.ord && i + 1 < bytesize
                i += 1
                escaped = content.byte_at(i).chr
                case escaped
                when 'n' then io << '\n'
                when 'r' then io << '\r'
                when 't' then io << '\t'
                else
                  io << escaped
                end
              else
                io.write_byte(byte)
              end
              i += 1
            end
          end
        end

        # Phase 14: Parse hash literal {"key" => value} or {} of K => V
        private def parse_hash_literal : ExprId
          lbrace = current_token
          advance  # consume {
          @brace_depth += 1  # Phase 103J: Track brace depth for newline handling
          skip_whitespace_and_optional_newlines

          entries_b = SmallVec(HashEntry, 4).new
          of_key_type : Slice(UInt8)? = nil
          of_value_type : Slice(UInt8)? = nil

          # Check for closing brace (empty hash)
          if current_token.kind == Token::Kind::RBrace
            @brace_depth -= 1  # Phase 103J
            advance  # consume }
            skip_trivia

            # Check for "of K => V" syntax (accept keyword Of or identifier "of")
            if current_token.kind == Token::Kind::Of ||
               (current_token.kind == Token::Kind::Identifier && slice_eq?(current_token.slice, "of"))
              advance
              skip_trivia

              # Parse key type
              key_type_token = current_token
              if key_type_token.kind.in?(Token::Kind::Identifier, Token::Kind::Self, Token::Kind::ColonColon)
                # Reuse type annotation parser to support paths/generics
                type_start = current_token
                type_slice = parse_type_annotation
                if previous_token
                  of_key_type = type_slice
                else
                  emit_unexpected(type_start)
                  return PREFIX_ERROR
                end
                skip_trivia

                # Expect =>
                unless current_token.kind == Token::Kind::Arrow || (current_token.kind == Token::Kind::Operator && slice_eq?(current_token.slice, "=>"))
                  emit_unexpected(current_token)
                  return PREFIX_ERROR
                end
                advance  # consume =>
                skip_trivia

                # Parse value type
                value_start = current_token
                value_slice = parse_type_annotation
                if previous_token
                  of_value_type = value_slice
                else
                  emit_unexpected(value_start)
                  return PREFIX_ERROR
                end
              else
                emit_unexpected(key_type_token)
                return PREFIX_ERROR
              end
            end

            closing_span = previous_token.try(&.span) || lbrace.span
            hash_span = lbrace.span.cover(closing_span)
            return @arena.add_typed(HashLiteralNode.new(
              hash_span,
              entries,
              of_key_type,
              of_value_type
            ))
          end

          # Parse hash entries: key => value, key => value, ...
          loop do
            # Parse key
            key = parse_expression(0)
            if key.invalid?
              @brace_depth -= 1  # Phase 103J
              return PREFIX_ERROR
            end
            key_span = node_span(key)

            skip_whitespace_and_optional_newlines  # Phase 103J

            # Expect =>
            unless current_token.kind == Token::Kind::Arrow
              emit_unexpected(current_token)
              @brace_depth -= 1  # Phase 103J
              return PREFIX_ERROR
            end
            arrow_token = current_token
            advance  # consume =>
            skip_whitespace_and_optional_newlines  # Phase 103J

            # Parse value
            value = parse_expression(0)
            if value.invalid?
              @brace_depth -= 1  # Phase 103J
              return PREFIX_ERROR
            end
            value_span = node_span(value)

            # Create entry with precise spans for LSP/diagnostics
            entry_span = key_span.cover(value_span)
            entries_b << HashEntry.new(key, value, entry_span, arrow_token.span)

            skip_whitespace_and_optional_newlines  # Phase 103J
            break if !(current_token.kind == Token::Kind::Comma)

            advance  # consume comma
            skip_whitespace_and_optional_newlines  # Phase 103J

            # Allow trailing comma
            if current_token.kind == Token::Kind::RBrace
              break
            end
          end

          # Expect closing brace
          unless current_token.kind == Token::Kind::RBrace
            emit_unexpected(current_token)
            @brace_depth -= 1  # Phase 103J
            return PREFIX_ERROR
          end

          @brace_depth -= 1  # Phase 103J
          closing_brace = current_token
          advance

          hash_span = lbrace.span.cover(closing_brace.span)
          @arena.add_typed(HashLiteralNode.new(
            hash_span,
            entries_b.to_a,
            of_key_type,
            of_value_type
          ))
        end

        # Phase 14/15/70: Disambiguate hash vs tuple vs named tuple literal
        # Hash: {"key" => value} or {} of K => V
        # Tuple: {1, 2, 3} or {value} or {value,}
        # Named Tuple: {name: "value", age: 30}
        #
        # Strategy: Look ahead after first element
        # - If we see "=>" → hash
        # - If we see ":" and first_elem is identifier → named tuple
        # - If we see "," or "}" → tuple
        # - Empty "{}" → hash (existing behavior)
        private def parse_hash_or_tuple : ExprId
          lbrace = current_token
          advance  # consume {
          @brace_depth += 1  # Phase 103J: Track brace depth for newline handling
          skip_whitespace_and_optional_newlines

          # Empty {} → hash
          if current_token.kind == Token::Kind::RBrace
            # Empty hash - delegate to parse_hash_literal
            return parse_hash_literal_from_lbrace(lbrace)
          end

          # Parse first element (key for hash/named tuple, value for tuple)
          # Fast path for named tuple: identifier followed by ':' → avoid full expression parse to not
          # interfere with call-without-parens heuristics.
          first_elem : ExprId
          saved_index = @index
          tok = current_token
          if tok.kind == Token::Kind::Identifier
            ident_token = tok
            # Peek next non-trivia for ':'
            nt = peek_next_non_trivia
            if nt.kind == Token::Kind::Colon
              # Consume identifier and leave ':' for the named tuple parser
              advance
              first_elem = @arena.add_typed(IdentifierNode.new(ident_token.span, @string_pool.intern(ident_token.slice)))
              # Do not skip trivia here; parse_named_tuple_literal_continued expects ':' next
            else
              # Fallback to general expression parsing
              @no_type_declaration += 1
              first_elem = parse_expression(0)
              @no_type_declaration -= 1
              if first_elem.invalid?
                @brace_depth -= 1
                return PREFIX_ERROR
              end
              skip_whitespace_and_optional_newlines
            end
          else
            @no_type_declaration += 1
            first_elem = parse_expression(0)
            @no_type_declaration -= 1
            if first_elem.invalid?
              @brace_depth -= 1
              return PREFIX_ERROR
            end
            skip_whitespace_and_optional_newlines
          end

          # Check what follows
          case current_token.kind
          when Token::Kind::Arrow
            # "=>" → this is a hash
            return parse_hash_literal_continued(lbrace, first_elem)
          when Token::Kind::Colon
            # ":" → named tuple (expression context: allow identifier and certain keyword labels like 'nil')
            first_node = @arena[first_elem]
            kind = Frontend.node_kind(first_node)
            if kind == Frontend::NodeKind::Identifier || kind == Frontend::NodeKind::Nil || kind == Frontend::NodeKind::Bool
              return parse_named_tuple_literal_continued(lbrace, first_elem)
            else
              emit_unexpected(current_token)
              return PREFIX_ERROR
            end
          when Token::Kind::Comma, Token::Kind::RBrace
            # "," or "}" → this is a tuple
            return parse_tuple_literal_continued(lbrace, first_elem)
          else
            # Unexpected token
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end
        end

        # Phase 15: Continue parsing tuple literal after first element
        private def parse_tuple_literal_continued(lbrace : Token, first_elem : ExprId) : ExprId
          elements = [first_elem]

          # Check for comma or closing brace
          loop do
            case current_token.kind
            when Token::Kind::RBrace
              # End of tuple
              break
            when Token::Kind::Comma
              advance  # consume comma
              skip_whitespace_and_optional_newlines

              # Allow trailing comma
              if current_token.kind == Token::Kind::RBrace
                break
              end

              # Parse next element
              elem = parse_expression(0)
              return PREFIX_ERROR if elem.invalid?
              elements << elem
              skip_whitespace_and_optional_newlines
            else
              emit_unexpected(current_token)
              return PREFIX_ERROR
            end
          end

          # Expect closing brace
          unless current_token.kind == Token::Kind::RBrace
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end

          closing_brace = current_token
          advance
          # Balance brace depth started in parse_hash_or_tuple
          @brace_depth -= 1 if @brace_depth > 0

          tuple_span = lbrace.span.cover(closing_brace.span)
          @arena.add_typed(TupleLiteralNode.new(
            tuple_span,
            elements
          ))
        end

        # Tolerant fallback for tuple literal: {expr, expr, ...}
        # Used only when parse_hash_or_tuple fails (should be rare)
        private def parse_brace_tuple_fallback : ExprId
          lbrace = current_token
          advance
          @brace_depth += 1
          elements = [] of ExprId
          skip_whitespace_and_optional_newlines

          unless current_token.kind == Token::Kind::RBrace
            loop do
              elem = parse_expression(0)
              return PREFIX_ERROR if elem.invalid?
              elements << elem
              skip_whitespace_and_optional_newlines
              break if current_token.kind == Token::Kind::RBrace
              unless current_token.kind == Token::Kind::Comma
                emit_unexpected(current_token)
                return PREFIX_ERROR
              end
              advance
              skip_whitespace_and_optional_newlines
            end
          end

          closing = current_token
          advance
          @brace_depth -= 1 if @brace_depth > 0
          span = lbrace.span.cover(closing.span)
          @arena.add_typed(TupleLiteralNode.new(span, elements))
        end

        # Tolerant fallback for named tuple literal: {key: expr, ...}
        # Used only when parse_hash_or_tuple fails
        private def parse_brace_named_tuple_fallback : ExprId
          lbrace = current_token
          advance
          @brace_depth += 1
          entries_b = SmallVec(NamedTupleEntry, 4).new
          skip_whitespace_and_optional_newlines

          unless current_token.kind == Token::Kind::RBrace
            loop do
              # key (identifier/keyword-like)
              key_token = current_token
              unless key_token.kind == Token::Kind::Identifier ||
                     key_token.kind == Token::Kind::Nil ||
                     key_token.kind == Token::Kind::True ||
                     key_token.kind == Token::Kind::False
                emit_unexpected(key_token)
                return PREFIX_ERROR
              end
              key_slice = key_token.slice
              key_span  = key_token.span
              advance
              skip_whitespace_and_optional_newlines

              # ':'
              unless current_token.kind == Token::Kind::Colon
                emit_unexpected(current_token)
                return PREFIX_ERROR
              end
              advance
              skip_whitespace_and_optional_newlines

              # value expression
              value = parse_expression(0)
              return PREFIX_ERROR if value.invalid?
              value_span = @arena[value].span
              skip_whitespace_and_optional_newlines

              entries_b << NamedTupleEntry.new(key_slice, value, key_span, value_span)

              break if current_token.kind == Token::Kind::RBrace
              unless current_token.kind == Token::Kind::Comma
                emit_unexpected(current_token)
                return PREFIX_ERROR
              end
              advance
              skip_whitespace_and_optional_newlines
            end
          end

          closing = current_token
          advance
          @brace_depth -= 1 if @brace_depth > 0
          span = lbrace.span.cover(closing.span)
          @arena.add_typed(NamedTupleLiteralNode.new(span, entries_b.to_a))
        end

        # Phase 70: Continue parsing named tuple literal after first key
        private def parse_named_tuple_literal_continued(lbrace : Token, first_key_expr : ExprId) : ExprId
          entries_b = SmallVec(NamedTupleEntry, 4).new

          # Get first key from first_key_expr (usually Identifier, but allow certain keywords used as labels)
          first_key_node = @arena[first_key_expr]
          first_key_span = first_key_node.span
          first_key = case Frontend.node_kind(first_key_node)
            when Frontend::NodeKind::Identifier
              Frontend.node_literal(first_key_node).not_nil!
            when Frontend::NodeKind::Nil
              @string_pool.intern("nil".to_slice)
            when Frontend::NodeKind::Bool
              # 'true'/'false' as labels are rare; map to their text if they appear
              # Here we fall back to text based on span content
              # Note: if needed, extend the AST nodes to expose exact literal text
              @string_pool.intern("true".to_slice)  # default; corrected below if false
            else
              Frontend.node_literal(first_key_node) || @string_pool.intern("".to_slice)
            end

          # Expect colon
          unless current_token.kind == Token::Kind::Colon
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end
          advance  # consume :
          skip_trivia

          # Parse first value
          first_value = parse_expression(0)
          return PREFIX_ERROR if first_value.invalid?
          first_value_span = @arena[first_value].span
          skip_trivia

          # Create first entry
          entries_b << NamedTupleEntry.new(
            first_key,
            first_value,
            first_key_span,
            first_value_span
          )

          # Parse remaining entries
          loop do
            # Allow newlines between entries
            skip_whitespace_and_optional_newlines
            # Be extra permissive: also tolerate standalone newlines
            if current_token.kind == Token::Kind::Newline
              skip_statement_end
            end
            case current_token.kind
            when Token::Kind::Newline
              advance
              next
            when Token::Kind::RBrace
              # End of named tuple
              break
            when Token::Kind::Comma
              advance  # consume comma
              skip_whitespace_and_optional_newlines

              # Allow trailing comma
              if current_token.kind == Token::Kind::RBrace
                break
              end

              # Parse key (identifier or selected keywords like nil/true/false)
              key_token = current_token
              valid_key = key_token.kind == Token::Kind::Identifier ||
                          key_token.kind == Token::Kind::Nil ||
                          key_token.kind == Token::Kind::True ||
                          key_token.kind == Token::Kind::False
              unless valid_key
                emit_unexpected(key_token)
                return PREFIX_ERROR
              end
              key = key_token.slice  # TIER 2.3: Zero-copy slice (includes keyword text)
              key_span = key_token.span
              advance
              skip_whitespace_and_optional_newlines

              # Expect colon
              unless current_token.kind == Token::Kind::Colon
                emit_unexpected(current_token)
                return PREFIX_ERROR
              end
              advance  # consume :
              skip_whitespace_and_optional_newlines

              # Parse value
              value = parse_expression(0)
              return PREFIX_ERROR if value.invalid?
              value_span = @arena[value].span
              skip_whitespace_and_optional_newlines

              # Create entry
              entries_b << NamedTupleEntry.new(
                key,
                value,
                key_span,
                value_span
              )
            else
              emit_unexpected(current_token)
              return PREFIX_ERROR
            end
          end

          # Expect closing brace
          unless current_token.kind == Token::Kind::RBrace
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end

          closing_brace = current_token
          advance

          named_tuple_span = lbrace.span.cover(closing_brace.span)
          @arena.add_typed(NamedTupleLiteralNode.new(
            named_tuple_span,
            entries_b.to_a
          ))
        end

        # Phase 14: Parse empty hash literal
        private def parse_hash_literal_from_lbrace(lbrace : Token) : ExprId
          # Current token is RBrace
          closing_brace = current_token
          advance  # consume }
          # Balance the brace depth increment done in parse_hash_or_tuple
          @brace_depth -= 1 if @brace_depth > 0
          skip_trivia

          entries = [] of HashEntry
          of_key_type : Slice(UInt8)? = nil
          of_value_type : Slice(UInt8)? = nil

          # Check for "of K => V" syntax
          if current_token.kind == Token::Kind::Of
            advance
            skip_trivia

            key_type_slice = parse_type_annotation
            if key_type_slice.empty?
              emit_unexpected(current_token)
              return PREFIX_ERROR
            end
            of_key_type = key_type_slice
            skip_trivia

            unless current_token.kind == Token::Kind::Arrow
              emit_unexpected(current_token)
              return PREFIX_ERROR
            end
            advance  # consume =>
            skip_trivia

            value_type_slice = parse_type_annotation
            if value_type_slice.empty?
              emit_unexpected(current_token)
              return PREFIX_ERROR
            end
            of_value_type = value_type_slice
            skip_trivia
          end

          # Use lbrace..closing_brace as span
          closing_span = lbrace.span.cover(closing_brace.span)
          @arena.add_typed(HashLiteralNode.new(
            closing_span,
            entries,
            of_key_type,
            of_value_type
          ))
        end

        # Phase 14: Continue parsing hash literal after first key
        # NOTE: @brace_depth already incremented by parse_hash_or_tuple
        private def parse_hash_literal_continued(lbrace : Token, first_key : ExprId) : ExprId
          # Current token should be Arrow
          unless current_token.kind == Token::Kind::Arrow
            emit_unexpected(current_token)
            @brace_depth -= 1  # Phase 103J
            return PREFIX_ERROR
          end
          arrow_token = current_token
          advance  # consume =>
          skip_whitespace_and_optional_newlines  # Phase 103J

          # Parse first value
          first_value = parse_expression(0)
          if first_value.invalid?
            @brace_depth -= 1  # Phase 103J
            return PREFIX_ERROR
          end

          key_span = node_span(first_key)
          value_span = node_span(first_value)
          entry_span = key_span.cover(value_span)
          skip_whitespace_and_optional_newlines  # Phase 103J

          entries_b = SmallVec(HashEntry, 4).new
          entries_b << HashEntry.new(first_key, first_value, entry_span, arrow_token.span)

          # Parse remaining entries
          loop do
            unless current_token.kind == Token::Kind::Comma
              break
            end

            advance  # consume comma
            skip_whitespace_and_optional_newlines  # Phase 103J

            # Allow trailing comma
            if current_token.kind == Token::Kind::RBrace
              break
            end

            # Parse key
            key = parse_expression(0)
            if key.invalid?
              @brace_depth -= 1  # Phase 103J
              return PREFIX_ERROR
            end
            key_span = node_span(key)
            skip_whitespace_and_optional_newlines  # Phase 103J

            # Expect =>
            unless current_token.kind == Token::Kind::Arrow
              emit_unexpected(current_token)
              @brace_depth -= 1  # Phase 103J
              return PREFIX_ERROR
            end
            arrow_token = current_token
            advance  # consume =>
            skip_whitespace_and_optional_newlines  # Phase 103J

            # Parse value
            value = parse_expression(0)
            if value.invalid?
              @brace_depth -= 1  # Phase 103J
              return PREFIX_ERROR
            end
            value_span = node_span(value)
            entry_span = key_span.cover(value_span)
            skip_whitespace_and_optional_newlines  # Phase 103J

            entries_b << HashEntry.new(key, value, entry_span, arrow_token.span)
          end

          # Expect closing brace
          unless current_token.kind == Token::Kind::RBrace
            emit_unexpected(current_token)
            @brace_depth -= 1  # Phase 103J
            return PREFIX_ERROR
          end

          @brace_depth -= 1  # Phase 103J
          closing_brace = current_token
          advance

          # Optional trailing "of K => V" after non-empty hash
          of_key_type : Slice(UInt8)? = nil
          of_value_type : Slice(UInt8)? = nil
          skip_trivia
          if current_token.kind == Token::Kind::Of || (current_token.kind == Token::Kind::Identifier && slice_eq?(current_token.slice, "of"))
            advance
            skip_trivia
            key_type = parse_type_annotation
            if key_type.empty?
              emit_unexpected(current_token)
              return PREFIX_ERROR
            end
            of_key_type = key_type
            skip_trivia
            unless current_token.kind == Token::Kind::Arrow || (current_token.kind == Token::Kind::Operator && slice_eq?(current_token.slice, "=>"))
              emit_unexpected(current_token)
              return PREFIX_ERROR
            end
            advance
            skip_trivia
            value_type = parse_type_annotation
            if value_type.empty?
              emit_unexpected(current_token)
              return PREFIX_ERROR
            end
            of_value_type = value_type
          end

          hash_span = lbrace.span.cover(closing_brace.span)
          @arena.add_typed(HashLiteralNode.new(
            hash_span,
            entries_b.to_a,
            of_key_type,
            of_value_type
          ))
        end

        # Phase 8: Parse string interpolation
        # Converts "Hello, #{name}!" into StringPiece array:
        # - Text("Hello, ")
        # - Expression(name_expr_id)
        # - Text("!")
        private def parse_string_interpolation(token : Token) : ExprId
          content = String.new(token.slice)
          pieces_b = SmallVec(StringPiece, 8).new
          i = 0

          while i < content.size
            # Find next #{
            text_start = i
            while i < content.size
              break if i + 1 < content.size && content[i] == '#' && content[i + 1] == '{'
              i += 1
            end

            # Add text piece if any
            if i > text_start
              pieces_b << StringPiece.text(content[text_start...i])
            end

            break if i >= content.size

            # Skip #{
            i += 2

            # Find matching } (handle nested braces)
            expr_start = i
            brace_depth = 1
            while i < content.size && brace_depth > 0
              if content[i] == '{'
                brace_depth += 1
              elsif content[i] == '}'
                brace_depth -= 1
              end
              i += 1 if brace_depth > 0
            end

            # Parse expression
            expr_text = content[expr_start...i]
            expr_id = parse_interpolation_expression(expr_text)
            pieces_b << StringPiece.expression(expr_id)

            # Move past the closing }
            i += 1
          end

          advance
          @arena.add_typed(StringInterpolationNode.new(
            token.span,
            pieces_b.to_a
          ))
        end

        # Determine if an expression can accept a trailing block
        # Only calls and member accesses (and identifiers that can form calls)
        # are valid block receivers. Literals like Nil/Number/etc. are not.
        private def can_attach_block_to?(expr : ExprId) : Bool
          node = @arena[expr]
          case Frontend.node_kind(node)
          when Frontend::NodeKind::Call
            true
          when Frontend::NodeKind::MemberAccess
            true
          when Frontend::NodeKind::Identifier
            true
          else
            false
          end
        end

        # Helper: Parse expression text from interpolation
        # Creates sub-parser with SHARED arena (no copying needed!)
        private def parse_interpolation_expression(expr_text : String) : ExprId
          # Create sub-parser that adds nodes to OUR arena
          sub_lexer = Lexer.new(expr_text)
          sub_parser = Parser.new(sub_lexer, @arena)  # Share arena!

          # Parse expression - nodes go directly into our arena
          expr_id = sub_parser.parse_expression(0)

          expr_id  # Already in our arena
        end

        # Phase 72: Parse method call with arguments (positional and/or named)
        # Examples:
        #   foo()             → no args
        #   foo(1, 2)         → positional args
        #   foo(x: 1, y: 2)   → named args
        #   foo(1, y: 2)      → mixed (positional first, then named)
        # Phase 103: Updated to support multi-line arguments
        private def parse_parenthesized_call(callee : ExprId) : ExprId
          lparen = current_token
          advance
          @paren_depth += 1  # Phase 103: entering parentheses
          skip_whitespace_and_optional_newlines

          args_b = SmallVec(ExprId, 4).new
          named_b = SmallVec(NamedArgument, 2).new

          # Empty call: foo()
          unless current_token.kind == Token::Kind::RParen
            loop do
              skip_whitespace_and_optional_newlines
              break if current_token.kind == Token::Kind::RParen

              # Phase 101: Check for block shorthand (&.method) or block capture (&block)
              # This creates: { |__arg0| __arg0.method } or passes block argument
              if current_token.kind == Token::Kind::Amp
                # Save position in case this is not block shorthand
                amp_token = current_token
                advance
                skip_whitespace_and_optional_newlines

                # Check if followed by dot (member access)
                if current_token.kind == Token::Kind::Operator
                  # This is block shorthand: try &.method (with space: Amp + Operator)
                  arg_expr = parse_block_shorthand(amp_token)
                  return PREFIX_ERROR if arg_expr.invalid?
                elsif current_token.kind == Token::Kind::Identifier || current_token.kind == Token::Kind::InstanceVar
                  # Phase 103I: Block capture argument: foo(&block) or foo(&@block)
                  # This passes a block as an argument (not block shorthand)
                  identifier_token = current_token
                  advance
                  identifier_span = identifier_token.span
                  identifier_node = @arena.add_typed(IdentifierNode.new(identifier_span, @string_pool.intern(identifier_token.slice)))

                  # Create block argument node (UnaryNode with & operator)
                  arg_span = amp_token.span.cover(identifier_span)
                  arg_expr = @arena.add_typed(UnaryNode.new(arg_span, amp_token.slice, identifier_node))
                else
                  # Not block shorthand or capture, rewind and parse normally
                  # This handles cases like: foo(& other_expr)
                  unadvance  # Go back to Amp token
                  # Disable type declarations to allow identifier: syntax for named args
                  @no_type_declaration += 1
                  arg_expr = parse_expression(0)
                  @no_type_declaration -= 1
                  return PREFIX_ERROR if arg_expr.invalid?
                end
              elsif current_token.kind == Token::Kind::AmpDot
                # Phase 101: Block shorthand without space: try &.method (AmpDot token)
                # In argument context, AmpDot is block shorthand, not safe navigation
                amp_token = current_token
                advance
                skip_whitespace_and_optional_newlines
                arg_expr = parse_block_shorthand(amp_token)
                return PREFIX_ERROR if arg_expr.invalid?
              else
                # Phase 68: Splat arguments (*args, **kwargs)
                if current_token.kind == Token::Kind::Star || current_token.kind == Token::Kind::StarStar
                  star_token = current_token
                  advance
                  skip_whitespace_and_optional_newlines
                  value_expr = parse_op_assign
                  return PREFIX_ERROR if value_expr.invalid?
                  span = star_token.span.cover(@arena[value_expr].span)
                  arg_expr = @arena.add_typed(SplatNode.new(span, value_expr))
                  args_b << arg_expr
                  skip_whitespace_and_optional_newlines
                  goto_comma = current_token.kind == Token::Kind::Comma
                  if goto_comma
                    advance
                    skip_whitespace_and_optional_newlines
                    next
                  else
                    # Allow immediate closing paren
                    next
                  end
                end
                # Phase NAMED_ARGUMENTS: Check if this is named argument BEFORE parsing expression
                # Pattern: identifier/keyword : value
                # This handles keywords like 'of:' which wouldn't create Identifier nodes
                if named_arg_start?
                  # Named argument detected
                  name_token = current_token
                  name_slice = name_token.slice
                  name_span = name_token.span
                  advance  # consume name (identifier/keyword)
                  skip_whitespace_and_optional_newlines

                  # Expect colon
                  unless current_token.kind == Token::Kind::Colon
                    emit_unexpected(current_token)
                    return PREFIX_ERROR
                  end
                  advance  # consume ':'
                  skip_whitespace_and_optional_newlines

                  # Parse value expression (allow assignments inside args)
                  @no_type_declaration += 1
                  value_expr = parse_op_assign
                  @no_type_declaration -= 1
                  return PREFIX_ERROR if value_expr.invalid?
                  value_span = @arena[value_expr].span

                  # Create NamedArgument (zero-copy)
                  named_b << NamedArgument.new(name_slice, value_expr, name_span, value_span)
                  skip_whitespace_and_optional_newlines
                else
                  # Parse first argument expression (allow assignments)
                  # Disable type declarations to allow identifier: syntax for named args
                  @no_type_declaration += 1
                  arg_expr = parse_op_assign
                  @no_type_declaration -= 1
                  return PREFIX_ERROR if arg_expr.invalid?
                  skip_whitespace_and_optional_newlines

                  # Check if this is named argument (identifier followed by colon)
                  # This handles case where identifier was already parsed
                  if current_token.kind == Token::Kind::Colon
                    arg_node = @arena[arg_expr]
                    if Frontend.node_kind(arg_node) == Frontend::NodeKind::Identifier
                      # Named argument: name: value (zero-copy)
                      name_slice = Frontend.node_literal(arg_node).not_nil!
                      name_span = arg_node.span

                      advance  # consume ':'
                      skip_whitespace_and_optional_newlines

                      # Parse value expression
                      # Disable type declarations in value (may contain nested named tuples/args)
                      @no_type_declaration += 1
                      value_expr = parse_op_assign
                      @no_type_declaration -= 1
                      return PREFIX_ERROR if value_expr.invalid?
                      value_span = @arena[value_expr].span

                      # Create NamedArgument
                      named_b << NamedArgument.new(name_slice, value_expr, name_span, value_span)
                  skip_whitespace_and_optional_newlines
                else
                  # Expression followed by colon is invalid
                  emit_unexpected(current_token)
                  return PREFIX_ERROR
                end
              else
                # Positional argument
                args_b << arg_expr
              end
            end  # close if named_arg_start?
          end  # close else from Amp/AmpDot check

              break unless current_token.kind == Token::Kind::Comma
              advance  # consume comma
              skip_whitespace_and_optional_newlines

              # Handle trailing comma: foo(x: 1, y: 2,)
              break if current_token.kind == Token::Kind::RParen
            end
          end

          expect_operator(Token::Kind::RParen)
          @paren_depth -= 1  # Phase 103: exiting parentheses

          # Materialize arrays once, then compute span cheaply (closing paren already consumed)
          args = args_b.to_a
          named_args = named_b.to_a
          closing_span = previous_token.try(&.span)
          call_span = if named_args.size > 0
            node_span(callee).cover(named_args.last.span)
          elsif args.size > 0
            node_span(callee).cover(@arena[args.last].span)
          elsif closing_span
            node_span(callee).cover(closing_span)
          else
            node_span(callee)
          end

          # Create Call node with both positional and named args
          @arena.add_typed(CallNode.new(
            call_span,
            callee,
            args,
            nil,  # block
            named_args.empty? ? nil : named_args
          ))
        end

        # Phase 103: Updated to support multi-line indexing
        private def parse_index(target : ExprId) : ExprId
          lbracket = current_token
          advance
          @bracket_depth += 1  # Phase 103: entering brackets
          indexes_b = SmallVec(ExprId, 3).new
          skip_whitespace_and_optional_newlines
          unless current_token.kind == Token::Kind::RBracket
            loop do
              # Support splat inside index: obj[*expr] or obj[**named]
              if current_token.kind == Token::Kind::Star || current_token.kind == Token::Kind::StarStar
                star_tok = current_token
                advance
                skip_trivia
                value = parse_expression(0)
                return PREFIX_ERROR if value.invalid?
                span = star_tok.span.cover(@arena[value].span)
                arg = @arena.add_typed(SplatNode.new(span, value))
                indexes_b << arg
              else
                expr = parse_expression(0)
                indexes_b << expr unless expr.invalid?
              end
              skip_whitespace_and_optional_newlines
              break unless current_token.kind == Token::Kind::Comma
              advance
              skip_whitespace_and_optional_newlines
              # Allow trailing comma before closing bracket: foo[1, 2, ]
              break if current_token.kind == Token::Kind::RBracket
            end
          end
          @bracket_depth -= 1  # Phase 103: exiting brackets
          expect_operator(Token::Kind::RBracket)
          # Compute span without allocating a spans array
          acc_span = node_span(target).cover(lbracket.span)
          indexes_b.each { |idx| acc_span = acc_span.cover(node_span(idx)) }
          if closing_span = previous_token.try(&.span)
            acc_span = acc_span.cover(closing_span)
          end

          if current_token.kind == Token::Kind::Question
            question_token = current_token
            advance

            # Build callee: target.[]?
            method_slice = @string_pool.intern("[]?".to_slice)
            callee_span = node_span(target).cover(question_token.span)
            callee = @arena.add_typed(
              MemberAccessNode.new(
                callee_span,
                target,
                method_slice
              )
            )

            # Include '?' in the call span
            call_span = acc_span.cover(question_token.span)
            indexes = indexes_b.to_a
            return @arena.add_typed(
              CallNode.new(
                call_span,
                callee,
                indexes
              )
            )
          end

          index_span = acc_span
          @arena.add_typed(IndexNode.new(index_span, target, indexes_b.to_a))
        end

        private def parse_member_access(receiver : ExprId) : ExprId
          dot = current_token
          advance
          skip_trivia
          member_token = current_token

          # Phase 44: Check for .as(Type) type cast
          if member_token.kind == Token::Kind::As
            return parse_as_cast(receiver, dot, member_token)
          end

          # Phase 45: Check for .as?(Type) safe cast
          if member_token.kind == Token::Kind::AsQuestion
            return parse_as_safe_cast(receiver, dot, member_token)
          end

          # Phase 93: Check for .is_a?(Type) type check
          if member_token.kind == Token::Kind::IsA
            return parse_is_a(receiver, dot, member_token)
          end

          # Phase 94: Check for .responds_to?(:method) method check
          if member_token.kind == Token::Kind::RespondsTo
            return parse_responds_to(receiver, dot, member_token)
          end

          if member_token.kind == Token::Kind::Identifier
            member_span = node_span(receiver).cover(dot.span).cover(member_token.span)
            node = @arena.add_typed(
              MemberAccessNode.new(
                member_span,
                receiver,
                member_token.slice
              )
            )
            advance

            # Check if this member access is followed by arguments without parentheses
            # Example: Foo.bar 1, b: 2 → CallNode with MemberAccessNode as callee
            # Must check for space (not newline!) like original Crystal parser
            space_consumed = false
            next_token_view = current_token
            if next_token_view.kind == Token::Kind::Whitespace
              advance  # Consume space
              space_consumed = true
              next_token_view = current_token
            elsif next_token_view.span.start_line == member_token.span.end_line &&
                  next_token_view.span.start_offset > member_token.span.end_offset
              # Heuristic: even without explicit whitespace tokens (default lexing),
              # detect a gap on the same line to allow DSL-style calls like
              # json.field "value", foo
              space_consumed = true
            end

            if space_consumed && @parsing_call_args == 0
              token = current_token
              if call_without_parens_disallowed?(token)
                node
              else
                # Try to parse arguments
                @parsing_call_args += 1

                args_b = SmallVec(ExprId, 4).new
                named_b = SmallVec(NamedArgument, 2).new

                loop do
                  # Stop parsing arguments if a 'do' block starts here; let postfix loop attach it.
                  break if current_token.kind == Token::Kind::Do

                  # Parse one argument
                  # 1) Splat arguments: * / **
                  if current_token.kind == Token::Kind::Star || current_token.kind == Token::Kind::StarStar
                    star_token = current_token
                    advance
                    skip_trivia
                    value_expr = parse_op_assign
                    if value_expr.invalid?
                      @parsing_call_args -= 1
                      return PREFIX_ERROR
                    end
                    span = star_token.span.cover(@arena[value_expr].span)
                    arg = @arena.add_typed(SplatNode.new(span, value_expr))
                  # 2) Block shorthand / capture: &.method or &ident
                  elsif current_token.kind == Token::Kind::Amp
                    amp_token = current_token
                    advance
                    skip_trivia
                    if current_token.kind == Token::Kind::Operator
                      # &.method — block shorthand
                      arg = parse_block_shorthand(amp_token)
                    elsif current_token.kind == Token::Kind::Identifier || current_token.kind == Token::Kind::InstanceVar
                      # &block — block capture argument
                      ident = current_token
                      advance
                      ident_span = ident.span
                      ident_node = if ident.kind == Token::Kind::Identifier
                        @arena.add_typed(IdentifierNode.new(ident_span, @string_pool.intern(ident.slice)))
                      else
                        @arena.add_typed(InstanceVarNode.new(ident_span, ident.slice))
                      end
                      arg_span = amp_token.span.cover(ident_span)
                      arg = @arena.add_typed(UnaryNode.new(arg_span, amp_token.slice, ident_node))
                    else
                      # Rewind one token and parse normally
                      unadvance
                      @no_type_declaration += 1
                      arg = parse_op_assign
                      @no_type_declaration -= 1
                    end
                    if arg.invalid?
                      @parsing_call_args -= 1
                      return PREFIX_ERROR
                    end
                  elsif current_token.kind == Token::Kind::AmpDot
                    # &.method with no parentheses
                    amp_token = current_token
                    advance
                    skip_trivia
                    arg = parse_block_shorthand(amp_token)
                  else
                    arg = parse_op_assign
                  end
                  if arg.invalid?
                    @parsing_call_args -= 1
                    return PREFIX_ERROR
                  end

                  # Check if this is a named argument BEFORE skipping trivia
                  # Named arguments require NO space before colon: "name: value"
                  # Type restrictions require space: "name : Type"
                  # By checking current token before skip_trivia, we can distinguish them
                  arg_node = @arena[arg]
                  if Frontend.node_kind(arg_node) == Frontend::NodeKind::Identifier &&
                     current_token.kind == Token::Kind::Colon
                  # Named argument! (no whitespace before colon, zero-copy)
                  name_span = arg_node.span
                  name_slice = Frontend.node_literal(arg_node).not_nil!

                    advance  # consume ':'
                    skip_trivia

                    value_expr = parse_op_assign
                    if value_expr.invalid?
                      @parsing_call_args -= 1
                      return PREFIX_ERROR
                    end

                    value_span = @arena[value_expr].span

                    named_b << NamedArgument.new(name_slice, value_expr, name_span, value_span)
                    skip_trivia
                  else
                    # Positional argument (including type-restricted assignments like "x : Type = value")
                    skip_trivia
                    args_b << arg
                  end

                  # Check for comma
                  if current_token.kind == Token::Kind::Comma
                    advance
                    skip_trivia
                  else
                    # If the next token starts a block, allow the postfix loop to handle it.
                    break if current_token.kind == Token::Kind::LBrace || current_token.kind == Token::Kind::Do
                    break
                  end
                end

                # Create CallNode with MemberAccessNode as callee
                # Materialize arrays once
                args = args_b.to_a
                named_args = named_b.to_a
                last_arg_id = args.last? || named_args.last?.try(&.value) || node
                call_span = member_span.cover(@arena[last_arg_id].span)
                result = @arena.add_typed(CallNode.new(
                  call_span,
                  node,  # MemberAccessNode as callee
                  args,
                  nil,   # no block
                  named_args.empty? ? nil : named_args
                ))

                @parsing_call_args -= 1
                result
              end
            else
              node
            end
          elsif !member_token.slice.empty?
            # Phase 101: Keywords can be method names after dot (e.g., .class, .select, .begin)
            member_span = node_span(receiver).cover(dot.span).cover(member_token.span)
            node = @arena.add_typed(
              MemberAccessNode.new(
                member_span,
                receiver,
                member_token.slice
              )
            )
            advance

            # Check if this member access is followed by arguments without parentheses
            # Example: Foo.bar 1, b: 2 → CallNode with MemberAccessNode as callee
            # Must check for space (not newline!) like original Crystal parser
            space_consumed = false
            next_token_view = current_token
            if next_token_view.kind == Token::Kind::Whitespace
              advance  # Consume space
              space_consumed = true
            elsif next_token_view.span.start_line == member_token.span.end_line &&
                  next_token_view.span.start_offset > member_token.span.end_offset
              # Heuristic: even without explicit whitespace tokens (default lexing),
              # detect a gap on the same line to allow DSL-style calls like
              # json.field "value", foo
              space_consumed = true
            end

            if space_consumed && @parsing_call_args == 0
              token = current_token
              if call_without_parens_disallowed?(token)
                node
              else
                # Try to parse arguments
                @parsing_call_args += 1

                args_b = SmallVec(ExprId, 4).new
                named_b = SmallVec(NamedArgument, 2).new

                loop do
                  # Stop parsing arguments if a 'do' block starts here; let postfix loop attach it.
                  break if current_token.kind == Token::Kind::Do

                  # Parse one argument
                  arg = parse_op_assign
                  if arg.invalid?
                    @parsing_call_args -= 1
                    return PREFIX_ERROR
                  end

                  # Check if this is a named argument BEFORE skipping trivia
                  # Named arguments require NO space before colon: "name: value"
                  # Type restrictions require space: "name : Type = value"
                  # By checking current token before skip_trivia, we can distinguish them
                  arg_node = @arena[arg]
                  if Frontend.node_kind(arg_node) == Frontend::NodeKind::Identifier &&
                     current_token.kind == Token::Kind::Colon
                    # Named argument! (no whitespace before colon, zero-copy)
                    name_span = arg_node.span
                    name_slice = Frontend.node_literal(arg_node).not_nil!

                    advance  # consume ':'
                    skip_trivia

                    value_expr = parse_op_assign
                    if value_expr.invalid?
                      @parsing_call_args -= 1
                      return PREFIX_ERROR
                    end

                    value_span = @arena[value_expr].span

                    named_b << NamedArgument.new(name_slice, value_expr, name_span, value_span)
                    skip_trivia
                  else
                    # Positional argument (including type-restricted assignments like "x : Type = value")
                    skip_trivia
                    args_b << arg
                  end

                  # Check for comma
                  if current_token.kind == Token::Kind::Comma
                    advance
                    skip_trivia
                  else
                    # If the next token starts a block, allow the postfix loop to handle it.
                    break if current_token.kind == Token::Kind::LBrace || current_token.kind == Token::Kind::Do
                    break
                  end
                end

                # Create CallNode with MemberAccessNode as callee
                # Materialize arrays once
                args = args_b.to_a
                named_args = named_b.to_a
                last_arg_id = args.last? || named_args.last?.try(&.value) || node
                call_span = member_span.cover(@arena[last_arg_id].span)
                result = @arena.add_typed(CallNode.new(
                  call_span,
                  node,  # MemberAccessNode as callee
                  args,
                  nil,   # no block
                  named_args.empty? ? nil : named_args
                ))

                @parsing_call_args -= 1
                result
              end
            else
              node
            end
          else
            emit_unexpected(member_token)
            receiver
          end
        end

        private def call_without_parens_disallowed?(token : Token) : Bool
          case token.kind
          when Token::Kind::Newline, Token::Kind::EOF,
               Token::Kind::Semicolon, Token::Kind::Then,
               Token::Kind::End, Token::Kind::Elsif, Token::Kind::Else,
               Token::Kind::When, Token::Kind::Rescue, Token::Kind::Ensure,
               Token::Kind::If, Token::Kind::Unless,
               Token::Kind::While, Token::Kind::Until,
               Token::Kind::RParen, Token::Kind::RBracket, Token::Kind::RBrace,
               Token::Kind::Comma,
               Token::Kind::Amp,
               Token::Kind::LBrace, Token::Kind::Do,
               Token::Kind::Eq, Token::Kind::Colon,
               Token::Kind::OrOrEq, Token::Kind::AndAndEq,
               Token::Kind::PlusEq, Token::Kind::MinusEq, Token::Kind::StarEq,
               Token::Kind::SlashEq, Token::Kind::FloorDivEq, Token::Kind::PercentEq,
               Token::Kind::StarStarEq, Token::Kind::AmpEq, Token::Kind::PipeEq,
               Token::Kind::CaretEq, Token::Kind::LShiftEq, Token::Kind::RShiftEq,
               Token::Kind::NilCoalesceEq,
               Token::Kind::NilCoalesce,
               Token::Kind::Plus, Token::Kind::Minus, Token::Kind::Star, Token::Kind::StarStar,
               Token::Kind::Slash, Token::Kind::FloorDiv, Token::Kind::Percent,
               Token::Kind::PercentRBrace,
               Token::Kind::OrOr, Token::Kind::AndAnd,
               Token::Kind::Question, Token::Kind::Arrow,
               Token::Kind::EqEq, Token::Kind::EqEqEq, Token::Kind::NotEq,
               Token::Kind::Less, Token::Kind::Greater,
               Token::Kind::LessEq, Token::Kind::GreaterEq,
               Token::Kind::Spaceship,
               Token::Kind::Pipe, Token::Kind::Caret,
               Token::Kind::LShift, Token::Kind::RShift,
               Token::Kind::DotDot, Token::Kind::DotDotDot,
               Token::Kind::Match, Token::Kind::NotMatch,
               Token::Kind::In,
               Token::Kind::LParen
            true
          else
            false
          end
        end

        # Phase 63: Parse path expression (Foo::Bar)
        private def parse_path(left : ExprId) : ExprId
          colon_colon = current_token
          advance
          skip_trivia

          # Parse right side - prefer identifier, but allow keyword method/const names too
          right_token = current_token
          slice = right_token.slice
          if right_token.kind != Token::Kind::Identifier
            if slice.empty?
              emit_unexpected(right_token)
              return left
            end
            first = slice[0]
            unless (first >= 'a'.ord.to_u8 && first <= 'z'.ord.to_u8) ||
                   (first >= 'A'.ord.to_u8 && first <= 'Z'.ord.to_u8) ||
                   first == '_'.ord.to_u8
              emit_unexpected(right_token)
              return left
            end
          end

          right_id = @arena.add_typed(
            IdentifierNode.new(
              right_token.span,
              slice
            )
          )
          advance

          # Create Path node
          path_span = cover_optional_spans(node_span(left), colon_colon.span, node_span(right_id))
          @arena.add_typed(
            PathNode.new(
              path_span,
              left,
              right_id
            )
          )
        end

        # Phase 63: Parse absolute path (::TopLevel)
        # Absolute paths start with :: and have no left side
        private def parse_absolute_path : ExprId
          colon_colon = current_token
          advance
          skip_trivia

          # Parse identifier (or keyword-like name) after ::
          identifier_token = current_token
          slice = identifier_token.slice
          if identifier_token.kind != Token::Kind::Identifier
            if slice.empty?
              emit_unexpected(identifier_token)
              return PREFIX_ERROR
            end
            first = slice[0]
            unless (first >= 'a'.ord.to_u8 && first <= 'z'.ord.to_u8) ||
                   (first >= 'A'.ord.to_u8 && first <= 'Z'.ord.to_u8) ||
                   first == '_'.ord.to_u8
              emit_unexpected(identifier_token)
              return PREFIX_ERROR
            end
          end

          right_id = @arena.add_typed(
            IdentifierNode.new(
              identifier_token.span,
              slice
            )
          )
          advance

          # Create Path node with nil left (indicates absolute path)
          path_span = colon_colon.span.cover(node_span(right_id))
          @arena.add_typed(
            PathNode.new(
              path_span,
              nil,  # No left side = absolute path
              right_id
            )
          )
        end

        # Parse generic application: Base(TypeArg1, TypeArg2, ...)
        # Returns GenericNode(base, args) or the original base if no '('
        private def parse_generic_application(base : ExprId) : ExprId
          return base unless current_token.kind == Token::Kind::LParen

          lparen = current_token
          advance
          @paren_depth += 1
          skip_whitespace_and_optional_newlines

          args_b = SmallVec(ExprId, 3).new
          unless current_token.kind == Token::Kind::RParen
            loop do
              arg = parse_expression(0)
              return base if arg.invalid?
              args_b << arg
              skip_whitespace_and_optional_newlines
              break unless current_token.kind == Token::Kind::Comma
              advance
              skip_whitespace_and_optional_newlines
            end
          end

          expect_operator(Token::Kind::RParen)
          @paren_depth -= 1
          close_span = previous_token.try(&.span)
          span = if close_span
            node_span(base).cover(close_span)
          else
            node_span(base)
          end
          @arena.add_typed(GenericNode.new(span, base, args_b.to_a))
        end

        # Phase 44: Parse type cast (.as(Type))
        # Phase 44/103J: Parse .as type cast
        # Supports both .as(Type) and .as Type syntaxes
        # Following original parser design (parser.cr:903-919)
        private def parse_as_cast(receiver : ExprId, dot : Token, as_token : Token) : ExprId
          advance  # Skip 'as' keyword
          skip_trivia

          type_start = current_token

          # Check if parentheses are used: .as(Type) vs .as Type
          if current_token.kind == Token::Kind::LParen
            # With parens: .as(Type) or .as(Proc(...))
            advance  # skip (
            skip_trivia

            target_type = parse_type_annotation
            type_end = previous_token.not_nil!

            skip_trivia
            unless current_token.kind == Token::Kind::RParen
              emit_unexpected(current_token)
              return PREFIX_ERROR
            end
            advance  # skip )
          else
            # Without parens: .as Float32 or .as Int32 | String
            target_type = parse_type_annotation
            type_end = previous_token.not_nil!
          end

          # Create As node
          as_span = node_span(receiver).cover(as_token.span).cover(type_end.span)

          @arena.add_typed(
            AsNode.new(
              as_span,
              receiver,
              target_type
            )
          )
        end

        # Phase 45/103J: Parse safe type cast (.as?(Type) or .as? Type)
        # Supports both .as?(Type) and .as? Type syntaxes
        # Following original parser design (parser.cr:903-919)
        private def parse_as_safe_cast(receiver : ExprId, dot : Token, as_question_token : Token) : ExprId
          advance  # Skip 'as?' keyword
          skip_trivia

          type_start = current_token

          # Check if parentheses are used: .as?(Type) vs .as? Type
          if current_token.kind == Token::Kind::LParen
            # With parens: .as?(Type) or .as?(Proc(...))
            advance  # skip (
            skip_trivia

            target_type = parse_type_annotation
            type_end = previous_token.not_nil!

            skip_trivia
            unless current_token.kind == Token::Kind::RParen
              emit_unexpected(current_token)
              return PREFIX_ERROR
            end
            advance  # skip )
          else
            # Without parens: .as? Float32 or .as? Int32 | String
            target_type = parse_type_annotation
            type_end = previous_token.not_nil!
          end

          # Create AsQuestion node
          as_question_span = node_span(receiver).cover(as_question_token.span).cover(type_end.span)

          @arena.add_typed(
            AsQuestionNode.new(
              as_question_span,
              receiver,
              target_type
            )
          )
        end

        # Phase 93: Parse type check (.is_a?(Type))
        private def parse_is_a(receiver : ExprId, dot : Token, is_a_token : Token) : ExprId
          advance  # Skip 'is_a?' keyword
          skip_trivia

          # Expect opening parenthesis
          unless current_token.kind == Token::Kind::LParen
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end
          lparen = current_token
          advance
          skip_trivia

          # Parse target type (reuse type-annotation parser to support Foo::Bar, unions, etc.)
          type_start_token = current_token
          target_type = parse_type_annotation
          type_end_token = previous_token
          unless type_end_token
            emit_unexpected(type_start_token)
            return PREFIX_ERROR
          end
          skip_trivia

          # Expect closing parenthesis
          unless current_token.kind == Token::Kind::RParen
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end
          rparen = current_token
          advance

          # Create IsA node
          # Accumulate span without temporary array
          is_a_span = node_span(receiver)
            .cover(dot.span)
            .cover(is_a_token.span)
            .cover(lparen.span)
            .cover(type_start_token.span)
            .cover(type_end_token.span)
            .cover(rparen.span)

          @arena.add_typed(
            IsANode.new(
              is_a_span,
              receiver,
              target_type
            )
          )
        end

        # Phase 94: Parse method check (.responds_to?(:method))
        private def parse_responds_to(receiver : ExprId, dot : Token, responds_to_token : Token) : ExprId
          advance  # Skip 'responds_to?' keyword
          skip_trivia

          if current_token.kind == Token::Kind::LParen
            # With parentheses: .responds_to?(:method)
            lparen = current_token
            advance
            skip_trivia

            method_name_expr = parse_expression(0)
            skip_trivia

            unless current_token.kind == Token::Kind::RParen
              emit_unexpected(current_token)
              return PREFIX_ERROR
            end
            rparen = current_token
            advance

            responds_to_span = node_span(receiver)
              .cover(dot.span)
              .cover(responds_to_token.span)
              .cover(lparen.span)
              .cover(node_span(method_name_expr))
              .cover(rparen.span)
          else
            # Without parentheses: .responds_to? :method
            method_name_expr = parse_expression(0)
            return PREFIX_ERROR if method_name_expr.invalid?
            responds_to_span = node_span(receiver)
              .cover(dot.span)
              .cover(responds_to_token.span)
              .cover(node_span(method_name_expr))
          end

          @arena.add_typed(
            RespondsToNode.new(
              responds_to_span,
              receiver,
              method_name_expr
            )
          )
        end

        # Phase 61: Parse type parameters for generic class/struct/module definitions
        # Example: class Box(T, K, V) -> ["T", "K", "V"]
        private def parse_type_parameters : Array(Slice(UInt8))?
          # Check if type parameters present: (
          unless current_token.kind == Token::Kind::LParen
            return nil  # No type parameters
          end

          advance  # Skip (
          skip_trivia

          type_params = [] of Slice(UInt8)

          # Parse comma-separated type parameter names
          loop do
            # Each type parameter must be an uppercase identifier
            unless current_token.kind == Token::Kind::Identifier
              emit_unexpected(current_token)
              return nil
            end

            # Type parameter names should be uppercase (T, K, V, etc.)
            # For now, accept any identifier
            type_params << current_token.slice
            advance
            skip_trivia

            # Check for comma or closing paren
            if current_token.kind == Token::Kind::Comma
              advance  # Skip comma
              skip_trivia
            elsif current_token.kind == Token::Kind::RParen
              break  # End of parameters
            else
              emit_unexpected(current_token)
              return nil
            end
          end

          # Expect closing parenthesis
          unless current_token.kind == Token::Kind::RParen
            emit_unexpected(current_token)
            return nil
          end
          advance  # Skip )

          type_params
        end

        # Phase 60: Parse generic type instantiation (Box(Int32), Hash(String, Int32))
        private def parse_generic_instantiation(name_token : Token) : ExprId
          base_expr = @arena.add_typed(
            IdentifierNode.new(
              name_token.span,
              @string_pool.intern(name_token.slice)
            )
          )
          parse_generic_instantiation_from_base(base_expr)
        end

        private def parse_generic_instantiation_from_base(base_expr : ExprId) : ExprId
          lparen = current_token
          unless lparen.kind == Token::Kind::LParen
            emit_unexpected(lparen)
            return PREFIX_ERROR
          end

          advance  # consume '('
          skip_whitespace_and_optional_newlines

          type_args_b = SmallVec(ExprId, 2).new
          unless current_token.kind == Token::Kind::RParen
            loop do
              arg_expr = parse_generic_type_argument_expr
              return PREFIX_ERROR if arg_expr.invalid?
              type_args_b << arg_expr

              skip_whitespace_and_optional_newlines
              if current_token.kind == Token::Kind::Comma
                advance
                skip_whitespace_and_optional_newlines
                next
              elsif current_token.kind == Token::Kind::RParen
                break
              else
                emit_unexpected(current_token)
                return PREFIX_ERROR
              end
            end
          end

          unless current_token.kind == Token::Kind::RParen
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end
          rparen = current_token
          advance

          # Accumulate span covering base, type args, and closing paren
          acc_span = node_span(base_expr).cover(lparen.span)
          type_args = type_args_b.to_a
          type_args.each { |arg| acc_span = acc_span.cover(node_span(arg)) }
          acc_span = acc_span.cover(rparen.span)

          @arena.add_typed(
            GenericNode.new(
              acc_span,
              base_expr,
              type_args
            )
          )
        end

        private def parse_generic_type_argument_expr : ExprId
          @no_type_declaration += 1
          expr = parse_expression(0)
          @no_type_declaration -= 1
          return PREFIX_ERROR if expr.invalid?
          expr
        end

        # Phase 47: Parse safe navigation (&.)
        private def parse_safe_navigation(receiver : ExprId) : ExprId
          amp_dot = current_token
          advance  # Skip '&.'
          skip_trivia

          member_token = current_token

          # Member must be identifier
          if member_token.kind == Token::Kind::Identifier
            # Compute safe navigation span without a temporary array
            safe_nav_span = node_span(receiver).cover(amp_dot.span).cover(member_token.span)

            node = @arena.add_typed(
              SafeNavigationNode.new(
                safe_nav_span,
                receiver,
                member_token.slice
              )
            )
            advance
            node
          else
            emit_unexpected(member_token)
            receiver
          end
        end

        private def expect_operator(kind : Token::Kind)
          token = current_token
          if token.kind == kind
            advance
          else
            emit_unexpected(token)
          end
        end

        private def expect_operator(symbol : String)
          token = current_token
          if token.kind == Token::Kind::Operator && slice_eq?(token.slice, symbol)
            advance
          else
            emit_unexpected(token)
          end
        end

        private def build_binary(left : ExprId, token : Token, right : ExprId) : ExprId
          @arena.add_typed(
            BinaryNode.new(
              cover_optional_spans(node_span(left), token.span, node_span(right)),
              token.slice,
              left,
              right
            )
          )
        end

        private def infix?(token : Token)
          # Phase 103J: Don't treat %} as infix (macro control terminator)
          if token.kind == Token::Kind::Percent
            next_tok = peek_token(1)
            return false if next_tok.kind == Token::Kind::RBrace
          end
          BINARY_PRECEDENCE.has_key?(token.kind)
        end

        # Check if token is an end token (following original parser logic)
        private def end_token?(token : Token) : Bool
          case token.kind
          when Token::Kind::RBrace, Token::Kind::RBracket, Token::Kind::RParen, Token::Kind::EOF
            return true
          when Token::Kind::End, Token::Kind::Else, Token::Kind::Elsif,
               Token::Kind::When, Token::Kind::Rescue, Token::Kind::Ensure
            return true
          end
          false
        end

        private def precedence_for(token : Token) : Int32
          BINARY_PRECEDENCE[token.kind]? || 0
        end

        private def emit_unexpected(token : Token)
          debug("emit_unexpected: #{token.kind} at #{token.span.start_line + 1}:#{token.span.start_column + 1}")
          if ENV["PARSER_UNEXPECTED_TRACE"]?
            STDERR.puts "[TRACE] unexpected #{token.kind} at #{token.span.start_line + 1}:#{token.span.start_column + 1} context=#{@expect_context}"
            STDERR.puts caller[0, 5].join("\n")
          end
          # Suppress a known false positive: closing ')' while parsing method parameter list
          if @parsing_method_params && token.kind == Token::Kind::RParen
            return
          end
          # Suppress false positive: stray ')' at statement start
          if token.kind == Token::Kind::RParen && @expect_context == "statement"
            return
          end
          @diagnostics << Diagnostic.new("unexpected #{token.kind}", token.span)
        end

        # Debug helper for deep debugging (enable via PARSER_DEBUG=1)
        private def debug(message : String)
          STDERR.puts "[PARSER_DEBUG] #{message}" if @debug_enabled
        end

        # Phase 103: Optimization - compare slice with string without allocation
        @[AlwaysInline]
        private def slice_eq?(slice : Slice(UInt8), str : String) : Bool
          return false if slice.size != str.bytesize
          slice.each_with_index do |byte, i|
            return false if byte != str.to_unsafe[i]
          end
          true
        end

        # DEPRECATED: Use token.slice directly or slice_eq? for comparisons
        # Kept temporarily for complex cases that need migration
        private def token_text(token : Token) : String
          String.new(token.slice)
        end

        private def peek_token(offset = 1)
          index = @index + offset
          ensure_token(index)
          index < @tokens.size ? @tokens[index] : @tokens.last
        end

        # Phase 77: Peek ahead to find next non-trivia token
        private def peek_next_non_trivia
          offset = 1
          loop do
            token = peek_token(offset)
            return token unless token.kind == Token::Kind::Whitespace ||
                               token.kind == Token::Kind::Newline ||
                               token.kind == Token::Kind::Comment
            offset += 1
          end
        end

        # Phase NAMED_ARGUMENTS: Check if token can be used as named argument name
        # In call argument context, keywords like 'of' are treated as identifiers
        private def token_can_be_arg_name?(token : Token) : Bool
          case token.kind
          when Token::Kind::Identifier
            true
          when Token::Kind::Of, Token::Kind::As, Token::Kind::In, Token::Kind::Out,
               Token::Kind::Do, Token::Kind::End, Token::Kind::If, Token::Kind::Unless,
               Token::Kind::Def, Token::Kind::For, Token::Kind::Then
            # Phase 103F: Keywords that can be used as parameter/argument names
            # Includes 'def' for cases like: getter def : Def, initialize(def: value)
            true
          else
            false
          end
        end

        # Phase NAMED_ARGUMENTS: Check if current position is start of named argument
        # Pattern: (identifier or keyword) followed by ':' but not '::'
        private def named_arg_start? : Bool
          return false unless token_can_be_arg_name?(current_token)

          next_token = peek_next_non_trivia
          return false unless next_token.kind == Token::Kind::Colon

          # Make sure it's not :: (scope resolution)
          offset = 1
          loop do
            token = peek_token(offset)
            if token.kind == Token::Kind::Whitespace ||
               token.kind == Token::Kind::Newline ||
               token.kind == Token::Kind::Comment
              offset += 1
              next
            elsif token.kind == Token::Kind::Colon
              # Found colon
              # Check if next token is also colon (::)
              offset += 1
              next_next = peek_token(offset)
              return next_next.kind != Token::Kind::Colon
            else
              return false
            end
          end
        end

        private def consume_macro_newline_escape?
          if operator_token?(current_token, "\\")
            advance
            true
          else
            false
          end
        end

        private def macro_expression_start?
          return false if @macro_mode == 0
          current = current_token
          return false unless current.kind == Token::Kind::LBrace
          peek = peek_token
          peek.kind == Token::Kind::LBrace
        end

        private def macro_expression_left_trim?
          second = peek_token(1)
          third = peek_token(2)
          second.kind == Token::Kind::LBrace && macro_trim_token?(third)
        end

        private def macro_context? : Bool
          @macro_mode > 0 || @macro_terminator == :control
        end

        private def macro_closing_sequence?(token : Token) : Bool
          if token.kind == Token::Kind::PercentRBrace
            return true
          elsif token.kind == Token::Kind::Percent
            next_token = peek_token
            return next_token.kind == Token::Kind::RBrace
          elsif token.kind == Token::Kind::Minus
            next_token = peek_token
            if next_token.kind == Token::Kind::PercentRBrace
              return true
            elsif next_token.kind == Token::Kind::Percent
              next_next = peek_token(2)
              return next_next.kind == Token::Kind::RBrace
            end
          end
          false
        end

        private def macro_variable_start? : Bool
          return false unless macro_context?
          return false unless current_token.kind == Token::Kind::Percent
          next_token = peek_next_non_trivia
          next_token.kind == Token::Kind::Identifier
        end

        private def parse_macro_variable_piece : {MacroPiece, Bool}
          start_token = current_token
          advance  # consume '%'
          skip_macro_whitespace

          name_token = current_token
          unless name_token.kind == Token::Kind::Identifier
            emit_unexpected(name_token)
            span = start_token.span
            span = span.cover(name_token.span) if name_token
            return {MacroPiece.text("%", span), false}
          end

          name = token_text(name_token)
          advance

          newline_escape = consume_macro_newline_escape?
          macro_span = start_token.span.cover(name_token.span)
          piece = MacroPiece.macro_var(name, macro_span)
          {piece, newline_escape}
        end

        private def parse_macro_variable_reference : ExprId
          percent_token = current_token
          advance

          name_token = current_token
          unless name_token.kind == Token::Kind::Identifier
            emit_unexpected(name_token)
            return PREFIX_ERROR
          end

          span = percent_token.span.cover(name_token.span)
          node = MacroVarNode.new(span, name_token.slice)
          advance
          @arena.add_typed(node)
        end

        private def macro_control_start?
          current_token.kind == Token::Kind::LBracePercent
        end

        private def macro_control_left_trim?
          return false unless current_token.kind == Token::Kind::LBracePercent
          next_token = peek_token
          macro_trim_token?(next_token)
        end

        # Peek ahead inside a '{' ... '}' literal to guess its kind without consuming tokens.
        # Returns :hash, :named_tuple, :tuple, or :unknown (fallback to general parser).
        private def peek_brace_literal_kind : Symbol
          # We assume current_token is LBrace
          idx = @index + 1
          paren = 0; bracket = 0; brace = 0
          last_non_trivia_kind = nil
          last_non_trivia_idx = idx - 1
          # Limit lookahead to avoid pathological cases
          max_lookahead = 128
          steps = 0

          while steps < max_lookahead
            ensure_token(idx)
            break if idx >= @tokens.size
            tok = @tokens[idx]

            # Track non-trivia
            if tok.kind != Token::Kind::Whitespace && tok.kind != Token::Kind::Newline && tok.kind != Token::Kind::Comment
              last_non_trivia_kind = tok.kind
              last_non_trivia_idx = idx
            end

            # Empty hash: immediately '}'
            if brace == 0 && paren == 0 && bracket == 0 && tok.kind == Token::Kind::RBrace
              return :hash  # {} is a hash in Crystal
            end

            # Track nesting
            case tok.kind
            when Token::Kind::LParen;   paren += 1
            when Token::Kind::RParen;   paren -= 1 if paren > 0
            when Token::Kind::LBracket; bracket += 1
            when Token::Kind::RBracket; bracket -= 1 if bracket > 0
            when Token::Kind::LBrace;   brace += 1
            when Token::Kind::RBrace
              if brace > 0
                brace -= 1
              else
                # Top-level closing brace without prior decision → treat as tuple
                return :tuple
              end
            end

            if paren == 0 && bracket == 0 && brace == 0
              # Top-level tokens that help disambiguate
              if tok.kind == Token::Kind::Arrow
                return :hash
              elsif tok.kind == Token::Kind::Comma
                return :tuple
              elsif tok.kind == Token::Kind::Colon
                # Named tuple only when previous non-trivia is Identifier and not part of '::'
                prev2 = last_non_trivia_idx >= 1 ? @tokens[last_non_trivia_idx] : tok
                next_tok = (ensure_token(idx + 1); idx + 1 < @tokens.size ? @tokens[idx + 1] : tok)
                if last_non_trivia_kind == Token::Kind::Identifier && next_tok.kind != Token::Kind::Colon
                  return :named_tuple
                end
              end
            end

            idx += 1
            steps += 1
          end

          :unknown
        end

        private def macro_trim_token?(token : Token) : Bool
          token.kind == Token::Kind::Minus ||
            (token.kind == Token::Kind::Operator && slice_eq?(token.slice, "-"))
        end

        private def macro_trim_operator? : Bool
          if macro_trim_token?(current_token)
            advance
            true
          else
            false
          end
        end

        # Check if keyword is a macro control keyword (vs expression)
        # Control keywords: if, unless, for, while, begin, verbatim, else, elsif, end, comment
        # Expression keywords: everything else (e.g., skip_file, raise, @type, x = 1)
        private def is_control_keyword?(keyword : String) : Bool
          case keyword
          when "if", "unless", "for", "while", "begin", "verbatim",
               "else", "elsif", "end", "comment"
            true
          else
            false
          end
        end

        # Peek ahead after {% to determine if this is a control or expression
        # Handles whitespace/newlines like original parser's next_token_skip_space_or_newline
        private def peek_macro_keyword_after_lbracepercent : String?
          return nil unless current_token.kind == Token::Kind::LBracePercent

          saved_index = @index
          saved_previous = @previous_token

          advance # skip {%

          # Skip optional trim token (-)
          if macro_trim_token?(current_token)
            advance
          end

          skip_macro_whitespace # skip whitespace/newlines

          # Handle both identifiers and keyword tokens (for, if, while, etc.)
          keyword = case current_token.kind
          when Token::Kind::Identifier,
               Token::Kind::For, Token::Kind::If, Token::Kind::Unless,
               Token::Kind::While, Token::Kind::Begin, Token::Kind::End,
               Token::Kind::Else, Token::Kind::Elsif
            token_text(current_token)
          else
            nil
          end

          # Restore position
          @index = saved_index
          @previous_token = saved_previous

          keyword
        end

        private def macro_terminator_reached?(token : Token)
          case @macro_terminator
          when :expression
            # Close on '}}' or '-}}'
            if token.kind == Token::Kind::RBrace
              return true if peek_token.kind == Token::Kind::RBrace
            elsif token.kind == Token::Kind::Minus
              return true if peek_token.kind == Token::Kind::RBrace && peek_token(2).kind == Token::Kind::RBrace
            end
          when :control
            # Close on '%}' or '-%}' as either combined or split tokens
            if token.kind == Token::Kind::PercentRBrace
              return true
            elsif token.kind == Token::Kind::Minus
              nxt = peek_token(1)
              return true if nxt.kind == Token::Kind::PercentRBrace
              return true if nxt.kind == Token::Kind::Percent && peek_token(2).kind == Token::Kind::RBrace
            elsif token.kind == Token::Kind::Percent
              return true if peek_token.kind == Token::Kind::RBrace
            end
          end
          false
        end

        private def consume_macro_close_span(message = "Expected '%}'") : Span?
          token = current_token
          case token.kind
          when Token::Kind::PercentRBrace
            advance
            return token.span
          when Token::Kind::Percent
            percent_span = token.span
            advance
            closing = current_token
            if closing.kind == Token::Kind::RBrace
              span = percent_span.cover(closing.span)
              advance
              return span
            end
            # Fallback scan ahead to find a split closer
          else
            # no-op
          end

          # Fallback: be tolerant and scan ahead (skipping whitespace/newlines) for a closer
          scan_index = @index
          max_lookahead = 32
          look = 0
          while look < max_lookahead
            tok = peek_token(look)
            if tok.kind == Token::Kind::EOF
              break
            end
            if tok.kind == Token::Kind::PercentRBrace
              # Consume everything up to and including closer
              unadvance(@index - @index) if false  # keep API balance (no-op)
              @index = @index + look + 1
              @previous_token = tok
              return tok.span
            elsif tok.kind == Token::Kind::Percent && peek_token(look + 1).kind == Token::Kind::RBrace
              first = tok
              second = peek_token(look + 1)
              @index = @index + look + 2
              @previous_token = second
              return first.span.cover(second.span)
            end
            look += 1
          end

          # Give up: report diagnostic and advance one token to guarantee progress
          @diagnostics << Diagnostic.new(message, token.span)
          advance unless token.kind == Token::Kind::EOF
          nil
        end

        private def consume_macro_control_start : Span?
          start_token = current_token
          case start_token.kind
          when Token::Kind::LBracePercent
            advance
            start_token.span
          when Token::Kind::LBrace
            advance
            percent_token = current_token
            unless percent_token.kind == Token::Kind::Percent
              emit_unexpected(percent_token)
              return nil
            end
            span = start_token.span.cover(percent_token.span)
            advance
            span
          else
            emit_unexpected(start_token)
            advance unless start_token.kind == Token::Kind::EOF
            nil
          end
        end

        private def expect_macro_close(message = "Expected '%}'") : Bool
          !!consume_macro_close_span(message)
        end

        private def peek_macro_keyword : String?
          saved_index = @index
          token = current_token

          if token.kind == Token::Kind::LBracePercent
            advance
          elsif token.kind == Token::Kind::LBrace
            next_token = peek_token
            unless next_token.kind == Token::Kind::Percent
              @index = saved_index
              return nil
            end
            advance  # consume {
            advance  # consume %
          else
            return nil
          end

          if macro_trim_token?(current_token)
            advance
          end

          skip_trivia
          keyword = token_text(current_token)
          @index = saved_index
          @previous_token = nil
          keyword
        end

        private def operator_token?(token : Token, kind : Token::Kind)
          token.kind == kind
        end

        private def operator_token?(token : Token, value : String)
          # Special case for "-": can be Minus or Operator
          if value == "-"
            return true if token.kind == Token::Kind::Minus
          end
          # Phase 18: Special case for "%": now Percent token
          if value == "%"
            return true if token.kind == Token::Kind::Percent
          end
          # Phase 15: Special cases for "{" and "}" (now LBrace/RBrace tokens)
          if value == "{"
            return true if token.kind == Token::Kind::LBrace
          end
          if value == "}"
            return true if token.kind == Token::Kind::RBrace
          end
          # Phase 21: Special case for "~": now Tilde token (bitwise NOT)
          if value == "~"
            return true if token.kind == Token::Kind::Tilde
          end
          # Generic check for Operator tokens
          token.kind == Token::Kind::Operator && slice_eq?(token.slice, value)
        end

        private def with_macro_terminator(terminator : Symbol)
          previous = @macro_terminator
          @macro_terminator = terminator
          result = yield
          @macro_terminator = previous
          result
        end

        private def skip_macro_whitespace
          while current_token.kind == Token::Kind::Whitespace ||
                current_token.kind == Token::Kind::Newline
            advance
          end
        end

        private def skip_macro_whitespace_after_escape
          # After backslash escape, consume newline + leading whitespace
          if current_token.kind == Token::Kind::Newline
            advance
            while current_token.kind == Token::Kind::Whitespace
              advance
            end
          end
        end

        private def whitespace_token?(token : Token)
          token.kind == Token::Kind::Whitespace || token.kind == Token::Kind::Newline
        end

        private def parse_macro_for_header
          # Parse: identifier [, identifier] in expression
          vars = [] of String

          if current_token.kind == Token::Kind::Identifier
            vars << token_text(current_token)
            advance
          else
            emit_unexpected(current_token)
            return {vars, nil}
          end

          skip_macro_whitespace

          if operator_token?(current_token, Token::Kind::Comma)
            advance
            skip_macro_whitespace
            if current_token.kind == Token::Kind::Identifier
              vars << token_text(current_token)
              advance
            else
              emit_unexpected(current_token)
            end
            skip_macro_whitespace
          end

          # Phase 79: "in" is now a keyword, not an identifier
          if current_token.kind == Token::Kind::In
            advance
          else
            emit_unexpected(current_token)
            return {vars, nil}
          end

          skip_macro_whitespace

          iterable = with_macro_terminator(:control) { parse_expression(0) }

          {vars, iterable}
        end

        private def parse_macro_control_piece(left_trim : Bool)
          start_token = current_token

          unless start_token.kind == Token::Kind::LBracePercent
            emit_unexpected(start_token)
            return {MacroPiece.text("", start_token.span), :none, false}
          end

          # Peek ahead to determine if this is a control keyword or expression
          keyword_peek = peek_macro_keyword_after_lbracepercent
          handled_as_control = keyword_peek && is_control_keyword?(keyword_peek)

          if handled_as_control
            # BRANCH A: Macro control (if, for, begin, verbatim, etc.)
            advance  # consume {% (combined token)

            if macro_trim_token?(current_token)
              left_trim = true
              advance
            end

            skip_macro_whitespace

            keyword_token = current_token
            keyword_index = @index
            keyword = token_text(keyword_token)
            advance

            expr = nil
            iter_vars = nil
            iterable = nil

            case keyword
            when "for"
              skip_macro_whitespace
              iter_vars, iterable = parse_macro_for_header
            when "if", "unless", "while", "elsif"
              skip_macro_whitespace
              expr = with_macro_terminator(:control) { parse_expression(0) }
            when "verbatim"
              # verbatim requires 'do' keyword before %}: {% verbatim do %}
              skip_macro_whitespace
              unless token_text(current_token) == "do"
                @diagnostics << Diagnostic.new("Expected 'do' after verbatim", current_token.span)
                return {MacroPiece.text("", start_token.span), :none, false}
              end
              advance  # consume 'do'
            when "else", "end", "comment", "begin"
              # no expression needed
            end
            skip_macro_whitespace
            right_trim = macro_trim_operator?

            previous_context = @expect_context
            end_span = nil
            begin
              @expect_context = "macro control #{keyword}"
              end_span = consume_macro_close_span("Expected '%}' after #{keyword}")
            ensure
              @expect_context = previous_context
            end

            unless end_span
              end_span = start_token.span
            end

            newline_escape = consume_macro_newline_escape?
            skip_whitespace = newline_escape || right_trim

            kind = case keyword
              when "else"
                MacroPiece::Kind::ControlElse
              when "elsif"
                MacroPiece::Kind::ControlElseIf
              when "end"
                MacroPiece::Kind::ControlEnd
              else
                MacroPiece::Kind::ControlStart
              end

            control_span = start_token.span.cover(end_span)
            piece = MacroPiece.control(kind, keyword, expr, left_trim, right_trim, iter_vars, iterable, control_span)

            effect = case keyword
              when "if", "unless", "for", "while", "comment", "begin", "verbatim"
                :push
              when "end"
                :pop
              else
                :none
              end

            {piece, effect, skip_whitespace}
          else
            # BRANCH B: Macro expression (not a control keyword)
            # Examples: {% x = 1 %}, {% @type %}, {% raise "error" %}
            advance  # consume {% (combined token)

            if macro_trim_token?(current_token)
              left_trim = true
              advance
            end

            skip_macro_whitespace

            # Set flag to enable newline skipping in macro expressions (like original parser)
            previous_context = @expect_context
            previous_in_macro = @in_macro_expression
            @expect_context = "macro expression"
            @in_macro_expression = true  # Enable newline skipping

            # Parse a full expression allowing assignment inside {% %}
            # This mirrors macro expression semantics (e.g., {% x = 1 %})
            expr = with_macro_terminator(:control) { parse_op_assign }

            # CRITICAL: Skip whitespace BEFORE resetting @in_macro_expression flag
            # This allows newlines to be skipped (matching original parser behavior)
            skip_macro_whitespace

            # Now restore context and flag
            @expect_context = previous_context
            @in_macro_expression = previous_in_macro

            # Tolerate parse quirks by fast-forwarding to the closing sequence if not positioned there
            unless macro_closing_sequence?(current_token)
              fast_forward_macro_expression
            end

            right_trim = macro_trim_operator?
            closing_span = consume_macro_close_span("Expected '%}' after macro expression")
            closing_span ||= start_token.span
            newline_escape = consume_macro_newline_escape?
            skip_whitespace = newline_escape || right_trim

            macro_span = start_token.span.cover(closing_span)
            macro_expr_id = @arena.add_typed(MacroExpressionNode.new(macro_span, expr))
            piece = MacroPiece.expression(macro_expr_id, left_trim, right_trim, macro_span)
            {piece, :none, skip_whitespace}
          end
        end

        private def expect_identifier(expected : String)
          token = current_token
          # Check if expected is a keyword that has its own token kind
          expected_kind = case expected
          when "if"    then Token::Kind::If
          when "elsif" then Token::Kind::Elsif
          when "else"  then Token::Kind::Else
          when "end"   then Token::Kind::End
          when "while" then Token::Kind::While
          when "do"    then Token::Kind::Do
          when "then"  then Token::Kind::Then
          when "def"   then Token::Kind::Def
          when "class" then Token::Kind::Class
          when "true"  then Token::Kind::True
          when "false" then Token::Kind::False
          when "nil"   then Token::Kind::Nil
          else
            nil
          end

          if expected_kind
            if token.kind == expected_kind
              advance
            else
              if token.kind == Token::Kind::EOF && ENV["PARSER_UNEXPECTED_TRACE"]?
                STDERR.puts("unexpected EOF while expecting #{expected.inspect} (kind #{expected_kind}) context=#{@expect_context}")
                STDERR.puts(caller[0, 3].join("\n"))
              end
              emit_unexpected(token)
            end
          else
            if token.kind == Token::Kind::Identifier && slice_eq?(token.slice, expected)
              advance
            else
              if token.kind == Token::Kind::EOF && ENV["PARSER_UNEXPECTED_TRACE"]?
                STDERR.puts("unexpected EOF while expecting identifier #{expected.inspect} context=#{@expect_context}")
                STDERR.puts(caller[0, 3].join("\n"))
              end
              emit_unexpected(token)
            end
          end
        end

        # Phase 103B: Parse {% control %} in expression context
        # Called from parse_prefix when seeing {% token sequence
        # Returns MacroIfNode or MacroForNode wrapped in ExprId
        private def parse_percent_macro_control : ExprId
          previous_terminator = @macro_terminator
          @macro_terminator = :control
          start_span = consume_macro_control_start
          return PREFIX_ERROR unless start_span

          if macro_trim_token?(current_token)
            advance
          end

          skip_trivia

          # Get keyword (if, for, unless, etc.)
          keyword_token = current_token
          keyword_index = @index
          keyword = token_text(keyword_token)
          advance

          skip_trivia

          case keyword
          when "if", "unless"
            parse_macro_if_control(start_span, keyword)
          when "for"
            parse_macro_for_control(start_span)
          when "begin"
            parse_macro_begin_control(start_span)
          when "verbatim"
            parse_macro_verbatim_control(start_span)
          when "else", "elsif", "end"
            # Phase 103J: These keywords are handled by the caller (parse_macro_if_control, etc.)
            # Return PREFIX_ERROR as a marker - not actually an error
            debug("parse_percent_macro_control: keyword '#{keyword}' should be handled by caller")
            PREFIX_ERROR
          else
            # Phase 103J: Unknown keywords → parse as macro expression (method call)
            # Example: {% skip_file %}, {% raise "error" %}, etc.
            debug("parse_percent_macro_control: parsing '#{keyword}' as macro expression")
            expr = parse_macro_expression_control(start_span, keyword_token, keyword_index)

            # Skip trivia unconditionally (matching original parser's skip_space_or_newline)
            # This skips the newline before %} regardless of flags
            skip_trivia

            # Consume the %} token
            end_span = consume_macro_close_span("Expected '%}' after macro expression")
            unless end_span
              return PREFIX_ERROR
            end

            full_span = start_span.cover(end_span)
            @arena.add_typed(MacroExpressionNode.new(full_span, expr))
          end
        ensure
          @macro_terminator = previous_terminator
        end

        # Parse {% if condition %}...{% end %} or {% unless %}
        private def parse_macro_if_control(start_span : Span, keyword : String) : ExprId
          branches = [] of NamedTuple(span: Span, condition: ExprId, body: ExprId)

          condition = parse_expression(0)
          return PREFIX_ERROR if condition.invalid?

          skip_trivia

          unless expect_macro_close("Expected '%}' after #{keyword} condition")
            return PREFIX_ERROR
          end

          then_body = parse_macro_body_until_branch
          return PREFIX_ERROR if then_body.invalid?

          branches << {span: start_span, condition: condition, body: then_body}

          else_body : ExprId? = nil
          final_end_span : Span? = nil

          loop do
            branch_start_span = consume_macro_control_start
            unless branch_start_span
              @diagnostics << Diagnostic.new("Expected '{% end %}', '{% elsif %}', or '{% else %}'", current_token.span)
              return PREFIX_ERROR
            end

            if macro_trim_token?(current_token)
              advance
            end

            skip_trivia

            branch_keyword = token_text(current_token)
            advance

            skip_trivia

            case branch_keyword
            when "elsif"
              branch_condition = parse_expression(0)
              return PREFIX_ERROR if branch_condition.invalid?

              skip_trivia

              unless expect_macro_close("Expected '%}' after elsif condition")
                return PREFIX_ERROR
              end

              branch_body = parse_macro_body_until_branch
              return PREFIX_ERROR if branch_body.invalid?

              branches << {span: branch_start_span, condition: branch_condition, body: branch_body}
            when "else"
              unless expect_macro_close("Expected '%}' after else")
                return PREFIX_ERROR
              end

              else_body = parse_macro_body_until_branch
              return PREFIX_ERROR if else_body.invalid?

              end_start_span = consume_macro_control_start
              unless end_start_span
                @diagnostics << Diagnostic.new("Expected '{% end %}'", current_token.span)
                return PREFIX_ERROR
              end

              if macro_trim_token?(current_token)
                advance
              end

              skip_trivia

              unless token_text(current_token) == "end"
                @diagnostics << Diagnostic.new("Expected 'end'", current_token.span)
                return PREFIX_ERROR
              end
              advance

              skip_trivia

              final_end_span = consume_macro_close_span("Expected '%}' after end")
              unless final_end_span
                return PREFIX_ERROR
              end

              break
            when "end"
              final_end_span = consume_macro_close_span("Expected '%}' after end")
              unless final_end_span
                return PREFIX_ERROR
              end
              break
            else
              @diagnostics << Diagnostic.new("Expected 'elsif', 'else', or 'end'", current_token.span)
              return PREFIX_ERROR
            end
          end

          end_span = final_end_span || start_span
          full_span = start_span.cover(end_span)

          current_else = else_body
          branches.reverse_each do |branch|
            current_else = @arena.add_typed(MacroIfNode.new(full_span, branch[:condition], branch[:body], current_else))
          end
          current_else || PREFIX_ERROR
        end

        # Parse {% for vars in iterable %}...{% end %}
        private def parse_macro_for_control(start_span : Span) : ExprId
          # Parse iteration variables
          vars = [] of Slice(UInt8)

          loop do
            unless current_token.kind == Token::Kind::Identifier
              @diagnostics << Diagnostic.new("Expected variable name in for loop", current_token.span)
              return PREFIX_ERROR
            end

            vars << current_token.slice
            advance

            skip_trivia

            if current_token.kind == Token::Kind::Comma
              advance
              skip_trivia
            else
              break
            end
          end

          # Expect 'in' keyword
          unless current_token.kind == Token::Kind::In
            @diagnostics << Diagnostic.new("Expected 'in' keyword in for loop", current_token.span)
            return PREFIX_ERROR
          end
          advance

          skip_trivia

          # Parse iterable expression
          iterable = parse_expression(0)
          return PREFIX_ERROR if iterable.invalid?

          skip_trivia

          # Expect %}
          unless expect_macro_close("Expected '%}' after for header")
            return PREFIX_ERROR
          end

          # Parse body until {% end %}
          body = parse_macro_body_until_end
          return PREFIX_ERROR if body.invalid?

          # Expect {% end %}
          end_start_span = consume_macro_control_start
          unless end_start_span
            @diagnostics << Diagnostic.new("Expected '{% end %}' to close for loop", current_token.span)
            return PREFIX_ERROR
          end

          if macro_trim_token?(current_token)
            advance
          end

          skip_trivia

          unless current_token.kind == Token::Kind::End
            @diagnostics << Diagnostic.new("Expected 'end' keyword", current_token.span)
            return PREFIX_ERROR
          end
          advance

          skip_trivia

          end_span = consume_macro_close_span("Expected '%}' after end")
          unless end_span
            return PREFIX_ERROR
          end

          full_span = start_span.cover(end_span)
          @arena.add_typed(MacroForNode.new(full_span, vars, iterable, body))
        end

        # Phase 103J: Parse {% expression %} - macro method calls
        # Example: {% skip_file %}, {% raise "error" %}, {% @type %}
        private def parse_macro_expression_control(start_span : Span, keyword_token : Token, keyword_index : Int32) : ExprId
          # We've already consumed {% keyword; reset to start of expression
          distance = @index - keyword_index
          unadvance(distance) if distance > 0

          # Parse as assignment/expression (like original parser's parse_op_assign)
          # This handles: x = 123, method_call, @type, etc.
          expr = parse_op_assign

          if expr.invalid?
            expr = fallback_macro_expression(keyword_token)
          end

          # Return expression - caller will skip trivia and consume %}
          expr
        end

        private def fallback_macro_expression(keyword_token : Token) : ExprId
          fast_forward_macro_expression
          name_slice = @string_pool.intern(keyword_token.slice)
          @arena.add_typed(IdentifierNode.new(keyword_token.span, name_slice))
        end

        private def fast_forward_macro_expression
          loop do
            token = current_token
            break if token.kind == Token::Kind::EOF
            break if macro_closing_sequence?(token)
            advance
          end
        end

        # Phase 103J: Parse {% begin %}...{% end %}
        private def parse_macro_begin_control(start_span : Span) : ExprId
          unless expect_macro_close("Expected '%}' after begin")
            return PREFIX_ERROR
          end

          # Parse body until {% end %}
          body = parse_macro_body_until_branch
          return PREFIX_ERROR if body.invalid?

          # Expect {% end %}
          end_start = consume_macro_control_start
          unless end_start
            @diagnostics << Diagnostic.new("Expected '{% end %}'", current_token.span)
            return PREFIX_ERROR
          end

          if macro_trim_token?(current_token)
            advance
          end

          skip_trivia

          unless token_text(current_token) == "end"
            @diagnostics << Diagnostic.new("Expected 'end'", current_token.span)
            return PREFIX_ERROR
          end
          advance  # end

          skip_trivia

          end_span = consume_macro_close_span("Expected '%}' after end")
          unless end_span
            return PREFIX_ERROR
          end

          # {% begin %} is like {% if true %} - always executes
          full_span = start_span.cover(end_span)
          true_node = @arena.add_typed(BoolNode.new(start_span, true))
          @arena.add_typed(MacroIfNode.new(full_span, true_node, body, nil))
        end

        # Phase 103J: Parse {% verbatim do %}...{% end %}
        # verbatim blocks are parsed (not skipped) but executed literally during macro expansion
        private def parse_macro_verbatim_control(start_span : Span) : ExprId
          # Expect 'do' keyword
          unless token_text(current_token) == "do"
            @diagnostics << Diagnostic.new("Expected 'do' after verbatim", current_token.span)
            return PREFIX_ERROR
          end
          advance  # consume do

          skip_trivia

          unless expect_macro_close("Expected '%}' after do")
            return PREFIX_ERROR
          end

          # Parse the macro body - it will consume {% end %} automatically
          # parse_macro_body handles the {% end %} internally and adds it to pieces
          pieces, macro_trim_left, macro_trim_right = parse_macro_body

          # After parse_macro_body, {% end %} has already been consumed
          # Use current token's span start as the end of verbatim block
          end_token = @index > 0 ? @tokens[@index - 1] : current_token
          verbatim_span = start_span.cover(end_token.span)
          # Create MacroLiteral node with parsed pieces (verbatim semantics handled at expansion time)
          @arena.add_typed(MacroLiteralNode.new(verbatim_span, pieces, macro_trim_left, macro_trim_right))
        end

        private def fast_forward_verbatim_body : Span?
          depth = 1
          loop do
            token = current_token
            return nil if token.kind == Token::Kind::EOF

            if token.kind == Token::Kind::LBracePercent
              saved_index = @index
              start_control_span = consume_macro_control_start
              unless start_control_span
                @index = saved_index
                @previous_token = nil
                advance
                next
              end

              # Optional trim
              if macro_trim_token?(current_token)
                advance
              end

              skip_trivia
              kw = token_text(current_token)
              advance
              skip_trivia

              case kw
              when "if", "for", "unless", "while", "begin", "verbatim"
                # For verbatim do, there may be an extra 'do'
                if kw == "verbatim" && token_text(current_token) == "do"
                  advance
                  skip_trivia
                end
                # Expect close %}
                unless expect_macro_close("Expected '%}' after #{kw}")
                  return nil
                end
                depth += 1
                next
              when "end"
                end_span = consume_macro_close_span("Expected '%}' after end")
                return nil unless end_span
                depth -= 1
                return start_control_span.cover(end_span) if depth == 0
                next
              else
                # Some other control header: treat as literal; rewind and step one token
                @index = saved_index
                @previous_token = nil
              end
            end

            advance
          end
        end

        # Parse macro body content until we hit {% elsif/else/end %}
        # Returns MacroLiteralNode with the body content
        # STUB VERSION: Just skip all tokens until target keyword
        # Full implementation would properly parse expressions and text
        private def parse_macro_body_until_branch(stop_on_branch : Bool = true) : ExprId
          start_span = current_token.span
          pieces = SmallVec(MacroPiece, 16).new
          depth = 0

          # Skip tokens until we hit {% keyword at depth 0
          loop do
            if current_token.kind == Token::Kind::EOF
              break
            end

            # Check for {% sequence
            if keyword = peek_macro_keyword
              if depth == 0 && keyword == "end"
                break
              elsif stop_on_branch && depth == 0 && (keyword == "elsif" || keyword == "else")
                break
              elsif keyword == "if" || keyword == "for" || keyword == "unless" || keyword == "while" || keyword == "begin" || keyword == "verbatim"
                skip_nested_macro_control
                next
              end
            end

            # Just skip this token
            advance
          end

          # Return stub MacroLiteralNode
          @arena.add_typed(MacroLiteralNode.new(start_span, pieces.to_a, false, false))
        end

        # Skip an entire nested macro control structure {% ... %}...{% end %}
        private def skip_nested_macro_control
          depth = 1
          start_span = consume_macro_control_start
          return unless start_span

          if macro_trim_token?(current_token)
            advance
          end

          loop do
            if current_token.kind == Token::Kind::EOF
              break
            end

            # Check for {% sequence
            if keyword = peek_macro_keyword
              saved = @index
              consume_macro_control_start

              if macro_trim_token?(current_token)
                advance
              end

              skip_trivia
              keyword_token = current_token
              keyword_text = token_text(keyword_token)
              advance
              skip_trivia

              if keyword_text == "if" || keyword_text == "for" || keyword_text == "unless" || keyword_text == "while" || keyword_text == "begin" || keyword_text == "verbatim"
                depth += 1
              elsif keyword_text == "end"
                depth -= 1
                if depth == 0
                  consume_macro_close_span
                  return
                end
              end

              @index = saved
              @previous_token = nil
            end

            advance
          end
        end

        # Parse macro body content until we hit {% end %}
        private def parse_macro_body_until_end : ExprId
          parse_macro_body_until_branch
        end

        # Phase 103B: Parse {{ expr }} in expression context
        # Called from parse_prefix when seeing {{ token sequence
        # Returns MacroExpressionNode wrapped in ExprId
        private def parse_percent_macro_expression : ExprId
          start_span = current_token.span

          # Expect {{ sequence
          unless current_token.kind == Token::Kind::LBrace
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end
          advance  # consume first {

          unless current_token.kind == Token::Kind::LBrace
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end
          advance  # consume second {

          skip_trivia

          # Parse expression inside {{ }}
          expr = with_macro_terminator(:expression) { parse_expression(0) }
          return PREFIX_ERROR if expr.invalid?

          skip_trivia

          # Expect }} sequence
          unless current_token.kind == Token::Kind::RBrace
            @diagnostics << Diagnostic.new("Expected '}' to close macro expression", current_token.span)
            return PREFIX_ERROR
          end
          advance  # consume first }

          unless current_token.kind == Token::Kind::RBrace
            @diagnostics << Diagnostic.new("Expected '}' to close macro expression", current_token.span)
            return PREFIX_ERROR
          end
          end_span = current_token.span
          advance  # consume second }

          full_span = start_span.cover(end_span)
          @arena.add_typed(MacroExpressionNode.new(full_span, expr))
        end

        private def parse_macro_expression_piece
          start_token = current_token
          # Phase 15: { is LBrace token
          expect_operator(Token::Kind::LBrace)
          expect_operator(Token::Kind::LBrace)
          skip_macro_whitespace
          expr = with_macro_terminator(:expression) { parse_expression(0) }
          skip_macro_whitespace

          # Phase 15: } is RBrace token
          expect_operator(Token::Kind::RBrace)
          expect_operator(Token::Kind::RBrace)
          closing_span = previous_token.try(&.span)

          newline_escape = consume_macro_newline_escape?
          skip_whitespace = newline_escape

          macro_span = closing_span ? start_token.span.cover(closing_span) : start_token.span
          macro_expr_id = @arena.add_typed(MacroExpressionNode.new(macro_span, expr))
          piece = MacroPiece.expression(macro_expr_id, false, false, macro_span)
          {piece, skip_whitespace}
        end

        # Phase 103: Inline hot path - frequently used for span calculations
        @[AlwaysInline]
        private def node_span(id : ExprId) : Span
          @arena[id].span
        end

        private def cover_optional_spans(*spans : Span?) : Span
          # Fast-path: avoid building arrays; compute min start and max end
          min_start : Span? = nil
          max_end : Span? = nil
          i = 0
          while i < spans.size
            if s = spans.unsafe_fetch(i)
              if min_start.nil? || s.start_offset < min_start.not_nil!.start_offset
                min_start = s
              end
              if max_end.nil? || s.end_offset > max_end.not_nil!.end_offset
                max_end = s
              end
            end
            i += 1
          end
          first = min_start
          last = max_end
          raise ArgumentError.new("cover_optional_spans requires at least one span") if first.nil? || last.nil?
          f = first.not_nil!
          l = last.not_nil!
          Span.new(f.start_offset, l.end_offset, f.start_line, f.start_column, l.end_line, l.end_column)
        end

        BINARY_PRECEDENCE = {
          Token::Kind::Question  => 2,   # Ternary operator (Phase 23, second lowest)
          Token::Kind::NilCoalesce => 3, # Nil-coalescing (Phase 81)
          Token::Kind::OrOr      => 3,   # Logical OR
          Token::Kind::AndAnd    => 4,   # Logical AND
          Token::Kind::DotDot    => 5,   # Inclusive range (Phase 13)
          Token::Kind::DotDotDot => 5,   # Exclusive range (Phase 13)
          Token::Kind::Pipe      => 6,   # Bitwise OR (Phase 21)
          Token::Kind::Caret     => 6,   # Bitwise XOR (Phase 21)
          Token::Kind::Amp       => 6,   # Bitwise AND (Phase 21)
          Token::Kind::EqEq      => 7,   # Equality
          Token::Kind::EqEqEq    => 7,   # Case equality (Phase 50)
          Token::Kind::NotEq     => 7,   # Inequality
          Token::Kind::Less      => 7,   # Less than
          Token::Kind::Greater   => 7,   # Greater than
          Token::Kind::LessEq    => 7,   # Less or equal
          Token::Kind::GreaterEq => 7,   # Greater or equal
          Token::Kind::Spaceship => 7,   # Three-way comparison (Phase 48)
          Token::Kind::Match     => 7,   # Regex match (Phase 80)
          Token::Kind::NotMatch  => 7,   # Regex not match (Phase 80)
          Token::Kind::In        => 7,   # Containment check (Phase 79)
          Token::Kind::Plus      => 10,  # Addition
          Token::Kind::Minus     => 10,  # Subtraction
          Token::Kind::AmpPlus   => 10,  # Wrapping addition (Phase 89)
          Token::Kind::AmpMinus  => 10,  # Wrapping subtraction (Phase 89)
          Token::Kind::LShift    => 10,  # Left shift / array push (Phase 9)
          Token::Kind::RShift    => 10,  # Right shift (Phase 22)
          Token::Kind::Star      => 20,  # Multiplication
          Token::Kind::Slash     => 20,  # Division
          Token::Kind::FloorDiv  => 20,  # Floor division (Phase 78)
          Token::Kind::Percent   => 20,  # Modulo (Phase 18)
          Token::Kind::AmpStar   => 20,  # Wrapping multiplication (Phase 89)
          Token::Kind::StarStar  => 25,  # Exponentiation (Phase 19, highest precedence)
          Token::Kind::AmpStarStar => 25, # Wrapping exponentiation (Phase 89)
        }

        # Phase 101: Parse block shorthand (&.method)
        # Transforms: try &.each_value
        # Into: try { |__arg0| __arg0.each_value }
        private def parse_block_shorthand(amp_token : Token) : ExprId
          location_start = amp_token.span

          # Generate temporary variable name
          temp_name = temp_var_name
          temp_name_slice = Slice(UInt8).new(temp_name.to_unsafe, temp_name.bytesize)

          # Create identifier node for temp variable
          temp_var = @arena.add_typed(IdentifierNode.new(
            location_start,  # Will be updated later
            @string_pool.intern(temp_name_slice)
          ))

          # Handle dot consumption and parsing based on token type
          if amp_token.kind == Token::Kind::Amp
            # Separate tokens: need to consume Operator (dot)
            if current_token.kind == Token::Kind::Operator
              advance
              skip_trivia
            else
              @diagnostics << Diagnostic.new("Expected '.' after '&' in block shorthand", current_token.span)
              return PREFIX_ERROR
            end

            # After consuming dot, manually create MemberAccess (dot already consumed)
            # Same logic as AmpDot case
            if current_token.kind == Token::Kind::LBracket
              # Indexing: &.[0]
              call_expr = parse_index(temp_var)
            else
              # Everything else: identifier, keyword, or operator as method name
              method_name = current_token.slice
              method_span = current_token.span
              advance

              # Create MemberAccess: temp_var.method
              member_span = location_start.cover(method_span)
              call_expr = @arena.add_typed(MemberAccessNode.new(
                member_span,
                temp_var,
                method_name
              ))
            end

            # Phase 103J: Parse method chains: &.value.close
            call_expr = consume_block_shorthand_postfix(call_expr)

            # Phase 101: Check for trailing do...end or {...} block (Amp case)
            # This handles: &.each_value do |x| ... end
            if current_token.kind == Token::Kind::Do || current_token.kind == Token::Kind::LBrace
              trailing_block = parse_block
              return PREFIX_ERROR if trailing_block.invalid?

              # Convert MemberAccess to Call with block argument
              call_expr_node = @arena[call_expr]
              call_span = call_expr_node.span.cover(@arena[trailing_block].span)
              call_expr = @arena.add_typed(
                CallNode.new(
                  call_span,
                  call_expr,
                  [trailing_block],
                  nil
                )
              )
            end
          elsif amp_token.kind == Token::Kind::AmpDot
            # AmpDot is a single token, dot already consumed
            # Current token can be: identifier, keyword (select), operator (+), or [ for indexing
            if current_token.kind == Token::Kind::LBracket
              # Indexing: &.[0]
              call_expr = parse_index(temp_var)
            else
              # Everything else: identifier, keyword, or operator as method name
              # In Crystal, keywords and operators can be method names after dot
              method_name = current_token.slice
              method_span = current_token.span
              advance

              # Create MemberAccess: temp_var.method
              member_span = location_start.cover(method_span)
              call_expr = @arena.add_typed(MemberAccessNode.new(
                member_span,
                temp_var,
                method_name
              ))
            end

            # Phase 103J: Parse method chains: &.value.close
            call_expr = consume_block_shorthand_postfix(call_expr)

            # Phase 101: Check for trailing do...end or {...} block (AmpDot case)
            # This handles: &.each_value do |x| ... end
            if current_token.kind == Token::Kind::Do || current_token.kind == Token::Kind::LBrace
              trailing_block = parse_block
              return PREFIX_ERROR if trailing_block.invalid?

              # Convert MemberAccess to Call with block argument
              call_expr_node = @arena[call_expr]
              call_span = call_expr_node.span.cover(@arena[trailing_block].span)
              call_expr = @arena.add_typed(
                CallNode.new(
                  call_span,
                  call_expr,
                  [trailing_block],
                  nil
                )
              )
            end
          else
            @diagnostics << Diagnostic.new("Expected '&' or '&.' for block shorthand", current_token.span)
            return PREFIX_ERROR
          end

          # Handle setter-style shorthand: &.foo=(value)
          if current_token.kind == Token::Kind::Eq
            advance
            skip_whitespace_and_optional_newlines
            value_expr = parse_expression(0)
            return PREFIX_ERROR if value_expr.invalid?

            assign_span = @arena[call_expr].span.cover(@arena[value_expr].span)
            call_expr = @arena.add_typed(AssignNode.new(
              assign_span,
              call_expr,
              value_expr
            ))
          end

          return PREFIX_ERROR if call_expr.invalid?

          # Get full span
          call_span = @arena[call_expr].span
          full_span = location_start.cover(call_span)

          # Create parameter for block: |__arg0|
          # TIER 2.1: Use temp_name_slice (zero-copy)
          param = Parameter.new(temp_name_slice, span: location_start, name_span: location_start)


          # Create block: { |__arg0| __arg0.method }
          block_id = @arena.add_typed(BlockNode.new(
            full_span,
            [param],
            [call_expr]
          ))

          block_id
        end

        private def consume_block_shorthand_postfix(call_expr : ExprId) : ExprId
          loop do
            skip_trivia
            token = current_token
            case token.kind
            when Token::Kind::Operator
              if slice_eq?(token.slice, ".")
                call_expr = parse_member_access(call_expr)
              else
                break
              end
            when Token::Kind::LParen
              call_expr = parse_parenthesized_call(call_expr)
            when Token::Kind::LBracket
              call_expr = parse_index(call_expr)
            else
              break
            end
          end
          call_expr
        end

        # Phase 103K: Parse fun parameters (supports proc types and varargs)
        # Example: fun foo(x : Int, callback : Int -> Void, ...) : Bool
        # Returns: {params, varargs_flag} or ExprId on error
        private def parse_fun_params : {Array(Parameter), Bool} | ExprId
          params = Array(Parameter).new(2)
          varargs = false
          skip_trivia
          return {params, varargs} unless operator_token?(current_token, Token::Kind::LParen)

          advance  # consume (
          @paren_depth += 1  # Track delimiter depth for multi-line support
          skip_whitespace_and_optional_newlines  # Skip newlines inside parentheses

          unless operator_token?(current_token, Token::Kind::RParen)
            loop do
              # Check for varargs: ...
              if current_token.kind == Token::Kind::DotDotDot
                varargs = true
                advance
                skip_whitespace_and_optional_newlines
                # Varargs must be last parameter
                break
              end

              param_start = current_token

              # Parse parameter name (optional for top-level fun)
              # Check if this is a named parameter (identifier : type) or unnamed (just type)
              param_name : Slice(UInt8)?
              param_name_span : Span?

              if current_token.kind == Token::Kind::Identifier
                # Lookahead to check if this identifier is followed by ':'
                # If yes: named parameter (name : type)
                # If no: unnamed parameter (just type - identifier is the type name)
                saved_index = @index
                advance
                skip_whitespace_and_optional_newlines
                has_colon = operator_token?(current_token, Token::Kind::Colon)
                @index = saved_index  # Restore position

                if has_colon
                  # Named parameter: name : type
                  param_name = current_token.slice
                  param_name_span = current_token.span
                  advance
                  skip_whitespace_and_optional_newlines
                  advance  # consume :
                  skip_whitespace_and_optional_newlines
                end
                # If no colon, fall through to parse_bare_proc_type (unnamed parameter)
              end

              # Parse parameter type using parse_bare_proc_type
              # This handles: Int32, Void* -> Void*, (Int, String) -> Bool, etc.
              param_type = parse_bare_proc_type
              if param_type.nil?
                emit_unexpected(current_token)
                @paren_depth -= 1
                return PREFIX_ERROR
              end

              param_span = param_start.span.cover(previous_token.not_nil!.span)

              params << Parameter.new(
                param_name,
                nil,           # no external name
                param_type,    # type from parse_bare_proc_type
                nil,           # no default value
                param_span,
                param_name_span,
                nil,           # no external name span
                nil,           # type span same as param_span for now
                nil            # no default span
              )

              skip_whitespace_and_optional_newlines

              if current_token.kind == Token::Kind::Comma
                advance
                skip_whitespace_and_optional_newlines
              else
                break
              end
            end
          end

          @paren_depth -= 1  # Exiting parentheses
          expect_operator(Token::Kind::RParen)
          {params, varargs}
        end

        # Phase 103K: Type parsing methods for fun declarations and type annotations
        # These methods implement full type parsing including proc types, union types, generics, etc.

        # Parse "bare" proc type (no wrapping parens required)
        # Examples: Int32, Int32 -> Void, Int32, String -> Bool
        # Returns type as zero-copy slice covering the full type expression
        private def parse_bare_proc_type : Slice(UInt8)?
          start_token = current_token

          # Allow bare proc notation with no arguments: -> ReturnType or just ->
          if start_token.kind == Token::Kind::ThinArrow
            advance
            skip_trivia

            has_return_type = case current_token.kind
                              when Token::Kind::Identifier, Token::Kind::Self,
                                   Token::Kind::Typeof, Token::Kind::LParen,
                                   Token::Kind::LBrace
                                true
                              else
                                false
                              end

            if has_return_type
              return_type = parse_union_type_for_annotation
              return nil if return_type.nil?
            end

            end_token = previous_token || start_token
            start_ptr = start_token.slice.to_unsafe
            end_ptr = end_token.slice.to_unsafe + end_token.slice.size
            return Slice.new(start_ptr, end_ptr - start_ptr)
          end

          # Parse first type
          first_type_start = current_token
          type = parse_union_type_for_annotation
          return nil if type.nil?

          skip_trivia

          # Check if this is a proc type (has -> or comma before ->)
          unless current_token.kind == Token::Kind::ThinArrow ||
                 (current_token.kind == Token::Kind::Comma && lookahead_for_arrow?)
            # Simple type, not proc
            return type
          end

          # This is a proc type - collect all input types
          if current_token.kind == Token::Kind::Comma
            # Multiple input types: Type1, Type2, ... -> ReturnType
            loop do
              advance  # consume comma
              skip_trivia

              next_type = parse_union_type_for_annotation
              return nil if next_type.nil?

              skip_trivia
              break unless current_token.kind == Token::Kind::Comma
            end
          end

          # Now parse -> ReturnType
          unless current_token.kind == Token::Kind::ThinArrow
            return nil
          end

          advance  # consume ->
          skip_trivia

          # Parse return type (optional - can be void)
          # Check if there's a type after the arrow
          has_return_type = case current_token.kind
                            when Token::Kind::Identifier, Token::Kind::Self,
                                 Token::Kind::Typeof, Token::Kind::LParen,
                                 Token::Kind::LBrace
                              true
                            else
                              false
                            end

          if has_return_type
            return_type = parse_union_type_for_annotation
            # If we expected a type but couldn't parse it, error
            return nil if return_type.nil?
          end

          # Build full proc type as zero-copy slice
          start_ptr = start_token.slice.to_unsafe
          end_token = previous_token.not_nil!
          end_ptr = end_token.slice.to_unsafe + end_token.slice.size
          Slice.new(start_ptr, end_ptr - start_ptr)
        end

        # Helper: lookahead to check if there's an arrow after commas
        private def lookahead_for_arrow? : Bool
          saved_index = @index

          # Skip commas and types
          loop do
            break unless current_token.kind == Token::Kind::Comma
            advance
            skip_trivia

            # Try to skip over a type
            case current_token.kind
            when Token::Kind::Identifier, Token::Kind::Self
              advance
              skip_trivia

              # Skip type suffixes
              while current_token.kind == Token::Kind::Star ||
                    current_token.kind == Token::Kind::StarStar ||
                    current_token.kind == Token::Kind::Question
                advance
                skip_trivia
              end
            else
              @index = saved_index
              return false
            end
          end

          result = current_token.kind == Token::Kind::ThinArrow
          @index = saved_index
          result
        end

        # Parse union type: Type1 | Type2 | Type3
        # Returns zero-copy slice covering the full union type
        private def parse_union_type_for_annotation : Slice(UInt8)?
          start_token = current_token

          # Parse first type
          type = parse_atomic_type_with_suffix_for_annotation
          return nil if type.nil?

          skip_trivia

          # Check for union operator |
          unless current_token.kind == Token::Kind::Pipe
            return type
          end

          # Parse additional types in union
          loop do
            advance  # consume |
            skip_trivia

            next_type = parse_atomic_type_with_suffix_for_annotation
            return nil if next_type.nil?

            skip_trivia
            break unless current_token.kind == Token::Kind::Pipe
          end

          # Build full union type as zero-copy slice
          end_token = previous_token.not_nil!
          start_ptr = start_token.slice.to_unsafe
          end_ptr = end_token.slice.to_unsafe + end_token.slice.size
          Slice.new(start_ptr, end_ptr - start_ptr)
        end

        # Parse atomic type with suffixes (?, *, **, [])
        # Returns zero-copy slice
        private def parse_atomic_type_with_suffix_for_annotation : Slice(UInt8)?
          start_token = current_token

          # Parse base atomic type
          base_type = parse_atomic_type_for_annotation
          return nil if base_type.nil?

          # Parse type suffixes
          loop do
            case current_token.kind
            when Token::Kind::Question
              # Nilable: Type?
              advance
            when Token::Kind::Star
              # Pointer: Type*
              advance
            when Token::Kind::StarStar
              # Double pointer: Type**
              advance
            when Token::Kind::LBracket
              # Static array: Type[N]
              advance
              # Skip contents until ]
              bracket_depth = 1
              while bracket_depth > 0 && current_token.kind != Token::Kind::EOF
                if current_token.kind == Token::Kind::LBracket
                  bracket_depth += 1
                elsif current_token.kind == Token::Kind::RBracket
                  bracket_depth -= 1
                end
                advance
              end
            else
              break
            end
            skip_trivia
          end

          # Build full type with suffixes as zero-copy slice
          end_token = previous_token.not_nil!
          # Allow trailing '.class' after type
          if current_token.kind == Token::Kind::Operator && slice_eq?(current_token.slice, ".")
            saved = @index
            advance
            if current_token.kind == Token::Kind::Class
              end_token = current_token
              advance
            else
              @index = saved
            end
          end
          start_ptr = start_token.slice.to_unsafe
          end_ptr = end_token.slice.to_unsafe + end_token.slice.size
          Slice.new(start_ptr, end_ptr - start_ptr)
        end

        # Parse atomic (base) type
        # Handles: identifiers, paths (A::B), generics (Box(T)), tuples, self, typeof, proc types with parens
        # Returns zero-copy slice
        private def parse_atomic_type_for_annotation : Slice(UInt8)?
          start_token = current_token

          case current_token.kind
          when Token::Kind::Self
            # self type
            advance
            skip_trivia
            return start_token.slice

          when Token::Kind::Typeof
            # typeof(expr)
            advance
            skip_trivia
            return nil unless current_token.kind == Token::Kind::LParen

            advance  # consume (
            paren_depth = 1
            while paren_depth > 0 && current_token.kind != Token::Kind::EOF
              if current_token.kind == Token::Kind::LParen
                paren_depth += 1
              elsif current_token.kind == Token::Kind::RParen
                paren_depth -= 1
              end
              advance
            end
            skip_trivia

            end_token = previous_token.not_nil!
            start_ptr = start_token.slice.to_unsafe
            end_ptr = end_token.slice.to_unsafe + end_token.slice.size
            return Slice.new(start_ptr, end_ptr - start_ptr)

          when Token::Kind::Identifier
            # Type name, possibly with :: path and generics
            advance
            skip_trivia

            # Handle :: scope resolution
            while current_token.kind == Token::Kind::ColonColon
              advance  # consume ::
              skip_trivia
              return nil unless current_token.kind == Token::Kind::Identifier
              advance
              skip_trivia
            end

            # Handle generic parameters: Type(A, B, C)
            if current_token.kind == Token::Kind::LParen
              advance  # consume (
              paren_depth = 1

              while paren_depth > 0 && current_token.kind != Token::Kind::EOF
                if current_token.kind == Token::Kind::LParen
                  paren_depth += 1
                elsif current_token.kind == Token::Kind::RParen
                  paren_depth -= 1
                end
                advance
              end
              skip_trivia
            end

            end_token = previous_token.not_nil!
            start_ptr = start_token.slice.to_unsafe
            end_ptr = end_token.slice.to_unsafe + end_token.slice.size
            return Slice.new(start_ptr, end_ptr - start_ptr)

          when Token::Kind::ColonColon
            # Global path: ::Type
            advance  # consume ::
            skip_trivia
            return nil unless current_token.kind == Token::Kind::Identifier

            # Now parse as identifier with potential :: and generics
            advance
            skip_trivia

            while current_token.kind == Token::Kind::ColonColon
              advance
              skip_trivia
              return nil unless current_token.kind == Token::Kind::Identifier
              advance
              skip_trivia
            end

            if current_token.kind == Token::Kind::LParen
              advance
              paren_depth = 1
              while paren_depth > 0 && current_token.kind != Token::Kind::EOF
                if current_token.kind == Token::Kind::LParen
                  paren_depth += 1
                elsif current_token.kind == Token::Kind::RParen
                  paren_depth -= 1
                end
                advance
              end
              skip_trivia
            end

            end_token = previous_token.not_nil!
            start_ptr = start_token.slice.to_unsafe
            end_ptr = end_token.slice.to_unsafe + end_token.slice.size
            return Slice.new(start_ptr, end_ptr - start_ptr)

          when Token::Kind::LBrace
            # Tuple or named tuple: {A, B} or {name: A, age: B}
            advance  # consume {
            brace_depth = 1

            while brace_depth > 0 && current_token.kind != Token::Kind::EOF
              if current_token.kind == Token::Kind::LBrace
                brace_depth += 1
              elsif current_token.kind == Token::Kind::RBrace
                brace_depth -= 1
              end
              advance
            end
            skip_trivia

            end_token = previous_token.not_nil!
            start_ptr = start_token.slice.to_unsafe
            end_ptr = end_token.slice.to_unsafe + end_token.slice.size
            return Slice.new(start_ptr, end_ptr - start_ptr)

          when Token::Kind::ThinArrow
            # Proc with no input types: -> ReturnType
            advance  # consume ->
            skip_trivia

            # Parse return type
            return_type = parse_union_type_for_annotation
            return nil if return_type.nil?

            end_token = previous_token.not_nil!
            start_ptr = start_token.slice.to_unsafe
            end_ptr = end_token.slice.to_unsafe + end_token.slice.size
            return Slice.new(start_ptr, end_ptr - start_ptr)

          when Token::Kind::LParen
            # Parenthesized proc type: (A, B) -> C or (A) -> B or just (Type)
            advance  # consume (
            skip_trivia

            # Parse first type
            first_type = parse_union_type_for_annotation
            return nil if first_type.nil?

            skip_trivia

            if current_token.kind == Token::Kind::RParen
              # Single type in parens: (Type)
              advance  # consume )
              skip_trivia

              # Check if this is proc type: (Type) -> ReturnType
              if current_token.kind == Token::Kind::ThinArrow
                advance  # consume ->
                skip_trivia

                return_type = parse_union_type_for_annotation
                return nil if return_type.nil?
              end

              end_token = previous_token.not_nil!
              start_ptr = start_token.slice.to_unsafe
              end_ptr = end_token.slice.to_unsafe + end_token.slice.size
              return Slice.new(start_ptr, end_ptr - start_ptr)

            elsif current_token.kind == Token::Kind::Comma
              # Multiple types: (A, B, ...) or (A, B, ... -> C)
              loop do
                advance  # consume comma
                skip_trivia
                break if current_token.kind == Token::Kind::RParen

                next_type = parse_union_type_for_annotation
                return nil if next_type.nil?

                skip_trivia
                break unless current_token.kind == Token::Kind::Comma
              end

              # Check for -> inside parens: (A, B, C -> D)
              if current_token.kind == Token::Kind::ThinArrow
                advance  # consume ->
                skip_trivia

                return_type = parse_union_type_for_annotation
                return nil if return_type.nil?

                skip_trivia
              end

              return nil unless current_token.kind == Token::Kind::RParen
              advance  # consume )
              skip_trivia

              # Check for -> after parens: (A, B, C) -> D
              if current_token.kind == Token::Kind::ThinArrow
                advance  # consume ->
                skip_trivia

                return_type = parse_union_type_for_annotation
                return nil if return_type.nil?
              end

              end_token = previous_token.not_nil!
              start_ptr = start_token.slice.to_unsafe
              end_ptr = end_token.slice.to_unsafe + end_token.slice.size
              return Slice.new(start_ptr, end_ptr - start_ptr)
            else
              return nil
            end

          else
            # Unknown type start
            return nil
          end
        end

        UNARY_OPERATORS = [Token::Kind::Plus, Token::Kind::Minus, Token::Kind::AmpPlus, Token::Kind::AmpMinus, Token::Kind::Not, Token::Kind::Tilde]
      end
    end
  end
end
