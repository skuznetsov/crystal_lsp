require "./ast"
require "./lexer"
require "./lexer/token"
require "./parser/diagnostic"

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

        def initialize(lexer : Lexer)
          @tokens = [] of Token
          lexer.each_token { |token| @tokens << token }
          @index = 0
          @arena = AstArena.new
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
        end

        # Phase 87B-2: Constructor for reparsing with existing arena
        # Used by macro expander to add parsed nodes to existing arena
        def initialize(lexer : Lexer, @arena : AstArena)
          @tokens = [] of Token
          lexer.each_token { |token| @tokens << token }
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
        end

        def parse_program : Program
          roots = [] of ExprId
          while current_token.kind != Token::Kind::EOF
            skip_trivia
            break if current_token.kind == Token::Kind::EOF

            if macro_definition_start?
              macro_def = parse_macro_definition
              roots << macro_def unless macro_def.invalid?
              consume_newlines
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
                  parse_annotation
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
              roots << node unless node.invalid?
              consume_newlines
              next
            end

            expr = parse_statement
            roots << expr unless expr.invalid?
            consume_newlines
          end
          Program.new(@arena, roots)
        end

        # Parse a statement (assignment or expression)
        private def parse_statement : ExprId
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

          # Phase 10: yield statements
          if current_token.kind == Token::Kind::Yield
            stmt = parse_yield
            return parse_postfix_if_modifier(stmt)
          end

          # Phase 39: super statements
          if current_token.kind == Token::Kind::Super
            stmt = parse_super
            return parse_postfix_if_modifier(stmt)
          end

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

          # Parse expression or assignment (like original Crystal's parse_op_assign)
          # This handles: assignments, multiple assignments, type declarations
          left = parse_op_assign
          return PREFIX_ERROR if left.invalid?

          skip_trivia
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
            skip_trivia

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
          @tokens[@index]
        end

        @[AlwaysInline]
        private def previous_token
          @previous_token
        end

        # Phase 103: Inline hot path - called after every token
        @[AlwaysInline]
        private def advance
          @previous_token = current_token
          @index += 1 if @index < @tokens.size - 1
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
            else
              break
            end
          end
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

        # Phase 103: Parse type annotation (supports namespaces, generics, unions, suffixes)
        # Examples: Int32, Token::Kind, Array(Int32), Int32 | String, Int32?
        # Returns: Slice from source covering the entire type (zero-copy!)
        private def parse_type_annotation : Slice(UInt8)
          start_token = current_token
          last_type_token = start_token
          paren_depth = 0
          bracket_depth = 0

          loop do
            token = current_token

            # Stop conditions (when not inside parentheses/brackets)
            if paren_depth == 0 && bracket_depth == 0
              break if token.kind == Token::Kind::Eq
              break if token.kind == Token::Kind::Comma
              break if operator_token?(token, Token::Kind::RParen)
              break if token.kind == Token::Kind::Newline
              break if token.kind == Token::Kind::EOF
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

            # Check if this token is part of type annotation
            is_type_token = case token.kind
            when Token::Kind::Identifier, Token::Kind::Number,
                 Token::Kind::ColonColon, Token::Kind::Operator,
                 Token::Kind::ThinArrow
              true
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
        # Called from parse_prefix when identifier followed by colon
        private def parse_type_declaration_from_identifier(identifier_token : Token) : ExprId
          # Current token is ':'
          advance  # consume ':'
          skip_trivia

          # Parse type annotation (supports complex types: Token::Kind, Array(Int32), etc.)
          type_annotation = parse_type_annotation
          if type_annotation.empty?
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end

          # Check for optional value: = expression
          value_expr : ExprId? = nil
          if current_token.kind == Token::Kind::Eq
            advance  # consume '='
            skip_trivia

            # Parse value expression
            val = parse_expression(0)
            return PREFIX_ERROR if val.invalid?
            value_expr = val
          end

          # Create type declaration node
          type_decl_span = if value_expr
            identifier_token.span.cover(@arena[value_expr].span)
          else
            identifier_token.span.cover(previous_token.not_nil!.span)
          end

          @arena.add_typed(
            TypeDeclarationNode.new(
              type_decl_span,
              identifier_token.slice,
              type_annotation,  # Already Slice(UInt8)
              value_expr
            )
          )
        end

        # Phase 100: macro upgraded from identifier to keyword
        private def macro_definition_start?
          current_token.kind == Token::Kind::Macro
        end

        # Phase 100: Added Macro to definition_start?
        private def definition_start?
          token = current_token
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
          unless name_token.kind == Token::Kind::Identifier
            emit_unexpected(name_token)
            return PREFIX_ERROR
          end
          advance

          skip_macro_parameters
          consume_newlines

          pieces, trim_left, trim_right = parse_macro_body
          expect_identifier("end")
          end_token = previous_token
          consume_newlines

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
              name_token.slice,
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
            skip_trivia

            if current_token.kind == Token::Kind::Operator && slice_eq?(current_token.slice, ".")
              dot = current_token
              advance
              skip_trivia
              {receiver, dot}
            else
              # self without dot - not a receiver, rewind
              @index -= 1
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
            # [] or []= operator
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

            # Check for = ([]= assignment operator)
            if current_token.kind == Token::Kind::Eq
              eq_token = current_token
              advance
              # Combine []= into single slice via string pool
              method_name_slice = @string_pool.intern("[]=".to_slice)
            else
              # Just [] indexer
              method_name_slice = @string_pool.intern("[]".to_slice)
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

          else
            emit_unexpected(name_token)
            return PREFIX_ERROR
          end

          params = parse_method_params
          return PREFIX_ERROR if params.is_a?(ExprId)  # Phase 71: Handle error from default value parsing

          # Parse optional return type annotation: : ReturnType
          return_type = nil
          skip_trivia
          if operator_token?(current_token, Token::Kind::Colon)
            advance  # consume ':'
            skip_trivia

            # Parse return type - supports generic types like Box(T)
            type_start_token = current_token
            type_end_token = current_token

            if type_start_token.kind == Token::Kind::Identifier
              type_end_token = type_start_token
              advance

              # Week 1 Day 2: Check for generic type parameters: Box(T)
              if operator_token?(current_token, Token::Kind::LParen)
                type_end_token = current_token  # '('
                advance  # consume '('
                paren_depth = 1

                # Parse until matching ')' - collect all non-whitespace tokens
                while paren_depth > 0 && current_token.kind != Token::Kind::EOF
                  # STOP at newline - type annotations are single-line
                  break if current_token.kind == Token::Kind::Newline

                  if operator_token?(current_token, Token::Kind::RParen)
                    type_end_token = current_token  # Include ')'
                    paren_depth -= 1
                    advance  # consume ')'
                    break if paren_depth == 0  # Found matching ')'
                  elsif operator_token?(current_token, Token::Kind::LParen)
                    type_end_token = current_token
                    paren_depth += 1
                    advance
                  elsif current_token.kind != Token::Kind::Whitespace
                    # Non-whitespace token inside parens
                    type_end_token = current_token
                    advance
                  else
                    # Skip whitespace without updating type_end_token
                    advance
                  end
                end
              end

              # Zero-copy return type annotation using pointer arithmetic
              start_ptr = type_start_token.slice.to_unsafe
              end_ptr = type_end_token.slice.to_unsafe + type_end_token.slice.size
              return_type = Slice.new(start_ptr, end_ptr - start_ptr)
            else
              emit_unexpected(type_start_token)
            end
          end

          consume_newlines

          # Phase 36: Abstract methods have no body
          body_ids = nil
          if !is_abstract
            actual_body = [] of ExprId
            loop do
              skip_trivia
              token = current_token
              break if token.kind == Token::Kind::End
              break if token.kind == Token::Kind::EOF

              # Phase 5B: Use parse_statement to handle assignments in method bodies
              expr = parse_statement
              actual_body << expr unless expr.invalid?
              consume_newlines
            end

            expect_identifier("end")
            end_token = previous_token
            consume_newlines
            body_ids = actual_body
          else
            # Abstract methods have no body, no 'end' keyword
            end_token = nil
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

          # Parse parameters (same as def)
          params = parse_method_params
          return PREFIX_ERROR if params.is_a?(ExprId)  # Phase 71: Handle error from default value parsing

          # Parse optional return type annotation: : ReturnType
          return_type = nil
          skip_trivia
          if operator_token?(current_token, Token::Kind::Colon)
            advance  # consume ':'
            skip_trivia

            # Parse return type - supports generic types like Box(T)
            type_start_token = current_token
            type_end_token = current_token

            if type_start_token.kind == Token::Kind::Identifier
              type_end_token = type_start_token
              advance

              # Week 1 Day 2: Check for generic type parameters: Box(T)
              if operator_token?(current_token, Token::Kind::LParen)
                type_end_token = current_token  # '('
                advance  # consume '('
                paren_depth = 1

                # Parse until matching ')' - collect all non-whitespace tokens
                while paren_depth > 0 && current_token.kind != Token::Kind::EOF
                  # STOP at newline - type annotations are single-line
                  break if current_token.kind == Token::Kind::Newline

                  if operator_token?(current_token, Token::Kind::RParen)
                    type_end_token = current_token  # Include ')'
                    paren_depth -= 1
                    advance  # consume ')'
                    break if paren_depth == 0  # Found matching ')'
                  elsif operator_token?(current_token, Token::Kind::LParen)
                    type_end_token = current_token
                    paren_depth += 1
                    advance
                  elsif current_token.kind != Token::Kind::Whitespace
                    # Non-whitespace token inside parens
                    type_end_token = current_token
                    advance
                  else
                    # Skip whitespace without updating type_end_token
                    advance
                  end
                end
              end

              # Zero-copy return type annotation using pointer arithmetic
              start_ptr = type_start_token.slice.to_unsafe
              end_ptr = type_end_token.slice.to_unsafe + type_end_token.slice.size
              return_type = Slice.new(start_ptr, end_ptr - start_ptr)
            else
              emit_unexpected(type_start_token)
            end
          end

          # Fun declarations have no body (external linkage)
          fun_span = fun_token.span.cover(current_token.span)
          @arena.add_typed(
            FunNode.new(
              fun_span,
              name_token.slice,
              params,
              return_type
            )
          )
        end

        private def parse_method_params
          params = [] of Parameter
          skip_trivia
          return params unless operator_token?(current_token, Token::Kind::LParen)

          advance
          skip_trivia
          unless operator_token?(current_token, Token::Kind::RParen)
            loop do
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
              name_token = current_token
              param_name : Slice(UInt8)?
              param_name_span : Span?
              is_instance_var = false

              # Phase BLOCK_CAPTURE: Check if this is anonymous block capture (&)
              if is_block && (current_token.kind == Token::Kind::Comma || operator_token?(current_token, Token::Kind::RParen))
                # Anonymous block capture: just '&' without name
                param_name = nil
                param_name_span = nil
                param_start_span = prefix_token.not_nil!.span
                # Don't advance - comma/rparen will be handled below
              else
                # Regular parameter or named block parameter
                # Phase KEYWORD_PARAMS: Allow keywords as parameter names (e.g., 'of', 'as', 'in')
                case name_token.kind
                when Token::Kind::Identifier, Token::Kind::InstanceVar
                  # Standard parameter names
                when Token::Kind::Of, Token::Kind::As, Token::Kind::In, Token::Kind::Out,
                     Token::Kind::Do, Token::Kind::End, Token::Kind::If, Token::Kind::Unless
                  # Phase KEYWORD_PARAMS: Common keywords that can be parameter names
                else
                  emit_unexpected(name_token)
                  break
                end

                # Track if this is instance variable shorthand: @value : T
                is_instance_var = (name_token.kind == Token::Kind::InstanceVar)
                param_name = name_token.slice  # TIER 2.1: Zero-copy slice (includes '@' for instance vars)
                param_name_span = name_token.span
                param_start_span = prefix_token ? prefix_token.span : name_token.span
                advance
                skip_trivia
              end

              # Parse optional type annotation: : Type
              # Phase 103: For block parameters, parse proc type (Token ->)
              type_annotation : Slice(UInt8)? = nil  # TIER 2.1: Zero-copy slice
              param_type_span = nil
              if operator_token?(current_token, Token::Kind::Colon)
                advance  # consume ':'
                skip_trivia

                if is_block
                  # Phase 103: Block parameter - parse proc type
                  # Key insight: NEVER break on comma before finding arrow
                  # Examples:
                  #   Token ->               (single arg proc)
                  #   String, Int32 ->       (multi-arg proc)
                  #   (Int32, String) -> Bool (parenthesized proc)
                  type_start = current_token
                  last_type_token = type_start  # TIER 2.4: Track last token for zero-copy slice
                  found_arrow = false
                  paren_depth = 0

                  loop do
                    break if current_token.kind == Token::Kind::EOF

                    # Track parentheses for complex proc types
                    if operator_token?(current_token, Token::Kind::LParen)
                      paren_depth += 1
                    elsif operator_token?(current_token, Token::Kind::RParen)
                      # If at depth 0, this closes the parameter list
                      break if paren_depth == 0
                      paren_depth -= 1
                    end

                    # Check for -> (proc type arrow)
                    if current_token.kind == Token::Kind::ThinArrow
                      last_type_token = current_token  # TIER 2.4: Update last token
                      advance
                      found_arrow = true
                      # Continue to collect optional return type
                      next
                    end

                    # AFTER finding arrow, stop at delimiters
                    if found_arrow && paren_depth == 0
                      break if current_token.kind == Token::Kind::Comma
                      break if operator_token?(current_token, Token::Kind::RParen)
                    end

                    # BEFORE finding arrow, collect everything (including commas)
                    last_type_token = current_token  # TIER 2.4: Update last token (was token_text)
                    advance

                    # Skip whitespace but include in token stream
                    if current_token.kind == Token::Kind::Whitespace
                      advance
                    end
                  end

                  # TIER 2.4: Zero-copy proc type parsing using pointer arithmetic
                  if last_type_token != type_start || current_token.kind == Token::Kind::EOF
                    start_ptr = type_start.slice.to_unsafe
                    end_ptr = last_type_token.slice.to_unsafe + last_type_token.slice.size
                    type_annotation = Slice.new(start_ptr, end_ptr - start_ptr)
                  end
                  param_type_span = type_start.span.cover(previous_token.not_nil!.span) if previous_token
                else
                  # Regular parameter - parse type annotation
                  # Supports: Int32, Box(T), Array(Int32), etc.
                  type_start_token = current_token
                  type_end_token = current_token

                  if type_start_token.kind == Token::Kind::Identifier
                    # Week 1 Day 2: Build string from tokens for generic types
                    # Can't use pointer arithmetic because tokens may not be contiguous
                    type_tokens = [] of Slice(UInt8)
                    type_tokens << type_start_token.slice
                    type_end_token = type_start_token
                    advance
                    skip_trivia

                    # Phase PERCENT_LITERALS: Handle :: scope resolution in types (JSON::Any, Process::Redirect)
                    while current_token.kind == Token::Kind::ColonColon
                      type_tokens << current_token.slice  # '::'
                      type_end_token = current_token
                      advance
                      skip_trivia

                      # Expect identifier after ::
                      if current_token.kind == Token::Kind::Identifier
                        type_tokens << current_token.slice
                        type_end_token = current_token
                        advance
                        skip_trivia
                      else
                        # Invalid token after ::, but we'll let it error later
                        break
                      end
                    end

                    # Week 1 Day 2: Check for generic type parameters: Box(T)
                    if operator_token?(current_token, Token::Kind::LParen)
                      paren_depth = 1
                      type_tokens << current_token.slice  # '('
                      advance  # consume '('

                      # Parse until matching ')' - collect only significant tokens
                      while paren_depth > 0 && current_token.kind != Token::Kind::EOF
                        # Stop at newlines - type annotations are single-line
                        break if current_token.kind == Token::Kind::Newline

                        # Skip whitespace inside parens
                        if current_token.kind == Token::Kind::Whitespace
                          advance
                          next
                        end

                        if operator_token?(current_token, Token::Kind::RParen)
                          type_tokens << current_token.slice  # ')'
                          type_end_token = current_token
                          paren_depth -= 1
                          advance
                          break if paren_depth == 0
                        elsif operator_token?(current_token, Token::Kind::LParen)
                          type_tokens << current_token.slice  # '('
                          paren_depth += 1
                          type_end_token = current_token
                          advance
                        elsif current_token.kind == Token::Kind::Identifier
                          type_tokens << current_token.slice
                          type_end_token = current_token
                          advance
                        elsif current_token.kind == Token::Kind::ColonColon
                          # Phase PERCENT_LITERALS: Handle :: inside generics (Array(JSON::Any))
                          type_tokens << current_token.slice  # '::'
                          type_end_token = current_token
                          advance
                        elsif current_token.kind == Token::Kind::Comma
                          type_tokens << current_token.slice  # ','
                          type_end_token = current_token
                          advance
                        else
                          # Unknown token inside generic - stop
                          break
                        end
                      end

                      skip_trivia
                    end

                    # Build type annotation string from collected tokens
                    if type_tokens.size == 1
                      # Simple type - use zero-copy
                      type_annotation = type_tokens[0]
                    else
                      # Generic type - build string and intern for deduplication
                      # Week 1 Day 2: Use string pool to avoid memory waste for repeated generic types
                      type_str = String.build do |io|
                        type_tokens.each { |slice| io.write(slice) }
                      end
                      type_annotation = @string_pool.intern(type_str.to_slice)
                    end

                    param_type_span = type_start_token.span.cover(type_end_token.span)
                  else
                    emit_unexpected(type_start_token)
                  end
                end
              end

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
                skip_trivia
              end

              # Calculate full parameter span
              param_span = if default_value_span
                param_start_span.cover(default_value_span)
              elsif param_type_span
                param_start_span.cover(param_type_span)
              else
                param_start_span
              end

              params << Parameter.new(
                param_name,
                type_annotation,
                default_value,
                param_span,
                param_name_span,
                param_type_span,
                default_value_span,
                is_splat,
                is_double_splat,
                is_block,  # Phase 103: block parameter flag
                is_instance_var  # Instance variable parameter shorthand: @value : T
              )

              break unless operator_token?(current_token, Token::Kind::Comma)
              advance
              skip_trivia
            end
          end

          expect_operator(Token::Kind::RParen)
          params
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
          consume_newlines

          # Parse then body
          then_body = [] of ExprId
          loop do
            skip_trivia
            token = current_token
            break if token.kind == Token::Kind::Elsif || token.kind == Token::Kind::Else || token.kind == Token::Kind::End
            break if token.kind == Token::Kind::EOF

            expr = parse_statement
            then_body << expr unless expr.invalid?
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
            elsif_body = [] of ExprId
            loop do
              skip_trivia
              token = current_token
              break if token.kind == Token::Kind::Elsif || token.kind == Token::Kind::Else || token.kind == Token::Kind::End
              break if token.kind == Token::Kind::EOF

              expr = parse_statement
              elsif_body << expr unless expr.invalid?
              consume_newlines
            end

            # Capture elsif span (from elsif keyword to last expression)
            elsif_span = if elsif_body.size > 0
              last_expr = @arena[elsif_body.last]
              elsif_token.span.cover(last_expr.span)
            else
              elsif_token.span
            end

            elsifs << ElsifBranch.new(elsif_condition, elsif_body, elsif_span)
          end

          # Parse optional else body
          else_body = nil
          token = current_token
          if token.kind == Token::Kind::Else
            advance
            consume_newlines

            else_body = [] of ExprId
            loop do
              skip_trivia
              token = current_token
              break if token.kind == Token::Kind::End
              break if token.kind == Token::Kind::EOF

              expr = parse_statement
              else_body << expr unless expr.invalid?
              consume_newlines
            end
          end

          expect_identifier("end")
          end_token = previous_token
          consume_newlines

          if_span = if end_token
            if_token.span.cover(end_token.span)
          else
            if_token.span
          end

          # Set elsifs to nil if array is empty (cleaner AST)
          elsifs_field = elsifs.size > 0 ? elsifs : nil

          @arena.add_typed(
            IfNode.new(
              if_span,
              condition,
              then_body,
              elsifs_field,
              else_body  # Already nil or Array
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
          consume_newlines

          # Parse then body (executed when condition is false)
          then_body = [] of ExprId
          loop do
            skip_trivia
            token = current_token
            break if token.kind == Token::Kind::Else || token.kind == Token::Kind::End
            break if token.kind == Token::Kind::EOF

            expr = parse_statement
            then_body << expr unless expr.invalid?
            consume_newlines
          end

          # Parse optional else body (executed when condition is true)
          else_body = nil
          token = current_token
          if token.kind == Token::Kind::Else
            advance
            consume_newlines

            else_body = [] of ExprId
            loop do
              skip_trivia
              token = current_token
              break if token.kind == Token::Kind::End
              break if token.kind == Token::Kind::EOF

              expr = parse_statement
              else_body << expr unless expr.invalid?
              consume_newlines
            end
          end

          expect_identifier("end")
          end_token = previous_token
          consume_newlines

          unless_span = if end_token
            unless_token.span.cover(end_token.span)
          else
            unless_token.span
          end

          @arena.add_typed(
            UnlessNode.new(
              unless_span,
              condition,
              then_body,
              else_body
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
          when_branches = [] of WhenBranch
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
            conditions = [] of ExprId
            loop do
              cond = parse_expression(0)
              return PREFIX_ERROR if cond.invalid?
              conditions << cond

              skip_trivia
              break unless current_token.kind == Token::Kind::Comma
              advance  # consume comma
              consume_newlines  # Phase 103: allow newlines after comma
            end

            skip_trivia

            # Optional "then" keyword
            if current_token.kind == Token::Kind::Then
              advance
            end

            consume_newlines

            # Parse when body
            when_body = [] of ExprId
            loop do
              skip_trivia
              token = current_token
              break if token.kind.in?(Token::Kind::When, Token::Kind::Else, Token::Kind::End, Token::Kind::EOF)

              stmt = parse_statement
              when_body << stmt unless stmt.invalid?
              consume_newlines
            end

            # Capture when span
            when_span = if when_body.size > 0
              last_expr = @arena[when_body.last]
              when_token.span.cover(last_expr.span)
            else
              when_token.span
            end

            when_branches << WhenBranch.new(conditions, when_body, when_span)
          end

          # Phase PERCENT_LITERALS: Parse `in` branches (pattern matching)
          # Same structure as `when`, but keyword is `in`
          # Check if we have any `in` branches after when branches
            in_branches_array = [] of WhenBranch

            loop do
              consume_newlines  # skip newlines before in
              token = current_token
              break unless token.kind == Token::Kind::In

              in_token = token
              advance
              skip_trivia

              # Parse in pattern (same as when condition for parser)
              # Type checker will handle pattern matching semantics
              patterns = [] of ExprId
              loop do
                pattern = parse_expression(0)
                return PREFIX_ERROR if pattern.invalid?
                patterns << pattern

                skip_trivia
                break unless current_token.kind == Token::Kind::Comma
                advance  # consume comma
                consume_newlines
              end

              skip_trivia

              # Optional "then" keyword
              if current_token.kind == Token::Kind::Then
                advance
              end

              consume_newlines

              # Parse in body
              in_body = [] of ExprId
              loop do
                skip_trivia
                token = current_token
                break if token.kind.in?(Token::Kind::In, Token::Kind::Else, Token::Kind::End, Token::Kind::EOF)

                stmt = parse_statement
                in_body << stmt unless stmt.invalid?
                consume_newlines
              end

              # Capture in span
              in_span = if in_body.size > 0
                last_expr = @arena[in_body.last]
                in_token.span.cover(last_expr.span)
              else
                in_token.span
              end

              in_branches_array << WhenBranch.new(patterns, in_body, in_span)
            end

            in_branches = in_branches_array unless in_branches_array.empty?

          # Parse optional else body
          else_body = nil
          token = current_token
          if token.kind == Token::Kind::Else
            advance
            consume_newlines

            else_body = [] of ExprId
            loop do
              skip_trivia
              token = current_token
              break if token.kind == Token::Kind::End
              break if token.kind == Token::Kind::EOF

              expr = parse_statement
              else_body << expr unless expr.invalid?
              consume_newlines
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

          @arena.add_typed(
            CaseNode.new(
              case_span,
              value,
              when_branches,
              else_body,
              in_branches  # Phase PERCENT_LITERALS: pattern matching branches
            )
          )
        end

        # Phase 90A: Parse select (concurrent channel operation selection)
        # Grammar: select when condition body... [else body...] end
        # Note: Unlike case, select has NO value expression after 'select' keyword
        private def parse_select : ExprId
          select_token = current_token
          advance
          consume_newlines

          # Parse when branches
          select_branches = [] of SelectBranch
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
            when_body = [] of ExprId
            loop do
              skip_trivia
              token = current_token
              break if token.kind.in?(Token::Kind::When, Token::Kind::Else, Token::Kind::End, Token::Kind::EOF)

              stmt = parse_statement
              when_body << stmt unless stmt.invalid?
              consume_newlines
            end

            # Capture when span
            when_span = if when_body.size > 0
              last_expr = @arena[when_body.last]
              when_token.span.cover(last_expr.span)
            else
              when_token.span
            end

            select_branches << SelectBranch.new(condition, when_body, when_span)
          end

          # Parse optional else body (non-blocking fallback)
          else_body = nil
          token = current_token
          if token.kind == Token::Kind::Else
            advance
            consume_newlines

            else_body = [] of ExprId
            loop do
              skip_trivia
              token = current_token
              break if token.kind == Token::Kind::End
              break if token.kind == Token::Kind::EOF

              expr = parse_statement
              else_body << expr unless expr.invalid?
              consume_newlines
            end
          end

          expect_identifier("end")
          end_token = previous_token
          consume_newlines

          select_span = if end_token
            select_token.span.cover(end_token.span)
          else
            select_token.span
          end

          @arena.add_typed(
            SelectNode.new(
              select_span,
              select_branches,
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
          body_ids = [] of ExprId
          loop do
            skip_trivia
            token = current_token
            break if token.kind == Token::Kind::End
            break if token.kind == Token::Kind::EOF

            expr = parse_statement
            body_ids << expr unless expr.invalid?
            consume_newlines
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
              body_ids
            )
          )
        end

        # Phase 83: infinite loop (loop do...end)
        private def parse_loop : ExprId
          loop_token = current_token
          advance
          skip_trivia

          # Expect "do" keyword
          token = current_token
          unless token.kind == Token::Kind::Do
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end
          advance
          consume_newlines

          # Parse body
          body_ids = [] of ExprId
          loop do
            skip_trivia
            token = current_token
            break if token.kind == Token::Kind::End
            break if token.kind == Token::Kind::EOF

            expr = parse_statement
            body_ids << expr unless expr.invalid?
            consume_newlines
          end

          expect_identifier("end")
          end_token = previous_token
          consume_newlines

          loop_span = if end_token
            loop_token.span.cover(end_token.span)
          else
            loop_token.span
          end

          @arena.add_typed(
            LoopNode.new(
              loop_span,
              body_ids
            )
          )
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
            body_ids = [] of ExprId
            loop do
              skip_trivia
              token = current_token
              break if token.kind == Token::Kind::End
              break if token.kind == Token::Kind::EOF

              expr = parse_statement
              body_ids << expr unless expr.invalid?
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
                body_ids
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
          body_ids = [] of ExprId
          loop do
            skip_trivia
            token = current_token
            break if token.kind == Token::Kind::End
            break if token.kind == Token::Kind::EOF

            expr = parse_statement
            body_ids << expr unless expr.invalid?
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
              body_ids
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
          body_ids = [] of ExprId
          loop do
            skip_trivia
            token = current_token
            break if token.kind == Token::Kind::End
            break if token.kind == Token::Kind::EOF

            expr = parse_statement
            body_ids << expr unless expr.invalid?
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
              body_ids
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

          # Parse main body
          body_ids = [] of ExprId
          loop do
            skip_trivia
            token = current_token
            break if token.kind == Token::Kind::Rescue || token.kind == Token::Kind::Ensure || token.kind == Token::Kind::End
            break if token.kind == Token::Kind::EOF

            expr = parse_statement
            body_ids << expr unless expr.invalid?
            consume_newlines
          end

          # Parse rescue clauses (Phase 29)
          rescue_clauses = nil
          while current_token.kind == Token::Kind::Rescue
            rescue_clauses ||= [] of RescueClause
            rescue_start = current_token
            advance  # consume 'rescue'
            skip_trivia

            # Optional: exception type and variable binding
            # rescue SomeError => e
            # rescue => e
            # rescue SomeError
            # rescue
            exception_type : Slice(UInt8)? = nil
            variable_name : Slice(UInt8)? = nil

            token = current_token
            # Check if we have an exception type (identifier before => or newline)
            if token.kind == Token::Kind::Identifier
              exception_type = token.slice
              advance
              skip_trivia
              token = current_token
            end

            # Check for => variable binding
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

            # Parse rescue body
            rescue_body = [] of ExprId
            loop do
              skip_trivia
              token = current_token
              break if token.kind == Token::Kind::Rescue || token.kind == Token::Kind::Ensure || token.kind == Token::Kind::End
              break if token.kind == Token::Kind::EOF

              expr = parse_statement
              rescue_body << expr unless expr.invalid?
              consume_newlines
            end

            rescue_span = rescue_start.span
            rescue_clauses << RescueClause.new(exception_type, variable_name, rescue_body, rescue_span)
          end

          # Parse ensure clause (Phase 29)
          ensure_body = nil
          if current_token.kind == Token::Kind::Ensure
            advance  # consume 'ensure'
            consume_newlines

            ensure_body = [] of ExprId
            loop do
              skip_trivia
              token = current_token
              break if token.kind == Token::Kind::End
              break if token.kind == Token::Kind::EOF

              expr = parse_statement
              ensure_body << expr unless expr.invalid?
              consume_newlines
            end
          end

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
                Bytes.new("raise".to_unsafe, 5)
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
            # Method call without parentheses: raise "msg", arg1, arg2
            # Create identifier node for "raise"
            raise_id = @arena.add_typed(
              IdentifierNode.new(
                raise_token.span,
                Bytes.new("raise".to_unsafe, 5)
              )
            )

            # Try to parse as call without parentheses
            call_result = try_parse_call_args_without_parens(raise_id)

            if call_result == PREFIX_ERROR
              # Failed to parse as call, maybe it's raise + expression
              value = parse_expression(0)
              return PREFIX_ERROR if value.invalid?

              value_span = node_span(value)
              raise_span = raise_token.span.cover(value_span)

              @arena.add_typed(
                RaiseNode.new(
                  raise_span,
                  value
                )
              )
            else
              # Successfully parsed as call
              call_result
            end
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
          body_ids = [] of ExprId
          loop do
            skip_trivia
            token = current_token
            break if token.kind == Token::Kind::End
            break if token.kind == Token::Kind::EOF

            expr = parse_statement
            body_ids << expr unless expr.invalid?
            consume_newlines
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
              body_ids
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
          if token.kind.in?(Token::Kind::Newline, Token::Kind::EOF, Token::Kind::End, Token::Kind::Else, Token::Kind::Elsif, Token::Kind::If)
            # Return without value (implicit nil)
            @arena.add_typed(ReturnNode.new(return_token.span, nil))
          else
            # Return with value
            value = parse_expression(0)
            return PREFIX_ERROR if value.invalid?

            value_span = node_span(value)
            return_span = return_token.span.cover(value_span)

            @arena.add_typed(ReturnNode.new(return_span, value))
          end
        end

        # Phase 12: Parse break expression
        # Grammar: break [value]
        private def parse_break : ExprId
          break_token = current_token
          advance
          skip_trivia

          # Check if there's a break value
          # break without value if: newline, EOF, end, else, elsif, if (for postfix)
          token = current_token
          if token.kind.in?(Token::Kind::Newline, Token::Kind::EOF, Token::Kind::End, Token::Kind::Else, Token::Kind::Elsif, Token::Kind::If)
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
        # Grammar: yield [arg1, arg2, ...]
        private def parse_yield : ExprId
          yield_token = current_token
          advance
          skip_trivia

          # Check if there are yield arguments
          # yield without args if: newline, EOF, end, else, elsif, if (for postfix), do, }
          token = current_token
          if token.kind.in?(Token::Kind::Newline, Token::Kind::EOF, Token::Kind::End, Token::Kind::Else, Token::Kind::Elsif, Token::Kind::If, Token::Kind::Do, Token::Kind::RBrace)
            # Yield without args
            @arena.add_typed(YieldNode.new(yield_token.span, nil))
          else
            # Yield with args - parse comma-separated expressions
            args = [] of ExprId
            loop do
              arg = parse_expression(0)
              return PREFIX_ERROR if arg.invalid?
              args << arg

              skip_trivia
              break if current_token.kind != Token::Kind::Comma

              advance  # consume comma
              skip_trivia
            end

            last_arg_span = node_span(args.last)
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

            args = [] of ExprId

            # Check for empty parens: super()
            if current_token.kind == Token::Kind::RParen
              rparen_token = current_token
              advance  # consume )
              return @arena.add_typed(
                SuperNode.new(
                  super_token.span.cover(rparen_token.span),
                  args  # Empty array = explicit no args
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
              args << arg

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
                args
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

            args = [] of ExprId

            # Check for empty parens: previous_def()
            if current_token.kind == Token::Kind::RParen
              rparen_token = current_token
              advance  # consume )
              return @arena.add_typed(
                PreviousDefNode.new(
                  previous_def_token.span.cover(rparen_token.span),
                  args  # Empty array = explicit no args
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
              args << arg

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
                args
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

          args = [] of ExprId

          # Parse at least one argument
          loop do
            arg = parse_expression(0)
            return PREFIX_ERROR if arg.invalid?
            args << arg

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
              args
            )
          )
        end

        # Phase 41: Parse sizeof (size in bytes)
        # Grammar: sizeof(expr) | sizeof(expr1, expr2, ...)
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

          args = [] of ExprId

          # Parse at least one argument
          loop do
            arg = parse_expression(0)
            return PREFIX_ERROR if arg.invalid?
            args << arg

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
              args
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

          args = [] of ExprId

          # Parse at least one argument
          loop do
            arg = parse_expression(0)
            return PREFIX_ERROR if arg.invalid?
            args << arg

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
              args
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

          args = [] of ExprId

          # Parse at least one argument
          loop do
            arg = parse_expression(0)
            return PREFIX_ERROR if arg.invalid?
            args << arg

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
              args
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

          args = [] of ExprId

          # Parse at least one argument
          loop do
            arg = parse_expression(0)
            return PREFIX_ERROR if arg.invalid?
            args << arg

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
              args
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

          args = [] of ExprId

          # Parse at least one argument (template string)
          loop do
            arg = parse_expression(0)
            return PREFIX_ERROR if arg.invalid?
            args << arg

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
              args
            )
          )
        end

        # Phase 98: Parse out keyword (C bindings output parameter)
        # Grammar: out identifier
        private def parse_out : ExprId
          out_token = current_token
          advance
          skip_trivia

          # Expect identifier
          unless current_token.kind == Token::Kind::Identifier
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end

          identifier_token = current_token
          advance

          @arena.add_typed(
            OutNode.new(
              out_token.span.cover(identifier_token.span),
              identifier_token.slice
            )
          )
        end

        # Phase 10: Parse block
        # Grammar: { |params| body } or do |params| body end
        private def parse_block : ExprId
          is_brace_form = current_token.kind == Token::Kind::LBrace
          start_token = current_token
          advance  # consume { or do
          skip_trivia

          # Parse optional block parameters: |x, y|
          params = [] of Parameter
          if current_token.kind == Token::Kind::Pipe
            advance  # consume opening |
            skip_trivia

            # Parse parameter list
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

              # TODO: Support type annotations in block params
              # For now, block params only have name (no type annotation)
              params << Parameter.new(
                param_name,
                nil,              # no type annotation
                nil,              # no default value
                param_span,       # full span = name span for now
                param_name_span,  # name span
                nil,              # no type span
                nil               # no default span
              )

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
          body = [] of ExprId
          loop do
            skip_trivia

            # Skip newlines in block body
            while current_token.kind == Token::Kind::Newline
              advance
              skip_trivia
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
            body << stmt
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
            params,
            body
          ))
        end

        # Phase 74: Parse proc literal: ->(x : Int32) : Int32 { x + 1 }
        private def parse_proc_literal : ExprId
          arrow_token = current_token
          advance  # consume ->
          skip_trivia

          # Parse optional parameters: (x : Type, y : Type)
          params = [] of Parameter
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

                  type_token = current_token
                  if type_token.kind == Token::Kind::Identifier
                    type_annotation = type_token.slice  # TIER 2.1: Zero-copy slice
                    type_span = type_token.span
                    param_span = param_span.cover(type_span)
                    advance
                    skip_trivia
                  else
                    emit_unexpected(type_token)
                    return PREFIX_ERROR
                  end
                end

                params << Parameter.new(
                  param_name,
                  type_annotation,
                  nil,              # no default value
                  param_span,
                  param_name_span,
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

            return_type_token = current_token
            if return_type_token.kind == Token::Kind::Identifier
              return_type = return_type_token.slice
              advance
              skip_trivia
            else
              emit_unexpected(return_type_token)
              return PREFIX_ERROR
            end
          end

          # Parse body: { } or do...end
          is_brace_form = current_token.kind == Token::Kind::LBrace
          is_do_form = current_token.kind == Token::Kind::Do

          unless is_brace_form || is_do_form
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end

          start_token = current_token
          advance  # consume { or do
          skip_trivia

          # Parse proc body
          body = [] of ExprId
          loop do
            skip_trivia

            # Skip newlines in proc body
            while current_token.kind == Token::Kind::Newline
              advance
              skip_trivia
            end

            # Check for proc terminator
            if is_brace_form
              break if current_token.kind == Token::Kind::RBrace
            else
              break if current_token.kind == Token::Kind::End
            end

            break if current_token.kind == Token::Kind::EOF

            stmt = parse_statement
            return PREFIX_ERROR if stmt.invalid?
            body << stmt
          end

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
            params,
            return_type,
            body
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
                [] of ExprId,
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
                [] of ExprId,
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
            condition = parse_expression(0)
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
                [] of ExprId
              )
            )
          end

          # Phase 26: Check for postfix unless
          if token.kind == Token::Kind::Unless
            advance  # consume 'unless'
            skip_trivia

            # Parse condition
            condition = parse_expression(0)
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
                [] of ExprId
              )
            )
          end

          # Phase 27: Check for postfix while
          if token.kind == Token::Kind::While
            advance  # consume 'while'
            skip_trivia

            # Parse condition
            condition = parse_expression(0)
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
            condition = parse_expression(0)
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

          consume_newlines

          body_ids = [] of ExprId
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
                  parse_annotation
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
              # Phase 5B: Use parse_statement for assignments
              expr = parse_statement
            end
            body_ids << expr unless expr.invalid?
            consume_newlines
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
              body_ids,
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

        # Phase 37: Parse private visibility modifier
        # Grammar: private def method_name
        private def parse_private : ExprId
          private_token = current_token
          advance
          skip_trivia

          # Currently only support private methods
          case current_token.kind
          when Token::Kind::Def
            parse_def(visibility: Visibility::Private)
          else
            emit_unexpected(current_token)
            PREFIX_ERROR
          end
        end

        # Phase 37: Parse protected visibility modifier
        # Grammar: protected def method_name
        private def parse_protected : ExprId
          protected_token = current_token
          advance
          skip_trivia

          # Currently only support protected methods
          case current_token.kind
          when Token::Kind::Def
            parse_def(visibility: Visibility::Protected)
          else
            emit_unexpected(current_token)
            PREFIX_ERROR
          end
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

          body_ids = [] of ExprId
          loop do
            skip_trivia
            token = current_token
            break if token.kind == Token::Kind::End
            break if token.kind == Token::Kind::EOF

            if definition_start?
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
                  parse_annotation
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
            body_ids << expr unless expr.invalid?
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
              body_ids
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

          # Parse optional base type: : Type
          base_type_token = nil
          if current_token.kind == Token::Kind::Colon
            advance  # consume ':'
            skip_trivia

            base_type_token = current_token
            unless base_type_token.kind == Token::Kind::Identifier
              emit_unexpected(base_type_token)
              return PREFIX_ERROR
            end
            advance
            skip_trivia
          end

          consume_newlines

          # Parse enum members
          members = [] of EnumMember
          loop do
            skip_trivia
            token = current_token
            break if token.kind == Token::Kind::End
            break if token.kind == Token::Kind::EOF

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
              base_type_token.try(&.slice),
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

          # Parse type (for now, just identifier - can be expanded later for generic types)
          type_token = current_token
          unless type_token.kind == Token::Kind::Identifier
            emit_unexpected(type_token)
            return PREFIX_ERROR
          end
          advance

          alias_span = alias_token.span.cover(type_token.span)

          @arena.add_typed(
            AliasNode.new(
              alias_span,
              name_token.slice,
              type_token.slice
            )
          )
        end

        # Phase 92: Parse annotation definition
        # Grammar: annotation Name [; body...] end
        # Phase 92A: Parser only - body ignored/empty
        private def parse_annotation : ExprId
          annotation_token = current_token
          advance
          skip_trivia

          # Parse annotation name
          name_token = current_token
          unless name_token.kind == Token::Kind::Identifier
            emit_unexpected(name_token)
            return PREFIX_ERROR
          end
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

          @arena.add_typed(
            AnnotationNode.new(
              annotation_span,
              name_token.slice
            )
          )
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

          body_ids = [] of ExprId
          loop do
            skip_trivia
            token = current_token
            break if token.kind == Token::Kind::End
            break if token.kind == Token::Kind::EOF

            if definition_start?
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
                  parse_annotation
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
            body_ids << expr unless expr.invalid?
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
              body_ids,
              type_params
            )
          )
        end

        # Phase 31: Parse include statement
        # Grammar: include ModuleName
        private def parse_include : ExprId
          include_token = current_token
          advance
          skip_trivia

          name_token = current_token
          unless name_token.kind == Token::Kind::Identifier
            emit_unexpected(name_token)
            return PREFIX_ERROR
          end
          advance

          include_span = include_token.span.cover(name_token.span)

          @arena.add_typed(
            IncludeNode.new(
              include_span,
              name_token.slice
            )
          )
        end

        # Phase 31: Parse extend statement
        # Grammar: extend ModuleName
        private def parse_extend : ExprId
          extend_token = current_token
          advance
          skip_trivia

          name_token = current_token
          unless name_token.kind == Token::Kind::Identifier
            emit_unexpected(name_token)
            return PREFIX_ERROR
          end
          advance

          extend_span = extend_token.span.cover(name_token.span)

          @arena.add_typed(
            ExtendNode.new(
              extend_span,
              name_token.slice
            )
          )
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

          # Expect type identifier
          type_token = current_token
          unless type_token.kind == Token::Kind::Identifier
            emit_unexpected(type_token)
            return PREFIX_ERROR
          end
          advance  # consume type

          decl_span = ivar_token.span.cover(type_token.span)

          @arena.add_typed(InstanceVarDeclNode.new(
            decl_span,
            ivar_token.slice,
            type_token.slice
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

          # Expect type identifier
          type_token = current_token
          unless type_token.kind == Token::Kind::Identifier
            emit_unexpected(type_token)
            return PREFIX_ERROR
          end
          advance  # consume type

          decl_span = cvar_token.span.cover(type_token.span)

          @arena.add_typed(ClassVarDeclNode.new(
            decl_span,
            cvar_token.slice,
            type_token.slice
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

          # Expect type identifier
          type_token = current_token
          unless type_token.kind == Token::Kind::Identifier
            emit_unexpected(type_token)
            return PREFIX_ERROR
          end
          advance  # consume type

          decl_span = gvar_token.span.cover(type_token.span)

          @arena.add_typed(
            GlobalVarDeclNode.new(
              decl_span,
              gvar_token.slice,        # $var
              type_token.slice  # Type
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
          pieces = [] of MacroPiece
          buffer = IO::Memory.new
          buffer_start_token : Token? = nil
          control_depth = 0
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

            if control_depth == 0 && token.kind == Token::Kind::End
              break
            end

            if macro_control_start?
              left_trim = macro_control_left_trim?
              already_empty = pieces.empty?
              trim_applied = left_trim
              flush_macro_text(buffer, pieces, trim_applied, buffer_start_token, previous_token)
              buffer_start_token = nil
              macro_trim_left ||= already_empty && trim_applied

              piece, effect, skip_whitespace = parse_macro_control_piece
              pieces << piece

              # Special handling for comment blocks - skip content
              if piece.control_keyword == "comment" && effect == :push
                comment_depth = 1
                # Skip tokens until matching {% end %}
                loop do
                  break if comment_depth == 0
                  break if current_token.kind == Token::Kind::EOF

                  if macro_control_start?
                    inner_piece, inner_effect, _ = parse_macro_control_piece
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
            end

            # Track start of text buffer
            if buffer_start_token.nil? && buffer.size == 0
              buffer_start_token = token
            end

            buffer.write(token.slice)
            advance
          end

          flush_macro_text(buffer, pieces, trim_final, buffer_start_token, previous_token)
          {
            pieces,
            macro_trim_left || pieces.first?.try(&.trim_left) || false,
            macro_trim_right || pieces.last?.try(&.trim_right) || false,
          }
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

          # Expect identifier (method name)
          method_token = current_token
          unless method_token.kind == Token::Kind::Identifier
            emit_unexpected(method_token)
            return PREFIX_ERROR
          end
          advance

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
                   left_kind == Frontend::NodeKind::Index ||
                   left_kind == Frontend::NodeKind::MemberAccess
              @diagnostics << Diagnostic.new("Assignment target must be an identifier, instance variable, class variable, global variable, or index expression", token.span)
              return PREFIX_ERROR
            end

            # Consume assignment token
            assign_token = token
            is_compound = assign_token.kind != Token::Kind::Eq
            advance
            skip_trivia

            # Parse right-hand side
            rhs = parse_op_assign  # Recursive for chained assignments
            return PREFIX_ERROR if rhs.invalid?

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
              when Token::Kind::NilCoalesceEq then "||"  # Special case
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
          case current_token.kind
          when Token::Kind::End, Token::Kind::Else, Token::Kind::Elsif,
               Token::Kind::When, Token::Kind::In, Token::Kind::Then,
               Token::Kind::Rescue, Token::Kind::Ensure
            return PREFIX_ERROR
          when Token::Kind::Newline, Token::Kind::EOF
            return PREFIX_ERROR
          # Don't parse as call if followed by binary/logical operators that can't start an argument
          when Token::Kind::OrOr, Token::Kind::AndAnd,
               Token::Kind::Question,  # ternary operator
               Token::Kind::Arrow,     # hash arrow =>
               # Comparison operators
               Token::Kind::EqEq, Token::Kind::NotEq,
               Token::Kind::Less, Token::Kind::Greater,
               Token::Kind::LessEq, Token::Kind::GreaterEq,
               Token::Kind::Spaceship,  # <=>
               # Other binary operators that can't be prefix
               Token::Kind::Pipe, Token::Kind::Caret, Token::Kind::Amp,
               Token::Kind::LShift, Token::Kind::RShift,
               Token::Kind::DotDot, Token::Kind::DotDotDot,
               Token::Kind::Match, Token::Kind::NotMatch  # =~, !~
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
          args = [] of ExprId
          named_args = [] of NamedArgument

          loop do
            # Parse one argument
            # Could be: positional arg, assignment as arg, or named arg
            arg = parse_op_assign
            if arg.invalid?
              @parsing_call_args -= 1
              return PREFIX_ERROR
            end

            skip_trivia

            # Check if this is a named argument: identifier followed by colon
            # Pattern: name: value
            arg_node = @arena[arg]
            if Frontend.node_kind(arg_node) == Frontend::NodeKind::Identifier &&
               current_token.kind == Token::Kind::Colon
              # This is named argument!
              name_span = arg_node.span
              name = String.new(Frontend.node_literal(arg_node).not_nil!)

              advance  # consume ':'
              skip_trivia

              # Parse value (using parse_op_assign like original Crystal)
              value_expr = parse_op_assign
              if value_expr.invalid?
                @parsing_call_args -= 1
                return PREFIX_ERROR
              end

              value_span = @arena[value_expr].span
              arg_span = name_span.cover(value_span)

              named_args << NamedArgument.new(name, value_expr, arg_span, name_span, value_span)
              skip_trivia
            else
              # Positional argument
              args << arg
            end

            # Check for comma (more arguments)
            if current_token.kind == Token::Kind::Comma
              advance  # consume comma
              skip_trivia
            else
              # No more arguments
              break
            end
          end

          # Parse optional block: do...end or {...}
          # Example: record Point, x : Int32 do ... end
          skip_trivia
          block_expr : ExprId? = nil
          if current_token.kind == Token::Kind::Do || current_token.kind == Token::Kind::LBrace
            block_expr = parse_block
            if block_expr.invalid?
              @parsing_call_args -= 1
              return PREFIX_ERROR
            end
          end

          # Create CallNode
          callee = @arena.add_typed(IdentifierNode.new(callee_token.span, callee_token.slice))

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
          skip_trivia
          left = parse_prefix
          debug("parse_expression(#{precedence}): after parse_prefix, left=#{left.invalid? ? "invalid" : "valid"}")
          return PREFIX_ERROR if left.invalid?

          loop do
            skip_trivia
            token = current_token
            debug("parse_expression(#{precedence}): postfix loop, token=#{token.kind}")
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
              left = attach_block_to_call(left)
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
                  @index -= 1
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

            unless infix?(token)
              debug("parse_expression(#{precedence}): token #{token.kind} is not infix, breaking")
              break
            end
            debug("parse_expression(#{precedence}): token #{token.kind} is infix")
            current_precedence = precedence_for(token)
            break if current_precedence < precedence

            advance
            # Disable type declarations for ternary true_branch (identifier: would conflict)
            if token.kind == Token::Kind::Question
              @no_type_declaration += 1
              right = parse_expression(current_precedence + 1)
              @no_type_declaration -= 1
            else
              right = parse_expression(current_precedence + 1)
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
          when Token::Kind::If
            parse_if
          when Token::Kind::Unless
            # Phase 24: unless condition
            parse_unless
          when Token::Kind::Case
            # Phase 11: case/when pattern matching
            parse_case
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
            advance  # Move past identifier

            # Phase CALLS_WITHOUT_PARENS: Check for whitespace before skip_trivia
            # This allows us to parse calls like: foo arg1, arg2
            space_consumed = current_token.kind == Token::Kind::Whitespace
            skip_trivia

            # Check if uppercase identifier followed by (
            if identifier_token.slice.size > 0 &&
               identifier_token.slice[0] >= 'A'.ord && identifier_token.slice[0] <= 'Z'.ord &&
               current_token.kind == Token::Kind::LParen
              # This is generic instantiation: Box(Int32)
              parse_generic_instantiation(identifier_token)
            # Phase 103: Check for type annotation (if enabled)
            elsif @no_type_declaration == 0 && current_token.kind == Token::Kind::Colon
              # This is type declaration: x : Type = value
              parse_type_declaration_from_identifier(identifier_token)
            # Phase CALLS_WITHOUT_PARENS: Try to parse call arguments if space was consumed
            # Don't try to parse nested calls when already parsing call arguments
            elsif space_consumed && @parsing_call_args == 0
              # Attempt to parse call arguments without parentheses
              # Example: def_equals value, kind
              maybe_call = try_parse_call_args_without_parens(identifier_token)
              if maybe_call.invalid?
                # Not a call, just an identifier
                @arena.add_typed(IdentifierNode.new(identifier_token.span, identifier_token.slice))
              else
                maybe_call
              end
            else
              # Regular identifier
              @arena.add_typed(IdentifierNode.new(identifier_token.span, identifier_token.slice))
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
            id = @arena.add_typed(StringNode.new(token.span, token.slice))
            advance
            id
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
            # Phase 9: Array literal
            parse_array_literal
          when Token::Kind::LBrace
            # Phase 14/15: Hash or Tuple literal (disambiguated by presence of =>)
            parse_hash_or_tuple
          when Token::Kind::ThinArrow
            # Phase 74: Proc literal (->(x) { ... })
            parse_proc_literal
          when Token::Kind::Operator
            # Phase IMPLICIT_RECEIVER: Handle implicit receiver (.method)
            # Generic fallback for unhandled operators (e.g., macro operators)
            if slice_eq?(token.slice, ".")
              # Implicit receiver: .method → ImplicitObj.method
              parse_implicit_receiver_call(token)
            elsif slice_eq?(token.slice, "(")
              parse_grouping
            else
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

        private def parse_grouping : ExprId
          lparen = current_token
          advance
          expr = parse_expression(0)
          return PREFIX_ERROR if expr.invalid?
          expect_operator(Token::Kind::RParen)
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

          elements = [] of ExprId
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
              elements,
              of_type_expr
            ))
          end

          # Parse array elements
          loop do
            element = parse_expression(0)
            if element.invalid?
              return PREFIX_ERROR
            end
            elements << element

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
          if current_token.kind == Token::Kind::Of
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
            elements,
            of_type_expr
          ))
        end

        # Phase 14: Parse hash literal {"key" => value} or {} of K => V
        private def parse_hash_literal : ExprId
          lbrace = current_token
          advance  # consume {
          skip_trivia

          entries = [] of HashEntry
          of_key_type : Slice(UInt8)? = nil
          of_value_type : Slice(UInt8)? = nil

          # Check for closing brace (empty hash)
          if current_token.kind == Token::Kind::RBrace
            advance  # consume }
            skip_trivia

            # Check for "of K => V" syntax
            if current_token.kind == Token::Kind::Identifier && slice_eq?(current_token.slice, "of")
              advance
              skip_trivia

              # Parse key type
              key_type_token = current_token
              if key_type_token.kind == Token::Kind::Identifier
                of_key_type = key_type_token.slice
                advance
                skip_trivia

                # Expect =>
                unless current_token.kind == Token::Kind::Arrow
                  emit_unexpected(current_token)
                  return PREFIX_ERROR
                end
                advance  # consume =>
                skip_trivia

                # Parse value type
                value_type_token = current_token
                if value_type_token.kind == Token::Kind::Identifier
                  of_value_type = value_type_token.slice
                  advance
                else
                  emit_unexpected(value_type_token)
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
              return PREFIX_ERROR
            end
            key_span = node_span(key)

            skip_trivia

            # Expect =>
            unless current_token.kind == Token::Kind::Arrow
              emit_unexpected(current_token)
              return PREFIX_ERROR
            end
            arrow_token = current_token
            advance  # consume =>
            skip_trivia

            # Parse value
            value = parse_expression(0)
            if value.invalid?
              return PREFIX_ERROR
            end
            value_span = node_span(value)

            # Create entry with precise spans for LSP/diagnostics
            entry_span = key_span.cover(value_span)
            entries << HashEntry.new(key, value, entry_span, arrow_token.span)

            skip_trivia
            break if !(current_token.kind == Token::Kind::Comma)

            advance  # consume comma
            skip_trivia

            # Allow trailing comma
            if current_token.kind == Token::Kind::RBrace
              break
            end
          end

          # Expect closing brace
          unless current_token.kind == Token::Kind::RBrace
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end

          closing_brace = current_token
          advance

          hash_span = lbrace.span.cover(closing_brace.span)
          @arena.add_typed(HashLiteralNode.new(
            hash_span,
            entries,
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
          skip_trivia

          # Empty {} → hash
          if current_token.kind == Token::Kind::RBrace
            # Empty hash - delegate to parse_hash_literal
            return parse_hash_literal_from_lbrace(lbrace)
          end

          # Parse first element (key for hash/named tuple, value for tuple)
          # Disable type declarations inside {} to allow identifier: syntax for named tuples
          @no_type_declaration += 1
          first_elem = parse_expression(0)
          @no_type_declaration -= 1
          return PREFIX_ERROR if first_elem.invalid?
          skip_trivia

          # Check what follows
          case current_token.kind
          when Token::Kind::Arrow
            # "=>" → this is a hash
            return parse_hash_literal_continued(lbrace, first_elem)
          when Token::Kind::Colon
            # ":" → check if first_elem is identifier for named tuple
            first_node = @arena[first_elem]
            if Frontend.node_kind(first_node) == Frontend::NodeKind::Identifier
              # identifier: value → named tuple
              return parse_named_tuple_literal_continued(lbrace, first_elem)
            else
              # non-identifier: value → error
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
              skip_trivia

              # Allow trailing comma
              if current_token.kind == Token::Kind::RBrace
                break
              end

              # Parse next element
              elem = parse_expression(0)
              return PREFIX_ERROR if elem.invalid?
              elements << elem
              skip_trivia
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

          tuple_span = lbrace.span.cover(closing_brace.span)
          @arena.add_typed(TupleLiteralNode.new(
            tuple_span,
            elements
          ))
        end

        # Phase 70: Continue parsing named tuple literal after first key
        private def parse_named_tuple_literal_continued(lbrace : Token, first_key_expr : ExprId) : ExprId
          entries = [] of NamedTupleEntry

          # Get first key from first_key_expr (we know it's Identifier)
          first_key_node = @arena[first_key_expr]
          first_key = Frontend.node_literal(first_key_node).not_nil!  # TIER 2.3: Already Slice(UInt8)
          first_key_span = first_key_node.span

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
          first_entry_span = first_key_span.cover(first_value_span)
          entries << NamedTupleEntry.new(
            first_key,
            first_value,
            first_entry_span,
            first_key_span,
            first_value_span
          )

          # Parse remaining entries
          loop do
            case current_token.kind
            when Token::Kind::RBrace
              # End of named tuple
              break
            when Token::Kind::Comma
              advance  # consume comma
              skip_trivia

              # Allow trailing comma
              if current_token.kind == Token::Kind::RBrace
                break
              end

              # Parse key (must be identifier)
              key_token = current_token
              unless key_token.kind == Token::Kind::Identifier
                emit_unexpected(key_token)
                return PREFIX_ERROR
              end
              key = key_token.slice  # TIER 2.3: Zero-copy slice
              key_span = key_token.span
              advance
              skip_trivia

              # Expect colon
              unless current_token.kind == Token::Kind::Colon
                emit_unexpected(current_token)
                return PREFIX_ERROR
              end
              advance  # consume :
              skip_trivia

              # Parse value
              value = parse_expression(0)
              return PREFIX_ERROR if value.invalid?
              value_span = @arena[value].span
              skip_trivia

              # Create entry
              entry_span = key_span.cover(value_span)
              entries << NamedTupleEntry.new(
                key,
                value,
                entry_span,
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
            entries
          ))
        end

        # Phase 14: Parse empty hash literal
        private def parse_hash_literal_from_lbrace(lbrace : Token) : ExprId
          # Current token is RBrace
          advance  # consume }
          skip_trivia

          entries = [] of HashEntry
          of_key_type : Slice(UInt8)? = nil
          of_value_type : Slice(UInt8)? = nil

          # Check for "of K => V" syntax
          if current_token.kind == Token::Kind::Identifier && slice_eq?(current_token.slice, "of")
            advance
            skip_trivia

            # Parse key type
            key_type_token = current_token
            if key_type_token.kind == Token::Kind::Identifier
              of_key_type = key_type_token.slice
              advance
              skip_trivia

              # Expect =>
              unless current_token.kind == Token::Kind::Arrow
                emit_unexpected(current_token)
                return PREFIX_ERROR
              end
              advance  # consume =>
              skip_trivia

              # Parse value type
              value_type_token = current_token
              if value_type_token.kind == Token::Kind::Identifier
                of_value_type = value_type_token.slice
                advance
              else
                emit_unexpected(value_type_token)
                return PREFIX_ERROR
              end
            else
              emit_unexpected(key_type_token)
              return PREFIX_ERROR
            end
          end

          # Use lbrace span as start, current as end (after "of K => V" if present)
          closing_span = lbrace.span.cover(lbrace.span)  # Minimal span for now
          @arena.add_typed(HashLiteralNode.new(
            closing_span,
            entries,
            of_key_type,
            of_value_type
          ))
        end

        # Phase 14: Continue parsing hash literal after first key
        private def parse_hash_literal_continued(lbrace : Token, first_key : ExprId) : ExprId
          # Current token should be Arrow
          unless current_token.kind == Token::Kind::Arrow
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end
          arrow_token = current_token
          advance  # consume =>
          skip_trivia

          # Parse first value
          first_value = parse_expression(0)
          return PREFIX_ERROR if first_value.invalid?

          key_span = node_span(first_key)
          value_span = node_span(first_value)
          entry_span = key_span.cover(value_span)
          skip_trivia

          entries = [HashEntry.new(first_key, first_value, entry_span, arrow_token.span)]

          # Parse remaining entries
          loop do
            unless current_token.kind == Token::Kind::Comma
              break
            end

            advance  # consume comma
            skip_trivia

            # Allow trailing comma
            if current_token.kind == Token::Kind::RBrace
              break
            end

            # Parse key
            key = parse_expression(0)
            return PREFIX_ERROR if key.invalid?
            key_span = node_span(key)
            skip_trivia

            # Expect =>
            unless current_token.kind == Token::Kind::Arrow
              emit_unexpected(current_token)
              return PREFIX_ERROR
            end
            arrow_token = current_token
            advance  # consume =>
            skip_trivia

            # Parse value
            value = parse_expression(0)
            return PREFIX_ERROR if value.invalid?
            value_span = node_span(value)
            entry_span = key_span.cover(value_span)
            skip_trivia

            entries << HashEntry.new(key, value, entry_span, arrow_token.span)
          end

          # Expect closing brace
          unless current_token.kind == Token::Kind::RBrace
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end

          closing_brace = current_token
          advance

          hash_span = lbrace.span.cover(closing_brace.span)
          @arena.add_typed(HashLiteralNode.new(
            hash_span,
            entries
          ))
        end

        # Phase 8: Parse string interpolation
        # Converts "Hello, #{name}!" into StringPiece array:
        # - Text("Hello, ")
        # - Expression(name_expr_id)
        # - Text("!")
        private def parse_string_interpolation(token : Token) : ExprId
          content = String.new(token.slice)
          pieces = [] of StringPiece
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
              pieces << StringPiece.text(content[text_start...i])
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
            pieces << StringPiece.expression(expr_id)

            # Move past the closing }
            i += 1
          end

          advance
          @arena.add_typed(StringInterpolationNode.new(
            token.span,
            pieces
          ))
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

          args = [] of ExprId
          named_args = [] of NamedArgument

          # Empty call: foo()
          unless current_token.kind == Token::Kind::RParen
            loop do
              # Phase 101: Check for block shorthand (&.method)
              # This creates: { |__arg0| __arg0.method }
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
                else
                  # Not block shorthand, rewind and parse normally
                  # This handles cases like: foo(& other_expr)
                  @index -= 1  # Go back to Amp token
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

                  # Parse value expression
                  @no_type_declaration += 1
                  value_expr = parse_expression(0)
                  @no_type_declaration -= 1
                  return PREFIX_ERROR if value_expr.invalid?
                  value_span = @arena[value_expr].span

                  # Create NamedArgument
                  name = String.new(name_slice)
                  arg_span = name_span.cover(value_span)
                  named_args << NamedArgument.new(name, value_expr, arg_span, name_span, value_span)
                  skip_whitespace_and_optional_newlines
                else
                  # Parse first expression/identifier
                  # Disable type declarations to allow identifier: syntax for named args
                  @no_type_declaration += 1
                  arg_expr = parse_expression(0)
                  @no_type_declaration -= 1
                  return PREFIX_ERROR if arg_expr.invalid?
                  skip_whitespace_and_optional_newlines

                  # Check if this is named argument (identifier followed by colon)
                  # This handles case where identifier was already parsed
                  if current_token.kind == Token::Kind::Colon
                    arg_node = @arena[arg_expr]
                    if Frontend.node_kind(arg_node) == Frontend::NodeKind::Identifier
                      # Named argument: name: value
                  name = String.new(Frontend.node_literal(arg_node).not_nil!)
                  name_span = arg_node.span

                  advance  # consume ':'
                  skip_whitespace_and_optional_newlines

                  # Parse value expression
                  # Disable type declarations in value (may contain nested named tuples/args)
                  @no_type_declaration += 1
                  value_expr = parse_expression(0)
                  @no_type_declaration -= 1
                  return PREFIX_ERROR if value_expr.invalid?
                  value_span = @arena[value_expr].span

                  # Create NamedArgument
                  arg_span = name_span.cover(value_span)
                  named_args << NamedArgument.new(name, value_expr, arg_span, name_span, value_span)
                  skip_whitespace_and_optional_newlines
                else
                  # Expression followed by colon is invalid
                  emit_unexpected(current_token)
                  return PREFIX_ERROR
                end
              else
                # Positional argument
                args << arg_expr
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

          @paren_depth -= 1  # Phase 103: exiting parentheses
          expect_operator(Token::Kind::RParen)

          # Calculate span
          spans = [] of Span
          spans << lparen.span
          spans << node_span(callee)
          args.each { |arg| spans << node_span(arg) }
          named_args.each { |na| spans << na.span }
          if closing_span = previous_token.try(&.span)
            spans << closing_span
          end
          call_span = Span.cover_all(spans)

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
          indexes = [] of ExprId
          skip_whitespace_and_optional_newlines
          unless current_token.kind == Token::Kind::RBracket
            loop do
              expr = parse_expression(0)
              indexes << expr unless expr.invalid?
              skip_whitespace_and_optional_newlines
              break unless current_token.kind == Token::Kind::Comma
              advance
              skip_whitespace_and_optional_newlines
            end
          end
          @bracket_depth -= 1  # Phase 103: exiting brackets
          expect_operator(Token::Kind::RBracket)
          spans = [] of Span
          spans << lbracket.span
          spans << node_span(target)
          indexes.each { |idx| spans << node_span(idx) }
          if closing_span = previous_token.try(&.span)
            spans << closing_span
          end
          index_span = Span.cover_all(spans)
          @arena.add_typed(IndexNode.new(index_span, target, indexes))
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
            spans = [] of Span
            spans << node_span(receiver)
            spans << dot.span
            spans << member_token.span
            member_span = Span.cover_all(spans)
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
            if current_token.kind == Token::Kind::Whitespace
              advance  # Consume space
              space_consumed = true
            end

            if space_consumed && @parsing_call_args == 0
              # After consuming space, check if next token could start an argument
              # If it's a newline, don't try to parse arguments (new statement on next line)
              token = current_token
              case token.kind
              when Token::Kind::Newline, Token::Kind::EOF,
                   Token::Kind::Semicolon, Token::Kind::Then,
                   Token::Kind::End, Token::Kind::Elsif, Token::Kind::Else,
                   Token::Kind::When, Token::Kind::Rescue, Token::Kind::Ensure,
                   Token::Kind::RParen, Token::Kind::RBracket, Token::Kind::RBrace,
                   Token::Kind::Comma,
                   Token::Kind::Amp  # Block parameter (&.method)
                # These tokens indicate end of expression, not start of arguments
                node
              when Token::Kind::Eq
                # Assignment, not argument
                node
              when Token::Kind::OrOr, Token::Kind::AndAnd,
                   Token::Kind::Question,  # ternary operator
                   Token::Kind::Arrow,     # hash arrow =>
                   # Comparison operators
                   Token::Kind::EqEq, Token::Kind::NotEq,
                   Token::Kind::Less, Token::Kind::Greater,
                   Token::Kind::LessEq, Token::Kind::GreaterEq,
                   Token::Kind::Spaceship,  # <=>
                   # Other binary operators that can't be prefix
                   Token::Kind::Pipe, Token::Kind::Caret,
                   Token::Kind::LShift, Token::Kind::RShift,
                   Token::Kind::DotDot, Token::Kind::DotDotDot,
                   Token::Kind::Match, Token::Kind::NotMatch  # =~, !~
                # Binary/logical operators that can't start arguments
                node
              else
                # Try to parse arguments
                @parsing_call_args += 1

                args = [] of ExprId
                named_args = [] of NamedArgument

                loop do
                  # Parse one argument
                  arg = parse_op_assign
                  if arg.invalid?
                    @parsing_call_args -= 1
                    return PREFIX_ERROR
                  end

                  skip_trivia

                  # Check if this is a named argument
                  arg_node = @arena[arg]
                  if Frontend.node_kind(arg_node) == Frontend::NodeKind::Identifier &&
                     current_token.kind == Token::Kind::Colon
                    # Named argument!
                    name_span = arg_node.span
                    name = String.new(Frontend.node_literal(arg_node).not_nil!)

                    advance  # consume ':'
                    skip_trivia

                    value_expr = parse_op_assign
                    if value_expr.invalid?
                      @parsing_call_args -= 1
                      return PREFIX_ERROR
                    end

                    value_span = @arena[value_expr].span
                    arg_span = name_span.cover(value_span)

                    named_args << NamedArgument.new(name, value_expr, arg_span, name_span, value_span)
                    skip_trivia
                  else
                    # Positional argument
                    args << arg
                  end

                  # Check for comma
                  if current_token.kind == Token::Kind::Comma
                    advance
                    skip_trivia
                  else
                    break
                  end
                end

                # Create CallNode with MemberAccessNode as callee
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
            spans = [] of Span
            spans << node_span(receiver)
            spans << dot.span
            spans << member_token.span
            member_span = Span.cover_all(spans)
            node = @arena.add_typed(
              MemberAccessNode.new(
                member_span,
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

        # Phase 63: Parse path expression (Foo::Bar)
        private def parse_path(left : ExprId) : ExprId
          colon_colon = current_token
          advance
          skip_trivia

          # Parse right side - must be identifier
          right_token = current_token
          unless right_token.kind == Token::Kind::Identifier
            emit_unexpected(right_token)
            return left
          end

          right_id = @arena.add_typed(
            IdentifierNode.new(
              right_token.span,
              right_token.slice
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

          # Parse identifier after ::
          identifier_token = current_token
          unless identifier_token.kind == Token::Kind::Identifier
            emit_unexpected(identifier_token)
            return PREFIX_ERROR
          end

          right_id = @arena.add_typed(
            IdentifierNode.new(
              identifier_token.span,
              identifier_token.slice
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

        # Phase 44: Parse type cast (.as(Type))
        private def parse_as_cast(receiver : ExprId, dot : Token, as_token : Token) : ExprId
          advance  # Skip 'as' keyword
          skip_trivia

          # Expect opening parenthesis
          unless current_token.kind == Token::Kind::LParen
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end
          lparen = current_token
          advance
          skip_trivia

          # Parse type (for now, just identifier - can be expanded for complex types)
          type_token = current_token
          unless type_token.kind == Token::Kind::Identifier
            emit_unexpected(type_token)
            return PREFIX_ERROR
          end
          target_type = type_token.slice
          advance
          skip_trivia

          # Expect closing parenthesis
          unless current_token.kind == Token::Kind::RParen
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end
          rparen = current_token
          advance

          # Create As node
          spans = [] of Span
          spans << node_span(receiver)
          spans << dot.span
          spans << as_token.span
          spans << lparen.span
          spans << type_token.span
          spans << rparen.span
          as_span = Span.cover_all(spans)

          @arena.add_typed(
            AsNode.new(
              as_span,
              receiver,
              target_type
            )
          )
        end

        # Phase 45: Parse safe type cast (.as?(Type))
        private def parse_as_safe_cast(receiver : ExprId, dot : Token, as_question_token : Token) : ExprId
          advance  # Skip 'as?' keyword
          skip_trivia

          # Expect opening parenthesis
          unless current_token.kind == Token::Kind::LParen
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end
          lparen = current_token
          advance
          skip_trivia

          # Parse type (for now, just identifier - can be expanded for complex types)
          type_token = current_token
          unless type_token.kind == Token::Kind::Identifier
            emit_unexpected(type_token)
            return PREFIX_ERROR
          end
          target_type = type_token.slice
          advance
          skip_trivia

          # Expect closing parenthesis
          unless current_token.kind == Token::Kind::RParen
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end
          rparen = current_token
          advance

          # Create AsQuestion node
          spans = [] of Span
          spans << node_span(receiver)
          spans << dot.span
          spans << as_question_token.span
          spans << lparen.span
          spans << type_token.span
          spans << rparen.span
          as_question_span = Span.cover_all(spans)

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

          # Parse type (for now, just identifier - can be expanded for complex types)
          type_token = current_token
          unless type_token.kind == Token::Kind::Identifier
            emit_unexpected(type_token)
            return PREFIX_ERROR
          end
          target_type = type_token.slice
          advance
          skip_trivia

          # Expect closing parenthesis
          unless current_token.kind == Token::Kind::RParen
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end
          rparen = current_token
          advance

          # Create IsA node
          spans = [] of Span
          spans << node_span(receiver)
          spans << dot.span
          spans << is_a_token.span
          spans << lparen.span
          spans << type_token.span
          spans << rparen.span
          is_a_span = Span.cover_all(spans)

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

          # Expect opening parenthesis
          unless current_token.kind == Token::Kind::LParen
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end
          lparen = current_token
          advance
          skip_trivia

          # Parse method name (Symbol or String expression)
          method_name_expr = parse_expression(0)
          skip_trivia

          # Expect closing parenthesis
          unless current_token.kind == Token::Kind::RParen
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end
          rparen = current_token
          advance

          # Create RespondsTo node
          spans = [] of Span
          spans << node_span(receiver)
          spans << dot.span
          spans << responds_to_token.span
          spans << lparen.span
          spans << node_span(method_name_expr)
          spans << rparen.span
          responds_to_span = Span.cover_all(spans)

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
          # name_token is the base type name (e.g., "Box", "Array", "Hash")
          # current_token should be LParen

          lparen = current_token
          advance  # Skip '('
          skip_trivia

          # Parse type arguments (comma-separated identifiers)
          type_args = [] of ExprId

          unless current_token.kind == Token::Kind::RParen
            loop do
              # Each type argument is an identifier (for now - simple types only)
              # Future: support nested generics like Array(Box(Int32))
              if current_token.kind != Token::Kind::Identifier
                emit_unexpected(current_token)
                return PREFIX_ERROR
              end

              type_arg_token = current_token
              type_arg = @arena.add_typed(IdentifierNode.new(
                type_arg_token.span,
                type_arg_token.slice
              ))
              type_args << type_arg
              advance
              skip_trivia

              # Check for comma or closing paren
              if current_token.kind == Token::Kind::Comma
                advance  # Skip comma
                skip_trivia
              elsif current_token.kind == Token::Kind::RParen
                break  # End of arguments
              else
                emit_unexpected(current_token)
                return PREFIX_ERROR
              end
            end
          end

          # Expect closing parenthesis
          unless current_token.kind == Token::Kind::RParen
            emit_unexpected(current_token)
            return PREFIX_ERROR
          end
          rparen = current_token
          advance

          # Create base type name node
          name_node = @arena.add_typed(IdentifierNode.new(
            name_token.span,
            name_token.slice
          ))

          # Calculate span covering entire generic expression
          spans = [name_token.span, lparen.span]
          type_args.each { |arg| spans << node_span(arg) }
          spans << rparen.span
          generic_span = Span.cover_all(spans)

          # Create Generic node
          @arena.add_typed(
            GenericNode.new(
              generic_span,
              name_node,
              type_args
            )
          )
        end

        # Phase 47: Parse safe navigation (&.)
        private def parse_safe_navigation(receiver : ExprId) : ExprId
          amp_dot = current_token
          advance  # Skip '&.'
          skip_trivia

          member_token = current_token

          # Member must be identifier
          if member_token.kind == Token::Kind::Identifier
            spans = [] of Span
            spans << node_span(receiver)
            spans << amp_dot.span
            spans << member_token.span
            safe_nav_span = Span.cover_all(spans)

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
          BINARY_PRECEDENCE.has_key?(token.kind)
        end

        private def precedence_for(token : Token) : Int32
          BINARY_PRECEDENCE[token.kind]? || 0
        end

        private def emit_unexpected(token : Token)
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
          if index < @tokens.size
            @tokens[index]
          else
            @tokens.last
          end
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
               Token::Kind::Do, Token::Kind::End, Token::Kind::If, Token::Kind::Unless
            # Common keywords that can be used as parameter/argument names
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

        private def consume_trim_marker?(marker : Char = '-')
          token = current_token
          # Check for trim marker
          # "-" can be Minus token (Phase 18)
          # "~" can be Tilde token (Phase 21)
          is_trim = case marker
          when '-'
            token.kind == Token::Kind::Minus ||
              (token.kind == Token::Kind::Operator && slice_eq?(token.slice, "-"))
          when '~'
            token.kind == Token::Kind::Tilde ||
              (token.kind == Token::Kind::Operator && slice_eq?(token.slice, "~"))
          else
            token.kind == Token::Kind::Operator && slice_eq?(token.slice, marker.to_s)
          end

          if is_trim
            advance
            true
          else
            false
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
          current = current_token
          return false unless current.kind == Token::Kind::LBrace
          peek = peek_token
          peek.kind == Token::Kind::LBrace
        end

        private def macro_expression_left_trim?
          second = peek_token(1)
          third = peek_token(2)
          second.kind == Token::Kind::LBrace &&
            third.kind == Token::Kind::Operator && slice_eq?(third.slice, "-")
        end

        private def macro_control_start?
          current = current_token
          return false unless current.kind == Token::Kind::LBrace
          peek = peek_token
          # Phase 18: % changed from Operator to Percent token
          peek.kind == Token::Kind::Percent
        end

        private def macro_control_left_trim?
          second = peek_token(1)
          third = peek_token(2)
          # Phase 18: % changed from Operator to Percent token
          second.kind == Token::Kind::Percent &&
            (third.kind == Token::Kind::Minus || (third.kind == Token::Kind::Operator && slice_eq?(third.slice, "-")))
        end

        private def macro_terminator_reached?(token : Token)
          case @macro_terminator
          when :expression
            return true if operator_token?(token, "}")
            return true if operator_token?(token, "-") && operator_token?(peek_token(1), "}")
          when :control
            return true if operator_token?(token, "%")
            return true if operator_token?(token, "-") && operator_token?(peek_token(1), "%")
          end
          false
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
          while current_token.kind == Token::Kind::Whitespace
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

        private def parse_macro_control_piece
          start_token = current_token
          # Phase 15/18: { is LBrace, % is Percent token
          expect_operator(Token::Kind::LBrace)
          expect_operator(Token::Kind::Percent)
          trim_left = consume_trim_marker?
          skip_macro_whitespace

          keyword_token = current_token
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
          when "else", "end", "comment"
            # no expression
          else
            emit_unexpected(keyword_token)
          end

          skip_macro_whitespace
          trim_right = false
          if (operator_token?(current_token, "-") || operator_token?(current_token, "~")) && operator_token?(peek_token(1), "%")
            advance
            trim_right = true
          end

          # Phase 15/18: % is Percent, } is RBrace
          expect_operator(Token::Kind::Percent)
          expect_operator(Token::Kind::RBrace)
          end_token = previous_token

          newline_escape = consume_macro_newline_escape?
          skip_whitespace = trim_right || newline_escape

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

          # Capture span covering full {% ... %} section
          control_span = if end_token
            start_token.span.cover(end_token.span)
          else
            start_token.span
          end

          piece = MacroPiece.control(kind, keyword, expr, trim_left, trim_right, iter_vars, iterable, control_span)
          # store trim flags later if needed

          effect = case keyword
            when "if", "unless", "for", "while", "comment"
              :push
            when "end"
              :pop
            else
              :none
            end

          {piece, effect, skip_whitespace}
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
              emit_unexpected(token)
            end
          else
            if token.kind == Token::Kind::Identifier && slice_eq?(token.slice, expected)
              advance
            else
              emit_unexpected(token)
            end
          end
        end

        private def parse_macro_expression_piece
          start_token = current_token
          # Phase 15: { is LBrace token
          expect_operator(Token::Kind::LBrace)
          expect_operator(Token::Kind::LBrace)
          left_trim = consume_trim_marker?('-') || consume_trim_marker?('~')
          skip_macro_whitespace
          expr = with_macro_terminator(:expression) { parse_expression(0) }
          skip_macro_whitespace

          right_trim = false
          if (operator_token?(current_token, "-") || operator_token?(current_token, "~")) && operator_token?(peek_token(1), "}")
            advance
            right_trim = true
          end
          advance if right_trim

          # Phase 15: } is RBrace token
          expect_operator(Token::Kind::RBrace)
          expect_operator(Token::Kind::RBrace)
          closing_span = previous_token.try(&.span)

          newline_escape = consume_macro_newline_escape?
          skip_whitespace = right_trim || newline_escape

          macro_span = closing_span ? start_token.span.cover(closing_span) : start_token.span
          macro_expr_id = @arena.add_typed(MacroExpressionNode.new(macro_span, expr))
          piece = MacroPiece.expression(macro_expr_id, left_trim, right_trim, macro_span)
          {piece, skip_whitespace}
        end

        # Phase 103: Inline hot path - frequently used for span calculations
        @[AlwaysInline]
        private def node_span(id : ExprId) : Span
          @arena[id].span
        end

        private def cover_optional_spans(*spans : Span?) : Span
          filtered = spans.to_a.compact
          raise ArgumentError.new("cover_optional_spans requires at least one span") if filtered.empty?
          Span.cover_all(filtered)
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
            temp_name_slice
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

            # Phase 101: Check for trailing do...end or {...} block (Amp case)
            # This handles: &.each_value do |x| ... end
            skip_trivia
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

            # Phase 101: Check for trailing do...end or {...} block (AmpDot case)
            # This handles: &.each_value do |x| ... end
            skip_trivia
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

        UNARY_OPERATORS = [Token::Kind::Plus, Token::Kind::Minus, Token::Kind::AmpPlus, Token::Kind::AmpMinus, Token::Kind::Not, Token::Kind::Tilde]
      end
    end
  end
end
