require "./rope"
require "./lexer/token"
require "./string_pool"
require "./watchdog"

module CrystalV2
  module Compiler
    module Frontend
      class Lexer
        @diagnostics : Array(CrystalV2::Compiler::Frontend::Diagnostic)?
        property diagnostics
        @last_token_kind : Token::Kind?  # Phase 57: for regex vs division disambiguation
        @at_statement_start : Bool       # Phase 57: Track statement boundaries for regex disambiguation
        @whitespace_before : Bool        # Track if whitespace preceded current token (for regex disambiguation)
        @macro_expr_depth : Int32        # Track nesting of {{ ... }} macro expressions
        @string_pool : StringPool  # String interning for memory optimization
        getter string_pool : StringPool  # Week 1 Day 2: expose for parser generic type interning
        @source : String
        getter source : String

        def initialize(source : String, *, diagnostics : Array(Diagnostic)? = nil)
          @source = source
          @rope = Rope.new(source)
          @offset = 0
          @line = 1
          @column = 1
          @processed_strings = [] of Bytes  # Phase 54: storage for escape-processed strings
          @last_token_kind = nil  # Phase 57: for regex vs division disambiguation
          @at_statement_start = true  # Phase 57: Start of file is statement start
          @whitespace_before = false  # Track if whitespace preceded current token
          @macro_expr_depth = 0
          @string_pool = StringPool.new  # String interning for memory optimization
          @diagnostics = diagnostics
        end

        private def debug(message : String)
          if ENV["LEXER_DEBUG"]?
            STDERR.puts message
          end
        end

        private def emit_diagnostic(message : String, span : Span)
          if diag = @diagnostics
            diag << Diagnostic.new(message, span)
          end
        end

        # Iterate all tokens. When skip_trivia is true, omits Whitespace and Comment tokens
        # but still yields Newline (needed by the parser for statement boundaries).
        def each_token(*, skip_trivia : Bool = false, &block : Token ->)
          while token = next_token
            if skip_trivia && (token.kind == Token::Kind::Whitespace || token.kind == Token::Kind::Comment)
              next
            end

            block.call token
            break if token.kind == Token::Kind::EOF
          end
        end

        def next_token : Token
          return eof_token if @offset >= @rope.size

          byte = current_byte

          token = case
          when whitespace?(byte)
            lex_whitespace
          when byte == NEWLINE
            lex_newline
          when byte == AT_SIGN
            lex_instance_var
          when byte == DOLLAR_SIGN
            # Phase 75: Global variables
            lex_global_var
          when byte == COLON
            # Phase 16: Check if this is a symbol literal
            lex_symbol_or_colon
          when identifier_start?(byte)
            lex_identifier
          when ascii_number?(byte)
            lex_number
          when byte == DOUBLE_QUOTE
            lex_string
          when byte == BACKTICK
            # Disambiguate between backtick command literal and backtick method name.
            # If the last significant token was `def`, a backtick denotes the
            # special backtick operator method name (def `(args)), not a command
            # literal. In that case, emit a single-character Operator token and
            # let the parser handle the method name.
            if @last_token_kind == Token::Kind::Def
              start_offset, start_line, start_column = capture_position
              advance # consume the backtick character
              Token.new(
                Token::Kind::Operator,
                Bytes[BACKTICK],
                build_span(start_offset, start_line, start_column)
              )
            else
              # Command literal with optional interpolation: `...`
              lex_backtick
            end
          when byte == LEFT_BRACE && peek_byte == LEFT_BRACE
            @macro_expr_depth += 1
            lex_macro_expr_start
          when byte == RIGHT_BRACE && peek_byte == RIGHT_BRACE && @macro_expr_depth > 0
            @macro_expr_depth -= 1
            lex_macro_expr_end
          when byte == SINGLE_QUOTE
            # Phase 56: Character literals
            lex_char
          when byte == HASH
            lex_comment
          else
            lex_operator
          end

          # Phase 57: Track last significant token for regex vs division disambiguation
          # Whitespace is not significant, but Newline/Semicolon mark statement boundaries
          case token.kind
          when Token::Kind::Whitespace
            # Track whitespace for regex disambiguation (foo /;/ vs foo/bar)
            @whitespace_before = true
          when Token::Kind::Newline, Token::Kind::Semicolon
            # Statement boundary - next slash could be regex
            @at_statement_start = true
            @whitespace_before = false
          else
            @last_token_kind = token.kind
            @at_statement_start = false
            @whitespace_before = false
          end

          token
        end

        private def eof_token
          Token.new(Token::Kind::EOF, Slice(UInt8).new(0), current_point_span)
        end

        private def capture_position
          {@offset, @line, @column}
        end

        private def current_byte
          @rope.bytes[@offset]
        end

        private def advance(count : Int32 = 1)
          count.times do
            Watchdog.check!
            byte = current_byte
            @offset += 1
            if byte == NEWLINE
              @line += 1
              @column = 1
            else
              @column += 1
            end
          end
        end

        # Lookahead without consuming
        private def peek_byte(offset : Int32 = 1) : UInt8?
          idx = @offset + offset
          return nil if idx >= @rope.size
          @rope.bytes[idx]
        end

        private def lex_whitespace
          start_offset, start_line, start_column = capture_position
          from = @offset
          while @offset < @rope.size && whitespace?(current_byte)
            advance
          end
          Token.new(
            Token::Kind::Whitespace,
            @rope.bytes[from...@offset],
            build_span(start_offset, start_line, start_column)
          )
        end

        private def lex_newline
          start_offset, start_line, start_column = capture_position
          advance
          Token.new(
            Token::Kind::Newline,
            @rope.bytes[start_offset...@offset],
            build_span(start_offset, start_line, start_column)
          )
        end

        private def lex_identifier
          start_offset, start_line, start_column = capture_position
          from = @offset
          while @offset < @rope.size && identifier_char?(current_byte)
            advance
          end
          if @offset < @rope.size && identifier_suffix?(current_byte)
            advance
          end

          # Slice of the identifier
          id = @rope.bytes[from...@offset]
          # Classify keyword without allocating String
          kind = keyword_kind_for(id)
          # Do not intern here to avoid copying for tokens not used in AST;
          # interning will be performed at IdentifierNode construction in parser
          slice = id

          Token.new(
            kind,
            slice,
            build_span(start_offset, start_line, start_column)
          )
        end

        # Classify identifier slice as keyword or identifier without allocating String
        # Correct length buckets; avoid misclassification (fast-path by size, exact compare by slice)
        private def keyword_kind_for(id : Slice(UInt8)) : Token::Kind
          # Special suffix keywords with punctuation
          return Token::Kind::AsQuestion  if id == "as?".to_slice
          return Token::Kind::IsA         if id == "is_a?".to_slice
          return Token::Kind::RespondsTo  if id == "responds_to?".to_slice

          case id.size
          when 2
            return Token::Kind::If   if id == "if".to_slice
            return Token::Kind::Do   if id == "do".to_slice
            return Token::Kind::In   if id == "in".to_slice
            return Token::Kind::Of   if id == "of".to_slice
            return Token::Kind::As   if id == "as".to_slice
          when 3
            return Token::Kind::End  if id == "end".to_slice
            return Token::Kind::Def  if id == "def".to_slice
            return Token::Kind::Nil  if id == "nil".to_slice
            return Token::Kind::Lib  if id == "lib".to_slice
            return Token::Kind::Out  if id == "out".to_slice
            return Token::Kind::For  if id == "for".to_slice
            return Token::Kind::Fun  if id == "fun".to_slice
            return Token::Kind::Asm  if id == "asm".to_slice
          when 4
            return Token::Kind::Then  if id == "then".to_slice
            return Token::Kind::Case  if id == "case".to_slice
            return Token::Kind::When  if id == "when".to_slice
            return Token::Kind::True  if id == "true".to_slice
            return Token::Kind::Loop  if id == "loop".to_slice
            return Token::Kind::Next  if id == "next".to_slice
            return Token::Kind::Enum  if id == "enum".to_slice
            return Token::Kind::With  if id == "with".to_slice
            return Token::Kind::Else  if id == "else".to_slice
            return Token::Kind::Self  if id == "self".to_slice
          when 5
            return Token::Kind::Begin   if id == "begin".to_slice
            return Token::Kind::Class   if id == "class".to_slice
            return Token::Kind::While   if id == "while".to_slice
            return Token::Kind::Until   if id == "until".to_slice
            return Token::Kind::Break   if id == "break".to_slice
            return Token::Kind::False   if id == "false".to_slice
            return Token::Kind::Super   if id == "super".to_slice
            return Token::Kind::Alias   if id == "alias".to_slice
            return Token::Kind::Union   if id == "union".to_slice
            return Token::Kind::Spawn   if id == "spawn".to_slice
            return Token::Kind::Elsif   if id == "elsif".to_slice
            return Token::Kind::Yield   if id == "yield".to_slice
            return Token::Kind::Raise   if id == "raise".to_slice
            return Token::Kind::Macro   if id == "macro".to_slice
          when 6
            return Token::Kind::Ensure   if id == "ensure".to_slice
            return Token::Kind::Return   if id == "return".to_slice
            return Token::Kind::Struct   if id == "struct".to_slice
            return Token::Kind::Rescue   if id == "rescue".to_slice
            return Token::Kind::Module   if id == "module".to_slice
            return Token::Kind::Select   if id == "select".to_slice
            return Token::Kind::Typeof   if id == "typeof".to_slice
            return Token::Kind::Unless   if id == "unless".to_slice
            return Token::Kind::Sizeof   if id == "sizeof".to_slice
            return Token::Kind::Extend   if id == "extend".to_slice
          when 7
            return Token::Kind::Private  if id == "private".to_slice
            return Token::Kind::Include  if id == "include".to_slice
            return Token::Kind::Require  if id == "require".to_slice
            return Token::Kind::Alignof  if id == "alignof".to_slice
          when 8
            return Token::Kind::Abstract if id == "abstract".to_slice
            return Token::Kind::Offsetof if id == "offsetof".to_slice
          when 9
            return Token::Kind::Protected if id == "protected".to_slice
            return Token::Kind::Pointerof if id == "pointerof".to_slice
          when 10
            return Token::Kind::Annotation if id == "annotation".to_slice
          when 12
            return Token::Kind::PreviousDef if id == "previous_def".to_slice
          when 13
            return Token::Kind::Uninitialized if id == "uninitialized".to_slice
          when 15
            return Token::Kind::Sizeof if id == "instance_sizeof".to_slice
          when 16
            return Token::Kind::InstanceAlignof if id == "instance_alignof".to_slice
          end

          Token::Kind::Identifier
        end

        private def lex_instance_var
          start_offset, start_line, start_column = capture_position
          from = @offset

          # Consume @
          advance

          # Phase 76: Check for class variable (@@var)
          if @offset < @rope.size && current_byte == AT_SIGN
            # This is a class variable
            advance  # consume second @
            return lex_class_var_continued(from, start_offset, start_line, start_column)
          end

          # Instance variable must start with identifier character
          if @offset >= @rope.size || !identifier_start?(current_byte)
            # Invalid instance variable - just @, return as operator
            return Token.new(
              Token::Kind::Operator,
              @rope.bytes[from...@offset],
              build_span(start_offset, start_line, start_column)
            )
          end

          # Read identifier part
          while @offset < @rope.size && identifier_char?(current_byte)
            advance
          end

          # Instance variables can have suffix (?, !)
          if @offset < @rope.size && identifier_suffix?(current_byte)
            advance
          end

          Token.new(
            Token::Kind::InstanceVar,
            @string_pool.intern(@rope.bytes[from...@offset]),
            build_span(start_offset, start_line, start_column)
          )
        end

        # Phase 76: Lex class variable (continued after @@ consumed)
        # @@class_var → ClassVar token with slice "@@class_var"
        private def lex_class_var_continued(from, start_offset, start_line, start_column)
          # Class variable must start with identifier character
          if @offset >= @rope.size || !identifier_start?(current_byte)
            # Invalid class variable - just @@, return as operator
            return Token.new(
              Token::Kind::Operator,
              @rope.bytes[from...@offset],
              build_span(start_offset, start_line, start_column)
            )
          end

          # Read identifier part
          while @offset < @rope.size && identifier_char?(current_byte)
            advance
          end

          # Class variables can have suffix (?, !)
          if @offset < @rope.size && identifier_suffix?(current_byte)
            advance
          end

          Token.new(
            Token::Kind::ClassVar,
            @string_pool.intern(@rope.bytes[from...@offset]),
            build_span(start_offset, start_line, start_column)
          )
        end

        # Phase 75: Lex global variable
        # $global_var → GlobalVar token with slice "$global_var"
        private def lex_global_var
          start_offset, start_line, start_column = capture_position
          from = @offset

          # Consume $
          advance

          # Special-case: allow regex globals and status: $?, $!, $1, $2, ..., $~ (with optional '?')
          if @offset < @rope.size && (current_byte == QUESTION || current_byte == EXCLAMATION)
            advance
            return Token.new(
              Token::Kind::GlobalVar,
              @string_pool.intern(@rope.bytes[from...@offset]),
              build_span(start_offset, start_line, start_column)
            )
          end

          # Regex capture globals: $1, $2, ..., optional '?'
          if @offset < @rope.size && ascii_number?(current_byte)
            # consume digits
            while @offset < @rope.size && ascii_number?(current_byte)
              advance
            end
            # optional suffix '?'
            if @offset < @rope.size && current_byte == QUESTION
              advance
            end
            return Token.new(
              Token::Kind::GlobalVar,
              @string_pool.intern(@rope.bytes[from...@offset]),
              build_span(start_offset, start_line, start_column)
            )
          end

          # Regex match data: $~ (optional '?')
          if @offset < @rope.size && current_byte == '~'.ord.to_u8
            advance
            if @offset < @rope.size && current_byte == QUESTION
              advance
            end
            return Token.new(
              Token::Kind::GlobalVar,
              @string_pool.intern(@rope.bytes[from...@offset]),
              build_span(start_offset, start_line, start_column)
            )
          end

          # Global variable must start with identifier character otherwise
          if @offset >= @rope.size || !identifier_start?(current_byte)
            # Invalid global variable - just '$' as operator
            return Token.new(
              Token::Kind::Operator,
              @rope.bytes[from...@offset],
              build_span(start_offset, start_line, start_column)
            )
          end

          # Read identifier part
          while @offset < @rope.size && identifier_char?(current_byte)
            advance
          end

          # Global variables can have suffix (?, !)
          if @offset < @rope.size && identifier_suffix?(current_byte)
            advance
          end

          Token.new(
            Token::Kind::GlobalVar,
            @string_pool.intern(@rope.bytes[from...@offset]),
            build_span(start_offset, start_line, start_column)
          )
        end

        # Phase 16: Lex symbol literal or colon
        # :identifier → Symbol token with slice ":identifier"
        # : (not followed by identifier) → Colon token
        private def lex_symbol_or_colon
          start_offset, start_line, start_column = capture_position
          from = @offset

          # Consume first :
          advance

          # Phase 63: Check if :: (path expression)
          if @offset < @rope.size && current_byte == ':'.ord.to_u8
            advance
            return Token.new(
              Token::Kind::ColonColon,
              @rope.bytes[from...@offset],
              build_span(start_offset, start_line, start_column)
            )
          end

          # Check if followed by identifier start or operator
          if @offset >= @rope.size || !identifier_start?(current_byte)
            # Try to lex operator symbol (e.g., :<<, :>, :===, :=~, :!~, :^, :&**)
            op_symbol = try_lex_operator_symbol(from, start_offset, start_line, start_column)
            return op_symbol if op_symbol

            # Just a colon (for type annotations)
            return Token.new(
              Token::Kind::Colon,
              @rope.bytes[from...@offset],
              build_span(start_offset, start_line, start_column)
            )
          end

          # Read identifier part
          while @offset < @rope.size && identifier_char?(current_byte)
            advance
          end

          # Symbols can have suffix (?, !)
          if @offset < @rope.size && identifier_suffix?(current_byte)
            advance
          end

          # Setter-style symbols can end with '=' (e.g., :foo=)
          if @offset < @rope.size && current_byte == '='.ord.to_u8
            advance
          end

          Token.new(
            Token::Kind::Symbol,
            @rope.bytes[from...@offset],
            build_span(start_offset, start_line, start_column)
          )
        end

        # Try to lex operator symbol (e.g., :<<, :>, :===, :=~, :!~, :^, :&**)
        # Returns nil if no operator symbol found
        private def try_lex_operator_symbol(from : Int32, start_offset : Int32, start_line : Int32, start_column : Int32) : Token?
          return nil if @offset >= @rope.size

          ch = current_byte
          case ch
          when '<'.ord.to_u8
            # :< :<<  :<=
            advance
            if @offset < @rope.size
              if current_byte == '<'.ord.to_u8
                advance  # :<<
              elsif current_byte == '='.ord.to_u8
                advance  # :<=
              end
              # else just :<
            end
          when '>'.ord.to_u8
            # :> :>>  :>=
            advance
            if @offset < @rope.size
              if current_byte == '>'.ord.to_u8
                advance  # :>>
              elsif current_byte == '='.ord.to_u8
                advance  # :>=
              end
              # else just :>
            end
          when '='.ord.to_u8
            # :=== :=~
            advance
            if @offset < @rope.size
              if current_byte == '='.ord.to_u8
                advance
                if @offset < @rope.size && current_byte == '='.ord.to_u8
                  advance  # :===
                else
                  @offset -= 1  # Not valid, rewind
                  return nil
                end
              elsif current_byte == '~'.ord.to_u8
                advance  # :=~
              else
                @offset -= 1  # Not valid, rewind
                return nil
              end
            else
              @offset -= 1  # Not valid, rewind
              return nil
            end
          when '!'.ord.to_u8
            # :!~
            advance
            if @offset < @rope.size && current_byte == '~'.ord.to_u8
              advance  # :!~
            else
              @offset -= 1  # Not valid, rewind
              return nil
            end
          when '^'.ord.to_u8
            # :^
            advance
          when '&'.ord.to_u8
            # :&**
            advance
            if @offset < @rope.size && current_byte == '*'.ord.to_u8
              advance
              if @offset < @rope.size && current_byte == '*'.ord.to_u8
                advance  # :&**
              else
                @offset -= 2  # Not valid, rewind
                return nil
              end
            else
              @offset -= 1  # Not valid, rewind
              return nil
            end
          else
            return nil
          end

          Token.new(
            Token::Kind::Symbol,
            @rope.bytes[from...@offset],
            build_span(start_offset, start_line, start_column)
          )
        end

        private def lex_number
          start_offset, start_line, start_column = capture_position
          from = @offset

          # Phase 53: Check for hex (0x), binary (0b), or octal (0o) prefix
          if current_byte == '0'.ord.to_u8 && @offset + 1 < @rope.size
            next_byte = @rope.bytes[@offset + 1]
            case next_byte
            when 'x'.ord.to_u8, 'X'.ord.to_u8
              return lex_hex_number(start_offset, start_line, start_column, from)
            when 'b'.ord.to_u8, 'B'.ord.to_u8
              return lex_binary_number(start_offset, start_line, start_column, from)
            when 'o'.ord.to_u8, 'O'.ord.to_u8
              return lex_octal_number(start_offset, start_line, start_column, from)
            end
          end

          # Phase 55: Read integer part (with underscore separators)
          while @offset < @rope.size && (ascii_number?(current_byte) || current_byte == UNDERSCORE)
            advance
          end

          # Phase 55: Skip trailing underscores (so suffix parser can see them)
          while @offset > from && @rope.bytes[@offset - 1] == UNDERSCORE
            @offset -= 1
          end

          # Check for decimal point (float)
          has_decimal = false
          if @offset < @rope.size && current_byte == '.'.ord.to_u8
            # Peek ahead to ensure next char is a digit (not method call like 42.abs)
            if @offset + 1 < @rope.size && ascii_number?(@rope.bytes[@offset + 1])
              has_decimal = true
              advance  # consume '.'
              # Phase 55: Read fractional part (with underscore separators)
              while @offset < @rope.size && (ascii_number?(current_byte) || current_byte == UNDERSCORE)
                advance
              end
              # Phase 55: Skip trailing underscores (so suffix parser can see them)
              while @offset > from && @rope.bytes[@offset - 1] == UNDERSCORE
                @offset -= 1
              end
            end
          end

          # Phase 103L: Exponent part for floats: [eE][+-]?digits(_ allowed)
          if @offset < @rope.size && (current_byte == 'e'.ord.to_u8 || current_byte == 'E'.ord.to_u8)
            exp_pos = @offset
            advance  # consume 'e'/'E'
            # Optional sign
            if @offset < @rope.size && (current_byte == '+'.ord.to_u8 || current_byte == '-'.ord.to_u8)
              advance
            end
            # Digits with underscores
            digits = false
            while @offset < @rope.size && (ascii_number?(current_byte) || current_byte == UNDERSCORE)
              digits = true if ascii_number?(current_byte)
              advance
            end
            # Trim trailing underscores
            while @offset > exp_pos && @rope.bytes[@offset - 1] == UNDERSCORE
              @offset -= 1
            end
            # If we didn't actually consume digits, rewind exponent
            unless digits
              @offset = exp_pos
            else
              has_decimal = true
            end
          end

          # Phase 103J: Check for suffix (_i8, _i16, _i32, _i64, _i128, _u8, _u16, _u32, _u64, _u128, _f32, _f64)
          number_kind : NumberKind? = nil
          if @offset < @rope.size && current_byte == '_'.ord.to_u8
            suffix_start = @offset
            advance  # consume '_'

            # Read suffix characters
            suffix_from = @offset
            while @offset < @rope.size && (ascii_letter?(current_byte) || ascii_number?(current_byte))
              advance
            end

            suffix = String.new(@rope.bytes[suffix_from...@offset])
            number_kind = case suffix
            # Signed integers
            when "i8"   then NumberKind::I8
            when "i16"  then NumberKind::I16
            when "i32"  then NumberKind::I32
            when "i64"  then NumberKind::I64
            when "i128" then NumberKind::I128
            # Unsigned integers
            when "u8"   then NumberKind::U8
            when "u16"  then NumberKind::U16
            when "u32"  then NumberKind::U32
            when "u64"  then NumberKind::U64
            when "u128" then NumberKind::U128
            # Floats
            when "f32"  then NumberKind::F32
            when "f64"  then NumberKind::F64
            else
              # Unknown suffix - ignore and treat as separate token
              # Reset to before underscore
              @offset = suffix_start
              nil
            end
          end

          # Phase 103L: Allow float suffix without underscore (e.g., 1e0f32, 1.0f64)
          if number_kind.nil? && @offset + 2 < @rope.size && ascii_letter?(current_byte)
            b0 = @rope.bytes[@offset]
            b1 = @rope.bytes[@offset + 1]
            b2 = @rope.bytes[@offset + 2]
            if b0 == 'f'.ord.to_u8 && b1 == '3'.ord.to_u8 && b2 == '2'.ord.to_u8
              number_kind = NumberKind::F32
              advance(3)
            elsif b0 == 'f'.ord.to_u8 && b1 == '6'.ord.to_u8 && b2 == '4'.ord.to_u8
              number_kind = NumberKind::F64
              advance(3)
            end
          end

          # Phase 103L: Allow integer suffix without underscore (e.g., 8u64, 1i32)
          if number_kind.nil? && @offset < @rope.size && ascii_letter?(current_byte)
            bytes = @rope.bytes
            b0 = bytes[@offset]
            # Helper lambda to match a small string and advance
            match = ->(s : String) do
              n = s.bytesize
              return false unless @offset + n <= @rope.size
              i = 0
              while i < n
                break unless bytes[@offset + i] == s.to_unsafe[i]
                i += 1
              end
              return false unless i == n
              advance(n)
              true
            end
            if b0 == 'i'.ord.to_u8
              if match.call("i8")
                number_kind = NumberKind::I8
              elsif match.call("i16")
                number_kind = NumberKind::I16
              elsif match.call("i32")
                number_kind = NumberKind::I32
              elsif match.call("i64")
                number_kind = NumberKind::I64
              elsif match.call("i128")
                number_kind = NumberKind::I128
              end
            elsif b0 == 'u'.ord.to_u8
              if match.call("u8")
                number_kind = NumberKind::U8
              elsif match.call("u16")
                number_kind = NumberKind::U16
              elsif match.call("u32")
                number_kind = NumberKind::U32
              elsif match.call("u64")
                number_kind = NumberKind::U64
              elsif match.call("u128")
                number_kind = NumberKind::U128
              end
            end
          end

          # Infer NumberKind if not explicitly specified
          if number_kind.nil?
            number_kind = has_decimal ? NumberKind::F64 : NumberKind::I32
          end

          Token.new(
            Token::Kind::Number,
            @rope.bytes[from...@offset],
            build_span(start_offset, start_line, start_column),
            number_kind: number_kind
          )
        end

        # Phase 53: Hexadecimal number literals (0xFF, 0x1A2B)
        private def lex_hex_number(start_offset : Int32, start_line : Int32, start_column : Int32, from : Int32)
          advance  # Skip '0'
          advance  # Skip 'x' or 'X'

          # Phase 55: Read hex digits (with underscore separators)
          while @offset < @rope.size && (hex_digit?(current_byte) || current_byte == UNDERSCORE)
            advance
          end

          # Phase 55: Skip trailing underscores (so suffix parser can see them)
          while @offset > from && @rope.bytes[@offset - 1] == UNDERSCORE
            @offset -= 1
          end

          # Check for suffix (_i32, _i64, _f64)
          number_kind = lex_number_suffix

          # Infer NumberKind if not explicitly specified
          number_kind ||= begin
            # Allow integer suffix without underscore (e.g., 0xFFu64)
            if @offset + 2 < @rope.size && ascii_letter?(current_byte)
              b0 = @rope.bytes[@offset]
              if b0 == 'i'.ord.to_u8
                if @offset + 3 <= @rope.size && @rope.bytes[@offset,3] == "i16".to_slice
                  advance(3); NumberKind::I16
                elsif @offset + 3 <= @rope.size && @rope.bytes[@offset,3] == "i32".to_slice
                  advance(3); NumberKind::I32
                elsif @offset + 3 <= @rope.size && @rope.bytes[@offset,3] == "i64".to_slice
                  advance(3); NumberKind::I64
                elsif @offset + 4 <= @rope.size && @rope.bytes[@offset,4] == "i128".to_slice
                  advance(4); NumberKind::I128
                elsif @offset + 2 <= @rope.size && @rope.bytes[@offset,2] == "i8".to_slice
                  advance(2); NumberKind::I8
                else
                  nil
                end
              elsif b0 == 'u'.ord.to_u8
                if @offset + 3 <= @rope.size && @rope.bytes[@offset,3] == "u16".to_slice
                  advance(3); NumberKind::U16
                elsif @offset + 3 <= @rope.size && @rope.bytes[@offset,3] == "u32".to_slice
                  advance(3); NumberKind::U32
                elsif @offset + 3 <= @rope.size && @rope.bytes[@offset,3] == "u64".to_slice
                  advance(3); NumberKind::U64
                elsif @offset + 4 <= @rope.size && @rope.bytes[@offset,4] == "u128".to_slice
                  advance(4); NumberKind::U128
                elsif @offset + 2 <= @rope.size && @rope.bytes[@offset,2] == "u8".to_slice
                  advance(2); NumberKind::U8
                else
                  nil
                end
              else
                nil
              end
            end || NumberKind::I32
          end

          Token.new(
            Token::Kind::Number,
            @rope.bytes[from...@offset],
            build_span(start_offset, start_line, start_column),
            number_kind: number_kind
          )
        end

        # Phase 53: Binary number literals (0b1010, 0B1111)
        private def lex_binary_number(start_offset : Int32, start_line : Int32, start_column : Int32, from : Int32)
          advance  # Skip '0'
          advance  # Skip 'b' or 'B'

          # Phase 55: Read binary digits (with underscore separators)
          while @offset < @rope.size && (binary_digit?(current_byte) || current_byte == UNDERSCORE)
            advance
          end

          # Phase 55: Skip trailing underscores (so suffix parser can see them)
          while @offset > from && @rope.bytes[@offset - 1] == UNDERSCORE
            @offset -= 1
          end

          # Check for suffix (_i32, _i64, _f64)
          number_kind = lex_number_suffix

          # Infer NumberKind if not explicitly specified
          number_kind ||= begin
            # Allow integer suffix without underscore (e.g., 0b1010u8)
            if @offset + 2 < @rope.size && ascii_letter?(current_byte)
              b0 = @rope.bytes[@offset]
              if b0 == 'i'.ord.to_u8
                if @offset + 3 <= @rope.size && @rope.bytes[@offset,3] == "i16".to_slice
                  advance(3); NumberKind::I16
                elsif @offset + 3 <= @rope.size && @rope.bytes[@offset,3] == "i32".to_slice
                  advance(3); NumberKind::I32
                elsif @offset + 3 <= @rope.size && @rope.bytes[@offset,3] == "i64".to_slice
                  advance(3); NumberKind::I64
                elsif @offset + 4 <= @rope.size && @rope.bytes[@offset,4] == "i128".to_slice
                  advance(4); NumberKind::I128
                elsif @offset + 2 <= @rope.size && @rope.bytes[@offset,2] == "i8".to_slice
                  advance(2); NumberKind::I8
                else
                  nil
                end
              elsif b0 == 'u'.ord.to_u8
                if @offset + 3 <= @rope.size && @rope.bytes[@offset,3] == "u16".to_slice
                  advance(3); NumberKind::U16
                elsif @offset + 3 <= @rope.size && @rope.bytes[@offset,3] == "u32".to_slice
                  advance(3); NumberKind::U32
                elsif @offset + 3 <= @rope.size && @rope.bytes[@offset,3] == "u64".to_slice
                  advance(3); NumberKind::U64
                elsif @offset + 4 <= @rope.size && @rope.bytes[@offset,4] == "u128".to_slice
                  advance(4); NumberKind::U128
                elsif @offset + 2 <= @rope.size && @rope.bytes[@offset,2] == "u8".to_slice
                  advance(2); NumberKind::U8
                else
                  nil
                end
              else
                nil
              end
            end || NumberKind::I32
          end

          Token.new(
            Token::Kind::Number,
            @rope.bytes[from...@offset],
            build_span(start_offset, start_line, start_column),
            number_kind: number_kind
          )
        end

        # Phase 53: Octal number literals (0o755, 0O644)
        private def lex_octal_number(start_offset : Int32, start_line : Int32, start_column : Int32, from : Int32)
          advance  # Skip '0'
          advance  # Skip 'o' or 'O'

          # Phase 55: Read octal digits (with underscore separators)
          while @offset < @rope.size && (octal_digit?(current_byte) || current_byte == UNDERSCORE)
            advance
          end

          # Phase 55: Skip trailing underscores (so suffix parser can see them)
          while @offset > from && @rope.bytes[@offset - 1] == UNDERSCORE
            @offset -= 1
          end

          # Check for suffix (_i32, _i64, _f64)
          number_kind = lex_number_suffix

          # Infer NumberKind if not explicitly specified
          number_kind ||= begin
            # Allow integer suffix without underscore (e.g., 0o755u16)
            if @offset + 2 < @rope.size && ascii_letter?(current_byte)
              b0 = @rope.bytes[@offset]
              if b0 == 'i'.ord.to_u8
                if @offset + 3 <= @rope.size && @rope.bytes[@offset,3] == "i16".to_slice
                  advance(3); NumberKind::I16
                elsif @offset + 3 <= @rope.size && @rope.bytes[@offset,3] == "i32".to_slice
                  advance(3); NumberKind::I32
                elsif @offset + 3 <= @rope.size && @rope.bytes[@offset,3] == "i64".to_slice
                  advance(3); NumberKind::I64
                elsif @offset + 4 <= @rope.size && @rope.bytes[@offset,4] == "i128".to_slice
                  advance(4); NumberKind::I128
                elsif @offset + 2 <= @rope.size && @rope.bytes[@offset,2] == "i8".to_slice
                  advance(2); NumberKind::I8
                else
                  nil
                end
              elsif b0 == 'u'.ord.to_u8
                if @offset + 3 <= @rope.size && @rope.bytes[@offset,3] == "u16".to_slice
                  advance(3); NumberKind::U16
                elsif @offset + 3 <= @rope.size && @rope.bytes[@offset,3] == "u32".to_slice
                  advance(3); NumberKind::U32
                elsif @offset + 3 <= @rope.size && @rope.bytes[@offset,3] == "u64".to_slice
                  advance(3); NumberKind::U64
                elsif @offset + 4 <= @rope.size && @rope.bytes[@offset,4] == "u128".to_slice
                  advance(4); NumberKind::U128
                elsif @offset + 2 <= @rope.size && @rope.bytes[@offset,2] == "u8".to_slice
                  advance(2); NumberKind::U8
                else
                  nil
                end
              else
                nil
              end
            end || NumberKind::I32
          end

          Token.new(
            Token::Kind::Number,
            @rope.bytes[from...@offset],
            build_span(start_offset, start_line, start_column),
            number_kind: number_kind
          )
        end

        # Phase 53: Extract number suffix parsing to helper (byte-wise, no String allocations)
        # Phase 103J: Parse numeric type suffix (_i8, _i16, _i32, _i64, _i128, _u8, _u16, _u32, _u64, _u128, _f32, _f64)
        # Used by hex, binary, octal number parsers
        private def lex_number_suffix : NumberKind?
          return nil unless @offset < @rope.size && current_byte == '_'.ord.to_u8
          suffix_start = @offset
          advance  # consume '_'

          from = @offset
          while @offset < @rope.size && (ascii_letter?(current_byte) || ascii_number?(current_byte))
            advance
          end
          len = @offset - from
          bytes = @rope.bytes

          kind = nil.as(NumberKind?)
          case len
          when 2
            if bytes[from] == 'i'.ord.to_u8
              case bytes[from + 1]
              when '8'.ord.to_u8  then kind = NumberKind::I8
              end
            elsif bytes[from] == 'u'.ord.to_u8
              case bytes[from + 1]
              when '8'.ord.to_u8  then kind = NumberKind::U8
              end
            end
          when 3
            if bytes[from] == 'i'.ord.to_u8
              # i16/i32/i64
              if bytes[from + 1] == '1'.ord.to_u8 && bytes[from + 2] == '6'.ord.to_u8
                kind = NumberKind::I16
              elsif bytes[from + 1] == '3'.ord.to_u8 && bytes[from + 2] == '2'.ord.to_u8
                kind = NumberKind::I32
              elsif bytes[from + 1] == '6'.ord.to_u8 && bytes[from + 2] == '4'.ord.to_u8
                kind = NumberKind::I64
              end
            elsif bytes[from] == 'u'.ord.to_u8
              # u16/u32/u64
              if bytes[from + 1] == '1'.ord.to_u8 && bytes[from + 2] == '6'.ord.to_u8
                kind = NumberKind::U16
              elsif bytes[from + 1] == '3'.ord.to_u8 && bytes[from + 2] == '2'.ord.to_u8
                kind = NumberKind::U32
              elsif bytes[from + 1] == '6'.ord.to_u8 && bytes[from + 2] == '4'.ord.to_u8
                kind = NumberKind::U64
              end
            elsif bytes[from] == 'f'.ord.to_u8
              # f32/f64
              if bytes[from + 1] == '3'.ord.to_u8 && bytes[from + 2] == '2'.ord.to_u8
                kind = NumberKind::F32
              elsif bytes[from + 1] == '6'.ord.to_u8 && bytes[from + 2] == '4'.ord.to_u8
                kind = NumberKind::F64
              end
            end
          when 4
            # i128/u128
            if bytes[from] == 'i'.ord.to_u8
              if bytes[from + 1] == '1'.ord.to_u8 && bytes[from + 2] == '2'.ord.to_u8 && bytes[from + 3] == '8'.ord.to_u8
                kind = NumberKind::I128
              end
            elsif bytes[from] == 'u'.ord.to_u8
              if bytes[from + 1] == '1'.ord.to_u8 && bytes[from + 2] == '2'.ord.to_u8 && bytes[from + 3] == '8'.ord.to_u8
                kind = NumberKind::U128
              end
            end
          end

          unless kind
            # Unknown suffix - reset to before underscore and ignore
            @offset = suffix_start
          end
          kind
        end

        private def lex_string
          start_offset, start_line, start_column = capture_position
          advance # opening quote
          from = @offset
          has_interpolation = false
          has_escapes = false

          # Phase 54: Check if string contains escapes or interpolation.
          # Track interpolation brace depth so embedded quotes inside #{...} don't terminate scanning.
          scan_offset = @offset
          brace_depth = 0
          heredoc_inside_interpolation = false
          while scan_offset < @rope.size
            byte = @rope.bytes[scan_offset]

            if brace_depth == 0 && byte == DOUBLE_QUOTE
              break
            end

            if byte == HASH && scan_offset + 1 < @rope.size && @rope.bytes[scan_offset + 1] == LEFT_BRACE && brace_depth == 0
              has_interpolation = true
              brace_depth = 1
              scan_offset += 2
              next
            end

            if brace_depth > 0
            # Detect heredoc start within interpolation for diagnostics
            if byte == '<'.ord.to_u8 && scan_offset + 2 < @rope.size && @rope.bytes[scan_offset + 1] == '<'.ord.to_u8 && @rope.bytes[scan_offset + 2] == '-'.ord.to_u8
              heredoc_inside_interpolation = true
            end
            if byte == LEFT_BRACE
              brace_depth += 1
            elsif byte == RIGHT_BRACE
              brace_depth -= 1
            end
              scan_offset += 1
              next
            end

            if byte == '\\'.ord.to_u8
              has_escapes = true
            end
            scan_offset += 1
          end

          # Emit diagnostic if we already saw heredoc opener inside interpolation during scan
          if heredoc_inside_interpolation
            emit_diagnostic("heredoc cannot be used inside interpolation", build_span(start_offset, start_line, start_column))
          end

          # If no escapes, use original fast path
          if !has_escapes
            brace_depth_fast = 0
            while @offset < @rope.size
              if brace_depth_fast == 0 && current_byte == DOUBLE_QUOTE
                break
              end

              if brace_depth_fast == 0 && current_byte == HASH && @offset + 1 < @rope.size && @rope.bytes[@offset + 1] == LEFT_BRACE
                if @offset + 4 < @rope.size && @rope.bytes[@offset + 2] == '<'.ord.to_u8 && @rope.bytes[@offset + 3] == '<'.ord.to_u8 && @rope.bytes[@offset + 4] == '-'.ord.to_u8
                  heredoc_inside_interpolation = true
                end
              brace_depth_fast = 1
              advance(2)
              next
            elsif brace_depth_fast > 0
              if current_byte == '<'.ord.to_u8 && @offset + 2 < @rope.size && @rope.bytes[@offset + 1] == '<'.ord.to_u8 && @rope.bytes[@offset + 2] == '-'.ord.to_u8
                heredoc_inside_interpolation = true
              end
              brace_depth_fast += 1 if current_byte == LEFT_BRACE
              brace_depth_fast -= 1 if current_byte == RIGHT_BRACE
            end

              advance
            end
            advance if @offset < @rope.size # closing quote

            kind = has_interpolation ? Token::Kind::StringInterpolation : Token::Kind::String
            if heredoc_inside_interpolation
              emit_diagnostic("heredoc cannot be used inside interpolation", build_span(start_offset, start_line, start_column))
            end
            return Token.new(
              kind,
              @rope.bytes[from...@offset - 1],
              build_span(start_offset, start_line, start_column)
            )
          end

          # Phase 54: Process escape sequences
          processed = Bytes.new(scan_offset - from)  # Allocate with estimated size
          buffer = IO::Memory.new
          brace_depth_processed = 0

          while @offset < @rope.size
            break if brace_depth_processed == 0 && current_byte == DOUBLE_QUOTE

            if brace_depth_processed == 0 && current_byte == HASH && @offset + 1 < @rope.size && @rope.bytes[@offset + 1] == LEFT_BRACE
              buffer.write_byte(current_byte)
              advance
              buffer.write_byte(current_byte)
              advance
              brace_depth_processed += 1
              next
            elsif brace_depth_processed > 0
              if current_byte == '<'.ord.to_u8 && @offset + 2 < @rope.size && @rope.bytes[@offset + 1] == '<'.ord.to_u8 && @rope.bytes[@offset + 2] == '-'.ord.to_u8
                heredoc_inside_interpolation = true
              end
              if current_byte == LEFT_BRACE
                brace_depth_processed += 1
              elsif current_byte == RIGHT_BRACE
                brace_depth_processed -= 1
              end
              buffer.write_byte(current_byte)
              advance
              next
            end

            if current_byte == '\\'.ord.to_u8 && @offset + 1 < @rope.size
              # Escape sequence
              advance  # Skip backslash
              case current_byte
              when 'n'.ord.to_u8
                buffer.write_byte '\n'.ord.to_u8
              when 't'.ord.to_u8
                buffer.write_byte '\t'.ord.to_u8
              when 'r'.ord.to_u8
                buffer.write_byte '\r'.ord.to_u8
              when '\\'.ord.to_u8
                buffer.write_byte '\\'.ord.to_u8
              when '"'.ord.to_u8
                buffer.write_byte '"'.ord.to_u8
              when 'u'.ord.to_u8
                # Phase 58: Unicode escapes \uXXXX or \u{XXXX}
                advance
                if current_byte == '{'.ord.to_u8
                  # Variable length \u{X...XXXXXX}
                  advance  # Skip '{'
                  codepoint = parse_unicode_hex_digits('}'.ord.to_u8)
                  if codepoint
                    write_utf8(buffer, codepoint)
                  else
                    # Invalid Unicode escape - keep as is
                    buffer.write_byte '\\'.ord.to_u8
                    buffer.write_byte 'u'.ord.to_u8
                  end
                else
                  # Fixed length \uXXXX (4 hex digits)
                  codepoint = parse_unicode_hex_fixed(4)
                  if codepoint
                    write_utf8(buffer, codepoint)
                  else
                    # Invalid Unicode escape - keep as is
                    buffer.write_byte '\\'.ord.to_u8
                    buffer.write_byte 'u'.ord.to_u8
                  end
                end
                next  # Don't advance again, helper methods already did
              when 'x'.ord.to_u8
                # Phase 59: Hex escapes \xXX (2 hex digits)
                advance
                byte_value = parse_unicode_hex_fixed(2)
                if byte_value
                  buffer.write_byte byte_value.to_u8
                else
                  # Invalid hex escape - keep as is
                  buffer.write_byte '\\'.ord.to_u8
                  buffer.write_byte 'x'.ord.to_u8
                end
                next  # Don't advance again, helper methods already did
              else
                # Phase 62: Check for octal escapes \NNN (1-3 octal digits)
                if octal_digit?(current_byte)
                  byte_value = parse_octal_fixed(3)
                  if byte_value
                    buffer.write_byte byte_value.to_u8
                  else
                    # Should not happen if octal_digit? returned true
                    buffer.write_byte '\\'.ord.to_u8
                    buffer.write_byte current_byte
                  end
                  next  # Don't advance again, parse_octal_fixed already did
                else
                  # Unknown escape - keep as is
                  buffer.write_byte '\\'.ord.to_u8
                  buffer.write_byte current_byte
                end
              end
              advance
            else
              buffer.write_byte current_byte
              advance
            end
          end

          advance if @offset < @rope.size # closing quote

          # Store processed string
          processed_bytes = buffer.to_slice
          @processed_strings << processed_bytes

          # Return token with processed string
          kind = has_interpolation ? Token::Kind::StringInterpolation : Token::Kind::String
          if heredoc_inside_interpolation
            emit_diagnostic("heredoc cannot be used inside interpolation", build_span(start_offset, start_line, start_column))
          end
          Token.new(
            kind,
            processed_bytes,
            build_span(start_offset, start_line, start_column)
          )
        end

        # Lex backtick command literal: ` ... `
        # Behaves like a string with optional interpolation (#{ ... }).
        # Emits StringInterpolation when interpolation is present, otherwise String.
        private def lex_backtick
          start_offset, start_line, start_column = capture_position
          advance # opening backtick
          from = @offset
          has_interpolation = false
          has_escapes = false

          # Scan ahead to detect interpolation and choose fast/slow path
          scan_offset = @offset
          brace_depth = 0
          while scan_offset < @rope.size
            byte = @rope.bytes[scan_offset]

            if brace_depth == 0 && byte == BACKTICK
              break
            end

            if byte == HASH && scan_offset + 1 < @rope.size && @rope.bytes[scan_offset + 1] == LEFT_BRACE && brace_depth == 0
              has_interpolation = true
              brace_depth = 1
              scan_offset += 2
              next
            end

            if brace_depth > 0
              if byte == LEFT_BRACE
                brace_depth += 1
              elsif byte == RIGHT_BRACE
                brace_depth -= 1
              end
              scan_offset += 1
              next
            end

            if byte == '\\'.ord.to_u8
              has_escapes = true
            end
            scan_offset += 1
          end

          if !has_escapes
            brace_depth_fast = 0
            while @offset < @rope.size
              if brace_depth_fast == 0 && current_byte == BACKTICK
                break
              end

              if brace_depth_fast == 0 && current_byte == HASH && @offset + 1 < @rope.size && @rope.bytes[@offset + 1] == LEFT_BRACE
                # Peek immediately after '#{' for heredoc opener
                if @offset + 4 < @rope.size && @rope.bytes[@offset + 2] == '<'.ord.to_u8 && @rope.bytes[@offset + 3] == '<'.ord.to_u8 && @rope.bytes[@offset + 4] == '-'.ord.to_u8
                  heredoc_inside_interpolation = true
                end
                brace_depth_fast = 1
                advance(2)
                next
              elsif brace_depth_fast > 0
                brace_depth_fast += 1 if current_byte == LEFT_BRACE
                brace_depth_fast -= 1 if current_byte == RIGHT_BRACE
              end

              advance
            end
            advance if @offset < @rope.size # closing backtick

            kind = has_interpolation ? Token::Kind::StringInterpolation : Token::Kind::String
            return Token.new(
              kind,
              @rope.bytes[from...@offset - 1],
              build_span(start_offset, start_line, start_column)
            )
          end

          # Slow path (escapes present): copy bytes while tracking interpolation braces
          processed = Bytes.new(scan_offset - from)
          buffer = IO::Memory.new
          brace_depth_processed = 0
          while @offset < @rope.size
            break if brace_depth_processed == 0 && current_byte == BACKTICK

            if brace_depth_processed == 0 && current_byte == HASH && @offset + 1 < @rope.size && @rope.bytes[@offset + 1] == LEFT_BRACE
              buffer.write_byte(current_byte)
              advance
              buffer.write_byte(current_byte)
              advance
              brace_depth_processed += 1
              next
            elsif brace_depth_processed > 0
              if current_byte == LEFT_BRACE
                brace_depth_processed += 1
              elsif current_byte == RIGHT_BRACE
                brace_depth_processed -= 1
              end
              buffer.write_byte(current_byte)
              advance
              next
            end

            if current_byte == '\\'.ord.to_u8
              advance
              if @offset >= @rope.size
                break
              end
              escaped = current_byte
              buffer.write_byte(escaped)
              advance
              next
            else
              buffer.write_byte(current_byte)
              advance
            end
          end

          advance if @offset < @rope.size # consume closing backtick
          processed = buffer.to_slice
          @processed_strings << processed
          kind = has_interpolation ? Token::Kind::StringInterpolation : Token::Kind::String
          Token.new(
            kind,
            processed,
            build_span(start_offset, start_line, start_column)
          )
        end

        # Phase 57: Check if '/' can be start of regex literal based on context
        private def can_be_regex? : Bool
          # At statement start (after newline/semicolon), slash always begins regex
          return true if @at_statement_start

          # Key insight: whitespace before '/' changes interpretation
          # `foo/bar` -> division (no whitespace)
          # `foo /bar/` -> method call with regex argument (whitespace before /)
          # `x / y` -> division (whitespace on BOTH sides)
          # `b // c` -> floor division (next char is '/')
          # So if whitespace precedes '/' after an identifier, check what follows
          if @whitespace_before && @last_token_kind == Token::Kind::Identifier
            # Peek at char after '/' to distinguish regex from division
            next_byte = peek_byte(1)
            # If next char is '/' -> floor division operator (//) or (//=), not regex
            return false if next_byte == '/'.ord.to_u8
            # Phase 103M: If next char is '=' -> /= compound assignment, not regex
            return false if next_byte == '='.ord.to_u8
            # If whitespace after '/' too -> division (balanced spacing)
            # If no whitespace after '/' -> regex (method call arg)
            return true unless next_byte && whitespace?(next_byte)
          end

          # Regex can appear after operators, keywords, delimiters, or at start
          # Regex CANNOT appear after identifiers, numbers, closing brackets, or literals
          case @last_token_kind
          when nil
            # Beginning of input
            true
          when Token::Kind::Identifier, Token::Kind::Number, Token::Kind::String,
               Token::Kind::Char, Token::Kind::Symbol, Token::Kind::InstanceVar,
               Token::Kind::RParen, Token::Kind::RBracket, Token::Kind::RBrace,
               Token::Kind::True, Token::Kind::False, Token::Kind::Nil,
               Token::Kind::Self, Token::Kind::Regex,
               Token::Kind::Def, Token::Kind::Macro
            # After these, '/' is division
            false
          else
            # After operators, keywords, delimiters - '/' can be regex
            true
          end
        end

        # Phase 57: Regex literals (/pattern/flags)
        private def lex_regex
          start_offset, start_line, start_column = capture_position
          advance  # Skip opening /

          # Read pattern until closing / (handling \/ escape)
          buffer = IO::Memory.new

          while @offset < @rope.size && current_byte != '/'.ord.to_u8
            if current_byte == '\\'.ord.to_u8 && @offset + 1 < @rope.size
              # Escape sequence - preserve it for regex engine
              buffer.write_byte current_byte
              advance
              buffer.write_byte current_byte
              advance
            else
              buffer.write_byte current_byte
              advance
            end
          end

          # Skip closing /
          if @offset < @rope.size && current_byte == '/'.ord.to_u8
            advance
          end

          # Read optional flags (i, m, x, s, etc.)
          # Include flags in the buffer as well
          if @offset < @rope.size && ascii_letter?(current_byte)
            buffer.write_byte '/'.ord.to_u8  # Separator between pattern and flags
            while @offset < @rope.size && ascii_letter?(current_byte)
              buffer.write_byte current_byte
              advance
            end
          end

          # Store pattern + flags together
          processed_bytes = buffer.to_slice
          @processed_strings << processed_bytes

          Token.new(
            Token::Kind::Regex,
            processed_bytes,
            build_span(start_offset, start_line, start_column)
          )
        end

        # Phase 56: Character literals ('a', '\n', etc.)
        private def lex_char
          start_offset, start_line, start_column = capture_position
          advance  # Skip opening '

          # Character literals must have exactly one character or escape sequence
          if @offset >= @rope.size
            # TODO: Error - empty character literal
            return Token.new(Token::Kind::Char, Slice(UInt8).new(0), build_span(start_offset, start_line, start_column))
          end

          # Check if it's an escape sequence
          if current_byte == '\\'.ord.to_u8 && @offset + 1 < @rope.size
            # Process escape sequence
            buffer = IO::Memory.new
            advance  # Skip backslash

            case current_byte
            when 'n'.ord.to_u8
              buffer.write_byte '\n'.ord.to_u8
            when 't'.ord.to_u8
              buffer.write_byte '\t'.ord.to_u8
            when 'r'.ord.to_u8
              buffer.write_byte '\r'.ord.to_u8
            when '\\'.ord.to_u8
              buffer.write_byte '\\'.ord.to_u8
            when '\''.ord.to_u8
              buffer.write_byte '\''.ord.to_u8
            when 'u'.ord.to_u8
              # Phase 58: Unicode escapes \uXXXX or \u{XXXX}
              advance
              if current_byte == '{'.ord.to_u8
                # Variable length \u{X...XXXXXX}
                advance  # Skip '{'
                codepoint = parse_unicode_hex_digits('}'.ord.to_u8)
                if codepoint
                  write_utf8(buffer, codepoint)
                else
                  # Invalid Unicode escape - keep as is
                  buffer.write_byte '\\'.ord.to_u8
                  buffer.write_byte 'u'.ord.to_u8
                end
              else
                # Fixed length \uXXXX (4 hex digits)
                codepoint = parse_unicode_hex_fixed(4)
                if codepoint
                  write_utf8(buffer, codepoint)
                else
                  # Invalid Unicode escape - keep as is
                  buffer.write_byte '\\'.ord.to_u8
                  buffer.write_byte 'u'.ord.to_u8
                end
              end
              # Don't advance again - helper methods already did
              # Jump directly to closing quote check
              if @offset < @rope.size && current_byte == SINGLE_QUOTE
                advance
              end

              # Store processed character
              processed_bytes = buffer.to_slice
              @processed_strings << processed_bytes

              return Token.new(
                Token::Kind::Char,
                processed_bytes,
                build_span(start_offset, start_line, start_column)
              )
            when 'x'.ord.to_u8
              # Phase 59: Hex escapes \xXX (2 hex digits)
              advance
              byte_value = parse_unicode_hex_fixed(2)
              if byte_value
                buffer.write_byte byte_value.to_u8
              else
                # Invalid hex escape - keep as is
                buffer.write_byte '\\'.ord.to_u8
                buffer.write_byte 'x'.ord.to_u8
              end
              # Don't advance again - helper method already did
              # Jump directly to closing quote check
              if @offset < @rope.size && current_byte == SINGLE_QUOTE
                advance
              end

              # Store processed character
              processed_bytes = buffer.to_slice
              @processed_strings << processed_bytes

              return Token.new(
                Token::Kind::Char,
                processed_bytes,
                build_span(start_offset, start_line, start_column)
              )
            else
              # Phase 62: Check for octal escapes \NNN (1-3 octal digits)
              if octal_digit?(current_byte)
                byte_value = parse_octal_fixed(3)
                if byte_value
                  buffer.write_byte byte_value.to_u8
                else
                  # Should not happen if octal_digit? returned true
                  buffer.write_byte '\\'.ord.to_u8
                  buffer.write_byte current_byte
                end
                # Don't advance again - helper method already did
                # Jump directly to closing quote check
                if @offset < @rope.size && current_byte == SINGLE_QUOTE
                  advance
                end

                # Store processed character
                processed_bytes = buffer.to_slice
                @processed_strings << processed_bytes

                return Token.new(
                  Token::Kind::Char,
                  processed_bytes,
                  build_span(start_offset, start_line, start_column)
                )
              else
                # Unknown escape - keep as is
                buffer.write_byte '\\'.ord.to_u8
                buffer.write_byte current_byte
              end
            end
            advance

            # Expect closing '
            if @offset < @rope.size && current_byte == SINGLE_QUOTE
              advance
            end

            # Store processed character
            processed_bytes = buffer.to_slice
            @processed_strings << processed_bytes

            return Token.new(
              Token::Kind::Char,
              processed_bytes,
              build_span(start_offset, start_line, start_column)
            )
          else
            # Simple character - consume full UTF-8 codepoint until closing quote
            from = @offset
            bytes_consumed = 0
            # Consume first byte
            advance
            bytes_consumed += 1
            # Consume continuation bytes (max 3 more) until we hit closing quote or rope end
            while @offset < @rope.size && current_byte != SINGLE_QUOTE && bytes_consumed < 4
              advance
              bytes_consumed += 1
            end

            # Expect closing '
            if @offset < @rope.size && current_byte == SINGLE_QUOTE
              advance
            end

            return Token.new(
              Token::Kind::Char,
              @rope.bytes[from...from + bytes_consumed],
              build_span(start_offset, start_line, start_column)
            )
          end
        end

        private def lex_comment
          start_offset, start_line, start_column = capture_position
          from = @offset
          while @offset < @rope.size && current_byte != NEWLINE
            # Stop before {% or {{ sequences — these are macro control/expression
            # boundaries that must remain as separate tokens for macro body parsing.
            # Without this, `{% begin %}...#{...}{% end %}` would swallow {% end %}
            # inside the comment and cause "Expected {% end %}" errors.
            if current_byte == '{'.ord.to_u8 && @offset + 1 < @rope.size
              next_b = @rope.bytes[@offset + 1]
              break if next_b == '%'.ord.to_u8 || next_b == '{'.ord.to_u8
            end
            advance
          end
          Token.new(
            Token::Kind::Comment,
            @rope.bytes[from...@offset],
            build_span(start_offset, start_line, start_column)
          )
        end

        # Tokenize '{{' as MacroExprStart
        private def lex_macro_expr_start
          start_offset, start_line, start_column = capture_position
          advance(2)
          Token.new(
            Token::Kind::MacroExprStart,
            @rope.bytes[start_offset...@offset],
            build_span(start_offset, start_line, start_column)
          )
        end

        # Tokenize '}}' as MacroExprEnd
        private def lex_macro_expr_end
          start_offset, start_line, start_column = capture_position
          advance(2)
          Token.new(
            Token::Kind::MacroExprEnd,
            @rope.bytes[start_offset...@offset],
            build_span(start_offset, start_line, start_column)
          )
        end

        private def lex_operator
          start_offset, start_line, start_column = capture_position
          from = @offset

          # Read first character
          first = current_byte

          # Phase 57: Check if '/' could be start of regex literal
          if first == '/'.ord.to_u8 && can_be_regex?
            return lex_regex
          end

          advance

          # Determine token kind based on operator
          kind : Token::Kind = case first
          when '+'.ord.to_u8
            # Check for +=
            if @offset < @rope.size && current_byte == '='.ord.to_u8
              advance
              Token::Kind::PlusEq  # Phase 20: Compound assignment
            else
              Token::Kind::Plus
            end
          when '-'.ord.to_u8
            # Check for -> or -=
            if @offset < @rope.size && current_byte == '>'.ord.to_u8
              advance
              Token::Kind::ThinArrow  # Phase 74: Proc literal
            elsif @offset < @rope.size && current_byte == '='.ord.to_u8
              advance
              Token::Kind::MinusEq  # Phase 20: Compound assignment
            else
              Token::Kind::Minus
            end
          when '*'.ord.to_u8
            # Check for ** or *= or **=
            if @offset < @rope.size && current_byte == '*'.ord.to_u8
              advance
              # Check for **=
              if @offset < @rope.size && current_byte == '='.ord.to_u8
                advance
                Token::Kind::StarStarEq  # Phase 20: Compound assignment
              else
                Token::Kind::StarStar  # Phase 19: Exponentiation
              end
            elsif @offset < @rope.size && current_byte == '='.ord.to_u8
              advance
              Token::Kind::StarEq  # Phase 20: Compound assignment
            else
              Token::Kind::Star
            end
          when '/'.ord.to_u8
            # Phase 78: Check for // and //= before /=
            if @offset < @rope.size && current_byte == '/'.ord.to_u8
              advance  # consume second /
              # Check for //=
              if @offset < @rope.size && current_byte == '='.ord.to_u8
                advance
                Token::Kind::FloorDivEq  # Phase 78: Floor division compound assignment
              else
                Token::Kind::FloorDiv  # Phase 78: Floor division
              end
            # Check for /=
            elsif @offset < @rope.size && current_byte == '='.ord.to_u8
              advance
              Token::Kind::SlashEq  # Phase 20: Compound assignment
            else
              Token::Kind::Slash
            end
          when '%'.ord.to_u8
            if @offset < @rope.size && current_byte == '}'.ord.to_u8
              advance
              Token::Kind::PercentRBrace
            elsif @offset < @rope.size && current_byte == '='.ord.to_u8
              advance
              Token::Kind::PercentEq  # Phase 20: Compound assignment
            else
              # Try to parse as percent literal (%(), %w(), %i(), etc)
              # Only when context allows starting a literal (not after 'def', etc.)
              # Note: we already advanced past '%', so we're at next char
              if percent_literal_allowed? && (token = scan_percent_literal(start_offset, start_line, start_column))
                return token
              else
                # Not a percent literal, treat as modulo operator
                Token::Kind::Percent  # Phase 18: Modulo operator
              end
            end
          when '('.ord.to_u8
            Token::Kind::LParen
          when ')'.ord.to_u8
            Token::Kind::RParen
          when '['.ord.to_u8
            Token::Kind::LBracket
          when ']'.ord.to_u8
            Token::Kind::RBracket
          when ','.ord.to_u8
            Token::Kind::Comma
          when ';'.ord.to_u8
            Token::Kind::Semicolon
          when ':'.ord.to_u8
            Token::Kind::Colon
          when '{'.ord.to_u8
            if @offset < @rope.size && current_byte == '%'.ord.to_u8
              advance
              Token::Kind::LBracePercent
            else
              Token::Kind::LBrace
            end
          when '}'.ord.to_u8
            Token::Kind::RBrace
          when '<'.ord.to_u8
            # Check for <<=, <<, <=>, or <=
            if @offset < @rope.size
              next_byte = current_byte
              if next_byte == '<'.ord.to_u8
                advance  # consume second '<'
                # Check for <<- (heredoc). Allow heredoc in expression contexts too
                # (assignment like `msg = <<-HERE`). Still keep recovery in scan_heredoc.
                if @offset < @rope.size && current_byte == '-'.ord.to_u8
                  advance  # consume '-'
                  # Try to scan heredoc
                  heredoc_token = scan_heredoc(start_offset, start_line, start_column)
                  if heredoc_token
                    return heredoc_token
                  else
                    Token::Kind::LShift  # fallback
                  end
                # Check for <<= (Phase 52)
                elsif @offset < @rope.size && current_byte == '='.ord.to_u8
                  advance  # consume '='
                  Token::Kind::LShiftEq
                else
                  Token::Kind::LShift
                end
              elsif next_byte == '='.ord.to_u8
                # Check for <=>
                advance  # consume '='
                if @offset < @rope.size && current_byte == '>'.ord.to_u8
                  advance  # consume '>'
                  Token::Kind::Spaceship  # Phase 48
                else
                  Token::Kind::LessEq
                end
              else
                Token::Kind::Less
              end
            else
              Token::Kind::Less
            end
          when '>'.ord.to_u8
            # Check for >>=, >>, or >=
            if @offset < @rope.size
              next_byte = current_byte
              if next_byte == '>'.ord.to_u8
                advance  # consume second '>'
                # Check for >>= (Phase 52)
                if @offset < @rope.size && current_byte == '='.ord.to_u8
                  advance  # consume '='
                  Token::Kind::RShiftEq
                else
                  Token::Kind::RShift  # Phase 22: Right shift
                end
              elsif next_byte == '='.ord.to_u8
                advance
                Token::Kind::GreaterEq
              else
                Token::Kind::Greater
              end
            else
              Token::Kind::Greater
            end
          when '='.ord.to_u8
            # Check for =~, =>, ===, and ==
            if @offset < @rope.size
              next_byte = current_byte
              if next_byte == '~'.ord.to_u8
                # Phase 80: =~ (regex match)
                advance
                Token::Kind::Match
              elsif next_byte == '>'.ord.to_u8
                advance
                Token::Kind::Arrow  # =>
              elsif next_byte == '='.ord.to_u8
                # Check for === (Phase 50)
                advance  # consume second '='
                if @offset < @rope.size && current_byte == '='.ord.to_u8
                  advance  # consume third '='
                  Token::Kind::EqEqEq  # ===
                else
                  Token::Kind::EqEq  # ==
                end
              else
                Token::Kind::Eq  # =
              end
            else
              Token::Kind::Eq
            end
          when '!'.ord.to_u8
            # Check for !~, !=
            if @offset < @rope.size
              next_byte = current_byte
              if next_byte == '~'.ord.to_u8
                # Phase 80: !~ (regex not match)
                advance
                Token::Kind::NotMatch
              elsif next_byte == '='.ord.to_u8
                advance
                Token::Kind::NotEq
              else
                # Phase 17: Logical not operator
                Token::Kind::Not
              end
            else
              Token::Kind::Not
            end
          when '&'.ord.to_u8
            # Phase 101: Check for &. (safe navigation vs block shorthand)
            # AmpDot is only safe navigation when there's NO whitespace before &
            # "obj&.method" → AmpDot (safe navigation)
            # "obj.try &.method" → Amp, then Operator (block shorthand)
            if @offset < @rope.size && current_byte == '.'.ord.to_u8
              # Check if there was whitespace before &
              had_whitespace_before = from > 0 && whitespace?(@rope.bytes[from - 1])

              if had_whitespace_before
                # Block shorthand context: don't create AmpDot
                Token::Kind::Amp
              else
                # Safe navigation: create AmpDot
                advance  # consume '.'
                Token::Kind::AmpDot
              end
            # Check for &&= and &&
            elsif @offset < @rope.size && current_byte == '&'.ord.to_u8
              advance  # consume second '&'
              # Check for &&= (Phase 51)
              if @offset < @rope.size && current_byte == '='.ord.to_u8
                advance  # consume '='
                Token::Kind::AndAndEq
              else
                Token::Kind::AndAnd
              end
            # Phase 89: Wrapping operators - check before &= and &
            # CRITICAL: Check &** before &* (longest match first!)
            elsif @offset < @rope.size && current_byte == '*'.ord.to_u8
              advance  # consume first '*'
              if @offset < @rope.size && current_byte == '*'.ord.to_u8
                advance  # consume second '*'
                # Check for &**=
                if @offset < @rope.size && current_byte == '='.ord.to_u8
                  advance  # consume '='
                  Token::Kind::AmpStarStarEq
                else
                  Token::Kind::AmpStarStar  # &**
                end
              elsif @offset < @rope.size && current_byte == '='.ord.to_u8
                advance  # consume '='
                Token::Kind::AmpStarEq  # &*=
              else
                Token::Kind::AmpStar  # &*
              end
            elsif @offset < @rope.size && current_byte == '+'.ord.to_u8
              advance  # consume '+'
              # Check for &+=
              if @offset < @rope.size && current_byte == '='.ord.to_u8
                advance  # consume '='
                Token::Kind::AmpPlusEq
              else
                Token::Kind::AmpPlus  # &+
              end
            elsif @offset < @rope.size && current_byte == '-'.ord.to_u8
              advance  # consume '-'
              # Check for &-=
              if @offset < @rope.size && current_byte == '='.ord.to_u8
                advance  # consume '='
                Token::Kind::AmpMinusEq
              else
                Token::Kind::AmpMinus  # &-
              end
            else
              # Check for &= (Phase 52)
              if @offset < @rope.size && current_byte == '='.ord.to_u8
                advance  # consume '='
                Token::Kind::AmpEq
              else
                # Phase 21: Bitwise AND
                Token::Kind::Amp
              end
            end
          when '|'.ord.to_u8
            # Check for ||= and ||
            if @offset < @rope.size && current_byte == '|'.ord.to_u8
              advance  # consume second '|'
              # Check for ||= (Phase 51)
              if @offset < @rope.size && current_byte == '='.ord.to_u8
                advance  # consume '='
                Token::Kind::OrOrEq
              else
                Token::Kind::OrOr
              end
            else
              # Check for |= (Phase 52)
              if @offset < @rope.size && current_byte == '='.ord.to_u8
                advance  # consume '='
                Token::Kind::PipeEq
              else
                # Phase 21: Bitwise OR
                Token::Kind::Pipe
              end
            end
          when '^'.ord.to_u8
            # Check for ^= (Phase 52)
            if @offset < @rope.size && current_byte == '='.ord.to_u8
              advance  # consume '='
              Token::Kind::CaretEq
            else
              # Phase 21: Bitwise XOR
              Token::Kind::Caret
            end
          when '~'.ord.to_u8
            # Phase 21: Bitwise NOT
            Token::Kind::Tilde
          when '.'.ord.to_u8
            # Check for .. and ...
            if @offset < @rope.size && current_byte == '.'.ord.to_u8
              advance  # consume second '.'
              # Check for third '.'
              if @offset < @rope.size && current_byte == '.'.ord.to_u8
                advance  # consume third '.'
                Token::Kind::DotDotDot  # ...
              else
                Token::Kind::DotDot  # ..
              end
            else
              # Standalone . - use generic Operator for now (member access)
              Token::Kind::Operator
            end
          when '?'.ord.to_u8
            # Check for ??=, ??, and ? (longest match first)
            if @offset < @rope.size && current_byte == '?'.ord.to_u8
              # Potential ?? or ??=
              advance  # consume second ?
              if @offset < @rope.size && current_byte == '='.ord.to_u8
                # Phase 82: ??= (nil-coalescing compound assignment)
                advance  # consume =
                Token::Kind::NilCoalesceEq
              else
                # Phase 81: ?? (nil-coalescing)
                Token::Kind::NilCoalesce
              end
            else
              # Phase 23: ? (ternary operator)
              Token::Kind::Question
            end
          else
            # Unknown operator - use generic fallback
            Token::Kind::Operator
          end

          Token.new(
            kind,
            @rope.bytes[from...@offset],
            build_span(start_offset, start_line, start_column)
          )
        end

        private def whitespace?(byte : UInt8) : Bool
          byte == SPACE || byte == TAB
        end

        private def identifier_start?(byte : UInt8) : Bool
          ascii_letter?(byte) || byte == UNDERSCORE
        end

        private def identifier_char?(byte : UInt8) : Bool
          ascii_letter?(byte) || ascii_number?(byte) || byte == UNDERSCORE
        end

        private def identifier_suffix?(byte : UInt8) : Bool
          byte == QUESTION || byte == EXCLAMATION
        end

        private def ascii_letter?(byte : UInt8) : Bool
          (byte >= 'a'.ord && byte <= 'z'.ord) || (byte >= 'A'.ord && byte <= 'Z'.ord)
        end

        private def ascii_number?(byte : UInt8) : Bool
          byte >= '0'.ord && byte <= '9'.ord
        end

        # Heuristic: percent literal can start only in expression-start contexts.
        # Minimal safeguard: disallow immediately after 'def' (method header), so
        # 'def %(x)' is parsed as operator method name, not a percent literal.
        private def percent_literal_allowed? : Bool
          @last_token_kind != Token::Kind::Def
        end

        # Phase PERCENT_LITERALS: Get closing delimiter for opening delimiter
        private def closing_delimiter(open : UInt8) : UInt8
          case open
          when '('.ord.to_u8 then ')'.ord.to_u8
          when '['.ord.to_u8 then ']'.ord.to_u8
          when '{'.ord.to_u8 then '}'.ord.to_u8
          when '<'.ord.to_u8 then '>'.ord.to_u8
          else open  # For | and other non-paired delimiters
          end
        end

        # Phase PERCENT_LITERALS: Scan percent literal (%(), %w(), %i(), etc)
        # Returns Token or nil (if not a percent literal)
        private def scan_percent_literal(start_offset : Int32, start_line : Int32, start_column : Int32) : Token?
          # Already consumed '%', peek next char
          return nil if @offset >= @rope.size

          next_byte = current_byte
          literal_type = Token::Kind::String  # default for %()
          array_type : Symbol? = nil  # :word_array for %w(), :symbol_array for %i()
          type_char : Char? = nil

          # Determine literal type
          case next_byte.chr
          when '(', '[', '{', '<', '|'
            # Plain percent literal: %(...) - creates string
            open_delim = next_byte
            close_delim = closing_delimiter(open_delim)
            advance  # consume opening delimiter
          when 'q', 'Q'
            type_char = next_byte.chr
            advance  # consume 'q' or 'Q'
            return nil if @offset >= @rope.size
            open_delim = current_byte
            return nil unless open_delim.chr.in?('(', '[', '{', '<', '|')
            close_delim = closing_delimiter(open_delim)
            literal_type = Token::Kind::String
            advance  # consume opening delimiter
          when 'r'
            # %r(...) - regex literal
            advance  # consume 'r'
            return nil if @offset >= @rope.size
            open_delim = current_byte
            return nil unless open_delim.chr.in?('(', '[', '{', '<', '|')
            close_delim = closing_delimiter(open_delim)
            advance  # consume opening delimiter

            # Capture pattern content with nesting
            regex_buffer = IO::Memory.new
            nesting_level = 0
            while @offset < @rope.size
              byte = current_byte
              if byte == close_delim
                if nesting_level == 0
                  advance  # consume closing delimiter
                  break
                else
                  nesting_level -= 1
                  regex_buffer.write_byte byte
                  advance
                end
              elsif byte == open_delim && open_delim != close_delim
                nesting_level += 1
                regex_buffer.write_byte byte
                advance
              elsif byte == NEWLINE
                regex_buffer.write_byte byte
                advance
                @line += 1
                @column = 1
              else
                # Preserve escapes as-is
                if byte == '\\'.ord.to_u8 && @offset + 1 < @rope.size
                  regex_buffer.write_byte byte
                  advance
                  regex_buffer.write_byte current_byte
                  advance
                else
                  regex_buffer.write_byte byte
                  advance
                end
              end
            end

            # Optional flags (i, m, x, s, etc.)
            if @offset < @rope.size && ascii_letter?(current_byte)
              regex_buffer.write_byte '/'.ord.to_u8
              while @offset < @rope.size && ascii_letter?(current_byte)
                regex_buffer.write_byte current_byte
                advance
              end
            end

            processed = regex_buffer.to_slice
            @processed_strings << processed
            return Token.new(Token::Kind::Regex, processed, build_span(start_offset, start_line, start_column))
          when 'w'
            # %w(...) - word array
            type_char = 'w'
            advance  # consume 'w'
            return nil if @offset >= @rope.size
            open_delim = current_byte
            return nil unless open_delim.chr.in?('(', '[', '{', '<', '|')
            close_delim = closing_delimiter(open_delim)
            array_type = :word_array
            literal_type = Token::Kind::LBracket  # Will generate array tokens
            advance  # consume opening delimiter
          when 'i'
            # %i(...) - symbol array
            type_char = 'i'
            advance  # consume 'i'
            return nil if @offset >= @rope.size
            open_delim = current_byte
            return nil unless open_delim.chr.in?('(', '[', '{', '<', '|')
            close_delim = closing_delimiter(open_delim)
            array_type = :symbol_array
            literal_type = Token::Kind::LBracket  # Will generate array tokens
            advance  # consume opening delimiter
          else
            # Not a percent literal, treat as modulo operator
            return nil
          end

          # Scan content with nesting support
          buffer = IO::Memory.new
          nesting_level = 0
          content_start = @offset
          content_end = @offset
          interpolation_depth = 0
          has_interpolation = false
          heredoc_inside_interpolation = false
          interpolation_allowed = array_type.nil? && literal_type == Token::Kind::String && type_char != 'q'

          while @offset < @rope.size
            byte = current_byte

            if interpolation_allowed
              if byte == HASH && @offset + 1 < @rope.size && @rope.bytes[@offset + 1] == LEFT_BRACE
                has_interpolation = true
                interpolation_depth += 1
              elsif interpolation_depth > 0
                if byte == '<'.ord.to_u8 && @offset + 2 < @rope.size && @rope.bytes[@offset + 1] == '<'.ord.to_u8 && @rope.bytes[@offset + 2] == '-'.ord.to_u8
                  heredoc_inside_interpolation = true
                  debug("heredoc in percent literal interpolation detected")
                end
                interpolation_depth += 1 if byte == LEFT_BRACE
                interpolation_depth -= 1 if byte == RIGHT_BRACE
              end
            end

            # Check for closing delimiter
            if byte == close_delim
              if nesting_level == 0
                content_end = @offset
                advance  # consume closing delimiter
                break
              else
                nesting_level -= 1
                buffer.write_byte byte
                advance
              end
            # Check for opening delimiter (for nesting)
            elsif byte == open_delim && open_delim != close_delim  # Don't nest | |
              nesting_level += 1
              buffer.write_byte byte
              advance
            # Handle newlines
            elsif byte == NEWLINE
              buffer.write_byte byte
              advance
              @line += 1
              @column = 1
            # Regular character
            else
              buffer.write_byte byte
              advance
            end
          end

          span = build_span(start_offset, start_line, start_column)

          if interpolation_allowed && heredoc_inside_interpolation
            emit_diagnostic("heredoc cannot be used inside interpolation", span)
          end

          if interpolation_allowed && has_interpolation
            content_slice = @rope.bytes[content_start...content_end]
            return Token.new(Token::Kind::StringInterpolation, content_slice, span)
          end

          # Get the slice from rope for the entire literal (including delimiters)
          slice = @rope.bytes[start_offset...@offset]
          span = build_span(start_offset, start_line, start_column)

          # For arrays, we need to return special token or handle differently
          # For now, return as String token with slice
          # Parser will need to handle array splitting and use buffer content
          Token.new(literal_type, slice, span)
        end

        # Phase 53: Hexadecimal digit check (0-9, a-f, A-F)
        private def hex_digit?(byte : UInt8) : Bool
          ascii_number?(byte) || (byte >= 'a'.ord && byte <= 'f'.ord) || (byte >= 'A'.ord && byte <= 'F'.ord)
        end

        # Phase 53: Binary digit check (0-1)
        private def binary_digit?(byte : UInt8) : Bool
          byte == '0'.ord || byte == '1'.ord
        end

        # Phase 53: Octal digit check (0-7)
        private def octal_digit?(byte : UInt8) : Bool
          byte >= '0'.ord && byte <= '7'.ord
        end

        # Phase 58: Convert hex digit to numeric value (0-15)
        private def hex_value(byte : UInt8) : Int32
          if byte >= '0'.ord && byte <= '9'.ord
            byte.to_i32 - '0'.ord.to_i32
          elsif byte >= 'a'.ord && byte <= 'f'.ord
            byte.to_i32 - 'a'.ord.to_i32 + 10
          elsif byte >= 'A'.ord && byte <= 'F'.ord
            byte.to_i32 - 'A'.ord.to_i32 + 10
          else
            0
          end
        end

        # Phase 58: Parse variable length Unicode hex digits (1-6 digits) until terminator
        # Returns codepoint or nil if invalid
        private def parse_unicode_hex_digits(terminator : UInt8) : Int32?
          codepoint = 0
          digit_count = 0

          while @offset < @rope.size && digit_count < 6
            if current_byte == terminator
              advance  # Skip terminator
              return digit_count > 0 ? codepoint : nil
            end

            unless hex_digit?(current_byte)
              return nil  # Invalid hex digit
            end

            codepoint = codepoint * 16 + hex_value(current_byte)
            digit_count += 1
            advance
          end

          # Should have found terminator
          if @offset < @rope.size && current_byte == terminator
            advance
            digit_count > 0 ? codepoint : nil
          else
            nil  # Missing terminator or too many digits
          end
        end

        # Phase 58: Parse fixed count of Unicode hex digits
        # Returns codepoint or nil if invalid
        private def parse_unicode_hex_fixed(count : Int32) : Int32?
          codepoint = 0

          count.times do
            return nil if @offset >= @rope.size
            return nil unless hex_digit?(current_byte)

            codepoint = codepoint * 16 + hex_value(current_byte)
            advance
          end

          codepoint
        end

        # Phase 62: Parse 1-3 octal digits (\NNN format)
        # Returns byte value (0-255) or nil if invalid
        private def parse_octal_fixed(max_count : Int32) : Int32?
          value = 0
          count = 0

          while count < max_count && @offset < @rope.size && octal_digit?(current_byte)
            digit_value = current_byte.to_i32 - '0'.ord.to_i32
            value = value * 8 + digit_value
            count += 1
            advance
          end

          # Return nil if no digits found, otherwise return value
          count > 0 ? value : nil
        end

        # Phase 58: Write Unicode codepoint as UTF-8 to buffer
        private def write_utf8(buffer : IO::Memory, codepoint : Int32)
          if codepoint < 0x80
            # 1-byte sequence (ASCII)
            buffer.write_byte codepoint.to_u8
          elsif codepoint < 0x800
            # 2-byte sequence
            buffer.write_byte (0xC0 | (codepoint >> 6)).to_u8
            buffer.write_byte (0x80 | (codepoint & 0x3F)).to_u8
          elsif codepoint < 0x10000
            # 3-byte sequence
            buffer.write_byte (0xE0 | (codepoint >> 12)).to_u8
            buffer.write_byte (0x80 | ((codepoint >> 6) & 0x3F)).to_u8
            buffer.write_byte (0x80 | (codepoint & 0x3F)).to_u8
          elsif codepoint < 0x110000
            # 4-byte sequence
            buffer.write_byte (0xF0 | (codepoint >> 18)).to_u8
            buffer.write_byte (0x80 | ((codepoint >> 12) & 0x3F)).to_u8
            buffer.write_byte (0x80 | ((codepoint >> 6) & 0x3F)).to_u8
            buffer.write_byte (0x80 | (codepoint & 0x3F)).to_u8
          end
          # If codepoint is out of range, write nothing (silently ignore)
        end

        SPACE        = 0x20_u8
        TAB          = 0x09_u8
        NEWLINE      = 0x0A_u8
        DOUBLE_QUOTE = '"'.ord.to_u8
        SINGLE_QUOTE = '\''.ord.to_u8  # Phase 56: character literals
        BACKTICK     = '`'.ord.to_u8    # Command literal delimiter
        HASH         = '#'.ord.to_u8
        UNDERSCORE   = '_'.ord.to_u8
        QUESTION     = '?'.ord.to_u8
        EXCLAMATION  = '!'.ord.to_u8
        AT_SIGN      = '@'.ord.to_u8
        DOLLAR_SIGN  = '$'.ord.to_u8  # Phase 75: for global variables
        LEFT_BRACE   = '{'.ord.to_u8  # Phase 8: for interpolation detection
        RIGHT_BRACE  = '}'.ord.to_u8
        COLON        = ':'.ord.to_u8  # Phase 16: for symbol literals

        private def build_span(start_offset, start_line, start_column)
          Span.new(
            start_offset,
            @offset,
            start_line,
            start_column,
            @line,
            @column
          )
        end

        private def current_point_span
          Span.new(
            @offset,
            @offset,
            @line,
            @column,
            @line,
            @column
          )
        end

        # Scan heredoc: <<-DELIMITER ... DELIMITER
        # Returns String token or nil if not valid heredoc
        private def scan_heredoc(start_offset : Int32, start_line : Int32, start_column : Int32) : Token?
          # At this point we've consumed <<-
          # Next should be identifier (delimiter)
          debug "[HEREDOC] scan_heredoc called, offset=#{@offset}, size=#{@rope.size}"
          if @offset >= @rope.size
            debug "[HEREDOC] returning nil: offset >= size"
            return nil
          end
          debug "[HEREDOC] current_byte=#{current_byte} (#{current_byte.chr})"
          delimiter : String
          # Heredoc inside interpolation is not allowed
          if @macro_expr_depth > 0
            emit_diagnostic("heredoc cannot be used inside interpolation", Span.new(start_offset, @offset, start_line, start_column, @line, @column))
            return nil
          end
          if current_byte == SINGLE_QUOTE || current_byte == DOUBLE_QUOTE
            quote = current_byte
            advance  # consume opening quote
            delimiter_start = @offset
            while @offset < @rope.size && current_byte != quote
              advance
            end
            if @offset >= @rope.size
              debug "[HEREDOC] returning nil: unterminated quoted delimiter"
              return nil
            end
            delimiter_end = @offset
            delimiter = String.new(@rope.slice(delimiter_start...delimiter_end))
            advance  # consume closing quote
          else
            unless is_identifier_start?(current_byte)
              debug "[HEREDOC] returning nil: not identifier start"
              return nil
            end

            # Read delimiter
            delimiter_start = @offset
            while @offset < @rope.size && is_identifier_part?(current_byte)
              advance
            end
            delimiter_end = @offset
            delimiter = String.new(@rope.slice(delimiter_start...delimiter_end))
          end
          debug "[HEREDOC] delimiter='#{delimiter}'"

          # Skip to end of line (heredoc content starts on NEXT line)
          while @offset < @rope.size && current_byte != '\n'.ord.to_u8
            advance
          end
          if @offset >= @rope.size
            debug "[HEREDOC] returning nil: EOF before newline after delimiter"
            emit_diagnostic("Unterminated heredoc", Span.new(start_offset, @offset, start_line, start_column, @line, @column))
            return nil
          end
          advance  # consume newline
          debug "[HEREDOC] starting content scan at offset #{@offset}"

          # Accumulate content until we find delimiter on its own line
          content = IO::Memory.new

          min_indent_seen = Int32::MAX
          closing_indent = 0

          loop do
            debug "[HEREDOC] loop iteration, offset=#{@offset}"
            line_start = @offset

            # Capture indentation (allowed to be zero with <<-)
            while @offset < @rope.size && (current_byte == ' '.ord.to_u8 || current_byte == '\t'.ord.to_u8)
              advance
            end
            indent = @offset - line_start
            line_has_content = !(current_byte == '\n'.ord.to_u8 || current_byte == '\r'.ord.to_u8)
            min_indent_seen = indent if line_has_content && indent < min_indent_seen

            # Check if this line starts with delimiter (after whitespace)
            matches_delimiter = true
            debug "[HEREDOC] checking delimiter match starting at offset #{@offset}"
            delimiter.each_byte do |byte|
              if @offset >= @rope.size || current_byte != byte
                matches_delimiter = false
                debug "[HEREDOC] delimiter mismatch at offset #{@offset}, expected #{byte}, got #{@offset >= @rope.size ? "EOF" : current_byte}"
                break
              end
              advance
            end

            if matches_delimiter
              debug "[HEREDOC] found matching delimiter at #{@offset}, checking termination"
              # Check that delimiter is followed by newline or EOF
              if @offset >= @rope.size || current_byte == '\n'.ord.to_u8 || current_byte == '\r'.ord.to_u8
                debug "[HEREDOC] delimiter properly terminated, breaking"
                closing_indent = indent
                # Found end delimiter, consume newline if present
                if @offset < @rope.size && (current_byte == '\n'.ord.to_u8 || current_byte == '\r'.ord.to_u8)
                  advance
                  if @offset < @rope.size && current_byte == '\n'.ord.to_u8  # handle \r\n
                    advance
                  end
                end
                break  # Done with heredoc
              end
              debug "[HEREDOC] delimiter not terminated (followed by #{current_byte.chr})"
            end

            # Not end delimiter, rewind to line start and consume the line
            debug "[HEREDOC] rewinding to line_start=#{line_start}"
            @offset = line_start
            while @offset < @rope.size && current_byte != '\n'.ord.to_u8
              content.write_byte(current_byte)
              advance
            end

            if @offset >= @rope.size
              debug "[HEREDOC] returning nil: unterminated heredoc (EOF before delimiter)"
              emit_diagnostic("Unterminated heredoc: can't find \"#{delimiter}\" anywhere before the end of file", Span.new(start_offset, @offset, start_line, start_column, @line, @column))
              return nil  # Unterminated heredoc
            end

            # Consume newline
            debug "[HEREDOC] consumed line, adding newline"
            content.write_byte('\n'.ord.to_u8)
            advance
            @line += 1
            @column = 0
          end

          # Emit diagnostic if content indent is less than closing delimiter indent
          if min_indent_seen < closing_indent
            emit_diagnostic("heredoc line must have an indent greater than or equal to #{closing_indent}", Span.new(start_offset, @offset, start_line, start_column, @line, @column))
          end

          # Create string token
          # Token expects (kind, slice, span)
          debug "[HEREDOC] creating token, content.size=#{content.size}"
          content_slice = Slice(UInt8).new(content.size)
          content.to_slice.copy_to(content_slice)

          debug "[HEREDOC] returning String token"
          Token.new(
            Token::Kind::String,
            content_slice,
            Span.new(
              start_offset,
              @offset,
              start_line,
              start_column,
              @line,
              @column
            )
          )
        end

        private def is_identifier_start?(byte : UInt8) : Bool
          (byte >= 'a'.ord.to_u8 && byte <= 'z'.ord.to_u8) ||
            (byte >= 'A'.ord.to_u8 && byte <= 'Z'.ord.to_u8) ||
            byte == '_'.ord.to_u8
        end

        private def is_identifier_part?(byte : UInt8) : Bool
          is_identifier_start?(byte) ||
            (byte >= '0'.ord.to_u8 && byte <= '9'.ord.to_u8)
        end
      end

      # Heuristic: percent literal can start only in expression-start contexts.
      # Minimal safeguard: disallow immediately after 'def' (method header), so
      # 'def %(x)' is parsed as operator method name, not a percent literal.
      private def percent_literal_allowed? : Bool
        @last_token_kind != Token::Kind::Def
      end

    end
  end
end
