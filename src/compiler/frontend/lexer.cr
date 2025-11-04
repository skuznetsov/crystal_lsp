require "./rope"
require "./lexer/token"
require "./string_pool"

module CrystalV2
  module Compiler
    module Frontend
      class Lexer
        @last_token_kind : Token::Kind?  # Phase 57: for regex vs division disambiguation
        @string_pool : StringPool  # String interning for memory optimization
        getter string_pool : StringPool  # Week 1 Day 2: expose for parser generic type interning

        def initialize(source : String)
          @rope = Rope.new(source)
          @offset = 0
          @line = 1
          @column = 1
          @processed_strings = [] of Bytes  # Phase 54: storage for escape-processed strings
          @last_token_kind = nil  # Phase 57: for regex vs division disambiguation
          @string_pool = StringPool.new  # String interning for memory optimization
        end

        private def debug(message : String)
          if ENV["LEXER_DEBUG"]?
            STDERR.puts message
          end
        end

        def each_token(&block : Token ->)
          while token = next_token
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
          when byte == SINGLE_QUOTE
            # Phase 56: Character literals
            lex_char
          when byte == HASH
            lex_comment
          else
            lex_operator
          end

          # Phase 57: Track last significant token for regex vs division disambiguation
          # Whitespace/Newline are not significant for this purpose
          unless token.kind == Token::Kind::Whitespace || token.kind == Token::Kind::Newline
            @last_token_kind = token.kind
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

          # Intern identifier slice for memory deduplication
          slice = @string_pool.intern(@rope.bytes[from...@offset])

          # Check if this is a keyword
          kind = case String.new(slice)
          when "if"     then Token::Kind::If
          when "elsif"  then Token::Kind::Elsif
          when "else"   then Token::Kind::Else
          when "end"    then Token::Kind::End
          when "while"  then Token::Kind::While
          when "loop"   then Token::Kind::Loop  # Phase 83
          when "spawn"  then Token::Kind::Spawn  # Phase 84
          when "do"     then Token::Kind::Do
          when "then"   then Token::Kind::Then
          when "def"    then Token::Kind::Def
          when "macro"  then Token::Kind::Macro  # Phase 100
          when "class"  then Token::Kind::Class
          when "true"   then Token::Kind::True
          when "false"  then Token::Kind::False
          when "nil"    then Token::Kind::Nil
          when "return" then Token::Kind::Return
          when "self"   then Token::Kind::Self
          when "super"  then Token::Kind::Super  # Phase 39
          when "previous_def" then Token::Kind::PreviousDef  # Phase 96
          when "typeof" then Token::Kind::Typeof  # Phase 40
          when "sizeof" then Token::Kind::Sizeof  # Phase 41
          when "pointerof" then Token::Kind::Pointerof  # Phase 42
          when "uninitialized" then Token::Kind::Uninitialized  # Phase 85
          when "offsetof" then Token::Kind::Offsetof  # Phase 86
          when "alignof" then Token::Kind::Alignof  # Phase 88
          when "instance_alignof" then Token::Kind::InstanceAlignof  # Phase 88
          when "asm" then Token::Kind::Asm  # Phase 95
          when "yield"  then Token::Kind::Yield
          when "case"   then Token::Kind::Case
          when "when"   then Token::Kind::When
          when "select" then Token::Kind::Select  # Phase 90A
          when "break"  then Token::Kind::Break
          when "next"   then Token::Kind::Next
          when "unless" then Token::Kind::Unless  # Phase 24
          when "until"  then Token::Kind::Until   # Phase 25
          when "for"    then Token::Kind::For     # Phase 99
          when "begin"  then Token::Kind::Begin   # Phase 28
          when "rescue" then Token::Kind::Rescue  # Phase 29
          when "ensure" then Token::Kind::Ensure  # Phase 29
          # "raise" is NOT a keyword in Crystal - it's a regular method call
          when "require" then Token::Kind::Require  # Phase 65
          when "with" then Token::Kind::With  # Phase 67
          when "module" then Token::Kind::Module  # Phase 31
          when "include" then Token::Kind::Include  # Phase 31
          when "extend" then Token::Kind::Extend  # Phase 31
          when "struct" then Token::Kind::Struct  # Phase 32
          when "union" then Token::Kind::Union  # Phase 97
          when "enum" then Token::Kind::Enum  # Phase 33
          when "alias" then Token::Kind::Alias  # Phase 34
          when "annotation" then Token::Kind::Annotation  # Phase 92
          when "abstract" then Token::Kind::Abstract  # Phase 36
          when "private" then Token::Kind::Private  # Phase 37
          when "protected" then Token::Kind::Protected  # Phase 37
          when "lib" then Token::Kind::Lib  # Phase 38
          when "fun" then Token::Kind::Fun  # Phase 64
          when "out" then Token::Kind::Out  # Phase 98
          when "as" then Token::Kind::As  # Phase 44
          when "as?" then Token::Kind::AsQuestion  # Phase 45
          when "is_a?" then Token::Kind::IsA  # Phase 93
          when "responds_to?" then Token::Kind::RespondsTo  # Phase 94
          when "in" then Token::Kind::In  # Phase 79
          when "of" then Token::Kind::Of  # Phase 91
          else
            Token::Kind::Identifier
          end

          Token.new(
            kind,
            slice,
            build_span(start_offset, start_line, start_column)
          )
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

          # Global variable must start with identifier character
          if @offset >= @rope.size || !identifier_start?(current_byte)
            # Invalid global variable - just $, return as operator
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

          # Check if followed by identifier start
          if @offset >= @rope.size || !identifier_start?(current_byte)
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
          number_kind ||= NumberKind::I32

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
          number_kind ||= NumberKind::I32

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
          number_kind ||= NumberKind::I32

          Token.new(
            Token::Kind::Number,
            @rope.bytes[from...@offset],
            build_span(start_offset, start_line, start_column),
            number_kind: number_kind
          )
        end

        # Phase 53: Extract number suffix parsing to helper
        # Phase 103J: Parse numeric type suffix (_i8, _i16, _i32, _i64, _i128, _u8, _u16, _u32, _u64, _u128, _f32, _f64)
        # Used by hex, binary, octal number parsers
        private def lex_number_suffix : NumberKind?
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

            return number_kind
          end

          nil
        end

        private def lex_string
          start_offset, start_line, start_column = capture_position
          advance # opening quote
          from = @offset
          has_interpolation = false
          has_escapes = false

          # Phase 54: Check if string contains escapes or interpolation
          # First pass: detect escapes/interpolation
          scan_offset = @offset
          while scan_offset < @rope.size && @rope.bytes[scan_offset] != DOUBLE_QUOTE
            byte = @rope.bytes[scan_offset]
            if byte == HASH && scan_offset + 1 < @rope.size && @rope.bytes[scan_offset + 1] == LEFT_BRACE
              has_interpolation = true
            elsif byte == '\\'.ord.to_u8
              has_escapes = true
            end
            scan_offset += 1
          end

          # If no escapes, use original fast path
          if !has_escapes
            while @offset < @rope.size && current_byte != DOUBLE_QUOTE
              advance
            end
            advance if @offset < @rope.size # closing quote

            kind = has_interpolation ? Token::Kind::StringInterpolation : Token::Kind::String
            return Token.new(
              kind,
              @rope.bytes[from...@offset - 1],
              build_span(start_offset, start_line, start_column)
            )
          end

          # Phase 54: Process escape sequences
          processed = Bytes.new(scan_offset - from)  # Allocate with estimated size
          buffer = IO::Memory.new

          while @offset < @rope.size && current_byte != DOUBLE_QUOTE
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
          Token.new(
            kind,
            processed_bytes,
            build_span(start_offset, start_line, start_column)
          )
        end

        # Phase 57: Check if '/' can be start of regex literal based on context
        private def can_be_regex? : Bool
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
               Token::Kind::Self, Token::Kind::Regex
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
            # Simple character - no escape
            from = @offset
            advance  # Consume the character

            # Expect closing '
            if @offset < @rope.size && current_byte == SINGLE_QUOTE
              advance
            end

            return Token.new(
              Token::Kind::Char,
              @rope.bytes[from...from + 1],
              build_span(start_offset, start_line, start_column)
            )
          end
        end

        private def lex_comment
          start_offset, start_line, start_column = capture_position
          from = @offset
          while @offset < @rope.size && current_byte != NEWLINE
            advance
          end
          Token.new(
            Token::Kind::Comment,
            @rope.bytes[from...@offset],
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
            # Check for %=
            if @offset < @rope.size && current_byte == '='.ord.to_u8
              advance
              Token::Kind::PercentEq  # Phase 20: Compound assignment
            else
              # Try to parse as percent literal (%(), %w(), %i(), etc)
              # Note: we already advanced past '%', so we're at next char
              if token = scan_percent_literal(start_offset, start_line, start_column)
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
            Token::Kind::LBrace
          when '}'.ord.to_u8
            Token::Kind::RBrace
          when '<'.ord.to_u8
            # Check for <<=, <<, <=>, or <=
            if @offset < @rope.size
              next_byte = current_byte
              if next_byte == '<'.ord.to_u8
                advance  # consume second '<'
                # Check for <<- (heredoc)
                if @offset < @rope.size && current_byte == '-'.ord.to_u8
                  advance  # consume '-'
                  # Try to scan heredoc
                  heredoc_token = scan_heredoc(start_offset, start_line, start_column)
                  if heredoc_token
                    return heredoc_token
                  else
                    # Not a valid heredoc, backtrack (TODO: proper error handling)
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

          # Determine literal type
          case next_byte.chr
          when '(', '[', '{', '<', '|'
            # Plain percent literal: %(...) - creates string
            open_delim = next_byte
            close_delim = closing_delimiter(open_delim)
            advance  # consume opening delimiter
          when 'w'
            # %w(...) - word array
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

          while @offset < @rope.size
            byte = current_byte

            # Check for closing delimiter
            if byte == close_delim
              if nesting_level == 0
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
        HASH         = '#'.ord.to_u8
        UNDERSCORE   = '_'.ord.to_u8
        QUESTION     = '?'.ord.to_u8
        EXCLAMATION  = '!'.ord.to_u8
        AT_SIGN      = '@'.ord.to_u8
        DOLLAR_SIGN  = '$'.ord.to_u8  # Phase 75: for global variables
        LEFT_BRACE   = '{'.ord.to_u8  # Phase 8: for interpolation detection
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
          debug "[HEREDOC] delimiter='#{delimiter}' (#{delimiter_start}...#{delimiter_end})"

          # Skip to end of line (heredoc content starts on NEXT line)
          while @offset < @rope.size && current_byte != '\n'.ord.to_u8
            advance
          end
          if @offset >= @rope.size
            debug "[HEREDOC] returning nil: EOF before newline after delimiter"
            return nil
          end
          advance  # consume newline
          debug "[HEREDOC] starting content scan at offset #{@offset}"

          # Accumulate content until we find delimiter on its own line
          content = IO::Memory.new

          loop do
            debug "[HEREDOC] loop iteration, offset=#{@offset}"
            line_start = @offset

            # Skip leading whitespace (for <<- syntax)
            while @offset < @rope.size && (current_byte == ' '.ord.to_u8 || current_byte == '\t'.ord.to_u8)
              advance
            end

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
              return nil  # Unterminated heredoc
            end

            # Consume newline
            debug "[HEREDOC] consumed line, adding newline"
            content.write_byte('\n'.ord.to_u8)
            advance
            @line += 1
            @column = 0
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
    end
  end
end
