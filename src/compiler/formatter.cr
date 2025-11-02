require "./frontend/lexer"

module CrystalV2
  module Compiler
    # Token-based formatter for Crystal code
    # Traverses token stream and emits formatted source with proper whitespace
    class Formatter
      @source : String
      @tokens : Array(Frontend::Token)
      @output : String::Builder
      @indent : Int32
      @at_line_start : Bool
      @last_was_newline : Bool
      @previous_token : Frontend::Token?

      # Format Crystal source code
      def self.format(source : String) : String
        new(source).format
      end

      def initialize(source : String)
        @source = source

        # Tokenize (comments are automatically included in token stream)
        lexer = Frontend::Lexer.new(source)

        @tokens = [] of Frontend::Token
        lexer.each_token do |token|
          @tokens << token
        end

        @output = String::Builder.new(source.bytesize)
        @indent = 0
        @at_line_start = true
        @last_was_newline = false
        @previous_token = nil
      end

      def format : String
        i = 0
        while i < @tokens.size
          current = @tokens[i]

          # Skip EOF
          break if current.kind == Frontend::Token::Kind::EOF

          # Handle newlines and indentation
          if current.kind == Frontend::Token::Kind::Newline
            emit_newline
            i += 1
            next
          end

          # Skip whitespace tokens (we generate our own)
          if current.kind == Frontend::Token::Kind::Whitespace
            i += 1
            next
          end

          # Find next non-whitespace token
          next_token = find_next_non_whitespace(i + 1)

          # Adjust indent BEFORE emitting indent for keywords that decrease it
          if @at_line_start && should_decrease_indent?(current)
            @indent -= 2
          end

          # Emit indentation if at line start
          if @at_line_start && current.kind != Frontend::Token::Kind::Newline
            emit_indent
            @at_line_start = false
          end

          # Emit current token
          emit_token(current)

          # Emit whitespace after current token
          if next_token
            emit_whitespace_after(current, next_token)
          end

          # Adjust indent AFTER emitting keyword that increases indent
          if should_increase_indent?(current, next_token)
            @indent += 2
          end

          # Track previous token for context-sensitive formatting
          @previous_token = current

          i += 1
        end

        @output.to_s
      end

      private def find_next_non_whitespace(start : Int32) : Frontend::Token?
        i = start
        while i < @tokens.size
          token = @tokens[i]
          return token unless token.kind == Frontend::Token::Kind::Whitespace
          i += 1
        end
        nil
      end

      private def emit_token(token : Frontend::Token)
        # Special case: Symbol after Identifier in type annotation context
        # Convert `:TypeName` to `: TypeName`
        if token.kind == Frontend::Token::Kind::Symbol &&
           @previous_token.try(&.kind) == Frontend::Token::Kind::Identifier
          span = token.span
          start_byte = span.start_offset
          end_byte = span.end_offset
          original_text = @source.byte_slice(start_byte, end_byte - start_byte)

          # Symbol text is like ":Int32" - split into ": Int32"
          if original_text.starts_with?(':')
            @output << ':'
            @output << ' '
            @output << original_text[1..-1]  # Type name without leading colon
            @last_was_newline = false
            return
          end
        end

        # Use span to get original text including delimiters (quotes, etc.)
        span = token.span
        start_byte = span.start_offset
        end_byte = span.end_offset

        # Extract original text from source
        original_text = @source.byte_slice(start_byte, end_byte - start_byte)
        @output << original_text

        @last_was_newline = false
      end

      private def emit_newline
        @output << '\n'
        @at_line_start = true
        @last_was_newline = true
      end

      private def emit_indent
        @indent.times { @output << ' ' }
      end

      private def emit_whitespace_after(current : Frontend::Token, next_token : Frontend::Token)
        # Skip whitespace before newlines or EOF
        return if next_token.kind == Frontend::Token::Kind::Newline
        return if next_token.kind == Frontend::Token::Kind::EOF

        # Helper to check if need space after keyword
        # Note: 'puts' is an Identifier, not a keyword, so handled separately
        need_space_after = case current.kind
        when Frontend::Token::Kind::Def, Frontend::Token::Kind::If,
             Frontend::Token::Kind::Unless, Frontend::Token::Kind::While,
             Frontend::Token::Kind::Until, Frontend::Token::Kind::Return,
             Frontend::Token::Kind::Class, Frontend::Token::Kind::Module,
             Frontend::Token::Kind::Struct, Frontend::Token::Kind::Elsif,
             Frontend::Token::Kind::Else, Frontend::Token::Kind::Require,
             Frontend::Token::Kind::Enum, Frontend::Token::Kind::Property,
             Frontend::Token::Kind::Getter, Frontend::Token::Kind::Setter,
             Frontend::Token::Kind::Include, Frontend::Token::Kind::Extend,
             Frontend::Token::Kind::Private, Frontend::Token::Kind::Protected,
             Frontend::Token::Kind::Alias, Frontend::Token::Kind::Case,
             Frontend::Token::Kind::When,
             Frontend::Token::Kind::Begin, Frontend::Token::Kind::Rescue,
             Frontend::Token::Kind::Ensure
          true
        else
          false
        end

        # Special case: identifier-like calls (puts, p, etc)
        is_identifier_call = current.kind == Frontend::Token::Kind::Identifier &&
                            next_token.kind != Frontend::Token::Kind::LParen &&
                            next_token.kind != Frontend::Token::Kind::Colon &&
                            next_token.kind != Frontend::Token::Kind::Symbol

        case {current.kind, next_token.kind}
        # No space before function call parens
        when {Frontend::Token::Kind::Identifier, Frontend::Token::Kind::LParen}
          # foo(x) - no space

        # Type annotation: Identifier followed by Symbol (e.g., x:Int32)
        # No space needed - emit_token splits Symbol into ": Type"
        when {Frontend::Token::Kind::Identifier, Frontend::Token::Kind::Symbol}
          # x:Int32 → "x" + (": Int32" from emit_token)

        # Around type annotations ':'
        # Space before ':' in type annotations
        when {Frontend::Token::Kind::Identifier, Frontend::Token::Kind::Colon}
          @output << ' '
        # Space after ':' in type annotations
        # (symbols like ':foo' are handled separately with no preceding identifier)
        when {Frontend::Token::Kind::Colon, _}
          @output << ' '

        # Around assignment '='
        when {_, Frontend::Token::Kind::Eq}
          @output << ' '
        when {Frontend::Token::Kind::Eq, _}
          @output << ' '

        # Around binary operators
        when {_, Frontend::Token::Kind::Plus}, {_, Frontend::Token::Kind::Minus},
             {_, Frontend::Token::Kind::Star}, {_, Frontend::Token::Kind::Slash}
          @output << ' '
        when {Frontend::Token::Kind::Plus, _}, {Frontend::Token::Kind::Minus, _},
             {Frontend::Token::Kind::Star, _}, {Frontend::Token::Kind::Slash, _}
          @output << ' '

        # Around comparison operators
        when {_, Frontend::Token::Kind::EqEq}, {_, Frontend::Token::Kind::NotEq},
             {_, Frontend::Token::Kind::Less}, {_, Frontend::Token::Kind::Greater},
             {_, Frontend::Token::Kind::LessEq}, {_, Frontend::Token::Kind::GreaterEq}
          @output << ' '
        when {Frontend::Token::Kind::EqEq, _}, {Frontend::Token::Kind::NotEq, _},
             {Frontend::Token::Kind::Less, _}, {Frontend::Token::Kind::Greater, _},
             {Frontend::Token::Kind::LessEq, _}, {Frontend::Token::Kind::GreaterEq, _}
          @output << ' '

        # After comma
        when {Frontend::Token::Kind::Comma, _}
          @output << ' '

        # No space after/before parens
        when {Frontend::Token::Kind::LParen, _}, {_, Frontend::Token::Kind::RParen}
          # (x) or x) - no space

        # After closing paren before various tokens - add space
        when {Frontend::Token::Kind::RParen, Frontend::Token::Kind::Identifier}
          @output << ' '

        # No space before dot (method call)
        when {_, Frontend::Token::Kind::Operator}
          # ex.message - no space before dot

        # Default: after keywords/identifier-calls add space (catch-all)
        else
          if (need_space_after || is_identifier_call) &&
             next_token.kind != Frontend::Token::Kind::Newline &&
             next_token.kind != Frontend::Token::Kind::EOF &&
             next_token.kind != Frontend::Token::Kind::LParen &&
             next_token.kind != Frontend::Token::Kind::RParen &&
             next_token.kind != Frontend::Token::Kind::Comma &&
             next_token.kind != Frontend::Token::Kind::Semicolon
            @output << ' '
          end
        end
      end

      private def should_increase_indent?(current : Frontend::Token, next_token : Frontend::Token?) : Bool
        # Keywords that open blocks
        case current.kind
        when Frontend::Token::Kind::Def, Frontend::Token::Kind::If,
             Frontend::Token::Kind::Unless, Frontend::Token::Kind::While,
             Frontend::Token::Kind::Until, Frontend::Token::Kind::Class,
             Frontend::Token::Kind::Module, Frontend::Token::Kind::Struct,
             Frontend::Token::Kind::Elsif, Frontend::Token::Kind::Else,
             Frontend::Token::Kind::Enum, Frontend::Token::Kind::Case,
             Frontend::Token::Kind::Begin, Frontend::Token::Kind::When,
             Frontend::Token::Kind::Rescue, Frontend::Token::Kind::Ensure
          true
        else
          false
        end
      end

      private def should_decrease_indent?(current : Frontend::Token) : Bool
        # 'end' closes block
        return true if current.kind == Frontend::Token::Kind::End

        # 'elsif', 'else', 'when', 'rescue', 'ensure' decrease then re-increase
        return true if current.kind == Frontend::Token::Kind::Elsif ||
                       current.kind == Frontend::Token::Kind::Else ||
                       current.kind == Frontend::Token::Kind::When ||
                       current.kind == Frontend::Token::Kind::Rescue ||
                       current.kind == Frontend::Token::Kind::Ensure

        false
      end
    end
  end
end
