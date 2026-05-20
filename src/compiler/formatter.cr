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
          started_line = @at_line_start

          # Adjust indent BEFORE emitting indent for keywords that decrease it
          if @at_line_start && should_decrease_indent?(current)
            @indent -= 2
          end

          # Emit indentation if at line start
          if @at_line_start && current.kind != Frontend::Token::Kind::Newline
            emit_line_prefix(current)
            @at_line_start = false
          end

          # Emit current token
          emit_token(current)

          # Emit whitespace after current token
          if next_token
            emit_whitespace_after(current, next_token)
          end

          # Adjust indent AFTER emitting keyword that increases indent
          if should_increase_indent?(current, next_token, started_line)
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
           @previous_token.try { |previous| variable_like_kind?(previous.kind) }
          span = token.span
          start_byte = span.start_offset
          end_byte = span.end_offset
          original_text = @source.byte_slice(start_byte, end_byte - start_byte)

          # Symbol text is like ":Int32" - split into ": Int32"
          if original_text.starts_with?(':')
            @output << ':'
            @output << ' '
            @output << original_text[1..-1] # Type name without leading colon
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

      private def emit_line_prefix(token : Frontend::Token)
        line_start = token.span.start_offset
        while line_start > 0 && @source.byte_at(line_start - 1) != '\n'.ord
          line_start -= 1
        end

        prefix_len = token.span.start_offset - line_start
        return if prefix_len == 0

        if prefix_len > 0
          prefix = @source.byte_slice(line_start, prefix_len)
          if prefix.each_char.all? { |char| char == ' ' || char == '\t' }
            @output << prefix
            return
          end
        end

        emit_indent
      end

      private def emit_whitespace_after(current : Frontend::Token, next_token : Frontend::Token)
        # Skip whitespace before newlines or EOF
        return if next_token.kind == Frontend::Token::Kind::Newline
        return if next_token.kind == Frontend::Token::Kind::EOF
        original_gap = original_gap_between(current, next_token)

        # Helper to check if need space after keyword
        # Note: 'puts' is an Identifier, not a keyword, so handled separately
        need_space_after = case current.kind
                           when Frontend::Token::Kind::Def, Frontend::Token::Kind::If,
                                Frontend::Token::Kind::Unless, Frontend::Token::Kind::While,
                                Frontend::Token::Kind::Until, Frontend::Token::Kind::Return,
                                Frontend::Token::Kind::Class, Frontend::Token::Kind::Module,
                                Frontend::Token::Kind::Struct, Frontend::Token::Kind::Elsif,
                                Frontend::Token::Kind::Else, Frontend::Token::Kind::Require,
                                Frontend::Token::Kind::Enum,
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

        case {current.kind, next_token.kind}
        when {Frontend::Token::Kind::Identifier, Frontend::Token::Kind::LParen}
          # foo(x) - no space

        when {Frontend::Token::Kind::Identifier, Frontend::Token::Kind::ColonColon},
             {Frontend::Token::Kind::ColonColon, Frontend::Token::Kind::Identifier}
          # Foo::Bar - no space

        when {Frontend::Token::Kind::Identifier, Frontend::Token::Kind::Symbol},
             {Frontend::Token::Kind::InstanceVar, Frontend::Token::Kind::Symbol},
             {Frontend::Token::Kind::ClassVar, Frontend::Token::Kind::Symbol},
             {Frontend::Token::Kind::GlobalVar, Frontend::Token::Kind::Symbol}
          # x:Int32 → "x" + (": Int32" from emit_token)

        when {Frontend::Token::Kind::Identifier, Frontend::Token::Kind::Colon},
             {Frontend::Token::Kind::InstanceVar, Frontend::Token::Kind::Colon},
             {Frontend::Token::Kind::ClassVar, Frontend::Token::Kind::Colon},
             {Frontend::Token::Kind::GlobalVar, Frontend::Token::Kind::Colon}
          @output << original_gap
        when {Frontend::Token::Kind::Colon, _}
          emit_original_gap_or_space(original_gap)
        when {_, Frontend::Token::Kind::Eq}
          emit_original_gap_or_space(original_gap)
        when {Frontend::Token::Kind::Eq, _}
          emit_original_gap_or_space(original_gap)
        when {_, Frontend::Token::Kind::Plus}, {_, Frontend::Token::Kind::Minus},
             {_, Frontend::Token::Kind::Star}, {_, Frontend::Token::Kind::Slash}
          @output << original_gap
        when {Frontend::Token::Kind::Plus, _}, {Frontend::Token::Kind::Minus, _},
             {Frontend::Token::Kind::Star, _}, {Frontend::Token::Kind::Slash, _}
          @output << original_gap
        when {_, Frontend::Token::Kind::EqEq}, {_, Frontend::Token::Kind::NotEq},
             {_, Frontend::Token::Kind::Less}, {_, Frontend::Token::Kind::Greater},
             {_, Frontend::Token::Kind::LessEq}, {_, Frontend::Token::Kind::GreaterEq}
          emit_original_gap_or_space(original_gap)
        when {_, Frontend::Token::Kind::Of}, {Frontend::Token::Kind::Of, _},
             {_, Frontend::Token::Kind::Arrow}, {Frontend::Token::Kind::Arrow, _}
          emit_original_gap_or_space(original_gap)
        when {Frontend::Token::Kind::EqEq, _}, {Frontend::Token::Kind::NotEq, _},
             {Frontend::Token::Kind::Less, _}, {Frontend::Token::Kind::Greater, _},
             {Frontend::Token::Kind::LessEq, _}, {Frontend::Token::Kind::GreaterEq, _}
          emit_original_gap_or_space(original_gap)
        when {Frontend::Token::Kind::Comma, _}
          if next_token.kind == Frontend::Token::Kind::Comment && original_gap.bytesize > 0
            @output << original_gap
          else
            @output << ' '
          end
        when {Frontend::Token::Kind::LParen, _}, {_, Frontend::Token::Kind::RParen}
          # (x) or x) - no space

        when {Frontend::Token::Kind::RParen, Frontend::Token::Kind::Identifier}
          @output << ' '
        when {_, Frontend::Token::Kind::Operator}
          @output << original_gap unless original_text(next_token) == "."
        else
          if original_gap.bytesize > 0
            @output << original_gap
          elsif need_space_after &&
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

      private def original_gap_between(current : Frontend::Token, next_token : Frontend::Token) : String
        gap_start = current.span.end_offset
        gap_end = next_token.span.start_offset
        return "" if gap_end <= gap_start

        @source.byte_slice(gap_start, gap_end - gap_start)
      end

      private def original_text(token : Frontend::Token) : String
        @source.byte_slice(token.span.start_offset, token.span.end_offset - token.span.start_offset)
      end

      private def emit_original_gap_or_space(original_gap : String)
        if original_gap.bytesize > 0
          @output << original_gap
        else
          @output << ' '
        end
      end

      private def should_increase_indent?(current : Frontend::Token, next_token : Frontend::Token?, started_line : Bool) : Bool
        if current.kind == Frontend::Token::Kind::If ||
           current.kind == Frontend::Token::Kind::Unless ||
           current.kind == Frontend::Token::Kind::Rescue
          # Modifier forms such as `return nil if x` and `foo rescue bar` do
          # not have a matching `end`; treating them as block openers makes
          # indentation drift across large files.
          return started_line || @previous_token.try(&.kind) == Frontend::Token::Kind::Eq
        end

        # Keywords that open blocks
        case current.kind
        when Frontend::Token::Kind::Def,
             Frontend::Token::Kind::While,
             Frontend::Token::Kind::Until, Frontend::Token::Kind::Class,
             Frontend::Token::Kind::Module, Frontend::Token::Kind::Struct,
             Frontend::Token::Kind::Elsif, Frontend::Token::Kind::Else,
             Frontend::Token::Kind::Enum, Frontend::Token::Kind::Case,
             Frontend::Token::Kind::Begin, Frontend::Token::Kind::When,
             Frontend::Token::Kind::Ensure
          true
        else
          false
        end
      end

      private def variable_like_kind?(kind : Frontend::Token::Kind) : Bool
        kind == Frontend::Token::Kind::Identifier ||
          kind == Frontend::Token::Kind::InstanceVar ||
          kind == Frontend::Token::Kind::ClassVar ||
          kind == Frontend::Token::Kind::GlobalVar
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
