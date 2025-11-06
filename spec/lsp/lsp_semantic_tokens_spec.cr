require "spec"

require "../../src/main"
require "../../src/compiler/lsp/protocol"
require "../../src/compiler/lsp/messages"
require "../../src/compiler/lsp/server"
require "../../src/compiler/frontend/parser"

# Helper methods for decoding semantic tokens in tests
module SemanticTokensSpecHelper
  def self.decode_tokens(data : Array(Int32)) : Array({Int32, Int32, Int32, Int32})
    out = [] of {Int32, Int32, Int32, Int32}
    prev_line = 0
    prev_start = 0
    i = 0
    while i + 4 < data.size
      delta_line = data[i]
      delta_start = data[i + 1]
      length = data[i + 2]
      token_type = data[i + 3]
      # modifiers = data[i + 4]
      line = prev_line + delta_line
      start_char = delta_line == 0 ? prev_start + delta_start : delta_start
      out << {line, start_char, length, token_type}
      prev_line = line
      prev_start = start_char
      i += 5
    end
    out
  end
end

describe "LSP Semantic Tokens" do
  describe "SemanticTokens struct" do
    it "creates semantic tokens with empty data" do
      tokens = CrystalV2::Compiler::LSP::SemanticTokens.new(data: [] of Int32)

      tokens.data.should be_empty
      tokens.result_id.should be_nil
    end

    it "creates semantic tokens with data" do
      data = [0, 5, 3, 2, 0]  # deltaLine, deltaStart, length, tokenType, modifiers
      tokens = CrystalV2::Compiler::LSP::SemanticTokens.new(data: data)

      tokens.data.should eq(data)
      tokens.data.size.should eq(5)
    end

    it "creates semantic tokens with result_id" do
      data = [0, 5, 3, 2, 0]
      tokens = CrystalV2::Compiler::LSP::SemanticTokens.new(data: data, result_id: "v1")

      tokens.result_id.should eq("v1")
    end

    it "serializes to JSON with camelCase" do
      data = [0, 5, 3, 2, 0]
      tokens = CrystalV2::Compiler::LSP::SemanticTokens.new(data: data, result_id: "v1")

      json = tokens.to_json
      json.should contain("\"data\"")
      json.should contain("\"resultId\"")
      json.should_not contain("\"result_id\"")  # Not snake_case
    end
  end

  describe "SemanticTokensParams struct" do
    it "creates params with text document" do
      text_doc = CrystalV2::Compiler::LSP::TextDocumentIdentifier.new(uri: "file:///test.cr")
      params = CrystalV2::Compiler::LSP::SemanticTokensParams.new(text_document: text_doc)

      params.text_document.uri.should eq("file:///test.cr")
    end

    it "serializes to JSON with camelCase" do
      text_doc = CrystalV2::Compiler::LSP::TextDocumentIdentifier.new(uri: "file:///test.cr")
      params = CrystalV2::Compiler::LSP::SemanticTokensParams.new(text_document: text_doc)

      json = params.to_json
      json.should contain("\"textDocument\"")
      json.should_not contain("\"text_document\"")
    end
  end

  describe "Delta encoding" do
    it "encodes single token correctly" do
      source = <<-CRYSTAL
      x = 42
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      server = CrystalV2::Compiler::LSP::Server.new
      tokens = server.collect_semantic_tokens(program, source)

      # Should have tokens for identifier and number
      # Delta encoding: first token uses absolute position
      tokens.data.should_not be_empty
      tokens.data.size.should be >= 5  # At least 1 token (5 integers)
    end

    it "encodes tokens on same line with relative position" do
      source = <<-CRYSTAL
      x = 42
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      server = CrystalV2::Compiler::LSP::Server.new
      tokens = server.collect_semantic_tokens(program, source)

      # Tokens on same line should use delta position (relative to previous)
      # Format: [deltaLine, deltaStart, length, tokenType, modifiers]
      data = tokens.data
      data.size.should be >= 10  # At least 2 tokens

      # First token: absolute position (deltaLine = 0 for first line)
      first_delta_line = data[0]
      first_delta_line.should eq(0)  # Line 0 (0-indexed)

      # Second token on same line: deltaLine = 0, deltaStart = relative
      if data.size >= 10
        second_delta_line = data[5]  # Second token starts at index 5
        second_delta_line.should eq(0)  # Same line
      end
    end

    it "encodes tokens on different lines with absolute character position" do
      source = <<-CRYSTAL
      x = 1
      y = 2
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      server = CrystalV2::Compiler::LSP::Server.new
      tokens = server.collect_semantic_tokens(program, source)

      data = tokens.data
      data.size.should be >= 20  # At least 4 tokens (x, 1, y, 2)

      # Find transition between lines
      # When deltaLine > 0, deltaStart should be absolute position on new line
      has_line_transition = false
      (0...data.size).step(5) do |i|
        if i + 5 < data.size
          current_delta_line = data[i]
          if current_delta_line > 0
            has_line_transition = true
            # deltaStart should be absolute position (not negative)
            delta_start = data[i + 1]
            delta_start.should be >= 0
          end
        end
      end

      has_line_transition.should be_true
    end
  end

  describe "Token collection" do
    it "collects class token" do
      source = <<-CRYSTAL
      class Calculator
      end
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      server = CrystalV2::Compiler::LSP::Server.new
      tokens = server.collect_semantic_tokens(program, source)

      # Should have token for class name "Calculator"
      # Token type: 2 (Class)
      data = tokens.data
      has_class_token = false

      (0...data.size).step(5) do |i|
        token_type = data[i + 3]
        if token_type == 2  # Class
          has_class_token = true
        end
      end

      has_class_token.should be_true
    end

    it "has precise positions for member and number tokens" do
      source = <<-CRYSTAL
      obj.calculate
      x = 42
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      server = CrystalV2::Compiler::LSP::Server.new
      tokens = server.collect_semantic_tokens(program, source)

      decoded = SemanticTokensSpecHelper.decode_tokens(tokens.data)

      # Expect a Method token (13) starting at column 4 on line 0 ("obj.")
      has_member_at_4 = decoded.any? { |(line, col, len, type)| line == 0 && col == 4 && type == 13 && len == "calculate".size }
      has_member_at_4.should be_true

      # Expect a Number token (19) starting at column 4 on line 1 (after "x = ") with length 2
      has_number_42 = decoded.any? { |(line, col, len, type)| line == 1 && col == 4 && type == 19 && len == 2 }
      has_number_42.should be_true
    end

    it "collects method token" do
      source = <<-CRYSTAL
      def calculate
      end
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      server = CrystalV2::Compiler::LSP::Server.new
      tokens = server.collect_semantic_tokens(program, source)

      # Should have token for method name "calculate"
      # Token type: 13 (Method)
      data = tokens.data
      has_method_token = false

      (0...data.size).step(5) do |i|
        token_type = data[i + 3]
        if token_type == 13  # Method
          has_method_token = true
        end
      end

      has_method_token.should be_true
    end

    it "collects parameter tokens" do
      source = <<-CRYSTAL
      def add(x : Int32, y : Int32)
      end
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      server = CrystalV2::Compiler::LSP::Server.new
      tokens = server.collect_semantic_tokens(program, source)

      # Should have tokens for parameters "x" and "y"
      # Token type: 7 (Parameter)
      data = tokens.data
      parameter_count = 0

      (0...data.size).step(5) do |i|
        token_type = data[i + 3]
        if token_type == 7  # Parameter
          parameter_count += 1
        end
      end

      parameter_count.should eq(2)  # x and y
    end

    it "collects variable token" do
      source = <<-CRYSTAL
      result = 42
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      server = CrystalV2::Compiler::LSP::Server.new
      tokens = server.collect_semantic_tokens(program, source)

      # Should have token for identifier "result"
      # Token type: 8 (Variable)
      data = tokens.data
      has_variable_token = false

      (0...data.size).step(5) do |i|
        token_type = data[i + 3]
        if token_type == 8  # Variable
          has_variable_token = true
        end
      end

      has_variable_token.should be_true
    end

    it "collects string literal token" do
      source = <<-CRYSTAL
      message = "hello"
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      server = CrystalV2::Compiler::LSP::Server.new
      tokens = server.collect_semantic_tokens(program, source)

      # Should have token for string "hello"
      # Token type: 18 (String)
      data = tokens.data
      has_string_token = false

      (0...data.size).step(5) do |i|
        token_type = data[i + 3]
        if token_type == 18  # String
          has_string_token = true
        end
      end

      has_string_token.should be_true
    end

    it "collects number literal token" do
      source = <<-CRYSTAL
      x = 42
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      server = CrystalV2::Compiler::LSP::Server.new
      tokens = server.collect_semantic_tokens(program, source)

      # Should have token for number 42
      # Token type: 19 (Number)
      data = tokens.data
      has_number_token = false

      (0...data.size).step(5) do |i|
        token_type = data[i + 3]
        if token_type == 19  # Number
          has_number_token = true
        end
      end

      has_number_token.should be_true
    end
  end

  # CRITICAL: Control flow tests (learned from previous bugs!)

  describe "Control flow token collection" do
    it "collects tokens inside if statement" do
      source = <<-CRYSTAL
      if x > 0
        result = 1
      end
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      server = CrystalV2::Compiler::LSP::Server.new
      tokens = server.collect_semantic_tokens(program, source)

      # Should collect tokens from condition and body
      # "x", "0", "result", "1"
      data = tokens.data
      data.should_not be_empty

      # Should have at least 4 tokens
      token_count = data.size // 5
      token_count.should be >= 4
    end

    it "collects tokens inside while loop" do
      source = <<-CRYSTAL
      while counter < 10
        process(counter)
      end
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      server = CrystalV2::Compiler::LSP::Server.new
      tokens = server.collect_semantic_tokens(program, source)

      # Should collect tokens from condition and body
      data = tokens.data
      data.should_not be_empty

      # Should have multiple tokens (counter, 10, process, counter)
      token_count = data.size // 5
      token_count.should be >= 3
    end

    it "collects tokens inside unless statement" do
      source = <<-CRYSTAL
      unless flag
        do_something(value)
      end
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      server = CrystalV2::Compiler::LSP::Server.new
      tokens = server.collect_semantic_tokens(program, source)

      # Should collect tokens from condition and body
      data = tokens.data
      data.should_not be_empty

      token_count = data.size // 5
      token_count.should be >= 2
    end

    it "collects tokens inside until loop" do
      source = <<-CRYSTAL
      until done
        work(item)
      end
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      server = CrystalV2::Compiler::LSP::Server.new
      tokens = server.collect_semantic_tokens(program, source)

      # Should collect tokens from condition and body
      data = tokens.data
      data.should_not be_empty

      token_count = data.size // 5
      token_count.should be >= 2
    end

    it "collects tokens inside loop block" do
      source = <<-CRYSTAL
      loop do
        perform(task)
        break if finished
      end
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      server = CrystalV2::Compiler::LSP::Server.new
      tokens = server.collect_semantic_tokens(program, source)

      # Should collect tokens from body
      data = tokens.data
      data.should_not be_empty

      token_count = data.size // 5
      token_count.should be >= 2
    end

    it "collects tokens from nested control flow" do
      source = <<-CRYSTAL
      if ready
        while has_items
          item = get_next
          process(item)
        end
      end
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      server = CrystalV2::Compiler::LSP::Server.new
      tokens = server.collect_semantic_tokens(program, source)

      # Should collect tokens from all nested levels
      data = tokens.data
      data.should_not be_empty

      # Should have many tokens (ready, has_items, item, get_next, process, item)
      token_count = data.size // 5
      token_count.should be >= 6
    end

    it "handles elsif clauses" do
      source = <<-CRYSTAL
      if x > 0
        positive = true
      elsif x < 0
        negative = true
      else
        zero = true
      end
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      server = CrystalV2::Compiler::LSP::Server.new
      tokens = server.collect_semantic_tokens(program, source)

      # Should collect tokens from all branches
      data = tokens.data
      data.should_not be_empty

      # Should have tokens from if, elsif, and else branches
      token_count = data.size // 5
      token_count.should be >= 9  # x, 0, positive, true, x, 0, negative, true, zero, true
    end
  end

  describe "Complex structures" do
    it "collects tokens from class with method and control flow" do
      source = <<-CRYSTAL
      class Processor
        def process(value : Int32)
          if value > 0
            result = value * 2
          else
            result = 0
          end
        end
      end
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      server = CrystalV2::Compiler::LSP::Server.new
      tokens = server.collect_semantic_tokens(program, source)

      # Should have tokens for:
      # - Class name "Processor" (type 2)
      # - Method name "process" (type 13)
      # - Parameter "value" (type 7)
      # - Variables and literals in body
      data = tokens.data
      data.should_not be_empty

      # Count token types
      has_class = false
      has_method = false
      has_parameter = false

      (0...data.size).step(5) do |i|
        token_type = data[i + 3]
        has_class = true if token_type == 2   # Class
        has_method = true if token_type == 13  # Method
        has_parameter = true if token_type == 7  # Parameter
      end

      has_class.should be_true
      has_method.should be_true
      has_parameter.should be_true
    end

    it "sorts tokens by line and character position" do
      source = <<-CRYSTAL
      x = 1
      y = 2
      z = 3
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      server = CrystalV2::Compiler::LSP::Server.new
      tokens = server.collect_semantic_tokens(program, source)

      # Tokens should be sorted, so deltaLine should be non-negative
      # and monotonically increasing
      data = tokens.data
      prev_line = 0
      prev_char = 0

      (0...data.size).step(5) do |i|
        delta_line = data[i]
        delta_start = data[i + 1]

        # Delta line should be non-negative
        delta_line.should be >= 0

        # Calculate absolute position
        current_line = prev_line + delta_line
        current_char = if delta_line == 0
                         prev_char + delta_start
                       else
                         delta_start
                       end

        # Position should not go backwards
        if delta_line == 0
          current_char.should be >= prev_char
        end

        prev_line = current_line
        prev_char = current_char
      end
    end
  end

  describe "Line number conversion" do
    it "converts from 1-indexed to 0-indexed" do
      source = <<-CRYSTAL
      x = 1
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      server = CrystalV2::Compiler::LSP::Server.new
      tokens = server.collect_semantic_tokens(program, source)

      # First token should be on line 0 (LSP 0-indexed)
      # Parser gives 1-indexed, but tokens should be 0-indexed
      data = tokens.data
      data.should_not be_empty

      first_delta_line = data[0]
      first_delta_line.should eq(0)  # First line is line 0 in LSP
    end

    it "verifies parser gives 1-indexed, tokens are 0-indexed" do
      source = <<-CRYSTAL
      def foo
        x = 1
      end
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      # Parser: 1-indexed (verified by previous tests)
      root = program.arena[program.roots[0]]
      def_node = root.as(CrystalV2::Compiler::Frontend::DefNode)
      def_node.span.start_line.should eq(1)  # Parser gives 1

      # Tokens: 0-indexed (LSP requirement)
      server = CrystalV2::Compiler::LSP::Server.new
      tokens = server.collect_semantic_tokens(program, source)

      data = tokens.data
      first_delta_line = data[0]
      first_delta_line.should eq(0)  # Tokens give 0 (converted)
    end
  end
end
