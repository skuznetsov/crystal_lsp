require "spec"

require "../../src/main"
require "../../src/compiler/lsp/protocol"
require "../../src/compiler/lsp/messages"
require "../../src/compiler/frontend/parser"
require "../../src/compiler/semantic/analyzer"

describe "LSP InlayHint" do
  describe "InlayHintKind enum" do
    it "has correct values" do
      CrystalV2::Compiler::LSP::InlayHintKind::Type.value.should eq(1)
      CrystalV2::Compiler::LSP::InlayHintKind::Parameter.value.should eq(2)
    end
  end

  describe "InlayHint struct" do
    it "creates basic inlay hint" do
      position = CrystalV2::Compiler::LSP::Position.new(line: 5, character: 10)

      hint = CrystalV2::Compiler::LSP::InlayHint.new(
        position: position,
        label: ": Int32",
        kind: CrystalV2::Compiler::LSP::InlayHintKind::Type.value
      )

      hint.position.line.should eq(5)
      hint.position.character.should eq(10)
      hint.label.should eq(": Int32")
      hint.kind.should eq(1)
    end

    it "supports padding options" do
      position = CrystalV2::Compiler::LSP::Position.new(line: 0, character: 0)

      hint = CrystalV2::Compiler::LSP::InlayHint.new(
        position: position,
        label: "value: ",
        kind: CrystalV2::Compiler::LSP::InlayHintKind::Parameter.value,
        padding_left: true,
        padding_right: false
      )

      hint.padding_left.should be_true
      hint.padding_right.should be_false
    end

    it "serializes to JSON correctly" do
      position = CrystalV2::Compiler::LSP::Position.new(line: 3, character: 7)

      hint = CrystalV2::Compiler::LSP::InlayHint.new(
        position: position,
        label: ": String",
        kind: CrystalV2::Compiler::LSP::InlayHintKind::Type.value
      )

      json = hint.to_json
      json.should contain("\"position\"")
      json.should contain("\"label\"")
      json.should contain("\"kind\"")
      json.should contain(": String")
    end
  end

  describe "InlayHintParams struct" do
    it "creates params with required fields" do
      text_doc = CrystalV2::Compiler::LSP::TextDocumentIdentifier.new(uri: "file:///test.cr")
      range = CrystalV2::Compiler::LSP::Range.new(
        start: CrystalV2::Compiler::LSP::Position.new(0, 0),
        end: CrystalV2::Compiler::LSP::Position.new(10, 0)
      )

      params = CrystalV2::Compiler::LSP::InlayHintParams.new(
        text_document: text_doc,
        range: range
      )

      params.text_document.uri.should eq("file:///test.cr")
      params.range.start.line.should eq(0)
      params.range.end.line.should eq(10)
    end

    it "serializes to JSON correctly" do
      text_doc = CrystalV2::Compiler::LSP::TextDocumentIdentifier.new(uri: "file:///test.cr")
      range = CrystalV2::Compiler::LSP::Range.new(
        start: CrystalV2::Compiler::LSP::Position.new(0, 0),
        end: CrystalV2::Compiler::LSP::Position.new(5, 0)
      )

      params = CrystalV2::Compiler::LSP::InlayHintParams.new(
        text_document: text_doc,
        range: range
      )

      json = params.to_json
      json.should contain("\"textDocument\"")
      json.should contain("\"range\"")
    end
  end

  describe "Type hints functionality" do
    it "has type context available after type inference" do
      source = <<-CRYSTAL
      x = 10
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      analyzer = CrystalV2::Compiler::Semantic::Analyzer.new(program)
      analyzer.collect_symbols
      result = analyzer.resolve_names
      identifier_symbols = result.identifier_symbols

      engine = analyzer.infer_types(identifier_symbols)
      type_context = engine.context

      # Type context should be available
      type_context.should_not be_nil

      # Find 'x' variable symbol
      x_symbol = analyzer.global_context.symbol_table.lookup("x")
      x_symbol.should_not be_nil
      x_symbol.should be_a(CrystalV2::Compiler::Semantic::VariableSymbol)
    end

    it "type inference runs without errors" do
      source = <<-CRYSTAL
      x = 10
      y = "hello"
      z = 3.14
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      analyzer = CrystalV2::Compiler::Semantic::Analyzer.new(program)
      analyzer.collect_symbols
      result = analyzer.resolve_names
      identifier_symbols = result.identifier_symbols

      engine = analyzer.infer_types(identifier_symbols)
      type_context = engine.context

      # Should complete without raising errors
      type_context.should_not be_nil
    end

    it "can query types from type context" do
      source = <<-CRYSTAL
      42
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      analyzer = CrystalV2::Compiler::Semantic::Analyzer.new(program)
      analyzer.collect_symbols
      result = analyzer.resolve_names
      identifier_symbols = result.identifier_symbols

      engine = analyzer.infer_types(identifier_symbols)
      type_context = engine.context

      # Literal should have inferred type
      root_id = program.roots[0]
      type = type_context.get_type(root_id)
      type.should_not be_nil
    end
  end

  describe "Parameter hints functionality" do
    it "finds method symbols with parameters" do
      source = <<-CRYSTAL
      def calculate(x : Int32, y : Int32) : Int32
        x + y
      end

      result = calculate(10, 20)
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      analyzer = CrystalV2::Compiler::Semantic::Analyzer.new(program)
      analyzer.collect_symbols
      result = analyzer.resolve_names

      identifier_symbols = result.identifier_symbols

      # Find 'calculate' method symbol
      calc_symbol = analyzer.global_context.symbol_table.lookup("calculate")
      calc_symbol.should_not be_nil
      calc_symbol = calc_symbol.not_nil!
      calc_symbol.should be_a(CrystalV2::Compiler::Semantic::MethodSymbol)

      method_symbol = calc_symbol.as(CrystalV2::Compiler::Semantic::MethodSymbol)

      # Method should have 2 parameters
      method_symbol.params.size.should eq(2)
      String.new(method_symbol.params[0].name).should eq("x")
      String.new(method_symbol.params[1].name).should eq("y")
    end

    it "finds method symbols for nested calls" do
      source = <<-CRYSTAL
      def add(a : Int32, b : Int32) : Int32
        a + b
      end

      def multiply(x : Int32, y : Int32) : Int32
        x * y
      end

      result = add(multiply(2, 3), 5)
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      analyzer = CrystalV2::Compiler::Semantic::Analyzer.new(program)
      analyzer.collect_symbols
      result = analyzer.resolve_names

      # Both methods should be in symbol table
      add_symbol = analyzer.global_context.symbol_table.lookup("add")
      add_symbol.should_not be_nil

      multiply_symbol = analyzer.global_context.symbol_table.lookup("multiply")
      multiply_symbol.should_not be_nil
    end

    it "handles methods with single parameter" do
      source = <<-CRYSTAL
      def greet(name : String) : String
        "Hello, \#{name}!"
      end

      message = greet("World")
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      analyzer = CrystalV2::Compiler::Semantic::Analyzer.new(program)
      analyzer.collect_symbols
      result = analyzer.resolve_names

      identifier_symbols = result.identifier_symbols

      # Find 'greet' method symbol
      greet_symbol = analyzer.global_context.symbol_table.lookup("greet")
      greet_symbol.should_not be_nil
      greet_symbol = greet_symbol.not_nil!
      greet_symbol.should be_a(CrystalV2::Compiler::Semantic::MethodSymbol)

      method_symbol = greet_symbol.as(CrystalV2::Compiler::Semantic::MethodSymbol)

      # Method should have 1 parameter
      method_symbol.params.size.should eq(1)
      String.new(method_symbol.params[0].name).should eq("name")
    end

    it "handles methods with no parameters" do
      source = <<-CRYSTAL
      def get_value : Int32
        42
      end

      x = get_value
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      analyzer = CrystalV2::Compiler::Semantic::Analyzer.new(program)
      analyzer.collect_symbols
      result = analyzer.resolve_names

      identifier_symbols = result.identifier_symbols

      # Find 'get_value' method symbol
      get_value_symbol = analyzer.global_context.symbol_table.lookup("get_value")
      get_value_symbol.should_not be_nil
      get_value_symbol = get_value_symbol.not_nil!
      get_value_symbol.should be_a(CrystalV2::Compiler::Semantic::MethodSymbol)

      method_symbol = get_value_symbol.as(CrystalV2::Compiler::Semantic::MethodSymbol)

      # Method should have 0 parameters
      method_symbol.params.size.should eq(0)
    end
  end

  describe "Range filtering" do
    it "filters hints based on visible range" do
      source = <<-CRYSTAL
      x = 10
      y = 20
      z = 30
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      analyzer = CrystalV2::Compiler::Semantic::Analyzer.new(program)
      analyzer.collect_symbols
      result = analyzer.resolve_names
      identifier_symbols = result.identifier_symbols

      engine = analyzer.infer_types(identifier_symbols)
      type_context = engine.context

      # Test range filtering: only line 0 (first line)
      in_range_count = 0
      identifier_symbols.each do |expr_id, symbol|
        if symbol.is_a?(CrystalV2::Compiler::Semantic::VariableSymbol)
          if expr_id == symbol.node_id
            node = program.arena[expr_id]
            # Check if node is on line 0 (1-indexed in span, so line 1)
            in_range_count += 1 if node.span.start_line == 1
          end
        end
      end

      # Only 'x' should be on line 0
      in_range_count.should eq(1)
    end

    it "includes hints at range boundaries" do
      source = <<-CRYSTAL
      a = 1
      b = 2
      c = 3
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      analyzer = CrystalV2::Compiler::Semantic::Analyzer.new(program)
      analyzer.collect_symbols
      result = analyzer.resolve_names
      identifier_symbols = result.identifier_symbols

      engine = analyzer.infer_types(identifier_symbols)
      type_context = engine.context

      # Test range: lines 0-1 (first two lines)
      in_range_count = 0
      identifier_symbols.each do |expr_id, symbol|
        if symbol.is_a?(CrystalV2::Compiler::Semantic::VariableSymbol)
          if expr_id == symbol.node_id
            node = program.arena[expr_id]
            # Lines 1-2 in 1-indexed span
            in_range_count += 1 if node.span.start_line >= 1 && node.span.start_line <= 2
          end
        end
      end

      # 'a' and 'b' should be in range
      in_range_count.should eq(2)
    end
  end

  describe "Position calculation" do
    it "converts span coordinates to LSP positions" do
      source = <<-CRYSTAL
      x = 10
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      analyzer = CrystalV2::Compiler::Semantic::Analyzer.new(program)
      analyzer.collect_symbols
      result = analyzer.resolve_names

      identifier_symbols = result.identifier_symbols

      # Find 'x' variable symbol
      x_symbol = analyzer.global_context.symbol_table.lookup("x")
      x_symbol.should_not be_nil
      x_symbol = x_symbol.not_nil!

      # Get node and check position conversion
      node = program.arena[x_symbol.node_id]

      # Span is 1-indexed, LSP Position is 0-indexed
      # Type hint should be positioned at end of identifier
      lsp_line = node.span.end_line - 1
      lsp_char = node.span.end_column - 1

      lsp_line.should eq(0)  # First line (0-indexed)
      lsp_char.should eq(1)  # After 'x' (0-indexed)
    end

    it "calculates correct line numbers for multi-line programs" do
      source = <<-CRYSTAL
      x = 10
      y = 20
      z = 30
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      analyzer = CrystalV2::Compiler::Semantic::Analyzer.new(program)
      analyzer.collect_symbols
      analyzer.resolve_names

      # Check all three variables are on different lines
      x_symbol = analyzer.global_context.symbol_table.lookup("x")
      y_symbol = analyzer.global_context.symbol_table.lookup("y")
      z_symbol = analyzer.global_context.symbol_table.lookup("z")

      x_symbol.should_not be_nil
      y_symbol.should_not be_nil
      z_symbol.should_not be_nil

      x_node = program.arena[x_symbol.not_nil!.node_id]
      y_node = program.arena[y_symbol.not_nil!.node_id]
      z_node = program.arena[z_symbol.not_nil!.node_id]

      # Verify they're on consecutive lines
      x_node.span.start_line.should eq(1)
      y_node.span.start_line.should eq(2)
      z_node.span.start_line.should eq(3)
    end
  end

  describe "Integration" do
    it "type context integrates with semantic analyzer" do
      source = <<-CRYSTAL
      x = 10
      y = "test"
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      analyzer = CrystalV2::Compiler::Semantic::Analyzer.new(program)
      analyzer.collect_symbols
      result = analyzer.resolve_names
      identifier_symbols = result.identifier_symbols

      engine = analyzer.infer_types(identifier_symbols)
      type_context = engine.context

      # Type context should be available
      type_context.should_not be_nil

      # Both variables should be in symbol table
      x_symbol = analyzer.global_context.symbol_table.lookup("x")
      y_symbol = analyzer.global_context.symbol_table.lookup("y")
      x_symbol.should_not be_nil
      y_symbol.should_not be_nil
    end

    it "symbol table contains methods and their parameters" do
      source = <<-CRYSTAL
      def add(a : Int32, b : Int32) : Int32
        a + b
      end

      result = add(5, 10)
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      analyzer = CrystalV2::Compiler::Semantic::Analyzer.new(program)
      analyzer.collect_symbols
      result = analyzer.resolve_names

      # Method should be in symbol table
      add_symbol = analyzer.global_context.symbol_table.lookup("add")
      add_symbol.should_not be_nil
      add_symbol.should be_a(CrystalV2::Compiler::Semantic::MethodSymbol)

      # Method should have parameters
      method_symbol = add_symbol.as(CrystalV2::Compiler::Semantic::MethodSymbol)
      method_symbol.params.size.should eq(2)

      # Result variable should be in symbol table
      result_symbol = analyzer.global_context.symbol_table.lookup("result")
      result_symbol.should_not be_nil
    end

    # Control flow coverage tests

    it "resolves variables inside while loop" do
      source = <<-CRYSTAL
      counter = 0
      while counter < 10
        temp = counter * 2
        counter = counter + 1
      end
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      analyzer = CrystalV2::Compiler::Semantic::Analyzer.new(program)
      analyzer.collect_symbols
      result = analyzer.resolve_names
      identifier_symbols = result.identifier_symbols

      # Both counter and temp should be in symbol table
      counter_symbol = analyzer.global_context.symbol_table.lookup("counter")
      counter_symbol.should_not be_nil

      # temp is declared inside while loop - should be in global scope (no block scope yet)
      temp_symbol = analyzer.global_context.symbol_table.lookup("temp")
      temp_symbol.should_not be_nil

      # identifier_symbols should have all usages
      counter_count = identifier_symbols.count { |_, sym| sym == counter_symbol }
      counter_count.should be > 3  # declaration + multiple uses
    end

    it "resolves variables inside if/else branches" do
      source = <<-CRYSTAL
      flag = true
      if flag
        x = 10
      else
        x = 20
      end
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      analyzer = CrystalV2::Compiler::Semantic::Analyzer.new(program)
      analyzer.collect_symbols
      result = analyzer.resolve_names
      identifier_symbols = result.identifier_symbols

      # flag should be resolved
      flag_symbol = analyzer.global_context.symbol_table.lookup("flag")
      flag_symbol.should_not be_nil

      # x should be in symbol table (first assignment wins)
      x_symbol = analyzer.global_context.symbol_table.lookup("x")
      x_symbol.should_not be_nil

      # flag should appear in identifier_symbols (declaration + condition)
      flag_count = identifier_symbols.count { |_, sym| sym == flag_symbol }
      flag_count.should eq(2)
    end
  end
end
