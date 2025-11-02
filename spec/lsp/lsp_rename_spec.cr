require "spec"

require "../../src/main"
require "../../src/compiler/lsp/protocol"
require "../../src/compiler/lsp/messages"
require "../../src/compiler/frontend/parser"
require "../../src/compiler/semantic/analyzer"

describe "LSP Rename" do
  describe "TextEdit struct" do
    it "creates text edit with range and new text" do
      range = CrystalV2::Compiler::LSP::Range.new(
        start: CrystalV2::Compiler::LSP::Position.new(0, 0),
        end: CrystalV2::Compiler::LSP::Position.new(0, 5)
      )
      edit = CrystalV2::Compiler::LSP::TextEdit.new(range: range, new_text: "hello")

      edit.range.should eq(range)
      edit.new_text.should eq("hello")
    end

    it "serializes to JSON with camelCase" do
      range = CrystalV2::Compiler::LSP::Range.new(
        start: CrystalV2::Compiler::LSP::Position.new(0, 0),
        end: CrystalV2::Compiler::LSP::Position.new(0, 5)
      )
      edit = CrystalV2::Compiler::LSP::TextEdit.new(range: range, new_text: "world")

      json = edit.to_json
      json.should contain("\"newText\"")
      json.should contain("world")
    end
  end

  describe "WorkspaceEdit struct" do
    it "creates workspace edit with URI and edits" do
      range = CrystalV2::Compiler::LSP::Range.new(
        start: CrystalV2::Compiler::LSP::Position.new(0, 0),
        end: CrystalV2::Compiler::LSP::Position.new(0, 1)
      )
      edit = CrystalV2::Compiler::LSP::TextEdit.new(range: range, new_text: "y")

      workspace_edit = CrystalV2::Compiler::LSP::WorkspaceEdit.new(
        changes: {"file:///test.cr" => [edit]}
      )

      workspace_edit.changes["file:///test.cr"].size.should eq(1)
      workspace_edit.changes["file:///test.cr"][0].new_text.should eq("y")
    end

    it "serializes to JSON correctly" do
      range = CrystalV2::Compiler::LSP::Range.new(
        start: CrystalV2::Compiler::LSP::Position.new(0, 0),
        end: CrystalV2::Compiler::LSP::Position.new(0, 1)
      )
      edit = CrystalV2::Compiler::LSP::TextEdit.new(range: range, new_text: "z")

      workspace_edit = CrystalV2::Compiler::LSP::WorkspaceEdit.new(
        changes: {"file:///test.cr" => [edit]}
      )

      json = workspace_edit.to_json
      json.should contain("\"changes\"")
      json.should contain("file:///test.cr")
    end
  end

  describe "PrepareRenameResult struct" do
    it "creates result with range and placeholder" do
      range = CrystalV2::Compiler::LSP::Range.new(
        start: CrystalV2::Compiler::LSP::Position.new(5, 10),
        end: CrystalV2::Compiler::LSP::Position.new(5, 15)
      )
      result = CrystalV2::Compiler::LSP::PrepareRenameResult.new(
        range: range,
        placeholder: "oldName"
      )

      result.range.should eq(range)
      result.placeholder.should eq("oldName")
    end

    it "serializes to JSON correctly" do
      range = CrystalV2::Compiler::LSP::Range.new(
        start: CrystalV2::Compiler::LSP::Position.new(0, 0),
        end: CrystalV2::Compiler::LSP::Position.new(0, 4)
      )
      result = CrystalV2::Compiler::LSP::PrepareRenameResult.new(
        range: range,
        placeholder: "test"
      )

      json = result.to_json
      json.should contain("\"range\"")
      json.should contain("\"placeholder\"")
      json.should contain("test")
    end
  end

  describe "PrepareRenameParams struct" do
    it "creates params with text document and position" do
      text_doc = CrystalV2::Compiler::LSP::TextDocumentIdentifier.new(uri: "file:///test.cr")
      position = CrystalV2::Compiler::LSP::Position.new(line: 10, character: 5)

      params = CrystalV2::Compiler::LSP::PrepareRenameParams.new(
        text_document: text_doc,
        position: position
      )

      params.text_document.uri.should eq("file:///test.cr")
      params.position.line.should eq(10)
      params.position.character.should eq(5)
    end

    it "serializes to JSON with camelCase" do
      text_doc = CrystalV2::Compiler::LSP::TextDocumentIdentifier.new(uri: "file:///test.cr")
      position = CrystalV2::Compiler::LSP::Position.new(line: 0, character: 0)

      params = CrystalV2::Compiler::LSP::PrepareRenameParams.new(
        text_document: text_doc,
        position: position
      )

      json = params.to_json
      json.should contain("\"textDocument\"")
      json.should contain("\"position\"")
    end
  end

  describe "RenameParams struct" do
    it "creates params with all required fields" do
      text_doc = CrystalV2::Compiler::LSP::TextDocumentIdentifier.new(uri: "file:///test.cr")
      position = CrystalV2::Compiler::LSP::Position.new(line: 5, character: 10)

      params = CrystalV2::Compiler::LSP::RenameParams.new(
        text_document: text_doc,
        position: position,
        new_name: "newIdentifier"
      )

      params.text_document.uri.should eq("file:///test.cr")
      params.position.line.should eq(5)
      params.position.character.should eq(10)
      params.new_name.should eq("newIdentifier")
    end

    it "serializes to JSON with camelCase" do
      text_doc = CrystalV2::Compiler::LSP::TextDocumentIdentifier.new(uri: "file:///test.cr")
      position = CrystalV2::Compiler::LSP::Position.new(line: 0, character: 0)

      params = CrystalV2::Compiler::LSP::RenameParams.new(
        text_document: text_doc,
        position: position,
        new_name: "renamed"
      )

      json = params.to_json
      json.should contain("\"textDocument\"")
      json.should contain("\"position\"")
      json.should contain("\"newName\"")
      json.should contain("renamed")
    end
  end

  describe "Rename functionality" do
    it "finds all occurrences of variable for rename" do
      source = <<-CRYSTAL
      x = 10
      y = x + 5
      z = x * 2
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      analyzer = CrystalV2::Compiler::Semantic::Analyzer.new(program)
      analyzer.collect_symbols
      result = analyzer.resolve_names
      identifier_symbols = result.identifier_symbols

      # Find symbol 'x'
      x_symbol = analyzer.global_context.symbol_table.lookup("x")
      x_symbol.should_not be_nil
      x_symbol = x_symbol.not_nil!

      # Count all occurrences (should include declaration + 2 usages)
      occurrence_count = 0
      identifier_symbols.each do |expr_id, symbol|
        occurrence_count += 1 if symbol == x_symbol
      end

      # Should have 3 total: declaration + 2 usages
      occurrence_count.should eq(3)
    end

    it "finds method definition and calls" do
      source = <<-CRYSTAL
      def calculate(a : Int32, b : Int32) : Int32
        a + b
      end

      result1 = calculate(10, 20)
      result2 = calculate(30, 40)
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      analyzer = CrystalV2::Compiler::Semantic::Analyzer.new(program)
      analyzer.collect_symbols
      result = analyzer.resolve_names
      identifier_symbols = result.identifier_symbols

      # Find 'calculate' method
      calc_symbol = analyzer.global_context.symbol_table.lookup("calculate")
      calc_symbol.should_not be_nil
      calc_symbol = calc_symbol.not_nil!

      # Count calls (identifier_symbols contains calls, not declaration)
      call_count = 0
      identifier_symbols.each do |expr_id, symbol|
        call_count += 1 if symbol == calc_symbol
      end

      # Should have 2 calls
      call_count.should eq(2)
    end

    it "finds parameter usages within method scope" do
      source = <<-CRYSTAL
      def process(value : Int32) : Int32
        doubled = value * 2
        tripled = value * 3
        doubled + tripled
      end
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      analyzer = CrystalV2::Compiler::Semantic::Analyzer.new(program)
      analyzer.collect_symbols
      result = analyzer.resolve_names
      identifier_symbols = result.identifier_symbols

      # Find 'process' method
      process_symbol = analyzer.global_context.symbol_table.lookup("process")
      process_symbol.should_not be_nil
      process_symbol.should be_a(CrystalV2::Compiler::Semantic::MethodSymbol)
      method_symbol = process_symbol.as(CrystalV2::Compiler::Semantic::MethodSymbol)

      # Get 'value' parameter from method's scope
      value_symbol = method_symbol.scope.lookup("value")
      value_symbol.should_not be_nil
      value_symbol = value_symbol.not_nil!

      # Count usages
      usage_count = 0
      identifier_symbols.each do |expr_id, symbol|
        usage_count += 1 if symbol == value_symbol
      end

      # Should have 2 usages
      usage_count.should eq(2)
    end

    it "distinguishes between shadowed variables in different scopes" do
      source = <<-CRYSTAL
      x = 10

      def test
        x = 20
        y = x + 5
      end

      z = x * 2
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      analyzer = CrystalV2::Compiler::Semantic::Analyzer.new(program)
      analyzer.collect_symbols
      result = analyzer.resolve_names
      identifier_symbols = result.identifier_symbols

      # Find global 'x'
      global_x = analyzer.global_context.symbol_table.lookup("x")
      global_x.should_not be_nil
      global_x = global_x.not_nil!

      # Count global x references
      global_x_count = 0
      identifier_symbols.each do |expr_id, symbol|
        global_x_count += 1 if symbol == global_x
      end

      # Should have 2: declaration + usage after method (not inside method)
      global_x_count.should eq(2)
    end

    it "handles class renaming" do
      source = <<-CRYSTAL
      class Calculator
        def initialize
          @value = 0
        end
      end

      calc = Calculator.new
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      analyzer = CrystalV2::Compiler::Semantic::Analyzer.new(program)
      analyzer.collect_symbols
      result = analyzer.resolve_names
      identifier_symbols = result.identifier_symbols

      # Find Calculator class
      calc_class = analyzer.global_context.symbol_table.lookup("Calculator")
      calc_class.should_not be_nil
      calc_class = calc_class.not_nil!

      # Count references (type references in code)
      ref_count = 0
      identifier_symbols.each do |expr_id, symbol|
        ref_count += 1 if symbol == calc_class
      end

      # Should have 1 usage (Calculator.new)
      ref_count.should eq(1)
    end

    it "creates correct TextEdit ranges" do
      source = <<-CRYSTAL
      x = 10
      y = x
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      analyzer = CrystalV2::Compiler::Semantic::Analyzer.new(program)
      analyzer.collect_symbols
      result = analyzer.resolve_names
      identifier_symbols = result.identifier_symbols

      # Find 'x' symbol
      x_symbol = analyzer.global_context.symbol_table.lookup("x")
      x_symbol.should_not be_nil
      x_symbol = x_symbol.not_nil!

      # Create TextEdit objects
      edits = [] of CrystalV2::Compiler::LSP::TextEdit
      identifier_symbols.each do |expr_id, symbol|
        if symbol == x_symbol
          node = program.arena[expr_id]
          range = CrystalV2::Compiler::LSP::Range.from_span(node.span)
          edits << CrystalV2::Compiler::LSP::TextEdit.new(range: range, new_text: "newX")
        end
      end

      edits.size.should eq(2)

      # First edit (declaration) on line 0
      edits[0].range.start.line.should eq(0)
      edits[0].new_text.should eq("newX")

      # Second edit (usage) on line 1
      edits[1].range.start.line.should eq(1)
      edits[1].new_text.should eq("newX")
    end

    it "creates valid WorkspaceEdit structure" do
      source = <<-CRYSTAL
      value = 42
      result = value * 2
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      analyzer = CrystalV2::Compiler::Semantic::Analyzer.new(program)
      analyzer.collect_symbols
      result = analyzer.resolve_names
      identifier_symbols = result.identifier_symbols

      # Find 'value' symbol
      value_symbol = analyzer.global_context.symbol_table.lookup("value")
      value_symbol.should_not be_nil
      value_symbol = value_symbol.not_nil!

      # Create edits
      edits = [] of CrystalV2::Compiler::LSP::TextEdit
      identifier_symbols.each do |expr_id, symbol|
        if symbol == value_symbol
          node = program.arena[expr_id]
          range = CrystalV2::Compiler::LSP::Range.from_span(node.span)
          edits << CrystalV2::Compiler::LSP::TextEdit.new(range: range, new_text: "newValue")
        end
      end

      # Create WorkspaceEdit
      uri = "file:///test.cr"
      workspace_edit = CrystalV2::Compiler::LSP::WorkspaceEdit.new(
        changes: {uri => edits}
      )

      workspace_edit.changes.has_key?(uri).should be_true
      workspace_edit.changes[uri].size.should eq(2)
      workspace_edit.changes[uri].all? { |e| e.new_text == "newValue" }.should be_true
    end

    it "handles unused variable correctly" do
      source = <<-CRYSTAL
      unused = 42
      x = 10
      y = x + 5
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      analyzer = CrystalV2::Compiler::Semantic::Analyzer.new(program)
      analyzer.collect_symbols
      result = analyzer.resolve_names
      identifier_symbols = result.identifier_symbols

      # Find 'unused' symbol
      unused_symbol = analyzer.global_context.symbol_table.lookup("unused")
      unused_symbol.should_not be_nil
      unused_symbol = unused_symbol.not_nil!

      # Count occurrences
      occurrence_count = 0
      identifier_symbols.each do |expr_id, symbol|
        occurrence_count += 1 if symbol == unused_symbol
      end

      # Should have 1: just the declaration
      occurrence_count.should eq(1)
    end

    it "handles multiple edits correctly" do
      source = <<-CRYSTAL
      i = 0
      while i < 10
        puts i
        i = i + 1
      end
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      analyzer = CrystalV2::Compiler::Semantic::Analyzer.new(program)
      analyzer.collect_symbols
      result = analyzer.resolve_names
      identifier_symbols = result.identifier_symbols

      # Find 'i' symbol
      i_symbol = analyzer.global_context.symbol_table.lookup("i")
      i_symbol.should_not be_nil
      i_symbol = i_symbol.not_nil!

      # Count all occurrences
      occurrence_count = 0
      identifier_symbols.each do |expr_id, symbol|
        occurrence_count += 1 if symbol == i_symbol
      end

      # Should have 5 occurrences:
      # 1. i = 0 (declaration/target)
      # 2. while i < 10 (condition)
      # 3. puts i (argument)
      # 4. i = i + 1 (left side - target/reassignment)
      # 5. i + 1 (right side - operand)
      occurrence_count.should eq(5)
    end

    it "validates symbol can be renamed (not primitive type)" do
      source = <<-CRYSTAL
      x = 10
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      analyzer = CrystalV2::Compiler::Semantic::Analyzer.new(program)
      analyzer.collect_symbols
      analyzer.resolve_names  # Need this to populate symbol table

      # Variable 'x' should be renameable
      x_symbol = analyzer.global_context.symbol_table.lookup("x")
      x_symbol.should_not be_nil
      x_symbol = x_symbol.not_nil!

      # Check it has valid node_id
      x_symbol.node_id.invalid?.should be_false
    end

    it "preserves correct order of edits" do
      source = <<-CRYSTAL
      data = [1, 2, 3]
      result = data.map { |x| x * 2 }
      puts data
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      analyzer = CrystalV2::Compiler::Semantic::Analyzer.new(program)
      analyzer.collect_symbols
      result = analyzer.resolve_names
      identifier_symbols = result.identifier_symbols

      # Find 'data' symbol
      data_symbol = analyzer.global_context.symbol_table.lookup("data")
      data_symbol.should_not be_nil
      data_symbol = data_symbol.not_nil!

      # Collect edits
      edits = [] of CrystalV2::Compiler::LSP::TextEdit
      identifier_symbols.each do |expr_id, symbol|
        if symbol == data_symbol
          node = program.arena[expr_id]
          range = CrystalV2::Compiler::LSP::Range.from_span(node.span)
          edits << CrystalV2::Compiler::LSP::TextEdit.new(range: range, new_text: "items")
        end
      end

      # Edits should be in source order (line 0, then line 1, then line 2)
      edits.size.should be >= 2
      edits.each_with_index do |edit, i|
        if i > 0
          # Each subsequent edit should be at same or later line
          edit.range.start.line.should be >= edits[i-1].range.start.line
        end
      end
    end

    # Control flow structure tests - critical for catching missing visitor implementations

    it "handles variables in if statement" do
      source = <<-CRYSTAL
      x = 5
      if x > 0
        puts x
      end
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      analyzer = CrystalV2::Compiler::Semantic::Analyzer.new(program)
      analyzer.collect_symbols
      result = analyzer.resolve_names
      identifier_symbols = result.identifier_symbols

      x_symbol = analyzer.global_context.symbol_table.lookup("x")
      x_symbol.should_not be_nil

      # Count: declaration, condition, then body
      occurrence_count = identifier_symbols.count { |_, symbol| symbol == x_symbol }
      occurrence_count.should eq(3)
    end

    it "handles variables in if-elsif-else statement" do
      source = <<-CRYSTAL
      status = 1
      if status == 0
        puts status
      elsif status == 1
        puts status
      else
        puts status
      end
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      analyzer = CrystalV2::Compiler::Semantic::Analyzer.new(program)
      analyzer.collect_symbols
      result = analyzer.resolve_names
      identifier_symbols = result.identifier_symbols

      status_symbol = analyzer.global_context.symbol_table.lookup("status")
      status_symbol.should_not be_nil

      # Count: declaration + if condition + then body + elsif condition + elsif body + else body = 6
      occurrence_count = identifier_symbols.count { |_, symbol| symbol == status_symbol }
      occurrence_count.should eq(6)
    end

    it "handles variables in unless statement" do
      source = <<-CRYSTAL
      flag = false
      unless flag
        puts flag
      end
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      analyzer = CrystalV2::Compiler::Semantic::Analyzer.new(program)
      analyzer.collect_symbols
      result = analyzer.resolve_names
      identifier_symbols = result.identifier_symbols

      flag_symbol = analyzer.global_context.symbol_table.lookup("flag")
      flag_symbol.should_not be_nil

      # Count: declaration, condition, body
      occurrence_count = identifier_symbols.count { |_, symbol| symbol == flag_symbol }
      occurrence_count.should eq(3)
    end

    it "handles variables in until loop" do
      source = <<-CRYSTAL
      counter = 0
      until counter >= 5
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

      counter_symbol = analyzer.global_context.symbol_table.lookup("counter")
      counter_symbol.should_not be_nil

      # Count: declaration, condition, left side of assignment, right side of addition
      occurrence_count = identifier_symbols.count { |_, symbol| symbol == counter_symbol }
      occurrence_count.should eq(4)
    end

    it "handles variables in loop statement" do
      source = <<-CRYSTAL
      total = 0
      loop do
        total = total + 1
        break
      end
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      analyzer = CrystalV2::Compiler::Semantic::Analyzer.new(program)
      analyzer.collect_symbols
      result = analyzer.resolve_names
      identifier_symbols = result.identifier_symbols

      total_symbol = analyzer.global_context.symbol_table.lookup("total")
      total_symbol.should_not be_nil

      # Count: declaration, left side of assignment, right side of addition
      occurrence_count = identifier_symbols.count { |_, symbol| symbol == total_symbol }
      occurrence_count.should eq(3)
    end

    it "handles nested control flow correctly" do
      source = <<-CRYSTAL
      value = 10
      if value > 0
        while value > 5
          value = value - 1
        end
      end
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      analyzer = CrystalV2::Compiler::Semantic::Analyzer.new(program)
      analyzer.collect_symbols
      result = analyzer.resolve_names
      identifier_symbols = result.identifier_symbols

      value_symbol = analyzer.global_context.symbol_table.lookup("value")
      value_symbol.should_not be_nil

      # Count: declaration, if condition, while condition, left assignment, right subtraction
      occurrence_count = identifier_symbols.count { |_, symbol| symbol == value_symbol }
      occurrence_count.should eq(5)
    end
  end
end
