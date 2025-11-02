require "spec"

require "../../src/main"
require "../../src/compiler/lsp/protocol"
require "../../src/compiler/lsp/messages"
require "../../src/compiler/frontend/parser"
require "../../src/compiler/semantic/analyzer"

describe "LSP References" do
  describe "ReferenceContext struct" do
    it "creates context with includeDeclaration flag" do
      context = CrystalV2::Compiler::LSP::ReferenceContext.new(include_declaration: true)
      context.include_declaration.should be_true

      context_false = CrystalV2::Compiler::LSP::ReferenceContext.new(include_declaration: false)
      context_false.include_declaration.should be_false
    end

    it "serializes to JSON with camelCase" do
      context = CrystalV2::Compiler::LSP::ReferenceContext.new(include_declaration: true)
      json = context.to_json
      json.should contain("\"includeDeclaration\"")
      json.should contain("true")
    end
  end

  describe "ReferenceParams struct" do
    it "creates params with all required fields" do
      text_doc = CrystalV2::Compiler::LSP::TextDocumentIdentifier.new(uri: "file:///test.cr")
      position = CrystalV2::Compiler::LSP::Position.new(line: 5, character: 10)
      context = CrystalV2::Compiler::LSP::ReferenceContext.new(include_declaration: true)

      params = CrystalV2::Compiler::LSP::ReferenceParams.new(
        text_document: text_doc,
        position: position,
        context: context
      )

      params.text_document.uri.should eq("file:///test.cr")
      params.position.line.should eq(5)
      params.position.character.should eq(10)
      params.context.include_declaration.should be_true
    end

    it "serializes to JSON correctly" do
      text_doc = CrystalV2::Compiler::LSP::TextDocumentIdentifier.new(uri: "file:///test.cr")
      position = CrystalV2::Compiler::LSP::Position.new(line: 5, character: 10)
      context = CrystalV2::Compiler::LSP::ReferenceContext.new(include_declaration: false)

      params = CrystalV2::Compiler::LSP::ReferenceParams.new(
        text_document: text_doc,
        position: position,
        context: context
      )

      json = params.to_json
      json.should contain("\"textDocument\"")
      json.should contain("\"position\"")
      json.should contain("\"context\"")
      json.should contain("\"includeDeclaration\"")
    end
  end

  describe "Find references functionality" do
    it "finds all references to variable (include declaration)" do
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

      # Find symbol 'x' (first occurrence - declaration)
      # Line 0 (x = 10), x is at column 6 (after spaces)
      # We need to find the target node in identifier_symbols
      # Let's iterate and find the symbol named 'x'
      x_symbol = analyzer.global_context.symbol_table.lookup("x")
      x_symbol.should_not be_nil
      x_symbol = x_symbol.not_nil!

      # Count references using identifier_symbols
      reference_count = 0
      identifier_symbols.each do |expr_id, symbol|
        if symbol == x_symbol
          reference_count += 1
        end
      end

      # Should have 3 references total: declaration + 2 usages
      reference_count.should eq(3)
    end

    it "finds references excluding declaration" do
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

      # Count references excluding declaration
      usage_count = 0
      identifier_symbols.each do |expr_id, symbol|
        if symbol == x_symbol && expr_id != x_symbol.node_id
          usage_count += 1
        end
      end

      # Should have 2 usages (excluding declaration)
      usage_count.should eq(2)
    end

    it "finds references to method" do
      source = <<-CRYSTAL
      def calculate(x : Int32) : Int32
        x + 1
      end

      result1 = calculate(10)
      result2 = calculate(20)
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

      # Count references
      reference_count = 0
      identifier_symbols.each do |expr_id, symbol|
        if symbol == calc_symbol
          reference_count += 1
        end
      end

      # Should have 2 calls (identifier_symbols contains only Call nodes, not method declaration)
      reference_count.should eq(2)
    end

    it "finds references to parameter" do
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

      # Get 'value' parameter symbol from method's scope
      value_symbol = method_symbol.scope.lookup("value")
      value_symbol.should_not be_nil
      value_symbol = value_symbol.not_nil!

      # Count references to 'value'
      reference_count = 0
      identifier_symbols.each do |expr_id, symbol|
        if symbol == value_symbol
          reference_count += 1
        end
      end

      # Should have 2 usages (identifier_symbols contains only uses, not parameter declaration)
      reference_count.should eq(2)
    end

    it "handles unused variable (no references)" do
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

      # Count references
      usage_count = 0
      identifier_symbols.each do |expr_id, symbol|
        if symbol == unused_symbol && expr_id != unused_symbol.node_id
          usage_count += 1
        end
      end

      # Should have 0 usages (only declaration)
      usage_count.should eq(0)
    end

    it "finds references across scopes" do
      source = <<-CRYSTAL
      x = 10

      def use_x
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
      x_symbol = analyzer.global_context.symbol_table.lookup("x")
      x_symbol.should_not be_nil
      x_symbol = x_symbol.not_nil!

      # Count references
      reference_count = 0
      identifier_symbols.each do |expr_id, symbol|
        if symbol == x_symbol
          reference_count += 1
        end
      end

      # Should have declaration + usage in method + usage after method = 3
      reference_count.should eq(3)
    end

    it "creates correct Location objects" do
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

      # Create locations
      locations = [] of CrystalV2::Compiler::LSP::Location
      identifier_symbols.each do |expr_id, symbol|
        if symbol == x_symbol
          node = program.arena[expr_id]
          range = CrystalV2::Compiler::LSP::Range.from_span(node.span)
          location = CrystalV2::Compiler::LSP::Location.new(uri: "file:///test.cr", range: range)
          locations << location
        end
      end

      locations.size.should eq(2)

      # First location (declaration) should be at line 0
      locations[0].uri.should eq("file:///test.cr")
      locations[0].range.start.line.should eq(0)

      # Second location (usage) should be at line 1
      locations[1].range.start.line.should eq(1)
    end
  end
end
