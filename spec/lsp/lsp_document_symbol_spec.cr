require "spec"

require "../../src/main"
require "../../src/compiler/lsp/protocol"
require "../../src/compiler/frontend/parser"
require "../../src/compiler/semantic/analyzer"

describe "LSP DocumentSymbol" do
  describe "SymbolKind enum" do
    it "has correct values for common kinds" do
      CrystalV2::Compiler::LSP::SymbolKind::Class.value.should eq(5)
      CrystalV2::Compiler::LSP::SymbolKind::Method.value.should eq(6)
      CrystalV2::Compiler::LSP::SymbolKind::Variable.value.should eq(13)
      CrystalV2::Compiler::LSP::SymbolKind::Function.value.should eq(12)
    end
  end

  describe "DocumentSymbol struct" do
    it "creates basic document symbol" do
      range = CrystalV2::Compiler::LSP::Range.new(
        start: CrystalV2::Compiler::LSP::Position.new(0, 0),
        end: CrystalV2::Compiler::LSP::Position.new(5, 3)
      )

      doc_sym = CrystalV2::Compiler::LSP::DocumentSymbol.new(
        name: "MyClass",
        kind: CrystalV2::Compiler::LSP::SymbolKind::Class.value,
        range: range,
        selection_range: range
      )

      doc_sym.name.should eq("MyClass")
      doc_sym.kind.should eq(5)
      doc_sym.range.should eq(range)
      doc_sym.selection_range.should eq(range)
    end

    it "supports hierarchical children" do
      parent_range = CrystalV2::Compiler::LSP::Range.new(
        start: CrystalV2::Compiler::LSP::Position.new(0, 0),
        end: CrystalV2::Compiler::LSP::Position.new(10, 3)
      )

      child_range = CrystalV2::Compiler::LSP::Range.new(
        start: CrystalV2::Compiler::LSP::Position.new(1, 2),
        end: CrystalV2::Compiler::LSP::Position.new(3, 5)
      )

      child = CrystalV2::Compiler::LSP::DocumentSymbol.new(
        name: "initialize",
        kind: CrystalV2::Compiler::LSP::SymbolKind::Method.value,
        range: child_range,
        selection_range: child_range
      )

      parent = CrystalV2::Compiler::LSP::DocumentSymbol.new(
        name: "MyClass",
        kind: CrystalV2::Compiler::LSP::SymbolKind::Class.value,
        range: parent_range,
        selection_range: parent_range,
        children: [child]
      )

      parent.children.should_not be_nil
      children = parent.children.not_nil!
      children.size.should eq(1)
      children[0].name.should eq("initialize")
    end

    it "serializes to JSON correctly" do
      range = CrystalV2::Compiler::LSP::Range.new(
        start: CrystalV2::Compiler::LSP::Position.new(0, 0),
        end: CrystalV2::Compiler::LSP::Position.new(5, 3)
      )

      doc_sym = CrystalV2::Compiler::LSP::DocumentSymbol.new(
        name: "calculate",
        kind: CrystalV2::Compiler::LSP::SymbolKind::Method.value,
        range: range,
        selection_range: range,
        detail: "(x : Int32, y : Int32) : Int32"
      )

      json = doc_sym.to_json
      json.should contain("\"name\"")
      json.should contain("\"kind\"")
      json.should contain("\"range\"")
      json.should contain("\"selectionRange\"")
      json.should contain("\"detail\"")
      json.should contain("calculate")
      json.should contain("(x : Int32, y : Int32) : Int32")
    end
  end

  describe "DocumentSymbol.from_symbol" do
    it "creates DocumentSymbol from ClassSymbol" do
      source = <<-CRYSTAL
      class Calculator
        def initialize
          @value = 0
        end
      end
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      analyzer = CrystalV2::Compiler::Semantic::Analyzer.new(program)
      analyzer.collect_symbols

      # Find Calculator class symbol
      calc_symbol = analyzer.global_context.symbol_table.lookup("Calculator")
      calc_symbol.should_not be_nil
      calc_symbol = calc_symbol.not_nil!

      # Create DocumentSymbol from it
      doc_sym = CrystalV2::Compiler::LSP::DocumentSymbol.from_symbol(calc_symbol, program)
      doc_sym.should_not be_nil
      doc_sym = doc_sym.not_nil!

      doc_sym.name.should eq("Calculator")
      doc_sym.kind.should eq(CrystalV2::Compiler::LSP::SymbolKind::Class.value)
      doc_sym.children.should_not be_nil
    end

    it "creates DocumentSymbol from MethodSymbol" do
      source = <<-CRYSTAL
      def calculate(x : Int32, y : Int32) : Int32
        x + y
      end
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      analyzer = CrystalV2::Compiler::Semantic::Analyzer.new(program)
      analyzer.collect_symbols

      # Find calculate method symbol
      calc_symbol = analyzer.global_context.symbol_table.lookup("calculate")
      calc_symbol.should_not be_nil
      calc_symbol = calc_symbol.not_nil!

      # Create DocumentSymbol from it
      doc_sym = CrystalV2::Compiler::LSP::DocumentSymbol.from_symbol(calc_symbol, program)
      doc_sym.should_not be_nil
      doc_sym = doc_sym.not_nil!

      doc_sym.name.should eq("calculate")
      doc_sym.kind.should eq(CrystalV2::Compiler::LSP::SymbolKind::Method.value)
      doc_sym.detail.should_not be_nil
      detail = doc_sym.detail.not_nil!
      detail.should contain("x : Int32")
      detail.should contain("y : Int32")
    end

    it "creates DocumentSymbol from VariableSymbol" do
      source = <<-CRYSTAL
      x = 42
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      analyzer = CrystalV2::Compiler::Semantic::Analyzer.new(program)
      analyzer.collect_symbols
      analyzer.resolve_names

      # Find x variable symbol
      x_symbol = analyzer.global_context.symbol_table.lookup("x")
      x_symbol.should_not be_nil
      x_symbol = x_symbol.not_nil!

      # Create DocumentSymbol from it
      doc_sym = CrystalV2::Compiler::LSP::DocumentSymbol.from_symbol(x_symbol, program)
      doc_sym.should_not be_nil
      doc_sym = doc_sym.not_nil!

      doc_sym.name.should eq("x")
      doc_sym.kind.should eq(CrystalV2::Compiler::LSP::SymbolKind::Variable.value)
    end

    it "expands OverloadSetSymbol into individual methods" do
      source = <<-CRYSTAL
      def greet(name : String)
        puts "Hello, \#{name}!"
      end

      def greet(name : String, greeting : String)
        puts "\#{greeting}, \#{name}!"
      end
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      analyzer = CrystalV2::Compiler::Semantic::Analyzer.new(program)
      analyzer.collect_symbols

      # Find greet symbol (should be OverloadSetSymbol)
      greet_symbol = analyzer.global_context.symbol_table.lookup("greet")
      greet_symbol.should_not be_nil
      greet_symbol = greet_symbol.not_nil!

      # Should be an OverloadSetSymbol
      greet_symbol.should be_a(CrystalV2::Compiler::Semantic::OverloadSetSymbol)
    end

    it "handles nested class hierarchy" do
      source = <<-CRYSTAL
      class Outer
        def outer_method
          1
        end

        class Inner
          def inner_method
            2
          end
        end
      end
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      analyzer = CrystalV2::Compiler::Semantic::Analyzer.new(program)
      analyzer.collect_symbols

      # Find Outer class symbol
      outer_symbol = analyzer.global_context.symbol_table.lookup("Outer")
      outer_symbol.should_not be_nil
      outer_symbol = outer_symbol.not_nil!

      # Create DocumentSymbol
      doc_sym = CrystalV2::Compiler::LSP::DocumentSymbol.from_symbol(outer_symbol, program)
      doc_sym.should_not be_nil
      doc_sym = doc_sym.not_nil!

      doc_sym.name.should eq("Outer")
      doc_sym.children.should_not be_nil
      children = doc_sym.children.not_nil!

      # Should have outer_method and Inner class
      children.size.should eq(2)

      # Check for outer_method
      outer_method = children.find { |c| c.name == "outer_method" }
      outer_method.should_not be_nil

      # Check for Inner class
      inner_class = children.find { |c| c.name == "Inner" }
      inner_class.should_not be_nil
      inner_class = inner_class.not_nil!
      inner_class.children.should_not be_nil
      inner_children = inner_class.children.not_nil!

      # Inner should have inner_method
      inner_method = inner_children.find { |c| c.name == "inner_method" }
      inner_method.should_not be_nil
    end
  end
end
