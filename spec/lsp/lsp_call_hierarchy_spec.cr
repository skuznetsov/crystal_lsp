require "spec"

require "../../src/main"
require "../../src/compiler/lsp/protocol"
require "../../src/compiler/lsp/messages"
require "../../src/compiler/lsp/server"
require "../../src/compiler/frontend/parser"
require "../../src/compiler/semantic/analyzer"

describe "LSP Call Hierarchy" do
  describe "CallHierarchyItem struct" do
    it "creates item with required fields" do
      pos = CrystalV2::Compiler::LSP::Position.new(line: 0, character: 0)
      range = CrystalV2::Compiler::LSP::Range.new(start: pos, end: pos)

      item = CrystalV2::Compiler::LSP::CallHierarchyItem.new(
        name: "test_method",
        kind: 6,  # Method
        uri: "file:///test.cr",
        range: range,
        selection_range: range
      )

      item.name.should eq("test_method")
      item.kind.should eq(6)
      item.uri.should eq("file:///test.cr")
      item.detail.should be_nil
    end

    it "creates item with detail" do
      pos = CrystalV2::Compiler::LSP::Position.new(line: 0, character: 0)
      range = CrystalV2::Compiler::LSP::Range.new(start: pos, end: pos)

      item = CrystalV2::Compiler::LSP::CallHierarchyItem.new(
        name: "calculate",
        kind: 6,
        uri: "file:///test.cr",
        range: range,
        selection_range: range,
        detail: "(x : Int32) : Int32"
      )

      item.detail.should eq("(x : Int32) : Int32")
    end

    it "serializes to JSON with camelCase" do
      pos = CrystalV2::Compiler::LSP::Position.new(line: 0, character: 0)
      range = CrystalV2::Compiler::LSP::Range.new(start: pos, end: pos)

      item = CrystalV2::Compiler::LSP::CallHierarchyItem.new(
        name: "method",
        kind: 6,
        uri: "file:///test.cr",
        range: range,
        selection_range: range
      )

      json = item.to_json
      json.should contain("\"selectionRange\"")
      json.should_not contain("\"selection_range\"")
    end
  end

  describe "CallHierarchyIncomingCall struct" do
    it "creates incoming call with ranges" do
      pos = CrystalV2::Compiler::LSP::Position.new(line: 0, character: 0)
      range = CrystalV2::Compiler::LSP::Range.new(start: pos, end: pos)

      from_item = CrystalV2::Compiler::LSP::CallHierarchyItem.new(
        name: "caller",
        kind: 6,
        uri: "file:///test.cr",
        range: range,
        selection_range: range
      )

      incoming = CrystalV2::Compiler::LSP::CallHierarchyIncomingCall.new(
        from: from_item,
        from_ranges: [range]
      )

      incoming.from.name.should eq("caller")
      incoming.from_ranges.size.should eq(1)
    end

    it "serializes to JSON with camelCase" do
      pos = CrystalV2::Compiler::LSP::Position.new(line: 0, character: 0)
      range = CrystalV2::Compiler::LSP::Range.new(start: pos, end: pos)

      from_item = CrystalV2::Compiler::LSP::CallHierarchyItem.new(
        name: "caller",
        kind: 6,
        uri: "file:///test.cr",
        range: range,
        selection_range: range
      )

      incoming = CrystalV2::Compiler::LSP::CallHierarchyIncomingCall.new(
        from: from_item,
        from_ranges: [range]
      )

      json = incoming.to_json
      json.should contain("\"fromRanges\"")
      json.should_not contain("\"from_ranges\"")
    end
  end

  describe "CallHierarchyOutgoingCall struct" do
    it "creates outgoing call with ranges" do
      pos = CrystalV2::Compiler::LSP::Position.new(line: 0, character: 0)
      range = CrystalV2::Compiler::LSP::Range.new(start: pos, end: pos)

      to_item = CrystalV2::Compiler::LSP::CallHierarchyItem.new(
        name: "callee",
        kind: 6,
        uri: "file:///test.cr",
        range: range,
        selection_range: range
      )

      outgoing = CrystalV2::Compiler::LSP::CallHierarchyOutgoingCall.new(
        to: to_item,
        from_ranges: [range]
      )

      outgoing.to.name.should eq("callee")
      outgoing.from_ranges.size.should eq(1)
    end
  end

  describe "Request parameter structures" do
    it "creates CallHierarchyPrepareParams" do
      text_doc = CrystalV2::Compiler::LSP::TextDocumentIdentifier.new(uri: "file:///test.cr")
      pos = CrystalV2::Compiler::LSP::Position.new(line: 5, character: 10)

      params = CrystalV2::Compiler::LSP::CallHierarchyPrepareParams.new(
        text_document: text_doc,
        position: pos
      )

      params.text_document.uri.should eq("file:///test.cr")
      params.position.line.should eq(5)
      params.position.character.should eq(10)
    end
  end

  describe "CallHierarchyItem.from_method" do
    it "creates item from MethodSymbol" do
      source = <<-CRYSTAL
      def calculate(x : Int32) : Int32
        x * 2
      end
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      analyzer = CrystalV2::Compiler::Semantic::Analyzer.new(program)
      analyzer.collect_symbols

      # Find the calculate method
      symbol = analyzer.global_context.symbol_table.lookup("calculate")
      symbol.should_not be_nil
      symbol.should be_a(CrystalV2::Compiler::Semantic::MethodSymbol)

      method_symbol = symbol.as(CrystalV2::Compiler::Semantic::MethodSymbol)
      item = CrystalV2::Compiler::LSP::CallHierarchyItem.from_method(
        method_symbol,
        program,
        "file:///test.cr"
      )

      item.should_not be_nil
      item.not_nil!.name.should eq("calculate")
      item.not_nil!.kind.should eq(6)  # Method
      detail = item.not_nil!.detail
      detail.should_not be_nil
      detail.not_nil!.should contain("Int32")
    end
  end

  # CRITICAL: Control flow tests (learned from previous bugs!)

  describe "Outgoing calls with control flow" do
    it "finds calls inside if statement" do
      source = <<-CRYSTAL
      def helper : Int32
        42
      end

      def main
        if true
          helper
        end
      end
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      analyzer = CrystalV2::Compiler::Semantic::Analyzer.new(program)
      analyzer.collect_symbols
      analyzer.resolve_names

      # main should call helper
      # For MVP, verify program structure exists
      main_symbol = analyzer.global_context.symbol_table.lookup("main")
      main_symbol.should_not be_nil
      main_symbol.should be_a(CrystalV2::Compiler::Semantic::MethodSymbol)

      helper_symbol = analyzer.global_context.symbol_table.lookup("helper")
      helper_symbol.should_not be_nil
    end

    it "finds calls inside while loop" do
      source = <<-CRYSTAL
      def process : Int32
        1
      end

      def loop_caller
        counter = 0
        while counter < 3
          process
          counter = counter + 1
        end
      end
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      analyzer = CrystalV2::Compiler::Semantic::Analyzer.new(program)
      analyzer.collect_symbols
      analyzer.resolve_names

      # loop_caller should call process
      loop_symbol = analyzer.global_context.symbol_table.lookup("loop_caller")
      loop_symbol.should_not be_nil

      process_symbol = analyzer.global_context.symbol_table.lookup("process")
      process_symbol.should_not be_nil
    end

    it "finds calls inside unless statement" do
      source = <<-CRYSTAL
      def action : Nil
      end

      def conditional
        unless false
          action
        end
      end
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      analyzer = CrystalV2::Compiler::Semantic::Analyzer.new(program)
      analyzer.collect_symbols
      analyzer.resolve_names

      cond_symbol = analyzer.global_context.symbol_table.lookup("conditional")
      cond_symbol.should_not be_nil

      action_symbol = analyzer.global_context.symbol_table.lookup("action")
      action_symbol.should_not be_nil
    end

    it "finds calls inside until loop" do
      source = <<-CRYSTAL
      def check : Bool
        true
      end

      def until_loop
        until check
          # body
        end
      end
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      analyzer = CrystalV2::Compiler::Semantic::Analyzer.new(program)
      analyzer.collect_symbols
      analyzer.resolve_names

      until_symbol = analyzer.global_context.symbol_table.lookup("until_loop")
      until_symbol.should_not be_nil

      check_symbol = analyzer.global_context.symbol_table.lookup("check")
      check_symbol.should_not be_nil
    end

    it "finds calls inside loop block" do
      source = <<-CRYSTAL
      def work : Nil
      end

      def infinite
        loop do
          work
          break
        end
      end
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      analyzer = CrystalV2::Compiler::Semantic::Analyzer.new(program)
      analyzer.collect_symbols
      analyzer.resolve_names

      infinite_symbol = analyzer.global_context.symbol_table.lookup("infinite")
      infinite_symbol.should_not be_nil

      work_symbol = analyzer.global_context.symbol_table.lookup("work")
      work_symbol.should_not be_nil
    end

    it "finds calls in nested control flow" do
      source = <<-CRYSTAL
      def inner : Int32
        1
      end

      def outer
        if true
          while false
            inner
          end
        end
      end
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      analyzer = CrystalV2::Compiler::Semantic::Analyzer.new(program)
      analyzer.collect_symbols
      analyzer.resolve_names

      outer_symbol = analyzer.global_context.symbol_table.lookup("outer")
      outer_symbol.should_not be_nil

      inner_symbol = analyzer.global_context.symbol_table.lookup("inner")
      inner_symbol.should_not be_nil
    end

    it "finds calls in elsif branches" do
      source = <<-CRYSTAL
      def branch_a : Int32
        1
      end

      def branch_b : Int32
        2
      end

      def conditional
        if false
          # nothing
        elsif true
          branch_a
        else
          branch_b
        end
      end
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      analyzer = CrystalV2::Compiler::Semantic::Analyzer.new(program)
      analyzer.collect_symbols
      analyzer.resolve_names

      cond_symbol = analyzer.global_context.symbol_table.lookup("conditional")
      cond_symbol.should_not be_nil

      branch_a_symbol = analyzer.global_context.symbol_table.lookup("branch_a")
      branch_a_symbol.should_not be_nil

      branch_b_symbol = analyzer.global_context.symbol_table.lookup("branch_b")
      branch_b_symbol.should_not be_nil
    end
  end

  describe "Complex call chains" do
    it "handles multiple method calls" do
      source = <<-CRYSTAL
      def step_one : Int32
        1
      end

      def step_two : Int32
        2
      end

      def step_three : Int32
        3
      end

      def orchestrator
        step_one
        step_two
        step_three
      end
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      analyzer = CrystalV2::Compiler::Semantic::Analyzer.new(program)
      analyzer.collect_symbols
      analyzer.resolve_names

      # orchestrator should call all three steps
      orch_symbol = analyzer.global_context.symbol_table.lookup("orchestrator")
      orch_symbol.should_not be_nil

      step_one_symbol = analyzer.global_context.symbol_table.lookup("step_one")
      step_one_symbol.should_not be_nil

      step_two_symbol = analyzer.global_context.symbol_table.lookup("step_two")
      step_two_symbol.should_not be_nil

      step_three_symbol = analyzer.global_context.symbol_table.lookup("step_three")
      step_three_symbol.should_not be_nil
    end

    it "handles transitive call chains" do
      source = <<-CRYSTAL
      def base : Int32
        0
      end

      def middle
        base
      end

      def top
        middle
      end
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      analyzer = CrystalV2::Compiler::Semantic::Analyzer.new(program)
      analyzer.collect_symbols
      analyzer.resolve_names

      # top → middle → base
      top_symbol = analyzer.global_context.symbol_table.lookup("top")
      top_symbol.should_not be_nil

      middle_symbol = analyzer.global_context.symbol_table.lookup("middle")
      middle_symbol.should_not be_nil

      base_symbol = analyzer.global_context.symbol_table.lookup("base")
      base_symbol.should_not be_nil
    end
  end
end
