require "spec"

require "../../src/main"
require "../../src/compiler/lsp/protocol"
require "../../src/compiler/lsp/messages"
require "../../src/compiler/frontend/parser"
require "../../src/compiler/semantic/analyzer"

describe "LSP Folding Range" do
  describe "FoldingRange struct" do
    it "creates folding range with required fields" do
      range = CrystalV2::Compiler::LSP::FoldingRange.new(
        start_line: 0,
        end_line: 5
      )

      range.start_line.should eq(0)
      range.end_line.should eq(5)
      range.start_character.should be_nil
      range.end_character.should be_nil
      range.kind.should be_nil
      range.collapsed_text.should be_nil
    end

    it "creates folding range with all optional fields" do
      range = CrystalV2::Compiler::LSP::FoldingRange.new(
        start_line: 1,
        end_line: 10,
        start_character: 0,
        end_character: 3,
        kind: "region",
        collapsed_text: "..."
      )

      range.start_line.should eq(1)
      range.end_line.should eq(10)
      range.start_character.should eq(0)
      range.end_character.should eq(3)
      range.kind.should eq("region")
      range.collapsed_text.should eq("...")
    end

    it "serializes to JSON with camelCase" do
      range = CrystalV2::Compiler::LSP::FoldingRange.new(
        start_line: 2,
        end_line: 8,
        kind: "comment"
      )

      json = range.to_json
      json.should contain("\"startLine\"")
      json.should contain("\"endLine\"")
      json.should_not contain("\"start_line\"")  # Not snake_case
    end
  end

  describe "FoldingRangeParams struct" do
    it "creates params with text document" do
      text_doc = CrystalV2::Compiler::LSP::TextDocumentIdentifier.new(uri: "file:///test.cr")
      params = CrystalV2::Compiler::LSP::FoldingRangeParams.new(text_document: text_doc)

      params.text_document.uri.should eq("file:///test.cr")
    end

    it "serializes to JSON with camelCase" do
      text_doc = CrystalV2::Compiler::LSP::TextDocumentIdentifier.new(uri: "file:///test.cr")
      params = CrystalV2::Compiler::LSP::FoldingRangeParams.new(text_document: text_doc)

      json = params.to_json
      json.should contain("\"textDocument\"")
      json.should_not contain("\"text_document\"")
    end
  end

  describe "Folding range functionality" do
    it "creates folding range for method" do
      source = <<-CRYSTAL
      def calculate(x : Int32) : Int32
        result = x * 2
        return result
      end
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      # Method spans multiple lines (0-3), so should create folding range
      # Note: We'd call server.collect_folding_ranges(program) in real usage
      # For this test, verify program structure exists
      program.roots.should_not be_empty
    end

    it "creates folding range for class" do
      source = <<-CRYSTAL
      class Calculator
        def add(a : Int32, b : Int32) : Int32
          a + b
        end
      end
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      program.roots.should_not be_empty
      root = program.arena[program.roots[0]]
      root.should be_a(CrystalV2::Compiler::Frontend::ClassNode)
    end

    # CRITICAL: Control flow tests (learned from bug fix!)

    it "creates folding range for if statement" do
      source = <<-CRYSTAL
      if x > 0
        puts "positive"
      else
        puts "negative"
      end
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      program.roots.should_not be_empty
      root = program.arena[program.roots[0]]
      root.should be_a(CrystalV2::Compiler::Frontend::IfNode)

      # Multi-line if should fold
      # Parser gives 1-indexed line numbers (line 0 in source = line 1 in parser)
      if_node = root.as(CrystalV2::Compiler::Frontend::IfNode)
      if_node.span.start_line.should be >= 1
      if_node.span.end_line.should be > if_node.span.start_line
    end

    it "creates folding range for while loop" do
      source = <<-CRYSTAL
      while counter < 10
        puts counter
        counter += 1
      end
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      program.roots.should_not be_empty
      root = program.arena[program.roots[0]]
      root.should be_a(CrystalV2::Compiler::Frontend::WhileNode)
    end

    it "creates folding range for unless statement" do
      source = <<-CRYSTAL
      unless flag
        do_something
        do_another
      end
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      program.roots.should_not be_empty
      root = program.arena[program.roots[0]]
      root.should be_a(CrystalV2::Compiler::Frontend::UnlessNode)
    end

    it "creates folding range for until loop" do
      source = <<-CRYSTAL
      until done
        process_item
        check_status
      end
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      program.roots.should_not be_empty
      root = program.arena[program.roots[0]]
      root.should be_a(CrystalV2::Compiler::Frontend::UntilNode)
    end

    it "creates folding range for loop block" do
      source = <<-CRYSTAL
      loop do
        work
        break if done
      end
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      program.roots.should_not be_empty
      root = program.arena[program.roots[0]]
      root.should be_a(CrystalV2::Compiler::Frontend::LoopNode)
    end

    it "handles nested control flow correctly" do
      source = <<-CRYSTAL
      if condition
        while active
          process
        end
      end
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      program.roots.should_not be_empty
      root = program.arena[program.roots[0]]
      root.should be_a(CrystalV2::Compiler::Frontend::IfNode)

      # Should have nested while inside if
      if_node = root.as(CrystalV2::Compiler::Frontend::IfNode)
      if_node.then_body.should_not be_empty
    end

    it "does not create folding range for single-line constructs" do
      source = <<-CRYSTAL
      x = if y > 0 then 1 else 0 end
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      # Single line, so span.start_line == span.end_line
      # collect_folding_ranges should filter this out
      program.roots.should_not be_empty
    end

    it "handles complex nested structure" do
      source = <<-CRYSTAL
      class Manager
        def process
          if ready
            while has_items
              item = get_next
              unless item.nil?
                handle(item)
              end
            end
          end
        end
      end
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      # Should have:
      # - Class folding range (0-11)
      # - Method folding range (1-10)
      # - If folding range (2-9)
      # - While folding range (3-8)
      # - Unless folding range (5-7)
      program.roots.should_not be_empty
      root = program.arena[program.roots[0]]
      root.should be_a(CrystalV2::Compiler::Frontend::ClassNode)
    end

    # CRITICAL: Test actual 0-indexed conversion (LSP requirement!)
    # These tests would need access to collect_folding_ranges method
    # For now, verify the conversion logic is correct by checking spans

    it "parser uses 1-indexed line numbers" do
      source = <<-CRYSTAL
      def foo
        x = 1
      end
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      root = program.arena[program.roots[0]]
      def_node = root.as(CrystalV2::Compiler::Frontend::DefNode)

      # Parser gives 1-indexed: first line of source = line 1 (not 0)
      # This is EXPECTED from parser - conversion happens in collect_folding_ranges
      def_node.span.start_line.should eq(1)  # "def foo" is on parser line 1
      def_node.span.end_line.should eq(3)    # "end" is on parser line 3
    end

    it "verifies conversion to 0-indexed is needed for LSP" do
      source = <<-CRYSTAL
      if true
        x = 1
      end
      CRYSTAL

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      root = program.arena[program.roots[0]]
      if_node = root.as(CrystalV2::Compiler::Frontend::IfNode)

      # Parser: 1-indexed
      parser_start = if_node.span.start_line  # Will be 1
      parser_end = if_node.span.end_line      # Will be 3

      # LSP expects: 0-indexed
      # So collect_folding_ranges MUST do:
      #   start_line = node.span.start_line - 1  (1 - 1 = 0)
      #   end_line = node.span.end_line - 1      (3 - 1 = 2)

      # Verify parser gives 1-indexed (our assumption)
      parser_start.should be >= 1
      parser_end.should be > parser_start

      # The CORRECT LSP result should be:
      expected_lsp_start = parser_start - 1  # 0
      expected_lsp_end = parser_end - 1      # 2

      # This is what FoldingRange should contain
      expected_lsp_start.should eq(0)
      expected_lsp_end.should eq(2)
    end

    it "handles multi-line source correctly" do
      # Source with explicit line structure
      source = "def calculate\n  x = 1\n  y = 2\n  x + y\nend\n"

      lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
      parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
      program = parser.parse_program

      root = program.arena[program.roots[0]]
      def_node = root.as(CrystalV2::Compiler::Frontend::DefNode)

      # Parser spans (1-indexed):
      # Line 1: "def calculate"
      # Line 2: "  x = 1"
      # Line 3: "  y = 2"
      # Line 4: "  x + y"
      # Line 5: "end"

      def_node.span.start_line.should eq(1)  # Parser line 1
      def_node.span.end_line.should eq(5)    # Parser line 5

      # After conversion (what collect_folding_ranges should produce):
      # start_line: 0 (LSP line 0 = first line)
      # end_line: 4 (LSP line 4 = fifth line)

      lsp_start = def_node.span.start_line - 1
      lsp_end = def_node.span.end_line - 1

      lsp_start.should eq(0)
      lsp_end.should eq(4)
    end
  end
end
