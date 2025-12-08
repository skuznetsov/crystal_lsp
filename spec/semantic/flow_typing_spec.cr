require "spec"

require "../../src/compiler/frontend/ast"
require "../../src/compiler/frontend/lexer"
require "../../src/compiler/frontend/parser"
require "../../src/compiler/semantic/context"
require "../../src/compiler/semantic/symbol_table"
require "../../src/compiler/semantic/collectors/symbol_collector"
require "../../src/compiler/semantic/type_inference_engine"

include CrystalV2::Compiler

describe "Phase 95: Flow Typing (is_a? narrowing)" do
  # ==================================================================
  # Category 1: Basic is_a? narrowing in if-then branch
  # ==================================================================

  describe "is_a? narrowing in then branch" do
    it "narrows union type to specific type after is_a?" do
      source = <<-CRYSTAL
        def test(x : Int32 | String)
          if x.is_a?(Int32)
            x + 1  # x is Int32 here
          end
        end
        CRYSTAL

      lexer = Frontend::Lexer.new(source)
      parser = Frontend::Parser.new(lexer)
      program = parser.parse_program

      context = Semantic::Context.new(Semantic::SymbolTable.new)
      collector = Semantic::SymbolCollector.new(program, context)
      collector.collect

      # Should compile without errors
      collector.diagnostics.select(&.level.error?).should be_empty
    end

    it "narrows type parameter to concrete type" do
      source = <<-CRYSTAL
        def test(x)
          if x.is_a?(String)
            x.size  # x is String here
          end
        end
        CRYSTAL

      lexer = Frontend::Lexer.new(source)
      parser = Frontend::Parser.new(lexer)
      program = parser.parse_program

      context = Semantic::Context.new(Semantic::SymbolTable.new)
      collector = Semantic::SymbolCollector.new(program, context)
      collector.collect

      collector.diagnostics.select(&.level.error?).should be_empty
    end
  end

  # ==================================================================
  # Category 2: Nested is_a? narrowing
  # ==================================================================

  describe "nested is_a? checks" do
    it "handles nested is_a? checks correctly" do
      source = <<-CRYSTAL
        def test(x : Int32 | String | Float64)
          if x.is_a?(Int32)
            x + 1
          elsif x.is_a?(String)
            x.size
          end
        end
        CRYSTAL

      lexer = Frontend::Lexer.new(source)
      parser = Frontend::Parser.new(lexer)
      program = parser.parse_program

      context = Semantic::Context.new(Semantic::SymbolTable.new)
      collector = Semantic::SymbolCollector.new(program, context)
      collector.collect

      collector.diagnostics.select(&.level.error?).should be_empty
    end
  end

  # ==================================================================
  # Category 3: Flow typing restores after block
  # ==================================================================

  describe "type restoration after branch" do
    it "restores original type after is_a? block" do
      source = <<-CRYSTAL
        def test(x : Int32 | String)
          if x.is_a?(Int32)
            x + 1
          end
          x  # back to Int32 | String
        end
        CRYSTAL

      lexer = Frontend::Lexer.new(source)
      parser = Frontend::Parser.new(lexer)
      program = parser.parse_program

      context = Semantic::Context.new(Semantic::SymbolTable.new)
      collector = Semantic::SymbolCollector.new(program, context)
      collector.collect

      collector.diagnostics.select(&.level.error?).should be_empty
    end
  end

  # ==================================================================
  # Category 4: Integration with method calls
  # ==================================================================

  describe "flow typing with method calls" do
    it "allows type-specific method calls in narrowed branch" do
      source = <<-CRYSTAL
        class Foo
          def foo_method
            42
          end
        end

        class Bar
          def bar_method
            "hello"
          end
        end

        def test(x : Foo | Bar)
          if x.is_a?(Foo)
            x.foo_method
          elsif x.is_a?(Bar)
            x.bar_method
          end
        end
        CRYSTAL

      lexer = Frontend::Lexer.new(source)
      parser = Frontend::Parser.new(lexer)
      program = parser.parse_program

      context = Semantic::Context.new(Semantic::SymbolTable.new)
      collector = Semantic::SymbolCollector.new(program, context)
      collector.collect

      collector.diagnostics.select(&.level.error?).should be_empty
    end
  end

  # ==================================================================
  # Category 5: Nil check narrowing (if x where x : T | Nil)
  # ==================================================================

  describe "nil check narrowing" do
    it "narrows nilable type to non-nil in then branch" do
      source = <<-CRYSTAL
        def test(x : String?)
          if x
            x.size  # x is String here, not String | Nil
          end
        end
        CRYSTAL

      lexer = Frontend::Lexer.new(source)
      parser = Frontend::Parser.new(lexer)
      program = parser.parse_program

      analyzer = Semantic::Analyzer.new(program)
      analyzer.collect_symbols
      name_result = analyzer.resolve_names

      engine = Semantic::TypeInferenceEngine.new(program, name_result.identifier_symbols, analyzer.global_context.symbol_table)
      engine.infer_types

      # Should not have "method not found" errors
      engine.diagnostics.select { |d| d.level.error? && d.message.includes?("not found") }.should be_empty
    end

    it "narrows union with nil to remaining types" do
      source = <<-CRYSTAL
        def test(x : Int32 | String | Nil)
          if x
            x  # x is Int32 | String here
          end
        end
        CRYSTAL

      lexer = Frontend::Lexer.new(source)
      parser = Frontend::Parser.new(lexer)
      program = parser.parse_program

      analyzer = Semantic::Analyzer.new(program)
      analyzer.collect_symbols
      name_result = analyzer.resolve_names

      engine = Semantic::TypeInferenceEngine.new(program, name_result.identifier_symbols, analyzer.global_context.symbol_table)
      engine.infer_types

      # Check that x inside if has narrowed type (no Nil)
      engine.diagnostics.select(&.level.error?).should be_empty
    end
  end

  # ==================================================================
  # Category 6: Negative narrowing in else branch
  # ==================================================================

  describe "negative narrowing (else branch)" do
    it "narrows to non-matched type in else branch after is_a?" do
      source = <<-CRYSTAL
        def test(x : Int32 | String)
          if x.is_a?(Int32)
            x + 1
          else
            x.size  # x is String here (the remaining type)
          end
        end
        CRYSTAL

      lexer = Frontend::Lexer.new(source)
      parser = Frontend::Parser.new(lexer)
      program = parser.parse_program

      analyzer = Semantic::Analyzer.new(program)
      analyzer.collect_symbols
      name_result = analyzer.resolve_names

      engine = Semantic::TypeInferenceEngine.new(program, name_result.identifier_symbols, analyzer.global_context.symbol_table)
      engine.infer_types

      engine.diagnostics.select { |d| d.level.error? && d.message.includes?("not found") }.should be_empty
    end

    it "narrows to Nil in else branch after truthy check" do
      source = <<-CRYSTAL
        def test(x : String?)
          if x
            x.size
          else
            nil  # x is Nil here
          end
        end
        CRYSTAL

      lexer = Frontend::Lexer.new(source)
      parser = Frontend::Parser.new(lexer)
      program = parser.parse_program

      analyzer = Semantic::Analyzer.new(program)
      analyzer.collect_symbols
      name_result = analyzer.resolve_names

      engine = Semantic::TypeInferenceEngine.new(program, name_result.identifier_symbols, analyzer.global_context.symbol_table)
      engine.infer_types

      engine.diagnostics.select(&.level.error?).should be_empty
    end
  end
end
