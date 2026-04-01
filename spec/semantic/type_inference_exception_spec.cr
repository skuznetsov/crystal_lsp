require "spec"

require "../../src/compiler/frontend/ast"
require "../../src/compiler/frontend/lexer"
require "../../src/compiler/frontend/parser"
require "../../src/compiler/semantic/symbol_table"
require "../../src/compiler/semantic/symbol"
require "../../src/compiler/semantic/collectors/symbol_collector"
require "../../src/compiler/semantic/resolvers/name_resolver"
require "../../src/compiler/semantic/analyzer"
require "../../src/compiler/semantic/types/type"
require "../../src/compiler/semantic/types/primitive_type"
require "../../src/compiler/semantic/types/class_type"
require "../../src/compiler/semantic/types/union_type"
require "../../src/compiler/semantic/types/type_context"
require "../../src/compiler/semantic/type_inference_engine"

include CrystalV2::Compiler::Frontend
include CrystalV2::Compiler::Semantic

# Helper: Parse source and run full semantic pipeline
private def infer_types(source : String)
  lexer = Lexer.new(source)
  parser = Parser.new(lexer)
  program = parser.parse_program

  # Run semantic analysis (symbol collection + name resolution)
  analyzer = Analyzer.new(program)
  analyzer.collect_symbols
  name_result = analyzer.resolve_names

  # Run type inference with global symbol table for fallback lookup
  engine = TypeInferenceEngine.new(program, name_result.identifier_symbols, analyzer.global_context.symbol_table)
  engine.infer_types

  {program, analyzer, engine}
end

describe TypeInferenceEngine do
  describe "Phase 29: Exception handling" do
    it "infers included-hook class macros and class methods on exception types" do
      source = <<-CRYSTAL
        module SystemError
          macro included
            extend ::SystemError::ClassMethods

            macro from_errno(message, **opts)
              ::{{ @type }}.from_os_error({{ message }}, 1, {{ opts.double_splat }})
            end
          end

          module ClassMethods
            def from_os_error(message : String?, os_error, **opts)
              self.new(message)
            end
          end
        end

        class Exception
          def initialize(@message : String?)
          end
        end

        class MyError < Exception
          include SystemError
        end

        MyError.from_os_error("boom", 1)
        MyError.from_errno("boom")
      CRYSTAL

      lexer = Lexer.new(source)
      parser = Parser.new(lexer)
      program = parser.parse_program

      analyzer = Analyzer.new(program)
      analyzer.collect_symbols
      name_result = analyzer.resolve_names
      engine = analyzer.infer_types(name_result.identifier_symbols)

      name_result.diagnostics.should be_empty
      engine.diagnostics.should be_empty
      analyzer.generated_overlay.top_level_roots.size.should eq(1)

      engine.context.get_type(program.roots[-2]).to_s.should eq("MyError")
      engine.context.get_type(analyzer.generated_overlay.top_level_roots.first).to_s.should eq("MyError")
    end

    it "forwards empty double splats through included class-method helpers" do
      source = <<-CRYSTAL
        module SystemError
          macro included
            extend ::SystemError::ClassMethods
          end

          module ClassMethods
            def from_os_error(message : String?, os_error, **opts)
              message = self.build_message(message, **opts)
              self.new_from_os_error(message, os_error, **opts)
            end

            protected def build_message(message : String?, **opts) : String?
              message
            end

            protected def new_from_os_error(message : String?, os_error, **opts)
              self.new(message)
            end
          end
        end

        class Exception
          def initialize(@message : String? = nil)
          end
        end

        class RuntimeError < Exception
          include SystemError
        end

        RuntimeError.from_os_error("boom", 1)
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.should be_empty
      engine.context.get_type(program.roots.last).to_s.should eq("RuntimeError")
    end

    it "forwards non-empty double splats into keyword-only helper methods" do
      source = <<-CRYSTAL
        module SystemError
          macro included
            extend ::SystemError::ClassMethods
          end

          module ClassMethods
            def from_os_error(message : String?, os_error, **opts)
              message = self.build_message(message, **opts)
              self.new_from_os_error(message, os_error, **opts)
            end

            protected def build_message(message : String?, **opts) : String?
              message
            end

            protected def new_from_os_error(message : String?, os_error, **opts)
              self.new(message)
            end
          end
        end

        class Exception
          def initialize(@message : String? = nil)
          end
        end

        class IO
          class Error < Exception
            include SystemError

            def initialize(message = nil, *, target : String? = nil)
              super(message)
            end
          end
        end

        class File
        end

        class File::Error < IO::Error
          private def self.new_from_os_error(message, os_error, **opts)
            self.new(message, **opts)
          end

          def initialize(message, *, file : String)
            super message, target: file
          end

          protected def self.build_message(message, *, file : String) : String
            message || file
          end
        end

        ::File::Error.from_os_error("boom", 1, file: "x")
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.should be_empty
      engine.context.get_type(program.roots.last).should_not be_nil
    end

    it "handles begin with rescue (union type)" do
      source = <<-CRYSTAL
        x = begin
          10
        rescue
          20
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      root_id = program.roots.first
      assign_type = engine.context.get_type(root_id)

      # Result is union of begin body (Int32) and rescue body (Int32) = Int32
      assign_type.to_s.should eq("Int32")
    end

    it "handles begin with rescue different types" do
      source = <<-CRYSTAL
        x = begin
          10
        rescue
          "error"
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      root_id = program.roots.first
      assign_type = engine.context.get_type(root_id)

      # Result is union of Int32 | String
      assign_type.to_s.should eq("Int32 | String")
    end

    it "handles begin with multiple rescue clauses" do
      source = <<-CRYSTAL
        x = begin
          10
        rescue RuntimeError
          20
        rescue ArgumentError
          "error"
        rescue
          true
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      root_id = program.roots.first
      assign_type = engine.context.get_type(root_id)

      # Result is union of all: Int32 | String | Bool
      assign_type.to_s.should eq("Bool | Int32 | String")
    end

    it "handles begin with ensure (ensure doesn't affect type)" do
      source = <<-CRYSTAL
        x = begin
          10
        ensure
          cleanup()
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      root_id = program.roots.first
      assign_type = engine.context.get_type(root_id)

      # Ensure doesn't affect type, result is Int32
      assign_type.to_s.should eq("Int32")
    end

    it "handles begin with rescue and ensure" do
      source = <<-CRYSTAL
        x = begin
          10
        rescue
          "error"
        ensure
          cleanup()
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      root_id = program.roots.first
      assign_type = engine.context.get_type(root_id)

      # Ensure doesn't affect type, result is union of begin and rescue
      assign_type.to_s.should eq("Int32 | String")
    end

    it "handles raise statement" do
      source = <<-CRYSTAL
        raise "error"
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      root_id = program.roots.first
      raise_type = engine.context.get_type(root_id)

      # Raise returns Nil (in real Crystal it would be NoReturn)
      raise_type.to_s.should eq("Nil")
    end

    it "handles bare raise (re-raise)" do
      source = <<-CRYSTAL
        begin
          x = 10
        rescue
          raise
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      root_id = program.roots.first
      begin_type = engine.context.get_type(root_id)

      # Begin + rescue with raise = Nil | Int32
      begin_type.to_s.should eq("Nil | Int32")
    end

    it "handles empty rescue clause" do
      source = <<-CRYSTAL
        x = begin
          10
        rescue
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      root_id = program.roots.first
      assign_type = engine.context.get_type(root_id)

      # Empty rescue returns Nil, so Nil | Int32
      assign_type.to_s.should eq("Nil | Int32")
    end

    it "handles nested begin/rescue blocks" do
      source = <<-CRYSTAL
        x = begin
          begin
            10
          rescue
            20
          end
        rescue
          "outer error"
        end
      CRYSTAL

      program, analyzer, engine = infer_types(source)

      root_id = program.roots.first
      assign_type = engine.context.get_type(root_id)

      # Inner: Int32 | Int32 = Int32
      # Outer: Int32 | String
      assign_type.to_s.should eq("Int32 | String")
    end
  end
end
