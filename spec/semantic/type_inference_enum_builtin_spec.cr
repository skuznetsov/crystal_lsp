require "spec"

require "../../src/compiler/frontend/ast"
require "../../src/compiler/frontend/lexer"
require "../../src/compiler/frontend/parser"
require "../../src/compiler/semantic/analyzer"
require "../../src/compiler/semantic/type_inference_engine"

alias Frontend = CrystalV2::Compiler::Frontend
alias Semantic = CrystalV2::Compiler::Semantic

private def infer_types_for_enum_builtins(source : String)
  parser = Frontend::Parser.new(Frontend::Lexer.new(source))
  program = parser.parse_program

  analyzer = Semantic::Analyzer.new(program)
  analyzer.collect_symbols
  name_result = analyzer.resolve_names
  engine = analyzer.infer_types(name_result.identifier_symbols)

  {program, analyzer, engine}
end

describe Semantic::TypeInferenceEngine do
  describe "enum builtins" do
    it "resolves enum constructors and value access using the enum base type" do
      source = <<-CRYSTAL
        enum Errno : UInt8
          OK = 0
        end

        Errno.new(0_u8).value
      CRYSTAL

      program, analyzer, engine = infer_types_for_enum_builtins(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.should be_empty
      engine.context.get_type(program.roots.last).to_s.should eq("UInt8")
    end

    it "resolves none? for nested flags enums through qualified paths" do
      source = <<-CRYSTAL
        class Regex
          @[Flags]
          enum MatchOptions
            ANCHORED
            NO_JIT
          end
        end

        Regex::MatchOptions.new(0).none?
      CRYSTAL

      program, analyzer, engine = infer_types_for_enum_builtins(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.should be_empty
      engine.context.get_type(program.roots.last).to_s.should eq("Bool")
    end

    it "does not invent none? for regular enums" do
      source = <<-CRYSTAL
        enum Color
          Red
          Blue
        end

        Color.new(0).none?
      CRYSTAL

      _, analyzer, engine = infer_types_for_enum_builtins(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.select(&.level.error?).map(&.message).should contain("Method 'none?' not found on Color")
    end
  end
end
