require "spec"

require "../../src/compiler/frontend/ast"
require "../../src/compiler/frontend/lexer"
require "../../src/compiler/frontend/parser"
require "../../src/compiler/semantic/analyzer"
require "../../src/compiler/semantic/type_inference_engine"

alias Frontend = CrystalV2::Compiler::Frontend
alias Semantic = CrystalV2::Compiler::Semantic

private def infer_enum_constant_types(source : String)
  parser = Frontend::Parser.new(Frontend::Lexer.new(source))
  program = parser.parse_program

  analyzer = Semantic::Analyzer.new(program)
  analyzer.collect_symbols
  name_result = analyzer.resolve_names
  engine = analyzer.infer_types(name_result.identifier_symbols)

  {program, analyzer, engine}
end

describe Semantic::TypeInferenceEngine do
  describe "nested enum member paths" do
    it "resolves nested enum members inside instance methods" do
      source = <<-CRYSTAL
        class OptionParser
          enum FlagValue
            Required
            Optional
            None
          end

          def test
            FlagValue::None
          end
        end

        OptionParser.new.test
      CRYSTAL

      program, analyzer, engine = infer_enum_constant_types(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.should be_empty

      root_type = engine.context.get_type(program.roots.last)
      root_type.should be_a(Semantic::EnumType)
      root_type.as(Semantic::EnumType).symbol.name.should eq("FlagValue")
    end

    it "preserves nested enum members through tuple-return multiple assignment" do
      source = <<-CRYSTAL
        class OptionParser
          enum FlagValue
            Required
            Optional
            None
          end

          def test(short_flag : String, long_flag : String)
            short_flag, short_value_type = parse_flag_definition(short_flag)
            long_flag, long_value_type = parse_flag_definition(long_flag)

            if short_value_type.required? || long_value_type.required?
              FlagValue::Required
            elsif short_value_type.optional? || long_value_type.optional?
              FlagValue::Optional
            else
              FlagValue::None
            end
          end

          private def parse_flag_definition(flag : String)
            case flag
            when /\A--(\S+)\s+\[\S+\]\z/
              {"--\#{$1}", FlagValue::Optional}
            when /\A--(\S+)(\s+|\=)(\S+)?\z/
              {"--\#{$1}", FlagValue::Required}
            when /\A--\S+\z/
              {flag, FlagValue::None}
            when /\A-(.)\s*\[\S+\]\z/
              {flag[0..1], FlagValue::Optional}
            when /\A-(.)\s+\S+\z/, /\A-(.)\s+\z/, /\A-(.)\S+\z/
              {flag[0..1], FlagValue::Required}
            else
              {flag, FlagValue::None}
            end
          end
        end

        OptionParser.new.test("-x ARG", "--long [ARG]")
      CRYSTAL

      program, analyzer, engine = infer_enum_constant_types(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.should be_empty

      root_type = engine.context.get_type(program.roots.last)
      root_type.should be_a(Semantic::EnumType)
      root_type.as(Semantic::EnumType).symbol.name.should eq("FlagValue")
    end
  end
end
