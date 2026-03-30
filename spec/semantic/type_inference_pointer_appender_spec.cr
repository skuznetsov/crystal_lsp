require "spec"

require "../../src/compiler/frontend/ast"
require "../../src/compiler/frontend/lexer"
require "../../src/compiler/frontend/parser"
require "../../src/compiler/semantic/analyzer"
require "../../src/compiler/semantic/type_inference_engine"

alias Frontend = CrystalV2::Compiler::Frontend
alias Semantic = CrystalV2::Compiler::Semantic

private def infer_types_for_pointer_appender(source : String)
  parser = Frontend::Parser.new(Frontend::Lexer.new(source))
  program = parser.parse_program

  analyzer = Semantic::Analyzer.new(program)
  analyzer.collect_symbols
  name_result = analyzer.resolve_names
  engine = analyzer.infer_types(name_result.identifier_symbols)

  {program, analyzer, engine}
end

describe Semantic::TypeInferenceEngine do
  describe "Pointer#appender builtins" do
    it "resolves appender size, push, and slice materialization" do
      source = <<-CRYSTAL
        module Probe
          def self.convert
            utf8 = uninitialized UInt8[16]
            appender = utf8.to_unsafe.appender

            if appender.size > 0
              appender << 65_u8
            else
              appender << 66_u8
            end

            appender.to_slice
          end
        end

        Probe.convert
      CRYSTAL

      program, analyzer, engine = infer_types_for_pointer_appender(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.should be_empty
      engine.context.get_type(program.roots.last).to_s.should eq("Array(UInt8)")
    end

    it "resolves the appender pointer getter" do
      source = <<-CRYSTAL
        module Probe
          def self.pointer
            utf8 = uninitialized UInt8[16]
            appender = utf8.to_unsafe.appender
            appender.pointer
          end
        end

        Probe.pointer
      CRYSTAL

      program, analyzer, engine = infer_types_for_pointer_appender(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.should be_empty
      engine.context.get_type(program.roots.last).to_s.should eq("Pointer(UInt8)")
    end
  end
end
