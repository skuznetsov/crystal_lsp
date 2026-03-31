require "spec"

require "../../src/compiler/frontend/ast"
require "../../src/compiler/frontend/lexer"
require "../../src/compiler/frontend/parser"
require "../../src/compiler/semantic/analyzer"
require "../../src/compiler/semantic/type_inference_engine"

alias Frontend = CrystalV2::Compiler::Frontend
alias Semantic = CrystalV2::Compiler::Semantic

private def infer_logical_rhs_narrowing_types(source : String)
  parser = Frontend::Parser.new(Frontend::Lexer.new(source))
  program = parser.parse_program

  analyzer = Semantic::Analyzer.new(program)
  analyzer.collect_symbols
  name_result = analyzer.resolve_names
  engine = analyzer.infer_types(name_result.identifier_symbols)

  {program, analyzer, engine}
end

describe Semantic::TypeInferenceEngine do
  describe "&& rhs narrowing" do
    it "keeps nilable receiver truthy narrowing while inferring rhs assignments" do
      source = <<-CRYSTAL
        def probe(peek : Array(UInt8)?, index : Int32)
          if peek && (byte = peek[index]?)
            byte.to_u32
          else
            0_u32
          end
        end

        probe([1_u8, 2_u8], 0)
      CRYSTAL

      program, analyzer, engine = infer_logical_rhs_narrowing_types(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.should be_empty
      engine.context.get_type(program.roots.last).to_s.should eq("UInt32")
    end

    it "applies truthy nil narrowing while inferring rhs calls" do
      source = <<-CRYSTAL
        def probe(value : String?)
          if value && value.bytesize > 0
            value.bytesize
          else
            0
          end
        end

        probe("hello")
      CRYSTAL

      program, analyzer, engine = infer_logical_rhs_narrowing_types(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.should be_empty
      engine.context.get_type(program.roots.last).to_s.should eq("Int32")
    end

    it "applies responds_to? narrowing while inferring rhs calls" do
      source = <<-CRYSTAL
        abstract class Device
        end

        class PositionalDevice < Device
          def unbuffered_pos : Int64
            7_i64
          end
        end

        def probe(device : Device)
          if device.responds_to?(:unbuffered_pos) && device.unbuffered_pos > 0
            1_i32
          else
            0_i32
          end
        end

        probe(PositionalDevice.new)
      CRYSTAL

      program, analyzer, engine = infer_logical_rhs_narrowing_types(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.should be_empty
      engine.context.get_type(program.roots.last).to_s.should eq("Int32")
    end

    it "supports []? on slice-like alias receivers" do
      source = <<-CRYSTAL
        alias Bytes = Slice(UInt8)

        bytes = Bytes.empty
        bytes[0]?
      CRYSTAL

      program, analyzer, engine = infer_logical_rhs_narrowing_types(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.should be_empty
      {"UInt8 | Nil", "Nil | UInt8"}.should contain(engine.context.get_type(program.roots.last).to_s)
    end

    it "preserves rhs narrowing through untyped instance-method call chains" do
      source = <<-CRYSTAL
        alias Bytes = Slice(UInt8)

        class Reader
          def decoder
            nil
          end

          def peek : Bytes?
            Bytes.empty
          end

          def skip(n)
            n
          end

          def read_utf8_byte : UInt8?
            1_u8
          end

          def read_char
            peek = self.peek unless decoder
            first = peek_or_read_utf8(peek, 0)
            return nil unless first
            first.to_u32
          end

          private def peek_or_read_utf8(peek, index)
            if peek && (byte = peek[index]?)
              skip(1)
              byte.to_u32
            else
              0_u32
            end
          end
        end

        Reader.new.read_char
      CRYSTAL

      program, analyzer, engine = infer_logical_rhs_narrowing_types(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.should be_empty
      {"UInt32 | Nil", "Nil | UInt32"}.should contain(engine.context.get_type(program.roots.last).to_s)
    end
  end
end
