require "spec"

require "../../src/compiler/frontend/ast"
require "../../src/compiler/frontend/lexer"
require "../../src/compiler/frontend/parser"
require "../../src/compiler/semantic/analyzer"
require "../../src/compiler/semantic/type_inference_engine"

alias Frontend = CrystalV2::Compiler::Frontend
alias Semantic = CrystalV2::Compiler::Semantic

private def infer_io_protocol_types(source : String)
  parser = Frontend::Parser.new(Frontend::Lexer.new(source))
  program = parser.parse_program

  analyzer = Semantic::Analyzer.new(program)
  analyzer.collect_symbols
  name_result = analyzer.resolve_names
  engine = analyzer.infer_types(name_result.identifier_symbols)

  {program, analyzer, engine}
end

describe Semantic::TypeInferenceEngine do
  describe "IO protocol helpers" do
    it "supports Object#to_s(io) and numeric to_io in untyped IO helpers" do
      source = <<-CRYSTAL
        class IO
          def <<(obj : _)
            obj.to_s(self)
            self
          end

          def write_bytes(object : _, format : IO::ByteFormat)
            object.to_io(self, format)
            nil
          end
        end

        enum IO::ByteFormat
          SystemEndian
        end

        def direct_numeric_to_io(io : IO, format : IO::ByteFormat)
          1_f64.to_io(io, format)
        end

        def direct_numeric_to_s(io : IO)
          1_u64.to_s(io, 16)
        end

        io = IO.new
        appended = io << 1_u8
        {appended, io}
      CRYSTAL

      program, analyzer, engine = infer_io_protocol_types(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.should be_empty
      engine.context.get_type(program.roots.last).to_s.should eq("Tuple(IO, IO)")
    end

    it "supports universal inspect overloads for pointer-like receivers" do
      source = <<-CRYSTAL
        class IO
        end

        def probe(io : IO)
          ptr = Pointer(Void).null
          ptr.inspect(io)
          ptr.inspect
        end

        probe(IO.new)
      CRYSTAL

      program, analyzer, engine = infer_io_protocol_types(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.should be_empty
      engine.context.get_type(program.roots.last).to_s.should eq("String")
    end
  end
end
