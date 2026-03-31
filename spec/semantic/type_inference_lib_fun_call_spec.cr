require "spec"

require "../../src/compiler/frontend/ast"
require "../../src/compiler/frontend/lexer"
require "../../src/compiler/frontend/parser"
require "../../src/compiler/semantic/analyzer"
require "../../src/compiler/semantic/type_inference_engine"

alias Frontend = CrystalV2::Compiler::Frontend
alias Semantic = CrystalV2::Compiler::Semantic

private def infer_lib_fun_call_types(source : String)
  parser = Frontend::Parser.new(Frontend::Lexer.new(source))
  program = parser.parse_program

  analyzer = Semantic::Analyzer.new(program)
  analyzer.collect_symbols
  name_result = analyzer.resolve_names
  engine = analyzer.infer_types(name_result.identifier_symbols)

  {program, analyzer, engine}
end

describe Semantic::TypeInferenceEngine do
  describe "lib fun calls" do
    it "supports variadic lib fun calls with extra runtime arguments" do
      source = <<-CRYSTAL
        lib LibC
          fun fcntl(fd : Int, cmd : Int, ...) : Int
        end

        LibC.fcntl(3, 4, 5)
      CRYSTAL

      program, analyzer, engine = infer_lib_fun_call_types(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.should be_empty
      engine.context.get_type(program.roots.last).to_s.should eq("Int")
    end

    it "accepts enum values for lib fun integer parameters" do
      source = <<-CRYSTAL
        enum Seek
          Set = 0
          Current = 1
        end

        lib LibC
          alias OffT = Int64
          fun lseek(fd : Int, offset : OffT, whence : Int) : OffT
        end

        whence : Seek = uninitialized Seek
        LibC.lseek(3, 0_i64, whence)
      CRYSTAL

      program, analyzer, engine = infer_lib_fun_call_types(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.should be_empty
      engine.context.get_type(program.roots.last).to_s.should eq("Int64")
    end

    it "accepts plain integer literals and nested enum values for lib fun calls" do
      source = <<-CRYSTAL
        module IO
          enum Seek
            Set = 0
            Current = 1
          end
        end

        lib LibC
          alias OffT = Int64
          fun lseek(fd : Int, offset : OffT, whence : Int) : OffT
        end

        whence : IO::Seek = uninitialized IO::Seek
        LibC.lseek(3, 0, whence)
      CRYSTAL

      program, analyzer, engine = infer_lib_fun_call_types(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.should be_empty
      engine.context.get_type(program.roots.last).to_s.should eq("Int64")
    end

    it "accepts nil for lib fun pointer arguments" do
      source = <<-CRYSTAL
        lib LibC
          SIG_SETMASK = 3
          alias SigsetT = UInt32
          fun pthread_sigmask(Int, SigsetT*, SigsetT*) : Int
        end

        newmask = uninitialized LibC::SigsetT
        LibC.pthread_sigmask(LibC::SIG_SETMASK, pointerof(newmask), nil)
      CRYSTAL

      program, analyzer, engine = infer_lib_fun_call_types(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.should be_empty
      engine.context.get_type(program.roots.last).to_s.should eq("Int")
    end
  end
end
