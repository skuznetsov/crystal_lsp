require "spec"

require "../../src/compiler/frontend/ast"
require "../../src/compiler/frontend/lexer"
require "../../src/compiler/frontend/parser"
require "../../src/compiler/semantic/analyzer"
require "../../src/compiler/semantic/type_inference_engine"

alias Frontend = CrystalV2::Compiler::Frontend
alias Semantic = CrystalV2::Compiler::Semantic

private def infer_absolute_path_types(source : String)
  parser = Frontend::Parser.new(Frontend::Lexer.new(source))
  program = parser.parse_program

  analyzer = Semantic::Analyzer.new(program)
  analyzer.collect_symbols
  name_result = analyzer.resolve_names
  engine = analyzer.infer_types(name_result.identifier_symbols)

  {program, analyzer, engine}
end

describe Semantic::TypeInferenceEngine do
  describe "absolute paths" do
    it "keeps ::Signal rooted at the top level inside shadowing modules" do
      source = <<-CRYSTAL
        module Crystal::System::Signal
          def self.shadowed
            nil
          end
        end

        enum Signal : Int32
          INT = 2
        end

        module Crystal::System::Threading
          def self.resume_signal
            ::Signal.new(2)
          end
        end

        Crystal::System::Threading.resume_signal
      CRYSTAL

      program, analyzer, engine = infer_absolute_path_types(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.should be_empty
      engine.context.get_type(program.roots.last).to_s.should eq("Signal")
    end

    it "resolves macro-generated enum members through absolute paths inside shadowing modules" do
      source = <<-CRYSTAL
        module Crystal::System::File
          def self.shadowed
            nil
          end
        end

        enum Errno
          {% for value in %w(ENOENT ENOTDIR) %}
            {{value.id}} = 1
          {% end %}
        end

        module Crystal::System::Threading
          def self.probe
            ::Errno::ENOENT
          end
        end

        Crystal::System::Threading.probe
      CRYSTAL

      program, analyzer, engine = infer_absolute_path_types(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.should be_empty
      engine.context.get_type(program.roots.last).to_s.should eq("Errno")
    end

    it "supports in? on enum receivers with macro-generated absolute-path members" do
      source = <<-CRYSTAL
        module Crystal::System::File
          def self.shadowed
            nil
          end
        end

        enum Errno
          {% for value in %w(ENOENT ENOTDIR) %}
            {{value.id}} = 1
          {% end %}
        end

        module Crystal::System::Threading
          def self.probe
            ::Errno::ENOENT.in?(::Errno::ENOENT, ::Errno::ENOTDIR)
          end
        end

        Crystal::System::Threading.probe
      CRYSTAL

      program, analyzer, engine = infer_absolute_path_types(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.should be_empty
      engine.context.get_type(program.roots.last).to_s.should eq("Bool")
    end

    it "keeps absolute annotations rooted at the top level inside shadowing modules" do
      source = <<-CRYSTAL
        struct Time
          NANOSECONDS_PER_MICROSECOND = 1_000_i64

          def to_unix : Int64
            1_i64
          end

          def nanosecond : Int32
            2
          end
        end

        module Crystal::System::Time
          def self.to_timeval(time : ::Time)
            time.to_unix + time.nanosecond // ::Time::NANOSECONDS_PER_MICROSECOND
          end
        end

        Crystal::System::Time.to_timeval(uninitialized Time)
      CRYSTAL

      program, analyzer, engine = infer_absolute_path_types(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.should be_empty
      engine.context.get_type(program.roots.last).to_s.should eq("Int64")
    end
  end
end
