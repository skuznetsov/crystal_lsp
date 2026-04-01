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

    it "carries assignment truthy narrowings across chained && calls" do
      source = <<-CRYSTAL
        def takes_string(value : String) : Bool
          value.starts_with?("a")
        end

        def probe(value : String?)
          if (current = value) && current.starts_with?("a") && takes_string(current)
            current.bytesize
          else
            0
          end
        end

        probe("abc")
      CRYSTAL

      program, analyzer, engine = infer_logical_rhs_narrowing_types(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.should be_empty
      engine.context.get_type(program.roots.last).to_s.should eq("Int32")
    end

    it "preserves assignment truthy narrowings for named class-method call conjuncts" do
      source = <<-CRYSTAL
        class File
          def self.info?(path : String, follow_symlinks = true) : Int32?
            1
          end
        end

        def probe(value : String?)
          if (pwd = value) && pwd.starts_with?("/") && (pwd_info = File.info?(pwd, follow_symlinks: true)) && pwd_info > 0
            pwd.bytesize
          else
            0
          end
        end

        probe("/tmp")
      CRYSTAL

      program, analyzer, engine = infer_logical_rhs_narrowing_types(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.should be_empty
      engine.context.get_type(program.roots.last).to_s.should eq("Int32")
    end

    it "preserves assignment truthy narrowings when the seed comes from a receiverless call" do
      source = <<-CRYSTAL
        module SystemFile
          def self.info?(path : String, follow_symlinks = true) : Int32?
            1
          end
        end

        def maybe_value : String?
          "/tmp"
        end

        def probe
          if (pwd = maybe_value) && pwd.starts_with?("/") && (pwd_info = SystemFile.info?(pwd, follow_symlinks: true)) && pwd_info > 0
            pwd.bytesize
          else
            0
          end
        end

        probe()
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

    it "preserves truthy narrowing after unless guards in outer reader call chains" do
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
            read_char_with_bytesize(peek)
          end

          def read_char_with_bytesize(peek = nil)
            first = peek_or_read_utf8(peek, 0)
            return nil unless first
            first = first.to_u32
            first
          end

          private def peek_or_read_utf8(peek, index)
            if peek && (byte = peek[index]?)
              skip(1)
              byte
            else
              read_utf8_byte
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

    it "preserves return-unless assignment narrowing inside while bodies" do
      source = <<-CRYSTAL
        class Breaks
          def empty?
            false
          end
        end

        class Group
          def breakables
            Breaks.new
          end
        end

        class GroupQueue
          def initialize
            @done = false
          end

          def deq
            unless @done
              @done = true
              Group.new
            end
          end
        end

        class Box
          def initialize
            @queue = GroupQueue.new
          end

          def probe
            while true
              return 0 unless group = @queue.deq
              return 1 if group.breakables.empty?
              return 2
            end
          end
        end

        Box.new.probe
      CRYSTAL

      program, analyzer, engine = infer_logical_rhs_narrowing_types(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.should be_empty
      {"Int32", "Nil | Int32", "Int32 | Nil"}.should contain(engine.context.get_type(program.roots.last).to_s)
    end
  end
end
