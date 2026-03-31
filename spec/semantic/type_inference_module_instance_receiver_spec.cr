require "spec"

require "../../src/compiler/frontend/ast"
require "../../src/compiler/frontend/lexer"
require "../../src/compiler/frontend/parser"
require "../../src/compiler/semantic/analyzer"
require "../../src/compiler/semantic/type_inference_engine"

include CrystalV2::Compiler::Frontend
include CrystalV2::Compiler::Semantic

private def infer_types(source : String)
  lexer = Lexer.new(source)
  parser = Parser.new(lexer)
  program = parser.parse_program

  analyzer = Analyzer.new(program)
  analyzer.collect_symbols
  name_result = analyzer.resolve_names

  engine = TypeInferenceEngine.new(program, name_result.identifier_symbols, analyzer.global_context.symbol_table)
  engine.infer_types

  {program, analyzer, engine}
end

describe TypeInferenceEngine do
  describe "module-owned instance methods" do
    it "defers eager body inference until an including receiver is available" do
      source = <<-CRYSTAL
        module Termios
        end

        lib LibC
          TCSANOW = 0
          ECHO    = 1_u64
          ECHOE   = 2_u64
          ECHOK   = 4_u64
          ECHONL  = 8_u64
          BRKINT  = 16_u64
          ISTRIP  = 32_u64
          ICRNL   = 64_u64
          IXON    = 128_u64
          OPOST   = 256_u64
          ICANON  = 512_u64
          ISIG    = 1024_u64
          IEXTEN  = 2048_u64

          alias TcflagT = UInt64

          struct Termios
            c_iflag : TcflagT
            c_oflag : TcflagT
            c_cflag : TcflagT
            c_lflag : TcflagT
          end

          fun tcgetattr(fd : Int32, termios_p : Termios*) : Int32
          fun tcsetattr(fd : Int32, optional_actions : Int32, termios_p : Termios*) : Int32
        end

        module Crystal::System::FileDescriptor
          private def system_echo(enable : Bool, mode = nil)
            new_mode = mode || system_tcgetattr
            flags = LibC::ECHO | LibC::ECHOE | LibC::ECHOK | LibC::ECHONL
            new_mode.c_lflag = enable ? (new_mode.c_lflag | flags) : (new_mode.c_lflag & ~flags)
            system_tcsetattr(LibC::TCSANOW, pointerof(new_mode))
          end

          private def system_tcgetattr
            termios = uninitialized LibC::Termios
            LibC.tcgetattr(fd, pointerof(termios))
            termios
          end

          private def system_tcsetattr(optional_actions, termios_p)
            LibC.tcsetattr(fd, optional_actions, termios_p)
          end

          private def system_raw(enable : Bool, mode = nil)
            new_mode = mode || system_tcgetattr
            new_mode.c_iflag |= LibC::BRKINT | LibC::ISTRIP | LibC::ICRNL | LibC::IXON
            new_mode.c_oflag |= LibC::OPOST
            new_mode.c_lflag |= LibC::ECHO | LibC::ECHOE | LibC::ECHOK | LibC::ECHONL | LibC::ICANON | LibC::ISIG | LibC::IEXTEN
            system_tcsetattr(LibC::TCSANOW, pointerof(new_mode))
          end
        end

        class Terminal
          include Crystal::System::FileDescriptor

          def initialize(@fd : Int32 = 0)
          end

          def fd : Int32
            @fd
          end

          def echo_off
            system_echo(false)
          end

          def raw_off
            system_raw(false)
          end
        end

        Terminal.new.echo_off
        Terminal.new.raw_off
      CRYSTAL

      _program, analyzer, engine = infer_types(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.should be_empty
    end

    it "treats libc integer aliases as numeric in struct field bitflag arithmetic" do
      source = <<-CRYSTAL
        lib LibC
          ECHO    = 0x00000008
          ECHOE   = 0x00000002
          ECHOK   = 0x00000004
          ECHONL  = 0x00000010
          BRKINT  = 0x00000002
          ISTRIP  = 0x00000020
          ICRNL   = 0x00000100
          IXON    = 0x00000200
          OPOST   = 0x00000001
          ICANON  = 0x00000100
          ISIG    = 0x00000080
          IEXTEN  = 0x00000400
          TCSANOW = 0

          alias ULong = UInt64
          alias TcflagT = ULong

          struct Termios
            c_iflag : TcflagT
            c_oflag : TcflagT
            c_cflag : TcflagT
            c_lflag : TcflagT
          end

          fun tcgetattr(fd : Int32, termios_p : Termios*) : Int32
          fun tcsetattr(fd : Int32, optional_actions : Int32, termios_p : Termios*) : Int32
        end

        module Crystal::System::FileDescriptor
          private def system_echo(enable : Bool, mode = nil)
            new_mode = mode || system_tcgetattr
            flags = LibC::ECHO | LibC::ECHOE | LibC::ECHOK | LibC::ECHONL
            new_mode.c_lflag = enable ? (new_mode.c_lflag | flags) : (new_mode.c_lflag & ~flags)
            system_tcsetattr(LibC::TCSANOW, pointerof(new_mode))
          end

          private def system_tcgetattr
            termios = uninitialized LibC::Termios
            LibC.tcgetattr(fd, pointerof(termios))
            termios
          end

          private def system_tcsetattr(optional_actions, termios_p)
            LibC.tcsetattr(fd, optional_actions, termios_p)
          end

          private def system_raw(enable : Bool, mode = nil)
            new_mode = mode || system_tcgetattr
            new_mode.c_iflag |= LibC::BRKINT | LibC::ISTRIP | LibC::ICRNL | LibC::IXON
            new_mode.c_oflag |= LibC::OPOST
            new_mode.c_lflag |= LibC::ECHO | LibC::ECHOE | LibC::ECHOK | LibC::ECHONL | LibC::ICANON | LibC::ISIG | LibC::IEXTEN
            system_tcsetattr(LibC::TCSANOW, pointerof(new_mode))
          end
        end

        class Terminal
          include Crystal::System::FileDescriptor

          def initialize(@fd : Int32 = 0)
          end

          def fd : Int32
            @fd
          end

          def echo_off
            system_echo(false)
          end

          def raw_off
            system_raw(false)
          end
        end

        Terminal.new.echo_off
        Terminal.new.raw_off
      CRYSTAL

      _program, analyzer, engine = infer_types(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.should be_empty
    end

    it "promotes libc integer aliases against numeric unions" do
      source = <<-CRYSTAL
        lib LibC
          alias ULong = UInt64
        end

        def probe(flag : LibC::ULong, other : UInt32 | UInt64)
          flag + other
        end

        probe(1_u64, 1_u32)
      CRYSTAL

      _program, analyzer, engine = infer_types(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.should be_empty
    end
  end
end
