require "spec"

require "../../src/compiler/frontend/ast"
require "../../src/compiler/frontend/lexer"
require "../../src/compiler/frontend/parser"
require "../../src/compiler/semantic/analyzer"
require "../../src/compiler/semantic/type_inference_engine"

alias Frontend = CrystalV2::Compiler::Frontend
alias Semantic = CrystalV2::Compiler::Semantic

class Semantic::TypeInferenceEngine
  def __spec_type_receiver_expression(expr_id : Frontend::ExprId) : Bool
    type_receiver_expression?(expr_id)
  end

  def __spec_global_table=(value : Semantic::SymbolTable?) : Nil
    @global_table = value
  end
end

private def infer_enum_class_method_types(source : String)
  parser = Frontend::Parser.new(Frontend::Lexer.new(source))
  program = parser.parse_program

  analyzer = Semantic::Analyzer.new(program)
  analyzer.collect_symbols
  name_result = analyzer.resolve_names
  engine = analyzer.infer_types(name_result.identifier_symbols)

  {program, analyzer, engine}
end

describe Semantic::TypeInferenceEngine do
  describe "enum class methods" do
    it "resolves enum self.new overloads for integer literals" do
      source = <<-CRYSTAL
        enum Permissions : Int16
          Read = 0

          def self.new(int : Int)
            new(int.to_i16)
          end
        end

        Permissions.new(0o644)
      CRYSTAL

      program, analyzer, engine = infer_enum_class_method_types(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.select(&.level.error?).should be_empty
      engine.context.get_type(program.roots.last).to_s.should eq("Permissions")
    end

    it "resolves absolute enum paths inside a shadowing module" do
      source = <<-CRYSTAL
        class File
          enum Permissions : Int16
            Read = 0

            def self.new(int : Int)
              new(int.to_i16)
            end
          end
        end

        module Crystal::System::File
          def self.probe
            ::File::Permissions.new(0o644)
          end
        end

        Crystal::System::File.probe
      CRYSTAL

      program, analyzer, engine = infer_enum_class_method_types(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.select(&.level.error?).should be_empty
      engine.context.get_type(program.roots.last).to_s.should eq("Permissions")
    end

    it "supports enum helper constructors that delegate to the base-type constructor" do
      source = <<-CRYSTAL
        enum Permissions : Int16
          Read = 0

          def self.new(int : Int)
            new(int.to_i16)
          end
        end

        Permissions.new(0o644)
      CRYSTAL

      program, analyzer, engine = infer_enum_class_method_types(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.select(&.level.error?).should be_empty
      engine.context.get_type(program.roots.last).to_s.should eq("Permissions")
    end

    it "resolves enum class-method self return types inside shadowing modules" do
      source = <<-CRYSTAL
        module Crystal::System::File
          def self.shadowed
            nil
          end
        end

        enum Errno
          NONE = 0

          def self.value : self
            new(0)
          end
        end

        module Crystal::System::Threading
          def self.probe
            ::Errno.value
          end
        end

        Crystal::System::Threading.probe
      CRYSTAL

      program, analyzer, engine = infer_enum_class_method_types(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.select(&.level.error?).should be_empty
      engine.context.get_type(program.roots.last).to_s.should eq("Errno")
    end

    it "keeps enum helper constructors concrete through narrowed union reassignment" do
      source = <<-CRYSTAL
        class File
          @[Flags]
          enum Permissions : Int16
            OwnerRead = 0o400

            def self.new(int : Int)
              new(int.to_i16)
            end
          end
        end

        module Checker
          def self.consume(value : ::File::Permissions) : Nil
          end
        end

        module Crystal::System::File
          def self.open(filename : String, mode : String, perm : Int32 | ::File::Permissions, blocking : Bool?) : Nil
            perm = ::File::Permissions.new(perm) if perm.is_a? Int32
            Checker.consume(perm)
          end
        end

        Crystal::System::File.open("x", "w", 0, true)
      CRYSTAL

      program, analyzer, engine = infer_enum_class_method_types(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.select(&.level.error?).should be_empty
      engine.context.get_type(program.roots.last).to_s.should eq("Nil")
    end

    it "treats resolved enum identifiers as type receivers without a second global lookup" do
      source = <<-CRYSTAL
        module Outer
          enum Errno
            NONE = 0
          end

          def self.probe(code : Int32)
            Errno.new(code)
          end
        end

        Outer.probe(0)
      CRYSTAL

      program, analyzer, engine = infer_enum_class_method_types(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.select(&.level.error?).should be_empty

      module_node = program.arena[program.roots.first].as(Frontend::ModuleNode)
      def_id = module_node.body.not_nil!.last
      def_node = program.arena[def_id].as(Frontend::DefNode)
      call_node = program.arena[def_node.body.not_nil!.first].as(Frontend::CallNode)
      callee = program.arena[call_node.callee].as(Frontend::MemberAccessNode)

      engine.__spec_global_table = Semantic::SymbolTable.new(nil)
      engine.__spec_type_receiver_expression(callee.object).should be_true
    end
  end
end
