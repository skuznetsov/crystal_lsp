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
  describe "generic extend self mixins" do
    it "binds self type args for nested member lookups" do
      source = <<-CRYSTAL
        module M(T)
          def foo(x : T::CarrierUInt) : T::CarrierUInt
            x
          end
        end

        module X
          alias CarrierUInt = UInt32
          extend M(self)
        end

        X.foo(1_u32)
      CRYSTAL

      program, _analyzer, engine = infer_types(source)

      engine.diagnostics.should be_empty

      root_type = engine.context.get_type(program.roots.last)
      root_type.should_not be_nil
      root_type.not_nil!.to_s.should eq("UInt32")
    end

    it "resolves bound type params in mixed-in method bodies" do
      source = <<-CRYSTAL
        module InfoMethods(D)
          def extract(u : D::CarrierUInt)
            mask = ~(UInt32::MAX << D::EXPONENT_BITS)
            ((u >> D::SIGNIFICAND_BITS) & mask).to_u32!
          end
        end

        module HostInfo
          alias CarrierUInt = UInt32
          EXPONENT_BITS = 8
          SIGNIFICAND_BITS = 23
          extend InfoMethods(self)
        end

        HostInfo.extract(1_u32)
      CRYSTAL

      program, _analyzer, engine = infer_types(source)

      engine.diagnostics.should be_empty

      root_type = engine.context.get_type(program.roots.last)
      root_type.should_not be_nil
      root_type.not_nil!.to_s.should eq("UInt32")
    end

    it "treats primitive type constants as value-typed path expressions" do
      source = <<-CRYSTAL
        UInt32::MAX << 8
      CRYSTAL

      program, _analyzer, engine = infer_types(source)

      engine.diagnostics.should be_empty

      root_type = engine.context.get_type(program.roots.last)
      root_type.should_not be_nil
      root_type.not_nil!.to_s.should eq("UInt32")
    end

    it "resolves specialized module args for direct class-method calls" do
      source = <<-CRYSTAL
        module CacheMethods(D)
        end

        module Info
          extend CacheMethods(self)
        end

        module CacheMethods(D)
          def get_cache(k : Int32) : Int32
            k + 1
          end
        end

        module Host(F, ImplInfo)
          def self.run
            ImplInfo.get_cache(1)
          end
        end

        Host(Float32, Info).run
      CRYSTAL

      program, _analyzer, engine = infer_types(source)

      engine.diagnostics.should be_empty

      root_type = engine.context.get_type(program.roots.last)
      root_type.should_not be_nil
      root_type.not_nil!.to_s.should eq("Int32")
    end

    it "keeps specialized generic module receivers for receiverless class-method calls" do
      source = <<-CRYSTAL
        module CacheMethods(D)
        end

        module Info
          extend CacheMethods(self)
        end

        module CacheMethods(D)
          def get_cache(k : Int32) : Int32
            k + 1
          end
        end

        module Host(F, ImplInfo)
          def self.helper
            ImplInfo.get_cache(1)
          end

          def self.run
            helper
          end
        end

        Host(Float32, Info).run
      CRYSTAL

      program, _analyzer, engine = infer_types(source)

      engine.diagnostics.should be_empty

      root_type = engine.context.get_type(program.roots.last)
      root_type.should_not be_nil
      root_type.not_nil!.to_s.should eq("Int32")
    end
  end
end
