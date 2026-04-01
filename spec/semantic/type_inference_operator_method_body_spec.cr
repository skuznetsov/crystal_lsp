require "spec"

require "../../src/compiler/frontend/ast"
require "../../src/compiler/frontend/lexer"
require "../../src/compiler/frontend/parser"
require "../../src/compiler/semantic/analyzer"
require "../../src/compiler/semantic/type_inference_engine"

alias Frontend = CrystalV2::Compiler::Frontend
alias Semantic = CrystalV2::Compiler::Semantic

private def infer_operator_method_body_types(source : String)
  parser = Frontend::Parser.new(Frontend::Lexer.new(source))
  program = parser.parse_program

  analyzer = Semantic::Analyzer.new(program)
  analyzer.collect_symbols
  name_result = analyzer.resolve_names
  engine = analyzer.infer_types(name_result.identifier_symbols)

  {program, analyzer, engine}
end

describe Semantic::TypeInferenceEngine do
  describe "binary operator methods without return annotations" do
    it "infers method bodies instead of collapsing to Nil" do
      source = <<-CRYSTAL
        struct UInt32
          def //(other : Int32)
            self
          end
        end

        module Probe
          def self.wrap(value : UInt32)
            value //= 10000
            value
          end
        end

        Probe.wrap(1_u32)
      CRYSTAL

      program, analyzer, engine = infer_operator_method_body_types(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.select(&.level.error?).should be_empty
      engine.context.get_type(program.roots.last).to_s.should eq("UInt32")
    end

    it "supports integer left shifts during on-demand body inference" do
      source = <<-CRYSTAL
        module Probe
          def self.shift32(value : UInt32)
            value << 1
          end

          def self.shift64(value : UInt64)
            value << 32
          end
        end

        {Probe.shift32(1_u32), Probe.shift64(1_u64)}
      CRYSTAL

      program, analyzer, engine = infer_operator_method_body_types(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.select(&.level.error?).should be_empty

      root_type = engine.context.get_type(program.roots.last)
      root_type.to_s.should eq("Tuple(UInt32, UInt64)")
    end

    it "supports integer right shifts with Int32 counts during on-demand body inference" do
      source = <<-CRYSTAL
        module Probe
          def self.shift128(value : Int128, amount : Int32)
            value >> amount
          end

          def self.shiftu128(value : UInt128, amount : Int32)
            value >> amount
          end
        end

        {Probe.shift128(1_i128, 127), Probe.shiftu128(1_u128, 64)}
      CRYSTAL

      program, analyzer, engine = infer_operator_method_body_types(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.select(&.level.error?).should be_empty

      root_type = engine.context.get_type(program.roots.last)
      root_type.to_s.should eq("Tuple(Int128, UInt128)")
    end

    it "supports Int arithmetic inside helper-heavy method bodies" do
      source = <<-CRYSTAL
        module Probe
          def self.jan1_to_unix(year : Int) : Int64
            year -= 1
            days = year * 365 + year // 4 - year // 100 + year // 400
            86400_i64 * days
          end
        end

        Probe.jan1_to_unix(2024)
      CRYSTAL

      program, analyzer, engine = infer_operator_method_body_types(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.select(&.level.error?).should be_empty

      root_type = engine.context.get_type(program.roots.last)
      root_type.to_s.should eq("Int64")
    end

    it "supports plain integer to_i in macro-shaped generic helper bodies" do
      source = <<-CRYSTAL
        module WUInt
          def self.umul64(x : UInt32, y : UInt32) : UInt64
            x.to_u64 &* y
          end

          def self.umul96_upper64(x : UInt32, y : UInt64) : UInt64
            yh = (y >> 32).to_u32!
            yl = y.to_u32!
            xyh = umul64(x, yh)
            xyl = umul64(x, yl)
            xyh &+ (xyl >> 32)
          end
        end

        module Impl(F, ImplInfo)
          def self.compute_mul(u, cache)
            {% if F == Float32 %}
              r = WUInt.umul96_upper64(u, cache)
              {
                ImplInfo::CarrierUInt.new!(r >> 32),
                ImplInfo::CarrierUInt.new!(r) == 0,
              }
            {% else %}
              {1_u64, true}
            {% end %}
          end

          def self.run(two_fc, beta)
            zi, is_z_integer = compute_mul((two_fc | 1) << beta, 123_u64)
            significand = zi // 10_u32
            {significand, is_z_integer}
          end
        end

        module Info
          alias CarrierUInt = UInt32
        end

        module Wrapper
          def self.to_decimal(signed_significand_bits, exponent_bits)
            exponent = exponent_bits.to_i
            Impl(Float32, Info).run(signed_significand_bits, exponent)
          end
        end

        Wrapper.to_decimal(1_u32, 1_u32)
      CRYSTAL

      program, analyzer, engine = infer_operator_method_body_types(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.select(&.level.error?).should be_empty

      root_type = engine.context.get_type(program.roots.last)
      root_type.to_s.should eq("Tuple(UInt32, Bool)")
    end
  end

  describe "primitive annotations with runtime primitive classes" do
    it "keeps unsigned primitive annotations primitive for top-level helper calls" do
      source = <<-CRYSTAL
        struct UInt32
        end

        def helper : UInt32
          1_u32
        end

        def probe(flag : Bool)
          idx = flag ? 0_u32 : helper
          idx.to_i32!
        end

        probe(false)
      CRYSTAL

      program, analyzer, engine = infer_operator_method_body_types(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.select(&.level.error?).should be_empty
      engine.context.get_type(program.roots.last).to_s.should eq("Int32")
    end

    it "keeps Int annotations primitive even when a runtime Int struct symbol exists" do
      source = <<-CRYSTAL
        struct Int
        end

        module Probe
          def self.jan1_to_unix(year : Int) : Int64
            year -= 1
            days = year * 365 + year // 4 - year // 100 + year // 400
            86400_i64 * days.to_i64
          end
        end

        Probe.jan1_to_unix(2024)
      CRYSTAL

      program, analyzer, engine = infer_operator_method_body_types(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.select(&.level.error?).should be_empty
      engine.context.get_type(program.roots.last).to_s.should eq("Int64")
    end
  end
end
