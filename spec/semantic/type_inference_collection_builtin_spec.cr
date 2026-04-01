require "spec"

require "../../src/compiler/frontend/ast"
require "../../src/compiler/frontend/lexer"
require "../../src/compiler/frontend/parser"
require "../../src/compiler/semantic/analyzer"
require "../../src/compiler/semantic/type_inference_engine"

alias Frontend = CrystalV2::Compiler::Frontend
alias Semantic = CrystalV2::Compiler::Semantic

private def infer_collection_builtin_types(source : String)
  parser = Frontend::Parser.new(Frontend::Lexer.new(source))
  program = parser.parse_program

  analyzer = Semantic::Analyzer.new(program)
  analyzer.collect_symbols
  name_result = analyzer.resolve_names
  engine = analyzer.infer_types(name_result.identifier_symbols)

  {program, analyzer, engine}
end

describe Semantic::TypeInferenceEngine do
  describe "collection and integer builtin surface" do
    it "supports integer zero? and unsafe_chr" do
      source = <<-'CRYSTAL'
        flag = 0.zero?
        char = 97.unsafe_chr
        char
      CRYSTAL

      program, analyzer, engine = infer_collection_builtin_types(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.should be_empty
      engine.context.get_type(program.roots[0]).to_s.should eq("Bool")
      engine.context.get_type(program.roots.last).to_s.should eq("Char")
    end

    it "supports array sort! and bsearch with blocks" do
      source = <<-'CRYSTAL'
        cccs = [{3, 2_u8}, {1, 1_u8}, {2, 3_u8}]
        cccs.sort! { |x, y| x[1] <=> y[1] }

        result = if value = cccs.bsearch { |entry| 2_u8 <= entry[1] }
                   value[0]
                 else
                   0
                 end

        result
      CRYSTAL

      program, analyzer, engine = infer_collection_builtin_types(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.should be_empty
      engine.context.get_type(program.roots.last).to_s.should eq("Int32")
    end

    it "supports array and hash clone plus array delete and map_with_index" do
      source = <<-'CRYSTAL'
        flags = ["alpha"]
        handlers = {"alpha" => 3}

        old_flags = flags.clone
        old_handlers = handlers.clone
        removed = old_flags.delete("alpha")
        mapped = ["beta"].map_with_index { |value, i| value }

        (old_handlers["alpha"]? || 0) + (removed || mapped[0]).bytesize
      CRYSTAL

      program, analyzer, engine = infer_collection_builtin_types(source)

      analyzer.semantic_diagnostics.should be_empty
      analyzer.name_resolver_diagnostics.should be_empty
      engine.diagnostics.should be_empty
      engine.context.get_type(program.roots.last).to_s.should eq("Int32")
    end
  end
end
