require "spec"
require "../src/compiler/hir/hir"
require "../src/compiler/hir/ast_to_hir"
require "../src/compiler/hir/escape_analysis"
require "../src/compiler/hir/taint_analysis"

module CycleDetectionSpec
  def self.analyze(source : String)
    lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
    parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
    result = parser.parse_program
    arena = result.arena
    exprs = result.roots

    hir_converter = Crystal::HIR::AstToHir.new(arena, "spec")

    def_nodes = [] of CrystalV2::Compiler::Frontend::DefNode
    class_nodes = [] of CrystalV2::Compiler::Frontend::ClassNode
    exprs.each do |expr_id|
      node = arena[expr_id]
      case node
      when CrystalV2::Compiler::Frontend::DefNode
        def_nodes << node
      when CrystalV2::Compiler::Frontend::ClassNode
        class_nodes << node
      end
    end

    class_nodes.each { |c| hir_converter.register_class(c) }
    def_nodes.each { |d| hir_converter.register_function(d) }
    class_nodes.each { |c| hir_converter.lower_class(c) }
    def_nodes.each { |d| hir_converter.lower_def(d) }

    hir_module = hir_converter.module
    hir_module.functions.each do |func|
      escape = Crystal::HIR::EscapeAnalyzer.new(func)
      escape.analyze
    end

    analyzer = nil
    hir_module.functions.each do |func|
      analyzer = Crystal::HIR::TaintAnalyzer.new(func)
      analyzer.analyze
    end
    analyzer.not_nil!
  end

  describe "Cyclic taint heuristics" do
    it "marks Array of self as cyclic" do
      analyzer = CycleDetectionSpec.analyze(<<-CR)
        class Node
          property children : Array(Node)
        end
        Node.new
      CR
      expect(analyzer.values_with_taint(Crystal::HIR::Taint::Cyclic)).not_to be_empty
    end

    it "marks optional self reference as cyclic" do
      analyzer = CycleDetectionSpec.analyze(<<-CR)
        class Node
          property next : Node?
        end
        Node.new
      CR
      expect(analyzer.values_with_taint(Crystal::HIR::Taint::Cyclic)).not_to be_empty
    end
  end
end
