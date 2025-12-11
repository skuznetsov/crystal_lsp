# Memory strategy assignment: ensure thread-shared taints select AtomicARC.

require "spec"
require "../src/compiler/hir/hir"
require "../src/compiler/hir/ast_to_hir"
require "../src/compiler/hir/escape_analysis"
require "../src/compiler/hir/taint_analysis"
require "../src/compiler/hir/memory_strategy"

def build_hir_for_mm(source : String)
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

  hir_converter.module
end

describe "Memory strategy" do
  it "marks thread-shared allocations as AtomicARC" do
    hir_module = build_hir_for_mm(<<-CR)
      class Foo; end
      @@shared = Foo.new
    CR

    hir_module.functions.each do |func|
      escape = Crystal::HIR::EscapeAnalyzer.new(func)
      escape.analyze
      ms = Crystal::HIR::MemoryStrategyAssigner.new(func)
      ms.assign
    end

    strategies = [] of Crystal::HIR::MemoryStrategy
    hir_module.functions.each do |func|
      func.blocks.each do |block|
        block.instructions.each do |inst|
          if inst.is_a?(Crystal::HIR::Allocate)
            strategies << (inst.memory_strategy || Crystal::HIR::MemoryStrategy::Unknown)
          end
        end
      end
    end

    strategies.should_not be_empty
    strategies.any?(Crystal::HIR::MemoryStrategy::AtomicARC).should be_true
  end
end
