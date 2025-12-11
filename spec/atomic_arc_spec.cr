require "spec"
require "../src/compiler/hir/hir"
require "../src/compiler/hir/ast_to_hir"
require "../src/compiler/hir/escape_analysis"
require "../src/compiler/hir/memory_strategy"
require "../src/compiler/mir/mir"
require "../src/compiler/mir/hir_to_mir"

def build_mir_for_atomic(source : String)
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
    ms = Crystal::HIR::MemoryStrategyAssigner.new(func)
    ms.assign
  end

  lowering = Crystal::MIR::HIRToMIRLowering.new(hir_module)
  lowering.register_class_types(hir_converter.class_info)
  lowering.lower
end

describe "Atomic ARC" do
  it "emits atomic rc for thread-shared allocations" do
    mir = build_mir_for_atomic(<<-CR)
      class Foo; end
      @@shared = Foo.new
    CR

    mir_text = mir.to_s
    mir_text.should contain("rc_inc_atomic")
  end
end
