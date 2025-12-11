require "spec"
require "../src/compiler/hir/hir"
require "../src/compiler/hir/ast_to_hir"
require "../src/compiler/hir/escape_analysis"
require "../src/compiler/mir/mir"
require "../src/compiler/mir/hir_to_mir"

def build_mir_for_abi(source : String)
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

  lowering = Crystal::MIR::HIRToMIRLowering.new(hir_module)
  lowering.register_class_types(hir_converter.class_info)
  lowering.register_union_types(hir_converter.union_descriptors)
  lowering.lower
end

describe "ABI layout" do
  it "aligns struct fields correctly" do
    mir = build_mir_for_abi(<<-CR)
      struct Pair
        property x : Int32
        property y : Int64
      end
    CR

    snapshot = mir.type_registry.layout_snapshot
    pair = mir.type_registry.get_by_name("Pair").not_nil!
    pair.kind.should eq Crystal::MIR::TypeKind::Struct
    pair.size.should be >= 16
    snapshot.should contain("@x")
    snapshot.should contain("@y")
  end

  it "reports union variant sizes" do
    mir = build_mir_for_abi(<<-CR)
      class A; end
      class B; end
      alias U = A | B
    CR

    snapshot = mir.type_registry.layout_snapshot
    union = mir.type_registry.get_by_name("U").not_nil!
    union.kind.should eq Crystal::MIR::TypeKind::Union
    snapshot.should contain("variant")
  end

  it "shows class ivar offsets" do
    mir = build_mir_for_abi(<<-CR)
      class Foo
        def initialize(@a : Int32, @b : Int64); end
      end
    CR

    snapshot = mir.type_registry.layout_snapshot
    snapshot.should contain("Foo")
    snapshot.should contain("@a")
    snapshot.should contain("@b")
  end
end
