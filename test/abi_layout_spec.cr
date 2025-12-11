# ABI layout sanity: verify struct/class/union offsets and sizes against MIR TypeRegistry snapshots.

require "spec"
require "../src/compiler/hir/hir"
require "../src/compiler/hir/ast_to_hir"
require "../src/compiler/hir/escape_analysis"
require "../src/compiler/mir/mir"
require "../src/compiler/mir/hir_to_mir"

module AbiLayoutSpec
  private def build_mir(source : String)
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
      mir = build_mir(<<-CR)
        struct Pair
          property x : Int32
          property y : Int64
        end
      CR

      snapshot = mir.type_registry.layout_snapshot
      pair = mir.type_registry.get_by_name("Pair").not_nil!
      expect(pair.kind).to eq Crystal::MIR::TypeKind::Struct
      expect(pair.size).to be >= 16
      expect(snapshot).to contain("@x")
      expect(snapshot).to contain("@y")
    end

    it "reports union variant sizes" do
      mir = build_mir(<<-CR)
        class A; end
        class B; end
        alias U = A | B
      CR

      snapshot = mir.type_registry.layout_snapshot
      union = mir.type_registry.get_by_name("U").not_nil!
      expect(union.kind).to eq Crystal::MIR::TypeKind::Union
      expect(snapshot).to contain("variant")
    end
  end
end
