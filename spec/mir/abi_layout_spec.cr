require "../spec_helper"
require "../../src/compiler/frontend/lexer"
require "../../src/compiler/frontend/parser"
require "../../src/compiler/hir/ast_to_hir"
require "../../src/compiler/mir/hir_to_mir"

# Parse Crystal code and return arena + roots
private def parse(code : String) : {CrystalV2::Compiler::Frontend::ArenaLike, Array(CrystalV2::Compiler::Frontend::ExprId)}
  lexer = CrystalV2::Compiler::Frontend::Lexer.new(code)
  parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
  result = parser.parse_program
  {result.arena, result.roots}
end

# Build MIR module with registered class/struct/union layouts (no function lowering).
private def build_mir_module(code : String) : Crystal::MIR::Module
  arena, exprs = parse(code)
  converter = Crystal::HIR::AstToHir.new(arena)

  class_nodes = [] of CrystalV2::Compiler::Frontend::ClassNode
  enum_nodes = [] of CrystalV2::Compiler::Frontend::EnumNode

  exprs.each do |expr_id|
    node = arena[expr_id]
    case node
    when CrystalV2::Compiler::Frontend::ClassNode
      class_nodes << node
    when CrystalV2::Compiler::Frontend::EnumNode
      enum_nodes << node
    end
  end

  enum_nodes.each { |n| converter.register_enum(n) }
  class_nodes.each { |n| converter.register_class(n) }

  mir_lowering = Crystal::MIR::HIRToMIRLowering.new(converter.module)
  mir_lowering.register_union_types(converter.union_descriptors)
  mir_lowering.register_class_types(converter.class_info)
  mir_lowering.mir_module
end

describe "MIR ABI layout sanity" do
  it "lays out class ivars with a header offset" do
    code = <<-CRYSTAL
      class A
        @x : Int32
        @y : Int64
      end
    CRYSTAL

    mir_mod = build_mir_module(code)
    a_type = mir_mod.type_registry.get_by_name("A").not_nil!

    a_type.kind.reference?.should be_true
    # 4-byte type_id header + Int32(4) + Int64(8) = 16, aligned to 8
    a_type.size.should eq(16)
    a_type.alignment.should eq(8)

    fields = a_type.fields.not_nil!
    fields.find(&.name.==("@x")).not_nil!.offset.should eq(4)
    fields.find(&.name.==("@y")).not_nil!.offset.should eq(8)
  end

  it "lays out subclass ivars after parent fields" do
    code = <<-CRYSTAL
      class Parent
        @x : Int32
      end

      class Child < Parent
        @y : Int64
      end
    CRYSTAL

    mir_mod = build_mir_module(code)
    child_type = mir_mod.type_registry.get_by_name("Child").not_nil!

    child_type.kind.reference?.should be_true
    # 4-byte type_id header + Int32(4) + Int64(8) = 16, aligned to 8
    child_type.size.should eq(16)
    child_type.alignment.should eq(8)

    fields = child_type.fields.not_nil!
    fields.find(&.name.==("@x")).not_nil!.offset.should eq(4)
    fields.find(&.name.==("@y")).not_nil!.offset.should eq(8)
  end

  it "lays out struct ivars without a header offset" do
    code = <<-CRYSTAL
      struct S
        @a : Int32
        @b : Int64
      end
    CRYSTAL

    mir_mod = build_mir_module(code)
    s_type = mir_mod.type_registry.get_by_name("S").not_nil!

    s_type.kind.struct?.should be_true
    # Struct has no header. @a:Int32@0(4) + 4 padding + @b:Int64@8(8) = 16
    s_type.size.should eq(16)
    s_type.alignment.should eq(8)

    fields = s_type.fields.not_nil!
    fields.find(&.name.==("@a")).not_nil!.offset.should eq(0)
    fields.find(&.name.==("@b")).not_nil!.offset.should eq(8)
  end

  it "computes union header and payload offsets" do
    code = <<-CRYSTAL
      class U
        @u : Int32 | Int64
      end
    CRYSTAL

    mir_mod = build_mir_module(code)
    union_type = mir_mod.type_registry.get_by_name("Int32 | Int64").not_nil!
    union_type.kind.union?.should be_true
    union_type.size.should eq(16)
    union_type.alignment.should eq(8)

    descriptor = mir_mod.get_union_descriptor(Crystal::MIR::TypeRef.new(union_type.id)).not_nil!
    descriptor.payload_offset.should eq(8)
    descriptor.max_payload_size.should eq(8)
  end

  it "aligns union payload to the max variant alignment" do
    code = <<-CRYSTAL
      class U
        @u : Int128 | Int8
      end
    CRYSTAL

    mir_mod = build_mir_module(code)
    union_type = mir_mod.type_registry.get_by_name("Int128 | Int8").not_nil!
    union_type.kind.union?.should be_true
    union_type.size.should eq(32)
    union_type.alignment.should eq(16)

    descriptor = mir_mod.get_union_descriptor(Crystal::MIR::TypeRef.new(union_type.id)).not_nil!
    descriptor.payload_offset.should eq(16)
    descriptor.max_payload_size.should eq(16)
  end
end
