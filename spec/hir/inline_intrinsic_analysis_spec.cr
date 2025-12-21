require "../spec_helper"
require "../../src/compiler/hir/ast_to_hir"
require "../../src/compiler/hir/escape_analysis"
require "../../src/compiler/hir/taint_analysis"
require "../../src/compiler/frontend/parser"
require "../../src/compiler/frontend/lexer"

private def parse(code : String) : {CrystalV2::Compiler::Frontend::ArenaLike, Array(CrystalV2::Compiler::Frontend::ExprId)}
  lexer = CrystalV2::Compiler::Frontend::Lexer.new(code)
  parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
  result = parser.parse_program
  {result.arena, result.roots}
end

private def lower_named_function(code : String, name : String) : {Crystal::HIR::Module, Crystal::HIR::Function}
  arena, exprs = parse(code)
  converter = Crystal::HIR::AstToHir.new(arena)

  class_nodes = [] of CrystalV2::Compiler::Frontend::ClassNode
  struct_nodes = [] of CrystalV2::Compiler::Frontend::StructNode
  enum_nodes = [] of CrystalV2::Compiler::Frontend::EnumNode
  def_nodes = [] of CrystalV2::Compiler::Frontend::DefNode

  exprs.each do |expr_id|
    node = arena[expr_id]
    case node
    when CrystalV2::Compiler::Frontend::ClassNode
      class_nodes << node
    when CrystalV2::Compiler::Frontend::StructNode
      struct_nodes << node
    when CrystalV2::Compiler::Frontend::EnumNode
      enum_nodes << node
    when CrystalV2::Compiler::Frontend::DefNode
      def_nodes << node
    end
  end

  enum_nodes.each { |node| converter.register_enum(node) }
  struct_nodes.each { |node| converter.register_struct(node) }
  class_nodes.each { |node| converter.register_class(node) }
  def_nodes.each { |node| converter.register_function(node) }

  target = def_nodes.find { |node| String.new(node.name) == name }
  raise "function #{name} not found" unless target

  {converter.module, converter.lower_def(target)}
end

private def find_call_by_prefix(
  func : Crystal::HIR::Function,
  prefix : String
) : Crystal::HIR::Call
  func.blocks.each do |block|
    block.instructions.each do |inst|
      next unless inst.is_a?(Crystal::HIR::Call)
      return inst if inst.method_name.starts_with?(prefix)
    end
  end
  raise "call with prefix #{prefix} not found"
end

private def find_value(func : Crystal::HIR::Function, id : Crystal::HIR::ValueId) : Crystal::HIR::Value?
  func.blocks.each do |block|
    block.instructions.each do |inst|
      return inst if inst.id == id
    end
  end
  func.params.find { |param| param.id == id }
end

describe "inline intrinsics analysis" do
  it "marks allocations inside times loops as ArgEscape" do
    code = <<-CRYSTAL
      class Data
      end

      def foo
        arr = [] of Data
        3.times { arr << Data.new }
        arr
      end
    CRYSTAL

    _, func = lower_named_function(code, "foo")
    analyzer = Crystal::HIR::EscapeAnalyzer.new(func)
    analyzer.analyze

    call = find_call_by_prefix(func, "Data.new")
    call.lifetime.should eq(Crystal::HIR::LifetimeTag::ArgEscape)
  end

  it "marks allocations inside range each loops as ArgEscape" do
    code = <<-CRYSTAL
      class Data
      end

      def foo
        arr = [] of Data
        (0..2).each { arr << Data.new }
        arr
      end
    CRYSTAL

    _, func = lower_named_function(code, "foo")
    analyzer = Crystal::HIR::EscapeAnalyzer.new(func)
    analyzer.analyze

    call = find_call_by_prefix(func, "Data.new")
    call.lifetime.should eq(Crystal::HIR::LifetimeTag::ArgEscape)
  end

  it "marks allocations inside array each loops as ArgEscape" do
    code = <<-CRYSTAL
      class Data
      end

      def foo
        arr = [] of Data
        [1, 2].each { arr << Data.new }
        arr
      end
    CRYSTAL

    _, func = lower_named_function(code, "foo")
    analyzer = Crystal::HIR::EscapeAnalyzer.new(func)
    analyzer.analyze

    call = find_call_by_prefix(func, "Data.new")
    call.lifetime.should eq(Crystal::HIR::LifetimeTag::ArgEscape)
  end

  it "marks by-reference captures as Mutable inside inline loops" do
    code = <<-CRYSTAL
      def foo
        x = 0
        3.times do
          -> { x += 1 }
        end
        x
      end
    CRYSTAL

    _, func = lower_named_function(code, "foo")
    analyzer = Crystal::HIR::TaintAnalyzer.new(func)
    analyzer.analyze

    closures = [] of Crystal::HIR::MakeClosure
    func.blocks.each do |block|
      block.instructions.each do |inst|
        closures << inst if inst.is_a?(Crystal::HIR::MakeClosure)
      end
    end

    closures.should_not be_empty

    mutable_non_alloc = func.blocks.any? do |block|
      block.instructions.any? do |inst|
        inst.taints.mutable? && !inst.is_a?(Crystal::HIR::Allocate)
      end
    end

    mutable_non_alloc.should be_true
  end
end
