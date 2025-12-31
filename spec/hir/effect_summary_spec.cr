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

private def lower_named_function(
  code : String,
  name : String
) : {Crystal::HIR::Module, Crystal::HIR::Function}
  arena, exprs = parse(code)
  converter = Crystal::HIR::AstToHir.new(arena)

  class_nodes = [] of CrystalV2::Compiler::Frontend::ClassNode
  enum_nodes = [] of CrystalV2::Compiler::Frontend::EnumNode
  def_nodes = [] of CrystalV2::Compiler::Frontend::DefNode

  exprs.each do |expr_id|
    node = arena[expr_id]
    case node
    when CrystalV2::Compiler::Frontend::ClassNode
      class_nodes << node
    when CrystalV2::Compiler::Frontend::EnumNode
      enum_nodes << node
    when CrystalV2::Compiler::Frontend::DefNode
      def_nodes << node
    end
  end

  enum_nodes.each { |node| converter.register_enum(node) }
  class_nodes.each { |node| converter.register_class(node) }
  def_nodes.each { |node| converter.register_function(node) }
  class_nodes.each { |node| converter.lower_class(node) }

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

describe "method effect summaries" do
  it "uses @[Transfer] to mark call arguments as ArgEscape" do
    code = <<-CRYSTAL
      annotation Transfer
      end

      class Box
        @[Transfer]
        def put(value)
        end
      end

      class Data
      end

      def foo
        box = Box.new
        item = Data.new
        box.put(item)
        box
      end
    CRYSTAL

    hir_mod, func = lower_named_function(code, "foo")
    analyzer = Crystal::HIR::EscapeAnalyzer.new(func, nil, hir_mod)
    analyzer.analyze

    call = find_call_by_prefix(func, "Data.new")
    call.lifetime.should eq(Crystal::HIR::LifetimeTag::ArgEscape)
  end

  it "uses @[ThreadShared] to taint call arguments" do
    code = <<-CRYSTAL
      annotation ThreadShared
      end

      class Bus
        @[ThreadShared]
        def publish(value)
        end
      end

      class Data
      end

      def foo
        bus = Bus.new
        item = Data.new
        bus.publish(item)
        bus
      end
    CRYSTAL

    hir_mod, func = lower_named_function(code, "foo")
    analyzer = Crystal::HIR::TaintAnalyzer.new(func, nil, hir_mod)
    analyzer.analyze

    call = find_call_by_prefix(func, "Data.new")
    call.taints.thread_shared?.should be_true
  end

  it "uses @[Taints(:thread_shared)] to taint call arguments" do
    code = <<-CRYSTAL
      annotation Taints
      end

      class Bus
        @[Taints(:thread_shared)]
        def publish(value)
        end
      end

      class Data
      end

      def foo
        bus = Bus.new
        item = Data.new
        bus.publish(item)
        bus
      end
    CRYSTAL

    hir_mod, func = lower_named_function(code, "foo")
    analyzer = Crystal::HIR::TaintAnalyzer.new(func, nil, hir_mod)
    analyzer.analyze

    call = find_call_by_prefix(func, "Data.new")
    call.taints.thread_shared?.should be_true
  end
end
