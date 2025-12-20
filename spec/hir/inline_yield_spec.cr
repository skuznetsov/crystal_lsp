require "../spec_helper"
require "../../src/compiler/hir/ast_to_hir"
require "../../src/compiler/frontend/parser"
require "../../src/compiler/frontend/lexer"

private def parse(code : String) : {CrystalV2::Compiler::Frontend::ArenaLike, Array(CrystalV2::Compiler::Frontend::ExprId)}
  lexer = CrystalV2::Compiler::Frontend::Lexer.new(code)
  parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
  result = parser.parse_program
  {result.arena, result.roots}
end

private def lower_named_function(code : String, name : String) : Crystal::HIR::Function
  arena, exprs = parse(code)
  converter = Crystal::HIR::AstToHir.new(arena)

  def_nodes = [] of CrystalV2::Compiler::Frontend::DefNode
  exprs.each do |expr_id|
    node = arena[expr_id]
    def_nodes << node.as(CrystalV2::Compiler::Frontend::DefNode) if node.is_a?(CrystalV2::Compiler::Frontend::DefNode)
  end

  def_nodes.each { |node| converter.register_function(node) }

  target = def_nodes.find { |node| String.new(node.name) == name }
  raise "function #{name} not found" unless target

  converter.lower_def(target)
end

private def phi_count(func : Crystal::HIR::Function) : Int32
  func.blocks.sum do |block|
    block.instructions.count { |inst| inst.is_a?(Crystal::HIR::Phi) }
  end
end

describe "inline yield phi propagation" do
  it "threads assigned locals from block into inlined loop" do
    code_with_assign = <<-CRYSTAL
      def repeat(n : Int32)
        i = 0
        while i < n
          yield i
          i += 1
        end
      end

      def foo
        sum = 0
        repeat(3) { |i| sum = sum + i }
        sum
      end
    CRYSTAL

    code_without_assign = <<-CRYSTAL
      def repeat(n : Int32)
        i = 0
        while i < n
          yield i
          i += 1
        end
      end

      def foo
        repeat(3) { |i| i }
        0
      end
    CRYSTAL

    with_assign = phi_count(lower_named_function(code_with_assign, "foo"))
    without_assign = phi_count(lower_named_function(code_without_assign, "foo"))

    with_assign.should be > without_assign
    without_assign.should eq(1)
  end
end
