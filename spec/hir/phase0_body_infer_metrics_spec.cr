require "../spec_helper"
require "../../src/compiler/hir/ast_to_hir"
require "../../src/compiler/frontend/parser"
require "../../src/compiler/frontend/lexer"

class Crystal::HIR::AstToHir
  def __test_record_phase0_body_infer_walk(
    node : CrystalV2::Compiler::Frontend::DefNode,
    resolved_arena : CrystalV2::Compiler::Frontend::ArenaLike,
    node_expr_id : CrystalV2::Compiler::Frontend::ExprId? = nil,
  ) : CrystalV2::Compiler::Semantic::DefIdentity?
    record_phase0_body_infer_walk(node, resolved_arena, node_expr_id)
  end

  def __test_phase0_body_infer_counts : Hash(CrystalV2::Compiler::Semantic::DefIdentity, Int32)
    @phase0_body_infer_counts.dup
  end
end

private def parse_phase0_metric_program(
  code : String,
) : {CrystalV2::Compiler::Frontend::ArenaLike, Array(CrystalV2::Compiler::Frontend::ExprId)}
  lexer = CrystalV2::Compiler::Frontend::Lexer.new(code)
  parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
  result = parser.parse_program
  {result.arena, result.roots}
end

private def first_phase0_metric_def(
  arena : CrystalV2::Compiler::Frontend::ArenaLike,
  exprs : Array(CrystalV2::Compiler::Frontend::ExprId),
) : {CrystalV2::Compiler::Frontend::ExprId, CrystalV2::Compiler::Frontend::DefNode}
  def_expr = exprs.find do |expr_id|
    arena[expr_id].is_a?(CrystalV2::Compiler::Frontend::DefNode)
  end
  raise "No function definition found" unless def_expr
  {def_expr, arena[def_expr].as(CrystalV2::Compiler::Frontend::DefNode)}
end

describe Crystal::HIR::AstToHir do
  describe "phase0 body inference metrics" do
    it "collapses reparsed defs by canonical identity" do
      code = <<-CRYSTAL
        def sample(value : Int32)
          value + 1
        end
      CRYSTAL

      arena_a, exprs_a = parse_phase0_metric_program(code)
      arena_b, exprs_b = parse_phase0_metric_program(code)
      expr_id_a, def_a = first_phase0_metric_def(arena_a, exprs_a)
      expr_id_b, def_b = first_phase0_metric_def(arena_b, exprs_b)

      path = "/tmp/phase0_body_infer_identity_spec.cr"
      sources_by_arena = {
        arena_a.object_id => code,
        arena_b.object_id => code,
      }
      paths_by_arena = {
        arena_a.object_id => path,
        arena_b.object_id => path,
      }

      converter = Crystal::HIR::AstToHir.new(
        arena_b,
        sources_by_arena: sources_by_arena,
        paths_by_arena: paths_by_arena,
        main_arenas: [arena_a, arena_b],
      )

      id_a = converter.__test_record_phase0_body_infer_walk(def_a, arena_a, expr_id_a)
      id_b = converter.__test_record_phase0_body_infer_walk(def_b, arena_b, expr_id_b)

      id_a.should_not be_nil
      id_b.should eq(id_a)
      id_a.not_nil!.arena_id.should eq(arena_a.object_id.to_u64)

      counts = converter.__test_phase0_body_infer_counts
      counts.size.should eq(1)
      counts[id_a.not_nil!].should eq(2)
    end

    it "normalizes caller arenas away from the canonical def identity" do
      code = <<-CRYSTAL
        def sample(value : Int32)
          value + 1
        end
      CRYSTAL

      arena_a, exprs_a = parse_phase0_metric_program(code)
      arena_b, exprs_b = parse_phase0_metric_program(code)
      arena_c, _exprs_c = parse_phase0_metric_program(code)
      expr_id_a, def_a = first_phase0_metric_def(arena_a, exprs_a)
      expr_id_b, def_b = first_phase0_metric_def(arena_b, exprs_b)

      path = "/tmp/phase0_body_infer_identity_spec_caller_arena.cr"
      sources_by_arena = {
        arena_a.object_id => code,
        arena_b.object_id => code,
        arena_c.object_id => code,
      }
      paths_by_arena = {
        arena_a.object_id => path,
        arena_b.object_id => path,
        arena_c.object_id => path,
      }

      converter = Crystal::HIR::AstToHir.new(
        arena_c,
        sources_by_arena: sources_by_arena,
        paths_by_arena: paths_by_arena,
        main_arenas: [arena_a, arena_b, arena_c],
      )

      canonical_id = converter.__test_record_phase0_body_infer_walk(def_a, arena_a, expr_id_a)
      wrong_caller_id = converter.__test_record_phase0_body_infer_walk(def_b, arena_c, expr_id_b)

      canonical_id.should_not be_nil
      wrong_caller_id.should eq(canonical_id)
      wrong_caller_id.not_nil!.arena_id.should eq(arena_a.object_id.to_u64)

      counts = converter.__test_phase0_body_infer_counts
      counts.size.should eq(1)
      counts[canonical_id.not_nil!].should eq(2)
    end
  end
end
