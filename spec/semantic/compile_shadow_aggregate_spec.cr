require "spec"

require "../../src/compiler/frontend/ast"
require "../../src/compiler/frontend/lexer"
require "../../src/compiler/frontend/parser"
require "../../src/compiler/semantic/analyzer"
require "../../src/compiler/semantic/compile_shadow_aggregate"
require "../../src/compiler/semantic/types/primitive_type"

module CompileShadowAggregateSpecAliases
  alias Frontend = CrystalV2::Compiler::Frontend
  alias Semantic = CrystalV2::Compiler::Semantic
end

include CompileShadowAggregateSpecAliases

private def build_shared_shadow_aggregate(sources : Array(String)) : Semantic::CompileShadowAggregate
  units = [] of NamedTuple(path: String, source: String)
  sources.each_with_index do |source, index|
    units << {path: "unit_#{index}.cr", source: source}
  end
  Semantic::CompileShadowAggregate.build(units)
end

describe "compile semantic shadow aggregate" do
  it "resolves cross-file method calls in a shared AstArena aggregate" do
    aggregate = build_shared_shadow_aggregate([
      <<-CR,
        def greet
          41
        end
      CR
      <<-CR,
        greet()
      CR
    ])
    program = aggregate.program

    analyzer = Semantic::Analyzer.new(program)
    analyzer.collect_symbols
    result = analyzer.resolve_names

    result.diagnostics.should be_empty

    call_root = program.roots.last
    call_node = program.arena[call_root].as(Frontend::CallNode)
    callee_id = call_node.callee.not_nil!
    result.identifier_symbols[callee_id].should be_a(Semantic::MethodSymbol)
    aggregate.path_for(callee_id).should eq("unit_1.cr")
  end

  it "infers types across aggregated files in shared AstArena order" do
    aggregate = build_shared_shadow_aggregate([
      <<-CR,
        def answer : Int32
          41
        end
      CR
      <<-CR,
        answer() + 1
      CR
    ])
    program = aggregate.program

    analyzer = Semantic::Analyzer.new(program)
    analyzer.collect_symbols
    result = analyzer.resolve_names
    engine = analyzer.infer_types(result.identifier_symbols)

    result.diagnostics.should be_empty
    analyzer.type_inference_diagnostics.should be_empty

    root_type = engine.context.get_type(program.roots.last)
    root_type.should be_a(Semantic::PrimitiveType)
    root_type.as(Semantic::PrimitiveType).name.should eq("Int32")
  end

  it "tracks file ownership for nested aggregate nodes" do
    aggregate = build_shared_shadow_aggregate([
      <<-CR,
        def greet
          41
        end
      CR
      <<-CR,
        greet() + 1
      CR
    ])

    program = aggregate.program
    call_root = program.roots.last
    binary_node = program.arena[call_root].as(Frontend::BinaryNode)
    call_expr = binary_node.left
    call_node = program.arena[call_expr].as(Frontend::CallNode)
    callee_id = call_node.callee.not_nil!

    aggregate.unit_summaries.size.should eq(2)
    aggregate.unit_summaries[0].path.should eq("unit_0.cr")
    aggregate.unit_summaries[1].path.should eq("unit_1.cr")
    aggregate.unit_summaries[0].node_count.should be > 0
    aggregate.unit_summaries[1].node_count.should be > 0
    aggregate.path_for(call_root).should eq("unit_1.cr")
    aggregate.path_for(call_expr).should eq("unit_1.cr")
    aggregate.path_for(callee_id).should eq("unit_1.cr")
  end

  it "resolves calls to root-level macro-generated methods across aggregate files" do
    aggregate = build_shared_shadow_aggregate([
      <<-CR,
        {% for name in %w(alpha beta) %}
          def {{name.id}}
            41
          end
        {% end %}
      CR
      <<-CR,
        alpha()
      CR
    ])
    program = aggregate.program

    analyzer = Semantic::Analyzer.new(program)
    analyzer.collect_symbols(node_file_path_provider: ->(expr_id : Frontend::ExprId) { aggregate.path_for(expr_id) })
    result = analyzer.resolve_names

    result.diagnostics.should be_empty

    call_root = program.roots.last
    call_node = program.arena[call_root].as(Frontend::CallNode)
    callee_id = call_node.callee.not_nil!
    result.identifier_symbols[callee_id].should be_a(Semantic::MethodSymbol)
    aggregate.path_for(callee_id).should eq("unit_1.cr")
    alpha_symbol = analyzer.global_context.symbol_table.lookup("alpha")
    alpha_symbol.should be_a(Semantic::MethodSymbol)
    alpha_symbol.not_nil!.file_path.should eq("unit_0.cr")
    analyzer.generated_node_file_paths.values.uniq.should eq(["unit_0.cr"])
    analyzer.generated_node_file_paths.size.should be > 0
  end

  it "resolves calls after top-level macro call expansion across aggregate files" do
    aggregate = build_shared_shadow_aggregate([
      <<-CR,
        macro define_alpha
          def alpha
            41
          end
        end

        define_alpha
      CR
      <<-CR,
        alpha()
      CR
    ])
    program = aggregate.program

    analyzer = Semantic::Analyzer.new(program)
    analyzer.collect_symbols(node_file_path_provider: ->(expr_id : Frontend::ExprId) { aggregate.path_for(expr_id) })
    result = analyzer.resolve_names
    engine = analyzer.infer_types(result.identifier_symbols)

    result.diagnostics.should be_empty
    analyzer.type_inference_diagnostics.should be_empty

    call_root = program.roots.last
    call_node = program.arena[call_root].as(Frontend::CallNode)
    callee_id = call_node.callee.not_nil!
    result.identifier_symbols[callee_id].should be_a(Semantic::MethodSymbol)
    aggregate.path_for(callee_id).should eq("unit_1.cr")
    analyzer.global_context.symbol_table.lookup("alpha").should be_a(Semantic::MethodSymbol)
    engine.context.get_type(call_root).should be_a(Semantic::PrimitiveType)
  end

  it "resolves cross-file argful macro expansion in aggregate" do
    aggregate = build_shared_shadow_aggregate([
      <<-CR,
        macro define_alpha(dummy)
          def alpha
            42
          end
        end
      CR
      <<-CR,
        define_alpha(1)
        alpha()
      CR
    ])
    program = aggregate.program

    analyzer = Semantic::Analyzer.new(program)
    analyzer.collect_symbols(node_file_path_provider: ->(expr_id : Frontend::ExprId) { aggregate.path_for(expr_id) })
    result = analyzer.resolve_names

    # The macro is defined in unit_0, called with arg in unit_1.
    # After expansion, `alpha` should be resolvable.
    alpha = analyzer.global_context.symbol_table.lookup("alpha")
    alpha.should be_a(Semantic::MethodSymbol)

    # The call `alpha()` in unit_1 should resolve without errors
    call_root = program.roots.last
    call_node = program.arena[call_root].as(Frontend::CallNode)
    callee_id = call_node.callee.not_nil!
    result.identifier_symbols[callee_id].should be_a(Semantic::MethodSymbol)
  end

  it "resolves cross-file zero-arg macro call in aggregate" do
    aggregate = build_shared_shadow_aggregate([
      <<-CR,
        macro define_beta
          def beta
            99
          end
        end
      CR
      <<-CR,
        define_beta
        beta()
      CR
    ])
    program = aggregate.program

    analyzer = Semantic::Analyzer.new(program)
    analyzer.collect_symbols(node_file_path_provider: ->(expr_id : Frontend::ExprId) { aggregate.path_for(expr_id) })
    result = analyzer.resolve_names

    beta = analyzer.global_context.symbol_table.lookup("beta")
    beta.should be_a(Semantic::MethodSymbol)

    call_root = program.roots.last
    call_node = program.arena[call_root].as(Frontend::CallNode)
    callee_id = call_node.callee.not_nil!
    result.identifier_symbols[callee_id].should be_a(Semantic::MethodSymbol)
  end
end
