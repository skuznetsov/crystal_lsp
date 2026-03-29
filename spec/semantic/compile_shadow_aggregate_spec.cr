require "spec"

require "../../src/compiler/frontend/ast"
require "../../src/compiler/frontend/diagnostic_formatter"
require "../../src/compiler/frontend/lexer"
require "../../src/compiler/frontend/parser"
require "../../src/compiler/semantic/analyzer"
require "../../src/compiler/semantic/compile_shadow_aggregate"
require "../../src/compiler/semantic/diagnostic_formatter"
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

private def build_shadow_sources(aggregate : Semantic::CompileShadowAggregate) : Hash(String, String)
  sources = {} of String => String
  aggregate.unit_summaries.each { |unit| sources[unit.path] = unit.source }
  sources
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

  it "expands cross-file positional arg with {{name.id}}" do
    aggregate = build_shared_shadow_aggregate([
      <<-CR,
        macro make_method(name)
          def {{name.id}}
            42
          end
        end
      CR
      <<-CR,
        make_method(:greet)
        greet()
      CR
    ])
    program = aggregate.program
    shadow_sources = {} of String => String
    aggregate.unit_summaries.each { |u| shadow_sources[u.path] = u.source }

    analyzer = Semantic::Analyzer.new(program)
    analyzer.collect_symbols(
      node_file_path_provider: ->(expr_id : Frontend::ExprId) { aggregate.path_for(expr_id) },
      source_for_path_provider: ->(path : String) { shadow_sources[path]? },
    )
    result = analyzer.resolve_names

    greet = analyzer.global_context.symbol_table.lookup("greet")
    greet.should be_a(Semantic::MethodSymbol)
    result.diagnostics.should be_empty
    analyzer.type_inference_diagnostics.should be_empty
  end

  it "expands cross-file named arg with {{name.id}}" do
    aggregate = build_shared_shadow_aggregate([
      <<-CR,
        macro make_method(name)
          def {{name.id}}
            42
          end
        end
      CR
      <<-CR,
        make_method(name: :hello)
        hello()
      CR
    ])
    program = aggregate.program
    shadow_sources = {} of String => String
    aggregate.unit_summaries.each { |u| shadow_sources[u.path] = u.source }

    analyzer = Semantic::Analyzer.new(program)
    analyzer.collect_symbols(
      node_file_path_provider: ->(expr_id : Frontend::ExprId) { aggregate.path_for(expr_id) },
      source_for_path_provider: ->(path : String) { shadow_sources[path]? },
    )
    result = analyzer.resolve_names

    hello = analyzer.global_context.symbol_table.lookup("hello")
    hello.should be_a(Semantic::MethodSymbol)
    result.diagnostics.should be_empty
    analyzer.type_inference_diagnostics.should be_empty
  end

  it "expands cross-file default arg with {{name.id}}" do
    aggregate = build_shared_shadow_aggregate([
      <<-CR,
        macro make_method(name = :fallback)
          def {{name.id}}
            42
          end
        end
      CR
      <<-CR,
        make_method
        fallback()
      CR
    ])
    program = aggregate.program
    shadow_sources = {} of String => String
    aggregate.unit_summaries.each { |u| shadow_sources[u.path] = u.source }

    analyzer = Semantic::Analyzer.new(program)
    analyzer.collect_symbols(
      node_file_path_provider: ->(expr_id : Frontend::ExprId) { aggregate.path_for(expr_id) },
      source_for_path_provider: ->(path : String) { shadow_sources[path]? },
    )
    result = analyzer.resolve_names

    fallback = analyzer.global_context.symbol_table.lookup("fallback")
    fallback.should be_a(Semantic::MethodSymbol)
    result.diagnostics.should be_empty
    analyzer.type_inference_diagnostics.should be_empty
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

  it "attaches generated node ownership back into aggregate units" do
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
    shadow_sources = {} of String => String
    aggregate.unit_summaries.each { |u| shadow_sources[u.path] = u.source }

    analyzer = Semantic::Analyzer.new(program)
    analyzer.collect_symbols(
      node_file_path_provider: ->(expr_id : Frontend::ExprId) { aggregate.path_for(expr_id) },
      source_for_path_provider: ->(path : String) { shadow_sources[path]? },
    )

    generated_indices = analyzer.generated_node_file_paths.keys.sort
    generated_indices.should_not be_empty
    first_generated_id = Frontend::ExprId.new(generated_indices.first)

    aggregate.path_for(first_generated_id).should be_nil

    aggregate.attach_generated_node_paths(analyzer.generated_node_file_paths)

    aggregate.path_for(first_generated_id).should eq("unit_1.cr")
    aggregate.unit_index_for(first_generated_id).should eq(1)
    aggregate.generated_node_count_for_unit(1).should eq(analyzer.generated_node_file_paths.size)
    aggregate.owned_node_count_for_unit(1).should eq(
      aggregate.unit_summaries[1].node_count + analyzer.generated_node_file_paths.size
    )
  end

  it "reports resolution diagnostics inside generated top-level def bodies" do
    aggregate = build_shared_shadow_aggregate([
      <<-CR,
        macro define_bad(name)
          def {{name.id}}
            missing + 1
          end
        end
      CR
      <<-CR,
        define_bad(:alpha)
        alpha()
      CR
    ])
    program = aggregate.program
    shadow_sources = build_shadow_sources(aggregate)

    analyzer = Semantic::Analyzer.new(program)
    analyzer.collect_symbols(
      node_file_path_provider: ->(expr_id : Frontend::ExprId) { aggregate.path_for(expr_id) },
      source_for_path_provider: ->(path : String) { shadow_sources[path]? },
    )
    analyzer.generated_top_level_roots.should_not be_empty
    aggregate.attach_generated_node_paths(analyzer.generated_node_file_paths)

    result = analyzer.resolve_names

    result.diagnostics.size.should eq(1)
    diagnostic = result.diagnostics.first
    diagnostic.message.should contain("undefined local variable or method 'missing'")
    diagnostic.node_id.should_not be_nil
    aggregate.path_for(diagnostic.node_id.not_nil!).should eq("unit_1.cr")
    analyzer.generated_source_for(diagnostic.node_id.not_nil!).not_nil!.should contain("missing + 1")
  end

  it "reports type diagnostics inside generated top-level def bodies" do
    aggregate = build_shared_shadow_aggregate([
      <<-CR,
        macro define_bad(name)
          def {{name.id}}
            1 + "x"
          end
        end
      CR
      <<-CR,
        define_bad(:alpha)
        alpha()
      CR
    ])
    program = aggregate.program
    shadow_sources = build_shadow_sources(aggregate)

    analyzer = Semantic::Analyzer.new(program)
    analyzer.collect_symbols(
      node_file_path_provider: ->(expr_id : Frontend::ExprId) { aggregate.path_for(expr_id) },
      source_for_path_provider: ->(path : String) { shadow_sources[path]? },
    )
    aggregate.attach_generated_node_paths(analyzer.generated_node_file_paths)

    result = analyzer.resolve_names
    analyzer.infer_types(result.identifier_symbols)

    result.diagnostics.should be_empty
    analyzer.type_inference_diagnostics.size.should eq(1)
    diagnostic = analyzer.type_inference_diagnostics.first
    diagnostic.message.should contain("Operator '+' not defined for Int32 and String")
    diagnostic.primary_node_id.should_not be_nil
    aggregate.path_for(diagnostic.primary_node_id.not_nil!).should eq("unit_1.cr")
    analyzer.generated_source_for(diagnostic.primary_node_id.not_nil!).not_nil!.should contain("1 + \"x\"")
  end

  it "formats generated resolution diagnostics against generated source text" do
    aggregate = build_shared_shadow_aggregate([
      <<-CR,
        macro define_bad(name)
          def {{name.id}}
            missing + 1
          end
        end
      CR
      <<-CR,
        define_bad(:alpha)
        alpha()
      CR
    ])
    program = aggregate.program
    shadow_sources = build_shadow_sources(aggregate)

    analyzer = Semantic::Analyzer.new(program)
    analyzer.collect_symbols(
      node_file_path_provider: ->(expr_id : Frontend::ExprId) { aggregate.path_for(expr_id) },
      source_for_path_provider: ->(path : String) { shadow_sources[path]? },
    )
    aggregate.attach_generated_node_paths(analyzer.generated_node_file_paths)
    result = analyzer.resolve_names

    diagnostic = result.diagnostics.first
    node_id = diagnostic.node_id.not_nil!
    generated_source = analyzer.generated_source_for(node_id).not_nil!
    display_path = "#{aggregate.path_for(node_id)} [generated]"
    formatted = Frontend::DiagnosticFormatter.format(
      {display_path => generated_source},
      diagnostic.with_file_path(display_path)
    )

    formatted.should contain("unit_1.cr [generated]:2:")
    formatted.should contain("missing + 1")
    formatted.should_not contain("define_bad(:alpha)")
    origin_node_id = analyzer.generated_origin_for(node_id).not_nil!
    aggregate.path_for(origin_node_id).should eq("unit_1.cr")
  end

  it "formats generated type diagnostics against generated source text" do
    aggregate = build_shared_shadow_aggregate([
      <<-CR,
        macro define_bad(name)
          def {{name.id}}
            1 + "x"
          end
        end
      CR
      <<-CR,
        define_bad(:alpha)
        alpha()
      CR
    ])
    program = aggregate.program
    shadow_sources = build_shadow_sources(aggregate)

    analyzer = Semantic::Analyzer.new(program)
    analyzer.collect_symbols(
      node_file_path_provider: ->(expr_id : Frontend::ExprId) { aggregate.path_for(expr_id) },
      source_for_path_provider: ->(path : String) { shadow_sources[path]? },
    )
    aggregate.attach_generated_node_paths(analyzer.generated_node_file_paths)
    result = analyzer.resolve_names
    analyzer.infer_types(result.identifier_symbols)

    diagnostic = analyzer.type_inference_diagnostics.first
    node_id = diagnostic.primary_node_id.not_nil!
    generated_source = analyzer.generated_source_for(node_id).not_nil!
    display_path = "#{aggregate.path_for(node_id)} [generated]"
    formatted = Semantic::DiagnosticFormatter.format(
      {display_path => generated_source},
      diagnostic.with_paths(display_path)
    )

    formatted.should contain("unit_1.cr [generated]:2:")
    formatted.should contain("1 + \"x\"")
    formatted.should_not contain("define_bad(:alpha)")
    origin_node_id = analyzer.generated_origin_for(node_id).not_nil!
    aggregate.path_for(origin_node_id).should eq("unit_1.cr")
  end

  it "adds origin note for generated resolution diagnostics" do
    aggregate = build_shared_shadow_aggregate([
      <<-CR,
        macro define_bad(name)
          def {{name.id}}
            missing + 1
          end
        end
      CR
      <<-CR,
        define_bad(:alpha)
        alpha()
      CR
    ])
    program = aggregate.program
    shadow_sources = build_shadow_sources(aggregate)

    analyzer = Semantic::Analyzer.new(program)
    analyzer.collect_symbols(
      node_file_path_provider: ->(expr_id : Frontend::ExprId) { aggregate.path_for(expr_id) },
      source_for_path_provider: ->(path : String) { shadow_sources[path]? },
    )
    aggregate.attach_generated_node_paths(analyzer.generated_node_file_paths)
    result = analyzer.resolve_names

    diagnostic = result.diagnostics.first
    node_id = diagnostic.node_id.not_nil!
    generated_source = analyzer.generated_source_for(node_id).not_nil!
    display_path = "#{aggregate.path_for(node_id)} [generated]"
    formatted = Frontend::DiagnosticFormatter.format(
      {display_path => generated_source},
      diagnostic.with_file_path(display_path)
    )
    origin_node_id = analyzer.generated_origin_for(node_id).not_nil!
    origin_path = aggregate.path_for(origin_node_id).not_nil!
    origin_source = shadow_sources[origin_path]
    origin_span = program.arena[origin_node_id].span
    origin_formatted = Frontend::DiagnosticFormatter.format(
      {origin_path => origin_source},
      Frontend::Diagnostic.new("expanded from macro call here", origin_span, origin_node_id, origin_path)
    )

    origin_formatted.should contain("define_bad(:alpha)")
    origin_formatted.should contain("expanded from macro call here")
    formatted.should_not contain("define_bad(:alpha)")
  end

  it "adds origin note for generated type diagnostics" do
    aggregate = build_shared_shadow_aggregate([
      <<-CR,
        macro define_bad(name)
          def {{name.id}}
            1 + "x"
          end
        end
      CR
      <<-CR,
        define_bad(:alpha)
        alpha()
      CR
    ])
    program = aggregate.program
    shadow_sources = build_shadow_sources(aggregate)

    analyzer = Semantic::Analyzer.new(program)
    analyzer.collect_symbols(
      node_file_path_provider: ->(expr_id : Frontend::ExprId) { aggregate.path_for(expr_id) },
      source_for_path_provider: ->(path : String) { shadow_sources[path]? },
    )
    aggregate.attach_generated_node_paths(analyzer.generated_node_file_paths)
    result = analyzer.resolve_names
    analyzer.infer_types(result.identifier_symbols)

    diagnostic = analyzer.type_inference_diagnostics.first
    node_id = diagnostic.primary_node_id.not_nil!
    generated_source = analyzer.generated_source_for(node_id).not_nil!
    display_path = "#{aggregate.path_for(node_id)} [generated]"
    formatted = Semantic::DiagnosticFormatter.format(
      {display_path => generated_source},
      diagnostic.with_paths(display_path)
    )
    origin_node_id = analyzer.generated_origin_for(node_id).not_nil!
    origin_path = aggregate.path_for(origin_node_id).not_nil!
    origin_source = shadow_sources[origin_path]
    origin_span = program.arena[origin_node_id].span
    origin_formatted = Frontend::DiagnosticFormatter.format(
      {origin_path => origin_source},
      Frontend::Diagnostic.new("expanded from macro call here", origin_span, origin_node_id, origin_path)
    )

    origin_formatted.should contain("define_bad(:alpha)")
    origin_formatted.should contain("expanded from macro call here")
    formatted.should_not contain("define_bad(:alpha)")
  end
end
