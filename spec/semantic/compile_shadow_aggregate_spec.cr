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

private def attach_generated_shadow_overlay(
  aggregate : Semantic::CompileShadowAggregate,
  analyzer : Semantic::Analyzer
) : Nil
  aggregate.attach_generated_overlay(analyzer.generated_overlay)
end

describe "compile semantic shadow aggregate" do
  it "exposes per-file source lookup and defensive source snapshots" do
    aggregate = build_shared_shadow_aggregate([
      "def alpha\n  41\nend\n",
      "alpha()\n",
    ])

    aggregate.source_for_path("unit_0.cr").should eq("def alpha\n  41\nend\n")
    aggregate.source_for_path("unit_1.cr").should eq("alpha()\n")
    aggregate.source_for_path("missing.cr").should be_nil

    sources = aggregate.sources_by_path
    sources["unit_0.cr"] = "changed"
    sources.delete("unit_1.cr")

    aggregate.sources_by_path["unit_0.cr"].should eq("def alpha\n  41\nend\n")
    aggregate.sources_by_path["unit_1.cr"].should eq("alpha()\n")
  end

  it "tracks shadow reparse diagnostics per unit" do
    aggregate = build_shared_shadow_aggregate([
      ")\n",
      "alpha()\n",
    ])

    aggregate.unit_summaries[0].parse_diagnostic_count.should eq(1)
    aggregate.unit_summaries[1].parse_diagnostic_count.should eq(0)
    aggregate.parse_diagnostics.size.should eq(1)
    aggregate.parse_diagnostics.first.message.should eq("unexpected RParen")
    aggregate.parse_diagnostics.first.file_path.should eq("unit_0.cr")
    aggregate.diagnostic_counts_by_unit(aggregate.parse_diagnostics).should eq([1, 0])
    aggregate.generated_diagnostic_counts_by_unit(aggregate.parse_diagnostics).should eq([0, 0])
  end

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

  it "enriches frontend diagnostics with aggregate file paths" do
    aggregate = build_shared_shadow_aggregate([
      "def alpha\nend\n",
      "alpha()\n",
    ])
    program = aggregate.program
    call_root = program.roots.last
    call_node = program.arena[call_root].as(Frontend::CallNode)
    callee_id = call_node.callee.not_nil!
    related_span = Frontend::RelatedSpan.new(program.arena[program.roots.first].span, "defined here", program.roots.first, nil)
    diagnostic = Frontend::Diagnostic.new(
      "test",
      program.arena[callee_id].span,
      node_id: callee_id,
      related_spans: [related_span],
    )

    enriched = aggregate.enrich_shadow_diagnostic(diagnostic)

    enriched.file_path.should eq("unit_1.cr")
    enriched.related_spans.first.file_path.should eq("unit_0.cr")
  end

  it "enriches semantic diagnostics with aggregate file paths" do
    aggregate = build_shared_shadow_aggregate([
      "def alpha\nend\n",
      "alpha()\n",
    ])
    program = aggregate.program
    call_root = program.roots.last
    call_node = program.arena[call_root].as(Frontend::CallNode)
    callee_id = call_node.callee.not_nil!
    secondary = Semantic::SecondarySpan.new(program.arena[program.roots.first].span, "defined here", program.roots.first, nil)
    diagnostic = Semantic::Diagnostic.new(
      Semantic::DiagnosticLevel::Error,
      "E9999",
      "test",
      program.arena[callee_id].span,
      [secondary],
      primary_node_id: callee_id,
    )

    enriched = aggregate.enrich_shadow_diagnostic(diagnostic)

    enriched.primary_file_path.should eq("unit_1.cr")
    enriched.secondary_spans.first.file_path.should eq("unit_0.cr")
  end

  it "formats parsed diagnostics without prior manual enrichment" do
    aggregate = build_shared_shadow_aggregate([
      "def alpha\nend\n",
      "alpha()\n",
    ])
    program = aggregate.program
    sources = build_shadow_sources(aggregate)
    call_root = program.roots.last
    call_node = program.arena[call_root].as(Frontend::CallNode)
    callee_id = call_node.callee.not_nil!
    related_span = Frontend::RelatedSpan.new(program.arena[program.roots.first].span, "defined here", program.roots.first, nil)
    diagnostic = Frontend::Diagnostic.new(
      "test",
      program.arena[callee_id].span,
      node_id: callee_id,
      related_spans: [related_span],
    )

    formatted = aggregate.format_shadow_diagnostic(diagnostic, sources)

    formatted.should contain("unit_1.cr:")
    formatted.should contain("note: defined here")
    formatted.should contain("unit_0.cr:")
  end

  it "formats semantic diagnostics without prior manual enrichment" do
    aggregate = build_shared_shadow_aggregate([
      "def alpha\nend\n",
      "alpha()\n",
    ])
    program = aggregate.program
    sources = build_shadow_sources(aggregate)
    call_root = program.roots.last
    call_node = program.arena[call_root].as(Frontend::CallNode)
    callee_id = call_node.callee.not_nil!
    secondary = Semantic::SecondarySpan.new(program.arena[program.roots.first].span, "defined here", program.roots.first, nil)
    diagnostic = Semantic::Diagnostic.new(
      Semantic::DiagnosticLevel::Error,
      "E9999",
      "test",
      program.arena[callee_id].span,
      [secondary],
      primary_node_id: callee_id,
    )

    formatted = aggregate.format_shadow_diagnostic(diagnostic, sources)

    formatted.should contain("unit_1.cr:")
    formatted.should contain("note: defined here")
    formatted.should contain("unit_0.cr:")
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

  it "walks splat children while assigning aggregate ownership" do
    aggregate = build_shared_shadow_aggregate([
      <<-CR,
        def consume(*items)
        end
      CR
      <<-CR,
        source = 1
        consume(*source)
      CR
    ])
    program = aggregate.program

    call_root = program.roots.last
    call_node = program.arena[call_root].as(Frontend::CallNode)
    splat_id = call_node.args.first
    splat_node = program.arena[splat_id].as(Frontend::SplatNode)

    aggregate.path_for(call_root).should eq("unit_1.cr")
    aggregate.path_for(splat_id).should eq("unit_1.cr")
    aggregate.path_for(splat_node.expr).should eq("unit_1.cr")
  end

  it "walks struct bodies while assigning aggregate ownership" do
    aggregate = build_shared_shadow_aggregate([
      <<-CR,
        struct Host
          def answer
            consume path_index
          end
        end
      CR
      "other_call(1)\n"
    ])
    program = aggregate.program

    struct_node = program.arena[program.roots.first].as(Frontend::ClassNode)
    def_id = struct_node.body.not_nil!.first
    def_node = program.arena[def_id].as(Frontend::DefNode)
    call_id = def_node.body.not_nil!.first
    call_node = program.arena[call_id].as(Frontend::CallNode)
    callee_id = call_node.callee.not_nil!

    aggregate.path_for(def_id).should eq("unit_0.cr")
    aggregate.path_for(call_id).should eq("unit_0.cr")
    aggregate.path_for(callee_id).should eq("unit_0.cr")
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
    analyzer.generated_overlay.node_file_paths.values.uniq.should eq(["unit_0.cr"])
    analyzer.generated_overlay.node_file_paths.size.should be > 0
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

  it "preserves nested module namespace for module-body macro expansion" do
    aggregate = build_shared_shadow_aggregate([
      <<-CR,
        class IO
        end

        class File
          class Info
            def self.readable?(path : String) : Bool
              true
            end
          end

          def self.file?(path : String) : Bool
            true
          end

          def self.open(path : String)
            yield IO.new
          end
        end

        class Time
          class Location
            def self.read_zoneinfo(name : String, io : IO) : Location
              new
            end
          end
        end

        module Crystal
          module System
            module Time
              {% if flag?(:android) %}
                def self.load_localtime
                  nil
                end
              {% else %}
                LOCALTIME = "/etc/localtime"

                def self.load_localtime
                  if ::File.file?(LOCALTIME) && ::File::Info.readable?(LOCALTIME)
                    ::File.open(LOCALTIME) do |file|
                      ::Time::Location.read_zoneinfo("Local", file)
                    end
                  end
                end
              {% end %}
            end
          end
        end

        Crystal::System::Time.load_localtime
      CR
    ])
    program = aggregate.program

    analyzer = Semantic::Analyzer.new(program)
    analyzer.collect_symbols(
      node_file_path_provider: ->(expr_id : Frontend::ExprId) { aggregate.path_for(expr_id) },
      source_for_path_provider: ->(path : String) { aggregate.source_for_path(path) },
    )
    result = analyzer.resolve_names
    engine = analyzer.infer_types(result.identifier_symbols)

    result.diagnostics.should be_empty
    analyzer.type_inference_diagnostics.should be_empty

    root_type = engine.context.get_type(program.roots.last)
    root_type.should_not be_nil
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

  it "expands cross-file argful non-method bundle declarations in aggregate" do
    aggregate = build_shared_shadow_aggregate([
      <<-CR,
        macro define_bundle(class_name, module_name, enum_name, const_name)
          class {{class_name.id}}
          end

          module {{module_name.id}}
          end

          enum {{enum_name.id}}
            One
          end

          {{const_name.id}} = 1
        end
      CR
      <<-CR,
        define_bundle(:Alpha, :Beta, :Mode, :FLAG)
      CR
    ])
    program = aggregate.program
    shadow_sources = build_shadow_sources(aggregate)

    analyzer = Semantic::Analyzer.new(program)
    analyzer.collect_symbols(
      node_file_path_provider: ->(expr_id : Frontend::ExprId) { aggregate.path_for(expr_id) },
      source_for_path_provider: ->(path : String) { shadow_sources[path]? },
    )
    attach_generated_shadow_overlay(aggregate, analyzer)
    result = analyzer.resolve_names
    analyzer.infer_types(result.identifier_symbols)

    alpha = analyzer.global_context.symbol_table.lookup("Alpha")
    beta = analyzer.global_context.symbol_table.lookup("Beta")
    mode = analyzer.global_context.symbol_table.lookup("Mode")
    flag = analyzer.global_context.symbol_table.lookup("FLAG")

    alpha.should be_a(Semantic::ClassSymbol)
    beta.should be_a(Semantic::ModuleSymbol)
    mode.should be_a(Semantic::EnumSymbol)
    flag.should be_a(Semantic::ConstantSymbol)

    alpha.not_nil!.file_path.should eq("unit_1.cr")
    beta.not_nil!.file_path.should eq("unit_1.cr")
    mode.not_nil!.file_path.should eq("unit_1.cr")
    flag.not_nil!.file_path.should eq("unit_1.cr")

    result.diagnostics.should be_empty
    analyzer.type_inference_diagnostics.should be_empty
    aggregate.generated_top_level_roots.should_not be_empty
    aggregate.generated_node_count_for_unit(1).should be > 0
    aggregate.owned_node_count_for_unit(1).should be > aggregate.unit_summaries[1].node_count
  end

  it "expands cross-file block-yield non-method bundle declarations in aggregate" do
    aggregate = build_shared_shadow_aggregate([
      <<-CR,
        macro define_bundle
          {{yield}}
        end
      CR
      <<-CR,
        define_bundle do
          class Alpha
          end

          module Beta
          end

          enum Mode
            One
          end

          FLAG = 1
        end
      CR
    ])
    program = aggregate.program
    shadow_sources = build_shadow_sources(aggregate)

    analyzer = Semantic::Analyzer.new(program)
    analyzer.collect_symbols(
      node_file_path_provider: ->(expr_id : Frontend::ExprId) { aggregate.path_for(expr_id) },
      source_for_path_provider: ->(path : String) { shadow_sources[path]? },
    )
    attach_generated_shadow_overlay(aggregate, analyzer)
    result = analyzer.resolve_names
    analyzer.infer_types(result.identifier_symbols)

    alpha = analyzer.global_context.symbol_table.lookup("Alpha")
    beta = analyzer.global_context.symbol_table.lookup("Beta")
    mode = analyzer.global_context.symbol_table.lookup("Mode")
    flag = analyzer.global_context.symbol_table.lookup("FLAG")

    alpha.should be_a(Semantic::ClassSymbol)
    beta.should be_a(Semantic::ModuleSymbol)
    mode.should be_a(Semantic::EnumSymbol)
    flag.should be_a(Semantic::ConstantSymbol)

    alpha.not_nil!.file_path.should eq("unit_1.cr")
    beta.not_nil!.file_path.should eq("unit_1.cr")
    mode.not_nil!.file_path.should eq("unit_1.cr")
    flag.not_nil!.file_path.should eq("unit_1.cr")

    result.diagnostics.should be_empty
    analyzer.type_inference_diagnostics.should be_empty
    aggregate.generated_top_level_roots.should_not be_empty
    aggregate.generated_node_count_for_unit(1).should be > 0
    aggregate.owned_node_count_for_unit(1).should be > aggregate.unit_summaries[1].node_count
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

    generated_indices = analyzer.generated_overlay.node_file_paths.keys.sort
    generated_indices.should_not be_empty
    first_generated_id = Frontend::ExprId.new(generated_indices.first)

    aggregate.path_for(first_generated_id).should be_nil

    attach_generated_shadow_overlay(aggregate, analyzer)

    aggregate.path_for(first_generated_id).should eq("unit_1.cr")
    aggregate.unit_index_for(first_generated_id).should eq(1)
    aggregate.generated_root_count_for_unit(1).should eq(1)
    aggregate.generated_node_count_for_unit(1).should eq(aggregate.generated_node_file_paths.size)
    aggregate.owned_node_count_for_unit(1).should eq(
      aggregate.unit_summaries[1].node_count + aggregate.generated_node_file_paths.size
    )
  end

  it "returns a defensive analyzer generated overlay snapshot" do
    aggregate = build_shared_shadow_aggregate([
      <<-CR,
        macro define_alpha
          def alpha
            42
          end
        end

        define_alpha
      CR
    ])
    program = aggregate.program

    analyzer = Semantic::Analyzer.new(program)
    analyzer.collect_symbols(node_file_path_provider: ->(expr_id : Frontend::ExprId) { aggregate.path_for(expr_id) })

    overlay = analyzer.generated_overlay
    overlay.node_file_paths.clear
    overlay.top_level_roots.clear

    analyzer.generated_overlay.node_file_paths.should_not be_empty
    analyzer.generated_overlay.top_level_roots.should_not be_empty
  end

  it "replaces previous generated overlay ownership on reattach" do
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
    shadow_sources = build_shadow_sources(aggregate)

    analyzer = Semantic::Analyzer.new(program)
    analyzer.collect_symbols(
      node_file_path_provider: ->(expr_id : Frontend::ExprId) { aggregate.path_for(expr_id) },
      source_for_path_provider: ->(path : String) { shadow_sources[path]? },
    )

    generated_indices = analyzer.generated_overlay.node_file_paths.keys.sort
    generated_indices.should_not be_empty
    first_generated_id = Frontend::ExprId.new(generated_indices.first)

    attach_generated_shadow_overlay(aggregate, analyzer)
    aggregate.path_for(first_generated_id).should eq("unit_1.cr")
    aggregate.generated_node_count_for_unit(1).should eq(aggregate.generated_node_file_paths.size)

    empty_overlay = Semantic::GeneratedOverlay.empty
    aggregate.attach_generated_overlay(empty_overlay)

    aggregate.path_for(first_generated_id).should be_nil
    aggregate.unit_index_for(first_generated_id).should be_nil
    aggregate.generated_root_count_for_unit(1).should eq(0)
    aggregate.generated_node_count_for_unit(1).should eq(0)
    aggregate.generated_top_level_roots.should be_empty
    aggregate.generated_node_file_paths.should be_empty
  end

  it "returns a defensive aggregate generated overlay snapshot" do
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
    shadow_sources = build_shadow_sources(aggregate)

    analyzer = Semantic::Analyzer.new(program)
    analyzer.collect_symbols(
      node_file_path_provider: ->(expr_id : Frontend::ExprId) { aggregate.path_for(expr_id) },
      source_for_path_provider: ->(path : String) { shadow_sources[path]? },
    )
    attach_generated_shadow_overlay(aggregate, analyzer)

    overlay = aggregate.generated_overlay
    overlay.node_file_paths.clear
    overlay.top_level_roots.clear

    aggregate.generated_node_file_paths.should_not be_empty
    aggregate.generated_top_level_roots.should_not be_empty
  end

  it "returns defensive generated root and path snapshots" do
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
    shadow_sources = build_shadow_sources(aggregate)

    analyzer = Semantic::Analyzer.new(program)
    analyzer.collect_symbols(
      node_file_path_provider: ->(expr_id : Frontend::ExprId) { aggregate.path_for(expr_id) },
      source_for_path_provider: ->(path : String) { shadow_sources[path]? },
    )
    attach_generated_shadow_overlay(aggregate, analyzer)

    roots = aggregate.generated_top_level_roots
    paths = aggregate.generated_node_file_paths
    roots.clear
    paths.clear

    aggregate.generated_top_level_roots.should_not be_empty
    aggregate.generated_node_file_paths.should_not be_empty
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
    attach_generated_shadow_overlay(aggregate, analyzer)
    aggregate.generated_top_level_roots.should_not be_empty

    result = analyzer.resolve_names

    result.diagnostics.size.should eq(1)
    diagnostic = result.diagnostics.first
    diagnostic.message.should contain("undefined local variable or method 'missing'")
    diagnostic.node_id.should_not be_nil
    aggregate.path_for(diagnostic.node_id.not_nil!).should eq("unit_1.cr")
    aggregate.generated_node?(diagnostic.node_id.not_nil!).should be_true
    aggregate.generated_source_for(diagnostic.node_id.not_nil!).not_nil!.should contain("missing + 1")
  end

  it "exposes generated provenance as a unified aggregate lookup" do
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
    attach_generated_shadow_overlay(aggregate, analyzer)
    result = analyzer.resolve_names

    diagnostic = result.diagnostics.first
    node_id = diagnostic.node_id.not_nil!
    info = aggregate.provenance_for(node_id)

    info.should_not be_nil
    info = info.not_nil!
    info.generated?.should be_true
    info.owning_path.should eq("unit_1.cr")
    info.generated_source.not_nil!.should contain("missing + 1")
    info.origin_call_path.should eq("unit_1.cr")
    info.origin_macro_def_path.should eq("unit_0.cr")
  end

  it "exposes parsed provenance without generated metadata" do
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
    call_root = program.roots.last
    call_node = program.arena[call_root].as(Frontend::CallNode)
    callee_id = call_node.callee.not_nil!

    info = aggregate.provenance_for(callee_id).not_nil!

    info.generated?.should be_false
    info.owning_path.should eq("unit_1.cr")
    info.generated_source.should be_nil
    info.origin_call_path.should be_nil
    info.origin_macro_def_path.should be_nil

    diagnostic = Frontend::Diagnostic.new(
      "test",
      program.arena[callee_id].span,
      node_id: callee_id,
    )
    aggregate.generated_shadow_diagnostic?(diagnostic).should be_false
    aggregate.diagnostic_provenance_context_for(diagnostic).should be_nil
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
    attach_generated_shadow_overlay(aggregate, analyzer)

    result = analyzer.resolve_names
    analyzer.infer_types(result.identifier_symbols)

    result.diagnostics.should be_empty
    analyzer.type_inference_diagnostics.size.should eq(1)
    diagnostic = analyzer.type_inference_diagnostics.first
    diagnostic.message.should contain("Operator '+' not defined for Int32 and String")
    diagnostic.primary_node_id.should_not be_nil
    aggregate.path_for(diagnostic.primary_node_id.not_nil!).should eq("unit_1.cr")
    aggregate.generated_node?(diagnostic.primary_node_id.not_nil!).should be_true
    aggregate.generated_source_for(diagnostic.primary_node_id.not_nil!).not_nil!.should contain("1 + \"x\"")
    aggregate.diagnostic_counts_by_unit(analyzer.type_inference_diagnostics).should eq([0, 1])
    aggregate.generated_diagnostic_counts_by_unit(analyzer.type_inference_diagnostics).should eq([0, 1])
  end

  it "reports resolution diagnostics inside generated top-level class bodies" do
    aggregate = build_shared_shadow_aggregate([
      <<-CR,
        macro define_bad_class
          class BadBox
            def self.call
              missing + 1
            end
          end
        end
      CR
      <<-CR,
        define_bad_class
        BadBox.call
      CR
    ])
    program = aggregate.program
    shadow_sources = build_shadow_sources(aggregate)

    analyzer = Semantic::Analyzer.new(program)
    analyzer.collect_symbols(
      node_file_path_provider: ->(expr_id : Frontend::ExprId) { aggregate.path_for(expr_id) },
      source_for_path_provider: ->(path : String) { shadow_sources[path]? },
    )
    attach_generated_shadow_overlay(aggregate, analyzer)
    aggregate.generated_top_level_roots.should_not be_empty

    result = analyzer.resolve_names

    result.diagnostics.size.should eq(1)
    diagnostic = result.diagnostics.first
    diagnostic.message.should contain("undefined local variable or method 'missing'")
    diagnostic.node_id.should_not be_nil
    aggregate.path_for(diagnostic.node_id.not_nil!).should eq("unit_1.cr")
    aggregate.generated_node?(diagnostic.node_id.not_nil!).should be_true
    generated_source = aggregate.generated_source_for(diagnostic.node_id.not_nil!).not_nil!
    generated_source.should contain("class BadBox")
    generated_source.should contain("missing + 1")
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
    attach_generated_shadow_overlay(aggregate, analyzer)
    result = analyzer.resolve_names

    diagnostic = result.diagnostics.first
    node_id = diagnostic.node_id.not_nil!
    generated_source = aggregate.generated_source_for(node_id).not_nil!
    display_path = "#{aggregate.path_for(node_id)} [generated]"
    formatted = Frontend::DiagnosticFormatter.format(
      {display_path => generated_source},
      diagnostic.with_file_path(display_path)
    )

    formatted.should contain("unit_1.cr [generated]:2:")
    formatted.should contain("missing + 1")
    formatted.should_not contain("define_bad(:alpha)")
    origin_node_id = aggregate.generated_origin_for(node_id).not_nil!
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
    attach_generated_shadow_overlay(aggregate, analyzer)
    result = analyzer.resolve_names
    analyzer.infer_types(result.identifier_symbols)

    diagnostic = analyzer.type_inference_diagnostics.first
    node_id = diagnostic.primary_node_id.not_nil!
    generated_source = aggregate.generated_source_for(node_id).not_nil!
    display_path = "#{aggregate.path_for(node_id)} [generated]"
    formatted = Semantic::DiagnosticFormatter.format(
      {display_path => generated_source},
      diagnostic.with_paths(display_path)
    )

    formatted.should contain("unit_1.cr [generated]:2:")
    formatted.should contain("1 + \"x\"")
    formatted.should_not contain("define_bad(:alpha)")
    origin_node_id = aggregate.generated_origin_for(node_id).not_nil!
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
    attach_generated_shadow_overlay(aggregate, analyzer)
    result = analyzer.resolve_names

    diagnostic = result.diagnostics.first
    aggregate.generated_shadow_diagnostic?(diagnostic).should be_true
    context = aggregate.diagnostic_provenance_context_for(diagnostic).not_nil!
    formatted = aggregate.format_shadow_diagnostic(diagnostic, {"unit_1.cr" => shadow_sources["unit_1.cr"]})

    formatted.should contain("note: expanded from macro call here")
    formatted.should contain("define_bad(:alpha)")
  end

  it "builds a generated source map overlay from generated diagnostic context" do
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
    attach_generated_shadow_overlay(aggregate, analyzer)
    result = analyzer.resolve_names

    diagnostic = result.diagnostics.first
    context = aggregate.diagnostic_provenance_context_for(diagnostic).not_nil!
    source_map = context.sources_with_generated({"unit_1.cr" => shadow_sources["unit_1.cr"]})

    source_map["unit_1.cr"].should eq(shadow_sources["unit_1.cr"])
    source_map[context.display_path.not_nil!].should contain("missing + 1")
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
    attach_generated_shadow_overlay(aggregate, analyzer)
    result = analyzer.resolve_names
    analyzer.infer_types(result.identifier_symbols)

    diagnostic = analyzer.type_inference_diagnostics.first
    aggregate.generated_shadow_diagnostic?(diagnostic).should be_true
    context = aggregate.diagnostic_provenance_context_for(diagnostic).not_nil!
    formatted = aggregate.format_shadow_diagnostic(diagnostic, {"unit_1.cr" => shadow_sources["unit_1.cr"]})

    formatted.should contain("note: expanded from macro call here")
    formatted.should contain("define_bad(:alpha)")
  end

  it "adds macro definition note for cross-file generated resolution diagnostics" do
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
    attach_generated_shadow_overlay(aggregate, analyzer)
    result = analyzer.resolve_names

    diagnostic = result.diagnostics.first
    context = aggregate.diagnostic_provenance_context_for(diagnostic).not_nil!
    formatted = aggregate.format_shadow_diagnostic(
      diagnostic,
      {"unit_0.cr" => shadow_sources["unit_0.cr"], "unit_1.cr" => shadow_sources["unit_1.cr"]}
    )

    formatted.should contain("note: macro defined here")
    formatted.should contain("macro define_bad(name)")
  end

  it "adds macro definition note for cross-file generated type diagnostics" do
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
    attach_generated_shadow_overlay(aggregate, analyzer)
    result = analyzer.resolve_names
    analyzer.infer_types(result.identifier_symbols)

    diagnostic = analyzer.type_inference_diagnostics.first
    context = aggregate.diagnostic_provenance_context_for(diagnostic).not_nil!
    formatted = aggregate.format_shadow_diagnostic(
      diagnostic,
      {"unit_0.cr" => shadow_sources["unit_0.cr"], "unit_1.cr" => shadow_sources["unit_1.cr"]}
    )

    formatted.should contain("note: macro defined here")
    formatted.should contain("macro define_bad(name)")
  end
end
