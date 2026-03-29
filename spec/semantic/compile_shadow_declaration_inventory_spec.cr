require "spec"

require "../../src/compiler/frontend/ast"
require "../../src/compiler/frontend/lexer"
require "../../src/compiler/frontend/parser"
require "../../src/compiler/semantic/analyzer"
require "../../src/compiler/semantic/compile_shadow_aggregate"
require "../../src/compiler/semantic/compile_shadow_declaration_inventory"

module CompileShadowDeclarationInventorySpecAliases
  alias Frontend = CrystalV2::Compiler::Frontend
  alias Semantic = CrystalV2::Compiler::Semantic
end

include CompileShadowDeclarationInventorySpecAliases

private def build_declaration_shadow_program(sources : Array(String)) : Frontend::Program
  units = [] of NamedTuple(path: String, source: String)
  sources.each_with_index do |source, index|
    units << {path: "decl_#{index}.cr", source: source}
  end
  Semantic::CompileShadowAggregate.build(units).program
end

describe "compile shadow declaration inventory" do
  it "collects top-level declarations from aggregate roots" do
    program = build_declaration_shadow_program([
      <<-CR,
        macro trace
        end

        class Box
        end

        module Util
        end

        enum Color
          Red
        end

        VALUE = 1

        def greet
        end
      CR
    ])

    inventory = Semantic::CompileShadowDeclarationInventory.from_program(program)

    inventory.total(Semantic::CompileShadowDeclarationKind::Macros).should eq(1)
    inventory.total(Semantic::CompileShadowDeclarationKind::Classes).should eq(1)
    inventory.total(Semantic::CompileShadowDeclarationKind::Modules).should eq(1)
    inventory.total(Semantic::CompileShadowDeclarationKind::Enums).should eq(1)
    inventory.total(Semantic::CompileShadowDeclarationKind::Constants).should eq(1)
    inventory.total(Semantic::CompileShadowDeclarationKind::Methods).should eq(1)
  end

  it "matches semantic top-level inventory for simple declarations" do
    program = build_declaration_shadow_program([
      <<-CR,
        macro trace
        end

        class Box
        end

        module Util
        end

        enum Color
          Red
        end

        FOO = 1

        def greet
        end
      CR
    ])

    analyzer = Semantic::Analyzer.new(program)
    analyzer.collect_symbols

    parse_inventory = Semantic::CompileShadowDeclarationInventory.from_program(program)
    semantic_inventory = Semantic::CompileShadowDeclarationInventory.from_symbol_table(analyzer.global_context.symbol_table)
    parity = Semantic::CompileShadowDeclarationParity.compare(parse_inventory, semantic_inventory)

    parity.gap_count.should eq(0)
  end

  it "materializes top-level macro-generated methods in semantic inventory" do
    program = build_declaration_shadow_program([
      <<-CR,
        def direct_greet
        end

        {% for name in %w(alpha beta) %}
          def {{name.id}}
          end
        {% end %}
      CR
    ])

    analyzer = Semantic::Analyzer.new(program)
    analyzer.collect_symbols

    semantic_inventory = Semantic::CompileShadowDeclarationInventory.from_symbol_table(analyzer.global_context.symbol_table)

    semantic_inventory.total(Semantic::CompileShadowDeclarationKind::Methods).should eq(3)
    semantic_inventory.unique_names(Semantic::CompileShadowDeclarationKind::Methods).should eq(["alpha", "beta", "direct_greet"])
  end

  it "materializes top-level macro-call-generated methods in semantic inventory" do
    program = build_declaration_shadow_program([
      <<-CR,
        macro define_alpha
          def alpha
          end
        end

        define_alpha
      CR
    ])

    analyzer = Semantic::Analyzer.new(program)
    analyzer.collect_symbols

    semantic_inventory = Semantic::CompileShadowDeclarationInventory.from_symbol_table(analyzer.global_context.symbol_table)

    semantic_inventory.total(Semantic::CompileShadowDeclarationKind::Methods).should eq(1)
    semantic_inventory.unique_names(Semantic::CompileShadowDeclarationKind::Methods).should eq(["alpha"])
    semantic_inventory.total(Semantic::CompileShadowDeclarationKind::Macros).should eq(1)
  end

  it "keeps zero name gaps while preserving total-count differences for overload families" do
    program = build_declaration_shadow_program([
      <<-CR,
        def greet
        end

        def greet(x : Int32)
        end
      CR
    ])

    analyzer = Semantic::Analyzer.new(program)
    analyzer.collect_symbols

    parse_inventory = Semantic::CompileShadowDeclarationInventory.from_program(program)
    semantic_inventory = Semantic::CompileShadowDeclarationInventory.from_symbol_table(analyzer.global_context.symbol_table)
    parity = Semantic::CompileShadowDeclarationParity.compare(parse_inventory, semantic_inventory)

    parity.gap_count.should eq(0)
    method_line = parity.summary_lines.find { |line| line.starts_with?("methods ") }
    method_line.should_not be_nil
    method_line.not_nil!.should contain("parse_total=2")
    method_line.not_nil!.should contain("parse_unique=1")
    method_line.not_nil!.should contain("semantic_total=1")
    method_line.not_nil!.should contain("semantic_unique=1")
  end

  it "supports custom labels for parity summaries" do
    left = Semantic::CompileShadowDeclarationInventory.new
    right = Semantic::CompileShadowDeclarationInventory.new
    left.record(Semantic::CompileShadowDeclarationKind::Methods, "greet")
    right.record(Semantic::CompileShadowDeclarationKind::Methods, "greet")

    parity = Semantic::CompileShadowDeclarationParity.compare(left, right)
    method_line = parity.summary_lines(5, "collector", "semantic").find { |line| line.starts_with?("methods ") }

    method_line.should_not be_nil
    method_line.not_nil!.should contain("collector_total=1")
    method_line.not_nil!.should contain("collector_unique=1")
    method_line.not_nil!.should contain("semantic_total=1")
    method_line.not_nil!.should contain("semantic_unique=1")
  end

  it "reports direct vs macro-expanded provenance counts" do
    inventory = Semantic::CompileShadowDeclarationInventory.new
    inventory.record(
      Semantic::CompileShadowDeclarationKind::Methods,
      "direct_greet",
      Semantic::CompileShadowDeclarationOrigin::Direct
    )
    inventory.record(
      Semantic::CompileShadowDeclarationKind::Methods,
      "generated_greet",
      Semantic::CompileShadowDeclarationOrigin::MacroExpanded
    )
    inventory.record(
      Semantic::CompileShadowDeclarationKind::Methods,
      "generated_greet",
      Semantic::CompileShadowDeclarationOrigin::MacroExpanded
    )

    method_line = inventory.provenance_lines("collector").find { |line| line.starts_with?("methods provenance ") }

    method_line.should_not be_nil
    method_line.not_nil!.should contain("collector_direct_total=1")
    method_line.not_nil!.should contain("collector_direct_unique=1")
    method_line.not_nil!.should contain("collector_macro_expanded_total=2")
    method_line.not_nil!.should contain("collector_macro_expanded_unique=1")
  end

  it "classifies missing semantic declarations by collector origin" do
    collector = Semantic::CompileShadowDeclarationInventory.new
    semantic = Semantic::CompileShadowDeclarationInventory.new

    collector.record(
      Semantic::CompileShadowDeclarationKind::Methods,
      "direct_greet",
      Semantic::CompileShadowDeclarationOrigin::Direct
    )
    collector.record(
      Semantic::CompileShadowDeclarationKind::Methods,
      "generated_alpha",
      Semantic::CompileShadowDeclarationOrigin::MacroExpanded
    )
    collector.record(
      Semantic::CompileShadowDeclarationKind::Methods,
      "generated_beta",
      Semantic::CompileShadowDeclarationOrigin::MacroExpanded
    )
    semantic.record(
      Semantic::CompileShadowDeclarationKind::Methods,
      "direct_greet"
    )

    parity = Semantic::CompileShadowDeclarationParity.compare(collector, semantic)
    lines = parity.summary_lines(5, "collector", "semantic")

    lines.should contain("  missing_macro_expanded_in_semantic=generated_alpha, generated_beta")
    lines.none? { |line| line.includes?("missing_direct_in_semantic") }.should be_true
  end

  it "records semantic provenance for direct and macro-expanded declarations" do
    aggregate = Semantic::CompileShadowAggregate.build([
      {
        path: "decl_lib.cr",
        source: <<-CR,
          macro define_alpha(name)
            def {{name.id}}
            end
          end
        CR
      },
      {
        path: "decl_main.cr",
        source: <<-CR,
          require "./decl_lib"

          def direct_greet
          end

          define_alpha(:alpha)
        CR
      },
    ])
    program = aggregate.program
    shadow_sources = {} of String => String
    aggregate.unit_summaries.each { |u| shadow_sources[u.path] = u.source }

    analyzer = Semantic::Analyzer.new(program)
    analyzer.collect_symbols(
      node_file_path_provider: ->(expr_id : Frontend::ExprId) { aggregate.path_for(expr_id) },
      source_for_path_provider: ->(path : String) { shadow_sources[path]? },
    )

    alpha_symbol = analyzer.global_context.symbol_table.lookup_local("alpha")
    alpha_symbol.should be_a(Semantic::MethodSymbol)
    alpha_symbol = alpha_symbol.as(Semantic::MethodSymbol)
    alpha_symbol.generated?.should be_true
    alpha_symbol.generated_origin_node_id.should_not be_nil
    alpha_symbol.generated_macro_definition_node_id.should_not be_nil
    alpha_symbol.file_path.should eq("decl_main.cr")

    semantic_inventory = Semantic::CompileShadowDeclarationInventory.from_symbol_table(
      analyzer.global_context.symbol_table
    )

    method_line = semantic_inventory.provenance_lines("semantic").find { |line| line.starts_with?("methods provenance ") }

    method_line.should_not be_nil
    method_line.not_nil!.should contain("semantic_direct_total=1")
    method_line.not_nil!.should contain("semantic_direct_unique=1")
    method_line.not_nil!.should contain("semantic_macro_expanded_total=1")
    method_line.not_nil!.should contain("semantic_macro_expanded_unique=1")
  end

  it "marks overload families as generated when a macro-expanded overload is present" do
    aggregate = Semantic::CompileShadowAggregate.build([
      {
        path: "decl_main.cr",
        source: <<-CR,
          def greet
          end

          macro define_greet(name)
            def greet(value : {{name.id}})
            end
          end

          define_greet(:Int32)
        CR
      },
    ])
    program = aggregate.program
    shadow_sources = {} of String => String
    aggregate.unit_summaries.each { |u| shadow_sources[u.path] = u.source }

    analyzer = Semantic::Analyzer.new(program)
    analyzer.collect_symbols(
      node_file_path_provider: ->(expr_id : Frontend::ExprId) { aggregate.path_for(expr_id) },
      source_for_path_provider: ->(path : String) { shadow_sources[path]? },
    )

    greet_symbol = analyzer.global_context.symbol_table.lookup_local("greet")
    greet_symbol.should be_a(Semantic::OverloadSetSymbol)
    greet_symbol = greet_symbol.as(Semantic::OverloadSetSymbol)
    greet_symbol.generated?.should be_true
    greet_symbol.overloads.size.should eq(2)
    greet_symbol.overloads.count(&.generated?).should eq(1)

    semantic_inventory = Semantic::CompileShadowDeclarationInventory.from_symbol_table(
      analyzer.global_context.symbol_table
    )
    method_line = semantic_inventory.provenance_lines("semantic").find { |line| line.starts_with?("methods provenance ") }

    method_line.should_not be_nil
    method_line.not_nil!.should contain("semantic_direct_total=1")
    method_line.not_nil!.should contain("semantic_macro_expanded_total=1")
  end
end
