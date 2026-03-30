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

  it "retains generated provenance for cross-file argful non-method macro-call bundles" do
    aggregate = Semantic::CompileShadowAggregate.build([
      {
        path: "decl_lib.cr",
        source: <<-CR,
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
      },
      {
        path: "decl_main.cr",
        source: <<-CR,
          require "./decl_lib"
          define_bundle(:Alpha, :Beta, :Mode, :FLAG)
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

    semantic_inventory = Semantic::CompileShadowDeclarationInventory.from_symbol_table(analyzer.global_context.symbol_table)

    alpha_symbol = analyzer.global_context.symbol_table.lookup_local("Alpha")
    alpha_symbol.should be_a(Semantic::ClassSymbol)
    alpha_symbol = alpha_symbol.as(Semantic::ClassSymbol)
    alpha_symbol.generated?.should be_true
    alpha_symbol.generated_origin_node_id.should_not be_nil
    alpha_symbol.generated_macro_definition_node_id.should_not be_nil
    alpha_symbol.file_path.should eq("decl_main.cr")

    beta_symbol = analyzer.global_context.symbol_table.lookup_local("Beta")
    beta_symbol.should be_a(Semantic::ModuleSymbol)
    beta_symbol = beta_symbol.as(Semantic::ModuleSymbol)
    beta_symbol.generated?.should be_true
    beta_symbol.generated_origin_node_id.should_not be_nil
    beta_symbol.generated_macro_definition_node_id.should_not be_nil
    beta_symbol.file_path.should eq("decl_main.cr")

    mode_symbol = analyzer.global_context.symbol_table.lookup_local("Mode")
    mode_symbol.should be_a(Semantic::EnumSymbol)
    mode_symbol = mode_symbol.as(Semantic::EnumSymbol)
    mode_symbol.generated?.should be_true
    mode_symbol.generated_origin_node_id.should_not be_nil
    mode_symbol.generated_macro_definition_node_id.should_not be_nil
    mode_symbol.file_path.should eq("decl_main.cr")

    flag_symbol = analyzer.global_context.symbol_table.lookup_local("FLAG")
    flag_symbol.should be_a(Semantic::ConstantSymbol)
    flag_symbol = flag_symbol.as(Semantic::ConstantSymbol)
    flag_symbol.generated?.should be_true
    flag_symbol.generated_origin_node_id.should_not be_nil
    flag_symbol.generated_macro_definition_node_id.should_not be_nil
    flag_symbol.file_path.should eq("decl_main.cr")

    semantic_inventory.unique_names(Semantic::CompileShadowDeclarationKind::Classes).should eq(["Alpha"])
    semantic_inventory.unique_names(Semantic::CompileShadowDeclarationKind::Modules).should eq(["Beta"])
    semantic_inventory.unique_names(Semantic::CompileShadowDeclarationKind::Enums).should eq(["Mode"])
    semantic_inventory.unique_names(Semantic::CompileShadowDeclarationKind::Constants).should eq(["FLAG"])

    class_line = semantic_inventory.provenance_lines("semantic").find { |line| line.starts_with?("classes provenance ") }
    module_line = semantic_inventory.provenance_lines("semantic").find { |line| line.starts_with?("modules provenance ") }
    enum_line = semantic_inventory.provenance_lines("semantic").find { |line| line.starts_with?("enums provenance ") }
    constant_line = semantic_inventory.provenance_lines("semantic").find { |line| line.starts_with?("constants provenance ") }

    class_line.should_not be_nil
    module_line.should_not be_nil
    enum_line.should_not be_nil
    constant_line.should_not be_nil
    class_line.not_nil!.should contain("semantic_macro_expanded_total=1")
    module_line.not_nil!.should contain("semantic_macro_expanded_total=1")
    enum_line.not_nil!.should contain("semantic_macro_expanded_total=1")
    constant_line.not_nil!.should contain("semantic_macro_expanded_total=1")
  end

  it "retains generated provenance for cross-file block-yield non-method macro-call bundles" do
    aggregate = Semantic::CompileShadowAggregate.build([
      {
        path: "decl_lib.cr",
        source: <<-CR,
          macro define_bundle
            {{yield}}
          end
        CR
      },
      {
        path: "decl_main.cr",
        source: <<-CR,
          require "./decl_lib"
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

    semantic_inventory = Semantic::CompileShadowDeclarationInventory.from_symbol_table(analyzer.global_context.symbol_table)

    alpha_symbol = analyzer.global_context.symbol_table.lookup_local("Alpha")
    alpha_symbol.should be_a(Semantic::ClassSymbol)
    alpha_symbol = alpha_symbol.as(Semantic::ClassSymbol)
    alpha_symbol.generated?.should be_true
    alpha_symbol.generated_origin_node_id.should_not be_nil
    alpha_symbol.generated_macro_definition_node_id.should_not be_nil
    alpha_symbol.file_path.should eq("decl_main.cr")

    beta_symbol = analyzer.global_context.symbol_table.lookup_local("Beta")
    beta_symbol.should be_a(Semantic::ModuleSymbol)
    beta_symbol = beta_symbol.as(Semantic::ModuleSymbol)
    beta_symbol.generated?.should be_true
    beta_symbol.generated_origin_node_id.should_not be_nil
    beta_symbol.generated_macro_definition_node_id.should_not be_nil
    beta_symbol.file_path.should eq("decl_main.cr")

    mode_symbol = analyzer.global_context.symbol_table.lookup_local("Mode")
    mode_symbol.should be_a(Semantic::EnumSymbol)
    mode_symbol = mode_symbol.as(Semantic::EnumSymbol)
    mode_symbol.generated?.should be_true
    mode_symbol.generated_origin_node_id.should_not be_nil
    mode_symbol.generated_macro_definition_node_id.should_not be_nil
    mode_symbol.file_path.should eq("decl_main.cr")

    flag_symbol = analyzer.global_context.symbol_table.lookup_local("FLAG")
    flag_symbol.should be_a(Semantic::ConstantSymbol)
    flag_symbol = flag_symbol.as(Semantic::ConstantSymbol)
    flag_symbol.generated?.should be_true
    flag_symbol.generated_origin_node_id.should_not be_nil
    flag_symbol.generated_macro_definition_node_id.should_not be_nil
    flag_symbol.file_path.should eq("decl_main.cr")

    semantic_inventory.unique_names(Semantic::CompileShadowDeclarationKind::Classes).should eq(["Alpha"])
    semantic_inventory.unique_names(Semantic::CompileShadowDeclarationKind::Modules).should eq(["Beta"])
    semantic_inventory.unique_names(Semantic::CompileShadowDeclarationKind::Enums).should eq(["Mode"])
    semantic_inventory.unique_names(Semantic::CompileShadowDeclarationKind::Constants).should eq(["FLAG"])

    class_line = semantic_inventory.provenance_lines("semantic").find { |line| line.starts_with?("classes provenance ") }
    module_line = semantic_inventory.provenance_lines("semantic").find { |line| line.starts_with?("modules provenance ") }
    enum_line = semantic_inventory.provenance_lines("semantic").find { |line| line.starts_with?("enums provenance ") }
    constant_line = semantic_inventory.provenance_lines("semantic").find { |line| line.starts_with?("constants provenance ") }

    class_line.should_not be_nil
    module_line.should_not be_nil
    enum_line.should_not be_nil
    constant_line.should_not be_nil
    class_line.not_nil!.should contain("semantic_macro_expanded_total=1")
    module_line.not_nil!.should contain("semantic_macro_expanded_total=1")
    enum_line.not_nil!.should contain("semantic_macro_expanded_total=1")
    constant_line.not_nil!.should contain("semantic_macro_expanded_total=1")
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

  it "builds a strict-mode message only when declaration gaps exist" do
    collector = Semantic::CompileShadowDeclarationInventory.new
    semantic = Semantic::CompileShadowDeclarationInventory.new

    collector.record(
      Semantic::CompileShadowDeclarationKind::Methods,
      "generated_alpha",
      Semantic::CompileShadowDeclarationOrigin::MacroExpanded
    )

    parity = Semantic::CompileShadowDeclarationParity.compare(collector, semantic)

    strict_message = parity.strict_message("collector", "semantic")
    strict_message.should_not be_nil
    strict_message = strict_message.not_nil!
    strict_message.should contain("semantic shadow strict declaration mismatch")
    strict_message.should contain("collector_total=1")
    strict_message.should contain("semantic_total=0")
    strict_message.should contain("missing_in_semantic=generated_alpha")

    matching = Semantic::CompileShadowDeclarationParity.compare(collector, collector)
    matching.strict_message("collector", "semantic").should be_nil
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

  it "retains generated provenance for macro-expanded classes modules enums and constants" do
    aggregate = Semantic::CompileShadowAggregate.build([
      {
        path: "decl_lib.cr",
        source: <<-CR,
          macro define_bundle
            class Alpha
            end

            module Beta
            end

            enum Delta
              One
            end

            GAMMA = 1
          end
        CR
      },
      {
        path: "decl_main.cr",
        source: <<-CR,
          require "./decl_lib"
          define_bundle
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

    alpha_symbol = analyzer.global_context.symbol_table.lookup_local("Alpha")
    alpha_symbol.should be_a(Semantic::ClassSymbol)
    alpha_symbol = alpha_symbol.as(Semantic::ClassSymbol)
    alpha_symbol.generated?.should be_true
    alpha_symbol.generated_origin_node_id.should_not be_nil
    alpha_symbol.generated_macro_definition_node_id.should_not be_nil
    alpha_symbol.file_path.should eq("decl_main.cr")

    beta_symbol = analyzer.global_context.symbol_table.lookup_local("Beta")
    beta_symbol.should be_a(Semantic::ModuleSymbol)
    beta_symbol = beta_symbol.as(Semantic::ModuleSymbol)
    beta_symbol.generated?.should be_true
    beta_symbol.generated_origin_node_id.should_not be_nil
    beta_symbol.generated_macro_definition_node_id.should_not be_nil
    beta_symbol.file_path.should eq("decl_main.cr")

    delta_symbol = analyzer.global_context.symbol_table.lookup_local("Delta")
    delta_symbol.should be_a(Semantic::EnumSymbol)
    delta_symbol = delta_symbol.as(Semantic::EnumSymbol)
    delta_symbol.generated?.should be_true
    delta_symbol.generated_origin_node_id.should_not be_nil
    delta_symbol.generated_macro_definition_node_id.should_not be_nil
    delta_symbol.file_path.should eq("decl_main.cr")

    gamma_symbol = analyzer.global_context.symbol_table.lookup_local("GAMMA")
    gamma_symbol.should be_a(Semantic::ConstantSymbol)
    gamma_symbol = gamma_symbol.as(Semantic::ConstantSymbol)
    gamma_symbol.generated?.should be_true
    gamma_symbol.generated_origin_node_id.should_not be_nil
    gamma_symbol.generated_macro_definition_node_id.should_not be_nil
    gamma_symbol.file_path.should eq("decl_main.cr")

    semantic_inventory = Semantic::CompileShadowDeclarationInventory.from_symbol_table(
      analyzer.global_context.symbol_table
    )

    class_line = semantic_inventory.provenance_lines("semantic").find { |line| line.starts_with?("classes provenance ") }
    module_line = semantic_inventory.provenance_lines("semantic").find { |line| line.starts_with?("modules provenance ") }
    enum_line = semantic_inventory.provenance_lines("semantic").find { |line| line.starts_with?("enums provenance ") }
    constant_line = semantic_inventory.provenance_lines("semantic").find { |line| line.starts_with?("constants provenance ") }

    class_line.should_not be_nil
    module_line.should_not be_nil
    enum_line.should_not be_nil
    constant_line.should_not be_nil
    class_line.not_nil!.should contain("semantic_macro_expanded_total=1")
    module_line.not_nil!.should contain("semantic_macro_expanded_total=1")
    enum_line.not_nil!.should contain("semantic_macro_expanded_total=1")
    constant_line.not_nil!.should contain("semantic_macro_expanded_total=1")
  end

  it "retains generated provenance for macro-expanded macro definitions" do
    aggregate = Semantic::CompileShadowAggregate.build([
      {
        path: "decl_lib.cr",
        source: <<-CR,
          macro define_trace
            macro generated_trace
            end
          end
        CR
      },
      {
        path: "decl_main.cr",
        source: <<-CR,
          require "./decl_lib"
          define_trace
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

    generated_trace_symbol = analyzer.global_context.symbol_table.lookup_local("generated_trace")
    generated_trace_symbol.should be_a(Semantic::MacroSymbol)
    generated_trace_symbol = generated_trace_symbol.as(Semantic::MacroSymbol)
    generated_trace_symbol.generated?.should be_true
    generated_trace_symbol.generated_origin_node_id.should_not be_nil
    generated_trace_symbol.generated_macro_definition_node_id.should_not be_nil
    generated_trace_symbol.file_path.should eq("decl_main.cr")

    semantic_inventory = Semantic::CompileShadowDeclarationInventory.from_symbol_table(
      analyzer.global_context.symbol_table
    )
    macro_line = semantic_inventory.provenance_lines("semantic").find { |line| line.starts_with?("macros provenance ") }

    macro_line.should_not be_nil
    macro_line.not_nil!.should contain("semantic_direct_total=1")
    macro_line.not_nil!.should contain("semantic_macro_expanded_total=1")
  end

  it "retains both direct and generated provenance across class reopenings" do
    aggregate = Semantic::CompileShadowAggregate.build([
      {
        path: "decl_lib.cr",
        source: <<-CR,
          macro define_alpha
            class Alpha
            end
          end
        CR
      },
      {
        path: "decl_main.cr",
        source: <<-CR,
          require "./decl_lib"
          define_alpha

          class Alpha
            def self.extra
            end
          end
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

    alpha_symbol = analyzer.global_context.symbol_table.lookup_local("Alpha")
    alpha_symbol.should be_a(Semantic::ClassSymbol)
    alpha_symbol = alpha_symbol.as(Semantic::ClassSymbol)
    alpha_symbol.direct_declaration_origin?.should be_true
    alpha_symbol.generated_declaration_origin?.should be_true

    semantic_inventory = Semantic::CompileShadowDeclarationInventory.from_symbol_table(
      analyzer.global_context.symbol_table
    )
    class_line = semantic_inventory.provenance_lines("semantic").find { |line| line.starts_with?("classes provenance ") }

    class_line.should_not be_nil
    class_line.not_nil!.should contain("semantic_direct_total=1")
    class_line.not_nil!.should contain("semantic_macro_expanded_total=1")
  end

  it "retains both direct and generated provenance across module reopenings" do
    aggregate = Semantic::CompileShadowAggregate.build([
      {
        path: "decl_lib.cr",
        source: <<-CR,
          macro define_alpha
            module Alpha
            end
          end
        CR
      },
      {
        path: "decl_main.cr",
        source: <<-CR,
          require "./decl_lib"
          define_alpha

          module Alpha
            VALUE = 1
          end
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

    alpha_symbol = analyzer.global_context.symbol_table.lookup_local("Alpha")
    alpha_symbol.should be_a(Semantic::ModuleSymbol)
    alpha_symbol = alpha_symbol.as(Semantic::ModuleSymbol)
    alpha_symbol.direct_declaration_origin?.should be_true
    alpha_symbol.generated_declaration_origin?.should be_true

    semantic_inventory = Semantic::CompileShadowDeclarationInventory.from_symbol_table(
      analyzer.global_context.symbol_table
    )
    module_line = semantic_inventory.provenance_lines("semantic").find { |line| line.starts_with?("modules provenance ") }

    module_line.should_not be_nil
    module_line.not_nil!.should contain("semantic_direct_total=1")
    module_line.not_nil!.should contain("semantic_macro_expanded_total=1")
  end
end
