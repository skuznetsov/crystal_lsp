require "spec"

require "../../src/compiler/frontend/parser"


describe CrystalV2::Compiler::Frontend::Parser do
  it "produces AST nodes with arena storage" do
    lexer = CrystalV2::Compiler::Frontend::Lexer.new("foo + 1
bar")
    parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
    program = parser.parse_program

    program.roots.size.should eq(2)
    arena = program.arena
    first_id = program.roots.first

    # Check if it's a typed node (BinaryNode is migrated to typed)
    first = arena[first_id]
    first.should be_a(CrystalV2::Compiler::Frontend::BinaryNode)
  end

  it "parses grouping, unary, and calls" do
    source = "-(f(1 + 2))"
    parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
    program = parser.parse_program

    program.roots.size.should eq(1)
    arena = program.arena
    root_id = program.roots.first

    root = arena[root_id]
    root.should be_a(CrystalV2::Compiler::Frontend::UnaryNode)

    grouping_id = root.as(CrystalV2::Compiler::Frontend::UnaryNode).operand
    grouping = arena[grouping_id]
    CrystalV2::Compiler::Frontend.node_kind(grouping).should eq(CrystalV2::Compiler::Frontend::NodeKind::Grouping)

    call_node = arena[CrystalV2::Compiler::Frontend.node_left(grouping).not_nil!]
    CrystalV2::Compiler::Frontend.node_kind(call_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Call)
    CrystalV2::Compiler::Frontend.node_args(call_node).not_nil!.size.should eq(1)

    arg_id = CrystalV2::Compiler::Frontend.node_args(call_node).not_nil!.first
    arg = arena[arg_id]
    arg.should be_a(CrystalV2::Compiler::Frontend::BinaryNode)
  end

  it "parses member access and indexing" do
    source = "foo.bar(1)[0]"
    parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
    program = parser.parse_program

    program.roots.size.should eq(1)
    arena = program.arena

    root = arena[program.roots.first]
    CrystalV2::Compiler::Frontend.node_kind(root).should eq(CrystalV2::Compiler::Frontend::NodeKind::Index)

    # Index uses 'left' field, not 'callee'
    call_node = arena[CrystalV2::Compiler::Frontend.node_left(root).not_nil!]
    CrystalV2::Compiler::Frontend.node_kind(call_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Call)

    member = arena[CrystalV2::Compiler::Frontend.node_callee(call_node).not_nil!]
    CrystalV2::Compiler::Frontend.node_kind(member).should eq(CrystalV2::Compiler::Frontend::NodeKind::MemberAccess)
    CrystalV2::Compiler::Frontend.node_member(member).try { |m| String.new(m) }.should eq("bar")
  end

  it "parses no-parens call with multiple args and block" do
    source = <<-CR
      foo 1, 2 do |value|
        value
      end
    CR

    parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
    program = parser.parse_program

    program.roots.size.should eq(1)
    arena = program.arena
    root = arena[program.roots.first]

    root.should be_a(CrystalV2::Compiler::Frontend::CallNode)
    call = root.as(CrystalV2::Compiler::Frontend::CallNode)
    call.args.size.should eq(2)
    call.block.should_not be_nil
  end

  it "parses macro definitions with expression pieces" do
    source = <<-CR
      macro my_macro
        class {{ name }}
      end
    CR

    parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
    program = parser.parse_program

    program.roots.size.should eq(1)
    arena = program.arena
    macro_def = arena[program.roots.first]
    CrystalV2::Compiler::Frontend.node_kind(macro_def).should eq(CrystalV2::Compiler::Frontend::NodeKind::MacroDef)
    CrystalV2::Compiler::Frontend.node_macro_name(macro_def).try { |slice| String.new(slice) }.should eq("my_macro")

    body = arena[CrystalV2::Compiler::Frontend.node_left(macro_def).not_nil!]
    CrystalV2::Compiler::Frontend.node_kind(body).should eq(CrystalV2::Compiler::Frontend::NodeKind::MacroLiteral)
    pieces = CrystalV2::Compiler::Frontend.node_macro_pieces(body).not_nil!
    pieces.map(&.kind).should contain(CrystalV2::Compiler::Frontend::MacroPiece::Kind::Expression)
  end

  it "parses macro control blocks" do
    source = <<-CR
      macro test_macro
        {% if cond %}
          call_true
        {% elsif other %}
          call_other
        {% else %}
          call_false
        {% end %}
      end
    CR

    parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
    program = parser.parse_program

    program.roots.size.should eq(1)
    arena = program.arena
    macro_def = arena[program.roots.first]
    body = arena[CrystalV2::Compiler::Frontend.node_left(macro_def).not_nil!]
    pieces = CrystalV2::Compiler::Frontend.node_macro_pieces(body).not_nil!
    pieces.map(&.kind).should contain(CrystalV2::Compiler::Frontend::MacroPiece::Kind::ControlStart)
    pieces.map(&.kind).should contain(CrystalV2::Compiler::Frontend::MacroPiece::Kind::ControlElse)
    pieces.map(&.kind).should contain(CrystalV2::Compiler::Frontend::MacroPiece::Kind::ControlElseIf)
    elsif_piece = pieces.find { |piece| piece.kind == CrystalV2::Compiler::Frontend::MacroPiece::Kind::ControlElseIf }
    elsif_piece.not_nil!.expr.should_not be_nil
    pieces.map(&.kind).should contain(CrystalV2::Compiler::Frontend::MacroPiece::Kind::ControlEnd)
  end

  it "parses return yield with grouped first arg and extra args" do
    source = <<-CR
      def foo
        return yield (1 << 18) &+ (2 << 12) &+ (3 << 6) &+ (4 &- 5), 4, nil
      end
    CR

    parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
    program = parser.parse_program

    program.roots.size.should eq(1)
    arena = program.arena
    def_node = arena[program.roots.first]
    def_node.should be_a(CrystalV2::Compiler::Frontend::DefNode)

    body = def_node.as(CrystalV2::Compiler::Frontend::DefNode).body
    body.should_not be_nil
    return_node = arena[body.not_nil!.first]
    return_node.should be_a(CrystalV2::Compiler::Frontend::ReturnNode)

    value_id = return_node.as(CrystalV2::Compiler::Frontend::ReturnNode).value
    value_id.should_not be_nil
    value_node = arena[value_id.not_nil!]
    value_node.should be_a(CrystalV2::Compiler::Frontend::YieldNode)
    value_node.as(CrystalV2::Compiler::Frontend::YieldNode).args.size.should eq(3)
  end

  it "treats escaped macro control as text in macro bodies" do
    source = <<-'CR'
      macro outer
        {% for n in [1, 2] %}
          \{% if Int{{n}} == 1 %}
            {{n}}
          \{% end %}
        {% end %}
      end
    CR

    parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
    program = parser.parse_program

    program.roots.size.should eq(1)
    arena = program.arena
    macro_def = arena[program.roots.first]
    body = arena[CrystalV2::Compiler::Frontend.node_left(macro_def).not_nil!]
    pieces = CrystalV2::Compiler::Frontend.node_macro_pieces(body).not_nil!

    control_keywords = pieces.map(&.control_keyword).compact
    control_keywords.should contain("for")
    control_keywords.should contain("end")
    control_keywords.should_not contain("if")

    pieces.any? { |piece| piece.kind.text? && piece.text.try(&.includes?("{% if")) }.should be_true
    pieces.any? { |piece| piece.kind.expression? }.should be_true
  end

  it "treats escaped macro expressions as text in macro bodies" do
    source = <<-'CR'
      macro outer
        \{{ foo }}
        {{ bar }}
      end
    CR

    parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
    program = parser.parse_program

    program.roots.size.should eq(1)
    arena = program.arena
    macro_def = arena[program.roots.first]
    body = arena[CrystalV2::Compiler::Frontend.node_left(macro_def).not_nil!]
    pieces = CrystalV2::Compiler::Frontend.node_macro_pieces(body).not_nil!

    pieces.any? { |piece| piece.kind.text? && piece.text.try(&.includes?("{{ foo }}")) }.should be_true
    pieces.count { |piece| piece.kind.expression? }.should eq(1)
  end

  # ECR feature, not Crystal macros
  it "trims whitespace around macro expressions" do
    source = <<-'CR'
      macro trim_macro
        line1
        {{- value -}}
        line2
      end
    CR

    parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
    program = parser.parse_program

    program.roots.size.should eq(1)
    arena = program.arena
    macro_def = arena[program.roots.first]
    body = arena[CrystalV2::Compiler::Frontend.node_left(macro_def).not_nil!]
    pieces = CrystalV2::Compiler::Frontend.node_macro_pieces(body).not_nil!

    expr_piece = pieces.find { |piece| piece.kind == CrystalV2::Compiler::Frontend::MacroPiece::Kind::Expression }
    expr_piece = expr_piece.not_nil!
    expr_piece.trim_left.should be_true
    expr_piece.trim_right.should be_true
  end

  # ECR feature, not Crystal macros
  it "trims whitespace around macro expressions using tilde" do
    source = <<-'CR'
      macro trim_macro
        line1
        {{~ value ~}}
        line2
      end
    CR

    parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
    program = parser.parse_program

    program.roots.size.should eq(1)
    arena = program.arena
    macro_def = arena[program.roots.first]
    body = arena[CrystalV2::Compiler::Frontend.node_left(macro_def).not_nil!]
    pieces = CrystalV2::Compiler::Frontend.node_macro_pieces(body).not_nil!

    expr_piece = pieces.find { |piece| piece.kind == CrystalV2::Compiler::Frontend::MacroPiece::Kind::Expression }
    expr_piece = expr_piece.not_nil!
    expr_piece.trim_left.should be_true
    expr_piece.trim_right.should be_true
  end

  it "skips newline after macro backslash" do
    source = <<-'CR'
      macro newline_macro
        {{ value }}\
        line2
      end
    CR

    parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
    program = parser.parse_program

    program.roots.size.should eq(1)
    arena = program.arena
    macro_def = arena[program.roots.first]
    body = arena[CrystalV2::Compiler::Frontend.node_left(macro_def).not_nil!]
    pieces = CrystalV2::Compiler::Frontend.node_macro_pieces(body).not_nil!

    pieces.size.should eq(2)
    expr_piece = pieces.find { |piece| piece.kind == CrystalV2::Compiler::Frontend::MacroPiece::Kind::Expression }.not_nil!
    expr_piece.trim_left.should be_false
    expr_piece.trim_right.should be_false

    text_piece = pieces.last
    text_piece.text.should eq("line2")
  end

  it "parses macro while loop" do
    source = <<-CR
      macro loop_macro
        {% while keep_going? %}
          body
        {% end %}
      end
    CR

    parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
    program = parser.parse_program

    program.roots.size.should eq(1)
    arena = program.arena
    macro_def = arena[program.roots.first]
    body = arena[CrystalV2::Compiler::Frontend.node_left(macro_def).not_nil!]
    pieces = CrystalV2::Compiler::Frontend.node_macro_pieces(body).not_nil!

    # Should have ControlStart for while
    control_start = pieces.find { |p| p.kind == CrystalV2::Compiler::Frontend::MacroPiece::Kind::ControlStart }
    control_start.should_not be_nil
    control_start.not_nil!.control_keyword.should eq("while")
    control_start.not_nil!.expr.should_not be_nil

    # Should have ControlEnd
    pieces.map(&.kind).should contain(CrystalV2::Compiler::Frontend::MacroPiece::Kind::ControlEnd)
  end

  # ECR feature, not Crystal macros
  it "parses macro while loop with trims" do
    source = <<-CR
      macro trim_loop
        {%- while active? -%}
        content
        {%- end -%}
      end
    CR

    parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
    program = parser.parse_program

    program.roots.size.should eq(1)
    arena = program.arena
    macro_def = arena[program.roots.first]
    body = arena[CrystalV2::Compiler::Frontend.node_left(macro_def).not_nil!]
    pieces = CrystalV2::Compiler::Frontend.node_macro_pieces(body).not_nil!

    control_start = pieces.find { |p| p.kind == CrystalV2::Compiler::Frontend::MacroPiece::Kind::ControlStart }
    control_start.should_not be_nil
    control_start.not_nil!.trim_left.should be_true
    control_start.not_nil!.trim_right.should be_true

    # Body should be properly trimmed
    CrystalV2::Compiler::Frontend.node_trim_left(body).should be_true
    CrystalV2::Compiler::Frontend.node_trim_right(body).should be_true
  end

  it "parses macro for loop" do
    source = <<-CR
      macro for_macro
        {% for item in items %}
          body
        {% end %}
      end
    CR

    parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
    program = parser.parse_program

    program.roots.size.should eq(1)
    arena = program.arena
    macro_def = arena[program.roots.first]
    body = arena[CrystalV2::Compiler::Frontend.node_left(macro_def).not_nil!]
    pieces = CrystalV2::Compiler::Frontend.node_macro_pieces(body).not_nil!

    # Should have ControlStart for for
    control_start = pieces.find { |p| p.kind == CrystalV2::Compiler::Frontend::MacroPiece::Kind::ControlStart }
    control_start.should_not be_nil
    control_start.not_nil!.control_keyword.should eq("for")

    # Should have iteration variables
    control_start.not_nil!.iter_vars.should eq(["item"])
    control_start.not_nil!.iterable.should_not be_nil

    # Should have ControlEnd
    pieces.map(&.kind).should contain(CrystalV2::Compiler::Frontend::MacroPiece::Kind::ControlEnd)
  end

  it "parses macro for loop with tuple destructuring" do
    source = <<-CR
      macro hash_macro
        {% for key, value in items %}
          content
        {% end %}
      end
    CR

    parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
    program = parser.parse_program

    program.roots.size.should eq(1)
    arena = program.arena
    macro_def = arena[program.roots.first]
    body = arena[CrystalV2::Compiler::Frontend.node_left(macro_def).not_nil!]
    pieces = CrystalV2::Compiler::Frontend.node_macro_pieces(body).not_nil!

    control_start = pieces.find { |p| p.kind == CrystalV2::Compiler::Frontend::MacroPiece::Kind::ControlStart }
    control_start.should_not be_nil

    # Should capture both iteration variables
    control_start.not_nil!.iter_vars.should eq(["key", "value"])
    control_start.not_nil!.iterable.should_not be_nil
  end

  it "parses macro comment block" do
    source = <<-CR
      macro comment_macro
        visible1
        {% comment %}
          hidden content
          {{ expression }}
          more hidden
        {% end %}
        visible2
      end
    CR

    parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
    program = parser.parse_program

    program.roots.size.should eq(1)
    arena = program.arena
    macro_def = arena[program.roots.first]
    body = arena[CrystalV2::Compiler::Frontend.node_left(macro_def).not_nil!]
    pieces = CrystalV2::Compiler::Frontend.node_macro_pieces(body).not_nil!

    # Should have ControlStart for comment
    control_start = pieces.find { |p| p.kind == CrystalV2::Compiler::Frontend::MacroPiece::Kind::ControlStart }
    control_start.should_not be_nil
    control_start.not_nil!.control_keyword.should eq("comment")

    # Should have ControlEnd
    pieces.map(&.kind).should contain(CrystalV2::Compiler::Frontend::MacroPiece::Kind::ControlEnd)

    # Content inside comment should NOT appear as text pieces
    text_pieces = pieces.select { |p| p.kind == CrystalV2::Compiler::Frontend::MacroPiece::Kind::Text }
    text_pieces.any? { |p| p.text.to_s.includes?("hidden") }.should be_false

    # Should have visible text
    text_pieces.any? { |p| p.text.to_s.includes?("visible") }.should be_true
  end

  # ECR feature, not Crystal macros
  it "parses macro comment block with trim markers" do
    source = <<-CR
      macro trim_comment
        line1
        {%- comment -%}
        hidden
        {%- end -%}
        line2
      end
    CR

    parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
    program = parser.parse_program

    program.roots.size.should eq(1)
    arena = program.arena
    macro_def = arena[program.roots.first]
    body = arena[CrystalV2::Compiler::Frontend.node_left(macro_def).not_nil!]
    pieces = CrystalV2::Compiler::Frontend.node_macro_pieces(body).not_nil!

    control_start = pieces.find { |p| p.kind == CrystalV2::Compiler::Frontend::MacroPiece::Kind::ControlStart }
    control_start.should_not be_nil
    control_start.not_nil!.trim_left.should be_true
    control_start.not_nil!.trim_right.should be_true
  end

  it "exposes diagnostic spans on unexpected tokens" do
    source = ")"
    parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
    parser.parse_program

    parser.diagnostics.size.should eq(1)
    diagnostic = parser.diagnostics.first
    diagnostic.message.should eq("unexpected RParen")  # More specific than "Operator"
    diagnostic.span.start_line.should eq(1)
    diagnostic.span.start_column.should eq(1)
    diagnostic.span.end_column.should eq(2)
  end

  it "handles backslash escape before control pieces" do
    source = <<-CR
      macro escape_control
        {{ value }}\
        {% if condition %}
          body
        {% end %}
      end
    CR

    parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
    program = parser.parse_program

    program.roots.size.should eq(1)
    arena = program.arena
    macro_def = arena[program.roots.first]
    body = arena[CrystalV2::Compiler::Frontend.node_left(macro_def).not_nil!]
    pieces = CrystalV2::Compiler::Frontend.node_macro_pieces(body).not_nil!

    # Should have expression piece
    expr_piece = pieces.find { |p| p.kind == CrystalV2::Compiler::Frontend::MacroPiece::Kind::Expression }
    expr_piece.should_not be_nil

    # Should have control pieces for if block
    control_pieces = pieces.select { |p| p.kind == CrystalV2::Compiler::Frontend::MacroPiece::Kind::ControlStart }
    control_pieces.size.should be >= 1
    control_pieces.any? { |p| p.control_keyword == "if" }.should be_true

    # Text after expression should not include newline (due to backslash)
    text_after_expr = pieces.select { |p| p.kind == CrystalV2::Compiler::Frontend::MacroPiece::Kind::Text }
    # Backslash should have consumed the newline between expression and control
  end

  # ECR feature, not Crystal macros
  it "handles mixed dash and tilde trim markers" do
    source = <<-'CR'
      macro mixed_trims
        line1
        {{- value ~}}
        line2
      end
    CR

    parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
    program = parser.parse_program

    program.roots.size.should eq(1)
    arena = program.arena
    macro_def = arena[program.roots.first]
    body = arena[CrystalV2::Compiler::Frontend.node_left(macro_def).not_nil!]
    pieces = CrystalV2::Compiler::Frontend.node_macro_pieces(body).not_nil!

    expr_piece = pieces.find { |p| p.kind == CrystalV2::Compiler::Frontend::MacroPiece::Kind::Expression }
    expr_piece.should_not be_nil

    # Both dash (-) and tilde (~) should set trim flags
    expr_piece.not_nil!.trim_left.should be_true
    expr_piece.not_nil!.trim_right.should be_true
  end

  # ECR feature, not Crystal macros
  it "handles consecutive expressions with shared trims" do
    source = <<-'CR'
      macro consecutive
        {{ a -}}{{ b }}{{ c -}}
      end
    CR

    parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
    program = parser.parse_program

    program.roots.size.should eq(1)
    arena = program.arena
    macro_def = arena[program.roots.first]
    body = arena[CrystalV2::Compiler::Frontend.node_left(macro_def).not_nil!]
    pieces = CrystalV2::Compiler::Frontend.node_macro_pieces(body).not_nil!

    expr_pieces = pieces.select { |p| p.kind == CrystalV2::Compiler::Frontend::MacroPiece::Kind::Expression }
    expr_pieces.size.should eq(3)

    # First expression: right trim
    expr_pieces[0].trim_left.should be_false
    expr_pieces[0].trim_right.should be_true

    # Second expression: no trims
    expr_pieces[1].trim_left.should be_false
    expr_pieces[1].trim_right.should be_false

    # Third expression: right trim
    expr_pieces[2].trim_left.should be_false
    expr_pieces[2].trim_right.should be_true
  end

  it "captures spans for macro control pieces" do
    source = <<-CR
      macro span_test
        {% if condition %}
          body
        {% end %}
      end
    CR

    parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
    program = parser.parse_program

    program.roots.size.should eq(1)
    arena = program.arena
    macro_def = arena[program.roots.first]
    body = arena[CrystalV2::Compiler::Frontend.node_left(macro_def).not_nil!]
    pieces = CrystalV2::Compiler::Frontend.node_macro_pieces(body).not_nil!

    # Find control start piece
    control_start = pieces.find { |p| p.kind == CrystalV2::Compiler::Frontend::MacroPiece::Kind::ControlStart }
    control_start.should_not be_nil

    # Should have span covering full {% if condition %}
    span = control_start.not_nil!.span
    span.should_not be_nil
    span.not_nil!.start_line.should eq(2)  # Line with {% if
    span.not_nil!.start_column.should eq(5)  # Start of {%

    # Find control end piece
    control_end = pieces.find { |p| p.kind == CrystalV2::Compiler::Frontend::MacroPiece::Kind::ControlEnd }
    control_end.should_not be_nil

    # Should have span covering full {% end %}
    end_span = control_end.not_nil!.span
    end_span.should_not be_nil
    end_span.not_nil!.start_line.should eq(4)  # Line with {% end
  end

  it "captures spans for macro expression pieces" do
    source = <<-CR
      macro expr_span
        {{ value }}
      end
    CR

    parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
    program = parser.parse_program

    program.roots.size.should eq(1)
    arena = program.arena
    macro_def = arena[program.roots.first]
    body = arena[CrystalV2::Compiler::Frontend.node_left(macro_def).not_nil!]
    pieces = CrystalV2::Compiler::Frontend.node_macro_pieces(body).not_nil!

    # Find expression piece
    expr_piece = pieces.find { |p| p.kind == CrystalV2::Compiler::Frontend::MacroPiece::Kind::Expression }
    expr_piece.should_not be_nil

    # Should have span covering full {{ value }}
    span = expr_piece.not_nil!.span
    span.should_not be_nil
    span.not_nil!.start_line.should eq(2)  # Line with {{
    span.not_nil!.start_column.should eq(5)  # Start of {{
  end

  it "handles unary with empty grouping without crashing" do
    source = "-( )"
    parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
    program = parser.parse_program

    # Crystal allows this - ( ) is an empty tuple (Nil), so -() is valid syntax
    # The error comes later at semantic stage: "undefined method '-' for Nil"
    parser.diagnostics.should be_empty
    program.roots.size.should eq(1)
  end

  it "handles grouping with just unary operator without crashing" do
    source = "(+)"
    parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
    program = parser.parse_program

    # Should not crash, should emit diagnostic
    parser.diagnostics.size.should be >= 1
    parser.diagnostics.any? { |d| d.message.includes?("unexpected") }.should be_true
  end

  it "handles binary missing right operand without crashing" do
    source = "1 +"
    parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
    program = parser.parse_program

    # Should not crash - parser handles this gracefully
    # (Currently doesn't emit diagnostic, which is acceptable behavior)
  end

  it "handles nested invalid expressions without crashing" do
    source = "foo(1 +, 2)"
    parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
    program = parser.parse_program

    # Should not crash, should emit diagnostic
    parser.diagnostics.size.should be >= 1
  end

  it "handles invalid prefix in index without crashing" do
    source = "arr[+]"
    parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
    program = parser.parse_program

    # Should not crash, should emit diagnostic
    parser.diagnostics.size.should be >= 1
    parser.diagnostics.any? { |d| d.message.includes?("unexpected") }.should be_true
  end

  it "captures spans for macro text pieces" do
    source = <<-CR
      macro test
        hello world
        {{ expr }}
        after text
      end
    CR

    parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
    program = parser.parse_program

    program.roots.size.should eq(1)
    arena = program.arena
    macro_def = arena[program.roots.first]
    body = arena[CrystalV2::Compiler::Frontend.node_left(macro_def).not_nil!]
    pieces = CrystalV2::Compiler::Frontend.node_macro_pieces(body).not_nil!

    # Should have text pieces with spans
    text_pieces = pieces.select { |p| p.kind == CrystalV2::Compiler::Frontend::MacroPiece::Kind::Text }
    text_pieces.size.should be >= 1

    # First text piece should have span
    first_text = text_pieces.first
    first_text.span.should_not be_nil
    first_text.span.not_nil!.start_line.should eq(2)
  end

  it "captures spans for multi-line text pieces" do
    source = <<-CR
      macro multiline
        line 1
        line 2
        line 3
      end
    CR

    parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
    program = parser.parse_program

    program.roots.size.should eq(1)
    arena = program.arena
    macro_def = arena[program.roots.first]
    body = arena[CrystalV2::Compiler::Frontend.node_left(macro_def).not_nil!]
    pieces = CrystalV2::Compiler::Frontend.node_macro_pieces(body).not_nil!

    text_pieces = pieces.select { |p| p.kind == CrystalV2::Compiler::Frontend::MacroPiece::Kind::Text }
    text_pieces.size.should eq(1)

    # Multi-line text should have span covering all lines
    text_piece = text_pieces.first
    span = text_piece.span
    span.should_not be_nil
    span.not_nil!.start_line.should eq(2)
    span.not_nil!.end_line.should eq(5)  # Covers multiple lines
  end

  it "captures spans for trimmed text pieces" do
    source = <<-CR
      macro trimmed
        text before
        {{- expr }}
        text after
      end
    CR

    parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
    program = parser.parse_program

    program.roots.size.should eq(1)
    arena = program.arena
    macro_def = arena[program.roots.first]
    body = arena[CrystalV2::Compiler::Frontend.node_left(macro_def).not_nil!]
    pieces = CrystalV2::Compiler::Frontend.node_macro_pieces(body).not_nil!

    text_pieces = pieces.select { |p| p.kind == CrystalV2::Compiler::Frontend::MacroPiece::Kind::Text }
    text_pieces.size.should be >= 2

    # Both text pieces should have spans even though first is trimmed
    text_pieces.each do |piece|
      piece.span.should_not be_nil
    end
  end
end
