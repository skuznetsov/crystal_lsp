require "spec"

require "../../src/compiler/frontend/parser"


describe "CrystalV2::Compiler::Frontend::Parser" do
  describe "Phase 69: Tuple literals {1, 2, 3} (DISCOVERED - Testing)" do
    it "parses simple tuple with multiple elements" do
      source = "{1, 2, 3}"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      tuple = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(tuple).should eq(CrystalV2::Compiler::Frontend::NodeKind::TupleLiteral)

      elements = CrystalV2::Compiler::Frontend.node_tuple_elements(tuple).not_nil!
      elements.size.should eq(3)
    end

    it "parses single element tuple" do
      source = "{42}"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      tuple = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(tuple).should eq(CrystalV2::Compiler::Frontend::NodeKind::TupleLiteral)

      elements = CrystalV2::Compiler::Frontend.node_tuple_elements(tuple).not_nil!
      elements.size.should eq(1)
    end

    it "parses single element tuple with trailing comma" do
      source = "{42,}"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      tuple = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(tuple).should eq(CrystalV2::Compiler::Frontend::NodeKind::TupleLiteral)

      elements = CrystalV2::Compiler::Frontend.node_tuple_elements(tuple).not_nil!
      elements.size.should eq(1)
    end

    it "parses tuple with trailing comma" do
      source = "{1, 2, 3,}"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      tuple = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(tuple).should eq(CrystalV2::Compiler::Frontend::NodeKind::TupleLiteral)

      elements = CrystalV2::Compiler::Frontend.node_tuple_elements(tuple).not_nil!
      elements.size.should eq(3)
    end

    it "parses nested tuples" do
      source = "{{1, 2}, {3, 4}}"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      outer_tuple = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(outer_tuple).should eq(CrystalV2::Compiler::Frontend::NodeKind::TupleLiteral)

      outer_elements = CrystalV2::Compiler::Frontend.node_tuple_elements(outer_tuple).not_nil!
      outer_elements.size.should eq(2)

      # Check first inner tuple
      inner1 = arena[outer_elements[0]]
      CrystalV2::Compiler::Frontend.node_kind(inner1).should eq(CrystalV2::Compiler::Frontend::NodeKind::TupleLiteral)

      # Check second inner tuple
      inner2 = arena[outer_elements[1]]
      CrystalV2::Compiler::Frontend.node_kind(inner2).should eq(CrystalV2::Compiler::Frontend::NodeKind::TupleLiteral)
    end

    it "parses tuple with expressions" do
      source = "{1 + 1, 2 * 2, 3 - 1}"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      tuple = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(tuple).should eq(CrystalV2::Compiler::Frontend::NodeKind::TupleLiteral)

      elements = CrystalV2::Compiler::Frontend.node_tuple_elements(tuple).not_nil!
      elements.size.should eq(3)

      # First element is binary expression
      elem1 = arena[elements[0]]
      CrystalV2::Compiler::Frontend.node_kind(elem1).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
    end

    it "parses tuple in assignment" do
      source = "x = {1, 2, 3}"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(assign).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)

      value = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(value).should eq(CrystalV2::Compiler::Frontend::NodeKind::TupleLiteral)
    end

    it "parses tuple in method call" do
      source = "foo({1, 2})"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      call_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(call_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Call)
      call_node.should be_a(CrystalV2::Compiler::Frontend::CallNode)

      args = call_node.as(CrystalV2::Compiler::Frontend::CallNode).args
      args.size.should eq(1)

      arg = arena[args[0]]
      CrystalV2::Compiler::Frontend.node_kind(arg).should eq(CrystalV2::Compiler::Frontend::NodeKind::TupleLiteral)
    end

    it "parses tuple in array literal" do
      source = "[{1, 2}, {3, 4}]"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      array = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(array).should eq(CrystalV2::Compiler::Frontend::NodeKind::ArrayLiteral)

      array_elements = CrystalV2::Compiler::Frontend.node_array_elements(array).not_nil!
      array_elements.size.should eq(2)

      # Both elements are tuples
      elem1 = arena[array_elements[0]]
      CrystalV2::Compiler::Frontend.node_kind(elem1).should eq(CrystalV2::Compiler::Frontend::NodeKind::TupleLiteral)

      elem2 = arena[array_elements[1]]
      CrystalV2::Compiler::Frontend.node_kind(elem2).should eq(CrystalV2::Compiler::Frontend::NodeKind::TupleLiteral)
    end

    it "disambiguates tuple from hash (tuple has comma)" do
      source = "{1, 2}"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(node).should eq(CrystalV2::Compiler::Frontend::NodeKind::TupleLiteral)
    end

    it "disambiguates hash from tuple (hash has arrow)" do
      source = "{1 => 2}"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(node).should eq(CrystalV2::Compiler::Frontend::NodeKind::HashLiteral)
    end

    it "parses empty braces as hash not tuple" do
      source = "{}"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      node = arena[program.roots[0]]
      # Empty {} is hash by default
      CrystalV2::Compiler::Frontend.node_kind(node).should eq(CrystalV2::Compiler::Frontend::NodeKind::HashLiteral)
    end

    it "parses tuple with identifier elements" do
      source = "{x, y, z}"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      tuple = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(tuple).should eq(CrystalV2::Compiler::Frontend::NodeKind::TupleLiteral)

      elements = CrystalV2::Compiler::Frontend.node_tuple_elements(tuple).not_nil!
      elements.size.should eq(3)

      # All elements are identifiers
      elem1 = arena[elements[0]]
      CrystalV2::Compiler::Frontend.node_kind(elem1).should eq(CrystalV2::Compiler::Frontend::NodeKind::Identifier)
    end
  end
end
