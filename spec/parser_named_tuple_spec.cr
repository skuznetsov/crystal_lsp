require "spec"

require "../src/compiler/frontend/parser"

describe "CrystalV2::Compiler::Frontend::Parser" do
  describe "Phase 70: Named tuple literals {name: \"value\"} (PRODUCTION-READY)" do
    it "parses simple named tuple with one entry" do
      source = "{name: \"Alice\"}"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      named_tuple = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(named_tuple).should eq(CrystalV2::Compiler::Frontend::NodeKind::NamedTupleLiteral)

      entries = CrystalV2::Compiler::Frontend.node_named_tuple_entries(named_tuple).not_nil!
      entries.size.should eq(1)
      entries[0].key.should eq("name".to_slice)
    end

    it "parses named tuple with multiple entries" do
      source = "{name: \"Alice\", age: 30}"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      named_tuple = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(named_tuple).should eq(CrystalV2::Compiler::Frontend::NodeKind::NamedTupleLiteral)

      entries = CrystalV2::Compiler::Frontend.node_named_tuple_entries(named_tuple).not_nil!
      entries.size.should eq(2)
      entries[0].key.should eq("name".to_slice)
      entries[1].key.should eq("age".to_slice)
    end

    it "parses named tuple with trailing comma" do
      source = "{name: \"Bob\", age: 25,}"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      named_tuple = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(named_tuple).should eq(CrystalV2::Compiler::Frontend::NodeKind::NamedTupleLiteral)

      entries = CrystalV2::Compiler::Frontend.node_named_tuple_entries(named_tuple).not_nil!
      entries.size.should eq(2)
    end

    it "parses named tuple with expression values" do
      source = "{x: 1 + 1, y: 2 * 2}"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      named_tuple = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(named_tuple).should eq(CrystalV2::Compiler::Frontend::NodeKind::NamedTupleLiteral)

      entries = CrystalV2::Compiler::Frontend.node_named_tuple_entries(named_tuple).not_nil!
      entries.size.should eq(2)

      # Values are binary expressions
      value1 = arena[entries[0].value]
      CrystalV2::Compiler::Frontend.node_kind(value1).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
    end

    it "parses named tuple with identifier values" do
      source = "{name: x, age: y}"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      named_tuple = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(named_tuple).should eq(CrystalV2::Compiler::Frontend::NodeKind::NamedTupleLiteral)

      entries = CrystalV2::Compiler::Frontend.node_named_tuple_entries(named_tuple).not_nil!
      entries.size.should eq(2)

      # Values are identifiers
      value1 = arena[entries[0].value]
      CrystalV2::Compiler::Frontend.node_kind(value1).should eq(CrystalV2::Compiler::Frontend::NodeKind::Identifier)
    end

    it "parses nested named tuples" do
      source = "{person: {name: \"Alice\", age: 30}}"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      outer = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(outer).should eq(CrystalV2::Compiler::Frontend::NodeKind::NamedTupleLiteral)

      entries = CrystalV2::Compiler::Frontend.node_named_tuple_entries(outer).not_nil!
      entries.size.should eq(1)
      entries[0].key.should eq("person".to_slice)

      # Inner is also named tuple
      inner = arena[entries[0].value]
      CrystalV2::Compiler::Frontend.node_kind(inner).should eq(CrystalV2::Compiler::Frontend::NodeKind::NamedTupleLiteral)
    end

    it "parses named tuple in assignment" do
      source = "x = {name: \"Alice\", age: 30}"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(assign).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)

      value = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(value).should eq(CrystalV2::Compiler::Frontend::NodeKind::NamedTupleLiteral)
    end

    it "parses named tuple in method call" do
      source = "foo({name: \"Alice\"})"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      call = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(call).should eq(CrystalV2::Compiler::Frontend::NodeKind::Call)

      args = CrystalV2::Compiler::Frontend.node_args(call).not_nil!
      args.size.should eq(1)

      arg = arena[args[0]]
      CrystalV2::Compiler::Frontend.node_kind(arg).should eq(CrystalV2::Compiler::Frontend::NodeKind::NamedTupleLiteral)
    end

    it "parses named tuple in array literal" do
      source = "[{name: \"Alice\"}, {name: \"Bob\"}]"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      array = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(array).should eq(CrystalV2::Compiler::Frontend::NodeKind::ArrayLiteral)

      array_elements = CrystalV2::Compiler::Frontend.node_array_elements(array).not_nil!
      array_elements.size.should eq(2)

      # Both elements are named tuples
      elem1 = arena[array_elements[0]]
      CrystalV2::Compiler::Frontend.node_kind(elem1).should eq(CrystalV2::Compiler::Frontend::NodeKind::NamedTupleLiteral)

      elem2 = arena[array_elements[1]]
      CrystalV2::Compiler::Frontend.node_kind(elem2).should eq(CrystalV2::Compiler::Frontend::NodeKind::NamedTupleLiteral)
    end

    it "disambiguates named tuple from hash (colon vs arrow)" do
      source = "{name: \"Alice\"}"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(node).should eq(CrystalV2::Compiler::Frontend::NodeKind::NamedTupleLiteral)
    end

    it "disambiguates hash from named tuple (arrow)" do
      source = "{\"name\" => \"Alice\"}"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(node).should eq(CrystalV2::Compiler::Frontend::NodeKind::HashLiteral)
    end

    it "disambiguates tuple from named tuple (comma vs colon)" do
      source = "{1, 2}"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(node).should eq(CrystalV2::Compiler::Frontend::NodeKind::TupleLiteral)
    end

    it "parses empty braces as hash not named tuple" do
      source = "{}"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      node = arena[program.roots[0]]
      # Empty {} is hash by default (existing behavior)
      CrystalV2::Compiler::Frontend.node_kind(node).should eq(CrystalV2::Compiler::Frontend::NodeKind::HashLiteral)
    end

    it "parses named tuple with many entries" do
      source = "{a: 1, b: 2, c: 3, d: 4, e: 5}"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      named_tuple = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(named_tuple).should eq(CrystalV2::Compiler::Frontend::NodeKind::NamedTupleLiteral)

      entries = CrystalV2::Compiler::Frontend.node_named_tuple_entries(named_tuple).not_nil!
      entries.size.should eq(5)
      entries[0].key.should eq("a".to_slice)
      entries[4].key.should eq("e".to_slice)
    end
  end
end
