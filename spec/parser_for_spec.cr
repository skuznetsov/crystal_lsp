require "spec"

require "../src/compiler/frontend/parser"

describe "CrystalV2::Compiler::Frontend::Parser" do
  describe "Phase 99: for loop (iteration)" do
    it "parses simple for loop" do
      source = <<-CRYSTAL
      for item in collection
        puts item
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena
      for_node = arena[program.roots.first]

      CrystalV2::Compiler::Frontend.node_kind(for_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::For)
      String.new(for_node.as(CrystalV2::Compiler::Frontend::ForNode).variable).should eq("item")

      # Check collection
      collection_id = for_node.as(CrystalV2::Compiler::Frontend::ForNode).collection
      collection = arena[collection_id]
      CrystalV2::Compiler::Frontend.node_kind(collection).should eq(CrystalV2::Compiler::Frontend::NodeKind::Identifier)
      String.new(CrystalV2::Compiler::Frontend.node_literal(collection).not_nil!).should eq("collection")

      # Check body
      body = for_node.as(CrystalV2::Compiler::Frontend::ForNode).body
      body.size.should be >= 1
    end

    it "parses for loop with do keyword" do
      source = <<-CRYSTAL
      for x in list do
        puts x
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena
      for_node = arena[program.roots.first]

      CrystalV2::Compiler::Frontend.node_kind(for_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::For)
      String.new(for_node.as(CrystalV2::Compiler::Frontend::ForNode).variable.not_nil!).should eq("x")
    end

    it "parses for loop with array literal" do
      source = <<-CRYSTAL
      for n in [1, 2, 3]
        puts n
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena
      for_node = arena[program.roots.first]

      CrystalV2::Compiler::Frontend.node_kind(for_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::For)

      # Collection is array literal
      collection_id = for_node.as(CrystalV2::Compiler::Frontend::ForNode).collection.not_nil!
      collection = arena[collection_id]
      CrystalV2::Compiler::Frontend.node_kind(collection).should eq(CrystalV2::Compiler::Frontend::NodeKind::ArrayLiteral)
    end

    it "parses for loop with range" do
      source = <<-CRYSTAL
      for i in 1..10
        puts i
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena
      for_node = arena[program.roots.first]

      CrystalV2::Compiler::Frontend.node_kind(for_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::For)

      # Collection is range
      collection_id = for_node.as(CrystalV2::Compiler::Frontend::ForNode).collection.not_nil!
      collection = arena[collection_id]
      CrystalV2::Compiler::Frontend.node_kind(collection).should eq(CrystalV2::Compiler::Frontend::NodeKind::Range)
    end

    it "parses for loop with multiple statements in body" do
      source = <<-CRYSTAL
      for item in items
        puts item
        total += item
        count += 1
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena
      for_node = arena[program.roots.first]

      CrystalV2::Compiler::Frontend.node_kind(for_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::For)

      # Body has 3 statements
      body = for_node.as(CrystalV2::Compiler::Frontend::ForNode).body.not_nil!
      body.size.should be >= 3
    end

    it "parses for loop with method call as collection" do
      source = <<-CRYSTAL
      for value in get_values()
        process value
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena
      for_node = arena[program.roots.first]

      CrystalV2::Compiler::Frontend.node_kind(for_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::For)

      # Collection is method call
      collection_id = for_node.as(CrystalV2::Compiler::Frontend::ForNode).collection.not_nil!
      collection = arena[collection_id]
      CrystalV2::Compiler::Frontend.node_kind(collection).should eq(CrystalV2::Compiler::Frontend::NodeKind::Call)
    end

    it "parses empty for loop" do
      source = <<-CRYSTAL
      for x in collection
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena
      for_node = arena[program.roots.first]

      CrystalV2::Compiler::Frontend.node_kind(for_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::For)

      # Empty body
      body = for_node.as(CrystalV2::Compiler::Frontend::ForNode).body.not_nil!
      body.size.should eq(0)
    end

    it "parses nested for loops" do
      source = <<-CRYSTAL
      for i in outer
        for j in inner
          puts i, j
        end
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena
      outer_for = arena[program.roots.first]

      CrystalV2::Compiler::Frontend.node_kind(outer_for).should eq(CrystalV2::Compiler::Frontend::NodeKind::For)
      String.new(outer_for.as(CrystalV2::Compiler::Frontend::ForNode).variable).should eq("i")

      # Body contains inner for loop
      outer_body = outer_for.as(CrystalV2::Compiler::Frontend::ForNode).body
      outer_body.size.should eq(1)

      inner_for = arena[outer_body[0]]
      CrystalV2::Compiler::Frontend.node_kind(inner_for).should eq(CrystalV2::Compiler::Frontend::NodeKind::For)
      String.new(inner_for.as(CrystalV2::Compiler::Frontend::ForNode).variable).should eq("j")
    end

    it "emits error for missing 'in' keyword" do
      source = <<-CRYSTAL
      for item collection
        puts item
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      # Parser should emit error
      parser.diagnostics.size.should be > 0
      diagnostic = parser.diagnostics.first
      diagnostic.message.should contain("unexpected")
    end

    it "emits error for missing variable name" do
      source = <<-CRYSTAL
      for in collection
        puts item
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      # Parser should emit error
      parser.diagnostics.size.should be > 0
      diagnostic = parser.diagnostics.first
      diagnostic.message.should contain("unexpected")
    end
  end
end
