require "spec"

require "../../src/compiler/frontend/parser"

describe "CrystalV2::Compiler::Frontend::Parser" do
  describe "Phase 28: Begin/end blocks" do
    it "parses basic begin/end block" do
      source = <<-CRYSTAL
        begin
          x = 10
        end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      begin_node = arena[program.roots.first]
      CrystalV2::Compiler::Frontend.node_kind(begin_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Begin)

      # Check body
      body = CrystalV2::Compiler::Frontend.node_begin_body(begin_node).not_nil!
      body.size.should eq(1)

      assign = arena[body[0]]
      CrystalV2::Compiler::Frontend.node_kind(assign).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)
    end

    it "parses begin with empty body" do
      source = <<-CRYSTAL
        begin
        end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      begin_node = arena[program.roots.first]
      CrystalV2::Compiler::Frontend.node_kind(begin_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Begin)

      body = CrystalV2::Compiler::Frontend.node_begin_body(begin_node).not_nil!
      body.size.should eq(0)
    end

    it "parses begin with multiple statements" do
      source = <<-CRYSTAL
        begin
          x = 10
          y = 20
          z = 30
        end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      begin_node = arena[program.roots.first]
      CrystalV2::Compiler::Frontend.node_kind(begin_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Begin)

      body = CrystalV2::Compiler::Frontend.node_begin_body(begin_node).not_nil!
      body.size.should eq(3)
    end

    it "parses nested begin blocks" do
      source = <<-CRYSTAL
        begin
          x = 10
          begin
            y = 20
          end
        end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      outer_begin = arena[program.roots.first]
      CrystalV2::Compiler::Frontend.node_kind(outer_begin).should eq(CrystalV2::Compiler::Frontend::NodeKind::Begin)

      outer_body = CrystalV2::Compiler::Frontend.node_begin_body(outer_begin).not_nil!
      outer_body.size.should eq(2)

      # First statement is assignment
      assign = arena[outer_body[0]]
      CrystalV2::Compiler::Frontend.node_kind(assign).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)

      # Second statement is nested begin block
      inner_begin = arena[outer_body[1]]
      CrystalV2::Compiler::Frontend.node_kind(inner_begin).should eq(CrystalV2::Compiler::Frontend::NodeKind::Begin)

      inner_body = CrystalV2::Compiler::Frontend.node_begin_body(inner_begin).not_nil!
      inner_body.size.should eq(1)
    end

    it "parses begin with various statement types" do
      source = <<-CRYSTAL
        begin
          x = 10
          if true
            y = 20
          end
          z = 30
        end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      begin_node = arena[program.roots.first]
      CrystalV2::Compiler::Frontend.node_kind(begin_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Begin)

      body = CrystalV2::Compiler::Frontend.node_begin_body(begin_node).not_nil!
      body.size.should eq(3)

      # First is assignment
      CrystalV2::Compiler::Frontend.node_kind(arena[body[0]]).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)

      # Second is if
      CrystalV2::Compiler::Frontend.node_kind(arena[body[1]]).should eq(CrystalV2::Compiler::Frontend::NodeKind::If)

      # Third is assignment
      CrystalV2::Compiler::Frontend.node_kind(arena[body[2]]).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)
    end
  end
end
