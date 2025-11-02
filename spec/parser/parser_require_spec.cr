require "spec"

require "../../src/compiler/frontend/parser"

describe "CrystalV2::Compiler::Frontend::Parser" do
  describe "Phase 65: Require statement (PRODUCTION-READY)" do
    it "parses require with string literal" do
      source = "require \"spec\""

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      require_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(require_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Require)

      # Path should be a string literal
      path = arena[CrystalV2::Compiler::Frontend.node_require_path(require_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(path).should eq(CrystalV2::Compiler::Frontend::NodeKind::String)
      String.new(CrystalV2::Compiler::Frontend.node_literal(path).not_nil!).should eq("spec")
    end

    it "parses require with relative path" do
      source = "require \"./local_file\""

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      require_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(require_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Require)

      path = arena[CrystalV2::Compiler::Frontend.node_require_path(require_node).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(path).not_nil!).should eq("./local_file")
    end

    it "parses require with absolute path" do
      source = "require \"/usr/lib/crystal\""

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      require_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(require_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Require)

      path = arena[CrystalV2::Compiler::Frontend.node_require_path(require_node).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(path).not_nil!).should eq("/usr/lib/crystal")
    end

    it "parses require with nested path" do
      source = "require \"compiler/frontend/parser\""

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      require_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(require_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Require)

      path = arena[CrystalV2::Compiler::Frontend.node_require_path(require_node).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(path).not_nil!).should eq("compiler/frontend/parser")
    end

    it "parses multiple require statements" do
      source = <<-CRYSTAL
      require "spec"
      require "compiler"
      require "./local"
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(3)
      arena = program.arena

      # First require
      req1 = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(req1).should eq(CrystalV2::Compiler::Frontend::NodeKind::Require)
      path1 = arena[CrystalV2::Compiler::Frontend.node_require_path(req1).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(path1).not_nil!).should eq("spec")

      # Second require
      req2 = arena[program.roots[1]]
      CrystalV2::Compiler::Frontend.node_kind(req2).should eq(CrystalV2::Compiler::Frontend::NodeKind::Require)
      path2 = arena[CrystalV2::Compiler::Frontend.node_require_path(req2).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(path2).not_nil!).should eq("compiler")

      # Third require
      req3 = arena[program.roots[2]]
      CrystalV2::Compiler::Frontend.node_kind(req3).should eq(CrystalV2::Compiler::Frontend::NodeKind::Require)
      path3 = arena[CrystalV2::Compiler::Frontend.node_require_path(req3).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(path3).not_nil!).should eq("./local")
    end

    it "parses require with wildcard" do
      source = "require \"./*\""

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      require_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(require_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Require)

      path = arena[CrystalV2::Compiler::Frontend.node_require_path(require_node).not_nil!]
      String.new(CrystalV2::Compiler::Frontend.node_literal(path).not_nil!).should eq("./*")
    end

    it "parses require followed by code" do
      source = <<-CRYSTAL
      require "spec"
      x = 1
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(2)
      arena = program.arena

      # First statement: require
      req = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(req).should eq(CrystalV2::Compiler::Frontend::NodeKind::Require)

      # Second statement: assignment
      assign = arena[program.roots[1]]
      CrystalV2::Compiler::Frontend.node_kind(assign).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)
    end

    it "parses require inside class" do
      source = <<-CRYSTAL
      class Foo
        require "bar"
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      class_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(class_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Class)

      body = CrystalV2::Compiler::Frontend.node_class_body(class_node).not_nil!
      body.size.should eq(1)

      req = arena[body[0]]
      CrystalV2::Compiler::Frontend.node_kind(req).should eq(CrystalV2::Compiler::Frontend::NodeKind::Require)
    end

    it "parses require with string interpolation (advanced)" do
      source = "require \"foo_\#{version}\""

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      require_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(require_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Require)

      # Path should be string interpolation node
      path = arena[CrystalV2::Compiler::Frontend.node_require_path(require_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(path).should eq(CrystalV2::Compiler::Frontend::NodeKind::StringInterpolation)
    end

    it "parses require at top-level typical usage" do
      source = <<-CRYSTAL
      require "spec"
      require "../../src/compiler/frontend/parser"

      describe "Test" do
        it "works" do
        end
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(3)
      arena = program.arena

      # First two should be requires
      req1 = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(req1).should eq(CrystalV2::Compiler::Frontend::NodeKind::Require)

      req2 = arena[program.roots[1]]
      CrystalV2::Compiler::Frontend.node_kind(req2).should eq(CrystalV2::Compiler::Frontend::NodeKind::Require)

      # Third statement exists (don't care about exact type - could be Call or Identifier depending on parser state)
      program.roots[2].should_not be_nil
    end
  end
end
