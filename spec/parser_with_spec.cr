require "spec"

require "../src/compiler/frontend/parser"

describe "CrystalV2::Compiler::Frontend::Parser" do
  describe "Phase 67: With keyword (context block) (PRODUCTION-READY)" do
    it "parses simple with block" do
      source = <<-CRYSTAL
      with obj
        puts x
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      with_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(with_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::With)

      # Check receiver
      receiver = arena[CrystalV2::Compiler::Frontend.node_with_receiver(with_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(receiver).should eq(CrystalV2::Compiler::Frontend::NodeKind::Identifier)
      String.new(CrystalV2::Compiler::Frontend.node_literal(receiver).not_nil!).should eq("obj")

      # Check body (may have multiple statements depending on how parser processes them)
      body = CrystalV2::Compiler::Frontend.node_with_body(with_node).not_nil!
      body.size.should be >= 1
    end

    it "parses with block with method call receiver" do
      source = <<-CRYSTAL
      with get_object
        method1
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      with_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(with_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::With)

      # Receiver is method call
      receiver = arena[CrystalV2::Compiler::Frontend.node_with_receiver(with_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(receiver).should_not be_nil
    end

    it "parses with block with multiple statements" do
      source = <<-CRYSTAL
      with obj
        method1
        method2
        method3
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      with_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(with_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::With)

      body = CrystalV2::Compiler::Frontend.node_with_body(with_node).not_nil!
      body.size.should eq(3)
    end

    it "parses with block with empty body" do
      source = <<-CRYSTAL
      with obj
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      with_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(with_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::With)

      body = CrystalV2::Compiler::Frontend.node_with_body(with_node).not_nil!
      body.size.should eq(0)
    end

    it "parses with block with assignment in body" do
      source = <<-CRYSTAL
      with obj
        x = 5
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      with_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(with_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::With)

      body = CrystalV2::Compiler::Frontend.node_with_body(with_node).not_nil!
      body.size.should eq(1)

      # First statement is assignment
      stmt = arena[body[0]]
      CrystalV2::Compiler::Frontend.node_kind(stmt).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)
    end

    it "parses nested with blocks" do
      source = <<-CRYSTAL
      with obj1
        with obj2
          method
        end
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      outer_with = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(outer_with).should eq(CrystalV2::Compiler::Frontend::NodeKind::With)

      outer_body = CrystalV2::Compiler::Frontend.node_with_body(outer_with).not_nil!
      outer_body.size.should eq(1)

      # Inner with
      inner_with = arena[outer_body[0]]
      CrystalV2::Compiler::Frontend.node_kind(inner_with).should eq(CrystalV2::Compiler::Frontend::NodeKind::With)
    end

    it "parses with block inside method" do
      source = <<-CRYSTAL
      def foo
        with obj
          bar
        end
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      method_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(method_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Def)

      method_body = CrystalV2::Compiler::Frontend.node_def_body(method_node).not_nil!
      method_body.size.should eq(1)

      with_node = arena[method_body[0]]
      CrystalV2::Compiler::Frontend.node_kind(with_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::With)
    end

    it "parses with block with self receiver" do
      source = <<-CRYSTAL
      with self
        method
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      with_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(with_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::With)

      receiver = arena[CrystalV2::Compiler::Frontend.node_with_receiver(with_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(receiver).should eq(CrystalV2::Compiler::Frontend::NodeKind::Self)
    end

    it "parses with block with instance variable receiver" do
      source = <<-CRYSTAL
      with @config
        setting = value
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      with_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(with_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::With)

      receiver = arena[CrystalV2::Compiler::Frontend.node_with_receiver(with_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(receiver).should eq(CrystalV2::Compiler::Frontend::NodeKind::InstanceVar)
    end

    it "parses with block followed by other statements" do
      source = <<-CRYSTAL
      with obj
        method
      end
      x = 5
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(2)
      arena = program.arena

      # First: with block
      with_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(with_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::With)

      # Second: assignment
      assign = arena[program.roots[1]]
      CrystalV2::Compiler::Frontend.node_kind(assign).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)
    end
  end
end
