require "spec"

require "../../src/compiler/frontend/parser"

private def fetch_class(program)
  program.arena[program.roots.first].as(CrystalV2::Compiler::Frontend::ClassNode)
end

private def fetch_method(arena, expr_id)
  arena[expr_id].as(CrystalV2::Compiler::Frontend::DefNode)
end

private def fetch_previous_def(arena, method_node)
  body = method_node.body.not_nil!
  arena[body.first].as(CrystalV2::Compiler::Frontend::PreviousDefNode)
end

describe "CrystalV2::Compiler::Frontend::Parser" do
  describe "Phase 96: previous_def keyword" do
    it "parses previous_def without parentheses (implicit args)" do
      source = <<-CRYSTAL
      class Foo
        def bar
          previous_def
        end
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena
      class_node = fetch_class(program)
      class_body = class_node.body.not_nil!
      method_node = fetch_method(arena, class_body[0])
      previous_def_node = fetch_previous_def(arena, method_node)

      previous_def_node.args.should be_nil  # nil = implicit args
    end

    it "parses previous_def with empty parentheses (explicit no args)" do
      source = <<-CRYSTAL
      class Foo
        def bar
          previous_def()
        end
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena
      class_node = fetch_class(program)
      class_body = class_node.body.not_nil!
      method_node = fetch_method(arena, class_body[0])
      previous_def_node = fetch_previous_def(arena, method_node)

      args = previous_def_node.args.not_nil!
      args.size.should eq(0)  # Empty array = explicit no args
    end

    it "parses previous_def with single argument" do
      source = <<-CRYSTAL
      class Foo
        def bar(x)
          previous_def(x + 1)
        end
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena
      class_node = fetch_class(program)
      class_body = class_node.body.not_nil!
      method_node = fetch_method(arena, class_body[0])
      previous_def_node = fetch_previous_def(arena, method_node)

      args = previous_def_node.args.not_nil!
      args.size.should eq(1)

      # Check argument is a binary expression (x + 1)
      arena[args[0]].as(CrystalV2::Compiler::Frontend::BinaryNode)
    end

    it "parses previous_def with multiple arguments" do
      source = <<-CRYSTAL
      class Foo
        def bar(x, y)
          previous_def(x, y + 1)
        end
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena
      class_node = fetch_class(program)
      class_body = class_node.body.not_nil!
      method_node = fetch_method(arena, class_body[0])
      previous_def_node = fetch_previous_def(arena, method_node)

      args = previous_def_node.args.not_nil!
      args.size.should eq(2)
    end

    it "parses previous_def with postfix if modifier" do
      source = <<-CRYSTAL
      class Foo
        def bar(x)
          previous_def if x > 0
        end
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena
      class_node = fetch_class(program)
      class_body = class_node.body.not_nil!
      method_node = fetch_method(arena, class_body[0])

      method_body = method_node.body.not_nil!
      if_node = arena[method_body[0]].as(CrystalV2::Compiler::Frontend::IfNode)

      # Then branch should contain previous_def
      if_then = if_node.then_body
      arena[if_then.first].as(CrystalV2::Compiler::Frontend::PreviousDefNode)
    end

    it "parses previous_def in multiple methods" do
      source = <<-CRYSTAL
      class Foo
        def bar
          previous_def
        end

        def baz(x)
          previous_def(x)
        end
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena
      class_node = fetch_class(program)

      class_body = class_node.body.not_nil!
      class_body.size.should eq(2)

      # First method: previous_def without args
      method1 = fetch_method(arena, class_body[0])
      previous_def1 = fetch_previous_def(arena, method1)
      previous_def1.args.should be_nil

      # Second method: previous_def with args
      method2 = fetch_method(arena, class_body[1])
      previous_def2 = fetch_previous_def(arena, method2)
      args = previous_def2.args.not_nil!
      args.size.should eq(1)
    end

    it "parses previous_def before other statements" do
      source = <<-CRYSTAL
      class Foo
        def bar
          previous_def
          puts "after previous_def"
        end
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena
      class_node = fetch_class(program)
      class_body = class_node.body.not_nil!
      method_node = fetch_method(arena, class_body[0])

      method_body = method_node.body.not_nil!
      method_body.size.should be >= 2

      # First statement is previous_def
      arena[method_body[0]].as(CrystalV2::Compiler::Frontend::PreviousDefNode)

      # Verify there are other statements
      method_body.size.should be > 1
    end

    it "parses previous_def with complex expressions as arguments" do
      source = <<-CRYSTAL
      class Foo
        def bar(x, y)
          previous_def(x * 2 + 1, y > 0 ? y : 0)
        end
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena
      class_node = fetch_class(program)
      class_body = class_node.body.not_nil!
      method_node = fetch_method(arena, class_body[0])
      previous_def_node = fetch_previous_def(arena, method_node)

      args = previous_def_node.args.not_nil!
      args.size.should eq(2)

      # First arg is binary expression
      arena[args[0]].as(CrystalV2::Compiler::Frontend::BinaryNode)

      # Second arg is ternary expression
      arena[args[1]].as(CrystalV2::Compiler::Frontend::TernaryNode)
    end

    it "distinguishes previous_def(), previous_def and previous_def(args)" do
      source = <<-CRYSTAL
      class Foo
        def bar
          previous_def
        end

        def baz
          previous_def()
        end

        def qux(x)
          previous_def(x)
        end
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena
      class_node = fetch_class(program)

      class_body = class_node.body.not_nil!
      class_body.size.should eq(3)

      # Method bar: previous_def (nil = implicit args)
      method_bar = fetch_method(arena, class_body[0])
      previous_def_bar = fetch_previous_def(arena, method_bar)
      previous_def_bar.args.should be_nil

      # Method baz: previous_def() (empty array = explicit no args)
      method_baz = fetch_method(arena, class_body[1])
      previous_def_baz = fetch_previous_def(arena, method_baz)
      args_baz = previous_def_baz.args.not_nil!
      args_baz.size.should eq(0)

      # Method qux: previous_def(x) (array with args)
      method_qux = fetch_method(arena, class_body[2])
      previous_def_qux = fetch_previous_def(arena, method_qux)
      args_qux = previous_def_qux.args.not_nil!
      args_qux.size.should eq(1)
    end
  end
end
