require "spec"

require "../src/compiler/frontend/parser"

private def fetch_class(program)
  program.arena[program.roots.first].as(CrystalV2::Compiler::Frontend::ClassNode)
end

private def fetch_method(arena, expr_id)
  arena[expr_id].as(CrystalV2::Compiler::Frontend::DefNode)
end

private def fetch_super(arena, method_node)
  body = method_node.body.not_nil!
  arena[body.first].as(CrystalV2::Compiler::Frontend::SuperNode)
end

describe "CrystalV2::Compiler::Frontend::Parser" do
  describe "Phase 39: super keyword (PRODUCTION-READY)" do
    it "parses super without parentheses (implicit args)" do
      source = <<-CRYSTAL
      class Child < Parent
        def foo
          super
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
      super_node = fetch_super(arena, method_node)

      super_node.args.should be_nil  # nil = implicit args
    end

    it "parses super with empty parentheses (explicit no args)" do
      source = <<-CRYSTAL
      class Child < Parent
        def foo
          super()
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
      super_node = fetch_super(arena, method_node)

      args = super_node.args.not_nil!
      args.size.should eq(0)  # Empty array = explicit no args
    end

    it "parses super with single argument" do
      source = <<-CRYSTAL
      class Child < Parent
        def foo(x)
          super(x + 1)
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
      super_node = fetch_super(arena, method_node)

      args = super_node.args.not_nil!
      args.size.should eq(1)

      # Check argument is a binary expression (x + 1)
      arg_node = arena[args[0]].as(CrystalV2::Compiler::Frontend::BinaryNode)
    end

    it "parses super with multiple arguments" do
      source = <<-CRYSTAL
      class Child < Parent
        def foo(x, y)
          super(x, y + 1)
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
      super_node = fetch_super(arena, method_node)

      args = super_node.args.not_nil!
      args.size.should eq(2)
    end

    it "parses super with postfix if modifier" do
      source = <<-CRYSTAL
      class Child < Parent
        def foo(x)
          super if x > 0
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

      # Then branch should contain super
      if_then = if_node.then_body
      super_node = arena[if_then.first].as(CrystalV2::Compiler::Frontend::SuperNode)
    end

    it "parses super in multiple methods" do
      source = <<-CRYSTAL
      class Child < Parent
        def foo
          super
        end

        def bar(x)
          super(x)
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

      # First method: super without args
      method1 = fetch_method(arena, class_body[0])
      super1 = fetch_super(arena, method1)
      super1.args.should be_nil

      # Second method: super with args
      method2 = fetch_method(arena, class_body[1])
      super2 = fetch_super(arena, method2)
      args = super2.args.not_nil!
      args.size.should eq(1)
    end

    it "parses super before other statements" do
      source = <<-CRYSTAL
      class Child < Parent
        def foo
          super
          puts "after super"
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

      # First statement is super
      super_node = arena[method_body[0]].as(CrystalV2::Compiler::Frontend::SuperNode)

      # Find the method call (might not be immediately after due to parsing)
      # Just verify super is first and there are other statements
      method_body.size.should be > 1
    end

    it "parses super with complex expressions as arguments" do
      source = <<-CRYSTAL
      class Child < Parent
        def foo(x, y)
          super(x * 2 + 1, y > 0 ? y : 0)
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
      super_node = fetch_super(arena, method_node)

      args = super_node.args.not_nil!
      args.size.should eq(2)

      # First arg is binary expression
      arg1 = arena[args[0]].as(CrystalV2::Compiler::Frontend::BinaryNode)

      # Second arg is ternary expression
      arena[args[1]].as(CrystalV2::Compiler::Frontend::TernaryNode)
    end

    it "distinguishes super(), super and super(args)" do
      source = <<-CRYSTAL
      class Child < Parent
        def foo
          super
        end

        def bar
          super()
        end

        def baz(x)
          super(x)
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

      # Method foo: super (nil = implicit args)
      method_foo = fetch_method(arena, class_body[0])
      super_foo = fetch_super(arena, method_foo)
      super_foo.args.should be_nil

      # Method bar: super() (empty array = explicit no args)
      method_bar = fetch_method(arena, class_body[1])
      super_bar = fetch_super(arena, method_bar)
      args_bar = super_bar.args.not_nil!
      args_bar.size.should eq(0)

      # Method baz: super(x) (array with args)
      method_baz = fetch_method(arena, class_body[2])
      super_baz = fetch_super(arena, method_baz)
      args_baz = super_baz.args.not_nil!
      args_baz.size.should eq(1)
    end
  end
end
