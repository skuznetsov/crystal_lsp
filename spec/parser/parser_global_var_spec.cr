require "spec"

require "../../src/compiler/frontend/parser"

describe "CrystalV2::Compiler::Frontend::Parser" do
  describe "Phase 75: Global variables ($var) (PRODUCTION-READY)" do
    it "parses simple global variable" do
      source = "$global_var"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      global_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(global_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Global)

      literal = CrystalV2::Compiler::Frontend.node_literal(global_node).not_nil!
      String.new(literal).should eq("$global_var")
    end

    it "parses global variable with underscores" do
      source = "$my_global_var"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      global_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(global_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Global)

      literal = CrystalV2::Compiler::Frontend.node_literal(global_node).not_nil!
      String.new(literal).should eq("$my_global_var")
    end

    it "parses global variable with question mark suffix" do
      source = "$flag?"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      global_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(global_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Global)

      literal = CrystalV2::Compiler::Frontend.node_literal(global_node).not_nil!
      String.new(literal).should eq("$flag?")
    end

    it "parses global variable with exclamation mark suffix" do
      source = "$important!"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      global_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(global_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Global)

      literal = CrystalV2::Compiler::Frontend.node_literal(global_node).not_nil!
      String.new(literal).should eq("$important!")
    end

    it "parses global variable assignment" do
      source = "$count = 42"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(assign_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)

      target = arena[CrystalV2::Compiler::Frontend.node_assign_target(assign_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(target).should eq(CrystalV2::Compiler::Frontend::NodeKind::Global)
    end

    it "parses multiple global variables" do
      source = <<-CRYSTAL
      $first = 1
      $second = 2
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(2)
      arena = program.arena

      assign1 = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(assign1).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)

      assign2 = arena[program.roots[1]]
      CrystalV2::Compiler::Frontend.node_kind(assign2).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)
    end

    it "parses global variable in expression" do
      source = "$count + 1"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      binary = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(binary).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)

      left = arena[CrystalV2::Compiler::Frontend.node_left(binary).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(left).should eq(CrystalV2::Compiler::Frontend::NodeKind::Global)
    end

    it "parses global variable as method argument" do
      source = "foo($global)"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      call = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(call).should eq(CrystalV2::Compiler::Frontend::NodeKind::Call)

      args = CrystalV2::Compiler::Frontend.node_args(call).not_nil!
      args.size.should eq(1)

      arg = arena[args[0]]
      CrystalV2::Compiler::Frontend.node_kind(arg).should eq(CrystalV2::Compiler::Frontend::NodeKind::Global)
    end

    it "parses global variable inside method body" do
      source = <<-CRYSTAL
      def foo
        $global_var = 42
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      method = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(method).should eq(CrystalV2::Compiler::Frontend::NodeKind::Def)

      body = CrystalV2::Compiler::Frontend.node_def_body(method).not_nil!
      body.size.should eq(1)

      assign = arena[body[0]]
      CrystalV2::Compiler::Frontend.node_kind(assign).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)
    end

    it "parses global variable in array literal" do
      source = "[$first, $second, $third]"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      array = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(array).should eq(CrystalV2::Compiler::Frontend::NodeKind::ArrayLiteral)

      elements = CrystalV2::Compiler::Frontend.node_array_elements(array).not_nil!
      elements.size.should eq(3)

      elem1 = arena[elements[0]]
      CrystalV2::Compiler::Frontend.node_kind(elem1).should eq(CrystalV2::Compiler::Frontend::NodeKind::Global)
    end

    it "distinguishes global variable from instance variable" do
      source = "$global + @instance"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      binary = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(binary).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)

      left = arena[CrystalV2::Compiler::Frontend.node_left(binary).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(left).should eq(CrystalV2::Compiler::Frontend::NodeKind::Global)

      right = arena[CrystalV2::Compiler::Frontend.node_right(binary).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(right).should eq(CrystalV2::Compiler::Frontend::NodeKind::InstanceVar)
    end

    it "parses global variable with numbers in name" do
      source = "$var123"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      global_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(global_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Global)

      literal = CrystalV2::Compiler::Frontend.node_literal(global_node).not_nil!
      String.new(literal).should eq("$var123")
    end
  end
end
