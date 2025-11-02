require "spec"

require "../../src/compiler/frontend/parser"

describe "CrystalV2::Compiler::Frontend::Parser" do
  describe "Phase 85: Uninitialized keyword (uninitialized variables)" do
    it "parses uninitialized with simple type" do
      source = "x = uninitialized(Int32)"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(assign).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)

      # Value should be uninitialized
      value = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(value).should eq(CrystalV2::Compiler::Frontend::NodeKind::Uninitialized)

      # Type should be Int32 (identifier)
      type_expr = arena[CrystalV2::Compiler::Frontend.node_uninitialized_type(value).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(type_expr).should eq(CrystalV2::Compiler::Frontend::NodeKind::Identifier)
      String.new(CrystalV2::Compiler::Frontend.node_literal(type_expr).not_nil!).should eq("Int32")
    end

    it "parses uninitialized with pointer type" do
      source = "ptr = uninitialized(Pointer(UInt8))"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(assign).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)

      value = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(value).should eq(CrystalV2::Compiler::Frontend::NodeKind::Uninitialized)

      # Type should be Pointer(UInt8) - generic instantiation
      type_expr = arena[CrystalV2::Compiler::Frontend.node_uninitialized_type(value).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(type_expr).should eq(CrystalV2::Compiler::Frontend::NodeKind::Generic)
    end

    it "parses uninitialized with custom type" do
      source = "obj = uninitialized(MyClass)"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      value = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(value).should eq(CrystalV2::Compiler::Frontend::NodeKind::Uninitialized)

      type_expr = arena[CrystalV2::Compiler::Frontend.node_uninitialized_type(value).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(type_expr).should eq(CrystalV2::Compiler::Frontend::NodeKind::Identifier)
      String.new(CrystalV2::Compiler::Frontend.node_literal(type_expr).not_nil!).should eq("MyClass")
    end

    it "parses uninitialized with array type" do
      source = "arr = uninitialized(Array(Int32))"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      value = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(value).should eq(CrystalV2::Compiler::Frontend::NodeKind::Uninitialized)

      # Type is Array(Int32) - generic instantiation
      type_expr = arena[CrystalV2::Compiler::Frontend.node_uninitialized_type(value).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(type_expr).should eq(CrystalV2::Compiler::Frontend::NodeKind::Generic)
    end

    it "parses uninitialized inside method" do
      source = <<-CRYSTAL
      def allocate_buffer
        buffer = uninitialized(UInt8)
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      method_def = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(method_def).should eq(CrystalV2::Compiler::Frontend::NodeKind::Def)

      # Method body should contain assignment
      body_exprs = CrystalV2::Compiler::Frontend.node_def_body(method_def).not_nil!
      body_exprs.size.should eq(1)

      assign = arena[body_exprs[0]]
      CrystalV2::Compiler::Frontend.node_kind(assign).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)

      value = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(value).should eq(CrystalV2::Compiler::Frontend::NodeKind::Uninitialized)
    end

    it "parses multiple uninitialized statements" do
      source = <<-CRYSTAL
      x = uninitialized(Int32)
      y = uninitialized(Int64)
      z = uninitialized(String)
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(3)
      arena = program.arena

      # All three should be assignments with uninitialized values
      assign1 = arena[program.roots[0]]
      value1 = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign1).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(value1).should eq(CrystalV2::Compiler::Frontend::NodeKind::Uninitialized)

      assign2 = arena[program.roots[1]]
      value2 = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign2).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(value2).should eq(CrystalV2::Compiler::Frontend::NodeKind::Uninitialized)

      assign3 = arena[program.roots[2]]
      value3 = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign3).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(value3).should eq(CrystalV2::Compiler::Frontend::NodeKind::Uninitialized)
    end

    it "parses uninitialized with union type" do
      source = "val = uninitialized(Int32 | String)"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      value = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(value).should eq(CrystalV2::Compiler::Frontend::NodeKind::Uninitialized)

      # Type is Int32 | String - binary expression with |
      type_expr = arena[CrystalV2::Compiler::Frontend.node_uninitialized_type(value).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(type_expr).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
    end

    it "parses uninitialized as method argument" do
      source = "process(uninitialized(Buffer))"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      call = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(call).should eq(CrystalV2::Compiler::Frontend::NodeKind::Call)

      # Argument should be uninitialized
      args = CrystalV2::Compiler::Frontend.node_args(call).not_nil!
      args.size.should eq(1)

      arg = arena[args[0]]
      CrystalV2::Compiler::Frontend.node_kind(arg).should eq(CrystalV2::Compiler::Frontend::NodeKind::Uninitialized)
    end

    it "parses uninitialized in array literal" do
      source = "[uninitialized(Int32), uninitialized(Int32)]"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      array = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(array).should eq(CrystalV2::Compiler::Frontend::NodeKind::ArrayLiteral)

      # Both elements should be uninitialized
      elements = CrystalV2::Compiler::Frontend.node_array_elements(array).not_nil!
      elements.size.should eq(2)

      elem1 = arena[elements[0]]
      CrystalV2::Compiler::Frontend.node_kind(elem1).should eq(CrystalV2::Compiler::Frontend::NodeKind::Uninitialized)

      elem2 = arena[elements[1]]
      CrystalV2::Compiler::Frontend.node_kind(elem2).should eq(CrystalV2::Compiler::Frontend::NodeKind::Uninitialized)
    end
  end
end
