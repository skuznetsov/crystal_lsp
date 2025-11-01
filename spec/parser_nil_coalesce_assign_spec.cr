require "spec"

require "../src/compiler/frontend/parser"

describe "CrystalV2::Compiler::Frontend::Parser" do
  describe "Phase 82: Nil-coalescing compound assignment (??=)" do
    it "parses simple ??= assignment" do
      source = "x ??= 42"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(assign).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)

      # Should desugar to: x = x ?? 42
      value = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(value).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      String.new(CrystalV2::Compiler::Frontend.node_operator(value).not_nil!).should eq("??")
    end

    it "parses ??= with instance variable" do
      source = "@cache ??= expensive_computation()"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(assign).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)

      # Target should be instance variable
      target = arena[CrystalV2::Compiler::Frontend.node_assign_target(assign).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(target).should eq(CrystalV2::Compiler::Frontend::NodeKind::InstanceVar)

      # Value should be binary ??
      value = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(value).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      String.new(CrystalV2::Compiler::Frontend.node_operator(value).not_nil!).should eq("??")
    end

    it "parses ??= with class variable" do
      source = "@@config ??= load_defaults()"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(assign).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)

      # Target should be class variable
      target = arena[CrystalV2::Compiler::Frontend.node_assign_target(assign).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(target).should eq(CrystalV2::Compiler::Frontend::NodeKind::ClassVar)
    end

    it "parses ??= with global variable" do
      source = "$logger ??= Logger.new"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(assign).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)

      # Target should be global variable
      target = arena[CrystalV2::Compiler::Frontend.node_assign_target(assign).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(target).should eq(CrystalV2::Compiler::Frontend::NodeKind::Global)
    end

    it "parses ??= with hash/array index" do
      source = "config[\"key\"] ??= \"default\""

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(assign).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)

      # Target should be index
      target = arena[CrystalV2::Compiler::Frontend.node_assign_target(assign).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(target).should eq(CrystalV2::Compiler::Frontend::NodeKind::Index)
    end

    it "parses ??= with complex expression on right" do
      source = "result ??= compute() + transform(data)"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(assign).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)

      # Value should be binary ??
      value = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(value).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      String.new(CrystalV2::Compiler::Frontend.node_operator(value).not_nil!).should eq("??")

      # Right side of ?? should be addition
      right = arena[CrystalV2::Compiler::Frontend.node_right(value).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(right).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      String.new(CrystalV2::Compiler::Frontend.node_operator(right).not_nil!).should eq("+")
    end

    it "parses multiple ??= statements" do
      source = <<-CRYSTAL
      @cache ??= {}
      @counter ??= 0
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

    it "parses ??= inside method" do
      source = <<-CRYSTAL
      def get_config
        @config ??= load_config()
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      method_def = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(method_def).should eq(CrystalV2::Compiler::Frontend::NodeKind::Def)

      # Body should contain assignment
      body_exprs = CrystalV2::Compiler::Frontend.node_def_body(method_def).not_nil!
      body_exprs.size.should eq(1)

      body = arena[body_exprs[0]]
      CrystalV2::Compiler::Frontend.node_kind(body).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)
    end

    it "disambiguates ??= from ?? and ? and =" do
      source = <<-CRYSTAL
      a = b ? c : d
      e = f ?? g
      h ??= i
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(3)
      arena = program.arena

      # First: b ? c : d (ternary)
      assign1 = arena[program.roots[0]]
      value1 = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign1).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(value1).should eq(CrystalV2::Compiler::Frontend::NodeKind::Ternary)

      # Second: f ?? g (nil-coalescing)
      assign2 = arena[program.roots[1]]
      value2 = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign2).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(value2).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      String.new(CrystalV2::Compiler::Frontend.node_operator(value2).not_nil!).should eq("??")

      # Third: h ??= i (compound assignment)
      assign3 = arena[program.roots[2]]
      value3 = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign3).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(value3).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      String.new(CrystalV2::Compiler::Frontend.node_operator(value3).not_nil!).should eq("??")
      # Left side of ?? should reference 'h'
      left3 = arena[CrystalV2::Compiler::Frontend.node_left(value3).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(left3).should eq(CrystalV2::Compiler::Frontend::NodeKind::Identifier)
    end

    it "parses ??= with literal nil on right" do
      source = "value ??= nil"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(assign).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)

      # Desugar to: value = value ?? nil
      value = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(value).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      String.new(CrystalV2::Compiler::Frontend.node_operator(value).not_nil!).should eq("??")

      # Right side should be nil
      right = arena[CrystalV2::Compiler::Frontend.node_right(value).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(right).should eq(CrystalV2::Compiler::Frontend::NodeKind::Nil)
    end
  end
end
