require "spec"

require "../../src/compiler/frontend/parser"

describe "CrystalV2::Compiler::Frontend::Parser" do
  describe "inline asm parsing" do
    it "parses asm with template only" do
      source = %(asm("nop"))
      parser = CrystalV2::Compiler::Frontend::Parser.new(
        CrystalV2::Compiler::Frontend::Lexer.new(source)
      )
      program = parser.parse_program

      parser.diagnostics.should be_empty
      program.roots.size.should eq(1)

      arena = program.arena
      asm_node = arena[program.roots.first].as(CrystalV2::Compiler::Frontend::AsmNode)
      CrystalV2::Compiler::Frontend.node_kind(asm_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Asm)

      args = CrystalV2::Compiler::Frontend.node_asm_args(asm_node)
      args.size.should eq(1)

      template = arena[args.first].as(CrystalV2::Compiler::Frontend::StringNode)
      String.new(template.value).should eq("nop")
    end

    # NOTE: This test uses invalid Crystal ASM syntax. Crystal ASM uses colons:
    #   asm("template" : outputs : inputs : clobbers : options)
    # Not comma-separated arguments like asm("add", 1, 2)
    pending "parses asm with multiple operands (INVALID SYNTAX)" do
      source = %(asm("add", 1, 2))
      parser = CrystalV2::Compiler::Frontend::Parser.new(
        CrystalV2::Compiler::Frontend::Lexer.new(source)
      )
      program = parser.parse_program

      parser.diagnostics.should be_empty
      program.roots.size.should eq(1)

      arena = program.arena
      asm_node = arena[program.roots.first].as(CrystalV2::Compiler::Frontend::AsmNode)
      CrystalV2::Compiler::Frontend.node_kind(asm_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Asm)

      args = CrystalV2::Compiler::Frontend.node_asm_args(asm_node)
      args.size.should eq(3)

      template = arena[args[0]].as(CrystalV2::Compiler::Frontend::StringNode)
      CrystalV2::Compiler::Frontend.node_kind(template).should eq(CrystalV2::Compiler::Frontend::NodeKind::String)
      String.new(template.value).should eq("add")

      first_operand = arena[args[1]].as(CrystalV2::Compiler::Frontend::NumberNode)
      CrystalV2::Compiler::Frontend.node_kind(first_operand).should eq(CrystalV2::Compiler::Frontend::NodeKind::Number)

      second_operand = arena[args[2]].as(CrystalV2::Compiler::Frontend::NumberNode)
      CrystalV2::Compiler::Frontend.node_kind(second_operand).should eq(CrystalV2::Compiler::Frontend::NodeKind::Number)
    end
  end
end
