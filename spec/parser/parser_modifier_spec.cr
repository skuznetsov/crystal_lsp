require "spec"

require "../../src/compiler/frontend/parser"

describe "CrystalV2::Compiler::Frontend::Parser" do
  describe "Phase 26: Modifier if/unless" do
    it "parses postfix if modifier with return" do
      source = <<-CRYSTAL
        return 10 if condition
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      # Root should be an if node
      if_node = arena[program.roots.first]
      CrystalV2::Compiler::Frontend.node_kind(if_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::If)

      # Condition should be identifier "condition"
      condition = arena[CrystalV2::Compiler::Frontend.node_condition(if_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(condition).should eq(CrystalV2::Compiler::Frontend::NodeKind::Identifier)

      # Then body should contain return statement
      then_body = CrystalV2::Compiler::Frontend.node_if_then(if_node).not_nil!
      then_body.size.should eq(1)

      return_stmt = arena[then_body[0]]
      CrystalV2::Compiler::Frontend.node_kind(return_stmt).should eq(CrystalV2::Compiler::Frontend::NodeKind::Return)
    end

    it "parses postfix unless modifier with return" do
      source = <<-CRYSTAL
        return 20 unless valid
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      # Root should be an unless node
      unless_node = arena[program.roots.first]
      CrystalV2::Compiler::Frontend.node_kind(unless_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Unless)

      # Condition should be identifier "valid"
      condition = arena[CrystalV2::Compiler::Frontend.node_condition(unless_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(condition).should eq(CrystalV2::Compiler::Frontend::NodeKind::Identifier)

      # Then body should contain return statement
      then_body = CrystalV2::Compiler::Frontend.node_if_then(unless_node).not_nil!
      then_body.size.should eq(1)

      return_stmt = arena[then_body[0]]
      CrystalV2::Compiler::Frontend.node_kind(return_stmt).should eq(CrystalV2::Compiler::Frontend::NodeKind::Return)
    end

    it "parses postfix if modifier with break" do
      source = <<-CRYSTAL
        break if done
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      if_node = arena[program.roots.first]
      CrystalV2::Compiler::Frontend.node_kind(if_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::If)

      then_body = CrystalV2::Compiler::Frontend.node_if_then(if_node).not_nil!
      then_body.size.should eq(1)

      break_stmt = arena[then_body[0]]
      CrystalV2::Compiler::Frontend.node_kind(break_stmt).should eq(CrystalV2::Compiler::Frontend::NodeKind::Break)
    end

    it "parses postfix unless modifier with next" do
      source = <<-CRYSTAL
        next unless ready
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      unless_node = arena[program.roots.first]
      CrystalV2::Compiler::Frontend.node_kind(unless_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Unless)

      then_body = CrystalV2::Compiler::Frontend.node_if_then(unless_node).not_nil!
      then_body.size.should eq(1)

      next_stmt = arena[then_body[0]]
      CrystalV2::Compiler::Frontend.node_kind(next_stmt).should eq(CrystalV2::Compiler::Frontend::NodeKind::Next)
    end

    it "parses postfix if modifier with assignment" do
      source = <<-CRYSTAL
        x = 10 if condition
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      if_node = arena[program.roots.first]
      CrystalV2::Compiler::Frontend.node_kind(if_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::If)

      then_body = CrystalV2::Compiler::Frontend.node_if_then(if_node).not_nil!
      then_body.size.should eq(1)

      assign = arena[then_body[0]]
      CrystalV2::Compiler::Frontend.node_kind(assign).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)
    end

    it "parses postfix unless modifier with assignment" do
      source = <<-CRYSTAL
        x = 20 unless initialized
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      unless_node = arena[program.roots.first]
      CrystalV2::Compiler::Frontend.node_kind(unless_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Unless)

      then_body = CrystalV2::Compiler::Frontend.node_if_then(unless_node).not_nil!
      then_body.size.should eq(1)

      assign = arena[then_body[0]]
      CrystalV2::Compiler::Frontend.node_kind(assign).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)
    end

    it "parses postfix if modifier with method call" do
      source = <<-CRYSTAL
        puts(x) if debug
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      if_node = arena[program.roots.first]
      CrystalV2::Compiler::Frontend.node_kind(if_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::If)

      then_body = CrystalV2::Compiler::Frontend.node_if_then(if_node).not_nil!
      then_body.size.should eq(1)

      call = arena[then_body[0]]
      CrystalV2::Compiler::Frontend.node_kind(call).should eq(CrystalV2::Compiler::Frontend::NodeKind::Call)
    end

    it "parses postfix if with complex condition" do
      source = <<-CRYSTAL
        return if x > 10 && y < 20
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      if_node = arena[program.roots.first]
      CrystalV2::Compiler::Frontend.node_kind(if_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::If)

      # Condition should be binary AND
      condition = arena[CrystalV2::Compiler::Frontend.node_condition(if_node).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(condition).should eq(CrystalV2::Compiler::Frontend::NodeKind::Binary)
      String.new(CrystalV2::Compiler::Frontend.node_operator(condition).not_nil!).should eq("&&")
    end

    it "handles statement without modifier" do
      source = <<-CRYSTAL
        x = 10
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      # Should be plain assignment, not wrapped in if/unless
      assign = arena[program.roots.first]
      CrystalV2::Compiler::Frontend.node_kind(assign).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)
    end
  end
end
