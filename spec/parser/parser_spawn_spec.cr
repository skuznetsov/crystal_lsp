require "spec"

require "../../src/compiler/frontend/parser"

describe "CrystalV2::Compiler::Frontend::Parser" do
  describe "Phase 84: Spawn keyword (fiber concurrency)" do
    it "parses spawn do...end block form" do
      source = <<-CRYSTAL
      spawn do
        puts("hello")
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      spawn_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(spawn_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Spawn)

      # Block form should have spawn_body
      body = CrystalV2::Compiler::Frontend.node_spawn_body(spawn_node).not_nil!
      body.size.should eq(1)

      # No spawn_expression in block form
      CrystalV2::Compiler::Frontend.node_spawn_expression(spawn_node).should be_nil
    end

    it "parses spawn expression form" do
      source = "spawn process_task()"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      spawn_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(spawn_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Spawn)

      # Expression form should have spawn_expression
      expr = CrystalV2::Compiler::Frontend.node_spawn_expression(spawn_node).not_nil!
      expr_node = arena[expr]
      CrystalV2::Compiler::Frontend.node_kind(expr_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Call)

      # No spawn_body in expression form
      CrystalV2::Compiler::Frontend.node_spawn_body(spawn_node).should be_nil
    end

    it "parses spawn block with multiple statements" do
      source = <<-CRYSTAL
      spawn do
        x = compute()
        y = transform(x)
        output(y)
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      spawn_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(spawn_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Spawn)

      body = CrystalV2::Compiler::Frontend.node_spawn_body(spawn_node).not_nil!
      body.size.should eq(3)
    end

    it "parses spawn inside method definition" do
      source = <<-CRYSTAL
      def async_operation
        spawn do
          perform_work()
        end
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      method_def = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(method_def).should eq(CrystalV2::Compiler::Frontend::NodeKind::Def)

      # Method body should contain spawn
      body_exprs = CrystalV2::Compiler::Frontend.node_def_body(method_def).not_nil!
      body_exprs.size.should eq(1)

      spawn_node = arena[body_exprs[0]]
      CrystalV2::Compiler::Frontend.node_kind(spawn_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Spawn)
    end

    it "parses spawn with assignment result" do
      source = "result = spawn worker.process()"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      assign = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(assign).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)

      # Assignment value should be spawn
      value = arena[CrystalV2::Compiler::Frontend.node_assign_value(assign).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(value).should eq(CrystalV2::Compiler::Frontend::NodeKind::Spawn)
    end

    it "parses nested spawn blocks" do
      source = <<-CRYSTAL
      spawn do
        spawn do
          inner_work()
        end
        outer_work()
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      outer_spawn = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(outer_spawn).should eq(CrystalV2::Compiler::Frontend::NodeKind::Spawn)

      outer_body = CrystalV2::Compiler::Frontend.node_spawn_body(outer_spawn).not_nil!
      outer_body.size.should eq(2)

      # First statement should be inner spawn
      inner_spawn = arena[outer_body[0]]
      CrystalV2::Compiler::Frontend.node_kind(inner_spawn).should eq(CrystalV2::Compiler::Frontend::NodeKind::Spawn)
    end

    it "parses spawn block with control flow" do
      source = <<-CRYSTAL
      spawn do
        if condition
          action()
        end
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      spawn_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(spawn_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Spawn)

      body = CrystalV2::Compiler::Frontend.node_spawn_body(spawn_node).not_nil!
      body.size.should eq(1)

      # Body should contain if statement
      if_stmt = arena[body[0]]
      CrystalV2::Compiler::Frontend.node_kind(if_stmt).should eq(CrystalV2::Compiler::Frontend::NodeKind::If)
    end

    it "parses empty spawn block" do
      source = <<-CRYSTAL
      spawn do
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      spawn_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(spawn_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Spawn)

      # Empty body
      body = CrystalV2::Compiler::Frontend.node_spawn_body(spawn_node).not_nil!
      body.size.should eq(0)
    end

    it "parses spawn with complex expression" do
      source = "spawn server.handle_request(client.connection, timeout: 30)"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      spawn_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(spawn_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Spawn)

      # Should have expression (method call with receiver and arguments)
      expr = CrystalV2::Compiler::Frontend.node_spawn_expression(spawn_node).not_nil!
      expr_node = arena[expr]
      CrystalV2::Compiler::Frontend.node_kind(expr_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Call)
    end

    it "parses multiple spawn statements" do
      source = <<-CRYSTAL
      spawn worker1.start()
      spawn worker2.start()
      spawn do
        worker3.start()
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(3)
      arena = program.arena

      # All three should be spawn nodes
      spawn1 = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(spawn1).should eq(CrystalV2::Compiler::Frontend::NodeKind::Spawn)

      spawn2 = arena[program.roots[1]]
      CrystalV2::Compiler::Frontend.node_kind(spawn2).should eq(CrystalV2::Compiler::Frontend::NodeKind::Spawn)

      spawn3 = arena[program.roots[2]]
      CrystalV2::Compiler::Frontend.node_kind(spawn3).should eq(CrystalV2::Compiler::Frontend::NodeKind::Spawn)
    end
  end
end
