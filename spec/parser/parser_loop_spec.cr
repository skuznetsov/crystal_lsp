require "spec"

require "../../src/compiler/frontend/parser"

describe "CrystalV2::Compiler::Frontend::Parser" do
  describe "Phase 83: Loop keyword (infinite loop)" do
    it "parses simple loop with single statement" do
      source = <<-CRYSTAL
      loop do
        puts("hello")
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      loop_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(loop_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Loop)

      # Body should contain one statement
      body = CrystalV2::Compiler::Frontend.node_loop_body(loop_node).not_nil!
      body.size.should eq(1)

      stmt = arena[body[0]]
      CrystalV2::Compiler::Frontend.node_kind(stmt).should eq(CrystalV2::Compiler::Frontend::NodeKind::Call)
    end

    it "parses loop with break statement" do
      source = <<-CRYSTAL
      loop do
        counter = counter + 1
        break if counter > 10
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      loop_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(loop_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Loop)

      # Body should contain two statements (assignment and break-if)
      body = CrystalV2::Compiler::Frontend.node_loop_body(loop_node).not_nil!
      body.size.should eq(2)
    end

    it "parses loop with next statement" do
      source = <<-CRYSTAL
      loop do
        next if skip
        process_item()
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      loop_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(loop_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Loop)

      body = CrystalV2::Compiler::Frontend.node_loop_body(loop_node).not_nil!
      body.size.should eq(2)

      # First statement should be if (suffix if parses as If node)
      if_stmt = arena[body[0]]
      CrystalV2::Compiler::Frontend.node_kind(if_stmt).should eq(CrystalV2::Compiler::Frontend::NodeKind::If)
    end

    it "parses loop with multiple statements" do
      source = <<-CRYSTAL
      loop do
        x = read_input()
        y = process(x)
        z = transform(y)
        output(z)
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      loop_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(loop_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Loop)

      body = CrystalV2::Compiler::Frontend.node_loop_body(loop_node).not_nil!
      body.size.should eq(4)
    end

    it "parses nested loops" do
      source = <<-CRYSTAL
      loop do
        loop do
          inner_work()
        end
        outer_work()
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      outer_loop = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(outer_loop).should eq(CrystalV2::Compiler::Frontend::NodeKind::Loop)

      outer_body = CrystalV2::Compiler::Frontend.node_loop_body(outer_loop).not_nil!
      outer_body.size.should eq(2)

      # First statement in outer body should be inner loop
      inner_loop = arena[outer_body[0]]
      CrystalV2::Compiler::Frontend.node_kind(inner_loop).should eq(CrystalV2::Compiler::Frontend::NodeKind::Loop)

      inner_body = CrystalV2::Compiler::Frontend.node_loop_body(inner_loop).not_nil!
      inner_body.size.should eq(1)
    end

    it "parses loop with return statement" do
      source = <<-CRYSTAL
      loop do
        return result if found
        continue_searching()
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      loop_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(loop_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Loop)

      body = CrystalV2::Compiler::Frontend.node_loop_body(loop_node).not_nil!
      body.size.should eq(2)

      # First statement should be if (suffix if parses as If node)
      if_stmt = arena[body[0]]
      CrystalV2::Compiler::Frontend.node_kind(if_stmt).should eq(CrystalV2::Compiler::Frontend::NodeKind::If)
    end

    it "parses loop inside method definition" do
      source = <<-CRYSTAL
      def server_loop
        loop do
          handle_request()
        end
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      method_def = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(method_def).should eq(CrystalV2::Compiler::Frontend::NodeKind::Def)

      # Method body should contain loop
      body_exprs = CrystalV2::Compiler::Frontend.node_def_body(method_def).not_nil!
      body_exprs.size.should eq(1)

      loop_node = arena[body_exprs[0]]
      CrystalV2::Compiler::Frontend.node_kind(loop_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Loop)
    end

    it "parses loop with if/else conditions inside" do
      source = <<-CRYSTAL
      loop do
        if condition
          action_a()
        else
          action_b()
        end
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      loop_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(loop_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Loop)

      body = CrystalV2::Compiler::Frontend.node_loop_body(loop_node).not_nil!
      body.size.should eq(1)

      # Body should contain if statement
      if_stmt = arena[body[0]]
      CrystalV2::Compiler::Frontend.node_kind(if_stmt).should eq(CrystalV2::Compiler::Frontend::NodeKind::If)
    end

    it "parses empty loop" do
      source = <<-CRYSTAL
      loop do
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      loop_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(loop_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Loop)

      # Empty body
      body = CrystalV2::Compiler::Frontend.node_loop_body(loop_node).not_nil!
      body.size.should eq(0)
    end

    it "parses loop with complex expressions" do
      source = <<-CRYSTAL
      loop do
        result = compute(a + b * c)
        cache[key] = result
        break if result > threshold
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      loop_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(loop_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Loop)

      body = CrystalV2::Compiler::Frontend.node_loop_body(loop_node).not_nil!
      body.size.should eq(3)

      # First statement: assignment with complex expression
      assign1 = arena[body[0]]
      CrystalV2::Compiler::Frontend.node_kind(assign1).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)

      # Second statement: index assignment
      assign2 = arena[body[1]]
      CrystalV2::Compiler::Frontend.node_kind(assign2).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)

      # Third statement: if (suffix if parses as If node)
      if_stmt = arena[body[2]]
      CrystalV2::Compiler::Frontend.node_kind(if_stmt).should eq(CrystalV2::Compiler::Frontend::NodeKind::If)
    end
  end
end
