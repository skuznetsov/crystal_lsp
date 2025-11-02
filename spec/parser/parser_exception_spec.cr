require "spec"

require "../../src/compiler/frontend/parser"

describe "CrystalV2::Compiler::Frontend::Parser" do
  describe "Phase 29: Exception handling" do
    it "parses begin with rescue" do
      source = <<-CRYSTAL
        begin
          x = 10
        rescue
          y = 20
        end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      begin_node = arena[program.roots.first]
      CrystalV2::Compiler::Frontend.node_kind(begin_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Begin)

      # Check begin body
      body = CrystalV2::Compiler::Frontend.node_begin_body(begin_node).not_nil!
      body.size.should eq(1)

      # Check rescue clause
      rescue_clauses = CrystalV2::Compiler::Frontend.node_rescue_clauses(begin_node).not_nil!
      rescue_clauses.size.should eq(1)
      rescue_clauses[0].body.size.should eq(1)
    end

    it "parses begin with typed rescue" do
      source = <<-CRYSTAL
        begin
          risky_operation()
        rescue RuntimeError
          handle_error()
        end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      begin_node = arena[program.roots.first]
      CrystalV2::Compiler::Frontend.node_kind(begin_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Begin)

      # Check rescue clause with type
      rescue_clauses = CrystalV2::Compiler::Frontend.node_rescue_clauses(begin_node).not_nil!
      rescue_clauses.size.should eq(1)

      rescue_clause = rescue_clauses[0]
      exception_type = rescue_clause.exception_type.not_nil!
      String.new(exception_type).should eq("RuntimeError")
      rescue_clause.variable_name.should be_nil
    end

    it "parses begin with rescue and variable binding" do
      source = <<-CRYSTAL
        begin
          x = 10
        rescue => e
          puts(e)
        end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      begin_node = arena[program.roots.first]

      rescue_clauses = begin_node.as(CrystalV2::Compiler::Frontend::BeginNode).rescue_clauses.not_nil!
      rescue_clauses.size.should eq(1)

      rescue_clause = rescue_clauses[0]
      rescue_clause.exception_type.should be_nil
      variable_name = rescue_clause.variable_name.not_nil!
      String.new(variable_name).should eq("e")
    end

    it "parses begin with typed rescue and variable" do
      source = <<-CRYSTAL
        begin
          x = 10
        rescue RuntimeError => e
          handle(e)
        end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      begin_node = arena[program.roots.first]

      rescue_clauses = begin_node.as(CrystalV2::Compiler::Frontend::BeginNode).rescue_clauses.not_nil!
      rescue_clause = rescue_clauses[0]

      exception_type = rescue_clause.exception_type.not_nil!
      String.new(exception_type).should eq("RuntimeError")

      variable_name = rescue_clause.variable_name.not_nil!
      String.new(variable_name).should eq("e")
    end

    it "parses begin with multiple rescue clauses" do
      source = <<-CRYSTAL
        begin
          x = 10
        rescue RuntimeError
          a = 1
        rescue ArgumentError
          b = 2
        rescue
          c = 3
        end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      begin_node = arena[program.roots.first]

      rescue_clauses = begin_node.as(CrystalV2::Compiler::Frontend::BeginNode).rescue_clauses.not_nil!
      rescue_clauses.size.should eq(3)

      # First rescue: RuntimeError
      String.new(rescue_clauses[0].exception_type.not_nil!).should eq("RuntimeError")

      # Second rescue: ArgumentError
      String.new(rescue_clauses[1].exception_type.not_nil!).should eq("ArgumentError")

      # Third rescue: catch-all
      rescue_clauses[2].exception_type.should be_nil
    end

    it "parses begin with ensure" do
      source = <<-CRYSTAL
        begin
          x = 10
        ensure
          cleanup()
        end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      begin_node = arena[program.roots.first]

      # No rescue clauses
      begin_node.as(CrystalV2::Compiler::Frontend::BeginNode).rescue_clauses.should be_nil

      # Has ensure body
      ensure_body = CrystalV2::Compiler::Frontend.node_ensure_body(begin_node).not_nil!
      ensure_body.size.should eq(1)
    end

    it "parses begin with rescue and ensure" do
      source = <<-CRYSTAL
        begin
          x = 10
        rescue
          y = 20
        ensure
          cleanup()
        end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      begin_node = arena[program.roots.first]

      # Has rescue
      rescue_clauses = begin_node.as(CrystalV2::Compiler::Frontend::BeginNode).rescue_clauses.not_nil!
      rescue_clauses.size.should eq(1)

      # Has ensure
      ensure_body = CrystalV2::Compiler::Frontend.node_ensure_body(begin_node).not_nil!
      ensure_body.size.should eq(1)
    end

    it "parses raise statement with expression" do
      source = <<-CRYSTAL
        raise "error message"
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      raise_node = arena[program.roots.first]
      CrystalV2::Compiler::Frontend.node_kind(raise_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Raise)

      raise_value = CrystalV2::Compiler::Frontend.node_raise_value(raise_node).not_nil!
      value_node = arena[raise_value]
      CrystalV2::Compiler::Frontend.node_kind(value_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::String)
    end

    it "parses bare raise (re-raise)" do
      source = <<-CRYSTAL
        begin
          x = 10
        rescue
          raise
        end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      begin_node = arena[program.roots.first]
      rescue_clauses = begin_node.as(CrystalV2::Compiler::Frontend::BeginNode).rescue_clauses.not_nil!

      # Rescue body contains raise
      rescue_body = rescue_clauses[0].body
      rescue_body.size.should eq(1)

      raise_node = arena[rescue_body[0]]
      CrystalV2::Compiler::Frontend.node_kind(raise_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Raise)
      CrystalV2::Compiler::Frontend.node_raise_value(raise_node).should be_nil  # Bare raise
    end
  end
end
