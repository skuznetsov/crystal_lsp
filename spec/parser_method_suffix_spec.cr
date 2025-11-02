require "spec"

require "../src/compiler/frontend/parser"


describe "CrystalV2::Compiler::Frontend::Parser" do
  describe "Phase 43: Method name suffixes (? and !) (PRODUCTION-READY)" do
    it "parses method definition with ? suffix" do
      source = <<-CRYSTAL
      def empty?
        true
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      method_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(method_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Def)
      String.new(CrystalV2::Compiler::Frontend.node_def_name(method_node).not_nil!).should eq("empty?")
    end

    it "parses method definition with ! suffix" do
      source = <<-CRYSTAL
      def save!
        42
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      method_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(method_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Def)
      String.new(CrystalV2::Compiler::Frontend.node_def_name(method_node).not_nil!).should eq("save!")
    end

    it "parses bare method call with ? suffix as identifier" do
      source = <<-CRYSTAL
      empty?
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      # Bare call parses as Identifier (semantic analysis determines it's a call)
      id_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(id_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Identifier)
      String.new(CrystalV2::Compiler::Frontend.node_literal(id_node).not_nil!).should eq("empty?")
    end

    it "parses bare method call with ! suffix as identifier" do
      source = <<-CRYSTAL
      save!
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      # Bare call parses as Identifier (semantic analysis determines it's a call)
      id_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(id_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Identifier)
      String.new(CrystalV2::Compiler::Frontend.node_literal(id_node).not_nil!).should eq("save!")
    end

    it "parses method call with ! suffix and parentheses" do
      source = <<-CRYSTAL
      save!()
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      # With parentheses, parses as Call
      call_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(call_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Call)

      callee_id = CrystalV2::Compiler::Frontend.node_callee(call_node).not_nil!
      callee = arena[callee_id]
      CrystalV2::Compiler::Frontend.node_kind(callee).should eq(CrystalV2::Compiler::Frontend::NodeKind::Identifier)
      String.new(CrystalV2::Compiler::Frontend.node_literal(callee).not_nil!).should eq("save!")
    end

    it "parses method with ? suffix in class" do
      source = <<-CRYSTAL
      class Array
        def empty?
          size == 0
        end
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      class_node = arena[program.roots[0]]
      class_body = CrystalV2::Compiler::Frontend.node_class_body(class_node).not_nil!
      method_node = arena[class_body[0]]

      CrystalV2::Compiler::Frontend.node_kind(method_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Def)
      String.new(CrystalV2::Compiler::Frontend.node_def_name(method_node).not_nil!).should eq("empty?")
    end

    it "parses method with ! suffix and parameters" do
      source = <<-CRYSTAL
      def update!(name, age)
        @name = name
        @age = age
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      method_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(method_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Def)
      String.new(CrystalV2::Compiler::Frontend.node_def_name(method_node).not_nil!).should eq("update!")

      params = CrystalV2::Compiler::Frontend.node_def_params(method_node).not_nil!
      params.size.should eq(2)
      String.new(params[0].name).should eq("name")
      String.new(params[1].name).should eq("age")
    end

    it "parses member access with ? suffix" do
      source = <<-CRYSTAL
      obj.nil?()
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      call_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(call_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Call)

      callee_id = CrystalV2::Compiler::Frontend.node_callee(call_node).not_nil!
      callee = arena[callee_id]
      CrystalV2::Compiler::Frontend.node_kind(callee).should eq(CrystalV2::Compiler::Frontend::NodeKind::MemberAccess)
      String.new(CrystalV2::Compiler::Frontend.node_member(callee).not_nil!).should eq("nil?")
    end

    it "parses member access with ! suffix" do
      source = <<-CRYSTAL
      user.save!()
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      call_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(call_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Call)

      callee_id = CrystalV2::Compiler::Frontend.node_callee(call_node).not_nil!
      callee = arena[callee_id]
      CrystalV2::Compiler::Frontend.node_kind(callee).should eq(CrystalV2::Compiler::Frontend::NodeKind::MemberAccess)
      String.new(CrystalV2::Compiler::Frontend.node_member(callee).not_nil!).should eq("save!")
    end

    it "parses chained method calls with suffixes" do
      source = <<-CRYSTAL
      user.valid?().to_s()
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      # Top level is call to to_s
      to_s_call = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(to_s_call).should eq(CrystalV2::Compiler::Frontend::NodeKind::Call)

      to_s_member_id = CrystalV2::Compiler::Frontend.node_callee(to_s_call).not_nil!
      to_s_member = arena[to_s_member_id]
      CrystalV2::Compiler::Frontend.node_kind(to_s_member).should eq(CrystalV2::Compiler::Frontend::NodeKind::MemberAccess)
      String.new(CrystalV2::Compiler::Frontend.node_member(to_s_member).not_nil!).should eq("to_s")

      # Left of to_s member access is call to valid?
      valid_call = arena[CrystalV2::Compiler::Frontend.node_left(to_s_member).not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(valid_call).should eq(CrystalV2::Compiler::Frontend::NodeKind::Call)

      valid_member_id = CrystalV2::Compiler::Frontend.node_callee(valid_call).not_nil!
      valid_member = arena[valid_member_id]
      CrystalV2::Compiler::Frontend.node_kind(valid_member).should eq(CrystalV2::Compiler::Frontend::NodeKind::MemberAccess)
      String.new(CrystalV2::Compiler::Frontend.node_member(valid_member).not_nil!).should eq("valid?")
    end

    it "parses method with ? suffix and type annotation" do
      source = <<-CRYSTAL
      def empty? : Bool
        true
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      method_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(method_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Def)
      String.new(CrystalV2::Compiler::Frontend.node_def_name(method_node).not_nil!).should eq("empty?")

      return_type = CrystalV2::Compiler::Frontend.node_def_return_type(method_node)
      return_type.should_not be_nil
      String.new(return_type.not_nil!).should eq("Bool")
    end

    it "parses multiple methods with different suffixes" do
      source = <<-CRYSTAL
      def valid?
        true
      end

      def save!
        42
      end

      def process
        1
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(3)
      arena = program.arena

      # First method: valid?
      method1 = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(method1).should eq(CrystalV2::Compiler::Frontend::NodeKind::Def)
      String.new(CrystalV2::Compiler::Frontend.node_def_name(method1).not_nil!).should eq("valid?")

      # Second method: save!
      method2 = arena[program.roots[1]]
      CrystalV2::Compiler::Frontend.node_kind(method2).should eq(CrystalV2::Compiler::Frontend::NodeKind::Def)
      String.new(CrystalV2::Compiler::Frontend.node_def_name(method2).not_nil!).should eq("save!")

      # Third method: process (no suffix)
      method3 = arena[program.roots[2]]
      CrystalV2::Compiler::Frontend.node_kind(method3).should eq(CrystalV2::Compiler::Frontend::NodeKind::Def)
      String.new(CrystalV2::Compiler::Frontend.node_def_name(method3).not_nil!).should eq("process")
    end

    it "parses method call with ! suffix and arguments" do
      source = <<-CRYSTAL
      delete!(key, value)
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      call_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(call_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Call)

      callee_id = CrystalV2::Compiler::Frontend.node_callee(call_node).not_nil!
      callee = arena[callee_id]
      CrystalV2::Compiler::Frontend.node_kind(callee).should eq(CrystalV2::Compiler::Frontend::NodeKind::Identifier)
      String.new(CrystalV2::Compiler::Frontend.node_literal(callee).not_nil!).should eq("delete!")

      args = CrystalV2::Compiler::Frontend.node_args(call_node).not_nil!
      args.size.should eq(2)
    end
  end
end
