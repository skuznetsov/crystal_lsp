require "spec"

require "../../src/compiler/frontend/parser"

describe "CrystalV2::Compiler::Frontend::Parser" do
  describe "Phase 101: &. block shorthand (PRODUCTION-READY)" do
    it "parses simple block shorthand: try &.method" do
      source = <<-CRYSTAL
      obj.try &.method
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      parser.diagnostics.size.should eq(0)

      # AST structure: Call(Call(obj, try), args: [Block(|__arg0| Call(__arg0, method))])
      arena = program.arena
      call_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(call_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Call)
    end

    it "parses block shorthand with method chaining: try &.first.second" do
      source = <<-CRYSTAL
      obj.try &.first.second
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      parser.diagnostics.size.should eq(0)
    end

    it "parses block shorthand with arguments in chain: try &.method(1, 2)" do
      source = <<-CRYSTAL
      obj.try &.method(1, 2)
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      parser.diagnostics.size.should eq(0)
    end

    it "parses block shorthand in method call" do
      source = <<-CRYSTAL
      map.transform_values &.to_s
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      parser.diagnostics.size.should eq(0)
    end

    it "parses block shorthand with safe navigation in receiver" do
      source = <<-CRYSTAL
      type.types?.try &.each_value
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      parser.diagnostics.size.should eq(0)
    end

    it "parses block shorthand with nested calls" do
      source = <<-CRYSTAL
      items.map &.process.transform
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      parser.diagnostics.size.should eq(0)
    end

    it "parses block shorthand with indexing" do
      source = <<-CRYSTAL
      array.map &.[0]
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      parser.diagnostics.size.should eq(0)
    end

    it "parses block shorthand with nilable index and nested call" do
      source = <<-CRYSTAL
      source.lines[idx+1]?.try(&.includes?("->"))
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      parser.diagnostics.size.should eq(0)
    end

    it "parses block shorthand with operator call" do
      source = <<-CRYSTAL
      numbers.map &.+(1)
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      parser.diagnostics.size.should eq(0)
    end

    it "distinguishes safe navigation from block shorthand" do
      # Safe navigation: obj&.method (no space, AmpDot token)
      source1 = <<-CRYSTAL
      obj&.method
      CRYSTAL

      parser1 = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source1))
      program1 = parser1.parse_program

      program1.roots.size.should eq(1)
      parser1.diagnostics.size.should eq(0)

      # Block shorthand: obj.try &.method (with space, Amp + Dot tokens)
      source2 = <<-CRYSTAL
      obj.try &.method
      CRYSTAL

      parser2 = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source2))
      program2 = parser2.parse_program

      program2.roots.size.should eq(1)
      parser2.diagnostics.size.should eq(0)
    end

    it "parses real-world example from stdlib" do
      source = <<-CRYSTAL
      def check_method
        type.types?.try &.each_value do |value_type|
          puts value_type
        end
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      parser.diagnostics.size.should eq(0)
    end

    it "parses multiple block shorthands in expression" do
      source = <<-CRYSTAL
      items.map &.process.filter &.valid?
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      parser.diagnostics.size.should eq(0)
    end

    it "parses block shorthand in assignment" do
      source = <<-CRYSTAL
      result = array.map &.to_s
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      parser.diagnostics.size.should eq(0)
    end

    it "parses block shorthand with question mark method" do
      source = <<-CRYSTAL
      items.select &.valid?
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      parser.diagnostics.size.should eq(0)
    end

    it "parses block shorthand with exclamation mark method" do
      source = <<-CRYSTAL
      items.each &.save!
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      parser.diagnostics.size.should eq(0)
    end

    it "parses block shorthand with nested block argument inside parentheses" do
      source = <<-CRYSTAL
      all_break_vars.try(&.all? &.has_key?(name))
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      parser.diagnostics.size.should eq(0)
    end

    it "parses block shorthand with indexing and try inside nested argument" do
      source = <<-CRYSTAL
      all_break_vars.try(&.any? &.[name]?.try &.nil_if_read?)
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      parser.diagnostics.size.should eq(0)
    end
  end
end
