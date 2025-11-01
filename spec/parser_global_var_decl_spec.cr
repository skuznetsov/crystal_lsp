require "spec"

require "../src/compiler/frontend/parser"

describe "CrystalV2::Compiler::Frontend::Parser" do
  describe "Phase 77: Global variable type declarations ($var : Type)" do
    it "parses simple global variable declaration" do
      source = "$count : Int32"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      decl = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(decl).should eq(CrystalV2::Compiler::Frontend::NodeKind::GlobalVarDecl)
      decl.should be_a(CrystalV2::Compiler::Frontend::GlobalVarDeclNode)

      global_decl = decl.as(CrystalV2::Compiler::Frontend::GlobalVarDeclNode)
      String.new(global_decl.name).should eq("$count")
      String.new(global_decl.type).should eq("Int32")
    end

    it "parses global variable declaration with String type" do
      source = "$name : String"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      arena = program.arena
      decl = arena[program.roots[0]]

      CrystalV2::Compiler::Frontend.node_kind(decl).should eq(CrystalV2::Compiler::Frontend::NodeKind::GlobalVarDecl)
      decl.should be_a(CrystalV2::Compiler::Frontend::GlobalVarDeclNode)
      global_decl = decl.as(CrystalV2::Compiler::Frontend::GlobalVarDeclNode)

      String.new(global_decl.name).should eq("$name")
      String.new(global_decl.type).should eq("String")
    end

    it "parses multiple global variable declarations" do
      source = <<-CRYSTAL
      $count : Int32
      $name : String
      $flag : Bool
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(3)
      arena = program.arena

      decl1 = arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::GlobalVarDeclNode)
      String.new(decl1.name).should eq("$count")
      String.new(decl1.type).should eq("Int32")

      decl2 = arena[program.roots[1]].as(CrystalV2::Compiler::Frontend::GlobalVarDeclNode)
      String.new(decl2.name).should eq("$name")
      String.new(decl2.type).should eq("String")

      decl3 = arena[program.roots[2]].as(CrystalV2::Compiler::Frontend::GlobalVarDeclNode)
      String.new(decl3.name).should eq("$flag")
      String.new(decl3.type).should eq("Bool")
    end

    it "parses global variable with underscores" do
      source = "$my_global_var : Int32"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      arena = program.arena
      decl = arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::GlobalVarDeclNode)

      String.new(decl.name).should eq("$my_global_var")
      String.new(decl.type).should eq("Int32")
    end

    it "parses global variable with custom type" do
      source = "$manager : Manager"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      arena = program.arena
      decl = arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::GlobalVarDeclNode)

      String.new(decl.name).should eq("$manager")
      String.new(decl.type).should eq("Manager")
    end

    it "parses global variable alongside other statements" do
      source = <<-CRYSTAL
      $count : Int32
      x = 10
      $name : String
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(3)
      arena = program.arena

      first_decl = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(first_decl).should eq(CrystalV2::Compiler::Frontend::NodeKind::GlobalVarDecl)

      assign = arena[program.roots[1]]
      CrystalV2::Compiler::Frontend.node_kind(assign).should eq(CrystalV2::Compiler::Frontend::NodeKind::Assign)

      last_decl = arena[program.roots[2]]
      CrystalV2::Compiler::Frontend.node_kind(last_decl).should eq(CrystalV2::Compiler::Frontend::NodeKind::GlobalVarDecl)
    end

    it "parses global variable with suffix" do
      source = "$debug? : Bool"

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      arena = program.arena
      decl = arena[program.roots[0]].as(CrystalV2::Compiler::Frontend::GlobalVarDeclNode)

      String.new(decl.name).should eq("$debug?")
      String.new(decl.type).should eq("Bool")
    end
  end
end
