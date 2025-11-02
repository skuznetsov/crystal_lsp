require "spec"

require "../../src/compiler/frontend/parser"

describe "CrystalV2::Compiler::Frontend::Parser" do
  describe "Phase 77: Class variable type declarations (@@var : Type)" do
    it "parses simple class variable declaration" do
      source = <<-CRYSTAL
      class Foo
        @@count : Int32
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      class_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(class_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Class)

      body = CrystalV2::Compiler::Frontend.node_class_body(class_node).not_nil!
      body.size.should eq(1)

      decl = arena[body[0]]
      CrystalV2::Compiler::Frontend.node_kind(decl).should eq(CrystalV2::Compiler::Frontend::NodeKind::ClassVarDecl)

      String.new(decl.as(CrystalV2::Compiler::Frontend::ClassVarDeclNode).name).should eq("@@count")
      String.new(decl.as(CrystalV2::Compiler::Frontend::ClassVarDeclNode).type).should eq("Int32")
    end

    it "parses class variable declaration with String type" do
      source = <<-CRYSTAL
      class Foo
        @@name : String
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      class_node = arena[program.roots[0]]
      body = CrystalV2::Compiler::Frontend.node_class_body(class_node).not_nil!
      decl = arena[body[0]]

      CrystalV2::Compiler::Frontend.node_kind(decl).should eq(CrystalV2::Compiler::Frontend::NodeKind::ClassVarDecl)
      String.new(decl.as(CrystalV2::Compiler::Frontend::ClassVarDeclNode).name).should eq("@@name")
      String.new(decl.as(CrystalV2::Compiler::Frontend::ClassVarDeclNode).type).should eq("String")
    end

    it "parses multiple class variable declarations" do
      source = <<-CRYSTAL
      class Foo
        @@count : Int32
        @@name : String
        @@flag : Bool
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      class_node = arena[program.roots[0]]
      body = CrystalV2::Compiler::Frontend.node_class_body(class_node).not_nil!
      body.size.should eq(3)

      decl1 = arena[body[0]]
      CrystalV2::Compiler::Frontend.node_kind(decl1).should eq(CrystalV2::Compiler::Frontend::NodeKind::ClassVarDecl)
      String.new(decl1.as(CrystalV2::Compiler::Frontend::ClassVarDeclNode).name).should eq("@@count")
      String.new(decl1.as(CrystalV2::Compiler::Frontend::ClassVarDeclNode).type).should eq("Int32")

      decl2 = arena[body[1]]
      String.new(decl2.as(CrystalV2::Compiler::Frontend::ClassVarDeclNode).name).should eq("@@name")
      String.new(decl2.as(CrystalV2::Compiler::Frontend::ClassVarDeclNode).type).should eq("String")

      decl3 = arena[body[2]]
      String.new(decl3.as(CrystalV2::Compiler::Frontend::ClassVarDeclNode).name).should eq("@@flag")
      String.new(decl3.as(CrystalV2::Compiler::Frontend::ClassVarDeclNode).type).should eq("Bool")
    end

    it "parses class variable with underscores" do
      source = <<-CRYSTAL
      class Foo
        @@my_class_var : Int32
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      arena = program.arena
      class_node = arena[program.roots[0]]
      body = CrystalV2::Compiler::Frontend.node_class_body(class_node).not_nil!
      decl = arena[body[0]]

      CrystalV2::Compiler::Frontend.node_kind(decl).should eq(CrystalV2::Compiler::Frontend::NodeKind::ClassVarDecl)
      String.new(decl.as(CrystalV2::Compiler::Frontend::ClassVarDeclNode).name).should eq("@@my_class_var")
    end

    it "parses class variable with custom type" do
      source = <<-CRYSTAL
      class Foo
        @@manager : Manager
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      arena = program.arena
      class_node = arena[program.roots[0]]
      body = CrystalV2::Compiler::Frontend.node_class_body(class_node).not_nil!
      decl = arena[body[0]]

      CrystalV2::Compiler::Frontend.node_kind(decl).should eq(CrystalV2::Compiler::Frontend::NodeKind::ClassVarDecl)
      String.new(decl.as(CrystalV2::Compiler::Frontend::ClassVarDeclNode).name).should eq("@@manager")
      String.new(decl.as(CrystalV2::Compiler::Frontend::ClassVarDeclNode).type).should eq("Manager")
    end

    it "parses class variable alongside methods" do
      source = <<-CRYSTAL
      class Foo
        @@count : Int32

        def increment
          @@count = @@count + 1
        end
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      arena = program.arena
      class_node = arena[program.roots[0]]
      body = CrystalV2::Compiler::Frontend.node_class_body(class_node).not_nil!
      body.size.should eq(2)

      decl = arena[body[0]]
      CrystalV2::Compiler::Frontend.node_kind(decl).should eq(CrystalV2::Compiler::Frontend::NodeKind::ClassVarDecl)

      method = arena[body[1]]
      CrystalV2::Compiler::Frontend.node_kind(method).should eq(CrystalV2::Compiler::Frontend::NodeKind::Def)
    end

    it "parses class variable alongside instance variables" do
      source = <<-CRYSTAL
      class Foo
        @instance : Int32
        @@class_var : String
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      arena = program.arena
      class_node = arena[program.roots[0]]
      body = CrystalV2::Compiler::Frontend.node_class_body(class_node).not_nil!
      body.size.should eq(2)

      ivar = arena[body[0]]
      CrystalV2::Compiler::Frontend.node_kind(ivar).should eq(CrystalV2::Compiler::Frontend::NodeKind::InstanceVarDecl)
      String.new(ivar.as(CrystalV2::Compiler::Frontend::InstanceVarDeclNode).name).should eq("@instance")

      cvar = arena[body[1]]
      CrystalV2::Compiler::Frontend.node_kind(cvar).should eq(CrystalV2::Compiler::Frontend::NodeKind::ClassVarDecl)
      String.new(cvar.as(CrystalV2::Compiler::Frontend::ClassVarDeclNode).name).should eq("@@class_var")
    end
  end
end
