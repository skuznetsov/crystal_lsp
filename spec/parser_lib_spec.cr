require "spec"

require "../src/compiler/frontend/parser"

describe "CrystalV2::Compiler::Frontend::Parser" do
  describe "Phase 38: lib (C bindings) (PRODUCTION-READY)" do
    it "parses empty lib" do
      source = <<-CRYSTAL
      lib LibC
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena
      lib_node = arena[program.roots.first]

      CrystalV2::Compiler::Frontend.node_kind(lib_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Lib)
      String.new(lib_node.as(CrystalV2::Compiler::Frontend::LibNode).name).should eq("LibC")
      body = lib_node.as(CrystalV2::Compiler::Frontend::LibNode).body
      body.should_not be_nil
      body.not_nil!.size.should eq(0)
    end

    it "parses lib with method definition" do
      source = <<-CRYSTAL
      lib LibC
        def strlen(str : String) : Int32
        end
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena
      lib_node = arena[program.roots.first].as(CrystalV2::Compiler::Frontend::LibNode)

      String.new(lib_node.name).should eq("LibC")

      lib_body = lib_node.body.not_nil!
      lib_body.size.should eq(1)

      method_node = arena[lib_body[0]]
      CrystalV2::Compiler::Frontend.node_kind(method_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Def)
      String.new(method_node.as(CrystalV2::Compiler::Frontend::DefNode).name).should eq("strlen")
    end

    it "parses lib with multiple method definitions" do
      source = <<-CRYSTAL
      lib LibC
        def strlen(str : String) : Int32
        end

        def strcmp(str1 : String, str2 : String) : Int32
        end
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena
      lib_node = arena[program.roots.first].as(CrystalV2::Compiler::Frontend::LibNode)

      lib_body = lib_node.body.not_nil!
      lib_body.size.should eq(2)

      # First method
      method1 = arena[lib_body[0]]
      String.new(method1.as(CrystalV2::Compiler::Frontend::DefNode).name).should eq("strlen")

      # Second method
      method2 = arena[lib_body[1]]
      String.new(method2.as(CrystalV2::Compiler::Frontend::DefNode).name).should eq("strcmp")
    end

    it "parses nested lib" do
      source = <<-CRYSTAL
      class MyClass
        lib LibC
          def strlen(str : String) : Int32
          end
        end
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena
      class_node = arena[program.roots.first]

      class_body = CrystalV2::Compiler::Frontend.node_class_body(class_node).not_nil!
      class_body.size.should eq(1)

      lib_node = arena[class_body[0]]
      CrystalV2::Compiler::Frontend.node_kind(lib_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Lib)
      String.new(lib_node.as(CrystalV2::Compiler::Frontend::LibNode).name).should eq("LibC")
    end

    it "parses lib in module" do
      source = <<-CRYSTAL
      module MyModule
        lib LibC
          def strlen(str : String) : Int32
          end
        end
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena
      module_node = arena[program.roots.first].as(CrystalV2::Compiler::Frontend::ModuleNode)

      module_body = module_node.body.not_nil!
      module_body.size.should eq(1)

      lib_node = arena[module_body[0]]
      CrystalV2::Compiler::Frontend.node_kind(lib_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Lib)
      String.new(lib_node.as(CrystalV2::Compiler::Frontend::LibNode).name).should eq("LibC")
    end

    it "parses lib with struct definition" do
      source = <<-CRYSTAL
      lib LibC
        struct TimeSpec
        end
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena
      lib_node = arena[program.roots.first].as(CrystalV2::Compiler::Frontend::LibNode)

      lib_body = lib_node.body.not_nil!
      lib_body.size.should eq(1)

      struct_node = arena[lib_body[0]]
      CrystalV2::Compiler::Frontend.node_kind(struct_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Struct)
      String.new(CrystalV2::Compiler::Frontend.node_class_name(struct_node).not_nil!).should eq("TimeSpec")
    end

    it "parses lib with type alias" do
      source = <<-CRYSTAL
      lib LibC
        alias SizeT = UInt64
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena
      lib_node = arena[program.roots.first].as(CrystalV2::Compiler::Frontend::LibNode)

      lib_body = lib_node.body.not_nil!
      lib_body.size.should eq(1)

      alias_node = arena[lib_body[0]]
      CrystalV2::Compiler::Frontend.node_kind(alias_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Alias)
      String.new(CrystalV2::Compiler::Frontend.node_alias_name(alias_node).not_nil!).should eq("SizeT")
      String.new(CrystalV2::Compiler::Frontend.node_alias_value(alias_node).not_nil!).should eq("UInt64")
    end

    it "parses multiple libs" do
      source = <<-CRYSTAL
      lib LibC
        def strlen(str : String) : Int32
        end
      end

      lib LibPthread
        def pthread_create : Int32
        end
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(2)
      arena = program.arena

      # First lib
      lib1 = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(lib1).should eq(CrystalV2::Compiler::Frontend::NodeKind::Lib)
      String.new(CrystalV2::Compiler::Frontend.node_lib_name(lib1).not_nil!).should eq("LibC")

      # Second lib
      lib2 = arena[program.roots[1]]
      CrystalV2::Compiler::Frontend.node_kind(lib2).should eq(CrystalV2::Compiler::Frontend::NodeKind::Lib)
      String.new(CrystalV2::Compiler::Frontend.node_lib_name(lib2).not_nil!).should eq("LibPthread")
    end

    it "parses lib with mixed definitions" do
      source = <<-CRYSTAL
      lib LibC
        alias SizeT = UInt64

        struct TimeSpec
        end

        def strlen(str : String) : Int32
        end
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena
      lib_node = arena[program.roots.first].as(CrystalV2::Compiler::Frontend::LibNode)

      lib_body = lib_node.body.not_nil!
      lib_body.size.should eq(3)

      # Alias
      alias_node = arena[lib_body[0]]
      CrystalV2::Compiler::Frontend.node_kind(alias_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Alias)

      # Struct
      struct_node = arena[lib_body[1]]
      CrystalV2::Compiler::Frontend.node_kind(struct_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Struct)

      # Method
      method_node = arena[lib_body[2]]
      CrystalV2::Compiler::Frontend.node_kind(method_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Def)
    end
  end
end
