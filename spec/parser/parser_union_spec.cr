require "spec"

require "../../src/compiler/frontend/parser"

module ParserUnionSpecAliases
  alias Frontend = CrystalV2::Compiler::Frontend
end

include ParserUnionSpecAliases

describe "CrystalV2::Compiler::Frontend::Parser" do
  describe "Phase 97: Union definition (C bindings)" do
    it "parses empty union" do
      source = <<-CRYSTAL
        union IntOrFloat
        end
      CRYSTAL

      program = Frontend::Parser.new(Frontend::Lexer.new(source)).parse_program
      arena = program.arena

      union_node = arena[program.roots.first]
      Frontend.node_kind(union_node).should eq(Frontend::NodeKind::Union)
      Frontend.node_class_is_union(union_node).should eq(true)
      Frontend.node_class_name(union_node).not_nil!.should eq("IntOrFloat".to_slice)

      body = Frontend.node_class_body(union_node)
      if body
        body.should be_empty
      else
        body.should be_nil
      end
    end

    it "parses union with fields" do
      source = <<-CRYSTAL
        union Value
          @int_val : Int32
          @float_val : Float64
        end
      CRYSTAL

      program = Frontend::Parser.new(Frontend::Lexer.new(source)).parse_program
      arena = program.arena

      union_node = arena[program.roots.first]
      Frontend.node_kind(union_node).should eq(Frontend::NodeKind::Union)
      Frontend.node_class_is_union(union_node).should eq(true)

      body = Frontend.node_class_body(union_node).not_nil!
      body.size.should eq(2)

      body.each do |entry_id|
        entry = arena[entry_id]
        Frontend.node_kind(entry).should eq(Frontend::NodeKind::InstanceVarDecl)
      end
    end

    it "parses union inside lib block" do
      source = <<-CRYSTAL
        lib C
          union Data
            @i : Int32
            @f : Float64
          end
        end
      CRYSTAL

      program = Frontend::Parser.new(Frontend::Lexer.new(source)).parse_program
      arena = program.arena

      lib_node = arena[program.roots.first]
      Frontend.node_kind(lib_node).should eq(Frontend::NodeKind::Lib)

      union_node = arena[Frontend.node_lib_body(lib_node).not_nil!.first]
      Frontend.node_kind(union_node).should eq(Frontend::NodeKind::Union)
      Frontend.node_class_is_union(union_node).should eq(true)
    end

    it "parses union with methods" do
      source = <<-CRYSTAL
        union Result
          def get_value
            42
          end
        end
      CRYSTAL

      program = Frontend::Parser.new(Frontend::Lexer.new(source)).parse_program
      arena = program.arena

      union_node = arena[program.roots.first]
      Frontend.node_kind(union_node).should eq(Frontend::NodeKind::Union)
      Frontend.node_class_is_union(union_node).should eq(true)

      body = Frontend.node_class_body(union_node).not_nil!
      body.size.should eq(1)
      Frontend.node_kind(arena[body.first]).should eq(Frontend::NodeKind::Def)
    end

    it "parses multiple unions" do
      source = <<-CRYSTAL
        union Value1
          @i : Int32
        end

        union Value2
          @f : Float64
        end
      CRYSTAL

      program = Frontend::Parser.new(Frontend::Lexer.new(source)).parse_program
      arena = program.arena

      program.roots.size.should eq(2)
      program.roots.each do |root_id|
        node = arena[root_id]
        Frontend.node_kind(node).should eq(Frontend::NodeKind::Union)
        Frontend.node_class_is_union(node).should eq(true)
      end
    end

    it "parses nested union in module" do
      source = <<-CRYSTAL
        module Container
          union InnerUnion
            @value : Int32
          end
        end
      CRYSTAL

      program = Frontend::Parser.new(Frontend::Lexer.new(source)).parse_program
      arena = program.arena

      module_node = arena[program.roots.first]
      Frontend.node_kind(module_node).should eq(Frontend::NodeKind::Module)

      union_node = arena[Frontend.node_module_body(module_node).not_nil!.first]
      Frontend.node_kind(union_node).should eq(Frontend::NodeKind::Union)
      Frontend.node_class_is_union(union_node).should eq(true)
    end

    it "parses abstract union" do
      source = <<-CRYSTAL
        abstract union BaseUnion
          @val : Int32
        end
      CRYSTAL

      program = Frontend::Parser.new(Frontend::Lexer.new(source)).parse_program
      arena = program.arena

      union_node = arena[program.roots.first]
      Frontend.node_kind(union_node).should eq(Frontend::NodeKind::Union)
      Frontend.node_class_is_union(union_node).should eq(true)
      Frontend.node_class_is_abstract(union_node).should eq(true)
    end

    it "distinguishes between class, struct and union" do
      source = <<-CRYSTAL
        class MyClass; end
        struct MyStruct; end
        union MyUnion; end
      CRYSTAL

      program = Frontend::Parser.new(Frontend::Lexer.new(source)).parse_program
      arena = program.arena

      class_node = arena[program.roots[0]]
      Frontend.node_kind(class_node).should eq(Frontend::NodeKind::Class)
      Frontend.node_class_is_union(class_node).should_not eq(true)
      Frontend.node_class_is_struct(class_node).should_not eq(true)

      struct_node = arena[program.roots[1]]
      Frontend.node_kind(struct_node).should eq(Frontend::NodeKind::Struct)
      Frontend.node_class_is_struct(struct_node).should eq(true)
      Frontend.node_class_is_union(struct_node).should_not eq(true)

      union_node = arena[program.roots[2]]
      Frontend.node_kind(union_node).should eq(Frontend::NodeKind::Union)
      Frontend.node_class_is_union(union_node).should eq(true)
    end
  end
end
