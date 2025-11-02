require "spec"

require "../../src/compiler/frontend/parser"

describe "CrystalV2::Compiler::Frontend::Parser" do
  describe "Phase 92: annotation keyword (user-defined annotation declarations)" do
    it "parses simple annotation definition" do
      source = <<-CRYSTAL
      annotation MyAnnotation
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      annotation_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(annotation_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Annotation)

      # Check name
      name = CrystalV2::Compiler::Frontend.node_annotation_name(annotation_node)
      name.should_not be_nil
      String.new(name.not_nil!).should eq("MyAnnotation")
    end

    it "parses annotation inside class" do
      source = <<-CRYSTAL
      class Foo
        annotation Internal
        end
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      class_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(class_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Class)

      class_body = CrystalV2::Compiler::Frontend.node_class_body(class_node).not_nil!
      class_body.size.should eq(1)

      annotation_node = arena[class_body[0]]
      CrystalV2::Compiler::Frontend.node_kind(annotation_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Annotation)
      String.new(CrystalV2::Compiler::Frontend.node_annotation_name(annotation_node).not_nil!).should eq("Internal")
    end

    it "parses annotation inside module" do
      source = <<-CRYSTAL
      module MyModule
        annotation Helper
        end
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      module_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(module_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Module)

      module_body = CrystalV2::Compiler::Frontend.node_module_body(module_node).not_nil!
      module_body.size.should eq(1)

      annotation_node = arena[module_body[0]]
      CrystalV2::Compiler::Frontend.node_kind(annotation_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Annotation)
      String.new(CrystalV2::Compiler::Frontend.node_annotation_name(annotation_node).not_nil!).should eq("Helper")
    end

    it "parses multiple annotations" do
      source = <<-CRYSTAL
      annotation First
      end

      annotation Second
      end

      annotation Third
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(3)
      arena = program.arena

      # First annotation
      ann1 = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(ann1).should eq(CrystalV2::Compiler::Frontend::NodeKind::Annotation)
      String.new(CrystalV2::Compiler::Frontend.node_annotation_name(ann1).not_nil!).should eq("First")

      # Second annotation
      ann2 = arena[program.roots[1]]
      CrystalV2::Compiler::Frontend.node_kind(ann2).should eq(CrystalV2::Compiler::Frontend::NodeKind::Annotation)
      String.new(CrystalV2::Compiler::Frontend.node_annotation_name(ann2).not_nil!).should eq("Second")

      # Third annotation
      ann3 = arena[program.roots[2]]
      CrystalV2::Compiler::Frontend.node_kind(ann3).should eq(CrystalV2::Compiler::Frontend::NodeKind::Annotation)
      String.new(CrystalV2::Compiler::Frontend.node_annotation_name(ann3).not_nil!).should eq("Third")
    end

    it "parses annotation with body (Phase 92A: body skipped)" do
      source = <<-CRYSTAL
      annotation MyAnnotation
        getter value : String
        getter count : Int32
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      annotation_node = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(annotation_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Annotation)
      String.new(CrystalV2::Compiler::Frontend.node_annotation_name(annotation_node).not_nil!).should eq("MyAnnotation")

      # Phase 92A: Body is skipped/ignored for now
      # Body parsing will be Phase 92B if needed
    end

    it "parses annotation before class definition" do
      source = <<-CRYSTAL
      annotation Deprecated
      end

      class Foo
      end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(2)
      arena = program.arena

      # First root is annotation
      ann = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(ann).should eq(CrystalV2::Compiler::Frontend::NodeKind::Annotation)

      # Second root is class
      cls = arena[program.roots[1]]
      CrystalV2::Compiler::Frontend.node_kind(cls).should eq(CrystalV2::Compiler::Frontend::NodeKind::Class)
    end
  end
end
