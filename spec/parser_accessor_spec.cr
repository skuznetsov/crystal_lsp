require "spec"

require "../src/compiler/frontend/parser"

describe "CrystalV2::Compiler::Frontend::Parser" do
  describe "Phase 30: Accessor macros (PRODUCTION-READY)" do
    it "parses getter with single name" do
      source = <<-CRYSTAL
        class Person
          getter name
        end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      class_node = arena[program.roots.first]
      CrystalV2::Compiler::Frontend.node_kind(class_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Class)

      class_body = CrystalV2::Compiler::Frontend.node_class_body(class_node).not_nil!
      class_body.size.should eq(1)

      getter_node = arena[class_body[0]]
      CrystalV2::Compiler::Frontend.node_kind(getter_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Getter)

      specs = CrystalV2::Compiler::Frontend.node_accessor_specs(getter_node).not_nil!
      specs.size.should eq(1)
      specs[0].name.should eq("name")
      specs[0].type_annotation.should be_nil
      specs[0].default_value.should be_nil
    end

    it "parses getter with type annotation" do
      source = <<-CRYSTAL
        class Person
          getter name : String
        end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      arena = program.arena
      class_node = arena[program.roots.first]
      class_body = CrystalV2::Compiler::Frontend.node_class_body(class_node).not_nil!

      getter_node = arena[class_body[0]]
      specs = CrystalV2::Compiler::Frontend.node_accessor_specs(getter_node).not_nil!

      specs[0].name.should eq("name")
      specs[0].type_annotation.should eq("String")
      specs[0].default_value.should be_nil
    end

    it "parses getter with default value" do
      source = <<-CRYSTAL
        class Person
          getter name = "unknown"
        end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      arena = program.arena
      class_node = arena[program.roots.first]
      class_body = CrystalV2::Compiler::Frontend.node_class_body(class_node).not_nil!

      getter_node = arena[class_body[0]]
      specs = CrystalV2::Compiler::Frontend.node_accessor_specs(getter_node).not_nil!

      specs[0].name.should eq("name")
      specs[0].type_annotation.should be_nil

      default_value = specs[0].default_value.not_nil!
      default_node = arena[default_value]
      CrystalV2::Compiler::Frontend.node_kind(default_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::String)
    end

    it "parses getter with type and default value" do
      source = <<-CRYSTAL
        class Person
          getter name : String = "unknown"
        end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      arena = program.arena
      class_node = arena[program.roots.first]
      class_body = CrystalV2::Compiler::Frontend.node_class_body(class_node).not_nil!

      getter_node = arena[class_body[0]]
      specs = CrystalV2::Compiler::Frontend.node_accessor_specs(getter_node).not_nil!

      specs[0].name.should eq("name")
      specs[0].type_annotation.should eq("String")

      default_value = specs[0].default_value.not_nil!
      default_node = arena[default_value]
      CrystalV2::Compiler::Frontend.node_kind(default_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::String)
    end

    it "parses getter with multiple names" do
      source = <<-CRYSTAL
        class Person
          getter name, age, email
        end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      arena = program.arena
      class_node = arena[program.roots.first]
      class_body = CrystalV2::Compiler::Frontend.node_class_body(class_node).not_nil!

      getter_node = arena[class_body[0]]
      specs = CrystalV2::Compiler::Frontend.node_accessor_specs(getter_node).not_nil!
      specs.size.should eq(3)
      specs[0].name.should eq("name")
      specs[1].name.should eq("age")
      specs[2].name.should eq("email")
    end

    it "parses getter with mixed specifications" do
      source = <<-CRYSTAL
        class Person
          getter name : String, age = 0, email
        end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      arena = program.arena
      class_node = arena[program.roots.first]
      class_body = CrystalV2::Compiler::Frontend.node_class_body(class_node).not_nil!

      getter_node = arena[class_body[0]]
      specs = CrystalV2::Compiler::Frontend.node_accessor_specs(getter_node).not_nil!
      specs.size.should eq(3)

      # name : String
      specs[0].name.should eq("name")
      specs[0].type_annotation.should eq("String")
      specs[0].default_value.should be_nil

      # age = 0
      specs[1].name.should eq("age")
      specs[1].type_annotation.should be_nil
      specs[1].default_value.should_not be_nil

      # email
      specs[2].name.should eq("email")
      specs[2].type_annotation.should be_nil
      specs[2].default_value.should be_nil
    end

    it "parses setter with type annotation" do
      source = <<-CRYSTAL
        class Person
          setter name : String
        end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      arena = program.arena
      class_node = arena[program.roots.first]
      class_body = CrystalV2::Compiler::Frontend.node_class_body(class_node).not_nil!

      setter_node = arena[class_body[0]]
      CrystalV2::Compiler::Frontend.node_kind(setter_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Setter)

      specs = CrystalV2::Compiler::Frontend.node_accessor_specs(setter_node).not_nil!
      specs[0].name.should eq("name")
      specs[0].type_annotation.should eq("String")
    end

    it "parses property with type annotation and default" do
      source = <<-CRYSTAL
        class Person
          property name : String = "unknown"
        end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      arena = program.arena
      class_node = arena[program.roots.first]
      class_body = CrystalV2::Compiler::Frontend.node_class_body(class_node).not_nil!

      property_node = arena[class_body[0]]
      CrystalV2::Compiler::Frontend.node_kind(property_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Property)

      specs = CrystalV2::Compiler::Frontend.node_accessor_specs(property_node).not_nil!
      specs[0].name.should eq("name")
      specs[0].type_annotation.should eq("String")
      specs[0].default_value.should_not be_nil
    end

    it "parses class with multiple accessor types" do
      source = <<-CRYSTAL
        class Person
          getter name : String
          setter age : Int32
          property email
        end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      arena = program.arena
      class_node = arena[program.roots.first]
      class_body = CrystalV2::Compiler::Frontend.node_class_body(class_node).not_nil!
      class_body.size.should eq(3)

      # getter name : String
      getter_node = arena[class_body[0]]
      CrystalV2::Compiler::Frontend.node_kind(getter_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Getter)

      # setter age : Int32
      setter_node = arena[class_body[1]]
      CrystalV2::Compiler::Frontend.node_kind(setter_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Setter)

      # property email
      property_node = arena[class_body[2]]
      CrystalV2::Compiler::Frontend.node_kind(property_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Property)
    end
  end
end
