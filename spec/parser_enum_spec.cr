require "spec"

require "../src/compiler/frontend/parser"

describe "CrystalV2::Compiler::Frontend::Parser" do
  describe "Phase 33: Enum definition (PRODUCTION-READY)" do
    it "parses empty enum" do
      source = <<-CRYSTAL
        enum Status
        end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(1)
      arena = program.arena

      enum_node = arena[program.roots.first]
      CrystalV2::Compiler::Frontend.node_kind(enum_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Enum)

      enum_name = String.new(CrystalV2::Compiler::Frontend.node_enum_name(enum_node).not_nil!)
      enum_name.should eq("Status")

      CrystalV2::Compiler::Frontend.node_enum_base_type(enum_node).should be_nil

      members = CrystalV2::Compiler::Frontend.node_enum_members(enum_node).not_nil!
      members.size.should eq(0)
    end

    it "parses enum with simple members" do
      source = <<-CRYSTAL
        enum Color
          Red
          Green
          Blue
        end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      arena = program.arena
      enum_node = arena[program.roots.first]
      CrystalV2::Compiler::Frontend.node_kind(enum_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Enum)

      members = CrystalV2::Compiler::Frontend.node_enum_members(enum_node).not_nil!
      members.size.should eq(3)

      members[0].name.should eq("Red")
      members[0].value.should be_nil

      members[1].name.should eq("Green")
      members[1].value.should be_nil

      members[2].name.should eq("Blue")
      members[2].value.should be_nil
    end

    it "parses enum with member values" do
      source = <<-CRYSTAL
        enum Priority
          Low = 1
          Medium = 5
          High = 10
        end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      arena = program.arena
      enum_node = arena[program.roots.first]
      CrystalV2::Compiler::Frontend.node_kind(enum_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Enum)

      members = CrystalV2::Compiler::Frontend.node_enum_members(enum_node).not_nil!
      members.size.should eq(3)

      # Low = 1
      members[0].name.should eq("Low")
      members[0].value.should_not be_nil
      value_node = arena[members[0].value.not_nil!]
      CrystalV2::Compiler::Frontend.node_kind(value_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Number)

      # Medium = 5
      members[1].name.should eq("Medium")
      members[1].value.should_not be_nil

      # High = 10
      members[2].name.should eq("High")
      members[2].value.should_not be_nil
    end

    it "parses enum with base type" do
      source = <<-CRYSTAL
        enum Status : Int32
          Active
          Inactive
        end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      arena = program.arena
      enum_node = arena[program.roots.first]
      CrystalV2::Compiler::Frontend.node_kind(enum_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Enum)

      enum_name = String.new(CrystalV2::Compiler::Frontend.node_enum_name(enum_node).not_nil!)
      enum_name.should eq("Status")

      base_type = String.new(CrystalV2::Compiler::Frontend.node_enum_base_type(enum_node).not_nil!)
      base_type.should eq("Int32")

      members = CrystalV2::Compiler::Frontend.node_enum_members(enum_node).not_nil!
      members.size.should eq(2)
    end

    it "parses enum with mixed members" do
      source = <<-CRYSTAL
        enum Level
          Low = 1
          Medium
          High = 10
          Critical
        end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      arena = program.arena
      enum_node = arena[program.roots.first].as(CrystalV2::Compiler::Frontend::EnumNode)

      members = enum_node.members
      members.size.should eq(4)

      # Low = 1 (has value)
      members[0].name.should eq("Low")
      members[0].value.should_not be_nil

      # Medium (no value)
      members[1].name.should eq("Medium")
      members[1].value.should be_nil

      # High = 10 (has value)
      members[2].name.should eq("High")
      members[2].value.should_not be_nil

      # Critical (no value)
      members[3].name.should eq("Critical")
      members[3].value.should be_nil
    end

    it "parses enum with base type and values" do
      source = <<-CRYSTAL
        enum ErrorCode : Int32
          NotFound = 404
          ServerError = 500
          OK = 200
        end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      arena = program.arena
      enum_node = arena[program.roots.first].as(CrystalV2::Compiler::Frontend::EnumNode)

      base_type = String.new(enum_node.base_type.not_nil!)
      base_type.should eq("Int32")

      members = CrystalV2::Compiler::Frontend.node_enum_members(enum_node).not_nil!
      members.size.should eq(3)

      members.each do |member|
        member.value.should_not be_nil
      end
    end

    it "parses nested enum in class" do
      source = <<-CRYSTAL
        class Container
          enum Status
            Active
            Inactive
          end
        end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      arena = program.arena
      class_node = arena[program.roots.first]
      CrystalV2::Compiler::Frontend.node_kind(class_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Class)

      class_body = CrystalV2::Compiler::Frontend.node_class_body(class_node).not_nil!
      class_body.size.should eq(1)

      # Nested enum
      enum_node = arena[class_body[0]]
      CrystalV2::Compiler::Frontend.node_kind(enum_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Enum)

      enum_name = String.new(CrystalV2::Compiler::Frontend.node_enum_name(enum_node).not_nil!)
      enum_name.should eq("Status")
    end

    it "parses multiple enums" do
      source = <<-CRYSTAL
        enum Color
          Red
          Green
        end

        enum Status
          Active
          Inactive
        end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      program.roots.size.should eq(2)
      arena = program.arena

      # First enum
      enum1 = arena[program.roots[0]]
      CrystalV2::Compiler::Frontend.node_kind(enum1).should eq(CrystalV2::Compiler::Frontend::NodeKind::Enum)
      String.new(CrystalV2::Compiler::Frontend.node_enum_name(enum1).not_nil!).should eq("Color")

      # Second enum
      enum2 = arena[program.roots[1]]
      CrystalV2::Compiler::Frontend.node_kind(enum2).should eq(CrystalV2::Compiler::Frontend::NodeKind::Enum)
      String.new(CrystalV2::Compiler::Frontend.node_enum_name(enum2).not_nil!).should eq("Status")
    end

    it "parses enum with expression values" do
      source = <<-CRYSTAL
        enum Flags
          Read = 1
          Write = 2
          Execute = 4
          All = 7
        end
      CRYSTAL

      parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
      program = parser.parse_program

      arena = program.arena
      enum_node = arena[program.roots.first].as(CrystalV2::Compiler::Frontend::EnumNode)

      members = enum_node.members
      members.size.should eq(4)

      # All members should have values
      members.each do |member|
        member.value.should_not be_nil
      end
    end
  end
end
