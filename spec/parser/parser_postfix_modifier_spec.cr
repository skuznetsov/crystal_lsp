require "spec"

require "../../src/compiler/bootstrap_shims"
require "../../src/compiler/frontend/parser"

describe "CrystalV2::Compiler::Frontend::Parser postfix modifiers" do
  it "does not attach a following if to a multiline if-assignment" do
    source = <<-CRYSTAL
      def f(x = true) : Nil
        ret =
          if x
            1
          else
            2
          end

        if ret != 0
          4
        end
      end
    CRYSTAL

    parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
    program = parser.parse_program

    parser.diagnostics.should be_empty
    program.roots.size.should eq(1)

    arena = program.arena
    method_def = arena[program.roots.first].as(CrystalV2::Compiler::Frontend::DefNode)
    body = method_def.body.not_nil!

    body.size.should eq(2)
    arena[body[0]].should be_a(CrystalV2::Compiler::Frontend::AssignNode)
    arena[body[1]].should be_a(CrystalV2::Compiler::Frontend::IfNode)
  end

  it "keeps subsequent defs inside the surrounding module" do
    source = <<-CRYSTAL
      module Demo
        def first(x = true) : Nil
          ret =
            if x
              1
            else
              2
            end

          if ret != 0
            4
          end
        end

        def second
          5
        end
      end
    CRYSTAL

    parser = CrystalV2::Compiler::Frontend::Parser.new(CrystalV2::Compiler::Frontend::Lexer.new(source))
    program = parser.parse_program

    parser.diagnostics.should be_empty
    program.roots.size.should eq(1)

    arena = program.arena
    module_node = arena[program.roots.first].as(CrystalV2::Compiler::Frontend::ModuleNode)
    body = module_node.body.not_nil!

    body.size.should eq(2)
    arena[body[0]].should be_a(CrystalV2::Compiler::Frontend::DefNode)
    arena[body[1]].should be_a(CrystalV2::Compiler::Frontend::DefNode)
    String.new(arena[body[0]].as(CrystalV2::Compiler::Frontend::DefNode).name).should eq("first")
    String.new(arena[body[1]].as(CrystalV2::Compiler::Frontend::DefNode).name).should eq("second")
  end
end
