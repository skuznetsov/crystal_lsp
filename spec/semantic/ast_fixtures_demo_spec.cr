require "spec"
require "./ast_fixtures"

require "../../src/compiler/frontend/ast"

# Demonstration of AST fixtures usage
describe "AstFixtures" do
  it "creates def nodes with parameters" do
    arena = CrystalV2::Compiler::Frontend::AstArena.new

    # Create: def greet(name)
    #           name
    #         end
    name_id = AstFixtures.make_identifier(arena, "name")
    def_id = AstFixtures.make_def(arena, "greet", params: ["name"], body: [name_id])

    node = arena[def_id]
    CrystalV2::Compiler::Frontend.node_kind(node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Def)
    String.new(CrystalV2::Compiler::Frontend.node_def_name(node).not_nil!).should eq("greet")
    CrystalV2::Compiler::Frontend.node_def_params(node).not_nil!.map(&.name).should eq(["name"])
    CrystalV2::Compiler::Frontend.node_def_body(node).not_nil!.size.should eq(1)
  end

  it "creates class nodes with methods" do
    arena = CrystalV2::Compiler::Frontend::AstArena.new

    # Create: class Person
    #           def greet
    #           end
    #         end
    greet_id = AstFixtures.make_def(arena, "greet")
    class_id = AstFixtures.make_class(arena, "Person", body: [greet_id])

    node = arena[class_id]
    CrystalV2::Compiler::Frontend.node_kind(node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Class)
    String.new(CrystalV2::Compiler::Frontend.node_class_name(node).not_nil!).should eq("Person")
    CrystalV2::Compiler::Frontend.node_class_body(node).not_nil!.size.should eq(1)
  end

  it "creates method calls" do
    arena = CrystalV2::Compiler::Frontend::AstArena.new

    # Create: greet("World")
    arg_id = AstFixtures.make_string(arena, "World")
    call_id = AstFixtures.make_call(arena, "greet", args: [arg_id])

    node = arena[call_id]
    CrystalV2::Compiler::Frontend.node_kind(node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Call)
    CrystalV2::Compiler::Frontend.node_args(node).not_nil!.size.should eq(1)
  end

  it "creates nested def in class" do
    arena = CrystalV2::Compiler::Frontend::AstArena.new

    # Create: class Greeter
    #           def say_hello
    #             greet("World")
    #           end
    #         end
    arg_id = AstFixtures.make_string(arena, "World")
    call_id = AstFixtures.make_call(arena, "greet", args: [arg_id])
    method_id = AstFixtures.make_def(arena, "say_hello", body: [call_id])
    class_id = AstFixtures.make_class(arena, "Greeter", body: [method_id])

    class_node = arena[class_id]
    CrystalV2::Compiler::Frontend.node_class_body(class_node).not_nil!.size.should eq(1)

    method_node = arena[CrystalV2::Compiler::Frontend.node_class_body(class_node).not_nil![0]]
    CrystalV2::Compiler::Frontend.node_kind(method_node).should eq(CrystalV2::Compiler::Frontend::NodeKind::Def)
    CrystalV2::Compiler::Frontend.node_def_body(method_node).not_nil!.size.should eq(1)
  end
end
