require "../spec_helper"
require "../../src/compiler/hir/hir"

describe Crystal::HIR do
  describe "LifetimeTag" do
    it "compares correctly" do
      Crystal::HIR::LifetimeTag::HeapEscape.escapes_more_than?(Crystal::HIR::LifetimeTag::StackLocal).should be_true
      Crystal::HIR::LifetimeTag::StackLocal.escapes_more_than?(Crystal::HIR::LifetimeTag::HeapEscape).should be_false
    end

    it "merges to most escaped" do
      Crystal::HIR::LifetimeTag::StackLocal.merge(Crystal::HIR::LifetimeTag::HeapEscape).should eq(Crystal::HIR::LifetimeTag::HeapEscape)
      Crystal::HIR::LifetimeTag::GlobalEscape.merge(Crystal::HIR::LifetimeTag::ArgEscape).should eq(Crystal::HIR::LifetimeTag::GlobalEscape)
    end
  end

  describe "TypeRef" do
    it "has primitive types" do
      Crystal::HIR::TypeRef::INT32.primitive?.should be_true
      Crystal::HIR::TypeRef::STRING.primitive?.should be_true
    end

    it "compares by id" do
      (Crystal::HIR::TypeRef::INT32 == Crystal::HIR::TypeRef::INT32).should be_true
      (Crystal::HIR::TypeRef::INT32 == Crystal::HIR::TypeRef::INT64).should be_false
    end
  end

  describe "Module" do
    it "creates empty module" do
      mod = Crystal::HIR::Module.new("test")
      mod.name.should eq("test")
      mod.functions.size.should eq(0)
    end

    it "interns strings" do
      mod = Crystal::HIR::Module.new
      id1 = mod.intern_string("hello")
      id2 = mod.intern_string("world")
      id3 = mod.intern_string("hello")

      id1.should eq(id3)  # Same string = same ID
      id1.should_not eq(id2)
      mod.strings.size.should eq(2)
    end

    it "creates function with entry block" do
      mod = Crystal::HIR::Module.new
      func = mod.create_function("foo", Crystal::HIR::TypeRef::VOID)

      func.name.should eq("foo")
      func.blocks.size.should eq(1)  # Entry block
      func.scopes.size.should eq(1)  # Function scope
    end

    it "includes virtual call targets by base name" do
      mod = Crystal::HIR::Module.new
      main = mod.create_function("__crystal_main", Crystal::HIR::TypeRef::VOID)
      receiver = main.add_param("self", Crystal::HIR::TypeRef::POINTER)

      call = Crystal::HIR::Call.new(
        1_u32,
        Crystal::HIR::TypeRef::VOID,
        receiver.id,
        "A#foo",
        [] of Crystal::HIR::ValueId,
        nil,
        true
      )
      main.blocks[0].add(call)

      mod.create_function("A#foo", Crystal::HIR::TypeRef::VOID)
      mod.create_function("B#foo", Crystal::HIR::TypeRef::VOID)
      mod.create_function("C#bar", Crystal::HIR::TypeRef::VOID)

      reachable = mod.reachable_function_names(["__crystal_main"])
      reachable.should contain("A#foo")
      reachable.should contain("B#foo")
      reachable.should_not contain("C#bar")
    end
  end

  describe "Function" do
    it "adds parameters" do
      mod = Crystal::HIR::Module.new
      func = mod.create_function("add", Crystal::HIR::TypeRef::INT32)

      param_a = func.add_param("a", Crystal::HIR::TypeRef::INT32)
      param_b = func.add_param("b", Crystal::HIR::TypeRef::INT32)

      func.params.size.should eq(2)
      param_a.name.should eq("a")
      param_a.index.should eq(0)
      param_b.index.should eq(1)
    end

    it "creates blocks and scopes" do
      mod = Crystal::HIR::Module.new
      func = mod.create_function("test", Crystal::HIR::TypeRef::VOID)

      # Entry block already exists
      func.blocks.size.should eq(1)

      # Create nested scope and block
      inner_scope = func.create_scope(Crystal::HIR::ScopeKind::Block, parent: 0_u32)
      inner_block = func.create_block(inner_scope)

      func.scopes.size.should eq(2)
      func.blocks.size.should eq(2)
      func.get_scope(inner_scope).parent.should eq(0_u32)
    end
  end

  describe "Values" do
    it "creates literal" do
      lit = Crystal::HIR::Literal.new(0_u32, Crystal::HIR::TypeRef::INT64, 42_i64)
      lit.value.should eq(42_i64)
      lit.lifetime.should eq(Crystal::HIR::LifetimeTag::StackLocal)
    end

    it "creates allocate" do
      user_type = Crystal::HIR::TypeRef.new(100_u32)
      alloc = Crystal::HIR::Allocate.new(0_u32, user_type)
      alloc.type.should eq(user_type)
      alloc.lifetime.should eq(Crystal::HIR::LifetimeTag::Unknown)
    end

    it "creates call" do
      call = Crystal::HIR::Call.new(
        id: 0_u32,
        type: Crystal::HIR::TypeRef::INT32,
        receiver: 1_u32,
        method_name: "foo",
        args: [2_u32, 3_u32]
      )
      call.method_name.should eq("foo")
      call.args.size.should eq(2)
    end

    it "creates closure with captures" do
      captures = [
        Crystal::HIR::CapturedVar.new(1_u32, "x", by_reference: true),
        Crystal::HIR::CapturedVar.new(2_u32, "y", by_reference: false),
      ]
      closure = Crystal::HIR::MakeClosure.new(0_u32, Crystal::HIR::TypeRef.new(50_u32), 5_u32, captures)

      closure.captures.size.should eq(2)
      closure.captures[0].by_reference.should be_true
      closure.captures[1].by_reference.should be_false
      closure.lifetime.should eq(Crystal::HIR::LifetimeTag::HeapEscape)
    end
  end

  describe "Terminators" do
    it "return has no successors" do
      ret = Crystal::HIR::Return.new(0_u32)
      ret.successors.should eq([] of Crystal::HIR::BlockId)
    end

    it "branch has two successors" do
      branch = Crystal::HIR::Branch.new(0_u32, 1_u32, 2_u32)
      branch.successors.should eq([1_u32, 2_u32])
    end

    it "jump has one successor" do
      jump = Crystal::HIR::Jump.new(5_u32)
      jump.successors.should eq([5_u32])
    end

    it "switch has multiple successors" do
      switch = Crystal::HIR::Switch.new(0_u32, [{1_u32, 2_u32}, {3_u32, 4_u32}], 5_u32)
      switch.successors.should eq([2_u32, 4_u32, 5_u32])
    end
  end

  describe "Block" do
    it "adds instructions" do
      block = Crystal::HIR::Block.new(0_u32, 0_u32)

      lit = Crystal::HIR::Literal.new(0_u32, Crystal::HIR::TypeRef::INT32, 1_i64)
      block.add(lit)

      block.instructions.size.should eq(1)
    end

    it "has default unreachable terminator" do
      block = Crystal::HIR::Block.new(0_u32, 0_u32)
      block.terminator.should be_a(Crystal::HIR::Unreachable)
    end
  end

  describe "Text output" do
    it "prints simple function" do
      mod = Crystal::HIR::Module.new("test")
      func = mod.create_function("add", Crystal::HIR::TypeRef::INT32)

      param_a = func.add_param("a", Crystal::HIR::TypeRef::INT32)
      param_b = func.add_param("b", Crystal::HIR::TypeRef::INT32)

      entry = func.get_block(func.entry_block)

      # %2 = binop Add %0, %1
      binop = Crystal::HIR::BinaryOperation.new(
        func.next_value_id,
        Crystal::HIR::TypeRef::INT32,
        Crystal::HIR::BinaryOp::Add,
        param_a.id,
        param_b.id
      )
      entry.add(binop)
      entry.terminator = Crystal::HIR::Return.new(binop.id)

      output = String.build { |io| mod.to_s(io) }

      output.should contain("func @add")
      output.should contain("binop Add")
      output.should contain("return %2")
    end
  end
end
