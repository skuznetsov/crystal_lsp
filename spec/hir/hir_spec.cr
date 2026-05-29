require "../spec_helper"
require "../../src/compiler/hir/hir"

describe Adamas::HIR do
  describe "LifetimeTag" do
    it "compares correctly" do
      Adamas::HIR::LifetimeTag::HeapEscape.escapes_more_than?(Adamas::HIR::LifetimeTag::StackLocal).should be_true
      Adamas::HIR::LifetimeTag::StackLocal.escapes_more_than?(Adamas::HIR::LifetimeTag::HeapEscape).should be_false
    end

    it "merges to most escaped" do
      Adamas::HIR::LifetimeTag::StackLocal.merge(Adamas::HIR::LifetimeTag::HeapEscape).should eq(Adamas::HIR::LifetimeTag::HeapEscape)
      Adamas::HIR::LifetimeTag::GlobalEscape.merge(Adamas::HIR::LifetimeTag::ArgEscape).should eq(Adamas::HIR::LifetimeTag::GlobalEscape)
    end
  end

  describe "TypeRef" do
    it "has primitive types" do
      Adamas::HIR::TypeRef::INT32.primitive?.should be_true
      Adamas::HIR::TypeRef::STRING.primitive?.should be_true
    end

    it "compares by id" do
      (Adamas::HIR::TypeRef::INT32 == Adamas::HIR::TypeRef::INT32).should be_true
      (Adamas::HIR::TypeRef::INT32 == Adamas::HIR::TypeRef::INT64).should be_false
    end
  end

  describe "Module" do
    it "creates empty module" do
      mod = Adamas::HIR::Module.new("test")
      mod.name.should eq("test")
      mod.functions.size.should eq(0)
    end

    it "interns strings" do
      mod = Adamas::HIR::Module.new
      id1 = mod.intern_string("hello")
      id2 = mod.intern_string("world")
      id3 = mod.intern_string("hello")

      id1.should eq(id3)  # Same string = same ID
      id1.should_not eq(id2)
      mod.strings.size.should eq(2)
    end

    it "creates function with entry block" do
      mod = Adamas::HIR::Module.new
      func = mod.create_function("foo", Adamas::HIR::TypeRef::VOID)

      func.name.should eq("foo")
      func.blocks.size.should eq(1)  # Entry block
      func.scopes.size.should eq(1)  # Function scope
    end

    it "includes virtual call targets by base name" do
      mod = Adamas::HIR::Module.new
      main = mod.create_function("__adamas_main", Adamas::HIR::TypeRef::VOID)
      receiver = main.add_param("self", Adamas::HIR::TypeRef::POINTER)

      call = Adamas::HIR::Call.new(
        1_u32,
        Adamas::HIR::TypeRef::VOID,
        receiver.id,
        "A#foo",
        [] of Adamas::HIR::ValueId,
        nil,
        true
      )
      main.blocks[0].add(call)

      # Register B as subclass of A, so virtual call to A#foo reaches B#foo
      mod.class_parents["B"] = "A"

      # RTA needs types to be instantiated (via Allocate) to include them
      a_type_ref = mod.intern_type(Adamas::HIR::TypeDescriptor.new(Adamas::HIR::TypeKind::Class, "A"))
      b_type_ref = mod.intern_type(Adamas::HIR::TypeDescriptor.new(Adamas::HIR::TypeKind::Class, "B"))
      alloc_a = Adamas::HIR::Allocate.new(2_u32, a_type_ref)
      alloc_b = Adamas::HIR::Allocate.new(3_u32, b_type_ref)
      main.blocks[0].add(alloc_a)
      main.blocks[0].add(alloc_b)

      mod.create_function("A#foo", Adamas::HIR::TypeRef::VOID)
      mod.create_function("B#foo", Adamas::HIR::TypeRef::VOID)
      mod.create_function("C#bar", Adamas::HIR::TypeRef::VOID)

      reachable = mod.reachable_function_names(["__adamas_main"])
      reachable.should contain("A#foo")
      reachable.should contain("B#foo")
      reachable.should_not contain("C#bar")
    end
  end

  describe "Function" do
    it "adds parameters" do
      mod = Adamas::HIR::Module.new
      func = mod.create_function("add", Adamas::HIR::TypeRef::INT32)

      param_a = func.add_param("a", Adamas::HIR::TypeRef::INT32)
      param_b = func.add_param("b", Adamas::HIR::TypeRef::INT32)

      func.params.size.should eq(2)
      param_a.name.should eq("a")
      param_a.index.should eq(0)
      param_b.index.should eq(1)
    end

    it "creates blocks and scopes" do
      mod = Adamas::HIR::Module.new
      func = mod.create_function("test", Adamas::HIR::TypeRef::VOID)

      # Entry block already exists
      func.blocks.size.should eq(1)

      # Create nested scope and block
      inner_scope = func.create_scope(Adamas::HIR::ScopeKind::Block, parent: 0_u32)
      inner_block = func.create_block(inner_scope)

      func.scopes.size.should eq(2)
      func.blocks.size.should eq(2)
      func.get_scope(inner_scope).parent.should eq(0_u32)
    end
  end

  describe "Values" do
    it "creates literal" do
      lit = Adamas::HIR::Literal.new(0_u32, Adamas::HIR::TypeRef::INT64, 42_i64)
      lit.value.should eq(42_i64)
      lit.lifetime.should eq(Adamas::HIR::LifetimeTag::StackLocal)
    end

    it "creates allocate" do
      user_type = Adamas::HIR::TypeRef.new(100_u32)
      alloc = Adamas::HIR::Allocate.new(0_u32, user_type)
      alloc.type.should eq(user_type)
      alloc.lifetime.should eq(Adamas::HIR::LifetimeTag::Unknown)
    end

    it "creates call" do
      call = Adamas::HIR::Call.new(
        id: 0_u32,
        type: Adamas::HIR::TypeRef::INT32,
        receiver: 1_u32,
        method_name: "foo",
        args: [2_u32, 3_u32]
      )
      call.method_name.should eq("foo")
      call.args.size.should eq(2)
    end

    it "creates closure with captures" do
      captures = [
        Adamas::HIR::CapturedVar.new(1_u32, "x", by_reference: true),
        Adamas::HIR::CapturedVar.new(2_u32, "y", by_reference: false),
      ]
      closure = Adamas::HIR::MakeClosure.new(0_u32, Adamas::HIR::TypeRef.new(50_u32), 5_u32, captures)

      closure.captures.size.should eq(2)
      closure.captures[0].by_reference.should be_true
      closure.captures[1].by_reference.should be_false
      closure.lifetime.should eq(Adamas::HIR::LifetimeTag::HeapEscape)
    end
  end

  describe "Terminators" do
    it "return has no successors" do
      ret = Adamas::HIR::Return.new(0_u32)
      ret.successors.should eq([] of Adamas::HIR::BlockId)
    end

    it "branch has two successors" do
      branch = Adamas::HIR::Branch.new(0_u32, 1_u32, 2_u32)
      branch.successors.should eq([1_u32, 2_u32])
    end

    it "jump has one successor" do
      jump = Adamas::HIR::Jump.new(5_u32)
      jump.successors.should eq([5_u32])
    end

    it "switch has multiple successors" do
      switch = Adamas::HIR::Switch.new(0_u32, [{1_u32, 2_u32}, {3_u32, 4_u32}], 5_u32)
      switch.successors.should eq([2_u32, 4_u32, 5_u32])
    end
  end

  describe "Block" do
    it "adds instructions" do
      block = Adamas::HIR::Block.new(0_u32, 0_u32)

      lit = Adamas::HIR::Literal.new(0_u32, Adamas::HIR::TypeRef::INT32, 1_i64)
      block.add(lit)

      block.instructions.size.should eq(1)
    end

    it "has default unreachable terminator" do
      block = Adamas::HIR::Block.new(0_u32, 0_u32)
      block.terminator.should be_a(Adamas::HIR::Unreachable)
    end
  end

  describe "Text output" do
    it "prints simple function" do
      mod = Adamas::HIR::Module.new("test")
      func = mod.create_function("add", Adamas::HIR::TypeRef::INT32)

      param_a = func.add_param("a", Adamas::HIR::TypeRef::INT32)
      param_b = func.add_param("b", Adamas::HIR::TypeRef::INT32)

      entry = func.get_block(func.entry_block)

      # %2 = binop Add %0, %1
      binop = Adamas::HIR::BinaryOperation.new(
        func.next_value_id,
        Adamas::HIR::TypeRef::INT32,
        Adamas::HIR::BinaryOp::Add,
        param_a.id,
        param_b.id
      )
      entry.add(binop)
      entry.terminator = Adamas::HIR::Return.new(binop.id)

      output = String.build { |io| mod.to_s(io) }

      output.should contain("func @add")
      output.should contain("binop Add")
      output.should contain("return %2")
    end
  end
end
