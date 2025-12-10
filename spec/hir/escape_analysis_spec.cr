require "../spec_helper"
require "../../src/compiler/hir/hir"
require "../../src/compiler/hir/escape_analysis"

# Helper to create a simple function for testing
private def create_function(name : String = "test") : {Crystal::HIR::Module, Crystal::HIR::Function}
  mod = Crystal::HIR::Module.new
  func = mod.create_function(name, Crystal::HIR::TypeRef::VOID)
  {mod, func}
end

describe Crystal::HIR::EscapeAnalyzer do

  # ═══════════════════════════════════════════════════════════════════════════
  # BASIC ESCAPE DETECTION
  # ═══════════════════════════════════════════════════════════════════════════

  describe "return escape detection" do
    it "marks returned values as HeapEscape" do
      mod, func = create_function

      entry = func.get_block(func.entry_block)

      # %0 = allocate SomeType
      alloc = Crystal::HIR::Allocate.new(func.next_value_id, Crystal::HIR::TypeRef.new(100_u32))
      entry.add(alloc)

      # return %0
      entry.terminator = Crystal::HIR::Return.new(alloc.id)

      analyzer = Crystal::HIR::EscapeAnalyzer.new(func)
      analyzer.analyze

      alloc.lifetime.should eq(Crystal::HIR::LifetimeTag::HeapEscape)
    end

    it "keeps non-returned values as StackLocal" do
      mod, func = create_function

      entry = func.get_block(func.entry_block)

      # %0 = literal 42
      lit = Crystal::HIR::Literal.new(func.next_value_id, Crystal::HIR::TypeRef::INT32, 42_i64)
      entry.add(lit)

      # %1 = allocate - not returned
      alloc = Crystal::HIR::Allocate.new(func.next_value_id, Crystal::HIR::TypeRef.new(100_u32))
      entry.add(alloc)

      # return %0
      entry.terminator = Crystal::HIR::Return.new(lit.id)

      analyzer = Crystal::HIR::EscapeAnalyzer.new(func)
      analyzer.analyze

      # Literal returned - escapes
      lit.lifetime.should eq(Crystal::HIR::LifetimeTag::HeapEscape)

      # Allocate not returned - stays local
      alloc.lifetime.should eq(Crystal::HIR::LifetimeTag::StackLocal)
    end
  end

  describe "closure capture detection" do
    it "marks closure-captured values as HeapEscape" do
      mod, func = create_function

      entry = func.get_block(func.entry_block)

      # %0 = literal 42
      lit = Crystal::HIR::Literal.new(func.next_value_id, Crystal::HIR::TypeRef::INT32, 42_i64)
      entry.add(lit)

      # Create a closure block
      closure_block = func.create_block(func.scopes[0].id)

      # %1 = make_closure capturing %0
      captures = [Crystal::HIR::CapturedVar.new(lit.id, "x", by_reference: true)]
      closure = Crystal::HIR::MakeClosure.new(func.next_value_id, Crystal::HIR::TypeRef.new(50_u32), closure_block, captures)
      entry.add(closure)

      # return nil
      entry.terminator = Crystal::HIR::Return.new(nil)

      analyzer = Crystal::HIR::EscapeAnalyzer.new(func)
      analyzer.analyze

      # Captured value escapes
      lit.lifetime.should eq(Crystal::HIR::LifetimeTag::HeapEscape)

      # Closure itself is stack local (not returned)
      closure.lifetime.should eq(Crystal::HIR::LifetimeTag::StackLocal)
    end

    it "marks returned closure as HeapEscape" do
      mod, func = create_function

      entry = func.get_block(func.entry_block)

      # Create closure block
      closure_block = func.create_block(func.scopes[0].id)

      # %0 = make_closure (no captures)
      closure = Crystal::HIR::MakeClosure.new(func.next_value_id, Crystal::HIR::TypeRef.new(50_u32), closure_block, [] of Crystal::HIR::CapturedVar)
      entry.add(closure)

      # return %0
      entry.terminator = Crystal::HIR::Return.new(closure.id)

      analyzer = Crystal::HIR::EscapeAnalyzer.new(func)
      analyzer.analyze

      closure.lifetime.should eq(Crystal::HIR::LifetimeTag::HeapEscape)
    end
  end

  describe "container add detection" do
    it "marks values added to containers as ArgEscape" do
      mod, func = create_function

      entry = func.get_block(func.entry_block)

      # %0 = allocate Array
      arr = Crystal::HIR::Allocate.new(func.next_value_id, Crystal::HIR::TypeRef.new(100_u32))
      entry.add(arr)

      # %1 = allocate Item
      item = Crystal::HIR::Allocate.new(func.next_value_id, Crystal::HIR::TypeRef.new(101_u32))
      entry.add(item)

      # %2 = call arr.<<(item)
      call = Crystal::HIR::Call.new(func.next_value_id, Crystal::HIR::TypeRef::VOID, arr.id, "<<", [item.id])
      entry.add(call)

      # return nil
      entry.terminator = Crystal::HIR::Return.new(nil)

      analyzer = Crystal::HIR::EscapeAnalyzer.new(func)
      analyzer.analyze

      # Item escapes into container
      item.lifetime.should eq(Crystal::HIR::LifetimeTag::ArgEscape)

      # Array stays local (not returned)
      arr.lifetime.should eq(Crystal::HIR::LifetimeTag::StackLocal)
    end

    it "detects various container add methods" do
      ["<<", "push", "unshift", "add", "insert", "[]=", "put", "store"].each do |method|
        mod, func = create_function

        entry = func.get_block(func.entry_block)

        container = Crystal::HIR::Allocate.new(func.next_value_id, Crystal::HIR::TypeRef.new(100_u32))
        entry.add(container)

        item = Crystal::HIR::Allocate.new(func.next_value_id, Crystal::HIR::TypeRef.new(101_u32))
        entry.add(item)

        # For []= we need an index
        args = method == "[]=" ? [Crystal::HIR::Literal.new(func.next_value_id, Crystal::HIR::TypeRef::INT32, 0_i64).tap { |l| entry.add(l) }.id, item.id] : [item.id]

        call = Crystal::HIR::Call.new(func.next_value_id, Crystal::HIR::TypeRef::VOID, container.id, method, args)
        entry.add(call)

        entry.terminator = Crystal::HIR::Return.new(nil)

        analyzer = Crystal::HIR::EscapeAnalyzer.new(func)
        analyzer.analyze

        item.lifetime.escapes_more_than?(Crystal::HIR::LifetimeTag::StackLocal).should be_true, "Method '#{method}' should cause escape"
      end
    end
  end

  describe "index set detection" do
    it "marks values stored via index as ArgEscape" do
      mod, func = create_function

      entry = func.get_block(func.entry_block)

      # %0 = allocate Array
      arr = Crystal::HIR::Allocate.new(func.next_value_id, Crystal::HIR::TypeRef.new(100_u32))
      entry.add(arr)

      # %1 = literal 0
      idx = Crystal::HIR::Literal.new(func.next_value_id, Crystal::HIR::TypeRef::INT32, 0_i64)
      entry.add(idx)

      # %2 = allocate Item
      item = Crystal::HIR::Allocate.new(func.next_value_id, Crystal::HIR::TypeRef.new(101_u32))
      entry.add(item)

      # %3 = index_set arr[0] = item
      index_set = Crystal::HIR::IndexSet.new(func.next_value_id, Crystal::HIR::TypeRef::VOID, arr.id, idx.id, item.id)
      entry.add(index_set)

      entry.terminator = Crystal::HIR::Return.new(nil)

      analyzer = Crystal::HIR::EscapeAnalyzer.new(func)
      analyzer.analyze

      item.lifetime.should eq(Crystal::HIR::LifetimeTag::ArgEscape)
    end
  end

  describe "global escape detection" do
    it "marks values stored in class vars as GlobalEscape" do
      mod, func = create_function

      entry = func.get_block(func.entry_block)

      # %0 = allocate Item
      item = Crystal::HIR::Allocate.new(func.next_value_id, Crystal::HIR::TypeRef.new(100_u32))
      entry.add(item)

      # %1 = class_var_set @@cache = item
      class_var_set = Crystal::HIR::ClassVarSet.new(func.next_value_id, Crystal::HIR::TypeRef::VOID, "MyClass", "cache", item.id)
      entry.add(class_var_set)

      entry.terminator = Crystal::HIR::Return.new(nil)

      analyzer = Crystal::HIR::EscapeAnalyzer.new(func)
      analyzer.analyze

      item.lifetime.should eq(Crystal::HIR::LifetimeTag::GlobalEscape)
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # ESCAPE PROPAGATION
  # ═══════════════════════════════════════════════════════════════════════════

  describe "escape propagation" do
    it "propagates escape through copy" do
      mod, func = create_function

      entry = func.get_block(func.entry_block)

      # %0 = allocate
      original = Crystal::HIR::Allocate.new(func.next_value_id, Crystal::HIR::TypeRef.new(100_u32))
      entry.add(original)

      # %1 = copy %0
      copy = Crystal::HIR::Copy.new(func.next_value_id, Crystal::HIR::TypeRef.new(100_u32), original.id)
      entry.add(copy)

      # return %1
      entry.terminator = Crystal::HIR::Return.new(copy.id)

      analyzer = Crystal::HIR::EscapeAnalyzer.new(func)
      analyzer.analyze

      # Copy escapes
      copy.lifetime.should eq(Crystal::HIR::LifetimeTag::HeapEscape)

      # Original also escapes (through copy)
      original.lifetime.should eq(Crystal::HIR::LifetimeTag::HeapEscape)
    end

    it "propagates escape through phi" do
      mod, func = create_function

      entry = func.get_block(func.entry_block)
      then_block = func.create_block(func.scopes[0].id)
      else_block = func.create_block(func.scopes[0].id)
      merge_block = func.create_block(func.scopes[0].id)

      # Entry: branch to then/else
      cond = Crystal::HIR::Literal.new(func.next_value_id, Crystal::HIR::TypeRef::BOOL, true)
      entry.add(cond)
      entry.terminator = Crystal::HIR::Branch.new(cond.id, then_block, else_block)

      # Then: allocate A
      alloc_a = Crystal::HIR::Allocate.new(func.next_value_id, Crystal::HIR::TypeRef.new(100_u32))
      func.get_block(then_block).add(alloc_a)
      func.get_block(then_block).terminator = Crystal::HIR::Jump.new(merge_block)

      # Else: allocate B
      alloc_b = Crystal::HIR::Allocate.new(func.next_value_id, Crystal::HIR::TypeRef.new(100_u32))
      func.get_block(else_block).add(alloc_b)
      func.get_block(else_block).terminator = Crystal::HIR::Jump.new(merge_block)

      # Merge: phi and return
      phi = Crystal::HIR::Phi.new(func.next_value_id, Crystal::HIR::TypeRef.new(100_u32))
      phi.add_incoming(then_block, alloc_a.id)
      phi.add_incoming(else_block, alloc_b.id)
      func.get_block(merge_block).add(phi)
      func.get_block(merge_block).terminator = Crystal::HIR::Return.new(phi.id)

      analyzer = Crystal::HIR::EscapeAnalyzer.new(func)
      analyzer.analyze

      # Both branches escape (through phi return)
      alloc_a.lifetime.should eq(Crystal::HIR::LifetimeTag::HeapEscape)
      alloc_b.lifetime.should eq(Crystal::HIR::LifetimeTag::HeapEscape)
      phi.lifetime.should eq(Crystal::HIR::LifetimeTag::HeapEscape)
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # ESCAPE SUMMARY
  # ═══════════════════════════════════════════════════════════════════════════

  describe "escape summary" do
    it "tracks parameter escapes" do
      mod, func = create_function

      # Add parameter
      param = func.add_param("x", Crystal::HIR::TypeRef.new(100_u32))

      entry = func.get_block(func.entry_block)

      # Store param in class var
      class_var_set = Crystal::HIR::ClassVarSet.new(func.next_value_id, Crystal::HIR::TypeRef::VOID, "Cache", "value", param.id)
      entry.add(class_var_set)

      entry.terminator = Crystal::HIR::Return.new(nil)

      analyzer = Crystal::HIR::EscapeAnalyzer.new(func)
      summary = analyzer.analyze

      summary.param_escapes[0].should eq(Crystal::HIR::LifetimeTag::GlobalEscape)
    end

    it "tracks return aliases to parameters" do
      mod, func = create_function

      # Add parameter
      param = func.add_param("x", Crystal::HIR::TypeRef.new(100_u32))

      entry = func.get_block(func.entry_block)

      # Return the parameter directly
      entry.terminator = Crystal::HIR::Return.new(param.id)

      analyzer = Crystal::HIR::EscapeAnalyzer.new(func)
      summary = analyzer.analyze

      summary.return_aliases_params.should contain(0)
    end

    it "tracks return aliases through copy" do
      mod, func = create_function

      param = func.add_param("x", Crystal::HIR::TypeRef.new(100_u32))

      entry = func.get_block(func.entry_block)

      # Copy param
      copy = Crystal::HIR::Copy.new(func.next_value_id, Crystal::HIR::TypeRef.new(100_u32), param.id)
      entry.add(copy)

      # Return copy
      entry.terminator = Crystal::HIR::Return.new(copy.id)

      analyzer = Crystal::HIR::EscapeAnalyzer.new(func)
      summary = analyzer.analyze

      summary.return_aliases_params.should contain(0)
    end

    it "tracks captures_args flag" do
      mod, func = create_function

      param = func.add_param("x", Crystal::HIR::TypeRef::INT32)

      entry = func.get_block(func.entry_block)

      closure_block = func.create_block(func.scopes[0].id)

      # Capture parameter in closure
      captures = [Crystal::HIR::CapturedVar.new(param.id, "x", by_reference: true)]
      closure = Crystal::HIR::MakeClosure.new(func.next_value_id, Crystal::HIR::TypeRef.new(50_u32), closure_block, captures)
      entry.add(closure)

      entry.terminator = Crystal::HIR::Return.new(nil)

      analyzer = Crystal::HIR::EscapeAnalyzer.new(func)
      summary = analyzer.analyze

      summary.captures_args.should be_true
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # CONVENIENCE METHODS
  # ═══════════════════════════════════════════════════════════════════════════

  describe "Function#analyze_escapes" do
    it "provides convenient access to escape analysis" do
      mod, func = create_function

      entry = func.get_block(func.entry_block)

      alloc = Crystal::HIR::Allocate.new(func.next_value_id, Crystal::HIR::TypeRef.new(100_u32))
      entry.add(alloc)

      entry.terminator = Crystal::HIR::Return.new(alloc.id)

      summary = func.analyze_escapes

      summary.should be_a(Crystal::HIR::EscapeSummary)
    end
  end

  describe "escaping_values helper" do
    it "returns all escaping values" do
      mod, func = create_function

      entry = func.get_block(func.entry_block)

      # %0 escapes (returned)
      alloc1 = Crystal::HIR::Allocate.new(func.next_value_id, Crystal::HIR::TypeRef.new(100_u32))
      entry.add(alloc1)

      # %1 does not escape
      alloc2 = Crystal::HIR::Allocate.new(func.next_value_id, Crystal::HIR::TypeRef.new(100_u32))
      entry.add(alloc2)

      entry.terminator = Crystal::HIR::Return.new(alloc1.id)

      analyzer = Crystal::HIR::EscapeAnalyzer.new(func)
      analyzer.analyze

      escaping = analyzer.escaping_values
      escaping.should contain(alloc1.id)
      escaping.should_not contain(alloc2.id)
    end
  end
end
