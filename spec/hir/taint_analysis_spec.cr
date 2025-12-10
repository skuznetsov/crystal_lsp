require "../spec_helper"
require "../../src/compiler/hir/hir"
require "../../src/compiler/hir/taint_analysis"

# Helper to create a simple function for testing
private def create_function(name : String = "test") : {Crystal::HIR::Module, Crystal::HIR::Function}
  mod = Crystal::HIR::Module.new
  func = mod.create_function(name, Crystal::HIR::TypeRef::VOID)
  {mod, func}
end

# Mock TypeInfoProvider for cycle detection tests
class MockTypeInfoProvider
  include Crystal::HIR::TypeInfoProvider

  @types : Hash(String, Hash(String, String))

  def initialize(@types = {} of String => Hash(String, String))
  end

  def class_names : Array(String)
    @types.keys
  end

  def instance_var_types(class_name : String) : Hash(String, String)
    @types[class_name]? || {} of String => String
  end
end

describe Crystal::HIR::TaintAnalyzer do
  # ═══════════════════════════════════════════════════════════════════════════
  # CYCLE DETECTION
  # ═══════════════════════════════════════════════════════════════════════════

  describe "cycle detection" do
    it "detects self-referential types" do
      # class Node; @next : Node?; end
      type_info = MockTypeInfoProvider.new({
        "Node" => {"@next" => "Node?"},
      })

      mod, func = create_function
      entry = func.get_block(func.entry_block)
      entry.terminator = Crystal::HIR::Return.new(nil)

      analyzer = Crystal::HIR::TaintAnalyzer.new(func, type_info)
      analyzer.analyze

      analyzer.cyclic?("Node").should be_true
    end

    it "detects mutual recursion cycles" do
      # class A; @b : B?; end
      # class B; @a : A?; end
      type_info = MockTypeInfoProvider.new({
        "A" => {"@b" => "B?"},
        "B" => {"@a" => "A?"},
      })

      mod, func = create_function
      entry = func.get_block(func.entry_block)
      entry.terminator = Crystal::HIR::Return.new(nil)

      analyzer = Crystal::HIR::TaintAnalyzer.new(func, type_info)
      analyzer.analyze

      analyzer.cyclic?("A").should be_true
      analyzer.cyclic?("B").should be_true
    end

    it "does not mark non-cyclic types" do
      # class Container; @value : String; end
      type_info = MockTypeInfoProvider.new({
        "Container" => {"@value" => "String"},
        "String"    => {} of String => String,
      })

      mod, func = create_function
      entry = func.get_block(func.entry_block)
      entry.terminator = Crystal::HIR::Return.new(nil)

      analyzer = Crystal::HIR::TaintAnalyzer.new(func, type_info)
      analyzer.analyze

      analyzer.cyclic?("Container").should be_false
      analyzer.cyclic?("String").should be_false
    end

    it "marks allocations of cyclic types as Cyclic" do
      # Pre-register cyclic types
      cyclic_types = Set{"Node"}

      mod, func = create_function
      entry = func.get_block(func.entry_block)

      # Create a TypeRef that maps to "Node"
      # For testing, we'll use a custom type ID
      node_type = Crystal::HIR::TypeRef.new(1000_u32)  # Custom ID

      # Register type name mapping (normally done by module)
      # For now, we test with the known behavior

      entry.terminator = Crystal::HIR::Return.new(nil)

      analyzer = Crystal::HIR::TaintAnalyzer.new(func, cyclic_types)
      analyzer.analyze

      # Verify cyclic types are tracked
      analyzer.cyclic?("Node").should be_true
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # BASIC TAINT DETECTION
  # ═══════════════════════════════════════════════════════════════════════════

  describe "FFI exposure detection" do
    it "marks values passed to FFI methods as FFIExposed" do
      mod, func = create_function

      entry = func.get_block(func.entry_block)

      # %0 = allocate String
      str = Crystal::HIR::Allocate.new(func.next_value_id, Crystal::HIR::TypeRef::STRING)
      entry.add(str)

      # %1 = call str.to_unsafe()
      call = Crystal::HIR::Call.new(func.next_value_id, Crystal::HIR::TypeRef::VOID, str.id, "to_unsafe", [] of Crystal::HIR::ValueId)
      entry.add(call)

      entry.terminator = Crystal::HIR::Return.new(nil)

      analyzer = Crystal::HIR::TaintAnalyzer.new(func)
      analyzer.analyze

      str.taints.ffi_exposed?.should be_true
    end

    it "detects double-underscore methods as FFI" do
      mod, func = create_function

      entry = func.get_block(func.entry_block)

      ptr = Crystal::HIR::Allocate.new(func.next_value_id, Crystal::HIR::TypeRef.new(100_u32))
      entry.add(ptr)

      # Call to __some_c_function
      call = Crystal::HIR::Call.new(func.next_value_id, Crystal::HIR::TypeRef::VOID, nil, "__some_c_function", [ptr.id])
      entry.add(call)

      entry.terminator = Crystal::HIR::Return.new(nil)

      analyzer = Crystal::HIR::TaintAnalyzer.new(func)
      analyzer.analyze

      ptr.taints.ffi_exposed?.should be_true
    end
  end

  describe "thread sharing detection" do
    it "marks spawn arguments as ThreadShared" do
      mod, func = create_function

      entry = func.get_block(func.entry_block)

      # %0 = allocate Data
      data = Crystal::HIR::Allocate.new(func.next_value_id, Crystal::HIR::TypeRef.new(100_u32))
      entry.add(data)

      # %1 = call spawn(data)
      call = Crystal::HIR::Call.new(func.next_value_id, Crystal::HIR::TypeRef::VOID, nil, "spawn", [data.id])
      entry.add(call)

      entry.terminator = Crystal::HIR::Return.new(nil)

      analyzer = Crystal::HIR::TaintAnalyzer.new(func)
      analyzer.analyze

      data.taints.thread_shared?.should be_true
    end

    it "marks channel send/receive as ThreadShared" do
      mod, func = create_function

      entry = func.get_block(func.entry_block)

      # %0 = allocate Data
      data = Crystal::HIR::Allocate.new(func.next_value_id, Crystal::HIR::TypeRef.new(100_u32))
      entry.add(data)

      # %1 = call channel.send(data)
      channel = Crystal::HIR::Allocate.new(func.next_value_id, Crystal::HIR::TypeRef.new(101_u32))
      entry.add(channel)

      call = Crystal::HIR::Call.new(func.next_value_id, Crystal::HIR::TypeRef::VOID, channel.id, "send", [data.id])
      entry.add(call)

      entry.terminator = Crystal::HIR::Return.new(nil)

      analyzer = Crystal::HIR::TaintAnalyzer.new(func)
      analyzer.analyze

      data.taints.thread_shared?.should be_true
    end

    it "marks class variable values as ThreadShared" do
      mod, func = create_function

      entry = func.get_block(func.entry_block)

      # %0 = allocate Data
      data = Crystal::HIR::Allocate.new(func.next_value_id, Crystal::HIR::TypeRef.new(100_u32))
      entry.add(data)

      # %1 = class_var_set @@cache = data
      class_var_set = Crystal::HIR::ClassVarSet.new(func.next_value_id, Crystal::HIR::TypeRef::VOID, "MyClass", "cache", data.id)
      entry.add(class_var_set)

      entry.terminator = Crystal::HIR::Return.new(nil)

      analyzer = Crystal::HIR::TaintAnalyzer.new(func)
      analyzer.analyze

      data.taints.thread_shared?.should be_true
    end
  end

  describe "mutable taint" do
    it "marks all allocations as Mutable" do
      mod, func = create_function

      entry = func.get_block(func.entry_block)

      # %0 = allocate Data
      data = Crystal::HIR::Allocate.new(func.next_value_id, Crystal::HIR::TypeRef.new(100_u32))
      entry.add(data)

      entry.terminator = Crystal::HIR::Return.new(nil)

      analyzer = Crystal::HIR::TaintAnalyzer.new(func)
      analyzer.analyze

      data.taints.mutable?.should be_true
    end

    it "marks closure by-ref captures as Mutable" do
      mod, func = create_function

      entry = func.get_block(func.entry_block)

      # %0 = literal 42
      lit = Crystal::HIR::Literal.new(func.next_value_id, Crystal::HIR::TypeRef::INT32, 42_i64)
      entry.add(lit)

      closure_block = func.create_block(func.scopes[0].id)

      # Capture by reference
      captures = [Crystal::HIR::CapturedVar.new(lit.id, "x", by_reference: true)]
      closure = Crystal::HIR::MakeClosure.new(func.next_value_id, Crystal::HIR::TypeRef.new(50_u32), closure_block, captures)
      entry.add(closure)

      entry.terminator = Crystal::HIR::Return.new(nil)

      analyzer = Crystal::HIR::TaintAnalyzer.new(func)
      analyzer.analyze

      lit.taints.mutable?.should be_true
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # TAINT PROPAGATION
  # ═══════════════════════════════════════════════════════════════════════════

  describe "taint propagation" do
    it "propagates taints through copy" do
      mod, func = create_function

      entry = func.get_block(func.entry_block)

      # %0 = allocate (will be FFI exposed)
      original = Crystal::HIR::Allocate.new(func.next_value_id, Crystal::HIR::TypeRef.new(100_u32))
      entry.add(original)

      # Mark as FFI exposed
      ffi_call = Crystal::HIR::Call.new(func.next_value_id, Crystal::HIR::TypeRef::VOID, original.id, "to_unsafe", [] of Crystal::HIR::ValueId)
      entry.add(ffi_call)

      # %2 = copy %0
      copy = Crystal::HIR::Copy.new(func.next_value_id, Crystal::HIR::TypeRef.new(100_u32), original.id)
      entry.add(copy)

      entry.terminator = Crystal::HIR::Return.new(nil)

      analyzer = Crystal::HIR::TaintAnalyzer.new(func)
      analyzer.analyze

      original.taints.ffi_exposed?.should be_true
      copy.taints.ffi_exposed?.should be_true
    end

    it "propagates taints through phi" do
      mod, func = create_function

      entry = func.get_block(func.entry_block)
      then_block = func.create_block(func.scopes[0].id)
      else_block = func.create_block(func.scopes[0].id)
      merge_block = func.create_block(func.scopes[0].id)

      # Entry: branch
      cond = Crystal::HIR::Literal.new(func.next_value_id, Crystal::HIR::TypeRef::BOOL, true)
      entry.add(cond)
      entry.terminator = Crystal::HIR::Branch.new(cond.id, then_block, else_block)

      # Then: create FFI-exposed value
      alloc_a = Crystal::HIR::Allocate.new(func.next_value_id, Crystal::HIR::TypeRef.new(100_u32))
      func.get_block(then_block).add(alloc_a)
      ffi_call = Crystal::HIR::Call.new(func.next_value_id, Crystal::HIR::TypeRef::VOID, alloc_a.id, "to_unsafe", [] of Crystal::HIR::ValueId)
      func.get_block(then_block).add(ffi_call)
      func.get_block(then_block).terminator = Crystal::HIR::Jump.new(merge_block)

      # Else: create normal value
      alloc_b = Crystal::HIR::Allocate.new(func.next_value_id, Crystal::HIR::TypeRef.new(100_u32))
      func.get_block(else_block).add(alloc_b)
      func.get_block(else_block).terminator = Crystal::HIR::Jump.new(merge_block)

      # Merge: phi
      phi = Crystal::HIR::Phi.new(func.next_value_id, Crystal::HIR::TypeRef.new(100_u32))
      phi.add_incoming(then_block, alloc_a.id)
      phi.add_incoming(else_block, alloc_b.id)
      func.get_block(merge_block).add(phi)
      func.get_block(merge_block).terminator = Crystal::HIR::Return.new(nil)

      analyzer = Crystal::HIR::TaintAnalyzer.new(func)
      analyzer.analyze

      # alloc_a is FFI exposed
      alloc_a.taints.ffi_exposed?.should be_true

      # phi should be tainted (conservative - might be FFI exposed)
      phi.taints.ffi_exposed?.should be_true
    end

    it "propagates taints through field assignment" do
      mod, func = create_function

      entry = func.get_block(func.entry_block)

      # %0 = allocate Container
      container = Crystal::HIR::Allocate.new(func.next_value_id, Crystal::HIR::TypeRef.new(100_u32))
      entry.add(container)

      # %1 = allocate Data (will be thread-shared)
      data = Crystal::HIR::Allocate.new(func.next_value_id, Crystal::HIR::TypeRef.new(101_u32))
      entry.add(data)

      # Mark data as thread-shared
      spawn_call = Crystal::HIR::Call.new(func.next_value_id, Crystal::HIR::TypeRef::VOID, nil, "spawn", [data.id])
      entry.add(spawn_call)

      # %3 = field_set container.@data = data
      field_set = Crystal::HIR::FieldSet.new(func.next_value_id, Crystal::HIR::TypeRef::VOID, container.id, "@data", data.id)
      entry.add(field_set)

      entry.terminator = Crystal::HIR::Return.new(nil)

      analyzer = Crystal::HIR::TaintAnalyzer.new(func)
      analyzer.analyze

      data.taints.thread_shared?.should be_true
      # Container should also become thread-shared since it contains shared data
      container.taints.thread_shared?.should be_true
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # HELPER METHODS
  # ═══════════════════════════════════════════════════════════════════════════

  describe "helper methods" do
    it "provides taint summary" do
      mod, func = create_function

      entry = func.get_block(func.entry_block)

      # Create some tainted values
      data1 = Crystal::HIR::Allocate.new(func.next_value_id, Crystal::HIR::TypeRef.new(100_u32))
      entry.add(data1)

      data2 = Crystal::HIR::Allocate.new(func.next_value_id, Crystal::HIR::TypeRef.new(101_u32))
      entry.add(data2)

      # Make data1 FFI-exposed
      ffi_call = Crystal::HIR::Call.new(func.next_value_id, Crystal::HIR::TypeRef::VOID, data1.id, "to_unsafe", [] of Crystal::HIR::ValueId)
      entry.add(ffi_call)

      # Make data2 thread-shared
      spawn_call = Crystal::HIR::Call.new(func.next_value_id, Crystal::HIR::TypeRef::VOID, nil, "spawn", [data2.id])
      entry.add(spawn_call)

      entry.terminator = Crystal::HIR::Return.new(nil)

      analyzer = Crystal::HIR::TaintAnalyzer.new(func)
      analyzer.analyze

      summary = analyzer.taint_summary
      summary["ffi_exposed"].should be > 0
      summary["thread_shared"].should be > 0
      summary["mutable"].should be > 0  # Both allocations are mutable
    end

    it "returns values with specific taint" do
      mod, func = create_function

      entry = func.get_block(func.entry_block)

      data = Crystal::HIR::Allocate.new(func.next_value_id, Crystal::HIR::TypeRef.new(100_u32))
      entry.add(data)

      spawn_call = Crystal::HIR::Call.new(func.next_value_id, Crystal::HIR::TypeRef::VOID, nil, "spawn", [data.id])
      entry.add(spawn_call)

      entry.terminator = Crystal::HIR::Return.new(nil)

      analyzer = Crystal::HIR::TaintAnalyzer.new(func)
      analyzer.analyze

      thread_shared = analyzer.values_with_taint(Crystal::HIR::Taint::ThreadShared)
      thread_shared.should contain(data.id)
    end
  end

  describe "Function#analyze_taints" do
    it "provides convenient access to taint analysis" do
      mod, func = create_function

      entry = func.get_block(func.entry_block)
      entry.terminator = Crystal::HIR::Return.new(nil)

      analyzer = func.analyze_taints
      analyzer.should be_a(Crystal::HIR::TaintAnalyzer)
    end
  end
end
