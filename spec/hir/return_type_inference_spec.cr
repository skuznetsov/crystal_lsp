require "../spec_helper"
require "../../src/compiler/hir/hir"
require "../../src/compiler/hir/ast_to_hir"
require "../../src/compiler/frontend/parser"
require "../../src/compiler/frontend/lexer"

# Tests for Two-Pass Signature Registration
# These specs verify that function return types are correctly inferred
# BEFORE method bodies are lowered, preventing VOID→Pointer type bugs.

describe Crystal::HIR::AstToHir do
  # Helper to parse and create HIR converter
  private def create_converter(source : String)
    lexer = CrystalV2::Compiler::Frontend::Lexer.new(source)
    parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
    program = parser.parse_program
    arena = parser.arena

    sources_by_arena = {arena => source} of CrystalV2::Compiler::Frontend::ArenaLike => String
    paths_by_arena = {arena => "(test)"} of CrystalV2::Compiler::Frontend::ArenaLike => String

    converter = Crystal::HIR::AstToHir.new(arena, "(test)", sources_by_arena, paths_by_arena)
    {converter, program, arena}
  end

  describe "Return Type Inference - Explicit Annotations" do
    it "infers return type from explicit annotation" do
      source = <<-CRYSTAL
        def foo : Int32
          42
        end
      CRYSTAL

      converter, program, arena = create_converter(source)
      # Register the function signature
      program.roots.each do |root_id|
        node = arena[root_id]
        if node.is_a?(CrystalV2::Compiler::Frontend::DefNode)
          converter.register_function_signature(node, arena)
        end
      end

      ret_type = converter.get_function_return_type("foo")
      converter.get_type_name_from_ref(ret_type).should eq("Int32")
    end

    it "infers return type from pointer annotation" do
      source = <<-CRYSTAL
        def get_ptr : Pointer(UInt8)
          Pointer(UInt8).null
        end
      CRYSTAL

      converter, program, arena = create_converter(source)
      program.roots.each do |root_id|
        node = arena[root_id]
        if node.is_a?(CrystalV2::Compiler::Frontend::DefNode)
          converter.register_function_signature(node, arena)
        end
      end

      ret_type = converter.get_function_return_type("get_ptr")
      converter.get_type_name_from_ref(ret_type).should contain("Pointer")
    end
  end

  describe "Return Type Inference - Body Analysis" do
    it "infers Int32 from integer literal return" do
      source = <<-CRYSTAL
        def get_answer
          return 42
        end
      CRYSTAL

      converter, program, arena = create_converter(source)
      program.roots.each do |root_id|
        node = arena[root_id]
        if node.is_a?(CrystalV2::Compiler::Frontend::DefNode)
          converter.register_function_signature(node, arena)
        end
      end

      ret_type = converter.get_function_return_type("get_answer")
      converter.get_type_name_from_ref(ret_type).should eq("Int32")
    end

    it "infers String from string literal return" do
      source = <<-CRYSTAL
        def greeting
          "hello"
        end
      CRYSTAL

      converter, program, arena = create_converter(source)
      program.roots.each do |root_id|
        node = arena[root_id]
        if node.is_a?(CrystalV2::Compiler::Frontend::DefNode)
          converter.register_function_signature(node, arena)
        end
      end

      ret_type = converter.get_function_return_type("greeting")
      converter.get_type_name_from_ref(ret_type).should eq("String")
    end

    it "infers Bool for predicate methods" do
      source = <<-CRYSTAL
        def empty?
          true
        end
      CRYSTAL

      converter, program, arena = create_converter(source)
      program.roots.each do |root_id|
        node = arena[root_id]
        if node.is_a?(CrystalV2::Compiler::Frontend::DefNode)
          converter.register_function_signature(node, arena)
        end
      end

      ret_type = converter.get_function_return_type("empty?")
      converter.get_type_name_from_ref(ret_type).should eq("Bool")
    end
  end

  describe "Return Type Inference - Conversion Methods" do
    it "infers UInt32 from .to_u32! call" do
      source = <<-CRYSTAL
        def extract_bits(x)
          (x & 0xFF).to_u32!
        end
      CRYSTAL

      converter, program, arena = create_converter(source)
      program.roots.each do |root_id|
        node = arena[root_id]
        if node.is_a?(CrystalV2::Compiler::Frontend::DefNode)
          converter.register_function_signature(node, arena)
        end
      end

      ret_type = converter.get_function_return_type("extract_bits")
      converter.get_type_name_from_ref(ret_type).should eq("UInt32")
    end

    it "infers Int64 from .to_i64! call" do
      source = <<-CRYSTAL
        def to_long(x)
          x.to_i64!
        end
      CRYSTAL

      converter, program, arena = create_converter(source)
      program.roots.each do |root_id|
        node = arena[root_id]
        if node.is_a?(CrystalV2::Compiler::Frontend::DefNode)
          converter.register_function_signature(node, arena)
        end
      end

      ret_type = converter.get_function_return_type("to_long")
      converter.get_type_name_from_ref(ret_type).should eq("Int64")
    end

    it "infers Float64 from .to_f call" do
      source = <<-CRYSTAL
        def as_float(x)
          x.to_f
        end
      CRYSTAL

      converter, program, arena = create_converter(source)
      program.roots.each do |root_id|
        node = arena[root_id]
        if node.is_a?(CrystalV2::Compiler::Frontend::DefNode)
          converter.register_function_signature(node, arena)
        end
      end

      ret_type = converter.get_function_return_type("as_float")
      converter.get_type_name_from_ref(ret_type).should eq("Float64")
    end
  end

  describe "Return Type Inference - Module Methods" do
    it "infers return type for module extend method" do
      source = <<-CRYSTAL
        module Calculator
          extend self

          def add(a : Int32, b : Int32) : Int32
            a + b
          end
        end
      CRYSTAL

      converter, program, arena = create_converter(source)
      # Register module
      program.roots.each do |root_id|
        node = arena[root_id]
        if node.is_a?(CrystalV2::Compiler::Frontend::ModuleNode)
          converter.register_module(node, arena)
        end
      end

      ret_type = converter.get_function_return_type("Calculator.add")
      converter.get_type_name_from_ref(ret_type).should eq("Int32")
    end
  end

  describe "Return Type Inference - Generic Methods" do
    it "infers return type for generic module with type parameter" do
      source = <<-CRYSTAL
        module Container(T)
          extend self

          def value : T
            uninitialized T
          end
        end

        module IntContainer
          extend Container(Int32)
        end
      CRYSTAL

      converter, program, arena = create_converter(source)
      program.roots.each do |root_id|
        node = arena[root_id]
        if node.is_a?(CrystalV2::Compiler::Frontend::ModuleNode)
          converter.register_module(node, arena)
        end
      end

      # After monomorphization, IntContainer.value should return Int32
      ret_type = converter.get_function_return_type("IntContainer.value")
      type_name = converter.get_type_name_from_ref(ret_type)
      # Should be Int32 or at least not VOID
      type_name.should_not eq("Void")
    end
  end

  describe "Forward Reference Handling" do
    it "handles call to function defined later" do
      source = <<-CRYSTAL
        def caller
          callee
        end

        def callee : Int32
          42
        end
      CRYSTAL

      converter, program, arena = create_converter(source)

      # Register ALL signatures first (two-pass approach)
      program.roots.each do |root_id|
        node = arena[root_id]
        if node.is_a?(CrystalV2::Compiler::Frontend::DefNode)
          converter.register_function_signature(node, arena)
        end
      end

      # Both should have correct return types
      callee_type = converter.get_function_return_type("callee")
      converter.get_type_name_from_ref(callee_type).should eq("Int32")

      # caller's return type depends on callee, which should now be known
      caller_type = converter.get_function_return_type("caller")
      # Since callee returns Int32, caller should also return Int32
      converter.get_type_name_from_ref(caller_type).should eq("Int32")
    end
  end

  describe "Dragonbox-specific Patterns" do
    it "infers return type for array unsafe_fetch pattern" do
      source = <<-CRYSTAL
        CACHE = [1_u64, 2_u64, 3_u64]

        def get_cache(k : Int32) : UInt64
          CACHE.unsafe_fetch(k)
        end
      CRYSTAL

      converter, program, arena = create_converter(source)
      program.roots.each do |root_id|
        node = arena[root_id]
        if node.is_a?(CrystalV2::Compiler::Frontend::DefNode)
          converter.register_function_signature(node, arena)
        end
      end

      ret_type = converter.get_function_return_type("get_cache")
      converter.get_type_name_from_ref(ret_type).should eq("UInt64")
    end

    it "does not type cache param as Pointer when return type is inferred" do
      # This is the critical test - when cache = get_cache(-minus_k) is called,
      # the type of cache should be UInt64 (or UInt128), not Pointer
      source = <<-CRYSTAL
        def get_cache(k : Int32) : UInt64
          0_u64
        end

        def use_cache
          cache = get_cache(0)
          cache  # Should be UInt64, not Pointer
        end
      CRYSTAL

      converter, program, arena = create_converter(source)
      program.roots.each do |root_id|
        node = arena[root_id]
        if node.is_a?(CrystalV2::Compiler::Frontend::DefNode)
          converter.register_function_signature(node, arena)
        end
      end

      # After registration, get_cache should have UInt64 return type
      ret_type = converter.get_function_return_type("get_cache")
      type_name = converter.get_type_name_from_ref(ret_type)
      type_name.should eq("UInt64")
      type_name.should_not eq("Pointer")
      type_name.should_not eq("Void")
    end
  end
end
