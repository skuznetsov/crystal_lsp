require "../spec_helper"
require "../../src/compiler/hir/ast_to_hir"
require "../../src/compiler/frontend/parser"
require "../../src/compiler/frontend/lexer"

# Test-only access to private parsing helpers (keeps production API small).
class Adamas::HIR::AstToHir
  def __test_split_generic_type_args(params_str : String) : Array(String)
    split_generic_type_args(params_str)
  end

  def __test_safe_slice_to_string(slice : Slice(UInt8)) : String?
    safe_slice_to_string(slice)
  end

  def __test_lower_function_if_needed(name : String) : Nil
    lower_function_if_needed(name)
  end

  def __test_get_function_return_type(name : String) : Adamas::HIR::TypeRef
    get_function_return_type(name)
  end

  def __test_type_ref_for_name(name : String) : Adamas::HIR::TypeRef
    type_ref_for_name(name)
  end

  def __test_get_type_name_from_ref(type_ref : Adamas::HIR::TypeRef) : String
    get_type_name_from_ref(type_ref)
  end

  def __test_repair_stale_call_return_types : Nil
    repair_stale_call_return_types
  end

  def __test_queue_pending_inside_lowering(name : String) : Nil
    old_depth = @lowering_depth
    @lowering_depth = @lowering_depth_limit + 1
    lower_function_if_needed(name)
  ensure
    @lowering_depth = old_depth.as(Int32)
  end

  def __test_rta_called_method?(name : String) : Bool
    @rta_called_methods.includes?(name)
  end

  def __test_rta_called_method_part?(name : String) : Bool
    @rta_called_method_parts.includes?(name)
  end

  def __test_pending_function?(name : String) : Bool
    @pending_function_queue.includes?(name)
  end

  def __test_remember_callsite_arg_types(name : String, arg_types : Array(Adamas::HIR::TypeRef), has_block : Bool = false) : Nil
    remember_callsite_arg_types(name, arg_types, has_block: has_block)
  end

  def __test_repair_partial_untyped_call_types_from_history(
    lookup_name : String,
    node : Adamas::Compiler::Frontend::DefNode,
    call_types : Array(Adamas::HIR::TypeRef),
  ) : Array(Adamas::HIR::TypeRef)
    repair_partial_untyped_call_types_from_history(lookup_name, node, call_types)
  end

  def __test_missing_required_runtime_param_types?(
    node : Adamas::Compiler::Frontend::DefNode,
    call_types : Array(Adamas::HIR::TypeRef),
  ) : Bool
    missing_required_runtime_param_types?(node, call_types)
  end

  def __test_classvar_lazy_init_key?(key : String) : Bool
    @classvar_lazy_init_info.has_key?(key)
  end

  def __test_deferred_classvar_init_names : Array(String)
    @deferred_classvar_inits.compact_map do |entry|
      expr_id, arena, _owner = entry
      extract_deferred_classvar_name(arena, expr_id)
    end
  end

  def __test_constant_literal_int_value(name : String) : Int64?
    value = @constant_literal_values[name]?
    return nil unless value.is_a?(Adamas::Compiler::Semantic::MacroNumberValue)
    raw = value.value
    raw.is_a?(Int64) ? raw : nil
  end
end

# Helper to parse Crystal code and get AST
private def parse(code : String) : {Adamas::Compiler::Frontend::ArenaLike, Array(Adamas::Compiler::Frontend::ExprId)}
  lexer = Adamas::Compiler::Frontend::Lexer.new(code)
  parser = Adamas::Compiler::Frontend::Parser.new(lexer)
  result = parser.parse_program
  {result.arena, result.roots}
end

# Helper to parse and lower a function
private def lower_function(code : String) : Adamas::HIR::Function
  arena, exprs = parse(code)
  converter = Adamas::HIR::AstToHir.new(arena)

  # Find DefNode
  def_expr = exprs.find do |expr_id|
    arena[expr_id].is_a?(Adamas::Compiler::Frontend::DefNode)
  end

  raise "No function definition found" unless def_expr
  def_node = arena[def_expr].as(Adamas::Compiler::Frontend::DefNode)

  converter.lower_def(def_node)
end

private def lower_function_with_converter(code : String) : {Adamas::HIR::Function, Adamas::HIR::AstToHir}
  arena, exprs = parse(code)
  converter = Adamas::HIR::AstToHir.new(arena)

  def_expr = exprs.find do |expr_id|
    arena[expr_id].is_a?(Adamas::Compiler::Frontend::DefNode)
  end

  raise "No function definition found" unless def_expr
  def_node = arena[def_expr].as(Adamas::Compiler::Frontend::DefNode)

  {converter.lower_def(def_node), converter}
end

private def lower_program(code : String) : Adamas::HIR::AstToHir
  arena, exprs = parse(code)
  converter = Adamas::HIR::AstToHir.new(arena)
  converter.arena = arena

  enum_nodes = [] of Adamas::Compiler::Frontend::EnumNode
  module_nodes = [] of Adamas::Compiler::Frontend::ModuleNode
  class_nodes = [] of Adamas::Compiler::Frontend::ClassNode
  def_nodes = [] of Adamas::Compiler::Frontend::DefNode

  exprs.each do |expr_id|
    node = arena[expr_id]
    case node
    when Adamas::Compiler::Frontend::EnumNode
      enum_nodes << node
    when Adamas::Compiler::Frontend::ModuleNode
      module_nodes << node
    when Adamas::Compiler::Frontend::ClassNode
      class_nodes << node
    when Adamas::Compiler::Frontend::DefNode
      def_nodes << node
    end
  end

  enum_nodes.each { |node| converter.register_enum(node) }
  module_nodes.each { |node| converter.register_module(node) }
  class_nodes.each { |node| converter.register_class(node) }
  def_nodes.each { |node| converter.register_function(node) }

  module_nodes.each { |node| converter.lower_module(node) }
  class_nodes.each { |node| converter.lower_class(node) }
  def_nodes.each { |node| converter.lower_def(node) }

  converter
end

private def lower_program_with_main(code : String) : Adamas::HIR::AstToHir
  arena, exprs = parse(code)
  converter = Adamas::HIR::AstToHir.new(arena)
  converter.arena = arena

  enum_nodes = [] of Adamas::Compiler::Frontend::EnumNode
  module_nodes = [] of Adamas::Compiler::Frontend::ModuleNode
  class_nodes = [] of Adamas::Compiler::Frontend::ClassNode
  def_nodes = [] of Adamas::Compiler::Frontend::DefNode
  main_exprs = [] of UInt64

  exprs.each do |expr_id|
    node = arena[expr_id]
    case node
    when Adamas::Compiler::Frontend::EnumNode
      enum_nodes << node
    when Adamas::Compiler::Frontend::ModuleNode
      module_nodes << node
    when Adamas::Compiler::Frontend::ClassNode
      class_nodes << node
    when Adamas::Compiler::Frontend::DefNode
      def_nodes << node
    when Adamas::Compiler::Frontend::CallNode
      main_exprs << expr_id.index.to_u64
    end
  end

  enum_nodes.each { |node| converter.register_enum(node) }
  module_nodes.each { |node| converter.register_module(node) }
  class_nodes.each { |node| converter.register_class(node) }
  def_nodes.each { |node| converter.register_function(node) }

  module_nodes.each { |node| converter.lower_module(node) }
  class_nodes.each { |node| converter.lower_class(node) }
  def_nodes.each { |node| converter.lower_def(node) }

  converter.lower_main(main_exprs) if main_exprs.size > 0

  converter
end

private def lower_program_with_sources(code : String) : Adamas::HIR::AstToHir
  arena, exprs = parse(code)
  sources_by_arena = {arena.object_id.to_u64 => code}
  converter = Adamas::HIR::AstToHir.new(arena, sources_by_arena: sources_by_arena)
  converter.arena = arena

  module_nodes = [] of Adamas::Compiler::Frontend::ModuleNode
  class_nodes = [] of Adamas::Compiler::Frontend::ClassNode
  def_nodes = [] of Adamas::Compiler::Frontend::DefNode
  macro_nodes = [] of Adamas::Compiler::Frontend::MacroDefNode
  main_exprs = [] of UInt64

  exprs.each do |expr_id|
    node = arena[expr_id]
    case node
    when Adamas::Compiler::Frontend::ModuleNode
      module_nodes << node
    when Adamas::Compiler::Frontend::ClassNode
      class_nodes << node
    when Adamas::Compiler::Frontend::DefNode
      def_nodes << node
    when Adamas::Compiler::Frontend::MacroDefNode
      macro_nodes << node
    when Adamas::Compiler::Frontend::CallNode
      main_exprs << expr_id.index.to_u64
    end
  end

  module_nodes.each { |node| converter.register_module(node) }
  class_nodes.each { |node| converter.register_class(node) }
  macro_nodes.each { |node| converter.register_macro(node) }
  def_nodes.each { |node| converter.register_function(node) }

  module_nodes.each { |node| converter.lower_module(node) }
  class_nodes.each { |node| converter.lower_class(node) }
  def_nodes.each { |node| converter.lower_def(node) }
  converter.lower_main(main_exprs) if main_exprs.size > 0

  converter
end

# Helper to get HIR text output
private def hir_text(func : Adamas::HIR::Function) : String
  String.build { |io| func.to_s(io) }
end

describe Adamas::HIR::AstToHir do
  describe "slice hardening" do
    it "returns nil for unreadable slices instead of crashing" do
      arena, _ = parse("def foo; 1; end")
      converter = Adamas::HIR::AstToHir.new(arena)
      bogus = Slice.new(Pointer(UInt8).new(0x6e6f6974_u64), 4)

      converter.__test_safe_slice_to_string(bogus).should be_nil
    end
  end

  describe "absolute generic paths" do
    it "keeps ::Set(String) rooted at the top level inside nested modules" do
      converter = lower_program(<<-CRYSTAL)
        class Set(T)
          def self.new
            uninitialized self
          end

          def includes?(value : T)
            true
          end
        end

        module Adamas::MIR
          def self.probe
            seen = ::Set(String).new
            seen.includes?("x")
          end
        end
      CRYSTAL

      func = converter.module.functions.find { |f| f.name == "Adamas::MIR.probe$arity0" }
      func.should_not be_nil

      text = hir_text(func.not_nil!)
      text.should contain("call ::Set(String).new()")
      text.should_not contain("__adamas_string_includes_string")

      new_call = func.not_nil!.blocks[0].instructions.find do |inst|
        inst.is_a?(Adamas::HIR::Call) && inst.as(Adamas::HIR::Call).method_name.ends_with?(".new")
      end
      new_call.should_not be_nil
      desc = converter.module.get_type_descriptor(new_call.not_nil!.as(Adamas::HIR::Call).type)
      desc.should_not be_nil
      desc.not_nil!.name.should contain("Set(String)")
    end

    it "keeps ::Set(String) rooted at the top level inside nested classes" do
      converter = lower_program(<<-CRYSTAL)
        class Set(T)
          def self.new
            uninitialized self
          end

          def includes?(value : T)
            true
          end
        end

        module Adamas::MIR
          class HIRToMIRLowering
            def prepare
              seen_names = ::Set(String).new
              seen_names.includes?("x")
            end
          end
        end
      CRYSTAL

      func = converter.module.functions.find { |f| f.name == "Adamas::MIR::HIRToMIRLowering#prepare$arity0" }
      func.should_not be_nil

      text = hir_text(func.not_nil!)
      text.should contain("call ::Set(String).new()")
      text.should_not contain("__adamas_string_includes_string")
    end

    it "does not drift ::Set(String).new receivers into nested Set types" do
      converter = lower_program(<<-CRYSTAL)
        class Set(T)
          def self.new
            uninitialized self
          end

          def includes?(value : T)
            true
          end
        end

        module Adamas::MIR
          class Set(T)
            def self.new
              uninitialized self
            end

            def includes?(value : T)
              false
            end
          end

          class HIRToMIRLowering
            def prepare
              seen_names = ::Set(String).new
              seen_names.includes?("x")
            end
          end
        end
      CRYSTAL

      func = converter.module.functions.find { |f| f.name == "Adamas::MIR::HIRToMIRLowering#prepare$arity0" }
      func.should_not be_nil

      new_call = func.not_nil!.blocks[0].instructions.find do |inst|
        inst.is_a?(Adamas::HIR::Call) && inst.as(Adamas::HIR::Call).method_name.ends_with?(".new")
      end
      new_call.should_not be_nil

      new_desc = converter.module.get_type_descriptor(new_call.not_nil!.as(Adamas::HIR::Call).type)
      new_desc.should_not be_nil
      new_desc.not_nil!.name.should eq("Set(String)")

      includes_call = func.not_nil!.blocks[0].instructions.find do |inst|
        inst.is_a?(Adamas::HIR::Call) && inst.as(Adamas::HIR::Call).method_name.includes?("includes?")
      end
      includes_call.should_not be_nil
      includes_call.not_nil!.as(Adamas::HIR::Call).method_name.should eq("Set(String)#includes?$String")
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # POSITIVE TESTS: LITERALS
  # ═══════════════════════════════════════════════════════════════════════════

  describe "literal lowering" do
    it "lowers integer literal" do
      func = lower_function("def foo; 42; end")
      text = hir_text(func)

      text.should contain("literal 42")
      func.blocks.size.should be >= 1
    end

    it "lowers float literal" do
      func = lower_function("def foo; 3.14; end")
      text = hir_text(func)

      text.should contain("literal 3.14")
    end

    it "lowers string literal" do
      func = lower_function("def foo; \"hello\"; end")
      text = hir_text(func)

      text.should contain("literal \"hello\"")
    end

    it "lowers char literal" do
      func = lower_function("def foo; 'a'; end")
      text = hir_text(func)

      text.should contain("literal 97 : Char")
    end

    it "lowers bool literals" do
      func = lower_function("def foo; true; end")
      text = hir_text(func)

      text.should contain("literal true")
    end

    it "lowers nil literal" do
      func = lower_function("def foo; nil; end")
      text = hir_text(func)

      text.should contain("literal nil")
    end

    it "lowers symbol literal" do
      func = lower_function("def foo; :hello; end")
      text = hir_text(func)

      text.should contain("literal")
      text.should contain("Symbol")
    end

    it "lowers negative numbers" do
      func = lower_function("def foo; -42; end")
      text = hir_text(func)

      # Should have unary negation
      text.should contain("unop Neg")
    end

    it "lowers typed integer suffixes" do
      func = lower_function("def foo; 42_i64; end")
      text = hir_text(func)

      # Should have Int64 type
      func.blocks[0].instructions.first.type.should eq(Adamas::HIR::TypeRef::INT64)
    end
  end

  describe "enum symbol arguments" do
    it "casts symbol literal to enum value and mangles double splat calls" do
      code = <<-CR
        enum Section
          Sched
        end

        def trace(section : Section, **metadata)
        end

        trace :sched, foo: 1
      CR

      converter = lower_program_with_main(code)
      text = converter.module.to_s

      # Symbol :sched is converted to enum value 0
      text.should contain("literal 0 : Int32")
      # Double splat methods use _double_splat suffix in mangled name
      text.should contain("call trace$Section_NamedTuple_double_splat")
    end

    it "resolves module-qualified enum types in context" do
      code = <<-CR
        module Crystal
          module Tracing
            enum Section
              Sched
            end
          end

          def self.trace(section : Tracing::Section, **metadata)
          end
        end

        Crystal.trace :sched, foo: 1
      CR

      converter = lower_program_with_main(code)
      text = converter.module.to_s

      text.should contain("call Crystal.trace$Crystal::Tracing::Section_NamedTuple_double_splat")
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # POSITIVE TESTS: VARIABLES
  # ═══════════════════════════════════════════════════════════════════════════

  describe "variable lowering" do
    it "lowers local variable assignment" do
      func = lower_function("def foo; x = 1; end")
      text = hir_text(func)

      text.should contain("local")
      text.should contain("x")
    end

    it "lowers local variable reference" do
      func = lower_function("def foo; x = 1; x; end")
      text = hir_text(func)

      text.should contain("copy")
    end

    it "lowers instance variable read" do
      func = lower_function("def foo; @value; end")
      text = hir_text(func)

      text.should contain("field_get")
      text.should contain("@value")
    end

    it "lowers instance variable write" do
      func = lower_function("def foo; @value = 42; end")
      text = hir_text(func)

      text.should contain("field_set")
      text.should contain("@value")
    end

    it "lowers class variable read" do
      func = lower_function("def foo; @@count; end")
      text = hir_text(func)

      text.should contain("classvar_get")
      text.should contain("@@count")
    end

    it "lowers class variable write" do
      func = lower_function("def foo; @@count = 0; end")
      text = hir_text(func)

      text.should contain("classvar_set")
    end

    it "lowers self" do
      func = lower_function("def foo; self; end")
      text = hir_text(func)

      text.should contain("local \"self\"")
    end

    it "lowers function parameters" do
      func = lower_function("def foo(x, y); x; end")

      func.params.size.should eq(2)
      func.params[0].name.should eq("x")
      func.params[1].name.should eq("y")
    end

    it "lowers typed parameters" do
      func = lower_function("def foo(x : Int32); x; end")

      func.params[0].type.should eq(Adamas::HIR::TypeRef::INT32)
    end
  end

  describe "block type lowering" do
    it "captures block parameter types as Proc" do
      func, converter = lower_function_with_converter("def foo(&block : Int32 -> String); 1; end")
      param = func.params.find { |p| p.name == "block" }
      param.should_not be_nil
      desc = converter.module.get_type_descriptor(param.not_nil!.type)
      desc.should_not be_nil
      desc.not_nil!.kind.should eq(Adamas::HIR::TypeKind::Proc)
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # POSITIVE TESTS: BINARY OPERATIONS
  # ═══════════════════════════════════════════════════════════════════════════

  describe "binary operation lowering" do
    it "lowers arithmetic operations" do
      func = lower_function("def foo; 1 + 2; end")
      text = hir_text(func)

      text.should contain("binop Add")
    end

    it "lowers subtraction" do
      func = lower_function("def foo; 5 - 3; end")
      text = hir_text(func)

      text.should contain("binop Sub")
    end

    it "lowers multiplication" do
      func = lower_function("def foo; 2 * 3; end")
      text = hir_text(func)

      text.should contain("binop Mul")
    end

    it "lowers division" do
      func = lower_function("def foo; 10 / 2; end")
      text = hir_text(func)

      text.should contain("binop Div")
    end

    it "lowers modulo" do
      func = lower_function("def foo; 10 % 3; end")
      text = hir_text(func)

      text.should contain("binop Mod")
    end

    it "lowers comparison operations" do
      func = lower_function("def foo; 1 < 2; end")
      text = hir_text(func)

      text.should contain("binop Lt")
    end

    it "lowers equality" do
      func = lower_function("def foo; 1 == 1; end")
      text = hir_text(func)

      text.should contain("binop Eq")
    end

    it "lowers logical and" do
      func = lower_function("def foo(x : Bool, y : Bool); x && y; end")
      text = hir_text(func)

      text.should contain("branch")
      text.should contain("phi")
    end

    it "lowers logical or" do
      func = lower_function("def foo(x : Bool, y : Bool); x || y; end")
      text = hir_text(func)

      text.should contain("branch")
      text.should contain("phi")
    end

    it "lowers bitwise operations" do
      func = lower_function("def foo; 1 & 2; end")
      text = hir_text(func)

      text.should contain("binop BitAnd")
    end

    it "lowers shift operations" do
      func = lower_function("def foo; 1 << 2; end")
      text = hir_text(func)

      text.should contain("binop Shl")
    end

    it "lowers chained operations" do
      func = lower_function("def foo; 1 + 2 + 3; end")
      text = hir_text(func)

      # Should have two Add operations
      text.scan(/binop Add/).size.should eq(2)
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # POSITIVE TESTS: UNARY OPERATIONS
  # ═══════════════════════════════════════════════════════════════════════════

  describe "unary operation lowering" do
    it "lowers negation" do
      func = lower_function("def foo; -x; end")
      text = hir_text(func)

      text.should contain("unop Neg")
    end

    it "lowers logical not" do
      func = lower_function("def foo; !true; end")
      text = hir_text(func)

      text.should contain("unop Not")
    end

    it "lowers bitwise not" do
      func = lower_function("def foo; ~1; end")
      text = hir_text(func)

      text.should contain("unop BitNot")
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # POSITIVE TESTS: CONTROL FLOW
  # ═══════════════════════════════════════════════════════════════════════════

  describe "control flow lowering" do
    it "lowers if expression" do
      # Use a non-constant condition so the lowering must build a real CFG.
      func = lower_function("def foo(x : Bool); if x; 1; end; end")
      text = hir_text(func)

      text.should contain("branch")
      text.should contain("phi")
      func.blocks.size.should be >= 3  # entry, then, else, merge
    end

    it "lowers if-else expression" do
      func = lower_function("def foo(x : Bool); if x; 1; else; 2; end; end")
      text = hir_text(func)

      text.should contain("branch")
      text.should contain("phi")
    end

    it "lowers unless expression" do
      func = lower_function("def foo(x : Bool); unless x; 1; end; end")
      text = hir_text(func)

      text.should contain("unop Not")  # Condition negated
      text.should contain("branch")
    end

    it "lowers while loop" do
      func = lower_function("def foo(x : Bool); while x; 1; end; end")
      text = hir_text(func)

      text.should contain("branch")
      text.should contain("jump")
    end

    it "lowers until loop" do
      func = lower_function("def foo(x : Bool); until x; 1; end; end")
      text = hir_text(func)

      text.should contain("branch")
      text.should contain("jump")
    end

    it "lowers ternary expression" do
      func = lower_function("def foo(x : Bool); x ? 1 : 2; end")
      text = hir_text(func)

      text.should contain("branch")
      text.should contain("phi")
    end

    it "lowers case expression" do
      func = lower_function("def foo(x); case x; when 1; \"one\"; when 2; \"two\"; else; \"other\"; end; end")
      text = hir_text(func)

      text.should contain("binop Eq")
      text.should contain("phi")
    end

    it "lowers nested if" do
      func = lower_function("def foo(a : Bool, b : Bool); if a; if b; 1; end; end; end")
      text = hir_text(func)

      # Multiple branches
      text.scan(/branch/).size.should be >= 2
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # POSITIVE TESTS: FUNCTIONS
  # ═══════════════════════════════════════════════════════════════════════════

  describe "function lowering" do
    it "lowers return statement" do
      func = lower_function("def foo; return 42; end")
      text = hir_text(func)

      text.should contain("return")
    end

    it "lowers implicit return" do
      func = lower_function("def foo; 42; end")
      text = hir_text(func)

      text.should contain("return")
    end

    it "lowers early return" do
      func = lower_function("def foo; return 1; 2; end")
      text = hir_text(func)

      text.should contain("return")
    end

    it "lowers yield" do
      func = lower_function("def foo; yield 1; end")
      text = hir_text(func)

      text.should contain("yield")
    end

    it "lowers yield with multiple args" do
      func = lower_function("def foo; yield 1, 2, 3; end")
      text = hir_text(func)

      text.should contain("yield")
    end

    it "lowers function with return type" do
      func = lower_function("def foo : Int32; 42; end")

      func.return_type.should eq(Adamas::HIR::TypeRef::INT32)
    end

    it "lowers function with multiple parameters" do
      func = lower_function("def foo(a : Int32, b : String, c); end")

      func.params.size.should eq(3)
      func.params[0].type.should eq(Adamas::HIR::TypeRef::INT32)
      func.params[1].type.should eq(Adamas::HIR::TypeRef::STRING)
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # POSITIVE TESTS: CALLS
  # ═══════════════════════════════════════════════════════════════════════════

  describe "call lowering" do
    it "lowers method call on receiver" do
      func = lower_function("def foo; x.bar; end")
      text = hir_text(func)

      text.should contain("call")
      text.should contain("bar")
    end

    it "resolves enum value method calls" do
      code = <<-CRYSTAL
        enum Signal
          CHLD

          def reset : Int32
            1
          end
        end

        def foo
          Signal::CHLD.reset
        end
      CRYSTAL

      converter = lower_program(code)
      func = converter.module.functions.find { |f| f.name.split("$").first == "foo" }
      func.should_not be_nil

      call = func.not_nil!.blocks.flat_map(&.instructions)
        .find { |inst| inst.is_a?(Adamas::HIR::Call) }
      call.should_not be_nil
      call.not_nil!.as(Adamas::HIR::Call).method_name.should eq("Signal#reset")
    end

    it "applies default args for member access calls" do
      code = <<-CRYSTAL
        class Foo
          def bar(x : Int32 = 1, y : Int32 = 2) : Int32
            x + y
          end
        end

        def foo
          Foo.new.bar
        end
      CRYSTAL

      converter = lower_program(code)
      func = converter.module.functions.find { |f| f.name.split("$").first == "foo" }
      func.should_not be_nil

      call = func.not_nil!.blocks.flat_map(&.instructions)
        .find { |inst| inst.is_a?(Adamas::HIR::Call) && inst.as(Adamas::HIR::Call).method_name.includes?("Foo#bar") }
      call.should_not be_nil
      call.not_nil!.as(Adamas::HIR::Call).args.size.should eq(2)
    end

    it "binds default params before inline yield lowering" do
      code = <<-CRYSTAL
        def each_with_index_like(offset = 0)
          while offset < 3
            yield offset
            offset += 1
          end
        end

        def probe
          each_with_index_like do |i|
            i + 10
          end
        end
      CRYSTAL

      converter = lower_program(code)
      func = converter.module.functions.find { |f| f.name.split("$").first == "probe" }
      func.should_not be_nil

      text = hir_text(func.not_nil!)
      text.should contain("literal 0 : Int32")
      text.should_not contain("local \"offset\" : 0")
    end

    it "synthesizes zero-arg allocators for generic structs before MIR lowering" do
      code = <<-CRYSTAL
        struct SmallVec(T, N)
          @arr : Array(T)

          def initialize
            @arr = Array(T).new(N)
          end

          def size : Int32
            @arr.size
          end
        end

        def probe
          vec = SmallVec(Int32, 64).new
          vec.size
        end
      CRYSTAL

      converter = lower_program(code)
      func = converter.module.functions.find { |f| f.name.split("$").first == "probe" }
      func.should_not be_nil

      text = hir_text(func.not_nil!)
      text.should contain("call SmallVec(Int32, 64).new()")

      new_func = converter.module.functions.find { |f| f.name == "SmallVec(Int32, 64).new" }
      new_func.should_not be_nil

      new_text = hir_text(new_func.not_nil!)
      new_text.should contain("SmallVec(Int32, 64)#initialize()")
    end

    it "preserves typed default literals in lowered function params" do
      func = lower_function("def foo(x : Int32 = 1)\n  x\nend")

      func.params.size.should eq(1)
      func.params[0].name.should eq("x")
      func.params[0].default_literal.should eq("1")
    end

    it "prefers non-block overload when no block is passed" do
      code = <<-CRYSTAL
        class Foo
          def self.malloc(size : Int32 = 1)
            size
          end

          def self.malloc(size : Int32, & : Int32 -> Int32)
            yield size
          end
        end

        def bar
          Foo.malloc
        end
      CRYSTAL

      converter = lower_program(code)
      func = converter.module.functions.find { |f| f.name.split("$").first == "bar" }
      func.should_not be_nil

      call = func.not_nil!.blocks.flat_map(&.instructions)
        .find { |inst| inst.is_a?(Adamas::HIR::Call) }
      call.should_not be_nil
      call_name = call.not_nil!.as(Adamas::HIR::Call).method_name
      call_name.should contain("Foo.malloc$Int32")
      call_name.should_not contain("block")
    end

    it "lowers method call with args" do
      func = lower_function("def foo; puts(1, 2); end")
      text = hir_text(func)

      text.should contain("call")
    end

    it "lowers free function call" do
      func = lower_function("def foo; puts(1); end")
      text = hir_text(func)

      text.should contain("extern_call")
      text.should contain("__adamas_print_int32_ln")
    end

    it "lowers index access" do
      func = lower_function("def foo; arr[0]; end")
      text = hir_text(func)

      # For unknown types, index access becomes a method call to []
      # (IndexGet is only used for known array types and pointer types)
      text.should contain("call")
      text.should contain("[]")
    end

    it "lowers index assignment" do
      func = lower_function("def foo; arr[0] = 1; end")
      text = hir_text(func)

      # For unknown types, index assignment becomes a method call to []=
      # (IndexSet is only used for known array types)
      text.should contain("call")
      text.should contain("[]=")
    end

    it "lowers chained calls" do
      func = lower_function("def foo; a.b.c; end")
      text = hir_text(func)

      text.scan(/call/).size.should be >= 2
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # POSITIVE TESTS: CLOSURES
  # ═══════════════════════════════════════════════════════════════════════════

  describe "closure lowering" do
    it "lowers proc literal" do
      func = lower_function("def foo; -> { 1 }; end")
      text = hir_text(func)

      text.should contain("func_pointer")
      text.should contain("__crystal_proc_")
    end

    it "lowers proc with parameters" do
      func = lower_function("def foo; ->(x : Int32) { x + 1 }; end")
      text = hir_text(func)

      text.should contain("func_pointer")
      text.should contain("__crystal_proc_")
    end

    it "lowers block argument" do
      func = lower_function("def foo; each { |x| x }; end")
      text = hir_text(func)

      text.should contain("call")
      text.should contain("with_block")
    end

    it "lowers standalone proc literals without make_closure wrappers" do
      func = lower_function("def foo; -> { 1 }; end")

      closure = func.blocks.flat_map(&.instructions).find { |i| i.is_a?(Adamas::HIR::MakeClosure) }
      closure.should be_nil

      func.blocks.flat_map(&.instructions).any? { |i| i.is_a?(Adamas::HIR::FuncPointer) }.should be_true
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # POSITIVE TESTS: COLLECTIONS
  # ═══════════════════════════════════════════════════════════════════════════

  describe "collection lowering" do
    it "lowers empty array" do
      func = lower_function("def foo; [] of Int32; end")
      text = hir_text(func)

      text.should contain("array_literal")
    end

    it "lowers array literal" do
      func = lower_function("def foo; [1, 2, 3]; end")
      text = hir_text(func)

      text.should contain("array_literal")
    end

    it "lowers hash literal" do
      func = lower_function("def foo; {\"a\" => 1}; end")
      text = hir_text(func)

      # Hash literals now lower to Hash.new() + []= calls
      text.should contain("Hash")
    end

    it "lowers tuple literal" do
      func = lower_function("def foo; {1, \"a\"}; end")
      text = hir_text(func)

      text.should contain("allocate")
    end

    it "lowers range" do
      func = lower_function("def foo; 1..10; end")
      text = hir_text(func)

      text.should contain("allocate")
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # POSITIVE TESTS: TYPE OPERATIONS
  # ═══════════════════════════════════════════════════════════════════════════

  describe "type operation lowering" do
    it "lowers as cast" do
      func = lower_function("def foo(x); x as Int32; end")
      text = hir_text(func)

      text.should contain("cast")
    end

    it "lowers as? safe cast" do
      func = lower_function("def foo(x); x as? Int32; end")
      text = hir_text(func)

      text.should contain("is_a")
      text.should contain("__adamas_select_ptr")
    end

    it "lowers is_a? check" do
      func = lower_function("def foo(x); x.is_a?(Int32); end")
      text = hir_text(func)

      # Parser produces IsANode for x.is_a?(Type)
      text.should contain("is_a")
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # NEGATIVE TESTS: EDGE CASES
  # ═══════════════════════════════════════════════════════════════════════════

  describe "edge cases" do
    it "handles empty function body" do
      func = lower_function("def foo; end")

      # Should still have entry block with return
      func.blocks.size.should be >= 1
    end

    it "handles deeply nested expressions" do
      func = lower_function("def foo; ((((1 + 2) + 3) + 4) + 5); end")
      text = hir_text(func)

      text.scan(/binop Add/).size.should eq(4)
    end

    it "handles multiple statements" do
      func = lower_function("def foo; x = 1; y = 2; x + y; end")
      text = hir_text(func)

      text.should contain("x")
      text.should contain("y")
      text.should contain("binop Add")
    end

    it "handles variable shadowing in nested scope" do
      func = lower_function("def foo; x = 1; if true; x = 2; end; x; end")
      text = hir_text(func)

      # Both assignments should be present
      text.scan(/local/).size.should be >= 1
    end

    it "handles complex control flow" do
      func = lower_function(<<-CRYSTAL)
        def foo(n)
          result = 0
          while n > 0
            if n % 2 == 0
              result = result + n
            end
            n = n - 1
          end
          result
        end
      CRYSTAL

      text = hir_text(func)
      text.should contain("branch")
      text.should contain("jump")
    end

    it "handles method with block and regular args" do
      func = lower_function("def foo; map(1, 2) { |x| x * 2 }; end")
      text = hir_text(func)

      text.should contain("call")
      text.should contain("with_block")
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # NEGATIVE TESTS: ERROR HANDLING
  # ═══════════════════════════════════════════════════════════════════════════

  describe "error handling" do
    it "lowers declaration nodes to nil" do
      # Declaration nodes are not first-class runtime values in the current
      # lowering path, so lowering them in isolation terminates the synthetic
      # test function as unreachable rather than materializing a Nil literal.

      arena, exprs = parse("class Foo; end")
      converter = Adamas::HIR::AstToHir.new(arena)

      class_expr = exprs.first
      class_node = arena[class_expr]

      func = converter.module.create_function("test", Adamas::HIR::TypeRef::VOID)
      ctx = Adamas::HIR::LoweringContext.new(func, converter.module, arena)

      converter.lower_node(ctx, class_node)
      hir_text(func).should contain("unreachable")
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # LIFETIME TESTS
  # ═══════════════════════════════════════════════════════════════════════════

  describe "lifetime annotations" do
    it "marks literals as StackLocal" do
      func = lower_function("def foo; 42; end")

      literal = func.blocks[0].instructions.first
      literal.lifetime.should eq(Adamas::HIR::LifetimeTag::StackLocal)
    end

    it "marks parameters as HeapEscape (conservative)" do
      func = lower_function("def foo(x); x; end")

      func.params[0].lifetime.should eq(Adamas::HIR::LifetimeTag::HeapEscape)
    end

    it "marks array literals as StackLocal initially" do
      func = lower_function("def foo; [1, 2, 3]; end")

      arr = func.blocks.flat_map(&.instructions).find { |i| i.is_a?(Adamas::HIR::ArrayLiteral) }
      arr.should_not be_nil
      arr.not_nil!.lifetime.should eq(Adamas::HIR::LifetimeTag::StackLocal)
    end

    it "marks class var access as GlobalEscape" do
      func = lower_function("def foo; @@x; end")

      class_var = func.blocks.flat_map(&.instructions).find { |i| i.is_a?(Adamas::HIR::ClassVarGet) }
      class_var.should_not be_nil
      class_var.not_nil!.lifetime.should eq(Adamas::HIR::LifetimeTag::GlobalEscape)
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # SCOPE TESTS
  # ═══════════════════════════════════════════════════════════════════════════

  describe "scope handling" do
    it "creates function scope" do
      func = lower_function("def foo; 1; end")

      func.scopes.size.should be >= 1
      func.scopes[0].kind.should eq(Adamas::HIR::ScopeKind::Function)
    end

    it "creates block scope for if" do
      func = lower_function("def foo; if true; 1; end; end")

      block_scopes = func.scopes.select { |s| s.kind == Adamas::HIR::ScopeKind::Block }
      block_scopes.size.should be >= 1
    end

    it "creates loop scope for while" do
      func = lower_function("def foo; while true; 1; end; end")

      loop_scopes = func.scopes.select { |s| s.kind == Adamas::HIR::ScopeKind::Loop }
      loop_scopes.size.should be >= 1
    end

    it "keeps proc literals as standalone functions without parent closure scopes" do
      func = lower_function("def foo; -> { 1 }; end")

      closure_scopes = func.scopes.select { |s| s.kind == Adamas::HIR::ScopeKind::Closure }
      closure_scopes.should be_empty
    end

    it "nests scopes correctly" do
      func = lower_function("def foo; if true; while false; 1; end; end; end")

      # Should have function > block > loop nesting
      func.scopes.size.should be >= 3
    end
  end

  describe "module mixin return inference" do
    it "prefers concrete self type for module-like return annotations" do
      code = <<-CRYSTAL
        module M
          def returns_self : M
            self
          end
        end

        class Box
          include M
        end
      CRYSTAL

      converter = lower_program(code)
      # Module methods get arity suffix when lowered for including class
      func = converter.module.functions.find { |f| f.name.starts_with?("Box#returns_self") }
      func.should_not be_nil

      box_type = converter.class_info["Box"].type_ref
      func.not_nil!.return_type.should eq(box_type)
    end
  end

  describe "module-typed locals" do
    it "keeps concrete initializer types for module-annotated locals" do
      code = <<-CRYSTAL
        module M
          def value : M
            self
          end
        end

        class Box
          include M
        end

        def foo
          x : M = Box.new
          x.value
        end
      CRYSTAL

      converter = lower_program(code)
      func = converter.module.functions.find { |f| f.name.split("$").first == "foo" }
      func.should_not be_nil

      call = func.not_nil!.blocks.flat_map(&.instructions)
        .find { |inst| inst.is_a?(Adamas::HIR::Call) && inst.as(Adamas::HIR::Call).method_name.includes?("#value") }
      call.should_not be_nil

      recv_id = call.not_nil!.as(Adamas::HIR::Call).receiver
      recv_id.should_not be_nil

      recv = func.not_nil!.blocks.flat_map(&.instructions).find { |inst| inst.id == recv_id }
      recv.should_not be_nil

      box_type = converter.class_info["Box"].type_ref
      recv.not_nil!.type.should eq(box_type)
    end

    it "prefers concrete assignment when includers are ambiguous" do
      code = <<-CRYSTAL
        module M
          def value : Int32
            1
          end
        end

        class Box
          include M
          def value : Int32
            2
          end
        end

        class Bag
          include M
          def value : Int32
            3
          end
        end

        def foo(x : M)
          x = Box.new
          x.value
        end
      CRYSTAL

      converter = lower_program(code)
      func = converter.module.functions.find { |f| f.name.split("$").first == "foo" }
      func.should_not be_nil

      call = func.not_nil!.blocks.flat_map(&.instructions)
        .find { |inst| inst.is_a?(Adamas::HIR::Call) && inst.as(Adamas::HIR::Call).method_name.includes?("#value") }
      call.should_not be_nil

      call.not_nil!.as(Adamas::HIR::Call).method_name.should contain("Box#value")
    end
  end

  describe "module-typed receivers" do
    it "resolves unique includer methods for module-typed params" do
      code = <<-CRYSTAL
        module M
          def value : Int32
            1
          end
        end

        class Box
          include M
        end

        def foo(x : M)
          x.value
        end
      CRYSTAL

      converter = lower_program(code)
      func = converter.module.functions.find { |f| f.name.split("$").first == "foo" }
      func.should_not be_nil

      call = func.not_nil!.blocks.flat_map(&.instructions)
        .find { |inst| inst.is_a?(Adamas::HIR::Call) }
      call.should_not be_nil

      call.not_nil!.as(Adamas::HIR::Call).method_name.should contain("Box#value")
    end

    it "does not guess when includers are ambiguous" do
      code = <<-CRYSTAL
        module M
        end

        class Box
          include M
          def value : Int32
            1
          end
        end

        class Bag
          include M
          def value : Int32
            2
          end
        end

        def foo(x : M)
          x.value
        end
      CRYSTAL

      converter = lower_program(code)
      func = converter.module.functions.find { |f| f.name.split("$").first == "foo" }
      func.should_not be_nil

      call = func.not_nil!.blocks.flat_map(&.instructions)
        .find { |inst| inst.is_a?(Adamas::HIR::Call) }
      call.should_not be_nil

      call_name = call.not_nil!.as(Adamas::HIR::Call).method_name
      call_name.should_not contain("Box#value")
      call_name.should_not contain("Bag#value")
    end

    it "prefers module class methods for module-typed receivers" do
      code = <<-CRYSTAL
        module M
          extend self

          def foo : Int32
            1
          end
        end

        class Box
          include M

          def foo : Int32
            2
          end
        end

        def foo(x : M)
          x.foo
        end
      CRYSTAL

      converter = lower_program(code)
      func = converter.module.functions.find { |f| f.name.split("$").first == "foo" }
      func.should_not be_nil

      call = func.not_nil!.blocks.flat_map(&.instructions)
        .find { |inst| inst.is_a?(Adamas::HIR::Call) }
      call.should_not be_nil

      call_name = call.not_nil!.as(Adamas::HIR::Call).method_name
      call_name.should contain("M.foo")
    end

    # TODO: Module-typed receiver resolution needs virtual dispatch enhancements
    pending "prefers includers that match arity for module-typed params" do
      code = <<-CRYSTAL
        module M
        end

        class Box
          include M
          def value(x : Int32) : Int32
            x
          end
        end

        class Bag
          include M
          def value(x : Int32, y : Int32) : Int32
            x + y
          end
        end

        def foo(x : M)
          x.value(1)
        end
      CRYSTAL

      converter = lower_program(code)
      func = converter.module.functions.find { |f| f.name.split("$").first == "foo" }
      func.should_not be_nil

      call = func.not_nil!.blocks.flat_map(&.instructions)
        .find { |inst| inst.is_a?(Adamas::HIR::Call) }
      call.should_not be_nil

      call.not_nil!.as(Adamas::HIR::Call).method_name.should contain("Box#value")
    end

    # TODO: Module-typed receiver resolution needs virtual dispatch enhancements
    pending "prefers includers that match parameter types for module-typed params" do
      code = <<-CRYSTAL
        module M
        end

        class Box
          include M
          def value(x : Int32) : Int32
            x
          end
        end

        class Bag
          include M
          def value(x : String) : Int32
            x.size
          end
        end

        def foo(x : M)
          x.value(1)
        end
      CRYSTAL

      converter = lower_program(code)
      func = converter.module.functions.find { |f| f.name.split("$").first == "foo" }
      func.should_not be_nil

      call = func.not_nil!.blocks.flat_map(&.instructions)
        .find { |inst| inst.is_a?(Adamas::HIR::Call) }
      call.should_not be_nil

      call.not_nil!.as(Adamas::HIR::Call).method_name.should contain("Box#value")
    end
  end

  describe "module accessor mixins" do
    it "generates accessors from included modules for classes" do
      code = <<-CRYSTAL
        module M
          getter foo : Int32
          property bar : Int32
        end

        class Box
          include M
          def initialize(@foo : Int32, @bar : Int32)
          end
        end
      CRYSTAL

      converter = lower_program(code)
      getter = converter.module.functions.find { |f| f.name == "Box#foo" }
      getter.should_not be_nil

      setter = converter.module.functions.find { |f| f.name.split("$").first == "Box#bar=" }
      setter.should_not be_nil

      ivars = converter.class_info["Box"].ivars.map(&.name)
      ivars.should contain("@foo")
      ivars.should contain("@bar")
    end

    it "generates accessors from included modules for structs" do
      code = <<-CRYSTAL
        module M
          getter foo : Int32
        end

        struct Bag
          include M
          def initialize(@foo : Int32)
          end
        end
      CRYSTAL

      converter = lower_program(code)
      getter = converter.module.functions.find { |f| f.name == "Bag#foo" }
      getter.should_not be_nil

      ivars = converter.class_info["Bag"].ivars.map(&.name)
      ivars.should contain("@foo")
    end
  end

  describe "generic block return types" do
    it "substitutes block return type params in call return types" do
      code = <<-CRYSTAL
        def map_like(&block : Int32 -> U) : Array(U) forall U
          [] of U
        end

        def foo
          map_like { 1 }
        end
      CRYSTAL

      converter = lower_program(code)
      func = converter.module.functions.find { |f| f.name.starts_with?("foo") }
      func.should_not be_nil

      call = func.not_nil!.blocks.flat_map(&.instructions)
        .find { |inst| inst.is_a?(Adamas::HIR::Call) && inst.as(Adamas::HIR::Call).method_name.includes?("map_like") }
      call.should_not be_nil

      desc = converter.module.get_type_descriptor(call.not_nil!.as(Adamas::HIR::Call).type)
      desc.should_not be_nil
      desc.not_nil!.name.should eq("Array(Int32)")
    end
  end

  describe "typeof in alias targets" do
    it "resolves typeof(self) without local context" do
      code = <<-CRYSTAL
        class Box
          alias SelfType = typeof(self)

          def foo : SelfType
            self
          end
        end
      CRYSTAL

      converter = lower_program(code)
      # Methods with no params get arity suffix
      func = converter.module.functions.find { |f| f.name.starts_with?("Box#foo") }
      func.should_not be_nil

      box_type = converter.class_info["Box"].type_ref
      func.not_nil!.return_type.should eq(box_type)
    end
  end

  describe "typeof in type positions" do
    it "resolves typeof in generic type args using locals" do
      code = <<-CRYSTAL
        class Box(T)
          def initialize(@value : T)
          end
        end

        def foo(x : Int32)
          b = Box(typeof(x)).new(x)
          b
        end
      CRYSTAL

      converter = lower_program(code)
      converter.class_info.has_key?("Box(Int32)").should be_true
    end

    it "handles nested Enumerable.element_type without parentheses" do
      code = <<-CRYSTAL
        def foo(indexables : Array(Array(Int32)))
          ary = [] of typeof(Enumerable.element_type Enumerable.element_type indexables)
          ary
        end
      CRYSTAL

      converter = lower_program(code)
      func = converter.module.functions.find { |f| f.name.starts_with?("foo$") || f.name == "foo" }
      func.should_not be_nil

      array_lit = func.not_nil!.blocks.flat_map(&.instructions)
        .find { |inst| inst.is_a?(Adamas::HIR::ArrayLiteral) }
      array_lit.should_not be_nil

      element_type = array_lit.not_nil!.as(Adamas::HIR::ArrayLiteral).element_type
      desc = converter.module.get_type_descriptor(element_type)
      if desc
        desc.not_nil!.name.should eq("Int32")
      else
        element_type.should eq(Adamas::HIR::TypeRef::INT32)
      end
    end
  end

  describe "generic specialization return inference" do
    it "infers return types for specialized generic methods" do
      code = <<-CRYSTAL
        class Box(T)
          def initialize(@value : T)
          end

          def value
            @value
          end
        end

        def use
          Box(Int32).new(1).value
        end
      CRYSTAL

      converter = lower_program(code)
      func = converter.module.functions.find { |f| f.name == "Box(Int32)#value" }
      func.should_not be_nil
      func.not_nil!.return_type.should eq(Adamas::HIR::TypeRef::INT32)
    end
  end

  describe "macro if in module bodies" do
    it "registers module methods from active flag branches" do
      code = <<-CRYSTAL
        module M
          {% if flag?(:darwin) %}
            def self.foo
              1
            end
          {% end %}
        end

        def use
          M.foo
        end
      CRYSTAL

      converter = lower_program_with_sources(code)
      converter.module.has_function?("M.foo").should be_true
    end

    it "registers extend self methods from macro bodies" do
      code = <<-CRYSTAL
        module M
          macro add
            extend self

            def foo
              1
            end
          end

          add
        end

        def use
          M.foo
        end
      CRYSTAL

      converter = lower_program_with_sources(code)
      converter.module.has_function?("M.foo").should be_true
    end
  end

  describe "macro expansion in HIR" do
    it "binds named args with external names" do
      code = <<-CRYSTAL
        macro delegate(*methods, to object)
          {% for method in methods %}
            def {{method.id}}
              {{object.id}}
            end
          {% end %}
        end

        class Wrapper
          def initialize(@value : Int32)
          end

          delegate size, to: @value
        end
      CRYSTAL

      converter = lower_program_with_sources(code)
      converter.module.has_function?("Wrapper#size").should be_true
    end

    it "expands record-style macros with assign/type declarations" do
      code = <<-CRYSTAL
        macro getter(name)
          def {{name.id}}
            @{{name.id}}
          end
        end

        macro record(__name name, *properties, **kwargs)
          struct {{name.id}}
            {% for property in properties %}
              {% if property.is_a?(Assign) %}
                getter {{property.target.id}}
              {% elsif property.is_a?(TypeDeclaration) %}
                getter {{property}}
              {% else %}
                getter :{{property.id}}
              {% end %}
            {% end %}

            def copy_with({{
                            properties.map do |property|
                              if property.is_a?(Assign)
                                "\#{property.target.id} _\#{property.target.id} = @\#{property.target.id}".id
                              elsif property.is_a?(TypeDeclaration)
                                "\#{property.var.id} _\#{property.var.id} = @\#{property.var.id}".id
                              else
                                "\#{property.id} _\#{property.id} = @\#{property.id}".id
                              end
                            end.splat
                          }})
              self.class.new({{
                               properties.map do |property|
                                if property.is_a?(Assign)
                                  "_\#{property.target.id}".id
                                elsif property.is_a?(TypeDeclaration)
                                  "_\#{property.var.id}".id
                                else
                                  "_\#{property.id}".id
                                end
                               end.splat
                             }})
            end

            def clone
              self.class.new({{
                               properties.map do |property|
                                if property.is_a?(Assign)
                                  "@\#{property.target.id}.clone".id
                                elsif property.is_a?(TypeDeclaration)
                                  "@\#{property.var.id}.clone".id
                                else
                                  "@\#{property.id}.clone".id
                                end
                               end.splat
                             }})
            end
          end
        end

        record Point, x : Int32, y = 2
      CRYSTAL

      converter = lower_program_with_sources(code)
      converter.module.has_function?("Point#x").should be_true
      converter.module.has_function?("Point#y").should be_true
      copy_with = converter.module.functions.find { |func| func.name.starts_with?("Point#copy_with") }
      copy_with.should_not be_nil
      if func = copy_with
        param_names = func.params.map(&.name)
        param_names.should contain("_x")
        param_names.should contain("_y")
      end
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # BLOCK STRUCTURE TESTS
  # ═══════════════════════════════════════════════════════════════════════════

  describe "block structure" do
    it "creates entry block" do
      func = lower_function("def foo; 1; end")

      func.entry_block.should eq(0_u32)
      func.blocks.size.should be >= 1
    end

    it "terminates all blocks" do
      func = lower_function("def foo(x : Bool); if x; 1; else; 2; end; end")

      func.blocks.each do |block|
        block.terminator.should_not be_a(Adamas::HIR::Unreachable)
      end
    end

    it "creates correct CFG for if" do
      func = lower_function("def foo(x : Bool); if x; 1; else; 2; end; end")

      # Should have: entry -> branch -> then/else -> merge
      func.blocks.size.should be >= 4

      # Entry should end with branch
      entry = func.get_block(func.entry_block)
      entry.terminator.should be_a(Adamas::HIR::Branch)
    end

    it "creates correct CFG for while" do
      func = lower_function("def foo; while true; 1; end; end")

      # Entry -> jump -> cond -> branch -> body/exit
      # body -> jump back to cond
      func.blocks.size.should be >= 3

      # Should have at least one Jump back (loop)
      jumps = func.blocks.count { |b| b.terminator.is_a?(Adamas::HIR::Jump) }
      jumps.should be >= 1
    end

    it "phi nodes have correct incoming edges" do
      func = lower_function("def foo(x : Bool); if x; 1; else; 2; end; end")

      phi = func.blocks.flat_map(&.instructions).find { |i| i.is_a?(Adamas::HIR::Phi) }
      phi.should_not be_nil

      phi_node = phi.not_nil!.as(Adamas::HIR::Phi)
      phi_node.incoming.size.should eq(2)  # then and else branches
    end
  end

  describe "block parameter types" do
    it "applies block param types from callee signature" do
      code = <<-CRYSTAL
        def consume(& : Pointer(Int32) ->)
        end

        def foo
          consume do |ptr|
            ptr
          end
        end
      CRYSTAL

      converter = lower_program(code)
      func = converter.module.functions.find { |f| f.name.starts_with?("foo$") || f.name == "foo" }
      func.should_not be_nil

      params = func.not_nil!.blocks.flat_map(&.instructions)
        .select { |inst| inst.is_a?(Adamas::HIR::Parameter) }
      ptr_param = params.find { |inst| inst.as(Adamas::HIR::Parameter).name == "ptr" }
      ptr_param.should_not be_nil

      param_type = ptr_param.not_nil!.as(Adamas::HIR::Parameter).type
      desc = converter.module.get_type_descriptor(param_type)
      desc.should_not be_nil
      desc.not_nil!.name.should eq("Pointer(Int32)")
    end

    it "substitutes generic block param types from receiver" do
      code = <<-CRYSTAL
        class Box(T)
          def initialize(@value : T)
          end

          def consume(& : T ->)
          end
        end

        def foo
          Box(Int32).new(1).consume do |value|
            value
          end
        end
      CRYSTAL

      converter = lower_program(code)
      func = converter.module.functions.find { |f| f.name.starts_with?("foo$") || f.name == "foo" }
      func.should_not be_nil

      params = func.not_nil!.blocks.flat_map(&.instructions)
        .select { |inst| inst.is_a?(Adamas::HIR::Parameter) }
      value_param = params.find { |inst| inst.as(Adamas::HIR::Parameter).name == "value" }
      value_param.should_not be_nil

      param_type = value_param.not_nil!.as(Adamas::HIR::Parameter).type
      desc = converter.module.get_type_descriptor(param_type)
      if desc
        desc.not_nil!.name.should eq("Int32")
      else
        param_type.should eq(Adamas::HIR::TypeRef::INT32)
      end
    end

    it "lowers enum literal to_i/value without a method call" do
      code = <<-CRYSTAL
        enum DayOfWeek
          Monday
          Wednesday = 3
        end

        def foo
          DayOfWeek::Wednesday.to_i
        end

        def bar
          DayOfWeek::Wednesday.value
        end
      CRYSTAL

      converter = lower_program(code)
      foo = converter.module.functions.find { |f| f.name.split("$").first == "foo" }
      bar = converter.module.functions.find { |f| f.name.split("$").first == "bar" }

      foo.should_not be_nil
      bar.should_not be_nil

      foo_calls = foo.not_nil!.blocks.flat_map(&.instructions)
        .select { |inst| inst.is_a?(Adamas::HIR::Call) }
        .map { |inst| inst.as(Adamas::HIR::Call).method_name }
      bar_calls = bar.not_nil!.blocks.flat_map(&.instructions)
        .select { |inst| inst.is_a?(Adamas::HIR::Call) }
        .map { |inst| inst.as(Adamas::HIR::Call).method_name }

      foo_calls.any? { |name| name.includes?("to_i") }.should be_false
      bar_calls.any? { |name| name.includes?("value") }.should be_false

      foo_literals = foo.not_nil!.blocks.flat_map(&.instructions)
        .select { |inst| inst.is_a?(Adamas::HIR::Literal) }
        .map { |inst| inst.as(Adamas::HIR::Literal).value }

      foo_literals.includes?(3_i64).should be_true
    end
  end

  describe "pointer overload mangling" do
    it "distinguishes pointer element types in overloads" do
      code = <<-CRYSTAL
        struct Pointer(T)
        end

        def foo(x : Pointer(Int32))
          1
        end

        def foo(x : Pointer(Float64))
          2
        end
      CRYSTAL

      converter = lower_program(code)
      names = converter.module.functions.map(&.name)
      names.should contain("foo$Pointer(Int32)")
      names.should contain("foo$Pointer(Float64)")
    end
  end

  describe "generic arg splitting" do
    it "does not treat braced proc type args as a proc continuation" do
      arena, _exprs = parse("1")
      converter = Adamas::HIR::AstToHir.new(arena)

      converter.__test_split_generic_type_args("String, {String, _} ->")
        .should eq(["String", "{String, _} ->"])
    end
  end

  describe "unreachable sequential lowering" do
    it "does not lower statements after an explicit return" do
      func = lower_function("def foo; return 1; 2; end")
      text = hir_text(func)

      text.should contain("literal 1")
      text.should_not contain("literal 2")
    end

    it "does not lower statements after raise" do
      func = lower_function("def foo; raise \"x\"; 1; end")
      text = hir_text(func)

      text.should_not contain("literal 1")
    end
  end

  describe "generic receiver union specialization" do
    it "does not collapse union type args to Pointer(Void) when instantiating generic receivers" do
      code = <<-CRYSTAL
        module Indexable(T)
          abstract def size : Int32
          abstract def unsafe_fetch(i : Int32) : T

          def [](i : Int32) : T
            unsafe_fetch(i)
          end

          private class ItemIterator(A, T)
            def initialize(@array : A, @index = 0)
            end

            def next : T
              if @index >= @array.size
                raise "stop"
              end
              value = @array[@index]
              @index += 1
              value
            end
          end

          def each
            ItemIterator(self, T).new(self)
          end
        end

        struct Box(T)
          include Indexable(T)

          def size : Int32
            0
          end

          def unsafe_fetch(i : Int32) : T
            uninitialized T
          end
        end

        def foo(b : Box(Int32 | Pointer(UInt8)))
          b.each
        end

        foo(Box(Int32 | Pointer(UInt8)).new)
      CRYSTAL

      converter = lower_program_with_main(code)
      func = converter.module.functions.find { |f| f.name.starts_with?("Box(") && f.name.includes?("#each") }
      func.should_not be_nil

      text = hir_text(func.not_nil!)
      text.should_not contain("ItemIterator(Pointer(Void), Pointer(Void))")
      text.should_not contain("Pointer(Void)#size")
    end

    it "keeps exact callsite tuple types available to typeof-based formatter specializations" do
      converter = lower_program_with_main(<<-CRYSTAL)
        class Holder(T)
          def self.marker
            1
          end
        end

        def wrap(args : Array | Tuple)
          Holder(typeof(args)).marker
        end

        value = {0.125_f64}
        wrap(value)
      CRYSTAL

      text = String.build do |io|
        converter.module.functions.each do |func|
          func.to_s(io)
        end
      end

      text.should contain("Holder(Tuple(Float64)).marker")
      text.should_not contain("Holder(Pointer(Void)).marker")
    end

    it "keeps formatter sequential arg fetches on the concrete tuple element type" do
      converter = lower_program_with_main(<<-CRYSTAL)
        value = 1.25_f64
        sprintf("%.3f", value)
      CRYSTAL

      converter.__test_lower_function_if_needed("String::Formatter(Tuple(Float64))#arg_at$Nil")
      converter.__test_lower_function_if_needed("String::Formatter(Tuple(Float64))#float$String::Formatter::Flags_Float64")

      arg_at = converter.module.function_by_name("String::Formatter(Tuple(Float64))#arg_at$Nil")
      arg_at.should_not be_nil
      arg_at_type = converter.__test_get_type_name_from_ref(arg_at.not_nil!.return_type)
      arg_at_type.should contain("Float64")
      arg_at_type.should_not eq("Int32")

      text = hir_text(arg_at.not_nil!)
      text.should contain("local \"arg\" : 13")
      text.should_not contain("local \"arg\" : 0")

      float_fn = converter.module.function_by_name("String::Formatter(Tuple(Float64))#float$String::Formatter::Flags_Float64 | Int32")
      float_fn.should_not be_nil
      float_text = hir_text(float_fn.not_nil!)
      float_text.should_not contain("Expected a float, not ")
      float_text.should_not contain("Float64#inspect()")

      converter.module.function_by_name("String::Formatter(Tuple(Float64))#float$String::Formatter::Flags_Int32").should be_nil
    end

    it "preserves concrete owner type params for generic-module unsafe_as and constants" do
      converter = lower_program_with_main(<<-CRYSTAL)
        module Reint(F, U)
          def self.bits(num : F)
            u = num.unsafe_as(U)
            u & U::MAX
          end
        end

        Reint(Float64, UInt64).bits(1.5_f64)
      CRYSTAL

      uint64_ref = converter.__test_type_ref_for_name("UInt64")

      converter.__test_lower_function_if_needed("Reint(Float64, UInt64).bits$Float64")
      func = converter.module.function_by_name("Reint(Float64, UInt64).bits$Float64")
      func.should_not be_nil

      text = hir_text(func.not_nil!)
      # Semantic checks — the concrete `F` type param must be materialized as UInt64
      # inside the generic-module body:
      #   - a cast to UInt64 is emitted (SSA ids shift as lowering evolves, don't pin)
      #   - the fallback generic `Object#unsafe_as$T` wrapper is not used
      #   - the `Tuple#to_i` misroute is not taken
      text.should match(/cast %\d+ as #{uint64_ref.id}\b/)
      text.should_not contain("Object#unsafe_as$T")
      text.should_not contain("Tuple#to_i")
    end

    it "preserves parameter reassignments across statically selected if branches" do
      converter = lower_program(<<-CRYSTAL)
        def foo(x : Bool?)
          if true
            x = true
          end
          x
        end
      CRYSTAL

      foo = converter.module.function_by_name("foo$Nil | Bool")
      foo.should_not be_nil

      exit_term = foo.not_nil!.get_block(foo.not_nil!.entry_block).terminator
      exit_term.should be_a(Adamas::HIR::Return)
      return_value = exit_term.as(Adamas::HIR::Return).value
      return_value.should_not be_nil

      return_copy = foo.not_nil!.blocks.flat_map(&.instructions).find do |inst|
        inst.is_a?(Adamas::HIR::Copy) && inst.id == return_value
      end
      return_copy.should_not be_nil
      return_copy.not_nil!.as(Adamas::HIR::Copy).source.should_not eq(foo.not_nil!.params.first.id)
    end

    it "materializes inherited struct instance dispatch under the concrete owner" do
      converter = lower_program_with_main(<<-CRYSTAL)
        module Outer
          module Inner
            struct Point
              def initialize(@x : Int32, @y : Int32)
              end
            end
          end
        end

        STDOUT << Outer::Inner::Point.new(1, 2)
      CRYSTAL

      converter.__test_lower_function_if_needed("IO#<<$Outer::Inner::Point")

      io_append = converter.module.function_by_name("IO#<<$Outer::Inner::Point")
      io_append.should_not be_nil
      io_text = hir_text(io_append.not_nil!)
      io_text.should contain("Outer::Inner::Point#to_s")
      io_text.should_not contain("Struct#to_s$IO")
    end

    it "keeps concrete array inspect virtual targets lowerable on demand" do
      converter = lower_program_with_main(<<-CRYSTAL)
        struct Point
          def initialize(@x : Int32, @y : Int32)
          end
        end

        puts [Point.new(1, 2)].inspect
      CRYSTAL

      converter.__test_lower_function_if_needed("Array(Point)#inspect$IO")
      converter.__test_lower_function_if_needed("Array(Point)#to_s$IO")

      converter.module.function_by_name("Array(Point)#inspect$IO").should_not be_nil
      converter.module.function_by_name("Array(Point)#to_s$IO").should_not be_nil
    end

    it "does not materialize unrelated inspect virtual targets when lowering one concrete array" do
      converter = lower_program_with_main(<<-CRYSTAL)
        struct Point
          def initialize(@x : Int32, @y : Int32)
          end
        end

        puts [Point.new(1, 2)].inspect
      CRYSTAL

      converter.__test_lower_function_if_needed("Array(Point)#inspect$IO")
      converter.__test_lower_function_if_needed("Array(Point)#to_s$IO")

      converter.module.function_by_name("Array(Point)#inspect$IO").should_not be_nil
      converter.module.function_by_name("Array(Point)#to_s$IO").should_not be_nil
      converter.module.function_by_name("Array(Array(Int32))#inspect$IO").should be_nil
      converter.module.function_by_name("Array(Array(Int32))#to_s$IO").should be_nil
    end

    it "preserves concrete generic receiver owners when lowering inherited inspect bodies" do
      converter = lower_program_with_main(<<-CRYSTAL)
        struct Point
          def initialize(@x : Int32, @y : Int32)
          end
        end

        puts [Point.new(1, 2)].inspect
      CRYSTAL

      converter.__test_lower_function_if_needed("Array(Point)#inspect$IO")
      converter.__test_lower_function_if_needed("Array(Point)#to_s$IO")

      inspect_fn = converter.module.function_by_name("Array(Point)#inspect$IO")
      to_s_fn = converter.module.function_by_name("Array(Point)#to_s$IO")
      inspect_fn.should_not be_nil
      to_s_fn.should_not be_nil
      hir_text(inspect_fn.not_nil!).should contain("Array(Point)#to_s$IO")
    end

    it "marks deferred concrete callees as lazy-rta call targets" do
      arena = parse("1")[0]
      converter = Adamas::HIR::AstToHir.new(arena)
      converter.arena = arena

      converter.__test_queue_pending_inside_lowering("Array(Point)#inspect$IO")

      converter.__test_rta_called_method?("Array(Point)#inspect$IO").should be_true
      converter.__test_rta_called_method?("Array(Point)#inspect").should be_false
      converter.__test_rta_called_method_part?("inspect$IO").should be_false
      converter.__test_rta_called_method_part?("inspect").should be_false
    end

    it "keeps duplicate deferred callee recording exact-name idempotent" do
      arena = parse("1")[0]
      converter = Adamas::HIR::AstToHir.new(arena)
      converter.arena = arena

      converter.__test_queue_pending_inside_lowering("Array(Point)#inspect$IO")
      converter.__test_rta_called_method?("Array(Point)#inspect$IO").should be_true

      converter.__test_queue_pending_inside_lowering("Array(Point)#inspect$IO")
      converter.__test_rta_called_method?("Array(Point)#inspect$IO").should be_true
    end
  end

  describe "named arg block overload resolution" do
    it "keeps named arg names when selecting block overloads" do
      converter = lower_program_with_main(<<-CRYSTAL)
        def foo(name = nil, &block)
          block.call
        end

        def foo(*, name = nil, same_thread = false, &block)
          value = same_thread
          block.call
        end

        foo(name: nil, same_thread: false) { 42 }
      CRYSTAL

      main = converter.module.function_by_name("__adamas_main")
      main.should_not be_nil

      text = hir_text(main.not_nil!)
      text.should contain("call foo$Nil_Bool_block")
      text.should_not contain("call foo$arity1")
    end
  end

  describe "allocator lookup recovery" do
    it "generates a class allocator for generic zero-arg .new calls" do
      converter = lower_program_with_main(<<-CRYSTAL)
        class Channel(T)
          def initialize(@capacity = 0)
          end
        end

        ch = Channel(Int32).new
      CRYSTAL

      converter.__test_lower_function_if_needed("Channel(Int32).new")

      allocator = converter.module.function_by_name("Channel(Int32).new")
      allocator.should_not be_nil
      allocator.not_nil!.blocks.any? { |block| !block.instructions.empty? }.should be_true

      allocator_text = hir_text(allocator.not_nil!)
      allocator_text.should contain("Channel(Int32)#initialize$Int32(%0)")
    end
  end

  describe "parent overload lookup" do
    it "keeps inherited typed overload callsites specialized" do
      converter = lower_program_with_main(<<-CRYSTAL)
        class Parent
          def read_bytes(type : UInt8, format : Int32) : UInt8
            type
          end

          def read_bytes(type : UInt64, format : Int32) : UInt64
            type
          end
        end

        class Child < Parent
        end

        def decode(io : Child)
          io.read_bytes(0_u8, 0).to_i
        end

        decode(Child.new)
      CRYSTAL

      func = converter.module.function_by_name("decode$Child")
      func.should_not be_nil

      text = hir_text(func.not_nil!)
      text.should contain("call %1.Parent#read_bytes$UInt8_Int32")
      text.should contain(": 7")
      text.should_not contain("Parent#read_bytes$UInt64_Int32")
      text.should_not contain(": 10")
    end

    it "does not reuse a base signature return type for later typed inherited calls" do
      converter = lower_program_with_main(<<-CRYSTAL)
        class Parent
          def read_bytes(type, format : Int32)
            type
          end
        end

        class Child < Parent
        end

        def warm(io : Child)
          io.read_bytes(0_u64, 0)
        end

        def decode(io : Child)
          io.read_bytes(0_u8, 0).to_i
        end

        warm(Child.new)
        decode(Child.new)
      CRYSTAL

      func = converter.module.function_by_name("decode$Child")
      func.should_not be_nil

      text = hir_text(func.not_nil!)
      text.should contain("call %1.Parent#read_bytes$UInt8_Int32")
      text.should contain(": 7")
      text.should_not contain(": 10")
    end

    it "keeps virtual generic callsites on their exact typed return instead of the lowered base return" do
      converter = lower_program_with_main(<<-CRYSTAL)
        class Parent
          def read_bytes(type, format : Int32)
            type
          end
        end

        class Child < Parent
        end

        def warm(io : Parent)
          io.read_bytes(0_u64, 0)
        end

        def decode(io : Parent)
          io.read_bytes(0_u16, 0).to_i
        end

        warm(Child.new)
        decode(Child.new)
      CRYSTAL

      func = converter.module.function_by_name("decode$Parent")
      func.should_not be_nil

      text = hir_text(func.not_nil!)
      text.should contain("Parent#read_bytes$UInt16_Int32")
      text.should contain(": 8 [virtual]")
      text.should_not contain(": 10 [virtual]")
    end
  end

  describe "default arg lexical context" do
    it "resolves nested constants in class method defaults against the callee owner" do
      converter = lower_program_with_main(<<-CRYSTAL)
        class Outer
          def self.wrap(value : ExecutionContext = ExecutionContext.current)
            value
          end
        end

        class Outer::ExecutionContext
          def self.current : Outer::ExecutionContext
            new
          end
        end

        Outer.wrap()
      CRYSTAL

      main = converter.module.function_by_name("__adamas_main")
      main.should_not be_nil

      text = hir_text(main.not_nil!)
      text.should contain("call Outer::ExecutionContext.current()")
      text.should contain("call Outer.wrap$Outer::ExecutionContext")
      text.should_not contain("NotFoundError#current")
    end
  end

  describe "implicit zero-arg member receivers" do
    it "keeps included zero-arg receiver calls bound to the inferred owner type" do
      converter = lower_program_with_main(<<-CRYSTAL)
        class WrongType
          def type
            99
          end
        end

        struct InfoType
          def pipe?
            true
          end
        end

        struct Info
          def type
            InfoType.new
          end
        end

        module HasInfo
          def system_info
            Info.new
          end
        end

        class Box
          include HasInfo

          def foo
            case system_info.type
            when .pipe?
              1
            else
              2
            end
          end
        end

        Box.new.foo
      CRYSTAL

      converter.__test_lower_function_if_needed("Box#foo")

      func = converter.module.function_by_name("Box#foo")
      func.should_not be_nil

      text = hir_text(func.not_nil!)
      text.should contain("Box#system_info")
      text.should contain("Info#type")
      text.should_not contain("WrongType#type")
    end

    it "materializes repaired nested zero-arg receiver callees from included modules" do
      converter = lower_program_with_main(<<-CRYSTAL)
        module HasSystemType
          def system_type
            7
          end
        end

        struct Info
          include HasSystemType

          def type
            system_type
          end
        end

        class Box
          def foo
            Info.new.type
          end
        end

        Box.new.foo
      CRYSTAL

      converter.__test_lower_function_if_needed("Box#foo")

      type_func = converter.module.function_by_name("Info#type")
      type_func.should_not be_nil
      hir_text(type_func.not_nil!).should contain("Info#system_type")

      system_type_func = converter.module.function_by_name("Info#system_type")
      system_type_func.should_not be_nil
      system_type_func.not_nil!.blocks.any? { |block| !block.instructions.empty? }.should be_true
    end

    it "materializes nested namespaced zero-arg receiver callees from included modules" do
      converter = lower_program_with_main(<<-CRYSTAL)
        module Crystal::System::FileInfo
          def system_type
            7
          end
        end

        class File
          struct Info
            include Crystal::System::FileInfo

            def type
              system_type
            end
          end
        end

        class Box
          def foo
            File::Info.new.type
          end
        end

        Box.new.foo
      CRYSTAL

      converter.__test_lower_function_if_needed("Box#foo")

      type_func = converter.module.function_by_name("File::Info#type")
      type_func.should_not be_nil
      hir_text(type_func.not_nil!).should contain("File::Info#system_type")

      system_type_func = converter.module.function_by_name("File::Info#system_type")
      system_type_func.should_not be_nil
      system_type_func.not_nil!.blocks.any? { |block| !block.instructions.empty? }.should be_true
    end

    it "drains pending nested receiver callees after late included-call repair" do
      converter = lower_program_with_main(<<-CRYSTAL)
        module Crystal::System::FileInfo
          def system_type
            7
          end
        end

        class File
          struct Info
            include Crystal::System::FileInfo

            def type
              system_type
            end
          end
        end

        module HasInfo
          def system_info
            File::Info.new
          end
        end

        class Box
          include HasInfo

          def foo
            system_info.type
          end
        end

        Box.new.foo
      CRYSTAL

      type_func = converter.module.function_by_name("File::Info#type")
      type_func.should_not be_nil
      hir_text(type_func.not_nil!).should contain("File::Info#system_type")

      system_type_func = converter.module.function_by_name("File::Info#system_type")
      system_type_func.should_not be_nil
      system_type_func.not_nil!.blocks.any? { |block| !block.instructions.empty? }.should be_true
    end

    it "rebinds consumer overloads after nested call return types are repaired" do
      converter = lower_program_with_main(<<-CRYSTAL)
        struct Point
          property val : Float64
          property ts : Int64

          def initialize(@val : Float64, @ts : Int64)
          end
        end

        puts [Point.new(1.0, 2_i64)].inspect
      CRYSTAL

      main = converter.module.function_by_name("__adamas_main")
      main.should_not be_nil

      text = hir_text(main.not_nil!)
      text.should contain("Array(Point)#inspect() : 15")
      text.should contain("__adamas_print_string_ln")
      text.should_not contain("IO#puts$Nil")
    end

    it "orders dependent deferred constants after their referenced constants" do
      converter = lower_program_with_main(<<-CRYSTAL)
        class H
          HASH_BITS = begin
            61
          end
          HASH_MODULUS = (1_i64 << HASH_BITS) - 1
        end

        def run
          H::HASH_MODULUS
        end

        run()
      CRYSTAL

      main = converter.module.function_by_name("__adamas_main")
      main.should_not be_nil

      text = hir_text(main.not_nil!)
      bits_idx = text.index("classvar_set H.@@HASH_BITS")
      bits_idx.should_not be_nil
      modulus_idx = text.index("classvar_set H.@@HASH_MODULUS")
      modulus_idx.should_not be_nil
      bits_idx.not_nil!.should be < modulus_idx.not_nil!
    end

    it "keeps source order for unrelated deferred constants instead of preferring deeper namespaces" do
      converter = lower_program_with_sources(<<-CRYSTAL)
        class H
          HASH_BITS = 61
          HASH_MODULUS = (1_i64 << HASH_BITS) - 1
        end

        module Outer
          module Inner
            DEEP = {1, 2}
          end
        end

        def run
          H::HASH_MODULUS
          Outer::Inner::DEEP
        end

        run()
      CRYSTAL

      main = converter.module.function_by_name("__adamas_main")
      main.should_not be_nil

      text = hir_text(main.not_nil!)
      modulus_idx = text.index("classvar_set H.@@HASH_MODULUS")
      modulus_idx.should_not be_nil
      deep_idx = text.index("classvar_set Outer::Inner.@@DEEP")
      deep_idx.should_not be_nil
      modulus_idx.not_nil!.should be < deep_idx.not_nil!
    end

    it "keeps typed super dispatch flags out of parsed callsite arg types" do
      converter = lower_program_with_main(<<-CRYSTAL)
        class Exception
          def initialize(@message : String? = nil, @cause : Exception? = nil)
          end
        end

        class ArgumentError < Exception
          def initialize(message = "Argument error")
            super(message)
          end
        end

        ArgumentError.new("boom")
      CRYSTAL

      converter.__test_lower_function_if_needed("ArgumentError#initialize$String")
      typed_init = converter.module.function_by_name("ArgumentError#initialize$String")
      typed_init.should_not be_nil
      typed_text = hir_text(typed_init.not_nil!)
      typed_text.should contain("Exception#initialize")
      typed_text.should contain("_super")
      typed_text.should_not contain("call %0.ArgumentError#initialize(")
    end

    it "does not retarget super-tagged exception initialize wrappers back to self" do
      converter = lower_program_with_main(<<-CRYSTAL)
        class Exception
          def initialize(@message : String? = nil, @cause : Exception? = nil)
          end
        end

        class ArgumentError < Exception
          def initialize(message = "Argument error")
            super(message)
          end
        end

        arg = ArgumentError.new("boom")
        arg.message
      CRYSTAL

      converter.__test_lower_function_if_needed("ArgumentError#initialize")

      base_init = converter.module.function_by_name("ArgumentError#initialize")
      base_init.should_not be_nil
      base_text = hir_text(base_init.not_nil!)
      base_text.should contain("Exception#initialize")
      base_text.should contain("_super")
      base_text.should_not contain("call %0.ArgumentError#initialize(")
    end

    it "backfills partial untyped default-arg call types from recorded history before lowering" do
      arena, exprs = parse(<<-CRYSTAL)
        def wait_like(io, timeout = nil)
          io.to_s
        end
      CRYSTAL

      converter = Adamas::HIR::AstToHir.new(arena)
      converter.arena = arena

      def_expr = exprs.find { |expr_id| arena[expr_id].is_a?(Adamas::Compiler::Frontend::DefNode) }
      def_expr.should_not be_nil
      def_node = arena[def_expr.not_nil!].as(Adamas::Compiler::Frontend::DefNode)

      converter.register_function(def_node)

      int32_ref = converter.__test_type_ref_for_name("Int32")
      nil_ref = Adamas::HIR::TypeRef::NIL
      partial = [Adamas::HIR::TypeRef::VOID, nil_ref]

      converter.__test_missing_required_runtime_param_types?(def_node, partial).should be_true
      converter.__test_remember_callsite_arg_types("wait_like", [int32_ref, nil_ref])

      repaired = converter.__test_repair_partial_untyped_call_types_from_history("wait_like", def_node, partial)
      repaired.should eq([int32_ref, nil_ref])
      converter.__test_missing_required_runtime_param_types?(def_node, repaired).should be_false
    end

    it "keeps visibility-wrapped private constants reachable in deferred init bookkeeping" do
      arena, exprs = parse(<<-CRYSTAL)
        class Box
          private C1 = 11_i64

          def value
            C1
          end
        end
      CRYSTAL

      converter = Adamas::HIR::AstToHir.new(arena)
      converter.arena = arena

      class_expr = exprs.find { |expr_id| arena[expr_id].is_a?(Adamas::Compiler::Frontend::ClassNode) }
      class_expr.should_not be_nil
      class_node = arena[class_expr.not_nil!].as(Adamas::Compiler::Frontend::ClassNode)

      converter.register_class(class_node)
      converter.lower_class(class_node)

      converter.__test_deferred_classvar_init_names.should contain("C1")
    end

    it "preserves high-bit UInt64 private constant literals instead of zeroing them" do
      converter = lower_program_with_main(<<-CRYSTAL)
        struct X
          private C1 = 0xacd5ad43274593b9_u64

          def self.c1
            C1
          end
        end

        X.c1
      CRYSTAL

      converter.__test_constant_literal_int_value("X::C1").should eq(0xacd5ad43274593b9_u64.unsafe_as(Int64))
    end

    it "inlines enum predicates through zero-arg enum-returning calls" do
      converter = lower_program_with_main(<<-CRYSTAL)
        enum FileType : UInt8
          File
          Pipe
        end

        class Info
          def type : FileType
            FileType::Pipe
          end
        end

        class Box
          def foo(info : Info)
            info.type.pipe?
          end
        end

        Box.new.foo(Info.new)
      CRYSTAL

      converter.__test_lower_function_if_needed("Box#foo$Info")

      foo = converter.module.function_by_name("Box#foo$Info")
      foo.should_not be_nil
      text = hir_text(foo.not_nil!)
      text.should contain("binop Eq")
      text.should_not contain(".pipe?()")
    end

    it "inlines enum predicates in case-dot-when branches over enum-returning calls" do
      converter = lower_program_with_main(<<-CRYSTAL)
        enum FileType : UInt8
          File
          Pipe
          Socket
        end

        class Info
          def type : FileType
            FileType::Pipe
          end
        end

        class Box
          def foo(info : Info)
            case info.type
            when .pipe?, .socket?
              1
            else
              2
            end
          end
        end

        Box.new.foo(Info.new)
      CRYSTAL

      converter.__test_lower_function_if_needed("Box#foo$Info")

      foo = converter.module.function_by_name("Box#foo$Info")
      foo.should_not be_nil
      text = hir_text(foo.not_nil!)
      text.should contain("binop Eq")
      text.should_not contain(".pipe?()")
      text.should_not contain(".socket?()")
    end

    it "inlines enum predicates in case-dot-when branches over nested receiver calls" do
      converter = lower_program_with_main(<<-CRYSTAL)
        enum FileType : UInt8
          File
          Pipe
          Socket
        end

        class Info
          def type : FileType
            FileType::Pipe
          end
        end

        module HasInfo
          def system_info
            Info.new
          end
        end

        class Box
          include HasInfo

          def foo
            case system_info.type
            when .pipe?, .socket?
              1
            else
              2
            end
          end
        end

        Box.new.foo
      CRYSTAL

      converter.__test_lower_function_if_needed("Box#foo")

      foo = converter.module.function_by_name("Box#foo")
      foo.should_not be_nil
      text = hir_text(foo.not_nil!)
      text.should contain("Box#system_info")
      text.should contain("Info#type")
      text.should contain("binop Eq")
      text.should_not contain(".pipe?()")
      text.should_not contain(".socket?()")
    end

    it "inlines case-dot-when predicates for nested enum owners with shorthand return annotations" do
      converter = lower_program_with_main(<<-CRYSTAL)
        class File
          enum Type : UInt8
            File
            Pipe
            Socket
          end

          class Info
            def type : Type
              Type::Pipe
            end
          end
        end

        module HasInfo
          def system_info
            File::Info.new
          end
        end

        class Box
          include HasInfo

          def foo
            case system_info.type
            when .pipe?, .socket?
              1
            else
              2
            end
          end
        end

        Box.new.foo
      CRYSTAL

      converter.__test_lower_function_if_needed("Box#foo")

      foo = converter.module.function_by_name("Box#foo")
      foo.should_not be_nil
      text = hir_text(foo.not_nil!)
      text.should contain("File::Info#type")
      text.should contain("binop Eq")
      text.should_not contain(".pipe?()")
      text.should_not contain(".socket?()")
    end

    it "inlines case-dot-when predicates through enum-returning forwarding methods" do
      converter = lower_program_with_main(<<-CRYSTAL)
        class File
          enum Type : UInt8
            File
            Pipe
            Socket
          end

          class Info
            def type : Type
              system_type
            end

            def system_type : Type
              Type::Pipe
            end
          end
        end

        module HasInfo
          def system_info
            File::Info.new
          end
        end

        class Box
          include HasInfo

          def foo
            case system_info.type
            when .pipe?, .socket?
              1
            else
              2
            end
          end
        end

        Box.new.foo
      CRYSTAL

      converter.__test_lower_function_if_needed("Box#foo")

      foo = converter.module.function_by_name("Box#foo")
      foo.should_not be_nil
      text = hir_text(foo.not_nil!)
      text.should contain("File::Info#type")
      text.should contain("binop Eq")
      text.should_not contain(".pipe?()")
      text.should_not contain(".socket?()")
    end

    it "inlines case-dot-when predicates through enum-returning forwarding methods on structs" do
      converter = lower_program_with_main(<<-CRYSTAL)
        class File
          enum Type : UInt8
            File
            Pipe
            Socket
          end

          struct Info
            def type : Type
              system_type
            end

            def system_type : Type
              Type::Pipe
            end
          end
        end

        module HasInfo
          def system_info
            File::Info.new
          end
        end

        class Box
          include HasInfo

          def foo
            case system_info.type
            when .pipe?, .socket?
              1
            else
              2
            end
          end
        end

        Box.new.foo
      CRYSTAL

      converter.__test_lower_function_if_needed("Box#foo")

      foo = converter.module.function_by_name("Box#foo")
      foo.should_not be_nil
      text = hir_text(foo.not_nil!)
      text.should contain("File::Info#type")
      text.should contain("binop Eq")
      text.should_not contain(".pipe?()")
      text.should_not contain(".socket?()")
    end

    it "inlines case-dot-when predicates through included-module enum forwarders on structs" do
      converter = lower_program_with_main(<<-CRYSTAL)
        module Crystal::System::FileInfo
          def system_type : ::File::Type
            ::File::Type::Pipe
          end
        end

        class File
          enum Type : UInt8
            File
            Pipe
            Socket
          end

          struct Info
            include Crystal::System::FileInfo

            def type : Type
              system_type
            end
          end
        end

        module HasInfo
          def system_info
            File::Info.new
          end
        end

        class Box
          include HasInfo

          def foo
            case system_info.type
            when .pipe?, .socket?
              1
            else
              2
            end
          end
        end

        Box.new.foo
      CRYSTAL

      converter.__test_lower_function_if_needed("Box#foo")

      foo = converter.module.function_by_name("Box#foo")
      foo.should_not be_nil
      text = hir_text(foo.not_nil!)
      text.should contain("File::Info#type")
      text.should contain("binop Eq")
      text.should_not contain(".pipe?()")
      text.should_not contain(".socket?()")
    end
  end

  describe "macro literal parameter filtering" do
    it "drops inactive inline flag-controlled params inside begin-wrapped class bodies" do
      converter = lower_program_with_sources(<<-CRYSTAL)
        class Outer
          {% begin %}
          def self.wrap(x : Int32{% if flag?(:execution_context) %}, y : String = "bad"{% end %}, &block)
            block.call
          end
          {% end %}
        end

        Outer.wrap(1) { 42 }
      CRYSTAL

      main = converter.module.function_by_name("__adamas_main")
      main.should_not be_nil

      text = hir_text(main.not_nil!)
      text.should contain("call Outer.wrap$Int32_block")
      text.should_not contain("Outer.wrap$Int32_String_block")
      text.should_not contain(%("bad"))
    end
  end

  describe "branch condition return inference" do
    it "keeps callsites union-typed when branch locals come from condition assignments" do
      converter = lower_program_with_main(<<-CRYSTAL)
        class Worker
          def initialize(@queue : Array(Int32)?, @fallback : Int32?)
          end

          def pick
            if (q = @queue) && !q.empty?
              value = q.first
              {1, value}
            elsif other = @fallback
              value = other
              {1, value}
            else
              {0, nil}
            end
          end

          def use
            state, value = pick
            {state, value}
          end
        end

        Worker.new([1], 2).use
      CRYSTAL

      use_func = converter.module.functions.find { |func| func.name.starts_with?("Worker#use") }
      use_func.should_not be_nil

      pick_call = use_func.not_nil!.blocks.flat_map(&.instructions).find do |inst|
        inst.is_a?(Adamas::HIR::Call) && inst.as(Adamas::HIR::Call).method_name.starts_with?("Worker#pick")
      end
      pick_call.should_not be_nil

      call_type = pick_call.not_nil!.as(Adamas::HIR::Call).type
      call_desc = converter.module.get_type_descriptor(call_type)
      call_desc.should_not be_nil
      call_desc.not_nil!.kind.should eq(Adamas::HIR::TypeKind::Union)
      call_desc.not_nil!.name.should contain("Tuple(Int32, Int32)")
      call_desc.not_nil!.name.should contain("Tuple(Int32, Nil)")
    end
  end

  describe "block-dependent query return inference" do
    it "keeps block-return-dependent query calls typed from the block instead of Bool" do
      previous = ENV["CRYSTAL_V2_DISABLE_INLINE_YIELD"]?
      ENV["CRYSTAL_V2_DISABLE_INLINE_YIELD"] = "1"
      begin
        converter = lower_program_with_main(<<-CRYSTAL)
          class Worker
            def read_section?(name, &)
              return nil if name == "miss"

              if name == "hit"
                yield 1, 2
              end
            end

            def use
              read_section?("hit") { |sh, io| sh + io }
            end
          end

          Worker.new.use
        CRYSTAL

        use_func = converter.module.functions.find { |func| func.name.starts_with?("Worker#use") }
        use_func.should_not be_nil

        section_call = use_func.not_nil!.blocks.flat_map(&.instructions).find do |inst|
          inst.is_a?(Adamas::HIR::Call) && inst.as(Adamas::HIR::Call).method_name.starts_with?("Worker#read_section?")
        end
        section_call.should_not be_nil

        call_type = section_call.not_nil!.as(Adamas::HIR::Call).type
        type_name = converter.__test_get_type_name_from_ref(call_type)
        call_desc = converter.module.get_type_descriptor(call_type)
        call_desc.try(&.kind).should eq(Adamas::HIR::TypeKind::Union)
        type_name.should contain("Int32")
        type_name.should contain("Nil")
        type_name.should_not eq("Bool")
      ensure
        if previous
          ENV["CRYSTAL_V2_DISABLE_INLINE_YIELD"] = previous
        else
          ENV.delete("CRYSTAL_V2_DISABLE_INLINE_YIELD")
        end
      end
    end

    it "repairs stale direct call types from finalized callee returns" do
      previous = ENV["CRYSTAL_V2_DISABLE_INLINE_YIELD"]?
      ENV["CRYSTAL_V2_DISABLE_INLINE_YIELD"] = "1"
      begin
        converter = lower_program_with_sources(<<-CRYSTAL)
          class Worker
            def use
              read_section?("hit") { |sh, io| sh }
            end

            def read_section?(name, &)
              return nil if name == "miss"

              seek(1) do
                yield 1, 2
              end
            end

            def seek(pos, &)
              yield
            end
          end

          Worker.new.use
        CRYSTAL

        use_func = converter.module.functions.find { |func| func.name.starts_with?("Worker#use") }
        use_func.should_not be_nil

        section_index = use_func.not_nil!.blocks[0].instructions.index! do |inst|
          inst.is_a?(Adamas::HIR::Call) && inst.as(Adamas::HIR::Call).method_name.starts_with?("Worker#read_section?")
        end
        original_call = use_func.not_nil!.blocks[0].instructions[section_index].as(Adamas::HIR::Call)
        use_func.not_nil!.blocks[0].instructions[section_index] = Adamas::HIR::Call.new(
          original_call.id,
          Adamas::HIR::TypeRef::BOOL,
          original_call.receiver,
          original_call.method_name,
          original_call.args,
          original_call.block,
          original_call.virtual
        )

        converter.__test_repair_stale_call_return_types

        repaired_call = use_func.not_nil!.blocks[0].instructions[section_index].as(Adamas::HIR::Call)
        repaired_desc = converter.module.get_type_descriptor(repaired_call.type)
        repaired_desc.should_not be_nil
        repaired_desc.not_nil!.kind.should eq(Adamas::HIR::TypeKind::Union)
        repaired_desc.not_nil!.name.should contain("Nil")
        repaired_desc.not_nil!.name.should contain("Int32")
      ensure
        if previous
          ENV["CRYSTAL_V2_DISABLE_INLINE_YIELD"] = previous
        else
          ENV.delete("CRYSTAL_V2_DISABLE_INLINE_YIELD")
        end
      end
    end
  end


  describe "inline block tuple binding" do
    it "destructures yielded tuples for multi-param blocks without retargeting the first param to the whole tuple" do
      converter = lower_program_with_sources(<<-CRYSTAL)
        struct Point
          property val : Float64
          property ts : Int64

          def initialize(@val : Float64, @ts : Int64)
          end
        end

        def same_point?(expected : Point, actual : Point) : Bool
          expected.ts == actual.ts
        end

        def assert_points(expected : Array(Point), actual : Array(Point)) : Bool
          expected.zip(actual).all? { |e, a| same_point?(e, a) }
        end

        p1 = Point.new(1.0, 1_i64)
        p2 = Point.new(2.0, 2_i64)
        assert_points([p1], [p2])
      CRYSTAL

      func = converter.module.function_by_name("assert_points$Array(Point)_Array(Point)")
      func.should_not be_nil

      text = hir_text(func.not_nil!)
      text.should contain("call same_point?$Point_Point")
      text.should_not contain("same_point?$Tuple(Point, Point)_Point")
    end
  end

  describe "Time.instant lowering" do
    it "registers Time.instant as a typed Time::Instant call instead of an unresolved void-like call" do
      converter = lower_program_with_sources(<<-CRYSTAL)
        class Time
          struct Instant
          end
        end

        def Time.instant : Time::Instant
          uninitialized Time::Instant
        end

        Time.instant()
      CRYSTAL

      converter.module.function_by_name("Time.instant$arity0").should_not be_nil

      main = converter.module.function_by_name("__adamas_main")
      main.should_not be_nil

      text = hir_text(main.not_nil!)
      text.should contain("call Time.instant()")
      text.should_not contain("call Time.instant() : 0")
    end

    it "keeps top-level path receivers on singleton defs during registration" do
      converter = lower_program_with_sources(<<-CRYSTAL)
        class Time
          class Location
          end
        end

        def Time::Location.utc : Int32
          1
        end

        Time::Location.utc()
      CRYSTAL

      converter.module.function_by_name("Time::Location.utc$arity0").should_not be_nil

      main = converter.module.function_by_name("__adamas_main")
      main.should_not be_nil

      text = hir_text(main.not_nil!)
      text.should contain("call Time::Location.utc()")
      text.should_not contain("call utc()")
    end

    it "keeps overloaded Time::Instant subtraction typed as Time::Span at call sites" do
      converter = lower_program_with_sources(<<-CRYSTAL)
        class Time
          struct Span
            def total_milliseconds : Int32
              1
            end
          end

          struct Instant
            def -(other : self) : Time::Span
              uninitialized Time::Span
            end
          end
        end

        def Time.instant : Time::Instant
          uninitialized Time::Instant
        end

        def probe : Int32
          started = Time.instant
          elapsed = Time.instant - started
          elapsed.total_milliseconds
        end

        probe()
      CRYSTAL

      converter.__test_get_function_return_type("Time::Instant#-$Time::Instant").should eq(
        converter.__test_type_ref_for_name("Time::Span")
      )

      probe = converter.module.function_by_name("probe$arity0")
      probe.should_not be_nil

      text = hir_text(probe.not_nil!)
      text.should contain("Time::Span#total_milliseconds")
      text.should_not contain("Time::Instant#total_milliseconds")
    end
  end

  describe "unary wrapping negation lowering" do
    it "lowers unary &- as a real negation instead of a void-like method call" do
      converter = lower_program_with_sources(<<-CRYSTAL)
        def probe(x : UInt64) : UInt64
          &-x
        end

        probe(16_u64)
      CRYSTAL

      probe = converter.module.function_by_name("probe$UInt64")
      probe.should_not be_nil

      text = hir_text(probe.not_nil!)
      text.should contain("unop Neg")
      text.should_not contain("UInt64#&-() : 0")
    end
  end

  describe "Pointer(Void) arithmetic" do
    it "keeps Pointer(Void) addition byte-strided inside struct initializers" do
      converter = lower_program_with_sources(<<-CRYSTAL)
        class StackBox
          def initialize(@pointer : Pointer(Void), @size : Int32)
            @bottom = @pointer + @size
          end
        end
      CRYSTAL

      init = converter.module.function_by_name("StackBox#initialize$Pointer(Void)_Int32")
      init.should_not be_nil

      pointer_add = init.not_nil!.blocks.flat_map(&.instructions).find(&.is_a?(Adamas::HIR::PointerAdd))
      pointer_add.should_not be_nil
      pointer_add.not_nil!.as(Adamas::HIR::PointerAdd).element_type.should eq(Adamas::HIR::TypeRef::UINT8)
    end
  end

  describe "accessor lowering" do
    it "materializes setter bodies even when the signature was pre-registered" do
      converter = lower_program_with_sources(<<-CRYSTAL)
        class Box
          property value : Int32

          def initialize
            @value = 0
          end
        end

        box = Box.new
        box.value = 10
      CRYSTAL

      setter = converter.module.function_by_name("Box#value=$Int32")
      setter.should_not be_nil
      setter.not_nil!.blocks.any? { |block| !block.instructions.empty? }.should be_true

      setter_text = hir_text(setter.not_nil!)
      setter_text.should contain("field_set")
      setter_text.should_not contain("unreachable")
    end
  end
end
