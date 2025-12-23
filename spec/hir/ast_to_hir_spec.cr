require "../spec_helper"
require "../../src/compiler/hir/ast_to_hir"
require "../../src/compiler/frontend/parser"
require "../../src/compiler/frontend/lexer"

# Helper to parse Crystal code and get AST
private def parse(code : String) : {CrystalV2::Compiler::Frontend::ArenaLike, Array(CrystalV2::Compiler::Frontend::ExprId)}
  lexer = CrystalV2::Compiler::Frontend::Lexer.new(code)
  parser = CrystalV2::Compiler::Frontend::Parser.new(lexer)
  result = parser.parse_program
  {result.arena, result.roots}
end

# Helper to parse and lower a function
private def lower_function(code : String) : Crystal::HIR::Function
  arena, exprs = parse(code)
  converter = Crystal::HIR::AstToHir.new(arena)

  # Find DefNode
  def_expr = exprs.find do |expr_id|
    arena[expr_id].is_a?(CrystalV2::Compiler::Frontend::DefNode)
  end

  raise "No function definition found" unless def_expr
  def_node = arena[def_expr].as(CrystalV2::Compiler::Frontend::DefNode)

  converter.lower_def(def_node)
end

private def lower_function_with_converter(code : String) : {Crystal::HIR::Function, Crystal::HIR::AstToHir}
  arena, exprs = parse(code)
  converter = Crystal::HIR::AstToHir.new(arena)

  def_expr = exprs.find do |expr_id|
    arena[expr_id].is_a?(CrystalV2::Compiler::Frontend::DefNode)
  end

  raise "No function definition found" unless def_expr
  def_node = arena[def_expr].as(CrystalV2::Compiler::Frontend::DefNode)

  {converter.lower_def(def_node), converter}
end

private def lower_program(code : String) : Crystal::HIR::AstToHir
  arena, exprs = parse(code)
  converter = Crystal::HIR::AstToHir.new(arena)
  converter.arena = arena

  module_nodes = [] of CrystalV2::Compiler::Frontend::ModuleNode
  class_nodes = [] of CrystalV2::Compiler::Frontend::ClassNode
  def_nodes = [] of CrystalV2::Compiler::Frontend::DefNode

  exprs.each do |expr_id|
    node = arena[expr_id]
    case node
    when CrystalV2::Compiler::Frontend::ModuleNode
      module_nodes << node
    when CrystalV2::Compiler::Frontend::ClassNode
      class_nodes << node
    when CrystalV2::Compiler::Frontend::DefNode
      def_nodes << node
    end
  end

  module_nodes.each { |node| converter.register_module(node) }
  class_nodes.each { |node| converter.register_class(node) }
  def_nodes.each { |node| converter.register_function(node) }

  module_nodes.each { |node| converter.lower_module(node) }
  class_nodes.each { |node| converter.lower_class(node) }
  def_nodes.each { |node| converter.lower_def(node) }

  converter
end

private def lower_program_with_sources(code : String) : Crystal::HIR::AstToHir
  arena, exprs = parse(code)
  sources_by_arena = {arena => code}
  converter = Crystal::HIR::AstToHir.new(arena, sources_by_arena: sources_by_arena)
  converter.arena = arena

  module_nodes = [] of CrystalV2::Compiler::Frontend::ModuleNode
  class_nodes = [] of CrystalV2::Compiler::Frontend::ClassNode
  def_nodes = [] of CrystalV2::Compiler::Frontend::DefNode

  exprs.each do |expr_id|
    node = arena[expr_id]
    case node
    when CrystalV2::Compiler::Frontend::ModuleNode
      module_nodes << node
    when CrystalV2::Compiler::Frontend::ClassNode
      class_nodes << node
    when CrystalV2::Compiler::Frontend::DefNode
      def_nodes << node
    end
  end

  module_nodes.each { |node| converter.register_module(node) }
  class_nodes.each { |node| converter.register_class(node) }
  def_nodes.each { |node| converter.register_function(node) }

  module_nodes.each { |node| converter.lower_module(node) }
  class_nodes.each { |node| converter.lower_class(node) }
  def_nodes.each { |node| converter.lower_def(node) }

  converter
end

# Helper to get HIR text output
private def hir_text(func : Crystal::HIR::Function) : String
  String.build { |io| func.to_s(io) }
end

describe Crystal::HIR::AstToHir do
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

      text.should contain("literal 'a'")
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
      func.blocks[0].instructions.first.type.should eq(Crystal::HIR::TypeRef::INT64)
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

      func.params[0].type.should eq(Crystal::HIR::TypeRef::INT32)
    end
  end

  describe "block type lowering" do
    it "captures block parameter types as Proc" do
      func, converter = lower_function_with_converter("def foo(&block : Int32 -> String); 1; end")
      param = func.params.find { |p| p.name == "block" }
      param.should_not be_nil
      desc = converter.module.get_type_descriptor(param.not_nil!.type)
      desc.should_not be_nil
      desc.not_nil!.kind.should eq(Crystal::HIR::TypeKind::Proc)
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
      func = lower_function("def foo; true && false; end")
      text = hir_text(func)

      text.should contain("branch")
      text.should contain("phi")
    end

    it "lowers logical or" do
      func = lower_function("def foo; true || false; end")
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
      func = lower_function("def foo; if true; 1; end; end")
      text = hir_text(func)

      text.should contain("branch")
      text.should contain("phi")
      func.blocks.size.should be >= 3  # entry, then, else, merge
    end

    it "lowers if-else expression" do
      func = lower_function("def foo; if true; 1; else; 2; end; end")
      text = hir_text(func)

      text.should contain("branch")
      text.should contain("phi")
    end

    it "lowers unless expression" do
      func = lower_function("def foo; unless false; 1; end; end")
      text = hir_text(func)

      text.should contain("unop Not")  # Condition negated
      text.should contain("branch")
    end

    it "lowers while loop" do
      func = lower_function("def foo; while true; 1; end; end")
      text = hir_text(func)

      text.should contain("branch")
      text.should contain("jump")
    end

    it "lowers until loop" do
      func = lower_function("def foo; until false; 1; end; end")
      text = hir_text(func)

      text.should contain("unop Not")
      text.should contain("branch")
    end

    it "lowers ternary expression" do
      func = lower_function("def foo; true ? 1 : 2; end")
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
      func = lower_function("def foo; if true; if false; 1; end; end; end")
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

      func.return_type.should eq(Crystal::HIR::TypeRef::INT32)
    end

    it "lowers function with multiple parameters" do
      func = lower_function("def foo(a : Int32, b : String, c); end")

      func.params.size.should eq(3)
      func.params[0].type.should eq(Crystal::HIR::TypeRef::INT32)
      func.params[1].type.should eq(Crystal::HIR::TypeRef::STRING)
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
        .find { |inst| inst.is_a?(Crystal::HIR::Call) && inst.as(Crystal::HIR::Call).method_name.includes?("Foo#bar") }
      call.should_not be_nil
      call.not_nil!.as(Crystal::HIR::Call).args.size.should eq(2)
    end

    it "lowers method call with args" do
      func = lower_function("def foo; x.bar(1, 2); end")
      text = hir_text(func)

      text.should contain("call")
    end

    it "lowers free function call" do
      func = lower_function("def foo; puts(1); end")
      text = hir_text(func)

      text.should contain("call")
      text.should contain("puts")
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

      text.should contain("make_closure")
    end

    it "lowers proc with parameters" do
      func = lower_function("def foo; ->(x : Int32) { x + 1 }; end")
      text = hir_text(func)

      text.should contain("make_closure")
    end

    it "lowers block argument" do
      func = lower_function("def foo; each { |x| x }; end")
      text = hir_text(func)

      text.should contain("call")
      text.should contain("with_block")
    end

    it "marks closures as HeapEscape" do
      func = lower_function("def foo; -> { 1 }; end")

      # Find MakeClosure instruction
      closure = func.blocks.flat_map(&.instructions).find { |i| i.is_a?(Crystal::HIR::MakeClosure) }
      closure.should_not be_nil
      closure.not_nil!.lifetime.should eq(Crystal::HIR::LifetimeTag::HeapEscape)
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

      text.should contain("allocate")
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
    # NOTE: Parser doesn't yet produce AsNode for "x as Type" syntax
    # These tests are pending until parser support is added
    pending "lowers as cast" do
      func = lower_function("def foo(x); x as Int32; end")
      text = hir_text(func)

      text.should contain("cast")
    end

    pending "lowers as? safe cast" do
      func = lower_function("def foo(x); x as? Int32; end")
      text = hir_text(func)

      text.should contain("cast")
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
      # Declaration nodes (class/module/require/etc.) are treated as non-values
      # during expression lowering and are represented as `nil` at runtime.

      arena, exprs = parse("class Foo; end")
      converter = Crystal::HIR::AstToHir.new(arena)

      class_expr = exprs.first
      class_node = arena[class_expr]

      func = converter.module.create_function("test", Crystal::HIR::TypeRef::VOID)
      ctx = Crystal::HIR::LoweringContext.new(func, converter.module, arena)

      converter.lower_node(ctx, class_node)
      hir_text(func).should contain("literal nil")
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # LIFETIME TESTS
  # ═══════════════════════════════════════════════════════════════════════════

  describe "lifetime annotations" do
    it "marks literals as StackLocal" do
      func = lower_function("def foo; 42; end")

      literal = func.blocks[0].instructions.first
      literal.lifetime.should eq(Crystal::HIR::LifetimeTag::StackLocal)
    end

    it "marks parameters as HeapEscape (conservative)" do
      func = lower_function("def foo(x); x; end")

      func.params[0].lifetime.should eq(Crystal::HIR::LifetimeTag::HeapEscape)
    end

    it "marks array literals as StackLocal initially" do
      func = lower_function("def foo; [1, 2, 3]; end")

      arr = func.blocks.flat_map(&.instructions).find { |i| i.is_a?(Crystal::HIR::ArrayLiteral) }
      arr.should_not be_nil
      arr.not_nil!.lifetime.should eq(Crystal::HIR::LifetimeTag::StackLocal)
    end

    it "marks class var access as GlobalEscape" do
      func = lower_function("def foo; @@x; end")

      class_var = func.blocks.flat_map(&.instructions).find { |i| i.is_a?(Crystal::HIR::ClassVarGet) }
      class_var.should_not be_nil
      class_var.not_nil!.lifetime.should eq(Crystal::HIR::LifetimeTag::GlobalEscape)
    end
  end

  # ═══════════════════════════════════════════════════════════════════════════
  # SCOPE TESTS
  # ═══════════════════════════════════════════════════════════════════════════

  describe "scope handling" do
    it "creates function scope" do
      func = lower_function("def foo; 1; end")

      func.scopes.size.should be >= 1
      func.scopes[0].kind.should eq(Crystal::HIR::ScopeKind::Function)
    end

    it "creates block scope for if" do
      func = lower_function("def foo; if true; 1; end; end")

      block_scopes = func.scopes.select { |s| s.kind == Crystal::HIR::ScopeKind::Block }
      block_scopes.size.should be >= 1
    end

    it "creates loop scope for while" do
      func = lower_function("def foo; while true; 1; end; end")

      loop_scopes = func.scopes.select { |s| s.kind == Crystal::HIR::ScopeKind::Loop }
      loop_scopes.size.should be >= 1
    end

    it "creates closure scope for proc" do
      func = lower_function("def foo; -> { 1 }; end")

      closure_scopes = func.scopes.select { |s| s.kind == Crystal::HIR::ScopeKind::Closure }
      closure_scopes.size.should be >= 1
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
      func = converter.module.functions.find { |f| f.name == "Box#returns_self" }
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
        .find { |inst| inst.is_a?(Crystal::HIR::Call) && inst.as(Crystal::HIR::Call).method_name.includes?("#value") }
      call.should_not be_nil

      recv_id = call.not_nil!.as(Crystal::HIR::Call).receiver
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
        .find { |inst| inst.is_a?(Crystal::HIR::Call) && inst.as(Crystal::HIR::Call).method_name.includes?("#value") }
      call.should_not be_nil

      call.not_nil!.as(Crystal::HIR::Call).method_name.should contain("Box#value")
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
        .find { |inst| inst.is_a?(Crystal::HIR::Call) }
      call.should_not be_nil

      call.not_nil!.as(Crystal::HIR::Call).method_name.should contain("Box#value")
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
        .find { |inst| inst.is_a?(Crystal::HIR::Call) }
      call.should_not be_nil

      call_name = call.not_nil!.as(Crystal::HIR::Call).method_name
      call_name.should_not contain("Box#value")
      call_name.should_not contain("Bag#value")
    end

    it "prefers includers that match arity for module-typed params" do
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
        .find { |inst| inst.is_a?(Crystal::HIR::Call) }
      call.should_not be_nil

      call.not_nil!.as(Crystal::HIR::Call).method_name.should contain("Box#value")
    end

    it "prefers includers that match parameter types for module-typed params" do
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
        .find { |inst| inst.is_a?(Crystal::HIR::Call) }
      call.should_not be_nil

      call.not_nil!.as(Crystal::HIR::Call).method_name.should contain("Box#value")
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
        .find { |inst| inst.is_a?(Crystal::HIR::Call) && inst.as(Crystal::HIR::Call).method_name.includes?("map_like") }
      call.should_not be_nil

      desc = converter.module.get_type_descriptor(call.not_nil!.as(Crystal::HIR::Call).type)
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
      func = converter.module.functions.find { |f| f.name == "Box#foo" }
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
        def foo(indexables : Indexable(Indexable))
          ary = [] of typeof(Enumerable.element_type Enumerable.element_type indexables)
          ary
        end
      CRYSTAL

      converter = lower_program(code)
      func = converter.module.functions.find { |f| f.name.starts_with?("foo$") || f.name == "foo" }
      func.should_not be_nil

      array_lit = func.not_nil!.blocks.flat_map(&.instructions)
        .find { |inst| inst.is_a?(Crystal::HIR::ArrayLiteral) }
      array_lit.should_not be_nil

      element_type = array_lit.not_nil!.as(Crystal::HIR::ArrayLiteral).element_type
      desc = converter.module.get_type_descriptor(element_type)
      if desc
        desc.not_nil!.name.should_not eq("Pointer(Void)")
      else
        element_type.should eq(Crystal::HIR::TypeRef::POINTER)
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
      func.not_nil!.return_type.should eq(Crystal::HIR::TypeRef::INT32)
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
      func = lower_function("def foo; if true; 1; else; 2; end; end")

      func.blocks.each do |block|
        block.terminator.should_not be_a(Crystal::HIR::Unreachable)
      end
    end

    it "creates correct CFG for if" do
      func = lower_function("def foo; if true; 1; else; 2; end; end")

      # Should have: entry -> branch -> then/else -> merge
      func.blocks.size.should be >= 4

      # Entry should end with branch
      entry = func.get_block(func.entry_block)
      entry.terminator.should be_a(Crystal::HIR::Branch)
    end

    it "creates correct CFG for while" do
      func = lower_function("def foo; while true; 1; end; end")

      # Entry -> jump -> cond -> branch -> body/exit
      # body -> jump back to cond
      func.blocks.size.should be >= 3

      # Should have at least one Jump back (loop)
      jumps = func.blocks.count { |b| b.terminator.is_a?(Crystal::HIR::Jump) }
      jumps.should be >= 1
    end

    it "phi nodes have correct incoming edges" do
      func = lower_function("def foo; if true; 1; else; 2; end; end")

      phi = func.blocks.flat_map(&.instructions).find { |i| i.is_a?(Crystal::HIR::Phi) }
      phi.should_not be_nil

      phi_node = phi.not_nil!.as(Crystal::HIR::Phi)
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
        .select { |inst| inst.is_a?(Crystal::HIR::Parameter) }
      ptr_param = params.find { |inst| inst.as(Crystal::HIR::Parameter).name == "ptr" }
      ptr_param.should_not be_nil

      param_type = ptr_param.not_nil!.as(Crystal::HIR::Parameter).type
      desc = converter.module.get_type_descriptor(param_type)
      desc.should_not be_nil
      desc.not_nil!.name.should eq("Pointer(Int32)")
    end
  end
end
