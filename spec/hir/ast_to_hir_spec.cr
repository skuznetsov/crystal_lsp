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

      text.should contain("binop And")
    end

    it "lowers logical or" do
      func = lower_function("def foo; true || false; end")
      text = hir_text(func)

      text.should contain("binop Or")
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

      text.should contain("index_get")
    end

    it "lowers index assignment" do
      func = lower_function("def foo; arr[0] = 1; end")
      text = hir_text(func)

      text.should contain("index_set")
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
    it "raises on unsupported node type" do
      # Macro nodes should raise error during lowering
      # (they should be expanded before HIR conversion)

      expect_raises(Crystal::HIR::LoweringError) do
        # Try to lower something that isn't a function
        arena, exprs = parse("class Foo; end")
        converter = Crystal::HIR::AstToHir.new(arena)

        # Get class node
        class_expr = exprs.first
        class_node = arena[class_expr]

        # Try to lower as expression (should work but produce limited HIR)
        ctx = Crystal::HIR::LoweringContext.new(
          converter.module.create_function("test", Crystal::HIR::TypeRef::VOID),
          converter.module,
          arena
        )
        converter.lower_node(ctx, class_node)
      end
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
end
