# Crystal V2 - Missing Features and Known Issues

This document tracks features and functionality that are not yet implemented or have known issues compared to the original Crystal compiler.

## Status: Current Test Results
- **Total Examples:** 1425
- **Passing:** 1421 (99.72%)
- **Failures:** 3 (0.21%)
- **Errors:** 1 (0.07%)
- **Pending:** 6 (skipped)
- **Last Updated:** 2025-11-10

### Recent Progress
- **Session Start (earlier):** 14 failures, 1 error (15 total issues)
- **After Compound Assignment + Union Types:** 5 failures, 1 error (6 total issues)
- **After String Interpolation + Begin/End fixes:** 3 failures, 1 error (4 total issues)
- **Total Improvement:** 11 tests fixed (73% reduction in failures)
- **Fixes:** Union types (2) + Compound assignments (7) + String interpolation (1) + Begin/end (1)

## 1. Parser - Missing Features

### 1.1 Proc Literal Return Type Annotation
**Status:** NOT IMPLEMENTED
**Priority:** Medium
**Test Files:** `spec/semantic_proc_literal_spec.cr:68`

**Issue:**
Parser does not support proc literal syntax with return type annotation:
```crystal
->(x : Int32) : Int32 { x * 2 }
#              ^^^^^^^^ - Return type annotation not supported
```

**Works:**
```crystal
->(x : Int32) { x * 2 }  # Without return type annotation
```

**Error:**
```
unexpected EOF
```

**Root Cause:**
Parser's proc literal parsing logic doesn't handle the optional `: ReturnType` syntax after the parameter list.

**Required Changes:**
- Modify `parse_proc_literal` method in `parser.cr`
- Add return type annotation parsing after closing `)` of parameter list
- Store return type in ProcLiteralNode AST structure

**Files to Modify:**
- `src/compiler/frontend/parser.cr` (proc literal parsing)
- `src/compiler/frontend/ast.cr` (ProcLiteralNode structure)

---

## 2. Type Inference - Incomplete Features

### 2.1 Compound Assignment Type Inference ✅ FIXED
**Status:** COMPLETED (2025-11-10)
**Priority:** High (was)
**Test Files:** ALL PASSING
- `spec/semantic/type_inference_spec.cr:3190` (+=) ✅
- `spec/semantic/type_inference_spec.cr:3212` (-=) ✅
- `spec/semantic/type_inference_spec.cr:3230` (*=) ✅
- `spec/semantic/type_inference_spec.cr:3248` (/=) ✅
- `spec/semantic/type_inference_spec.cr:3266` (%=) ✅
- `spec/semantic/type_inference_spec.cr:3284` (**=) ✅
- `spec/semantic/type_inference_spec.cr:3302` (with float) ✅

**Issue (was):**
Compound assignments like `x += 5` returned `Nil` instead of Int32.

**Root Cause:**
Cycle detection in iterative loop (line 95-101) was setting IdentifierNodes to `nil_type` when it detected them as "cycles". This prevented the recursive path from checking `@assignments` hash.

**Solution:**
Modified cycle detection to SKIP IdentifierNodes:
```crystal
when 1
  # Cycle detected; assign nil_type to break it
  # EXCEPTION: Don't set Nil for IdentifierNode - it needs to check @assignments
  child_node = @program.arena[child]
  unless child_node.is_a?(Frontend::IdentifierNode)
    @context.set_type(child, @context.nil_type)
  end
  next
```

**Files Modified:**
- `src/compiler/semantic/type_inference_engine.cr:95-102`

**Result:** ALL 7 compound assignment tests now passing!

---

### 2.2 Union Type Inference
**Status:** INCOMPLETE
**Priority:** Medium
**Test Files:**
- `spec/semantic/type_inference_spec.cr:775` (Int64 + Float64 union)
- `spec/semantic/type_inference_spec.cr:823` (case without else → Nil union)
- `spec/semantic/type_inference_spec.cr:845` (empty case branch → Nil)

**Issue:**
Union type creation and inference not fully working in some edge cases.

**Examples:**
1. Numeric type promotion to union types
2. Case expressions without else clause should return union with Nil
3. Empty case branches should return Nil

**Required Investigation:**
- Union type creation logic in `@context.union_of`
- Case/when type inference in control flow
- Integration with nil type

---

### 2.3 Numeric Type Promotion
**Status:** INCOMPLETE
**Priority:** Medium
**Test Files:**
- `spec/semantic/type_inference_spec.cr:775` (Int64 + Float64 → Float64)
- `spec/semantic/type_inference_spec.cr:808` (complex promotion)
- `spec/semantic/type_inference_spec.cr:3302` (compound with float)
- 1 more test

**Issue:**
Numeric type promotion rules not fully implemented.

**Crystal Rules:**
- Int32 + Int64 → Int64
- Int64 + Float64 → Float64
- Any integer + Float → Float
- Smaller int + bigger int → bigger int

**Current Status:**
Basic promotion exists in `promote_numeric_types` method, but edge cases fail.

**Required Work:**
- Review and complete `promote_numeric_types` logic
- Handle all combinations of numeric types
- Test with complex expressions

---

## 3. Break/Next Statements
**Status:** INCOMPLETE
**Priority:** Medium
**Test Files:**
- `spec/semantic/type_inference_spec.cr:2253` (break with value)
- `spec/semantic/type_inference_spec.cr:2325` (break with String)

**Issue:**
Break statements with values not properly typed.

**Example:**
```crystal
while true
  break 42  # Should propagate Int32 as loop result type
end
```

**Required Investigation:**
- How break/next values affect loop result types
- Integration with control flow type inference
- Union of all break values + normal loop completion type

---

## 4. Recently Fixed Issues ✅

### 4.1 String Interpolation Expression Types ✅
**Fixed:** 2025-11-10
**Test:** `spec/semantic/type_inference_spec.cr:1706`

**Issue:**
Expressions inside string interpolation (like `#{x + y}`) were returning `nil` type instead of their actual type (Int32).

**Root Cause:**
`compute_node_type_no_recurse` was returning `string_type` for StringInterpolationNode at lines 594-598, taking the iterative shortcut. This skipped the recursive `infer_string_interpolation` method which actually infers types for the expression pieces.

**Solution:**
Changed StringInterpolationNode handling in `compute_node_type_no_recurse` (lines 594-598) to return `nil`, forcing the recursive path where `infer_string_interpolation` is called and expression pieces get their types inferred.

**Files Modified:**
- `src/compiler/semantic/type_inference_engine.cr:594-598, 1914-1916`

---

### 4.2 Begin/End Block Return Type ✅
**Fixed:** 2025-11-10
**Test:** `spec/semantic/type_inference_begin_spec.cr:146`

**Issue:**
`begin...end` blocks were returning `Nil` instead of the type of their last expression.

**Root Cause:**
`compute_node_type_no_recurse` was handling BeginNode in the iterative path (lines 574-587) trying to call `infer_expression(body.last)`, but this couldn't work reliably if body expressions hadn't been processed yet.

**Solution:**
Changed BeginNode handling in `compute_node_type_no_recurse` (lines 574-578) to return `nil`, forcing the recursive path where `infer_begin` properly handles scoping and returns the type of the last expression.

**Files Modified:**
- `src/compiler/semantic/type_inference_engine.cr:574-578`

---

### 4.3 Diagnostic Emission
**Fixed:** 2025-11-10
**Commit:** (current session)

**Issue:**
Control flow validation diagnostics (Bool type checks for if/while conditions, operator type validation) were not being emitted.

**Root Cause:**
`compute_node_type_no_recurse` (iterative path) had logic for IfNode, WhileNode, UnlessNode, and BinaryNode but didn't perform validation. Validation logic was only in recursive methods (infer_if, infer_while, infer_binary).

**Solution:**
Modified `compute_node_type_no_recurse` to return `nil` for nodes requiring validation, triggering recursive fallback where diagnostics are emitted:
- IfNode, UnlessNode, WhileNode, UntilNode → return nil
- BinaryNode (except comparisons and ranges) → return nil

**Tests Fixed:** 4 diagnostic tests

---

### 4.2 IdentifierNode Type Inference
**Fixed:** (previous session)
**Commit:** 75067baa7

**Issue:**
Class name identifiers (e.g., `Calculator` in `Calculator.new`) returned Nil instead of ClassType.

**Root Cause:**
`compute_node_type_no_recurse` for IdentifierNode only checked `@assignments` hash (local variables) and returned `@context.nil_type` for class names. This prevented the recursive fallback to symbol table lookup.

**Solution:**
Changed IdentifierNode handling to:
1. Check `@assignments[name]?` first (returns Type or nil)
2. If nil, return nil to trigger recursive fallback
3. Recursive path (`infer_identifier`) does symbol table lookup for class names

**Tests Fixed:** 25 tests

---

### 4.3 Debug Infrastructure
**Added:** (previous session)

Added unified `debug()` method pattern across:
- `lexer.cr`
- `parser.cr`
- `type_inference_engine.cr`

**Pattern:**
```crystal
@debug_enabled : Bool
# Set in initializer from ENV["COMPONENT_DEBUG"]? == "1"

private def debug(msg : String)
  STDERR.puts "[COMPONENT_DEBUG] #{msg}" if @debug_enabled
end
```

**Benefit:**
Consistent debugging experience across all compiler components.

---

## 5. Architecture Notes

### 5.1 Dual-Path Type Inference
The type inference engine uses a dual-path architecture:

1. **Iterative Path:** `compute_node_type_no_recurse`
   - Processes simple nodes using already-computed child types
   - Fast, non-recursive
   - Returns `Type?` (nil for complex nodes)

2. **Recursive Path:** `infer_*` methods
   - Handles complex nodes requiring validation
   - Emits diagnostics
   - Full type computation

**Decision Rule:**
- Simple nodes (literals, operators with known behavior) → iterative
- Complex nodes (control flow, method calls, validation required) → recursive (return nil from iterative)

### 5.2 Node Classification
Nodes are classified by **operational complexity**, not syntactic simplicity:

- **Simple:** NumberNode, StringNode, BoolNode, comparisons, ranges
- **Complex:** IfNode (needs Bool validation), BinaryNode with arithmetic (needs operator validation), IdentifierNode for class names (needs symbol lookup)

---

## 6. Testing Strategy

### 6.1 Current Test Organization
- `spec/semantic/type_inference_spec.cr` - Main type inference tests
- `spec/semantic_proc_literal_spec.cr` - Proc literal specific tests
- `spec/lsp/` - LSP functionality tests (all passing)

### 6.2 Test Execution
```bash
# Full test suite
env CRYSTAL_CACHE_DIR=./.crystal-cache crystal spec

# Specific test
env CRYSTAL_CACHE_DIR=./.crystal-cache crystal spec spec/semantic/type_inference_spec.cr:170

# With debug output
env TYPE_INFERENCE_DEBUG=1 CRYSTAL_CACHE_DIR=./.crystal-cache crystal spec
```

---

## 7. Priority Roadmap

### Immediate (This Session)
1. ✅ Fix diagnostic emission (DONE)
2. ⏳ Investigate union type issues
3. ⏳ Investigate numeric promotion

### Short Term (Next Sessions)
1. Fix break/next type inference
2. Implement proc literal return type annotation (parser)
3. Complete numeric promotion logic
4. Fix union type edge cases

### Medium Term
1. **Compound assignment** - Complex architectural issue requiring careful investigation
2. Full test suite passing (100%)

### Long Term
1. Performance optimization
2. Full LSP feature parity
3. Integration with original Crystal codebase

---

## 8. References

### Key Files
- **Parser:** `src/compiler/frontend/parser.cr`
- **Lexer:** `src/compiler/frontend/lexer.cr`
- **Type Inference:** `src/compiler/semantic/type_inference_engine.cr`
- **AST:** `src/compiler/frontend/ast.cr`
- **Types:** `src/compiler/semantic/type.cr`

### Git Log
```bash
# View recent type inference commits
git log --oneline -- src/compiler/semantic/type_inference_engine.cr

# Current branch
git status
# Branch: new_crystal_parser
```

### Knowledge Base
This TODO.md should be kept in sync with:
- Test failures
- Git commits
- Session notes
- Knowledge Core entries (if using MCP)

---

Last Updated: 2025-11-10
