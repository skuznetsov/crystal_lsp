# Crystal v2 Frontend & LSP Roadmap

This TODO tracks work to bring the v2 frontend (parser + macros + semantic/type
layer) to parity with the original Crystal compiler, and then grow beyond it.

The immediate priority is **LSP correctness**: the v2 LSP server must not lie
about syntax or types and should match what the original compiler would report.

---

## Current Status (2025-12-07)

### Test Coverage
- **2928 tests**, 0 failures, 12 pending
- **1390 test cases** ported from Crystal's original `parser_spec.cr`
- **97.6% parser compatibility** with original Crystal

### Completed
- [x] Parser parity with original Crystal (~97.6%)
- [x] AST class inheritance migration (94 node types)
- [x] String interning (Phase A memory optimization)
- [x] `out` keyword handling (C bindings + identifier contexts)
- [x] Inline `asm` basic syntax
- [x] VirtualArena zero-copy multi-file AST
- [x] Parallel FileLoader with deduplication
- [x] Basic type inference engine
- [x] Symbol table and name resolution
- [x] MVP MacroExpander (`{{ }}`, `{% if/for %}`, `@type.name/instance_vars`)
- [x] LSP semantic tokens: symbol literals emit full-span enumMember tokens (no overlaps)
- [x] Hover/definition fallback to cached expr types (scoped lookup fixes; e.g., `cas` → `Array(Atom)`)
- [x] lsp_probe speaks correct binary Content-Length (no dropped responses)

### Pending (7 tests)
- 6 macro whitespace trimming (`{%- -%}`, `{%~ ~%}`) - for web templates
- 1 invalid ASM syntax test (intentionally pending)

---

## 1. Parser Parity - COMPLETE

- [x] Global diagnostics reduced from ~24k to ~0
- [x] 1390/1390 original Crystal parser tests passing
- [x] Command-call chains, tuple types, double splat, postfix modifiers
- [x] Heredocs, multi-line strings, blocks, case/when, rescue/ensure
- [x] Inline `asm` basic syntax
- [x] `out` keyword (C bindings output parameter + identifier fallback)

### Remaining Edge Cases (Low Priority)
- [ ] Macro whitespace trimming (`{%- -%}`, `{%~ ~%}`) - web templates only
- [ ] Full `{% ... %}` complex nested cases (rare in practice)

---

## 2. MacroExpander Parity - IN PROGRESS (~90%)

- [x] MVP macro engine:
  - [x] `{{ ... }}` expansion for basic literals/paths
  - [x] `{% if/elsif/else/end %}` with boolean/numeric conditions
  - [x] `{% for %}` over arrays and integer ranges
  - [x] `%var` macro variables with deterministic naming
  - [x] Re-parse expanded code through v2 parser

- [x] Basic `@type.*` API:
  - [x] `@type.name(generic_args: false)` and `@type.size`
  - [x] `@type.instance_vars` iteration
  - [x] `@type.overrides?(Base, "method")` check

- [x] Basic annotation support:
  - [x] Collect annotations in SymbolCollector
  - [x] Truthiness-style `ivar.annotation(Foo)` checks

### Completed Rich Macro API
- [x] `@type.instance_vars` with full metadata (`ivar.type`, `ivar.has_default_value?`, `ivar.default_value`, `ivar.type.nilable?`)
- [x] `@type.methods`, `@type.superclass`, `@type.has_method?("name")`
- [x] Macro methods: `.stringify`, `.id`, `.class_name`
- [x] Type predicates: `@type.class?`, `@type.struct?`, `@type.module?`
- [x] Complex condition evaluation (`&&`, `||`, numeric comparisons)

### Completed Rich Macro API (continued)
- [x] Annotation objects with `.args`, `.named_args`, `[]` access (`ann[:key]` pattern)
- [x] `@type.abstract?` with abstract flag tracking

### Completed Compile-Time Operators
- [x] `typeof(...)` - infers type of expressions (literals, variables)
- [x] `sizeof(...)` - returns size of primitive types (Int8-128, Float32/64, Pointer)
- [x] `alignof(...)` - returns alignment of primitive types
- [x] `instance_alignof(...)` - returns instance alignment (pointer alignment)

### Completed Rich Type Introspection
- [x] `@type.name(generic_args: true/false)` - returns type name with/without generic params

### TODO: Rich Macro API
- [ ] Full type graph integration (instantiated generic types)

---

## 3. Semantic & Type Inference - IN PROGRESS (~75%)

- [x] Basic type inference (literals, variables, simple methods)
- [x] Symbol table with scope tracking
- [x] SymbolCollector (classes, methods, variables, annotations)
- [x] Name resolver (finds definitions)
- [x] Diagnostic formatter

### Completed Type Graph
- [x] Full type graph: ClassType, ModuleType, UnionType, TupleType, InstanceType
- [x] ProcType for proc literals with parameter and return types
- [x] NamedTupleType for named tuple literals
- [x] PointerType for C interop
- [x] ArrayType, HashType, RangeType
- [x] TypeParameter for generic types
- [x] Generic type instantiation and unification (basic)

### Completed Module System
- [x] include/extend module mixins (modules added to scope.included_modules)
- [x] Method resolution through included modules (MRO search)
- [x] Transitive include support (C includes B includes A)
- [x] extend adds to class_scope for class methods

### Completed Flow Typing
- [x] Union type narrowing with flow-sensitive analysis (Phase 96)
- [x] Nil check narrowing: `if x` where x : T? narrows to T
- [x] is_a? narrowing: `if x.is_a?(T)` narrows to T
- [x] Negative narrowing in else branch (remaining types)
- [x] T? syntax parsing as T | Nil union
- [x] T | U | V union syntax parsing

### TODO: Advanced Type System
- [ ] Case/when type narrowing
- [ ] Method overload resolution based on argument types
- [ ] Virtual types (inheritance-aware method dispatch)
- [ ] Integrate macro expansion into semantic phase

---

## 4. LSP Server Correctness - NEXT FOCUS

Goal: v2 LSP must report only real errors and match original compiler behavior.

- [x] Wired to v2 parser and symbol collector
- [x] Diagnostics parity: fixed []? vs ternary disambiguation (no false positives on server.cr)
  - Fixed `&.[expr]?` block shorthand pattern
  - Fixed `h ? h[x]? : nil` ([]? inside ternary then-branch)
  - Fixed `ENV["X"]? ? 25 : 0` ([]? followed by ternary)
- [x] Types & hover accuracy: stdlib types (Array, String, Hash, Int32, Float64), array element access (arr[0] → Atom)
- [x] Navigation to stdlib/prelude symbols (Time, File, etc.)
- [x] Prelude handling with cache + mtime tracking (cached summaries/types for warm start)

### Tests Needed
- [x] Structured LSP tests for stdlib symbols (`Time.now`, `File.basename`, array types, etc.) - see `stdlib_hover_spec.cr`, `stdlib_navigation_spec.cr`
- [ ] Diff v2 diagnostics against original compiler on representative files
- [ ] Hover/definition regression spec covering cached types across required files
- [x] Integration specs for hover/definition sequences (single-file path regression)
- [x] Integration specs for references via server across VirtualArena requires
- [x] Diagnostics spec with semantic diagnostics enabled (semantic error guard)
- [x] Inlay hints end-to-end on a small program (positions/labels)
- [x] Semantic tokens integration: require strings stay strings (no enumMember); symbol literals remain single full-span token
- [x] Integration specs for hover/definition covering stdlib/prelude and indexing-in-progress guard
- [x] Integration specs for rename via server across VirtualArena requires
- [x] Guard hover/definition when indexing in progress (soft-fail)
- [x] Rename guard for stdlib/prelude symbols (no-op or error)
- [x] VSCode extension: request/response log channel and “Indexing…” status indicator in UI
- [x] Navigation to stdlib/prelude (tests + impl)
- [x] Folding ranges for begin/rescue/else/ensure without overfolding; semantic tokens for symbol literals fixed
- [ ] Regression scenarios via `tools/lsp_scripts` (rename, stdlib, hover→definition chains; nested consts A/M::A; class/instance vars)
- [x] LSP spec coverage for member access typed via locals/arrays (`cas`, `a.sigma`, class vars) - see `stdlib_hover_spec.cr`
- [ ] Optional: detect external workspace roots for cache reuse when opening files outside @project_root

---

## LSP Project Cache (New)
- [x] Versioned project cache (v2) with symbol summaries (classes/modules/method signatures) + real mtime
- [x] Background indexing of `root/src/**/*.cr` to populate cache automatically
- [ ] Extend summaries with ivars/consts and richer type info (params/return types already stored)
- [x] Restore symbol_table from cache for unchanged files; avoid re-parse/resolve when mtime matches (spans placeholder)
- [x] Merge cached project symbols into analysis to avoid reloading requires on warm didOpen
- [x] Cache and restore symbol spans and inferred types in summaries (cache version v3); expose cached types for hover/definition fallback
- [ ] Mark cached files (`from_cache`) and use summaries for hover/definition when AST is missing
- [ ] Strict cache validation (version/root hash/mtime) with full reparse fallback (root hash/version done; add fallback wiring)
- [ ] Extend summaries with ivars/class vars/consts and richer type info; reuse same pipeline for prelude
- [ ] Make cache/inference idempotent: if infer times out, resume later and backfill tables in background fibers
- [ ] Apply rich cache pipeline to prelude: spans/types/ivars/class vars, rebuild prelude symbol_table from cache without full parse when unchanged

---

## TypeIndex Integration (Phase 1-3)

**Goal:** Replace JSON-based `expr_types_json` with binary TypeIndex for 40-60% storage reduction and faster loads.

**Architecture (Quadrumvirate-analyzed 2025-12-06):**
- TypeArena: Interned type storage with O(1) lookup via TypeId
- ExprTypeIndex: Dense array + sparse hash hybrid for ExprId→TypeId
- Binary serialization with magic bytes + version header
- File range tracking for incremental invalidation

### Phase 1: Parallel Storage (Complete)
- [x] TypeIndex core implementation (type_index.cr)
- [x] TypeArena with interning (14 tests pass)
- [x] Binary serialization with symbol fallback
- [x] Add TypeIndex to ProjectCache (v4)
- [x] Add TypeIndex to PreludeCache (v4)
- [x] Write both formats during save (JSON for backwards compat)
- [x] Read TypeIndex when available, fallback on EOF

### Phase 2: Validation (Complete)
- [x] Add validation logging to load_from_cache
- [x] Track metrics: matches, mismatches, json_only, typeindex_only
- [x] Add reset_validation_metrics and validation_metrics accessors
- [x] 4 validation spec tests passing
- [x] Fixed ExprId collision via per-file TypeIndex partitioning

**Phase 2 Implementation:**
- TypeIndex now stores per-file ExprTypeIndex maps: `@file_expr_types : Hash(String, ExprTypeIndex)`
- `set_type(path, expr_id, type)` and `get_type(path, expr_id)` for file-scoped access
- TypeIndex serialization version bumped to v2 (per-file format)
- Validation confirms 0 mismatches across multi-file scenarios

**Phase 2 Benchmark Results:**
- JSON parse: 0.166s for 1000 types × 100 iterations
- Binary (TypeIndex): 0.029s for same workload
- **Speedup: 5.6x faster** than JSON parsing
- Storage: ~18KB JSON → ~8KB binary (estimated 40-55% reduction)

### Phase 3: Migration (Complete)
- [x] Deprecate expr_types_json (bump cache version to v5)
- [x] TypeIndex becomes primary storage
- [x] Remove JSON fallback code
- [x] Update server.cr to use TypeIndex-only loading
- [x] 199 LSP tests pass, 4 cache validation tests pass

**Critical vulnerabilities (ADVERSARY analysis):**
1. ExprId instability on file edit → must invalidate on change
2. Symbol table load order → graceful PrimitiveType fallback
3. Truncated file handling → robust EOF detection
4. ~~ExprId collision across files~~ → **FIXED** via per-file partitioning

---

## 5. Beyond Parity: IR & Codegen (Future)

(After LSP correctness achieved)

### 5.1 Typed SSA IR
- [ ] Crystal-specific typed SSA IR before LLVM lowering
- [ ] Per-function IR with explicit control flow graph
- [ ] Effect summaries for function boundaries
- [ ] Region-based alias analysis in IR

### 5.2 Memory Management: Lifetime Coloring (No Borrow Checker)

**Vision:** Compile-time memory management via AST-based lifetime analysis,
without Rust-style lifetime annotations. Developer writes code like Ruby/Go,
compiler infers optimal memory strategy.

**Inspiration:**
- V lang: Compiler tracks allocations/deallocations automatically
- Linux kernel slabs: Pre-allocated memory pools per context
- Swift ARC: Reference counting without GC pauses

**Analysis Pipeline:**
```
AST + Type Graph
      │
      ├─▶ Escape Analysis (does value leave scope?)
      ├─▶ Alias Analysis (who else references this?)
      └─▶ Lifetime Coloring (paint value's journey)
              │
              ▼
      Memory Strategy Map (per allocation site)
```

**Strategy Selection:**
| Condition | Strategy | Runtime Cost |
|-----------|----------|--------------|
| No escape, short-lived | Stack allocation | 0 |
| No escape, large/dynamic | Arena/slab | ~0 |
| Single owner, escapes | Move semantics | 0 |
| Multiple owners, no cycles | ARC (ref counting) | ref ops |
| Complex cycles | Fallback to GC | GC pause |

**Fiber-local Arenas (slab idea):**
- Pre-allocate arena per fiber/coroutine
- All allocations within fiber go to its slab
- On fiber completion, bulk-free entire slab (single dealloc)

**TODO:**
- [ ] Escape analysis pass in IR
- [ ] Alias analysis (region-based)
- [ ] Lifetime coloring algorithm
- [ ] Strategy selector based on analysis results
- [ ] Arena allocator integration
- [ ] ARC codegen for multi-owner cases
- [ ] Configurable GC fallback (Boehm baseline)

### 5.3 LLVM Codegen
- [ ] Per-thread LLVM module contexts
- [ ] Batched optimization pass pipelines
- [ ] Profile-guided optimization (PGO) hooks
- [ ] LTO toggle for release builds
- [ ] Fast LLVM codegen with competitive build times

### 5.4 Alternative Backends (Experimental)
- [ ] WASM emitter through same mid-end
- [ ] eBPF emitter for kernel/tracing use cases

---

## 6. Follow-up (Post-LSP Stability)

- [ ] Semantic service/API for agents (structured queries)
- [ ] Structural patch layer (rename/extract/move with validation)
- [ ] Zero-copy name handling (interning, span-based lookups)
- [ ] JVM backend (experimental)

---

## Quick Reference

| Component | Status | Tests |
|-----------|--------|-------|
| Parser | ~97.6% | 1390+1466 |
| Lexer | Complete | Part of parser tests |
| AST | Complete | Class inheritance done |
| MacroExpander | ~90% | MVP + ann[:key] + typeof/sizeof + @type.name(generic_args) |
| Type Inference | ~75% | Full type graph + ProcType + NamedTupleType + include/extend + flow typing |
| LSP Server | ~70% | 21 methods implemented |
| Codegen | 0% | Future phase |
