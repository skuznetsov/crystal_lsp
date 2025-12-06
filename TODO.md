# Crystal v2 Frontend & LSP Roadmap

This TODO tracks work to bring the v2 frontend (parser + macros + semantic/type
layer) to parity with the original Crystal compiler, and then grow beyond it.

The immediate priority is **LSP correctness**: the v2 LSP server must not lie
about syntax or types and should match what the original compiler would report.

---

## Current Status (2025-12-06)

### Test Coverage
- **2856 tests**, 0 failures, 7 pending
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

## 2. MacroExpander Parity - IN PROGRESS (~60%)

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

### TODO: Rich Macro API
- [ ] Rich `@type.name(generic_args: ...)` with full type graph
- [ ] `@type.instance_vars` with full metadata (type, default_value, nilable?, etc.)
- [ ] `@type.methods`, `@type.annotations`, `@type.superclass`
- [ ] Annotation objects with `.args`, `.named_args`, `[]`
- [ ] Macro methods: `.stringify`, `.id`, `.class_name`, `.class?`, `.struct?`, etc.
- [ ] `typeof(...)`, `sizeof(...)`, `alignof(...)` in macros
- [ ] Complex condition evaluation (`&&`, `||`, `!`, arithmetic)

---

## 3. Semantic & Type Inference - IN PROGRESS (~50%)

- [x] Basic type inference (literals, variables, simple methods)
- [x] Symbol table with scope tracking
- [x] SymbolCollector (classes, methods, variables, annotations)
- [x] Name resolver (finds definitions)
- [x] Diagnostic formatter

### TODO: Full Type System
- [ ] Full type graph: ClassType, ModuleType, UnionType, TupleType, ProcType, etc.
- [ ] Generic type instantiation and unification
- [ ] Union type narrowing with flow-sensitive analysis (`if var`, `is_a?`, `case`)
- [ ] Method overload resolution based on argument types
- [ ] include/extend, inheritance, virtual types
- [ ] Integrate macro expansion into semantic phase

---

## 4. LSP Server Correctness - NEXT FOCUS

Goal: v2 LSP must report only real errors and match original compiler behavior.

- [x] Wired to v2 parser and symbol collector
- [x] Diagnostics parity: fixed []? vs ternary disambiguation (no false positives on server.cr)
  - Fixed `&.[expr]?` block shorthand pattern
  - Fixed `h ? h[x]? : nil` ([]? inside ternary then-branch)
  - Fixed `ENV["X"]? ? 25 : 0` ([]? followed by ternary)
- [ ] Types & hover accuracy (match original compiler)
- [ ] Navigation to stdlib/prelude and macro-generated methods
- [x] Prelude handling with cache + mtime tracking (cached summaries/types for warm start)

### Tests Needed
- [ ] Structured LSP tests for stdlib symbols (`Time.monotonic`, `File.read`, etc.)
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
- [ ] LSP spec coverage for member access typed via locals/arrays (`cas`, `a.sigma`, class vars) to avoid Nil/Unknown hovers/definitions
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
| MacroExpander | ~60% | MVP working |
| Type Inference | ~50% | Basic working |
| LSP Server | ~70% | 21 methods implemented |
| Codegen | 0% | Future phase |
