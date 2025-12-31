# Crystal v2 Frontend & LSP Roadmap

This TODO tracks work to bring the v2 frontend (parser + macros + semantic/type
layer) to parity with the original Crystal compiler, and then grow beyond it.

The immediate priority is **LSP correctness**: the v2 LSP server must not lie
about syntax or types and should match what the original compiler would report.

---

## Current Status (2025-12-19)

### Test Coverage
- **3407 tests**, 0 failures, 9 pending
- **1390 test cases** ported from Crystal's original `parser_spec.cr`
- **97.6% parser compatibility** with original Crystal

### Completed
- [x] Parser parity with original Crystal (~97.6%)
- [x] AST class inheritance migration (94 node types)
- [x] String interning (Phase A memory optimization)
- [x] `out` keyword handling (C bindings + identifier contexts)
- [x] Inline `asm` basic syntax
- [x] Macro literal text spans cover full token range for accurate hover/definition (2025-12-31)
- [x] VirtualArena zero-copy multi-file AST
- [x] Parallel FileLoader with deduplication
- [x] Full type inference engine (Phase 103A-C)
- [x] Symbol table and name resolution
- [x] Full MacroExpander with @type API
- [x] LSP semantic tokens: symbol literals emit full-span enumMember tokens (no overlaps)
- [x] Hover/definition fallback to cached expr types (scoped lookup fixes; e.g., `cas` → `Array(Atom)`)
- [x] lsp_probe speaks correct binary Content-Length (no dropped responses)
- [x] TypeIndex binary storage (5.6x faster than JSON)
- [x] HIR macro condition evaluation: tri-state merge + duplicate module method guard (2025-12-23)
- [x] Driver trace logging gated via `CRYSTAL_V2_DRIVER_TRACE` (2025-12-23)
- [x] Resolve module method calls without parens (`M.foo`) to static dispatch (2025-12-23)
- [x] Macro expansion in HIR handles class/module body calls and main macro calls (2025-12-26)
- [x] MacroExpander reparse uses parse_program; macro is_a? and macro literal gap fixes (2025-12-26)

### Pending (1 test)
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
- [x] Macro whitespace trimming (`{%- -%}`, `{%~ ~%}`) - web templates only
- [x] Full `{% ... %}` complex nested cases (escaped `\{%`/`\{{}` inside macro bodies) (2025-12-25)

---

## 2. MacroExpander Parity - COMPLETE (~99%)

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
- [x] Annotation objects with `.args`, `.named_args`, `[]` access (`ann[:key]` pattern)
- [x] `@type.abstract?` with abstract flag tracking
- [x] `@type.type_vars` for generic type parameters
- [x] Full generic class support (@type.* works on Box(T), Pair(K,V), etc.)

### Completed Compile-Time Operators
- [x] `typeof(...)` - infers type of expressions (literals, variables)
- [x] `sizeof(...)` - returns size of primitive types (Int8-128, Float32/64, Pointer)
- [x] `alignof(...)` - returns alignment of primitive types
- [x] `instance_alignof(...)` - returns instance alignment (pointer alignment)

### Completed Rich Type Introspection
- [x] `@type.name(generic_args: true/false)` - returns type name with/without generic params

### Low Priority (Codegen-only)
- [ ] Runtime instantiated generic type resolution (e.g., T=Int32 at call site in macro) - requires full codegen

---

## 3. Semantic & Type Inference - COMPLETE (~99%)

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
- [x] Generic type instantiation and unification

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
- [x] Case/when type narrowing (Phase 97)

### Completed Overload Resolution
- [x] Method overload resolution based on argument types (Phase 98)
- [x] Subtype matching: Child matches Parent parameter
- [x] Union type matching: Int32 matches Int32 | String
- [x] Specificity ranking: prefers exact match over subtype/union
- [x] Inheritance chain walking for subtype check

### Completed Virtual Types
- [x] VirtualType class for polymorphic dispatch (Phase 99)
- [x] Method lookup in virtual type (base + subclass overrides)
- [x] Subtype matching with VirtualType
- [x] Deep inheritance chain method resolution

### Completed Macro Integration
- [x] Macro expansion in semantic phase (already working via SymbolCollector)
- [x] Compiler flags support in LSP (`LSP_COMPILER_FLAGS` env, `compiler_flags` config)
- [x] Custom flags propagation to MacroExpander via Context
- [x] Runtime.target_flags for platform detection (darwin, linux, x86_64, etc.)

### Completed Generic Type System
- [x] Generic class instantiation (Box(T) → Box(Int32))
- [x] Generic method type inference (def identity(x : T) : T)
- [x] Multiple type parameters (def pair(a : T, b : U) : T)
- [x] Generic class arguments (def unwrap(box : Box(T)) : T)
- [x] Chained generic method calls
- [x] Type parameter substitution in return types
- [x] Nested generics: Container(Box(Array(Int32))) fully supported
- [x] Array/Hash of generic types: Array(Box(Int32)), Hash(String, Box(T))

### Completed Extended Type Inference (Phase 103A-C)
- [x] TypeDeclarationNode handling (x : Type = value)
- [x] All numeric types (Int8-128, UInt8-128, Float32/64)
- [x] Nilable type syntax (T? → T | Nil)
- [x] Union type methods: not_nil!, nil?, try with blocks
- [x] Block inference for map/collect/each with element types
- [x] Smart compact: Array(T | Nil) → Array(T)
- [x] Smart flatten: Array(Array(T)) → Array(T)
- [x] Short block form (&.method) proper handling

---

## 4. LSP Server Correctness - COMPLETE

Goal: v2 LSP must report only real errors and match original compiler behavior.

**26 LSP Methods Implemented:**
- General: initialize, shutdown
- Sync: didOpen, didChange, didClose, didChangeWatchedFiles
- Language: hover, definition, typeDefinition, completion, signatureHelp, documentSymbol, references, documentHighlight, rename, prepareRename, codeAction, formatting, rangeFormatting, foldingRange, semanticTokens/full, inlayHint
- Workspace: symbol
- Call Hierarchy: prepare, incomingCalls, outgoingCalls

**Not Yet Implemented (lower priority):**
- textDocument/declaration, textDocument/implementation
- textDocument/codeLens, codeLens/resolve
- textDocument/documentLink
- textDocument/onTypeFormatting
- textDocument/selectionRange
- workspace/executeCommand

- [x] Wired to v2 parser and symbol collector
- [x] Diagnostics parity: fixed []? vs ternary disambiguation (no false positives on server.cr)
  - Fixed `&.[expr]?` block shorthand pattern
  - Fixed `h ? h[x]? : nil` ([]? inside ternary then-branch)
  - Fixed `ENV["X"]? ? 25 : 0` ([]? followed by ternary)
- [x] Types & hover accuracy: stdlib types (Array, String, Hash, Int32, Float64), array element access (arr[0] → Atom)
- [x] Navigation to stdlib/prelude symbols (Time, File, etc.)
- [x] Prelude handling with cache + mtime tracking (cached summaries/types for warm start)

### Tests - COMPLETE
- [x] Structured LSP tests for stdlib symbols (`Time.now`, `File.basename`, array types, etc.) - see `stdlib_hover_spec.cr`, `stdlib_navigation_spec.cr`
- [x] Diff v2 diagnostics against original compiler on representative files (0 false positives on 30 files)
- [x] Hover/definition regression spec covering cached types across required files - see `cached_types_cross_file_spec.cr`
- [x] Integration specs for hover/definition sequences (single-file path regression)
- [x] Integration specs for references via server across VirtualArena requires
- [x] Diagnostics spec with semantic diagnostics enabled (semantic error guard)
- [x] Inlay hints end-to-end on a small program (positions/labels)
- [x] Semantic tokens integration: require strings stay strings (no enumMember); symbol literals remain single full-span token
- [x] Integration specs for hover/definition covering stdlib/prelude and indexing-in-progress guard
- [x] Integration specs for rename via server across VirtualArena requires
- [x] Guard hover/definition when indexing in progress (soft-fail)
- [x] Rename guard for stdlib/prelude symbols (no-op or error)
- [x] VSCode extension: request/response log channel and "Indexing…" status indicator in UI
- [x] Navigation to stdlib/prelude (tests + impl)
- [x] Folding ranges for begin/rescue/else/ensure without overfolding; semantic tokens for symbol literals fixed
- [x] Regression scenarios via `tools/lsp_scripts` - rename, stdlib, hover→definition chains, nested consts, class/instance vars
- [x] LSP spec coverage for member access typed via locals/arrays (`cas`, `a.sigma`, class vars) - see `stdlib_hover_spec.cr`

### GitHub Issues Fixed (2025-12-09)
- [x] #2: Go to Definition returns name-only range for F12 looping
- [x] #3: Go to Definition in ternary if (variables + method calls)
- [x] #4: Hover over comments shows parent tooltip (suppressed)
- [x] #5: Class methods in completion for `MyClass.` (uses class_scope)

---

## LSP Project Cache - COMPLETE
- [x] Versioned project cache (v2) with symbol summaries (classes/modules/method signatures) + real mtime
- [x] Background indexing of `root/src/**/*.cr` to populate cache automatically
- [x] Extend summaries with ivars/class vars/consts (class vars and constants now collected from class_scope)
- [x] Restore symbol_table from cache for unchanged files; avoid re-parse/resolve when mtime matches (spans placeholder)
- [x] Merge cached project symbols into analysis to avoid reloading requires on warm didOpen
- [x] Cache and restore symbol spans and inferred types in summaries (cache version v3); expose cached types for hover/definition fallback
- [x] Mark cached files (`from_cache`) and use summaries for hover/definition when AST is missing
- [x] Strict cache validation (version/root hash/mtime) with full reparse fallback
- [x] Extend summaries with ivars/class vars/consts and richer type info; reuse same pipeline for prelude
- [x] Make cache/inference idempotent: if infer times out, resume later and backfill tables in background fibers
- [x] Apply rich cache pipeline to prelude: spans/types/ivars/class vars, rebuild prelude symbol_table from cache without full parse when unchanged
- [x] AST cache version bump to invalidate stale roots (fixes top-level def leakage in codegen) (2025-12-24)

---

## TypeIndex Integration - COMPLETE

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

## Type Inference Performance Optimizations - COMPLETE

### Completed (2025-12-08)
- [x] **Large array sampling**: For arrays >10 elements, sample first 3 and use uniform type if all same PrimitiveType
- [x] **Large hash sampling**: Same optimization for Hash literals >10 entries
- [x] **Lazy debug evaluation**: Wrap debug() with @debug_enabled check to avoid string allocations in hot paths
- [x] **Binary SymbolSummary**: Replace JSON with binary serialization (52% faster cache rebuild)

**Results:**
| File | Before | After | Improvement |
|------|--------|-------|-------------|
| dragonbox_cache.cr | 70ms | 51ms | 27% |
| enumerable.cr | 16ms | 13ms | 19% |
| SymbolTable rebuild | 240ms | 115ms | 52% |

### Completed (2025-12-09)
- [x] **Type cache warming**: Background prelude loading on LSP start (pre-populates Int32, String, Array, Hash, etc.)
- [x] **Method body lazy inference**: DefNode not in children_of - method bodies inferred on-demand
- [x] **Incremental inference**: Implemented in `unified_project.cr`:
  - Dependency graph (`dependencies`/`dependents` hashes)
  - Symbol invalidation (`invalidate_file_symbols`)
  - Dirty file tracking (`dirty_files` set)
  - Incremental reanalysis (`reanalyze_dirty`)
  - Per-file state with versions and mtime

### Experiments & Findings (2025-12-08)
- [x] **Wave-based parallel parsing**: Tested parsing files in parallel using fibers
  - Crystal fibers = cooperative concurrency (single-threaded), NOT parallel threads
  - Fiber spawn overhead negates any benefit
  - Result: ~11% slower than sequential (2860ms vs 2577ms)
  - **Conclusion**: True parallelism requires `-Dpreview_mt` or external multi-processing

### Future Optimizations (Low Priority)
- [ ] **MT parallel parsing**: Use `-Dpreview_mt` for true multi-threaded file parsing (requires thread-safe AST arena)
- [ ] **Stdlib precompilation**: Ship pre-computed cache with LSP binary (premature - cache already auto-generates)
- [ ] **Arena pre-allocation**: Pre-allocate memory for common type structures (micro-optimization)
- [ ] **String interning in types**: Intern type names to reduce memory
- [ ] **Batch watchdog checks**: Check every N iterations instead of every node
- [ ] **SIMD type comparison**: Vectorize type equality checks for unions

---

## 5. Codegen: Multi-Stage Compilation with Hybrid Memory Management

**Status:** Active development on `codegen` branch (2025-12-12)

### Completed Milestones (2025-12-12)
- [x] **M1.1** HIR data structures (`src/compiler/hir/hir.cr`) - 87 tests
- [x] **M1.2** AST → HIR lowering (`src/compiler/hir/ast_to_hir.cr`) - 87 tests
- [x] **M2.1** Basic escape analysis (`src/compiler/hir/escape_analysis.cr`) - 16 tests
- [x] **M2.3** Taint propagation (`src/compiler/hir/taint_analysis.cr`) - 17 tests
- [x] **M2.4** Memory strategy integration (`src/compiler/hir/memory_strategy.cr`) - 15 tests
- [x] **M3.1** MIR data structures (`src/compiler/mir/mir.cr`) - 20 tests
- [x] **M3.1b** MIR optimizations (`src/compiler/mir/optimizations.cr`) - 45 tests
  - RC elision (remove redundant rc_inc/rc_dec pairs)
  - Dead code elimination
  - Constant folding
  - Copy propagation (algebraic identities, store→load forwarding)
  - Local CSE (pure ops within a block)
  - Peephole simplifications (no-op casts, constant branches)
- [x] **M3.2** HIR → MIR lowering (`src/compiler/mir/hir_to_mir.cr`) - 19 tests
  - Full HIR to MIR SSA transformation
  - Memory strategy assignment based on escape/taint analysis
  - Automatic RC insertion for ARC allocations
- [x] **M3.2b** Profile infrastructure (`src/compiler/mir/profile.cr`) - 46 tests
  - AllocationSiteStats, BranchStats, LoopStats, CallSiteStats, BlockStats
  - Binary serialization (CRPF v3 format)
  - ProfileGuidedOptimizer, CompilerFlags (--mm=profile-gen/use)
- [x] **M3.3** Profile-Guided Optimizations (`src/compiler/mir/pgo_passes.cr`) - 26 tests
  - DevirtualizationPass: converts hot virtual calls to guarded direct calls
  - CrossFunctionRCElisionPass: elides RC ops across function boundaries
  - MemoryStrategyRefinementPass: adjusts memory strategies based on profile
  - PGOPipeline: coordinates all passes with aggregated statistics

**Test Coverage:** 307 new tests (155 HIR + 152 MIR)

**Architecture (Quadrumvirate-analyzed):**
```
┌─────────────────────────────────────────────────────────────────────┐
│                    MULTI-STAGE COMPILATION                          │
├─────────────────────────────────────────────────────────────────────┤
│                                                                     │
│  Phase 1: AST → Extended IR                                         │
│  ┌─────────────────────────────────────────────────────────────┐   │
│  │  • Escape analysis (does value leave scope?)                │   │
│  │  • Alias analysis (who else references this?)               │   │
│  │  • Taint propagation:                                       │   │
│  │    - thread-shared (needs atomic ops)                       │   │
│  │    - ffi-exposed (C code may hold reference)                │   │
│  │    - cyclic (participates in reference cycle)               │   │
│  │  • Lifetime annotations on IR nodes                         │   │
│  └─────────────────────────────────────────────────────────────┘   │
│                              │                                      │
│                              ▼                                      │
│  Phase 2: IR Optimization + MM Assignment                           │
│  ┌─────────────────────────────────────────────────────────────┐   │
│  │  Per-object memory strategy assignment:                     │   │
│  │  • Stack: no escape, small, known size                      │   │
│  │  • Slab/Arena: no escape, dynamic size, fiber-local         │   │
│  │  • ARC: escapes, single/few owners, no cycles               │   │
│  │  • GC: cycles detected, FFI boundary, fallback              │   │
│  │                                                             │   │
│  │  Cycle detection → automatic GC marking                     │   │
│  │  Profile-guided hints (optional)                            │   │
│  └─────────────────────────────────────────────────────────────┘   │
│                              │                                      │
│                              ▼                                      │
│  Phase 3: IR → LLVM BC → Machine Code                               │
│  ┌─────────────────────────────────────────────────────────────┐   │
│  │  • Generate LLVM IR with appropriate alloc/dealloc calls    │   │
│  │  • LLVM handles low-level optimizations                     │   │
│  │  • Output: native binary for target platform                │   │
│  └─────────────────────────────────────────────────────────────┘   │
│                                                                     │
└─────────────────────────────────────────────────────────────────────┘
```

**Compiler Flags (MM mode hints):**
- `--mm=conservative` → prefer GC, maximize safety
- `--mm=balanced` → auto-infer optimal strategy (default)
- `--mm=aggressive` → prefer stack/ARC, maximize speed
- `--mm=profile` → use runtime profile data for decisions

**Reference Implementations:**
| Compiler | Approach | What We Learn |
|----------|----------|---------------|
| Swift | ARC + escape analysis on SIL | ARC works, but overhead exists |
| Rust | Borrow checker on MIR | Too complex for Crystal's goals |
| V lang | Autofree on AST | Works 80%, leaks on graphs |
| Lobster | Compile-time RC | Good for games, limited scope |
| Koka | Perceus reuse analysis | Academic, elegant, complex |
| Zig | Manual + comptime | Fast compile, good reference |

---

### 5.1 Phase 1: Extended IR with Lifetime Annotations

**Goal:** Transform AST into IR that carries lifetime and ownership information.

#### 5.1.1 IR Design
- [ ] Define Extended IR node types (EIR)
- [ ] Map AST nodes to EIR nodes
- [ ] Add lifetime annotation slots to EIR
- [ ] Control flow graph (CFG) construction
- [ ] SSA form conversion (optional, evaluate need)

#### 5.1.2 Escape Analysis
- [ ] Intra-procedural escape analysis
- [ ] Track: stack-local, heap-escape, argument-escape, return-escape
- [ ] Handle closures (captured variables escape)
- [ ] Handle `array << obj` (container escape)
- [ ] Handle virtual dispatch (conservative for polymorphic calls)

**Critical cases (from ADVERSARY):**
```crystal
# Case 1: Closure capture
def make_counter
  x = 0
  -> { x += 1 }  # x escapes via closure!
end

# Case 2: Container escape
arr = [] of Foo
arr << Foo.new  # Foo lifetime tied to arr

# Case 3: Return escape
def create
  Foo.new  # escapes to caller
end
```

#### 5.1.2a Escape Analysis Robustness (edge cases)
- [x] Refine virtual-call detection: treat final/struct/monomorphic receivers as non-virtual; avoid blanket HeapEscape on method calls (2025-12-31).
- [x] Method effect summaries: cache per-signature effects (`no_escape`, `transfer`, `thread_shared`, `ffi_exposed`, `returns_alias`) to replace name-based heuristics (2025-12-31).
- [x] Apply effect summaries during Call handling (escape/taint honoring `NoEscape`/`Transfer`/`ThreadShared`/`FFIExposed`) (2025-12-31).
- [x] Unknown-effect boundary: limit propagation and pick safe local strategy without poisoning the full escape/taint graph (2025-12-31).
- [ ] Add stdlib-only annotations: `@[NoEscape]`, `@[Transfer]`, `@[Taints(...)]`, `@[Arena("name")]` to override heuristics (Taints support added; Arena pending; Array/Hash/Set/Channel/Deque/SmallDeque/PointerLinkedList/Thread::LinkedList/PointerPairingHeap/Once::Operation partial coverage done).
- [ ] Builder/borrow region: tie child lifetimes to owner; only escape when owner escapes.
- [ ] Closure capture in loops: copy/move captured loop vars when closure escapes (avoid last-iteration capture/UAF).
- [ ] Any/Union boundary: treat as analysis boundary; force ARC/GC or slab to avoid stack UAF.
- [ ] `--no-gc` diagnostics: report allocation site + reason (cycle/ffi/thread_shared).

#### 5.1.3 Alias Analysis
- [ ] Region-based alias analysis
- [ ] Track pointer aliasing
- [ ] Handle instance variables (@ivar may alias)
- [ ] Handle array/hash element aliasing

#### 5.1.4 Taint Propagation
- [ ] `thread-shared` taint (needs atomic RC or GC)
- [ ] `ffi-exposed` taint (C may hold reference)
- [ ] `cyclic` taint (participates in reference cycle)
- [ ] Propagate taints through assignments and calls

---

### 5.2 Phase 2: Memory Management Assignment

**Goal:** Assign optimal MM strategy per allocation site.

#### 5.2.1 Strategy Selector
- [ ] Implement decision tree based on analysis results
- [ ] Stack: !escapes && size_known && size < threshold
- [ ] Slab: !escapes && fiber_local && dynamic_size
- [ ] ARC: escapes && !cyclic && !thread_shared
- [ ] GC: cyclic || thread_shared || ffi_exposed || fallback

#### 5.2.2 Cycle Detection
- [ ] Static cycle detection in type graph (recursive types)
- [ ] Mark types that CAN form cycles
- [ ] Conservative: if cycle possible → GC or weak refs
- [ ] Annotation: `@[Acyclic]` for user override

**Cyclic type example:**
```crystal
class Node
  property next : Node?  # Can form cycle!
end
```

#### 5.2.3 ARC Implementation
- [ ] Reference count field layout
- [ ] Atomic vs non-atomic RC (based on thread_shared taint)
- [ ] RC increment/decrement insertion
- [ ] Weak reference support for breaking cycles

#### 5.2.4 Slab/Arena Allocator
- [ ] Fiber-local arena design
- [ ] Bulk deallocation on fiber exit
- [ ] Arena size heuristics
- [ ] Overflow to heap fallback

#### 5.2.5 Profile-Guided Optimization (M3.3)

**Quadrumvirate Analysis (2025-12-09):**

The key insight is: **Don't compete with LLVM, complement it.**
- LLVM already handles: branch layout, loop unrolling, basic inlining
- Crystal should focus on: ARC semantics, type-based devirtualization, memory strategy

**Crystal-Specific PGO Stack:**
```
┌─────────────────────────────────────────────────────────────┐
│                    CRYSTAL PGO (M3.3)                       │
├─────────────────────────────────────────────────────────────┤
│  1. Profile-Guided Devirtualization                         │
│     - dominant_target → guarded direct call                 │
│     - Enables inlining of hot virtual calls                 │
│                                                             │
│  2. Profile-Guided ARC Optimization                         │
│     - Cross-function RC elision based on call patterns      │
│     - Owned/borrowed inference from escape frequency        │
│     - Elide RC for "always escapes" or "never escapes"      │
│                                                             │
│  3. Profile-Guided Memory Strategy                          │
│     - Refine Stack/Slab/ARC/GC based on actual lifetime     │
│     - Slab pool sizing from allocation patterns             │
│     - Arena reset points from deallocation clustering       │
│                                                             │
│  4. Profile-Guided Specialization (future)                  │
│     - Clone hot functions for dominant type combinations    │
│     - Monomorphize generics that are 95%+ one type          │
│                                                             │
└─────────────────────────────────────────────────────────────┘
                           ↓
┌─────────────────────────────────────────────────────────────┐
│                    LLVM PGO (EXISTING)                      │
├─────────────────────────────────────────────────────────────┤
│  - Branch probability → block layout                        │
│  - Loop trip counts → unrolling                             │
│  - Call counts → inlining decisions                         │
│  - Hot/cold → function splitting                            │
└─────────────────────────────────────────────────────────────┘
```

**Implementation Status:**
- [x] Profile data structures (AllocationSiteStats, BranchStats, LoopStats, CallSiteStats, BlockStats)
- [x] Binary serialization (CRPF v3)
- [x] ProfileInstrumentationPass
- [x] CompilerFlags (--mm=profile-gen/use)
- [x] **M3.3a** DevirtualizationPass - guarded direct calls from dominant_target (26 tests)
- [x] **M3.3b** CrossFunctionRCElisionPass - elide RC across call boundaries (26 tests)
- [x] **M3.3c** MemoryStrategyRefinementPass - update strategy from profile data (26 tests)
- [x] PGOPipeline - coordinates all passes, aggregates statistics

**Priority (Impact × Uniqueness):**
| # | Pass | Impact | Uniqueness | Status |
|---|------|--------|------------|--------|
| 1 | Devirtualization | HIGH | HIGH | ✅ |
| 2 | ARC Cross-function | HIGH | UNIQUE | ✅ |
| 3 | Memory Strategy Refinement | MED | HIGH | ✅ |

---

### 5.3 Phase 3: LLVM Backend

**Goal:** Generate LLVM IR from optimized MIR, produce native code.

#### 5.3.1 LLVM IR Generation (M4.1)
- [x] MIR Type system (TypeKind, Type, Field, TypeRegistry)
- [x] LLVMTypeMapper (MIR types → LLVM IR type names)
- [x] LLVMIRGenerator (text-based LLVM IR output)
- [x] Function codegen (params, blocks, instructions, terminators)
- [x] Control flow translation (branch, jump, switch, phi)
- [x] Memory operation codegen (alloc, free, load, store, gep)
- [x] Memory strategy support (Stack, Slab, ARC, AtomicARC, GC)
- [x] RC operations (rc_inc, rc_dec with destructor)
- [x] Binary/unary ops, casts, calls
- [x] 24 spec tests passing

#### 5.3.2 Debug DX: Type Metadata for LLDB/DAP
- [x] TypeInfoEntry, FieldInfoEntry structures
- [x] __crystal_type_info global array generation
- [x] __crystal_field_info global array generation
- [x] __crystal_type_strings string table
- [x] Design doc: docs/debug_dx_design.md
- [x] LLDB Python formatters (tools/lldb/crystal_formatters.py)
  - CrystalObjectProvider, CrystalClosureProvider, CrystalUnionProvider
  - `crystal types` and `crystal type` commands
  - 13 unit tests
- [x] DAP server integration (tools/lldb/crystal_dap.py)
  - CrystalDAPExtension for enhanced variable display
  - VS Code launch.json generator
  - .lldbinit generator

#### 5.3.3 Runtime Support
- [x] Minimal runtime library (src/runtime/)
  - memory.cr: malloc/realloc/free wrappers with stats
  - arc.cr: Reference counting (rc_inc, rc_dec, arc_alloc)
  - slab.cr: Size-class based allocator (malloc fallback for now)
  - 43 runtime specs
- [ ] GC integration (Boehm as baseline)
- [ ] Arena allocator runtime
- [ ] Full slab allocator (with actual slab pooling)

#### 5.3.3 Optimization Pipeline
- [x] LLVM optimization passes (O0/O1/O2/O3)
- [x] LTO support for release builds (clang link path)
- [x] PGO hooks (profile-guided LLVM opts via clang flags)

#### Post-Bootstrap Optimizations (defer until full-prelude bootstrap works)
- [x] Algebraic simplifications in MIR (x + 0, x * 1, x * 0, x | 0, x & -1)
- [x] Extend constant folding to UInt64 and Bool ops (comparisons + bitwise)
- [x] Local store→load forwarding in a block for no_alias (no full alias analysis)
- [x] Copy propagation: real def-use replacement beyond cast/select/phi (cross-block where safe)
- [x] Local CSE for pure ops (arith/compare/bitcast/gep) within a block
- [x] Peephole simplifications: redundant casts, constant-branch to jump, phi with identical incoming

#### 5.3.4 Platform Support
- [ ] macOS (arm64, x86_64)
- [ ] Linux (arm64, x86_64)
- [ ] Windows (x86_64)
- [ ] Cross-compilation support

#### 5.3.5 Immediate Validation & Hardening
- [x] **Structural NoAlias Analysis** (2025-12-11): Paradigm shift from ultra-conservative to allocation-site based.
  - Track allocation sites through Load/GEP chains
  - Track escaped allocations (stored to field/container)
  - Replace "Store clears ALL" with targeted may_alias query
  - NoAlias(a,b) := different_alloc_site ∧ ¬escaped(a) ∧ ¬escaped(b)
  - 5 new tests
- [x] **TBAA (Type-Based Alias Analysis)** (2025-12-11): Primitive types cannot alias reference types.
  - TypeRef: primitive?, reference?, numeric?, may_alias_type?
  - Int32* cannot alias MyClass* → enables RC elision across incompatible stores
  - 6 new tests, 28 total optimization tests passing
- [x] **Refined Cycle Detection** (2025-12-11): Collections only cyclic if element type is cyclic.
  - Array(Int32) → NOT cyclic (was: all Arrays marked cyclic)
  - Array(Node) → cyclic only if Node is cyclic
  - extract_generic_params() parses generic type parameters
  - PRIMITIVE_TYPES set for types that cannot form cycles
  - 6 new tests, 23 total taint analysis tests
- [x] Guarded devirtualization safety specs: ensure fallback when profile misses a type (switch/if coverage).
- [x] ABI sanity harness: golden tests for class/struct/union layout (offset/align/payload), union header, vtable layout (if present).
- [x] Inline intrinsics RC/taint audit: propagate lifetime/taints through inlined .times/.each/Range; re-evaluate captured vars post-inline.
  - Yield inlining now threads caller locals through loop phis and preserves phi-bound locals across iterations.
  - Spec: `spec/hir/inline_yield_spec.cr`
- [x] ThreadShared propagation → atomic RC or GC fallback for closures/objects crossing fiber boundaries; add spec (spawn block captures covered).
- [x] Arena/slab frame experiment: prolog/epilog frame for no-escape functions (behind flag).
- [x] LTP/WBA optimization framework implemented (2025-12-11): 4-component potential, Window/Corridor tracking, legal moves.
- [x] **Tests green after recent hardening** (2025-12-10): all specs passing (9 pending intentional) after fixing yield/puts/array/lifetime, empty hash inference, struct LLVM type mapping, and stabilizing pipeline.
- [x] **Next:** cycle detection (generic element types) (2025-12-26)
- [x] ABI harness (offset/align/union) (2025-12-26)
- [x] RC/taint inline audit
- [x] ThreadShared→atomic/GC enforcement
- [ ] Benchmark LavinMQ (post-bootstrap): compare v2 vs official compiler on compile time, binary size, and runtime perf

---

### Build DX (Codegen)
- [x] CLI flags for faster iteration: `--no-llvm-opt` and `--no-link` (2025-12-23)
- [x] LLVM opt/llc artifact cache keyed by ll hash + flags (2025-12-23)
- [x] AST cache key stabilized (FNV hash) and verified hits on warm run (parse ~164ms → ~79ms)
- [x] Fix AST cache save failures (ClassNode→StructNode, SplatNode→Unary) seen in verbose compile logs (2025-12-20)
- [x] Optimize macro-literal require scanning (linear scan, avoids String#index O(n^2)); prelude parse now ~47ms on bootstrap_array (2025-12-21)
- [x] Add `--no-llvm-metadata` to skip type metadata (small LLVM time reduction)
- [x] Reachability roots include `__crystal_main` (avoid emitting all funcs; LLVM ≈ 0.35s on /tmp/cv2_smoke.cr)
- [x] Investigate release compile latency on small programs (43s on /tmp/cv2_smoke.cr); add per-phase timing + cache hit diagnostics (2025-12-20)
  - Current: `--no-prelude` ≈ 16ms total; with prelude HIR ≈ 0.11s, MIR ≈ 0.1ms, LLVM ≈ 1.3ms, total ≈ 0.22s (lazy HIR lowering + reachability)
  - Current (release + caches): `./bin/crystal_v2 --release --stats --no-link /tmp/cv2_smoke.cr` total ≈ 188ms, opt ≈ 0.1ms, llc ≈ 21.5ms
  - Current (release + caches, latest): `./bin/crystal_v2 --stats --no-link /tmp/cv2_smoke.cr` total ≈ 132ms, hir_reach=9, mir_funcs=9
  - Added `hir_funcs` / `hir_reach` / `mir_funcs` counts to --stats output (cv2_smoke: 915 / 8 / 8)
- [x] Validate lazy HIR lowering for dynamic dispatch (virtual calls / module mixins) to avoid pruning needed methods (2025-12-20)
  - Virtual calls now expand reachability by base method name; spec covers HIR reachability for virtual calls.
- [x] Lazy monomorphization flush by default to avoid prelude stalls; set `CRYSTAL_V2_EAGER_MONO=1` to restore eager behavior (2025-12-27)
- [x] HIR lowering for `spawn` keyword via synthetic `spawn { ... }` call (2025-12-27)

### GC Minimization (DX / Bootstrap)
- [x] Wire CLI/driver `--mm=conservative|balanced|aggressive` to HIR MemoryConfig.
- [x] Add `--mm-stack-threshold` tuning + `--no-gc` diagnostic mode (fail on GC allocations).
- [x] Report memory strategy totals in `--stats` output.
- [x] Reduce false-positive GC via taint refinement (thread_shared / ffi_exposed) + specs.
- [x] Optional: type-info-backed cycle detection + `@[Acyclic]` override.

### 5.3.6 LTP/WBA Optimization Framework

**Status:** 🔧 WIP (2025-12-11)

**Theory:** LTP (Local Trigger → Transport → Potential) is a unifying descent framework where:
- **Trigger (BR-1):** Every non-optimal configuration admits a detectable local window W
- **Transport (BR-2):** From W starts a corridor that exits boundary or triggers alternative frame
- **Potential (BR-3):** Well-founded lexicographic Φ strictly decreases under every legal move
- **Dual Frame (BR-4):** If progress stalls, switch to certified alternative analysis
- **Finiteness (BR-5):** No infinite descending chains; process terminates

**Legal Moves:**
- **Spike:** Length-2 cancellation (rc_inc + rc_dec pair elision)
- **Ladder:** Short corridor elimination (single-use intermediates)
- **Diamond:** Confluent resolution of critical pairs (choose better Φ decrease)
- **Collapse:** Removal of redundant instruction while lowering Φ (DCE)

**Current vs Target:**

| Component | Current | Target |
|-----------|---------|--------|
| Potential | `(rc_ops, insts, unsafe)` | `(I, -M, P, area)` 4-component |
| Window/Trigger | Implicit (any rc_inc) | Explicit max-exposure window |
| Transport | Primitive Load alias | Def-use corridor tracing |
| Dual Frame | None | Escape analysis fallback |
| Moves | Spike + Collapse | Spike + Ladder + Diamond + Collapse |

#### Implementation Tasks:

**Phase 1: Enhanced Potential (Φ′)**
- [x] Add window metrics (overlap/tie-plateau/corner-mismatch) via `LTPPotential`
- [x] Implement `find_window()` to select max-exposure trigger instruction
- [x] Update potential to 4-component `(I, -M, P, area)`
- [x] Implement lexicographic comparison for new potential

**Phase 2: Window & Corridor (BR-1, BR-2)**
- [x] Implement `Window` struct representing a boundary cell (instruction + context)
- [x] Implement `Corridor` struct for def-use chain from trigger to terminator
- [x] Add `trace_corridor(window)` to follow value through uses
- [x] Corridor exits: boundary (func return), escape (call arg), or alternative frame

**Phase 3: Legal Moves Library**
- [x] **Spike move:** rc_inc/rc_dec pair cancellation (existing, enhance)
  - Track must-alias for safe elision
  - Decrease: ΔI or Δ(-M) if tie-breaker
- [x] **Ladder move:** Short corridor elimination
  - If rc_inc → single_use → rc_dec, remove middle
  - Decrease: ΔP (corner mismatch)
- [x] **Diamond move:** Confluent critical pair resolution
  - When two moves conflict, compute Φ for both, choose lower
  - Decrease: ΔP or Δarea
- [x] **Collapse move:** Redundant instruction removal (DCE)
  - Decrease: Δarea only (I, M, P fixed)

**Phase 4: Dual Frame Fallback (BR-4)**
- [x] Detect "stuck" state: no legal move decreases Φ
- [x] Switch to escape analysis frame (initial: constant-folding fallback)
- [x] If escape frame also stuck, switch to curvature/lifetime frame
  - [x] Add corridor-length "curvature" metric (sum/max path length) to guide the frame
  - [x] Add lifetime-pressure metric (distance between rc_inc/rc_dec along def-use)
  - [x] Implement curvature/lifetime frame pass (RC elision + DCE gated by metrics)
  - [x] Add specs for curvature frame fallback (monotone descent across frames)
- [x] Unified potential across frames (Φ_esc compatible with Φ_primary)
  - [x] Define frame-normalized LTPPotential mapping (same 4 components)
  - [x] Reject frame switch if mapped Φ does not decrease
  - [x] Spec: cross-frame monotone descent with mixed moves

**Phase 5: L2-Engine Scheduler**
- [x] Priority: S ≻ L ≻ D ≻ C (Spike > Ladder > Diamond > Collapse)
- [x] Main loop: find window → trace corridor → apply best move → recompute Φ
- [x] Termination: Φ stops decreasing or area = 0
- [x] Logging: emit move sequence for debugging

**Phase 6: Integration & Testing**
- [x] Replace `optimize_with_potential` with LTP engine (returns LTPPotential; LTP run after legacy loop)
- [x] Add specs for each move type
- [x] Add specs for dual-frame fallback
- [x] Benchmark: compare old vs new on bootstrap examples
  - Script: `scripts/bench_ltp.sh` (uses `--no-ltp` for baseline)
  - Result (29 bootstrap files, `--no-link --no-llvm-opt`):
    - LTP avg total ≈ 157.1ms, baseline avg total ≈ 139.3ms (Δ ≈ +17.8ms)
    - LTP avg mir_opt ≈ 0.100ms, baseline ≈ 0.003ms
- [x] Verify monotone descent property

**Files to modify:**
- `src/compiler/mir/optimizations.cr` - Main LTP implementation
- `src/compiler/mir/mir.cr` - Add Window/Corridor types if needed
- `spec/compiler/mir/ltp_wba_spec.cr` - New test file

---

### 5.4 Alternative Backends (Future)

- [ ] **WebAssembly**: Direct WASM emitter (no LLVM)
- [ ] **eBPF**: Kernel/tracing use cases
- [ ] **Cranelift**: Fast debug builds (like Rust)

---

### 5.5 Backward Compatibility

**Principle:** Existing Crystal code must work without changes.

- [ ] GC as default for `--mm=conservative`
- [ ] Gradual opt-in to aggressive MM
- [ ] Stdlib compatibility (written for GC)
- [ ] No required lifetime annotations (unlike Rust)

---

### Critical Risks (ADVERSARY Analysis)

| Risk | Impact | Mitigation |
|------|--------|------------|
| Closure escape not detected | Use-after-free | Conservative: closures → GC |
| Cycle not detected | Memory leak | Type graph analysis + weak refs |
| FFI boundary | Dangling pointer | `@[FFI]` annotation → GC |
| Thread safety | Data race | thread_shared taint → atomic RC |
| ABI between MM modes | Crashes | Unified object header layout |

---

### Milestones

| Milestone | Description | Status | Tests |
|-----------|-------------|--------|-------|
| M1.1 | HIR data structures | ✅ Complete | 87 |
| M1.2 | AST → HIR lowering | ✅ Complete | 87 |
| M2.1 | Escape analysis | ✅ Complete | 16 |
| M2.3 | Taint propagation | ✅ Complete | 17 |
| M2.4 | Memory strategy | ✅ Complete | 15 |
| M3.1 | MIR data structures | ✅ Complete | 20 |
| M3.1b | MIR optimizations | ✅ Complete | 17 |
| M3.2 | HIR → MIR lowering | ✅ Complete | 19 |
| M3.2b | Profile infrastructure | ✅ Complete | 46 |
| M3.3 | Profile-Guided Optimizations | ✅ Complete | 26 |
| M4.1 | LLVM IR generation | ✅ Complete | 24 |
| M4.1b | Debug DX (type metadata) | ✅ Complete | 24 |
| M4.1c | LLDB/DAP tooling | ✅ Complete | 13 (py) |
| M4.2 | Runtime library | ✅ Complete | 43 |
| M4.3 | End-to-end compile | 🔧 In Progress | 27+ bootstrap |

**M4.3 Bootstrap Progress (2025-12-18):**
- Basic codegen fully working (unions, nil?, not_nil!, conditionals, loops)
- Namespace resolution for nested structs/classes in modules
- Getter/setter monomorphization for generics
- Stdlib compilation blocked on: typeof in types, generic blocks, module mixins
- Yield inlining: removed Slice#fetch skip; block missing symbols now 0 in `/tmp/bootstrap_array_full.link.log` (2026-01-xx)

---

## 6. Follow-up (Post-LSP Stability)

- [ ] Semantic service/API for agents (structured queries)
- [ ] Structural patch layer (rename/extract/move with validation)
- [ ] Zero-copy name handling (interning, span-based lookups)
- [ ] JVM backend (experimental)
- [ ] Alias/region pass integrated into RC elision/stack/ARC decisions

---

## Quick Reference

| Component | Status | Tests |
|-----------|--------|-------|
| Parser | ~97.6% | 1390+1466 |
| Lexer | Complete | Part of parser tests |
| AST | Complete | Class inheritance done |
| MacroExpander | ~99% | Full @type API + annotations + typeof/sizeof/alignof |
| Type Inference | ~99% | Full generics + flow typing + blocks + unions (Phase 103A-C) |
| LSP Server | Complete | 26 methods, 4 GitHub issues fixed |
| TypeIndex | Complete | 5.6x faster than JSON, per-file partitioning |
| Performance | Complete | Incremental inference, lazy method bodies, cache warming |
| HIR | Complete | 155 tests (data structures, lowering, escape, taint, memory strategy) |
| MIR | Complete | 128 tests (SSA form, memory ops, optimizations, PGO passes) |
| Codegen | 75% | M1-M4.2 done, basic codegen working |
| Bootstrap | ~65% | Basic codegen works, stdlib needs typeof/blocks/mixins |

---

## 7. Bootstrap Compiler (Self-Hosting Path)

**Status:** Active development on `codegen` branch (2025-12-12)

**Strategy:** Original Crystal compiles v2 compiler. Add features one by one until v2 can compile itself.

### 7.1 Completed Features

| Feature | Tests | Notes |
|---------|-------|-------|
| Basic class with @ivars | ✅ | bootstrap_test1/2.cr |
| Constructor (initialize, .new) | ✅ | Parameter forwarding |
| Class variables (@@var) | ✅ | bootstrap_classvar.cr |
| Union types (Int32 \| Nil) | ✅ | 5 union tests |
| is_a?, .as() for unions | ✅ | Type checking + extraction |
| If/else/unless/while | ✅ | Control flow |
| Binary/unary operations | ✅ | Arithmetic, comparison |
| puts for debugging | ✅ | Int32, Int64, String |
| **case/when** | ✅ | All variants (value, range, type, else) |
| **Blocks + yield** | ✅ | Variant C inline expansion |
| **.times intrinsic** | ✅ | Mutable vars in blocks |
| **Range#each** | ✅ | (1..3).each { \|i\| } |
| **Array literal** | ✅ | [1, 2, 3] stack-allocated |
| **Array indexing** | ✅ | arr[i] |
| **Array#each** | ✅ | arr.each { \|x\| } |
| **String literal** | ✅ | puts "hello" |
| **struct** | ✅ | Value type, stack allocation (2025-12-12) |
| **require** | ✅ | Multi-file compilation (2025-12-12) |
| **module** | ✅ | Module methods with self. prefix (2025-12-12) |
| **enum** | ✅ | Enum::Member access, .value method (2025-12-12) |
| **Hash(K,V)** | ✅ | Generic hash with [], []=, has_key? (2025-12-12) |
| **Set(T)** | ✅ | Generic set with add, includes?, size (2025-12-12) |
| **OptionParser** | ✅ | Minimal stdlib implementation (2025-12-12) |
| **Array#map** | ✅ | Compile-time unrolling for literals (2025-12-12) |
| **Array#select** | ✅ | Compile-time predicate evaluation (2025-12-12) |
| **abstract class/def** | ✅ | Skip codegen for abstract methods (2025-12-12) |

### 7.2 Bug Fixes (2025-12-12)

| Fix | Description |
|-----|-------------|
| Class reopening | Preserve ivars when class is reopened (e.g., String in hash.cr) |
| Built-in type fields | Register fields on existing MIR types (String, etc.) |
| Call return type tracking | Register return types for chained method calls |
| Index operator dispatch | Emit method calls for `[]`/`[]=` on non-array types |
| Yield function inline | Fixed mangled name lookup for yield function expansion |
| Yield method inlining (cross-file) | Inline yield-containing methods across arenas (bind receiver as `self`) |
| Block return values | Blocks now properly return values from inlined yield |
| Array literal type | Register array literals as POINTER type for indexing |
| Require directory resolution | Try `dir/dir.cr` when require path is a directory |
| Member access inheritance | Use inheritance-aware method resolution for obj.method |

### 7.3 Pending (by priority)

| Feature | Uses in v2 | Priority |
|---------|------------|----------|
| macro | 133 | LOW - NOT blocking self-host (our stdlib/code don't use macros) |
| exception stacktrace | - | LOW - debugging aid |

**Note:** Self-hosting does NOT require macros because:
- Our compiler code doesn't use `{% %}` or `{{ }}` syntax
- Our stdlib (`src/stdlib/`) is macro-free
- Dependencies (option_parser, set) have macro-free implementations

### 7.4 Self-Hosting Target Constructs (grep of v2 codebase)

```
.each:    846  ← ✅ DONE
class:    491  ← ✅ DONE
case:     471  ← ✅ DONE
yield:    182  ← ✅ DONE
struct:   169  ← ✅ DONE
require:  167  ← ✅ DONE
macro:    133  ← defer (metaprogramming)
.map:      90  ← ✅ DONE (compile-time unrolling)
.select:   ~40 ← ✅ DONE (compile-time predicate)
module:    65  ← ✅ DONE
enum:      64  ← ✅ DONE
```

---

## 8. Stage 2 Bootstrap: Full Prelude Compilation

**Status:** Active (2025-12-18) - Basic codegen working, stdlib requires advanced features

### 8.1 Completed (2025-12-18)

| Fix | Description |
|-----|-------------|
| Generic monomorphization | Prevent infinite recursion with visited sets + unresolved type detection |
| Type alias chains | Resolve LibC::ULong → UInt64 with chain resolution + depth limits |
| Pointer type caching | Fix Void*/T*/Pointer(T) returning VOID due to cache placeholder bug |
| ptr 0 → ptr null | Fix invalid LLVM IR in extern call arguments |
| bitcast to void | Convert to identity bitcast or null pointer |
| nil?/not_nil! intrinsics | Emit inline LLVM IR for nil checks on union types |
| Nil type mapping | Fix `Nil` being treated as `Void` during lowering (broke union tagging and nil? checks) |
| Union variant tagging | Use union descriptors to resolve variant ids (remove buggy overload that forced `Nil -> 0`) |
| Top-level def mangling | Lower top-level defs using mangled names (fixes mutual recursion forward refs) |
| Union function returns | Fix phi node nil detection for union return types (type_id check) |
| Union return type VOID | Fix cache placeholder bug in type_ref_for_name (set after union check) |
| not_nil! union unwrap | Use descriptor variant ids + MIR→HIR mapping (fixes `UInt8 | Nil#unsafe_chr`) (2025-12-24) |
| Namespace resolution | Register short name aliases for nested classes/structs in modules |
| Getter/setter monomorphization | Handle GetterNode/SetterNode/PropertyNode in generic class lowering |
| typeof filter | Filter out functions with unresolved typeof(...) patterns in LLVM emission |
| Module mixin expansion | Copy `include`d module instance methods into concrete classes/structs during HIR lowering |
| Varargs prototypes | Use real fixed-parameter signatures for known C varargs (`printf`, `fcntl`, etc.) |
| Varargs fixed-arg coercion | Insert ptr/int casts for fixed params to satisfy LLVM verifier (`opt -O1`) |
| Module-typed return inference | If return type is module-like (e.g., `Iterator(T)`), infer concrete return type when body is `Type.new(...)` |
| Stdlib-style combinators | Infer return types for unannotated combinators (e.g., `Iterator#with_object`) from last expression + keep function return map in sync |
| Scope-safe type resolution | Resolve unqualified type names in the current namespace before caching (avoid poisoning `type_ref_for_name` cache) |
| Yield inlining re-entry guard | Skip re-lowering when mangled names fall back to base during inlining (prevents HIR segfaults) (2025-12-25) |
| typeof in type args (locals/params) | Resolve `typeof(x)` in generic instantiations using live locals (2025-12-25) |
| Block-return generic substitution | Substitute block-return type params in generic method return types (2025-12-20) |
| Module-typed receiver resolution | Resolve module-typed locals to unique includer methods; arity+type-aware includer filtering (2025-12-20) |
| Member access default args | Apply defaults + lazy lowering for no-parens member calls (2025-12-20) |
| Generic param substitution before cache | Substitute type params before type cache lookup (fixes `T` leaking into mangled names) (2025-12-27) |
| Tuple literal type normalization | Normalize `{A, B}` to `Tuple(A, B)` in type_ref_for_name (2025-12-27) |
| Operator lazy lowering | Binary/unary operator calls now remember callsite arg types and lazy-lower targets (2025-12-27) |
| Call-site type refinement | Refine annotated base types (Array/Hash/etc.) using concrete call types (2025-12-27) |
| IndexNode lazy lowering | IndexNode now triggers lazy lowering for []/[]? calls (fix missing Slice(UInt8)#[] defs) (2025-12-30) |
| Module-typed ivar access | Lower `obj.@ivar` for module-typed receivers via includer ivars (fixes FileDescriptor timeouts) (2025-12-30) |
| Enum symbol arg coercion | Coerce symbol literals to enum values + pack double splat NamedTuple in call lowering (fixes Crystal.trace in prelude) (2025-12-31) |
| Block pass + try return | Extract &. / &block arguments as blocks; try returns block type with nilable receiver union (2026-01-03) |
| Block capture in parens | Preserve `foo(&block)` args + inline yield for block-pass blocks (2025-12-27) |
| Yield in literals | Detect yield/return inside tuple/array/hash/named tuple/string interpolation (2025-12-27) |

### 8.2 Current Status

**Basic codegen working:**
```crystal
# This compiles and runs correctly with --no-prelude:
def maybe(give : Bool) : Int32 | Nil
  if give; 42; else; nil; end
end
r1 = maybe(true)   # => 42
r2 = maybe(false)  # => nil
```

**Prelude build progress (with stdlib/prelude):**
- Reaches LLVM IR emission and `opt -O1` successfully; link still fails due to missing runtime/stdlib symbols (expected at this stage).
- Timing snapshot (release + `--stats --no-llvm-opt --no-llvm-metadata`): parse prelude ~167ms, HIR ~2.0s, MIR ~0.3ms, LLVM ~1.8ms, total ~2.2s; link failure is the current blocker.
- Linker missing symbols (bootstrap_array full-prelude run 2026-01-xx; 92 entries; full list in `/tmp/missing_symbols_latest.txt`).
  - ByteFormat decode/from_io resolved (no `_IO__ByteFormat_decode_UInt32_IO`).

**Recent fixes (prelude bootstrap path):**
- Normalize `flag?` macro arguments (strip leading `:`) + require cache v3; pthread requires now load.
- Coerce integer args to `i128` in LLVM backend for mismatch widths.
- Treat module names as `TypeKind::Module`; module-typed params refine to concrete defaults (IO::ByteFormat → SystemEndian), removing `_IO__ByteFormat_decode_UInt32_IO` (2026-01-xx).
- EventLoop interface dispatch: force EventLoop::FileDescriptor/Socket to module kind and map instance calls to Polling/IOCP/Wasi (removes EventLoop__FileDescriptor_* missing symbols) (2026-01-xx).
- Fix module class-method deferred lookup to use the module arena (prevents `Index out of bounds` in `find_module_class_def`) (2026-01-xx).
- Track enum value types for `.new`/`.value` and propagate via assignments/identifiers in HIR lowering.
- Register MacroIf/MacroLiteral nodes inside nested modules during HIR lowering.
- Remove `StructNode` handling from macro-parsed class bodies; rely on `ClassNode.is_struct` (2026-01-02).
- Handle inline returns during yield inlining (guard proc/block bodies + safe block bounds) to preserve Enumerable semantics.
- Fix inline-yield return override so `return` inside block targets caller (removes `Nil#offset`/`Nil#size` in prelude HIR) (2026-01-05).
- Recheck registered return types after lowering to avoid fallback pointer returns (fixes `Crystal::System.to_string_slice` -> `Slice(UInt8)`) (2026-01-05).
- Narrow locals for `is_a?` conditions in if/elsif branches (avoids `String#null?` in `to_string_slice`) (2026-01-08).
- Lower `is_a?` calls to intrinsic checks (UnionIs/IsA) and guard missing type args (2026-01-08).
- Lower inherited class methods via Object fallback in codegen (fixes `String.set_crystal_type_id`) (2026-01-xx).
- Fix escaped macro controls in macro bodies to avoid false `{% for %}` nesting errors (restores `Object.set_crystal_type_id`) (2026-01-xx).
- Resolve lib out-struct types via `short_type_index` guard in `type_param_like?` (fixes `DlInfo` resolution; removes `Pointer(UInt8)#dli_*` missing symbols) (2026-01-xx).
- Normalize union type names using resolved variant names (avoid namespace cache poisoning across contexts) (2026-01-xx).
- Normalize tuple literal type args inside generics (`Array({Int32, Int32})` → `Array(Tuple(Int32, Int32))`) to align mangling (2026-01-xx).
- Prefer allocator base `Class.new` when no explicit overload matches (ignore block-only `new` for no-block calls); ensures `Array(Tuple...).new` is generated (2026-01-xx).
- Infer bsearch/bsearch_index returns for unannotated methods and prefer arity-specific overloads in member access (fixes `Array(Row)#address` in LineNumbers find) (2026-01-xx).
- Preserve callsite arg types per signature and consume consistently during lazy lowering (reduces base-name collisions; missing symbols now 96) (2026-01-xx).
- Context-aware type cache keys + invalidation on module/class reopen and macro reparse output (missing symbols now 95) (2026-01-xx).
- Bump AST cache version for macro parse changes (2026-01-xx).
- Release build uses `-O2` by default (`CRYSTAL_V2_OPT_LEVEL` override) after `-O3` segfaults during deep yield inlining; root cause TBD (2026-01-xx).
- Lower inherited instance methods via parent fallback in codegen (fixes `IO::FileDescriptor#puts` resolution) (2025-12-28).
- Use array element types for `each`/`each_with_index` block params to avoid Array(T)#field fallbacks.
- Infer `find`/`find_index` return types from element types (nullable) during member access lowering.
- Guard yield inlining when callee arena mismatches (fallback to non-inline call to avoid OOB).
- Resolve nested generic class literals in class/module context (fixes `Sender(T).new` → `Channel::Sender(T).new` and removes `_Sender_Int32__new`) (2026-01-xx).
- Substitute type params in receiver names during method resolution; log unresolved generic receivers via debug hooks (2026-01-05).
- Log unresolved generic receivers for class method calls and lowering paths (Array(T).build tracing) (2026-01-05).
- Resolve overloads via full scan when call uses base name (avoid picking block overloads without blocks; removes missing func451 in raise_without_backtrace) (2026-01-06).
- Prefer module namespace over top-level aliases for mixin instance methods; carry module namespace into lazy lowering (fixes `FileDescriptor.system_info` resolving to `Crystal::System::FileDescriptor`) (2026-01-07).
- Expand macro calls for static member access (class/module) during call lowering (fixes macro-only class methods like `IO::Error.from_errno`) (2026-01-07).
- Run `macro included` during include registration/lowering; register macros + `extend` class methods from included modules (fixes `SystemError`-style class methods) (2026-01-07).
- Prefer mangled def names during method resolution when a definition exists (avoid base fallback) (2026-01-xx).
- Store callsite arg types by CallSignature (base+arity+block) to reduce `$arity`/`_splat` collisions (2026-01-xx).
- Force class-method lowering for module `extend self` methods when called as `Module.method` (fixes `self.*` calls inside class methods) (2026-01-xx).
- Capture callsite arg types by base+arity to survive `_splat`/`$arity` name shifts (2026-01-xx).
- Prefer typed overloads during mangled-prefix lookup in `lower_function_if_needed` to avoid wrong overload selection (2026-01-xx).
- Register and lower `lib` structs/unions as `ClassNode` (enable `LibC::Sigaction.new` and field accessors) (2026-01-xx).
- [x] Preserve `@[Link(...)]` annotations on top-level `lib` defs and lower them into link libraries (driver collect_top_level_nodes + HIR register_lib path) (2025-12-31).
- Lower lib struct field access (`action.sa_mask`) to direct field get/set (avoid `_LibC__Sigaction__sa_mask`) (2026-01-xx).
- Treat `TypeDeclarationNode` inside structs as lib field declarations (`field : Type`) (2026-01-xx).
- Unwrap pointer unions for `value/[]/+=` intrinsics to avoid llc type mismatch in Array(String) buffer stores (2026-01-xx).
- Remove `StructNode` from AST + LSP AST cache; structs are `ClassNode.is_struct` (cache version bump) (2025-12-25).
- Register module instance methods as class methods when `extend self` is present (fixes `Math.min/max`) (2025-12-25).
- Propagate `extend self` through macro-literal/module branches when registering module methods (2025-12-25).
- Parse no-parens calls with multiple args + `do` blocks by treating `do` as an expression boundary (fixes `return bsearch_internal ... do`) (2026-01-xx).
- Inline yield uses block arena ownership guard; fallback when block body arena mismatches (2026-01-xx).
- Lower `String.build` to `String::Builder.new` + `to_s` (removes malloc stub) (2026-01-xx).
- Array/Hash/Tuple literal lowering registers concrete generic types (fixes Array << Tuple in DWARF; missing symbols now 93) (2026-01-xx).
- Index lowering uses primitive class names for `[]`; unsigned integers treated as bitshift for `<<` (2026-01-xx).
- Debug callsite context added for `function.lookup.*` hooks (2026-01-xx).
- Resolve PathNode constants to values before member access (fixes `Char::REPLACEMENT.ord`) (2026-01-xx).

### Holistic risk scan (2026-01-xx)

- [x] Module macro-for expansion registered during HIR module processing (ByteFormat now emits `self.decode`/`self.encode`) (2026-01-xx).
- [x] Module class-method registration honors macro-generated defs across `{% for %}` / `{% if %}` branches (ByteFormat canary) (2026-01-xx).
- [x] Type literal flags survive `LocalRef` copies and module-type literals (avoid losing `T.class` / module dispatch) (2026-01-xx).
- [ ] Module-typed method resolution should prefer `Module.method` (.) and only fall back to includer lookup when uniquely resolvable (dynamic dispatch still missing).
- [ ] Module class methods defined by `extend self` in macro bodies must be added to class-method tables consistently.

### Holistic findings (2026-01-xx)

- Call-resolution still mixes base/mangled names across HIR lowering and def lookup; missing symbol spikes correlate with fallback-to-base calls.
- Callsite argument typing uses string keys (`$arity`, `_splat`) that shift during lowering; needs a single CallSignature representation.
- Cache keys in type/function lookup still elide namespace/owner in some paths; collisions remain a regression risk.
- Yield inlining is guarded but still touches cross-arena defs; a single ownership source + fallback path is needed.

### Bootstrap Stabilization Plan (prioritized, 2026-01-xx)

1) Call-resolution pipeline unification (highest impact) - DONE (2026-01-xx)
   - [x] Preserve callsite arg types per signature (base+arity+block) and consume consistently.
   - [x] Normalize splat/double-splat callsite keys before overload selection (base key strip).
   - [x] Keep multiple callsites per signature to avoid arg-type collisions.
   - DoD: missing symbol count in `/tmp/bootstrap_array_full.link.log` dropped to 96 (from 112 baseline); no `String_first` for `Char.in_set?` (2026-01-xx).

2) Cache poisoning / namespace resolution hardening - DONE (2026-01-xx)
   - [x] Include namespace + owner + type params in type/function cache keys (context-aware type cache).
   - [x] Invalidate caches on module/class reopen and macro reparse output with type defs.
   - DoD: missing symbol count dropped to 95 in `/private/tmp/bootstrap_array_full.link.log`; LSP stability spot-check pending.

3) Yield inlining arena safety
   - [x] Single source of truth for arena ownership during inline.
   - [x] Guard inliner against cross-arena AST and fallback to non-inline call.
   - DoD: `./bin/crystal_v2 examples/bootstrap_array.cr -o /tmp/bootstrap_array_full 2> /private/tmp/bootstrap_array_full.link.log` runs without OOB/segfault; no inline-yield guard logs present (2026-01-xx).
- Infer class var types from `uninitialized` and typed literals (Array/Hash/NamedTuple) to avoid VOID globals (fixes `Thread@@threads`, `Hasher@@seed`, `Time::Location@@location_cache`) (2025-12-25).
- Preserve generic class reopenings during monomorphization (fixes `Range#bsearch` defs) (2025-12-26).
- Resolve bare method calls inside class context to top-level when the class does not define the method (fixes `bsearch_internal`) (2025-12-26).
- Primitive template fallback for numeric receivers (Int/Float method bodies) to avoid missing defs in stdlib (2025-12-26).
- Pointer/new + mem intrinsics lowering hardened (ptr/int casts, llvm.mem* width selection) (2025-12-26).
- Treat `T.size` macro patterns as `Int32` during lightweight return-type inference (2025-12-26).
- Macro interpolation uses source spans for text pieces; `record` copy_with/clone expansion fixed (2025-12-27).
- HIR spec asserts `record` macro copy_with params (`_x`, `_y`) (2025-12-27).
- Block-pass handling for `&.`/`&block` + `try` return type union fixes `Indexable#[]` lowering (Array(Abbrev)#attributes no longer missing) (2026-01-03).

**Stdlib requires advanced features not yet implemented:**

| Feature | Issue | Priority |
|---------|-------|----------|
| `typeof(...)` in types | Partial: locals/params resolved; remaining complex/macros | HIGH |
| Generic methods with blocks | `def self.build(capacity : Int, &)` | HIGH |
| Module mixins (Indexable, Enumerable) | Instance methods from `include`d modules are expanded into concrete types; module-typed receivers still need better resolution | MED |
| Macro expansion | `getter`, `property` need compile-time expansion | MED |

**Additional codegen gaps (observed):**
- Top-level `{% if flag? %}` bodies now use raw source spans to parse defs/modules for simple flag branches; general macro expansion for complex bodies is still missing.
- Mixed-width primitive calls in untyped methods (e.g., `Math.min` with `Int64` + `Int32`) can emit LLVM phis with mismatched integer widths; needs numeric promotion/common-type coercion (2025-12-26).
- [x] Enum method bodies are captured and registered (enum defs now emitted for `Signal#reset` etc.) (2025-01-02)
- [x] Macro `flag?` branches inside class/struct bodies now register defs (e.g., `Crystal::Scheduler.init`). (2025-01-02)
- [x] Replace remaining `StructNode` checks with `ClassNode.is_struct` (parser does not emit StructNode). (2025-12-25)

### 8.3 Known Limitations

1. **typeof in type positions**: remaining gaps for module `self`/macro contexts; nested `Enumerable.element_type` chains on locals now resolve
2. **Block parameters**: callee-provided block param types now applied for non-inlined calls; remaining gaps around return-type constraints for `Proc` signatures without outputs
3. **Module mixin methods**: Include expansion works, but module-typed receiver resolution is still incomplete (beyond simple `Type.new(...)` return bodies).

### 8.4 TODO

1. [x] **Implement typeof resolution** - Compile-time evaluation of typeof(...) in type annotations
   - [x] `typeof(self)` / `typeof(arg)` inside generic instantiations (HIR lowering) (2025-12-19)
   - [x] General `typeof(...)` evaluation in type positions (params/returns/ivars/self) during lowering (2025-12-23)
   - [x] Simple constant/path typeof in type strings (no local scope) (2025-12-24)
   - [x] Enumerable/Indexable element_type patterns in typeof (2025-12-24)
   - [x] typeof(...) inside type aliases without local context (2025-12-20)
2. [x] **Fix generic methods with blocks** - Parse block parameter types into Proc signatures (2025-12-24)
3. [x] **Module mixin monomorphization** - Generate methods from included modules for concrete types
   - Partial: include expansion + module-typed return inference + stdlib-style combinator return inference
   - Improved: module-typed receiver fallback via includer map + last-expression return inference (2025-12-24)
   - [x] Prefer concrete `self`/ivar returns for module-like annotations (reduces module-typed receivers) (2025-12-25)
   - [x] Preserve concrete initializer types for module-annotated locals (avoids includer heuristics) (2025-12-25)
   - [x] Robust module-typed receiver resolution: avoid includer guessing; require unique match or concrete local type (2025-12-26)
     - Restrict class-scan fallbacks to unknown receiver types
     - Module-typed fallback only for module-like receiver names
4. [x] **Macro expansion for `getter`/`property`** - Compile-time accessor generation (module mixins) (2025-12-20)
