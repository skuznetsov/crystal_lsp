# Crystal v2 Frontend & LSP Roadmap

This TODO tracks work to bring the v2 frontend (parser + macros + semantic/type
layer) to parity with the original Crystal compiler, and then grow beyond it.

The immediate priority is **LSP correctness**: the v2 LSP server must not lie
about syntax or types and should match what the original compiler would report.

---

## Current Status (2025-12-31)

### Test Coverage
- **3400+ tests**, 0 failures, minimal pending
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
- [x] LSP expr span index cached per document to avoid full AST scans on hover/definition (2026-01-xx)
- [x] LSP line offset cache for position→offset lookup (reduces per-request line scans) (2026-01-xx)
- [x] LSP comment-position check uses cached line offsets (avoids full document scans) (2026-01-xx)
- [x] lsp_probe speaks correct binary Content-Length (no dropped responses)
- [x] TypeIndex binary storage (5.6x faster than JSON)
- [x] HIR macro condition evaluation: tri-state merge + duplicate module method guard (2025-12-23)
- [x] Driver trace logging gated via `CRYSTAL_V2_DRIVER_TRACE` (2025-12-23)
- [x] Resolve module method calls without parens (`M.foo`) to static dispatch (2025-12-23)
- [x] Macro expansion in HIR handles class/module body calls and main macro calls (2025-12-26)
- [x] MacroExpander reparse uses parse_program; macro is_a? and macro literal gap fixes (2025-12-26)
- [x] LSP 30x performance improvement: type inference cache skip, fast-path background indexing, cycle guard (2025-12-31)
- [x] If-condition short-circuit lowering branches directly (avoids phi use-before-def) (2026-01-xx)
- [x] HIR->MIR lowering uses CFG order to avoid forward references (2026-01-xx)
- [x] LLVM union returns treat null as nil union (2026-01-xx)

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
- [ ] Refine virtual-call detection: treat final/struct/monomorphic receivers as non-virtual; avoid blanket HeapEscape on method calls (call.virtual should be backed by class hierarchy; currently conservative for class receivers).
- [x] Method effect summaries: cache per-signature effects (`no_escape`, `transfer`, `thread_shared`, `ffi_exposed`, `returns_alias`) to replace name-based heuristics (2025-12-31).
- [x] Apply effect summaries during Call handling (escape/taint honoring `NoEscape`/`Transfer`/`ThreadShared`/`FFIExposed`) (2025-12-31).
- [x] Unknown-effect boundary: limit propagation and pick safe local strategy without poisoning the full escape/taint graph (2025-12-31).
- [ ] Add stdlib-only annotations: `@[NoEscape]`, `@[Transfer]`, `@[Taints(...)]`, `@[Arena("name")]` to override heuristics (Taints support added; Arena pending; Array/Hash/Set/Channel/Deque/SmallDeque/PointerLinkedList/Thread::LinkedList/PointerPairingHeap/Once::Operation partial coverage done).
- [ ] Replace name-based escape heuristics (container add/FFI/spawn lists) with annotation-driven effects; keep heuristics only as a safe fallback for unknown code.
- [ ] Builder/borrow region: tie child lifetimes to owner; only escape when owner escapes.
- [ ] Closure capture in loops: copy/move captured loop vars when closure escapes (avoid last-iteration capture/UAF).
- [ ] Any/Union boundary: treat as analysis boundary; force ARC/GC or slab to avoid stack UAF.
- [x] `--no-gc` diagnostics: report allocation site + reason (cycle/ffi/thread_shared). (Location now reported when source span is available.) (2026-01-03)

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
- [x] CLI honors `CRYSTAL_V2_STOP_AFTER_{PARSE,HIR,MIR}` for accurate phase profiling (2026-01-xx)

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
- Linker missing symbols (bootstrap_array full-prelude run 2025-12-31; 132 entries; full list in `/tmp/missing_symbols_latest.txt`).
- Update (2026-01-xx): full-prelude `bootstrap_array` now links with 51 missing symbols (see `/private/tmp/bootstrap_array_full.link.log` and `/private/tmp/missing_symbols_latest.txt`); removed `_Pointer_UInt8__includes__UInt8`, `_Tuple_key`, `_Tuple_value`.
- Update (2026-01-xx): missing symbols now 48 after super resolution via include-chain + pointer hash inline; removed `_Value_index_UInt8_Int32`, `_Value_reverse_`, `_Pointer_Void__hash_Crystal__Hasher`.
  - ByteFormat decode/from_io resolved (no `_IO__ByteFormat_decode_UInt32_IO`).
- Update (2026-01-10): numeric inherited methods now specialize on primitive owners (Int32#divmod), removing `Number_*` callsites. Current full-prelude `bootstrap_array` link shows 70 missing symbols (see `/private/tmp/bootstrap_array_full.link.log` and `/private/tmp/missing_symbols_latest.txt`).
- Update (2026-01-10): inherited overload resolution now respects arity for non-numeric receivers (Tuple#to_s no longer resolves to IO overload); Nil callsites removed. Missing symbols now 49 (see `/private/tmp/bootstrap_array_full.link.log` and `/private/tmp/missing_symbols_latest.txt`).
- Update (2026-01-12): union virtual dispatch now falls back to class vdispatch for abstract union variants (e.g., `IO | Nil | Pointer`), removing `_IO_read_Slice_UInt8_`/`_IO_write_Slice_UInt8_` callsites; current full-prelude `bootstrap_array` link shows 98 missing symbols (see `/private/tmp/bootstrap_array_full.link.log`).
- Update (2026-01-12): Polling class/module dedup + union virtual subclass lowering; `system_del` lowered and missing symbols now 43 (see `/private/tmp/bootstrap_array_full.link.log` and `/private/tmp/missing_symbols_latest.txt`).
- Update (2026-01-12): resolve included module aliases before lowering; module-level aliases no longer leak globally. `IO::Handle` no longer mis-registered as a class; missing symbols remain 43 (see `/private/tmp/bootstrap_array_full.link.log`).

**Regressions (open):**
- [ ] GH #10 (crystal_lsp): prelude build links for minimal `fib.cr`, but runtime segfault persists.
  - Repro (2026-01-xx): `./bin/crystal_v2 build --release --no-llvm-metadata /tmp/fib.cr -o /tmp/fib` succeeds; `/tmp/fib` exits 139.
  - `--no-prelude` path works: `/tmp/fib_no_prelude` prints `267914296` and exits 0.
  - DoD: prelude build runs without segfault and prints correct result.
  - Update (2026-01-xx): after lowering stdlib `fun main` as C-ABI entrypoint, build now fails at link with 27 missing symbols (see `/tmp/fib_noputs.link.log`).
  - Update (2026-01-xx): resolve forward refs in nested namespaces to current scope; `Crystal__System__Entry_name` removed. Link now fails with 64 missing symbols (see `/tmp/fib_noputs.link.log`).
- [x] LLVM opt/llc type mismatch in full-prelude `fib.cr` build.
  - Fix: resolve `.class` type literals before context prefixing; ByteFormat `UInt32.class` no longer resolves to `IO::ByteFormat::LittleEndian::UInt32`.
  - DoD: `./bin/crystal_v2 build --release --no-llvm-metadata /tmp/fib.cr -o /tmp/fib` now reaches link stage (missing symbols only).

**Recent fixes (prelude bootstrap path):**
- Normalize `flag?` macro arguments (strip leading `:`) + require cache v3; pthread requires now load.
- Coerce integer args to `i128` in LLVM backend for mismatch widths.
- Treat module names as `TypeKind::Module`; module-typed params refine to concrete defaults (IO::ByteFormat → SystemEndian), removing `_IO__ByteFormat_decode_UInt32_IO` (2026-01-xx).
- Refine module-typed params from mangled callsite suffix; `UInt32.from_io$IO_IO::ByteFormat::LittleEndian` now binds `format` to LittleEndian and emits `IO::ByteFormat::LittleEndian.decode` (2026-02-xx).
- Register `MacroForNode` inside nested modules so reopened macro modules (IO::ByteFormat::{Little,Big}Endian) contribute defs; `IO::ByteFormat::LittleEndian.decode` functions now appear in HIR (2026-02-xx).
- LLVM backend treats pointer constants `"0"` as `null` for ptr→int casts, removing `ptrtoint ptr 0` llc errors (2026-02-xx).
- EventLoop interface dispatch: force EventLoop::FileDescriptor/Socket to module kind and map instance calls to Polling/IOCP/Wasi (removes EventLoop__FileDescriptor_* missing symbols) (2026-01-xx).
- Upgrade module-typed forward declarations to class/struct on registration (removes duplicate `Module`/`Class` types like Polling and restores virtual lowering); union virtual calls now expand class subclasses for variants (2026-01-12).
- Fix module class-method deferred lookup to use the module arena (prevents `Index out of bounds` in `find_module_class_def`) (2026-01-xx).
- Track enum value types for `.new`/`.value` and propagate via assignments/identifiers in HIR lowering.
- Propagate enum return types from method calls into enum predicate lowering (e.g., `File::Info#type` predicates) (2026-01-xx).
- Track functions that return type literals; mark call results as type literals and resolve absolute `::` paths in HIR (fixes `EventLoop.backend_class`/nested class lookups) (2026-01-xx).
- Preserve `self` binding across block lowering so implicit self calls inside blocks use the correct receiver (fixes `Object#to_s` calling abstract `Object#to_s(io)` without virtual dispatch) (2026-01-xx).
- Invalidate type cache on enum registration to prevent enum names from collapsing to Class; IO::Seek predicate now lowers to compare (removes `_IO__Seek_current_`).
- Resolve superclass name in context when registering classes (fixes `FileDescriptor_initialize_Int32` super call).
- Seed top-level type names + class kinds in CLI/driver; prefer top-level class/struct over module (fixes `IO::File` resolution in `IO::ARGF` and `IO#read` missing symbols) (2026-01-xx).
- Resolve top-level type names before namespace prefixing in HIR (generic base seeding + union split) and bump AST cache VERSION to 28 to avoid stale namespace leaks (`File::Path`, `Path::Random`) (2026-01-xx).
- Treat proc-typed generic args as a single type in `split_generic_type_args` (fixes `Array(Int32, Exception? ->)`), and bump AST cache VERSION to 29 to invalidate stale parsed ASTs (AtExitHandlers block now parses with both statements) (2026-01-xx).
- Infer class var types from assignment sites inside module/class defs (e.g., `Crystal::AtExitHandlers@@handlers` from `||=` with `[] of ...`) (2026-01-xx).
- Register MacroIf/MacroLiteral nodes inside nested modules during HIR lowering.
- Remove `StructNode` handling from macro-parsed class bodies; rely on `ClassNode.is_struct` (2026-01-02).
- Attach trailing `do` blocks to parenthesized/member-access calls (fixes `Array(T).build(size) do` + `times do` in stdlib) (2026-01-xx).
- Infer identifier call types for implicit class/module methods during type inference; block param fallback now overrides stale type-param maps using receiver element types (fixes `Unicode.in_any_category?` lowering to `in_category?$Int32_Array(Tuple(...))`, removes `Unicode.in_category?$Int32_UInt64` from HIR) (2026-02-xx).
- Handle inline returns during yield inlining (guard proc/block bodies + safe block bounds) to preserve Enumerable semantics.
- Coerce short-circuit phi incomings into union variants (fixes `String::Grapheme.codepoints` classvar `||=` union phi) (2026-01-xx).
- Deduplicate union type emissions by name and use max payload size (fixes `opt` redefinition of `%_T__T_.union`) (2026-01-xx).
- Fix inline-yield return override so `return` inside block targets caller (removes `Nil#offset`/`Nil#size` in prelude HIR) (2026-01-05).
- Fix ivar assignment lowering to return RHS value (not void) so inline-yield blocks infer `Char` instead of `Tuple` (fixes `Char::Reader#decode_char_before` result typing) (2026-01-xx).
- Recheck registered return types after lowering to avoid fallback pointer returns (fixes `Crystal::System.to_string_slice` -> `Slice(UInt8)`) (2026-01-05).
- Fix range-index lowering to infer return types from defs/owner (prevents `Slice#[]` returning `Pointer` in `IO::Delimited`) (2026-01-xx).
- Update initialize ivar typing to replace VOID ivars with annotated param types; map `Bytes` to `Slice(UInt8)` in type lookup (2026-01-xx).
- Narrow locals for `is_a?` conditions in if/elsif branches (avoids `String#null?` in `to_string_slice`) (2026-01-08).
- Lower `is_a?` calls to intrinsic checks (UnionIs/IsA) and guard missing type args (2026-01-08).
- Lower `unsafe_as(T)` calls as intrinsic casts (no method call) and bitcast same-size float/int in MIR cast lowering (fixes `float_as_int`/`Object#unsafe_as(Int64)` returning ptr; removes `llc` phi type mismatches) (2026-01-01).
- Convert integer arithmetic results to float payloads when wrapping union math into float variants (fixes `store double %binop.raw` llc error in `LineNumbers#find`) (2026-01-01).
- Lower inherited class methods via Object fallback in codegen (fixes `String.set_crystal_type_id`) (2026-01-xx).
- Resolve inherited methods in parent namespace during lazy lowering (fixes `IO::Encoder` class/instance resolution) (2026-01-xx).
- Fix escaped macro controls in macro bodies to avoid false `{% for %}` nesting errors (restores `Object.set_crystal_type_id`) (2026-01-xx).
- Resolve lib out-struct types via `short_type_index` guard in `type_param_like?` (fixes `DlInfo` resolution; removes `Pointer(UInt8)#dli_*` missing symbols) (2026-01-xx).
- Normalize union type names using resolved variant names (avoid namespace cache poisoning across contexts) (2026-01-xx).
- Normalize tuple literal type args inside generics (`Array({Int32, Int32})` → `Array(Tuple(Int32, Int32))`) to align mangling (2026-01-xx).
- Prefer allocator base `Class.new` when no explicit overload matches (ignore block-only `new` for no-block calls); ensures `Array(Tuple...).new` is generated (2026-01-xx).
- Infer bsearch/bsearch_index returns for unannotated methods and prefer arity-specific overloads in member access (fixes `Array(Row)#address` in LineNumbers find) (2026-01-xx).
- Preserve callsite arg types per signature and consume consistently during lazy lowering (reduces base-name collisions; missing symbols now 96) (2026-01-xx).
- Context-aware type cache keys + invalidation on module/class reopen and macro reparse output (missing symbols now 95) (2026-01-xx).
- Bump AST cache version to 18 for parser/macro parse changes (2026-01-01).
- Release build uses `-O2` by default (`CRYSTAL_V2_OPT_LEVEL` override) after `-O3` segfaults during deep yield inlining; root cause TBD (2026-01-xx).
- Lower inherited instance methods via parent fallback in codegen (fixes `IO::FileDescriptor#puts` resolution) (2025-12-28).
- Use array element types for `each`/`each_with_index` block params to avoid Array(T)#field fallbacks.
- Infer `find`/`find_index` return types from element types (nullable) during member access lowering.
- Guard yield inlining when callee arena mismatches (fallback to non-inline call to avoid OOB).
- Resolve nested generic class literals in class/module context (fixes `Sender(T).new` → `Channel::Sender(T).new` and removes `_Sender_Int32__new`) (2026-01-xx).
- Substitute type params in receiver names during method resolution; log unresolved generic receivers via debug hooks (2026-01-05).
- Log unresolved generic receivers for class method calls and lowering paths (Array(T).build tracing) (2026-01-05).
- Resolve overloads via full scan when call uses base name (avoid picking block overloads without blocks; removes missing func451 in raise_without_backtrace) (2026-01-06).
- Prefer static call resolution only when identifier is not a local; emit extern static member calls directly (fixes `LibC.pthread_self`) (2026-01-xx).
- Prefer module namespace over top-level aliases for mixin instance methods; carry module namespace into lazy lowering (fixes `FileDescriptor.system_info` resolving to `Crystal::System::FileDescriptor`) (2026-01-07).
- Expand macro calls for static member access (class/module) during call lowering (fixes macro-only class methods like `IO::Error.from_errno`) (2026-01-07).
- Run `macro included` during include registration/lowering; register macros + `extend` class methods from included modules (fixes `SystemError`-style class methods) (2026-01-07).
- Capture `initialize` params from included modules for `new` signature inference (2026-01-xx).
- Prefer mangled def names during method resolution when a definition exists (avoid base fallback) (2026-01-xx).
- Register generic class-method aliases for base owners and honor block-aware overload resolution (fixes `Deque.half_slices` missing symbols) (2026-01-xx).
- Store callsite arg types by CallSignature (base+arity+block) to reduce `$arity`/`_splat` collisions (2026-01-xx).
- Force class-method lowering for module `extend self` methods when called as `Module.method` (fixes `self.*` calls inside class methods) (2026-01-xx).
- Capture callsite arg types by base+arity to survive `_splat`/`$arity` name shifts (2026-01-xx).
- Prefer typed overloads during mangled-prefix lookup in `lower_function_if_needed` to avoid wrong overload selection (2026-01-xx).
- Preserve mangled callsite suffix when falling back to base/parent/object/primitive defs (fixes typed numeric methods like `Int32#divmod$Int32`) (2026-01-xx).
- Register and lower `lib` structs/unions as `ClassNode` (enable `LibC::Sigaction.new` and field accessors) (2026-01-xx).
- [x] Preserve `@[Link(...)]` annotations on top-level `lib` defs and lower them into link libraries (driver collect_top_level_nodes + HIR register_lib path) (2025-12-31).
- Lower lib struct field access (`action.sa_mask`) to direct field get/set (avoid `_LibC__Sigaction__sa_mask`) (2026-01-xx).
- Treat `TypeDeclarationNode` inside structs as lib field declarations (`field : Type`) (2026-01-xx).
- Unwrap pointer unions for `value/[]/+=` intrinsics to avoid llc type mismatch in Array(String) buffer stores (2026-01-xx).
- Union-to-scalar casts now extract union payload instead of bitcast (fixes `Char::Reader#previous_char` union→i8 llc error) (2026-01-xx).
- Remove `StructNode` from AST + LSP AST cache; structs are `ClassNode.is_struct` (cache version bump) (2025-12-25).
- Case/when enum predicate matching now ignores underscores (e.g., `.character_device?`), lowering to enum == literal and removing `_character_device_` missing symbol (2026-01-02).
- Full-prelude bootstrap_array link status now fluctuates; latest run shows 52 missing symbols (see `/private/tmp/bootstrap_array_full.link.log`) (2026-01-xx).
- Macro body parsing: skip block depth for `abstract def` inside macro bodies to avoid false `{% end %}` errors (2026-01-02).
- Macro `flag?` expansion handles nested if/elsif/else/unless branches and strips leading comments in macro bodies (2026-01-xx).
- Bump AST cache version to 20 for macro-parse + enum predicate matching fixes (2026-01-02).
- Bump AST cache version to 21 for block/command-call parsing fixes (2026-01-03).
- Parser: don't treat wrapping ops/compound assignments as command-call args; allow nested blocks inside call-arg parsing (fixes `am.mantissa &+= ...` and `ticks.to_u64! &* ...` parsing, `&.each { ... { |e| ... } }` block params) (2026-01-03).
- Register module instance methods as class methods when `extend self` is present (fixes `Math.min/max`) (2025-12-25).
- Propagate `extend self` through macro-literal/module branches when registering module methods (2025-12-25).
- Parse no-parens calls with multiple args + `do` blocks by treating `do` as an expression boundary (fixes `return bsearch_internal ... do`) (2026-01-xx).
- Inline yield uses block arena ownership guard; fallback when block body arena mismatches (2026-01-xx).
- Lower `String.build` to `String::Builder.new` + `to_s` (removes malloc stub) (2026-01-xx).
- Array/Hash/Tuple literal lowering registers concrete generic types (fixes Array << Tuple in DWARF; missing symbols now 93) (2026-01-xx).
- Index lowering uses primitive class names for `[]`; unsigned integers treated as bitshift for `<<` (2026-01-xx).
- Debug callsite context added for `function.lookup.*` hooks (2026-01-xx).
- Resolve PathNode constants to values before member access (fixes `Char::REPLACEMENT.ord`) (2026-01-xx).
- Driver parse_file_recursive now uses AST + require cache when `CRYSTAL_V2_AST_CACHE` is enabled (speeds self-host parsing) (2026-01-xx).
- TypeInferenceEngine caches `children_of` per ExprId to reduce repeated traversal during inference (2026-01-xx).
- TypeInferenceEngine uses array-backed `children_of` cache (avoid hash overhead; auto-resize for arena growth) (2026-01-xx).
- TypeInferenceEngine caches identifier names by ExprId (reduce repeated String allocations during inference) (2026-01-xx).
- TypeInferenceEngine caches member-access names by ExprId (reduce repeated String allocations during call/member inference) (2026-01-xx).
- TypeInferenceEngine interns name slices to canonical Strings (reduces duplicate String allocations across nodes) (2026-01-xx).
- Frontend::StringPool gains String interning (`intern_string`) and Program carries string_pool (shared canonical Strings per parse) (2026-01-xx).
- Deduplicate callsite arg-type recording by base+arity to reduce Hash churn during HIR lowering (2026-01-xx).
- HIR `unless` applies truthy/is_a narrowing to else branch and treats `Return` as no-flow (fixes guard-clause nilable unwraps like `Arena#at?`) (2026-01-xx).

### Holistic risk scan (2026-01-xx)

- [x] Module macro-for expansion registered during HIR module processing (ByteFormat now emits `self.decode`/`self.encode`) (2026-01-xx).
- [x] Module class-method registration honors macro-generated defs across `{% for %}` / `{% if %}` branches (ByteFormat canary) (2026-01-xx).
- [x] Type literal flags survive `LocalRef` copies and module-type literals (avoid losing `T.class` / module dispatch) (2026-01-xx).
- [x] Module-typed method resolution prefers `Module.method` (.) and falls back to includer lookup when uniquely resolvable (dynamic dispatch still missing) (2026-01-xx).
- [x] Module class methods defined by `extend self` in macro bodies are added to class-method tables consistently (2026-01-xx).

### Holistic findings (2026-01-xx)

- Call-resolution still mixes base/mangled names across HIR lowering and def lookup; missing symbol spikes correlate with fallback-to-base calls.
- Callsite argument typing uses string keys (`$arity`, `_splat`) that shift during lowering; needs a single CallSignature representation.
- Cache keys in type/function lookup still elide namespace/owner in some paths; collisions remain a regression risk.
- Yield inlining is guarded but still touches cross-arena defs; a single ownership source + fallback path is needed.
- Unions of unrelated class types collapse to the first class in HIR (no UnionType), so dynamic dispatch is bypassed and calls become unsound.
- Self-host compile still stalls in `__crystal_main` at `driver.compile` call; `DEBUG_MAIN=1` + `DEBUG_LOWER_PROGRESS=CompilerDriver#compile` shows slowdown inside `all_arenas.each` block. Follow-up showed `parse_file_recursive` lowering `Parser#parse_program` as the hotspot (~40s) during compiler self-compile; driver now uses AST+require cache (2026-01-xx) — parse-only run is ~5.5s with cache, but `CRYSTAL_V2_STOP_AFTER_HIR=1` still times out (>120s). `DEBUG_HIR_TIMINGS=1` indicates stall inside module lowering; slow modules include `Crystal::Dwarf`, `Crystal::MachO`, `Crystal::EventLoop::Polling`, `ENV`, `Iterator`, `Indexable`, `Float::FastFloat` (2026-01-xx). `DEBUG_MAIN_SLOW_ONLY=1` shows `driver.compile` call is the last main expr; `DEBUG_LOWER_PROGRESS=parse_file_recursive` shows slow lowering at `source = File.read(abs_path)` (likely pulling in heavy IO/std lib code). (2026-01-xx)
- `DEBUG_LOWER_METHOD_SLOW_MS=200` during `CRYSTAL_V2_STOP_AFTER_HIR=1` self-host run shows hot spots inside compiler type inference and CLI: `Analyzer#infer_types` (~5.1s), `TypeInferenceEngine#infer_types` (~5.0s), `TypeInferenceEngine#infer_call` (~1.2s), `TypeInferenceEngine#infer_method_body_type` (~1.0s), `CLI#run_check` (~13.1s), and stdlib helpers (`Path#join`, `String#tr`, `File.join`) ~0.58s each. Indicates HIR stall is dominated by lowering the compiler's own inference engine, not just IO. (2026-01-xx)
- `DEBUG_LOWER_PROGRESS=CLI#run_check` shows slow subcalls inside `CLI#run_check`: `analyzer.collect_symbols` (~3.4s), `analyzer.resolve_names` (~1.2s), `analyzer.infer_types` (~5.7s). `DEBUG_LOWER_PROGRESS=infer_types` shows `TypeInferenceEngine#infer_types` dominated by `infer_expression(root_id)` (~5.8s). `DEBUG_LOWER_PROGRESS=infer_expression` highlights the large `case Frontend.node_kind(node)` and `infer_call` paths as slow in `infer_expression` (2026-01-xx).
- `DEBUG_LOWER_METHOD_SLOW_MS=500` with file paths shows main hotspots in `src/compiler/semantic/*` (SymbolCollector/NameResolver/TypeInferenceEngine), `src/compiler/frontend/parser.cr`, and stdlib `path.cr`/`file.cr`/`string.cr`. Suggests self-host HIR time is dominated by lowering the compiler/stdlib code itself, not a single stuck method. (2026-01-xx)
- Self-host HIR pass completes (≈13 min) with `CRYSTAL_V2_STOP_AFTER_HIR=1` + AST cache enabled; no errors in log. (2026-01-xx)
- `CRYSTAL_V2_STOP_AFTER_HIR=1` + `--stats` on `src/compiler/driver.cr` reports `hir=325660ms` (parse=179ms, prelude=108ms) with AST cache enabled (2026-01-xx).

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
4) Virtual dispatch lowering (IO/abstract receivers) - DONE (2026-01-02)
   - [x] Lower HIR `Call.virtual` into MIR type-id switch for class/union receivers.
   - [x] Module-typed dispatch uses includer set + subclasses in MIR (method resolution still governed by item 1100).
   - [ ] Emit vtables (or direct dispatch table) for concrete classes; store vtable ptr in class layout (deferred; type-id switch in use).
   - [x] Treat abstract defs as virtual in HIR call marking.
   - DoD: missing `_IO_read_Slice_UInt8_` / `_IO_write_Slice_UInt8_` / `_FileDescriptor_*` removed from `/private/tmp/missing_symbols_latest.txt` after full-prelude bootstrap (2026-01-02).
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
- Pointer null comparisons can emit invalid IR (`icmp ne ptr 0, null`); fix applied in LLVM backend (convert `0` to `null` for ptr NOT/branch conditions). Needs rebuild verification (2026-01-xx).
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

### 8.5 Bootstrap Debugging Notes (2026-01-01)

**Session findings for next developer:**

#### Issue 1: bsearch_internal param type (ptr vs double) - FIXED
- **Symptom**: LLVM error `bitcast ptr to double` in `bsearch_internal_Float64_Bool`
- **Root cause**: When arg type is VOID at call site, it's filtered from mangled name. `bsearch_internal(Float64, ???, Bool)` mangles to `bsearch_internal$Float64_Bool` (missing type for param 1). When function is lowered, params don't align with types.
- **Fix**: Added `refine_void_args_from_overloads()` in `ast_to_hir.cr:7923-7995` to infer VOID types from overload parameter annotations.
- **Verified**: HIR now shows `bsearch_internal$Float64_Float64_Bool` with correct types.

#### Issue 2: Array/Hash generic method instantiation - FIXED
- **Symptom**: 150+ missing symbols like `Array_String_____String` (mangled `Array(String)#<<$String`)
- **Observation**: Generic template for Array only has 14 nodes, should have 100+
- **Root cause**: **AST cache corruption**. The LSP AST cache was saving/loading stale data with corrupted body node counts.
  1. Parser correctly produces body_size=174 for Array
  2. Cache serialization/deserialization was working correctly
  3. BUT: Old cache files from previous versions were being loaded (version check passes but data was from incompatible parser output)
- **Fix** (2026-01-01, commit pending):
  1. Bumped AST cache VERSION from 15 to 16 to invalidate old caches
  2. Added VERSION to cache path (`~/.cache/crystal_v2_lsp/ast/v16/...`) so old caches are automatically orphaned
  3. Fixed `find_method_in_generic_template()` to use template's arena instead of `@arena` for visibility unwrapping
- **Verified**: Array now has body_size=174 (all methods), missing symbols reduced from 149 to 107

#### Issue 3: Flow typing for variable reassignment - FIXED (2026-01-xx)
- **Symptom**: `bsearch_internal_Float64_Float64` still in missing symbols
- **Root cause**: In stdlib `bsearch.cr:38-45`:
  ```crystal
  def bsearch_internal(from : Float64, to : Float64, exclusive)
    from = float_as_int from  # After this, from should be Int64, not Float64
    to = float_as_int to
    bsearch_internal(from, to, false) { ... }  # Call should use Int64 types
  end
  ```
  Variable reassignment doesn't update the type in our type inference. The call is still mangled with Float64 types instead of Int64.
- **Fix applied**:
  - infer_type_from_expr now flow-updates locals on assignment when inferred type is concrete.
  - infer_type_from_expr handles unary ops (+/-/!) and `unsafe_as` return types for branch inference.
  - infer_type_from_expr handles `as` casts to preserve target type.
  - Result: `bsearch_internal(from : Float, to : Float)` reassignment to Int no longer mangles as Float.

#### Issue 4: Macro expansion for {% begin %} blocks with {{@type}} - FIXED (2026-01-xx)
- **Symptom**: `Int#remainder` returns Nil because macro body isn't expanded
- **Root cause**: Macro blocks like `{% begin %} ... {{@type}} ... {% end %}` with `@type` references aren't being properly expanded.
- **Impact**: Methods with macro-generated bodies become empty, returning nil.
 - **Fix applied**:
   - MacroExpander now handles `{% begin %}` in MacroLiteral control flow (evaluates nested pieces like an always-true block).
   - Added `evaluate_begin_block` and wired into `evaluate_macro_body` / `evaluate_pieces_range`.
 - **Verification**: `/tmp/macro_begin_test.cr` compiled with `--no-link` (no undefined method errors).

#### Files Modified (commit 0a2444b):
- `src/compiler/hir/ast_to_hir.cr`:
  - `refine_void_args_from_overloads()` at lines 7923-7995
  - `find_method_in_generic_template()` at lines 15560-15583
  - Generic template body fallback in `lower_function_if_needed` at lines 15789-15815

#### Missing Symbols Snapshot:
- **Before fix**: 149 entries (`/tmp/missing_symbols_new.txt`)
- **After AST cache fix** (2026-01-01): 107 entries
- **After Proc#call fix** (2026-01-01): 81 entries (`/tmp/missing_symbols_now.txt`)
  - Fixed: Proc#call now emits indirect call through function pointer
  - Eliminated: 26 symbols including all `call_Pointer_*` for Proc types

**Remaining categories (81 symbols):**
- `Crystal__System__*` functions - system module stubs (7 entries)
- `String_*` functions - instance method dispatch (13 entries)
- `Nil_*` functions - union type method dispatch (9 entries)
- `Int32_exception_*` - exception handling (8 entries)
- `call_Pointer_*` - remaining non-Proc pointer calls (6 entries)
- Type conversion issues - union coercion (various)
  - `Nil_*` functions - nil method calls on unions (7 entries)
  - `bsearch_internal_Float64_Float64` - flow typing issue
  - Various DWARF/debug functions

#### Debug Environment Variables:
- `DEBUG_GENERIC_TEMPLATE=1` - traces generic template registration (shows body_size)
- `DEBUG_TEMPLATE_LOOKUP=1` - traces generic template body searches
- `DEBUG_LOOKUP=1` - traces function name lookups
- `CRYSTAL_V2_STOP_AFTER_PARSE=1` - stops driver after parse (self-host parse + AST cache ≈ 5.5s on 273 files, 2026-01-xx)
- `CRYSTAL_V2_STOP_AFTER_HIR=1` - stops driver after HIR lowering (useful to isolate post-HIR stalls)
- `CRYSTAL_V2_STOP_AFTER_MIR=1` - stops driver after MIR lowering (useful to isolate LLVM/llc stalls)
- `CRYSTAL_V2_LAZY_HIR=1` - skips eager module/class lowering (relies on lazy lower on call)
- `DEBUG_HIR_SLOW_MS=NN` - logs per-method HIR lowering slower than NN ms (driver only)
- `DEBUG_HIR_TIMINGS=1` - logs per-pass HIR timings in driver (collect/register/lower)
- `DEBUG_MAIN_SLOW_ONLY=1` - only log slow main expressions (no per-expr start spam)
- `DEBUG_MAIN_SLOW_MS=NN` - threshold for slow main expr logging (default 50ms)
- `DEBUG_MAIN_PROGRESS_EVERY=N` - progress interval for main lowering (default 500)
- `DEBUG_LOWER_SLOW_ONLY=1` - only log slow expressions in lower_def when DEBUG_LOWER_PROGRESS matches
- `DEBUG_LOWER_PROGRESS_EVERY=N` - log every Nth expr index during lower_def progress (reduces snippet spam)
- `DEBUG_LOWER_METHOD_SLOW_MS=NN` - log method lowering times only when >= NN ms
- Missing trace entries now include `virtual=` and `abstract=` flags to separate abstract virtual calls from real missing defs (2026-01-xx).

### 8.6 Bootstrap Session Notes (2026-01-01 - Session 2)

#### Issue 5: Nil method calls from incorrect type inference - IN PROGRESS

**Symptom**: 112 missing symbols including many `Nil_*` methods (`Nil_bytesize`, `Nil_empty_`, `Nil_check_no_null_byte`, etc.)

**Investigation findings**:
1. `resolve_method_call` returns `Nil#method` when `ctx.type_of(receiver_id)` returns `TypeRef::NIL (id=16)`
2. This happens in functions like `Path#join` where a parameter like `part` should be typed as `String` after `part = part.to_s`, but is still typed as `Nil`
3. The return type of `to_s` is being registered as `NIL` (id=16) instead of the String type

**Root cause analysis**:
- Debug output shows `[NIL_METHOD] Nil#bytesize receiver_id=36 recv_type=16 type_desc=nil func=Path#join$Pointer`
- `recv_type=16` is `TypeRef::NIL`, and `type_desc=nil` means no type descriptor was found
- The issue is that `ctx.register_type(call.id, return_type)` is being called with `return_type=NIL` for `to_s` calls
- This comes from `get_function_return_type()` returning NIL because the registered function type for `Pointer#to_s` or similar is NIL

**Debug flags added**:
- `DEBUG_NIL_METHODS=1` - shows Nil method calls with receiver_id, recv_type, and type_desc
- `DEBUG_TO_S_TYPE=1` - shows return types for all `to_s` calls

**Sample debug output**:
```
[TO_S_TYPE] return_type=16 mangled=Int#to_s$IO_Int32_Int32_Bool func=Reference#to_s
[TO_S_TYPE] return_type=16 mangled=Int#to_s$IO_Int32_Int32_Bool func=Pointer(UInt8)#to_s
```

The return_type=16 (NIL) for `to_s` methods is incorrect - should be String type.

**Fixes applied (partial)**:
1. Line 18569 - Added check to prevent NIL from overriding concrete receiver-derived return types:
   ```crystal
   if resolved_return_type != TypeRef::VOID && resolved_return_type != TypeRef::NIL && resolved_return_type != return_type
     return_type = resolved_return_type
   end
   ```
2. Lines 18437-18448 and 22116-22133 - Updated `methods_returning_receiver_type` to apply even when return_type is NIL
3. `get_function_return_type()` now treats VOID/NIL base names as unknown and falls back to cached base return types (prevents `Tuple#to_s()` returning NIL via a mismatched overload)
4. `register_function_type()` and `lower_def()` now allow NIL base return types to be replaced by non-NIL return types for the same base name
5. `resolve_method_call()` now prefers an overload with matching arity and falls back to ancestor overloads before using the base name (fixes `Tuple#to_s()` resolving to `Tuple#to_s(io)`).

**Progress note**:
- **Root cause found**: parser treated `in` as a binary operator while parsing `case VALUE` and truncated `class String` at `unicode_normalized?`. As a result, `String#to_s` was parsed as top-level def, not a method.
- **Fix applied**: disable `in` operator while parsing `case` value (`@allow_in_operator` guard) so `case ... in` branches parse correctly; `class String` now spans full file and includes `to_s`.
- **Cache invalidation**: bumped AST cache version to 17 to invalidate stale parsed ASTs after parser fix.
- **Result**: `String#to_s` is registered; union `String | Nil#to_s` resolves to `String#to_s`; `Nil#empty?`/`Nil#bytesize`/`Nil#check_no_null_byte` no longer appear in `/tmp/bootstrap_array_full.hir`.
- **Fix applied**: avoid refining VOID args to Float64 when untyped overloads exist; prefer untyped overloads for VOID arg sets. `Math.min/max` now lower to integer paths and `llc` no longer errors on `Slice_UInt8_____Int32_Int32` (2026-01-02).
- **Fix applied**: inline `try` for union receivers (nil-check + block inlining) to avoid generating union `try` symbols; `*_try` entries removed from missing list (2026-01-xx).
- **Fix applied**: preserve enum value tracking across nested lowering (push/pop) so callsite enum hints survive; `Unicode.check_downcase_turkic` now lowers to enum predicate compare (no `Int32#turkic?` in HIR).
- **Fix applied**: propagate enum types through @param auto-assign + ivar/cvar loads; `Path#windows?` and enum predicates no longer emit `Int32#windows?` in HIR.
- **Fix applied**: inline `nil?` for member access on non-union receivers; removes `Int32#nil?` / `Nil#nil?` call sites in HIR.
- **Fix applied**: infer `try` return types when block shorthand is parsed as an argument (BlockNode in CallNode args). This avoids `Bool#each$block` from `Exception#backtrace?` by returning the block's type (nilable union). Verification: `DEBUG_INFER_TRY=1 CRYSTAL_V2_STOP_AFTER_HIR=1 ./bin/crystal_v2 --no-prelude --no-link /tmp/try_test.cr -o /tmp/try_test` logs `[INFER_TRY] return=Int32`; `rg "Bool#each" /tmp/bootstrap_array_full.hir` returns no matches (2026-01-xx).
- **Fix applied**: infer ivar types from initialize default values for instance-var params; removed `Nil#[]`/`Nil#when` in `Time::Location` transitions (2026-01-xx).
- **Fix applied**: widen loop phi types using local assignment inference (loop-body scan + union wrap/cast). Prevents reassigned locals from collapsing to Nil/NoReturn in loops; `Array(Time::Location::ZoneTransition)#when` no longer appears in `/private/tmp/fib.hir` (verified via `rg`) (2026-02-xx).
- **Fix applied**: resolve `::Pointer(self)` in type names (strip leading `::`, still resolve generic args/`self`) and use `hir_to_mir_type_ref` for union sizes. Prevents ivar widening to pointer unions and removes the LLVM opt failure in `EventLoop::Polling#resume_all` (now reaches link stage) (2026-02-xx).
- **Fix applied**: yield inline lookup now matches base names regardless of `$...` suffix, so yield-bearing defs like `Deque.half_slices` inline instead of emitting calls. Missing symbols drop to 54 in `/private/tmp/missing_symbols_latest.txt`; no `_Deque_half_slices_*` in `/private/tmp/bootstrap_array_full.link.log` (2026-02-xx).
- **Fix applied**: handle `elsif` branches in yield/return detection + yield arg collection, and resolve def arenas before yield detection during method registration. `String#dump_or_inspect_char` now registers yield and inlines; `_String_dump_or_inspect_char_block` removed from `/private/tmp/missing_symbols_latest.txt`. Missing symbols now 52 (2026-02-xx).
- **Fix applied**: macro-for iterable `%w/%i` lists are parsed from source spans and `strip_macro_lines` preserves newlines. Macro-expanded class bodies now parse correctly, removing `Crystal::MachO::Nlist64::Type_*` missing symbols. Missing symbols now 48 (2026-02-xx).
- **Fix applied**: GEP index cast now truncates i128 → i64 (previously emitted `sext i128 to i64`, which LLVM rejects). This unblocks llc on `bin/fib.cr` with full prelude (2026-01-xx).
- **Fix applied**: pre-register implicit `self` for instance methods and block capture, so inline-yield and implicit calls use the receiver instead of VOID. `Object#to_s$IO` now logs with `recv=Object` and no `recv=Void` in `/tmp/fib_missing_trace_latest.log` (2026-01-xx).
- **Fix applied**: treat constant truthy/falsey short-circuit conditions as static; skip RHS lowering for `&&`/`||` when `left_cond` is a literal Bool. Removes `Nil#[]?` in `peek_or_read_utf8$Nil_Int32` (default nil `peek`) and drops those entries from `/tmp/fib_unresolved.log` (2026-02-xx).
- **Fix applied**: normalize tuple literal type names during namespace resolution (including prefixed `Foo::{...}`); tuple literals now resolve to `Tuple(...)`, removing `Unicode::{Int32, Int32, Int32}#[]$Int32` unresolved calls in `/tmp/fib_unresolved.log` (2026-02-xx).
- **Instrumentation**: added `CRYSTAL_V2_UNRESOLVED_CALL_TRACE=1` to log MIR-level unresolved calls (fallback to extern). Top offenders from `/tmp/unresolved_calls.log`: `Pointer(UInt8)#each$block`, `Pointer::Appender#<<$UInt8`, `Time::Location::ZoneTransition#when`, `Nil#offset`, `Time::Location::ZoneTransition#index` (2026-01-xx).
- **Fix applied**: magic identifiers `__FILE__`/`__DIR__`/`__LINE__`/`__END_LINE__` now lower as typed literals (no VOID locals in HIR) (2026-01-xx).
- **Fix applied**: bind `ARGC_UNSAFE`/`ARGV_UNSAFE` to `__crystal_main` params (argv typed as `Pointer(Pointer(UInt8))`), eliminating mis-resolved `Crystal::DWARF::Abbrev::Attribute#value` in PROGRAM_NAME init (2026-01-xx).
- **Fix applied**: rescue variable binding for lowercase identifiers (`rescue ex`) via parser + HIR fallback; `Crystal.init_runtime`, `Crystal.main_user_code`, `Crystal.exit`, and `Crystal::System.print_exception` are now lowered in HIR (2026-01-xx).
- **Update**: full-prelude `bootstrap_array` link now fails on EventLoop/Thread/Fiber/CallStack decode + vararg tuple extraction; see `/private/tmp/bootstrap_array_full.link.log` (2026-01-xx).
- **Fix applied**: allow tuple types for splat params even when some callsite args are `Void`, and fall back to `Tuple` when callsite types are missing. `Crystal::System.print_error$splat` now has a non-VOID args param (see `/private/tmp/bootstrap_array_full.hir`). Remaining `___Int32` now comes from `String::CHAR_TO_DIGIT.to_unsafe` in `Char#to_i` (bare `[]$Int32` receiver) (2026-01-xx).
- **Fix applied**: constant inference for `begin` blocks + uppercase identifiers; `String::CHAR_TO_DIGIT`/`CHAR_TO_DIGIT62` now infer as `StaticArray(Int8, 256)`, so `to_unsafe` resolves to `StaticArray#to_unsafe` and the `Nlist64::Type#to_unsafe`/`___Int32` fallback disappears from HIR (2026-02-xx).
- **Update (2026-02-xx)**: full-prelude `bootstrap_array` link now reports **31 missing symbols** (see `/private/tmp/bootstrap_array_full.link.log` and `/private/tmp/missing_symbols_latest.txt`).

#### Issue 6: ExprId -1 in inline_loop_vars_union (union keyword) - FIXED (2026-01-xx)
- **Symptom**: self-host compile crashed with `ExprId out of bounds: -1` while lowering `AstToHir#inline_loop_vars_union`.
- **Root cause**: `union` token was always treated as a definition start. In `inline_loop_vars_union`, `union = Set(String).new` was mis-parsed as `Set(String).new`, and `union.add(...)` had an invalid receiver.
- **Fix applied**:
  - `definition_start?` now treats `union` as a definition only when followed by an identifier.
  - `parse_program` fast-path now guards `union` the same way.
  - AST cache version bumped to 22 to invalidate stale cached ASTs.
  - Added parser spec to ensure `union` works as a local identifier.
- **Result**: `inspect_invalid_expr` finds no invalid expr ids under `inline_loop_vars_union`. Full self-host compile still slow; confirm completion in long run.

#### Issue 7: from_chars_advanced overload collapse during deferred module lookup - FIXED (2026-01-xx)
- **Symptom**: self-host compile stalled while lowering `Float::FastFloat::BinaryFormat_Float64#from_chars_advanced`; call to `from_chars_advanced(pns, value)` kept resolving to the 4-arg overload.
- **Root cause**: deferred module lookup used base names with `expected_param_count=0`, so the first matching def was picked; overloads collapsed to the base name, and `@lowering_functions` couldn't distinguish overloads.
- **Fix applied**:
  - Use callsite arg count to set `expected_param_count` when the name has no `$`.
  - Preserve mangled callsite name (`$...`) as `target_name` so overloads are lowered under distinct keys.
  - Emit `fcmp one` for float truthiness in LLVM backend to avoid `icmp ne double ... 0` errors when non-bool float conditions slip through.
- **Verification**: `DEBUG_FROM_CHARS=1 DEBUG_LOWER_PROGRESS=from_chars_advanced ./bin/crystal_v2 --no-llvm-opt --no-llvm-metadata --no-link /tmp/ff_test2.cr -o /tmp/ff_test2` completes, and both overloads are lowered.

#### Issue 8: Getter return type stays VOID in member access - FIXED (2026-01-11)
- **Symptom**: `/tmp/ivar_getter.cr` compiled to HIR with `call Box#buffer() : 0` while `func @Box#buffer -> 15` (String).
- **Fix applied**:
  - `get_function_return_type` now falls back to base-name/ivar return types even when the callsite name is mangled.
  - `ivar_return_type_for_method` strips `$...` mangling before looking up ivars.
- **Verification**:
  - `CRYSTAL_V2_STOP_AFTER_HIR=1 ./bin/crystal_v2 --emit hir --no-link --no-llvm-opt --no-llvm-metadata /tmp/ivar_getter.cr -o /tmp/ivar_getter`
  - `rg "Box#buffer" /tmp/ivar_getter.hir` shows `call ... : 15`.

#### Issue 8: Namespaced type params causing generic arity errors - FIXED (2026-01-xx)
- **Symptom**: self-host `CRYSTAL_V2_STOP_AFTER_HIR=1` fails with `Generic Hash expects 2 type args, got 1` on `Hash(Crystal::HIR::V)`.
- **Root cause**: type parameter names were getting namespace-qualified (e.g., `Crystal::HIR::V`), and `concrete_type_args?` treated them as concrete.
- **Fix**: treat namespaced type params as unresolved by checking the last path segment; skip monomorphization for path-like args; add DEBUG_MONO arg/arity diagnostics.
- **Verification**: `rg "Generic Hash expects" /tmp/selfhost_hir_mono.log` returns no matches with `DEBUG_MONO=1` run.

#### Issue 8: Parser lowering time spikes (parse_expression / macro parsing) - IN PROGRESS (2026-01-xx)
- **Symptom**: self-host compile appears to stall; `DEBUG_LOWER_METHOD_TIME=Parser#` shows large lowering times:
  - `Parser#parse_expression` ~45s
  - `Parser#parse_prefix` ~44s
  - `Parser#parse_macro_body` / `parse_macro_control_piece` / `parse_macro_definition` ~46–48s
  - `Parser#parse_program` ~49s
- **Finding**: `DEBUG_LOWER_PROGRESS=Parser#parse_macro_for_header` shows the slow call is `parse_expression(0)` (CallNode); the slowdown is in lowering parse_expression and its dependencies (inclusive).
- **Instrumentation added**:
  - `DEBUG_LOWER_METHOD_TIME` logs per-method lowering time.
  - `LOWER_SLOW_BODY` now includes call target (CallNode callee name).
- **Fix applied**:
  - `lookup_function_def_for_call` now uses a per-base overload index instead of scanning `@function_defs` for every call.
  - Result: `Parser#parse_expression` lowering dropped from ~49s → ~3.3s (see `logs/lower_method_time_parser.log`).
  - Cached `resolve_type_name_in_context` and `.class`/`.metaclass` resolution (clears alongside type cache) to reduce `type_ref_for_name` hot-path overhead (2026-01-xx).
  - Skip duplicate enum registrations; only compute enum base type once per enum name (reduces `register_enum`/`enum_base_type_for_node` overhead in self-host) (2026-01-xx).
  - Targeted invalidation for resolved type-name and type-literal caches (avoid full cache clear on each enum) (2026-01-xx).
  - Treat built-in type names and built-in generic bases as global for type-cache keys (avoid per-namespace cache churn) (2026-01-xx).
  - Fast-path builtin type names in `type_ref_for_name` to skip context/typeof handling (2026-01-xx).
  - Indexed type-cache invalidation by namespace component (avoid O(n) scans on every enum/class/module registration) (2026-01-xx).
  - Pre-index function definition overloads by base name (single scan on cache rebuild, avoids per-lookup full map scans) (2026-01-xx).
  - Use overload index in block-function lookup (avoid full function_defs scan for block overloads) (2026-01-xx).
  - Cache def param stats (counts/splats/block/type params) for overload scoring (avoids repeated param scans) (2026-01-xx).
  - Cache module def lookups (module/class methods) to avoid repeated include/def scans during lowering (2026-01-xx).
  - Use overload index in `lower_super` to avoid full function_defs prefix scans (2026-01-xx).
  - Cache instance method name lists per class for macro symbol tables (avoid full function_defs scans) (2026-01-xx).
  - Track classes with subclasses in a set for virtual-call checks (avoid scanning class_info each call) (2026-01-xx).
  - Cache `class_info_for_type` by type id (avoid linear scan over class_info for each lookup) (2026-01-xx).
  - Index HIR module functions by name (O(1) `has_function?`, avoid array scans) (2026-01-xx).
  - Index function_type keys by base for operator lookup (avoid scanning all function types on `<<` fallback) (2026-01-xx).
  - Index method base names by method for unknown-receiver fallback in resolve_method_call (avoid class_info scans) (2026-01-xx).
  - Use class_info_by_type_id in lower_call receiver resolution (avoid class_info scans) (2026-01-xx).
  - Index parent->children for module-typed resolution (avoid class_info scans in module/ivar fallback) (2026-01-xx).
  - Use class_info_by_type_id and short index in member-access resolution (avoid class_info scans) (2026-01-xx).
  - Index module includer keys by suffix (avoid scanning module_includers keys) (2026-01-xx).
  - Index instance method names by owner for macro symbol tables (avoid function_defs scans) (2026-01-xx).
  - Cache method inheritance resolution (class+method) with invalidation on function/module/class changes (2026-01-xx).
  - Track class_info mutation version to invalidate method inheritance cache on in-place updates (2026-01-xx).
  - Index HIR functions by base name (avoid scanning module.functions for fuzzy matches) (2026-01-xx).
- **Profile check** (2026-01-xx):
  - `tmp/profile_parser.cr` with `DEBUG_LOWER_METHOD_TIME=1` shows ~2.1–2.3s self-time per Parser parse_* method (parse_program/parse_macro_*), resolve/infer time ≈ 0. Cost is raw lowering, not lookup/inference.
- `DEBUG_LOWER_PROGRESS=Parser#parse_program DEBUG_LOWER_SLOW_MS=50` shows slowest node is the `parse_macro_definition` call (dominant cost inside parse_program).
- Chain from `parse_program` slow node: `parse_macro_definition` → `parse_macro_body` → `parse_macro_control_piece` → `parse_macro_for_header` → `parse_expression(0)` (all ~2.1s self-time). Root cost is `parse_expression` lowering itself.
- `parse_expression` slow node is `parse_prefix`; `parse_prefix` slow node is its large `case token.kind` dispatch. Cost scales with method body size; suggests HIR caching/pre-lowered blobs for compiler frontend would be higher leverage than more lookup caching.
- **Next**:
  - Profile for hotspots inside lowering (resolve_method_call / infer_type_from_expr / lower_function_if_needed).
  - Consider caching/memoization or an indexed lookup to avoid repeated full-map scans.
  - `DEBUG_LOWER_METHOD_STATS=1 DEBUG_LOWER_METHOD_TIME=register_enum` shows resolve/infer time = 0; register_enum time is dominated by raw lowering cost, not inference (2026-01-xx).

#### Issue 9: Untyped base methods generate unqualified calls (index$UInt8) - FIXED (2026-01-xx)
- **Symptom**: missing-trace reports `index$UInt8` with `recv=Void` from `peek.index(delimiter_byte)` in `IO#gets`.
- **HIR evidence**: both `IO#gets_peek$Char_Int32_Bool_Slice(UInt8)` (typed) and an untyped `IO#gets_peek(%1: 0, %2: 0, %3: 0, %4: 0)` are lowered. The untyped base emits `call %19.index$UInt8(%20)` (no owner), producing missing symbols.
- **Hypothesis**: eager lowering of defs happens before callsite arg types are recorded, so untyped methods are lowered with VOID params even though specialized callsites exist.
- **Fix applied**:
  - Defer lowering methods with all-VOID params unless a callsite signature is present (guard in `lower_method`).
  - Verification: `/tmp/gets_peek.hir` has `IO#gets_peek$Char_Int32_Bool_Slice(UInt8)` and no untyped `IO#gets_peek` or `index$UInt8` entries.

**Next steps for GPT-5.2**:
1. **Flow typing for variable reassignment**: DONE (see Issue 3).

**Files to investigate**:
- `src/compiler/hir/ast_to_hir.cr`:
  - `get_function_return_type()` - where function return types are looked up
  - `lower_call()` around lines 18400-18600 - where return types are determined
  - `register_function_type()` - where function types are registered

**Current missing symbol count**: 72 (after `bin/fib.cr` with prelude, log `/tmp/fib_link.log`, list `/tmp/missing_symbols_latest.txt`, 2026-02-xx).
- Update (2026-01-xx): `./bin/crystal_v2 --no-llvm-opt --no-llvm-metadata bin/fib.cr -o /tmp/fib` yields 104 missing symbols (`/tmp/missing_symbols_latest.txt`), only 3 are `Nil_*` (`Nil_index`, `Nil_to_u64`, `Nil_when`).
- Update (2026-01-xx): IO namespace wrappers no longer force `IO` to Module; `/tmp/fib.hir` shows `Class IO` and no `Nil#to_u64`. New blocker: llc error `expected 'i32' but got 'double'` for GEP index at `/tmp/fib.ll:45707`.
- Update (2026-01-xx): array GEP indices now cast from float to i32; llc error resolved. Current missing symbols: 100 (`/tmp/missing_symbols_latest.txt`, log `/tmp/fib_link.log`).
- Update (2026-02-xx): Nil element index lowering returns typed nil (skips ArrayGet/Set), and array index casts extend i1/i8/i16 to i32; llc void/idx mismatch resolved. Missing symbols now 72 (`/tmp/fib_link.log`, `/tmp/missing_symbols_latest.txt`).
- Update (2026-01-xx): fixed `lower_if` static is_a? guard to handle `false` (not just `true`), pruning Float32/Float64 branches in `Range#bsearch`; `Range(Int32, Int32)#unsafe_fetch$Float64` removed from `/tmp/fib.hir`. Missing symbols still 100 (`/tmp/fib_link.log`).
- Update (2026-01-xx): `restore_locals` now restores `self` from saved locals (no override by callee `self`), fixing inline-yield block receiver leakage. `Range(Int32, Int32)#unsafe_fetch$Pointer | Pointer` and `Nil#unsafe_fetch$Nil` removed from `/tmp/fib.hir`; missing symbols down to 80 (`/tmp/fib_link.log`).
- Update (2026-01-xx): `new` callsites now feed initializer callsite types and trigger lowering; `ArgumentError#initialize` is emitted in `/private/tmp/arg_error.hir` (no more missing `_ArgumentError_initialize`), missing symbols now 77 (`/tmp/fib_link.log`).
- Update (2026-01-xx): allow `initialize` with all-default params to infer types from defaults when callsite types are empty; `DivisionByZeroError#initialize` emitted in `/private/tmp/div_zero.hir`, missing symbols now 73 (`/tmp/fib_link.log`).
- Update (2026-01-xx): generic value params now emit numeric literals; numeric args stay unqualified (no `Crystal::EventLoop::65536` in HIR). `entries_per_block` returns `Int32`, and `Int32#divmod$Int32` is typed in `/tmp/fib.hir`. Missing symbols now 95 (`/tmp/fib_link.log`).
- Update (2026-02-xx): treat `_`-annotated params as untyped for eager-lowering checks; avoids VOID receivers in `IO#<<` paths. Missing symbols now 84 (`/tmp/fib_link.log`, `/tmp/missing_symbols_latest.txt`).
- **Missing trace snapshot (2026-01-xx)**: `CRYSTAL_V2_DEBUG_HOOKS=1 CRYSTAL_V2_MISSING_TRACE=1` on `bin/fib.cr` shows 98 unique `abstract=false` missing symbols. Top offenders: `Pointer(UInt8)#each$block` (21), `Range(Int32,...)` (9), `Int32#get_entry$Int32` (8), `Crystal::SpinLock#add_timer$Pointer` (8), `Int32#fit_in_indices$Crystal::Hasher` (4).
  - Remaining categories: EventLoop (`system_*`, arena helpers, `PollDescriptor_owned_by_`), DWARF (`Attribute_*`, `LineNumbers_decode_sequences`), MachO `Nlist64::Type_*`, IO/Path/File (`IO_read/write`, `Process.executable_path`, `PATH_MAX`, `realpath_DARWIN_EXTSN`, `File::Error.from_errno`, `Path.separators`), string/regex helpers (`String_*`, `Regex__MatchData_*`), pointer/tuple/slice helpers, `Thread_threads`, `_func*` stubs, and `__context`.
- Update (2026-02-xx): allow lowering untyped defs with all-default params by inferring call types from defaults (fixes base `IO#read_char_with_bytesize` no-arg calls; no `method.lower.defer` on `read_char_with_bytesize` in debug hooks).
- Update (2026-02-xx): `CRYSTAL_V2_UNRESOLVED_CALL_TRACE=1` on `bin/fib.cr` shows 283 unresolved calls; `read_char_with_bytesize` no longer appears in `/tmp/fib_unresolved.log`.
- Update (2026-02-xx): macro arg reparse fallback wraps `record` calls to handle keyword identifiers (`when : Int64`), restoring `ZoneTransition#when`/`#index` getters; unresolved call trace now 256 (from 283).
- Update (2026-02-xx): `|| raise` now unwraps nilable unions in `lower_short_circuit` and inference (adds `union_unwrap` for `peek_or_read_utf8_masked`); `UInt8 | Nil#to_u32` no longer appears in `/tmp/peek_utf8.hir`.

### 8.7 Bootstrap Session Notes (2026-01-08) - Linker Symbol Fixes

**Starting point**: ~150 undefined symbols when compiling `fib.cr` with prelude.

**Completed fixes**:

| Fix | Commit | Symbols Fixed |
|-----|--------|---------------|
| `to_s()` return type should be String, not receiver type | f025e32 (parent class) | ~10 |
| Yield inlining for `min_by`, `max_by`, etc. | e241c3a | ~15 |
| Yield functions with `$block` suffix detection | e241c3a | ~10 |
| Numeric union type conversions (`Int32 \| Int64` → common type) | 2ff6523 | ~8 |
| Flags enum `none?` intrinsic inlining | e31b8af | ~5 |
| `Pointer.new!` with type suffix intrinsic | 279757b | ~5 |
| `ascii_alphanumeric?` intrinsic inlining | e241c3a | ~3 |
| Primitive type names in `hir_type_name` | e241c3a | ~5 |
| Parent class method lookup for implicit self calls | f025e32 | ~20 |
| `has_constant?` macro method support | (local) | ~5 |
| Brace-literal postfix now attaches `do`/`{}` blocks + AST cache v24 invalidation | (local) | (kqueue types) |
| Avoid type-like fallback on non-type receivers (tuple literal `.each_with_index`) | (local) | ~1 |

**Recent unverified fixes (2026-01-xx):**
- Include type param mapping now uses current bindings when arg name matches a type param (unblocks generic include resolution).
- Module-typed resolution: prefer `System::FileDescriptor` → `IO::FileDescriptor`, `System::Socket` → `Socket`; allow lazy accessor generation when DefNode is missing.
- Module-typed ivar access: avoid marking module-typed params as type literals and prefer `IO::FileDescriptor` when multiple includer matches; removes `Crystal__System__FileDescriptor__read_timeout`/`write_timeout` from `/tmp/fib_link.log`.
- Module accessor setters on `obj.field = ...` now generate synthetic setters when missing and prefer module-typed class for setter resolution (fixes `IO::FileDescriptor#__evloop_data=` missing symbol; verified in `/tmp/fib_link.log`, 35d1973).
- Implicit self calls now force a receiver when resolving `Class#method` (fixes `IO#read`/`IO#write` missing symbols; `/tmp/fib_link.log`, 5acb793).
- Bare call resolution prefers `self` type before `@current_class` (fixes `Slice(Pointer(T))#unsafe_fetch` mis-resolving to `Slice(UInt8)`).
- Type cache hardening: builtin refs override stale cached types; module-kind correction for cached entries; `Crystal::` prefix resolution for modules; single-variant unions collapse to concrete type.
- Inline yield propagation: carry block param types (including fallback element inference for `String`/`Enumerable`), coerce yield args, and preserve param types across nested inlining.
- Inline yield fallback now filters by receiver ancestry; prevents `Crystal::DWARF::Info#each` from inlining into unrelated methods (removed `Nil#read_attribute_value` from `String#compare` HIR; verified via `rg` on `/tmp/bootstrap_array_full_nocache.hir`).
- Yield block param inference now scans callee bodies when block type annotations are missing (e.g., `Deque.half_slices`), using yield argument types to set block param types. This removes `Pointer(UInt8)#each$block` unresolved calls; verified with `CRYSTAL_V2_UNRESOLVED_CALL_TRACE=1 ... --no-link bin/fib.cr` (2026-01-xx).
- `TernaryNode` now participates in yield detection and yield-arg collection, so yield-functions like `Hash#put` are correctly marked and inlined. This removes unresolved `Hash(... )#put$..._block` calls; `CRYSTAL_V2_STOP_AFTER_MIR=1 CRYSTAL_V2_UNRESOLVED_CALL_TRACE=1 ... bin/fib.cr` shows 313→286 unresolved (2026-01-xx).
- Allow lowering untyped base defs when no typed overloads exist (fixes `Crystal::DWARF::LineNumbers#decode_sequences` missing symbol in `/tmp/fib_link.log`).
- Parenthesized calls no longer attach `{}`/`do` blocks across newlines (prevents `if foo() { ... }` from stealing the then-body tuple literal; `Path#separators` now parsed inside the `Path` class).

**Progress**: 150 → 64 → 30 symbols remaining.

**Session 2026-01-15 fixes**:
- Added modules to `@short_type_index` so short module names resolve correctly (e.g., `Printer` → `Float::Printer`)
- Fixed `register_nested_module` to scan for `extend self` and register module methods with `@module_extend_self`
- Emit loop stuck sigs reduced: 20 → 12 → 9
- `Float::Printer.shortest` now resolves and emits correctly

**Remaining symbol categories** (30 symbols):

1. **Malformed names** (~5 symbols):
   - `____Int32` - empty method name with Int32 receiver
   - `_func946` - anonymous function reference
   - `_inspect_IO`, `_to_s_IO_Int32` - incomplete mangling
   - `_self_to_u8_` - self receiver issue
   - **Root cause**: Method name empty or receiver not correctly resolved during mangling

2. **System module methods** (~7 symbols):
   - `Crystal__System__Fiber_current`, `_init`, `_suspend` - methods don't exist in unix/fiber.cr
   - `Crystal__System__Thread_current`
   - `Crystal__System__Time_day`, `_hour`, `_to_unix`
   - **Root cause**: Platform-specific methods not being included or called incorrectly

3. **Union types in mangled names** (~3 symbols):
   - `Float__Printer__Dragonbox_to_decimal_Float32___Float64` - union `|` in arg type
   - **Root cause**: Union types being stringified with `|` in method signatures

4. **Misc missing methods** (~15 symbols):
   - `String__Builder_initialize_Int32`, `File__Error_from_errno_String_String`
   - `Exception__CallStack_decode_function_name`, `_decode_line_number`, `_skip`
   - `Tuple_count`, `Pointer_UInt8__size`, `_to_unsafe`
   - `LibC__PATH_MAX_to_u32`, `realpath_DARWIN_EXTSN`
   - `RuntimeError_from_os_error_...` (complex overload)

**Current investigation**:
- `has_constant?` macro method added to `try_evaluate_macro_condition`
- Platform-specific constant detection via `evaluate_macro_flag("darwin")` etc.
- Issue: Types inside macro branches still inferring wrong (Int32 instead of struct pointer)

**Next steps**:
1. Debug why `kevent` variable type is Int32 inside `{% if LibC.has_constant?(:EVFILT_USER) %}` branch
2. Check if macro branch body parsing preserves outer scope types
3. Verify `process_interrupt?` function body is being parsed with correct types

**Files modified**:
- `src/compiler/hir/ast_to_hir.cr`:
  - Added `evaluate_has_constant()` at lines 14078-14118
  - Added `has_constant?` handling in `try_evaluate_macro_condition` at lines 14163-14196

### 8.8 Bootstrap Session (2026-01-15) - Return Type Inference Root Cause

**Key discovery**: The `____Int32` and other EMPTY_CLASS errors share a common root cause - **method return types are not being inferred/registered correctly**.

**Debug findings**:

1. **Pattern observed**: When `DEBUG_EMPTY_CLASS=1`, many methods have VOID receiver:
   ```
   [EMPTY_CLASS] method=put receiver_id=14 receiver_type.id=0 ... func=Array(String)#to_s
   [EMPTY_CLASS] method=delete receiver_id=72 receiver_type.id=0 ... func=Array(String)#to_s
   ```

2. **Receiver value trace** shows receivers are `Copy` of local variables:
   ```
   recv_value=Copy(src=8)  # hash variable in exec_recursive
   ```

3. **Return type lookup** reveals the actual problem:
   ```
   [GET_RETURN] name=Fiber#exec_recursive_hash func_type=Void base_type=Void module_rt=(nil)
   [GET_RETURN] name=Crystal::System::Fiber.current func_type=(nil) base_type=(nil) module_rt=(nil)
   ```

**Root cause chain**:
1. `Fiber#exec_recursive_hash` returns `Hash({UInt64, Symbol}, Nil)` but is registered with VOID
2. When lowering `hash = Fiber.current.exec_recursive_hash`, the local `hash` gets VOID type
3. Subsequent `hash.put(...)` call has VOID receiver → empty class name → malformed symbol `_put`

**Methods affected** (return VOID instead of actual type):
- `Fiber#exec_recursive_hash` → should return `Hash({UInt64, Symbol}, Nil)`
- `Fiber#exec_recursive_clone_hash` → should return `Hash(UInt64, UInt64)`
- Many instance methods with inferred return types from `||=` expressions

**Specific code pattern not handled**:
```crystal
def exec_recursive_hash
  @exec_recursive_hash ||= Hash({UInt64, Symbol}, Nil).new  # ||= not inferring type
end
```

**Files with fixes made this session**:
- `src/compiler/hir/ast_to_hir.cr`:
  - Added `Pointer#value` handling in `infer_type_from_expr` (~line 5051)
  - Added generic template check in `resolve_method_with_inheritance` (~line 14811)
  - Enhanced EMPTY_CLASS debug to show `recv_value` info

**Next steps**:
1. **Fix return type inference for `||=` expressions** - the right-hand side of `||=` should determine return type
2. **Audit ivar accessor return types** - methods like `@foo ||= X.new` need to return type of X
3. **Check if `Crystal::System::Fiber.current` is being called incorrectly** - this method doesn't exist

**Symbol count**: 30 remaining (unchanged - investigation phase)

**Update (2026-01-16)**:
- `Fiber.current` now resolves to the top-level class (no `Crystal::System::Fiber.current` in HIR) when `--emit hir` is used.
- `exec_recursive_hash` return type infers `Hash(Tuple(UInt64, Symbol), Nil)` by recognizing `Hash(...).new` in member-access inference and `||=`.
- Verification: `/tmp/exec_hash.hir` shows `call Fiber.current() : <non-VOID>` and `call %...Fiber#exec_recursive_hash() : Hash(...)`.
- `bin/fib.cr` link now reports **30** missing symbols (`/tmp/fib_link.log`).
- **Update (2026-01-xx)**: If/branch inference now uses branch-local context + unions across then/elsif/else. `Crystal::System.to_string_slice` now infers `Slice(UInt8)` (DEBUG_INFER_BODY_NAME). `bin/fib.cr` missing symbols down to **28** (`/tmp/fib_link.log`, list in `/tmp/missing_symbols_latest.txt`).
- **Update (2026-01-xx)**: lib extern globals now emit `external global` and lib globals resolve via member access; `LibGC_stackbottom` removed. `bin/fib.cr` missing symbols now **27** (`/tmp/fib_link.log`).
- **Update (2026-01-16)**: `bin/fib.cr` link now reports **26** missing symbols (authoritative list in `/tmp/fib_link.log`; `/tmp/missing_symbols_latest.txt` is stale).
  - **Runtime helpers (declared, no defs)**: `Crystal__ex_message`, `Crystal__ex_backtrace_`, `Crystal__handler_ex_message`, `Crystal_trace_Int32_String_UInt64___Nil_NamedTuple`
  - **Block funcs missing**: `each_block`, `func965`, `func1649` (no emitted block defs)
  - **Stdlib defs not lowered**: `String__Builder_initialize_Int32`, `Thread_threads`, `Crystal__System__Signal_inspect`, `Location__Zone_inspect_IO`, `Exception__CallStack_decode_function_name`, `Exception__CallStack_decode_line_number`, `RuntimeError_from_os_error_String___Nil_Errno___WinError___WasiError___Nil_NamedTuple_double_splat`
  - **LibC / OS externs**: `realpath_DARWIN_EXTSN`, `File_fstat_Int32_Pointer`, `LibC__PATH_MAX_to_u32`
  - **Receiver/loss / mangling**: `set_crystal_type_id_Pointer_UInt8_`, `self_to_u8_`
  - **EventLoop Unknown methods**: `Crystal__EventLoop__Unknown_to_s_IO`, `Crystal__EventLoop__Unknown_inspect_IO`
  - **Remaining misc**: `Crystal__EventLoop__Polling__Arena_Crystal__EventLoop__Polling__PollDescriptor__65536__unsafe_grow`, `File__Error_from_errno_String_String`, `Dragonbox_to_decimal_Float32___Float64`, `Tuple_count`, `TupleCrystal__TupleVoid___Crystal__String__String____Int32`
- **Update (2026-01-16)**: relaxed generic inline-yield skip (when receiver type params are known). `bin/fib.cr` missing symbols now **23** (`/tmp/missing_current.txt`, `/tmp/fib_link.log`).
  - Removed: `Crystal__ex_message`, `Crystal__ex_backtrace_`, `Crystal__handler_ex_message`, `func965`, `func1649`.
- Remaining block funcs: `each_block`, `func1031`, `func1708` (still no block func emission for some cases).
- **Update (2026-01-16)**: `bin/fib.cr` link now reports **18** missing symbols (see `/tmp/fib_link.log`).
  - Missing: `Crystal__EventLoop__Polling__Arena_Crystal__EventLoop__Polling__PollDescriptor__65536__unsafe_grow`,
    `Crystal__EventLoop__Unknown_to_s_IO`, `Crystal__EventLoop__Unknown_inspect_IO`,
    `Crystal__System__Signal_inspect`, `Crystal_trace_Int32_String_UInt64___Nil_NamedTuple`,
    `Dragonbox_to_decimal_Float32___Float64`,
    `Exception__CallStack_decode_function_name`, `Exception__CallStack_decode_line_number`,
    `File__Error_from_errno_String_String`, `File_fstat_Int32_Pointer`, `LibC__PATH_MAX_to_u32`,
    `Location__Zone_inspect_IO`, `RuntimeError_from_os_error_String___Nil_Errno___WinError___WasiError___Nil_NamedTuple_double_splat`,
    `String__Builder_initialize_Int32`, `Thread_threads`,
    `TupleCrystal__TupleString___Crystal__Nil__String____Int32`, `Tuple_count`, `func1030`, `func1708`,
    `realpath_DARWIN_EXTSN`, `self_to_u8_`, `set_crystal_type_id_Pointer_UInt8_`.
- **Update (2026-02-xx)**: `./bin/crystal_v2 --no-llvm-opt bin/fib.cr -o /tmp/fib` link now reports **20** missing symbols (see `/tmp/fib_link.log`).
  - Missing: `Crystal__EventLoop__Polling__Arena_Crystal__EventLoop__Polling__PollDescriptor__65536__unsafe_grow`,
    `Crystal__EventLoop__Unknown_to_s_IO`, `Crystal__EventLoop__Unknown_inspect_IO`,
    `Crystal__System__Signal_inspect`, `Crystal_trace_Int32_String_UInt64___Nil_NamedTuple`,
    `Dragonbox_to_decimal_Float32___Float64`,
    `Exception__CallStack_decode_function_name`, `Exception__CallStack_decode_line_number`,
    `File__Error_from_errno_String_String`, `File_fstat_Int32_Pointer`, `LibC__PATH_MAX_to_u32`,
    `Location__Zone_inspect_IO`, `RuntimeError_from_os_error_String___Nil_Errno___WinError___WasiError___Nil_NamedTuple_double_splat`,
    `String__Builder_initialize_Int32`, `Thread_threads`,
    `TupleCrystal__TupleString___Crystal__Nil__String____Int32`, `func1704`,
    `realpath_DARWIN_EXTSN`, `self_to_u8_`, `set_crystal_type_id_Pointer_UInt8_`.
  - **Update (2026-02-xx)**: allocator init fallback now forces `String::Builder#initialize$Int32` lowering; `./bin/crystal_v2 --no-llvm-opt examples/bench_fibonacci.cr -o /tmp/fib` link reports **19** missing symbols (see `/tmp/fib_link.log`).
    - Missing: `Crystal__EventLoop__Polling__Arena_Crystal__EventLoop__Polling__PollDescriptor__65536__unsafe_grow`,
      `Crystal__EventLoop__Unknown_to_s_IO`, `Crystal__EventLoop__Unknown_inspect_IO`,
      `Crystal__System__Signal_inspect`, `Crystal_trace_Int32_String_UInt64___Nil_NamedTuple`,
      `Dragonbox_to_decimal_Float32___Float64`,
      `Exception__CallStack_decode_function_name`, `Exception__CallStack_decode_line_number`,
      `File__Error_from_errno_String_String`, `File_fstat_Int32_Pointer`, `LibC__PATH_MAX_to_u32`,
      `Location__Zone_inspect_IO`, `RuntimeError_from_os_error_String___Nil_Errno___WinError___WasiError___Nil_NamedTuple_double_splat`,
      `Thread_threads`, `TupleCrystal__TupleString___Crystal__Nil__String____Int32`, `func1782`,
      `realpath_DARWIN_EXTSN`, `self_to_u8_`, `set_crystal_type_id_Pointer_UInt8_`.
  - **Update (2026-02-xx)**: class-method fallback now resolves `set_crystal_type_id` to `String.set_crystal_type_id`; missing symbols now **18** (see `/tmp/fib_link.log`).
    - Missing: `Crystal__EventLoop__Polling__Arena_Crystal__EventLoop__Polling__PollDescriptor__65536__unsafe_grow`,
      `Crystal__EventLoop__Unknown_to_s_IO`, `Crystal__EventLoop__Unknown_inspect_IO`,
      `Crystal__System__Signal_inspect`, `Crystal_trace_Int32_String_UInt64___Nil_NamedTuple`,
      `Dragonbox_to_decimal_Float32___Float64`,
      `Exception__CallStack_decode_function_name`, `Exception__CallStack_decode_line_number`,
      `File__Error_from_errno_String_String`, `File_fstat_Int32_Pointer`, `LibC__PATH_MAX_to_u32`,
      `Location__Zone_inspect_IO`, `RuntimeError_from_os_error_String___Nil_Errno___WinError___WasiError___Nil_NamedTuple_double_splat`,
      `Thread_threads`, `TupleCrystal__TupleString___Crystal__Nil__String____Int32`, `func1782`,
      `realpath_DARWIN_EXTSN`, `self_to_u8_`.
  - **Update (2026-02-xx)**: lib constants are registered in lib bodies and relative type paths resolve in type contexts. `LibC::PATH_MAX` and `Location::Zone` are now resolved. `./bin/crystal_v2 --no-llvm-opt bin/fib.cr -o /tmp/fib` link now reports **16** missing symbols (see `/tmp/fib_link.log`):
    - `Crystal__EventLoop__Polling__Arena_Crystal__EventLoop__Polling__PollDescriptor__65536__unsafe_grow`
    - `Crystal__EventLoop__Unknown_to_s_IO`, `Crystal__EventLoop__Unknown_inspect_IO`
    - `Crystal__System__Signal_inspect`
    - `Crystal__TupleCrystal__TupleString___Crystal__Nil__String____Int32`
    - `Crystal_trace_Int32_String_UInt64___Nil_NamedTuple`
    - `Dragonbox_to_decimal_Float32___Float64`
    - `Exception__CallStack_decode_function_name`, `Exception__CallStack_decode_line_number`
    - `File__Error_from_errno_String_String`, `File_fstat_Int32_Pointer`
    - `RuntimeError_from_os_error_String___Nil_Errno___WinError___WasiError___Nil_NamedTuple_double_splat`
    - `Thread_threads`
    - `func1781`
    - `realpath_DARWIN_EXTSN`
    - `self_to_u8_`
  - **Update (2026-02-xx)**: `Crystal__System__Time_instant` resolved by adding `Crystal::System::Time.instant`.
  - **Update (2026-02-xx)**: `includes__Int32` resolved by inferring default types for untyped params (Range literal in `Float::Printer.decimal`).

### 8.9 Grok Review Notes (2026-02-xx)

**Summary (actionable):**
- Implicit generic inference still collapses to `VOID/Any` in some flows (Array/Hash).
- Lazy monomorphization + conditional callsites leave signatures recorded but not lowered.
- Several HIR lowering heuristics are string-based (array detection) and can misfire.

**Proposed fixes (bootstrapping blockers first):**
1) **Force lower all tracked callsite signatures** (even when calls are in conditional paths).
   - Source: `@callsite_args` / recorded signatures.
   - DoD: missing symbols for callsite-only functions drop in `/tmp/fib_link.log`.
2) **Generic instantiation for implicit params**:
   - Avoid `Array(VOID)`/`Hash(VOID, ...)` fallback in `get_function_return_type`.
   - Ensure `type_params` are substituted for implicit locals inferred from body.
   - DoD: `Array(U)` with implicit U resolves to concrete in HIR (no `VOID`).
3) **Yield/block lowering completeness**:
   - Either inline all yield-bearing defs or implement MIR lowering for yield.
   - DoD: no `each_block` or `func####` missing in `/tmp/fib_link.log`.
4) **HIR lowering robustness**:
   - Replace string-based type checks (e.g., `"Array"` prefix) with TypeKind.
   - Ensure phi creation includes loop-carried locals.

**Medium-term (post-bootstrap):**
- Add annotation-driven escape/taint metadata (`@[NoEscape]`, `@[Transfer]`, `@[Arena]`).
- Improve `--no-gc` diagnostics: point to variable + source span for GC-requiring allocation.

**Update (2026-02-xx)**:
- Bare identifier fallback now resolves to top-level functions when no local exists (e.g., `caller`).
  - Evidence: `/tmp/caller_test.hir` contains `call caller()` and no `local "caller"`, and the loop lowers via `array_size` (no `each$block`).
  - Evidence (prelude): `/private/tmp/fib.hir` now contains `call caller()` inside `Crystal::Scheduler#fatal_resume_error`; no `each$block` for `caller.each`.

### 8.10 Bootstrap Blockers: Budgeted Callsite Lowering (PROPOSED)

**Problem**: Naive “lower all tracked callsite signatures” risks compile-time blowups.  
**Goal**: Keep missing symbols dropping without exploding compile time or memory.

**Plan (fast, low-risk):**
1) **Baseline metrics**: record number of pending callsite signatures and time spent in `emit_all_tracked_signatures` on `bin/fib.cr`.
   - DoD: `DEBUG_EMIT_SIGS=1 ./bin/crystal_v2 --no-llvm-opt --no-link bin/fib.cr -o /tmp/fib` logs counts.
   - **Update (2026-02-xx)**: baseline shows `emit_all_tracked_signatures` loops 100 iterations with 16 sigs each time (no progress). Log: `/tmp/fib_emit_sigs.log`.
2) **Budgeted lowering**: cap the number of signatures lowered per iteration (global cap + per-base cap).
   - Prefer non-VOID callsite args.
   - Skip bare generics and VOID-only signatures unless explicitly requested.
3) **Feedback loop**: use missing-symbol lists to re-run lowering for the exact missing signatures (manual two-pass workflow).
   - DoD: missing symbols drop between pass 1 and pass 2 without large time regression.

**Rationale**: Keeps monomorphization lazy but prevents “recorded-only” symbols from being dropped.
