# Crystal v2 Frontend & LSP Roadmap

This TODO tracks work to bring the v2 frontend (parser + macros + semantic/type
layer) to parity with the original Crystal compiler, and then grow beyond it.

The immediate priority is **LSP correctness**: the v2 LSP server must not lie
about syntax or types and should match what the original compiler would report.

---

## Current Status (2025-12-09)

### Test Coverage
- **2958 tests**, 0 failures, 7 pending
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
- [x] Full type inference engine (Phase 103A-C)
- [x] Symbol table and name resolution
- [x] Full MacroExpander with @type API
- [x] LSP semantic tokens: symbol literals emit full-span enumMember tokens (no overlaps)
- [x] Hover/definition fallback to cached expr types (scoped lookup fixes; e.g., `cas` → `Array(Atom)`)
- [x] lsp_probe speaks correct binary Content-Length (no dropped responses)
- [x] TypeIndex binary storage (5.6x faster than JSON)

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

**Status:** Active development on `codegen` branch (2025-12-09)

### Completed Milestones (2025-12-09)
- [x] **M1.1** HIR data structures (`src/compiler/hir/hir.cr`) - 87 tests
- [x] **M1.2** AST → HIR lowering (`src/compiler/hir/ast_to_hir.cr`) - 87 tests
- [x] **M2.1** Basic escape analysis (`src/compiler/hir/escape_analysis.cr`) - 16 tests
- [x] **M2.3** Taint propagation (`src/compiler/hir/taint_analysis.cr`) - 17 tests
- [x] **M2.4** Memory strategy integration (`src/compiler/hir/memory_strategy.cr`) - 15 tests
- [x] **M3.1** MIR data structures (`src/compiler/mir/mir.cr`) - 20 tests
- [x] **M3.1b** MIR optimizations (`src/compiler/mir/optimizations.cr`) - 17 tests
  - RC elision (remove redundant rc_inc/rc_dec pairs)
  - Dead code elimination
  - Constant folding
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

#### 5.3.2 Runtime Support
- [ ] Minimal runtime library
- [ ] GC integration (Boehm as baseline)
- [ ] ARC runtime functions
- [ ] Arena allocator runtime

#### 5.3.3 Optimization Pipeline
- [ ] LLVM optimization passes (O0/O1/O2/O3)
- [ ] LTO support for release builds
- [ ] PGO hooks (profile-guided LLVM opts)

#### 5.3.4 Platform Support
- [ ] macOS (arm64, x86_64)
- [ ] Linux (arm64, x86_64)
- [ ] Windows (x86_64)
- [ ] Cross-compilation support

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
| M4.2 | Runtime library | 🔲 Pending | - |
| M4.3 | End-to-end compile | 🔲 Pending | - |

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
| MacroExpander | ~99% | Full @type API + annotations + typeof/sizeof/alignof |
| Type Inference | ~99% | Full generics + flow typing + blocks + unions (Phase 103A-C) |
| LSP Server | Complete | 26 methods, 4 GitHub issues fixed |
| TypeIndex | Complete | 5.6x faster than JSON, per-file partitioning |
| Performance | Complete | Incremental inference, lazy method bodies, cache warming |
| HIR | Complete | 155 tests (data structures, lowering, escape, taint, memory strategy) |
| MIR | Complete | 128 tests (SSA form, memory ops, optimizations, PGO passes) |
| Codegen | 55% | M1-M3.3 done, LLVM backend pending |
