# Phase 2 Spike: Compile Semantic Shadow via Shared AstArena

## Purpose

This spike adds a **shadow-only** compile-side semantic prepass behind:

- `CRYSTAL_V2_SEMANTIC_SHADOW=1`
- optional strict mode: `CRYSTAL_V2_SEMANTIC_SHADOW_STRICT=1`

It does **not** change compile output or lowering behavior.
It only verifies that the existing semantic stack can analyze a compile-like
aggregate program before any demand-driven rewrite work begins.

## Why shared AstArena instead of VirtualArena

The current `VirtualArena` is suitable for file grouping and top-level access,
but it does not rewrite nested `ExprId` references inside parsed nodes.

That means a deep semantic traversal over a multi-file `VirtualArena` would
follow file-local child ids as if they were global ids, which is not a safe
substrate for `Analyzer`, `NameResolver`, or `TypeInferenceEngine`.

For this spike, the compile shadow path reparses already-loaded compile units
into **one shared `AstArena`**:

1. compile path still parses files normally into `ParsedUnit`
2. shadow mode reparses each `ParsedUnit.source` into a shared `AstArena`
3. the resulting aggregate `Frontend::Program` is fed to `Analyzer`

This keeps all nested `ExprId` references globally valid without touching the
legacy lowering pipeline.

## Current scope

The shadow prepass currently reports:

- file count
- root count
- aggregate node count
- top-level symbol count
- resolved identifier count
- semantic / name-resolution / type-inference diagnostic counts
- compile-collector vs semantic top-level declaration gap count

When `--verbose` is enabled, it also prints a **file-level unit summary** from
the shared aggregate:

- file path
- root count in that unit
- reachable node count in that unit
- top-level symbol count attributed to that unit
- resolved identifier count attributed to that unit
- semantic diagnostic count attributed to that unit
- name-resolution diagnostic count attributed to that unit
- type diagnostic count attributed to that unit

Collector, name-resolution, and type-inference diagnostics now carry optional
node/file metadata in shadow mode, so the shadow path can format them against
the right per-file source text inside the shared aggregate.

When `--verbose` is enabled, the shadow path also prints a declaration parity
inventory comparing:

- top-level declarations collected by the compile-side top-level collector
- top-level declarations materialized in the semantic global symbol table

This is intentionally limited to comparable kinds:

- methods
- classes
- modules
- enums
- macros
- constants

It is an **integration substrate**, not a correctness gate for compile output.

The verbose declaration inventory now also prints **collector provenance**
for those same kinds:

- declarations collected directly from compile-unit arenas
- declarations collected from macro-expanded temporary arenas

This keeps the current signal honest: we can see when the collector side is
already materializing declarations through top-level macro expansion, without
pretending that this is full macro-expansion parity with lowering.

On the current tree, that top-level macro gap is now closed for the semantic
symbol table: a no-prelude carrier with a top-level `{% for %}` that generates
two methods now reports `collector_total=3` and `semantic_total=3` for methods.
The collector-side provenance lines remain useful because they still distinguish
the one direct method from the two macro-expanded ones.

The remaining caveat is file attribution for post-parse macro expansion:
the shared aggregate node graph still reflects the original parse graph, but
symbol ownership is now rebound through the semantic shadow file-path provider,
so per-unit shadow summaries can attribute root-level macro-generated symbols
back to the originating source file.

## Current limitations

- reparses sources instead of reusing parsed compile arenas
- file-level ownership now exists for aggregate nodes and all current shadow
  diagnostic families can be file-aware, but the legacy lowering pipeline
  remains the source of truth
- declaration parity is currently `compile collector -> semantic symbol table`
  for comparable kinds; it is still not a full lowering contract and not yet a
  dedicated macro-expanded parity gate
- collector provenance lines distinguish `direct` vs `macro_expanded`
  declarations only on the collector side; semantic inventory still has no
  matching expansion provenance contract
- post-parse macro-generated nodes are not yet folded back into the shared
  aggregate ownership map, so node counts still describe the original aggregate
  parse graph rather than an expanded semantic graph
- does not yet include macro-expansion parity with `AstToHir`
- does not yet run normalized HIR comparison

## Next step after this spike

The next honest move is to replace the reparse-based aggregate with a real
compile-path program graph substrate that preserves:

- multi-file provenance
- prelude/require ordering
- macro-expanded units
- compile diagnostics parity
