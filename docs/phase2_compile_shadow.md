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

Bare top-level macro calls are now also materialized on both the semantic side
and the collector-side declaration inventory.
For a reducer like:

```crystal
macro define_alpha
  def alpha
  end
end

define_alpha
alpha()
```

the shadow path no longer reports name-resolution or type-inference errors for
the macro call, and the collector-side declaration parity is now green too:
`methods collector_total=1 collector_unique=1 semantic_total=1 semantic_unique=1 gaps=0`.
On that carrier the provenance line now shows
`collector_direct_total=0 collector_macro_expanded_total=1`, which is the
right signal: the method came from a collector-side macro expansion path, not
from the original parse roots.

The same collector-side parity now also covers the narrow same-file call-site
corridor where the root is a real `CallNode` with positional arguments, for
example:

```crystal
macro define_alpha(dummy)
  def alpha
  end
end

define_alpha(1)
alpha()
```

On the current tree that carrier now reports
`methods collector_total=1 collector_unique=1 semantic_total=1 semantic_unique=1 gaps=0`.

Cross-file argful macro calls are now green too. A carrier like:

```crystal
# shadow_macro_lib.cr
macro define_alpha(dummy)
  def alpha
  end
end

# shadow_macro_main.cr
require "./shadow_macro_lib"
define_alpha(1)
alpha()
```

now reports `methods collector_total=1 ... semantic_total=1 ... gaps=0`, and
the same aggregate-based shadow path is green across the currently measured
macro-call shapes: bare identifier, positional args, named args, default arg,
and block-yield.

Generated top-level roots now participate in shadow `resolve_names` and
`infer_types`, not just in collector parity. For example, a carrier like:

```crystal
# shadow_generated_resolution_lib.cr
macro define_bad(name)
  def {{name.id}}
    missing + 1
  end
end

# shadow_generated_resolution_main.cr
require "./shadow_generated_resolution_lib"
define_bad(:alpha)
alpha()
```

now reports `resolution_diags=1` and the generated-body diagnostic is
attributed back to the caller unit. Likewise a generated body containing
`1 + "x"` now surfaces `type_diags=1` in shadow mode.

The remaining caveat is file attribution for post-parse macro expansion:
the shared aggregate node graph still reflects the original parse graph, but
symbol ownership is now rebound through the semantic shadow file-path provider,
and the aggregate now accepts a generated-node ownership overlay after semantic
collection. That means per-unit shadow summaries can attribute root-level
macro-generated symbols back to the originating source file and report both:

- original parse ownership via `nodes=`
- expanded semantic ownership via `owned_nodes=`

The shadow summary still reports `generated_nodes` separately, so expanded
semantic ownership is visible without pretending that the aggregate parse graph
itself changed.

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
- post-parse macro-generated nodes are now folded back into an ownership
  overlay for the shared aggregate, but the original parse graph is still
  preserved as a separate layer
- generated top-level defs are now walked by shadow name resolution and type
  inference via explicit `generated_top_level_roots`, but generated nodes are
  still not spliced back into the aggregate parse graph itself
- `generated_nodes` is a semantic-side provenance counter, not a replacement
  for aggregate `nodes`; `owned_nodes` is the expanded ownership count, and
  the three numbers intentionally describe different layers
- the compile-side collector now materializes the currently measured macro-call
  shapes in shadow parity, but this is still not a general lowering contract
- does not yet include macro-expansion parity with `AstToHir`
- does not yet run normalized HIR comparison

## Next step after this spike

The next honest move is no longer “make generated top-level defs visible”;
that part now works in shadow mode. The remaining honest move is to replace the
reparse-based aggregate with a real compile-path program graph substrate that
preserves:

- multi-file provenance
- prelude/require ordering
- macro-expanded units
- compile diagnostics parity
