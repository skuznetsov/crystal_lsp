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

When `--verbose` is enabled, it also prints a **file-level unit summary** from
the shared aggregate:

- file path
- root count in that unit
- reachable node count in that unit
- top-level symbol count attributed to that unit
- resolved identifier count attributed to that unit

It is an **integration substrate**, not a correctness gate for compile output.

## Current limitations

- reparses sources instead of reusing parsed compile arenas
- file-level ownership now exists for aggregate nodes, but semantic diagnostics
  still do not preserve compile-path provenance because they only carry spans,
  not canonical `ExprId` identities
- does not yet include macro-expansion parity with `AstToHir`
- does not yet run normalized HIR comparison

## Next step after this spike

The next honest move is to replace the reparse-based aggregate with a real
compile-path program graph substrate that preserves:

- multi-file provenance
- prelude/require ordering
- macro-expanded units
- compile diagnostics parity
