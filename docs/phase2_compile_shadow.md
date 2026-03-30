# Phase 2 Spike: Compile Semantic Shadow via Shared AstArena

## Purpose

This spike adds a **shadow-only** compile-side semantic prepass behind:

- `CRYSTAL_V2_SEMANTIC_SHADOW=1`
- optional strict mode: `CRYSTAL_V2_SEMANTIC_SHADOW_STRICT=1`

On the current tree, strict mode now also treats two shadow-parity drifts as
hard failures:

- parser-diagnostic parity drift
- collector-vs-semantic declaration parity drift

If either of those diverges, shadow strict mode raises instead of only printing
telemetry.

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
- original compile-side parse diagnostic counts
- shared-aggregate shadow reparse diagnostic counts
- parser-diagnostic parity gap count between those two paths
- semantic / name-resolution / type-inference diagnostic counts
- generated semantic / name-resolution / type-inference diagnostic counts
- compile-collector vs semantic top-level declaration gap count

When `--verbose` is enabled, it also prints a **file-level unit summary** from
the shared aggregate:

- file path
- parse-root count in that unit
- analysis-root count in that unit
- generated-root count in that unit
- reachable node count in that unit
- top-level symbol count attributed to that unit
- generated top-level symbol count attributed to that unit
- resolved identifier count attributed to that unit
- original compile-side parse diagnostic count attributed to that unit
- shared-aggregate shadow reparse diagnostic count attributed to that unit
- semantic diagnostic count attributed to that unit
- generated semantic diagnostic count attributed to that unit
- name-resolution diagnostic count attributed to that unit
- generated name-resolution diagnostic count attributed to that unit
- type diagnostic count attributed to that unit
- generated type diagnostic count attributed to that unit

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

The semantic-side declaration inventory now prints matching provenance lines
too. On a carrier with one direct method and one macro-expanded method, verbose
shadow output now includes:

```text
Semantic shadow declarations: methods provenance semantic_direct_total=1 semantic_direct_unique=1 semantic_macro_expanded_total=1 semantic_macro_expanded_unique=1
```

This closes the old asymmetry where only the collector side could distinguish
`direct` from `macro_expanded` declarations.

Shadow summaries now also separate total semantic symbols from generated
semantic symbols. On a carrier with one direct method and one macro-expanded
method, the global and per-unit summaries now include `generated_symbols=1`,
which makes semantic ownership more explicit than inferring generated coverage
from `generated_nodes` alone.

That generated-symbol signal now also treats overload families honestly: if a
top-level `OverloadSetSymbol` contains one direct overload and one
macro-expanded overload, shadow summaries still report `generated_symbols=1`
for that family instead of dropping the generated contribution just because the
top-level entry is an overload set. When the direct and generated overloads
come from different files, the per-unit `generated_symbols` count is attributed
to the unit that contributed the generated overload, not blindly to the
overload-set wrapper's original `node_id`.

The same symbol-backed provenance path now covers non-method declaration kinds
too. A macro-expanded top-level `class`, `module`, `enum`, or constant
assignment now retains generated origin metadata on the semantic symbol itself,
and verbose shadow parity reports `semantic_macro_expanded_total=1` for those
kinds rather than treating generated non-method declarations as direct.

That same path also covers macro-generated top-level macro definitions. When a
macro expands into another top-level `macro`, the generated `MacroSymbol`
retains generated origin metadata and verbose shadow parity reports that extra
macro under `macros provenance ... semantic_macro_expanded_total=1`.

Declaration provenance is now also merged across reopened non-method symbols.
If a top-level `class` is introduced via macro expansion and then reopened
directly later in the source, semantic shadow parity reports both
`semantic_direct_total=1` and `semantic_macro_expanded_total=1` for that class
family instead of collapsing the final symbol to only its last-seen origin.

Generated declaration provenance and `generated_*_diags` counters now use the
explicit generated-origin mapping carried by the semantic stack. In particular,
generated method/class/module/etc. symbols now retain generated origin metadata
directly on the semantic symbol objects, so declaration parity no longer needs
an analyzer-side callback to classify those symbols as `macro_expanded`. The
generated source text map is still used only for formatting `... [generated]`
snippets, not for provenance classification.

That shadow-only metadata now also has a unified aggregate-side lookup:
`CompileShadowAggregate#generated_info_for(node_id)`, which bundles the
generated root, generated source text, macro call origin, and macro
definition site into one provenance record for downstream telemetry/formatting
code. The analyzer still computes that overlay, but it now crosses the
collector/analyzer -> aggregate boundary as an explicit `GeneratedOverlay`
contract instead of five ad-hoc hash maps, and the compile-shadow graph
substrate owns the lookup used by CLI formatting and generated-diagnostic
counting. `GeneratedOverlay` now also exposes an explicit empty/dup API, so
aggregate/analyzer/collector no longer have to hand-roll those snapshot
copies at each boundary.

CLI formatting now consumes aggregate-owned generated diagnostic contexts
instead of rebuilding display paths, generated source text, provenance notes,
temporary generated source-map overlays, or diagnostic-decoration glue in the
CLI layer. Same-file expansions still omit the `macro defined here` note to
avoid redundant output when the call site and definition live in the same
file.

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

CLI regression coverage now also locks non-method macro-call declaration parity
for both same-file and cross-file bundle carriers that generate top-level
`class`, `module`, `enum`, and constant declarations under the shadow path.
That coverage now also includes argful bundle carriers that materialize those
same non-method declarations through `{{name.id}}`-style macro arguments in
both same-file and cross-file forms.

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

The same traversal now also covers diagnostics nested inside generated
non-method roots. For example, a macro-expanded top-level `class` whose method
body contains `missing + 1` now surfaces a generated resolution diagnostic
with the generated class body snippet and the usual origin notes.

Verbose formatting now also uses the generated expansion text for generated
nodes instead of reusing the caller file source snippet. In those cases the
diagnostic path is rendered as `... [generated]`, for example:

```text
/tmp/shadow_generated_resolution_main.cr [generated]:2:5-2:5 undefined local variable or method 'missing'
  2 |     missing + 1
    |     ^
```

The same verbose path now also appends the originating macro call site as a
shadow-only note:

```text
note: expanded from macro call here
  --> /tmp/shadow_generated_resolution_main.cr:2:1-2:8
  2 | define_bad(:alpha)
    | ^^^^^^^
```

For cross-file macro expansions, verbose shadow formatting now also appends
the macro definition site itself:

```text
note: macro defined here
  --> /tmp/shadow_generated_macrodef_lib.cr:1:1-5:1
  1 | macro define_bad(name)
```

That origin note is now carried as first-class diagnostic metadata in the
shadow path, and the aggregate substrate now assembles those note spans
directly rather than leaving that work in ad-hoc CLI string glue:

- frontend diagnostics use `related_spans`
- semantic diagnostics reuse `secondary_spans`

Cross-file generated diagnostics can therefore carry two distinct provenance
notes in verbose shadow mode:

- where the macro was invoked
- where that macro was defined

The summary telemetry now also separates generated-body diagnostics from
parse-graph diagnostics. For the generated unresolved-name carrier above, the
global summary reports:

```text
generated_resolution_diags=1 generated_type_diags=1
```

and the caller-unit summary reports the same generated counts for that file.
For the generated type-error sibling carrier, the global summary reports:

```text
generated_resolution_diags=0 generated_type_diags=1
```

This keeps the shadow signal honest: generated-body failures are no longer
hidden inside the aggregate `resolution_diags` / `type_diags` totals.

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
It now also separates parse roots from actual traversal roots:

- `roots=`: original parse roots
- `generated_roots=`: generated top-level roots injected into shadow traversal
- `analysis_roots=`: parse roots plus generated roots

## Current limitations

- reparses sources instead of reusing parsed compile arenas
- file-level ownership now exists for aggregate nodes and all current shadow
  diagnostic families can be file-aware, but the legacy lowering pipeline
  remains the source of truth
- declaration parity is currently `compile collector -> semantic symbol table`
  for comparable kinds; it is still not a full lowering contract and not yet a
  dedicated macro-expanded parity gate
- collector provenance lines distinguish `direct` vs `macro_expanded`
  declarations on both the collector and semantic sides, but the semantic
  origin still comes from shadow-only generated-origin metadata rather than a
  compile-authoritative expansion provenance contract
- post-parse macro-generated nodes are now folded back into an ownership
  overlay for the shared aggregate, but the original parse graph is still
  preserved as a separate layer
- generated top-level defs are now walked by shadow name resolution and type
  inference via explicit `generated_top_level_roots`, but generated nodes are
  still not spliced back into the aggregate parse graph itself
- generated diagnostics now format against generated source text with a
  synthetic `... [generated]` path, but this is still a shadow-only
  presentation layer rather than a true compile-path source map contract
- shadow summaries can now distinguish original compile parse diagnostics from
  aggregate reparse diagnostics via `compile_parse_diags` and
  `shadow_parse_diags`, but that is still telemetry rather than a unified
  compile-authoritative parser diagnostic contract
- verbose shadow output now also prints parser-diagnostic parity summary lines
  and a `parse_diag_gaps` counter, so parser-side drift is visible without
  dumping raw parser diagnostics into the compile path
- `generated_*_diags` now distinguishes generated-body diagnostics from
  parse-graph diagnostics in the summary, but it still relies on shadow-only
  generated-origin metadata rather than a compile-authoritative source map
- generated diagnostics now also show the originating macro call site as a
  shadow-only note, but the primary span is still the generated-body span, not
  a fully remapped compile-path source map
- cross-file generated diagnostics now also show the macro definition site as a
  shadow-only note, but this is still formatter-level provenance, not a
  compile-authoritative expansion source map
- frontend origin notes are now first-class metadata in shadow formatting, but
  that relation still exists only inside the shadow pipeline and is not yet a
  general compile-path diagnostic contract
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
