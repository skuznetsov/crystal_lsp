# Phase 1: Semantic Identity Layer — Lifecycle & Invalidation Rules

## 1. SemanticTypeId Lifecycle

**Creation:** `SemanticTypeInternTable.intern(key)` assigns a unique `UInt32` id.

**Stability scope:** One compilation run. Ids are NOT stable across runs
(the intern table starts at 0 each time).

**Equality contract:** Two `SemanticTypeId` values are equal iff their `.id` matches.
Since ids are assigned by the intern table (not hashed), there are zero collisions.

**Invalidation:** SemanticTypeId values become invalid when the `SemanticTypeInternTable`
that created them is discarded (end of compilation).

## 2. DefIdentity Lifecycle

**What counts as "same def":**
- Same `AstArena` (by object_id) AND same `ExprId.index` within that arena.
- This identifies a syntactic Def node, not a typed instantiation.
- Reopened classes produce different Def nodes in the same or different arenas,
  so they get different DefIdentity values. This is correct — reopened defs
  are separate syntax trees even if they share a method name.

**Stability scope:** One compilation run. Arena object_ids are heap addresses,
stable within a process but not across runs.

**What does NOT determine identity:**
- Method name (two overloads of `foo` have different DefIdentity)
- Mangled name
- HIR FunctionId
- Type annotations (those go into DefInstanceKey, not DefIdentity)

## 3. DefInstanceKey Lifecycle

**What it represents:** A unique typed instantiation of a def — the same def
called with different argument types produces different keys.

**Components:**
- `def_identity`: which syntactic def
- `receiver_type`: semantic type of receiver (nil for top-level)
- `arg_types`: ordered list of argument semantic types
- `block_type`: semantic type of block argument (if any)
- `named_arg_types`: ordered list of `{name, type}` for named arguments

**Equality contract:** All components must match. Arrays are defensively copied
in the constructor, so mutation of the original array after key creation does
not affect the key.

**Cache semantics (future Phase 4):**
- First encounter with a DefInstanceKey → analyze body, cache result
- Second encounter with the same key → return cached result
- This is the demand-driven equivalent of original Crystal's `def_instances`

**Invalidation rules (for future cache):**
- Key becomes invalid if the underlying DefIdentity's arena is freed
  (would only happen with incremental compilation, not in current design)
- Key remains valid for the entire compilation run
- No time-based invalidation needed within a single compile

## 4. SemanticToHIRAdapter Lifecycle

**Purpose:** Maps `SemanticTypeId → HIR::TypeRef` at the emission boundary.

**Direction:** One-way. Semantic → HIR only. The reverse map exists for
diagnostics but must NOT be used for cache keys or identity.

**Population:** The adapter is populated during HIR emission. Each semantic type
that needs an HIR representation gets registered exactly once.

**Invalidation:** Same as SemanticTypeInternTable — valid for one compilation run.

## 5. DryRunTracker Lifecycle

**Purpose:** Observation-only side channel. Counts how many body inferences
would be cache hits if a DefInstanceKey cache existed.

**Behavior guarantee:** The tracker changes NO compilation behavior.
It only observes and reports statistics.

**Activation:** `CRYSTAL_V2_IDENTITY_DRY_RUN=1` environment variable.

**Output:** After compilation, dumps to STDERR:
```
[IDENTITY_DRY_RUN] lookups=N hits=N misses=N hit_rate=N%
[IDENTITY_DRY_RUN] unique_keys=N duplicate_keys=N interned_types=N
```

## 6. Boundary: SemanticTypeId vs HIR TypeRef

**Rule:** SemanticTypeId lives in semantic caches and DefInstanceKey.
HIR TypeRef lives in HIR Module, functions, instructions.

**Crossing the boundary:** Only through `SemanticToHIRAdapter.resolve()`.

**Never cross back:** HIR TypeRef must not appear in DefInstanceKey or
any semantic cache key. The adapter's reverse lookup is for diagnostics only.

## 7. Current Dry-Run Results (hello world)

```
lookups=5561  hits=5048  misses=513  hit_rate=90.8%
unique_keys=513  duplicate_keys=391  interned_types=261
```

This means 90.8% of body inferences are redundant — the same def with the
same receiver type was already analyzed. A proper DefInstanceKey cache
(Phase 4) would eliminate ~5048 redundant body walks.
