# RFC: Demand-Driven Semantic Rewrite for Compile Path

Status: Draft  
Audience: Claude Opus implementation track, architecture review  
Scope: Compiler compile path only; check path unification is part of rollout, not day 1  
Supersedes: high-level direction in `PLAN_DEMAND_DRIVEN_REWRITE.md`

## 1. Purpose

This RFC turns the current rewrite idea into a migration contract.

The goal is not "rewrite AstToHir because it is large".
The goal is to replace the current supply-driven compile path with a
demand-driven semantic pipeline that:

- removes queue-explosion architecture from compile
- makes body analysis cacheable by semantic identity
- preserves current HIR and downstream contracts during rollout
- keeps self-hosted bootstrap risk bounded with feature flags and shadow mode

This is a strategic architecture track. It is not justified as the immediate
fix for the current runtime-stability frontier.

## 2. Current Constraints

The current tree has two worlds:

- compile path: `Parse -> AstToHir -> HIR -> MIR -> LLVM`
- check path: `Parse -> Analyzer -> SymbolCollector -> resolve_names -> infer_types`

Important constraint: these worlds do not share one type identity model today.

- semantic stack uses `Semantic::Type` in `TypeContext`
- HIR explicitly uses `HIR::TypeRef` and avoids coupling to `Semantic::Type`

Important compile-path constraint: compile does not operate on one small parser
program. It loads prelude and `require` graph into multiple parsed units and
source maps.

Important rollout constraint: raw HIR equality is not a valid shadow gate.
Function ids, value ids, block ids, and type ids are order-sensitive.

## 3. Non-Negotiable Invariants

These are hard requirements for the rewrite.

### 3.1 Semantic identity is canonical

The new semantic layer must define one canonical type identity model for:

- cache keys
- invalidation
- typed-def identity
- conversion into HIR `TypeRef`

The compile path must not mix `Semantic::Type` and `HIR::TypeRef` in the same
cache key without a canonical adapter.

Required artifact:

- `SemanticTypeId` or equivalent canonicalization layer
- stable mapping:
  - semantic type -> canonical semantic id
  - canonical semantic id -> HIR `TypeRef` at emission boundary

### 3.2 Bridge to AstToHir is leaf-only

The new pipeline may temporarily delegate only to leaf emission helpers.

It must never depend on legacy supply-driven control flow:

- `emit_all_tracked_signatures`
- `process_pending_lower_functions`
- `force_lower_function_for_return_type`
- legacy pending-function safety nets

Required runtime assertion under the new feature flag:

- if any of the above paths execute, compilation fails loudly

### 3.3 Declaration phase reaches a full fixed point

The declaration phase must produce all compile-visible metadata required by
later phases, including:

- classes, modules, structs, enums, libs
- aliases and alias chains
- macros relevant to compile path
- enum constant resolution
- C struct sizes and alignment-relevant info
- module includers/extenders
- class hierarchy
- generic template declarations
- method effect annotations
- extern function/global registration

No later phase may depend on ad hoc discovery of these declarations.

### 3.4 Shadow mode compares normalized equivalence, not raw dumps

The rollout gate is semantic equivalence after normalization, not byte-identical
HIR text.

Required normalized comparison includes:

- function name and signature set
- return types
- instruction shape per function
- call target set
- method effect summaries
- class hierarchy and module includer metadata
- extern tables
- type descriptor set after normalization
- MIR generation success
- LLVM generation success
- reducer runtime behavior on selected smoke cases

## 4. Feature Flags

### 4.1 Primary flag

`CRYSTAL_V2_SEMANTIC_COMPILE=1`

- `0` or unset: legacy compile path
- `1`: new semantic compile path

### 4.2 Validation flags

Recommended auxiliary flags:

- `CRYSTAL_V2_SEMANTIC_SHADOW=1`
- `CRYSTAL_V2_SEMANTIC_ASSERT_NO_LEGACY_QUEUE=1`
- `CRYSTAL_V2_SEMANTIC_DUMP_NORMALIZED=1`

## 5. Phases

### Phase 0: Contracts and Instrumentation

Purpose:
- define what must be preserved before changing architecture

Must produce:
- contract doc for HIR inputs required by MIR and LLVM
- normalized shadow comparator spec
- counters for legacy supply-driven behavior:
  - forced lowers
  - pending queue growth
  - safety-net pass count
  - duplicate body analysis count if measurable

Exit criteria:
- normalized comparison format is implemented or fully specified
- legacy-path metrics are observable in CI/local runs
- kill-switch assertions are defined for new flag

### Phase 1: Canonical Semantic Identity Layer

Purpose:
- remove ambiguity between semantic types and HIR types

Must produce:
- canonical semantic type id
- typed-def identity model
- canonical `DefInstanceKey`

Required `DefInstanceKey` shape:

```crystal
record DefInstanceKey,
  def_id : UInt64,
  receiver_type_id : SemanticTypeId?,
  arg_type_ids : Array(SemanticTypeId),
  block_type_id : SemanticTypeId?,
  named_arg_types : Array({String, SemanticTypeId})?
```

Rules:
- no mangled names in semantic cache keys
- no `HIR::TypeRef` in semantic cache keys
- no cache keyed by unstable object graph unless object identity is the intended
  semantic identity and lifecycle is controlled

Exit criteria:
- one adapter exists from semantic id to `HIR::TypeRef`
- one adapter exists from semantic id to printable normalized type name
- cache invalidation rules are documented

### Phase 2: Compile-Path Integration Substrate

Purpose:
- make existing semantic stack compile-capable before demand-driven rewrite

This is the missing migration phase between current check path and compile path.

Must cover:
- multi-file aggregation
- prelude loading behavior
- require graph handling
- source-map parity
- macro expansion parity for compile path
- arena strategy that does not assume only one parser-built `AstArena`

Required changes:
- `Analyzer` can consume compile-path program graph, not only check-path unit
- `TypeInferenceEngine` no longer assumes one parser-built `AstArena`
- symbol collection and macro expansion preserve file/source provenance needed by
  compile diagnostics and later lowering

Exit criteria:
- semantic pipeline can analyze a compile-path aggregate program without going
  through AstToHir
- file/line provenance matches current compile diagnostics on a smoke suite

### Phase 3: Full Declaration Fixed Point

Purpose:
- move all declaration and registration logic required for compile into the
  semantic pipeline

Must produce:
- all declaration metadata listed in invariant 3.3

Must not rely on:
- later fallback registration from AstToHir
- runtime discovery of types during emission

Exit criteria:
- declaration output can seed HIR builder without extra AstToHir registration
- legacy declaration pass and new declaration pass are normalized-equivalent on
  selected stdlib and repo inputs

### Phase 4: Demand-Driven Body Typing

Purpose:
- type-check bodies on demand and cache by semantic identity

Core rules:
- bodies are analyzed only when reached from top-level demand
- each typed def is analyzed once per unique `DefInstanceKey`
- generic instantiation is on-demand
- recursive and cyclic cases use explicit in-progress states, not fallback queue
  growth

Must produce:
- demand-driven call resolver
- typed-def cache
- duplicate body analysis counter

Exit criteria:
- repeated calls with identical semantic signature hit cache
- legacy forced-return-type lowering is not used
- body-analysis count drops on benchmark reducers

### Phase 5: HIR Builder Replacement

Purpose:
- emit current HIR contracts from typed semantic results

Allowed bridge:
- leaf helpers only

Forbidden bridge:
- any delegation that can invoke legacy queue/safety-net architecture

Must emit:
- HIR instructions compatible with current `hir_to_mir`
- method effect summaries
- lifetime and taint seeds required downstream
- class hierarchy and module includers
- extern tables
- type descriptors and generic metadata needed downstream

Exit criteria:
- new HIR builder passes normalized shadow compare against legacy path on
  reducer suite
- `CRYSTAL_V2_SEMANTIC_ASSERT_NO_LEGACY_QUEUE=1` stays green

### Phase 6: Shadow Rollout

Purpose:
- validate semantics before switching default

Stages:
1. build both pipelines
2. normalize both outputs
3. compare equivalence
4. run MIR/LLVM generation on both
5. run runtime smoke suite on both

Required shadow suite:
- hello world
- no-prelude tiny carrier
- representative block/proc case
- generic container reducer
- macro-heavy reducer
- enum/lib/alias reducer
- current stage2/stage3 compile reducers

Exit criteria:
- normalized comparator green on agreed suite
- downstream MIR/LLVM green
- no regressions in runtime smoke behavior
- no legacy queue assertions fired

### Phase 7: Default Switch and Deletion

Purpose:
- switch compile default only after shadow confidence is high

Order:
1. default-on behind escape hatch
2. soak period
3. remove legacy compile-path dependency
4. unify check and compile semantic paths where appropriate

Exit criteria:
- default-on is stable on reducer suite and bootstrap suite
- rollback flag still works during soak
- dead code deletion does not remove still-used leaf helpers prematurely

## 6. Normalized Shadow Comparison Spec

The comparator must ignore unstable ids and emission order where order is not
semantically meaningful.

Normalization rules:

- normalize `FunctionId`, `ValueId`, `BlockId`, `TypeId`
- compare functions by name + signature
- compare calls by normalized callee + normalized operand shape
- compare type descriptors by normalized name/kind/params
- compare metadata maps by normalized keys
- compare instruction streams after id renaming

A shadow run is green only if:

- normalized HIR equivalent
- MIR build succeeds
- LLVM build succeeds
- selected runtime smokes behave the same

## 7. Kill-Switch Rules

Under `CRYSTAL_V2_SEMANTIC_COMPILE=1`, fail immediately if:

- `emit_all_tracked_signatures` executes
- `process_pending_lower_functions` executes
- `force_lower_function_for_return_type` executes
- legacy pending-function queue exceeds zero after semantic typing is active
- new semantic cache falls back to mangled-name identity

## 8. Metrics

Primary metrics:

- forced lowers per compile
- safety-net pass count
- pending queue growth
- duplicate typed-body analysis count
- compile wall time
- peak RSS on selected reducers

Secondary metrics:

- normalized HIR mismatches
- MIR mismatches
- LLVM mismatches
- bootstrap stage success rate

## 9. Out of Scope for Initial Rollout

- redesigning MIR
- redesigning LLVM backend
- changing HIR semantics unless required by contract gap
- solving every current runtime correctness bug through this RFC

## 10. Definition of Done

This RFC is complete when:

- compile path is demand-driven by semantic identity
- legacy supply-driven queue machinery is not required under the new flag
- normalized shadow suite is green
- bootstrap and reducer suite are green enough to justify default switch
- semantic and HIR type identity boundary is explicit and stable

