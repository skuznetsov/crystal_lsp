# Phase 2.1 Draft: Compile-Authoritative Provenance Contract

## Purpose

Phase 2 compile shadow is now strong enough to answer three observational
questions reliably:

- which file owns a parsed or generated node in the shared shadow aggregate
- which top-level declarations were materialized on the collector and semantic
  sides
- which generated diagnostics came from generated code, the macro call site,
  and the macro definition site

It is **not** yet strong enough to answer the next question:

> Can the compile path itself expose one provenance/source-map contract that
> both shadow telemetry and future compile diagnostics can consume without
> reparsing-specific heuristics?

This document scopes that next step.

## Current State

The current tree already has a useful shadow-only substrate:

- `Semantic::GeneratedOverlay`
- `CompileShadowAggregate#provenance_for(node_id)`
- `CompileShadowAggregate#diagnostic_provenance_context_for(node_id)`
- parser parity telemetry via `compile_parse_diags`, `shadow_parse_diags`,
  and `parse_diag_gaps`
- strict shadow failure on parser or declaration parity drift

These are valuable, but still observational:

- generated diagnostics use a synthetic `...[generated]` display path
- generated provenance is attached after semantic expansion, not carried by a
  compile-authoritative source map
- parser parity compares compile parse diagnostics to aggregate reparse
  diagnostics, rather than consuming a shared diagnostic identity model
- the shared aggregate is still a reparse substrate, not the compile graph

## Non-Goals

This step should **not**:

- rewrite lowering or `AstToHir`
- replace the shared-aggregate shadow path with a real compile graph
- splice generated nodes back into the original parse graph
- claim normalized HIR parity
- change default compile behavior

## Problem Statement

Today the compile path and the shadow path can both talk about provenance, but
they talk about it through different contracts.

Compile path currently owns:

- real parsed units
- require/prelude ordering
- compile diagnostics from the authoritative parse/load flow

Shadow path currently owns:

- generated node ownership
- generated snippets
- generated call-site notes
- generated macro-definition notes
- file-aware semantic diagnostic formatting

That split is good enough for telemetry, but not for the next integration step.
Without one explicit provenance contract, the project will keep duplicating
logic in three places:

- compile driver orchestration
- semantic shadow aggregate
- future compile-side semantic integration

## Target Contract

The next artifact should be an explicit provenance/source-map layer with these
invariants.

### 1. Stable Provenance Identity

Every compile-visible node or diagnostic origin should be able to answer:

- `origin_kind`: `parsed | generated`
- `owning_path`
- `origin_call_path?`
- `origin_call_span?`
- `origin_macro_def_path?`
- `origin_macro_def_span?`
- `generated_source?`

This is not yet a request for a global remapped `ExprId`.
It is only a request for a compile-authoritative provenance record.

### 2. One Formatting Contract

Formatter callers should not rebuild provenance ad hoc.
They should consume one context object that already knows:

- primary display path
- source map to use for snippet extraction
- related/secondary origin notes
- whether the primary span is generated or parsed

Current shadow diagnostic provenance context is the right local shape, but it
is not yet the compile-authoritative source of truth.

### 3. One Declaration-Origin Contract

Collector parity, semantic parity, and future compile integration should all
classify declaration provenance from the same source.

Minimum origin categories remain:

- `direct`
- `macro_expanded`

But the contract should not require shadow-only analyzer callbacks to recover
that classification.

### 4. Parser Diagnostic Identity

Parser parity should compare stable diagnostic identities, not only raw counts.

Minimum identity components:

- file path
- diagnostic code or normalized message
- normalized span

This remains parity telemetry at first.
It does not need to become a hard compile gate beyond the existing strict mode.

## Proposed Minimal Data Model

The next spike should introduce an explicit provenance record at the semantic
/ compile boundary, conceptually shaped like:

```crystal
record ProvenanceInfo,
  origin_kind : OriginKind,
  owning_path : String,
  primary_source : String?,
  origin_call_path : String?,
  origin_call_span : Frontend::Span?,
  origin_macro_def_path : String?,
  origin_macro_def_span : Frontend::Span?,
  generated_source : String?
```

And one formatting-facing wrapper:

```crystal
record DiagnosticProvenanceContext,
  display_path : String,
  sources_by_path : Hash(String, String),
  related_spans : Array(Frontend::RelatedSpan),
  secondary_spans : Array(Semantic::SecondarySpan)
```

Names can change.
What matters is the boundary: provenance lookup and provenance formatting should
stop being distributed responsibilities.

## Integration Plan

### Step A: Freeze the Boundary

Add one new compile-shadow-facing provenance API and route existing shadow
helpers through it without changing behavior.

Definition of done:

- current shadow output is unchanged
- `cli.cr` loses more provenance assembly glue
- aggregate/analyzer no longer expose multiple overlapping provenance helpers

### Step B: Unify Generated Origins

Move current generated origin fields into the explicit provenance record.

Definition of done:

- generated diagnostics
- generated declaration provenance
- generated ownership summaries

all read from the same provenance lookup object.

### Step C: Unify Parse Diagnostic Identity

Introduce a normalized parser diagnostic identity object and make
`parse_diag_gaps` compare identities, not only counts.

Definition of done:

- parser parity lines can report unique identities honestly
- strict mode still uses the same parity object

### Step D: Prepare for Compile-Authoritative Adoption

Document the remaining delta between:

- reparse aggregate provenance
- compile-authoritative provenance from real parsed units / macro expansion

This step is still observational.
It should end with a precise list of what must move out of the shadow-only
layer before the compile path can consume the same contract.

## Exit Criteria

This Phase 2.1 contract work is complete when:

- shadow formatting consumes one provenance context object
- generated declaration provenance consumes the same underlying provenance data
- parser parity compares normalized diagnostic identities
- `cli.cr` no longer rebuilds provenance from multiple partial inputs
- docs explicitly list the remaining gap between shadow-only provenance and a
  compile-authoritative source map

## Explicitly Deferred

These belong to later work, not to this spike:

- replacing the shared aggregate with a real compile graph
- remapping all nested `ExprId` through compile units
- lowering/HIR parity
- compile-authoritative macro expansion storage
- full compile diagnostic source-map replacement

## Why This Is the Right Size

This is the smallest next step that:

- keeps Phase 2 moving forward
- avoids reopening resolved shadow parity work
- does not pretend shadow provenance is already compile-authoritative
- gives the next engineer a bounded implementation target instead of a vague
  “source-map layer” idea
