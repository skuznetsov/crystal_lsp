# Compiler Refactor Architecture Plan

Status: Draft
Date: 2026-04-11
Scope: compile pipeline maintainability, LLVM IR emission, and HIR service
boundaries
Related: `PLAN_DEMAND_DRIVEN_REWRITE_RFC.md`, `docs/ast_to_hir_audit.md`,
`docs/codegen_architecture.md`

## 1. Purpose

This document captures a staged architecture plan for reducing the debugging
and maintenance cost of the Crystal V2 compiler without interrupting the
current runtime-stability work.

The goal is not to split files because they are large. The goal is to replace
implicit string/state coupling with explicit contracts:

- HIR lowering should expose narrow services for overload resolution, macro
  provenance, primitive lowering, and call lowering.
- LLVM emission should move from ad hoc string construction toward a typed,
  streaming writer that avoids large intermediate strings and stringly typed
  fixups.
- Rollout must keep current HIR/MIR/backend contracts stable until shadow
  checks prove equivalence.

## 2. Current Pain Points

### 2.1 HIR lowering

`src/compiler/hir/ast_to_hir.cr` is a single stateful class that currently owns
too many responsibilities:

- declaration collection and registration
- macro expansion and macro-origin/provenance handling
- type lookup and type inference shortcuts
- overload lookup and callsite specialization
- primitive and union lowering
- RTA/lazy lowering and pending-function queues
- HIR instruction emission
- many debug and bootstrap workarounds

This creates poor debugging locality. A bug that appears as a runtime stub can
originate from overload registration, macro provenance, type substitution, or a
late pending-function replay. The code allows local fixes that are correct for a
single symptom but fragile for the surrounding pipeline.

### 2.2 LLVM backend

`src/compiler/mir/llvm_backend.cr` still mixes:

- type mapping
- string formatting for LLVM IR
- function emission scheduling
- call argument coercion
- debug metadata generation
- backend-synthesized runtime shims
- parallel worker output merging
- textual post-fixups such as `fixup_call_arg_types(args : String)`

The project already hit a verified self-host failure from intermediate LLVM
header string construction: `Array(String)#join` in function header emission
caused a `Negative capacity` failure in stage2. The local fix streamed function
headers directly into the output buffer. That fix is the right pattern to
generalize.

### 2.3 Textual `.ll` as the only boundary

The current backend often treats LLVM IR as plain text too early. This makes
the code easy to print, but hard to validate:

- call arguments become `"TYPE VALUE"` strings before the backend has finished
  checking type/value consistency
- phi incoming lists and parameter lists are assembled through generic
  containers and joins
- debug metadata and normal IR write into the same textual surface
- malformed IR is frequently caught late by `opt`/`llc`, or by runtime behavior
  after a coercion happened in the wrong string branch

The desired direction is not a full in-memory LLVM AST. The desired direction is
a compact typed streaming layer that validates small fragments before writing
text.

## 3. Non-Goals

- Do not rewrite the whole compiler while runtime benchmark frontiers are still
  moving.
- Do not change stdlib files.
- Do not switch the compile path to the demand-driven semantic pipeline in this
  plan. That is covered by `PLAN_DEMAND_DRIVEN_REWRITE_RFC.md`.
- Do not build a full persistent LLVM AST for the entire module. That would add
  memory pressure and duplicate LLVM's own IR model.
- Do not replace LLVM text emission with the LLVM C API in this plan. That may
  become a later option, but it is not required to solve the current
  maintainability and self-host fragility problems.

## 4. Target Architecture

### 4.1 HIR service boundaries

The first HIR target is to make the existing pipeline more modular without
changing its external behavior.

Proposed service extraction order:

1. `MethodNameCodec`
   - Owns mangling/demangling and method-name part parsing.
   - Removes repeated method-name parsing from resolver and backend-adjacent
     code.
   - Exit gate: old and new codecs produce identical normalized names for the
     regression corpus.

2. `OverloadSelector`
   - Owns overload candidate filtering by receiver, arg types, named args,
     block presence, and concrete suffix preservation.
   - Must not emit HIR or mutate pending-function queues.
   - Exit gate: targeted overload regressions and a traceable candidate ledger.

3. `MacroExpansionRegistry`
   - Owns macro expansion provenance, source path retention, and generated
     owner mapping.
   - Must expose a canonical "generated def identity" instead of pathless or
     arena-local fallbacks.
   - Exit gate: macro-body and module-path regressions plus debug-source
     identity checks.

4. `PrimitiveLowerer`
   - Owns primitive operations, primitive-union binary ops, casts, and unsafe
     reinterpretation lowering.
   - Must be independent from overload lookup except for a small typed
     `PrimitiveCall` input.
   - Exit gate: primitive union, float unsafe-as, integer formatting, and
     numeric benchmark reducers.

5. `CallLowerer`
   - Owns final callsite emission after method resolution has produced a
     typed target.
   - Must not redo overload selection from string names.
   - Exit gate: callsite target set is stable against legacy path in shadow
     comparison.

Extraction should happen by vertical slices. A file split without reducing the
state surface is not an improvement.

### 4.2 Typed streaming LLVM writer

Introduce a backend-internal writer layer that still emits `.ll` text, but does
not force all intermediate state into strings.

Suggested types:

```crystal
module Crystal::MIR::LLVMText
  struct LlType
    getter text : String
  end

  struct LlValue
    getter type : LlType
    getter name : String
  end

  struct LlParam
    getter type : LlType
    getter name : String?
  end

  struct LlArg
    getter type : LlType
    getter value : String
  end

  abstract class LlSink
    abstract def write(bytes : Bytes) : Nil
    abstract def write(text : String) : Nil
  end

  class LlWriter
    def function_header(return_type : LlType, name : String, params : Slice(LlParam))
    end

    def call(result : String?, return_type : LlType, callee : String, args : Slice(LlArg))
    end

    def phi(result : String, type : LlType, incoming : Slice(Tuple(String, String)))
    end
  end
end
```

This layer should keep the current `.ll` output format. The difference is that
the backend builds typed fragments and streams them directly, rather than
joining arrays of strings and then parsing those strings again.

### 4.3 Compact `.ll` representation

The recommended compact representation is per-function and stream-oriented:

- Keep MIR as the durable compact IR.
- For LLVM output, keep only the current function's typed emission state in
  memory.
- Write function bodies to a sink as soon as their dependency set is known.
- Let parallel workers produce bounded shards and merge them through a typed
  section manifest, not through ad hoc string concatenation.
- Keep metadata sections append-only and explicit; do not interleave metadata
  construction with normal instruction formatting unless the writer API makes
  that relationship visible.

Avoid a full-module LLVM object graph unless there is a specific optimization
or verification pass that cannot be expressed on MIR plus per-function LLVM
fragments.

## 5. Rollout Plan

### Phase 0: Contracts and metrics

Purpose: make the current behavior observable before refactoring.

Tasks:

- Add or update docs for backend output contracts:
  - valid LLVM value names
  - argument coercion rules
  - union payload conventions
  - debug metadata attachment rules
  - section ordering for globals, declarations, functions, metadata
- Add counters for:
  - total `emit` calls
  - total `emit_raw` calls
  - bytes emitted by family
  - number of call-arg fixups
  - number of string joins still used in backend hot paths
- Add a grep-friendly `CRYSTAL_V2_LLVM_WRITER_TRACE=1` trace for one function.

Exit criteria:

- No behavior changes.
- Existing regression suite is not worse.
- One representative benchmark compile produces a small writer metrics report.

### Phase 1: Function header and declaration writer

Purpose: generalize the already verified "stream function headers" pattern.

Tasks:

- Add `LLVMText::LlWriter` and route only function headers through it.
- Route forward declarations and intrinsic declarations through the same writer.
- Replace `param_types.join(", ")` patterns in function/declaration headers.

Exit criteria:

- Stage1 compiler build passes.
- Existing stage2 smoke that previously moved past `Negative capacity` still
  moves past that point.
- Emitted `.ll` diff is normalized-equivalent for headers and declarations.

### Phase 2: Typed call arguments

Purpose: remove the most fragile stringly typed call boundary.

Tasks:

- Replace `args : String` in call emission with `Array(LlArg)` or a small
  stack-friendly builder.
- Convert `fixup_call_arg_types(args : String)` into typed validation and typed
  coercion before final formatting.
- Make conversion paths return `LlArg`, not `"TYPE VALUE"` strings.

Exit criteria:

- No `split(", ")` or reparsing of call argument strings in backend call
  emission.
- Float/int/pointer/union call coercion reducers pass.
- `tmp/repro_ryu_fixed_direct.cr` and benchmark formatting reducers remain
  green before and after this phase.

### Phase 3: Phi, parameter, and metadata list writer

Purpose: remove the next highest-risk join-heavy IR lists.

Tasks:

- Route phi incoming lists through `LlWriter#phi`.
- Route debug metadata reference arrays through bounded writer helpers where
  practical.
- Preserve deterministic ordering for diffability.

Exit criteria:

- No regression in debug-info emission.
- `--debug` builds keep stepping behavior no worse than baseline.
- Large metadata arrays avoid per-element `DIDerivedType` string explosion when
  a compact `DIArray`/subrange representation is available and semantically
  valid.

### Phase 4: Backend section manifest

Purpose: make parallel worker output merging explicit.

Tasks:

- Introduce a small `LlSectionManifest` for declarations, globals, functions,
  metadata, duplicate constants, and synthesized runtime shims.
- Make each worker return a manifest plus bounded text shards.
- Merge sections in one deterministic place.

Exit criteria:

- Parallel and single-worker LLVM output are normalized-equivalent.
- Worker output no longer relies on implicit string concatenation order.
- `CRYSTAL_V2_LLVM_WORKERS=1` and default worker count both pass selected
  compile/run reducers.

### Phase 5: HIR service extraction

Purpose: reduce HIR debugging surface after the backend writer is safer.

Tasks:

- Extract `MethodNameCodec`.
- Extract `OverloadSelector` behind a legacy-compatible facade.
- Extract `MacroExpansionRegistry`.
- Extract `PrimitiveLowerer`.
- Extract `CallLowerer`.

Exit criteria:

- Each extraction is one logical commit.
- Each service has focused regression coverage and at least one traceable
  reducer.
- Legacy and extracted paths can be compared with a normalized shadow check
  before deleting old code.

## 6. Verification Strategy

Every phase must define a small acceptance gate before implementation.

Minimum gates:

- compiler host build:
  - `crystal build src/crystal_v2.cr -o /tmp/cv2_refactor_gate --error-trace`
- safe compile/run for selected reducers:
  - `scripts/run_safe.sh /tmp/cv2_refactor_gate ...`
- normalized `.ll` comparison for the touched backend surface
- at least one negative/adversary check:
  - run the same reducer with `CRYSTAL_V2_LLVM_WORKERS=1`
  - run with debug metadata enabled if the phase touches metadata
  - compare against original Crystal for user-visible runtime formatting

Do not label a phase complete if only the `.ll` file is syntactically emitted.
For runtime-visible codegen changes, a produced binary must run through
`scripts/run_safe.sh`.

## 7. Migration Rules

- Keep one logical change per commit.
- Keep the old path until the new path has a shadow or normalized equivalence
  gate.
- Prefer writer APIs that accept slices or small stack-friendly builders over
  `Array(String)#join`.
- Do not add broad stubs to make a reducer pass.
- Do not introduce new global mutable state in service extraction. If a service
  needs state, make it explicit in a context object.
- Do not move code across files without either reducing state coupling or adding
  a testable contract.

## 8. First Implementation Candidate

The safest first implementation candidate is a backend-only writer slice:

1. Create `src/compiler/mir/llvm_text_writer.cr`.
2. Add `LlType`, `LlValue`, `LlParam`, `LlArg`, and `LlWriter`.
3. Route only `emit_function_definition_header(...)` through `LlWriter`.
4. Preserve current textual output byte-for-byte except for harmless whitespace
   if the normalized comparison allows it.
5. Add one regression or script-level check that exercises a function with:
   - zero params
   - multiple params
   - debug metadata enabled
   - entry function opt guard attributes

This is intentionally small. It proves the typed writer seam without changing
call coercion, metadata ownership, or worker merging.

## 9. Decision Summary

Recommended direction:

- Use MIR as the compact durable IR.
- Use typed per-function streaming for `.ll` emission.
- Extract HIR services by behavior boundary, not by line count.
- Keep the demand-driven semantic rewrite as the strategic long-term track.
- Start with the LLVM writer because it has a verified failure pattern,
  relatively narrow scope, and low interaction with stdlib/runtime behavior.

This plan should be revisited after the current benchmark formatter frontier is
green, because that frontier will likely add another concrete backend or
macro-provenance contract to preserve during refactoring.
