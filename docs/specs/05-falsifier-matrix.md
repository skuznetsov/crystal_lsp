# Crystal V2 Falsifier Matrix

> Status: Draft v0.1, 2026-05-08.
> Companion to: `docs/specs/*.md`, `TODO.md`, `LANDMARKS.md`.
> Format: each row maps a normative claim to the smallest known falsifier.

## 1. Status Legend

- `[FALSIFIABLE]`: guard exists or the falsifier command is explicit.
- `[FRONTIER]`: known boundary; not yet fixed.
- `[MISSING-FALSIFIER]`: claim is important but lacks a narrow guard.
- `[REFUTED]`: branch tried and recorded as not a valid fix.

Each non-refuted row has a phase pressure:

- `current`: must be resolved before claiming the active frontier fixed.
- `next-touch`: must be resolved when code in that contract family changes.
- `pre-s2-clean`: must be resolved before declaring `s1 -> s2b` clean.
- `later`: useful but not on the active bootstrap gate.

`[MISSING-FALSIFIER]` rows without phase pressure are invalid.

## 2. Bootstrap Corridor

| ID | Claim | Source | Smallest Falsifier | Phase | Status |
|----|-------|--------|--------------------|-------|--------|
| B1 | Produced stage must be built through `scripts/run_safe.sh`. | `00-bootstrap-contract.md` section 4 | Review/CI command check; direct produced binary execution is protocol violation. | current | [FALSIFIABLE] |
| B2 | A moved frontier is acceptable only with a guard and a named residual boundary. | `00-bootstrap-contract.md` section 5 | Commit lacks guard or TODO/LANDMARK boundary for a claimed fix. | current | [FALSIFIABLE] |
| B3 | Original-vs-stage semantic oracle is required when a change touches language behavior. | `00-bootstrap-contract.md` section 3.1 | A semantic change has only stage1-vs-s2 evidence and no original comparison or stated semantic-line oracle. | next-touch | [MISSING-FALSIFIER] |

## 3. HIR Name Resolution

| ID | Claim | Source | Smallest Falsifier | Phase | Status |
|----|-------|--------|--------------------|-------|--------|
| H1 | Qualified nested names must not duplicate owner segments. | `01-hir-name-resolution.md` section 2 | `regression_tests/p2_qualified_module_namespace_no_prelude.sh <compiler>` finds `Float::Float::ParsedNumberStringT` or duplicated `Float::FastFloat`. | current | [FALSIFIABLE] |
| H2 | Self-reopen wrappers must not recursively register the current owner. | `01-hir-name-resolution.md` section 2.2, LM-553 | `regression_tests/p2_self_nested_module_registration_frontier.sh <compiler>`. | current | [FALSIFIABLE] |
| H3 | Nested builtin annotations must remain top-level unless structurally nested. | `01-hir-name-resolution.md` section 3, LM-554 | `regression_tests/p2_full_prelude_generic_template_namespace_no_pollution.sh <compiler>`. | current | [FALSIFIABLE] |
| H4 | Type-literal name queries lower to literal strings, not static stubs. | `01-hir-name-resolution.md` section 4, LM-558 | `regression_tests/p2_type_literal_name_query_no_stub.sh <compiler>`. | current | [FALSIFIABLE] |
| H5 | Function body presence must distinguish real bodies from stubs. | `01-hir-name-resolution.md` section 6 | Add no-prelude HIR/MIR guard that registers a bodyless function and proves downstream stages do not treat it as emitted. | next-touch | [MISSING-FALSIFIER] |

## 4. Generic Template Registration

| ID | Claim | Source | Smallest Falsifier | Phase | Status |
|----|-------|--------|--------------------|-------|--------|
| G1 | Generic/container fixes must not use arbitrary depth caps. | `02-generic-template-registration.md` section 2 | Add a deep nested tuple/hash/array/proc oracle before changing demand or registration pruning code. Static diff review alone is not enough. | next-touch | [MISSING-FALSIFIER] |
| G2 | Empty or repeated generic owner names such as `Iterator::` or `Indexable::Indexable::...` are invalid. | `02-generic-template-registration.md` section 3 | Add trace/IR guard that fails on empty owner suffixes or repeated adjacent owner segments in generated stage. | next-touch | [MISSING-FALSIFIER] |
| G3 | Generic template and instance keys must be semantic keys, not rendered strings. | `02-generic-template-registration.md` section 3 | Add key-rendering oracle that compares canonical key fields against rendered names for nested generic owners. | pre-s2-clean | [MISSING-FALSIFIER] |
| G4 | Broad source-gated generic-template body scan is not an acceptable fix. | `02-generic-template-registration.md` section 5 | Reverted experiment regressed earlier around `Crystal::PointerLinkedList` / trace paths. | current | [REFUTED] |
| G5 | Produced `s2` full-prelude `puts 42` must get past the later generic/template registration frontier. | `TODO.md`, LM-559 | `CRYSTAL_V2_TRACE_CLASS_FRONTIER=1 scripts/run_safe.sh <produced-s2> 60 4096 /tmp/hello.cr -o /tmp/hello_bin`. | pre-s2-clean | [FRONTIER] |

## 5. MIR Call ABI

| ID | Claim | Source | Smallest Falsifier | Phase | Status |
|----|-------|--------|--------------------|-------|--------|
| M1 | Exact static calls lower before receiver mutation. | `03-mir-call-abi.md` section 3 | `regression_tests/p2_stage2_static_call_named_llvm_no_prelude.sh <compiler>` emits fallback `@func1` or wrong arity. | current | [FALSIFIABLE] |
| M2 | Receiver calls include a runtime receiver; static calls do not. | `03-mir-call-abi.md` section 4 | Add MIR shape check for the static-call reducer: named callee, callee arity equals args, no synthetic receiver. | next-touch | [MISSING-FALSIFIER] |
| M3 | Null/missing HIR `TypeRef` is not an ordinary runtime object. | `03-mir-call-abi.md` section 6 | Add no-prelude MIR oracle covering a null `TypeRef` conversion path. | pre-s2-clean | [MISSING-FALSIFIER] |
| M4 | Debug value-location metadata is opt-in during bootstrap and not semantic. | `03-mir-call-abi.md` section 7 | Build with metadata disabled and verify semantic guards still pass. | current | [FALSIFIABLE] |

## 6. LLVM Emission

| ID | Claim | Source | Smallest Falsifier | Phase | Status |
|----|-------|--------|--------------------|-------|--------|
| L1 | LLVM backend must resolve MIR `FunctionId` to the named callee. | `04-llvm-emission.md` section 2, LM-559 | `regression_tests/p2_stage2_static_call_named_llvm_no_prelude.sh <compiler>` rejects `@func1`. | current | [FALSIFIABLE] |
| L2 | LLVM calls must not have empty return type spelling. | `04-llvm-emission.md` section 3, LM-559 | Same guard rejects `call  @`. | current | [FALSIFIABLE] |
| L3 | Emitted IR for the static-call reducer must pass `llc` when available. | `04-llvm-emission.md` section 6 | Same guard runs `llc -filetype=obj` if `llc` exists. | current | [FALSIFIABLE] |
| L4 | Hardcoded IO overrides should use generated accessors instead of unstable offsets. | `04-llvm-emission.md` section 4 | Add guard that changes/observes fd ivar layout and verifies IO overrides still emit correct fd load. | next-touch | [MISSING-FALSIFIER] |

## 7. CLI Output

| ID | Claim | Source | Smallest Falsifier | Phase | Status |
|----|-------|--------|--------------------|-------|--------|
| C1 | Emit-only success does not prove normal binary output success. | `06-cli-output-contract.md` section 2 | Same reducer passes `--emit llvm-ir --no-link` but normal `-o <bin>` exits 139. | current | [FALSIFIABLE] |
| C2 | Post-LLVM tail fixes must localize crash after LLVM finalization. | `06-cli-output-contract.md` section 4, LM-564 | `regression_tests/p2_stage2_cli_output_tail_no_prelude.sh <compiler>`; a future tail fix lacking the section 7 localization log is invalid evidence. | current | [FALSIFIABLE] |
| C3 | Binary-output fix must verify adjacent modes. | `06-cli-output-contract.md` section 5 | A commit claims binary-output fix with only `--emit llvm-ir --no-link` evidence. | current | [FALSIFIABLE] |

## 8. Refuted Branches

| ID | Branch | Evidence | Status |
|----|--------|----------|--------|
| R1 | Source-first generic type-param extraction as a broad fix. | Did not move the Float/ParsedNumberStringT frontier. | [REFUTED] |
| R2 | Caching `node.body` as a broad generic-template registration fix. | Did not move the frontier. | [REFUTED] |
| R3 | Source-gating generic-template nested-type body scan. | Failed earlier around `Crystal::PointerLinkedList` / trace paths. | [REFUTED] |
| R4 | Re-enabling source-backed top-level return annotations after LM-558. | Regressed produced `s2` full-prelude `puts 42` to earlier class registration crash around `class register idx=51/104`. | [REFUTED] |
