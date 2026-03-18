# Crystal V2 Bootstrap — TODO (Updated 2026-03-18)

## Current State
- **Branch**: `bootstrap-benchmark`
- **Latest committed baseline**: `781b7f28` — restore fresh self-hosted stage2 on nameprio branch
- **Working tree**:
  - uncommitted parser rewind hardening in `src/compiler/frontend/parser.cr`
  - unrelated local diffs in `src/compiler/mir/hir_to_mir.cr` and `src/crystal_v2.cr` must stay out of the next commit
- **Fresh release stage1 (current tree)**: `/Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_funlookahead`
- **Fresh release stage2 (current tree)**: `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_funlookahead_fresh`
- **Current timings**:
  - original Crystal -> fresh `stage1_release_funlookahead`: `544.95s`
  - fresh `stage1_release_funlookahead` -> fresh `stage2_release_funlookahead_fresh`: `174.80s`
  - previous fresh self-hosted release stage2 checkpoint (`stage2_release_nameprio_fresh`): `164.03s`
- **Compiler parse-only status**: fresh `stage2_release_funlookahead_fresh` survives `CRYSTAL_V2_STOP_AFTER_PARSE=1 src/crystal_v2.cr --release` `3/3`
- **Stage3 bootstrap**: **FAILS** after `1.06s` with `status=139` on `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_funlookahead_fresh`
- **Current smallest clean/red HIR controls**:
  - `--release --no-prelude /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_simple_one.cr` is green in `0.02s`
  - `--release --no-prelude src/stdlib/prelude.cr` hits wrapper memory cap in `14.49s`
- **Benchmark status**: blocked — stage2 compiler is still unstable and crashes before finishing stage3

### Completed In This Cycle
1. **Parser rewind hardening moved the active frontier past full-compiler parse-only**
   - `src/compiler/frontend/parser.cr` now restores `@previous_token` together with `@index` inside `lookahead_for_arrow?`, including the early `::Foo` false return, the generic false return, and the speculative trailing `.class` rewind
   - full compiler parse-only on the fresh stage2 is now clean:
     ```bash
     for i in 1 2 3; do
       env CRYSTAL_V2_STOP_AFTER_PARSE=1 \
         /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_funlookahead_fresh \
         src/crystal_v2.cr --release \
         -o /Users/sergey/Projects/Crystal/.codex_artifacts/stage3_funlookahead_parse_only_$i
     done
     ```
     Result: `rc=0, 0, 0`

2. **Fresh release bootstrap remains green with the parser rewind patch**
   - fresh original-Crystal rebuild:
     ```bash
     /usr/bin/time -p scripts/timeout_sample_lldb.sh -t 5400 -m 40960 -s 8 -l 20 -n 12 --no-series \
       -o /Users/sergey/Projects/Crystal/.codex_artifacts/logs/stage1_release_funlookahead_timeout \
       -- crystal build src/crystal_v2.cr --release --error-trace \
       -o /Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_funlookahead
     ```
     Result: `status=0`, `real 544.95s`
   - fresh self-hosted rebuild from that new stage1:
     ```bash
     /usr/bin/time -p env CRYSTAL_CACHE_DIR_STAGE2_RELEASE=/Users/sergey/Projects/Crystal/.codex_artifacts/cache_stage2_release_funlookahead_fresh \
       CRYSTAL_V2_PIPELINE_CACHE=0 CRYSTAL_V2_LLVM_CACHE=0 \
       scripts/timeout_sample_lldb.sh -t 1800 -m 40960 -s 8 -l 20 -n 12 --no-series \
       -o /Users/sergey/Projects/Crystal/.codex_artifacts/logs/stage2_release_funlookahead_fresh_timeout \
       -- scripts/build_stage2_release.sh \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_funlookahead \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_funlookahead_fresh
     ```
     Result: `status=0`, `real 174.80s`

3. **The reduced parser-body oracle remains green on the fresh stage2**
   - command:
     ```bash
     bash regression_tests/stage2_block_body_exprid_parser_repro.sh \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_funlookahead_fresh
     ```
     Result: `exit 0` / `not reproduced`

4. **The active crash frontier moved again, from whole-compiler parse-only into stdlib/prelude HIR registration**
   - the smallest green HIR control is now the truly trivial no-prelude file:
     ```bash
     env CRYSTAL_V2_STOP_AFTER_HIR=1 \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_funlookahead_fresh \
       --release --no-prelude /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_simple_one.cr \
       -o /Users/sergey/Projects/Crystal/.codex_artifacts/funlookahead_hir_simple_noprelude_out
     ```
     Result: `status=0`, `real 0.02s`
   - `src/stdlib/prelude.cr` is now the smallest confirmed red stdlib HIR control:
     ```bash
     /usr/bin/time -p env CRYSTAL_V2_STOP_AFTER_HIR=1 scripts/timeout_sample_lldb.sh \
       -t 120 -m 8192 -s 5 -l 10 -n 8 --no-series \
       -o /Users/sergey/Projects/Crystal/.codex_artifacts/logs/funlookahead_hir_prelude_noprelude \
       -- /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_funlookahead_fresh \
       --release --no-prelude src/stdlib/prelude.cr \
       -o /Users/sergey/Projects/Crystal/.codex_artifacts/funlookahead_hir_prelude_noprelude_out
     ```
     Result: `status=125`, `real 14.49s`
   - bounded LLDB/sample on the red prelude control shows `Parser#initialize -> Lexer#next_token/lex_identifier` above hundreds of repeated `AstToHir#register_module(...)+1492` frames, with hotspots still dominated by `macro_token_text_slice`, `parse_macro_body`, and lexer identifier work

5. **Stage3 remains blocked, but after the parser corridor**
   - command:
     ```bash
     /usr/bin/time -p env CRYSTAL_CACHE_DIR_STAGE2_RELEASE=/Users/sergey/Projects/Crystal/.codex_artifacts/cache_stage3_release_funlookahead_fresh \
       CRYSTAL_V2_PIPELINE_CACHE=0 CRYSTAL_V2_LLVM_CACHE=0 \
       scripts/timeout_sample_lldb.sh -t 1800 -m 40960 -s 8 -l 20 -n 12 --no-series \
       -o /Users/sergey/Projects/Crystal/.codex_artifacts/logs/stage3_release_funlookahead_fresh_timeout \
       -- scripts/build_stage2_release.sh \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_funlookahead_fresh \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage3_release_funlookahead_fresh
     ```
     Result: `status=139`, `real 1.06s`

---

## Task 1: Stabilize the remaining stage2 self-hosted crash [ACTIVE]
**Priority: HIGH — blocks stage3 bootstrap and real stage1-vs-stage2 benchmark**

### Problem
The current `ast_to_hir` branch restores a full fresh `stage1 -> stage2` release bootstrap, but the resulting fresh stage2 compiler is still unstable when it compiles the full compiler source tree:
- reduced parser-body oracle remains green
- fresh `stage2_release_funlookahead_fresh` is buildable and reaches LLVM backend during bootstrap
- `src/crystal_v2.cr` no longer dies in plain parse-only mode
- HIR-bounded no-prelude compile of `1` is clean, so the active red path now lives inside stdlib/prelude registration
- stage3 still fails before a benchmark can be taken

### Current Evidence
1. **Fresh current-tree bootstrap is green again**
   ```bash
   /usr/bin/time -p scripts/timeout_sample_lldb.sh -t 5400 -m 40960 -s 8 -l 20 -n 12 --no-series \
     -o /Users/sergey/Projects/Crystal/.codex_artifacts/logs/stage1_release_funlookahead_timeout \
     -- crystal build src/crystal_v2.cr --release --error-trace \
     -o /Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_funlookahead
   /usr/bin/time -p env CRYSTAL_CACHE_DIR_STAGE2_RELEASE=/Users/sergey/Projects/Crystal/.codex_artifacts/cache_stage2_release_funlookahead_fresh \
     CRYSTAL_V2_PIPELINE_CACHE=0 CRYSTAL_V2_LLVM_CACHE=0 \
     scripts/timeout_sample_lldb.sh -t 1800 -m 40960 -s 8 -l 20 -n 12 --no-series \
     -o /Users/sergey/Projects/Crystal/.codex_artifacts/logs/stage2_release_funlookahead_fresh_timeout \
     -- scripts/build_stage2_release.sh \
     /Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_funlookahead \
     /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_funlookahead_fresh
   ```
   Result: `stage1 real 544.95s`, `stage2 real 174.80s`, both `status=0`

2. **Reduced parser-body oracle stays green on the fresh stage2**
   ```bash
   bash regression_tests/stage2_block_body_exprid_parser_repro.sh \
     /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_funlookahead_fresh
   ```
   Result: `exit 0` / `not reproduced`

3. **Plain parse-only on the full compiler source is now green**
   ```bash
   for i in 1 2 3; do
     env CRYSTAL_V2_STOP_AFTER_PARSE=1 \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_funlookahead_fresh \
       src/crystal_v2.cr --release \
       -o /Users/sergey/Projects/Crystal/.codex_artifacts/stage3_funlookahead_parse_only_$i
   done
   ```
   Result: `rc=0, 0, 0`

4. **The smallest clean/red HIR controls are now separated cleanly**
   ```bash
   env CRYSTAL_V2_STOP_AFTER_HIR=1 \
     /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_funlookahead_fresh \
     --release --no-prelude /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_simple_one.cr \
     -o /Users/sergey/Projects/Crystal/.codex_artifacts/funlookahead_hir_simple_noprelude_out
   env CRYSTAL_V2_STOP_AFTER_HIR=1 \
     /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_funlookahead_fresh \
     --release --no-prelude src/stdlib/prelude.cr \
     -o /Users/sergey/Projects/Crystal/.codex_artifacts/funlookahead_hir_prelude_noprelude_out
   ```
   Result: trivial no-prelude file exits `0` in `0.02s`; `prelude.cr` hits memory cap with wrapper `status=125` in `14.49s`

5. **Bounded LLDB/sample on the current red path points at prelude/stdlib module registration**
   ```bash
   /usr/bin/time -p env CRYSTAL_V2_STOP_AFTER_HIR=1 scripts/timeout_sample_lldb.sh \
     -t 120 -m 8192 -s 5 -l 10 -n 8 --no-series \
     -o /Users/sergey/Projects/Crystal/.codex_artifacts/logs/funlookahead_hir_prelude_noprelude \
     -- /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_funlookahead_fresh \
     --release --no-prelude src/stdlib/prelude.cr \
     -o /Users/sergey/Projects/Crystal/.codex_artifacts/funlookahead_hir_prelude_noprelude_out
   ```
   Result: LLDB shows `Parser#initialize -> Lexer#next_token/lex_identifier` above repeated `AstToHir#register_module(...)+1492`; sample/hotspots stay dominated by `macro_token_text_slice`, `parse_macro_body`, and lexer identifier work

6. **Stage3 self-bootstrap is still blocked**
   ```bash
   /usr/bin/time -p env CRYSTAL_CACHE_DIR_STAGE2_RELEASE=/Users/sergey/Projects/Crystal/.codex_artifacts/cache_stage3_release_funlookahead_fresh \
     CRYSTAL_V2_PIPELINE_CACHE=0 CRYSTAL_V2_LLVM_CACHE=0 \
     scripts/timeout_sample_lldb.sh -t 1800 -m 40960 -s 8 -l 20 -n 12 --no-series \
     -o /Users/sergey/Projects/Crystal/.codex_artifacts/logs/stage3_release_funlookahead_fresh_timeout \
     -- scripts/build_stage2_release.sh \
     /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_funlookahead_fresh \
     /Users/sergey/Projects/Crystal/.codex_artifacts/stage3_release_funlookahead_fresh
   ```
   Result: `status=139`, `real 1.06s`

### What To Debug Next
1. Keep `regression_tests/stage2_block_body_exprid_parser_repro.sh` as the cheap parser-only control for future parser experiments
2. Treat the current parser rewind patch as a separate boundary-shift commit; do **not** mix it with the next HIR/module experiment
3. Use `CRYSTAL_V2_STOP_AFTER_HIR=1 --release --no-prelude src/stdlib/prelude.cr` as the new smallest stdlib red control and `... stage2_simple_one.cr --no-prelude` as the new green control
4. Narrow the failing `require` chain inside `src/stdlib/prelude.cr` before returning to another full stage3

---

## Task 2: Finish benchmark once stage3 is stable [BLOCKED on Task 1]
Goal:
1. stage1 compiler builds stage2
2. stage2 compiler builds stage3
3. compare stage2-build time vs stage3-build time

Current measurable data:
- fresh stage1 compiler -> fresh stage2 compiler: `174.80s`
- stage2 compiler -> stage3 compiler: **not yet measurable** (current binary crashes after `1.06s`)

---

## Task 3: Finish the remaining enum hardening sweep if the crash points back there [BACKLOG]
Subagent review found that the obvious top-level enum reads are now source-first, but the enum corridor is not fully clean yet:
- `enum_name_from_node`, `enum_base_type_name_from_node`, and `enum_member_name_from_node` still contain `safe_slice_to_string(...)` fallbacks
- `resolve_enum_member_value` still has original-AST value fallbacks
- some lazy / macro / lowering-time enum registration sites still build names from slice-backed fields

This is useful follow-up only if the new self-hosted crash walks back into enum registration. Do not mix it into the active parse-heisenbug investigation without new evidence.

---

## Task 4: Revisit union/layout inconsistency after bootstrap stability [BACKLOG]
Historical note from the previous frontier:
- `union_all_reference_types?` / HIR-vs-MIR all-ref-union handling is still conceptually inconsistent
- changing HIR struct handling still regresses `array_concat_string_runtime` and `test_generics_stack`
- current bootstrap blocker is no longer that path, so do **not** mix that work into the active stage2 crash investigation unless new evidence points back there

---

## Build Commands Reference
```bash
# Build stage1 (Crystal compiling V2 compiler)
crystal build src/crystal_v2.cr -o bin/crystal_v2 --error-trace

# Build stage2 (V2 compiler compiling itself)
bin/crystal_v2 src/crystal_v2.cr

# Run regression tests
bash regression_tests/run_all.sh          # 69 individual tests
bash regression_tests/run_all_suites.sh   # both suites (69 + 20 combined)

# Run single test safely
bin/crystal_v2 /tmp/test_hello.cr
scripts/run_safe.sh /tmp/test_hello 5 512

# Generate LLVM IR for comparison
CRYSTAL_V2_EMIT_IR=/tmp/test_ir.ll bin/crystal_v2 some_test.cr
```

## Key Files
- `src/compiler/hir/ast_to_hir.cr` — HIR lowering (72k lines, main file being modified)
  - `union_all_reference_types?` at ~line 27409
  - `field_storage_size` at ~line 27532
  - `type_size` at ~line 27323
  - `align_all_class_ivars` at ~line 19324
  - `hir_union_ivar_storage_size` at ~line 27442
- `src/compiler/mir/hir_to_mir.cr` — MIR lowering
  - `container_elem_storage_size_u64` at line 722
  - `lower_pointer_add` at line 5220
  - `lower_pointer_load` at line 5158
  - `lower_pointer_store` at line 5184
  - `register_class_types` at line 391
  - `all_ref_union_descriptor?` at line 673
  - `convert_type` at line 5482 (HIR→MIR type ID mapping, +20 offset for user types)
- `src/compiler/mir/llvm_backend.cr` — LLVM IR emission
  - `container_elem_storage_size_u64` at line 815
  - `emit_gep_dynamic` at line 11046 (use_byte_gep logic at line 11178)
  - `is_all_ref_union?` at line 128

## IMPORTANT RULES
- **Fix root causes, NOT symptoms** — no hardcoding, no workarounds
- **One feature/bugfix = one commit**
- **NEVER modify stdlib files** — must be 100% compatible with original Crystal stdlib
- **Always test**: `scripts/run_safe.sh <binary> <timeout> <max_mem_mb>` (NEVER run directly)
- **V2 ABI**: ALL Crystal structs are heap-allocated as pointers. C lib structs are inlined.
