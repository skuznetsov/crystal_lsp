# Crystal V2 Bootstrap — TODO (Updated 2026-03-18)

## Current State
- **Branch**: `bootstrap-benchmark`
- **Latest committed baseline**: `24bf5d7c` — restore parser rewind token state
- **Working tree**:
  - unrelated local diffs in `src/compiler/mir/hir_to_mir.cr` and `src/crystal_v2.cr` must stay out of the next commit
- **Fresh release stage1 (current tree)**: `/Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_funlookahead`
- **Fresh release stage2 (current tree)**: `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_funlookahead_fresh`
- **Previous local stage2 checkpoint (class reparse fallback)**: `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_reparse_class_clean`
- **Current local stage2 candidate (macro-body piece capacity 128)**: `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_macro_piececap128`
- **Current timings**:
  - original Crystal -> fresh `stage1_release_funlookahead`: `544.95s`
  - fresh `stage1_release_funlookahead` -> fresh `stage2_release_funlookahead_fresh`: `174.80s`
  - previous fresh self-hosted release stage2 checkpoint (`stage2_release_nameprio_fresh`): `164.03s`
  - fresh `stage1_release_funlookahead` -> current local `stage2_release_macro_piececap128`: `177.18s`
- **New regression surface**:
  - `bash regression_tests/stage2_full_compiler_parse_only_repro.sh <compiler>`
  - `bash regression_tests/stage2_object_hir_noprelude_repro.sh <compiler>`
  - `bash regression_tests/stage2_nested_macro_method_missing_repro.sh <compiler>`
  - `bash regression_tests/stage2_reparsed_module_wrapper_repro.sh <compiler>`
  - `bash regression_tests/stage2_require_boehm_noprelude_parse_repro.sh <compiler>`
- **Compiler parse-only status**:
  - baseline `stage2_release_nameprio_fresh`: `rc=0,138,138,138,138`
  - fresh `stage2_release_funlookahead_fresh`: `rc=0,0,0,0,0`
- **Current focused parser boundary**:
  - `bash regression_tests/stage2_require_boehm_noprelude_parse_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_funlookahead` -> `exit 0` / `not reproduced`
  - `bash regression_tests/stage2_require_boehm_noprelude_parse_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_reparse_class_clean` -> `exit 1` / wrapper `status=138`
  - `bash regression_tests/stage2_require_boehm_noprelude_parse_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_macro_piececap128` -> `exit 0` / `not reproduced`
- **Stage3 bootstrap**: **FAILS** after `1.06s` with `status=139` on `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_funlookahead_fresh`
- **Current local stage3 probe**: still **FAILS** after `1.06s` with `status=139` on `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_macro_piececap128`
- **Current smallest clean/red HIR controls**:
  - `--release --no-prelude /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_simple_one.cr` is green in `0.02s`
  - the path-wrapper oracle stays green on the newest local candidate:
    - `stage2_release_reparse_fix_dbg2`: `exit 1` / `reproduced: compiler failed before lower_main on the path-wrapper module repro`
    - `stage2_release_reparse_class_clean`: `exit 0` / `not reproduced: compiler reached lower_main exprs=0 on the path-wrapper repro`
  - the previously smallest nested-macro red control is now also green on the same candidate:
    - `bash regression_tests/stage2_nested_macro_method_missing_repro.sh /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_reparse_class_clean`
    - Result: `exit 0` / `not reproduced`
- **Benchmark status**: blocked — stage2 compiler is still unstable and crashes before finishing stage3

### New Verified Locally (Uncommitted)
1. **Widening `parse_macro_body`'s initial `Array(MacroPiece)` capacity to `128` removes the focused `require "gc/boehm"` parser-only crash without regressing broader parser stability**
   - parser change:
     - `src/compiler/frontend/parser.cr`: `pieces = Array(MacroPiece).new(128)` in `parse_macro_body`
   - focused regression surface:
     ```bash
     bash regression_tests/stage2_require_boehm_noprelude_parse_repro.sh \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_reparse_class_clean
     bash regression_tests/stage2_require_boehm_noprelude_parse_repro.sh \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_macro_piececap128
     ```
     Result:
     - old checkpoint: `exit 1` / `reproduced: require gc/boehm parse-only compile failed with status=138`
     - current local candidate: `exit 0` / `not reproduced`
   - adversary checks:
     ```bash
     env CRYSTAL_V2_STOP_AFTER_PARSE=1 \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_macro_piececap128 \
       --release --no-prelude \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_macro_begin_inline_if_repro.cr \
       -o /tmp/ignore
     bash regression_tests/stage2_full_compiler_parse_only_repro.sh \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_macro_piececap128
     ```
     Result:
     - cheap `begin + inline if` parser probe: `status=0`
     - full compiler parse-only loop: `rcs: 0 0 0 0 0`
   - boundary:
     - `CRYSTAL_V2_STOP_AFTER_HIR=1 --release --no-prelude regression_tests/stage2_require_boehm_noprelude_parse_repro.cr` still fails on the same local candidate with wrapper `status=139`
     - `stage2_release_macro_piececap128 -> stage3_release_macro_piececap128` still dies in `1.06s` with `status=139`

2. **The abandoned `MacroPieceBuffer` experiment is a verified false path**
   - the scalarizing `MacroPieceBuffer#<<` branch temporarily made the focused `gc/boehm` parse-only oracle green, but it introduced a new smaller parser-only crash surface:
     - `/Users/sergey/Projects/Crystal/.codex_artifacts/stage2_macro_begin_inline_if_repro.cr`
   - split:
     - stage1 `stage1_release_funlookahead`: `status=0`
     - baseline stage2 `stage2_release_reparse_class_clean`: `status=0`
     - experimental `stage2_release_macro_piecebuf`: wrapper `status=139`
     - experimental `stage2_release_macro_piecebuf_spanbuf`: wrapper `status=138`
   - LLDB on `stage2_release_macro_piecebuf` reduces the failure to:
     - `CLI#evaluate_macro_condition`
     - `CLI#macro_literal_require_texts`
     - `CLI#process_require_node`
   - implication:
     - do not resurrect the field-unpacking `MacroPieceBuffer` path without a separate proof that `MacroPiece` field reads are safe in self-hosted release builds

### New Verified Since `30a4a88c`
1. **Class-side snippet fallback removes the old `register_class` reparse loop on the nested-macro micro-probe**
   - `src/compiler/hir/ast_to_hir.cr` now extracts a one-shot class name directly from the reparsed snippet header and uses that recovered name to call `register_class_with_name(...)`, instead of recursively calling `register_class(...)` again on the same nameless reparsed node
   - fresh verification:
     ```bash
     bash regression_tests/stage2_reparsed_module_wrapper_repro.sh \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_reparse_class_clean
     bash regression_tests/stage2_nested_macro_method_missing_repro.sh \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_reparse_class_clean
     ```
     Result:
     - path-wrapper oracle: `exit 0` / `not reproduced: compiler reached lower_main exprs=0 on the path-wrapper repro`
     - nested-macro oracle: `exit 0` / `not reproduced`
   - integration boundary:
     ```bash
     /usr/bin/time -p env CRYSTAL_CACHE_DIR_STAGE2_RELEASE=/Users/sergey/Projects/Crystal/.codex_artifacts/cache_stage3_release_reparse_class_clean \
       CRYSTAL_V2_PIPELINE_CACHE=0 CRYSTAL_V2_LLVM_CACHE=0 \
       scripts/timeout_sample_lldb.sh -t 1800 -m 40960 -s 8 -l 20 -n 12 --no-series \
       -o /Users/sergey/Projects/Crystal/.codex_artifacts/logs/stage3_release_reparse_class_clean_timeout \
       -- scripts/build_stage2_release.sh \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_reparse_class_clean \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage3_release_reparse_class_clean
     ```
     Result: `status=138`, `real 1.07s`, underlying `Bus error: 10`

### Completed In This Cycle
1. **Path-wrapper module-name recovery moved the stage2 frontier past the old reparsed-module loop**
   - `src/compiler/hir/ast_to_hir.cr` now treats path-wrapper module headers from `class/struct/union/enum Foo::Bar` as valid module-name sources, and `definition_header_text_from_source` now uses a manual byte-scan instead of `each_line`/`lstrip`
   - the smallest focused repro is now:
     ```bash
     bash regression_tests/stage2_reparsed_module_wrapper_repro.sh <compiler>
     ```
   - fresh verification on the old/new boundary:
     ```bash
     bash regression_tests/stage2_reparsed_module_wrapper_repro.sh \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_reparse_fix_dbg2
     bash regression_tests/stage2_reparsed_module_wrapper_repro.sh \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_reparse_clean_fix
     ```
     Result:
     - old stage2: `exit 1` / `reproduced: compiler failed before lower_main on the path-wrapper module repro`
     - current local fix: `exit 0` / `not reproduced: compiler reached lower_main exprs=0 on the path-wrapper repro`
   - boundary shift on the same minimal `struct A::B` no-prelude control: the old stage2 died in the empty-name reparse loop before emitting any `lower_main` trace, while the current local fix now reaches `lower_main: exprs=0` and only then dies later with `status=139`

2. **Parser rewind hardening moved the active frontier past full-compiler parse-only**
   - `src/compiler/frontend/parser.cr` now restores `@previous_token` together with `@index` inside `lookahead_for_arrow?`, including the early `::Foo` false return, the generic false return, and the speculative trailing `.class` rewind
   - full compiler parse-only regression surface now cleanly separates old vs fresh stage2:
     ```bash
     bash regression_tests/stage2_full_compiler_parse_only_repro.sh \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_nameprio_fresh
     bash regression_tests/stage2_full_compiler_parse_only_repro.sh \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_funlookahead_fresh
     ```
     Result:
     - baseline `stage2_release_nameprio_fresh`: `rc=0,138,138,138,138`
     - fresh `stage2_release_funlookahead_fresh`: `rc=0,0,0,0,0`

3. **Fresh release bootstrap remains green with the parser rewind patch**
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

4. **The reduced parser-body oracle remains green on the fresh stage2**
   - command:
     ```bash
     bash regression_tests/stage2_block_body_exprid_parser_repro.sh \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_funlookahead_fresh
     ```
     Result: `exit 0` / `not reproduced`

5. **The active crash frontier moved again, from whole-compiler parse-only into direct `object.cr` HIR registration**
   - the smallest green HIR control is now the truly trivial no-prelude file:
     ```bash
     env CRYSTAL_V2_STOP_AFTER_HIR=1 \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_funlookahead_fresh \
       --release --no-prelude /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_simple_one.cr \
       -o /Users/sergey/Projects/Crystal/.codex_artifacts/funlookahead_hir_simple_noprelude_out
     ```
     Result: `status=0`, `real 0.02s`
   - the red stdlib path reduces past `prelude.cr` and into direct `src/stdlib/object.cr`:
     ```bash
     /usr/bin/time -p env CRYSTAL_V2_STOP_AFTER_HIR=1 scripts/timeout_sample_lldb.sh \
       -t 120 -m 8192 -s 5 -l 10 -n 8 --no-series \
       -o /Users/sergey/Projects/Crystal/.codex_artifacts/logs/object_direct_hir_noprelude \
       -- /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_funlookahead_fresh \
       --release --no-prelude src/stdlib/object.cr \
       -o /Users/sergey/Projects/Crystal/.codex_artifacts/object_direct_hir_noprelude_out
     ```
     Result: `status=125`, `real 17.78s`
   - the same direct control stays green on fresh stage1:
     ```bash
     /usr/bin/time -p env CRYSTAL_V2_STOP_AFTER_HIR=1 scripts/timeout_sample_lldb.sh \
       -t 120 -m 8192 -s 5 -l 10 -n 8 --no-series \
       -o /Users/sergey/Projects/Crystal/.codex_artifacts/logs/object_direct_hir_noprelude_stage1 \
       -- /Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_funlookahead \
       --release --no-prelude src/stdlib/object.cr \
       -o /Users/sergey/Projects/Crystal/.codex_artifacts/object_direct_hir_noprelude_stage1_out
     ```
     Result: `status=0`, `real 1.06s`
   - require-prefix falsifiers now show `lib_c + object` is enough to reproduce, while `lib_c + macros` is not:
     ```text
     require "lib_c"
     require "object"
     1
     ```
     Result: wrapper `status=125`, `real 19.58s`
     ```text
     require "lib_c"
     require "macros"
     1
     ```
     Result: quick compiler error `error: Index out of bounds`, not the runaway path
   - bounded LLDB/sample on the red object control shows `Parser#initialize -> Lexer#next_token/lex_identifier` above hundreds of repeated `AstToHir#register_class(...)+1900` frames, which is narrower than the earlier `register_module`-heavy prelude control

6. **There is now a dedicated regression script for the direct object repro**
   - script:
     ```bash
     bash regression_tests/stage2_object_hir_noprelude_repro.sh /path/to/compiler
     ```
   - expected current behavior:
     - fresh stage1: `exit 0` / `not reproduced`
     - fresh stage2: `exit 1` / `reproduced: object HIR no-prelude compile failed`

7. **A smaller nested-macro micro-probe now reproduces the same stage2-specific class corridor**
   - fixture:
     ```bash
     regression_tests/stage2_nested_macro_method_missing_repro.cr
     ```
   - verification:
     ```bash
     bash regression_tests/stage2_nested_macro_method_missing_repro.sh \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_funlookahead
     bash regression_tests/stage2_nested_macro_method_missing_repro.sh \
       /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_funlookahead_fresh
     ```
     Result:
     - fresh stage1: `exit 0` / `not reproduced`
     - fresh stage2: `exit 1` / `reproduced: nested macro method_missing compile failed`
   - bounded LLDB/sample on the fresh-stage2 failure still shows `Parser#initialize -> Lexer#next_token` above repeated `AstToHir#register_class(...)+1900`, so the active corridor is now inside the nested macro/class shape rather than `object.cr` as a whole

8. **Stage3 remains blocked, but after the parser corridor**
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
- HIR-bounded no-prelude compile of `1` is clean, and the active red path now reduces to a nested macro/class shape extracted from `src/stdlib/object.cr`
- the older empty-name `register_module -> reparse -> register_module` loop on path-wrapped `struct A::B` is now removed on the local `reparse_clean_fix` candidate, but that same minimal control still dies later after `lower_main: exprs=0`
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

3. **Plain parse-only on the full compiler source is now green on the fresh stage2 and red on the previous baseline**
   ```bash
   bash regression_tests/stage2_full_compiler_parse_only_repro.sh \
     /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_nameprio_fresh
   bash regression_tests/stage2_full_compiler_parse_only_repro.sh \
     /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_funlookahead_fresh
   ```
   Result:
   - baseline `stage2_release_nameprio_fresh`: `rc=0,138,138,138,138`
   - fresh `stage2_release_funlookahead_fresh`: `rc=0,0,0,0,0`

4. **The smallest clean/red HIR controls are now separated cleanly**
   ```bash
   env CRYSTAL_V2_STOP_AFTER_HIR=1 \
     /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_funlookahead_fresh \
     --release --no-prelude /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_simple_one.cr \
     -o /Users/sergey/Projects/Crystal/.codex_artifacts/funlookahead_hir_simple_noprelude_out
   bash regression_tests/stage2_nested_macro_method_missing_repro.sh \
     /Users/sergey/Projects/Crystal/.codex_artifacts/stage1_release_funlookahead
   bash regression_tests/stage2_nested_macro_method_missing_repro.sh \
     /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_funlookahead_fresh
   ```
   Result: trivial no-prelude file exits `0` in `0.02s`; fresh stage1 is green on the nested-macro micro-probe; fresh stage2 reproduces there with wrapper `status=125`

5. **Bounded LLDB/sample on the current red path points at direct class registration**
   ```bash
   /usr/bin/time -p env CRYSTAL_V2_STOP_AFTER_HIR=1 scripts/timeout_sample_lldb.sh \
     -t 60 -m 4096 -s 5 -l 10 -n 8 --no-series \
     -o /Users/sergey/Projects/Crystal/.codex_artifacts/logs/object_probe_method_missing_stage2 \
     -- /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_funlookahead_fresh \
     --release --no-prelude /Users/sergey/Projects/Crystal/.codex_artifacts/object_probe_method_missing.cr \
     -o /Users/sergey/Projects/Crystal/.codex_artifacts/object_probe_method_missing_stage2_out
   ```
   Result: LLDB shows `Parser#initialize -> Lexer#next_token` above repeated `AstToHir#register_class(...)+1900`; the fresh-stage2 micro-probe also logs a `Bus error: 10` before wrapper teardown

6. **Direct `object.cr` is still a useful larger-file corroborating control**
   ```bash
   bash regression_tests/stage2_object_hir_noprelude_repro.sh \
     /Users/sergey/Projects/Crystal/.codex_artifacts/stage2_release_funlookahead_fresh
   ```
   Result: `exit 1` / `reproduced: object HIR no-prelude compile failed`

7. **Stage3 self-bootstrap is still blocked**
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
1. Keep `regression_tests/stage2_full_compiler_parse_only_repro.sh` as the strongest current parser regression surface, with `stage2_block_body_exprid_parser_repro.sh` as the smaller parser-only control
2. Keep parser rewind commit `24bf5d7c` isolated; do **not** mix the next HIR/module experiment into that boundary shift
3. Use `bash regression_tests/stage2_nested_macro_method_missing_repro.sh <compiler>` as the new smallest red control and `... stage2_simple_one.cr --no-prelude` as the green control
4. Debug the nested macro/class corridor first, with `src/stdlib/object.cr` retained only as the larger corroborating control

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
