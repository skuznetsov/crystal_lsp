# Codex status — closure-env ABI P1 scaffold

Date: 2026-04-19
Branch: `closure-env-abi-p1-wip`

## Current HEAD

- This status file is maintained as a handoff log; inspect `git log --oneline`
  for the exact latest commit hash after each Codex update.
- Latest Codex update in this file: entry-box requirement scaffold for boxed
  capture predeclaration.
- `a1fcb5fd docs: add closure env ABI handoff status`
- `85c39c79 scaffold(proc-abi): require seeded entry hoists for boxed locals`
- `cc4d0f31 perf(hir): cache debug env lookups`
- Parent context: `31878cbd` introduced I14-monotonic dominance by entry-block allocation.

## What changed after Claude's last handoff

1. Resolved the local `src/compiler/hir/ast_to_hir.cr` conflict between:
   - closure-env P1 scaffold (`BoxedLocal`, `LocalsSnapshot`, I14-monotonic), and
   - stashed perf/env-gate hoist (`@trace_shovel_types_enabled`, `@debug_type_literal_enabled`, negative env cache).

2. Landed `cc4d0f31`:
   - caches hot debug/env gates in HIR lowering;
   - keeps behavior unchanged when debug env vars are unset.

3. Hardened I14 in `85c39c79`:
   - `hoist_box_for_local` now takes `initial_value`;
   - emits the Box allocation in `function.entry_block`;
   - emits a seed `PointerStore` from the current local value;
   - rejects invocation unless `ctx.current_block == ctx.function.entry_block`.

## Important correction

Entry-block allocation alone was not enough. The previous zero-init model was only correct for variables whose current value was zero. A captured local with an earlier non-zero assignment, for example `counter = 5`, would read `0` from the Box after a late branch-local hoist.

Current invariant:

- P1 must predeclare boxed captures before branch/case/loop lowering.
- Box hoist must happen at local declaration / first assignment, with the current local value seeded into the Box.
- `lower_proc_literal` must not discover and late-hoist an already-initialized parent local from inside a branch. It should find `ctx.lookup_boxed_local(name)` or stop.

## Forward guards

Updated non-zero reducers:

- `regression_tests/conditional_closure_capture_repro.sh`
- `regression_tests/escaping_branch_closure_capture_repro.sh`

Both expect `12\n12` from `counter = 5; p.call(7); puts counter`. This catches the invalid "allocation + zero-init is enough" shortcut.

## Verification run

- `crystal build src/crystal_v2.cr -o bin/crystal_v2 --error-trace` — green, only the known `Random::DEFAULT` warning.
- `regression_tests/spawn_capture_block_param_repro.sh bin/crystal_v2` — exit 0, reproduced expected known-red sums `(16, 12)`.
- `regression_tests/conditional_closure_capture_repro.sh bin/crystal_v2` — exit 1, correct forward-guard output.
- `regression_tests/escaping_branch_closure_capture_repro.sh bin/crystal_v2` — exit 1, correct forward-guard output.
- `bin/crystal_v2 regression_tests/test_proc_basic.cr -o /tmp/test_proc_basic_codex && scripts/run_safe.sh /tmp/test_proc_basic_codex 5 512` — prints `7`, `50`, `proc_test_done`.
- `bin/crystal_v2 regression_tests/channel_ping_pong_repro.cr -o /tmp/channel_ping_pong_codex && scripts/run_safe.sh /tmp/channel_ping_pong_codex 10 512` — prints `channel_ping_pong_ok`.
- `git diff --check` — clean.

## Next safe step

Superseded by the 2026-04-19 Codex P1 WIP below.

## 2026-04-19 Codex P1 WIP update

Status: atomic Proc ABI flip is implemented locally and under verification. It
is not committed in this log entry until the final guard set passes.

Key implementation changes now in the working tree:

- `lower_proc_literal` now emits user-visible `HIR::MakeProc(fn_ptr, env_ptr)`.
- Captured proc literals receive a hidden `__closure_env` param; zero-capture
  proc literals keep the raw signature so `Proc#pointer` remains usable for
  runtime callbacks such as `Fiber#makecontext`.
- MIR `Proc#call` loads `fn/env` from the heap Proc object and dispatches as:
  `env == null ? fn(args...) : fn(env, args...)`.
- MIR `Proc#call` infers its concrete return type from the receiver
  `Proc(...)` descriptor (`type_params.last`) instead of the generic
  `Proc#call` placeholder `R`. This fixed the no-arg closure return crash in
  `test_complex_closures_capture`.
- MIR Proc accessors now load from heap Proc object fields:
  `pointer -> fn@0`, `closure_data -> env@8`, `closure? -> env != null`.
- `lower_closure` allocates a byte-sized env object using capture slot offsets
  rather than the old zero-sized/index-GEP placeholder.
- `spawn` and `Fiber` block arguments are materialized as heap Proc objects;
  generic runtime-yield block callbacks remain raw function pointers to avoid
  breaking stdlib callback ABI.
- Box allocation is split:
  - entry-required parent locals use entry allocation plus binding-site seed;
  - loop/block-param captures use current-block allocation so each iteration /
    invocation gets a fresh box.

Verification so far:

- `crystal build src/crystal_v2.cr -o bin/crystal_v2 --error-trace` — green,
  only the known `Random::DEFAULT` warning.
- `regression_tests/spawn_capture_block_param_repro.sh bin/crystal_v2` — exit 1,
  both probes print `_ok`; original known-red is fixed.
- `regression_tests/conditional_closure_capture_repro.sh bin/crystal_v2` — exit 1,
  correct `12/12`.
- `regression_tests/escaping_branch_closure_capture_repro.sh bin/crystal_v2` —
  exit 1, correct `12/12`.
- `bin/crystal_v2 regression_tests/test_proc_basic.cr -o /tmp/test_proc_basic &&
  scripts/run_safe.sh /tmp/test_proc_basic 5 512` — prints `7`, `50`,
  `proc_test_done`.
- `bin/crystal_v2 regression_tests/test_blocks.cr -o /tmp/test_blocks &&
  scripts/run_safe.sh /tmp/test_blocks 5 512` — prints `blocks_done`.
- `bin/crystal_v2 regression_tests/channel_ping_pong_repro.cr -o
  /tmp/channel_ping_pong_repro && scripts/run_safe.sh /tmp/channel_ping_pong_repro
  5 512` — prints `channel_ping_pong_ok`.
- `bin/crystal_v2 regression_tests/tuple_int32_int64_layout.cr -o
  /tmp/tuple_int32_int64_layout && scripts/run_safe.sh
  /tmp/tuple_int32_int64_layout 5 512` — prints both tuple layout ok markers.
- `bin/crystal_v2 regression_tests/combined/test_complex_closures_capture.cr -o
  /tmp/test_complex_closures_capture && scripts/run_safe.sh
  /tmp/test_complex_closures_capture 5 512` — prints `closures_capture_all_ok`
  after the `Proc#call` return-type fix.
- `regression_tests/run_combined.sh bin/crystal_v2 4` was attempted after
  cleaning old result files. Final result: 26 passed, 5 failed out of 31.
  Failures: `test_collections`, `test_edge_hash_complex`,
  `test_complex_generic_dispatch`, `test_generics_unions`, `test_strings_join`.
  A temporary `ede53ed8` worktree with a freshly built `/tmp/cv2_ede53ed8`
  reproduced all five failures with the same signatures, so they are baseline
  issues rather than regressions from `5696b6de`. The Proc/capture combined
  file above was rerun directly and passes.

Important caveat for Claude/Codex continuation:

- This is narrower than the original fully-uniform block Proc ABI. Generic
  `lower_block_to_proc` remains raw by default because making every block thunk
  heap-backed broke stdlib runtime-yield callbacks. The heap path is targeted
  to `spawn`/`Fiber` block arguments and `->` proc literals.
- Before committing, run `git diff --check`, inspect the final diff, and run at
  least the same verification matrix above.

## 2026-04-19 Codex baseline hash checkpoint

Status: one baseline combined failure family is partially reduced and fixed as
a separate logical change after the Proc ABI commit.

Key finding:

- The first `test_collections` / `test_edge_hash_complex` failure was not
  caused by Proc ABI. It was the stdlib small-Hash linear-scan path:
  entries were stored and visible through iteration, but
  `find_entry_with_index_linear_scan` always fell through to nil because V2
  lowered `return entry, index if ...` inside `each_entry_with_index` as a
  block-local return.

Implementation:

- `src/compiler/mir/llvm_backend.cr` now emits direct backend overrides for
  small `Hash(String, V)` / `Hash(Int32, V)`:
  `find_entry_with_index_linear_scan` and `update_linear_scan`.
- `regression_tests/hash_small_linear_scan_repro.sh` covers:
  String-key lookup after insert, String-key overwrite without size growth, and
  Int32-key overwrite without duplicated entries.

Verification:

- `crystal build src/crystal_v2.cr -o bin/crystal_v2 --error-trace` — green,
  only the known `Random::DEFAULT` warning.
- `regression_tests/hash_small_linear_scan_repro.sh bin/crystal_v2` — green,
  prints `hash_small_linear_scan_ok`.
- Focused `/tmp/hash_string_probe.cr` — prints `1 / true / true / 1`.
- Focused `/tmp/hash_int_probe.cr` — prints `true / 1 / 9`.
- `regression_tests/spawn_capture_block_param_repro.sh bin/crystal_v2` —
  fixed-state exit `1`, both probes print `_ok`.
- `git diff --check` — clean.
- `regression_tests/run_combined.sh bin/crystal_v2 4` — `27 passed, 4 failed
  out of 31`; `test_collections` is now green.

Remaining frontier:

- `combined/test_edge_hash_complex.cr` now moves past the original missing-key
  site, but later segfaults after `h.each { |k, v| total += v }`; a reduced
  probe showed `total` remains `0` and the next independent Hash loop can
  segfault. Treat this as a block-write/iteration/capture frontier, not part of
  the small-Hash scan fix.
- Remaining combined failures after this checkpoint:
  `test_complex_generic_dispatch` (`Pointer#width` stub),
  `test_edge_hash_complex` (block-write/iteration segfault),
  `test_generics_unions` (segfault), and
  `test_strings_join` (`Reference#join(String)_super` stub).

## 2026-04-19 Codex Hash#each writeback checkpoint

Status: second small hash frontier fixed as a separate HIR intrinsic change.

Key finding:

- `Hash#each` for dynamic hashes was not using the generic Proc ABI path here.
  It was lowered by `lower_hash_each_dynamic`.
- That intrinsic lowered the user block inside a pushed block scope, then called
  `ctx.pop_scope` before reading the updated caller-local values for merge PHIs.
  As a result, `h.each { |k, v| total += v }` executed but the `total` backedge
  used the pre-block value and printed `0`.

Implementation:

- `src/compiler/hir/ast_to_hir.cr` now snapshots `post_exec_values` before
  popping the hash-each block scope.
- The exec/skip merge PHI uses the saved post-exec value instead of reading
  locals after the scope restore.
- `regression_tests/hash_each_block_writeback_repro.sh` covers the reduced
  `Hash(String, Int32)#each` caller-local sum case.

Verification:

- `crystal build src/crystal_v2.cr -o bin/crystal_v2 --error-trace` — green,
  only the known `Random::DEFAULT` warning.
- `regression_tests/hash_each_block_writeback_repro.sh bin/crystal_v2` —
  green, prints `hash_each_block_writeback_ok`.
- `regression_tests/hash_small_linear_scan_repro.sh bin/crystal_v2` — still
  green.
- `regression_tests/spawn_capture_block_param_repro.sh bin/crystal_v2` —
  fixed-state exit `1`, both probes print `_ok`.
- `regression_tests/run_combined.sh bin/crystal_v2 4` — still `27 passed, 4
  failed out of 31`; `test_edge_hash_complex` now reaches the expected
  iteration total `26` before the next segfault.

Remaining frontier:

- `combined/test_edge_hash_complex.cr` now fails after the corrected total, at
  the following `Hash(String, Int32).new(0)` counting section. Treat this as a
  distinct hash/default-value or post-iteration corruption defect.

## 2026-04-19 Codex post-Hash#each constructor type checkpoint

Status: the next `test_edge_hash_complex` frontier is fixed as a separate HIR
call-type repair.

Key finding:

- The segfault after the corrected `Hash#each` total was already present in
  HIR: after the `h.each` intrinsic, the following
  `Hash(String, Int32).new(0)` call was emitted with return type
  `Hash(String, Void)`.
- That happened because `Hash#each` had forced constructor helper bodies to be
  available. A later generic "prefer actual lowered function return type" pass
  then replaced the constructor's concrete owner type with the helper body's
  lowered return type.

Implementation:

- `src/compiler/hir/ast_to_hir.cr` now pins concrete class `new` return types
  once the owner has been resolved, so late function-body return repair cannot
  downgrade constructor call-sites.
- `regression_tests/hash_each_then_hash_new_type_repro.sh` covers the reduced
  sequence: `Hash#each`, then `Hash(String, Int32).new(0)`, then `[]=` and
  `[]`.

Verification:

- `crystal build src/crystal_v2.cr -o bin/crystal_v2 --error-trace` — green,
  only the known `Random::DEFAULT` warning.
- `regression_tests/hash_each_then_hash_new_type_repro.sh bin/crystal_v2` —
  green, prints `hash_each_then_hash_new_type_ok`.
- Focused HIR for `/tmp/hash_after_each_empty_counts.cr` now types the
  post-`Hash#each` constructor as `Hash(String, Int32)` and follows with
  `Hash(String, Int32)#[]=` / `Hash(String, Int32)#[]`.
- `regression_tests/combined/test_edge_hash_complex.cr` now prints
  `hash_complex_all_ok` and exits `0`.
- `regression_tests/run_combined.sh bin/crystal_v2 4` now reports
  `28 passed, 3 failed out of 31`; `test_edge_hash_complex` is green.

Remaining frontier:

- The hash frontiers covered by `test_collections` and
  `test_edge_hash_complex` are now closed. The remaining combined failures are
  expected to be in separate generic dispatch, generics/unions, and strings/join
  families.

## 2026-04-19 Codex checkpoint: module-super join frontier closed

Status: `test_strings_join` is green after a HIR super-resolution fix.

Root cause:

- `Array(Bool)#join` uses `Indexable#join`; its `super(separator)` should reach
  `Enumerable#join`.
- V2 tracked the direct include wrapper `Indexable::Mutable` instead of the
  actual method-owner module `Indexable` for recursive module lookup.
- The next `super` therefore skipped the wrong layer and fell through to the
  class parent chain, emitting an unlowered `Reference#join(String)_super` abort
  stub.

Fix:

- `src/compiler/hir/ast_to_hir.cr` now has owner-aware recursive module lookup.
- Module-body `super` can walk the actual source module's include chain and emit
  a distinct wrapper such as `Array(Bool)#join_super_from_Enumerable$String`.
- Added `regression_tests/array_bool_join_module_super_repro.sh` for the reduced
  `Array(Bool)#join("|")` route.

Verification:

- `crystal build src/crystal_v2.cr -o bin/crystal_v2 --error-trace` — green,
  only the known `Random::DEFAULT` warning.
- `LIBRARY_PATH=/opt/homebrew/lib regression_tests/array_bool_join_module_super_repro.sh bin/crystal_v2` — green, prints `array_bool_join_module_super_ok`.
- `regression_tests/combined/test_strings_join.cr` — green, prints
  `strings_join_all_ok`.
- Focused HIR for `/tmp/bool_join.cr` now shows
  `Array(Bool)#join_super$String -> Array(Bool)#join_super_from_Enumerable$String`,
  not `Reference#join(String)_super`.
- `LIBRARY_PATH=/opt/homebrew/lib regression_tests/run_combined.sh bin/crystal_v2 4`
  — `29 passed, 2 failed out of 31`.

Remaining combined frontiers:

- `test_complex_generic_dispatch` — `Pointer#width` abort stub.
- `test_generics_unions` — segfault after several `unknown` outputs.

## 2026-04-19 Codex checkpoint: abstract Array#sum generic dispatch frontier closed

Status: `test_complex_generic_dispatch` is green after a bounded HIR Array#sum
intrinsic.

Root cause:

- `Container#width` executes `@children.sum { |c| c.width }` where `@children`
  is `Array(Widget)`.
- The previous lowering went through the current dynamic Array#reduce intrinsic,
  which hardcoded the element type to `Pointer`.
- The block body therefore emitted `Pointer#width` instead of virtual
  `Widget#width`, producing the abort stub `STUB CALLED: Pointer#width`.

Fix:

- `src/compiler/hir/ast_to_hir.cr` now intercepts `Array#sum { }` for
  non-primitive element arrays and lowers a direct Int32 accumulator loop.
- The block parameter type comes from `array_element_type_for_value`, preserving
  `Widget` for `Array(Widget)` and allowing normal virtual dispatch.
- Added `regression_tests/array_widget_sum_block_repro.sh` for the reduced
  `Array(Widget)#sum { |item| item.width }` route.

Verification:

- `crystal build src/crystal_v2.cr -o bin/crystal_v2 --error-trace` — green,
  only the known `Random::DEFAULT` warning.
- `LIBRARY_PATH=/opt/homebrew/lib regression_tests/array_widget_sum_block_repro.sh bin/crystal_v2` — green, prints `array_widget_sum_block_ok`.
- `regression_tests/combined/test_complex_generic_dispatch.cr` — green, prints
  `generic_dispatch_all_ok` under `scripts/run_safe.sh`.
- Focused HIR for `Container#width` now shows `index_get ... : Widget` followed
  by virtual `Widget#width()`, not `Pointer#width`.
- `LIBRARY_PATH=/opt/homebrew/lib regression_tests/run_combined.sh bin/crystal_v2 4`
  — `30 passed, 1 failed out of 31`.

Remaining combined frontier:

- `test_generics_unions` — still segfaults after several `unknown` outputs.

## 2026-04-19 Codex checkpoint: generics/unions combined frontier closed

Status: `test_generics_unions` is green after two union ABI fixes.

Root causes:

- Array-of-union storage: `Array(Int32 | String)#push$Int32/String` wrote bare
  variant values into a `Pointer(Int32 | String)` buffer. Later `index_get`
  produced union-shaped values with invalid discriminators, so
  `describe_value(v)` fell through to `unknown` for every mixed-array element.
- Nilable reference return: `get_config : Config?` returned an all-ref
  raw-pointer union, but MIR block cleanup still `rc_dec`-ed the fresh
  `Config.new` result immediately after `UnionWrap`. The caller then read a
  freed object at `cfg.name`.

Fixes:

- `src/compiler/hir/ast_to_hir.cr`: indexed `Pointer(T)#[]=` assignment now uses
  the pointer element type as the store contract and coerces the value before
  `PointerStore`, so union element stores emit `UnionWrap` first.
- `src/compiler/mir/hir_to_mir.cr`: `UnionWrap` of an owned ARC temporary into
  an all-ref union marks the source value as moved, matching the existing
  `FieldSet` ownership-transfer pattern.
- Added `regression_tests/generics_unions_union_array_nilable_repro.sh` covering
  both reduced corridors.

Verification:

- `crystal build src/crystal_v2.cr -o bin/crystal_v2 --error-trace` — green,
  only the known `Random::DEFAULT` warning.
- `LIBRARY_PATH=/opt/homebrew/lib regression_tests/generics_unions_union_array_nilable_repro.sh bin/crystal_v2` — green, prints `generics_unions_union_array_nilable_ok`.
- `regression_tests/combined/test_generics_unions.cr` — green, prints
  `generics_unions_all_ok` under `scripts/run_safe.sh`.
- HIR shape: `Array(Int32 | String)#push$Int32/String` now has
  `union_wrap ... : Int32 | String` before `ptr_store`.
- LLVM shape: `get_config` no longer emits `__crystal_v2_rc_dec` of the returned
  `Config.new` result before `ret ptr`.

Remaining combined frontier:

- None observed. `LIBRARY_PATH=/opt/homebrew/lib regression_tests/run_combined.sh bin/crystal_v2 4` reports `31 passed, 0 failed out of 31`.

## 2026-04-19 Codex checkpoint: broader run_all frontier after combined green

Status: combined is closed; broader `run_all.sh` still has five known-red
frontiers.

Verification:

- `LIBRARY_PATH=/opt/homebrew/lib regression_tests/run_all.sh bin/crystal_v2`
  — `140 passed, 5 failed out of 145`.

Failures outside combined:

- `sprintf_float_precision` — output failure; runtime raises `case6`.
- `test_3mod` — abort stub `STUB CALLED: Type#name`.
- `test_byteformat_decode_u32` — abort stub
  `IO::ByteFormat::LittleEndian#==(IO::ByteFormat::LittleEndian)`.
- `test_closure_ref` — exits `138` without expected `42`.
- `test_nilable_struct_union_layout` — segfault.

Current clean commits after Claude handoff:

- `e664295e fix(hir): lower array sum blocks directly`
- `9e4ac156 fix(union): preserve pointer stores and owned wraps`

Recommended next local frontier:

- Start with one reduced `run_all.sh` failure, not combined. The most adjacent
  to the just-landed work is `test_nilable_struct_union_layout`; the most
  isolated abort-stub candidate is `test_byteformat_decode_u32`.

## 2026-04-19 Codex checkpoint: nilable struct/union layout run_all frontier closed

Status: `test_nilable_struct_union_layout` is green after a bounded HIR
allocator/initializer forwarding fix.

Root cause:

- `NodeWithNilableStructs` has untyped shorthand ivar params in `initialize`,
  but declared nilable field types via getters (`Array(Int32)?`,
  `Slice(UInt8)?`, `Bool?`).
- The stored init params were already correct, but allocator lowering used the
  concrete callsite arg types to lower the shared `initialize$arity7` body.
- The first non-nil constructor call therefore specialized the initializer
  params as non-nil `Array/Slice/Bool`; the later nil constructor overload
  passed `Nil` into those slots, and the initializer body wrapped nil arguments
  as non-nil union variants before crashing.

Fix:

- `src/compiler/hir/ast_to_hir.cr` now computes an explicit initializer
  signature for allocator forwarding from stored declared init params.
- Concrete `.new$...` overload params stay callsite-shaped, but forwarded args
  are coerced into the initializer signature before the `initialize` call.
- This keeps `initialize$arity7` typed with nilable union params while preserving
  existing specialized constructor overload names.

Verification:

- `crystal build src/crystal_v2.cr -o bin/crystal_v2 --error-trace` — green,
  only the known `Random::DEFAULT` warning.
- Focused HIR for `regression_tests/test_nilable_struct_union_layout.cr` —
  `NodeWithNilableStructs#initialize$arity7` now accepts union params and both
  nil/non-nil constructor overloads emit `union_wrap` before the initializer
  call.
- `LIBRARY_PATH=/opt/homebrew/lib bin/crystal_v2 regression_tests/test_nilable_struct_union_layout.cr -o /tmp/test_nilable_struct_union_layout && scripts/run_safe.sh /tmp/test_nilable_struct_union_layout 8 512`
  — prints `layout_ok`.
- `LIBRARY_PATH=/opt/homebrew/lib regression_tests/stage2_nilable_struct_union_overflow_repro.sh bin/crystal_v2`
  — fixed-state `not reproduced: nilable struct union layout is correct`.
- `LIBRARY_PATH=/opt/homebrew/lib regression_tests/run_combined.sh bin/crystal_v2 4`
  — still `31 passed, 0 failed out of 31`.
- `LIBRARY_PATH=/opt/homebrew/lib regression_tests/run_all.sh bin/crystal_v2`
  — `141 passed, 4 failed out of 145`.

Remaining `run_all.sh` frontiers:

- `sprintf_float_precision` — output failure; runtime raises `case6`.
- `test_3mod` — abort stub `STUB CALLED: Type#name`.
- `test_byteformat_decode_u32` — abort stub
  `IO::ByteFormat::LittleEndian#==(IO::ByteFormat::LittleEndian)`.
- `test_closure_ref` — exits `138` without expected `42`.

Separate adjacent frontier:

- `regression_tests/complex/test_nilable_struct_union.cr` still fails
  `count_non_nil`; HIR shows the `count += 1 unless item.nil?` branch computes
  the increment but the loop-back phi keeps the old count. Treat as a
  block/loop local writeback issue, not part of the allocator/initializer fix.

## 2026-04-19 Codex checkpoint: byteformat decode run_all frontier closed

Status: `test_byteformat_decode_u32` is green after a stacked HIR/MIR/LLVM
boundary fix.

Root cause stack:

- HIR lowered `IO::ByteFormat::LittleEndian == self` as a normal instance call
  and reached the missing
  `IO::ByteFormat::LittleEndian#==(IO::ByteFormat::LittleEndian)` stub.
- After the endian branch was fixed, `__vdispatch__IO#read(Slice)` passed a
  union storage pointer into concrete `IO::Memory#read(Slice(UInt8))`, so the
  callee interpreted the union header as a `Slice(UInt8)`.
- After the union-payload call boundary was fixed, stdlib
  `IO#read_fully(Slice(UInt8))` exposed the current HIR loop pollution path
  around `slice += read_bytes`, which can turn the loop value into
  `Int32 | Slice(UInt8)` and call `Int32#size`.

Fix:

- `src/compiler/hir/ast_to_hir.cr` folds `==`, `!=`, and `===` for type/module
  literal operands before method dispatch.
- `src/compiler/mir/llvm_backend.cr` unwraps a union payload when a call
  parameter is concrete pointer-shaped, while keeping full union storage when
  the callee parameter is a union.
- `src/compiler/mir/llvm_backend.cr` has a localized
  `IO#read_fully(Slice(UInt8))` loop override and registers the synthesized
  callees so missing vdispatch bodies go through the existing late-emit/stub
  path instead of producing invalid LLVM IR.

Verification:

- `crystal build src/crystal_v2.cr -o bin/crystal_v2 --error-trace` — green,
  only the known `Random::DEFAULT` warning.
- Focused HIR for `regression_tests/test_byteformat_decode_u32.cr` — endian
  checks emit `literal true : Bool`; no relevant
  `ByteFormat::LittleEndian#==` call remains.
- `LIBRARY_PATH=/opt/homebrew/lib bin/crystal_v2 regression_tests/test_byteformat_decode_u32.cr -o /tmp/test_byteformat_decode_u32 && scripts/run_safe.sh /tmp/test_byteformat_decode_u32 5 512`
  — prints `byteformat_u32_ok`.
- `LIBRARY_PATH=/opt/homebrew/lib regression_tests/run_mini_oracles.sh bin/crystal_v2`
  — `Mini-oracles: 6 passed, 0 failed out of 6 tests`.
- `LIBRARY_PATH=/opt/homebrew/lib regression_tests/run_combined.sh bin/crystal_v2 4`
  — `31 passed, 0 failed out of 31`.
- `LIBRARY_PATH=/opt/homebrew/lib regression_tests/run_all.sh bin/crystal_v2`
  — `142 passed, 3 failed out of 145`.

Remaining `run_all.sh` frontiers:

- `sprintf_float_precision` — output failure; runtime raises `case6`.
- `test_3mod` — abort stub `STUB CALLED: Type#name`.
- `test_closure_ref` — exits `138` without expected `42`.

Note for Claude/Codex continuation:

- The closure-env ABI branch remains untouched by this checkpoint.
- The `IO#read_fully(Slice(UInt8))` override is intentionally localized
  bootstrap glue. A future broader HIR fix for `slice += read_bytes` loop
  pollution can remove it, but the current change is verified against the full
  regression suite baseline.

## 2026-04-19 Codex checkpoint: sprintf precision / unless writeback frontier closed

Status: `sprintf_float_precision` is green after a bounded HIR `lower_unless`
local-writeback fix.

Root cause:

- The focused `sprintf("%.5f", 0.1_f64)` path printed `.10000` instead of
  `0.10000`.
- A direct `Float::Printer::RyuPrintf.d2fixed_buffered_n(0.1_f64, 5, buf)`
  probe produced the same malformed prefix, isolating the issue below
  `sprintf` formatting dispatch.
- Emitted HIR for `d2fixed_buffered_n` showed the `unless nonzero` branch wrote
  the leading `'0'` and computed `index &+= 1`, but the merge block kept the
  pre-branch index, so the following decimal-point write overwrote the zero.
- Source cause: `lower_unless` saved then-branch locals after `ctx.pop_scope`;
  the else branch already saved before pop.

Fix:

- `src/compiler/hir/ast_to_hir.cr` now saves `then_locals` and
  `then_inline_locals` before popping the then-branch scope in `lower_unless`.
- Added `regression_tests/unless_branch_local_writeback.cr` to guard both
  normal `+=` and wrapping `&+=` local writes through an `unless` branch.

Verification:

- `crystal build src/crystal_v2.cr -o bin/crystal_v2 --error-trace` — green,
  only the known `Random::DEFAULT` warning.
- `LIBRARY_PATH=/opt/homebrew/lib bin/crystal_v2 regression_tests/unless_branch_local_writeback.cr -o /tmp/unless_branch_local_writeback && scripts/run_safe.sh /tmp/unless_branch_local_writeback 5 512`
  — prints `unless_branch_local_writeback_ok`.
- `LIBRARY_PATH=/opt/homebrew/lib bin/crystal_v2 regression_tests/sprintf_float_precision.cr -o /tmp/sprintf_float_precision && scripts/run_safe.sh /tmp/sprintf_float_precision 5 512`
  — prints `sprintf_float_precision_ok`.
- Direct Ryu probe under `scripts/run_safe.sh` — prints `0.10000`.
- `LIBRARY_PATH=/opt/homebrew/lib regression_tests/run_mini_oracles.sh bin/crystal_v2`
  — `Mini-oracles: 6 passed, 0 failed out of 6 tests`.
- `LIBRARY_PATH=/opt/homebrew/lib regression_tests/run_combined.sh bin/crystal_v2 4`
  — `31 passed, 0 failed out of 31`.
- `LIBRARY_PATH=/opt/homebrew/lib regression_tests/run_all.sh bin/crystal_v2`
  — `144 passed, 2 failed out of 146`.

Remaining `run_all.sh` frontiers:

- `test_3mod` — abort stub `STUB CALLED: Type#name`.
- `test_closure_ref` — exits `138` without expected `42`.

Note for Claude/Codex continuation:

- The closure-env ABI branch remains untouched by this checkpoint.
- The next deterministic full-suite frontier is `test_3mod`; `test_closure_ref`
  is still the closure-cell/ABI-adjacent known-red.

## 2026-04-19 Codex checkpoint: abstract module self-dispatch frontier closed

Status: `test_3mod` is green after a narrow HIR receiverless self-dispatch fix.

Root cause:

- `Type#describe` calls `name` and `byte_size` without an explicit receiver.
- Those methods are abstract contracts declared by included modules
  `Named`/`Sized`, then implemented by concrete subclasses.
- HIR lowered the receiverless identifiers as static calls to
  `Type#name` / `Type#byte_size`.
- MIR preserved that static shape and emitted `extern_call @Type#name`, so
  runtime reached the abstract stub instead of subclass dispatch.

Fix:

- `src/compiler/hir/ast_to_hir.cr` now recognizes the narrow shape in
  `lower_identifier`: non-class, non-struct instance method; zero args;
  identifier name matches an abstract def on a module included by the current
  class.
- That shape is emitted as a virtual self call, with concrete subclass targets
  lowered and return type inferred through the existing virtual target helper.
- The broad common-call virtual-dispatch path remains untouched.

Verification:

- `crystal build src/crystal_v2.cr -o bin/crystal_v2 --error-trace` — green,
  only the known `Random::DEFAULT` warning.
- `LIBRARY_PATH=/opt/homebrew/lib bin/crystal_v2 regression_tests/test_3mod.cr -o /tmp/test_3mod && scripts/run_safe.sh /tmp/test_3mod 5 512`
  — prints `Int32(4)`, `Ptr(Int32)(8)`, `Arr(Int32)(24)`, `done`.
- Adjacent smokes `test_6_classes`, `test_module`, and
  `test_complex_hierarchy` compile and run under `scripts/run_safe.sh`.
- `LIBRARY_PATH=/opt/homebrew/lib regression_tests/run_mini_oracles.sh bin/crystal_v2`
  — `Mini-oracles: 6 passed, 0 failed out of 6 tests`.
- `LIBRARY_PATH=/opt/homebrew/lib regression_tests/run_combined.sh bin/crystal_v2 4`
  — `31 passed, 0 failed out of 31`.
- `LIBRARY_PATH=/opt/homebrew/lib regression_tests/run_all.sh bin/crystal_v2`
  — `145 passed, 1 failed out of 146`.

Remaining `run_all.sh` frontier:

- `test_closure_ref` — exits `138` without expected `42`; still the
  closure-cell/ABI-adjacent front.

Note for Claude/Codex continuation:

- The closure-env ABI branch remains untouched by this checkpoint.
- The next deterministic full-suite frontier is now exactly
  `test_closure_ref`.

## 2026-04-19 Codex checkpoint: closure_ref full-suite red closed

Status: `test_closure_ref` is green, and the current branch reaches a full
`run_all.sh` green baseline.

Root cause:

- The caller passed a raw block function pointer into `call_block(&block)`.
- The callee-side `Proc#call` path already used the heap-backed Proc shape and
  loaded `fn` and `env` from offsets `0` and `8`.
- That mismatch made `call_block` treat `@__crystal_block_proc_1` code bytes as
  Proc object storage, causing the previous bus error.
- After heap materialization, the first attempt still printed `10`: inline
  block-argument binding created the capture box, but `pop_scope` restored
  `@boxed_locals` and hid the box from the parent read after the call.

Fix:

- `src/compiler/hir/ast_to_hir.cr` now decides block-argument carrier shape via
  `block_arg_requires_heap_proc?`.
- The predicate keeps the existing spawn/Fiber heap path and also detects defs
  that call their block parameter, using the existing AST scan plus a bounded
  source-span fallback for direct `block.call` bodies.
- Late call lowering, inline-yield fallback, and inline block-param binding all
  use the same predicate.
- Boxed locals stay visible to their owning lexical binding across
  same-function scope restores; same-name locals in later scopes are rejected
  by owner-aware lookup.

Verification:

- `LIBRARY_PATH=/opt/homebrew/lib bin/crystal_v2 regression_tests/test_closure_ref.cr -o /tmp/test_closure_ref && scripts/run_safe.sh /tmp/test_closure_ref 5 512`
  — prints `42`.
- Adjacent smokes `test_proc_basic`, `test_blocks`, and `test_blocks_closures`
  compile and run under `scripts/run_safe.sh`.
- `LIBRARY_PATH=/opt/homebrew/lib regression_tests/run_mini_oracles.sh bin/crystal_v2`
  — `Mini-oracles: 6 passed, 0 failed out of 6 tests`.
- `LIBRARY_PATH=/opt/homebrew/lib regression_tests/run_combined.sh bin/crystal_v2 4`
  — `31 passed, 0 failed out of 31`.
- `LIBRARY_PATH=/opt/homebrew/lib regression_tests/run_all.sh bin/crystal_v2`
  — `146 passed, 0 failed out of 146 tests`.

Adversary note:

- One earlier parallel `run_combined.sh` attempt reported
  `test_control_flow` compile failure with `ExprId out of bounds`.
- Focused compile of `regression_tests/combined/test_control_flow.cr` passed,
  and a clean repeated combined run passed `31/31`.
- Treat that as a non-reproduced transient unless it appears again.

Boundary:

- This is a bounded full-suite closure-ref fix, not the broader closure-env ABI
  P1 atomic flip.
- Next shared frontier is to resume the tracked closure-env ABI/P1 work from
  the current green baseline, preserving the known-red guards outside
  `run_all.sh`.

## 2026-04-19 Codex checkpoint: P1 scaffold hygiene after full-green baseline

Status: additive hygiene only; no compiler behavior changed after the
`test_closure_ref` fix.

Applied:

- `docs/closure_env_abi_p1_plan.md` now states the real status:
  implementation checkpoint, not plan-only.
- The plan now has a current-anchor table for the already-wired pieces:
  `CapturedVar` metadata, `MakeProc`, HIR `emit_make_proc_value`, MIR
  `lower_make_proc`, `call_heap_proc`, and `lower_closure`.
- The plan explicitly marks the old control-flow map as historical and
  re-anchor-before-edit.
- I14-monotonic text matched code at that checkpoint: `restore_locals` does not rewind
  `@boxed_locals`; `push_scope` records a snapshot only for symmetry/debugging,
  and `pop_scope` discards it.
- `regression_tests/conditional_closure_capture_repro.sh` now runs the compiled
  binary through `scripts/run_safe.sh` and keeps its forward-guard contract:
  exit `1` means correct/fixed, exit `0` means reproduced, exit `2` means
  inconclusive compile/setup failure.
- `HIR::MakeProc` source comments no longer claim it is unused.

Verification:

- `git diff --check` — clean.
- `bash -n regression_tests/conditional_closure_capture_repro.sh` — clean.
- `LIBRARY_PATH=/opt/homebrew/lib regression_tests/conditional_closure_capture_repro.sh bin/crystal_v2; echo SCRIPT_RC:$?`
  — prints `correct: proc.call(7) -> result=12, counter=12` and
  `SCRIPT_RC:1`.

Still behavior-changing / not split:

- `@proc_captures_by_value` remains live and feeds hidden-arg paths.
- Legacy `@closure_ref_cells` remains live for reads/writes and the non-heap
  `lower_block_to_proc` fallback.
- `lower_block_to_proc` remains dual-mode: heap Proc for called blocks,
  legacy closure cells for other block-proc paths.
- Removing those paths must stay coupled with the ABI cleanup so no mixed
  hidden-arg/env-first calling state is committed.

## 2026-04-19 Codex checkpoint: owner-aware boxed locals close spawn guard

Status: bounded behavior fix on top of the heap Proc block-argument checkpoint.
This is still not the full closure-env ABI cleanup.

Finding:

- The old `spawn_capture_block_param_repro.sh` became partial after
  `test_closure_ref` was fixed: probe A printed `_ok`, but probe B still used
  the stale final value from probe A.
- A split two-probe LLVM check showed the second `4.times do |i|` passed the
  first probe's boxed `i` into `send_id`, because `@boxed_locals` was keyed only
  by name and preserved across same-function scopes.

Applied:

- `LoweringContext::BoxedLocal` now records `owner_local`.
- Boxed reads, writes, and `capture_var_for_env` reuse a box only when the
  current lexical binding matches `owner_local`, or when the entry is env-bound
  / entry-required.
- `hoist_box_for_local` records the owning `initial_value` when it creates a
  box, so current-block boxes for loop/block parameters do not leak to later
  same-name block parameters.
- The P1 plan now names this I14-owner-aware, replacing the too-broad
  append-only wording.

Verification:

- `regression_tests/spawn_capture_block_param_repro.sh bin/crystal_v2; echo SPAWN_RC:$?`
  — both probes print `_ok`; script exits `1` (fixed forward-guard status).
- `bin/crystal_v2 regression_tests/test_closure_ref.cr -o /tmp/test_closure_ref && scripts/run_safe.sh /tmp/test_closure_ref 5 512`
  — prints `42`.
- Manual two-probe file prints `a=6` and `b=6`.
- `regression_tests/test_proc_basic.cr`, `test_blocks.cr`, and
  `test_blocks_closures.cr` compile and run under `scripts/run_safe.sh`.
- `regression_tests/run_mini_oracles.sh bin/crystal_v2`
  — `6 passed, 0 failed out of 6`.
- `regression_tests/run_combined.sh bin/crystal_v2 4`
  — `31 passed, 0 failed out of 31`.

Boundary:

- `@proc_captures_by_value` and legacy `@closure_ref_cells` are still live.
- The remaining ABI cleanup must still be atomic; this checkpoint only fixes
  the stale same-name boxed-local aliasing exposed by the spawn guard.

## 2026-04-19 Codex checkpoint: closure guard hygiene

Status: behavior-neutral regression-script/doc hygiene after `b85e55a5`.

Applied:

- `regression_tests/spawn_capture_block_param_repro.sh` is no longer labelled
  known-red. Exit `1` is now documented as the fixed forward-guard state, and
  the fixed message says `fixed: both probes printed _ok markers`.
- `regression_tests/conditional_closure_capture_repro.sh` and
  `regression_tests/escaping_branch_closure_capture_repro.sh` now refer to
  I14-owner-aware boxed-local dominance instead of the old I14-monotonic name.
- `escaping_branch_closure_capture_repro.sh` now runs its compiled binary via
  `scripts/run_safe.sh`, matching the project Safe Testing Protocol.
- `docs/closure_env_abi_p1_plan.md` no longer says R9 flips the known-red
  guard; R2/R9 are documented as keeping the historical spawn/fanout bug fixed.

Verification:

- `bash -n regression_tests/spawn_capture_block_param_repro.sh regression_tests/conditional_closure_capture_repro.sh regression_tests/escaping_branch_closure_capture_repro.sh`
  — clean.
- `regression_tests/spawn_capture_block_param_repro.sh bin/crystal_v2; echo SPAWN_RC:$?`
  — both probes `_ok`, `SPAWN_RC:1`.
- `regression_tests/conditional_closure_capture_repro.sh bin/crystal_v2; echo CONDITIONAL_RC:$?`
  — correct `12/12`, `CONDITIONAL_RC:1`.
- `regression_tests/escaping_branch_closure_capture_repro.sh bin/crystal_v2; echo ESCAPING_RC:$?`
  — correct branch-local escape output, `ESCAPING_RC:1`.

## 2026-04-19 Codex checkpoint: P1 shape guard added

Status: additive regression guard only. No compiler behavior changed.

Applied:

- Added `regression_tests/p1_ir_shape_check.sh`.
- The script builds a focused fixture, emits HIR and LLVM IR, dynamically
  extracts the captured block proc symbol from HIR, and checks the current
  hybrid P1 boundary:
  - HIR has boxed capture metadata in `make_closure` dumps.
  - User-visible Proc values are materialized by `make_proc`.
  - The focused heap block `Proc#call` does not carry hidden capture args.
  - LLVM does not use `%__crystal_proc` as a by-value Proc representation.
  - The captured block function receives `ptr %__closure_env` and reads through
    it, not through closure-cell globals.
  - The heap Proc object stores `fn@0` and `env@8`, then dispatch passes env to
    the loaded function pointer.

Verification:

- `bash -n regression_tests/p1_ir_shape_check.sh`
  — clean.
- `regression_tests/p1_ir_shape_check.sh bin/crystal_v2; echo P1_SHAPE_RC:$?`
  — `p1_ir_shape_ok captured_fn=__crystal_block_proc_1`, `P1_SHAPE_RC:0`.
- `regression_tests/spawn_capture_block_param_repro.sh bin/crystal_v2`
  — fixed-state exit `1`; both `_ok` markers.
- `regression_tests/conditional_closure_capture_repro.sh bin/crystal_v2`
  — fixed-state exit `1`; correct `12/12`.
- `regression_tests/escaping_branch_closure_capture_repro.sh bin/crystal_v2`
  — fixed-state exit `1`; correct branch-local escape output.
- `git diff --check`
  — clean.

Boundary:

- This guard intentionally documents the current hybrid boundary. It does not
  claim the larger closure-env ABI cleanup is complete.
- `@proc_captures_by_value`, `@closure_ref_cells`, and the dual-mode
  `lower_block_to_proc` legacy path remain live and should not be removed
  outside a coupled ABI cleanup.

## 2026-04-19 Codex checkpoint: hybrid boundary source guard added

Status: additive source-level regression guard only. No compiler behavior
changed.

Applied:

- Added `regression_tests/p1_hybrid_boundary_guard.sh`.
- The guard intentionally checks for the current mixed state:
  - `@proc_captures_by_value` declaration and hidden capture append paths.
  - `@closure_ref_cells` / `@closure_ref_prefer_cell` read/write fallback
    anchors.
  - `lower_block_to_proc` dual-mode split: heap path through `MakeProc`, legacy
    raw callback path through closure cells and `FuncPointer`.
  - Plan/TODO/collab docs still mark these as intentional pre-cleanup anchors.

Verification:

- `bash -n regression_tests/p1_hybrid_boundary_guard.sh`
  — clean.
- `regression_tests/p1_hybrid_boundary_guard.sh; echo HYBRID_RC:$?`
  — `p1_hybrid_boundary_ok`, `HYBRID_RC:0`.
- `regression_tests/p1_ir_shape_check.sh bin/crystal_v2; echo P1_SHAPE_RC:$?`
  — `p1_ir_shape_ok captured_fn=__crystal_block_proc_1`, `P1_SHAPE_RC:0`.
- `git diff --check`
  — clean.

Boundary:

- This guard is presence-based by design. It is not semantic proof that legacy
  paths are correct.
- When the coupled closure-env ABI cleanup intentionally removes these anchors,
  this guard and the status docs must be updated in the same commit.

## 2026-04-19 Codex checkpoint: dead Proc hidden-capture map removed

Status: narrow dead-code cleanup. No semantic behavior change intended.

Finding:

- `@proc_captures_by_value` had no producer assignment in the current branch.
- The only writes were copy-propagation writes that could only copy an existing
  entry.
- The only consumers were hidden-arg append checks in the HIR `Proc#call`
  intercepts.

Applied:

- Removed `@proc_captures_by_value`.
- Removed the dead copy-propagation sites.
- Removed the dead hidden-arg append checks from both HIR `Proc#call`
  intercepts.
- Updated `regression_tests/p1_hybrid_boundary_guard.sh` so it now rejects
  reintroducing `@proc_captures_by_value` while still guarding the live
  `@closure_ref_cells` and dual-mode `lower_block_to_proc` anchors.

Verification:

- `rg -n "@proc_captures_by_value" src/compiler/hir/ast_to_hir.cr; echo RG_SRC_RC:$?`
  — no matches, `RG_SRC_RC:1`.
- `crystal build src/crystal_v2.cr -o bin/crystal_v2 --error-trace`
  — green with only the known `Random::DEFAULT` warning.
- `regression_tests/p1_hybrid_boundary_guard.sh`
  — `p1_hybrid_boundary_ok`.
- `regression_tests/p1_ir_shape_check.sh bin/crystal_v2`
  — `p1_ir_shape_ok captured_fn=__crystal_block_proc_1`.
- `regression_tests/spawn_capture_block_param_repro.sh bin/crystal_v2`
  — fixed-state exit `1`; both `_ok` markers.
- `regression_tests/conditional_closure_capture_repro.sh bin/crystal_v2`
  — fixed-state exit `1`; correct `12/12`.
- `regression_tests/escaping_branch_closure_capture_repro.sh bin/crystal_v2`
  — fixed-state exit `1`; correct branch-local escape output.
- `regression_tests/run_mini_oracles.sh bin/crystal_v2`
  — `6 passed, 0 failed out of 6`.
- `regression_tests/run_combined.sh bin/crystal_v2 4`
  — `31 passed, 0 failed out of 31`.
- `regression_tests/run_all.sh bin/crystal_v2 4`
  — `146 passed, 0 failed out of 146`.
- `git diff --check`
  — clean.

Boundary:

- Legacy `@closure_ref_cells` and the dual-mode `lower_block_to_proc` path
  remain live. Removing those is still behavior-changing ABI cleanup work.

## 2026-04-19 Codex checkpoint: raw callback shape guard added

Status: additive regression guard only. No compiler behavior changed.

Applied:

- Added `regression_tests/p1_raw_callback_shape_check.sh`.
- The guard compiles and runs a focused direct-`yield` callback fixture:
  `def each_once(&block : Int32 ->); yield 3; end`, with a mutable captured
  `sum`.
- It asserts:
  - runtime output is `4`;
  - focused HIR initializes a `__closure_cell_N`;
  - focused HIR creates a bare `func_pointer @__crystal_block_proc_N`;
  - focused HIR does not materialize `make_proc` / `make_closure`;
  - the raw block function reads and writes the same closure cell and does not
    use `@__closure_env`.

Verification:

- `bash -n regression_tests/p1_raw_callback_shape_check.sh`
  — clean.
- `regression_tests/p1_raw_callback_shape_check.sh bin/crystal_v2; echo RAW_RC:$?`
  — `p1_raw_callback_shape_ok block_fn=__crystal_block_proc_1 cell=__closure_cell_2`, `RAW_RC:0`.

Boundary:

- This pins the currently intentional legacy raw callback shape. It should be
  updated only when the remaining closure-env ABI cleanup deliberately replaces
  that shape.

## 2026-04-19 Codex/Spark checkpoint: raw callback env carrier conclusion

Status: read-only architecture conclusion. No code changed.

Finding:

- Raw callback lowering cannot gain a hidden env pointer as a local cleanup.
- `lower_block_to_proc` is explicitly dual-mode:
  - heap path: captures become env slots and the value returns through
    `emit_make_proc_value(...)`;
  - raw path: captures still use `@closure_ref_cells`, and the returned value
    is a bare `FuncPointer`.
- Call sites append only that single callback value to arguments.
- MIR `Yield` lowering calls `builder.call_indirect(block_val, args, ...)`;
  it does not prepend or recover an env value.

Conclusion:

- Replacing `@closure_ref_cells` for raw callbacks requires changing the raw
  callback carrier, callback function signature, and MIR yield dispatch
  together.
- The safe incremental knob is still `block_arg_requires_heap_proc?`: move only
  specific env-sensitive block callsites to the existing heap Proc path, while
  leaving direct-`yield` raw callbacks unchanged until a coupled ABI change is
  planned.

## 2026-04-19 Codex checkpoint: MIR Yield type-only heap dispatch rejected

Status: local experiment reverted; no source behavior change committed.

Experiment:

- Patched MIR `lower_yield` to call existing `call_heap_proc(block_val, args,
  return_type)` whenever `block_val`'s HIR type descriptor had
  `TypeKind::Proc`.
- This was intended as a narrow defense-in-depth guard for Proc-typed yield
  targets, while leaving `TypeRef::POINTER` raw callbacks unchanged.

Result:

- Rejected. The combined suite regressed badly under the candidate binary,
  proving that some raw callback carriers are still Proc-typed in HIR while the
  actual ABI value is a bare function pointer.
- After reverting the source and rebuilding `bin/crystal_v2`, the broad red was
  gone. A repeated combined run reached `30 passed, 1 failed`; the single
  failure was `test_edge_string_prefix_suffix` linking with `library 'gc' not
  found`.
- Focused rerun with the same `LIBRARY_PATH=/opt/homebrew/lib` compiled,
  linked, and ran `test_edge_string_prefix_suffix` successfully, printing
  `edge_sb_prefix_ok`.

Conclusion:

- `TypeKind::Proc` is not a safe MIR `Yield` carrier discriminator in the
  current hybrid ABI.
- Any future yield rewrite needs an explicit HIR/MIR carrier marker or value
  provenance check that distinguishes heap `MakeProc` objects from raw callback
  function pointers.
- Process note: after reverting source from a candidate, rebuild
  `bin/crystal_v2` before rerunning suites; otherwise the runner tests a stale
  candidate binary.

## 2026-04-19 Codex checkpoint: MIR Yield carrier guard landed

Status: additive regression guard; no ABI behavior changed.

Applied:

- `src/compiler/mir/hir_to_mir.cr` now marks `lower_yield` with
  `MIR_YIELD_DISPATCH_MODE: raw_fnptr_only`.
- `regression_tests/p1_hybrid_boundary_guard.sh` now checks the MIR
  `lower_yield` window for:
  - the raw `builder.call_indirect(block_val, args, convert_type(yield_type))`
    dispatch;
  - the `raw_fnptr_only` mode marker;
  - absence of `call_heap_proc` in that window.
- The guard also checks the plan/TODO/collab text that records the rejected
  type-only `TypeKind::Proc` heuristic.

Verification:

- `git diff --check` — clean before commit.
- `bash -n regression_tests/p1_hybrid_boundary_guard.sh` — clean.
- `LIBRARY_PATH=/opt/homebrew/lib regression_tests/p1_hybrid_boundary_guard.sh`
  — `p1_hybrid_boundary_ok`.
- `crystal build src/crystal_v2.cr -o bin/crystal_v2 --error-trace` — green,
  known `Random::DEFAULT` warning only.
- `LIBRARY_PATH=/opt/homebrew/lib regression_tests/p1_raw_callback_shape_check.sh bin/crystal_v2`
  — `p1_raw_callback_shape_ok ...`.
- `LIBRARY_PATH=/opt/homebrew/lib regression_tests/p1_ir_shape_check.sh bin/crystal_v2`
  — `p1_ir_shape_ok ...`.
- `LIBRARY_PATH=/opt/homebrew/lib regression_tests/run_mini_oracles.sh bin/crystal_v2`
  — `6 passed, 0 failed out of 6 tests`.

Broad baseline:

- `LIBRARY_PATH=/opt/homebrew/lib regression_tests/run_all.sh bin/crystal_v2 4`
  — `145 passed, 1 failed out of 146 tests`; sole failure was compile-phase
  `test_generics_stack` with truncated stderr in the suite summary.
- Focused falsifier:
  `LIBRARY_PATH=/opt/homebrew/lib bin/crystal_v2 regression_tests/test_generics_stack.cr -o /tmp/test_generics_stack && scripts/run_safe.sh /tmp/test_generics_stack 5 512`
  compiled, linked, ran, and printed the expected stack outputs ending in
  `done`.

Conclusion:

- The guard commit is behavior-neutral.
- The current actionable closure-env boundary remains unchanged: do not rewrite
  MIR `Yield` based on Proc type alone; future raw callback ABI work needs an
  explicit carrier/provenance marker.
