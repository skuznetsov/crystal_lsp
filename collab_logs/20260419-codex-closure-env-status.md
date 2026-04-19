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
