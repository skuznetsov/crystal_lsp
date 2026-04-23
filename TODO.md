# Crystal V2 Bootstrap TODO

Updated: 2026-04-23
Branch: `codegen`

This is the active working backlog only. Historical detail is in git history,
especially `65eb6f62^:TODO.md`. Reusable evidence lives in `LANDMARKS.md`.

## Goal

Reach a clean bootstrap corridor:

`original -> stage1 -> s2b -> s3b -> s4b -> s5b`

with normalized HIR/MIR/LLVM semantic equivalence across stages.

Working policy:

- Prefer fast `--no-prelude` oracles.
- Use `s1 -> s2b` as the main integration gate.
- Run `s1 -> s5b` rarely, after `s1 -> s2b` is clean.

## Current Checkpoint

Direct `s1 -> s2` now produces a stage2 compiler in the focused gate:

```bash
crystal build src/crystal_v2.cr -o /tmp/cv2_hir_emit_stop --error-trace
CRYSTAL_V2_PHASE_STATS=1 \
  scripts/run_safe.sh /tmp/cv2_hir_emit_stop 300 4096 \
    src/crystal_v2.cr -o /tmp/cv2_s2_hir_emit_stop
```

Verified signal: `[EXIT: 0] after ~265s`, produced `/tmp/cv2_s2_hir_emit_stop`.

Fast stage2 HIR emit also passes:

```bash
regression_tests/p2_selfhost_hir_emit_no_prelude.sh /tmp/cv2_s2_hir_emit_stop
```

Verified signal: `p2_selfhost_hir_emit_no_prelude_ok`.

The full wrapper gate now reaches the generated stage2 compiler, then stops on
the generated-compiler smoke:

```bash
BOOTSTRAP_STAGE_OUT=/tmp/cv2_bs_s2 \
BOOTSTRAP_CHAIN_STAGES=2 \
BOOTSTRAP_TIMEOUT_SEC=300 \
BOOTSTRAP_MEM_MB=4096 \
  scripts/build_bootstrap_stages.sh --stages 2 --out /tmp/cv2_bs_s2
```

Current signal after the semantic helper materialization fixes: stage1 build +
plain/no-prelude smokes pass; stage2 build passes (latest accepted wrapper:
`309.04s` wall, `run_safe` child `[EXIT: 0] after ~237s`, peak RSS about
`3.43GB`, artifact `/tmp/cv2_bs_s2_semantic_helpers/cv2_s2`); generated stage2
plain smoke still times out in the prelude loading branch after confirming the
prelude exists. The generated stage2 no-codegen no-prelude smoke moved from
`T#lookup_macro` / `NameResolver#current_owner_symbol` to the
`TypeInferenceEngine#guard_watchdog!` stub; the HIR/MIR shape root is now fixed:
`guard_watchdog!` is materialized immediately as a leaf guard instead of being
left in a stale deferred queue. The next generated-stage blocker must be
re-measured from a fresh `s2b`; direct `s1` full codegen still timed out under
the 300s sandbox gate during later lowering, while `STOP_AFTER_HIR` and p2
shape oracles pass. The first generated-stage blocker is therefore still after
HIR self-host materialization, not in the initial stage1 host build.

Current diagnosis / recently fixed roots:

- Bare receiverless `puts/print/p/pp` no longer fall through the late
  `Object#...` implicit-receiver fallback in `AstToHir#lower_call`. That
  fallback was missing the same builtin exemption already present in the
  earlier self-resolution branches, so fresh generated `s2b` no-prelude
  compiles could drift into receiver-call resolution and die in the helper
  tuple-iteration corridor (`Tuple$Heach$$block`) before the direct runtime
  print fallback had a chance to run. Evidence:
  `regression_tests/stage2_no_prelude_puts_runtime_repro.sh /tmp/cv2_puts_receiverfix`
  -> `not reproduced`;
  `regression_tests/p2_generated_stage2_no_prelude_interp.sh /tmp/cv2_puts_receiverfix`
  -> `p2_generated_stage2_no_prelude_interp_ok`;
  `regression_tests/p2_generated_stage2_no_prelude_puts_guard.sh /tmp/cv2_owned_return_fix3`
  -> `p2_generated_stage2_no_prelude_puts_guard_ok frontier=io_filedescriptor_tell`.
  The old generated-stage2 no-prelude `Tuple$Heach$$block` frontier is removed;
  the old synthetic-main MIR blockers (`Missing hash key: __crystal_main` and
  `MIR function stub not found for: __crystal_main`) are removed. Generated
  `s2b` now reaches `STUB CALLED: IO::FileDescriptor#tell`.
- Stage2 shape guard now protects four self-host codegen roots in one MIR
  gate (`regression_tests/p2_selfhost_stage2_shape_guard.sh`):
  - stale cache-only call return repair no longer rewrites
    `Slice(UInt8)#[]` from `UInt8` to stale container-shaped returns;
  - bare `return` in nilable functions now materializes a nil union value
    (`String#byte_index(Int32, Int32)` no longer emits bare MIR `ret`);
  - deferred runtime constants update `@constant_types` after real lowering
    (`CRYSTAL_SRC_PATH` now reads as `String`, not `VOID`, avoiding
    `Path | String` variant miswrap);
  - splat parameters are rebound to tuple locals in the method body, so
    `Dir.glob(*patterns, &block)` no longer self-recurses through its
    `_block_splat` wrapper.
  - nested inline-yield fallback no longer emits a call back to the currently
    lowered `_block_splat` wrapper. The fallback now resolves splat/block
    targets through the block-overload table and records the corrected call
    target without eagerly forcing the callee body.
  - scalar splat fallback targets now keep their `_block_splat` wrapper instead
    of being over-corrected to the `Enumerable` overload. This keeps
    `Dir.glob("pattern", &block)` from dispatching `String#each$block` inside
    `Dir.glob$Enumerable...`.
  - `SymbolCollector#@table_stack` is explicitly typed as
    `Array(SymbolTable)`, preventing V2 from widening it to
    `Array(SymbolTable) | Array(String)` and routing `current_table.lookup_macro`
    through `T#lookup_macro`.
  - trivial `NameResolver` zero-arg helpers are no longer required as generated
    compiler call targets; their bodies are inlined at source call sites, moving
    generated no-prelude smoke past the `current_owner_symbol` helper stub
    cluster.
  - `TypeInferenceEngine#guard_watchdog!` now bypasses deferred work-queue
    lowering as a leaf guard, so self-host HIR/MIR contains the helper body
    instead of leaving a concrete call target for LLVM to synthesize as an abort
    stub. A broad stale-Pending requeue was tested and rejected because it
    reopens the deep generic helper fan-out that lazy RTA intentionally prunes.
- Nilable query calls on concrete containers can now materialize inherited
  included-module implementations instead of falling back to the first fuzzy
  overload. This keeps `Array(Nil | Array(ExprId))#[]?$Int32` on the
  `Indexable#[]?` path instead of mis-targeting `#[]?$Range`.
- Semantic compiler cache key hashing no longer calls `.hash` on immediate
  primitive fields (`UInt64`, `Bool`) while self-hosting. The cache keys now
  combine object ids and booleans arithmetically, avoiding the generated
  stage2 `Object#hash` vdispatch corridor.
- `TypeInferenceEngine#primitive_metaclass?` no longer relies on flow narrowing
  across `type.is_a?(PrimitiveType) && type.name...`. It now explicitly casts to
  `PrimitiveType` before calling `#name`, so HIR emits
  `PrimitiveType#name -> String` followed by `String#ends_with?`, not stale
  `Hash(... )#ends_with?`.
- `Hash(String, Nil).new(block, initial_capacity:)` no longer resolves to the
  `default_value : V` overload. Generic overload matching now evaluates
  annotations in the requested concrete owner context, so `V` is `Nil` for
  `Hash(String, Nil)` instead of a wildcard.
- Explicit receiver block calls now keep the concrete generic receiver owner
  when searching block thunks. This removes late generic-module abort stubs such
  as `Indexable(T)#reverse_each$$block`; the self-host HIR trace now lowers
  concrete `Array(...)#reverse_each$block` targets instead.
- Default argument expansion now searches included module chains before final
  target canonicalization. This preserves `Enumerable#each_with_index(offset =
  0, &)` when reached through concrete Array/Slice owners, so zero-arg block
  calls become one-arg calls before block proc lowering.
- Direct LLVM small-Hash linear-scan overrides are disabled. They duplicated
  `Hash::Entry` layout knowledge in the backend and corrupted self-hosted
  `Hash(String, Nil)` / `Hash(String, T)` paths; normal HIR/MIR lowering now
  owns those method bodies.
- Exact-demand helper bodies invalidated by layout repair are requeued and can
  be processed again in the same pending pass. This removed late abort stubs for
  `Array(String)#increase_capacity` and `Array(Crystal::HIR::TypeRef)#to_unsafe`.
- HIR-only emit no longer depends on backend/runtime weak spots:
  - `--emit hir --no-link` stops after writing HIR when MIR/LLVM emit is not
    requested.
  - HIR pretty-printers avoid `Enumerable#join(io, ...)`, which pulled
    `IO::FileDescriptor#tell`.
  - CLI HIR output uses the same `LibC.open` / `LibC.close` pattern as LLVM
    output instead of `File.open` / `IO::FileDescriptor#system_close`.

Remaining risk:

- The current generated stage2 plain smoke times out in prelude loading after
  `prelude exists`. The current generated stage2 no-codegen no-prelude smoke
  times out after `STUB CALLED:
  CrystalV2::Compiler::Semantic::TypeInferenceEngine#guard_watchdog!`.
  Treat these as the next root-cause targets before any `s3b+` attempt.
- Stage2 still has a separate generic module block corridor around
  `Enumerable(T)#any?$$block`. A no-prelude function-definition HIR emit and a
  full `puts 42` smoke can still hang/abort there under the generated s2
  compiler. A broader implicit-self block receiver experiment was refuted
  because it caused an early `Index out of bounds` in self-host HIR lowering.
- `lower_missing` still grows HIR heavily during full self-compile
  (`~25k -> ~54k` functions in the latest focused s2 build). This no longer
  blocks producing s2 in the current gate, but it remains the main demand-driven
  cleanup target.
- Dominant families are broad fallback helpers on compiler-internal containers:
  `Array#to_s`, `Array#inspect`, `Array#exec_recursive`,
  `Array#object_id`, `Hash#to_s`, `Hash#inspect`,
  `Hash#exec_recursive`, and `Hash::Entry#to_s/#inspect`.
- Current source contexts:
  - `Object#to_s` enqueues `Array#to_s` / `Hash#to_s`
  - `Object#inspect` enqueues `Array#inspect` / `Hash#inspect`
  - `Reference#same?` enqueues `Array#object_id`
  - `Dir::Globber#glob` enqueues some `Hash#each`
- `DEBUG_RTA_KEEP_REASONS=1` shows the active `process_pending` frontier is
  dominated by `keep:exact_called`, not by owner/method-part fallback:
  `Array#to_s`, `Array#object_id`, `Hash#to_s`, `Hash#object_id`,
  `Hash::Entry#to_s`.
- `scripts/timeout_sample_lldb.sh` confirms the time is spent in HIR lowering /
  type-name lookup / string hashing, consistent with excessive admitted wrapper
  volume rather than a single tight runtime loop.

See `LANDMARKS.md` LM-463..LM-475 for detailed evidence and refutations.

## Refuted Fix Branches

Do not retry without new evidence:

- Broad `Object` / `Reference` virtual-target replay gating alone.
- `emit_all_tracked_signatures` universal-method pruning alone.
- Replay gating plus emit pruning combination.
- Defer/enqueue guard for universal helpers on deep generic owners.
- RTA replay-depth guard that prevents speculative replay enqueues from marking
  exact `@rta_called_methods`.
- `rta_method_part_matches_owner?` broad-root helper ancestor filter.
  - No movement on `p2_root_self_replay_no_prelude.sh`: `process_delta=20`,
    `object_replays=28`, `reference_replays=21` unchanged.
- Combined broad-root immediate-replay gate plus broad-root helper RTA filter.
  - Synthetic oracle reduced replay counts (`Object 28->16`, `Reference 21->16`)
    but not `process_delta` or `total`.
  - 120s `STOP_AFTER_HIR` diagnostic still timed out, with queue reaching `40k`
    and the same helper families (`Array#to_json`, `Array#inspect`,
    `Array#to_s`, `Array#exec_recursive`, `Array#hash`, `Hash#...`).
- Replacing `TypeInferenceEngine#guard_watchdog!` calls with direct
  `Frontend::Watchdog.check!` calls.
  - It removes the helper stub but duplicates watchdog lowering at every call
    site and fails the stage2 build envelope before producing `cv2_s2`.
- Changing `guard_watchdog!` visibility from private to public.
  - HIR still contains calls to `guard_watchdog!` but no function body; the
    missing-helper root is not method visibility.

Common lesson: name-family containment can remove individual symptoms but has
not yet removed the underlying broad fallback demand leak.

## Fast Oracles

Run before expensive bootstrap attempts:

```bash
regression_tests/p2_bootstrap_semantic_emit_oracle.sh bin/crystal_v2
regression_tests/p2_selfhost_hir_emit_no_prelude.sh bin/crystal_v2
regression_tests/p2_pending_budget_no_prelude.sh bin/crystal_v2
regression_tests/p2_root_self_replay_no_prelude.sh bin/crystal_v2
regression_tests/p2_universal_helper_fanout_no_prelude.sh bin/crystal_v2
regression_tests/p2_selfhost_stage2_shape_guard.sh bin/crystal_v2
```

Expected current signals:

- `p2_bootstrap_semantic_emit_oracle_ok`
- `p2_selfhost_hir_emit_no_prelude_ok`
- `p2_pending_budget_no_prelude_ok ... total=103 max_queue=57`
- `p2_root_self_replay_no_prelude_ok process_delta=20 total=47 ...`
- `p2_universal_helper_fanout_no_prelude_ok deep_helpers=0`
- `p2_selfhost_stage2_shape_guard_ok`
- `p2_generated_stage2_no_prelude_interp_ok`

Latest generated-stage2 frontier:

- `s1 -> s2b` builds with `/tmp/cv2_puts` in about `241s`.
- Generated `s2b` no-prelude no-codegen smoke moved past
  `Class$Dcrystal_type_id`, `Char$Hascii_control$Q`,
  `Printer$Dshortest$$Float32_IO`, and the top-level no-prelude `puts`
  semantic error. `regression_tests/p2_generated_stage2_no_prelude_interp.sh
  /tmp/cv2_puts` is now green.
- Generated `s2b` no longer aborts on `STUB CALLED: Tuple$Heach$$block`
  for the tiny no-prelude runtime repro `puts 7`. The root cause was that
  `AstToHir#emit_runtime_print_fallback` inferred "prelude IO print is
  available" from ambient method tables instead of the actual compile mode.
  In generated stage2 that drift disabled the runtime no-prelude print path
  and let `puts` fall back into the variadic tuple corridor. `AstToHir` now
  receives `options.no_prelude` from CLI and treats `--no-prelude` as a
  hard gate for runtime print fallback selection. Evidence:
  `regression_tests/stage2_no_prelude_puts_runtime_repro.sh
  /tmp/cv2_noprel_printfix` -> `not reproduced`, while
  `regression_tests/p2_generated_stage2_no_prelude_interp.sh
  /tmp/cv2_noprel_printfix` stays green.
- Root moved: type-literal `crystal_type_id`/`crystal_instance_type_id`
  must lower to an `Int32` type-id literal before both `lower_call` and
  `lower_member_access` rewrite type literals to static `Class.*` targets.
  `Char#ascii_control?` is a leaf predicate on the raw `Char` codepoint and
  now lowers inline as `self < 0x20 || self == 0x7f`. The shape guard rejects
  both stale `Class.crystal_type_id` / `Class#crystal_type_id` and
  `Char#ascii_control?` self-host MIR targets. Separately, `TypeInferenceEngine`
  debug strings now evaluate lazily, so disabled debug hooks no longer trigger
  `Object#to_s(io)` on compiler-internal objects and accidentally materialize
  float-printing stubs during generated-stage2 semantic inference. Receiverless
  semantic inference now also treats top-level `puts`/`print` as builtins,
  matching the HIR lowering corridor.

- Next frontier: re-measure the first failing generated-stage2 corridor after
  the no-prelude print-mode fix. The previous `Tuple$Heach$$block` repro is now
  green, so the next blocker must be rediscovered from the updated generated
  compiler rather than assumed from stale notes.

Boundary: `src/crystal_v2.cr --no-prelude` still exits `11` in an
inline-yield recursion / force-return corridor before it can serve as a green
pending-budget oracle.

## Next Work

1. Re-measure the next generated-stage2 failure after the no-prelude print-mode
   fix, starting from the fast no-prelude runtime/oracle corpus instead of full
   bootstrap.
2. Add a fast no-prelude oracle for the generated-stage2 `puts$String` hang, or
   reduce it to the smallest HIR/MIR shape that reproduces without full wrapper
   bootstrap.
3. Fix the generated-stage2 `String#each(&)` block stub in prelude loading.
4. Compare `s1_bootstrap` and `s2b` on the fixed no-prelude corpus before
   trying `s3b+`.
5. Add/inspect exact-called provenance for `record_pending_callee_for_rta` so
   the source of remaining `keep:exact_called Array#to_s` / `Hash#to_s` demand
   is explicit.
6. Verify whether broad fallback self-calls should mark exact concrete wrapper
   names as demanded, or whether they should remain virtual/demand-local until a
   real callsite asks for that concrete owner.

## Stop Conditions

- Do not run `s3b+` until generated `s2b` passes plain/no-prelude smokes and
  normalized corpus comparison is green.
- Do not increase timeout or memory to hide pending expansion.
- Do not modify stdlib/runtime.
- Do not land another name-family guard unless it measurably reduces the
  `~61k` process-pending expansion.
- If two more bounded containment fixes fail, pivot from heuristics to explicit
  demand-provenance design.

## Strategic Track

Architecture target:

- `PLAN_DEMAND_DRIVEN_REWRITE.md`
- `PLAN_DEMAND_DRIVEN_REWRITE_RFC.md`

Current short-term track: bootstrap containment plus fast no-prelude oracle
coverage, not a full compile-path switch.
