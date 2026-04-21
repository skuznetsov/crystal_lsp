# Crystal V2 Bootstrap TODO

Updated: 2026-04-20
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

## Active Blocker

`s1 -> s2b` still times out before producing `s2b`.

Baseline integration command:

```bash
BOOTSTRAP_STAGE_OUT=/tmp/cv2_bs_s2 \
BOOTSTRAP_CHAIN_STAGES=2 \
BOOTSTRAP_TIMEOUT_SEC=300 \
BOOTSTRAP_MEM_MB=4096 \
  scripts/build_bootstrap_stages.sh --stages 2 --out /tmp/cv2_bs_s2
```

Current diagnosis:

- `lower_main` is exonerated: all 30 top-level expressions start and return.
- The live failure is `process_pending_lower_functions` fanout.
- The decisive pass reaches about `78k` queued functions, lowers about `61k`,
  and grows HIR from about `3k` to `64k` functions before timeout.
- Dominant families are broad fallback helpers on compiler-internal containers:
  `Array#to_s`, `Array#inspect`, `Array#exec_recursive`,
  `Array#object_id`, `Hash#to_s`, `Hash#inspect`,
  `Hash#exec_recursive`, and `Hash::Entry#to_s/#inspect`.
- Current source contexts:
  - `Object#to_s` enqueues `Array#to_s` / `Hash#to_s`
  - `Object#inspect` enqueues `Array#inspect` / `Hash#inspect`
  - `Reference#same?` enqueues `Array#object_id`
  - `Dir::Globber#glob` enqueues some `Hash#each`

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

Common lesson: name-family containment can remove individual symptoms but has
not yet removed the underlying broad fallback demand leak.

## Fast Oracles

Run before expensive bootstrap attempts:

```bash
regression_tests/p2_bootstrap_semantic_emit_oracle.sh bin/crystal_v2
regression_tests/p2_pending_budget_no_prelude.sh bin/crystal_v2
regression_tests/p2_root_self_replay_no_prelude.sh bin/crystal_v2
regression_tests/p2_universal_helper_fanout_no_prelude.sh bin/crystal_v2
```

Expected current signals:

- `p2_bootstrap_semantic_emit_oracle_ok`
- `p2_pending_budget_no_prelude_ok ... total=103 max_queue=57`
- `p2_root_self_replay_no_prelude_ok process_delta=20 total=47 ...`
- `p2_universal_helper_fanout_no_prelude_ok deep_helpers=0`

Boundary: `src/crystal_v2.cr --no-prelude` still exits `11` in an
inline-yield recursion / force-return corridor before it can serve as a green
pending-budget oracle.

## Next Work

1. Verify whether calls on `self` inside root fallback methods should be
   constrained to the current owner instead of replaying every subclass/generic
   instantiation.
2. Attempt a bounded behavior fix only after the root self-replay oracle and
   existing p2 guards define the expected movement.
3. After fast oracles pass, run the `s1 -> s2b` integration gate.
4. If `s2b` is produced, compare `s1_bootstrap` and `s2b` on the fixed corpus
   before trying `s3b+`.

## Stop Conditions

- Do not run `s3b+` while `s1 -> s2b` cannot produce `s2b`.
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
