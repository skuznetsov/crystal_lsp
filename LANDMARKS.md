# LANDMARKS

Updated: 2026-04-20
Context: compiler/bootstrap/stage2-stability

This file is the active working set only. Historical landmarks before this
checkpoint remain recoverable from git history, especially:

- `15e448b9:LANDMARKS.md`
- archived full-file SHA256:
  `d43826fdcc2277b6075026244764a84d0069d1a30b675642b603f3511b14a1e5`

## Active Bootstrap Gate

[LM-462|verified]: Bootstrap semantic-equivalence scaffolding exists as a thin
scripts-only layer over the current bootstrap ladder. `scripts/build_bootstrap_stages.sh`
wraps `scripts/bootstrap_chain.sh` and exposes stable names
`s1_bootstrap`..`s5b`; `scripts/emit_bootstrap_ir.sh` emits HIR/MIR/LLVM for a
compiler/corpus pair under `scripts/run_safe.sh`; `scripts/normalize_bootstrap_ir.sh`
strips known non-semantic ids, tmp paths, temp suffixes, and stub-name hashes;
`scripts/compare_bootstrap_stages.sh` diffs normalized S1..S5 dumps against
`regression_tests/bootstrap_semantic_corpus.cr`. Evidence: `bash -n` is green,
one emit smoke with `bin/crystal_v2` produced all three artifacts, and a
synthetic five-stage directory where all stage names point at the same compiler
prints `SEMANTIC_EQ: S1..S5 ok`. Boundary: this is only the gate scaffold; it
does not prove the real `original -> stage1 -> s2b -> s3b -> s4b -> s5b` chain
is green. {F/G/R: 0.90/0.58/0.92} [verified]

[LM-463|verified]: The first real use of the bootstrap semantic gate stops at
`s1 -> s2b`, before any HIR/MIR/LLVM comparison is possible. Command:
`BOOTSTRAP_STAGE_OUT=/tmp/cv2_bs_s2 BOOTSTRAP_CHAIN_STAGES=2
BOOTSTRAP_TIMEOUT_SEC=300 BOOTSTRAP_MEM_MB=4096 scripts/build_bootstrap_stages.sh
--stages 2 --out /tmp/cv2_bs_s2`. Stage1 built with host Crystal and both
plain/no-prelude smokes passed. Stage2 self-host build was killed by
`scripts/run_safe.sh`: `[KILL] Timeout after 300s (FDs: 12, RSS:
2281984KB)`, no `/tmp/cv2_bs_s2/cv2_s2` was produced, and the last initial
trace reached `lower_main: exprs=30`. Boundary: do not advance to `s3b+` until
this stage2 stall is explained. {F/G/R: 0.93/0.50/0.95} [verified]

[LM-464|verified]: The stage2 `lower_main: exprs=30` timeout has been refined
to a HIR pending-lowering queue explosion, not a stuck top-level expression.
`DEBUG_MAIN=1 DEBUG_MAIN_PROGRESS_EVERY=1` showed all 30 main expressions start
and return; expr 29 took about `9.3s`, expr 30 took about `80.9s`, and stage2
still timed out. A focused rerun with `CRYSTAL_V2_STOP_AFTER_HIR=1
CRYSTAL_V2_PHASE_STATS=1 CRYSTAL_V2_LOWER_PROGRESS=1` timed out before the
STOP_AFTER_HIR gate but entered `process_pending_lower_functions`: the queue
reached about `78k` entries (`idx=9877/78012`, `idx=12022/76438`) and visible
entries were broad compiler-container `#inspect`, `#to_s`, and `#object_id`
instantiations. Boundary: stop chasing `lower_main` expr 30; localize pending
queue producers. {F/G/R: 0.94/0.55/0.96} [verified]

[LM-465|verified]: `CRYSTAL_V2_PENDING_EXPLOSION_TRACE=1` identifies the first
observed deep Array `#inspect` enqueue during the stage2 pending explosion.
Evidence: a build of `/tmp/cv2_pending_trace_ctx` succeeded, and the focused
run emitted `[PENDING_EXPLOSION] first deep Array inspect enqueued source=defer
current=Object#inspect depth=1 queue=12325
name=Array(Array(Array(Tuple(UInt32, Array(Hash(String, UInt32))))))#inspect$IO`
before `[MAIN] expr 30/30`. Boundary: the first observed trigger is
`Object#inspect` fallback lowering on a deep compiler-container Array shape,
not the later `RelatedSpan` symptom. {F/G/R: 0.92/0.52/0.94} [verified]

[LM-466|verified]: Virtual-target diagnostics confirm that the first deep
Array `#inspect` enqueue is caused by eager virtual-target replay from broad
`Object` targets. The decisive sequence in `/tmp/cv2_s2_vtarget_diag.log` is:
`record parent=Object method=to_s args=[405]`, replay of the deep Array child,
`record parent=Object method=inspect args=[405]`, replay of the same child, and
then `[PENDING_EXPLOSION] ... current=Object#inspect ...`. The same log also
shows broad early `Reference#object_id` replay over many compiler-internal
Array/Hash shapes. Boundary: virtual replay is a contributor, but any replay
gate must preserve vdispatch-table completeness. {F/G/R: 0.94/0.62/0.95}
[verified]

## Refuted Bootstrap Fix Branches

[LM-467|refuted]: Broad virtual-target replay gating alone is not a sufficient
fix for the stage2 `STOP_AFTER_HIR` timeout. Guard A skipped immediate
`Object`/`Reference` replay in `record_virtual_target` while
`@lazy_rta_active == false`; it lowered the first deep `#inspect` queue from
`12325` to `9866`, but `[PENDING_EXPLOSION]` still appeared and the 120s
diagnostic still timed out. Extended guard A2 also skipped broad ancestors in
`replay_virtual_targets_for_registered_class` and local call/member replay
loops before lazy RTA; it removed the first `[PENDING_EXPLOSION]` line, but the
300s `STOP_AFTER_HIR` run still timed out: `process_pending` took `248224.0ms`,
lowered `61454` functions, grew HIR functions `3088 -> 64182`, then began
another pending/safety-net pass from about `2300` queued functions. Boundary:
do not land broad replay gating by itself. {F/G/R: 0.95/0.60/0.95} [verified]

[LM-468|refuted]: Emit-only pruning, and the replay+emit combination, are not
the next sufficient fix either. A bounded `emit_all_tracked_signatures` guard
for universal `inspect/to_s/object_id/to_json` on deep generic container owners
still timed out at the original frontier; the first `[PENDING_EXPLOSION]`
remained under `Object#inspect` and the run did not advance into
`emit_tracked_sigs`. The combined patch (broad replay gating + emit pruning)
also timed out: `/tmp/cv2_s2_combo_emit_replay.log` still showed
`process_pending` lowering `61454` functions, HIR functions growing
`3088 -> 64185`, and `[PHASE_STATS] process_pending: ... in 260147.0ms`.
Boundary: do not retry replay/emit heuristics alone; the live blocker is still
growth inside `process_pending_lower_functions` and its active producers.
{F/G/R: 0.94/0.58/0.95} [verified]

[LM-469|refuted]: A defer/enqueue guard for universal helper families on deep
generic/compiler-internal owners did not move the active frontier. The local
experiment added a narrow guard inside `lower_function_if_needed_impl` before
the `inside_lowering?` pending append, targeting
`hash/to_json/to_i/inspect/to_s/object_id` on deep `Array/Hash/Tuple` and
compiler-internal owners unless demanded by RTA/AST reachability. The focused
300s diagnostic still showed the old frontier: first `[PENDING_EXPLOSION]`
under `Object#inspect` at queue `12325`, `[LOWER] p0 #9600 idx=9877/78012`,
and broad helper-family entries around the same queue positions. The patch was
reverted. Boundary: do not retry name-family enqueue guards without better
provenance accounting. {F/G/R: 0.88/0.48/0.90} [verified]

## Active Working Hypothesis

[LM-470|hypothesis]: The current bootstrap blocker is not one universal-method
family but missing demand provenance. Multiple producers can turn potential
targets into pending work: virtual replay, `lower_virtual_target_owner`,
`remember_callsite_arg_types`, direct body lowering, RTA call records, and
late safety nets. The next useful step is not another name-based guard; it is
fast `--no-prelude` oracle coverage plus enqueue-provenance accounting by
`source -> family -> owner-base -> current function`. {F/G/R: 0.65/0.55/0.70}
[hypothesis]

[LM-471|verified]: Fast p2 no-prelude sentinels now protect the bootstrap
debug loop from using full `s1 -> s2b` as the first falsifier. Evidence:
`regression_tests/p2_pending_budget_no_prelude.sh /tmp/cv2_pending_sources`
prints `p2_pending_budget_no_prelude_ok process_delta=25 emit_delta=7
lower_missing_delta=30 total=103 max_queue=57`;
`regression_tests/p2_universal_helper_fanout_no_prelude.sh
/tmp/cv2_pending_sources` prints
`p2_universal_helper_fanout_no_prelude_ok deep_helpers=0`;
`regression_tests/p2_bootstrap_semantic_emit_oracle.sh /tmp/cv2_pending_sources`
prints `p2_bootstrap_semantic_emit_oracle_ok`. Boundary: these are fast
sentinels, not bootstrap proof. {F/G/R: 0.92/0.45/0.94} [verified]

[LM-472|verified]: Periodic pending-source diagnostics now expose the dominant
producer families before the 120s timeout. With
`DEBUG_PENDING_SOURCES=1 DEBUG_PENDING_SOURCES_SAMPLES=1
DEBUG_PENDING_SOURCES_EVERY=5000 DEBUG_PENDING_SOURCES_TOP=15
CRYSTAL_V2_PENDING_EXPLOSION_TRACE=1 CRYSTAL_V2_STOP_AFTER_HIR=1
CRYSTAL_V2_PHASE_STATS=1 CRYSTAL_V2_LOWER_PROGRESS=1`, the focused run timed
out as expected but printed `[PENDING_SOURCES]` snapshots at queue
`5000..35000`. At queue `35000`, the dominant families were `Array#to_s: 5479`,
`Array#inspect: 5476`, `Array#exec_recursive: 5448`, `Array#object_id: 2741`,
`Hash::Entry#to_s: 1221`, `Hash::Entry#inspect: 814`, `Hash#to_s: 812`,
`Hash#inspect: 810`, and `Hash#exec_recursive: 798`. Boundary: next work should
target the source of recursive formatting demand, not another isolated
`Object#inspect` guard. {F/G/R: 0.93/0.55/0.94} [verified]

[LM-473|verified]: Context-enhanced pending-source samples identify the current
dominant source contexts. With sample context enabled, the 80s run timed out as
expected but showed `Array#to_s` samples enqueued from `Object#to_s`,
`Array#inspect` from `Object#inspect`, `Array#object_id` from
`Reference#same?`, `Hash#to_s` from `Object#to_s`, `Hash#inspect` from
`Object#inspect`, and `Hash#each` from `Dir::Globber#glob`. Boundary: the next
bounded fix/reducer should target broad universal fallback adapter replay, not
deep-container name guards. {F/G/R: 0.94/0.58/0.94} [verified]

[LM-474|verified]: Virtual-target context logging confirms the earliest
broad-parent replay callsites. In the 45s diagnostic,
`record parent=Reference method=object_id args=[] ... current=Reference#same?`
immediately replayed `Array(Float64)` under `Reference`, and
`record parent=Object method=to_s args=[405] ... current=Object#to_s`
immediately replayed `Array(Float64)` under `Object`. Boundary: the next
candidate fix should consider self-calls inside root fallback methods as
current-owner static/demand-local operations, not global subclass replay.
{F/G/R: 0.94/0.60/0.94} [verified]

[LM-475|refuted]: Suppressing exact RTA-called marking during speculative
virtual-target replay is not sufficient. The uncommitted experiment added a
replay-depth guard around `lower_virtual_target_owner` and made
`record_pending_callee_for_rta` ignore functions enqueued inside that depth.
Fast p2 guards stayed green, but the 120s full diagnostic still timed out with
the same first `[PENDING_EXPLOSION]` at queue `12325` and `[PENDING_SOURCES]`
snapshots through queue `35000`. The patch was reverted. Boundary: the active
fanout is not explained by exact `@rta_called_methods` marking alone.
{F/G/R: 0.90/0.50/0.92} [verified]

[LM-476|obj]: `regression_tests/p2_root_self_replay_no_prelude.sh` is the
small synthetic oracle for the broad-root replay corridor. It defines
`Object#to_s`, `Object#inspect`, `Reference#same?`, and nested `Box(T)` /
`Pair(A, B)` owners under `--no-prelude`; current baseline:
`process_delta=20`, `total=47`, `object_replays=28`,
`reference_replays=21`, `deep_owner_replays=12`. This proves the corridor is
exercised without full-prelude bootstrap and gives future fixes a fast movement
signal before `s1 -> s2b`.
{F/G/R: 0.93/0.55/0.94} [verified]

[LM-477|refuted]: Filtering `rta_method_part_matches_owner?` so broad
`Object` / `Reference` receivers do not ancestor-match universal helper method
parts is not sufficient. The uncommitted experiment built successfully and kept
fast p2 guards green, but `p2_root_self_replay_no_prelude.sh` was unchanged:
`process_delta=20`, `total=47`, `object_replays=28`,
`reference_replays=21`. The patch was reverted. Boundary: exact queued method
names / replay-generated wrappers are enough to keep the synthetic corridor
alive even without broad ancestor matching.
{F/G/R: 0.92/0.45/0.94} [verified]

[LM-478|refuted]: Combining broad-root immediate replay gating with the
broad-root helper RTA filter is still not enough. Synthetic root oracle replay
counts moved (`Object 28->16`, `Reference 21->16`) but `process_delta=20` and
`total=47` did not move. A 120s full `STOP_AFTER_HIR` diagnostic still timed
out; queue reached `40k`, first deep explosion moved to the deep
`Array#inspect` owner itself, and top producers remained universal helper
families (`Array#to_json`, `Array#inspect`, `Array#to_s`,
`Array#exec_recursive`, `Array#hash`, `Hash#...`). The source patch was
reverted. Boundary: partial replay reduction is still symptomatic.
{F/G/R: 0.91/0.55/0.93} [verified]

[LM-479|verified]: RTA keep-reason diagnostics identify the active admission
mechanism. Env-gated `DEBUG_RTA_KEEP_REASONS=1` reports top keep/defer buckets
inside `process_pending_lower_functions`. In a 120s STOP_AFTER_HIR diagnostic,
the first snapshot at `idx=5000 queue=34512` was dominated by
`keep:exact_called`: `Array#to_s: 1469`, `Array#object_id: 738`,
`Hash#to_s: 650`, `Hash#object_id: 341`, `Hash::Entry#to_s: 314`.
Boundary: the next fix must explain why these concrete wrapper names are marked
exact-called; owner/method-part fallback is not the primary keeper at this
frontier. {F/G/R: 0.94/0.62/0.94} [verified]

[LM-480|verified]: `scripts/timeout_sample_lldb.sh` is useful on this
compiler. A 90s sampled STOP_AFTER_HIR run showed hotspots in string hashing,
`type_ref_for_name_inner`, `type_name_cache_depends_on_context?`, and
`lower_node/lower_expr`; LLDB backtrace caught nested
`force_lower_function_for_return_type -> lower_call -> lower_method` activity.
Boundary: current cost is HIR/type/name work from excessive admitted wrappers,
not LLVM or a single runtime tight loop. {F/G/R: 0.86/0.55/0.88} [verified]

## Active Strategy

- Main fast loop: `--no-prelude` oracles and focused STOP_AFTER_HIR budget
  checks.
- Integration gate: `s1 -> s2b` only after fast oracles are green.
- Rare full gate: `s1 -> s5b` plus normalized HIR/MIR/LL equality.
- Do not run `s3b+` while `s1 -> s2b` cannot produce `s2b`.
