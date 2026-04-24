# LANDMARKS

Updated: 2026-04-23
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

[LM-475|verified]: The generated-stage2 no-prelude `Tuple$Heach$$block`
frontier was a receiverless-call resolution bug, not a print-runtime bug.
IR for the fresh self-host artifact (`/tmp/cv2_puts_stringfix_s2_ir.ll`) showed
the crashing `Tuple$Heach$$block` call came from `lower_call`'s
`explicit_call_target_known` helper over `{primary_mangled_name,
mangled_method_name}`, not from the runtime print fallback. The enabling source
bug was the final bare-call `Object` fallback in `AstToHir#lower_call`: unlike
the earlier self-resolution branches, it did not exempt `puts/print/p/pp`, so
generated `s2b` could incorrectly bind bare no-prelude `puts` to receiver-call
resolution and die before the direct runtime print corridor. Adding the missing
builtin exemption removes the old frontier. Evidence:
`regression_tests/stage2_no_prelude_puts_runtime_repro.sh /tmp/cv2_puts_receiverfix`
=> `not reproduced`;
`regression_tests/p2_generated_stage2_no_prelude_interp.sh /tmp/cv2_puts_receiverfix`
=> `p2_generated_stage2_no_prelude_interp_ok`;
`regression_tests/p2_generated_stage2_no_prelude_puts_guard.sh /tmp/cv2_owned_return_fix3`
=> `p2_generated_stage2_no_prelude_puts_guard_ok frontier=io_filedescriptor_tell`.
Boundary: this does not make generated no-prelude codegen fully green; the next
blocker is `STUB CALLED: IO::FileDescriptor#tell`. The synthetic-main MIR
blockers (`Missing hash key: __crystal_main`, then `MIR function stub not found
for: __crystal_main`) are removed by the function-name fallback fix in MIR
owned-return/stub lookup. {F/G/R: 0.92/0.70/0.93}
[verified]

[LM-480|verified]: The generated-stage2 `IO::FileDescriptor#tell` abort was a
HIR inherited-wrapper materialization bug, not a missing runtime helper. The
front-end resolved `IO::FileDescriptor#tell` to ancestor `IO#tell`, but
`lower_function_if_needed_impl` treated the ancestor body as sufficient and
skipped materializing the requested child symbol. The bounded fix switches the
"already lowered" gate and lowering state bookkeeping to the actual
materialized symbol, and lowers inherited instance wrappers under the requested
owner when the callsite needs a concrete child method body. Evidence:
`crystal build src/crystal_v2.cr -o /tmp/cv2_tell_fix --error-trace` succeeded;
plain `File.open { |f| f.tell }` HIR emitted by `/tmp/cv2_tell_fix` contains
only `IO#tell` (`rg -n "func @IO::FileDescriptor#tell|func @IO#tell"
/tmp/io_tell_probe_plain_fix.hir` => only `func @IO#tell`);
`scripts/run_safe.sh /tmp/cv2_tell_fix 420 4096 src/crystal_v2.cr -o
/tmp/cv2_tell_fix_s2` succeeded; `lldb --batch -o 'disassemble -n
IO$CCFileDescriptor$Htell' /tmp/cv2_tell_fix_s2` shows a real delegate body
calling `IO$CCFileDescriptor$Hpos`, not an abort stub; and
`regression_tests/p2_generated_stage2_no_prelude_puts_guard.sh
/tmp/cv2_puts_fix2` now also disassembles `IO$CCFileDescriptor$Hputs` and
verifies the nilary wrapper delegates to `print(Char)` instead of reusing the
string-overload body. Full self-host MIR emitted by `/tmp/cv2_puts_fix2`
contains `func @IO::FileDescriptor#puts(%0: Type#204) -> Nil` and a separate
`func @IO::FileDescriptor#puts$String(%0: Type#204, %1: String) -> Nil`; the
old generated-stage2 newline crash in `String$Hbytesize` is gone. Boundary:
generated no-prelude stage2 still is not green; the next blocker moved earlier
to a fresh crash right after `lower_main: exprs=1`. {F/G/R: 0.94/0.75/0.93}
[verified]

[LM-471|verified]: `Array(String)#each_index` fallback block-param inference
must yield `Int32`, not the element type. The generated-stage2 crash after
`lower_main: exprs=1` was reproduced as a segfault in
`__crystal_block_proc_291` because `Array(String)#each$block` passed an Int32
index to a callback materialized as `String ->`; host HIR/MIR showed
`func @__crystal_block_proc_291(%0: String)`. The root was
`fallback_block_param_types`, which only handled `*_with_index` as index-aware
and treated bare `each_index` like element-yielding `each`. After the fix,
fresh self-host HIR has `func @__crystal_block_proc_291(%2: 4)` and
`Array(String)#unsafe_fetch$Int32`; `regression_tests/p2_selfhost_stage2_shape_guard.sh
/tmp/cv2_emitblock_fix` passes and
`regression_tests/p2_generated_stage2_no_prelude_puts_guard.sh
/tmp/cv2_emitblock_fix` moves the frontier to
`hash_each_entry_with_index_null_block`. {F/G/R: 0.93/0.70/0.92}
[verified]

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

[LM-481|verified]: Concrete receiver block-target lookup fixes the
`Indexable(T)#reverse_each$$block` abort-stub corridor. The root cause was that
explicit receiver block lookup skipped receiver descriptors whose names
contained generic arguments, so calls on concrete `Array(...)` receivers could
fall back to the generic module block owner. The fix keeps
`yield_receiver_base_name(ctx.type_of(receiver_id))` for block-target lookup,
canonicalization, and block emit lookup. Evidence:
`DEBUG_CALL_TRACE=reverse_each DEBUG_HOOK_FILTER=reverse_each
CRYSTAL_V2_STOP_AFTER_HIR=1 scripts/run_safe.sh /tmp/cv2_commit_candidate 300
4096 src/crystal_v2.cr -o /tmp/cv2_commit_candidate_reverse_stop` exited 0,
and grep found no `Indexable(T)#reverse_each$block` trace while concrete
`Array(...)#reverse_each$block` targets were lowered. {F/G/R:
0.93/0.62/0.94} [verified]

[LM-482|verified]: Default argument expansion must search included modules
before final call-target canonicalization. The root cause was that
`apply_default_args` looked up the pre-canonical concrete owner only, then
parent classes, and missed defaulted module methods such as
`Enumerable#each_with_index(offset = 0, &)`. The fix walks the receiver owner's
included-module chain with `find_module_def_recursive_with_owner` and preserves
the found arena for parameter/default reads. Evidence:
`DEBUG_CALL_TRACE=each_with_index DEBUG_HOOK_FILTER=each_with_index
CRYSTAL_V2_STOP_AFTER_HIR=1 scripts/run_safe.sh /tmp/cv2_commit_candidate 300
4096 src/crystal_v2.cr -o /tmp/cv2_commit_candidate_each_stop` exited 0 and
showed repeated `after_args ... args=0` followed by `after_defaults ... args=1`
for concrete Array/Slice calls. {F/G/R: 0.93/0.62/0.94} [verified]

[LM-483|verified]: Direct LLVM small-Hash linear-scan overrides are unsound for
self-hosted `Hash(String, Nil)` / `Hash(String, T)` paths. The root cause was
duplicating `Hash::Entry` field layout in the backend while V2's entry payloads
and offsets are owned by the type registry and normal lowering. The fix disables
`emit_hash_string_linear_scan_override` and lets HIR/MIR lowering emit the real
method body. Evidence: full self-compile with
`CRYSTAL_V2_PHASE_STATS=1 scripts/run_safe.sh /tmp/cv2_commit_candidate 300
4096 src/crystal_v2.cr -o /tmp/cv2_s2_commit_candidate` exited 0, the generated
LL had no `direct small Hash linear scan` marker and no
`Hash$LString$C$_Nil$R$Hupdate_linear_scan`, and
`regression_tests/p2_selfhost_hir_emit_no_prelude.sh /tmp/cv2_s2_commit_candidate`
printed `p2_selfhost_hir_emit_no_prelude_ok`. Boundary: stage2 still has a
separate `Enumerable(T)#any?$$block` blocker for richer no-prelude/function
smokes. {F/G/R: 0.92/0.55/0.93} [verified]

[LM-484|verified]: Four stage2 shape roots were isolated and guarded in
`regression_tests/p2_selfhost_stage2_shape_guard.sh`. First, cache-only return
repair must not overwrite already concrete call-site types: the bad
`Slice(UInt8)#[] -> Slice(UInt8)` repair caused `Parser#is_constant_name?` to
load a `Char` from a `UInt8` value; the fixed MIR keeps `UInt8` and emits
`zext ... : Char`. Second, bare `return` in nilable functions must emit a nil
union value; `String#byte_index(Int32, Int32)` no longer contains a bare `ret`.
Third, deferred runtime constants must update `@constant_types` after lowering;
`CRYSTAL_SRC_PATH` now reads as `String` instead of `VOID`, avoiding
`Path | String` variant-0 miswrap and the previous `String#bytesize` crash.
Fourth, splat parameters must be rebound as tuple locals inside method bodies;
`Dir.glob$..._block_splat` now allocates a tuple for `patterns` and no longer
self-recurses. Evidence: `crystal build src/crystal_v2.cr -o
/tmp/cv2_splat_tuple_guard --error-trace` exited 0;
`CRYSTAL_V2_STOP_AFTER_MIR=1 scripts/run_safe.sh /tmp/cv2_splat_tuple_guard
300 4096 src/crystal_v2.cr --emit mir --no-link -o
/tmp/cv2_splat_tuple_guard_mir` exited 0; and
`regression_tests/p2_selfhost_stage2_shape_guard.sh /tmp/cv2_splat_tuple_guard`
printed `p2_selfhost_stage2_shape_guard_ok`. Boundary: this proves shape
invariants, not a green generated-stage2 compiler. {F/G/R: 0.94/0.62/0.94}
[verified]

[LM-485|verified]: The current generated stage2 compiler still times out even
after LM-484. Full-prelude `puts 42` compilation moves past the old
`CRYSTAL_SRC_PATH`/`Dir.glob` crashes but times out under `run_safe.sh`.
The smallest no-prelude smoke also times out:
`scripts/run_safe.sh /tmp/cv2_s2_splat_tuple_guard 30 2048
/tmp/cv2_no_prelude_expr_splat_tuple_guard.cr --no-prelude --no-codegen`.
Samples identify the next root area rather than the fixed roots:
`__crystal_v2_string_eq` in one timeout and
`Indexable.range_to_index_and_count -> Range(Int32, Int32)#begin` in another.
Boundary: do not run `s3b+`; next work is a minimal no-prelude oracle for this
string/range primitive hang. {F/G/R: 0.88/0.42/0.90} [verified]

[LM-486|verified]: Three additional generated-stage2 no-prelude blockers were
moved forward. First, nilable query calls on concrete containers must preserve
receiver-owned specializations even when the implementation lives in an
included module; `Array(Nil | Array(ExprId))#[]?$Int32` now materializes through
`Indexable#[]?` instead of falling back to `#[]?$Range`. Second, semantic cache
key hashes must avoid `.hash` on immediate primitive fields while self-hosting;
`MethodLookupKey` and related keys now combine object ids and booleans with
integer arithmetic, removing the generated-stage2 `Object#hash` vdispatch
blocker. Third, `TypeInferenceEngine#primitive_metaclass?` must not rely on
flow narrowing across `type.is_a?(PrimitiveType) && type.name...`; explicit
`PrimitiveType` casting makes HIR emit `PrimitiveType#name -> String` followed
by `String#ends_with?`, not stale `Hash(...HIR::Value)#ends_with?`. Evidence:
`crystal build src/crystal_v2.cr -o /tmp/cv2_primitive_metaclass_narrow
--error-trace` exited 0; `CRYSTAL_V2_STOP_AFTER_HIR=1 ... --emit hir --no-link`
showed the explicit cast and `String#ends_with?$String`;
`CRYSTAL_V2_STOP_AFTER_MIR=1 ... --emit mir --no-link` exited 0 and showed
`PrimitiveType#name` plus `call @... : Bool`; full stage2 build produced
`/tmp/cv2_s2_primitive_metaclass_narrow`; and
`regression_tests/p2_selfhost_stage2_shape_guard.sh
/tmp/cv2_primitive_metaclass_narrow` printed
`p2_selfhost_stage2_shape_guard_ok`. Boundary: generated stage2 still times out
after parse on minimal no-prelude; latest sample is hot in
`__crystal_v2_string_eq`, which is the next root target. {F/G/R:
0.92/0.55/0.93} [verified]

[LM-487|verified]: The full `s1 -> s2b` wrapper gate now produces the stage2
compiler but fails at the generated-compiler smoke. Command:
`BOOTSTRAP_STAGE_OUT=/tmp/cv2_bs_s2 BOOTSTRAP_CHAIN_STAGES=2
BOOTSTRAP_TIMEOUT_SEC=300 BOOTSTRAP_MEM_MB=4096
scripts/build_bootstrap_stages.sh --stages 2 --out /tmp/cv2_bs_s2`. Stage1
build and both stage1 smokes passed; stage2 self-host build passed in
`293.23s` with peak RSS about `3437.70MB`, producing
`/tmp/cv2_bs_s2/cv2_s2`; stage2 plain `puts 42` smoke timed out after `60s`.
A `run_safe.sh` safety-harness defect was found at the same boundary: wedged
child processes can block `lsof` / `wait`, so the wrapper parent may not return
even after writing the timeout marker. Boundary: next compiler work should
debug the generated `s2b` smoke/no-prelude timeout, not the stage2 self-host
build. {F/G/R: 0.93/0.55/0.94} [verified]

[LM-488|verified]: Nested inline-yield fallback must not emit a call back to
the currently lowered splat/block wrapper. The root cause was
`inline_yield_fallback_call` preserving an `inline_key` that already contained
`$..._block_splat`; because the old correction ran only for bare names, a depth
or repeat guard inside `Dir.glob(*patterns, &block)` emitted a self-call that
repacked the splat tuple indefinitely in generated stage2. The fix resolves
bare, `_splat`, and current-function fallback targets through the block overload
table, prefers a typed non-splat block target when available, and does not
eagerly force the corrected callee body during fallback. Evidence:
`crystal build src/crystal_v2.cr -o /tmp/cv2_dirglob_rootfix3 --error-trace`
exited 0; mini `Dir.glob` HIR emit under `scripts/run_safe.sh` no longer
contains `Dir.glob$Path | String_File::MatchOptions_Bool_block_splat`; full
`CRYSTAL_V2_STOP_AFTER_HIR=1 ... src/crystal_v2.cr --emit hir --no-link`
exited 0 after about `189s`; `regression_tests/p2_bootstrap_semantic_emit_oracle.sh
/tmp/cv2_dirglob_rootfix3` and
`regression_tests/p2_selfhost_stage2_shape_guard.sh /tmp/cv2_dirglob_rootfix3`
both passed. Boundary: `s1 -> s2b` now builds `cv2_s2`, but generated-stage2
smokes still fail: plain prelude smoke hits `STUB CALLED: String$Heach$$block`,
and no-prelude smoke times out after `puts$String`. {F/G/R: 0.94/0.58/0.94}
[verified]

[LM-489|verified]: The inline-yield fallback correction must not de-splat
scalar splat-wrapper calls. The regression from LM-488 was that a scalar
`Dir.glob("pattern", &block)` fallback could be over-corrected from the
`Path | String ... _block_splat` wrapper to the `Enumerable` overload, making
the generated stage2 compiler dispatch `String#each$block` inside
`Dir.glob$Enumerable...`. The fix only prefers a typed non-splat block target
when the first call argument is already a tuple/collection; scalar calls keep
the `_block_splat` wrapper so the wrapper performs tuple packing before
forwarding. Evidence: `crystal build src/crystal_v2.cr -o
/tmp/cv2_dirglob_scalar_guard --error-trace` exited 0; a mini scalar
`Dir.glob` HIR emit showed the wrapper calling `Dir.glob$Enumerable...` with a
tuple local and no `String#each$block`; full
`CRYSTAL_V2_STOP_AFTER_HIR=1 ... src/crystal_v2.cr --emit hir --no-link`
exited 0 after about `187s`; `regression_tests/p2_bootstrap_semantic_emit_oracle.sh
/tmp/cv2_dirglob_scalar_guard` and
`regression_tests/p2_selfhost_stage2_shape_guard.sh /tmp/cv2_dirglob_scalar_guard`
both passed. The full `s1 -> s2b` wrapper built stage2 in `295.29s` with peak
RSS about `3315.53MB`; generated-stage2 smokes still fail later, now as
timeouts in prelude loading and after `puts$String`, not as
`String#each$block`. Boundary: do not treat this as a green generated-stage2
compiler; the next root is the generated compiler timeout frontier.
{F/G/R: 0.94/0.58/0.94} [verified]

[LM-490|verified]: Generated-stage2 semantic helper stubs can come from
source-level helper calls whose HIR call targets are emitted but whose bodies
are not materialized by the current demand pipeline. Two adjacent roots were
moved. First, `SymbolCollector#@table_stack` inferred as
`Array(SymbolTable) | Array(String)`, so `current_table` called generic
`Array#last() -> T` and the generated compiler hit `T#lookup_macro$String`.
Adding `@table_stack : Array(SymbolTable)` makes `current_table` return
`SymbolTable` and removes `T#lookup_macro` from HIR/MIR. Second, trivial
`NameResolver` zero-arg helpers (`current_owner_symbol`, `in_method_body?`,
`current_method_is_class_method?`, `top_level_scope?`,
`type_expression_context?`) were present as calls but not materialized as
bodies; inlining their simple stack/depth checks at source call sites removes
that abort-stub cluster. Evidence: `crystal build src/crystal_v2.cr -o
/tmp/cv2_semantic_helper_commit --error-trace` exited 0;
`regression_tests/p2_bootstrap_semantic_emit_oracle.sh
/tmp/cv2_semantic_helper_commit` and
`regression_tests/p2_selfhost_stage2_shape_guard.sh
/tmp/cv2_semantic_helper_commit` both passed; the full `s1 -> s2b` wrapper
built stage2 at `/tmp/cv2_bs_s2_semantic_helpers/cv2_s2`, and generated
no-codegen no-prelude smoke moved past `T#lookup_macro` and
`NameResolver#current_owner_symbol` to
`TypeInferenceEngine#guard_watchdog!`. Refuted: direct replacement of
`guard_watchdog!` with `Frontend::Watchdog.check!` removes the stub but
duplicates watchdog lowering and fails the stage2 build envelope; changing
`guard_watchdog!` visibility to public still leaves calls without a body in HIR.
Boundary: next root is the demand/materialization issue for
`TypeInferenceEngine#guard_watchdog!`, not the already-moved helper cluster.
{F/G/R: 0.91/0.52/0.92} [verified]

[LM-491|verified]: `TypeInferenceEngine#guard_watchdog!` was a stale deferred
leaf-helper target, not a missing def registration. The def was registered
early, but calls emitted during `TypeInferenceEngine` lowering deferred the
zero-arg helper into the work queue; later lazy-RTA/safety-net passes could
leave a concrete call target without a materialized body, so LLVM generated
`STUB CALLED: CrystalV2::Compiler::Semantic::TypeInferenceEngine#guard_watchdog!`.
The safe fix is not a broad stale-Pending requeue: that was tested and rejected
because it reopens the deep generic formatting/iterator fan-out and times out.
Instead, this specific leaf guard bypasses nested deferral and is lowered
immediately. Evidence: `crystal build src/crystal_v2.cr -o /tmp/cv2_guardleaf
--error-trace` exited 0; `CRYSTAL_V2_STOP_AFTER_HIR=1 scripts/run_safe.sh
/tmp/cv2_guardleaf 300 4096 src/crystal_v2.cr -o /tmp/cv2_guardleaf_stop`
exited 0 after about `197s`; `--emit hir --no-link` produced a HIR body
`func @CrystalV2::Compiler::Semantic::TypeInferenceEngine#guard_watchdog!`
that calls `Frontend::Watchdog.check!`; `regression_tests/p2_selfhost_stage2_shape_guard.sh
/tmp/cv2_guardleaf` and `regression_tests/p2_bootstrap_semantic_emit_oracle.sh
/tmp/cv2_guardleaf` both passed. Boundary: a direct full `s1` codegen attempt
still timed out at 300s in later lowering, so the next frontier must be
re-measured from a fresh generated `s2b` rather than assumed green.
{F/G/R: 0.92/0.48/0.91} [verified]

[LM-492|verified]: The generated-stage2 `Class$Dcrystal_type_id` abort was a
type-literal primitive lowering hole duplicated across `lower_call` and
`lower_member_access`. `Hasher#class(value)` was emitted as
`copy %value; call Class.crystal_type_id()` because member-access on a
type-literal receiver fell through to static `Class.*` resolution before
primitive lowering could emit the original compiler's metaclass/type-id
semantics. The fix keeps `crystal_type_id` and `crystal_instance_type_id` on
the primitive path for type-literal receivers and emits an `Int32` type-id
literal in both call and no-parens member-access paths. Evidence:
`crystal build src/crystal_v2.cr -o /tmp/cv2_typeid3 --error-trace` exited 0;
`regression_tests/p2_selfhost_stage2_shape_guard.sh /tmp/cv2_typeid3` passed
and now rejects `Class.crystal_type_id` / `Class#crystal_type_id`;
`regression_tests/p2_bootstrap_semantic_emit_oracle.sh /tmp/cv2_typeid3`
passed; fresh `scripts/run_safe.sh /tmp/cv2_typeid3 420 4096
src/crystal_v2.cr -o /tmp/cv2_typeid3_s2_full` exited 0 after about `248s`;
generated `s2b` no-prelude no-codegen smoke moved past
`Class$Dcrystal_type_id` to `STUB CALLED: Char$Hascii_control$Q`. Boundary:
this is a root fix for type-id primitive dispatch, not a green generated-stage2
compiler; the next frontier is `Char#ascii_control?` materialization.
{F/G/R: 0.93/0.55/0.93} [verified]

[LM-493|verified]: The generated-stage2 `Char$Hascii_control$Q` abort was a
leaf primitive materialization hole, not a demand-queue root cause.
`Char#control?` calls `ascii_control?` after `ascii?`, but generated `s2b`
still emitted an abort stub for the raw `Char` predicate. The fix lowers
implicit self, explicit call, and no-parens member-access forms of
`Char#ascii_control?` inline as `self < 0x20 || self == 0x7f`, matching
`src/stdlib/char.cr` without touching stdlib/runtime. Evidence:
`crystal build src/crystal_v2.cr -o /tmp/cv2_charctl_final --error-trace`
exited 0; `regression_tests/p2_bootstrap_semantic_emit_oracle.sh
/tmp/cv2_charctl_final` and `regression_tests/p2_selfhost_stage2_shape_guard.sh
/tmp/cv2_charctl_final` passed; fresh `scripts/run_safe.sh
/tmp/cv2_charctl_final 420 4096 src/crystal_v2.cr -o
/tmp/cv2_charctl_final_s2_full` exited 0 after about `252s`; generated `s2b`
no-prelude no-codegen smoke moved past `Char$Hascii_control$Q` to
`STUB CALLED: Printer$Dshortest$$Float32_IO`. Boundary: this is a root fix for
one missing primitive predicate, but the generated-stage2 compiler is still
not green.
{F/G/R: 0.92/0.45/0.92} [verified]

[LM-494|verified]: The generated-stage2 `Printer$Dshortest$$Float32_IO` abort
was caused by eager debug string interpolation inside
`Semantic::TypeInferenceEngine`, not by missing float-print helpers in user
code. `infer_identifier` eagerly built debug strings such as
`receiver=#{@receiver_type_context.try(&.to_s)}` before checking
`@debug_enabled`, and `debug_hook` is a compile-time no-op in normal builds.
That forced `Object#to_s(io)` on compiler-internal objects during semantic
inference, which reached `Float32#to_s(io)` and the unlowered
`Printer.shortest(self, io)` corridor. The fix replaces eager `debug` and
`debug_type_trace` methods with runtime-gated macros in
`type_inference_engine.cr`, so interpolated strings are only built when
debugging is actually enabled. Evidence:
`crystal build src/crystal_v2.cr -o /tmp/cv2_printerfix --error-trace` exited
0; `regression_tests/p2_bootstrap_semantic_emit_oracle.sh /tmp/cv2_printerfix`
and `regression_tests/p2_selfhost_stage2_shape_guard.sh /tmp/cv2_printerfix`
passed; fresh `scripts/run_safe.sh /tmp/cv2_printerfix 420 4096
src/crystal_v2.cr -o /tmp/cv2_printerfix_s2_full` exited 0 after about `242s`;
generated `s2b` no-prelude no-codegen smoke moved past
`Printer$Dshortest$$Float32_IO` and now reaches semantic checking with
`error[E3001]: Function 'puts' not found`. Boundary: this fixes the eager debug
formatting root cause in semantic inference, but no-prelude top-level `puts`
resolution is still missing.
{F/G/R: 0.93/0.58/0.93} [verified]

[LM-495|verified]: The generated-stage2 no-prelude top-level `puts` failure was
a semantic/HIR parity gap, not a new runtime problem. After LM-494, generated
`s2b` no longer aborted in `Printer.shortest`, but
`test_no_prelude_interpolation.cr --no-prelude --no-codegen` still stopped in
semantic analysis with `error[E3001]: Function 'puts' not found`. The HIR
lowerer already has receiverless `puts`/`print` corridors; type inference did
not. The fix adds a tiny receiverless builtin semantic path for top-level
`puts`/`print`, returning `Nil` and letting HIR handle the actual lowering.
Evidence: `crystal build src/crystal_v2.cr -o /tmp/cv2_puts --error-trace`
exited 0; `regression_tests/p2_bootstrap_semantic_emit_oracle.sh /tmp/cv2_puts`
and `regression_tests/p2_selfhost_stage2_shape_guard.sh /tmp/cv2_puts` passed;
fresh `scripts/run_safe.sh /tmp/cv2_puts 420 4096 src/crystal_v2.cr -o
/tmp/cv2_puts_s2_full` exited 0 after about `241s`;
`regression_tests/p2_generated_stage2_no_prelude_interp.sh /tmp/cv2_puts`
passed. The next measured blocker is
`STUB CALLED: Tuple$Heach$$block` from
`regression_tests/stage2_no_prelude_puts_runtime_repro.sh /tmp/cv2_puts_s2_full`.
Boundary: top-level `puts` semantic parity is fixed, but runtime no-prelude
`puts` still falls into a tuple/block lowering corridor in generated stage2.
{F/G/R: 0.94/0.60/0.94} [verified]

[LM-496|verified]: The generated-stage2 `Tuple$Heach$$block` abort for
no-prelude `puts 7` was a compile-mode tracking bug in HIR print fallback
selection, not a real tuple root cause. `s1` already compiled the tiny
`puts 7 --no-prelude` repro to a direct `call void @__crystal_v2_print_int32_ln`
shape, but generated `s2b` still disabled `emit_runtime_print_fallback`
because `prelude_io_print_available?` inferred availability from ambient
`IO` method tables instead of the actual `--no-prelude` option. That let the
generated compiler drift back into the ordinary variadic `puts(*objects)` path,
which iterates the implicit tuple and hit `Tuple#each(&block)` in self-host
mode. The fix threads `options.no_prelude` into `HIR::AstToHir` and makes
`prelude_io_print_available?` return false under `--no-prelude`, so supported
primitive/string/bool print calls always take the runtime fallback corridor in
that mode. Evidence: `CRYSTAL_CACHE_DIR=/tmp/crystal_cache_v2_noprel_printfix
crystal build src/crystal_v2.cr -o /tmp/cv2_noprel_printfix --error-trace`
exited 0; `regression_tests/stage2_no_prelude_puts_runtime_repro.sh
/tmp/cv2_noprel_printfix` returned `not reproduced`; and
`regression_tests/p2_generated_stage2_no_prelude_interp.sh
/tmp/cv2_noprel_printfix` remained green. Boundary: this proves the
no-prelude print-mode decision must depend on compile options, but the next
generated-stage2 frontier still needs fresh measurement after this fix.
{F/G/R: 0.95/0.66/0.95} [verified]

[LM-497|verified]: The generated-stage2 no-prelude `puts 7` frontier moved
past the late backend Hash iterator / block-param-shape corridor. Root chain:
`Crystal::MIR::LLVMIRGenerator#emit_missing_crystal_function_stubs` built a
temporary missing-function `Hash` and then re-walked it via `Hash#each` or
`each_key`; both lower through `Hash#each_entry_with_index`, which exposed the
open nested raw callback ABI and crashed in a null block callback. Returning a
flat `Array({name, return_type, arg_count, arg_types})` snapshot from
`collect_missing_crystal_functions` removes that artificial Hash iterator from
the late emission pass. The first Array snapshot attempt used a nested tuple
payload and exposed a separate generated-stage2 aggregate-layout bug, so the
snapshot is intentionally flat; nested tuple/aggregate block params remain a
real follow-up oracle, not a general flattening policy. A second root in the
same path was block-param inference for compiler collection aliases:
`Crystal::MIR::Array(T)` was not normalized before element inference, so
`Array(T)#each` block procs could be emitted as `Void ->`. Reusing
`normalize_compiler_collection_owner_name` in element/hash block-param
inference changes the self-host HIR for the late-emission Array loop from a
`Void` block param to a real tuple param. Evidence:
`crystal build src/crystal_v2.cr -o /tmp/cv2_flat_missing --error-trace`
passed; `scripts/run_safe.sh /tmp/cv2_flat_missing 420 4096 src/crystal_v2.cr
-o /tmp/cv2_flat_missing_s2` exited 0; `scripts/run_safe.sh
/tmp/cv2_flat_missing_s2 120 1024 /tmp/repro_puts7.cr --no-prelude -o
/tmp/repro_puts7_bin` moved to `STUB CALLED:
IO$CCFileDescriptor$Hsystem_pos` instead of the old null callback / tuple
segfault frontiers. {F/G/R: 0.94/0.67/0.93} [verified]

[LM-498|verified]: The generated-stage2 no-prelude `puts 7` frontier moved
past `IO::FileDescriptor#system_pos`, `Crystal::System::Kqueue.set`, and the
`File#file_descriptor_close` recursion crash. The first two were exact-demand
and overload-resolution gaps: same-owner system/class helper calls needed to
mark concrete targets as RTA demand, and raw `Pointer` arguments needed to
match typed `Pointer(T)` parameters so the real Kqueue overload was selected
instead of an abort stub. The bus-error frontier was a separate inherited
wrapper root: requested `File#file_descriptor_close` was materialized by
lowering the ancestor `IO::FileDescriptor` body under `@current_class = File`,
so implicit calls inside the ancestor body resolved back to the child wrapper
and self-recursed. The fix preserves requested wrapper owner only for
value/primitive/generic owner-specialization cases; normal reference-class
inherited wrappers lower the resolved ancestor body while still materializing
the requested dispatch symbol. Evidence: `crystal build src/crystal_v2.cr -o
/tmp/cv2_inherited_owner --error-trace` passed; HIR emitted with
`DEBUG_CALL_LOOKUP=file_descriptor_close DEBUG_BLOCK_CALL_ABI=1` shows
`File#file_descriptor_close` calling
`IO::FileDescriptor#file_descriptor_close$block`, not itself; and
`regression_tests/p2_generated_stage2_no_prelude_puts_guard.sh
/tmp/cv2_inherited_owner` reports
`p2_generated_stage2_no_prelude_puts_guard_ok frontier=string_null_byte`.
`regression_tests/p2_selfhost_stage2_shape_guard.sh /tmp/cv2_inherited_owner`
also passes after updating its `Dir.glob(...block_splat)` oracle from the stale
tuple-allocation shape to the actual invariant: the forwarding block proc is
`String`-shaped and the old `_block_splat` / `String#each$block` regressions
are absent.
Boundary: `IO#pos` is now an accepted runtime dispatch-helper shape for
`IO::FileDescriptor#tell`; the next root is generated-stage2
`String#byte_index(0)` / null-byte false positive, not another
`check_no_null_byte` callsite workaround. {F/G/R: 0.94/0.68/0.93} [verified]

[LM-499|verified]: The generated-stage2 `String contains null byte` frontier was
a div/rem signedness bug in `llvm_backend`, not a `String#byte_index(0)` search
bug. Root chain: `CLI` builds `pipeline_hash_str = pipeline_hash.to_s(16)` from
a `UInt64` FNV hash whose seed is `0xcbf29ce484222325` (high bit set);
`Int#to_s(base)` calls `num.remainder(base).abs` where `num : UInt64` and
`base : Int32`; MIR emits `BinaryOp::Mod` with `receiver_type = UInt64` as
result but keeps the `Int32` right operand untouched; the backend then
promoted both operands via `sext Int32 -> i64` (fine) but selected `srem`
because `is_signed = left_is_signed || right_is_signed` was true whenever any
operand was signed. `srem i64 0xcbf29ce484222325, 16` returns a negative
remainder (srem follows dividend sign in 2's complement), which `.abs` on a
`UInt64` treats as a huge index into the `digits` buffer, corrupting bytes and
inserting `0x00`. `File.exists?("#{pipeline_hash_str}.ll")` then raises
`String contains null byte` through `check_no_null_byte`. The fix mirrors
original Crystal `primitives.cr:149`: `t1.signed? ? srem : urem`. In
`llvm_backend.cr`, div/rem now derives signedness from dividend only
(`left_is_signed`) instead of OR-ing both operands. Evidence:
`regression_tests/p2_u64_to_s_base16_no_null.sh bin/crystal_v2` baseline
without fix printed corrupted bytes with embedded nul (`byte_index(0)==0`);
with fix prints `cbf29ce484222325`, `16`, `true`. Full regression suite delta
vs baseline: zero changed. The generated-stage2 no-prelude `puts 7` frontier
moves past `string_null_byte` to a new corridor: `--no-codegen` now hits
`STUB CALLED: Array(Nil | Array(Crystal::Compiler::Frontend::ExprId))#check_index_out_of_bounds$Int32_block`;
full codegen times out in `Crystal::RWLock#write_lock` reached from
`Process.fork`. Boundary: this is a codegen root fix, not a demand-pipeline
fix; the next frontier is the new `check_index_out_of_bounds` stub on a
deep nilable-Array container. {F/G/R: 0.94/0.70/0.94} [verified]

[LM-500|verified]: The generated-stage2 `check_index_out_of_bounds` ABORT-stub
frontier was a lazy-RTA allowlist gap, not a virtual-dispatch or receiver-set
bug. Root chain: `Indexable#fetch(index : Int, &)` calls the private helper
`check_index_out_of_bounds(index) { return yield }`. The private helper is
visible only through the fetch body (not through any virtual dispatch site), so
under lazy RTA its method-part is tracked in `@rta_called_method_parts` but its
virtual-receiver set never includes concrete container types. In
`process_pending_lower_functions`, `should_keep` walks
`@rta_called_methods` (exact) → `rta_live_owner?` (owner liveness) →
`rta_method_part_matches_owner?` (virtual-dispatch receiver match). All three
miss for private Indexable helpers on live container types, so the function is
deferred and later emitted as an ABORT stub by `llvm_backend.cr`. The existing
mechanism for this class of helper is
`internal_container_helper_exact_demand?` /
`internal_container_helper_name_exact_demand?` in `ast_to_hir.cr`, which
allowlists private helpers so `record_pending_callee_for_rta` adds them to
`@rta_called_methods` exactly. The allowlist already contained `unsafe_fetch`,
`fetch`, `increase_capacity`, etc., but `check_index_out_of_bounds` was missing
for Array, Slice, and Deque. The fix adds `check_index_out_of_bounds` to the
Array, Slice, and Deque arms of both allowlists. Evidence: targeted
`[CIOOB_TRACE]` instrumentation at the defer decision showed
`reason=defer:method_part owner_live=true mpart_matches=false` for 6 affected
types before the fix; after the fix `generated_s2.ll` contains 78 real
`check_index_out_of_bounds` function definitions and 0 `abort_stub` lines; the
`--no-codegen` probe now exits 0 with no `STUB CALLED`, advancing the
`p2_generated_stage2_no_prelude_puts_guard.sh` recorded frontier from
`array_check_index_oob_stub` to `nocodegen_clean_full_codegen_hang`; full
regression suite delta vs baseline on the same branch is zero (original 147:
133-134/13-14 with `bootstrap_semantic_corpus` flaking equally on both; combined
31: 23/8 identical). Boundary: the next frontier for the full-codegen
`puts 7 --no-prelude` corridor is the 60s hang after `lower_main: exprs=1`,
likely still the `Crystal::RWLock#write_lock` / `Process.fork` corridor noted
in LM-499. {F/G/R: 0.92/0.72/0.94} [verified]

[LM-501|verified]: The generated-stage2 `Crystal::RWLock#write_lock` prologue
emitted `mov w9, #0x4 ; str w9, [x10]`, writing LLVM
`AtomicOrdering::Acquire = 4` into the `@writer` atomic slot instead of the
intended `LOCKED = 1`. Root cause: the inline lowering of `Atomic(T)#set` and
`Atomic(T)#swap` in `src/compiler/mir/hir_to_mir.cr` lines 2978-2992 read
`args[2]` as the stored value whenever the HIR call carried three arguments.
Crystal's `Atomic#swap(value, ordering)` puts the value at arg index 1 and the
ordering enum at index 2, so the inliner was storing the ordering enum (Acquire
= 4) into the slot and reading the prior contents back as an 8-byte `ptr`
(aliasing the full pointer-sized word of the heap-allocated `Atomic(Int32)`
struct). Fix: both branches now read `new_val = args.size > 1 ? args[1] :
const_int(0, INT32)`; the old `args[2]` fallback is removed. Evidence: before
the fix, the generated-stage2 binary's `Crystal$CCRWLock$Hwrite_lock`
disassembly contained `mov w9, #0x4 ; str w9, [x10]`; after the fix the
prologue instead does `adrp x8, Crystal$CCRWLock__classvar__LOCKED ; ldr w9,
[x8] ; str w9, [x10]` (loads the `LOCKED` classvar and stores that i32 into
the atomic slot). Grok (xAI grok-build via `~/.grok/bin/grok_acp_delegate.py`)
located the suspicious inline lowering and the pre-existing proper-atomic path
(`emit_atomic_rmw` in `src/compiler/mir/llvm_backend.cr` near line 23719) with
28 read-only tool calls on a timeboxed task file; verification happened here.
Regression guard is extended in
`regression_tests/p2_generated_stage2_no_prelude_puts_guard.sh` to assert the
positive shape (no raw `#0x4` store, presence of the LOCKED classvar symbol
reference) in the write_lock disassembly. Boundary: the write_lock body is
still a non-atomic load+store inline — `emit_atomic_rmw` / `atomicrmw xchg`
infrastructure exists in the backend but is not reached because hir_to_mir
short-circuits `Atomic#swap` before any MIR `AtomicRMW` is produced; for
single-threaded stage2 the non-atomic behaviour is not the active blocker.
The full-codegen `puts 7 --no-prelude` corridor now fails one level deeper:
`Crystal::System::Process.@@rwlock` classvar stays `null` because
`Crystal::RWLock.new` is never lowered (RTA never records the constructor for
a struct-classvar init), and `write_lock(NULL)` faults on entry with
`EXC_BAD_ACCESS address=0x0` at offset +24 (first load through self). That is
the next frontier. Regression suite delta vs baseline on the same branch is
zero (original and combined counts unchanged; the first combined 21/10 run
was a flake reproduced back to 23/8 on isolated rerun). {F/G/R: 0.92/0.55/0.92}
[verified]

## Active Strategy

- Main fast loop: `--no-prelude` oracles and focused STOP_AFTER_HIR budget
  checks.
- Integration gate: `s1 -> s2b` only after fast oracles are green.
- Rare full gate: `s1 -> s5b` plus normalized HIR/MIR/LL equality.
- Do not run `s3b+` while generated `s2b` cannot pass plain/no-prelude smokes.
