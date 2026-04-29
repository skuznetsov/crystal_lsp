# LANDMARKS

Updated: 2026-04-29
Context: compiler/bootstrap/stage2-stability

This file is the active working set only. Historical landmarks before this
checkpoint remain recoverable from git history, especially:

- `15e448b9:LANDMARKS.md`
- archived full-file SHA256:
  `d43826fdcc2277b6075026244764a84d0069d1a30b675642b603f3511b14a1e5`

## Active Bootstrap Gate

[LM-519|verified]: Generic receiver stripping must preserve namespace path
segments. The old overload/method-index helpers normalized
`Indexable(T)::ItemIterator(Array(String), String)#each` to `Indexable#each`,
so `ItemIterator#each` could select the `Indexable(T)#each` body and emit a
bogus nested constructor demand
`Indexable(T)::ItemIterator(Indexable(T)::ItemIterator(Array(String), String), String).new`.
The fix strips generic arguments per namespace segment for method-index keys
and stripped overload lookup, producing `Indexable::ItemIterator#each`, and
adds generic-template resolution for classes declared under generic
namespaces. Evidence: `regression_tests/p2_nested_generic_new_inference.sh
/tmp/cv2_method_index_path3` requires the specialized iterator constructors
and rejects the bogus nested `ItemIterator(...).new`; build and p1/p2 focused
guards passed; full-source `STOP_AFTER_HIR` exits 0 after about 234s. Boundary:
full-source `lower_missing` still grows `17423 -> 50628 (+33205)`, so the next
bootstrap root is broad concrete-call demand volume, not this namespace
lookup bug. {F/G/R: 0.92/0.66/0.93} [verified]

[LM-481|verified]: Backend-owned runtime intrinsics must not be demand-driven
as source-level HIR functions. HIR currently emits `__crystal_v2_string_eq`,
`__crystal_v2_hash_get_entry_ptr`, `__crystal_v2_hash_entry_deleted`, and
`__crystal_v2_select_ptr` as plain `Call` instructions, but MIR lowering turns
unresolved calls into `extern_call`, and the LLVM backend either defines those
runtime helpers or intercepts them specially (`select_ptr`). A focused patch
skips that exact allowlist in `lower_missing_call_targets`,
`remember_callsite_arg_types`, and `lower_function_if_needed_impl`. Evidence:
`regression_tests/p2_backend_intrinsic_boundary_no_prelude.sh
/tmp/cv2_intrinsic_boundary_check` keeps `string_eq` / `select_ptr` visible in
HIR while rejecting their appearance in missing-source logs; fresh generated
`s1` full-source `STOP_AFTER_HIR` exits 0 after about 220s, and
`rg '__crystal_v2_(string_eq|hash_get_entry_ptr|hash_entry_deleted|select_ptr)'
/tmp/cv2_missing_intrinsic/run.log` returns no matches. Boundary: this removes
one wrong demand source but does not by itself shrink the remaining
`lower_missing` total (`+25690` in the measured run). {F/G/R:
0.91/0.62/0.92} [verified]

[LM-482|verified]: Class vdispatch wrappers can share a case body when many
runtime type IDs resolve to the exact same inherited implementation. The MIR
vdispatch generator now keeps all switch labels but interns only class-dispatch
case bodies by callee `FunctionId`; union-dispatch cases and
`dispatch_class`-specialized cases remain unique because they carry
case-specific unwrap/specialization semantics. Evidence: local hostile review
of `generate_vdispatch_body` confirmed legal multi-label switch targets and
PHI predecessor shape; the focused self-host artifact reduced broad
`Object#hash` wrapper size from roughly 50k lines to roughly 10k lines, and the
current canonical `s1 -> s2` partial `cv2_s2.ll` is about 3.7MB instead of the
previous 170MB+ over-materialized artifacts. Boundary: this is an IR-size/root
compaction, not the final bootstrap fix; full `s1 -> s2` still times out after
allocator flush. {F/G/R: 0.86/0.64/0.84} [verified]

[LM-483|verified]: Generated stage2 can still miss inline-default ivar
initialization for closure by-reference state in `AstToHir`; keep those fields
explicitly initialized in the constructor until the broader inline-default
root is fixed. Evidence: the vdispatch-compacted generated `cv2_s2` no-prelude
smoke crashed in `Set(String)#includes?` from
`AstToHir#lower_identifier` because `@closure_ref_prefer_cell` was nil despite
the inline ivar default. Explicit constructor initialization restored the
focused no-prelude guards. Boundary: this is a contained workaround for a known
generated-stage2 initialization bug, not a replacement for the later general
inline-default fix. {F/G/R: 0.86/0.45/0.88} [verified]

[LM-484|verified]: Current full `s1 -> s2` frontier has moved past HIR
STOP_AFTER_HIR but still fails the canonical 300s bootstrap gate in the
post-HIR tail. Command:
`BOOTSTRAP_STAGE_OUT=/tmp/cv2_bs_s2_intrinsic BOOTSTRAP_CHAIN_STAGES=2
BOOTSTRAP_TIMEOUT_SEC=300 BOOTSTRAP_MEM_MB=4096
scripts/build_bootstrap_stages.sh --stages 2 --out /tmp/cv2_bs_s2_intrinsic`.
Stage1 builds and both smokes pass; stage2 is killed at about 302s after
`[ALLOC_FLUSH] Generated 98 deferred allocators`, with partial
`/tmp/cv2_bs_s2_intrinsic/cv2_s2.ll` around 3.7MB. Boundary: next work should
sample the allocator/MIR/LLVM tail, not re-open the backend intrinsic boundary
unless new evidence appears. {F/G/R: 0.93/0.54/0.94} [verified]

[LM-485|verified]: The canonical `s1 -> s2` timeout is visible after allocator
flush, but the measured primary supplier is the initial missing-target sweep,
not allocator generation or repair fixed points. A phase-split
`CRYSTAL_V2_STOP_AFTER_HIR=1 CRYSTAL_V2_PHASE_STATS=1` run using
`/tmp/cv2_phase_split_check` reports:
`process_pending: 3159 -> 17572 (+14413)`, `emit_tracked_sigs: 17572 -> 17836
(+264)`, `lower_missing.initial: 17836 -> 43126 (+25290) in 144271.9ms`,
`repair_stale_calls: +26`, `repair_receiver_calls: +217`,
`deferred_allocators: +5`, and `final_missing.fixed_point: +110`. The same
run exits `STOP_AFTER_HIR` in about 220s. A separate
`CRYSTAL_V2_STOP_AFTER_MIR=1` run still times out at 300s during
`Pass 2: Lowering 35221 function bodies... Body 20001/35221`, so MIR is
processing the large reachable set created upstream. Refuted branches:
pre-sizing MIR `@cross_block_values` did not move the full bootstrap frontier,
and a delta-only `lower_missing_call_targets` scan changed fixed-point timing
and grew the HIR set to 47120 functions. Next work should reduce concrete-call
demand admitted by `lower_missing.initial`, not optimize allocator flush first.
{F/G/R: 0.93/0.58/0.93} [verified]

[LM-518|verified]: Env-gated macro-body diagnostics were a real but partial
source-demand leak because `MacroExpander` imported `json` solely for
diagnostic output and used `Hash#to_json` inside runtime-disabled branches. HIR
still lowers whole method bodies, so those branches pulled generic
`Array/Hash/Set#to_json` and `JSON::Builder` into the compiler bootstrap graph.
The root fix replaced the two diagnostic `.to_json` calls with a local
scalar-only `MacroDiagJson` writer and removed `require "json"` from
`src/compiler/semantic/macro_expander.cr`. Evidence:
`crystal build src/crystal_v2.cr -o /tmp/cv2_macro_json_free --error-trace`;
`regression_tests/p2_pending_budget_no_prelude.sh /tmp/cv2_macro_json_free`
printed `process_delta=2 emit_delta=4 lower_missing_delta=0 total=40
max_queue=29`; the p2 semantic emit, backend-intrinsic, and each-index
no-prelude guards passed; full-source `STOP_AFTER_HIR` exited in about 201s
with `42859` functions and no `JSON::Builder`/generic `to_json` top supplier in
the fresh missing summary. Boundary: the remaining `lower_missing.initial`
volume is still about `+25104`, now dominated by virtual/abstract calls
(`IO#<<`, `Proc#call`) and hash/object-id helper corridors, so this is not the
final bootstrap fix. {F/G/R: 0.94/0.58/0.94} [verified]

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

[LM-508|verified]: The opt-in AST demand reachability filter reduces the early
all-defs supply phase but does not yet fix the canonical bootstrap graph size.
Patch state: default `compute_ast_reachable_functions` remains conservative
unless `CRYSTAL_V2_AST_FILTER_DEMAND=1`; the opt-in path scans packed
`main_exprs`, walks reachable method names, gates candidate owners by
constructed/always-reachable types, and feeds the existing AST filter. Evidence:
`regression_tests/p2_ast_filter_demand_no_prelude.sh /tmp/cv2_ast_demand2`
prints `p2_ast_filter_demand_no_prelude_ok process_delta=2
lower_missing_delta=45 total=92`; full `STOP_AFTER_HIR` with
`CRYSTAL_V2_AST_FILTER=1 CRYSTAL_V2_AST_FILTER_DEMAND=1` exits 0 and shifts
phase stats from baseline `process_pending +14371, lower_missing +25702,
43471 total` to `process_pending +4148, lower_missing +35210, 43091 total`.
`DEBUG_MISSING_SUMMARY=1` identifies the compensating concrete-call demand as
`IO#<<`, `__crystal_v2_string_eq`, `Array#root_buffer`, Hash internals,
`JSON::Builder`, and `Hash::Entry#inspect/to_s`. Boundary: do not enable this
by default or filter `lower_missing` blindly; the next root target is why
serialization/formatting/hash bodies enter HIR before the concrete missing-call
sweep. {F/G/R: 0.91/0.58/0.93} [verified]

[LM-509|verified]: LLVM backend reachability pruning is now exposed behind
`CRYSTAL_V2_LLVM_REACHABILITY=1` but remains default-off. Evidence:
`regression_tests/p2_llvm_reachability_no_prelude.sh /tmp/cv2_llvm_reach`
prints `p2_llvm_reachability_no_prelude_ok ... emitting 5 functions`;
full compiler progress run with the env enabled reaches backend RTA and emits
`27833 functions (37792 total, 9959 pruned)` with a `146MB` `.ll` artifact,
versus the previous `37711` emit-all / `189MB` canonical timeout shape. Boundary:
this does not complete the 300s `s1 -> s2b` gate; the run still times out after
function emission while emitting LLVM tail declarations/finalization, so the
next root target is remaining huge-IR tail cost and missing backend reachability
edges, not flipping this env on by default. {F/G/R: 0.92/0.60/0.93} [verified]

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

[LM-502|verified]: The `Crystal::System::Process.@@rwlock` classvar staying
`null` after LM-501 (so `Process.fork`'s `lock_write { LibC.fork }` faulted
on entry to `write_lock(NULL)` at offset +24) was a deferred-init recording
gap, not an RTA / lowering issue. `Crystal::System::Process` reopens with
`@@rwlock = Crystal::RWLock.new` under a `{% else %}` Darwin branch, so the
`AssignNode` target reaches HIR through a macro-branch expansion. The four
class-body / macro-expansion iteration loops in `ast_to_hir.cr` (sites
~20448, ~20519, ~20590, ~20656) all matched `when AssignNode` but only
forwarded to `record_constant_definition` when `target.is_a?(ConstantNode)`;
`ClassVarNode` targets were silently dropped, so `@deferred_classvar_inits`
never received the rwlock entry and no `__classvar_init__Crystal$$CCSystem$$CCProcess__rwlock`
function was emitted. Fix: extracted helper `register_class_assign_from_expansion`
that preserves the existing `ConstantNode` recording and additionally pushes
`{expr_id, @arena, class_name}` onto `@deferred_classvar_inits` for
`ClassVarNode` targets; the four existing AssignNode call sites now route
through this helper. The deepest macro-literal inner loop (no prior
ConstantNode/AssignNode arm) was deliberately left untouched: an exploratory
addition there flipped `String::Formatter::HAS_RYU_PRINTF` macro branches
during constant rediscovery and stubbed
`String::Formatter(Tuple(Float64))#current_char`, so the fix is intentionally
narrow. Evidence: `lower_main: lazy classvar recording` count rises from 20
to 21 on full-prelude `puts 7`; the fork test IR contains
`define void @__classvar_init__Crystal$$CCSystem$$CCProcess__rwlock()` whose
body executes `%r3 = call ptr @Crystal$CCRWLock$Dnew()` and
`store ptr %r3, ptr @Crystal$CCSystem$CCProcess__classvar__rwlock`, with
matching call sites at every `@@rwlock` read; `--no-codegen` probe still
exits 0; `p2_generated_stage2_no_prelude_puts_guard.sh` reports
`frontier=nocodegen_clean_full_codegen_hang` (LM-500 boundary preserved);
sprintf_float_fixed_prefix_repro still fails with
`STUB CALLED: String$CCFormatter$LTuple$LFloat64$R$R$Hcurrent_char` on both
fix and freshly-rebuilt baseline (deterministic isolated repro 3/3 each),
confirming it is a pre-existing failure that was masked as `PASS` by a
parallel-run flake in the baseline regression log. Boundary: with the
classvar populated, `Process.fork`'s parent path no longer NULL-derefs, but
the post-fork child now hangs in `Crystal::System::Signal.after_fork`'s
`@@pipe.each` block (offset +68 in the disassembly) — that is the next
frontier. {F/G/R: 0.85/0.55/0.85} [verified]

[LM-503|investigation][parked]: Re-running the LM-502 fork verification
script (`/tmp/lm502/fork_test.cr`) now exposes two distinct V2 bugs that
sit *behind* `Process.fork` but are off the bootstrap critical path. They
are recorded so future work doesn't rediscover them.

(A) **Method overload conflation** — `Crystal::System::Process.fork` has
two overloads: `def self.fork(*, will_exec = false)` (keyword) and
`def self.fork(&)` (block). RTA lowered only ONE function symbol
`Crystal$CCSystem$CCProcess$Dfork$block(ptr %will_exec)`: the body
belongs to the keyword overload, but the signature carries the block
overload's `$block` mangling and accepts the block proc as a single ptr
parameter. At the call site the block proc address is passed as
`will_exec`, the if-test becomes `icmp ne ptr null` (always true), and
control flows down the will_exec=true branch. Cannot reduce to an
isolated repro — 9+ minimal tests with surface-similar shapes
(`fork_overload.cr`, `fork_ov2.cr`, `fork_ov3.cr`,
`fork_close_overload.cr`, `inner_lib.cr` … `inner_lib5.cr`) all emit
both `$arity1` and `$block` overloads correctly. The bug needs more
context — likely macro guards on the keyword overload's body (the
`{% if SOCK_CLOEXEC %}{% else %}` block at process.cr:146-173) plus the
specific suffix-flag combinatorics in `strip_mangled_suffix_flags`
(`ast_to_hir.cr` ~84768) interact with `resolve_method_call`
(~30393+) in a way that the synthetic tests don't trigger.

(B) **`Crystal::EventLoop#after_fork_before_exec` never lowered** —
abstract base at `event_loop.cr:1` (no method); subclasses define it
(`libevent.cr:15`, `kqueue.cr:31-43`, `polling.cr:112-114`,
`epoll.cr` similarly). `fork_run.ll` contains an ABORT-stub for
`Crystal$CCEventLoop$Hafter_fork_before_exec` (lines 222701-222703) and
no subclass overrides — but the *sibling* method `after_fork` is fully
emitted (definition at line 138629, vdispatch at 147572). The
counter-intuitive part: `after_fork` lives inside
`{% unless flag?(:preview_mt) %} … {% end %}` macro guards in every
subclass, while `after_fork_before_exec` does NOT. So this is the
opposite of LM-502's macro-guard skipping pattern. Likely an RTA
discovery gap specific to the sites that *call*
`after_fork_before_exec` (only `Crystal::System::Process.fork`'s
keyword overload at process.cr:196), which never gets exercised
because of bug (A).

Strategic decision: park. `Process.fork` is legacy/deprecated and not
on the bootstrap critical path; TODO.md frontiers are
`guard_watchdog!`, prelude load timeout, `Enumerable(T)#any?$block`,
`lower_missing` growth — all independent. {F/G/R: 0.55/0.40/0.65}
[parked]

[LM-504|verified]: The generated-stage2 `puts 7 --no-prelude` full-codegen
hang (guard script recorded frontier `nocodegen_clean_full_codegen_hang`)
had a different root cause than the LM-501/LM-502 RWLock corridor: HIR
`lower_unary` always lowered `node.operand` *first*, and then matched
the operator text. For `->Module.method` (parsed as
`UnaryNode("->", Call(...))`), that evaluated the target method
eagerly at the literal site. In stage2's prelude, the line

    class_property after_fork_child_callbacks = [
      ->Crystal::System::Signal.after_fork,
      ->Crystal::System::SignalChildHandler.after_fork,
      -> { Random::DEFAULT.new_seed },
    ]

was compiled as three direct method calls at `__crystal_main` time,
before signal pipes / channels were initialised. `Signal.after_fork`
iterated a nil `@@pipe` and spun. The fix adds a prefix check on
`op_str == "->"` in `ast_to_hir.cr:52877` that dispatches to a new
`lower_method_pointer` helper. The helper synthesises a
`__crystal_method_ptr_N` thunk function, lowers the operand inside the
thunk's own `LoweringContext` (saving/restoring outer inline-yield and
loop stacks), terminates the thunk with `Return(call_value)`, and emits
`emit_make_proc_value` with a null environment for the outer context.
The proc type is `Proc(ReturnType)` with no parameters (sufficient for
the 0-arity `after_fork_child_callbacks` shape).

Evidence:

- Probe `/tmp/lm504/probe3.cr` (`->Foo.bar.call`) now prints `42` and
  HIR contains `func_pointer @__crystal_method_ptr_0 + make_proc`
  instead of `call Foo.bar()` at the literal site.
- Stage2 LLVM IR contains thunks numbered 1889, 1890, 1891 matching
  the three `->...` call sites in `Process.after_fork_child_callbacks`.
- Regression suite: 22 → 23 passing out of 31 combined (pre-fix
  baseline `/tmp/cv2_lm502_built` vs fixed `bin/crystal_v2`); no new
  failures. Remaining 8 failures are pre-existing RTA STUB gaps
  (`Permissions$Hvalue`, `UInt8$Hremainder`, etc.) unrelated to
  proc-pointer paths.
- Generated-stage2 `puts 7 --no-prelude` no longer hangs — it now
  exits in ~0s with `STUB CALLED: Crystal$CCEventLoop$Hafter_fork`
  followed by `llc failed` (ABORT stub emitted for the abstract
  `EventLoop#after_fork` because RTA never discovered the virtual
  dispatch reached via `Proc.call` in the child iteration).

The new frontier — RTA discovery gap for `Crystal::EventLoop#after_fork`
called through the `Process.after_fork_child_callbacks` proc chain —
is recorded for follow-up as a sibling to LM-503(B) (which is the
same pattern for `after_fork_before_exec`). The existing guard script
`p2_generated_stage2_no_prelude_puts_guard.sh` does not yet recognise
this shape; it still falls through to the historical
`nocodegen_clean_full_codegen_hang` label because no earlier shape
check matches `STUB CALLED: Crystal$CCEventLoop$Hafter_fork`.

Regression: `regression_tests/proc_pointer_module_method.cr`
(EXPECT: ok). {F/G/R: 0.9/0.6/0.9} [verified]

[LM-505|verified]: Dead `exception = nil` branches were still creating
concrete `Nil#inspect_with_backtrace$IO` demand after the packed-splat
alignment fix. Minimal no-prelude repro:

    def buffered(message : String, *args, exception = nil)
      if exception
        exception.inspect_with_backtrace(IO.new)
      end
    end

called through a wrapper as `buffered(message, *args, exception: exception)`
with the wrapper defaulting `exception` to nil. HIR before the fix already had
`branch false`, but both branches had been lowered first, so the dead then-body
still contained `%5.Nil#inspect_with_backtrace$IO`. Root cause: `lower_if`
asked `static_nil_condition_value` before lowering the condition, but that
static evaluator understood `nil?`/`null?` checks and literal forms, not a bare
IdentifierNode whose current HIR local type was exactly `Nil`. The fix adds only
that narrow source-semantics case (`local : Nil => if local` is statically
false), avoiding broader "non-nil type => true" pruning because current runtime
null-check safeguards for reference/pointer-like values are separate.

Evidence:

- `regression_tests/dead_nil_branch_after_splat_repro.sh /tmp/cv2_nil_branch_fix`
  -> `dead_nil_branch_after_splat_ok`
- `regression_tests/named_arg_after_splat_type_alignment.sh /tmp/cv2_nil_branch_fix`
  -> `named_arg_after_splat_type_alignment_ok`
- `CRYSTAL_V2_STOP_AFTER_HIR=1 CRYSTAL_V2_PHASE_STATS=1
  DEBUG_PENDING_SOURCES=1 ... scripts/run_safe.sh /tmp/cv2_nil_branch_fix 300
  4096 src/crystal_v2.cr -o /tmp/cv2_nil_branch_stop_hir` -> `[EXIT: 0]`;
  `lower_missing` remains large (`17775 -> 46442`, `+28667`), proving this
  closes a real dead-demand bug but does not solve the broader formatting
  helper explosion. {F/G/R: 0.9/0.55/0.9} [verified]

[LM-506|verified]: RTA method-part replay was over-permissive for root-typed
virtual calls. A call such as `exception : Object;
exception.inspect_with_backtrace(io)` records a broad receiver, then
`rta_method_part_matches_owner?` could keep any live owner whose hierarchy
matched the broad receiver, even if that owner did not declare or inherit the
called instance method. This produced thousands of queued/lowered names like
`Array(UInt64)#inspect_with_backtrace$IO::Memory` from
`Crystal#buffered_message`; `lower_function_if_needed_impl` later reported a
lookup miss for that exact name. A broad method-family suppression was refuted:
it removed the concrete `MyError#inspect_with_backtrace` override for an
`Object`-typed receiver. The accepted fix instead adds a method-existence gate
inside RTA method-part matching: the candidate owner must declare or inherit the
short method name directly, via ancestors, or via included modules before the
method part can keep/replay it.

Evidence:

- `regression_tests/rta_root_virtual_method_replay_guard.sh
  /tmp/cv2_rta_declared_method` -> `rta_root_virtual_method_replay_ok`
  (preserves `MyError#inspect_with_backtrace$IO`, rejects unrelated owner).
- `regression_tests/p2_pending_budget_no_prelude.sh
  /tmp/cv2_rta_declared_method` ->
  `p2_pending_budget_no_prelude_ok process_delta=3 emit_delta=4
  lower_missing_delta=44 total=92 max_queue=57`.
- `CRYSTAL_V2_STOP_AFTER_HIR=1 CRYSTAL_V2_PHASE_STATS=1
  DEBUG_PENDING_SOURCES=1 ... scripts/run_safe.sh /tmp/cv2_rta_declared_method
  300 4096 src/crystal_v2.cr -o /tmp/cv2_rta_decl_stop_hir` -> `[EXIT: 0]`;
  `lower_missing` improved from `+28667` to `+25702`, and `Array` function
  prefix count dropped from `11817` to `8930`. `Array#inspect_with_backtrace`
  remains visible in enqueue-source accounting, so the next root remains the
  broader `lower_missing`/container-helper materialization corridor.
  {F/G/R: 0.9/0.65/0.9} [verified]

[LM-507|verified]: The canonical bootstrap-stage wrapper had an infrastructure
bug independent of compiler codegen. On macOS Bash 3.2 with `set -u`, invoking
`scripts/build_bootstrap_stages.sh --stages 2 --out ...` with no extra
bootstrap-chain arguments failed immediately at `"${CHAIN_ARGS[@]}"` with
`CHAIN_ARGS[@]: unbound variable`. The fix branches on
`${#CHAIN_ARGS[@]}` and calls `bootstrap_chain.sh` without expanding the empty
array when no passthrough arguments exist.

Evidence:

- `bash -n scripts/build_bootstrap_stages.sh` -> exit 0.
- `scripts/build_bootstrap_stages.sh --help` -> prints usage.
- Re-running `BOOTSTRAP_TIMEOUT_SEC=300 BOOTSTRAP_MEM_MB=4096
  scripts/build_bootstrap_stages.sh --stages 2 --out /tmp/cv2_bs_current_s2`
  no longer fails in the wrapper; it builds stage1, passes both stage1 smokes,
  then reaches the real stage2 compiler build.

Boundary: the real canonical `s1 -> s2b` gate still fails at stage2 timeout
after emitting `/tmp/cv2_bs_current_s2/cv2_s2.ll` (189MB, 3,930,328 lines,
39,112 LLVM `define`s, 338 stub markers) and after
`[ALLOC_FLUSH] Generated 98 deferred allocators`. This points back to the
over-materialized helper graph / large-IR corridor, not to the wrapper.
{F/G/R: 0.96/0.8/0.95} [verified]

[LM-508|verified]: The late LLVM backend timeout hypothesis was narrowed by
opt-in tail-generation timing. `CRYSTAL_V2_LLVM_TAIL_STATS=1` is intentionally
paired with `CRYSTAL_V2_TRACE_STDERR=1` because the probes use
`bootstrap_trace_puts`; without the trace env the diagnostic remains silent.
On the full compiler stage2 attempt with LLVM reachability enabled, backend
generation reported `RTA kept: 27806 (pruned 9921)` from `37727` MIR functions,
then completed `generate(io)` and reached `[LLVM_TAIL_GEN] phase=finalize_enter
out=180584919` before `run_safe` killed the overall compile at 300s. The
tail helpers themselves were fast: string constants about `50ms`, undefined
extern declarations about `98ms`, missing Crystal stubs about `21ms`, and
`emit_type_name_table` about `166ms` while adding the largest tail payload
(`~27.8MB` for `21694` type names).

Evidence:

- `crystal build src/crystal_v2.cr -o /tmp/cv2_tail_stats --error-trace`
  -> exit 0.
- `regression_tests/p2_llvm_tail_stats_no_prelude.sh /tmp/cv2_tail_stats`
  -> `p2_llvm_tail_stats_no_prelude_ok phase=type_name_table ...`.
- `CRYSTAL_V2_TRACE_STDERR=1 CRYSTAL_V2_LLVM_REACHABILITY=1
  CRYSTAL_V2_LLVM_TAIL_STATS=1 scripts/run_safe.sh /tmp/cv2_tail_stats 300
  4096 src/crystal_v2.cr -o /tmp/cv2_tail_stats_trace_s2` -> expected
  timeout, but the log contains `[STAGE2_TRACE] step5: generate done` before
  `[KILL] Timeout`.

Boundary: this is diagnostic only. It refutes "one slow backend tail helper" as
the current root and moves the frontier to total generated-IR volume / pre-llc
budget. It does not make `s1 -> s2b` green and does not justify increasing
timeouts. {F/G/R: 0.9/0.65/0.9} [verified]

[LM-509|verified]: Generated stage2 no-prelude `puts 7` exposed that
bootstrap-hot debug helpers must not depend on variadic tuple splats. Before
the fix, the generated compiler aborted during pass3 setup with:

    STUB CALLED: Crystal$CCHIR$CCAstToHir$Hdebug_env_filter_match$Q$$String_Tuple$LString$R_splat

The root was not `puts` lowering. `debug_env_filter_match?(env_key, *texts)`,
`debug_hook_filter_match?(*texts)`, and `debug_class_repair_enabled_for?(*texts)`
generated tuple-splat helper calls throughout the compiler, but generated
stage2 had ABORT stubs for those helper bodies. The fix changes those helpers
to fixed optional text slots (current callsites use at most four texts) and
keeps the matching logic local, preserving debug-env behavior without requiring
Tuple splat lowering in the bootstrap-hot path.

Evidence:

- `crystal build src/crystal_v2.cr -o /tmp/cv2_debug_filter_fix --error-trace`
  -> exit 0.
- `regression_tests/p2_generated_stage2_no_prelude_puts_guard.sh
  /tmp/cv2_debug_filter_fix` -> `p2_generated_stage2_no_prelude_puts_guard_ok
  frontier=nocodegen_clean_full_codegen_hang`.
- The fresh full-codegen compile log no longer mentions
  `debug_env_filter_match`; it now stops at
  `Tuple$LString$C$_Crystal$CCMIR$CCType$R$Hjoin$$IO_String_block`, while the
  secondary `--no-codegen` probe exits 0.

Boundary: this is a root fix for debug helper splat usage, not a general tuple
block lowering fix. The next generated-stage2 root is the tuple `join` block
stub family. {F/G/R: 0.9/0.65/0.9} [verified]

[LM-510|verified]: The tuple `join` generated-stage2 frontier was localized to
backend extern-call argument formatting, not user `puts` semantics. An lldb
abort backtrace for generated `puts 7 --no-prelude` showed:

    Tuple(String, Crystal::MIR::Type)#join(IO, String, &block)
    Tuple#to_s(IO)
    Tuple#to_s
    Crystal::MIR::LLVMIRGenerator#emit_extern_call

The triggering source was `args = arg_entries.map { |(t, v, _)| "#{t} #{v}" }
.join(", ")` in `emit_extern_call`. In generated stage2, that block
destructuring / interpolation path could format the tuple itself and reach the
unlowered tuple `join` block stub. The accepted fix keeps the formatting inline
and indexed (`entry[0]`, `entry[1]`) so no new helper method must be discovered
by RTA and no tuple `to_s`/block-join body is needed in this bootstrap-hot path.

Evidence:

- `crystal build src/crystal_v2.cr -o /tmp/cv2_extern_join_inline
  --error-trace` -> exit 0.
- `regression_tests/p2_bootstrap_semantic_emit_oracle.sh
  /tmp/cv2_extern_join_inline` -> `p2_bootstrap_semantic_emit_oracle_ok`.
- `regression_tests/p2_generated_stage2_no_prelude_puts_guard.sh
  /tmp/cv2_extern_join_inline` -> `p2_generated_stage2_no_prelude_puts_guard_ok
  frontier=eventloop_close_fd_rta_gap`.

Boundary: this is a root fix for the current backend formatter dependency on
tuple block formatting, not a general tuple block/destructuring implementation.
The next generated-stage2 root is `Crystal::EventLoop#close(IO::FileDescriptor)`
RTA/lowering discovery. {F/G/R: 0.9/0.62/0.9} [verified]

[LM-511|verified]: The generated-stage2
`Crystal::EventLoop#close(IO::FileDescriptor)` frontier was a two-stage demand
tracking mismatch, not an EventLoop-specific backend bug.

Findings:

- HIR lowering did emit the call as virtual and materialized the inherited
  implementation as `Crystal::EventLoop::Polling#close$Crystal::System::FileDescriptor`.
- Final HIR RTA then pruned that materialized virtual target because it rebuilt
  reachability from calls and type descriptors without honoring the target set
  already demanded by HIR virtual-dispatch lowering.
- After retaining those HIR-demanded targets, MIR still needed one compatibility
  rule: a virtual call with a typed suffix may resolve to a unique same-method,
  same-arity inherited implementation when the exact typed name is absent. This
  is constrained to a single candidate so ambiguous overload families such as
  `<<$Char` vs `<<$String` stay rejected.

Evidence:

- `crystal build src/crystal_v2.cr -o /tmp/cv2_vtarget_mir --error-trace`
  -> exit 0.
- `CRYSTAL_V2_STOP_AFTER_HIR=1 scripts/run_safe.sh /tmp/cv2_vtarget_fix 300
  4096 src/crystal_v2.cr --emit hir --no-link -o /tmp/cv2_vtarget_fix_hir`
  -> exit 0; final HIR retained
  `Crystal::EventLoop::Polling#close$Crystal::System::FileDescriptor`.
- `CRYSTAL_V2_TRACE_STDERR=1 scripts/run_safe.sh /tmp/cv2_vtarget_mir 360
  4096 src/crystal_v2.cr --emit llvm-ir --no-link -o /tmp/cv2_vtarget_mir_ir`
  -> exit 0; `IO::FileDescriptor#system_close` calls
  `__vdispatch__Crystal$CCEventLoop$Hclose$$IO$CCFileDescriptor$$T329`, the
  vdispatch body calls `Crystal$CCEventLoop$CCPolling$Hclose$$Crystal$CCSystem$CCFileDescriptor`,
  and the old `STUB CALLED: Crystal$CCEventLoop$Hclose$$IO$CCFileDescriptor`
  string is absent.
- `regression_tests/p2_generated_stage2_no_prelude_puts_guard.sh
  /tmp/cv2_vtarget_mir` -> `p2_generated_stage2_no_prelude_puts_guard_ok
  frontier=nocodegen_clean_full_codegen_hang`.
- `regression_tests/p2_bootstrap_semantic_emit_oracle.sh /tmp/cv2_vtarget_mir`,
  `regression_tests/p2_llvm_tail_stats_no_prelude.sh /tmp/cv2_vtarget_mir`,
  and `regression_tests/p2_pending_budget_no_prelude.sh /tmp/cv2_vtarget_mir`
  -> all ok.

Boundary: this preserves HIR-demanded virtual targets and allows only unique
same-arity MIR typed-suffix fallback. It is not a broad arity-only overload
fallback and does not implement unresolved block/tuple lowering families.
Current generated-stage2 guard frontier is the full-codegen-only
`nocodegen_clean_full_codegen_hang` state. {F/G/R: 0.92/0.68/0.92} [verified]

[LM-512|verified]: The generated-stage2 `String#call` / duplicate-HIR-id
frontier was two independent HIR registration/lowering bugs, not a Nil
call-argument ABI bug.

Findings:

- `of -> Nil` was stringified without preserving the nilary proc shape, so
  registration-time inference for `Process.after_fork_child_callbacks` seeded
  `Array(String)`. That later lowered callback elements as `String#call`.
- Generic container names that used `get_type_name_from_ref` collapsed
  `Proc(...)` to the display name `Proc`, losing callable type parameters for
  Array/Hash/NamedTuple specialization and element access.
- Struct/HIR getter inlining treated any zero-arg method sharing an ivar name as
  a field getter. In generated stage2 this inlined
  `HIR::Function#next_value_id` as a raw `@next_value_id` field load, skipping
  the increment and causing duplicate HIR ids such as repeated `%2`.
- A failed alternate branch showed `SystemError#included` expands to a
  `BeginNode` containing `extend ::SystemError::ClassMethods`, but naive
  recursive processing currently reintroduces a stage2 `lower_main` timeout.
  Keep that as a separate CAUTION root task; it is not part of this verified
  slice.

Fix:

- `stringify_type_expr` handles unary `->` as `Proc(Return)`, mapping
  nilary `-> Nil` to `Proc(Void)` to match emitted callback bodies.
- Generic container canonicalization now preserves full Proc parameter shape via
  `generic_param_type_name_from_ref`.
- Array element typing checks the value's own Array descriptor before trusting a
  stale lowering-context type map.
- Getter field inlining is proof-based: only a DefNode whose body is the
  trivial `@ivar` getter can inline; out-of-arena getter body ExprIds return
  "not proven getter" instead of raising.
- `p2_generated_stage2_no_prelude_puts_guard.sh` now fails closed on any
  unrecorded `STUB CALLED` before accepting the current no-codegen-clean/full
  codegen frontier.

Evidence:

- `crystal build src/crystal_v2.cr -o /tmp/cv2_safe_commit --error-trace` ->
  exit 0, only the known `Random::DEFAULT` warning.
- `regression_tests/p2_bootstrap_semantic_emit_oracle.sh /tmp/cv2_safe_commit`
  -> `p2_bootstrap_semantic_emit_oracle_ok`.
- `regression_tests/p2_pending_budget_no_prelude.sh /tmp/cv2_safe_commit` ->
  `p2_pending_budget_no_prelude_ok process_delta=3 emit_delta=4
  lower_missing_delta=44 total=92 max_queue=57`.
- `bash -n regression_tests/p2_generated_stage2_no_prelude_puts_guard.sh` and
  `git diff --check` -> exit 0.
- `regression_tests/p2_generated_stage2_no_prelude_puts_guard.sh
  /tmp/cv2_safe_commit` -> `p2_generated_stage2_no_prelude_puts_guard_ok
  frontier=nocodegen_clean_full_codegen_hang`.

Boundary: this is a root fix for proc-shaped type registration and proven
getter field inlining. It does not implement general `SystemError#included`
BeginNode expansion, full codegen hang diagnosis, or general nested tuple/block
payload lowering. {F/G/R: 0.92/0.70/0.92} [verified]

[LM-513|verified]: The generated-stage2 no-prelude `puts 7` malformed LLVM
frontier was an LLVM backend slot-map consumption bug, not stale Hash storage.

Findings:

- Generated stage2 emitted invalid IR:
  `store ptr null, ptr %` inside `__crystal_main`.
- A temporary clear-check showed `@cross_block_slots.size == 0`,
  `has_key?(3_u32) == false`, and `@cross_block_slots[3_u32]? == nil` at
  function entry in generated stage2, so the map was not retaining stale keys.
- A falsifier that routed real `Hash#clear` functions through the existing V2
  layout-safe clear body did materialize those bodies in `generated_s2.ll`, but
  the empty `%` stores still reproduced. That refuted the stale-Hash-clear
  hypothesis for this frontier.
- The surviving source pattern was the backend's use of
  `@cross_block_slots[inst.id]?` directly in assignment-in-condition. In the
  generated compiler this could enter the slot-store branch for a missing key
  and bind an empty local string. The backend invariant is stricter: cross-block
  slot stores are legal only when the slot map contains the key.

Fix:

- `emit_instruction` now gates cross-block slot consumption with
  `@cross_block_slots.has_key?(inst.id)` and indexes only after that guard.
- The generated-stage2 guard now treats the old `store ptr null, ptr %` shape
  as a hard regression and records the next frontier precisely as
  `extern_puts_arg_type_codegen_gap`.

Evidence:

- `crystal build src/crystal_v2.cr -o /tmp/cv2_slot_haskey_only --error-trace`
  -> exit 0, only the known `Random::DEFAULT` warning.
- `regression_tests/p2_generated_stage2_no_prelude_puts_guard.sh
  /tmp/cv2_slot_haskey_only` ->
  `p2_generated_stage2_no_prelude_puts_guard_ok
  frontier=extern_puts_arg_type_codegen_gap`; saved IR no longer contains
  `store ptr null, ptr %`.
- `regression_tests/p2_bootstrap_semantic_emit_oracle.sh
  /tmp/cv2_slot_haskey_only` -> `p2_bootstrap_semantic_emit_oracle_ok`.
- `regression_tests/p2_pending_budget_no_prelude.sh
  /tmp/cv2_slot_haskey_only` ->
  `p2_pending_budget_no_prelude_ok process_delta=3 emit_delta=4
  lower_missing_delta=44 total=92 max_queue=57`.
- `bash -n regression_tests/p2_generated_stage2_no_prelude_puts_guard.sh` and
  `git diff --check` -> exit 0.

Boundary: this fixes the empty cross-block slot malformed-LLVM class. It does
not fix the next generated-stage2 semantic codegen bug: `puts 7` currently
lowers to duplicate `__crystal_v2_print_int32_ln(ptr null)` calls instead of a
single `i32 7` extern call. {F/G/R: 0.92/0.62/0.91} [verified]

[LM-514|verified]: The generated-stage2 no-prelude `puts 7` extern-call ABI
frontier was two backend key-presence failures, not a HIR runtime-print fallback
bug.

Findings:

- Generated HIR for the repro already contained one extern call:
  `extern_call @__crystal_v2_print_int32_ln(%2)`, with `%2` as `Int32`.
- Generated MIR lowering first duplicated that one HIR block:
  `[MIR_LOWER] function=__crystal_main blocks=1`, followed by
  `ordered blocks count=2` and two emitted extern calls. The source was
  `order_blocks_for` using `Set(HIR::BlockId)`, which is backed by
  `Hash(BlockId, Nil)`; generated stage2 mis-deduped the single-entry function.
- After block ordering was made deterministic with a linear visited list, the
  backend still emitted `call void @__crystal_v2_print_int32_ln(ptr 7)`.
  That refuted the earlier "ptr null only" formulation and exposed the second
  root: extern-call arg typing used `@value_types[arg_id]? || TypeRef::POINTER`
  even though the Int32 type entry was present.
- The same generated-stage2 hazard had already appeared in slot lookup: nilable
  `hash[key]?` in critical codegen maps can conflate missing keys with present
  values or enter the wrong branch. The backend invariant is key-presence first,
  then indexing.

Fix:

- `HIRToMIR#order_blocks_for` uses a small linear `Array(HIR::BlockId)` visited
  list instead of `Set(HIR::BlockId)` for this tiny traversal.
- `LLVMIRGenerator#emit_extern_call` gates `@value_types` argument lookups and
  called-function signature tracking with `has_key?` before indexing.
- `LLVMIRGenerator#value_ref` applies the same key-presence invariant for
  constants, cross-block slots, and value names.
- `p2_generated_stage2_no_prelude_puts_guard.sh` now classifies any
  `__crystal_v2_print_int32_ln(ptr ...)` call as the extern arg type frontier,
  so `ptr 7` cannot be hidden as a generic full-codegen frontier again.

Evidence:

- `crystal build src/crystal_v2.cr -o /tmp/cv2_extern_arg_type_fix
  --error-trace` -> exit 0, only the known `Random::DEFAULT` warning.
- `regression_tests/p2_generated_stage2_no_prelude_puts_guard.sh
  /tmp/cv2_extern_arg_type_fix` ->
  `p2_generated_stage2_no_prelude_puts_guard_ok
  frontier=nocodegen_clean_full_codegen_hang`.
- Raw kept IR from that guard shows exactly:
  `call void @__crystal_v2_print_int32_ln(i32 7)`.
- `regression_tests/p2_bootstrap_semantic_emit_oracle.sh
  /tmp/cv2_extern_arg_type_fix` -> `p2_bootstrap_semantic_emit_oracle_ok`.
- `regression_tests/p2_pending_budget_no_prelude.sh
  /tmp/cv2_extern_arg_type_fix` ->
  `p2_pending_budget_no_prelude_ok process_delta=3 emit_delta=4
  lower_missing_delta=44 total=92 max_queue=57`.
- `bash -n regression_tests/p2_generated_stage2_no_prelude_puts_guard.sh` and
  `git diff --check` -> exit 0.

Boundary: no-prelude extern-call ABI for this reducer is fixed, but generated
stage2 still does not produce a runnable binary in the full-codegen path. The
recorded next frontier remains `nocodegen_clean_full_codegen_hang`: the same
generated compiler exits cleanly under `--no-codegen`, and the full path emits
a valid-looking `.ll`/object but leaves no executable. {F/G/R: 0.93/0.65/0.92}
[verified]

[LM-515|verified]: The full-prelude Kqueue `after_fork` HIR branch leak was a
macro-literal registration ordering bug, not a stdlib/runtime problem.

Findings:

- `Crystal::EventLoop::Kqueue#after_fork` was first registered through the
  module macro-literal path, before the later class macro-literal consistency
  path could replace the poisoned base symbol.
- `process_macro_literal_in_module` evaluated `strip_macro_lines` before
  `expand_flag_macro_text`, destroying `{% if LibC.has_constant?(:EVFILT_USER)
  %}` / `{% else %}` markers before the platform branch selector could run.
  The parser then saw both branch bodies as plain Crystal and registered the
  EVFILT_USER path together with the fallback `@pipe` / `system_pipe` path.
- Semantic macro expansion also needed a platform `LibC.has_constant?`
  fallback for constants whose declarations are hidden behind platform
  requires. HIR and semantic fallback lists must stay synchronized; this
  checkpoint aligns the modeled kqueue/epoll/io_uring/POSIX signal constants.

Fix:

- Expand flag/member-query macro controls before stripping macro lines in the
  raw-text and per-text `process_macro_literal_in_module` paths.
- Parse expanded class macro-literal bodies with
  `parse_macro_literal_class_body` and feed children through
  `register_class_members_from_expansion`, avoiding a second hand-written
  registration case tree.
- Teach `evaluate_flag_condition_state` to evaluate simple
  `LibC.has_constant?(:X)` / `Type.has_method?(:x)` text conditions when
  `expand_flag_macro_text` sees source text instead of AST `MacroIfNode`s.
- Add a Darwin/BSD regression guard that extracts
  `Crystal::EventLoop::Kqueue#after_fork` HIR and requires `LibC.@@EVFILT_USER`
  while rejecting `Crystal::System::FileDescriptor.system_pipe`,
  `LibC.@@EVFILT_READ`, and `Crystal::EventLoop::Polling#pipe` in that body.

Evidence:

- `crystal build src/crystal_v2.cr -o /tmp/cv2_macro_control_check
  --error-trace` -> exit 0, only the known `Random::DEFAULT` warning.
- `regression_tests/p2_macro_control_module_literal_guard.sh
  /tmp/cv2_macro_control_check` -> `p2_macro_control_module_literal_guard_ok`.
- `regression_tests/p2_bootstrap_semantic_emit_oracle.sh
  /tmp/cv2_macro_control_check` -> `p2_bootstrap_semantic_emit_oracle_ok`.
- `regression_tests/p2_pending_budget_no_prelude.sh
  /tmp/cv2_macro_control_check` ->
  `p2_pending_budget_no_prelude_ok process_delta=3 emit_delta=4
  lower_missing_delta=44 total=92 max_queue=57`.
- `regression_tests/p2_generated_stage2_no_prelude_puts_guard.sh
  /tmp/cv2_macro_control_check` ->
  `p2_generated_stage2_no_prelude_puts_guard_ok
  frontier=nocodegen_clean_full_codegen_hang`.
- `bash -n regression_tests/p2_macro_control_module_literal_guard.sh` and
  `git diff --check` -> exit 0.

Boundary: this proves the Kqueue `after_fork` branch leak is removed on the
local Darwin target. It does not prove cross-target macro semantics. A later
test-oracle maintenance pass restored `p2_selfhost_stage2_shape_guard.sh` by
making stale demand-tied callback sentinels demand-aware; see LM-516.
{F/G/R: 0.91/0.58/0.90} [verified]

[LM-516|verified]: `p2_selfhost_stage2_shape_guard.sh` needed demand-aware
callback sentinels after recent demand/RTA and macro-control fixes removed two
incidental old materialization paths.

Findings:

- The LM-471 `Array(String)#each_index` bug was real, but the self-host MIR
  gate was requiring a historical side effect: `Array(String)#each$block`
  happened to contain a nested `each_index` callback under an older generated
  stage2 frontier. Current self-host MIR may not materialize that wrapper at
  all, so absence of the nested proc is not a shape regression.
- The `Dir.glob(..._block_splat)` callback-shape check had the same issue:
  earlier fixes deliberately moved or removed the old wrapper demand while
  preserving the invariant that, if the forwarding proc is emitted, it must be
  `String`-shaped and must not self-recurse.
- A one-pass AWK check was order-fragile because MIR function definitions can
  be printed before the function that references their `func_pointer`. The
  guard now scans the MIR twice: first to collect nested callback proc names
  from the wrapper body, then to validate the referenced proc definitions.

Fix:

- Make the `Array(String)#each_index` and `Dir.glob(..._block_splat)` shape
  sentinels demand-aware: if the nested proc is present, its signature is
  enforced; if the wrapper/proc is absent, the self-host MIR gate does not fail.
- Add `regression_tests/p2_each_index_block_param_no_prelude.sh`, a direct fast
  no-prelude HIR oracle for the actual LM-471 invariant. It compiles
  `["x"].each_index { |i| i }`, requires the `Array(String)#each_index$block`
  call, and rejects a `String`-shaped block proc.

Evidence:

- `crystal build src/crystal_v2.cr -o /tmp/cv2_shape_guard_check
  --error-trace` -> exit 0, only the known `Random::DEFAULT` warning.
- `regression_tests/p2_each_index_block_param_no_prelude.sh
  /tmp/cv2_shape_guard_check` -> `p2_each_index_block_param_no_prelude_ok`.
- `regression_tests/p2_selfhost_stage2_shape_guard.sh
  /tmp/cv2_shape_guard_check` -> `p2_selfhost_stage2_shape_guard_ok`.
- `bash -n regression_tests/p2_each_index_block_param_no_prelude.sh
  regression_tests/p2_selfhost_stage2_shape_guard.sh` and `git diff --check`
  -> exit 0.

Boundary: this is test-oracle maintenance, not a compiler behavior change. It
keeps the old shape checks active when the old wrappers are emitted and moves
the `each_index` root invariant to a direct no-prelude guard. It does not add a
direct `Dir.glob` focused oracle; that remains covered indirectly by the
existing self-host gate when the wrapper is materialized. {F/G/R:
0.93/0.62/0.92} [verified]

[LM-517|verified]: The generated-stage2 no-prelude `puts 7` full-codegen/link
frontier is cleared by fixing the bootstrap CLI command tail, not by changing
HIR/MIR/LLVM code generation for the program body.

Findings:

- Preserved artifacts showed the generated compiler emitted `repro_bin.ll` and
  a valid Mach-O object, but left only `repro_bin.o.cmdtmp` and exited without a
  final executable. The first root was in `CLI#run_command_capture_output`:
  generated stage2 mis-lowered `Crystal::System::Process.fork`'s nilable
  parent/child contract as a plain `Int32`, so the parent compiler process also
  entered the child `execvp(llc)` path and skipped the rename/link tail.
- After switching that path to raw `LibC.fork`, the next root was
  `LibC.waitpid(pid, out status, 0)`: generated stage2 mis-lowered the `out`
  storage and decoded pointer garbage as the wait status. Explicit
  `pointerof(status)` observes the real tool status.
- The next no-prelude link tail pulled an unlowered `Time#<=>` through the
  runtime-stub freshness check, and the LLVM cache path could treat stale or
  empty artifacts as hits. The tail now avoids Time ordering for the stub, gates
  LLVM cache hits with `command_output_ready?`, and copies cache artifacts via a
  small LibC read/write helper instead of bootstrap-hot `FileUtils.cp`.
- The `p2_generated_stage2_no_prelude_puts_guard.sh` RWLock sentinel is now
  demand-aware: if `Crystal::RWLock#write_lock` is emitted, it must still load
  `LOCKED`; if the demand-driven generated compiler does not materialize it,
  the guard no longer fails on absence alone.

Evidence:

- `crystal build src/crystal_v2.cr -o /tmp/cv2_runner_rawcopy_check
  --error-trace` -> exit 0, only the known `ld64.lld` stack-size warning.
- `regression_tests/p2_generated_stage2_no_prelude_puts_guard.sh
  /tmp/cv2_runner_rawcopy_check` ->
  `p2_generated_stage2_no_prelude_puts_guard_ok`.
- `regression_tests/p2_pending_budget_no_prelude.sh
  /tmp/cv2_runner_rawcopy_check` ->
  `p2_pending_budget_no_prelude_ok process_delta=3 emit_delta=4
  lower_missing_delta=44 total=92 max_queue=57`.
- `regression_tests/p2_bootstrap_semantic_emit_oracle.sh
  /tmp/cv2_runner_rawcopy_check` -> `p2_bootstrap_semantic_emit_oracle_ok`.
- `regression_tests/p2_each_index_block_param_no_prelude.sh
  /tmp/cv2_runner_rawcopy_check` -> `p2_each_index_block_param_no_prelude_ok`.
- `bash -n regression_tests/p2_generated_stage2_no_prelude_puts_guard.sh`
  and `git diff --check` -> exit 0.

Boundary: this is a root fix for the bootstrap-critical CLI command/link tail.
It is not a general fix for all nilable-return lowering, all `out` arg
lowering, `Time#<=>`, or `FileUtils.cp` in arbitrary code. Those remain
separate compiler/runtime follow-ups if they appear outside this tail.
{F/G/R: 0.94/0.62/0.94} [verified]

[LM-518|verified]: Lowered constant truthiness must prune dead branch bodies,
not just emit a constant branch terminator.

Findings:

- `responds_to?` can become a Bool literal only after expression lowering.
  The previous `lower_if` static path only handled AST-literal conditions, and
  `lower_condition_branch` always emitted a `Branch` even when the lowered
  condition was a constant Bool.
- The first fix converted constant lowered conditions to direct `Jump`, but a
  hostile no-prelude oracle still failed for `dynamic && x.responds_to?(:object_id)`:
  `lower_if` had already created `then_block` and unconditionally lowered the
  then body, leaving an unreachable `Int32#object_id` call in HIR.
- The root fix is to preserve condition side effects, compute CFG reachability
  after condition lowering, and for no-`elsif` `if` expressions lower only the
  reachable body when exactly one body block is reachable. This keeps dead
  `responds_to?` branches from becoming source demand.
- A refuted adjacent experiment: treating exact `Proc#call` as backend-owned
  removed that name from the top missing summary but changed full-source
  `lower_missing.initial` by only one function (`+25104` -> `+25103`) and was
  reverted. It is not the current lower-missing root.

Evidence:

- `crystal build src/crystal_v2.cr -o /tmp/cv2_static_truthy_if
  --error-trace` -> exit 0.
- `regression_tests/p2_static_truthy_dead_branch_no_prelude.sh
  /tmp/cv2_static_truthy_if` ->
  `p2_static_truthy_dead_branch_no_prelude_ok lower_missing_delta=0`.
- The focused HIR for `dynamic && x.responds_to?(:object_id)` has no
  `Int32#object_id` call; it emits a dynamic branch to the RHS block, the RHS
  lowers `responds_to?` to `false`, and jumps to the else block.
- `regression_tests/p2_pending_budget_no_prelude.sh
  /tmp/cv2_static_truthy_if`,
  `regression_tests/p2_bootstrap_semantic_emit_oracle.sh
  /tmp/cv2_static_truthy_if`,
  `regression_tests/p2_backend_intrinsic_boundary_no_prelude.sh
  /tmp/cv2_static_truthy_if`,
  `regression_tests/p2_each_index_block_param_no_prelude.sh
  /tmp/cv2_static_truthy_if`, and
  `regression_tests/p1_ir_shape_check.sh /tmp/cv2_static_truthy_if` all
  passed.
- Full-source `STOP_AFTER_HIR` exited 0 with
  `process_pending: 3146 -> 17177 (+14031)`,
  `emit_tracked_sigs: 17177 -> 17404 (+227)`,
  `lower_missing.initial: 17404 -> 42402 (+24998)`, and
  `lower_missing: 17404 -> 42732 (+25328)`.

Boundary: this is a verified root fix for lowered-constant dead-branch demand,
but not the final Hash/object-id corridor. Full-source logs still show
`Hash#entry_matches?` / union call-shape demand producing value-type
`object_id` missing targets; that is the next separate root to localize.
{F/G/R: 0.91/0.56/0.90} [verified]

[LM-519|verified]: `responds_to?(:object_id)` must be answered from the
Reference/value type hierarchy, not from the mutable function registry.

Findings:

- Full-source HIR after LM-518 still showed value-type `object_id` demand.
  Inspecting the dump found functions where `UInt32.responds_to?(:object_id)`
  had lowered to `literal true`, for example in `Hash(UInt32, Int32)#key_hash`.
- Focused minimal Hash programs were clean, which ruled out the source
  `Hash#key_hash` logic itself as the only root. The full compiler run had
  polluted the function registry with synthetic value-type `object_id`
  specializations; later `type_responds_to_method?` calls used
  `has_function_base?` and treated those synthetic entries as real method
  availability.
- `object_id` is a Reference primitive in Crystal. Value types such as
  `UInt32`, `Int32`, and `Tuple` must answer false regardless of whether a
  previous lowering pass has created a synthetic `Type#object_id` function.

Fix:

- `type_responds_to_method?` now handles instance `object_id` through the class
  parent chain: `Reference` and descendants answer true; `Object`/value
  hierarchies answer false. Other methods keep the existing lookup path.
- Added `p2_object_id_responds_to_semantics.sh`, which checks that
  `UInt32.responds_to?(:object_id)` lowers to false and emits no
  `UInt32#object_id`, while `String.responds_to?(:object_id)` still preserves
  the `Reference#object_id` path.

Evidence:

- `crystal build src/crystal_v2.cr -o /tmp/cv2_object_id_semantics
  --error-trace` -> exit 0.
- `regression_tests/p2_object_id_responds_to_semantics.sh
  /tmp/cv2_object_id_semantics` -> `p2_object_id_responds_to_semantics_ok`.
- `regression_tests/p2_static_truthy_dead_branch_no_prelude.sh`,
  `p2_pending_budget_no_prelude.sh`, `p2_bootstrap_semantic_emit_oracle.sh`,
  `p2_backend_intrinsic_boundary_no_prelude.sh`,
  `p2_each_index_block_param_no_prelude.sh`, and `p1_ir_shape_check.sh` all
  passed with `/tmp/cv2_object_id_semantics`.
- Full-source `STOP_AFTER_HIR` exited 0; value-type `object_id` dropped out of
  the top missing summary. Phase stats were essentially unchanged:
  `lower_missing.initial: 17404 -> 42403 (+24999)` and
  `lower_missing: 17404 -> 42733 (+25329)`.

Boundary: this is a correctness/root fix for `responds_to?(:object_id)`
semantic pollution, not a bootstrap-volume fix. The next volume roots are now
visible as `Indexable#new`, `Proc#call`, HIR/MIR value initializers, and debug
helper demand in the initial missing-target sweep.
{F/G/R: 0.92/0.58/0.90} [verified]

## Active Strategy

- Main fast loop: `--no-prelude` oracles and focused STOP_AFTER_HIR budget
  checks.
- Integration gate: canonical `s1 -> s2b` must pass before any `s2 -> s3`
  attempt. The generated-stage2 no-prelude `puts 7` full-codegen/link guard is
  now green; next gate is broader no-prelude corpus emission/comparison.
- Rare full gate: `s1 -> s5b` plus normalized HIR/MIR/LL equality.
- Do not run `s3b+` until generated `s2b` passes the fixed no-prelude corpus and
  normalized `s1_bootstrap` vs `s2b` semantic comparison.
