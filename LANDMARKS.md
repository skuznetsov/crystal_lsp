# LANDMARKS

Updated: 2026-03-05
Context: compiler/bootstrap/stage2-stability

[LM-1|goal]: `stage1 -> stage2` bootstrap is reproducibly buildable in `--release` on current branch (how: `scripts/build_stage1_original_release.sh` + `scripts/build_stage2_release.sh`) {F/G/R: 0.9/0.6/0.9} [verified]

[LM-2|pattern]: stage2-only failure class is broader than container-specific triggers: minimal `trivial.cr` / `macro.cr` also fail on stage2 while stage1 passes (how: direct control matrix with `/tmp/stage1_rel_flags_fix` vs `/tmp/stage2_rel_flags_fix`) {F/G/R: 0.9/0.7/0.9} [verified]

[LM-3|repro]: focused stage2 repro exists independent of full bootstrap source: `regression_tests/stage2_container_clear_index_oob_repro.sh` (single `Hash(UInt32, String)` + `clear`) fails on stage2 with `Index out of bounds` and passes on stage1 control {F/G/R: 0.9/0.8/0.9} [verified]

[LM-4|perf]: release timings continue to show stage2 faster than stage1 on this host, with high variance from cache/workspace state (examples: `stage1 725.24s -> stage2 685.75s`; isolated-cache run `stage1 432.29s -> stage2 236.14s`, ~`1.83x`) {F/G/R: 0.9/0.5/0.8} [working]

[LM-5|boundary]: `stage_stats_output_repro.sh` still fails on stage2 (`Index out of bounds`) and stage2->stage3 remains unstable {F/G/R: 0.9/0.6/0.9} [verified]

[LM-6|boundary]: updated `stage2_llvm_setup_pre_generate_segfault_repro.sh` signature remains stage2-only and catches current boundary drift (status `139`, `[MIR_SETUP] before lowering.new`, no `[LLVM_SETUP] generate(io|string) done`) {F/G/R: 0.9/0.7/0.9} [verified]

[LM-7|boundary]: with `LLVM_INIT` trace enabled, stage2 reaches `after generator.new` and `generate(io) start`, then crashes before `generate(io) done` on minimal macro repro {F/G/R: 0.9/0.7/0.9} [verified]

[LM-8|boundary]: with `LLVM_GEN` trace enabled, stage2 reaches `emit_function start __crystal_main` and then crashes inside `reset_value_names` clear-sequence before `emit_function done` {F/G/R: 0.9/0.7/0.9} [verified]

[LM-9|repro]: `regression_tests/stage2_reset_value_names_fiberevent_clear_repro.sh` was previously reproducing, but no longer reproduces on `/tmp/stage2_rel_status_20260305_002400` (`status=139`, `fiber_clear_calls_in_reset_value_names=0`) {F/G/R: 0.9/0.7/0.9} [stale]

[LM-10|root-cause]: prior `FiberEvent#clear`-in-reset path is no longer the active top-frame on the current binaries after ivar-decl fixes; treat it as historical localization, not current boundary {F/G/R: 0.8/0.5/0.8} [stale]

[LM-11|stability]: latest release bootstrap chain still fails at stage2->stage3 despite successful stage1->stage2 build (`/tmp/stage2_rel_current`), with immediate `Index out of bounds` on stage3 attempt (`real 0.67`) {F/G/R: 0.9/0.7/0.9} [verified]

[LM-12|boundary]: LLDB on `/tmp/stage2_dbg_status_20260305_002400` now shows crash path `Hash(MIR::BlockId, Nil)#clear_impl -> Hash(MIR::BlockId, Nil)#clear -> Set(HIR::BlockId)#clear -> LLVMIRGenerator#emit_function` during first function emission on minimal macro repro {F/G/R: 0.9/0.8/0.9} [verified]

[LM-13|repro]: focused oracle `regression_tests/stage2_emit_function_blockid_clear_repro.sh` reproduces the updated stage2 crash class on `/tmp/stage2_dbg_status_20260305_002400` and does not reproduce on stage1 control `/tmp/stage1_rel_status_20260305_002400` {F/G/R: 0.9/0.8/0.9} [verified]

[LM-14|perf]: latest autonomous release bootstrap pair succeeded with strong stage1->stage2 speedup (`/tmp/stage1_rel_autonomous_20260305y` `real 438.48` -> `/tmp/stage2_rel_autonomous_20260305y` `real 117.26`, ~`3.74x`) {F/G/R: 0.9/0.4/0.8} [verified]

[LM-15|repro]: new fast oracle `regression_tests/stage2_main_selfloop_repro.sh` reproduces stage2-only `main` self-loop signature while compiling `src/crystal_v2.cr --release` (LLDB frame #0 in `Crystal$Dmain...` + self-branch `b 0x... -> same 0x...`) and does not reproduce on stage1 control {F/G/R: 0.9/0.7/0.9} [verified]

Contradiction ledger
- [LM-C1|refute]: broad `reset_value_names` reinit experiment (replace many `clear` with fresh container allocations) did not produce robust stabilization; it shifted crash boundaries and was rejected.
- [LM-C2|refute]: cache-only explanation is insufficient: fresh isolated stage2 debug cache (`CRYSTAL_CACHE_DIR_STAGE2_DEBUG=/tmp/crystal_cache_stage2_debug_reset_clean`) still reproduces `stage2_reset_value_names_fiberevent_clear_repro` with `status=139` and `FiberEvent$Hclear` drift.
- [LM-C3|refute]: `Set`-specific root-cause hypothesis is insufficient; stage2 fails equally on trivial/no-container and `Hash/Set` micro-repros.
- [LM-C4|refute]: targeted `@alloc_types.clear -> reinit` workaround in `reset_value_names` was unstable and regressed crash boundary; rejected.
- [LM-C5|refute]: converting `@alloc_types/@alloc_element_types` from Hash to array-indexed storage did not provide robust stage2 stabilization; boundary remained unstable and change was reverted.
- [LM-C6|refute]: `FiberEvent$Hclear` call-target drift in `reset_value_names` is no longer reproduced on current stage2 release/debug binaries; active crash shifted to BlockId hash/set clear path in `emit_function`.
- [LM-C7|refute]: current stage2 instability cannot be modeled as only `Index out of bounds` or only `emit_function` clear-path crashes; release stage2 also shows an early runtime self-loop in `Crystal$Dmain...` on stage3 bootstrap input.

Current hypothesis
- Root-cause cluster remains a broader stage2 self-host corruption class (monomorphization/lowering/runtime ABI interactions), with at least two active manifestations: (1) BlockId container-clear crash path in `emit_function` and (2) early `Crystal$Dmain...` self-loop during `stage2 -> stage3` release bootstrap.
