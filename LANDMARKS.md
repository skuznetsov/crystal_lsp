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

[LM-16|boundary]: stage2 self-loop reproduces even after rebuilding stage2 with a fresh isolated stage2 cache (`/private/tmp/crystal_cache_stage2_release_selfloop_probe`), so this failure class is not explained by stale stage-cache reuse {F/G/R: 0.9/0.7/0.9} [verified]

[LM-17|root-cause]: on the same generated IR (`/private/tmp/stage2_rel_selfloop_probe.ll`), `llc -O3` alone keeps a valid `Crystal$Dmain` flow, while `opt -O3` + `llc -O3` collapses `__crystal_main` and `Crystal$Dmain_user_code...` into `tailrecurse` infinite loops; the runtime stage2 self-loop is introduced at/after LLVM `opt` stage {F/G/R: 0.9/0.8/0.9} [verified]

[LM-18|repro]: focused oracle `regression_tests/stage2_opt_tailrecurse_repro.sh` reproduces `opt`-stage collapse on stage2-generated IR (`O3` reproduces, `O0` does not) {F/G/R: 0.9/0.8/0.9} [verified]

[LM-19|tooling]: `/tmp` hygiene utility `scripts/cleanup_tmp_stage_artifacts.sh` is available with dry-run default and explicit `--yes` deletion gate; validated by cleaning stage artifacts while preserving selected baseline binaries via `--keep` globs (`~0.94 GB` reclaimed in one run) {F/G/R: 0.9/0.7/0.9} [verified]

[LM-20|repro]: `regression_tests/stage2_opt_tailrecurse_bisect.sh` localizes the first bad LLVM opt bisect limit for dual collapse (`__crystal_main` + `main_user_code`) on current IR to `495575`, with the last running pass `InlinerPass` on `Crystal$Dmain_user_code...` {F/G/R: 0.9/0.8/0.9} [verified]

[LM-21|boundary]: refreshed timeout diagnostics (`scripts/timeout_sample_lldb.sh`) on `stage2 -> stage3` show active hot loop in `Crystal$Dmain... + 268` with unconditional self-branch, matching run-safe 240s timeouts (cache on/off) {F/G/R: 0.9/0.8/0.9} [verified]

[LM-22|mitigation]: CLI-level LLVM entry guard (`CRYSTAL_V2_LLVM_ENTRY_OPT_GUARD`, default on) patches generated `.ll` definitions for `__crystal_main` / `Crystal$Dmain...` / `main_user_code` with `noinline optnone` before `opt`; on rebuilt toolchain this removes `stage2_opt_tailrecurse_repro` collapse for `__crystal_main`+`main_user_code` (`collapsed=0/0`) {F/G/R: 0.9/0.7/0.9} [verified]

[LM-23|boundary]: after entry guard, previous `Crystal$Dmain` self-loop signature is no longer primary (`stage2_main_selfloop_repro` becomes signature mismatch with `main_frame_hits=0`), but stage2 still hangs with a shifted self-loop at `IO$CCFileDescriptor$Dfrom_stdio$$Int32` {F/G/R: 0.9/0.8/0.9} [verified]

[LM-24|repro]: focused oracle `regression_tests/stage2_from_stdio_selfloop_repro.sh` reproduces the new stage2-only `IO::FileDescriptor.from_stdio` self-branch signature (`from_stdio_frame_hits>=1` + self `b` target) and does not reproduce on stage1 control {F/G/R: 0.9/0.8/0.9} [verified]

[LM-25|root-cause]: parser bug, not HIR owner lookup, explains the `from_stdio` leak: a standalone `if` following a multiline assignment with RHS `if ... end` was being attached as a postfix modifier because `parse_postfix_if_modifier` used `previous_token` instead of the parsed statement span for same-line gating; verified both on a minimal parser-only repro and on `src/stdlib/crystal/system/unix/file_descriptor.cr`, where top-level leaked defs (`system_pipe`, `pread`, `from_stdio`, ...) disappear after the fix and remain nested under `Crystal::System::FileDescriptor` {F/G/R: 0.95/0.8/0.95} [verified]

Contradiction ledger
- [LM-C1|refute]: broad `reset_value_names` reinit experiment (replace many `clear` with fresh container allocations) did not produce robust stabilization; it shifted crash boundaries and was rejected.
- [LM-C2|refute]: cache-only explanation is insufficient: fresh isolated stage2 debug cache (`CRYSTAL_CACHE_DIR_STAGE2_DEBUG=/tmp/crystal_cache_stage2_debug_reset_clean`) still reproduces `stage2_reset_value_names_fiberevent_clear_repro` with `status=139` and `FiberEvent$Hclear` drift.
- [LM-C3|refute]: `Set`-specific root-cause hypothesis is insufficient; stage2 fails equally on trivial/no-container and `Hash/Set` micro-repros.
- [LM-C4|refute]: targeted `@alloc_types.clear -> reinit` workaround in `reset_value_names` was unstable and regressed crash boundary; rejected.
- [LM-C5|refute]: converting `@alloc_types/@alloc_element_types` from Hash to array-indexed storage did not provide robust stage2 stabilization; boundary remained unstable and change was reverted.
- [LM-C6|refute]: `FiberEvent$Hclear` call-target drift in `reset_value_names` is no longer reproduced on current stage2 release/debug binaries; active crash shifted to BlockId hash/set clear path in `emit_function`.
- [LM-C7|refute]: current stage2 instability cannot be modeled as only `Index out of bounds` or only `emit_function` clear-path crashes; release stage2 also shows an early runtime self-loop in `Crystal$Dmain...` on stage3 bootstrap input.
- [LM-C8|refute]: isolated `CRYSTAL_CACHE_DIR_STAGE2_RELEASE` cleanup alone does not remove the `Crystal$Dmain` self-loop; collapse persists on fresh-cache rebuilds.
- [LM-C9|refute]: fixing only `__crystal_main`/`main_user_code` opt-collapse is not sufficient for stage2 stability; after entry-guard mitigation the active self-loop shifted to `IO::FileDescriptor.from_stdio`.
- [LM-C10|refute]: the `from_stdio` leak is not macro-specific; the same malformed AST is reproduced by a plain multiline `ret = if ... end` followed by a normal `if`, with no `{% ... %}` control nodes involved.

Current hypothesis
- Root-cause cluster remains broader than a single bug. The `IO::FileDescriptor.from_stdio` self-loop branch is now explained by the parser postfix-modifier bug ([LM-25]); after rebuilding stage1/stage2 with that fix, the next active blocker needs to be remeasured to distinguish remaining LLVM `opt` collapse from any additional parser/codegen issues.
